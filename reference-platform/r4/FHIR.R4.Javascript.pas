unit FHIR.R4.Javascript;

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

interface

// FHIR v3.4.0 generated 2018-06-10T06:42:03+10:00

uses
  FHIR.Javascript, FHIR.Javascript.Base;

procedure registerFHIRTypes(js : TFHIRJavascript);

implementation

procedure defineElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Element4', 'id', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Element4', 'extension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineBackboneElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'BackboneElement4', 'modifierExtension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineBackboneTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'BackboneType4', 'modifierExtension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Resource4', 'id', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Resource4', 'meta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Resource4', 'implicitRules', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Resource4', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;


procedure defineDomainResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'DomainResource4', 'text', 'Narrative', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DomainResource4', 'contained', 'Resource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DomainResource4', 'extension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DomainResource4', 'modifierExtension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineParametersParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ParametersParameter4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ParametersParameter4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ParametersParameter4', 'valueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ParametersParameter4', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParametersParameter4', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParametersParameter4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'resource', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'part', '@Parameters.parameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParametersParameter4', nil, 'ParametersParameter4', js.FHIRFactoryJs);
  defineParametersParameterPropsJs(js, def);
end;


procedure defineParametersPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Parameters4', 'parameter', 'ParametersParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Parameters4', nil, 'Parameters4', js.FHIRFactoryJs);
  defineParametersPropsJs(js, def);
end;


procedure defineMetadataResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
end;


procedure defineExtensionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Extension4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Extension4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Extension4', 'valueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension4', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Extension4', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Extension4', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Extension4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExtensionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Extension4', nil, 'Extension4', js.FHIRFactoryJs);
  defineExtensionPropsJs(js, def);
end;


procedure defineNarrativePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Narrative4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Narrative4', 'div', 'xhtml', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNarrativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Narrative4', nil, 'Narrative4', js.FHIRFactoryJs);
  defineNarrativePropsJs(js, def);
end;


procedure defineContributorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Contributor4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contributor4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contributor4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContributorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contributor4', nil, 'Contributor4', js.FHIRFactoryJs);
  defineContributorPropsJs(js, def);
end;


procedure defineAttachmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Attachment4', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment4', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment4', 'data', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Attachment4', 'url', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment4', 'size', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment4', 'hash', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Attachment4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment4', 'creation', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineAttachmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Attachment4', nil, 'Attachment4', js.FHIRFactoryJs);
  defineAttachmentPropsJs(js, def);
end;


procedure defineDataRequirementCodeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementCodeFilter4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter4', 'valueSet', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter4', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataRequirementCodeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementCodeFilter4', nil, 'DataRequirementCodeFilter4', js.FHIRFactoryJs);
  defineDataRequirementCodeFilterPropsJs(js, def);
end;


procedure defineDataRequirementDateFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementDateFilter4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementDateFilter4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DataRequirementDateFilter4', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirementDateFilter4', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDataRequirementDateFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementDateFilter4', nil, 'DataRequirementDateFilter4', js.FHIRFactoryJs);
  defineDataRequirementDateFilterPropsJs(js, def);
end;


procedure defineDataRequirementSortPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementSort4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementSort4', 'direction', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDataRequirementSortJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementSort4', nil, 'DataRequirementSort4', js.FHIRFactoryJs);
  defineDataRequirementSortPropsJs(js, def);
end;


procedure defineDataRequirementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirement4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirement4', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirement4', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirement4', 'codeFilter', 'DataRequirementCodeFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataRequirement4', 'dateFilter', 'DataRequirementDateFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataRequirement4', 'limit', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'DataRequirement4', 'sort', 'DataRequirementSort', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataRequirementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirement4', nil, 'DataRequirement4', js.FHIRFactoryJs);
  defineDataRequirementPropsJs(js, def);
end;


procedure defineDosageDoseAndRatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DosageDoseAndRate4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate4', 'doseRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate4', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate4', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate4', 'rateRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate4', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDosageDoseAndRateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DosageDoseAndRate4', nil, 'DosageDoseAndRate4', js.FHIRFactoryJs);
  defineDosageDoseAndRatePropsJs(js, def);
end;


procedure defineDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'Dosage4', 'sequence', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Dosage4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Dosage4', 'additionalInstruction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Dosage4', 'patientInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Dosage4', 'timing', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Dosage4', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'doseAndRate', 'DosageDoseAndRate', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Dosage4', 'maxDosePerPeriod', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'maxDosePerAdministration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'maxDosePerLifetime', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Dosage4', nil, 'Dosage4', js.FHIRFactoryJs);
  defineDosagePropsJs(js, def);
end;


procedure defineMoneyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Money4', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Money4', 'currency', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMoneyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Money4', nil, 'Money4', js.FHIRFactoryJs);
  defineMoneyPropsJs(js, def);
end;


procedure defineMarketingStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'MarketingStatus4', 'country', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus4', 'jurisdiction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus4', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus4', 'dateRange', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus4', 'restoreDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineMarketingStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MarketingStatus4', nil, 'MarketingStatus4', js.FHIRFactoryJs);
  defineMarketingStatusPropsJs(js, def);
end;


procedure defineIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Identifier4', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Identifier4', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Identifier4', 'assigner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Identifier4', nil, 'Identifier4', js.FHIRFactoryJs);
  defineIdentifierPropsJs(js, def);
end;


procedure defineSubstanceAmountReferenceRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceAmountReferenceRange4', 'lowLimit', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmountReferenceRange4', 'highLimit', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceAmountReferenceRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceAmountReferenceRange4', nil, 'SubstanceAmountReferenceRange4', js.FHIRFactoryJs);
  defineSubstanceAmountReferenceRangePropsJs(js, def);
end;


procedure defineSubstanceAmountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'SubstanceAmount4', 'amountQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount4', 'amountRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount4', 'amountString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceAmount4', 'amountType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount4', 'amountText', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceAmount4', 'referenceRange', 'SubstanceAmountReferenceRange', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceAmountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceAmount4', nil, 'SubstanceAmount4', js.FHIRFactoryJs);
  defineSubstanceAmountPropsJs(js, def);
end;


procedure defineCodingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Coding4', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding4', 'userSelected', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineCodingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coding4', nil, 'Coding4', js.FHIRFactoryJs);
  defineCodingPropsJs(js, def);
end;


procedure defineSampledDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SampledData4', 'origin', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SampledData4', 'period', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData4', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData4', 'lowerLimit', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData4', 'upperLimit', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData4', 'dimensions', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SampledData4', 'data', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSampledDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SampledData4', nil, 'SampledData4', js.FHIRFactoryJs);
  defineSampledDataPropsJs(js, def);
end;


procedure defineRatioPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Ratio4', 'numerator', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Ratio4', 'denominator', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRatioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Ratio4', nil, 'Ratio4', js.FHIRFactoryJs);
  defineRatioPropsJs(js, def);
end;


procedure defineSubstanceMoietyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'SubstanceMoiety4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceMoiety4', 'stereochemistry', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety4', 'opticalActivity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety4', 'molecularFormula', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceMoiety4', 'amount', 'SubstanceAmount', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceMoietyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceMoiety4', nil, 'SubstanceMoiety4', js.FHIRFactoryJs);
  defineSubstanceMoietyPropsJs(js, def);
end;


procedure defineReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Reference4', 'reference', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Reference4', 'type', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Reference4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Reference4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Reference4', nil, 'Reference4', js.FHIRFactoryJs);
  defineReferencePropsJs(js, def);
end;


procedure defineTriggerDefinitionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TriggerDefinitionCondition4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinitionCondition4', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinitionCondition4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTriggerDefinitionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TriggerDefinitionCondition4', nil, 'TriggerDefinitionCondition4', js.FHIRFactoryJs);
  defineTriggerDefinitionConditionPropsJs(js, def);
end;


procedure defineTriggerDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TriggerDefinition4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinition4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinition4', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition4', 'timingReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition4', 'timingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TriggerDefinition4', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TriggerDefinition4', 'data', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition4', 'condition', 'TriggerDefinitionCondition', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTriggerDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TriggerDefinition4', nil, 'TriggerDefinition4', js.FHIRFactoryJs);
  defineTriggerDefinitionPropsJs(js, def);
end;


procedure definePeriodPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Period4', 'start', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Period4', 'end', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure definePeriodJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Period4', nil, 'Period4', js.FHIRFactoryJs);
  definePeriodPropsJs(js, def);
end;


procedure defineQuantityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Quantity4', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Quantity4', 'comparator', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity4', 'unit', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity4', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineQuantityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Quantity4', nil, 'Quantity4', js.FHIRFactoryJs);
  defineQuantityPropsJs(js, def);
end;


procedure defineRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Range4', 'low', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Range4', 'high', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Range4', nil, 'Range4', js.FHIRFactoryJs);
  defineRangePropsJs(js, def);
end;


procedure defineRelatedArtifactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'RelatedArtifact4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact4', 'citation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact4', 'url', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact4', 'document', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedArtifact4', 'resource', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRelatedArtifactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedArtifact4', nil, 'RelatedArtifact4', js.FHIRFactoryJs);
  defineRelatedArtifactPropsJs(js, def);
end;


procedure defineAnnotationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Annotation4', 'authorReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Annotation4', 'authorString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Annotation4', 'time', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Annotation4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAnnotationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Annotation4', nil, 'Annotation4', js.FHIRFactoryJs);
  defineAnnotationPropsJs(js, def);
end;


procedure defineProductShelfLifePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'ProductShelfLife4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife4', 'period', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife4', 'specialPrecautionsForStorage', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductShelfLifeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductShelfLife4', nil, 'ProductShelfLife4', js.FHIRFactoryJs);
  defineProductShelfLifePropsJs(js, def);
end;


procedure defineContactDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactDetail4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactDetail4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContactDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactDetail4', nil, 'ContactDetail4', js.FHIRFactoryJs);
  defineContactDetailPropsJs(js, def);
end;


procedure defineUsageContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'UsageContext4', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineUsageContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UsageContext4', nil, 'UsageContext4', js.FHIRFactoryJs);
  defineUsageContextPropsJs(js, def);
end;


procedure defineSignaturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Signature4', 'type', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Signature4', 'when', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Signature4', 'whoUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature4', 'whoReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature4', 'onBehalfOfUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature4', 'onBehalfOfReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature4', 'targetFormat', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature4', 'sigFormat', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature4', 'blob', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineSignatureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Signature4', nil, 'Signature4', js.FHIRFactoryJs);
  defineSignaturePropsJs(js, def);
end;


procedure defineProdCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'ProdCharacteristic4', 'height', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'width', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'depth', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'weight', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'nominalVolume', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'externalDiameter', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'shape', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProdCharacteristic4', 'image', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProdCharacteristic4', 'scoring', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProdCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProdCharacteristic4', nil, 'ProdCharacteristic4', js.FHIRFactoryJs);
  defineProdCharacteristicPropsJs(js, def);
end;


procedure defineCodeableConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'CodeableConcept4', 'coding', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeableConcept4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeableConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeableConcept4', nil, 'CodeableConcept4', js.FHIRFactoryJs);
  defineCodeableConceptPropsJs(js, def);
end;


procedure defineParameterDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ParameterDefinition4', 'name', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition4', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition4', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParameterDefinition4', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition4', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineParameterDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParameterDefinition4', nil, 'ParameterDefinition4', js.FHIRFactoryJs);
  defineParameterDefinitionPropsJs(js, def);
end;


procedure defineContactPointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactPoint4', 'system', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint4', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint4', 'rank', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ContactPoint4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContactPointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactPoint4', nil, 'ContactPoint4', js.FHIRFactoryJs);
  defineContactPointPropsJs(js, def);
end;


procedure defineHumanNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'HumanName4', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName4', 'family', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineHumanNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HumanName4', nil, 'HumanName4', js.FHIRFactoryJs);
  defineHumanNamePropsJs(js, def);
end;


procedure defineMetaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Meta4', 'versionId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Meta4', 'lastUpdated', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Meta4', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Meta4', 'security', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Meta4', 'tag', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMetaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Meta4', nil, 'Meta4', js.FHIRFactoryJs);
  defineMetaPropsJs(js, def);
end;


procedure defineAddressPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Address4', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'city', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'district', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'state', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'postalCode', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'country', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAddressJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Address4', nil, 'Address4', js.FHIRFactoryJs);
  defineAddressPropsJs(js, def);
end;


procedure defineElementDefinitionSlicingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicing4', 'discriminator', 'ElementDefinitionSlicingDiscriminator', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinitionSlicing4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicing4', 'ordered', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionSlicing4', 'rules', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicing4', nil, 'ElementDefinitionSlicing4', js.FHIRFactoryJs);
  defineElementDefinitionSlicingPropsJs(js, def);
end;


procedure defineElementDefinitionSlicingDiscriminatorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicingDiscriminator4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicingDiscriminator4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingDiscriminatorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicingDiscriminator4', nil, 'ElementDefinitionSlicingDiscriminator4', js.FHIRFactoryJs);
  defineElementDefinitionSlicingDiscriminatorPropsJs(js, def);
end;


procedure defineElementDefinitionBasePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBase4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase4', 'min', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase4', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionBaseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBase4', nil, 'ElementDefinitionBase4', js.FHIRFactoryJs);
  defineElementDefinitionBasePropsJs(js, def);
end;


procedure defineElementDefinitionTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionType4', 'code', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionType4', 'versioning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionTypeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionType4', nil, 'ElementDefinitionType4', js.FHIRFactoryJs);
  defineElementDefinitionTypePropsJs(js, def);
end;


procedure defineElementDefinitionExamplePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionExample4', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineElementDefinitionExampleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionExample4', nil, 'ElementDefinitionExample4', js.FHIRFactoryJs);
  defineElementDefinitionExamplePropsJs(js, def);
end;


procedure defineElementDefinitionConstraintPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'key', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'human', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'xpath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'source', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionConstraintJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionConstraint4', nil, 'ElementDefinitionConstraint4', js.FHIRFactoryJs);
  defineElementDefinitionConstraintPropsJs(js, def);
end;


procedure defineElementDefinitionBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBinding4', 'strength', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding4', 'valueSet', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBinding4', nil, 'ElementDefinitionBinding4', js.FHIRFactoryJs);
  defineElementDefinitionBindingPropsJs(js, def);
end;


procedure defineElementDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionMapping4', 'identity', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping4', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping4', 'map', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionMapping4', nil, 'ElementDefinitionMapping4', js.FHIRFactoryJs);
  defineElementDefinitionMappingPropsJs(js, def);
end;


procedure defineElementDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'ElementDefinition4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'sliceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition4', 'slicing', 'ElementDefinitionSlicing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'short', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'definition', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'comment', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'requirements', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'min', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'base', 'ElementDefinitionBase', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'contentReference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'type', 'ElementDefinitionType', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'meaningWhenMissing', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'orderMeaning', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition4', 'patternBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'patternCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition4', 'patternId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'patternInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'patternMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'patternString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'example', 'ElementDefinitionExample', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'minValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'maxLength', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'constraint', 'ElementDefinitionConstraint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition4', 'mustSupport', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'isModifier', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'isModifierReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'isSummary', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'binding', 'ElementDefinitionBinding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'mapping', 'ElementDefinitionMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineElementDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinition4', nil, 'ElementDefinition4', js.FHIRFactoryJs);
  defineElementDefinitionPropsJs(js, def);
end;


procedure defineTimingRepeatPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TimingRepeat4', 'boundsDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat4', 'boundsRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat4', 'boundsPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat4', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat4', 'countMax', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat4', 'duration', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat4', 'durationMax', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat4', 'durationUnit', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat4', 'frequency', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat4', 'frequencyMax', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat4', 'period', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat4', 'periodMax', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat4', 'periodUnit', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat4', 'offset', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTimingRepeatJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TimingRepeat4', nil, 'TimingRepeat4', js.FHIRFactoryJs);
  defineTimingRepeatPropsJs(js, def);
end;


procedure defineTimingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'Timing4', 'repeat', 'TimingRepeat', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Timing4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTimingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Timing4', nil, 'Timing4', js.FHIRFactoryJs);
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
  def := js.defineClass('Count4', nil, 'Count4', js.FHIRFactoryJs);
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
  def := js.defineClass('Age4', nil, 'Age4', js.FHIRFactoryJs);
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
  def := js.defineClass('Distance4', nil, 'Distance4', js.FHIRFactoryJs);
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
  def := js.defineClass('Duration4', nil, 'Duration4', js.FHIRFactoryJs);
  defineDurationPropsJs(js, def);
end;


procedure defineAccountCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AccountCoverage4', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AccountCoverage4', 'priority', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineAccountCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AccountCoverage4', nil, 'AccountCoverage4', js.FHIRFactoryJs);
  defineAccountCoveragePropsJs(js, def);
end;


procedure defineAccountGuarantorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AccountGuarantor4', 'party', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AccountGuarantor4', 'onHold', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'AccountGuarantor4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAccountGuarantorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AccountGuarantor4', nil, 'AccountGuarantor4', js.FHIRFactoryJs);
  defineAccountGuarantorPropsJs(js, def);
end;


procedure defineAccountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Account4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account4', 'servicePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account4', 'coverage', 'AccountCoverage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account4', 'owner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account4', 'guarantor', 'AccountGuarantor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account4', 'partOf', 'Reference(Account)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAccountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Account4', nil, 'Account4', js.FHIRFactoryJs);
  defineAccountPropsJs(js, def);
end;


procedure defineActivityDefinitionParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ActivityDefinitionParticipant4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionParticipant4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineActivityDefinitionParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinitionParticipant4', nil, 'ActivityDefinitionParticipant4', js.FHIRFactoryJs);
  defineActivityDefinitionParticipantPropsJs(js, def);
end;


procedure defineActivityDefinitionDynamicValuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ActivityDefinitionDynamicValue4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue4', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineActivityDefinitionDynamicValueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinitionDynamicValue4', nil, 'ActivityDefinitionDynamicValue4', js.FHIRFactoryJs);
  defineActivityDefinitionDynamicValuePropsJs(js, def);
end;


procedure defineActivityDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ActivityDefinition4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ActivityDefinition4', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition4', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'participant', 'ActivityDefinitionParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'productReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'productCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'dosage', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'specimenRequirement', 'Reference(SpecimenDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'transform', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'dynamicValue', 'ActivityDefinitionDynamicValue', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineActivityDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinition4', nil, 'ActivityDefinition4', js.FHIRFactoryJs);
  defineActivityDefinitionPropsJs(js, def);
end;


procedure defineAdverseEventSuspectEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AdverseEventSuspectEntity4', 'instance', 'Reference(Immunization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntity4', 'causality', 'AdverseEventSuspectEntityCausality', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAdverseEventSuspectEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEventSuspectEntity4', nil, 'AdverseEventSuspectEntity4', js.FHIRFactoryJs);
  defineAdverseEventSuspectEntityPropsJs(js, def);
end;


procedure defineAdverseEventSuspectEntityCausalityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality4', 'assessment', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality4', 'productRelatedness', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality4', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality4', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAdverseEventSuspectEntityCausalityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEventSuspectEntityCausality4', nil, 'AdverseEventSuspectEntityCausality4', js.FHIRFactoryJs);
  defineAdverseEventSuspectEntityCausalityPropsJs(js, def);
end;


procedure defineAdverseEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AdverseEvent4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'actuality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEvent4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'event', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AdverseEvent4', 'resultingCondition', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'seriousness', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'severity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'recorder', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'contributor', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'suspectEntity', 'AdverseEventSuspectEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'subjectMedicalHistory', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'referenceDocument', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'study', 'Reference(ResearchStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAdverseEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEvent4', nil, 'AdverseEvent4', js.FHIRFactoryJs);
  defineAdverseEventPropsJs(js, def);
end;


procedure defineAllergyIntoleranceReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'substance', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'manifestation', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'onset', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'exposureRoute', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntoleranceReaction4', nil, 'AllergyIntoleranceReaction4', js.FHIRFactoryJs);
  defineAllergyIntoleranceReactionPropsJs(js, def);
end;


procedure defineAllergyIntolerancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AllergyIntolerance4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance4', 'clinicalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance4', 'verificationStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance4', 'criticality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'onsetDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance4', 'onsetAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance4', 'assertedDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance4', 'recorder', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'asserter', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'lastOccurrence', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance4', 'reaction', 'AllergyIntoleranceReaction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntolerance4', nil, 'AllergyIntolerance4', js.FHIRFactoryJs);
  defineAllergyIntolerancePropsJs(js, def);
end;


procedure defineAppointmentParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AppointmentParticipant4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentParticipant4', 'actor', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentParticipant4', 'required', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentParticipant4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentParticipant4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAppointmentParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentParticipant4', nil, 'AppointmentParticipant4', js.FHIRFactoryJs);
  defineAppointmentParticipantPropsJs(js, def);
end;


procedure defineAppointmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Appointment4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment4', 'serviceCategory', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'appointmentType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment4', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'indication', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'priority', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment4', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment4', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment4', 'minutesDuration', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Appointment4', 'slot', 'Reference(Slot)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment4', 'patientInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment4', 'basedOn', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'participant', 'AppointmentParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'requestedPeriod', 'Period', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAppointmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Appointment4', nil, 'Appointment4', js.FHIRFactoryJs);
  defineAppointmentPropsJs(js, def);
end;


procedure defineAppointmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AppointmentResponse4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse4', 'appointment', 'Reference(Appointment)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse4', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse4', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse4', 'participantType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse4', 'actor', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse4', 'participantStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentResponse4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAppointmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentResponse4', nil, 'AppointmentResponse4', js.FHIRFactoryJs);
  defineAppointmentResponsePropsJs(js, def);
end;


procedure defineAuditEventAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventAgent4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventAgent4', 'reference', 'Reference(PractitionerRole)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'userId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'altId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgent4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgent4', 'requestor', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'AuditEventAgent4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'media', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'network', 'AuditEventAgentNetwork', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'purposeOfUse', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventAgent4', nil, 'AuditEventAgent4', js.FHIRFactoryJs);
  defineAuditEventAgentPropsJs(js, def);
end;


procedure defineAuditEventAgentNetworkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventAgentNetwork4', 'address', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgentNetwork4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAuditEventAgentNetworkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventAgentNetwork4', nil, 'AuditEventAgentNetwork4', js.FHIRFactoryJs);
  defineAuditEventAgentNetworkPropsJs(js, def);
end;


procedure defineAuditEventSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventSource4', 'site', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventSource4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventSource4', 'type', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventSource4', nil, 'AuditEventSource4', js.FHIRFactoryJs);
  defineAuditEventSourcePropsJs(js, def);
end;


procedure defineAuditEventEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEntity4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity4', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity4', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity4', 'role', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity4', 'lifecycle', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity4', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventEntity4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntity4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntity4', 'query', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'AuditEventEntity4', 'detail', 'AuditEventEntityDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEntity4', nil, 'AuditEventEntity4', js.FHIRFactoryJs);
  defineAuditEventEntityPropsJs(js, def);
end;


procedure defineAuditEventEntityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEntityDetail4', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntityDetail4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntityDetail4', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineAuditEventEntityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEntityDetail4', nil, 'AuditEventEntityDetail4', js.FHIRFactoryJs);
  defineAuditEventEntityDetailPropsJs(js, def);
end;


procedure defineAuditEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AuditEvent4', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent4', 'subtype', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent4', 'action', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent4', 'recorded', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AuditEvent4', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent4', 'outcomeDesc', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent4', 'purposeOfEvent', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent4', 'agent', 'AuditEventAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent4', 'source', 'AuditEventSource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent4', 'entity', 'AuditEventEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEvent4', nil, 'AuditEvent4', js.FHIRFactoryJs);
  defineAuditEventPropsJs(js, def);
end;


procedure defineBasicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Basic4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Basic4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic4', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic4', 'created', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Basic4', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBasicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Basic4', nil, 'Basic4', js.FHIRFactoryJs);
  defineBasicPropsJs(js, def);
end;


procedure defineBinaryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Binary4', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Binary4', 'securityContext', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Binary4', 'content', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineBinaryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Binary4', nil, 'Binary4', js.FHIRFactoryJs);
  defineBinaryPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductCollectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductCollection4', 'collector', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection4', 'source', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection4', 'collectedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection4', 'collectedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductCollectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductCollection4', nil, 'BiologicallyDerivedProductCollection4', js.FHIRFactoryJs);
  defineBiologicallyDerivedProductCollectionPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductProcessingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing4', 'procedure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing4', 'additive', 'Reference(Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing4', 'timeDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing4', 'timePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductProcessingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductProcessing4', nil, 'BiologicallyDerivedProductProcessing4', js.FHIRFactoryJs);
  defineBiologicallyDerivedProductProcessingPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductManipulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation4', 'timeDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation4', 'timePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductManipulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductManipulation4', nil, 'BiologicallyDerivedProductManipulation4', js.FHIRFactoryJs);
  defineBiologicallyDerivedProductManipulationPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductStoragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductStorage4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage4', 'temperature', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage4', 'scale', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage4', 'duration', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductStorageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductStorage4', nil, 'BiologicallyDerivedProductStorage4', js.FHIRFactoryJs);
  defineBiologicallyDerivedProductStoragePropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'productCategory', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'productCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'request', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'quantity', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'parent', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'collection', 'BiologicallyDerivedProductCollection', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'processing', 'BiologicallyDerivedProductProcessing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'manipulation', 'BiologicallyDerivedProductManipulation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'storage', 'BiologicallyDerivedProductStorage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineBiologicallyDerivedProductJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProduct4', nil, 'BiologicallyDerivedProduct4', js.FHIRFactoryJs);
  defineBiologicallyDerivedProductPropsJs(js, def);
end;


procedure defineBodyStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BodyStructure4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'BodyStructure4', 'morphology', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BodyStructure4', 'location', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BodyStructure4', 'locationQualifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BodyStructure4', 'image', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBodyStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BodyStructure4', nil, 'BodyStructure4', js.FHIRFactoryJs);
  defineBodyStructurePropsJs(js, def);
end;


procedure defineBundleLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleLink4', 'relation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleLink4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineBundleLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleLink4', nil, 'BundleLink4', js.FHIRFactoryJs);
  defineBundleLinkPropsJs(js, def);
end;


procedure defineBundleEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntry4', 'link', '@Bundle.link', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BundleEntry4', 'fullUrl', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntry4', 'resource', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry4', 'search', 'BundleEntrySearch', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry4', 'request', 'BundleEntryRequest', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry4', 'response', 'BundleEntryResponse', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntry4', nil, 'BundleEntry4', js.FHIRFactoryJs);
  defineBundleEntryPropsJs(js, def);
end;


procedure defineBundleEntrySearchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntrySearch4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntrySearch4', 'score', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineBundleEntrySearchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntrySearch4', nil, 'BundleEntrySearch4', js.FHIRFactoryJs);
  defineBundleEntrySearchPropsJs(js, def);
end;


procedure defineBundleEntryRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryRequest4', 'method', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest4', 'ifNoneMatch', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest4', 'ifModifiedSince', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryRequest4', 'ifMatch', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest4', 'ifNoneExist', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineBundleEntryRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryRequest4', nil, 'BundleEntryRequest4', js.FHIRFactoryJs);
  defineBundleEntryRequestPropsJs(js, def);
end;


procedure defineBundleEntryResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryResponse4', 'status', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse4', 'location', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse4', 'etag', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse4', 'lastModified', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryResponse4', 'outcome', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleEntryResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryResponse4', nil, 'BundleEntryResponse4', js.FHIRFactoryJs);
  defineBundleEntryResponsePropsJs(js, def);
end;


procedure defineBundlePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Bundle4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Bundle4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Bundle4', 'timestamp', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Bundle4', 'total', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Bundle4', 'link', 'BundleLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Bundle4', 'entry', 'BundleEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Bundle4', 'signature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Bundle4', nil, 'Bundle4', js.FHIRFactoryJs);
  defineBundlePropsJs(js, def);
end;


procedure defineCapabilityStatementSoftwarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementSoftware4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementSoftware4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementSoftware4', 'releaseDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineCapabilityStatementSoftwareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementSoftware4', nil, 'CapabilityStatementSoftware4', js.FHIRFactoryJs);
  defineCapabilityStatementSoftwarePropsJs(js, def);
end;


procedure defineCapabilityStatementImplementationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementImplementation4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementImplementation4', 'url', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementImplementationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementImplementation4', nil, 'CapabilityStatementImplementation4', js.FHIRFactoryJs);
  defineCapabilityStatementImplementationPropsJs(js, def);
end;


procedure defineCapabilityStatementRestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRest4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'security', 'CapabilityStatementRestSecurity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'resource', 'CapabilityStatementRestResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'interaction', 'CapabilityStatementRestInteraction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'searchParam', '@CapabilityStatement.rest.resource.searchParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'operation', '@CapabilityStatement.rest.resource.operation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRest4', nil, 'CapabilityStatementRest4', js.FHIRFactoryJs);
  defineCapabilityStatementRestPropsJs(js, def);
end;


procedure defineCapabilityStatementRestSecurityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestSecurity4', 'cors', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity4', 'service', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestSecurityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestSecurity4', nil, 'CapabilityStatementRestSecurity4', js.FHIRFactoryJs);
  defineCapabilityStatementRestSecurityPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'interaction', 'CapabilityStatementRestResourceInteraction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'versioning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'readHistory', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'updateCreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'conditionalCreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'conditionalRead', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'conditionalUpdate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'conditionalDelete', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'searchParam', 'CapabilityStatementRestResourceSearchParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'operation', 'CapabilityStatementRestResourceOperation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResource4', nil, 'CapabilityStatementRestResource4', js.FHIRFactoryJs);
  defineCapabilityStatementRestResourcePropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceInteraction4', nil, 'CapabilityStatementRestResourceInteraction4', js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceInteractionPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceSearchParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam4', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceSearchParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceSearchParam4', nil, 'CapabilityStatementRestResourceSearchParam4', js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceSearchParamPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation4', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation4', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceOperation4', nil, 'CapabilityStatementRestResourceOperation4', js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceOperationPropsJs(js, def);
end;


procedure defineCapabilityStatementRestInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestInteraction4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestInteraction4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestInteraction4', nil, 'CapabilityStatementRestInteraction4', js.FHIRFactoryJs);
  defineCapabilityStatementRestInteractionPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessaging4', 'endpoint', 'CapabilityStatementMessagingEndpoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementMessaging4', 'reliableCache', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessaging4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessaging4', 'supportedMessage', 'CapabilityStatementMessagingSupportedMessage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementMessagingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessaging4', nil, 'CapabilityStatementMessaging4', js.FHIRFactoryJs);
  defineCapabilityStatementMessagingPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint4', 'protocol', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint4', 'address', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementMessagingEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingEndpoint4', nil, 'CapabilityStatementMessagingEndpoint4', js.FHIRFactoryJs);
  defineCapabilityStatementMessagingEndpointPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingSupportedMessagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage4', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementMessagingSupportedMessageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingSupportedMessage4', nil, 'CapabilityStatementMessagingSupportedMessage4', js.FHIRFactoryJs);
  defineCapabilityStatementMessagingSupportedMessagePropsJs(js, def);
end;


procedure defineCapabilityStatementDocumentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementDocument4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument4', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementDocumentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementDocument4', nil, 'CapabilityStatementDocument4', js.FHIRFactoryJs);
  defineCapabilityStatementDocumentPropsJs(js, def);
end;


procedure defineCapabilityStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CapabilityStatement4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatement4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CapabilityStatement4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'software', 'CapabilityStatementSoftware', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatement4', 'implementation', 'CapabilityStatementImplementation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatement4', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'rest', 'CapabilityStatementRest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement4', 'messaging', 'CapabilityStatementMessaging', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement4', 'document', 'CapabilityStatementDocument', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatement4', nil, 'CapabilityStatement4', js.FHIRFactoryJs);
  defineCapabilityStatementPropsJs(js, def);
end;


procedure defineCarePlanActivityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivity4', 'outcomeCodeableConcept', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity4', 'outcomeReference', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity4', 'progress', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity4', 'reference', 'Reference(Appointment)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivity4', 'detail', 'CarePlanActivityDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCarePlanActivityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivity4', nil, 'CarePlanActivity4', js.FHIRFactoryJs);
  defineCarePlanActivityPropsJs(js, def);
end;


procedure defineCarePlanActivityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivityDetail4', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'instantiates', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'goal', 'Reference(Goal)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'statusReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'scheduledTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'scheduledPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'scheduledString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'performer', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'productCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'productReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'dailyAmount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCarePlanActivityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivityDetail4', nil, 'CarePlanActivityDetail4', js.FHIRFactoryJs);
  defineCarePlanActivityDetailPropsJs(js, def);
end;


procedure defineCarePlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CarePlan4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'replaces', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'partOf', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan4', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan4', 'author', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'careTeam', 'Reference(CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'addresses', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'goal', 'Reference(Goal)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'activity', 'CarePlanActivity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCarePlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlan4', nil, 'CarePlan4', js.FHIRFactoryJs);
  defineCarePlanPropsJs(js, def);
end;


procedure defineCareTeamParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CareTeamParticipant4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant4', 'member', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant4', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCareTeamParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CareTeamParticipant4', nil, 'CareTeamParticipant4', js.FHIRFactoryJs);
  defineCareTeamParticipantPropsJs(js, def);
end;


procedure defineCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CareTeam4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CareTeam4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CareTeam4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam4', 'participant', 'CareTeamParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'managingOrganization', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CareTeam4', nil, 'CareTeam4', js.FHIRFactoryJs);
  defineCareTeamPropsJs(js, def);
end;


procedure defineChargeItemPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemPerformer4', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemPerformer4', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineChargeItemPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemPerformer4', nil, 'ChargeItemPerformer4', js.FHIRFactoryJs);
  defineChargeItemPerformerPropsJs(js, def);
end;


procedure defineChargeItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ChargeItem4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItem4', 'partOf', 'Reference(ChargeItem)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem4', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'performer', 'ChargeItemPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'performingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'requestingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'costCenter', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'bodysite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'factorOverride', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ChargeItem4', 'priceOverride', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'overrideReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItem4', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'enteredDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem4', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'service', 'Reference(DiagnosticReport)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'productReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'productCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'account', 'Reference(Account)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineChargeItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItem4', nil, 'ChargeItem4', js.FHIRFactoryJs);
  defineChargeItemPropsJs(js, def);
end;


procedure defineChargeItemDefinitionApplicabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinitionApplicability4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinitionApplicability4', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinitionApplicability4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineChargeItemDefinitionApplicabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinitionApplicability4', nil, 'ChargeItemDefinitionApplicability4', js.FHIRFactoryJs);
  defineChargeItemDefinitionApplicabilityPropsJs(js, def);
end;


procedure defineChargeItemDefinitionPropertyGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroup4', 'applicability', '@ChargeItemDefinition.applicability', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroup4', 'priceComponent', 'ChargeItemDefinitionPropertyGroupPriceComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineChargeItemDefinitionPropertyGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinitionPropertyGroup4', nil, 'ChargeItemDefinitionPropertyGroup4', js.FHIRFactoryJs);
  defineChargeItemDefinitionPropertyGroupPropsJs(js, def);
end;


procedure defineChargeItemDefinitionPropertyGroupPriceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent4', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent4', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineChargeItemDefinitionPropertyGroupPriceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinitionPropertyGroupPriceComponent4', nil, 'ChargeItemDefinitionPropertyGroupPriceComponent4', js.FHIRFactoryJs);
  defineChargeItemDefinitionPropertyGroupPriceComponentPropsJs(js, def);
end;


procedure defineChargeItemDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinition4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'instance', 'Reference(Medication)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'applicability', 'ChargeItemDefinitionApplicability', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'propertyGroup', 'ChargeItemDefinitionPropertyGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineChargeItemDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinition4', nil, 'ChargeItemDefinition4', js.FHIRFactoryJs);
  defineChargeItemDefinitionPropsJs(js, def);
end;


procedure defineClaimRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimRelated4', 'claim', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimRelated4', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimRelated4', 'reference', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimRelated4', nil, 'ClaimRelated4', js.FHIRFactoryJs);
  defineClaimRelatedPropsJs(js, def);
end;


procedure defineClaimPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimPayee4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee4', 'resource', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee4', 'party', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimPayee4', nil, 'ClaimPayee4', js.FHIRFactoryJs);
  defineClaimPayeePropsJs(js, def);
end;


procedure defineClaimCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimCareTeam4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimCareTeam4', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCareTeam4', 'responsible', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimCareTeam4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCareTeam4', 'qualification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimCareTeam4', nil, 'ClaimCareTeam4', js.FHIRFactoryJs);
  defineClaimCareTeamPropsJs(js, def);
end;


procedure defineClaimInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimInformation4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimInformation4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation4', 'timingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimInformation4', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimInformation4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimInformation4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation4', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation4', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation4', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimInformation4', nil, 'ClaimInformation4', js.FHIRFactoryJs);
  defineClaimInformationPropsJs(js, def);
end;


procedure defineClaimDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimDiagnosis4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimDiagnosis4', 'diagnosisCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis4', 'diagnosisReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimDiagnosis4', 'packageCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimDiagnosis4', nil, 'ClaimDiagnosis4', js.FHIRFactoryJs);
  defineClaimDiagnosisPropsJs(js, def);
end;


procedure defineClaimProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimProcedure4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimProcedure4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimProcedure4', 'procedureCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimProcedure4', 'procedureReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimProcedure4', nil, 'ClaimProcedure4', js.FHIRFactoryJs);
  defineClaimProcedurePropsJs(js, def);
end;


procedure defineClaimInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimInsurance4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimInsurance4', 'focal', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimInsurance4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInsurance4', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInsurance4', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimInsurance4', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimInsurance4', nil, 'ClaimInsurance4', js.FHIRFactoryJs);
  defineClaimInsurancePropsJs(js, def);
end;


procedure defineClaimAccidentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimAccident4', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimAccident4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimAccident4', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimAccident4', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimAccidentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimAccident4', nil, 'ClaimAccident4', js.FHIRFactoryJs);
  defineClaimAccidentPropsJs(js, def);
end;


procedure defineClaimItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItem4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItem4', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem4', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem4', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimItem4', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'locationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItem4', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem4', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'subSite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem4', 'encounter', 'Reference(Encounter)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem4', 'detail', 'ClaimItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItem4', nil, 'ClaimItem4', js.FHIRFactoryJs);
  defineClaimItemPropsJs(js, def);
end;


procedure defineClaimItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetail4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetail4', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail4', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetail4', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail4', 'subDetail', 'ClaimItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetail4', nil, 'ClaimItemDetail4', js.FHIRFactoryJs);
  defineClaimItemDetailPropsJs(js, def);
end;


procedure defineClaimItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetailSubDetail4', nil, 'ClaimItemDetailSubDetail4', js.FHIRFactoryJs);
  defineClaimItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Claim4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'billablePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Claim4', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'fundsReserve', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'related', 'ClaimRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'prescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'originalPrescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'payee', 'ClaimPayee', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'referral', 'Reference(ServiceRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'careTeam', 'ClaimCareTeam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'information', 'ClaimInformation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'diagnosis', 'ClaimDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'procedure', 'ClaimProcedure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'insurance', 'ClaimInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'accident', 'ClaimAccident', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'item', 'ClaimItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'total', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Claim4', nil, 'Claim4', js.FHIRFactoryJs);
  defineClaimPropsJs(js, def);
end;


procedure defineClaimResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItem4', 'itemSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItem4', 'adjudication', 'ClaimResponseItemAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItem4', 'detail', 'ClaimResponseItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItem4', nil, 'ClaimResponseItem4', js.FHIRFactoryJs);
  defineClaimResponseItemPropsJs(js, def);
end;


procedure defineClaimResponseItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemAdjudication4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication4', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication4', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication4', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineClaimResponseItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemAdjudication4', nil, 'ClaimResponseItemAdjudication4', js.FHIRFactoryJs);
  defineClaimResponseItemAdjudicationPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetail4', 'detailSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetail4', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItemDetail4', 'subDetail', 'ClaimResponseItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetail4', nil, 'ClaimResponseItemDetail4', js.FHIRFactoryJs);
  defineClaimResponseItemDetailPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail4', 'subDetailSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail4', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailSubDetail4', nil, 'ClaimResponseItemDetailSubDetail4', js.FHIRFactoryJs);
  defineClaimResponseItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimResponseAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItem4', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'fee', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItem4', nil, 'ClaimResponseAddItem4', js.FHIRFactoryJs);
  defineClaimResponseAddItemPropsJs(js, def);
end;


procedure defineClaimResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseError4', 'itemSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError4', 'detailSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError4', 'subDetailSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseError4', nil, 'ClaimResponseError4', js.FHIRFactoryJs);
  defineClaimResponseErrorPropsJs(js, def);
end;


procedure defineClaimResponseTotalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseTotal4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseTotal4', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseTotalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseTotal4', nil, 'ClaimResponseTotal4', js.FHIRFactoryJs);
  defineClaimResponseTotalPropsJs(js, def);
end;


procedure defineClaimResponsePaymentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponsePayment4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment4', 'adjustment', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment4', 'adjustmentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment4', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponsePayment4', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponsePaymentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponsePayment4', nil, 'ClaimResponsePayment4', js.FHIRFactoryJs);
  defineClaimResponsePaymentPropsJs(js, def);
end;


procedure defineClaimResponseProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseProcessNote4', 'number', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseProcessNote4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseProcessNote4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseProcessNote4', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseProcessNote4', nil, 'ClaimResponseProcessNote4', js.FHIRFactoryJs);
  defineClaimResponseProcessNotePropsJs(js, def);
end;


procedure defineClaimResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseInsurance4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseInsurance4', 'focal', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimResponseInsurance4', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseInsurance4', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseInsurance4', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseInsurance4', nil, 'ClaimResponseInsurance4', js.FHIRFactoryJs);
  defineClaimResponseInsurancePropsJs(js, def);
end;


procedure defineClaimResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClaimResponse4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponse4', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'request', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse4', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse4', 'payeeType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'item', 'ClaimResponseItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'addItem', 'ClaimResponseAddItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'error', 'ClaimResponseError', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'total', 'ClaimResponseTotal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'payment', 'ClaimResponsePayment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'reserved', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'processNote', 'ClaimResponseProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'communicationRequest', 'Reference(CommunicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'insurance', 'ClaimResponseInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponse4', nil, 'ClaimResponse4', js.FHIRFactoryJs);
  defineClaimResponsePropsJs(js, def);
end;


procedure defineClinicalImpressionInvestigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionInvestigation4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionInvestigation4', 'item', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClinicalImpressionInvestigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionInvestigation4', nil, 'ClinicalImpressionInvestigation4', js.FHIRFactoryJs);
  defineClinicalImpressionInvestigationPropsJs(js, def);
end;


procedure defineClinicalImpressionFindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionFinding4', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding4', 'itemReference', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding4', 'basis', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineClinicalImpressionFindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionFinding4', nil, 'ClinicalImpressionFinding4', js.FHIRFactoryJs);
  defineClinicalImpressionFindingPropsJs(js, def);
end;


procedure defineClinicalImpressionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClinicalImpression4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression4', 'assessor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'previous', 'Reference(ClinicalImpression)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'problem', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'investigation', 'ClinicalImpressionInvestigation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'summary', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression4', 'finding', 'ClinicalImpressionFinding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'prognosisCodeableConcept', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'prognosisReference', 'Reference(RiskAssessment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'action', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClinicalImpressionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpression4', nil, 'ClinicalImpression4', js.FHIRFactoryJs);
  defineClinicalImpressionPropsJs(js, def);
end;


procedure defineCodeSystemFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemFilter4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemFilter4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemFilter4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemFilter4', nil, 'CodeSystemFilter4', js.FHIRFactoryJs);
  defineCodeSystemFilterPropsJs(js, def);
end;


procedure defineCodeSystemPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemProperty4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty4', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemProperty4', nil, 'CodeSystemProperty4', js.FHIRFactoryJs);
  defineCodeSystemPropertyPropsJs(js, def);
end;


procedure defineCodeSystemConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConcept4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept4', 'definition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept4', 'designation', 'CodeSystemConceptDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystemConcept4', 'property', 'CodeSystemConceptProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystemConcept4', 'concept', '@CodeSystem.concept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCodeSystemConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConcept4', nil, 'CodeSystemConcept4', js.FHIRFactoryJs);
  defineCodeSystemConceptPropsJs(js, def);
end;


procedure defineCodeSystemConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConceptDesignation4', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptDesignation4', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CodeSystemConceptDesignation4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConceptDesignation4', nil, 'CodeSystemConceptDesignation4', js.FHIRFactoryJs);
  defineCodeSystemConceptDesignationPropsJs(js, def);
end;


procedure defineCodeSystemConceptPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineCodeSystemConceptPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConceptProperty4', nil, 'CodeSystemConceptProperty4', js.FHIRFactoryJs);
  defineCodeSystemConceptPropertyPropsJs(js, def);
end;


procedure defineCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CodeSystem4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CodeSystem4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CodeSystem4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'caseSensitive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem4', 'valueSet', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'hierarchyMeaning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'compositional', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem4', 'versionNeeded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem4', 'content', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'supplements', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'count', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'filter', 'CodeSystemFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'property', 'CodeSystemProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'concept', 'CodeSystemConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystem4', nil, 'CodeSystem4', js.FHIRFactoryJs);
  defineCodeSystemPropsJs(js, def);
end;


procedure defineCommunicationPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationPayload4', 'contentString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationPayload4', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationPayload4', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationPayload4', nil, 'CommunicationPayload4', js.FHIRFactoryJs);
  defineCommunicationPayloadPropsJs(js, def);
end;


procedure defineCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Communication4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'partOf', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'inResponseTo', 'Reference(Communication)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Communication4', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Communication4', 'medium', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication4', 'recipient', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'topic', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication4', 'about', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication4', 'sent', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication4', 'received', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication4', 'sender', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'payload', 'CommunicationPayload', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Communication4', nil, 'Communication4', js.FHIRFactoryJs);
  defineCommunicationPropsJs(js, def);
end;


procedure defineCommunicationRequestPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationRequestPayload4', 'contentString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequestPayload4', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequestPayload4', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationRequestPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequestPayload4', nil, 'CommunicationRequestPayload4', js.FHIRFactoryJs);
  defineCommunicationRequestPayloadPropsJs(js, def);
end;


procedure defineCommunicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CommunicationRequest4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'replaces', 'Reference(CommunicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest4', 'medium', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'recipient', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'about', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'payload', 'CommunicationRequestPayload', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest4', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest4', 'requester', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'sender', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCommunicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequest4', nil, 'CommunicationRequest4', js.FHIRFactoryJs);
  defineCommunicationRequestPropsJs(js, def);
end;


procedure defineCompartmentDefinitionResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompartmentDefinitionResource4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinitionResource4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCompartmentDefinitionResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompartmentDefinitionResource4', nil, 'CompartmentDefinitionResource4', js.FHIRFactoryJs);
  defineCompartmentDefinitionResourcePropsJs(js, def);
end;


procedure defineCompartmentDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CompartmentDefinition4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompartmentDefinition4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'search', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition4', 'resource', 'CompartmentDefinitionResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompartmentDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompartmentDefinition4', nil, 'CompartmentDefinition4', js.FHIRFactoryJs);
  defineCompartmentDefinitionPropsJs(js, def);
end;


procedure defineCompositionAttesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionAttester4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionAttester4', 'time', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompositionAttester4', 'party', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCompositionAttesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionAttester4', nil, 'CompositionAttester4', js.FHIRFactoryJs);
  defineCompositionAttesterPropsJs(js, def);
end;


procedure defineCompositionRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionRelatesTo4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionRelatesTo4', 'targetIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionRelatesTo4', 'targetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCompositionRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionRelatesTo4', nil, 'CompositionRelatesTo4', js.FHIRFactoryJs);
  defineCompositionRelatesToPropsJs(js, def);
end;


procedure defineCompositionEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionEvent4', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionEvent4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionEvent4', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionEvent4', nil, 'CompositionEvent4', js.FHIRFactoryJs);
  defineCompositionEventPropsJs(js, def);
end;


procedure defineCompositionSectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionSection4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionSection4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection4', 'author', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionSection4', 'text', 'Narrative', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionSection4', 'orderedBy', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection4', 'entry', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionSection4', 'emptyReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection4', 'section', '@Composition.section', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionSectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionSection4', nil, 'CompositionSection4', js.FHIRFactoryJs);
  defineCompositionSectionPropsJs(js, def);
end;


procedure defineCompositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Composition4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Composition4', 'author', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition4', 'confidentiality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition4', 'attester', 'CompositionAttester', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition4', 'custodian', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'relatesTo', 'CompositionRelatesTo', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition4', 'event', 'CompositionEvent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition4', 'section', 'CompositionSection', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Composition4', nil, 'Composition4', js.FHIRFactoryJs);
  defineCompositionPropsJs(js, def);
end;


procedure defineConceptMapGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroup4', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup4', 'sourceVersion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup4', 'target', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup4', 'targetVersion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup4', 'element', 'ConceptMapGroupElement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapGroup4', 'unmapped', 'ConceptMapGroupUnmapped', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConceptMapGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroup4', nil, 'ConceptMapGroup4', js.FHIRFactoryJs);
  defineConceptMapGroupPropsJs(js, def);
end;


procedure defineConceptMapGroupElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElement4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElement4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElement4', 'target', 'ConceptMapGroupElementTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapGroupElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElement4', nil, 'ConceptMapGroupElement4', js.FHIRFactoryJs);
  defineConceptMapGroupElementPropsJs(js, def);
end;


procedure defineConceptMapGroupElementTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'equivalence', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'dependsOn', 'ConceptMapGroupElementTargetDependsOn', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'product', '@ConceptMap.group.element.target.dependsOn', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapGroupElementTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElementTarget4', nil, 'ConceptMapGroupElementTarget4', js.FHIRFactoryJs);
  defineConceptMapGroupElementTargetPropsJs(js, def);
end;


procedure defineConceptMapGroupElementTargetDependsOnPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn4', 'property', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn4', 'system', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConceptMapGroupElementTargetDependsOnJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElementTargetDependsOn4', nil, 'ConceptMapGroupElementTargetDependsOn4', js.FHIRFactoryJs);
  defineConceptMapGroupElementTargetDependsOnPropsJs(js, def);
end;


procedure defineConceptMapGroupUnmappedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupUnmapped4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped4', 'url', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConceptMapGroupUnmappedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupUnmapped4', nil, 'ConceptMapGroupUnmapped4', js.FHIRFactoryJs);
  defineConceptMapGroupUnmappedPropsJs(js, def);
end;


procedure defineConceptMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ConceptMap4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConceptMap4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ConceptMap4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'sourceUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'sourceCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'targetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'targetCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'group', 'ConceptMapGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMap4', nil, 'ConceptMap4', js.FHIRFactoryJs);
  defineConceptMapPropsJs(js, def);
end;


procedure defineConditionStagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionStage4', 'summary', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConditionStage4', 'assessment', 'Reference(ClinicalImpression)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConditionStage4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConditionStageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionStage4', nil, 'ConditionStage4', js.FHIRFactoryJs);
  defineConditionStagePropsJs(js, def);
end;


procedure defineConditionEvidencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionEvidence4', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConditionEvidence4', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionEvidenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionEvidence4', nil, 'ConditionEvidence4', js.FHIRFactoryJs);
  defineConditionEvidencePropsJs(js, def);
end;


procedure defineConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Condition4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition4', 'clinicalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition4', 'verificationStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition4', 'severity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'onsetDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition4', 'onsetAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition4', 'abatementDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition4', 'abatementAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'abatementPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'abatementRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'abatementString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition4', 'assertedDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition4', 'recorder', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'asserter', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'stage', 'ConditionStage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition4', 'evidence', 'ConditionEvidence', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Condition4', nil, 'Condition4', js.FHIRFactoryJs);
  defineConditionPropsJs(js, def);
end;


procedure defineConsentPolicyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentPolicy4', 'authority', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentPolicy4', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConsentPolicyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentPolicy4', nil, 'ConsentPolicy4', js.FHIRFactoryJs);
  defineConsentPolicyPropsJs(js, def);
end;


procedure defineConsentVerificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentVerification4', 'verified', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConsentVerification4', 'verifiedWith', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentVerification4', 'verificationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineConsentVerificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentVerification4', nil, 'ConsentVerification4', js.FHIRFactoryJs);
  defineConsentVerificationPropsJs(js, def);
end;


procedure defineConsentProvisionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvision4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentProvision4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvision4', 'actor', 'ConsentProvisionActor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'action', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'purpose', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'class', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'dataPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvision4', 'data', 'ConsentProvisionData', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'provision', '@Consent.provision', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConsentProvisionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvision4', nil, 'ConsentProvision4', js.FHIRFactoryJs);
  defineConsentProvisionPropsJs(js, def);
end;


procedure defineConsentProvisionActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvisionActor4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvisionActor4', 'reference', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentProvisionActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvisionActor4', nil, 'ConsentProvisionActor4', js.FHIRFactoryJs);
  defineConsentProvisionActorPropsJs(js, def);
end;


procedure defineConsentProvisionDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvisionData4', 'meaning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentProvisionData4', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentProvisionDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvisionData4', nil, 'ConsentProvisionData4', js.FHIRFactoryJs);
  defineConsentProvisionDataPropsJs(js, def);
end;


procedure defineConsentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Consent4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Consent4', 'scope', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'dateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Consent4', 'performer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'organization', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'sourceAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'sourceIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'sourceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'policy', 'ConsentPolicy', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'policyRule', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'verification', 'ConsentVerification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'provision', 'ConsentProvision', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Consent4', nil, 'Consent4', js.FHIRFactoryJs);
  defineConsentPropsJs(js, def);
end;


procedure defineContractTermPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTerm4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTerm4', 'applies', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'subType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'offer', 'ContractTermOffer', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'asset', 'ContractTermAsset', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm4', 'agent', 'ContractTermAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm4', 'action', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm4', 'actionReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm4', 'group', '@Contract.term', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTerm4', nil, 'ContractTerm4', js.FHIRFactoryJs);
  defineContractTermPropsJs(js, def);
end;


procedure defineContractTermOfferPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermOffer4', 'topic', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer4', 'decision', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermOffer4', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineContractTermOfferJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermOffer4', nil, 'ContractTermOffer4', js.FHIRFactoryJs);
  defineContractTermOfferPropsJs(js, def);
end;


procedure defineContractTermAssetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAsset4', 'class', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset4', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset4', 'dataPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset4', 'data', 'ContractTermAssetData', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset4', 'valuedItem', 'ContractTermAssetValuedItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset4', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermAssetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAsset4', nil, 'ContractTermAsset4', js.FHIRFactoryJs);
  defineContractTermAssetPropsJs(js, def);
end;


procedure defineContractTermAssetDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAssetData4', 'meaning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermAssetData4', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermAssetDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAssetData4', nil, 'ContractTermAssetData4', js.FHIRFactoryJs);
  defineContractTermAssetDataPropsJs(js, def);
end;


procedure defineContractTermAssetValuedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'entityCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'entityReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'effectiveTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'points', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermAssetValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAssetValuedItem4', nil, 'ContractTermAssetValuedItem4', js.FHIRFactoryJs);
  defineContractTermAssetValuedItemPropsJs(js, def);
end;


procedure defineContractTermAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAgent4', 'actor', 'Reference(Contract)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAgent4', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAgent4', nil, 'ContractTermAgent4', js.FHIRFactoryJs);
  defineContractTermAgentPropsJs(js, def);
end;


procedure defineContractSignerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractSigner4', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner4', 'party', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner4', 'signature', 'Signature', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractSignerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractSigner4', nil, 'ContractSigner4', js.FHIRFactoryJs);
  defineContractSignerPropsJs(js, def);
end;


procedure defineContractFriendlyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractFriendly4', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractFriendly4', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractFriendlyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractFriendly4', nil, 'ContractFriendly4', js.FHIRFactoryJs);
  defineContractFriendlyPropsJs(js, def);
end;


procedure defineContractLegalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractLegal4', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractLegal4', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractLegalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractLegal4', nil, 'ContractLegal4', js.FHIRFactoryJs);
  defineContractLegalPropsJs(js, def);
end;


procedure defineContractRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractRule4', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractRule4', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractRule4', nil, 'ContractRule4', js.FHIRFactoryJs);
  defineContractRulePropsJs(js, def);
end;


procedure defineContractPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Contract4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract4', 'contentDerivative', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Contract4', 'applies', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'subject', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'authority', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'domain', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'term', 'ContractTerm', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'signer', 'ContractSigner', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'friendly', 'ContractFriendly', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'legal', 'ContractLegal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'rule', 'ContractRule', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'legallyBindingAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'legallyBindingReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contract4', nil, 'Contract4', js.FHIRFactoryJs);
  defineContractPropsJs(js, def);
end;


procedure defineCoverageClassPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageClass4', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageClass4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageClass4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCoverageClassJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageClass4', nil, 'CoverageClass4', js.FHIRFactoryJs);
  defineCoverageClassPropsJs(js, def);
end;


procedure defineCoverageCopayPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageCopay4', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageCopay4', 'value', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCoverageCopayJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageCopay4', nil, 'CoverageCopay4', js.FHIRFactoryJs);
  defineCoverageCopayPropsJs(js, def);
end;


procedure defineCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Coverage4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'policyHolder', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'subscriber', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'subscriberId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage4', 'beneficiary', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'dependent', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage4', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'payor', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage4', 'class', 'CoverageClass', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage4', 'order', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Coverage4', 'network', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage4', 'copay', 'CoverageCopay', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage4', 'contract', 'Reference(Contract)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coverage4', nil, 'Coverage4', js.FHIRFactoryJs);
  defineCoveragePropsJs(js, def);
end;


procedure defineDetectedIssueMitigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DetectedIssueMitigation4', 'action', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssueMitigation4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssueMitigation4', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDetectedIssueMitigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssueMitigation4', nil, 'DetectedIssueMitigation4', js.FHIRFactoryJs);
  defineDetectedIssueMitigationPropsJs(js, def);
end;


procedure defineDetectedIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DetectedIssue4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue4', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssue4', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue4', 'implicated', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue4', 'detail', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue4', 'reference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue4', 'mitigation', 'DetectedIssueMitigation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDetectedIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssue4', nil, 'DetectedIssue4', js.FHIRFactoryJs);
  defineDetectedIssuePropsJs(js, def);
end;


procedure defineDeviceUdiPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceUdi4', 'deviceIdentifier', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi4', 'jurisdiction', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi4', 'carrierHRF', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi4', 'carrierAIDC', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'DeviceUdi4', 'issuer', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi4', 'entryType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceUdiJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUdi4', nil, 'DeviceUdi4', js.FHIRFactoryJs);
  defineDeviceUdiPropsJs(js, def);
end;


procedure defineDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Device4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'udi', 'DeviceUdi', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device4', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'manufacturer', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'manufactureDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device4', 'expirationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device4', 'model', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device4', 'owner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device4', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'safety', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Device4', nil, 'Device4', js.FHIRFactoryJs);
  defineDevicePropsJs(js, def);
end;


procedure defineDeviceComponentProductionSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceComponentProductionSpecification4', 'specType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProductionSpecification4', 'componentId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProductionSpecification4', 'productionSpec', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceComponentProductionSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponentProductionSpecification4', nil, 'DeviceComponentProductionSpecification4', js.FHIRFactoryJs);
  defineDeviceComponentProductionSpecificationPropsJs(js, def);
end;


procedure defineDeviceComponentPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceComponentProperty4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProperty4', 'valueQuantity', 'Quantity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponentProperty4', 'valueCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceComponentPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponentProperty4', nil, 'DeviceComponentProperty4', js.FHIRFactoryJs);
  defineDeviceComponentPropertyPropsJs(js, def);
end;


procedure defineDeviceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceComponent4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent4', 'lastSystemChange', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceComponent4', 'source', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent4', 'parent', 'Reference(DeviceComponent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent4', 'operationalStatus', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent4', 'parameterGroup', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent4', 'measurementPrinciple', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceComponent4', 'productionSpecification', 'DeviceComponentProductionSpecification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent4', 'languageCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent4', 'property', 'DeviceComponentProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponent4', nil, 'DeviceComponent4', js.FHIRFactoryJs);
  defineDeviceComponentPropsJs(js, def);
end;


procedure defineDeviceMetricCalibrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceMetricCalibration4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration4', 'state', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration4', 'time', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineDeviceMetricCalibrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetricCalibration4', nil, 'DeviceMetricCalibration4', js.FHIRFactoryJs);
  defineDeviceMetricCalibrationPropsJs(js, def);
end;


procedure defineDeviceMetricPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceMetric4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceMetric4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric4', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric4', 'source', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric4', 'parent', 'Reference(DeviceComponent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric4', 'operationalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric4', 'color', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric4', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric4', 'measurementPeriod', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric4', 'calibration', 'DeviceMetricCalibration', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceMetricJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetric4', nil, 'DeviceMetric4', js.FHIRFactoryJs);
  defineDeviceMetricPropsJs(js, def);
end;


procedure defineDeviceRequestParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceRequestParameter4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineDeviceRequestParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceRequestParameter4', nil, 'DeviceRequestParameter4', js.FHIRFactoryJs);
  defineDeviceRequestParameterPropsJs(js, def);
end;


procedure defineDeviceRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceRequest4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'priorRequest', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest4', 'intent', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest4', 'codeReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'codeCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'parameter', 'DeviceRequestParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest4', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest4', 'requester', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'performer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'insurance', 'Reference(Coverage)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceRequest4', nil, 'DeviceRequest4', js.FHIRFactoryJs);
  defineDeviceRequestPropsJs(js, def);
end;


procedure defineDeviceUseStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceUseStatement4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement4', 'basedOn', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUseStatement4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'derivedFrom', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement4', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement4', 'recordedOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement4', 'source', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement4', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceUseStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUseStatement4', nil, 'DeviceUseStatement4', js.FHIRFactoryJs);
  defineDeviceUseStatementPropsJs(js, def);
end;


procedure defineDiagnosticReportMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticReportMedia4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReportMedia4', 'link', 'Reference(Media)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDiagnosticReportMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReportMedia4', nil, 'DiagnosticReportMedia4', js.FHIRFactoryJs);
  defineDiagnosticReportMediaPropsJs(js, def);
end;


procedure defineDiagnosticReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DiagnosticReport4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport4', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport4', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport4', 'performer', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'resultsInterpreter', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'result', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'imagingStudy', 'Reference(ImagingStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'media', 'DiagnosticReportMedia', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'conclusion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport4', 'codedDiagnosis', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'presentedForm', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDiagnosticReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReport4', nil, 'DiagnosticReport4', js.FHIRFactoryJs);
  defineDiagnosticReportPropsJs(js, def);
end;


procedure defineDocumentManifestAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestAgent4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestAgent4', 'who', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentManifestAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestAgent4', nil, 'DocumentManifestAgent4', js.FHIRFactoryJs);
  defineDocumentManifestAgentPropsJs(js, def);
end;


procedure defineDocumentManifestRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestRelated4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestRelated4', 'ref', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentManifestRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestRelated4', nil, 'DocumentManifestRelated4', js.FHIRFactoryJs);
  defineDocumentManifestRelatedPropsJs(js, def);
end;


procedure defineDocumentManifestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentManifest4', 'masterIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentManifest4', 'agent', 'DocumentManifestAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest4', 'recipient', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest4', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest4', 'content', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest4', 'related', 'DocumentManifestRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifest4', nil, 'DocumentManifest4', js.FHIRFactoryJs);
  defineDocumentManifestPropsJs(js, def);
end;


procedure defineDocumentReferenceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceAgent4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceAgent4', 'who', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceAgent4', nil, 'DocumentReferenceAgent4', js.FHIRFactoryJs);
  defineDocumentReferenceAgentPropsJs(js, def);
end;


procedure defineDocumentReferenceRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceRelatesTo4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReferenceRelatesTo4', 'target', 'Reference(DocumentReference)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceRelatesTo4', nil, 'DocumentReferenceRelatesTo4', js.FHIRFactoryJs);
  defineDocumentReferenceRelatesToPropsJs(js, def);
end;


procedure defineDocumentReferenceContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContent4', 'attachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContent4', 'format', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContent4', nil, 'DocumentReferenceContent4', js.FHIRFactoryJs);
  defineDocumentReferenceContentPropsJs(js, def);
end;


procedure defineDocumentReferenceContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContext4', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'event', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'facilityType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'practiceSetting', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'sourcePatientInfo', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'related', 'DocumentReferenceContextRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentReferenceContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContext4', nil, 'DocumentReferenceContext4', js.FHIRFactoryJs);
  defineDocumentReferenceContextPropsJs(js, def);
end;


procedure defineDocumentReferenceContextRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContextRelated4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContextRelated4', 'ref', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceContextRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContextRelated4', nil, 'DocumentReferenceContextRelated4', js.FHIRFactoryJs);
  defineDocumentReferenceContextRelatedPropsJs(js, def);
end;


procedure defineDocumentReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentReference4', 'masterIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference4', 'docStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference4', 'date', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference4', 'agent', 'DocumentReferenceAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'authenticator', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'custodian', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'relatesTo', 'DocumentReferenceRelatesTo', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference4', 'securityLabel', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'content', 'DocumentReferenceContent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'context', 'DocumentReferenceContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReference4', nil, 'DocumentReference4', js.FHIRFactoryJs);
  defineDocumentReferencePropsJs(js, def);
end;


procedure defineEligibilityRequestAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityRequestAuthorization4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'EligibilityRequestAuthorization4', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequestAuthorization4', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityRequestAuthorization4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequestAuthorization4', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequestAuthorization4', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequestAuthorization4', 'diagnosis', 'EligibilityRequestAuthorizationDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityRequestAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityRequestAuthorization4', nil, 'EligibilityRequestAuthorization4', js.FHIRFactoryJs);
  defineEligibilityRequestAuthorizationPropsJs(js, def);
end;


procedure defineEligibilityRequestAuthorizationDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityRequestAuthorizationDiagnosis4', 'diagnosisCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequestAuthorizationDiagnosis4', 'diagnosisReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityRequestAuthorizationDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityRequestAuthorizationDiagnosis4', nil, 'EligibilityRequestAuthorizationDiagnosis4', js.FHIRFactoryJs);
  defineEligibilityRequestAuthorizationDiagnosisPropsJs(js, def);
end;


procedure defineEligibilityRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityRequest4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityRequest4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityRequest4', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest4', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityRequest4', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityRequest4', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest4', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest4', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest4', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest4', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest4', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityRequest4', 'benefitCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest4', 'benefitSubCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest4', 'authorization', 'EligibilityRequestAuthorization', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityRequest4', nil, 'EligibilityRequest4', js.FHIRFactoryJs);
  defineEligibilityRequestPropsJs(js, def);
end;


procedure defineEligibilityResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsurance4', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsurance4', 'contract', 'Reference(Contract)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsurance4', 'benefitBalance', 'EligibilityResponseInsuranceBenefitBalance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsurance4', nil, 'EligibilityResponseInsurance4', js.FHIRFactoryJs);
  defineEligibilityResponseInsurancePropsJs(js, def);
end;


procedure defineEligibilityResponseInsuranceBenefitBalancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance4', 'subCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance4', 'excluded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance4', 'network', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance4', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance4', 'term', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance4', 'financial', 'EligibilityResponseInsuranceBenefitBalanceFinancial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseInsuranceBenefitBalanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsuranceBenefitBalance4', nil, 'EligibilityResponseInsuranceBenefitBalance4', js.FHIRFactoryJs);
  defineEligibilityResponseInsuranceBenefitBalancePropsJs(js, def);
end;


procedure defineEligibilityResponseInsuranceBenefitBalanceFinancialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial4', 'allowedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial4', 'allowedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial4', 'allowedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial4', 'usedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial4', 'usedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityResponseInsuranceBenefitBalanceFinancialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsuranceBenefitBalanceFinancial4', nil, 'EligibilityResponseInsuranceBenefitBalanceFinancial4', js.FHIRFactoryJs);
  defineEligibilityResponseInsuranceBenefitBalanceFinancialPropsJs(js, def);
end;


procedure defineEligibilityResponseAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseAuthorization4', 'authorizationSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'EligibilityResponseAuthorization4', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponseAuthorization4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseAuthorization4', nil, 'EligibilityResponseAuthorization4', js.FHIRFactoryJs);
  defineEligibilityResponseAuthorizationPropsJs(js, def);
end;


procedure defineEligibilityResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseError4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseError4', nil, 'EligibilityResponseError4', js.FHIRFactoryJs);
  defineEligibilityResponseErrorPropsJs(js, def);
end;


procedure defineEligibilityResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityResponse4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityResponse4', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse4', 'request', 'Reference(EligibilityRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse4', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse4', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse4', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse4', 'inforce', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponse4', 'insurance', 'EligibilityResponseInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse4', 'preAuthRef', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse4', 'authorization', 'EligibilityResponseAuthorization', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse4', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse4', 'error', 'EligibilityResponseError', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponse4', nil, 'EligibilityResponse4', js.FHIRFactoryJs);
  defineEligibilityResponsePropsJs(js, def);
end;


procedure defineEncounterStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterStatusHistory4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EncounterStatusHistory4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterStatusHistory4', nil, 'EncounterStatusHistory4', js.FHIRFactoryJs);
  defineEncounterStatusHistoryPropsJs(js, def);
end;


procedure defineEncounterClassHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterClassHistory4', 'class', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterClassHistory4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterClassHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterClassHistory4', nil, 'EncounterClassHistory4', js.FHIRFactoryJs);
  defineEncounterClassHistoryPropsJs(js, def);
end;


procedure defineEncounterParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterParticipant4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterParticipant4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterParticipant4', 'individual', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterParticipant4', nil, 'EncounterParticipant4', js.FHIRFactoryJs);
  defineEncounterParticipantPropsJs(js, def);
end;


procedure defineEncounterDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterDiagnosis4', 'condition', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis4', 'rank', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineEncounterDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterDiagnosis4', nil, 'EncounterDiagnosis4', js.FHIRFactoryJs);
  defineEncounterDiagnosisPropsJs(js, def);
end;


procedure defineEncounterHospitalizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterHospitalization4', 'preAdmissionIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization4', 'origin', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization4', 'admitSource', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization4', 'reAdmission', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization4', 'dietPreference', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization4', 'specialCourtesy', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization4', 'specialArrangement', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization4', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization4', 'dischargeDisposition', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterHospitalizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterHospitalization4', nil, 'EncounterHospitalization4', js.FHIRFactoryJs);
  defineEncounterHospitalizationPropsJs(js, def);
end;


procedure defineEncounterLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterLocation4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterLocation4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EncounterLocation4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterLocation4', nil, 'EncounterLocation4', js.FHIRFactoryJs);
  defineEncounterLocationPropsJs(js, def);
end;


procedure defineEncounterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Encounter4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Encounter4', 'statusHistory', 'EncounterStatusHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'class', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'classHistory', 'EncounterClassHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'serviceType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'episodeOfCare', 'Reference(EpisodeOfCare)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'basedOn', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'participant', 'EncounterParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'appointment', 'Reference(Appointment)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'length', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'diagnosis', 'EncounterDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'account', 'Reference(Account)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'hospitalization', 'EncounterHospitalization', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'location', 'EncounterLocation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'serviceProvider', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'partOf', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Encounter4', nil, 'Encounter4', js.FHIRFactoryJs);
  defineEncounterPropsJs(js, def);
end;


procedure defineEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Endpoint4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Endpoint4', 'connectionType', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Endpoint4', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint4', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint4', 'payloadType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint4', 'address', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Endpoint4', nil, 'Endpoint4', js.FHIRFactoryJs);
  defineEndpointPropsJs(js, def);
end;


procedure defineEnrollmentRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentRequest4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentRequest4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentRequest4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentRequest4', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest4', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest4', 'candidate', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest4', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentRequest4', nil, 'EnrollmentRequest4', js.FHIRFactoryJs);
  defineEnrollmentRequestPropsJs(js, def);
end;


procedure defineEnrollmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentResponse4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentResponse4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse4', 'request', 'Reference(EnrollmentRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse4', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse4', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentResponse4', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse4', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentResponse4', nil, 'EnrollmentResponse4', js.FHIRFactoryJs);
  defineEnrollmentResponsePropsJs(js, def);
end;


procedure defineEntryDefinitionRelatedEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EntryDefinitionRelatedEntry4', 'relationtype', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinitionRelatedEntry4', 'item', 'Reference(EntryDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEntryDefinitionRelatedEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EntryDefinitionRelatedEntry4', nil, 'EntryDefinitionRelatedEntry4', js.FHIRFactoryJs);
  defineEntryDefinitionRelatedEntryPropsJs(js, def);
end;


procedure defineEntryDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EntryDefinition4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition4', 'purpose', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition4', 'referencedItem', 'Reference(Medication)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition4', 'additionalIdentifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition4', 'classification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition4', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition4', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition4', 'lastUpdated', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EntryDefinition4', 'additionalCharacteristic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition4', 'additionalClassification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition4', 'relatedEntry', 'EntryDefinitionRelatedEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEntryDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EntryDefinition4', nil, 'EntryDefinition4', js.FHIRFactoryJs);
  defineEntryDefinitionPropsJs(js, def);
end;


procedure defineEpisodeOfCareStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareStatusHistory4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCareStatusHistory4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEpisodeOfCareStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareStatusHistory4', nil, 'EpisodeOfCareStatusHistory4', js.FHIRFactoryJs);
  defineEpisodeOfCareStatusHistoryPropsJs(js, def);
end;


procedure defineEpisodeOfCareDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareDiagnosis4', 'condition', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareDiagnosis4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareDiagnosis4', 'rank', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineEpisodeOfCareDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareDiagnosis4', nil, 'EpisodeOfCareDiagnosis4', js.FHIRFactoryJs);
  defineEpisodeOfCareDiagnosisPropsJs(js, def);
end;


procedure defineEpisodeOfCarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCare4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCare4', 'statusHistory', 'EpisodeOfCareStatusHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'diagnosis', 'EpisodeOfCareDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare4', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare4', 'referralRequest', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'careManager', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare4', 'team', 'Reference(CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'account', 'Reference(Account)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEpisodeOfCareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCare4', nil, 'EpisodeOfCare4', js.FHIRFactoryJs);
  defineEpisodeOfCarePropsJs(js, def);
end;


procedure defineEventDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'EventDefinition4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EventDefinition4', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition4', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition4', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition4', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'trigger', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEventDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EventDefinition4', nil, 'EventDefinition4', js.FHIRFactoryJs);
  defineEventDefinitionPropsJs(js, def);
end;


procedure defineExampleScenarioActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioActor4', 'actorId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExampleScenarioActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioActor4', nil, 'ExampleScenarioActor4', js.FHIRFactoryJs);
  defineExampleScenarioActorPropsJs(js, def);
end;


procedure defineExampleScenarioInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstance4', 'resourceId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance4', 'resourceType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance4', 'version', 'ExampleScenarioInstanceVersion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenarioInstance4', 'containedInstance', 'ExampleScenarioInstanceContainedInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstance4', nil, 'ExampleScenarioInstance4', js.FHIRFactoryJs);
  defineExampleScenarioInstancePropsJs(js, def);
end;


procedure defineExampleScenarioInstanceVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstanceVersion4', 'versionId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstanceVersion4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExampleScenarioInstanceVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstanceVersion4', nil, 'ExampleScenarioInstanceVersion4', js.FHIRFactoryJs);
  defineExampleScenarioInstanceVersionPropsJs(js, def);
end;


procedure defineExampleScenarioInstanceContainedInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstanceContainedInstance4', 'resourceId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstanceContainedInstance4', 'versionId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExampleScenarioInstanceContainedInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstanceContainedInstance4', nil, 'ExampleScenarioInstanceContainedInstance4', js.FHIRFactoryJs);
  defineExampleScenarioInstanceContainedInstancePropsJs(js, def);
end;


procedure defineExampleScenarioProcessPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcess4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess4', 'preConditions', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess4', 'postConditions', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess4', 'step', 'ExampleScenarioProcessStep', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcess4', nil, 'ExampleScenarioProcess4', js.FHIRFactoryJs);
  defineExampleScenarioProcessPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStep4', 'process', '@ExampleScenario.process', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenarioProcessStep4', 'pause', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStep4', 'operation', 'ExampleScenarioProcessStepOperation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExampleScenarioProcessStep4', 'alternative', 'ExampleScenarioProcessStepAlternative', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExampleScenarioProcessStepJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStep4', nil, 'ExampleScenarioProcessStep4', js.FHIRFactoryJs);
  defineExampleScenarioProcessStepPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'number', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'initiator', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'receiver', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'initiatorActive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'receiverActive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'request', '@ExampleScenario.instance.containedInstance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'response', '@ExampleScenario.instance.containedInstance', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExampleScenarioProcessStepOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepOperation4', nil, 'ExampleScenarioProcessStepOperation4', js.FHIRFactoryJs);
  defineExampleScenarioProcessStepOperationPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepAlternativePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative4', 'option', 'ExampleScenarioProcessStepAlternativeOption', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessStepAlternativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepAlternative4', nil, 'ExampleScenarioProcessStepAlternative4', js.FHIRFactoryJs);
  defineExampleScenarioProcessStepAlternativePropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepAlternativeOptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternativeOption4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternativeOption4', 'step', '@ExampleScenario.process.step', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessStepAlternativeOptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepAlternativeOption4', nil, 'ExampleScenarioProcessStepAlternativeOption4', js.FHIRFactoryJs);
  defineExampleScenarioProcessStepAlternativeOptionPropsJs(js, def);
end;


procedure defineExampleScenarioPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ExampleScenario4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenario4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExampleScenario4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'actor', 'ExampleScenarioActor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'instance', 'ExampleScenarioInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'process', 'ExampleScenarioProcess', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenario4', nil, 'ExampleScenario4', js.FHIRFactoryJs);
  defineExampleScenarioPropsJs(js, def);
end;


procedure defineExpansionProfileFixedVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileFixedVersion4', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileFixedVersion4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileFixedVersion4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExpansionProfileFixedVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileFixedVersion4', nil, 'ExpansionProfileFixedVersion4', js.FHIRFactoryJs);
  defineExpansionProfileFixedVersionPropsJs(js, def);
end;


procedure defineExpansionProfileExcludedSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileExcludedSystem4', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileExcludedSystem4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExpansionProfileExcludedSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileExcludedSystem4', nil, 'ExpansionProfileExcludedSystem4', js.FHIRFactoryJs);
  defineExpansionProfileExcludedSystemPropsJs(js, def);
end;


procedure defineExpansionProfileDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignation4', 'include', 'ExpansionProfileDesignationInclude', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfileDesignation4', 'exclude', 'ExpansionProfileDesignationExclude', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignation4', nil, 'ExpansionProfileDesignation4', js.FHIRFactoryJs);
  defineExpansionProfileDesignationPropsJs(js, def);
end;


procedure defineExpansionProfileDesignationIncludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationInclude4', 'designation', 'ExpansionProfileDesignationIncludeDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExpansionProfileDesignationIncludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationInclude4', nil, 'ExpansionProfileDesignationInclude4', js.FHIRFactoryJs);
  defineExpansionProfileDesignationIncludePropsJs(js, def);
end;


procedure defineExpansionProfileDesignationIncludeDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationIncludeDesignation4', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileDesignationIncludeDesignation4', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationIncludeDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationIncludeDesignation4', nil, 'ExpansionProfileDesignationIncludeDesignation4', js.FHIRFactoryJs);
  defineExpansionProfileDesignationIncludeDesignationPropsJs(js, def);
end;


procedure defineExpansionProfileDesignationExcludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationExclude4', 'designation', 'ExpansionProfileDesignationExcludeDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExpansionProfileDesignationExcludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationExclude4', nil, 'ExpansionProfileDesignationExclude4', js.FHIRFactoryJs);
  defineExpansionProfileDesignationExcludePropsJs(js, def);
end;


procedure defineExpansionProfileDesignationExcludeDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationExcludeDesignation4', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileDesignationExcludeDesignation4', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationExcludeDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationExcludeDesignation4', nil, 'ExpansionProfileDesignationExcludeDesignation4', js.FHIRFactoryJs);
  defineExpansionProfileDesignationExcludeDesignationPropsJs(js, def);
end;


procedure defineExpansionProfilePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ExpansionProfile4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExpansionProfile4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile4', 'fixedVersion', 'ExpansionProfileFixedVersion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile4', 'excludedSystem', 'ExpansionProfileExcludedSystem', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile4', 'includeDesignations', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile4', 'designation', 'ExpansionProfileDesignation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile4', 'includeDefinition', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile4', 'activeOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile4', 'excludeNested', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile4', 'excludeNotForUI', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile4', 'excludePostCoordinated', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile4', 'displayLanguage', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile4', 'limitedExpansion', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineExpansionProfileJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfile4', nil, 'ExpansionProfile4', js.FHIRFactoryJs);
  defineExpansionProfilePropsJs(js, def);
end;


procedure defineExplanationOfBenefitRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitRelated4', 'claim', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitRelated4', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitRelated4', 'reference', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitRelated4', nil, 'ExplanationOfBenefitRelated4', js.FHIRFactoryJs);
  defineExplanationOfBenefitRelatedPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitPayee4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayee4', 'resource', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayee4', 'party', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayee4', nil, 'ExplanationOfBenefitPayee4', js.FHIRFactoryJs);
  defineExplanationOfBenefitPayeePropsJs(js, def);
end;


procedure defineExplanationOfBenefitInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'timingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation4', 'reason', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitInformation4', nil, 'ExplanationOfBenefitInformation4', js.FHIRFactoryJs);
  defineExplanationOfBenefitInformationPropsJs(js, def);
end;


procedure defineExplanationOfBenefitCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam4', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam4', 'responsible', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam4', 'qualification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitCareTeam4', nil, 'ExplanationOfBenefitCareTeam4', js.FHIRFactoryJs);
  defineExplanationOfBenefitCareTeamPropsJs(js, def);
end;


procedure defineExplanationOfBenefitDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'diagnosisCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'diagnosisReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'packageCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitDiagnosis4', nil, 'ExplanationOfBenefitDiagnosis4', js.FHIRFactoryJs);
  defineExplanationOfBenefitDiagnosisPropsJs(js, def);
end;


procedure defineExplanationOfBenefitProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitProcedure4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure4', 'procedureCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure4', 'procedureReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitProcedure4', nil, 'ExplanationOfBenefitProcedure4', js.FHIRFactoryJs);
  defineExplanationOfBenefitProcedurePropsJs(js, def);
end;


procedure defineExplanationOfBenefitInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitInsurance4', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitInsurance4', nil, 'ExplanationOfBenefitInsurance4', js.FHIRFactoryJs);
  defineExplanationOfBenefitInsurancePropsJs(js, def);
end;


procedure defineExplanationOfBenefitAccidentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAccident4', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident4', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident4', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitAccidentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAccident4', nil, 'ExplanationOfBenefitAccident4', js.FHIRFactoryJs);
  defineExplanationOfBenefitAccidentPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'locationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'subSite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'encounter', 'Reference(Encounter)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'adjudication', 'ExplanationOfBenefitItemAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'detail', 'ExplanationOfBenefitItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItem4', nil, 'ExplanationOfBenefitItem4', js.FHIRFactoryJs);
  defineExplanationOfBenefitItemPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication4', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication4', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication4', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineExplanationOfBenefitItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemAdjudication4', nil, 'ExplanationOfBenefitItemAdjudication4', js.FHIRFactoryJs);
  defineExplanationOfBenefitItemAdjudicationPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'subDetail', 'ExplanationOfBenefitItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemDetail4', nil, 'ExplanationOfBenefitItemDetail4', js.FHIRFactoryJs);
  defineExplanationOfBenefitItemDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemDetailSubDetail4', nil, 'ExplanationOfBenefitItemDetailSubDetail4', js.FHIRFactoryJs);
  defineExplanationOfBenefitItemDetailSubDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'fee', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItem4', nil, 'ExplanationOfBenefitAddItem4', js.FHIRFactoryJs);
  defineExplanationOfBenefitAddItemPropsJs(js, def);
end;


procedure defineExplanationOfBenefitTotalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitTotal4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitTotal4', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitTotalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitTotal4', nil, 'ExplanationOfBenefitTotal4', js.FHIRFactoryJs);
  defineExplanationOfBenefitTotalPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPaymentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'adjustment', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'adjustmentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPaymentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayment4', nil, 'ExplanationOfBenefitPayment4', js.FHIRFactoryJs);
  defineExplanationOfBenefitPaymentPropsJs(js, def);
end;


procedure defineExplanationOfBenefitProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote4', 'number', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote4', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitProcessNote4', nil, 'ExplanationOfBenefitProcessNote4', js.FHIRFactoryJs);
  defineExplanationOfBenefitProcessNotePropsJs(js, def);
end;


procedure defineExplanationOfBenefitBenefitBalancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'subCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'excluded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'network', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'term', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'financial', 'ExplanationOfBenefitBenefitBalanceFinancial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitBenefitBalanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitBenefitBalance4', nil, 'ExplanationOfBenefitBenefitBalance4', js.FHIRFactoryJs);
  defineExplanationOfBenefitBenefitBalancePropsJs(js, def);
end;


procedure defineExplanationOfBenefitBenefitBalanceFinancialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'allowedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'allowedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'allowedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'usedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'usedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitBenefitBalanceFinancialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitBenefitBalanceFinancial4', nil, 'ExplanationOfBenefitBenefitBalanceFinancial4', js.FHIRFactoryJs);
  defineExplanationOfBenefitBenefitBalanceFinancialPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefit4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'billablePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'referral', 'Reference(ServiceRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'claim', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'related', 'ExplanationOfBenefitRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'prescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'originalPrescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'payee', 'ExplanationOfBenefitPayee', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'information', 'ExplanationOfBenefitInformation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'careTeam', 'ExplanationOfBenefitCareTeam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'diagnosis', 'ExplanationOfBenefitDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'procedure', 'ExplanationOfBenefitProcedure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'precedence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'insurance', 'ExplanationOfBenefitInsurance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'accident', 'ExplanationOfBenefitAccident', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'item', 'ExplanationOfBenefitItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'addItem', 'ExplanationOfBenefitAddItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'total', 'ExplanationOfBenefitTotal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'payment', 'ExplanationOfBenefitPayment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'processNote', 'ExplanationOfBenefitProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'benefitBalance', 'ExplanationOfBenefitBenefitBalance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefit4', nil, 'ExplanationOfBenefit4', js.FHIRFactoryJs);
  defineExplanationOfBenefitPropsJs(js, def);
end;


procedure defineFamilyMemberHistoryConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'onsetAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistoryCondition4', nil, 'FamilyMemberHistoryCondition4', js.FHIRFactoryJs);
  defineFamilyMemberHistoryConditionPropsJs(js, def);
end;


procedure defineFamilyMemberHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistory4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'dataAbsentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'bornPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'bornDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'bornString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'ageAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'ageRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'ageString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'estimatedAge', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'deceasedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'deceasedAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'deceasedRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'deceasedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'deceasedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'condition', 'FamilyMemberHistoryCondition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistory4', nil, 'FamilyMemberHistory4', js.FHIRFactoryJs);
  defineFamilyMemberHistoryPropsJs(js, def);
end;


procedure defineFlagPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Flag4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Flag4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Flag4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Flag4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag4', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag4', 'author', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineFlagJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Flag4', nil, 'Flag4', js.FHIRFactoryJs);
  defineFlagPropsJs(js, def);
end;


procedure defineGoalTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GoalTarget4', 'measure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget4', 'detailQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget4', 'detailRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget4', 'detailCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget4', 'dueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GoalTarget4', 'dueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineGoalTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GoalTarget4', nil, 'GoalTarget4', js.FHIRFactoryJs);
  defineGoalTargetPropsJs(js, def);
end;


procedure defineGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Goal4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'description', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'startDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal4', 'startCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'target', 'GoalTarget', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'statusDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal4', 'statusReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal4', 'expressedBy', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'addresses', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'outcomeCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'outcomeReference', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Goal4', nil, 'Goal4', js.FHIRFactoryJs);
  defineGoalPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLink4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink4', 'sliceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink4', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'GraphDefinitionLink4', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink4', 'target', 'GraphDefinitionLinkTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLink4', nil, 'GraphDefinitionLink4', js.FHIRFactoryJs);
  defineGraphDefinitionLinkPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLinkTarget4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget4', 'params', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget4', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget4', 'compartment', 'GraphDefinitionLinkTargetCompartment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget4', 'link', '@GraphDefinition.link', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionLinkTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLinkTarget4', nil, 'GraphDefinitionLinkTarget4', js.FHIRFactoryJs);
  defineGraphDefinitionLinkTargetPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkTargetCompartmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment4', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment4', 'rule', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineGraphDefinitionLinkTargetCompartmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLinkTargetCompartment4', nil, 'GraphDefinitionLinkTargetCompartment4', js.FHIRFactoryJs);
  defineGraphDefinitionLinkTargetCompartmentPropsJs(js, def);
end;


procedure defineGraphDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'GraphDefinition4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GraphDefinition4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GraphDefinition4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'start', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'link', 'GraphDefinitionLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinition4', nil, 'GraphDefinition4', js.FHIRFactoryJs);
  defineGraphDefinitionPropsJs(js, def);
end;


procedure defineGroupCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupCharacteristic4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic4', 'exclude', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineGroupCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupCharacteristic4', nil, 'GroupCharacteristic4', js.FHIRFactoryJs);
  defineGroupCharacteristicPropsJs(js, def);
end;


procedure defineGroupMemberPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupMember4', 'entity', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupMember4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupMember4', 'inactive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineGroupMemberJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupMember4', nil, 'GroupMember4', js.FHIRFactoryJs);
  defineGroupMemberPropsJs(js, def);
end;


procedure defineGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Group4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Group4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Group4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group4', 'actual', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Group4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Group4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group4', 'quantity', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group4', 'characteristic', 'GroupCharacteristic', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Group4', 'member', 'GroupMember', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Group4', nil, 'Group4', js.FHIRFactoryJs);
  defineGroupPropsJs(js, def);
end;


procedure defineGuidanceResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'GuidanceResponse4', 'requestId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse4', 'moduleUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse4', 'moduleCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse4', 'moduleCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GuidanceResponse4', 'performer', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse4', 'evaluationMessage', 'Reference(OperationOutcome)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse4', 'outputParameters', 'Reference(Parameters)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'result', 'Reference(CarePlan)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'dataRequirement', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGuidanceResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GuidanceResponse4', nil, 'GuidanceResponse4', js.FHIRFactoryJs);
  defineGuidanceResponsePropsJs(js, def);
end;


procedure defineHealthcareServiceAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceAvailableTime4', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime4', 'availableStartTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime4', 'availableEndTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineHealthcareServiceAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceAvailableTime4', nil, 'HealthcareServiceAvailableTime4', js.FHIRFactoryJs);
  defineHealthcareServiceAvailableTimePropsJs(js, def);
end;


procedure defineHealthcareServiceNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceNotAvailable4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceNotAvailable4', 'during', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineHealthcareServiceNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceNotAvailable4', nil, 'HealthcareServiceNotAvailable4', js.FHIRFactoryJs);
  defineHealthcareServiceNotAvailablePropsJs(js, def);
end;


procedure defineHealthcareServicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'HealthcareService4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService4', 'providedBy', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService4', 'extraDetails', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService4', 'photo', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'coverageArea', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'serviceProvisionCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'eligibility', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService4', 'eligibilityNote', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService4', 'characteristic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'referralMethod', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'appointmentRequired', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService4', 'availableTime', 'HealthcareServiceAvailableTime', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'notAvailable', 'HealthcareServiceNotAvailable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService4', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineHealthcareServiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareService4', nil, 'HealthcareService4', js.FHIRFactoryJs);
  defineHealthcareServicePropsJs(js, def);
end;


procedure defineImagingStudySeriesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeries4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries4', 'number', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries4', 'modality', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries4', 'numberOfInstances', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries4', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries4', 'bodySite', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries4', 'laterality', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries4', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries4', 'started', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudySeries4', 'performer', 'ImagingStudySeriesPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries4', 'instance', 'ImagingStudySeriesInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudySeriesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeries4', nil, 'ImagingStudySeries4', js.FHIRFactoryJs);
  defineImagingStudySeriesPropsJs(js, def);
end;


procedure defineImagingStudySeriesPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeriesPerformer4', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeriesPerformer4', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImagingStudySeriesPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeriesPerformer4', nil, 'ImagingStudySeriesPerformer4', js.FHIRFactoryJs);
  defineImagingStudySeriesPerformerPropsJs(js, def);
end;


procedure defineImagingStudySeriesInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeriesInstance4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeriesInstance4', 'number', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance4', 'sopClass', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeriesInstance4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImagingStudySeriesInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeriesInstance4', nil, 'ImagingStudySeriesInstance4', js.FHIRFactoryJs);
  defineImagingStudySeriesInstancePropsJs(js, def);
end;


procedure defineImagingStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImagingStudy4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy4', 'modality', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy4', 'started', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudy4', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'referrer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy4', 'interpreter', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'numberOfSeries', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy4', 'numberOfInstances', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy4', 'procedureReference', 'Reference(Procedure)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy4', 'procedureCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'series', 'ImagingStudySeries', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudy4', nil, 'ImagingStudy4', js.FHIRFactoryJs);
  defineImagingStudyPropsJs(js, def);
end;


procedure defineImmunizationPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationPerformer4', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationPerformer4', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImmunizationPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationPerformer4', nil, 'ImmunizationPerformer4', js.FHIRFactoryJs);
  defineImmunizationPerformerPropsJs(js, def);
end;


procedure defineImmunizationEducationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationEducation4', 'documentType', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEducation4', 'reference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEducation4', 'publicationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationEducation4', 'presentationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineImmunizationEducationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationEducation4', nil, 'ImmunizationEducation4', js.FHIRFactoryJs);
  defineImmunizationEducationPropsJs(js, def);
end;


procedure defineImmunizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Immunization4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization4', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'vaccineCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization4', 'primarySource', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization4', 'reportOrigin', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization4', 'expirationDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization4', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'performer', 'ImmunizationPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'isPotent', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization4', 'subpotentReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'education', 'ImmunizationEducation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'programEligibility', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'fundingSource', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImmunizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Immunization4', nil, 'Immunization4', js.FHIRFactoryJs);
  defineImmunizationPropsJs(js, def);
end;


procedure defineImmunizationEvaluationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationEvaluation4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'authority', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'targetDisease', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'immunizationEvent', 'Reference(Immunization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'doseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'doseStatusReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'series', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'doseNumberPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'doseNumberString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'seriesDosesPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'seriesDosesString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImmunizationEvaluationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationEvaluation4', nil, 'ImmunizationEvaluation4', js.FHIRFactoryJs);
  defineImmunizationEvaluationPropsJs(js, def);
end;


procedure defineImmunizationRecommendationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'vaccineCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'targetDisease', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'contraindicatedVaccineCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'forecastStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'forecastReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'dateCriterion', 'ImmunizationRecommendationRecommendationDateCriterion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'series', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'doseNumberPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'doseNumberString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'seriesDosesPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'seriesDosesString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'supportingImmunization', 'Reference(Immunization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'supportingPatientInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendation4', nil, 'ImmunizationRecommendationRecommendation4', js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationPropsJs(js, def);
end;


procedure defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion4', 'value', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineImmunizationRecommendationRecommendationDateCriterionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendationDateCriterion4', nil, 'ImmunizationRecommendationRecommendationDateCriterion4', js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js, def);
end;


procedure defineImmunizationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendation4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendation4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationRecommendation4', 'authority', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation4', 'recommendation', 'ImmunizationRecommendationRecommendation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendation4', nil, 'ImmunizationRecommendation4', js.FHIRFactoryJs);
  defineImmunizationRecommendationPropsJs(js, def);
end;


procedure defineImplementationGuideDependsOnPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDependsOn4', 'uri', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDependsOn4', 'packageId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDependsOn4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDependsOnJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDependsOn4', nil, 'ImplementationGuideDependsOn4', js.FHIRFactoryJs);
  defineImplementationGuideDependsOnPropsJs(js, def);
end;


procedure defineImplementationGuideGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideGlobal4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideGlobal4', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideGlobal4', nil, 'ImplementationGuideGlobal4', js.FHIRFactoryJs);
  defineImplementationGuideGlobalPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinition4', 'package', 'ImplementationGuideDefinitionPackage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideDefinition4', 'resource', 'ImplementationGuideDefinitionResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideDefinition4', 'page', 'ImplementationGuideDefinitionPage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideDefinition4', 'parameter', 'ImplementationGuideDefinitionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideDefinition4', 'template', 'ImplementationGuideDefinitionTemplate', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinition4', nil, 'ImplementationGuideDefinition4', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionPackagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionPackage4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPackage4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionPackageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionPackage4', nil, 'ImplementationGuideDefinitionPackage4', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionPackagePropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'exampleBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'exampleCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'package', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionResource4', nil, 'ImplementationGuideDefinitionResource4', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionResourcePropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionPagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionPage4', 'nameUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage4', 'nameReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage4', 'generation', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage4', 'page', '@ImplementationGuide.definition.page', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideDefinitionPageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionPage4', nil, 'ImplementationGuideDefinitionPage4', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionPagePropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionParameter4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionParameter4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionParameter4', nil, 'ImplementationGuideDefinitionParameter4', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionParameterPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionTemplatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionTemplate4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionTemplate4', 'source', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionTemplate4', 'scope', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionTemplateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionTemplate4', nil, 'ImplementationGuideDefinitionTemplate4', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionTemplatePropsJs(js, def);
end;


procedure defineImplementationGuideManifestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideManifest4', 'rendering', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideManifest4', 'resource', 'ImplementationGuideManifestResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideManifest4', 'page', 'ImplementationGuideManifestPage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideManifest4', nil, 'ImplementationGuideManifest4', js.FHIRFactoryJs);
  defineImplementationGuideManifestPropsJs(js, def);
end;


procedure defineImplementationGuideManifestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideManifestResource4', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideManifestResource4', 'exampleBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideManifestResource4', 'exampleCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideManifestResource4', 'relativePath', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideManifestResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideManifestResource4', nil, 'ImplementationGuideManifestResource4', js.FHIRFactoryJs);
  defineImplementationGuideManifestResourcePropsJs(js, def);
end;


procedure defineImplementationGuideManifestPagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideManifestPage4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideManifestPage4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideManifestPageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideManifestPage4', nil, 'ImplementationGuideManifestPage4', js.FHIRFactoryJs);
  defineImplementationGuideManifestPagePropsJs(js, def);
end;


procedure defineImplementationGuidePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuide4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuide4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuide4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'packageId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'license', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'dependsOn', 'ImplementationGuideDependsOn', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide4', 'global', 'ImplementationGuideGlobal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide4', 'definition', 'ImplementationGuideDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuide4', 'manifest', 'ImplementationGuideManifest', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuide4', nil, 'ImplementationGuide4', js.FHIRFactoryJs);
  defineImplementationGuidePropsJs(js, def);
end;


procedure defineInvoiceParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceParticipant4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceParticipant4', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInvoiceParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceParticipant4', nil, 'InvoiceParticipant4', js.FHIRFactoryJs);
  defineInvoiceParticipantPropsJs(js, def);
end;


procedure defineInvoiceLineItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceLineItem4', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'InvoiceLineItem4', 'chargeItem', 'Reference(ChargeItem)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItem4', 'priceComponent', 'InvoiceLineItemPriceComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInvoiceLineItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceLineItem4', nil, 'InvoiceLineItem4', js.FHIRFactoryJs);
  defineInvoiceLineItemPropsJs(js, def);
end;


procedure defineInvoiceLineItemPriceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceLineItemPriceComponent4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent4', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent4', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInvoiceLineItemPriceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceLineItemPriceComponent4', nil, 'InvoiceLineItemPriceComponent4', js.FHIRFactoryJs);
  defineInvoiceLineItemPriceComponentPropsJs(js, def);
end;


procedure defineInvoicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Invoice4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Invoice4', 'cancelledReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Invoice4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'recipient', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Invoice4', 'participant', 'InvoiceParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice4', 'issuer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'account', 'Reference(Account)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'lineItem', 'InvoiceLineItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice4', 'totalPriceComponent', '@Invoice.lineItem.priceComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice4', 'totalNet', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'totalGross', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'paymentTerms', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Invoice4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInvoiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Invoice4', nil, 'Invoice4', js.FHIRFactoryJs);
  defineInvoicePropsJs(js, def);
end;


procedure defineItemInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ItemInstance4', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ItemInstance4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ItemInstance4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ItemInstance4', 'manufactureDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ItemInstance4', 'expiryDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ItemInstance4', 'currentSWVersion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ItemInstance4', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ItemInstance4', 'serialNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ItemInstance4', 'carrierAIDC', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ItemInstance4', 'carrierHRF', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineItemInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ItemInstance4', nil, 'ItemInstance4', js.FHIRFactoryJs);
  defineItemInstancePropsJs(js, def);
end;


procedure defineLibraryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Library4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Library4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library4', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library4', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library4', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library4', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'parameter', 'ParameterDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'dataRequirement', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'content', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLibraryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Library4', nil, 'Library4', js.FHIRFactoryJs);
  defineLibraryPropsJs(js, def);
end;


procedure defineLinkageItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LinkageItem4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'LinkageItem4', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineLinkageItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LinkageItem4', nil, 'LinkageItem4', js.FHIRFactoryJs);
  defineLinkageItemPropsJs(js, def);
end;


procedure defineLinkagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Linkage4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Linkage4', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Linkage4', 'item', 'LinkageItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLinkageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Linkage4', nil, 'Linkage4', js.FHIRFactoryJs);
  defineLinkagePropsJs(js, def);
end;


procedure defineListEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ListEntry4', 'flag', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ListEntry4', 'deleted', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ListEntry4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ListEntry4', 'item', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineListEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ListEntry4', nil, 'ListEntry4', js.FHIRFactoryJs);
  defineListEntryPropsJs(js, def);
end;


procedure defineListPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'List4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List4', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'List4', 'source', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List4', 'orderedBy', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List4', 'entry', 'ListEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List4', 'emptyReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineListJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('List4', nil, 'List4', js.FHIRFactoryJs);
  defineListPropsJs(js, def);
end;


procedure defineLocationPositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LocationPosition4', 'longitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition4', 'latitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition4', 'altitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineLocationPositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LocationPosition4', nil, 'LocationPosition4', js.FHIRFactoryJs);
  defineLocationPositionPropsJs(js, def);
end;


procedure defineLocationHoursOfOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LocationHoursOfOperation4', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'LocationHoursOfOperation4', 'openingTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'LocationHoursOfOperation4', 'closingTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineLocationHoursOfOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LocationHoursOfOperation4', nil, 'LocationHoursOfOperation4', js.FHIRFactoryJs);
  defineLocationHoursOfOperationPropsJs(js, def);
end;


procedure defineLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Location4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location4', 'operationalStatus', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location4', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'physicalType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'position', 'LocationPosition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'partOf', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'hoursOfOperation', 'LocationHoursOfOperation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location4', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location4', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Location4', nil, 'Location4', js.FHIRFactoryJs);
  defineLocationPropsJs(js, def);
end;


procedure defineMeasureGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroup4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroup4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroup4', 'population', 'MeasureGroupPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureGroup4', 'stratifier', 'MeasureGroupStratifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroup4', nil, 'MeasureGroup4', js.FHIRFactoryJs);
  defineMeasureGroupPropsJs(js, def);
end;


procedure defineMeasureGroupPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupPopulation4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupPopulation4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupPopulation4', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMeasureGroupPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupPopulation4', nil, 'MeasureGroupPopulation4', js.FHIRFactoryJs);
  defineMeasureGroupPopulationPropsJs(js, def);
end;


procedure defineMeasureGroupStratifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupStratifier4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupStratifier4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupStratifier4', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupStratifier4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMeasureGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupStratifier4', nil, 'MeasureGroupStratifier4', js.FHIRFactoryJs);
  defineMeasureGroupStratifierPropsJs(js, def);
end;


procedure defineMeasureSupplementalDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureSupplementalData4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureSupplementalData4', 'usage', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureSupplementalData4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureSupplementalData4', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureSupplementalData4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMeasureSupplementalDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureSupplementalData4', nil, 'MeasureSupplementalData4', js.FHIRFactoryJs);
  defineMeasureSupplementalDataPropsJs(js, def);
end;


procedure defineMeasurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Measure4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Measure4', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure4', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'disclaimer', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'scoring', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'compositeScoring', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'riskAdjustment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'rateAggregation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'rationale', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'clinicalRecommendationStatement', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'improvementNotation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'guidance', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'set', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'group', 'MeasureGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'supplementalData', 'MeasureSupplementalData', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Measure4', nil, 'Measure4', js.FHIRFactoryJs);
  defineMeasurePropsJs(js, def);
end;


procedure defineMeasureReportGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroup4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroup4', 'population', 'MeasureReportGroupPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroup4', 'measureScore', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroup4', 'stratifier', 'MeasureReportGroupStratifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureReportGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroup4', nil, 'MeasureReportGroup4', js.FHIRFactoryJs);
  defineMeasureReportGroupPropsJs(js, def);
end;


procedure defineMeasureReportGroupPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupPopulation4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupPopulation4', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupPopulation4', 'subjects', 'Reference(List)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupPopulation4', nil, 'MeasureReportGroupPopulation4', js.FHIRFactoryJs);
  defineMeasureReportGroupPopulationPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifier4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifier4', 'stratum', 'MeasureReportGroupStratifierStratum', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureReportGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifier4', nil, 'MeasureReportGroupStratifier4', js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierStratumPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum4', 'value', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum4', 'population', 'MeasureReportGroupStratifierStratumPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum4', 'measureScore', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratum4', nil, 'MeasureReportGroupStratifierStratum4', js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierStratumPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation4', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation4', 'subjects', 'Reference(List)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratumPopulation4', nil, 'MeasureReportGroupStratifierStratumPopulation4', js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumPopulationPropsJs(js, def);
end;


procedure defineMeasureReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MeasureReport4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReport4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport4', 'measure', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MeasureReport4', 'reporter', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport4', 'group', 'MeasureReportGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReport4', 'evaluatedResources', 'Reference(Bundle)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReport4', nil, 'MeasureReport4', js.FHIRFactoryJs);
  defineMeasureReportPropsJs(js, def);
end;


procedure defineMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Media4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media4', 'basedOn', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media4', 'partOf', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'modality', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'view', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'createdDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Media4', 'createdPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Media4', 'operator', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media4', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'deviceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media4', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'height', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media4', 'width', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media4', 'frames', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media4', 'duration', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Media4', 'content', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Media4', nil, 'Media4', js.FHIRFactoryJs);
  defineMediaPropsJs(js, def);
end;


procedure defineMedicationIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationIngredient4', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationIngredient4', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationIngredient4', 'isActive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationIngredient4', 'amount', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationIngredient4', nil, 'MedicationIngredient4', js.FHIRFactoryJs);
  defineMedicationIngredientPropsJs(js, def);
end;


procedure defineMedicationBatchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationBatch4', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationBatch4', 'expirationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationBatch4', 'serialNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMedicationBatchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationBatch4', nil, 'MedicationBatch4', js.FHIRFactoryJs);
  defineMedicationBatchPropsJs(js, def);
end;


procedure defineMedicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Medication4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Medication4', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication4', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication4', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication4', 'ingredient', 'MedicationIngredient', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Medication4', 'batch', 'MedicationBatch', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Medication4', nil, 'Medication4', js.FHIRFactoryJs);
  defineMedicationPropsJs(js, def);
end;


procedure defineMedicationAdministrationPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationPerformer4', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationPerformer4', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationAdministrationPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationPerformer4', nil, 'MedicationAdministrationPerformer4', js.FHIRFactoryJs);
  defineMedicationAdministrationPerformerPropsJs(js, def);
end;


procedure defineMedicationAdministrationDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'dose', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationAdministrationDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationDosage4', nil, 'MedicationAdministrationDosage4', js.FHIRFactoryJs);
  defineMedicationAdministrationDosagePropsJs(js, def);
end;


procedure defineMedicationAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationAdministration4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'partOf', 'Reference(MedicationAdministration)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministration4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationAdministration4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'performer', 'MedicationAdministrationPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'statusReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'request', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'device', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'dosage', 'MedicationAdministrationDosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'eventHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministration4', nil, 'MedicationAdministration4', js.FHIRFactoryJs);
  defineMedicationAdministrationPropsJs(js, def);
end;


procedure defineMedicationDispensePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispensePerformer4', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispensePerformer4', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationDispensePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispensePerformer4', nil, 'MedicationDispensePerformer4', js.FHIRFactoryJs);
  defineMedicationDispensePerformerPropsJs(js, def);
end;


procedure defineMedicationDispenseSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispenseSubstitution4', 'wasSubstituted', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationDispenseSubstitution4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseSubstitution4', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispenseSubstitution4', 'responsibleParty', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationDispenseSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispenseSubstitution4', nil, 'MedicationDispenseSubstitution4', js.FHIRFactoryJs);
  defineMedicationDispenseSubstitutionPropsJs(js, def);
end;


procedure defineMedicationDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationDispense4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'partOf', 'Reference(Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationDispense4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'performer', 'MedicationDispensePerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'authorizingPrescription', 'Reference(MedicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'daysSupply', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'whenPrepared', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense4', 'whenHandedOver', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense4', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'receiver', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'dosageInstruction', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'substitution', 'MedicationDispenseSubstitution', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'detectedIssue', 'Reference(DetectedIssue)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'statusReasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'statusReasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'eventHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispense4', nil, 'MedicationDispense4', js.FHIRFactoryJs);
  defineMedicationDispensePropsJs(js, def);
end;


procedure defineMedicationKnowledgeMonographPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeMonograph4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeMonograph4', 'document', 'Reference(DocumentReference)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeMonographJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeMonograph4', nil, 'MedicationKnowledgeMonograph4', js.FHIRFactoryJs);
  defineMedicationKnowledgeMonographPropsJs(js, def);
end;


procedure defineMedicationKnowledgeIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeIngredient4', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeIngredient4', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeIngredient4', 'isActive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationKnowledgeIngredient4', 'strength', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeIngredient4', nil, 'MedicationKnowledgeIngredient4', js.FHIRFactoryJs);
  defineMedicationKnowledgeIngredientPropsJs(js, def);
end;


procedure defineMedicationKnowledgeCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeCost4', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledgeCost4', 'source', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledgeCost4', 'cost', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeCost4', nil, 'MedicationKnowledgeCost4', js.FHIRFactoryJs);
  defineMedicationKnowledgeCostPropsJs(js, def);
end;


procedure defineMedicationKnowledgeMonitoringProgramPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeMonitoringProgram4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeMonitoringProgram4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMedicationKnowledgeMonitoringProgramJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeMonitoringProgram4', nil, 'MedicationKnowledgeMonitoringProgram4', js.FHIRFactoryJs);
  defineMedicationKnowledgeMonitoringProgramPropsJs(js, def);
end;


procedure defineMedicationKnowledgeAdministrationGuidelinesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines4', 'dosage', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines4', 'indicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines4', 'indicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines4', 'patientCharacteristics', 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeAdministrationGuidelinesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeAdministrationGuidelines4', nil, 'MedicationKnowledgeAdministrationGuidelines4', js.FHIRFactoryJs);
  defineMedicationKnowledgeAdministrationGuidelinesPropsJs(js, def);
end;


procedure defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics4', 'characteristicCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics4', 'characteristicQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics4', nil, 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics4', js.FHIRFactoryJs);
  defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsPropsJs(js, def);
end;


procedure defineMedicationKnowledgeMedicineClassificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeMedicineClassification4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeMedicineClassification4', 'classification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeMedicineClassificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeMedicineClassification4', nil, 'MedicationKnowledgeMedicineClassification4', js.FHIRFactoryJs);
  defineMedicationKnowledgeMedicineClassificationPropsJs(js, def);
end;


procedure defineMedicationKnowledgePackagingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgePackaging4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgePackaging4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgePackagingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgePackaging4', nil, 'MedicationKnowledgePackaging4', js.FHIRFactoryJs);
  defineMedicationKnowledgePackagingPropsJs(js, def);
end;


procedure defineMedicationKnowledgeDrugCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic4', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineMedicationKnowledgeDrugCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeDrugCharacteristic4', nil, 'MedicationKnowledgeDrugCharacteristic4', js.FHIRFactoryJs);
  defineMedicationKnowledgeDrugCharacteristicPropsJs(js, def);
end;


procedure defineMedicationKnowledgePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledge4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledge4', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'relatedMedicationKnowledge', 'Reference(MedicationKnowledge)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'associatedMedication', 'Reference(Medication)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'productType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'monograph', 'MedicationKnowledgeMonograph', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'halfLifePeriod', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'ingredient', 'MedicationKnowledgeIngredient', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'preparationInstruction', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledge4', 'intendedRoute', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'cost', 'MedicationKnowledgeCost', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'monitoringProgram', 'MedicationKnowledgeMonitoringProgram', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'administrationGuidelines', 'MedicationKnowledgeAdministrationGuidelines', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'medicineClassification', 'MedicationKnowledgeMedicineClassification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'packaging', 'MedicationKnowledgePackaging', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'drugCharacteristic', 'MedicationKnowledgeDrugCharacteristic', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'contraindication', 'Reference(DetectedIssue)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledge4', nil, 'MedicationKnowledge4', js.FHIRFactoryJs);
  defineMedicationKnowledgePropsJs(js, def);
end;


procedure defineMedicationRequestDispenseRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'initialFill', 'MedicationRequestDispenseRequestInitialFill', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'dispenseInterval', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'numberOfRepeatsAllowed', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'expectedSupplyDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'performer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestDispenseRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestDispenseRequest4', nil, 'MedicationRequestDispenseRequest4', js.FHIRFactoryJs);
  defineMedicationRequestDispenseRequestPropsJs(js, def);
end;


procedure defineMedicationRequestDispenseRequestInitialFillPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestDispenseRequestInitialFill4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequestInitialFill4', 'duration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestDispenseRequestInitialFillJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestDispenseRequestInitialFill4', nil, 'MedicationRequestDispenseRequestInitialFill4', js.FHIRFactoryJs);
  defineMedicationRequestDispenseRequestInitialFillPropsJs(js, def);
end;


procedure defineMedicationRequestSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestSubstitution4', 'allowed', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationRequestSubstitution4', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestSubstitution4', nil, 'MedicationRequestSubstitution4', js.FHIRFactoryJs);
  defineMedicationRequestSubstitutionPropsJs(js, def);
end;


procedure defineMedicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationRequest4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest4', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest4', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationRequest4', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationRequest4', 'requester', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'performer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'recorder', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'insurance', 'Reference(Coverage)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'dosageInstruction', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'dispenseRequest', 'MedicationRequestDispenseRequest', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'substitution', 'MedicationRequestSubstitution', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'priorPrescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'detectedIssue', 'Reference(DetectedIssue)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'eventHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequest4', nil, 'MedicationRequest4', js.FHIRFactoryJs);
  defineMedicationRequestPropsJs(js, def);
end;


procedure defineMedicationStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationStatement4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'basedOn', 'Reference(MedicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'partOf', 'Reference(MedicationAdministration)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement4', 'statusReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'dateAsserted', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement4', 'informationSource', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'derivedFrom', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'dosage', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationStatement4', nil, 'MedicationStatement4', js.FHIRFactoryJs);
  defineMedicationStatementPropsJs(js, def);
end;


procedure defineMedicinalProductNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductName4', 'fullName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductName4', 'namePart', 'MedicinalProductNameNamePart', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductName4', 'countryLanguage', 'MedicinalProductNameCountryLanguage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductName4', nil, 'MedicinalProductName4', js.FHIRFactoryJs);
  defineMedicinalProductNamePropsJs(js, def);
end;


procedure defineMedicinalProductNameNamePartPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductNameNamePart4', 'part', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductNameNamePart4', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductNameNamePartJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductNameNamePart4', nil, 'MedicinalProductNameNamePart4', js.FHIRFactoryJs);
  defineMedicinalProductNameNamePartPropsJs(js, def);
end;


procedure defineMedicinalProductNameCountryLanguagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage4', 'country', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage4', 'jurisdiction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage4', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductNameCountryLanguageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductNameCountryLanguage4', nil, 'MedicinalProductNameCountryLanguage4', js.FHIRFactoryJs);
  defineMedicinalProductNameCountryLanguagePropsJs(js, def);
end;


procedure defineMedicinalProductManufacturingBusinessOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'operationType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'authorisationReferenceNumber', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'effectiveDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'confidentialityIndicator', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'regulator', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductManufacturingBusinessOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductManufacturingBusinessOperation4', nil, 'MedicinalProductManufacturingBusinessOperation4', js.FHIRFactoryJs);
  defineMedicinalProductManufacturingBusinessOperationPropsJs(js, def);
end;


procedure defineMedicinalProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProduct4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'combinedPharmaceuticalDoseForm', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'additionalMonitoringIndicator', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'paediatricUseIndicator', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'orphanDesignationStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'productClassification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'marketingAuthorization', 'Reference(MedicinalProductAuthorization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'packagedMedicinalProduct', 'Reference(MedicinalProductPackaged)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'pharmaceuticalProduct', 'Reference(MedicinalProductPharmaceutical)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'clinicalParticulars', 'Reference(MedicinalProductClinicals)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'attachedDocument', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'masterFile', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'name', 'MedicinalProductName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'crossReference', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'manufacturingBusinessOperation', 'MedicinalProductManufacturingBusinessOperation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProduct4', nil, 'MedicinalProduct4', js.FHIRFactoryJs);
  defineMedicinalProductPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationJurisdictionalAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization4', 'country', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization4', 'jurisdiction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization4', 'number', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization4', 'legalStatusOfSupply', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductAuthorizationJurisdictionalAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationJurisdictionalAuthorization4', nil, 'MedicinalProductAuthorizationJurisdictionalAuthorization4', js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationJurisdictionalAuthorizationPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure4', 'number', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure4', 'date', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure4', 'application', 'MedicinalProductAuthorizationProcedureApplication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductAuthorizationProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationProcedure4', nil, 'MedicinalProductAuthorizationProcedure4', js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationProcedurePropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationProcedureApplicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedureApplication4', 'number', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedureApplication4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedureApplication4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineMedicinalProductAuthorizationProcedureApplicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationProcedureApplication4', nil, 'MedicinalProductAuthorizationProcedureApplication4', js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationProcedureApplicationPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'country', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'legalStatusOfSupply', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'statusDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'restoreDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'dataExclusivityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'dateOfFirstAuthorization', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'internationalBirthDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'jurisdictionalAuthorization', 'MedicinalProductAuthorizationJurisdictionalAuthorization', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'holder', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'regulator', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'procedure', 'MedicinalProductAuthorizationProcedure', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'marketingStatus', 'MarketingStatus', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorization4', nil, 'MedicinalProductAuthorization4', js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsUndesirableEffectsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects4', 'symptomConditionEffect', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects4', 'classification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects4', 'frequencyOfOccurrence', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects4', 'population', 'MedicinalProductClinicalsUndesirableEffectsPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsUndesirableEffectsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsUndesirableEffects4', nil, 'MedicinalProductClinicalsUndesirableEffects4', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsUndesirableEffectsPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsUndesirableEffectsPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation4', 'ageRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation4', 'ageCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation4', 'gender', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation4', 'race', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation4', 'physiologicalCondition', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductClinicalsUndesirableEffectsPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsUndesirableEffectsPopulation4', nil, 'MedicinalProductClinicalsUndesirableEffectsPopulation4', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsUndesirableEffectsPopulationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsTherapeuticIndicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication4', 'diseaseSymptomProcedure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication4', 'diseaseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication4', 'comorbidity', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication4', 'intendedEffect', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication4', 'duration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication4', 'undesirableEffects', '@MedicinalProductClinicals.undesirableEffects', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication4', 'otherTherapy', 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication4', 'population', '@MedicinalProductClinicals.undesirableEffects.population', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsTherapeuticIndicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsTherapeuticIndication4', nil, 'MedicinalProductClinicalsTherapeuticIndication4', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsTherapeuticIndicationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsTherapeuticIndicationOtherTherapyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy4', 'therapyRelationshipType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy4', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy4', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductClinicalsTherapeuticIndicationOtherTherapyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsTherapeuticIndicationOtherTherapy4', nil, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy4', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsTherapeuticIndicationOtherTherapyPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsContraindicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication4', 'disease', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication4', 'diseaseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication4', 'comorbidity', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication4', 'therapeuticIndication', '@MedicinalProductClinicals.therapeuticIndication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication4', 'otherTherapy', '@MedicinalProductClinicals.therapeuticIndication.otherTherapy', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication4', 'population', '@MedicinalProductClinicals.undesirableEffects.population', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsContraindicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsContraindication4', nil, 'MedicinalProductClinicalsContraindication4', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsContraindicationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsInteractionsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions4', 'interactant', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions4', 'effect', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions4', 'incidence', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions4', 'management', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductClinicalsInteractionsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsInteractions4', nil, 'MedicinalProductClinicalsInteractions4', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsInteractionsPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicals4', 'undesirableEffects', 'MedicinalProductClinicalsUndesirableEffects', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicals4', 'therapeuticIndication', 'MedicinalProductClinicalsTherapeuticIndication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicals4', 'contraindication', 'MedicinalProductClinicalsContraindication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicals4', 'interactions', 'MedicinalProductClinicalsInteractions', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicals4', nil, 'MedicinalProductClinicals4', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsPropsJs(js, def);
end;


procedure defineMedicinalProductDeviceSpecMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductDeviceSpecMaterial4', 'substance', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpecMaterial4', 'alternate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicinalProductDeviceSpecMaterial4', 'allergenicIndicator', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineMedicinalProductDeviceSpecMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductDeviceSpecMaterial4', nil, 'MedicinalProductDeviceSpecMaterial4', js.FHIRFactoryJs);
  defineMedicinalProductDeviceSpecMaterialPropsJs(js, def);
end;


procedure defineMedicinalProductDeviceSpecPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'tradeName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'listingNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'modelNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'sterilityIndicator', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'sterilisationRequirement', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'usage', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'nomenclature', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'shelfLife', 'ProductShelfLife', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'physicalCharacteristics', 'ProdCharacteristic', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'otherCharacteristics', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'batchIdentifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec4', 'material', 'MedicinalProductDeviceSpecMaterial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductDeviceSpecJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductDeviceSpec4', nil, 'MedicinalProductDeviceSpec4', js.FHIRFactoryJs);
  defineMedicinalProductDeviceSpecPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance4', 'group', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance4', 'confidentiality', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance4', 'strength', 'MedicinalProductIngredientSpecifiedSubstanceStrength', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstance4', nil, 'MedicinalProductIngredientSpecifiedSubstance4', js.FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstancePropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'presentation', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'concentration', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'measurementPoint', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'country', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'referenceStrength', 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstanceStrength4', nil, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', js.FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength4', 'substance', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength4', nil, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength4', js.FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSubstance4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSubstance4', 'strength', '@MedicinalProductIngredient.specifiedSubstance.strength', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSubstance4', nil, 'MedicinalProductIngredientSubstance4', js.FHIRFactoryJs);
  defineMedicinalProductIngredientSubstancePropsJs(js, def);
end;


procedure defineMedicinalProductIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredient4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredient4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredient4', 'allergenicIndicator', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicinalProductIngredient4', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredient4', 'specifiedSubstance', 'MedicinalProductIngredientSpecifiedSubstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredient4', 'substance', 'MedicinalProductIngredientSubstance', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredient4', nil, 'MedicinalProductIngredient4', js.FHIRFactoryJs);
  defineMedicinalProductIngredientPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedBatchIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedBatchIdentifier4', 'outerPackaging', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedBatchIdentifier4', 'immediatePackaging', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductPackagedBatchIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedBatchIdentifier4', nil, 'MedicinalProductPackagedBatchIdentifier4', js.FHIRFactoryJs);
  defineMedicinalProductPackagedBatchIdentifierPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPackageItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'material', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'alternateMaterial', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'device', 'Reference(MedicinalProductDeviceSpec)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'manufacturedItem', 'MedicinalProductPackagedPackageItemManufacturedItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'otherCharacteristics', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'packageItem', '@MedicinalProductPackaged.packageItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'physicalCharacteristics', 'ProdCharacteristic', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'shelfLifeStorage', 'ProductShelfLife', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPackagedPackageItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedPackageItem4', nil, 'MedicinalProductPackagedPackageItem4', js.FHIRFactoryJs);
  defineMedicinalProductPackagedPackageItemPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPackageItemManufacturedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem4', 'manufacturedDoseForm', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem4', 'unitOfPresentation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem4', 'xManufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem4', 'ingredient', 'Reference(MedicinalProductIngredient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem4', 'physicalCharacteristics', 'ProdCharacteristic', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductPackagedPackageItemManufacturedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedPackageItemManufacturedItem4', nil, 'MedicinalProductPackagedPackageItemManufacturedItem4', js.FHIRFactoryJs);
  defineMedicinalProductPackagedPackageItemManufacturedItemPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackaged4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'marketingStatus', 'MarketingStatus', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'batchIdentifier', 'MedicinalProductPackagedBatchIdentifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'packageItem', 'MedicinalProductPackagedPackageItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPackagedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackaged4', nil, 'MedicinalProductPackaged4', js.FHIRFactoryJs);
  defineMedicinalProductPackagedPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalCharacteristicsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceuticalCharacteristics4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalCharacteristics4', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductPharmaceuticalCharacteristicsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceuticalCharacteristics4', nil, 'MedicinalProductPharmaceuticalCharacteristics4', js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalCharacteristicsPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'administrableDoseForm', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'unitOfPresentation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'routeOfAdministration', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'ingredient', 'Reference(MedicinalProductIngredient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'characteristics', 'MedicinalProductPharmaceuticalCharacteristics', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPharmaceuticalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceutical4', nil, 'MedicinalProductPharmaceutical4', js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalPropsJs(js, def);
end;


procedure defineMessageDefinitionFocusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionFocus4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus4', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus4', 'min', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus4', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageDefinitionFocusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinitionFocus4', nil, 'MessageDefinitionFocus4', js.FHIRFactoryJs);
  defineMessageDefinitionFocusPropsJs(js, def);
end;


procedure defineMessageDefinitionAllowedResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionAllowedResponse4', 'message', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionAllowedResponse4', 'situation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageDefinitionAllowedResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinitionAllowedResponse4', nil, 'MessageDefinitionAllowedResponse4', js.FHIRFactoryJs);
  defineMessageDefinitionAllowedResponsePropsJs(js, def);
end;


procedure defineMessageDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'MessageDefinition4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinition4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MessageDefinition4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MessageDefinition4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'base', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'eventCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinition4', 'eventUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'focus', 'MessageDefinitionFocus', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition4', 'responseRequired', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'allowedResponse', 'MessageDefinitionAllowedResponse', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMessageDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinition4', nil, 'MessageDefinition4', js.FHIRFactoryJs);
  defineMessageDefinitionPropsJs(js, def);
end;


procedure defineMessageHeaderDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderDestination4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination4', 'target', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderDestination4', 'endpoint', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination4', 'receiver', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMessageHeaderDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderDestination4', nil, 'MessageHeaderDestination4', js.FHIRFactoryJs);
  defineMessageHeaderDestinationPropsJs(js, def);
end;


procedure defineMessageHeaderSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderSource4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource4', 'software', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource4', 'contact', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderSource4', 'endpoint', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderSource4', nil, 'MessageHeaderSource4', js.FHIRFactoryJs);
  defineMessageHeaderSourcePropsJs(js, def);
end;


procedure defineMessageHeaderResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderResponse4', 'identifier', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse4', 'details', 'Reference(OperationOutcome)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMessageHeaderResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderResponse4', nil, 'MessageHeaderResponse4', js.FHIRFactoryJs);
  defineMessageHeaderResponsePropsJs(js, def);
end;


procedure defineMessageHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MessageHeader4', 'eventCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'eventUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeader4', 'destination', 'MessageHeaderDestination', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader4', 'sender', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'source', 'MessageHeaderSource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'responsible', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'response', 'MessageHeaderResponse', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'focus', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader4', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeader4', nil, 'MessageHeader4', js.FHIRFactoryJs);
  defineMessageHeaderPropsJs(js, def);
end;


procedure defineNamingSystemUniqueIdPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NamingSystemUniqueId4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId4', 'preferred', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'NamingSystemUniqueId4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNamingSystemUniqueIdJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystemUniqueId4', nil, 'NamingSystemUniqueId4', js.FHIRFactoryJs);
  defineNamingSystemUniqueIdPropsJs(js, def);
end;


procedure defineNamingSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'NamingSystem4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NamingSystem4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem4', 'responsible', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NamingSystem4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem4', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'uniqueId', 'NamingSystemUniqueId', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineNamingSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystem4', nil, 'NamingSystem4', js.FHIRFactoryJs);
  defineNamingSystemPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'schedule', 'Timing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'nutrient', 'NutritionOrderOralDietNutrient', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'texture', 'NutritionOrderOralDietTexture', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'fluidConsistencyType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderOralDietJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDiet4', nil, 'NutritionOrderOralDiet4', js.FHIRFactoryJs);
  defineNutritionOrderOralDietPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietNutrientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietNutrient4', 'modifier', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietNutrient4', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietNutrientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietNutrient4', nil, 'NutritionOrderOralDietNutrient4', js.FHIRFactoryJs);
  defineNutritionOrderOralDietNutrientPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietTexturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietTexture4', 'modifier', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietTexture4', 'foodType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietTextureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietTexture4', nil, 'NutritionOrderOralDietTexture4', js.FHIRFactoryJs);
  defineNutritionOrderOralDietTexturePropsJs(js, def);
end;


procedure defineNutritionOrderSupplementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderSupplement4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement4', 'productName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderSupplement4', 'schedule', 'Timing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderSupplement4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement4', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderSupplementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderSupplement4', nil, 'NutritionOrderSupplement4', js.FHIRFactoryJs);
  defineNutritionOrderSupplementPropsJs(js, def);
end;


procedure defineNutritionOrderEnteralFormulaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'baseFormulaType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'baseFormulaProductName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'additiveType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'additiveProductName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'caloricDensity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'routeofAdministration', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'administration', 'NutritionOrderEnteralFormulaAdministration', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'maxVolumeToDeliver', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'administrationInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderEnteralFormulaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormula4', nil, 'NutritionOrderEnteralFormula4', js.FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaPropsJs(js, def);
end;


procedure defineNutritionOrderEnteralFormulaAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration4', 'schedule', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration4', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration4', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderEnteralFormulaAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormulaAdministration4', nil, 'NutritionOrderEnteralFormulaAdministration4', js.FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaAdministrationPropsJs(js, def);
end;


procedure defineNutritionOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'NutritionOrder4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrder4', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrder4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder4', 'dateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NutritionOrder4', 'orderer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder4', 'allergyIntolerance', 'Reference(AllergyIntolerance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder4', 'foodPreferenceModifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder4', 'excludeFoodModifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder4', 'oralDiet', 'NutritionOrderOralDiet', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder4', 'supplement', 'NutritionOrderSupplement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder4', 'enteralFormula', 'NutritionOrderEnteralFormula', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineNutritionOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrder4', nil, 'NutritionOrder4', js.FHIRFactoryJs);
  defineNutritionOrderPropsJs(js, def);
end;


procedure defineObservationReferenceRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationReferenceRange4', 'low', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange4', 'high', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange4', 'appliesTo', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationReferenceRange4', 'age', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineObservationReferenceRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationReferenceRange4', nil, 'ObservationReferenceRange4', js.FHIRFactoryJs);
  defineObservationReferenceRangePropsJs(js, def);
end;


procedure defineObservationComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationComponent4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ObservationComponent4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ObservationComponent4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ObservationComponent4', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'dataAbsentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'interpretation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'referenceRange', '@Observation.referenceRange', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineObservationComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationComponent4', nil, 'ObservationComponent4', js.FHIRFactoryJs);
  defineObservationComponentPropsJs(js, def);
end;


procedure defineObservationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Observation4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'partOf', 'Reference(MedicationAdministration)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'focus', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'effectiveTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation4', 'performer', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Observation4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Observation4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation4', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'dataAbsentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'interpretation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation4', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'specimen', 'Reference(Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'referenceRange', 'ObservationReferenceRange', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'hasMember', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'derivedFrom', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'component', 'ObservationComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineObservationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Observation4', nil, 'Observation4', js.FHIRFactoryJs);
  defineObservationPropsJs(js, def);
end;


procedure defineObservationDefinitionQuantitativeDetailsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails4', 'customaryUnit', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails4', 'unit', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails4', 'conversionFactor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails4', 'decimalPrecision', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineObservationDefinitionQuantitativeDetailsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinitionQuantitativeDetails4', nil, 'ObservationDefinitionQuantitativeDetails4', js.FHIRFactoryJs);
  defineObservationDefinitionQuantitativeDetailsPropsJs(js, def);
end;


procedure defineObservationDefinitionQualifiedIntervalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'range', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'appliesTo', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'age', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'gestationalAge', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'condition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineObservationDefinitionQualifiedIntervalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinitionQualifiedInterval4', nil, 'ObservationDefinitionQualifiedInterval4', js.FHIRFactoryJs);
  defineObservationDefinitionQualifiedIntervalPropsJs(js, def);
end;


procedure defineObservationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ObservationDefinition4', 'category', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition4', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition4', 'permittedDataType', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition4', 'multipleResultsAllowed', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ObservationDefinition4', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition4', 'preferredReportName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition4', 'quantitativeDetails', 'ObservationDefinitionQuantitativeDetails', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition4', 'qualifiedInterval', 'ObservationDefinitionQualifiedInterval', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition4', 'validCodedValueSet', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition4', 'normalCodedValueSet', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition4', 'abnormalCodedValueSet', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition4', 'criticalCodedValueSet', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineObservationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinition4', nil, 'ObservationDefinition4', js.FHIRFactoryJs);
  defineObservationDefinitionPropsJs(js, def);
end;


procedure defineOccupationalDataEmploymentStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataEmploymentStatus4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataEmploymentStatus4', 'effective', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOccupationalDataEmploymentStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataEmploymentStatus4', nil, 'OccupationalDataEmploymentStatus4', js.FHIRFactoryJs);
  defineOccupationalDataEmploymentStatusPropsJs(js, def);
end;


procedure defineOccupationalDataUsualWorkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataUsualWork4', 'occupation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataUsualWork4', 'industry', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataUsualWork4', 'start', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalDataUsualWork4', 'duration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOccupationalDataUsualWorkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataUsualWork4', nil, 'OccupationalDataUsualWork4', js.FHIRFactoryJs);
  defineOccupationalDataUsualWorkPropsJs(js, def);
end;


procedure defineOccupationalDataPastOrPresentJobPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataPastOrPresentJob4', 'occupation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentJob4', 'industry', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentJob4', 'effective', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentJob4', 'employer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentJob4', 'workClassification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentJob4', 'supervisoryLevel', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentJob4', 'workSchedule', 'OccupationalDataPastOrPresentJobWorkSchedule', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOccupationalDataPastOrPresentJobJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataPastOrPresentJob4', nil, 'OccupationalDataPastOrPresentJob4', js.FHIRFactoryJs);
  defineOccupationalDataPastOrPresentJobPropsJs(js, def);
end;


procedure defineOccupationalDataPastOrPresentJobWorkSchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataPastOrPresentJobWorkSchedule4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentJobWorkSchedule4', 'weeklyWorkDays', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentJobWorkSchedule4', 'dailyWorkHours', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineOccupationalDataPastOrPresentJobWorkScheduleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataPastOrPresentJobWorkSchedule4', nil, 'OccupationalDataPastOrPresentJobWorkSchedule4', js.FHIRFactoryJs);
  defineOccupationalDataPastOrPresentJobWorkSchedulePropsJs(js, def);
end;


procedure defineOccupationalDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OccupationalData4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OccupationalData4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalData4', 'recorder', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OccupationalData4', 'informant', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OccupationalData4', 'employmentStatus', 'OccupationalDataEmploymentStatus', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OccupationalData4', 'combatZonePeriod', 'Period', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OccupationalData4', 'usualWork', 'OccupationalDataUsualWork', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData4', 'pastOrPresentJob', 'OccupationalDataPastOrPresentJob', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOccupationalDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalData4', nil, 'OccupationalData4', js.FHIRFactoryJs);
  defineOccupationalDataPropsJs(js, def);
end;


procedure defineOperationDefinitionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameter4', 'name', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'searchType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'binding', 'OperationDefinitionParameterBinding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'part', '@OperationDefinition.parameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameter4', nil, 'OperationDefinitionParameter4', js.FHIRFactoryJs);
  defineOperationDefinitionParameterPropsJs(js, def);
end;


procedure defineOperationDefinitionParameterBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameterBinding4', 'strength', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterBinding4', 'valueSet', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationDefinitionParameterBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameterBinding4', nil, 'OperationDefinitionParameterBinding4', js.FHIRFactoryJs);
  defineOperationDefinitionParameterBindingPropsJs(js, def);
end;


procedure defineOperationDefinitionOverloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionOverload4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationDefinitionOverloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionOverload4', nil, 'OperationDefinitionOverload4', js.FHIRFactoryJs);
  defineOperationDefinitionOverloadPropsJs(js, def);
end;


procedure defineOperationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'OperationDefinition4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OperationDefinition4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'affectsState', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'comment', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'base', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'system', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition4', 'type', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition4', 'instance', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition4', 'inputProfile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'outputProfile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'parameter', 'OperationDefinitionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition4', 'overload', 'OperationDefinitionOverload', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinition4', nil, 'OperationDefinition4', js.FHIRFactoryJs);
  defineOperationDefinitionPropsJs(js, def);
end;


procedure defineOperationOutcomeIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationOutcomeIssue4', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue4', 'details', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationOutcomeIssue4', 'diagnostics', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationOutcomeIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcomeIssue4', nil, 'OperationOutcomeIssue4', js.FHIRFactoryJs);
  defineOperationOutcomeIssuePropsJs(js, def);
end;


procedure defineOperationOutcomePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OperationOutcome4', 'issue', 'OperationOutcomeIssue', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationOutcomeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcome4', nil, 'OperationOutcome4', js.FHIRFactoryJs);
  defineOperationOutcomePropsJs(js, def);
end;


procedure defineOrganizationContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationContact4', 'purpose', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact4', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationContact4', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOrganizationContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationContact4', nil, 'OrganizationContact4', js.FHIRFactoryJs);
  defineOrganizationContactPropsJs(js, def);
end;


procedure defineOrganizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Organization4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Organization4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Organization4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization4', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization4', 'partOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Organization4', 'contact', 'OrganizationContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization4', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrganizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Organization4', nil, 'Organization4', js.FHIRFactoryJs);
  defineOrganizationPropsJs(js, def);
end;


procedure defineOrganizationRoleAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationRoleAvailableTime4', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OrganizationRoleAvailableTime4', 'availableStartTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OrganizationRoleAvailableTime4', 'availableEndTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOrganizationRoleAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationRoleAvailableTime4', nil, 'OrganizationRoleAvailableTime4', js.FHIRFactoryJs);
  defineOrganizationRoleAvailableTimePropsJs(js, def);
end;


procedure defineOrganizationRoleNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationRoleNotAvailable4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OrganizationRoleNotAvailable4', 'during', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOrganizationRoleNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationRoleNotAvailable4', nil, 'OrganizationRoleNotAvailable4', js.FHIRFactoryJs);
  defineOrganizationRoleNotAvailablePropsJs(js, def);
end;


procedure defineOrganizationRolePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OrganizationRole4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OrganizationRole4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationRole4', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationRole4', 'participatingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationRole4', 'network', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole4', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole4', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole4', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole4', 'healthcareService', 'Reference(HealthcareService)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole4', 'availableTime', 'OrganizationRoleAvailableTime', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole4', 'notAvailable', 'OrganizationRoleNotAvailable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole4', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OrganizationRole4', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrganizationRoleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationRole4', nil, 'OrganizationRole4', js.FHIRFactoryJs);
  defineOrganizationRolePropsJs(js, def);
end;


procedure definePatientContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientContact4', 'relationship', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PatientContact4', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PatientContact4', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact4', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PatientContact4', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePatientContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientContact4', nil, 'PatientContact4', js.FHIRFactoryJs);
  definePatientContactPropsJs(js, def);
end;


procedure definePatientCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientCommunication4', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientCommunication4', 'preferred', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure definePatientCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientCommunication4', nil, 'PatientCommunication4', js.FHIRFactoryJs);
  definePatientCommunicationPropsJs(js, def);
end;


procedure definePatientLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientLink4', 'other', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientLink4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePatientLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientLink4', nil, 'PatientLink4', js.FHIRFactoryJs);
  definePatientLinkPropsJs(js, def);
end;


procedure definePatientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Patient4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient4', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Patient4', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Patient4', 'deceasedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient4', 'deceasedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Patient4', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'maritalStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient4', 'multipleBirthBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient4', 'multipleBirthInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Patient4', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'contact', 'PatientContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'communication', 'PatientCommunication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'generalPractitioner', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient4', 'link', 'PatientLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePatientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Patient4', nil, 'Patient4', js.FHIRFactoryJs);
  definePatientPropsJs(js, def);
end;


procedure definePaymentNoticePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentNotice4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentNotice4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentNotice4', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'statusDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice4', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'paymentStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePaymentNoticeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentNotice4', nil, 'PaymentNotice4', js.FHIRFactoryJs);
  definePaymentNoticePropsJs(js, def);
end;


procedure definePaymentReconciliationDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'submitter', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'payee', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePaymentReconciliationDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationDetail4', nil, 'PaymentReconciliationDetail4', js.FHIRFactoryJs);
  definePaymentReconciliationDetailPropsJs(js, def);
end;


procedure definePaymentReconciliationProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationProcessNote4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliationProcessNote4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePaymentReconciliationProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationProcessNote4', nil, 'PaymentReconciliationProcessNote4', js.FHIRFactoryJs);
  definePaymentReconciliationProcessNotePropsJs(js, def);
end;


procedure definePaymentReconciliationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliation4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliation4', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'request', 'Reference(ProcessRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation4', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation4', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'detail', 'PaymentReconciliationDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation4', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'total', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'processNote', 'PaymentReconciliationProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePaymentReconciliationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliation4', nil, 'PaymentReconciliation4', js.FHIRFactoryJs);
  definePaymentReconciliationPropsJs(js, def);
end;


procedure definePersonLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PersonLink4', 'target', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PersonLink4', 'assurance', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePersonLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PersonLink4', nil, 'PersonLink4', js.FHIRFactoryJs);
  definePersonLinkPropsJs(js, def);
end;


procedure definePersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Person4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person4', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person4', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Person4', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Person4', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person4', 'photo', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Person4', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Person4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Person4', 'link', 'PersonLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Person4', nil, 'Person4', js.FHIRFactoryJs);
  definePersonPropsJs(js, def);
end;


procedure definePlanDefinitionGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionGoal4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'description', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'start', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'addresses', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'documentation', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'target', 'PlanDefinitionGoalTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionGoal4', nil, 'PlanDefinitionGoal4', js.FHIRFactoryJs);
  definePlanDefinitionGoalPropsJs(js, def);
end;


procedure definePlanDefinitionGoalTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionGoalTarget4', 'measure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget4', 'detailQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget4', 'detailRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget4', 'detailCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget4', 'due', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionGoalTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionGoalTarget4', nil, 'PlanDefinitionGoalTarget4', js.FHIRFactoryJs);
  definePlanDefinitionGoalTargetPropsJs(js, def);
end;


procedure definePlanDefinitionActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionAction4', 'prefix', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'textEquivalent', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'documentation', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'trigger', 'TriggerDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'condition', 'PlanDefinitionActionCondition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'input', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'output', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'relatedAction', 'PlanDefinitionActionRelatedAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'participant', 'PlanDefinitionActionParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'groupingBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'selectionBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'requiredBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'precheckBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'cardinalityBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'transform', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'dynamicValue', 'PlanDefinitionActionDynamicValue', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'action', '@PlanDefinition.action', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionAction4', nil, 'PlanDefinitionAction4', js.FHIRFactoryJs);
  definePlanDefinitionActionPropsJs(js, def);
end;


procedure definePlanDefinitionActionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionCondition4', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition4', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePlanDefinitionActionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionCondition4', nil, 'PlanDefinitionActionCondition4', js.FHIRFactoryJs);
  definePlanDefinitionActionConditionPropsJs(js, def);
end;


procedure definePlanDefinitionActionRelatedActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction4', 'actionId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction4', 'relationship', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction4', 'offsetDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction4', 'offsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionRelatedActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionRelatedAction4', nil, 'PlanDefinitionActionRelatedAction4', js.FHIRFactoryJs);
  definePlanDefinitionActionRelatedActionPropsJs(js, def);
end;


procedure definePlanDefinitionActionParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionParticipant4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionParticipant4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionParticipant4', nil, 'PlanDefinitionActionParticipant4', js.FHIRFactoryJs);
  definePlanDefinitionActionParticipantPropsJs(js, def);
end;


procedure definePlanDefinitionActionDynamicValuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue4', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePlanDefinitionActionDynamicValueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionDynamicValue4', nil, 'PlanDefinitionActionDynamicValue4', js.FHIRFactoryJs);
  definePlanDefinitionActionDynamicValuePropsJs(js, def);
end;


procedure definePlanDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'PlanDefinition4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PlanDefinition4', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition4', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition4', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition4', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'goal', 'PlanDefinitionGoal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'action', 'PlanDefinitionAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinition4', nil, 'PlanDefinition4', js.FHIRFactoryJs);
  definePlanDefinitionPropsJs(js, def);
end;


procedure definePractitionerQualificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerQualification4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerQualification4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification4', 'issuer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePractitionerQualificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerQualification4', nil, 'PractitionerQualification4', js.FHIRFactoryJs);
  definePractitionerQualificationPropsJs(js, def);
end;


procedure definePractitionerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Practitioner4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Practitioner4', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Practitioner4', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Practitioner4', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'qualification', 'PractitionerQualification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'communication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePractitionerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Practitioner4', nil, 'Practitioner4', js.FHIRFactoryJs);
  definePractitionerPropsJs(js, def);
end;


procedure definePractitionerRoleAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerRoleAvailableTime4', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PractitionerRoleAvailableTime4', 'availableStartTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRoleAvailableTime4', 'availableEndTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePractitionerRoleAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRoleAvailableTime4', nil, 'PractitionerRoleAvailableTime4', js.FHIRFactoryJs);
  definePractitionerRoleAvailableTimePropsJs(js, def);
end;


procedure definePractitionerRoleNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerRoleNotAvailable4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRoleNotAvailable4', 'during', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePractitionerRoleNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRoleNotAvailable4', nil, 'PractitionerRoleNotAvailable4', js.FHIRFactoryJs);
  definePractitionerRoleNotAvailablePropsJs(js, def);
end;


procedure definePractitionerRolePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PractitionerRole4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PractitionerRole4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole4', 'practitioner', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole4', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole4', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'healthcareService', 'Reference(HealthcareService)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'availableTime', 'PractitionerRoleAvailableTime', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'notAvailable', 'PractitionerRoleNotAvailable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRole4', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePractitionerRoleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRole4', nil, 'PractitionerRole4', js.FHIRFactoryJs);
  definePractitionerRolePropsJs(js, def);
end;


procedure defineProcedurePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedurePerformer4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer4', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer4', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedurePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedurePerformer4', nil, 'ProcedurePerformer4', js.FHIRFactoryJs);
  defineProcedurePerformerPropsJs(js, def);
end;


procedure defineProcedureFocalDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedureFocalDevice4', 'action', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureFocalDevice4', 'manipulated', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedureFocalDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedureFocalDevice4', nil, 'ProcedureFocalDevice4', js.FHIRFactoryJs);
  defineProcedureFocalDevicePropsJs(js, def);
end;


procedure defineProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Procedure4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'partOf', 'Reference(Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Procedure4', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'performedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Procedure4', 'performedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'performedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Procedure4', 'performedAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'performedRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'recorder', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'asserter', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'performer', 'ProcedurePerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'report', 'Reference(DiagnosticReport)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'complication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'complicationDetail', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'followUp', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'focalDevice', 'ProcedureFocalDevice', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'usedReference', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'usedCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Procedure4', nil, 'Procedure4', js.FHIRFactoryJs);
  defineProcedurePropsJs(js, def);
end;


procedure defineProcessRequestItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessRequestItem4', 'sequenceLinkId', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineProcessRequestItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessRequestItem4', nil, 'ProcessRequestItem4', js.FHIRFactoryJs);
  defineProcessRequestItemPropsJs(js, def);
end;


procedure defineProcessRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessRequest4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest4', 'action', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest4', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessRequest4', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest4', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest4', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest4', 'nullify', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ProcessRequest4', 'reference', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest4', 'item', 'ProcessRequestItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcessRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessRequest4', nil, 'ProcessRequest4', js.FHIRFactoryJs);
  defineProcessRequestPropsJs(js, def);
end;


procedure defineProcessResponseProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessResponseProcessNote4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponseProcessNote4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineProcessResponseProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponseProcessNote4', nil, 'ProcessResponseProcessNote4', js.FHIRFactoryJs);
  defineProcessResponseProcessNotePropsJs(js, def);
end;


procedure defineProcessResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessResponse4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse4', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessResponse4', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse4', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse4', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse4', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse4', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse4', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse4', 'processNote', 'ProcessResponseProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse4', 'error', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse4', 'communicationRequest', 'Reference(CommunicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcessResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponse4', nil, 'ProcessResponse4', js.FHIRFactoryJs);
  defineProcessResponsePropsJs(js, def);
end;


procedure defineProductPlanContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanContact4', 'purpose', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanContact4', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanContact4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanContact4', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProductPlanContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanContact4', nil, 'ProductPlanContact4', js.FHIRFactoryJs);
  defineProductPlanContactPropsJs(js, def);
end;


procedure defineProductPlanCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanCoverage4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanCoverage4', 'network', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanCoverage4', 'benefit', 'ProductPlanCoverageBenefit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanCoverage4', nil, 'ProductPlanCoverage4', js.FHIRFactoryJs);
  defineProductPlanCoveragePropsJs(js, def);
end;


procedure defineProductPlanCoverageBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanCoverageBenefit4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanCoverageBenefit4', 'requirement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProductPlanCoverageBenefit4', 'limit', 'ProductPlanCoverageBenefitLimit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanCoverageBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanCoverageBenefit4', nil, 'ProductPlanCoverageBenefit4', js.FHIRFactoryJs);
  defineProductPlanCoverageBenefitPropsJs(js, def);
end;


procedure defineProductPlanCoverageBenefitLimitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanCoverageBenefitLimit4', 'value', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanCoverageBenefitLimit4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProductPlanCoverageBenefitLimitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanCoverageBenefitLimit4', nil, 'ProductPlanCoverageBenefitLimit4', js.FHIRFactoryJs);
  defineProductPlanCoverageBenefitLimitPropsJs(js, def);
end;


procedure defineProductPlanPlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlan4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanPlan4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlan4', 'coverageArea', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanPlan4', 'network', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanPlan4', 'generalCost', 'ProductPlanPlanGeneralCost', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanPlan4', 'specificCost', 'ProductPlanPlanSpecificCost', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanPlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlan4', nil, 'ProductPlanPlan4', js.FHIRFactoryJs);
  defineProductPlanPlanPropsJs(js, def);
end;


procedure defineProductPlanPlanGeneralCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlanGeneralCost4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanGeneralCost4', 'groupSize', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ProductPlanPlanGeneralCost4', 'cost', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanGeneralCost4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineProductPlanPlanGeneralCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlanGeneralCost4', nil, 'ProductPlanPlanGeneralCost4', js.FHIRFactoryJs);
  defineProductPlanPlanGeneralCostPropsJs(js, def);
end;


procedure defineProductPlanPlanSpecificCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlanSpecificCost4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanSpecificCost4', 'benefit', 'ProductPlanPlanSpecificCostBenefit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanPlanSpecificCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlanSpecificCost4', nil, 'ProductPlanPlanSpecificCost4', js.FHIRFactoryJs);
  defineProductPlanPlanSpecificCostPropsJs(js, def);
end;


procedure defineProductPlanPlanSpecificCostBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlanSpecificCostBenefit4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanSpecificCostBenefit4', 'cost', 'ProductPlanPlanSpecificCostBenefitCost', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanPlanSpecificCostBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlanSpecificCostBenefit4', nil, 'ProductPlanPlanSpecificCostBenefit4', js.FHIRFactoryJs);
  defineProductPlanPlanSpecificCostBenefitPropsJs(js, def);
end;


procedure defineProductPlanPlanSpecificCostBenefitCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlanSpecificCostBenefitCost4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanSpecificCostBenefitCost4', 'applicability', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanSpecificCostBenefitCost4', 'qualifiers', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanPlanSpecificCostBenefitCost4', 'value', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProductPlanPlanSpecificCostBenefitCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlanSpecificCostBenefitCost4', nil, 'ProductPlanPlanSpecificCostBenefitCost4', js.FHIRFactoryJs);
  defineProductPlanPlanSpecificCostBenefitCostPropsJs(js, def);
end;


procedure defineProductPlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProductPlan4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProductPlan4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProductPlan4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan4', 'ownedBy', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan4', 'administeredBy', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan4', 'coverageArea', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan4', 'contact', 'ProductPlanContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan4', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan4', 'network', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan4', 'coverage', 'ProductPlanCoverage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan4', 'plan', 'ProductPlanPlan', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlan4', nil, 'ProductPlan4', js.FHIRFactoryJs);
  defineProductPlanPropsJs(js, def);
end;


procedure defineProvenanceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceAgent4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent4', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProvenanceAgent4', 'whoIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent4', 'whoReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent4', 'onBehalfOfIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent4', 'onBehalfOfReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProvenanceAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceAgent4', nil, 'ProvenanceAgent4', js.FHIRFactoryJs);
  defineProvenanceAgentPropsJs(js, def);
end;


procedure defineProvenanceEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceEntity4', 'role', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity4', 'whatIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity4', 'whatReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity4', 'agent', '@Provenance.agent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProvenanceEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceEntity4', nil, 'ProvenanceEntity4', js.FHIRFactoryJs);
  defineProvenanceEntityPropsJs(js, def);
end;


procedure defineProvenancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Provenance4', 'target', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance4', 'occurredPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance4', 'occurredDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance4', 'recorded', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance4', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance4', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance4', 'activity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance4', 'agent', 'ProvenanceAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance4', 'entity', 'ProvenanceEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance4', 'signature', 'Signature', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProvenanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Provenance4', nil, 'Provenance4', js.FHIRFactoryJs);
  defineProvenancePropsJs(js, def);
end;


procedure defineQuestionnaireItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItem4', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'definition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem4', 'prefix', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'enableWhen', 'QuestionnaireItemEnableWhen', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem4', 'enableBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem4', 'repeats', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem4', 'readOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem4', 'maxLength', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItem4', 'options', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'option', 'QuestionnaireItemOption', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem4', 'initial', 'QuestionnaireItemInitial', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem4', 'item', '@Questionnaire.item', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItem4', nil, 'QuestionnaireItem4', js.FHIRFactoryJs);
  defineQuestionnaireItemPropsJs(js, def);
end;


procedure defineQuestionnaireItemEnableWhenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'question', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'operator', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineQuestionnaireItemEnableWhenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemEnableWhen4', nil, 'QuestionnaireItemEnableWhen4', js.FHIRFactoryJs);
  defineQuestionnaireItemEnableWhenPropsJs(js, def);
end;


procedure defineQuestionnaireItemOptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemOption4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemOption4', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemOption4', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemOption4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemOption4', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemOption4', 'initialSelected', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineQuestionnaireItemOptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemOption4', nil, 'QuestionnaireItemOption4', js.FHIRFactoryJs);
  defineQuestionnaireItemOptionPropsJs(js, def);
end;


procedure defineQuestionnaireItemInitialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineQuestionnaireItemInitialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemInitial4', nil, 'QuestionnaireItemInitial4', js.FHIRFactoryJs);
  defineQuestionnaireItemInitialPropsJs(js, def);
end;


procedure defineQuestionnairePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Questionnaire4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Questionnaire4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire4', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire4', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Questionnaire4', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire4', 'item', 'QuestionnaireItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Questionnaire4', nil, 'Questionnaire4', js.FHIRFactoryJs);
  defineQuestionnairePropsJs(js, def);
end;


procedure defineQuestionnaireResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'definition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'answer', 'QuestionnaireResponseItemAnswer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'item', '@QuestionnaireResponse.item', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseItem4', nil, 'QuestionnaireResponseItem4', js.FHIRFactoryJs);
  defineQuestionnaireResponseItemPropsJs(js, def);
end;


procedure defineQuestionnaireResponseItemAnswerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'item', '@QuestionnaireResponse.item', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseItemAnswerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseItemAnswer4', nil, 'QuestionnaireResponseItemAnswer4', js.FHIRFactoryJs);
  defineQuestionnaireResponseItemAnswerPropsJs(js, def);
end;


procedure defineQuestionnaireResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponse4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'partOf', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'questionnaire', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'authored', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'author', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'source', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'item', 'QuestionnaireResponseItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponse4', nil, 'QuestionnaireResponse4', js.FHIRFactoryJs);
  defineQuestionnaireResponsePropsJs(js, def);
end;


procedure defineRelatedPersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RelatedPerson4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'RelatedPerson4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson4', 'relationship', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedPerson4', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RelatedPerson4', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRelatedPersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedPerson4', nil, 'RelatedPerson4', js.FHIRFactoryJs);
  defineRelatedPersonPropsJs(js, def);
end;


procedure defineRequestGroupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupAction4', 'prefix', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'textEquivalent', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction4', 'documentation', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction4', 'condition', 'RequestGroupActionCondition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction4', 'relatedAction', 'RequestGroupActionRelatedAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'participant', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'groupingBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'selectionBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'requiredBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'precheckBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'cardinalityBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'action', '@RequestGroup.action', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRequestGroupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupAction4', nil, 'RequestGroupAction4', js.FHIRFactoryJs);
  defineRequestGroupActionPropsJs(js, def);
end;


procedure defineRequestGroupActionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupActionCondition4', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition4', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRequestGroupActionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupActionCondition4', nil, 'RequestGroupActionCondition4', js.FHIRFactoryJs);
  defineRequestGroupActionConditionPropsJs(js, def);
end;


procedure defineRequestGroupActionRelatedActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupActionRelatedAction4', 'actionId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction4', 'relationship', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction4', 'offsetDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction4', 'offsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRequestGroupActionRelatedActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupActionRelatedAction4', nil, 'RequestGroupActionRelatedAction4', js.FHIRFactoryJs);
  defineRequestGroupActionRelatedActionPropsJs(js, def);
end;


procedure defineRequestGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RequestGroup4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'replaces', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup4', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup4', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup4', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroup4', 'author', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'action', 'RequestGroupAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRequestGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroup4', nil, 'RequestGroup4', js.FHIRFactoryJs);
  defineRequestGroupPropsJs(js, def);
end;


procedure defineResearchStudyArmPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchStudyArm4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudyArm4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudyArm4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineResearchStudyArmJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudyArm4', nil, 'ResearchStudyArm4', js.FHIRFactoryJs);
  defineResearchStudyArmPropsJs(js, def);
end;


procedure defineResearchStudyObjectivePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchStudyObjective4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudyObjective4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineResearchStudyObjectiveJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudyObjective4', nil, 'ResearchStudyObjective4', js.FHIRFactoryJs);
  defineResearchStudyObjectivePropsJs(js, def);
end;


procedure defineResearchStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchStudy4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy4', 'protocol', 'Reference(PlanDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'partOf', 'Reference(ResearchStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy4', 'primaryPurposeType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'phase', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'focus', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'condition', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'keyword', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'location', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy4', 'enrollment', 'Reference(Group)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'sponsor', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'principalInvestigator', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'site', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'reasonStopped', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'arm', 'ResearchStudyArm', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'objective', 'ResearchStudyObjective', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineResearchStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudy4', nil, 'ResearchStudy4', js.FHIRFactoryJs);
  defineResearchStudyPropsJs(js, def);
end;


procedure defineResearchSubjectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchSubject4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchSubject4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject4', 'study', 'Reference(ResearchStudy)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject4', 'individual', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject4', 'assignedArm', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject4', 'actualArm', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject4', 'consent', 'Reference(Consent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineResearchSubjectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchSubject4', nil, 'ResearchSubject4', js.FHIRFactoryJs);
  defineResearchSubjectPropsJs(js, def);
end;


procedure defineRiskAssessmentPredictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'probabilityDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'probabilityRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'qualitativeRisk', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'relativeRisk', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'whenPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'whenRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'rationale', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRiskAssessmentPredictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessmentPrediction4', nil, 'RiskAssessmentPrediction4', js.FHIRFactoryJs);
  defineRiskAssessmentPredictionPropsJs(js, def);
end;


procedure defineRiskAssessmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RiskAssessment4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment4', 'basedOn', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'parent', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment4', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskAssessment4', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'condition', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'performer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment4', 'basis', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment4', 'prediction', 'RiskAssessmentPrediction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment4', 'mitigation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskAssessmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessment4', nil, 'RiskAssessment4', js.FHIRFactoryJs);
  defineRiskAssessmentPropsJs(js, def);
end;


procedure defineSchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Schedule4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule4', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Schedule4', 'serviceCategory', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule4', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule4', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule4', 'actor', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule4', 'planningHorizon', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Schedule4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineScheduleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Schedule4', nil, 'Schedule4', js.FHIRFactoryJs);
  defineSchedulePropsJs(js, def);
end;


procedure defineSearchParameterComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SearchParameterComponent4', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameterComponent4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSearchParameterComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameterComponent4', nil, 'SearchParameterComponent4', js.FHIRFactoryJs);
  defineSearchParameterComponentPropsJs(js, def);
end;


procedure defineSearchParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'SearchParameter4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'derivedFrom', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SearchParameter4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'xpath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'xpathUsage', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'multipleOr', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter4', 'multipleAnd', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter4', 'component', 'SearchParameterComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSearchParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameter4', nil, 'SearchParameter4', js.FHIRFactoryJs);
  defineSearchParameterPropsJs(js, def);
end;


procedure defineSequenceReferenceSeqPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceReferenceSeq4', 'chromosome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq4', 'genomeBuild', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceReferenceSeq4', 'orientation', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceReferenceSeq4', 'referenceSeqId', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq4', 'referenceSeqPointer', 'Reference(Sequence)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq4', 'referenceSeqString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceReferenceSeq4', 'strand', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceReferenceSeq4', 'windowStart', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceReferenceSeq4', 'windowEnd', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineSequenceReferenceSeqJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceReferenceSeq4', nil, 'SequenceReferenceSeq4', js.FHIRFactoryJs);
  defineSequenceReferenceSeqPropsJs(js, def);
end;


procedure defineSequenceVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceVariant4', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceVariant4', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceVariant4', 'observedAllele', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant4', 'referenceAllele', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant4', 'cigar', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant4', 'variantPointer', 'Reference(Observation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSequenceVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceVariant4', nil, 'SequenceVariant4', js.FHIRFactoryJs);
  defineSequenceVariantPropsJs(js, def);
end;


procedure defineSequenceQualityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceQuality4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceQuality4', 'standardSequence', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality4', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceQuality4', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceQuality4', 'score', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality4', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality4', 'truthTP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality4', 'queryTP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality4', 'truthFN', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality4', 'queryFP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality4', 'gtFP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality4', 'precision', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality4', 'recall', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality4', 'fScore', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality4', 'roc', 'SequenceQualityRoc', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSequenceQualityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceQuality4', nil, 'SequenceQuality4', js.FHIRFactoryJs);
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
  def := js.defineClass('SequenceQualityRoc4', nil, 'SequenceQualityRoc4', js.FHIRFactoryJs);
  defineSequenceQualityRocPropsJs(js, def);
end;


procedure defineSequenceRepositoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceRepository4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository4', 'datasetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository4', 'variantsetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository4', 'readsetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSequenceRepositoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceRepository4', nil, 'SequenceRepository4', js.FHIRFactoryJs);
  defineSequenceRepositoryPropsJs(js, def);
end;


procedure defineSequenceStructureVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceStructureVariant4', 'precision', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceStructureVariant4', 'reportedaCGHRatio', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceStructureVariant4', 'length', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceStructureVariant4', 'outer', 'SequenceStructureVariantOuter', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceStructureVariant4', 'inner', 'SequenceStructureVariantInner', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSequenceStructureVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceStructureVariant4', nil, 'SequenceStructureVariant4', js.FHIRFactoryJs);
  defineSequenceStructureVariantPropsJs(js, def);
end;


procedure defineSequenceStructureVariantOuterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceStructureVariantOuter4', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceStructureVariantOuter4', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineSequenceStructureVariantOuterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceStructureVariantOuter4', nil, 'SequenceStructureVariantOuter4', js.FHIRFactoryJs);
  defineSequenceStructureVariantOuterPropsJs(js, def);
end;


procedure defineSequenceStructureVariantInnerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceStructureVariantInner4', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceStructureVariantInner4', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineSequenceStructureVariantInnerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceStructureVariantInner4', nil, 'SequenceStructureVariantInner4', js.FHIRFactoryJs);
  defineSequenceStructureVariantInnerPropsJs(js, def);
end;


procedure defineSequencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Sequence4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Sequence4', 'coordinateSystem', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Sequence4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence4', 'specimen', 'Reference(Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence4', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence4', 'performer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence4', 'referenceSeq', 'SequenceReferenceSeq', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence4', 'variant', 'SequenceVariant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence4', 'observedSeq', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Sequence4', 'quality', 'SequenceQuality', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence4', 'readCoverage', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Sequence4', 'repository', 'SequenceRepository', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence4', 'pointer', 'Reference(Sequence)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence4', 'structureVariant', 'SequenceStructureVariant', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSequenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Sequence4', nil, 'Sequence4', js.FHIRFactoryJs);
  defineSequencePropsJs(js, def);
end;


procedure defineServiceRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ServiceRequest4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'replaces', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'requisition', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest4', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest4', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ServiceRequest4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'orderDetail', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceRequest4', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ServiceRequest4', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceRequest4', 'requester', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'performer', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'insurance', 'Reference(Coverage)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'patientInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest4', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineServiceRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ServiceRequest4', nil, 'ServiceRequest4', js.FHIRFactoryJs);
  defineServiceRequestPropsJs(js, def);
end;


procedure defineSlotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Slot4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot4', 'serviceCategory', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot4', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot4', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot4', 'appointmentType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot4', 'schedule', 'Reference(Schedule)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Slot4', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Slot4', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Slot4', 'overbooked', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Slot4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSlotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Slot4', nil, 'Slot4', js.FHIRFactoryJs);
  defineSlotPropsJs(js, def);
end;


procedure defineSpecimenCollectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenCollection4', 'collector', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'collectedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenCollection4', 'collectedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenCollectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenCollection4', nil, 'SpecimenCollection4', js.FHIRFactoryJs);
  defineSpecimenCollectionPropsJs(js, def);
end;


procedure defineSpecimenProcessingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenProcessing4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenProcessing4', 'procedure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenProcessing4', 'additive', 'Reference(Substance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenProcessing4', 'timeDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenProcessing4', 'timePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenProcessingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenProcessing4', nil, 'SpecimenProcessing4', js.FHIRFactoryJs);
  defineSpecimenProcessingPropsJs(js, def);
end;


procedure defineSpecimenContainerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenContainer4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenContainer4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenContainer4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer4', 'capacity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer4', 'specimenQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer4', 'additiveCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer4', 'additiveReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenContainerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenContainer4', nil, 'SpecimenContainer4', js.FHIRFactoryJs);
  defineSpecimenContainerPropsJs(js, def);
end;


procedure defineSpecimenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Specimen4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'accessionIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Specimen4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen4', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen4', 'receivedTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Specimen4', 'parent', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'request', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'collection', 'SpecimenCollection', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen4', 'processing', 'SpecimenProcessing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'container', 'SpecimenContainer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Specimen4', nil, 'Specimen4', js.FHIRFactoryJs);
  defineSpecimenPropsJs(js, def);
end;


procedure defineSpecimenDefinitionSpecimenToLabPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'isDerived', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'preference', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'containerMaterial', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'containerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'containerCap', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'containerDescription', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'containerCapacity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'containerMinimumVolume', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'containerAdditive', 'SpecimenDefinitionSpecimenToLabContainerAdditive', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'containerPreparation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'requirement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'retentionTime', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'rejectionCriterion', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab4', 'handling', 'SpecimenDefinitionSpecimenToLabHandling', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenDefinitionSpecimenToLabJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionSpecimenToLab4', nil, 'SpecimenDefinitionSpecimenToLab4', js.FHIRFactoryJs);
  defineSpecimenDefinitionSpecimenToLabPropsJs(js, def);
end;


procedure defineSpecimenDefinitionSpecimenToLabContainerAdditivePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabContainerAdditive4', 'additiveCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabContainerAdditive4', 'additiveReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenDefinitionSpecimenToLabContainerAdditiveJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionSpecimenToLabContainerAdditive4', nil, 'SpecimenDefinitionSpecimenToLabContainerAdditive4', js.FHIRFactoryJs);
  defineSpecimenDefinitionSpecimenToLabContainerAdditivePropsJs(js, def);
end;


procedure defineSpecimenDefinitionSpecimenToLabHandlingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling4', 'conditionSet', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling4', 'tempRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling4', 'maxDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling4', 'lightExposure', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling4', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSpecimenDefinitionSpecimenToLabHandlingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionSpecimenToLabHandling4', nil, 'SpecimenDefinitionSpecimenToLabHandling4', js.FHIRFactoryJs);
  defineSpecimenDefinitionSpecimenToLabHandlingPropsJs(js, def);
end;


procedure defineSpecimenDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinition4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinition4', 'typeCollected', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinition4', 'patientPreparation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinition4', 'timeAspect', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinition4', 'collection', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinition4', 'specimenToLab', 'SpecimenDefinitionSpecimenToLab', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinition4', nil, 'SpecimenDefinition4', js.FHIRFactoryJs);
  defineSpecimenDefinitionPropsJs(js, def);
end;


procedure defineStructureDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionMapping4', 'identity', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping4', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionMapping4', nil, 'StructureDefinitionMapping4', js.FHIRFactoryJs);
  defineStructureDefinitionMappingPropsJs(js, def);
end;


procedure defineStructureDefinitionContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionContext4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionContext4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureDefinitionContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionContext4', nil, 'StructureDefinitionContext4', js.FHIRFactoryJs);
  defineStructureDefinitionContextPropsJs(js, def);
end;


procedure defineStructureDefinitionSnapshotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionSnapshot4', 'element', 'ElementDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionSnapshotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionSnapshot4', nil, 'StructureDefinitionSnapshot4', js.FHIRFactoryJs);
  defineStructureDefinitionSnapshotPropsJs(js, def);
end;


procedure defineStructureDefinitionDifferentialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionDifferential4', 'element', 'ElementDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionDifferentialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionDifferential4', nil, 'StructureDefinitionDifferential4', js.FHIRFactoryJs);
  defineStructureDefinitionDifferentialPropsJs(js, def);
end;


procedure defineStructureDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'StructureDefinition4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureDefinition4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'keyword', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'mapping', 'StructureDefinitionMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'abstract', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition4', 'context', 'StructureDefinitionContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'type', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'baseDefinition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'derivation', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'snapshot', 'StructureDefinitionSnapshot', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureDefinition4', 'differential', 'StructureDefinitionDifferential', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineStructureDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinition4', nil, 'StructureDefinition4', js.FHIRFactoryJs);
  defineStructureDefinitionPropsJs(js, def);
end;


procedure defineStructureMapStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapStructure4', 'url', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure4', 'alias', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapStructure4', nil, 'StructureMapStructure4', js.FHIRFactoryJs);
  defineStructureMapStructurePropsJs(js, def);
end;


procedure defineStructureMapGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroup4', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup4', 'extends', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup4', 'typeMode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup4', 'input', 'StructureMapGroupInput', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroup4', 'rule', 'StructureMapGroupRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroup4', nil, 'StructureMapGroup4', js.FHIRFactoryJs);
  defineStructureMapGroupPropsJs(js, def);
end;


procedure defineStructureMapGroupInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupInput4', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput4', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput4', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupInput4', nil, 'StructureMapGroupInput4', js.FHIRFactoryJs);
  defineStructureMapGroupInputPropsJs(js, def);
end;


procedure defineStructureMapGroupRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRule4', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRule4', 'source', 'StructureMapGroupRuleSource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule4', 'target', 'StructureMapGroupRuleTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule4', 'rule', '@StructureMap.group.rule', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule4', 'dependent', 'StructureMapGroupRuleDependent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule4', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRule4', nil, 'StructureMapGroupRule4', js.FHIRFactoryJs);
  defineStructureMapGroupRulePropsJs(js, def);
end;


procedure defineStructureMapGroupRuleSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'context', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'element', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'listMode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'variable', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'condition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'check', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleSource4', nil, 'StructureMapGroupRuleSource4', js.FHIRFactoryJs);
  defineStructureMapGroupRuleSourcePropsJs(js, def);
end;


procedure defineStructureMapGroupRuleTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'context', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'contextType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'element', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'variable', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'listRuleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'transform', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'parameter', 'StructureMapGroupRuleTargetParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapGroupRuleTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleTarget4', nil, 'StructureMapGroupRuleTarget4', js.FHIRFactoryJs);
  defineStructureMapGroupRuleTargetPropsJs(js, def);
end;


procedure defineStructureMapGroupRuleTargetParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter4', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter4', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineStructureMapGroupRuleTargetParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleTargetParameter4', nil, 'StructureMapGroupRuleTargetParameter4', js.FHIRFactoryJs);
  defineStructureMapGroupRuleTargetParameterPropsJs(js, def);
end;


procedure defineStructureMapGroupRuleDependentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleDependent4', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleDependentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleDependent4', nil, 'StructureMapGroupRuleDependent4', js.FHIRFactoryJs);
  defineStructureMapGroupRuleDependentPropsJs(js, def);
end;


procedure defineStructureMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'StructureMap4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMap4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMap4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'structure', 'StructureMapStructure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap4', 'group', 'StructureMapGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMap4', nil, 'StructureMap4', js.FHIRFactoryJs);
  defineStructureMapPropsJs(js, def);
end;


procedure defineSubscriptionChannelPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubscriptionChannel4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel4', 'endpoint', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel4', 'payload', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubscriptionChannelJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubscriptionChannel4', nil, 'SubscriptionChannel4', js.FHIRFactoryJs);
  defineSubscriptionChannelPropsJs(js, def);
end;


procedure defineSubscriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Subscription4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription4', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Subscription4', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Subscription4', 'reason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription4', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription4', 'error', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription4', 'channel', 'SubscriptionChannel', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Subscription4', 'tag', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubscriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Subscription4', nil, 'Subscription4', js.FHIRFactoryJs);
  defineSubscriptionPropsJs(js, def);
end;


procedure defineSubstanceInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceInstance4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceInstance4', 'expiry', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceInstance4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceInstance4', nil, 'SubstanceInstance4', js.FHIRFactoryJs);
  defineSubstanceInstancePropsJs(js, def);
end;


procedure defineSubstanceIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceIngredient4', 'quantity', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient4', 'substanceCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient4', 'substanceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceIngredient4', nil, 'SubstanceIngredient4', js.FHIRFactoryJs);
  defineSubstanceIngredientPropsJs(js, def);
end;


procedure defineSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Substance4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Substance4', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Substance4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Substance4', 'instance', 'SubstanceInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance4', 'ingredient', 'SubstanceIngredient', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Substance4', nil, 'Substance4', js.FHIRFactoryJs);
  defineSubstancePropsJs(js, def);
end;


procedure defineSubstancePolymerMonomerSetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerMonomerSet4', 'ratioType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSet4', 'startingMaterial', 'SubstancePolymerMonomerSetStartingMaterial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerMonomerSetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerMonomerSet4', nil, 'SubstancePolymerMonomerSet4', js.FHIRFactoryJs);
  defineSubstancePolymerMonomerSetPropsJs(js, def);
end;


procedure defineSubstancePolymerMonomerSetStartingMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial4', 'material', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial4', 'isDefining', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial4', 'amount', 'SubstanceAmount', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstancePolymerMonomerSetStartingMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerMonomerSetStartingMaterial4', nil, 'SubstancePolymerMonomerSetStartingMaterial4', js.FHIRFactoryJs);
  defineSubstancePolymerMonomerSetStartingMaterialPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeat4', 'numberOfUnits', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstancePolymerRepeat4', 'averageMolecularFormula', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeat4', 'repeatUnitAmountType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeat4', 'repeatUnit', 'SubstancePolymerRepeatRepeatUnit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerRepeatJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeat4', nil, 'SubstancePolymerRepeat4', js.FHIRFactoryJs);
  defineSubstancePolymerRepeatPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit4', 'orientationOfPolymerisation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit4', 'repeatUnit', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit4', 'amount', 'SubstanceAmount', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit4', 'degreeOfPolymerisation', 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit4', 'structuralRepresentation', 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnit4', nil, 'SubstancePolymerRepeatRepeatUnit4', js.FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation4', 'degree', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation4', 'amount', 'SubstanceAmount', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation4', nil, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation4', js.FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation4', 'representation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation4', 'attachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnitStructuralRepresentation4', nil, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation4', js.FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationPropsJs(js, def);
end;


procedure defineSubstancePolymerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstancePolymer4', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymer4', 'geometry', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymer4', 'copolymerConnectivity', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymer4', 'monomerSet', 'SubstancePolymerMonomerSet', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymer4', 'repeat', 'SubstancePolymerRepeat', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymer4', nil, 'SubstancePolymer4', js.FHIRFactoryJs);
  defineSubstancePolymerPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationGenePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationGene4', 'geneSequenceOrigin', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGene4', 'gene', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGene4', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationGeneJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationGene4', nil, 'SubstanceReferenceInformationGene4', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationGenePropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationGeneElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement4', 'element', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement4', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationGeneElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationGeneElement4', nil, 'SubstanceReferenceInformationGeneElement4', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationGeneElementPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationClassificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationClassification4', 'domain', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification4', 'classification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification4', 'subtype', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification4', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationClassificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationClassification4', nil, 'SubstanceReferenceInformationClassification4', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationClassificationPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationRelationshipPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'substanceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'substanceCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'interaction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'isDefining', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'amountQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'amountRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'amountString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'amountType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'amountText', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship4', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationRelationshipJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationRelationship4', nil, 'SubstanceReferenceInformationRelationship4', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationRelationshipPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'target', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'interaction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'organism', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'organismType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'amountQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'amountRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'amountString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'amountType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceReferenceInformationTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationTarget4', nil, 'SubstanceReferenceInformationTarget4', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationTargetPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'gene', 'SubstanceReferenceInformationGene', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'geneElement', 'SubstanceReferenceInformationGeneElement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'classification', 'SubstanceReferenceInformationClassification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'relationship', 'SubstanceReferenceInformationRelationship', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'target', 'SubstanceReferenceInformationTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformation4', nil, 'SubstanceReferenceInformation4', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationPropsJs(js, def);
end;


procedure defineSubstanceSpecificationMoietyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'stereochemistry', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'opticalActivity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'molecularFormula', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'amount', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSpecificationMoietyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationMoiety4', nil, 'SubstanceSpecificationMoiety4', js.FHIRFactoryJs);
  defineSubstanceSpecificationMoietyPropsJs(js, def);
end;


procedure defineSubstanceSpecificationPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'name', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'parameters', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'substanceId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'substanceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'amount', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSpecificationPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationProperty4', nil, 'SubstanceSpecificationProperty4', js.FHIRFactoryJs);
  defineSubstanceSpecificationPropertyPropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'stereochemistry', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'opticalActivity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'molecularFormula', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'molecularFormulaByMoiety', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'isotope', 'SubstanceSpecificationStructureIsotope', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'molecularWeight', '@SubstanceSpecification.structure.isotope.molecularWeight', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'referenceSource', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'structuralRepresentation', 'SubstanceSpecificationStructureStructuralRepresentation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructure4', nil, 'SubstanceSpecificationStructure4', js.FHIRFactoryJs);
  defineSubstanceSpecificationStructurePropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureIsotopePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'nuclideId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'nuclideName', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'substitutionType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'nuclideHalfLife', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'amount', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'molecularWeight', 'SubstanceSpecificationStructureIsotopeMolecularWeight', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureIsotopeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureIsotope4', nil, 'SubstanceSpecificationStructureIsotope4', js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureIsotopePropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureIsotopeMolecularWeightPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight4', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight4', 'amount', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSpecificationStructureIsotopeMolecularWeightJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureIsotopeMolecularWeight4', nil, 'SubstanceSpecificationStructureIsotopeMolecularWeight4', js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureIsotopeMolecularWeightPropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureStructuralRepresentationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureStructuralRepresentation4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureStructuralRepresentation4', 'representation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructureStructuralRepresentation4', 'attachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureStructuralRepresentationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureStructuralRepresentation4', nil, 'SubstanceSpecificationStructureStructuralRepresentation4', js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureStructuralRepresentationPropsJs(js, def);
end;


procedure defineSubstanceSpecificationSubstanceCodePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode4', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode4', 'statusDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSpecificationSubstanceCodeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationSubstanceCode4', nil, 'SubstanceSpecificationSubstanceCode4', js.FHIRFactoryJs);
  defineSubstanceSpecificationSubstanceCodePropsJs(js, def);
end;


procedure defineSubstanceSpecificationSubstanceNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName4', 'language', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName4', 'domain', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName4', 'officialName', 'SubstanceSpecificationSubstanceNameOfficialName', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationSubstanceNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationSubstanceName4', nil, 'SubstanceSpecificationSubstanceName4', js.FHIRFactoryJs);
  defineSubstanceSpecificationSubstanceNamePropsJs(js, def);
end;


procedure defineSubstanceSpecificationSubstanceNameOfficialNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationSubstanceNameOfficialName4', 'authority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceNameOfficialName4', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceNameOfficialName4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineSubstanceSpecificationSubstanceNameOfficialNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationSubstanceNameOfficialName4', nil, 'SubstanceSpecificationSubstanceNameOfficialName4', js.FHIRFactoryJs);
  defineSubstanceSpecificationSubstanceNameOfficialNamePropsJs(js, def);
end;


procedure defineSubstanceSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecification4', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecification4', 'stoichiometric', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstanceSpecification4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'moiety', 'SubstanceSpecificationMoiety', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'property', 'SubstanceSpecificationProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'referenceInformation', 'Reference(SubstanceReferenceInformation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'structure', 'SubstanceSpecificationStructure', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'substanceCode', 'SubstanceSpecificationSubstanceCode', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'substanceName', 'SubstanceSpecificationSubstanceName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'molecularWeight', '@SubstanceSpecification.structure.isotope.molecularWeight', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'polymer', 'Reference(SubstancePolymer)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecification4', nil, 'SubstanceSpecification4', js.FHIRFactoryJs);
  defineSubstanceSpecificationPropsJs(js, def);
end;


procedure defineSupplyDeliverySuppliedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyDeliverySuppliedItem4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDeliverySuppliedItem4', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDeliverySuppliedItem4', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyDeliverySuppliedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDeliverySuppliedItem4', nil, 'SupplyDeliverySuppliedItem4', js.FHIRFactoryJs);
  defineSupplyDeliverySuppliedItemPropsJs(js, def);
end;


procedure defineSupplyDeliveryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyDelivery4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery4', 'basedOn', 'Reference(SupplyRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery4', 'partOf', 'Reference(SupplyDelivery)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyDelivery4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'suppliedItem', 'SupplyDeliverySuppliedItem', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyDelivery4', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'supplier', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'receiver', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSupplyDeliveryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDelivery4', nil, 'SupplyDelivery4', js.FHIRFactoryJs);
  defineSupplyDeliveryPropsJs(js, def);
end;


procedure defineSupplyRequestParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyRequestParameter4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineSupplyRequestParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequestParameter4', nil, 'SupplyRequestParameter4', js.FHIRFactoryJs);
  defineSupplyRequestParameterPropsJs(js, def);
end;


procedure defineSupplyRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyRequest4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest4', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest4', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'parameter', 'SupplyRequestParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest4', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest4', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest4', 'requester', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'supplier', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest4', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest4', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest4', 'deliverFrom', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'deliverTo', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequest4', nil, 'SupplyRequest4', js.FHIRFactoryJs);
  defineSupplyRequestPropsJs(js, def);
end;


procedure defineTaskRestrictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskRestriction4', 'repetitions', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskRestriction4', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskRestriction4', 'recipient', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTaskRestrictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskRestriction4', nil, 'TaskRestriction4', js.FHIRFactoryJs);
  defineTaskRestrictionPropsJs(js, def);
end;


procedure defineTaskInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskInput4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'TaskInput4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TaskInput4', 'valueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput4', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TaskInput4', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskInput4', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskInput4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTaskInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskInput4', nil, 'TaskInput4', js.FHIRFactoryJs);
  defineTaskInputPropsJs(js, def);
end;


procedure defineTaskOutputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskOutput4', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'TaskOutput4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TaskOutput4', 'valueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput4', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput4', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TaskOutput4', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskOutput4', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskOutput4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTaskOutputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskOutput4', nil, 'TaskOutput4', js.FHIRFactoryJs);
  defineTaskOutputPropsJs(js, def);
end;


procedure defineTaskPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Task4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'instantiatesUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'instantiatesReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'partOf', 'Reference(Task)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'businessStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'focus', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'for', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'executionPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Task4', 'lastModified', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Task4', 'requester', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'performerType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'owner', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'reasonCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'reasonReference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'restriction', 'TaskRestriction', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'input', 'TaskInput', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'output', 'TaskOutput', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTaskJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Task4', nil, 'Task4', js.FHIRFactoryJs);
  defineTaskPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem4', 'uri', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem4', 'version', 'TerminologyCapabilitiesCodeSystemVersion', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystem4', nil, 'TerminologyCapabilitiesCodeSystem4', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion4', 'code', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion4', 'isDefault', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion4', 'compositional', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion4', 'filter', 'TerminologyCapabilitiesCodeSystemVersionFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystemVersion4', nil, 'TerminologyCapabilitiesCodeSystemVersion4', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemVersionPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemVersionFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersionFilter4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemVersionFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystemVersionFilter4', nil, 'TerminologyCapabilitiesCodeSystemVersionFilter4', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemVersionFilterPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesExpansionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion4', 'hierarchical', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion4', 'paging', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion4', 'incomplete', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion4', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion4', 'textFilter', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesExpansion4', nil, 'TerminologyCapabilitiesExpansion4', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesExpansionPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesValidateCodePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesValidateCode4', 'translations', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesValidateCodeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesValidateCode4', nil, 'TerminologyCapabilitiesValidateCode4', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesValidateCodePropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesTranslationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesTranslation4', 'needsMap', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesTranslationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesTranslation4', nil, 'TerminologyCapabilitiesTranslation4', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesTranslationPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesClosurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesClosure4', 'translation', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesClosureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesClosure4', nil, 'TerminologyCapabilitiesClosure4', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesClosurePropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilities4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'lockedDate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'codeSystem', 'TerminologyCapabilitiesCodeSystem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'expansion', 'TerminologyCapabilitiesExpansion', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'codeSearch', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'validateCode', 'TerminologyCapabilitiesValidateCode', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'translation', 'TerminologyCapabilitiesTranslation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'closure', 'TerminologyCapabilitiesClosure', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTerminologyCapabilitiesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilities4', nil, 'TerminologyCapabilities4', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesPropsJs(js, def);
end;


procedure defineTestReportParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportParticipant4', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportParticipant4', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportParticipant4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportParticipant4', nil, 'TestReportParticipant4', js.FHIRFactoryJs);
  defineTestReportParticipantPropsJs(js, def);
end;


procedure defineTestReportSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetup4', 'action', 'TestReportSetupAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetup4', nil, 'TestReportSetup4', js.FHIRFactoryJs);
  defineTestReportSetupPropsJs(js, def);
end;


procedure defineTestReportSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupAction4', 'operation', 'TestReportSetupActionOperation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReportSetupAction4', 'assert', 'TestReportSetupActionAssert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupAction4', nil, 'TestReportSetupAction4', js.FHIRFactoryJs);
  defineTestReportSetupActionPropsJs(js, def);
end;


procedure defineTestReportSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupActionOperation4', 'result', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionOperation4', 'message', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionOperation4', 'detail', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupActionOperation4', nil, 'TestReportSetupActionOperation4', js.FHIRFactoryJs);
  defineTestReportSetupActionOperationPropsJs(js, def);
end;


procedure defineTestReportSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupActionAssert4', 'result', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionAssert4', 'message', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionAssert4', 'detail', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupActionAssert4', nil, 'TestReportSetupActionAssert4', js.FHIRFactoryJs);
  defineTestReportSetupActionAssertPropsJs(js, def);
end;


procedure defineTestReportTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTest4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportTest4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportTest4', 'action', 'TestReportTestAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTest4', nil, 'TestReportTest4', js.FHIRFactoryJs);
  defineTestReportTestPropsJs(js, def);
end;


procedure defineTestReportTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTestAction4', 'operation', '@TestReport.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReportTestAction4', 'assert', '@TestReport.setup.action.assert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTestAction4', nil, 'TestReportTestAction4', js.FHIRFactoryJs);
  defineTestReportTestActionPropsJs(js, def);
end;


procedure defineTestReportTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTeardown4', 'action', 'TestReportTeardownAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTeardown4', nil, 'TestReportTeardown4', js.FHIRFactoryJs);
  defineTestReportTeardownPropsJs(js, def);
end;


procedure defineTestReportTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTeardownAction4', 'operation', '@TestReport.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTeardownAction4', nil, 'TestReportTeardownAction4', js.FHIRFactoryJs);
  defineTestReportTeardownActionPropsJs(js, def);
end;


procedure defineTestReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'TestReport4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport4', 'testScript', 'Reference(TestScript)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport4', 'result', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport4', 'score', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TestReport4', 'tester', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport4', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TestReport4', 'participant', 'TestReportParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestReport4', 'setup', 'TestReportSetup', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport4', 'test', 'TestReportTest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestReport4', 'teardown', 'TestReportTeardown', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReport4', nil, 'TestReport4', js.FHIRFactoryJs);
  defineTestReportPropsJs(js, def);
end;


procedure defineTestScriptOriginPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptOrigin4', 'index', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptOrigin4', 'profile', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptOriginJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptOrigin4', nil, 'TestScriptOrigin4', js.FHIRFactoryJs);
  defineTestScriptOriginPropsJs(js, def);
end;


procedure defineTestScriptDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptDestination4', 'index', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptDestination4', 'profile', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptDestination4', nil, 'TestScriptDestination4', js.FHIRFactoryJs);
  defineTestScriptDestinationPropsJs(js, def);
end;


procedure defineTestScriptMetadataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadata4', 'link', 'TestScriptMetadataLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScriptMetadata4', 'capability', 'TestScriptMetadataCapability', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptMetadataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadata4', nil, 'TestScriptMetadata4', js.FHIRFactoryJs);
  defineTestScriptMetadataPropsJs(js, def);
end;


procedure defineTestScriptMetadataLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataLink4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataLink4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptMetadataLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataLink4', nil, 'TestScriptMetadataLink4', js.FHIRFactoryJs);
  defineTestScriptMetadataLinkPropsJs(js, def);
end;


procedure defineTestScriptMetadataCapabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataCapability4', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability4', 'validated', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataCapability4', 'destination', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptMetadataCapability4', 'capabilities', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptMetadataCapabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataCapability4', nil, 'TestScriptMetadataCapability4', js.FHIRFactoryJs);
  defineTestScriptMetadataCapabilityPropsJs(js, def);
end;


procedure defineTestScriptFixturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptFixture4', 'autocreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture4', 'autodelete', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture4', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptFixtureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptFixture4', nil, 'TestScriptFixture4', js.FHIRFactoryJs);
  defineTestScriptFixturePropsJs(js, def);
end;


procedure defineTestScriptVariablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptVariable4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'defaultValue', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'headerField', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'hint', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptVariableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptVariable4', nil, 'TestScriptVariable4', js.FHIRFactoryJs);
  defineTestScriptVariablePropsJs(js, def);
end;


procedure defineTestScriptRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRule4', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptRule4', 'param', 'TestScriptRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRule4', nil, 'TestScriptRule4', js.FHIRFactoryJs);
  defineTestScriptRulePropsJs(js, def);
end;


procedure defineTestScriptRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRuleParam4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptRuleParam4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRuleParam4', nil, 'TestScriptRuleParam4', js.FHIRFactoryJs);
  defineTestScriptRuleParamPropsJs(js, def);
end;


procedure defineTestScriptRulesetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRuleset4', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptRuleset4', 'rule', 'TestScriptRulesetRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptRulesetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRuleset4', nil, 'TestScriptRuleset4', js.FHIRFactoryJs);
  defineTestScriptRulesetPropsJs(js, def);
end;


procedure defineTestScriptRulesetRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRulesetRule4', 'ruleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptRulesetRule4', 'param', 'TestScriptRulesetRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptRulesetRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRulesetRule4', nil, 'TestScriptRulesetRule4', js.FHIRFactoryJs);
  defineTestScriptRulesetRulePropsJs(js, def);
end;


procedure defineTestScriptRulesetRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRulesetRuleParam4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptRulesetRuleParam4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptRulesetRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRulesetRuleParam4', nil, 'TestScriptRulesetRuleParam4', js.FHIRFactoryJs);
  defineTestScriptRulesetRuleParamPropsJs(js, def);
end;


procedure defineTestScriptSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetup4', 'action', 'TestScriptSetupAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetup4', nil, 'TestScriptSetup4', js.FHIRFactoryJs);
  defineTestScriptSetupPropsJs(js, def);
end;


procedure defineTestScriptSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupAction4', 'operation', 'TestScriptSetupActionOperation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupAction4', 'assert', 'TestScriptSetupActionAssert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupAction4', nil, 'TestScriptSetupAction4', js.FHIRFactoryJs);
  defineTestScriptSetupActionPropsJs(js, def);
end;


procedure defineTestScriptSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'resource', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'accept', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'destination', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'encodeRequestUrl', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'origin', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'params', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'requestHeader', 'TestScriptSetupActionOperationRequestHeader', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'requestId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'responseId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'targetId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'url', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperation4', nil, 'TestScriptSetupActionOperation4', js.FHIRFactoryJs);
  defineTestScriptSetupActionOperationPropsJs(js, def);
end;


procedure defineTestScriptSetupActionOperationRequestHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader4', 'field', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationRequestHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperationRequestHeader4', nil, 'TestScriptSetupActionOperationRequestHeader4', js.FHIRFactoryJs);
  defineTestScriptSetupActionOperationRequestHeaderPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'direction', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'compareToSourceId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'compareToSourceExpression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'compareToSourcePath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'headerField', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'minimumId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'navigationLinks', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'operator', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'requestMethod', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'requestURL', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'resource', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'response', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'responseCode', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'rule', 'TestScriptSetupActionAssertRule', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'ruleset', 'TestScriptSetupActionAssertRuleset', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'validateProfileId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'warningOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTestScriptSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssert4', nil, 'TestScriptSetupActionAssert4', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRule4', 'ruleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRule4', 'param', 'TestScriptSetupActionAssertRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRule4', nil, 'TestScriptSetupActionAssertRule4', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulePropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleParam4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleParam4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionAssertRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRuleParam4', nil, 'TestScriptSetupActionAssertRuleParam4', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRuleParamPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulesetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleset4', 'rulesetId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleset4', 'rule', 'TestScriptSetupActionAssertRulesetRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRulesetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRuleset4', nil, 'TestScriptSetupActionAssertRuleset4', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulesetRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRule4', 'ruleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRule4', 'param', 'TestScriptSetupActionAssertRulesetRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRulesetRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRulesetRule4', nil, 'TestScriptSetupActionAssertRulesetRule4', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetRulePropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulesetRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRuleParam4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRuleParam4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionAssertRulesetRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRulesetRuleParam4', nil, 'TestScriptSetupActionAssertRulesetRuleParam4', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetRuleParamPropsJs(js, def);
end;


procedure defineTestScriptTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTest4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest4', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest4', 'action', 'TestScriptTestAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTest4', nil, 'TestScriptTest4', js.FHIRFactoryJs);
  defineTestScriptTestPropsJs(js, def);
end;


procedure defineTestScriptTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTestAction4', 'operation', '@TestScript.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptTestAction4', 'assert', '@TestScript.setup.action.assert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTestAction4', nil, 'TestScriptTestAction4', js.FHIRFactoryJs);
  defineTestScriptTestActionPropsJs(js, def);
end;


procedure defineTestScriptTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardown4', 'action', 'TestScriptTeardownAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardown4', nil, 'TestScriptTeardown4', js.FHIRFactoryJs);
  defineTestScriptTeardownPropsJs(js, def);
end;


procedure defineTestScriptTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardownAction4', 'operation', '@TestScript.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardownAction4', nil, 'TestScriptTeardownAction4', js.FHIRFactoryJs);
  defineTestScriptTeardownActionPropsJs(js, def);
end;


procedure defineTestScriptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'TestScript4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScript4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TestScript4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'origin', 'TestScriptOrigin', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'destination', 'TestScriptDestination', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'metadata', 'TestScriptMetadata', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript4', 'fixture', 'TestScriptFixture', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'profile', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'variable', 'TestScriptVariable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'rule', 'TestScriptRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'ruleset', 'TestScriptRuleset', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'setup', 'TestScriptSetup', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript4', 'test', 'TestScriptTest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'teardown', 'TestScriptTeardown', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScript4', nil, 'TestScript4', js.FHIRFactoryJs);
  defineTestScriptPropsJs(js, def);
end;


procedure defineUserSessionStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'UserSessionStatus4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'UserSessionStatus4', 'source', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineUserSessionStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UserSessionStatus4', nil, 'UserSessionStatus4', js.FHIRFactoryJs);
  defineUserSessionStatusPropsJs(js, def);
end;


procedure defineUserSessionContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'UserSessionContext4', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'UserSessionContext4', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UserSessionContext4', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineUserSessionContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UserSessionContext4', nil, 'UserSessionContext4', js.FHIRFactoryJs);
  defineUserSessionContextPropsJs(js, def);
end;


procedure defineUserSessionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'UserSession4', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UserSession4', 'user', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UserSession4', 'status', 'UserSessionStatus', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UserSession4', 'workstation', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UserSession4', 'focus', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'UserSession4', 'created', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'UserSession4', 'expires', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'UserSession4', 'context', 'UserSessionContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineUserSessionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UserSession4', nil, 'UserSession4', js.FHIRFactoryJs);
  defineUserSessionPropsJs(js, def);
end;


procedure defineValueSetComposePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCompose4', 'lockedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetCompose4', 'inactive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetCompose4', 'include', 'ValueSetComposeInclude', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetCompose4', 'exclude', '@ValueSet.compose.include', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCompose4', nil, 'ValueSetCompose4', js.FHIRFactoryJs);
  defineValueSetComposePropsJs(js, def);
end;


procedure defineValueSetComposeIncludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeInclude4', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude4', 'concept', 'ValueSetComposeIncludeConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetComposeInclude4', 'filter', 'ValueSetComposeIncludeFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeInclude4', nil, 'ValueSetComposeInclude4', js.FHIRFactoryJs);
  defineValueSetComposeIncludePropsJs(js, def);
end;


procedure defineValueSetComposeIncludeConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConcept4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept4', 'designation', 'ValueSetComposeIncludeConceptDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConcept4', nil, 'ValueSetComposeIncludeConcept4', js.FHIRFactoryJs);
  defineValueSetComposeIncludeConceptPropsJs(js, def);
end;


procedure defineValueSetComposeIncludeConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation4', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation4', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConceptDesignation4', nil, 'ValueSetComposeIncludeConceptDesignation4', js.FHIRFactoryJs);
  defineValueSetComposeIncludeConceptDesignationPropsJs(js, def);
end;


procedure defineValueSetComposeIncludeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeFilter4', 'property', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter4', 'op', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter4', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeFilter4', nil, 'ValueSetComposeIncludeFilter4', js.FHIRFactoryJs);
  defineValueSetComposeIncludeFilterPropsJs(js, def);
end;


procedure defineValueSetExpansionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansion4', 'identifier', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansion4', 'timestamp', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetExpansion4', 'total', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion4', 'offset', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion4', 'parameter', 'ValueSetExpansionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansion4', 'contains', 'ValueSetExpansionContains', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansion4', nil, 'ValueSetExpansion4', js.FHIRFactoryJs);
  defineValueSetExpansionPropsJs(js, def);
end;


procedure defineValueSetExpansionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetExpansionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionParameter4', nil, 'ValueSetExpansionParameter4', js.FHIRFactoryJs);
  defineValueSetExpansionParameterPropsJs(js, def);
end;


procedure defineValueSetExpansionContainsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionContains4', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'abstract', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'inactive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'designation', '@ValueSet.compose.include.concept.designation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'contains', '@ValueSet.expansion.contains', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetExpansionContainsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionContains4', nil, 'ValueSetExpansionContains4', js.FHIRFactoryJs);
  defineValueSetExpansionContainsPropsJs(js, def);
end;


procedure defineValueSetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ValueSet4', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet4', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet4', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSet4', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet4', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet4', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet4', 'immutable', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet4', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'extensible', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet4', 'compose', 'ValueSetCompose', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSet4', 'expansion', 'ValueSetExpansion', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineValueSetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSet4', nil, 'ValueSet4', js.FHIRFactoryJs);
  defineValueSetPropsJs(js, def);
end;


procedure defineVerificationResultPrimarySourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'validationProcess', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'validationStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'validationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'canPushUpdates', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'pushTypeAvailable', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVerificationResultPrimarySourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultPrimarySource4', nil, 'VerificationResultPrimarySource4', js.FHIRFactoryJs);
  defineVerificationResultPrimarySourcePropsJs(js, def);
end;


procedure defineVerificationResultAttestationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultAttestation4', 'source', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'sourceIdentityCertificate', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'proxyIdentityCertificate', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'signedProxyRightString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'signedProxyRightUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'signedSourceAttestationString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'signedSourceAttestationUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineVerificationResultAttestationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultAttestation4', nil, 'VerificationResultAttestation4', js.FHIRFactoryJs);
  defineVerificationResultAttestationPropsJs(js, def);
end;


procedure defineVerificationResultValidatorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultValidator4', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultValidator4', 'identityCertificate', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultValidator4', 'signedValidatorAttestationString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultValidator4', 'signedValidatorAttestationUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineVerificationResultValidatorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultValidator4', nil, 'VerificationResultValidator4', js.FHIRFactoryJs);
  defineVerificationResultValidatorPropsJs(js, def);
end;


procedure defineVerificationResultPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VerificationResult4', 'target', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult4', 'need', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResult4', 'statusDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult4', 'validationType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult4', 'validationProcess', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult4', 'frequency', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult4', 'lastPerformed', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult4', 'nextScheduled', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult4', 'failureAction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult4', 'primarySource', 'VerificationResultPrimarySource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult4', 'attestation', 'VerificationResultAttestation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult4', 'validator', 'VerificationResultValidator', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVerificationResultJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResult4', nil, 'VerificationResult4', js.FHIRFactoryJs);
  defineVerificationResultPropsJs(js, def);
end;


procedure defineVisionPrescriptionDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'product', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'eye', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'sphere', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'cylinder', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'axis', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'prism', 'VisionPrescriptionDispensePrism', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'add', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'power', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'backCurve', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'diameter', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'duration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'color', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'brand', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense4', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVisionPrescriptionDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescriptionDispense4', nil, 'VisionPrescriptionDispense4', js.FHIRFactoryJs);
  defineVisionPrescriptionDispensePropsJs(js, def);
end;


procedure defineVisionPrescriptionDispensePrismPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionDispensePrism4', 'amount', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispensePrism4', 'base', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineVisionPrescriptionDispensePrismJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescriptionDispensePrism4', nil, 'VisionPrescriptionDispensePrism4', js.FHIRFactoryJs);
  defineVisionPrescriptionDispensePrismPropsJs(js, def);
end;


procedure defineVisionPrescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VisionPrescription4', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VisionPrescription4', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescription4', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription4', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription4', 'dateWritten', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VisionPrescription4', 'prescriber', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription4', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription4', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription4', 'dispense', 'VisionPrescriptionDispense', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVisionPrescriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescription4', nil, 'VisionPrescription4pp', js.FHIRFactoryJs);
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
  defineChargeItemPerformerJs(js); 
  defineChargeItemJs(js); 
  defineChargeItemDefinitionApplicabilityJs(js); 
  defineChargeItemDefinitionPropertyGroupJs(js); 
  defineChargeItemDefinitionPropertyGroupPriceComponentJs(js); 
  defineChargeItemDefinitionJs(js); 
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
  defineClaimResponseTotalJs(js); 
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
  defineEligibilityRequestAuthorizationDiagnosisJs(js); 
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
  defineHealthcareServiceAvailableTimeJs(js); 
  defineHealthcareServiceNotAvailableJs(js); 
  defineHealthcareServiceJs(js); 
  defineImagingStudySeriesJs(js); 
  defineImagingStudySeriesPerformerJs(js); 
  defineImagingStudySeriesInstanceJs(js); 
  defineImagingStudyJs(js); 
  defineImmunizationPerformerJs(js); 
  defineImmunizationEducationJs(js); 
  defineImmunizationJs(js); 
  defineImmunizationEvaluationJs(js); 
  defineImmunizationRecommendationRecommendationJs(js); 
  defineImmunizationRecommendationRecommendationDateCriterionJs(js); 
  defineImmunizationRecommendationJs(js); 
  defineImplementationGuideDependsOnJs(js); 
  defineImplementationGuideGlobalJs(js); 
  defineImplementationGuideDefinitionJs(js); 
  defineImplementationGuideDefinitionPackageJs(js); 
  defineImplementationGuideDefinitionResourceJs(js); 
  defineImplementationGuideDefinitionPageJs(js); 
  defineImplementationGuideDefinitionParameterJs(js); 
  defineImplementationGuideDefinitionTemplateJs(js); 
  defineImplementationGuideManifestJs(js); 
  defineImplementationGuideManifestResourceJs(js); 
  defineImplementationGuideManifestPageJs(js); 
  defineImplementationGuideJs(js); 
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
  defineMedicationKnowledgeMonographJs(js); 
  defineMedicationKnowledgeIngredientJs(js); 
  defineMedicationKnowledgeCostJs(js); 
  defineMedicationKnowledgeMonitoringProgramJs(js); 
  defineMedicationKnowledgeAdministrationGuidelinesJs(js); 
  defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsJs(js); 
  defineMedicationKnowledgeMedicineClassificationJs(js); 
  defineMedicationKnowledgePackagingJs(js); 
  defineMedicationKnowledgeDrugCharacteristicJs(js); 
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
  defineOccupationalDataUsualWorkJs(js); 
  defineOccupationalDataPastOrPresentJobJs(js); 
  defineOccupationalDataPastOrPresentJobWorkScheduleJs(js); 
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
  defineProductPlanCoverageBenefitLimitJs(js); 
  defineProductPlanPlanJs(js); 
  defineProductPlanPlanGeneralCostJs(js); 
  defineProductPlanPlanSpecificCostJs(js); 
  defineProductPlanPlanSpecificCostBenefitJs(js); 
  defineProductPlanPlanSpecificCostBenefitCostJs(js); 
  defineProductPlanJs(js); 
  defineProvenanceAgentJs(js); 
  defineProvenanceEntityJs(js); 
  defineProvenanceJs(js); 
  defineQuestionnaireItemJs(js); 
  defineQuestionnaireItemEnableWhenJs(js); 
  defineQuestionnaireItemOptionJs(js); 
  defineQuestionnaireItemInitialJs(js); 
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
  defineVisionPrescriptionDispensePrismJs(js); 
  defineVisionPrescriptionJs(js); 

end;

end.

