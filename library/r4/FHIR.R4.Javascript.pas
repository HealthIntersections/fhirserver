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

// FHIR v4.0.0 generated 2018-12-27T22:37:54+11:00

uses
  FHIR.Javascript, FHIR.Javascript.Base;

procedure registerFHIRTypes(js : TFHIRJavascript);

implementation

procedure defineElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Element4', 'id', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Element4', 'extension', 'Extension4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineBackboneElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'BackboneElement4', 'modifierExtension', 'Extension4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineBackboneTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'BackboneType4', 'modifierExtension', 'Extension4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Resource4', 'id', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Resource4', 'meta', 'Meta4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Resource4', 'implicitRules', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Resource4', 'language', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;


procedure defineDomainResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'DomainResource4', 'text', 'Narrative4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DomainResource4', 'contained', 'Resource4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DomainResource4', 'extension', 'Extension4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DomainResource4', 'modifierExtension', 'Extension4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineParametersParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ParametersParameter4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ParametersParameter4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ParametersParameter4', 'valueCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ParametersParameter4', 'valueId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParametersParameter4', 'valueMarkdown', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueOid', 'oid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valuePositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParametersParameter4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueUrl', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueUuid', 'uuid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter4', 'valueAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueAnnotation', 'Annotation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueContactPoint', 'ContactPoint4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueCount', 'Count4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDistance', 'Distance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueHumanName', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valuePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueContactDetail', 'ContactDetail4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueContributor', 'Contributor4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueParameterDefinition', 'ParameterDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueRelatedArtifact', 'RelatedArtifact4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueTriggerDefinition', 'TriggerDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueUsageContext', 'UsageContext4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'valueDosage', 'Dosage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'resource', 'Resource4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter4', 'part', '@Parameters.parameter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParametersParameter4', nil, js.FHIRFactoryJs);
  defineParametersParameterPropsJs(js, def);
end;


procedure defineParametersPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Parameters4', 'parameter', 'ParametersParameter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Parameters4', nil, js.FHIRFactoryJs);
  defineParametersPropsJs(js, def);
end;


procedure defineMetadataResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
end;


procedure defineExtensionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Extension4', 'url', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Extension4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Extension4', 'valueCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Extension4', 'valueId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Extension4', 'valueMarkdown', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueOid', 'oid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valuePositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Extension4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueUrl', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueUuid', 'uuid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension4', 'valueAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueAnnotation', 'Annotation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueContactPoint', 'ContactPoint4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueCount', 'Count4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueDistance', 'Distance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueHumanName', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valuePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueContactDetail', 'ContactDetail4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueContributor', 'Contributor4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueParameterDefinition', 'ParameterDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueRelatedArtifact', 'RelatedArtifact4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueTriggerDefinition', 'TriggerDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueUsageContext', 'UsageContext4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension4', 'valueDosage', 'Dosage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExtensionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Extension4', nil, js.FHIRFactoryJs);
  defineExtensionPropsJs(js, def);
end;


procedure defineNarrativePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Narrative4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Narrative4', 'div_', 'xhtml4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNarrativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Narrative4', nil, js.FHIRFactoryJs);
  defineNarrativePropsJs(js, def);
end;


procedure defineContributorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Contributor4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contributor4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contributor4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContributorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contributor4', nil, js.FHIRFactoryJs);
  defineContributorPropsJs(js, def);
end;


procedure defineAttachmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Attachment4', 'contentType', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment4', 'language', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment4', 'data', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Attachment4', 'url', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment4', 'size', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment4', 'hash', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Attachment4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment4', 'creation', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineAttachmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Attachment4', nil, js.FHIRFactoryJs);
  defineAttachmentPropsJs(js, def);
end;


procedure defineDataRequirementCodeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementCodeFilter4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter4', 'searchParam', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter4', 'valueSet', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter4', 'code', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataRequirementCodeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementCodeFilter4', nil, js.FHIRFactoryJs);
  defineDataRequirementCodeFilterPropsJs(js, def);
end;


procedure defineDataRequirementDateFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementDateFilter4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementDateFilter4', 'searchParam', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementDateFilter4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DataRequirementDateFilter4', 'valuePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirementDateFilter4', 'valueDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDataRequirementDateFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementDateFilter4', nil, js.FHIRFactoryJs);
  defineDataRequirementDateFilterPropsJs(js, def);
end;


procedure defineDataRequirementSortPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementSort4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementSort4', 'direction', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDataRequirementSortJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementSort4', nil, js.FHIRFactoryJs);
  defineDataRequirementSortPropsJs(js, def);
end;


procedure defineDataRequirementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirement4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirement4', 'subjectCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirement4', 'subjectReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirement4', 'codeFilter', 'DataRequirementCodeFilter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataRequirement4', 'dateFilter', 'DataRequirementDateFilter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataRequirement4', 'limit', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'DataRequirement4', 'sort', 'DataRequirementSort4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataRequirementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirement4', nil, js.FHIRFactoryJs);
  defineDataRequirementPropsJs(js, def);
end;


procedure defineDosageDoseAndRatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DosageDoseAndRate4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate4', 'doseRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate4', 'doseQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate4', 'rateRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate4', 'rateRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate4', 'rateQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDosageDoseAndRateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DosageDoseAndRate4', nil, js.FHIRFactoryJs);
  defineDosageDoseAndRatePropsJs(js, def);
end;


procedure defineDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'Dosage4', 'sequence', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Dosage4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Dosage4', 'additionalInstruction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Dosage4', 'patientInstruction', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Dosage4', 'timing', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'asNeededBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Dosage4', 'asNeededCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'site', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'route', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'method', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'doseAndRate', 'DosageDoseAndRate4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Dosage4', 'maxDosePerPeriod', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'maxDosePerAdministration', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage4', 'maxDosePerLifetime', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Dosage4', nil, js.FHIRFactoryJs);
  defineDosagePropsJs(js, def);
end;


procedure defineMoneyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Money4', 'value', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Money4', 'currency', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMoneyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Money4', nil, js.FHIRFactoryJs);
  defineMoneyPropsJs(js, def);
end;


procedure defineMarketingStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'MarketingStatus4', 'country', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus4', 'jurisdiction', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus4', 'status', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus4', 'dateRange', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus4', 'restoreDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineMarketingStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MarketingStatus4', nil, js.FHIRFactoryJs);
  defineMarketingStatusPropsJs(js, def);
end;


procedure defineIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Identifier4', 'use', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Identifier4', 'system', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Identifier4', 'assigner', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Identifier4', nil, js.FHIRFactoryJs);
  defineIdentifierPropsJs(js, def);
end;


procedure defineSubstanceAmountReferenceRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceAmountReferenceRange4', 'lowLimit', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmountReferenceRange4', 'highLimit', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceAmountReferenceRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceAmountReferenceRange4', nil, js.FHIRFactoryJs);
  defineSubstanceAmountReferenceRangePropsJs(js, def);
end;


procedure defineSubstanceAmountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'SubstanceAmount4', 'amountQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount4', 'amountRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount4', 'amountString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceAmount4', 'amountType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount4', 'amountText', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceAmount4', 'referenceRange', 'SubstanceAmountReferenceRange4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceAmountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceAmount4', nil, js.FHIRFactoryJs);
  defineSubstanceAmountPropsJs(js, def);
end;


procedure defineCodingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Coding4', 'system', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding4', 'userSelected', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineCodingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coding4', nil, js.FHIRFactoryJs);
  defineCodingPropsJs(js, def);
end;


procedure defineSampledDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SampledData4', 'origin', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SampledData4', 'period', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData4', 'lowerLimit', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData4', 'upperLimit', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData4', 'dimensions', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SampledData4', 'data', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSampledDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SampledData4', nil, js.FHIRFactoryJs);
  defineSampledDataPropsJs(js, def);
end;


procedure definePopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'Population4', 'ageRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Population4', 'ageCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Population4', 'gender', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Population4', 'race', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Population4', 'physiologicalCondition', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Population4', nil, js.FHIRFactoryJs);
  definePopulationPropsJs(js, def);
end;


procedure defineRatioPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Ratio4', 'numerator', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Ratio4', 'denominator', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRatioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Ratio4', nil, js.FHIRFactoryJs);
  defineRatioPropsJs(js, def);
end;


procedure defineReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Reference4', 'reference', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Reference4', 'type_', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Reference4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Reference4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Reference4', nil, js.FHIRFactoryJs);
  defineReferencePropsJs(js, def);
end;


procedure defineTriggerDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TriggerDefinition4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinition4', 'timingTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition4', 'timingReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition4', 'timingDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TriggerDefinition4', 'timingDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TriggerDefinition4', 'data', 'DataRequirement4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TriggerDefinition4', 'condition', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTriggerDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TriggerDefinition4', nil, js.FHIRFactoryJs);
  defineTriggerDefinitionPropsJs(js, def);
end;


procedure definePeriodPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Period4', 'start', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Period4', 'end_', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure definePeriodJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Period4', nil, js.FHIRFactoryJs);
  definePeriodPropsJs(js, def);
end;


procedure defineQuantityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Quantity4', 'value', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Quantity4', 'comparator', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity4', 'unit_', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity4', 'system', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineQuantityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Quantity4', nil, js.FHIRFactoryJs);
  defineQuantityPropsJs(js, def);
end;


procedure defineRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Range4', 'low', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Range4', 'high', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Range4', nil, js.FHIRFactoryJs);
  defineRangePropsJs(js, def);
end;


procedure defineRelatedArtifactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'RelatedArtifact4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact4', 'label_', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact4', 'citation', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact4', 'url', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact4', 'document', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedArtifact4', 'resource', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRelatedArtifactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedArtifact4', nil, js.FHIRFactoryJs);
  defineRelatedArtifactPropsJs(js, def);
end;


procedure defineAnnotationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Annotation4', 'authorReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Annotation4', 'authorString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Annotation4', 'time', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Annotation4', 'text', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAnnotationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Annotation4', nil, js.FHIRFactoryJs);
  defineAnnotationPropsJs(js, def);
end;


procedure defineProductShelfLifePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'ProductShelfLife4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife4', 'period', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife4', 'specialPrecautionsForStorage', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductShelfLifeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductShelfLife4', nil, js.FHIRFactoryJs);
  defineProductShelfLifePropsJs(js, def);
end;


procedure defineContactDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactDetail4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactDetail4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContactDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactDetail4', nil, js.FHIRFactoryJs);
  defineContactDetailPropsJs(js, def);
end;


procedure defineExpressionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Expression4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Expression4', 'name', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Expression4', 'language', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Expression4', 'expression', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Expression4', 'reference', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExpressionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Expression4', nil, js.FHIRFactoryJs);
  defineExpressionPropsJs(js, def);
end;


procedure defineUsageContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'UsageContext4', 'code', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineUsageContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UsageContext4', nil, js.FHIRFactoryJs);
  defineUsageContextPropsJs(js, def);
end;


procedure defineSignaturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Signature4', 'type', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Signature4', 'when', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Signature4', 'who', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature4', 'onBehalfOf', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature4', 'targetFormat', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature4', 'sigFormat', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature4', 'data', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineSignatureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Signature4', nil, js.FHIRFactoryJs);
  defineSignaturePropsJs(js, def);
end;


procedure defineProdCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'ProdCharacteristic4', 'height', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'width', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'depth', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'weight', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'nominalVolume', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'externalDiameter', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic4', 'shape', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProdCharacteristic4', 'image', 'Attachment4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProdCharacteristic4', 'scoring', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProdCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProdCharacteristic4', nil, js.FHIRFactoryJs);
  defineProdCharacteristicPropsJs(js, def);
end;


procedure defineCodeableConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'CodeableConcept4', 'coding', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeableConcept4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeableConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeableConcept4', nil, js.FHIRFactoryJs);
  defineCodeableConceptPropsJs(js, def);
end;


procedure defineParameterDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ParameterDefinition4', 'name', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition4', 'use', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition4', 'min', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParameterDefinition4', 'max', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition4', 'documentation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition4', 'profile', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineParameterDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParameterDefinition4', nil, js.FHIRFactoryJs);
  defineParameterDefinitionPropsJs(js, def);
end;


procedure defineContactPointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactPoint4', 'system', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint4', 'use', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint4', 'rank', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ContactPoint4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContactPointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactPoint4', nil, js.FHIRFactoryJs);
  defineContactPointPropsJs(js, def);
end;


procedure defineHumanNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'HumanName4', 'use', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName4', 'family', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineHumanNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HumanName4', nil, js.FHIRFactoryJs);
  defineHumanNamePropsJs(js, def);
end;


procedure defineMetaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Meta4', 'versionId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Meta4', 'lastUpdated', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Meta4', 'source', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Meta4', 'security', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Meta4', 'tag', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMetaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Meta4', nil, js.FHIRFactoryJs);
  defineMetaPropsJs(js, def);
end;


procedure defineAddressPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Address4', 'use', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'city', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'district', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'state', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'postalCode', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'country', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAddressJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Address4', nil, js.FHIRFactoryJs);
  defineAddressPropsJs(js, def);
end;


procedure defineElementDefinitionSlicingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicing4', 'discriminator', 'ElementDefinitionSlicingDiscriminator4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinitionSlicing4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicing4', 'ordered', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionSlicing4', 'rules', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicing4', nil, js.FHIRFactoryJs);
  defineElementDefinitionSlicingPropsJs(js, def);
end;


procedure defineElementDefinitionSlicingDiscriminatorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicingDiscriminator4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicingDiscriminator4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingDiscriminatorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicingDiscriminator4', nil, js.FHIRFactoryJs);
  defineElementDefinitionSlicingDiscriminatorPropsJs(js, def);
end;


procedure defineElementDefinitionBasePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBase4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase4', 'min', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase4', 'max', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionBaseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBase4', nil, js.FHIRFactoryJs);
  defineElementDefinitionBasePropsJs(js, def);
end;


procedure defineElementDefinitionTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionType4', 'code', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionType4', 'versioning', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionTypeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionType4', nil, js.FHIRFactoryJs);
  defineElementDefinitionTypePropsJs(js, def);
end;


procedure defineElementDefinitionExamplePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionExample4', 'label_', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueMarkdown', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueOid', 'oid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valuePositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueUrl', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueUuid', 'uuid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueAnnotation', 'Annotation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueContactPoint', 'ContactPoint4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueCount', 'Count4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDistance', 'Distance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueHumanName', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valuePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueContactDetail', 'ContactDetail4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueContributor', 'Contributor4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueParameterDefinition', 'ParameterDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueRelatedArtifact', 'RelatedArtifact4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueTriggerDefinition', 'TriggerDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueUsageContext', 'UsageContext4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample4', 'valueDosage', 'Dosage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineElementDefinitionExampleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionExample4', nil, js.FHIRFactoryJs);
  defineElementDefinitionExamplePropsJs(js, def);
end;


procedure defineElementDefinitionConstraintPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'key', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'requirements', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'severity', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'human', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'expression', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'xpath', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint4', 'source', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionConstraintJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionConstraint4', nil, js.FHIRFactoryJs);
  defineElementDefinitionConstraintPropsJs(js, def);
end;


procedure defineElementDefinitionBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBinding4', 'strength', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding4', 'valueSet', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBinding4', nil, js.FHIRFactoryJs);
  defineElementDefinitionBindingPropsJs(js, def);
end;


procedure defineElementDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionMapping4', 'identity', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping4', 'language', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping4', 'map', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionMapping4', nil, js.FHIRFactoryJs);
  defineElementDefinitionMappingPropsJs(js, def);
end;


procedure defineElementDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'ElementDefinition4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'sliceName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'sliceIsConstraining', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'label_', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'code', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition4', 'slicing', 'ElementDefinitionSlicing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'short', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'definition', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'comment', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'requirements', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'min', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'max', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'base', 'ElementDefinitionBase4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'contentReference', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'type', 'ElementDefinitionType4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueMarkdown', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueOid', 'oid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValuePositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueUrl', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueUuid', 'uuid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueAnnotation', 'Annotation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueContactPoint', 'ContactPoint4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueCount', 'Count4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDistance', 'Distance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueHumanName', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValuePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueContactDetail', 'ContactDetail4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueContributor', 'Contributor4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueParameterDefinition', 'ParameterDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueRelatedArtifact', 'RelatedArtifact4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueTriggerDefinition', 'TriggerDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueUsageContext', 'UsageContext4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'defaultValueDosage', 'Dosage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'meaningWhenMissing', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'orderMeaning', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedMarkdown', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedOid', 'oid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedPositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedUrl', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedUuid', 'uuid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedAnnotation', 'Annotation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedContactPoint', 'ContactPoint4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedCount', 'Count4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDistance', 'Distance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedHumanName', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedContactDetail', 'ContactDetail4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedContributor', 'Contributor4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedParameterDefinition', 'ParameterDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedRelatedArtifact', 'RelatedArtifact4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedTriggerDefinition', 'TriggerDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedUsageContext', 'UsageContext4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'fixedDosage', 'Dosage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition4', 'patternBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'patternCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition4', 'patternId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'patternInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'patternMarkdown', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternOid', 'oid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternPositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'patternString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternUrl', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternUuid', 'uuid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'patternAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternAnnotation', 'Annotation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternContactPoint', 'ContactPoint4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternCount', 'Count4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDistance', 'Distance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternHumanName', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternContactDetail', 'ContactDetail4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternContributor', 'Contributor4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternParameterDefinition', 'ParameterDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternRelatedArtifact', 'RelatedArtifact4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternTriggerDefinition', 'TriggerDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternUsageContext', 'UsageContext4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'patternDosage', 'Dosage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'example', 'ElementDefinitionExample4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'minValuePositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'minValueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValuePositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'maxValueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'maxLength', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition4', 'constraint', 'ElementDefinitionConstraint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition4', 'mustSupport', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'isModifier', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'isModifierReason', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition4', 'isSummary', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition4', 'binding', 'ElementDefinitionBinding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition4', 'mapping', 'ElementDefinitionMapping4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineElementDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinition4', nil, js.FHIRFactoryJs);
  defineElementDefinitionPropsJs(js, def);
end;


procedure defineTimingRepeatPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TimingRepeat4', 'boundsDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat4', 'boundsRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat4', 'boundsPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat4', 'count', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat4', 'countMax', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat4', 'duration', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat4', 'durationMax', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat4', 'durationUnit', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat4', 'frequency', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat4', 'frequencyMax', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat4', 'period', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat4', 'periodMax', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat4', 'periodUnit', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat4', 'offset', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTimingRepeatJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TimingRepeat4', nil, js.FHIRFactoryJs);
  defineTimingRepeatPropsJs(js, def);
end;


procedure defineTimingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'Timing4', 'repeat', 'TimingRepeat4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Timing4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTimingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Timing4', nil, js.FHIRFactoryJs);
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
  def := js.defineClass('Count4', nil, js.FHIRFactoryJs);
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
  def := js.defineClass('Age4', nil, js.FHIRFactoryJs);
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
  def := js.defineClass('Distance4', nil, js.FHIRFactoryJs);
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
  def := js.defineClass('Duration4', nil, js.FHIRFactoryJs);
  defineDurationPropsJs(js, def);
end;


procedure defineAccountCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AccountCoverage4', 'coverage', 'Reference(Coverage)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AccountCoverage4', 'priority', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineAccountCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AccountCoverage4', nil, js.FHIRFactoryJs);
  defineAccountCoveragePropsJs(js, def);
end;


procedure defineAccountGuarantorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AccountGuarantor4', 'party', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AccountGuarantor4', 'onHold', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'AccountGuarantor4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAccountGuarantorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AccountGuarantor4', nil, js.FHIRFactoryJs);
  defineAccountGuarantorPropsJs(js, def);
end;


procedure defineAccountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Account4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account4', 'subject', 'Reference(Patient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account4', 'servicePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account4', 'coverage', 'AccountCoverage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account4', 'owner', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account4', 'guarantor', 'AccountGuarantor4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account4', 'partOf', 'Reference(Account)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAccountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Account4', nil, js.FHIRFactoryJs);
  defineAccountPropsJs(js, def);
end;


procedure defineActivityDefinitionParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ActivityDefinitionParticipant4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionParticipant4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineActivityDefinitionParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinitionParticipant4', nil, js.FHIRFactoryJs);
  defineActivityDefinitionParticipantPropsJs(js, def);
end;


procedure defineActivityDefinitionDynamicValuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ActivityDefinitionDynamicValue4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue4', 'expression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineActivityDefinitionDynamicValueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinitionDynamicValue4', nil, js.FHIRFactoryJs);
  defineActivityDefinitionDynamicValuePropsJs(js, def);
end;


procedure defineActivityDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ActivityDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'subtitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ActivityDefinition4', 'subjectCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'subjectReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'usage', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'kind', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'profile', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'intent', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'doNotPerform', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'timingDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'participant', 'ActivityDefinitionParticipant4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'productReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'productCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition4', 'dosage', 'Dosage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'bodySite', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'specimenRequirement', 'Reference(SpecimenDefinition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'observationRequirement', 'Reference(ObservationDefinition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'observationResultRequirement', 'Reference(ObservationDefinition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition4', 'transform', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition4', 'dynamicValue', 'ActivityDefinitionDynamicValue4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineActivityDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinition4', nil, js.FHIRFactoryJs);
  defineActivityDefinitionPropsJs(js, def);
end;


procedure defineAdverseEventSuspectEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AdverseEventSuspectEntity4', 'instance', 'Reference(Immunization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntity4', 'causality', 'AdverseEventSuspectEntityCausality4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAdverseEventSuspectEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEventSuspectEntity4', nil, js.FHIRFactoryJs);
  defineAdverseEventSuspectEntityPropsJs(js, def);
end;


procedure defineAdverseEventSuspectEntityCausalityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality4', 'assessment', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality4', 'productRelatedness', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality4', 'author', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality4', 'method', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAdverseEventSuspectEntityCausalityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEventSuspectEntityCausality4', nil, js.FHIRFactoryJs);
  defineAdverseEventSuspectEntityCausalityPropsJs(js, def);
end;


procedure defineAdverseEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AdverseEvent4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'actuality', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEvent4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'event', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AdverseEvent4', 'detected', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AdverseEvent4', 'recordedDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AdverseEvent4', 'resultingCondition', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'seriousness', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'severity', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'outcome', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'recorder', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent4', 'contributor', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'suspectEntity', 'AdverseEventSuspectEntity4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'subjectMedicalHistory', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'referenceDocument', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent4', 'study', 'Reference(ResearchStudy)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAdverseEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEvent4', nil, js.FHIRFactoryJs);
  defineAdverseEventPropsJs(js, def);
end;


procedure defineAllergyIntoleranceReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'substance', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'manifestation', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'onset', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'severity', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'exposureRoute', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntoleranceReaction4', nil, js.FHIRFactoryJs);
  defineAllergyIntoleranceReactionPropsJs(js, def);
end;


procedure defineAllergyIntolerancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AllergyIntolerance4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance4', 'clinicalStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'verificationStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance4', 'criticality', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'onsetDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance4', 'onsetAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'onsetPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'onsetRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'onsetString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance4', 'recordedDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance4', 'recorder', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'asserter', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance4', 'lastOccurrence', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance4', 'reaction', 'AllergyIntoleranceReaction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntolerance4', nil, js.FHIRFactoryJs);
  defineAllergyIntolerancePropsJs(js, def);
end;


procedure defineAppointmentParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AppointmentParticipant4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentParticipant4', 'actor', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentParticipant4', 'required', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentParticipant4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentParticipant4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAppointmentParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentParticipant4', nil, js.FHIRFactoryJs);
  defineAppointmentParticipantPropsJs(js, def);
end;


procedure defineAppointmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Appointment4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment4', 'cancelationReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment4', 'serviceCategory', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'serviceType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'specialty', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'appointmentType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'priority', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment4', 'supportingInformation', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'start', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment4', 'end_', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment4', 'minutesDuration', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Appointment4', 'slot', 'Reference(Slot)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment4', 'patientInstruction', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment4', 'basedOn', 'Reference(ServiceRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'participant', 'AppointmentParticipant4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment4', 'requestedPeriod', 'Period4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAppointmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Appointment4', nil, js.FHIRFactoryJs);
  defineAppointmentPropsJs(js, def);
end;


procedure defineAppointmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AppointmentResponse4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse4', 'appointment', 'Reference(Appointment)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse4', 'start', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse4', 'end_', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse4', 'participantType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse4', 'actor', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse4', 'participantStatus', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentResponse4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAppointmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentResponse4', nil, js.FHIRFactoryJs);
  defineAppointmentResponsePropsJs(js, def);
end;


procedure defineAuditEventAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventAgent4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'role', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventAgent4', 'who', 'Reference(PractitionerRole)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'altId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgent4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgent4', 'requestor', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'AuditEventAgent4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'media', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'network', 'AuditEventAgentNetwork4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent4', 'purposeOfUse', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventAgent4', nil, js.FHIRFactoryJs);
  defineAuditEventAgentPropsJs(js, def);
end;


procedure defineAuditEventAgentNetworkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventAgentNetwork4', 'address', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgentNetwork4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAuditEventAgentNetworkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventAgentNetwork4', nil, js.FHIRFactoryJs);
  defineAuditEventAgentNetworkPropsJs(js, def);
end;


procedure defineAuditEventSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventSource4', 'site', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventSource4', 'observer', 'Reference(PractitionerRole)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventSource4', 'type', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventSource4', nil, js.FHIRFactoryJs);
  defineAuditEventSourcePropsJs(js, def);
end;


procedure defineAuditEventEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEntity4', 'what', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity4', 'type', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity4', 'role', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity4', 'lifecycle', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity4', 'securityLabel', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventEntity4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntity4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntity4', 'query', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'AuditEventEntity4', 'detail', 'AuditEventEntityDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEntity4', nil, js.FHIRFactoryJs);
  defineAuditEventEntityPropsJs(js, def);
end;


procedure defineAuditEventEntityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEntityDetail4', 'type_', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntityDetail4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntityDetail4', 'valueBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineAuditEventEntityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEntityDetail4', nil, js.FHIRFactoryJs);
  defineAuditEventEntityDetailPropsJs(js, def);
end;


procedure defineAuditEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AuditEvent4', 'type', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent4', 'subtype', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent4', 'action', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent4', 'recorded', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AuditEvent4', 'outcome', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent4', 'outcomeDesc', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent4', 'purposeOfEvent', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent4', 'agent', 'AuditEventAgent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent4', 'source', 'AuditEventSource4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent4', 'entity', 'AuditEventEntity4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEvent4', nil, js.FHIRFactoryJs);
  defineAuditEventPropsJs(js, def);
end;


procedure defineBasicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Basic4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Basic4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic4', 'subject', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic4', 'created', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Basic4', 'author', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBasicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Basic4', nil, js.FHIRFactoryJs);
  defineBasicPropsJs(js, def);
end;


procedure defineBinaryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Binary4', 'contentType', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Binary4', 'securityContext', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Binary4', 'data', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineBinaryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Binary4', nil, js.FHIRFactoryJs);
  defineBinaryPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductCollectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductCollection4', 'collector', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection4', 'source', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection4', 'collectedDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection4', 'collectedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductCollectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductCollection4', nil, js.FHIRFactoryJs);
  defineBiologicallyDerivedProductCollectionPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductProcessingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing4', 'procedure', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing4', 'additive', 'Reference(Substance)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing4', 'timeDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing4', 'timePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductProcessingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductProcessing4', nil, js.FHIRFactoryJs);
  defineBiologicallyDerivedProductProcessingPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductManipulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation4', 'timeDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation4', 'timePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductManipulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductManipulation4', nil, js.FHIRFactoryJs);
  defineBiologicallyDerivedProductManipulationPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductStoragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductStorage4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage4', 'temperature', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage4', 'scale', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage4', 'duration', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductStorageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductStorage4', nil, js.FHIRFactoryJs);
  defineBiologicallyDerivedProductStoragePropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'productCategory', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'productCode', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'request', 'Reference(ServiceRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'quantity', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'parent', 'Reference(BiologicallyDerivedProduct)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'collection', 'BiologicallyDerivedProductCollection4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'processing', 'BiologicallyDerivedProductProcessing4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'manipulation', 'BiologicallyDerivedProductManipulation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct4', 'storage', 'BiologicallyDerivedProductStorage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineBiologicallyDerivedProductJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProduct4', nil, js.FHIRFactoryJs);
  defineBiologicallyDerivedProductPropsJs(js, def);
end;


procedure defineBodyStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BodyStructure4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'BodyStructure4', 'morphology', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BodyStructure4', 'location', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BodyStructure4', 'locationQualifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BodyStructure4', 'image', 'Attachment4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBodyStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BodyStructure4', nil, js.FHIRFactoryJs);
  defineBodyStructurePropsJs(js, def);
end;


procedure defineBundleLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleLink4', 'relation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleLink4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineBundleLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleLink4', nil, js.FHIRFactoryJs);
  defineBundleLinkPropsJs(js, def);
end;


procedure defineBundleEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntry4', 'link', '@Bundle.link4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BundleEntry4', 'fullUrl', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntry4', 'resource', 'Resource4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry4', 'search', 'BundleEntrySearch4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry4', 'request', 'BundleEntryRequest4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry4', 'response', 'BundleEntryResponse4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntry4', nil, js.FHIRFactoryJs);
  defineBundleEntryPropsJs(js, def);
end;


procedure defineBundleEntrySearchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntrySearch4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntrySearch4', 'score', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineBundleEntrySearchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntrySearch4', nil, js.FHIRFactoryJs);
  defineBundleEntrySearchPropsJs(js, def);
end;


procedure defineBundleEntryRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryRequest4', 'method', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest4', 'ifNoneMatch', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest4', 'ifModifiedSince', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryRequest4', 'ifMatch', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest4', 'ifNoneExist', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineBundleEntryRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryRequest4', nil, js.FHIRFactoryJs);
  defineBundleEntryRequestPropsJs(js, def);
end;


procedure defineBundleEntryResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryResponse4', 'status', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse4', 'location', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse4', 'etag', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse4', 'lastModified', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryResponse4', 'outcome', 'Resource4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleEntryResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryResponse4', nil, js.FHIRFactoryJs);
  defineBundleEntryResponsePropsJs(js, def);
end;


procedure defineBundlePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Bundle4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Bundle4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Bundle4', 'timestamp', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Bundle4', 'total', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Bundle4', 'link', 'BundleLink4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Bundle4', 'entry', 'BundleEntry4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Bundle4', 'signature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Bundle4', nil, js.FHIRFactoryJs);
  defineBundlePropsJs(js, def);
end;


procedure defineCapabilityStatementSoftwarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementSoftware4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementSoftware4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementSoftware4', 'releaseDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineCapabilityStatementSoftwareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementSoftware4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementSoftwarePropsJs(js, def);
end;


procedure defineCapabilityStatementImplementationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementImplementation4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementImplementation4', 'url', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementImplementation4', 'custodian', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCapabilityStatementImplementationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementImplementation4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementImplementationPropsJs(js, def);
end;


procedure defineCapabilityStatementRestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRest4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'documentation', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'security', 'CapabilityStatementRestSecurity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'resource', 'CapabilityStatementRestResource4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'interaction', 'CapabilityStatementRestInteraction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'searchParam', '@CapabilityStatement.rest.resource.searchParam4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest4', 'operation', '@CapabilityStatement.rest.resource.operation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRest4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestPropsJs(js, def);
end;


procedure defineCapabilityStatementRestSecurityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestSecurity4', 'cors', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity4', 'service', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestSecurityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestSecurity4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestSecurityPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'profile', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'documentation', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'interaction', 'CapabilityStatementRestResourceInteraction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'versioning', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'readHistory', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'updateCreate', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'conditionalCreate', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'conditionalRead', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'conditionalUpdate', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'conditionalDelete', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'searchParam', 'CapabilityStatementRestResourceSearchParam4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestResource4', 'operation', 'CapabilityStatementRestResourceOperation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResource4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestResourcePropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction4', 'documentation', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceInteraction4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceInteractionPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceSearchParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam4', 'definition', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam4', 'documentation', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceSearchParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceSearchParam4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceSearchParamPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation4', 'definition', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation4', 'documentation', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceOperation4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceOperationPropsJs(js, def);
end;


procedure defineCapabilityStatementRestInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestInteraction4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestInteraction4', 'documentation', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestInteraction4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestInteractionPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessaging4', 'endpoint', 'CapabilityStatementMessagingEndpoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementMessaging4', 'reliableCache', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessaging4', 'documentation', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessaging4', 'supportedMessage', 'CapabilityStatementMessagingSupportedMessage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementMessagingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessaging4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementMessagingPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint4', 'protocol', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint4', 'address', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementMessagingEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingEndpoint4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementMessagingEndpointPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingSupportedMessagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage4', 'definition', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementMessagingSupportedMessageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingSupportedMessage4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementMessagingSupportedMessagePropsJs(js, def);
end;


procedure defineCapabilityStatementDocumentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementDocument4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument4', 'documentation', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument4', 'profile', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementDocumentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementDocument4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementDocumentPropsJs(js, def);
end;


procedure defineCapabilityStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CapabilityStatement4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatement4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CapabilityStatement4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'kind', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'software', 'CapabilityStatementSoftware4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatement4', 'implementation', 'CapabilityStatementImplementation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatement4', 'fhirVersion', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement4', 'rest', 'CapabilityStatementRest4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement4', 'messaging', 'CapabilityStatementMessaging4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement4', 'document', 'CapabilityStatementDocument4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatement4', nil, js.FHIRFactoryJs);
  defineCapabilityStatementPropsJs(js, def);
end;


procedure defineCarePlanActivityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivity4', 'outcomeCodeableConcept', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity4', 'outcomeReference', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity4', 'progress', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity4', 'reference', 'Reference(Appointment)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivity4', 'detail', 'CarePlanActivityDetail4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCarePlanActivityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivity4', nil, js.FHIRFactoryJs);
  defineCarePlanActivityPropsJs(js, def);
end;


procedure defineCarePlanActivityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivityDetail4', 'kind', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'goal', 'Reference(Goal)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'statusReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'doNotPerform', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'scheduledTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'scheduledPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'scheduledString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'performer', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'productCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'productReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'dailyAmount', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCarePlanActivityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivityDetail4', nil, js.FHIRFactoryJs);
  defineCarePlanActivityDetailPropsJs(js, def);
end;


procedure defineCarePlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CarePlan4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'basedOn', 'Reference(CarePlan)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'replaces', 'Reference(CarePlan)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'partOf', 'Reference(CarePlan)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan4', 'intent', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CarePlan4', 'author', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan4', 'contributor', 'Reference(Patient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'careTeam', 'Reference(CareTeam)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'addresses', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'supportingInfo', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'goal', 'Reference(Goal)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'activity', 'CarePlanActivity4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCarePlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlan4', nil, js.FHIRFactoryJs);
  defineCarePlanPropsJs(js, def);
end;


procedure defineCareTeamParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CareTeamParticipant4', 'role', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeamParticipant4', 'member', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant4', 'onBehalfOf', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCareTeamParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CareTeamParticipant4', nil, js.FHIRFactoryJs);
  defineCareTeamParticipantPropsJs(js, def);
end;


procedure defineCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CareTeam4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CareTeam4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CareTeam4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam4', 'participant', 'CareTeamParticipant4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'managingOrganization', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CareTeam4', nil, js.FHIRFactoryJs);
  defineCareTeamPropsJs(js, def);
end;


procedure defineCatalogEntryRelatedEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CatalogEntryRelatedEntry4', 'relationtype', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CatalogEntryRelatedEntry4', 'item', 'Reference(CatalogEntry)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCatalogEntryRelatedEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CatalogEntryRelatedEntry4', nil, js.FHIRFactoryJs);
  defineCatalogEntryRelatedEntryPropsJs(js, def);
end;


procedure defineCatalogEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CatalogEntry4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CatalogEntry4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CatalogEntry4', 'orderable', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CatalogEntry4', 'referencedItem', 'Reference(Medication)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CatalogEntry4', 'additionalIdentifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CatalogEntry4', 'classification', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CatalogEntry4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CatalogEntry4', 'validityPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CatalogEntry4', 'validTo', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CatalogEntry4', 'lastUpdated', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CatalogEntry4', 'additionalCharacteristic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CatalogEntry4', 'additionalClassification', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CatalogEntry4', 'relatedEntry', 'CatalogEntryRelatedEntry4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCatalogEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CatalogEntry4', nil, js.FHIRFactoryJs);
  defineCatalogEntryPropsJs(js, def);
end;


procedure defineChargeItemPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemPerformer4', 'function', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemPerformer4', 'actor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineChargeItemPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemPerformer4', nil, js.FHIRFactoryJs);
  defineChargeItemPerformerPropsJs(js, def);
end;


procedure defineChargeItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ChargeItem4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItem4', 'partOf', 'Reference(ChargeItem)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'context', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'occurrenceDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem4', 'occurrencePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'occurrenceTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'performer', 'ChargeItemPerformer4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'performingOrganization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'requestingOrganization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'costCenter', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'bodysite', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'factorOverride', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ChargeItem4', 'priceOverride', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'overrideReason', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItem4', 'enterer', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'enteredDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem4', 'reason', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'service', 'Reference(DiagnosticReport)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'productReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'productCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem4', 'account', 'Reference(Account)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem4', 'supportingInformation', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineChargeItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItem4', nil, js.FHIRFactoryJs);
  defineChargeItemPropsJs(js, def);
end;


procedure defineChargeItemDefinitionApplicabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinitionApplicability4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinitionApplicability4', 'language', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinitionApplicability4', 'expression', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineChargeItemDefinitionApplicabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinitionApplicability4', nil, js.FHIRFactoryJs);
  defineChargeItemDefinitionApplicabilityPropsJs(js, def);
end;


procedure defineChargeItemDefinitionPropertyGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroup4', 'applicability', '@ChargeItemDefinition.applicability4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroup4', 'priceComponent', 'ChargeItemDefinitionPropertyGroupPriceComponent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineChargeItemDefinitionPropertyGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinitionPropertyGroup4', nil, js.FHIRFactoryJs);
  defineChargeItemDefinitionPropertyGroupPropsJs(js, def);
end;


procedure defineChargeItemDefinitionPropertyGroupPriceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent4', 'amount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineChargeItemDefinitionPropertyGroupPriceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinitionPropertyGroupPriceComponent4', nil, js.FHIRFactoryJs);
  defineChargeItemDefinitionPropertyGroupPriceComponentPropsJs(js, def);
end;


procedure defineChargeItemDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'instance', 'Reference(Medication)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'applicability', 'ChargeItemDefinitionApplicability4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition4', 'propertyGroup', 'ChargeItemDefinitionPropertyGroup4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineChargeItemDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinition4', nil, js.FHIRFactoryJs);
  defineChargeItemDefinitionPropsJs(js, def);
end;


procedure defineClaimRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimRelated4', 'claim', 'Reference(Claim)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimRelated4', 'relationship', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimRelated4', 'reference', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimRelated4', nil, js.FHIRFactoryJs);
  defineClaimRelatedPropsJs(js, def);
end;


procedure defineClaimPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimPayee4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee4', 'party', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimPayee4', nil, js.FHIRFactoryJs);
  defineClaimPayeePropsJs(js, def);
end;


procedure defineClaimCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimCareTeam4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimCareTeam4', 'provider', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCareTeam4', 'responsible', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimCareTeam4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCareTeam4', 'qualification', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimCareTeam4', nil, js.FHIRFactoryJs);
  defineClaimCareTeamPropsJs(js, def);
end;


procedure defineClaimSupportingInfoPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimSupportingInfo4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimSupportingInfo4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo4', 'timingDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimSupportingInfo4', 'timingPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimSupportingInfo4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimSupportingInfo4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo4', 'valueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo4', 'reason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimSupportingInfoJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimSupportingInfo4', nil, js.FHIRFactoryJs);
  defineClaimSupportingInfoPropsJs(js, def);
end;


procedure defineClaimDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimDiagnosis4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimDiagnosis4', 'diagnosisCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis4', 'diagnosisReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimDiagnosis4', 'onAdmission', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis4', 'packageCode', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimDiagnosis4', nil, js.FHIRFactoryJs);
  defineClaimDiagnosisPropsJs(js, def);
end;


procedure defineClaimProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimProcedure4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimProcedure4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimProcedure4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimProcedure4', 'procedureCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimProcedure4', 'procedureReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimProcedure4', 'udi', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimProcedure4', nil, js.FHIRFactoryJs);
  defineClaimProcedurePropsJs(js, def);
end;


procedure defineClaimInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimInsurance4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimInsurance4', 'focal', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimInsurance4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInsurance4', 'coverage', 'Reference(Coverage)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInsurance4', 'businessArrangement', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimInsurance4', 'claimResponse', 'Reference(ClaimResponse)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimInsurance4', nil, js.FHIRFactoryJs);
  defineClaimInsurancePropsJs(js, def);
end;


procedure defineClaimAccidentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimAccident4', 'date', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimAccident4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimAccident4', 'locationAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimAccident4', 'locationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimAccidentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimAccident4', nil, js.FHIRFactoryJs);
  defineClaimAccidentPropsJs(js, def);
end;


procedure defineClaimItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItem4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItem4', 'revenue', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem4', 'programCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem4', 'servicedDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimItem4', 'servicedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'locationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'locationAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'locationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItem4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'udi', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem4', 'bodySite', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem4', 'subSite', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem4', 'encounter', 'Reference(Encounter)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem4', 'detail', 'ClaimItemDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItem4', nil, js.FHIRFactoryJs);
  defineClaimItemPropsJs(js, def);
end;


procedure defineClaimItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetail4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetail4', 'revenue', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail4', 'programCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetail4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail4', 'udi', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail4', 'subDetail', 'ClaimItemDetailSubDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetail4', nil, js.FHIRFactoryJs);
  defineClaimItemDetailPropsJs(js, def);
end;


procedure defineClaimItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'revenue', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'programCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail4', 'udi', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetailSubDetail4', nil, js.FHIRFactoryJs);
  defineClaimItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Claim4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'subType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'use', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'billablePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Claim4', 'enterer', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'insurer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'provider', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'priority', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'fundsReserve', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'related', 'ClaimRelated4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'prescription', 'Reference(DeviceRequest)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'originalPrescription', 'Reference(DeviceRequest)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'payee', 'ClaimPayee4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'referral', 'Reference(ServiceRequest)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'facility', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'careTeam', 'ClaimCareTeam4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'supportingInfo', 'ClaimSupportingInfo4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'diagnosis', 'ClaimDiagnosis4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'procedure', 'ClaimProcedure4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'insurance', 'ClaimInsurance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'accident', 'ClaimAccident4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim4', 'item', 'ClaimItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim4', 'total', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Claim4', nil, js.FHIRFactoryJs);
  defineClaimPropsJs(js, def);
end;


procedure defineClaimResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItem4', 'itemSequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItem4', 'adjudication', 'ClaimResponseItemAdjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItem4', 'detail', 'ClaimResponseItemDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItem4', nil, js.FHIRFactoryJs);
  defineClaimResponseItemPropsJs(js, def);
end;


procedure defineClaimResponseItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemAdjudication4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication4', 'reason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication4', 'amount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication4', 'value', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineClaimResponseItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemAdjudication4', nil, js.FHIRFactoryJs);
  defineClaimResponseItemAdjudicationPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetail4', 'detailSequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetail4', 'adjudication', '@ClaimResponse.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItemDetail4', 'subDetail', 'ClaimResponseItemDetailSubDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetail4', nil, js.FHIRFactoryJs);
  defineClaimResponseItemDetailPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail4', 'subDetailSequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail4', 'adjudication', '@ClaimResponse.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailSubDetail4', nil, js.FHIRFactoryJs);
  defineClaimResponseItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimResponseAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItem4', 'provider', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'programCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'servicedDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'servicedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'locationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'locationAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'locationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'bodySite', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'subSite', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'adjudication', '@ClaimResponse.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem4', 'detail', 'ClaimResponseAddItemDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItem4', nil, js.FHIRFactoryJs);
  defineClaimResponseAddItemPropsJs(js, def);
end;


procedure defineClaimResponseAddItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemDetail4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail4', 'adjudication', '@ClaimResponse.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail4', 'subDetail', 'ClaimResponseAddItemDetailSubDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemDetail4', nil, js.FHIRFactoryJs);
  defineClaimResponseAddItemDetailPropsJs(js, def);
end;


procedure defineClaimResponseAddItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail4', 'adjudication', '@ClaimResponse.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemDetailSubDetail4', nil, js.FHIRFactoryJs);
  defineClaimResponseAddItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimResponseTotalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseTotal4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseTotal4', 'amount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseTotalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseTotal4', nil, js.FHIRFactoryJs);
  defineClaimResponseTotalPropsJs(js, def);
end;


procedure defineClaimResponsePaymentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponsePayment4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment4', 'adjustment', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment4', 'adjustmentReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment4', 'date', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponsePayment4', 'amount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponsePaymentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponsePayment4', nil, js.FHIRFactoryJs);
  defineClaimResponsePaymentPropsJs(js, def);
end;


procedure defineClaimResponseProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseProcessNote4', 'number', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseProcessNote4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseProcessNote4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseProcessNote4', 'language', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseProcessNote4', nil, js.FHIRFactoryJs);
  defineClaimResponseProcessNotePropsJs(js, def);
end;


procedure defineClaimResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseInsurance4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseInsurance4', 'focal', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimResponseInsurance4', 'coverage', 'Reference(Coverage)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseInsurance4', 'businessArrangement', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseInsurance4', 'claimResponse', 'Reference(ClaimResponse)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseInsurance4', nil, js.FHIRFactoryJs);
  defineClaimResponseInsurancePropsJs(js, def);
end;


procedure defineClaimResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseError4', 'itemSequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError4', 'detailSequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError4', 'subDetailSequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseError4', nil, js.FHIRFactoryJs);
  defineClaimResponseErrorPropsJs(js, def);
end;


procedure defineClaimResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClaimResponse4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'subType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'use', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponse4', 'insurer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'requestor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'request', 'Reference(Claim)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'outcome', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse4', 'disposition', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse4', 'preAuthRef', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse4', 'preAuthPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'payeeType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'item', 'ClaimResponseItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'addItem', 'ClaimResponseAddItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'adjudication', '@ClaimResponse.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'total', 'ClaimResponseTotal4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'payment', 'ClaimResponsePayment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'fundsReserve', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'formCode', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'form', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse4', 'processNote', 'ClaimResponseProcessNote4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'communicationRequest', 'Reference(CommunicationRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'insurance', 'ClaimResponseInsurance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse4', 'error', 'ClaimResponseError4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponse4', nil, js.FHIRFactoryJs);
  defineClaimResponsePropsJs(js, def);
end;


procedure defineClinicalImpressionInvestigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionInvestigation4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionInvestigation4', 'item', 'Reference(Observation)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClinicalImpressionInvestigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionInvestigation4', nil, js.FHIRFactoryJs);
  defineClinicalImpressionInvestigationPropsJs(js, def);
end;


procedure defineClinicalImpressionFindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionFinding4', 'itemCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding4', 'itemReference', 'Reference(Condition)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding4', 'basis', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineClinicalImpressionFindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionFinding4', nil, js.FHIRFactoryJs);
  defineClinicalImpressionFindingPropsJs(js, def);
end;


procedure defineClinicalImpressionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClinicalImpression4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression4', 'statusReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'effectiveDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression4', 'assessor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'previous', 'Reference(ClinicalImpression)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression4', 'problem', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'investigation', 'ClinicalImpressionInvestigation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'summary', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression4', 'finding', 'ClinicalImpressionFinding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'prognosisCodeableConcept', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'prognosisReference', 'Reference(RiskAssessment)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'supportingInfo', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClinicalImpressionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpression4', nil, js.FHIRFactoryJs);
  defineClinicalImpressionPropsJs(js, def);
end;


procedure defineCodeSystemFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemFilter4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemFilter4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemFilter4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemFilter4', nil, js.FHIRFactoryJs);
  defineCodeSystemFilterPropsJs(js, def);
end;


procedure defineCodeSystemPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemProperty4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty4', 'uri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemProperty4', nil, js.FHIRFactoryJs);
  defineCodeSystemPropertyPropsJs(js, def);
end;


procedure defineCodeSystemConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConcept4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept4', 'definition', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept4', 'designation', 'CodeSystemConceptDesignation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystemConcept4', 'property', 'CodeSystemConceptProperty4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystemConcept4', 'concept', '@CodeSystem.concept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCodeSystemConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConcept4', nil, js.FHIRFactoryJs);
  defineCodeSystemConceptPropsJs(js, def);
end;


procedure defineCodeSystemConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConceptDesignation4', 'language', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptDesignation4', 'use', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CodeSystemConceptDesignation4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConceptDesignation4', nil, js.FHIRFactoryJs);
  defineCodeSystemConceptDesignationPropsJs(js, def);
end;


procedure defineCodeSystemConceptPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CodeSystemConceptProperty4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineCodeSystemConceptPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConceptProperty4', nil, js.FHIRFactoryJs);
  defineCodeSystemConceptPropertyPropsJs(js, def);
end;


procedure defineCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CodeSystem4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CodeSystem4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'caseSensitive', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem4', 'valueSet', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'hierarchyMeaning', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'compositional', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem4', 'versionNeeded', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem4', 'content', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'supplements', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'count', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem4', 'filter', 'CodeSystemFilter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'property', 'CodeSystemProperty4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem4', 'concept', 'CodeSystemConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystem4', nil, js.FHIRFactoryJs);
  defineCodeSystemPropsJs(js, def);
end;


procedure defineCommunicationPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationPayload4', 'contentString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationPayload4', 'contentAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationPayload4', 'contentReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationPayload4', nil, js.FHIRFactoryJs);
  defineCommunicationPayloadPropsJs(js, def);
end;


procedure defineCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Communication4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'basedOn', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'partOf', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'inResponseTo', 'Reference(Communication)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Communication4', 'statusReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Communication4', 'medium', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication4', 'topic', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication4', 'about', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication4', 'sent', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication4', 'received', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication4', 'recipient', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'sender', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'payload', 'CommunicationPayload4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Communication4', nil, js.FHIRFactoryJs);
  defineCommunicationPropsJs(js, def);
end;


procedure defineCommunicationRequestPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationRequestPayload4', 'contentString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequestPayload4', 'contentAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequestPayload4', 'contentReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationRequestPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequestPayload4', nil, js.FHIRFactoryJs);
  defineCommunicationRequestPayloadPropsJs(js, def);
end;


procedure defineCommunicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CommunicationRequest4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'basedOn', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'replaces', 'Reference(CommunicationRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'groupIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest4', 'statusReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest4', 'doNotPerform', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CommunicationRequest4', 'medium', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'about', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'payload', 'CommunicationRequestPayload4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'occurrenceDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest4', 'occurrencePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'authoredOn', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest4', 'requester', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'recipient', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'sender', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCommunicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequest4', nil, js.FHIRFactoryJs);
  defineCommunicationRequestPropsJs(js, def);
end;


procedure defineCompartmentDefinitionResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompartmentDefinitionResource4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinitionResource4', 'documentation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCompartmentDefinitionResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompartmentDefinitionResource4', nil, js.FHIRFactoryJs);
  defineCompartmentDefinitionResourcePropsJs(js, def);
end;


procedure defineCompartmentDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CompartmentDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompartmentDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition4', 'search', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition4', 'resource', 'CompartmentDefinitionResource4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompartmentDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompartmentDefinition4', nil, js.FHIRFactoryJs);
  defineCompartmentDefinitionPropsJs(js, def);
end;


procedure defineCompositionAttesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionAttester4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionAttester4', 'time', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompositionAttester4', 'party', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCompositionAttesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionAttester4', nil, js.FHIRFactoryJs);
  defineCompositionAttesterPropsJs(js, def);
end;


procedure defineCompositionRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionRelatesTo4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionRelatesTo4', 'targetIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionRelatesTo4', 'targetReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCompositionRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionRelatesTo4', nil, js.FHIRFactoryJs);
  defineCompositionRelatesToPropsJs(js, def);
end;


procedure defineCompositionEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionEvent4', 'code', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionEvent4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionEvent4', 'detail', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionEvent4', nil, js.FHIRFactoryJs);
  defineCompositionEventPropsJs(js, def);
end;


procedure defineCompositionSectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionSection4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionSection4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection4', 'author', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionSection4', 'focus', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection4', 'text', 'Narrative4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionSection4', 'orderedBy', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection4', 'entry', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionSection4', 'emptyReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection4', 'section', '@Composition.section4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionSectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionSection4', nil, js.FHIRFactoryJs);
  defineCompositionSectionPropsJs(js, def);
end;


procedure defineCompositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Composition4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition4', 'subject', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Composition4', 'author', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition4', 'confidentiality', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition4', 'attester', 'CompositionAttester4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition4', 'custodian', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition4', 'relatesTo', 'CompositionRelatesTo4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition4', 'event', 'CompositionEvent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition4', 'section', 'CompositionSection4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Composition4', nil, js.FHIRFactoryJs);
  defineCompositionPropsJs(js, def);
end;


procedure defineConceptMapGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroup4', 'source', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup4', 'sourceVersion', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup4', 'target', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup4', 'targetVersion', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup4', 'element', 'ConceptMapGroupElement4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapGroup4', 'unmapped', 'ConceptMapGroupUnmapped4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConceptMapGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroup4', nil, js.FHIRFactoryJs);
  defineConceptMapGroupPropsJs(js, def);
end;


procedure defineConceptMapGroupElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElement4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElement4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElement4', 'target', 'ConceptMapGroupElementTarget4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapGroupElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElement4', nil, js.FHIRFactoryJs);
  defineConceptMapGroupElementPropsJs(js, def);
end;


procedure defineConceptMapGroupElementTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'equivalence', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'dependsOn', 'ConceptMapGroupElementTargetDependsOn4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget4', 'product', '@ConceptMap.group.element.target.dependsOn4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapGroupElementTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElementTarget4', nil, js.FHIRFactoryJs);
  defineConceptMapGroupElementTargetPropsJs(js, def);
end;


procedure defineConceptMapGroupElementTargetDependsOnPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn4', 'property_', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn4', 'system', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConceptMapGroupElementTargetDependsOnJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElementTargetDependsOn4', nil, js.FHIRFactoryJs);
  defineConceptMapGroupElementTargetDependsOnPropsJs(js, def);
end;


procedure defineConceptMapGroupUnmappedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupUnmapped4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped4', 'url', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConceptMapGroupUnmappedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupUnmapped4', nil, js.FHIRFactoryJs);
  defineConceptMapGroupUnmappedPropsJs(js, def);
end;


procedure defineConceptMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ConceptMap4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConceptMap4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ConceptMap4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'sourceUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'sourceCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'targetUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'targetCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap4', 'group', 'ConceptMapGroup4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMap4', nil, js.FHIRFactoryJs);
  defineConceptMapPropsJs(js, def);
end;


procedure defineConditionStagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionStage4', 'summary', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConditionStage4', 'assessment', 'Reference(ClinicalImpression)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConditionStage4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConditionStageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionStage4', nil, js.FHIRFactoryJs);
  defineConditionStagePropsJs(js, def);
end;


procedure defineConditionEvidencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionEvidence4', 'code', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConditionEvidence4', 'detail', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionEvidenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionEvidence4', nil, js.FHIRFactoryJs);
  defineConditionEvidencePropsJs(js, def);
end;


procedure defineConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Condition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition4', 'clinicalStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'verificationStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition4', 'severity', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'bodySite', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'onsetDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition4', 'onsetAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'onsetPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'onsetRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'onsetString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition4', 'abatementDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition4', 'abatementAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'abatementPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'abatementRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'abatementString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition4', 'recordedDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition4', 'recorder', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'asserter', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition4', 'stage', 'ConditionStage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition4', 'evidence', 'ConditionEvidence4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Condition4', nil, js.FHIRFactoryJs);
  defineConditionPropsJs(js, def);
end;


procedure defineConsentPolicyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentPolicy4', 'authority', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentPolicy4', 'uri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConsentPolicyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentPolicy4', nil, js.FHIRFactoryJs);
  defineConsentPolicyPropsJs(js, def);
end;


procedure defineConsentVerificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentVerification4', 'verified', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConsentVerification4', 'verifiedWith', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentVerification4', 'verificationDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineConsentVerificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentVerification4', nil, js.FHIRFactoryJs);
  defineConsentVerificationPropsJs(js, def);
end;


procedure defineConsentProvisionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvision4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentProvision4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvision4', 'actor', 'ConsentProvisionActor4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'action', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'securityLabel', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'purpose', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'class', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'code', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'dataPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvision4', 'data', 'ConsentProvisionData4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision4', 'provision', '@Consent.provision4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConsentProvisionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvision4', nil, js.FHIRFactoryJs);
  defineConsentProvisionPropsJs(js, def);
end;


procedure defineConsentProvisionActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvisionActor4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvisionActor4', 'reference', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentProvisionActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvisionActor4', nil, js.FHIRFactoryJs);
  defineConsentProvisionActorPropsJs(js, def);
end;


procedure defineConsentProvisionDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvisionData4', 'meaning', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentProvisionData4', 'reference', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentProvisionDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvisionData4', nil, js.FHIRFactoryJs);
  defineConsentProvisionDataPropsJs(js, def);
end;


procedure defineConsentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Consent4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Consent4', 'scope', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'dateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Consent4', 'performer', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'organization', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'sourceAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'sourceReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'policy', 'ConsentPolicy4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'policyRule', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent4', 'verification', 'ConsentVerification4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent4', 'provision', 'ConsentProvision4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Consent4', nil, js.FHIRFactoryJs);
  defineConsentPropsJs(js, def);
end;


procedure defineContractContentDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractContentDefinition4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractContentDefinition4', 'subType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractContentDefinition4', 'publisher', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractContentDefinition4', 'publicationDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractContentDefinition4', 'publicationStatus', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractContentDefinition4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineContractContentDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractContentDefinition4', nil, js.FHIRFactoryJs);
  defineContractContentDefinitionPropsJs(js, def);
end;


procedure defineContractTermPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTerm4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'issued', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTerm4', 'applies', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'topicCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'topicReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'subType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTerm4', 'securityLabel', 'ContractTermSecurityLabel4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm4', 'offer', 'ContractTermOffer4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm4', 'asset', 'ContractTermAsset4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm4', 'action', 'ContractTermAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm4', 'group', '@Contract.term4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTerm4', nil, js.FHIRFactoryJs);
  defineContractTermPropsJs(js, def);
end;


procedure defineContractTermSecurityLabelPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermSecurityLabel4', 'classification', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermSecurityLabel4', 'category', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermSecurityLabel4', 'control', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermSecurityLabelJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermSecurityLabel4', nil, js.FHIRFactoryJs);
  defineContractTermSecurityLabelPropsJs(js, def);
end;


procedure defineContractTermOfferPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermOffer4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermOffer4', 'party', 'ContractTermOfferParty4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermOffer4', 'topic', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer4', 'decision', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer4', 'decisionMode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermOffer4', 'answer', 'ContractTermOfferAnswer4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermOffer4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineContractTermOfferJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermOffer4', nil, js.FHIRFactoryJs);
  defineContractTermOfferPropsJs(js, def);
end;


procedure defineContractTermOfferPartyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermOfferParty4', 'reference', 'Reference(Patient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermOfferParty4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermOfferPartyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermOfferParty4', nil, js.FHIRFactoryJs);
  defineContractTermOfferPartyPropsJs(js, def);
end;


procedure defineContractTermOfferAnswerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOfferAnswer4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermOfferAnswerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermOfferAnswer4', nil, js.FHIRFactoryJs);
  defineContractTermOfferAnswerPropsJs(js, def);
end;


procedure defineContractTermAssetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAsset4', 'scope', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset4', 'typeReference', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset4', 'subtype', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset4', 'relationship', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset4', 'context', 'ContractTermAssetContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset4', 'condition', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermAsset4', 'periodType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset4', 'period', 'Period4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset4', 'usePeriod', 'Period4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermAsset4', 'answer', '@Contract.term.offer.answer4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset4', 'valuedItem', 'ContractTermAssetValuedItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermAssetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAsset4', nil, js.FHIRFactoryJs);
  defineContractTermAssetPropsJs(js, def);
end;


procedure defineContractTermAssetContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAssetContext4', 'reference', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetContext4', 'code', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAssetContext4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineContractTermAssetContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAssetContext4', nil, js.FHIRFactoryJs);
  defineContractTermAssetContextPropsJs(js, def);
end;


procedure defineContractTermAssetValuedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'entityCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'entityReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'effectiveTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'points', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'payment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'paymentDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'responsible', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem4', 'recipient', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermAssetValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAssetValuedItem4', nil, js.FHIRFactoryJs);
  defineContractTermAssetValuedItemPropsJs(js, def);
end;


procedure defineContractTermActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAction4', 'doNotPerform', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ContractTermAction4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction4', 'subject', 'ContractTermActionSubject4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAction4', 'intent', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction4', 'status', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction4', 'context', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction4', 'occurrenceDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermAction4', 'occurrencePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction4', 'occurrenceTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction4', 'requester', 'Reference(Patient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAction4', 'performerType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAction4', 'performerRole', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction4', 'performer', 'Reference(RelatedPerson)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAction4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAction4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAction4', nil, js.FHIRFactoryJs);
  defineContractTermActionPropsJs(js, def);
end;


procedure defineContractTermActionSubjectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermActionSubject4', 'reference', 'Reference(Patient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermActionSubject4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermActionSubjectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermActionSubject4', nil, js.FHIRFactoryJs);
  defineContractTermActionSubjectPropsJs(js, def);
end;


procedure defineContractSignerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractSigner4', 'type', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner4', 'party', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner4', 'signature', 'Signature4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractSignerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractSigner4', nil, js.FHIRFactoryJs);
  defineContractSignerPropsJs(js, def);
end;


procedure defineContractFriendlyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractFriendly4', 'contentAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractFriendly4', 'contentReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractFriendlyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractFriendly4', nil, js.FHIRFactoryJs);
  defineContractFriendlyPropsJs(js, def);
end;


procedure defineContractLegalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractLegal4', 'contentAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractLegal4', 'contentReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractLegalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractLegal4', nil, js.FHIRFactoryJs);
  defineContractLegalPropsJs(js, def);
end;


procedure defineContractRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractRule4', 'contentAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractRule4', 'contentReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractRule4', nil, js.FHIRFactoryJs);
  defineContractRulePropsJs(js, def);
end;


procedure defineContractPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Contract4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract4', 'legalState', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'instantiatesCanonical', 'Reference(Contract)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'instantiatesUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract4', 'contentDerivative', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'issued', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Contract4', 'applies', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'expirationType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'subject', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'authority', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'domain', 'Reference(Location)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'site', 'Reference(Location)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract4', 'subtitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract4', 'author', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'scope', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'topicCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'topicReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'subType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'contentDefinition', 'ContractContentDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'term', 'ContractTerm4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'supportingInfo', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'relevantHistory', 'Reference(Provenance)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'signer', 'ContractSigner4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'friendly', 'ContractFriendly4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'legal', 'ContractLegal4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'rule', 'ContractRule4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract4', 'legallyBindingAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract4', 'legallyBindingReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contract4', nil, js.FHIRFactoryJs);
  defineContractPropsJs(js, def);
end;


procedure defineCoverageClassPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageClass4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageClass4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageClass4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCoverageClassJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageClass4', nil, js.FHIRFactoryJs);
  defineCoverageClassPropsJs(js, def);
end;


procedure defineCoverageCostToBeneficiaryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageCostToBeneficiary4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageCostToBeneficiary4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageCostToBeneficiary4', 'valueMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageCostToBeneficiary4', 'exception', 'CoverageCostToBeneficiaryException4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageCostToBeneficiaryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageCostToBeneficiary4', nil, js.FHIRFactoryJs);
  defineCoverageCostToBeneficiaryPropsJs(js, def);
end;


procedure defineCoverageCostToBeneficiaryExceptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageCostToBeneficiaryException4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageCostToBeneficiaryException4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCoverageCostToBeneficiaryExceptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageCostToBeneficiaryException4', nil, js.FHIRFactoryJs);
  defineCoverageCostToBeneficiaryExceptionPropsJs(js, def);
end;


procedure defineCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Coverage4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'policyHolder', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'subscriber', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'subscriberId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage4', 'beneficiary', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'dependent', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage4', 'relationship', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage4', 'payor', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage4', 'class', 'CoverageClass4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage4', 'order', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Coverage4', 'network', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage4', 'costToBeneficiary', 'CoverageCostToBeneficiary4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage4', 'subrogation', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Coverage4', 'contract', 'Reference(Contract)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coverage4', nil, js.FHIRFactoryJs);
  defineCoveragePropsJs(js, def);
end;


procedure defineCoverageEligibilityRequestSupportingInfoPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityRequestSupportingInfo4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'CoverageEligibilityRequestSupportingInfo4', 'information', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestSupportingInfo4', 'appliesToAll', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineCoverageEligibilityRequestSupportingInfoJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityRequestSupportingInfo4', nil, js.FHIRFactoryJs);
  defineCoverageEligibilityRequestSupportingInfoPropsJs(js, def);
end;


procedure defineCoverageEligibilityRequestInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityRequestInsurance4', 'focal', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CoverageEligibilityRequestInsurance4', 'coverage', 'Reference(Coverage)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestInsurance4', 'businessArrangement', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCoverageEligibilityRequestInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityRequestInsurance4', nil, js.FHIRFactoryJs);
  defineCoverageEligibilityRequestInsurancePropsJs(js, def);
end;


procedure defineCoverageEligibilityRequestItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityRequestItem4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem4', 'provider', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem4', 'facility', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem4', 'diagnosis', 'CoverageEligibilityRequestItemDiagnosis4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem4', 'detail', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageEligibilityRequestItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityRequestItem4', nil, js.FHIRFactoryJs);
  defineCoverageEligibilityRequestItemPropsJs(js, def);
end;


procedure defineCoverageEligibilityRequestItemDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityRequestItemDiagnosis4', 'diagnosisCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItemDiagnosis4', 'diagnosisReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCoverageEligibilityRequestItemDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityRequestItemDiagnosis4', nil, js.FHIRFactoryJs);
  defineCoverageEligibilityRequestItemDiagnosisPropsJs(js, def);
end;


procedure defineCoverageEligibilityRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'priority', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'servicedDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'servicedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'enterer', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'provider', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'insurer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'facility', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'supportingInfo', 'CoverageEligibilityRequestSupportingInfo4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'insurance', 'CoverageEligibilityRequestInsurance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityRequest4', 'item', 'CoverageEligibilityRequestItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageEligibilityRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityRequest4', nil, js.FHIRFactoryJs);
  defineCoverageEligibilityRequestPropsJs(js, def);
end;


procedure defineCoverageEligibilityResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityResponseInsurance4', 'coverage', 'Reference(Coverage)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsurance4', 'inforce', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsurance4', 'benefitPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsurance4', 'item', 'CoverageEligibilityResponseInsuranceItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageEligibilityResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityResponseInsurance4', nil, js.FHIRFactoryJs);
  defineCoverageEligibilityResponseInsurancePropsJs(js, def);
end;


procedure defineCoverageEligibilityResponseInsuranceItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'provider', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'excluded', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'network', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'unit', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'term', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'benefit', 'CoverageEligibilityResponseInsuranceItemBenefit4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'authorizationRequired', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'authorizationSupporting', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem4', 'authorizationUrl', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCoverageEligibilityResponseInsuranceItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityResponseInsuranceItem4', nil, js.FHIRFactoryJs);
  defineCoverageEligibilityResponseInsuranceItemPropsJs(js, def);
end;


procedure defineCoverageEligibilityResponseInsuranceItemBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit4', 'allowedUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit4', 'allowedString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit4', 'allowedMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit4', 'usedUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit4', 'usedString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit4', 'usedMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCoverageEligibilityResponseInsuranceItemBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityResponseInsuranceItemBenefit4', nil, js.FHIRFactoryJs);
  defineCoverageEligibilityResponseInsuranceItemBenefitPropsJs(js, def);
end;


procedure defineCoverageEligibilityResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityResponseError4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCoverageEligibilityResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityResponseError4', nil, js.FHIRFactoryJs);
  defineCoverageEligibilityResponseErrorPropsJs(js, def);
end;


procedure defineCoverageEligibilityResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'servicedDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'servicedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'requestor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'request', 'Reference(CoverageEligibilityRequest)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'outcome', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'disposition', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'insurer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'insurance', 'CoverageEligibilityResponseInsurance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'preAuthRef', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'form', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse4', 'error', 'CoverageEligibilityResponseError4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageEligibilityResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityResponse4', nil, js.FHIRFactoryJs);
  defineCoverageEligibilityResponsePropsJs(js, def);
end;


procedure defineDetectedIssueEvidencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DetectedIssueEvidence4', 'code', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssueEvidence4', 'detail', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDetectedIssueEvidenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssueEvidence4', nil, js.FHIRFactoryJs);
  defineDetectedIssueEvidencePropsJs(js, def);
end;


procedure defineDetectedIssueMitigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DetectedIssueMitigation4', 'action', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssueMitigation4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssueMitigation4', 'author', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDetectedIssueMitigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssueMitigation4', nil, js.FHIRFactoryJs);
  defineDetectedIssueMitigationPropsJs(js, def);
end;


procedure defineDetectedIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DetectedIssue4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue4', 'severity', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue4', 'identifiedDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssue4', 'identifiedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue4', 'author', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue4', 'implicated', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue4', 'evidence', 'DetectedIssueEvidence4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue4', 'detail', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue4', 'reference', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue4', 'mitigation', 'DetectedIssueMitigation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDetectedIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssue4', nil, js.FHIRFactoryJs);
  defineDetectedIssuePropsJs(js, def);
end;


procedure defineDeviceUdiCarrierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceUdiCarrier4', 'deviceIdentifier', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdiCarrier4', 'issuer', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdiCarrier4', 'jurisdiction', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdiCarrier4', 'carrierAIDC', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'DeviceUdiCarrier4', 'carrierHRF', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdiCarrier4', 'entryType', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceUdiCarrierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUdiCarrier4', nil, js.FHIRFactoryJs);
  defineDeviceUdiCarrierPropsJs(js, def);
end;


procedure defineDeviceDeviceNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDeviceName4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDeviceName4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceDeviceNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDeviceName4', nil, js.FHIRFactoryJs);
  defineDeviceDeviceNamePropsJs(js, def);
end;


procedure defineDeviceSpecializationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceSpecialization4', 'systemType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceSpecialization4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceSpecializationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceSpecialization4', nil, js.FHIRFactoryJs);
  defineDeviceSpecializationPropsJs(js, def);
end;


procedure defineDeviceVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceVersion4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceVersion4', 'component', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceVersion4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceVersion4', nil, js.FHIRFactoryJs);
  defineDeviceVersionPropsJs(js, def);
end;


procedure defineDevicePropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceProperty4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceProperty4', 'valueQuantity', 'Quantity4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceProperty4', 'valueCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDevicePropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceProperty4', nil, js.FHIRFactoryJs);
  defineDevicePropertyPropsJs(js, def);
end;


procedure defineDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Device4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'definition', 'Reference(DeviceDefinition)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device4', 'udiCarrier', 'DeviceUdiCarrier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'statusReason', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'distinctIdentifier', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'manufacturer', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'manufactureDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device4', 'expirationDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device4', 'lotNumber', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'serialNumber', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'deviceName', 'DeviceDeviceName4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'modelNumber', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'partNumber', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device4', 'specialization', 'DeviceSpecialization4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'version', 'DeviceVersion4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'property', 'DeviceProperty4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device4', 'owner', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device4', 'contact', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'safety', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device4', 'parent', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Device4', nil, js.FHIRFactoryJs);
  defineDevicePropsJs(js, def);
end;


procedure defineDeviceDefinitionUdiDeviceIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionUdiDeviceIdentifier4', 'deviceIdentifier', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinitionUdiDeviceIdentifier4', 'issuer', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinitionUdiDeviceIdentifier4', 'jurisdiction', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceDefinitionUdiDeviceIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionUdiDeviceIdentifier4', nil, js.FHIRFactoryJs);
  defineDeviceDefinitionUdiDeviceIdentifierPropsJs(js, def);
end;


procedure defineDeviceDefinitionDeviceNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionDeviceName4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinitionDeviceName4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceDefinitionDeviceNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionDeviceName4', nil, js.FHIRFactoryJs);
  defineDeviceDefinitionDeviceNamePropsJs(js, def);
end;


procedure defineDeviceDefinitionSpecializationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionSpecialization4', 'systemType', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinitionSpecialization4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceDefinitionSpecializationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionSpecialization4', nil, js.FHIRFactoryJs);
  defineDeviceDefinitionSpecializationPropsJs(js, def);
end;


procedure defineDeviceDefinitionCapabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionCapability4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinitionCapability4', 'description', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceDefinitionCapabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionCapability4', nil, js.FHIRFactoryJs);
  defineDeviceDefinitionCapabilityPropsJs(js, def);
end;


procedure defineDeviceDefinitionPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionProperty4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinitionProperty4', 'valueQuantity', 'Quantity4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinitionProperty4', 'valueCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceDefinitionPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionProperty4', nil, js.FHIRFactoryJs);
  defineDeviceDefinitionPropertyPropsJs(js, def);
end;


procedure defineDeviceDefinitionMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionMaterial4', 'substance', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinitionMaterial4', 'alternate', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'DeviceDefinitionMaterial4', 'allergenicIndicator', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineDeviceDefinitionMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionMaterial4', nil, js.FHIRFactoryJs);
  defineDeviceDefinitionMaterialPropsJs(js, def);
end;


procedure defineDeviceDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceDefinition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'udiDeviceIdentifier', 'DeviceDefinitionUdiDeviceIdentifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'manufacturerString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinition4', 'manufacturerReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition4', 'deviceName', 'DeviceDefinitionDeviceName4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'modelNumber', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinition4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition4', 'specialization', 'DeviceDefinitionSpecialization4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'safety', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'shelfLifeStorage', 'ProductShelfLife4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'physicalCharacteristics', 'ProdCharacteristic4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition4', 'languageCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'capability', 'DeviceDefinitionCapability4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'property', 'DeviceDefinitionProperty4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'owner', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition4', 'contact', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinition4', 'onlineInformation', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinition4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition4', 'parentDevice', 'Reference(DeviceDefinition)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition4', 'material', 'DeviceDefinitionMaterial4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinition4', nil, js.FHIRFactoryJs);
  defineDeviceDefinitionPropsJs(js, def);
end;


procedure defineDeviceMetricCalibrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceMetricCalibration4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration4', 'state', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration4', 'time', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineDeviceMetricCalibrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetricCalibration4', nil, js.FHIRFactoryJs);
  defineDeviceMetricCalibrationPropsJs(js, def);
end;


procedure defineDeviceMetricPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceMetric4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceMetric4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric4', 'unit', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric4', 'source', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric4', 'parent', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric4', 'operationalStatus', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric4', 'color', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric4', 'category', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric4', 'measurementPeriod', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric4', 'calibration', 'DeviceMetricCalibration4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceMetricJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetric4', nil, js.FHIRFactoryJs);
  defineDeviceMetricPropsJs(js, def);
end;


procedure defineDeviceRequestParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceRequestParameter4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineDeviceRequestParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceRequestParameter4', nil, js.FHIRFactoryJs);
  defineDeviceRequestParameterPropsJs(js, def);
end;


procedure defineDeviceRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceRequest4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'basedOn', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'priorRequest', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'groupIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest4', 'intent', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest4', 'codeReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'codeCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'parameter', 'DeviceRequestParameter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'occurrenceDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest4', 'occurrencePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'occurrenceTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'authoredOn', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest4', 'requester', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'performerType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'performer', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'insurance', 'Reference(Coverage)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'supportingInfo', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest4', 'relevantHistory', 'Reference(Provenance)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceRequest4', nil, js.FHIRFactoryJs);
  defineDeviceRequestPropsJs(js, def);
end;


procedure defineDeviceUseStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceUseStatement4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement4', 'basedOn', 'Reference(ServiceRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUseStatement4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'derivedFrom', 'Reference(ServiceRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement4', 'timingTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'timingPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'timingDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement4', 'recordedOn', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement4', 'source', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'device', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement4', 'bodySite', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceUseStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUseStatement4', nil, js.FHIRFactoryJs);
  defineDeviceUseStatementPropsJs(js, def);
end;


procedure defineDiagnosticReportMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticReportMedia4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReportMedia4', 'link', 'Reference(Media)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDiagnosticReportMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReportMedia4', nil, js.FHIRFactoryJs);
  defineDiagnosticReportMediaPropsJs(js, def);
end;


procedure defineDiagnosticReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DiagnosticReport4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'basedOn', 'Reference(CarePlan)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport4', 'effectiveDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport4', 'issued', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport4', 'performer', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'resultsInterpreter', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'specimen', 'Reference(Specimen)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'result', 'Reference(Observation)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'imagingStudy', 'Reference(ImagingStudy)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'media', 'DiagnosticReportMedia4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'conclusion', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport4', 'conclusionCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport4', 'presentedForm', 'Attachment4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDiagnosticReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReport4', nil, js.FHIRFactoryJs);
  defineDiagnosticReportPropsJs(js, def);
end;


procedure defineDocumentManifestRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestRelated4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestRelated4', 'ref', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentManifestRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestRelated4', nil, js.FHIRFactoryJs);
  defineDocumentManifestRelatedPropsJs(js, def);
end;


procedure defineDocumentManifestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentManifest4', 'masterIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentManifest4', 'author', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest4', 'recipient', 'Reference(Patient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest4', 'source', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest4', 'content', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest4', 'related', 'DocumentManifestRelated4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifest4', nil, js.FHIRFactoryJs);
  defineDocumentManifestPropsJs(js, def);
end;


procedure defineDocumentReferenceRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceRelatesTo4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReferenceRelatesTo4', 'target', 'Reference(DocumentReference)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceRelatesTo4', nil, js.FHIRFactoryJs);
  defineDocumentReferenceRelatesToPropsJs(js, def);
end;


procedure defineDocumentReferenceContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContent4', 'attachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContent4', 'format', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContent4', nil, js.FHIRFactoryJs);
  defineDocumentReferenceContentPropsJs(js, def);
end;


procedure defineDocumentReferenceContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContext4', 'encounter', 'Reference(Encounter)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'event', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'facilityType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'practiceSetting', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'sourcePatientInfo', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext4', 'related', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentReferenceContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContext4', nil, js.FHIRFactoryJs);
  defineDocumentReferenceContextPropsJs(js, def);
end;


procedure defineDocumentReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentReference4', 'masterIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference4', 'docStatus', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'date', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference4', 'author', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'authenticator', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'custodian', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference4', 'relatesTo', 'DocumentReferenceRelatesTo4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference4', 'securityLabel', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'content', 'DocumentReferenceContent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference4', 'context', 'DocumentReferenceContext4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReference4', nil, js.FHIRFactoryJs);
  defineDocumentReferencePropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisSampleSizePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisSampleSize4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesisSampleSize4', 'numberOfStudies', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'EffectEvidenceSynthesisSampleSize4', 'numberOfParticipants', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineEffectEvidenceSynthesisSampleSizeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisSampleSize4', nil, js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisSampleSizePropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisResultsByExposurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisResultsByExposure4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesisResultsByExposure4', 'exposureState', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesisResultsByExposure4', 'variantState', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisResultsByExposure4', 'riskEvidenceSynthesis', 'Reference(RiskEvidenceSynthesis)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEffectEvidenceSynthesisResultsByExposureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisResultsByExposure4', nil, js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisResultsByExposurePropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisEffectEstimatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate4', 'variantState', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate4', 'value', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate4', 'unitOfMeasure', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate4', 'precisionEstimate', 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEffectEvidenceSynthesisEffectEstimateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisEffectEstimate4', nil, js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisEffectEstimatePropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisEffectEstimatePrecisionEstimatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate4', 'level', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate4', 'from', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate4', 'to_', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineEffectEvidenceSynthesisEffectEstimatePrecisionEstimateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisEffectEstimatePrecisionEstimate4', nil, js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisEffectEstimatePrecisionEstimatePropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisCertaintyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisCertainty4', 'rating', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesisCertainty4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesisCertainty4', 'certaintySubcomponent', 'EffectEvidenceSynthesisCertaintyCertaintySubcomponent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEffectEvidenceSynthesisCertaintyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisCertainty4', nil, js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisCertaintyPropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisCertaintyCertaintySubcomponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisCertaintyCertaintySubcomponent4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisCertaintyCertaintySubcomponent4', 'rating', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesisCertaintyCertaintySubcomponent4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEffectEvidenceSynthesisCertaintyCertaintySubcomponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisCertaintyCertaintySubcomponent4', nil, js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisCertaintyCertaintySubcomponentPropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'synthesisType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'studyType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'population', 'Reference(EvidenceVariable)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'exposure', 'Reference(EvidenceVariable)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'exposureAlternative', 'Reference(EvidenceVariable)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'outcome', 'Reference(EvidenceVariable)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'sampleSize', 'EffectEvidenceSynthesisSampleSize4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'resultsByExposure', 'EffectEvidenceSynthesisResultsByExposure4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'effectEstimate', 'EffectEvidenceSynthesisEffectEstimate4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis4', 'certainty', 'EffectEvidenceSynthesisCertainty4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEffectEvidenceSynthesisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesis4', nil, js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisPropsJs(js, def);
end;


procedure defineEncounterStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterStatusHistory4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EncounterStatusHistory4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterStatusHistory4', nil, js.FHIRFactoryJs);
  defineEncounterStatusHistoryPropsJs(js, def);
end;


procedure defineEncounterClassHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterClassHistory4', 'class', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterClassHistory4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterClassHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterClassHistory4', nil, js.FHIRFactoryJs);
  defineEncounterClassHistoryPropsJs(js, def);
end;


procedure defineEncounterParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterParticipant4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterParticipant4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterParticipant4', 'individual', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterParticipant4', nil, js.FHIRFactoryJs);
  defineEncounterParticipantPropsJs(js, def);
end;


procedure defineEncounterDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterDiagnosis4', 'condition', 'Reference(Condition)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis4', 'use', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis4', 'rank', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineEncounterDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterDiagnosis4', nil, js.FHIRFactoryJs);
  defineEncounterDiagnosisPropsJs(js, def);
end;


procedure defineEncounterHospitalizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterHospitalization4', 'preAdmissionIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization4', 'origin', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization4', 'admitSource', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization4', 'reAdmission', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization4', 'dietPreference', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization4', 'specialCourtesy', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization4', 'specialArrangement', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization4', 'destination', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization4', 'dischargeDisposition', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterHospitalizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterHospitalization4', nil, js.FHIRFactoryJs);
  defineEncounterHospitalizationPropsJs(js, def);
end;


procedure defineEncounterLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterLocation4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterLocation4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EncounterLocation4', 'physicalType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterLocation4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterLocation4', nil, js.FHIRFactoryJs);
  defineEncounterLocationPropsJs(js, def);
end;


procedure defineEncounterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Encounter4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Encounter4', 'statusHistory', 'EncounterStatusHistory4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'class', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'classHistory', 'EncounterClassHistory4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'serviceType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'priority', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'episodeOfCare', 'Reference(EpisodeOfCare)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'basedOn', 'Reference(ServiceRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'participant', 'EncounterParticipant4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'appointment', 'Reference(Appointment)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'length', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'diagnosis', 'EncounterDiagnosis4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'account', 'Reference(Account)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'hospitalization', 'EncounterHospitalization4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'location', 'EncounterLocation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter4', 'serviceProvider', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter4', 'partOf', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Encounter4', nil, js.FHIRFactoryJs);
  defineEncounterPropsJs(js, def);
end;


procedure defineEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Endpoint4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Endpoint4', 'connectionType', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Endpoint4', 'managingOrganization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint4', 'contact', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint4', 'payloadType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint4', 'address', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Endpoint4', nil, js.FHIRFactoryJs);
  defineEndpointPropsJs(js, def);
end;


procedure defineEnrollmentRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentRequest4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentRequest4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentRequest4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentRequest4', 'insurer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest4', 'provider', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest4', 'candidate', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest4', 'coverage', 'Reference(Coverage)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentRequest4', nil, js.FHIRFactoryJs);
  defineEnrollmentRequestPropsJs(js, def);
end;


procedure defineEnrollmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentResponse4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentResponse4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse4', 'request', 'Reference(EnrollmentRequest)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse4', 'outcome', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse4', 'disposition', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentResponse4', 'organization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse4', 'requestProvider', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentResponse4', nil, js.FHIRFactoryJs);
  defineEnrollmentResponsePropsJs(js, def);
end;


procedure defineEpisodeOfCareStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareStatusHistory4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCareStatusHistory4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEpisodeOfCareStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareStatusHistory4', nil, js.FHIRFactoryJs);
  defineEpisodeOfCareStatusHistoryPropsJs(js, def);
end;


procedure defineEpisodeOfCareDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareDiagnosis4', 'condition', 'Reference(Condition)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareDiagnosis4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareDiagnosis4', 'rank', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineEpisodeOfCareDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareDiagnosis4', nil, js.FHIRFactoryJs);
  defineEpisodeOfCareDiagnosisPropsJs(js, def);
end;


procedure defineEpisodeOfCarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCare4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCare4', 'statusHistory', 'EpisodeOfCareStatusHistory4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'diagnosis', 'EpisodeOfCareDiagnosis4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare4', 'managingOrganization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare4', 'referralRequest', 'Reference(ServiceRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'careManager', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare4', 'team', 'Reference(CareTeam)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare4', 'account', 'Reference(Account)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEpisodeOfCareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCare4', nil, js.FHIRFactoryJs);
  defineEpisodeOfCarePropsJs(js, def);
end;


procedure defineEventDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'EventDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'subtitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EventDefinition4', 'subjectCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition4', 'subjectReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'usage', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition4', 'trigger', 'TriggerDefinition4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEventDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EventDefinition4', nil, js.FHIRFactoryJs);
  defineEventDefinitionPropsJs(js, def);
end;


procedure defineEvidencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Evidence4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence4', 'shortTitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence4', 'subtitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Evidence4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Evidence4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Evidence4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Evidence4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'exposureBackground', 'Reference(EvidenceVariable)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Evidence4', 'exposureVariant', 'Reference(EvidenceVariable)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence4', 'outcome', 'Reference(EvidenceVariable)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEvidenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Evidence4', nil, js.FHIRFactoryJs);
  defineEvidencePropsJs(js, def);
end;


procedure defineEvidenceVariableCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'definitionReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'definitionCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'definitionCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'definitionExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'definitionDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'definitionTriggerDefinition', 'TriggerDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'usageContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'exclude', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'participantEffectiveDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'participantEffectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'participantEffectiveDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'participantEffectiveTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'timeFromStart', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic4', 'groupMeasure', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineEvidenceVariableCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EvidenceVariableCharacteristic4', nil, js.FHIRFactoryJs);
  defineEvidenceVariableCharacteristicPropsJs(js, def);
end;


procedure defineEvidenceVariablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'EvidenceVariable4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'shortTitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'subtitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EvidenceVariable4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EvidenceVariable4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EvidenceVariable4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariable4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable4', 'characteristic', 'EvidenceVariableCharacteristic4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEvidenceVariableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EvidenceVariable4', nil, js.FHIRFactoryJs);
  defineEvidenceVariablePropsJs(js, def);
end;


procedure defineExampleScenarioActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioActor4', 'actorId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExampleScenarioActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioActor4', nil, js.FHIRFactoryJs);
  defineExampleScenarioActorPropsJs(js, def);
end;


procedure defineExampleScenarioInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstance4', 'resourceId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance4', 'resourceType', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance4', 'version', 'ExampleScenarioInstanceVersion4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenarioInstance4', 'containedInstance', 'ExampleScenarioInstanceContainedInstance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstance4', nil, js.FHIRFactoryJs);
  defineExampleScenarioInstancePropsJs(js, def);
end;


procedure defineExampleScenarioInstanceVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstanceVersion4', 'versionId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstanceVersion4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExampleScenarioInstanceVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstanceVersion4', nil, js.FHIRFactoryJs);
  defineExampleScenarioInstanceVersionPropsJs(js, def);
end;


procedure defineExampleScenarioInstanceContainedInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstanceContainedInstance4', 'resourceId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstanceContainedInstance4', 'versionId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExampleScenarioInstanceContainedInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstanceContainedInstance4', nil, js.FHIRFactoryJs);
  defineExampleScenarioInstanceContainedInstancePropsJs(js, def);
end;


procedure defineExampleScenarioProcessPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcess4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess4', 'preConditions', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess4', 'postConditions', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess4', 'step', 'ExampleScenarioProcessStep4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcess4', nil, js.FHIRFactoryJs);
  defineExampleScenarioProcessPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStep4', 'process', '@ExampleScenario.process4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenarioProcessStep4', 'pause', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStep4', 'operation', 'ExampleScenarioProcessStepOperation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExampleScenarioProcessStep4', 'alternative', 'ExampleScenarioProcessStepAlternative4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessStepJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStep4', nil, js.FHIRFactoryJs);
  defineExampleScenarioProcessStepPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'number', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'type_', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'initiator', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'receiver', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'initiatorActive', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'receiverActive', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'request', '@ExampleScenario.instance.containedInstance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation4', 'response', '@ExampleScenario.instance.containedInstance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExampleScenarioProcessStepOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepOperation4', nil, js.FHIRFactoryJs);
  defineExampleScenarioProcessStepOperationPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepAlternativePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative4', 'step', '@ExampleScenario.process.step4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessStepAlternativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepAlternative4', nil, js.FHIRFactoryJs);
  defineExampleScenarioProcessStepAlternativePropsJs(js, def);
end;


procedure defineExampleScenarioPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ExampleScenario4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenario4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExampleScenario4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario4', 'actor', 'ExampleScenarioActor4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'instance', 'ExampleScenarioInstance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario4', 'process', 'ExampleScenarioProcess4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenario4', nil, js.FHIRFactoryJs);
  defineExampleScenarioPropsJs(js, def);
end;


procedure defineExplanationOfBenefitRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitRelated4', 'claim', 'Reference(Claim)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitRelated4', 'relationship', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitRelated4', 'reference', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitRelated4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitRelatedPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitPayee4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayee4', 'party', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayee4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitPayeePropsJs(js, def);
end;


procedure defineExplanationOfBenefitCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam4', 'provider', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam4', 'responsible', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam4', 'qualification', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitCareTeam4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitCareTeamPropsJs(js, def);
end;


procedure defineExplanationOfBenefitSupportingInfoPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'timingDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'timingPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'valueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo4', 'reason', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitSupportingInfoJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitSupportingInfo4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitSupportingInfoPropsJs(js, def);
end;


procedure defineExplanationOfBenefitDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'diagnosisCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'diagnosisReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'onAdmission', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis4', 'packageCode', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitDiagnosis4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitDiagnosisPropsJs(js, def);
end;


procedure defineExplanationOfBenefitProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitProcedure4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure4', 'procedureCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure4', 'procedureReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure4', 'udi', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitProcedure4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitProcedurePropsJs(js, def);
end;


procedure defineExplanationOfBenefitInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitInsurance4', 'focal', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitInsurance4', 'coverage', 'Reference(Coverage)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitInsurance4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitInsurancePropsJs(js, def);
end;


procedure defineExplanationOfBenefitAccidentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAccident4', 'date', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident4', 'locationAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident4', 'locationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitAccidentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAccident4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitAccidentPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'revenue', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'programCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'servicedDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'servicedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'locationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'locationAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'locationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'udi', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'bodySite', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'subSite', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'encounter', 'Reference(Encounter)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'adjudication', 'ExplanationOfBenefitItemAdjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem4', 'detail', 'ExplanationOfBenefitItemDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItem4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitItemPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication4', 'reason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication4', 'amount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication4', 'value', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineExplanationOfBenefitItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemAdjudication4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitItemAdjudicationPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'revenue', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'programCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'udi', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'adjudication', '@ExplanationOfBenefit.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail4', 'subDetail', 'ExplanationOfBenefitItemDetailSubDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemDetail4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitItemDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'revenue', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'programCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'udi', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail4', 'adjudication', '@ExplanationOfBenefit.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemDetailSubDetail4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitItemDetailSubDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'provider', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'programCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'servicedDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'servicedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'locationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'locationAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'locationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'bodySite', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'subSite', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'adjudication', '@ExplanationOfBenefit.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem4', 'detail', 'ExplanationOfBenefitAddItemDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItem4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitAddItemPropsJs(js, def);
end;


procedure defineExplanationOfBenefitAddItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail4', 'adjudication', '@ExplanationOfBenefit.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail4', 'subDetail', 'ExplanationOfBenefitAddItemDetailSubDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItemDetail4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitAddItemDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitAddItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail4', 'productOrService', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail4', 'modifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail4', 'unitPrice', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail4', 'net', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail4', 'adjudication', '@ExplanationOfBenefit.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItemDetailSubDetail4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitAddItemDetailSubDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitTotalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitTotal4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitTotal4', 'amount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitTotalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitTotal4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitTotalPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPaymentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'adjustment', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'adjustmentReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'date', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'amount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPaymentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayment4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitPaymentPropsJs(js, def);
end;


procedure defineExplanationOfBenefitProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote4', 'number', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote4', 'language', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitProcessNote4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitProcessNotePropsJs(js, def);
end;


procedure defineExplanationOfBenefitBenefitBalancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'excluded', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'network', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'unit', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'term', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance4', 'financial', 'ExplanationOfBenefitBenefitBalanceFinancial4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitBenefitBalanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitBenefitBalance4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitBenefitBalancePropsJs(js, def);
end;


procedure defineExplanationOfBenefitBenefitBalanceFinancialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'allowedUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'allowedString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'allowedMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'usedUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial4', 'usedMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitBenefitBalanceFinancialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitBenefitBalanceFinancial4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitBenefitBalanceFinancialPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefit4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'subType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'use', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'billablePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'enterer', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'insurer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'provider', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'priority', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'fundsReserveRequested', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'fundsReserve', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'related', 'ExplanationOfBenefitRelated4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'prescription', 'Reference(MedicationRequest)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'originalPrescription', 'Reference(MedicationRequest)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'payee', 'ExplanationOfBenefitPayee4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'referral', 'Reference(ServiceRequest)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'facility', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'claim', 'Reference(Claim)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'claimResponse', 'Reference(ClaimResponse)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'outcome', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'disposition', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'preAuthRefPeriod', 'Period4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'careTeam', 'ExplanationOfBenefitCareTeam4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'supportingInfo', 'ExplanationOfBenefitSupportingInfo4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'diagnosis', 'ExplanationOfBenefitDiagnosis4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'procedure', 'ExplanationOfBenefitProcedure4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'precedence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'insurance', 'ExplanationOfBenefitInsurance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'accident', 'ExplanationOfBenefitAccident4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'item', 'ExplanationOfBenefitItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'addItem', 'ExplanationOfBenefitAddItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'adjudication', '@ExplanationOfBenefit.item.adjudication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'total', 'ExplanationOfBenefitTotal4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'payment', 'ExplanationOfBenefitPayment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'formCode', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'form', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'processNote', 'ExplanationOfBenefitProcessNote4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'benefitPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit4', 'benefitBalance', 'ExplanationOfBenefitBenefitBalance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefit4', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitPropsJs(js, def);
end;


procedure defineFamilyMemberHistoryConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'outcome', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'contributedToDeath', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'onsetAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'onsetRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'onsetPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'onsetString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistoryCondition4', nil, js.FHIRFactoryJs);
  defineFamilyMemberHistoryConditionPropsJs(js, def);
end;


procedure defineFamilyMemberHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistory4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'dataAbsentReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'relationship', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'sex', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'bornPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'bornDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'bornString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'ageAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'ageRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'ageString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'estimatedAge', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'deceasedBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'deceasedAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'deceasedRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'deceasedDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'deceasedString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory4', 'condition', 'FamilyMemberHistoryCondition4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistory4', nil, js.FHIRFactoryJs);
  defineFamilyMemberHistoryPropsJs(js, def);
end;


procedure defineFlagPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Flag4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Flag4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Flag4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Flag4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag4', 'author', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineFlagJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Flag4', nil, js.FHIRFactoryJs);
  defineFlagPropsJs(js, def);
end;


procedure defineGoalTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GoalTarget4', 'measure', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget4', 'detailQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget4', 'detailRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget4', 'detailCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget4', 'detailString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GoalTarget4', 'detailBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GoalTarget4', 'detailInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'GoalTarget4', 'detailRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget4', 'dueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GoalTarget4', 'dueDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineGoalTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GoalTarget4', nil, js.FHIRFactoryJs);
  defineGoalTargetPropsJs(js, def);
end;


procedure defineGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Goal4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'lifecycleStatus', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal4', 'achievementStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'priority', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'description', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'startDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal4', 'startCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'target', 'GoalTarget4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'statusDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal4', 'statusReason', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal4', 'expressedBy', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal4', 'addresses', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'outcomeCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal4', 'outcomeReference', 'Reference(Observation)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Goal4', nil, js.FHIRFactoryJs);
  defineGoalPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLink4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink4', 'sliceName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink4', 'min', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'GraphDefinitionLink4', 'max', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink4', 'target', 'GraphDefinitionLinkTarget4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLink4', nil, js.FHIRFactoryJs);
  defineGraphDefinitionLinkPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLinkTarget4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget4', 'params', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget4', 'profile', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget4', 'compartment', 'GraphDefinitionLinkTargetCompartment4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget4', 'link', '@GraphDefinition.link4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionLinkTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLinkTarget4', nil, js.FHIRFactoryJs);
  defineGraphDefinitionLinkTargetPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkTargetCompartmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment4', 'use', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment4', 'rule', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment4', 'expression', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineGraphDefinitionLinkTargetCompartmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLinkTargetCompartment4', nil, js.FHIRFactoryJs);
  defineGraphDefinitionLinkTargetCompartmentPropsJs(js, def);
end;


procedure defineGraphDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'GraphDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GraphDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GraphDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'start', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'profile', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition4', 'link', 'GraphDefinitionLink4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinition4', nil, js.FHIRFactoryJs);
  defineGraphDefinitionPropsJs(js, def);
end;


procedure defineGroupCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupCharacteristic4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic4', 'exclude', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineGroupCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupCharacteristic4', nil, js.FHIRFactoryJs);
  defineGroupCharacteristicPropsJs(js, def);
end;


procedure defineGroupMemberPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupMember4', 'entity', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupMember4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupMember4', 'inactive', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineGroupMemberJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupMember4', nil, js.FHIRFactoryJs);
  defineGroupMemberPropsJs(js, def);
end;


procedure defineGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Group4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Group4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Group4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group4', 'actual', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Group4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Group4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group4', 'quantity', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group4', 'managingEntity', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Group4', 'characteristic', 'GroupCharacteristic4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Group4', 'member', 'GroupMember4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Group4', nil, js.FHIRFactoryJs);
  defineGroupPropsJs(js, def);
end;


procedure defineGuidanceResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'GuidanceResponse4', 'requestIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse4', 'moduleUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse4', 'moduleCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse4', 'moduleCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'occurrenceDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GuidanceResponse4', 'performer', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse4', 'evaluationMessage', 'Reference(OperationOutcome)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse4', 'outputParameters', 'Reference(Parameters)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'result', 'Reference(CarePlan)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse4', 'dataRequirement', 'DataRequirement4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGuidanceResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GuidanceResponse4', nil, js.FHIRFactoryJs);
  defineGuidanceResponsePropsJs(js, def);
end;


procedure defineHealthcareServiceEligibilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceEligibility4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareServiceEligibility4', 'comment', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineHealthcareServiceEligibilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceEligibility4', nil, js.FHIRFactoryJs);
  defineHealthcareServiceEligibilityPropsJs(js, def);
end;


procedure defineHealthcareServiceAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceAvailableTime4', 'allDay', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime4', 'availableStartTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime4', 'availableEndTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineHealthcareServiceAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceAvailableTime4', nil, js.FHIRFactoryJs);
  defineHealthcareServiceAvailableTimePropsJs(js, def);
end;


procedure defineHealthcareServiceNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceNotAvailable4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceNotAvailable4', 'during', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineHealthcareServiceNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceNotAvailable4', nil, js.FHIRFactoryJs);
  defineHealthcareServiceNotAvailablePropsJs(js, def);
end;


procedure defineHealthcareServicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'HealthcareService4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService4', 'providedBy', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'specialty', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'location', 'Reference(Location)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService4', 'extraDetails', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService4', 'photo', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'coverageArea', 'Reference(Location)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'serviceProvisionCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'eligibility', 'HealthcareServiceEligibility4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'program', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'characteristic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'communication', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'referralMethod', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'appointmentRequired', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService4', 'availableTime', 'HealthcareServiceAvailableTime4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'notAvailable', 'HealthcareServiceNotAvailable4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService4', 'availabilityExceptions', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService4', 'endpoint', 'Reference(Endpoint)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineHealthcareServiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareService4', nil, js.FHIRFactoryJs);
  defineHealthcareServicePropsJs(js, def);
end;


procedure defineImagingStudySeriesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeries4', 'uid', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries4', 'number', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries4', 'modality', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries4', 'numberOfInstances', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries4', 'endpoint', 'Reference(Endpoint)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries4', 'bodySite', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries4', 'laterality', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries4', 'specimen', 'Reference(Specimen)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries4', 'started', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudySeries4', 'performer', 'ImagingStudySeriesPerformer4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries4', 'instance', 'ImagingStudySeriesInstance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudySeriesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeries4', nil, js.FHIRFactoryJs);
  defineImagingStudySeriesPropsJs(js, def);
end;


procedure defineImagingStudySeriesPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeriesPerformer4', 'function', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeriesPerformer4', 'actor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImagingStudySeriesPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeriesPerformer4', nil, js.FHIRFactoryJs);
  defineImagingStudySeriesPerformerPropsJs(js, def);
end;


procedure defineImagingStudySeriesInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeriesInstance4', 'uid', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance4', 'sopClass', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeriesInstance4', 'number', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImagingStudySeriesInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeriesInstance4', nil, js.FHIRFactoryJs);
  defineImagingStudySeriesInstancePropsJs(js, def);
end;


procedure defineImagingStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImagingStudy4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy4', 'modality', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy4', 'started', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudy4', 'basedOn', 'Reference(CarePlan)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'referrer', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy4', 'interpreter', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'endpoint', 'Reference(Endpoint)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'numberOfSeries', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy4', 'numberOfInstances', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy4', 'procedureReference', 'Reference(Procedure)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy4', 'procedureCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy4', 'series', 'ImagingStudySeries4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudy4', nil, js.FHIRFactoryJs);
  defineImagingStudyPropsJs(js, def);
end;


procedure defineImmunizationPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationPerformer4', 'function', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationPerformer4', 'actor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImmunizationPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationPerformer4', nil, js.FHIRFactoryJs);
  defineImmunizationPerformerPropsJs(js, def);
end;


procedure defineImmunizationEducationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationEducation4', 'documentType', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEducation4', 'reference', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEducation4', 'publicationDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationEducation4', 'presentationDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineImmunizationEducationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationEducation4', nil, js.FHIRFactoryJs);
  defineImmunizationEducationPropsJs(js, def);
end;


procedure defineImmunizationReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationReaction4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationReaction4', 'detail', 'Reference(Observation)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationReaction4', 'reported', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineImmunizationReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationReaction4', nil, js.FHIRFactoryJs);
  defineImmunizationReactionPropsJs(js, def);
end;


procedure defineImmunizationProtocolAppliedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationProtocolApplied4', 'series', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationProtocolApplied4', 'authority', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationProtocolApplied4', 'targetDisease', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationProtocolApplied4', 'doseNumberPositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationProtocolApplied4', 'doseNumberString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationProtocolApplied4', 'seriesDosesPositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationProtocolApplied4', 'seriesDosesString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImmunizationProtocolAppliedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationProtocolApplied4', nil, js.FHIRFactoryJs);
  defineImmunizationProtocolAppliedPropsJs(js, def);
end;


procedure defineImmunizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Immunization4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization4', 'statusReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'vaccineCode', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'occurrenceDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization4', 'occurrenceString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization4', 'recorded', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization4', 'primarySource', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization4', 'reportOrigin', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'manufacturer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'lotNumber', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization4', 'expirationDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization4', 'site', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'route', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'doseQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'performer', 'ImmunizationPerformer4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'isSubpotent', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization4', 'subpotentReason', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'education', 'ImmunizationEducation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'programEligibility', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'fundingSource', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization4', 'reaction', 'ImmunizationReaction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization4', 'protocolApplied', 'ImmunizationProtocolApplied4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Immunization4', nil, js.FHIRFactoryJs);
  defineImmunizationPropsJs(js, def);
end;


procedure defineImmunizationEvaluationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationEvaluation4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'authority', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'targetDisease', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'immunizationEvent', 'Reference(Immunization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'doseStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'doseStatusReason', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'series', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'doseNumberPositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'doseNumberString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'seriesDosesPositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationEvaluation4', 'seriesDosesString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImmunizationEvaluationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationEvaluation4', nil, js.FHIRFactoryJs);
  defineImmunizationEvaluationPropsJs(js, def);
end;


procedure defineImmunizationRecommendationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'vaccineCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'targetDisease', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'contraindicatedVaccineCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'forecastStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'forecastReason', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'dateCriterion', 'ImmunizationRecommendationRecommendationDateCriterion4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'series', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'doseNumberPositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'doseNumberString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'seriesDosesPositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'seriesDosesString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'supportingImmunization', 'Reference(Immunization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation4', 'supportingPatientInformation', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendation4', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationPropsJs(js, def);
end;


procedure defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion4', 'value', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineImmunizationRecommendationRecommendationDateCriterionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendationDateCriterion4', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js, def);
end;


procedure defineImmunizationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendation4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendation4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationRecommendation4', 'authority', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation4', 'recommendation', 'ImmunizationRecommendationRecommendation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendation4', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationPropsJs(js, def);
end;


procedure defineImplementationGuideDependsOnPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDependsOn4', 'uri', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDependsOn4', 'packageId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDependsOn4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDependsOnJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDependsOn4', nil, js.FHIRFactoryJs);
  defineImplementationGuideDependsOnPropsJs(js, def);
end;


procedure defineImplementationGuideGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideGlobal4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideGlobal4', 'profile', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideGlobal4', nil, js.FHIRFactoryJs);
  defineImplementationGuideGlobalPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinition4', 'grouping', 'ImplementationGuideDefinitionGrouping4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideDefinition4', 'resource', 'ImplementationGuideDefinitionResource4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideDefinition4', 'page', 'ImplementationGuideDefinitionPage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideDefinition4', 'parameter', 'ImplementationGuideDefinitionParameter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideDefinition4', 'template', 'ImplementationGuideDefinitionTemplate4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinition4', nil, js.FHIRFactoryJs);
  defineImplementationGuideDefinitionPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionGroupingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionGrouping4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionGrouping4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionGroupingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionGrouping4', nil, js.FHIRFactoryJs);
  defineImplementationGuideDefinitionGroupingPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'reference', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'exampleBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'exampleCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource4', 'groupingId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionResource4', nil, js.FHIRFactoryJs);
  defineImplementationGuideDefinitionResourcePropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionPagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionPage4', 'nameUrl', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage4', 'nameReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage4', 'generation', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage4', 'page', '@ImplementationGuide.definition.page4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideDefinitionPageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionPage4', nil, js.FHIRFactoryJs);
  defineImplementationGuideDefinitionPagePropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionParameter4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionParameter4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionParameter4', nil, js.FHIRFactoryJs);
  defineImplementationGuideDefinitionParameterPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionTemplatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionTemplate4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionTemplate4', 'source', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionTemplate4', 'scope', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionTemplateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionTemplate4', nil, js.FHIRFactoryJs);
  defineImplementationGuideDefinitionTemplatePropsJs(js, def);
end;


procedure defineImplementationGuideManifestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideManifest4', 'rendering', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideManifest4', 'resource', 'ImplementationGuideManifestResource4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideManifest4', 'page', 'ImplementationGuideManifestPage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideManifest4', nil, js.FHIRFactoryJs);
  defineImplementationGuideManifestPropsJs(js, def);
end;


procedure defineImplementationGuideManifestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideManifestResource4', 'reference', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideManifestResource4', 'exampleBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideManifestResource4', 'exampleCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideManifestResource4', 'relativePath', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideManifestResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideManifestResource4', nil, js.FHIRFactoryJs);
  defineImplementationGuideManifestResourcePropsJs(js, def);
end;


procedure defineImplementationGuideManifestPagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideManifestPage4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideManifestPage4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideManifestPageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideManifestPage4', nil, js.FHIRFactoryJs);
  defineImplementationGuideManifestPagePropsJs(js, def);
end;


procedure defineImplementationGuidePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuide4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuide4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuide4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'packageId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'license', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide4', 'dependsOn', 'ImplementationGuideDependsOn4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide4', 'global', 'ImplementationGuideGlobal4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide4', 'definition', 'ImplementationGuideDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuide4', 'manifest', 'ImplementationGuideManifest4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuide4', nil, js.FHIRFactoryJs);
  defineImplementationGuidePropsJs(js, def);
end;


procedure defineInsurancePlanContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanContact4', 'purpose', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanContact4', 'name', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanContact4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanContact4', 'address', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInsurancePlanContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanContact4', nil, js.FHIRFactoryJs);
  defineInsurancePlanContactPropsJs(js, def);
end;


procedure defineInsurancePlanCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanCoverage4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanCoverage4', 'network', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanCoverage4', 'benefit', 'InsurancePlanCoverageBenefit4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanCoverage4', nil, js.FHIRFactoryJs);
  defineInsurancePlanCoveragePropsJs(js, def);
end;


procedure defineInsurancePlanCoverageBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanCoverageBenefit4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanCoverageBenefit4', 'requirement', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'InsurancePlanCoverageBenefit4', 'limit', 'InsurancePlanCoverageBenefitLimit4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanCoverageBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanCoverageBenefit4', nil, js.FHIRFactoryJs);
  defineInsurancePlanCoverageBenefitPropsJs(js, def);
end;


procedure defineInsurancePlanCoverageBenefitLimitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanCoverageBenefitLimit4', 'value', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanCoverageBenefitLimit4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInsurancePlanCoverageBenefitLimitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanCoverageBenefitLimit4', nil, js.FHIRFactoryJs);
  defineInsurancePlanCoverageBenefitLimitPropsJs(js, def);
end;


procedure defineInsurancePlanPlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanPlan4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanPlan4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlan4', 'coverageArea', 'Reference(Location)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanPlan4', 'network', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanPlan4', 'generalCost', 'InsurancePlanPlanGeneralCost4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanPlan4', 'specificCost', 'InsurancePlanPlanSpecificCost4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanPlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanPlan4', nil, js.FHIRFactoryJs);
  defineInsurancePlanPlanPropsJs(js, def);
end;


procedure defineInsurancePlanPlanGeneralCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanPlanGeneralCost4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanGeneralCost4', 'groupSize', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'InsurancePlanPlanGeneralCost4', 'cost', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanGeneralCost4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineInsurancePlanPlanGeneralCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanPlanGeneralCost4', nil, js.FHIRFactoryJs);
  defineInsurancePlanPlanGeneralCostPropsJs(js, def);
end;


procedure defineInsurancePlanPlanSpecificCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanPlanSpecificCost4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanSpecificCost4', 'benefit', 'InsurancePlanPlanSpecificCostBenefit4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanPlanSpecificCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanPlanSpecificCost4', nil, js.FHIRFactoryJs);
  defineInsurancePlanPlanSpecificCostPropsJs(js, def);
end;


procedure defineInsurancePlanPlanSpecificCostBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefit4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefit4', 'cost', 'InsurancePlanPlanSpecificCostBenefitCost4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanPlanSpecificCostBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanPlanSpecificCostBenefit4', nil, js.FHIRFactoryJs);
  defineInsurancePlanPlanSpecificCostBenefitPropsJs(js, def);
end;


procedure defineInsurancePlanPlanSpecificCostBenefitCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefitCost4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefitCost4', 'applicability', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefitCost4', 'qualifiers', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefitCost4', 'value', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInsurancePlanPlanSpecificCostBenefitCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanPlanSpecificCostBenefitCost4', nil, js.FHIRFactoryJs);
  defineInsurancePlanPlanSpecificCostBenefitCostPropsJs(js, def);
end;


procedure defineInsurancePlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'InsurancePlan4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'InsurancePlan4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'InsurancePlan4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlan4', 'ownedBy', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlan4', 'administeredBy', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlan4', 'coverageArea', 'Reference(Location)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan4', 'contact', 'InsurancePlanContact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan4', 'endpoint', 'Reference(Endpoint)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan4', 'network', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan4', 'coverage', 'InsurancePlanCoverage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan4', 'plan', 'InsurancePlanPlan4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlan4', nil, js.FHIRFactoryJs);
  defineInsurancePlanPropsJs(js, def);
end;


procedure defineInvoiceParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceParticipant4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceParticipant4', 'actor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInvoiceParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceParticipant4', nil, js.FHIRFactoryJs);
  defineInvoiceParticipantPropsJs(js, def);
end;


procedure defineInvoiceLineItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceLineItem4', 'sequence', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'InvoiceLineItem4', 'chargeItemReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItem4', 'chargeItemCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItem4', 'priceComponent', 'InvoiceLineItemPriceComponent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInvoiceLineItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceLineItem4', nil, js.FHIRFactoryJs);
  defineInvoiceLineItemPropsJs(js, def);
end;


procedure defineInvoiceLineItemPriceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceLineItemPriceComponent4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent4', 'factor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent4', 'amount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInvoiceLineItemPriceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceLineItemPriceComponent4', nil, js.FHIRFactoryJs);
  defineInvoiceLineItemPriceComponentPropsJs(js, def);
end;


procedure defineInvoicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Invoice4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Invoice4', 'cancelledReason', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Invoice4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'recipient', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Invoice4', 'participant', 'InvoiceParticipant4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice4', 'issuer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'account', 'Reference(Account)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'lineItem', 'InvoiceLineItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice4', 'totalPriceComponent', '@Invoice.lineItem.priceComponent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice4', 'totalNet', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'totalGross', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice4', 'paymentTerms', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Invoice4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInvoiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Invoice4', nil, js.FHIRFactoryJs);
  defineInvoicePropsJs(js, def);
end;


procedure defineLibraryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Library4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'subtitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Library4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library4', 'subjectCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library4', 'subjectReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'usage', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'parameter', 'ParameterDefinition4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'dataRequirement', 'DataRequirement4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library4', 'content', 'Attachment4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLibraryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Library4', nil, js.FHIRFactoryJs);
  defineLibraryPropsJs(js, def);
end;


procedure defineLinkageItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LinkageItem4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'LinkageItem4', 'resource', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineLinkageItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LinkageItem4', nil, js.FHIRFactoryJs);
  defineLinkageItemPropsJs(js, def);
end;


procedure defineLinkagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Linkage4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Linkage4', 'author', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Linkage4', 'item', 'LinkageItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLinkageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Linkage4', nil, js.FHIRFactoryJs);
  defineLinkagePropsJs(js, def);
end;


procedure defineListEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ListEntry4', 'flag', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ListEntry4', 'deleted', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ListEntry4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ListEntry4', 'item', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineListEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ListEntry4', nil, js.FHIRFactoryJs);
  defineListEntryPropsJs(js, def);
end;


procedure defineListPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'List4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'List4', 'source', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List4', 'orderedBy', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List4', 'entry', 'ListEntry4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List4', 'emptyReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineListJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('List4', nil, js.FHIRFactoryJs);
  defineListPropsJs(js, def);
end;


procedure defineLocationPositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LocationPosition4', 'longitude', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition4', 'latitude', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition4', 'altitude', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineLocationPositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LocationPosition4', nil, js.FHIRFactoryJs);
  defineLocationPositionPropsJs(js, def);
end;


procedure defineLocationHoursOfOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LocationHoursOfOperation4', 'allDay', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'LocationHoursOfOperation4', 'openingTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'LocationHoursOfOperation4', 'closingTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineLocationHoursOfOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LocationHoursOfOperation4', nil, js.FHIRFactoryJs);
  defineLocationHoursOfOperationPropsJs(js, def);
end;


procedure defineLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Location4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location4', 'operationalStatus', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location4', 'address', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'physicalType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'position', 'LocationPosition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'managingOrganization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'partOf', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location4', 'hoursOfOperation', 'LocationHoursOfOperation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location4', 'availabilityExceptions', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location4', 'endpoint', 'Reference(Endpoint)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Location4', nil, js.FHIRFactoryJs);
  defineLocationPropsJs(js, def);
end;


procedure defineMeasureGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroup4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroup4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroup4', 'population', 'MeasureGroupPopulation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureGroup4', 'stratifier', 'MeasureGroupStratifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroup4', nil, js.FHIRFactoryJs);
  defineMeasureGroupPropsJs(js, def);
end;


procedure defineMeasureGroupPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupPopulation4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupPopulation4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupPopulation4', 'criteria', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureGroupPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupPopulation4', nil, js.FHIRFactoryJs);
  defineMeasureGroupPopulationPropsJs(js, def);
end;


procedure defineMeasureGroupStratifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupStratifier4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupStratifier4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupStratifier4', 'criteria', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupStratifier4', 'component', 'MeasureGroupStratifierComponent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupStratifier4', nil, js.FHIRFactoryJs);
  defineMeasureGroupStratifierPropsJs(js, def);
end;


procedure defineMeasureGroupStratifierComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupStratifierComponent4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupStratifierComponent4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupStratifierComponent4', 'criteria', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureGroupStratifierComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupStratifierComponent4', nil, js.FHIRFactoryJs);
  defineMeasureGroupStratifierComponentPropsJs(js, def);
end;


procedure defineMeasureSupplementalDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureSupplementalData4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureSupplementalData4', 'usage', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureSupplementalData4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureSupplementalData4', 'criteria', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureSupplementalDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureSupplementalData4', nil, js.FHIRFactoryJs);
  defineMeasureSupplementalDataPropsJs(js, def);
end;


procedure defineMeasurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Measure4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'subtitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Measure4', 'subjectCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'subjectReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'usage', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'disclaimer', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'scoring', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'compositeScoring', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'riskAdjustment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'rateAggregation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'rationale', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'clinicalRecommendationStatement', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'improvementNotation', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure4', 'guidance', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure4', 'group', 'MeasureGroup4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure4', 'supplementalData', 'MeasureSupplementalData4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Measure4', nil, js.FHIRFactoryJs);
  defineMeasurePropsJs(js, def);
end;


procedure defineMeasureReportGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroup4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroup4', 'population', 'MeasureReportGroupPopulation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroup4', 'measureScore', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroup4', 'stratifier', 'MeasureReportGroupStratifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureReportGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroup4', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupPropsJs(js, def);
end;


procedure defineMeasureReportGroupPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupPopulation4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupPopulation4', 'count', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupPopulation4', 'subjectResults', 'Reference(List)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupPopulation4', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupPopulationPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifier4', 'code', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroupStratifier4', 'stratum', 'MeasureReportGroupStratifierStratum4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureReportGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifier4', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierStratumPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum4', 'value', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum4', 'component', 'MeasureReportGroupStratifierStratumComponent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum4', 'population', 'MeasureReportGroupStratifierStratumPopulation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum4', 'measureScore', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratum4', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierStratumComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumComponent4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumComponent4', 'value', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratumComponent4', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumComponentPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierStratumPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation4', 'count', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation4', 'subjectResults', 'Reference(List)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratumPopulation4', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumPopulationPropsJs(js, def);
end;


procedure defineMeasureReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MeasureReport4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReport4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport4', 'measure', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MeasureReport4', 'reporter', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport4', 'improvementNotation', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport4', 'group', 'MeasureReportGroup4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReport4', 'evaluatedResource', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReport4', nil, js.FHIRFactoryJs);
  defineMeasureReportPropsJs(js, def);
end;


procedure defineMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Media4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media4', 'basedOn', 'Reference(ServiceRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media4', 'partOf', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'modality', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'view', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'createdDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Media4', 'createdPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'issued', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Media4', 'operator', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media4', 'bodySite', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'deviceName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media4', 'device', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'height', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media4', 'width', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media4', 'frames', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media4', 'duration', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Media4', 'content', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Media4', nil, js.FHIRFactoryJs);
  defineMediaPropsJs(js, def);
end;


procedure defineMedicationIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationIngredient4', 'itemCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationIngredient4', 'itemReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationIngredient4', 'isActive', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationIngredient4', 'strength', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationIngredient4', nil, js.FHIRFactoryJs);
  defineMedicationIngredientPropsJs(js, def);
end;


procedure defineMedicationBatchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationBatch4', 'lotNumber', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationBatch4', 'expirationDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineMedicationBatchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationBatch4', nil, js.FHIRFactoryJs);
  defineMedicationBatchPropsJs(js, def);
end;


procedure defineMedicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Medication4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Medication4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Medication4', 'manufacturer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication4', 'form', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication4', 'amount', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication4', 'ingredient', 'MedicationIngredient4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Medication4', 'batch', 'MedicationBatch4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Medication4', nil, js.FHIRFactoryJs);
  defineMedicationPropsJs(js, def);
end;


procedure defineMedicationAdministrationPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationPerformer4', 'function', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationPerformer4', 'actor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationAdministrationPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationPerformer4', nil, js.FHIRFactoryJs);
  defineMedicationAdministrationPerformerPropsJs(js, def);
end;


procedure defineMedicationAdministrationDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'site', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'route', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'method', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'dose', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'rateRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage4', 'rateQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationAdministrationDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationDosage4', nil, js.FHIRFactoryJs);
  defineMedicationAdministrationDosagePropsJs(js, def);
end;


procedure defineMedicationAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationAdministration4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'partOf', 'Reference(MedicationAdministration)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministration4', 'statusReason', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'medicationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'medicationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'context', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'supportingInformation', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'effectiveDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationAdministration4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'performer', 'MedicationAdministrationPerformer4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'request', 'Reference(MedicationRequest)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'device', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration4', 'dosage', 'MedicationAdministrationDosage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration4', 'eventHistory', 'Reference(Provenance)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministration4', nil, js.FHIRFactoryJs);
  defineMedicationAdministrationPropsJs(js, def);
end;


procedure defineMedicationDispensePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispensePerformer4', 'function', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispensePerformer4', 'actor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationDispensePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispensePerformer4', nil, js.FHIRFactoryJs);
  defineMedicationDispensePerformerPropsJs(js, def);
end;


procedure defineMedicationDispenseSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispenseSubstitution4', 'wasSubstituted', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationDispenseSubstitution4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseSubstitution4', 'reason', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispenseSubstitution4', 'responsibleParty', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationDispenseSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispenseSubstitution4', nil, js.FHIRFactoryJs);
  defineMedicationDispenseSubstitutionPropsJs(js, def);
end;


procedure defineMedicationDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationDispense4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'partOf', 'Reference(Procedure)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationDispense4', 'statusReasonCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'statusReasonReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'medicationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'medicationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'context', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'supportingInformation', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'performer', 'MedicationDispensePerformer4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'authorizingPrescription', 'Reference(MedicationRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'daysSupply', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'whenPrepared', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense4', 'whenHandedOver', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense4', 'destination', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'receiver', 'Reference(Patient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'dosageInstruction', 'Dosage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'substitution', 'MedicationDispenseSubstitution4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense4', 'detectedIssue', 'Reference(DetectedIssue)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense4', 'eventHistory', 'Reference(Provenance)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispense4', nil, js.FHIRFactoryJs);
  defineMedicationDispensePropsJs(js, def);
end;


procedure defineMedicationKnowledgeRelatedMedicationKnowledgePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeRelatedMedicationKnowledge4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeRelatedMedicationKnowledge4', 'reference', 'Reference(MedicationKnowledge)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeRelatedMedicationKnowledgeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeRelatedMedicationKnowledge4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeRelatedMedicationKnowledgePropsJs(js, def);
end;


procedure defineMedicationKnowledgeMonographPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeMonograph4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeMonograph4', 'source', 'Reference(DocumentReference)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeMonographJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeMonograph4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeMonographPropsJs(js, def);
end;


procedure defineMedicationKnowledgeIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeIngredient4', 'itemCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeIngredient4', 'itemReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeIngredient4', 'isActive', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationKnowledgeIngredient4', 'strength', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeIngredient4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeIngredientPropsJs(js, def);
end;


procedure defineMedicationKnowledgeCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeCost4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeCost4', 'source', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledgeCost4', 'cost', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeCost4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeCostPropsJs(js, def);
end;


procedure defineMedicationKnowledgeMonitoringProgramPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeMonitoringProgram4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeMonitoringProgram4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMedicationKnowledgeMonitoringProgramJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeMonitoringProgram4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeMonitoringProgramPropsJs(js, def);
end;


procedure defineMedicationKnowledgeAdministrationGuidelinesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines4', 'dosage', 'MedicationKnowledgeAdministrationGuidelinesDosage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines4', 'indicationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines4', 'indicationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines4', 'patientCharacteristics', 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeAdministrationGuidelinesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeAdministrationGuidelines4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeAdministrationGuidelinesPropsJs(js, def);
end;


procedure defineMedicationKnowledgeAdministrationGuidelinesDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelinesDosage4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelinesDosage4', 'dosage', 'Dosage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeAdministrationGuidelinesDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeAdministrationGuidelinesDosage4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeAdministrationGuidelinesDosagePropsJs(js, def);
end;


procedure defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics4', 'characteristicCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics4', 'characteristicQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsPropsJs(js, def);
end;


procedure defineMedicationKnowledgeMedicineClassificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeMedicineClassification4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeMedicineClassification4', 'classification', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeMedicineClassificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeMedicineClassification4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeMedicineClassificationPropsJs(js, def);
end;


procedure defineMedicationKnowledgePackagingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgePackaging4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgePackaging4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgePackagingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgePackaging4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgePackagingPropsJs(js, def);
end;


procedure defineMedicationKnowledgeDrugCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic4', 'valueBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineMedicationKnowledgeDrugCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeDrugCharacteristic4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeDrugCharacteristicPropsJs(js, def);
end;


procedure defineMedicationKnowledgeRegulatoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeRegulatory4', 'regulatoryAuthority', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeRegulatory4', 'substitution', 'MedicationKnowledgeRegulatorySubstitution4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeRegulatory4', 'schedule', 'MedicationKnowledgeRegulatorySchedule4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeRegulatory4', 'maxDispense', 'MedicationKnowledgeRegulatoryMaxDispense4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeRegulatoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeRegulatory4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeRegulatoryPropsJs(js, def);
end;


procedure defineMedicationKnowledgeRegulatorySubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeRegulatorySubstitution4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeRegulatorySubstitution4', 'allowed', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineMedicationKnowledgeRegulatorySubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeRegulatorySubstitution4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeRegulatorySubstitutionPropsJs(js, def);
end;


procedure defineMedicationKnowledgeRegulatorySchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeRegulatorySchedule4', 'schedule', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeRegulatoryScheduleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeRegulatorySchedule4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeRegulatorySchedulePropsJs(js, def);
end;


procedure defineMedicationKnowledgeRegulatoryMaxDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeRegulatoryMaxDispense4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeRegulatoryMaxDispense4', 'period', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeRegulatoryMaxDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeRegulatoryMaxDispense4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeRegulatoryMaxDispensePropsJs(js, def);
end;


procedure defineMedicationKnowledgeKineticsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeKinetics4', 'areaUnderCurve', 'Quantity4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeKinetics4', 'lethalDose50', 'Quantity4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeKinetics4', 'halfLifePeriod', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeKineticsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeKinetics4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgeKineticsPropsJs(js, def);
end;


procedure defineMedicationKnowledgePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledge4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledge4', 'manufacturer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'doseForm', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'amount', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'relatedMedicationKnowledge', 'MedicationKnowledgeRelatedMedicationKnowledge4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'associatedMedication', 'Reference(Medication)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'productType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'monograph', 'MedicationKnowledgeMonograph4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'ingredient', 'MedicationKnowledgeIngredient4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'preparationInstruction', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledge4', 'intendedRoute', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'cost', 'MedicationKnowledgeCost4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'monitoringProgram', 'MedicationKnowledgeMonitoringProgram4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'administrationGuidelines', 'MedicationKnowledgeAdministrationGuidelines4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'medicineClassification', 'MedicationKnowledgeMedicineClassification4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'packaging', 'MedicationKnowledgePackaging4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge4', 'drugCharacteristic', 'MedicationKnowledgeDrugCharacteristic4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'contraindication', 'Reference(DetectedIssue)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'regulatory', 'MedicationKnowledgeRegulatory4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge4', 'kinetics', 'MedicationKnowledgeKinetics4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledge4', nil, js.FHIRFactoryJs);
  defineMedicationKnowledgePropsJs(js, def);
end;


procedure defineMedicationRequestDispenseRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'initialFill', 'MedicationRequestDispenseRequestInitialFill4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'dispenseInterval', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'validityPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'numberOfRepeatsAllowed', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'expectedSupplyDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest4', 'performer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestDispenseRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestDispenseRequest4', nil, js.FHIRFactoryJs);
  defineMedicationRequestDispenseRequestPropsJs(js, def);
end;


procedure defineMedicationRequestDispenseRequestInitialFillPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestDispenseRequestInitialFill4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequestInitialFill4', 'duration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestDispenseRequestInitialFillJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestDispenseRequestInitialFill4', nil, js.FHIRFactoryJs);
  defineMedicationRequestDispenseRequestInitialFillPropsJs(js, def);
end;


procedure defineMedicationRequestSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestSubstitution4', 'allowedBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationRequestSubstitution4', 'allowedCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestSubstitution4', 'reason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestSubstitution4', nil, js.FHIRFactoryJs);
  defineMedicationRequestSubstitutionPropsJs(js, def);
end;


procedure defineMedicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationRequest4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest4', 'statusReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'intent', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest4', 'doNotPerform', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationRequest4', 'reportedBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationRequest4', 'reportedReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'medicationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'medicationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'supportingInformation', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'authoredOn', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationRequest4', 'requester', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'performer', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'performerType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'recorder', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'basedOn', 'Reference(CarePlan)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'groupIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'courseOfTherapyType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'insurance', 'Reference(Coverage)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'dosageInstruction', 'Dosage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'dispenseRequest', 'MedicationRequestDispenseRequest4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'substitution', 'MedicationRequestSubstitution4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'priorPrescription', 'Reference(MedicationRequest)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest4', 'detectedIssue', 'Reference(DetectedIssue)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest4', 'eventHistory', 'Reference(Provenance)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequest4', nil, js.FHIRFactoryJs);
  defineMedicationRequestPropsJs(js, def);
end;


procedure defineMedicationStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationStatement4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'basedOn', 'Reference(MedicationRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'partOf', 'Reference(MedicationAdministration)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement4', 'statusReason', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'medicationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'medicationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'context', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'effectiveDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'dateAsserted', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement4', 'informationSource', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement4', 'derivedFrom', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement4', 'dosage', 'Dosage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationStatement4', nil, js.FHIRFactoryJs);
  defineMedicationStatementPropsJs(js, def);
end;


procedure defineMedicinalProductNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductName4', 'productName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductName4', 'namePart', 'MedicinalProductNameNamePart4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductName4', 'countryLanguage', 'MedicinalProductNameCountryLanguage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductName4', nil, js.FHIRFactoryJs);
  defineMedicinalProductNamePropsJs(js, def);
end;


procedure defineMedicinalProductNameNamePartPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductNameNamePart4', 'part', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductNameNamePart4', 'type', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductNameNamePartJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductNameNamePart4', nil, js.FHIRFactoryJs);
  defineMedicinalProductNameNamePartPropsJs(js, def);
end;


procedure defineMedicinalProductNameCountryLanguagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage4', 'country', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage4', 'jurisdiction', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage4', 'language', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductNameCountryLanguageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductNameCountryLanguage4', nil, js.FHIRFactoryJs);
  defineMedicinalProductNameCountryLanguagePropsJs(js, def);
end;


procedure defineMedicinalProductManufacturingBusinessOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'operationType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'authorisationReferenceNumber', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'effectiveDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'confidentialityIndicator', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'manufacturer', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation4', 'regulator', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductManufacturingBusinessOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductManufacturingBusinessOperation4', nil, js.FHIRFactoryJs);
  defineMedicinalProductManufacturingBusinessOperationPropsJs(js, def);
end;


procedure defineMedicinalProductSpecialDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductSpecialDesignation4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation4', 'intendedUse', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation4', 'indicationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation4', 'indicationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation4', 'status', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation4', 'species', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductSpecialDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductSpecialDesignation4', nil, js.FHIRFactoryJs);
  defineMedicinalProductSpecialDesignationPropsJs(js, def);
end;


procedure defineMedicinalProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProduct4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'domain', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'combinedPharmaceuticalDoseForm', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'legalStatusOfSupply', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'additionalMonitoringIndicator', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'paediatricUseIndicator', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct4', 'productClassification', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'marketingStatus', 'MarketingStatus4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'pharmaceuticalProduct', 'Reference(MedicinalProductPharmaceutical)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'packagedMedicinalProduct', 'Reference(MedicinalProductPackaged)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'attachedDocument', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'masterFile', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'contact', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'clinicalTrial', 'Reference(ResearchStudy)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'name', 'MedicinalProductName4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'crossReference', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'manufacturingBusinessOperation', 'MedicinalProductManufacturingBusinessOperation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct4', 'specialDesignation', 'MedicinalProductSpecialDesignation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProduct4', nil, js.FHIRFactoryJs);
  defineMedicinalProductPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationJurisdictionalAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization4', 'country', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization4', 'legalStatusOfSupply', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization4', 'validityPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductAuthorizationJurisdictionalAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationJurisdictionalAuthorization4', nil, js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationJurisdictionalAuthorizationPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure4', 'datePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure4', 'dateDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure4', 'application', '@MedicinalProductAuthorization.procedure4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductAuthorizationProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationProcedure4', nil, js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationProcedurePropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'subject', 'Reference(MedicinalProduct)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'country', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'status', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'statusDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'restoreDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'validityPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'dataExclusivityPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'dateOfFirstAuthorization', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'internationalBirthDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'legalBasis', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'jurisdictionalAuthorization', 'MedicinalProductAuthorizationJurisdictionalAuthorization4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'holder', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'regulator', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization4', 'procedure', 'MedicinalProductAuthorizationProcedure4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorization4', nil, js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationPropsJs(js, def);
end;


procedure defineMedicinalProductContraindicationOtherTherapyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductContraindicationOtherTherapy4', 'therapyRelationshipType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductContraindicationOtherTherapy4', 'medicationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductContraindicationOtherTherapy4', 'medicationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductContraindicationOtherTherapyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductContraindicationOtherTherapy4', nil, js.FHIRFactoryJs);
  defineMedicinalProductContraindicationOtherTherapyPropsJs(js, def);
end;


procedure defineMedicinalProductContraindicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductContraindication4', 'subject', 'Reference(MedicinalProduct)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductContraindication4', 'disease', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductContraindication4', 'diseaseStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductContraindication4', 'comorbidity', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductContraindication4', 'therapeuticIndication', 'Reference(MedicinalProductIndication)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductContraindication4', 'otherTherapy', 'MedicinalProductContraindicationOtherTherapy4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductContraindication4', 'population', 'Population4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductContraindicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductContraindication4', nil, js.FHIRFactoryJs);
  defineMedicinalProductContraindicationPropsJs(js, def);
end;


procedure defineMedicinalProductIndicationOtherTherapyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIndicationOtherTherapy4', 'therapyRelationshipType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndicationOtherTherapy4', 'medicationCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndicationOtherTherapy4', 'medicationReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductIndicationOtherTherapyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIndicationOtherTherapy4', nil, js.FHIRFactoryJs);
  defineMedicinalProductIndicationOtherTherapyPropsJs(js, def);
end;


procedure defineMedicinalProductIndicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIndication4', 'subject', 'Reference(MedicinalProduct)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIndication4', 'diseaseSymptomProcedure', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndication4', 'diseaseStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndication4', 'comorbidity', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIndication4', 'intendedEffect', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndication4', 'duration', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndication4', 'otherTherapy', 'MedicinalProductIndicationOtherTherapy4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIndication4', 'undesirableEffect', 'Reference(MedicinalProductUndesirableEffect)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIndication4', 'population', 'Population4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIndicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIndication4', nil, js.FHIRFactoryJs);
  defineMedicinalProductIndicationPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance4', 'group', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance4', 'confidentiality', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance4', 'strength', 'MedicinalProductIngredientSpecifiedSubstanceStrength4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstance4', nil, js.FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstancePropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'presentation', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'presentationLowLimit', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'concentration', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'concentrationLowLimit', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'measurementPoint', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'country', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength4', 'referenceStrength', 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstanceStrength4', nil, js.FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength4', 'substance', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength4', 'strength', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength4', 'strengthLowLimit', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength4', 'measurementPoint', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength4', 'country', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength4', nil, js.FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSubstance4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSubstance4', 'strength', '@MedicinalProductIngredient.specifiedSubstance.strength4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSubstance4', nil, js.FHIRFactoryJs);
  defineMedicinalProductIngredientSubstancePropsJs(js, def);
end;


procedure defineMedicinalProductIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredient4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredient4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredient4', 'allergenicIndicator', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicinalProductIngredient4', 'manufacturer', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredient4', 'specifiedSubstance', 'MedicinalProductIngredientSpecifiedSubstance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredient4', 'substance', 'MedicinalProductIngredientSubstance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredient4', nil, js.FHIRFactoryJs);
  defineMedicinalProductIngredientPropsJs(js, def);
end;


procedure defineMedicinalProductInteractionInteractantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductInteractionInteractant4', 'itemReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductInteractionInteractant4', 'itemCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductInteractionInteractantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductInteractionInteractant4', nil, js.FHIRFactoryJs);
  defineMedicinalProductInteractionInteractantPropsJs(js, def);
end;


procedure defineMedicinalProductInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductInteraction4', 'subject', 'Reference(MedicinalProduct)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductInteraction4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductInteraction4', 'interactant', 'MedicinalProductInteractionInteractant4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductInteraction4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductInteraction4', 'effect', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductInteraction4', 'incidence', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductInteraction4', 'management', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductInteraction4', nil, js.FHIRFactoryJs);
  defineMedicinalProductInteractionPropsJs(js, def);
end;


procedure defineMedicinalProductManufacturedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductManufactured4', 'manufacturedDoseForm', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufactured4', 'unitOfPresentation', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufactured4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufactured4', 'manufacturer', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductManufactured4', 'ingredient', 'Reference(MedicinalProductIngredient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductManufactured4', 'physicalCharacteristics', 'ProdCharacteristic4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufactured4', 'otherCharacteristics', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductManufacturedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductManufactured4', nil, js.FHIRFactoryJs);
  defineMedicinalProductManufacturedPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedBatchIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedBatchIdentifier4', 'outerPackaging', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedBatchIdentifier4', 'immediatePackaging', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductPackagedBatchIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedBatchIdentifier4', nil, js.FHIRFactoryJs);
  defineMedicinalProductPackagedBatchIdentifierPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPackageItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'material', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'alternateMaterial', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'device', 'Reference(DeviceDefinition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'manufacturedItem', 'Reference(MedicinalProductManufactured)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'packageItem', '@MedicinalProductPackaged.packageItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'physicalCharacteristics', 'ProdCharacteristic4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'otherCharacteristics', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'shelfLifeStorage', 'ProductShelfLife4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem4', 'manufacturer', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPackagedPackageItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedPackageItem4', nil, js.FHIRFactoryJs);
  defineMedicinalProductPackagedPackageItemPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackaged4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'subject', 'Reference(MedicinalProduct)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'legalStatusOfSupply', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'marketingStatus', 'MarketingStatus4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'marketingAuthorization', 'Reference(MedicinalProductAuthorization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'manufacturer', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'batchIdentifier', 'MedicinalProductPackagedBatchIdentifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged4', 'packageItem', 'MedicinalProductPackagedPackageItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPackagedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackaged4', nil, js.FHIRFactoryJs);
  defineMedicinalProductPackagedPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalCharacteristicsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceuticalCharacteristics4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalCharacteristics4', 'status', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductPharmaceuticalCharacteristicsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceuticalCharacteristics4', nil, js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalCharacteristicsPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration4', 'firstDose', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration4', 'maxSingleDose', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration4', 'maxDosePerDay', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration4', 'maxDosePerTreatmentPeriod', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration4', 'maxTreatmentPeriod', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration4', 'targetSpecies', 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceuticalRouteOfAdministration4', nil, js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalRouteOfAdministrationPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies4', 'withdrawalPeriod', 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies4', nil, js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod4', 'tissue', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod4', 'value', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod4', 'supportingInformation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod4', nil, js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'administrableDoseForm', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'unitOfPresentation', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'ingredient', 'Reference(MedicinalProductIngredient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'device', 'Reference(DeviceDefinition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'characteristics', 'MedicinalProductPharmaceuticalCharacteristics4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical4', 'routeOfAdministration', 'MedicinalProductPharmaceuticalRouteOfAdministration4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPharmaceuticalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceutical4', nil, js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalPropsJs(js, def);
end;


procedure defineMedicinalProductUndesirableEffectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductUndesirableEffect4', 'subject', 'Reference(MedicinalProduct)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductUndesirableEffect4', 'symptomConditionEffect', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductUndesirableEffect4', 'classification', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductUndesirableEffect4', 'frequencyOfOccurrence', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductUndesirableEffect4', 'population', 'Population4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductUndesirableEffectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductUndesirableEffect4', nil, js.FHIRFactoryJs);
  defineMedicinalProductUndesirableEffectPropsJs(js, def);
end;


procedure defineMessageDefinitionFocusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionFocus4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus4', 'profile', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus4', 'min', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus4', 'max', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageDefinitionFocusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinitionFocus4', nil, js.FHIRFactoryJs);
  defineMessageDefinitionFocusPropsJs(js, def);
end;


procedure defineMessageDefinitionAllowedResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionAllowedResponse4', 'message', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionAllowedResponse4', 'situation', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageDefinitionAllowedResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinitionAllowedResponse4', nil, js.FHIRFactoryJs);
  defineMessageDefinitionAllowedResponsePropsJs(js, def);
end;


procedure defineMessageDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'MessageDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MessageDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MessageDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'base', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'eventCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinition4', 'eventUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'category', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'focus', 'MessageDefinitionFocus4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition4', 'responseRequired', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition4', 'allowedResponse', 'MessageDefinitionAllowedResponse4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMessageDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinition4', nil, js.FHIRFactoryJs);
  defineMessageDefinitionPropsJs(js, def);
end;


procedure defineMessageHeaderDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderDestination4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination4', 'target', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderDestination4', 'endpoint', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination4', 'receiver', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMessageHeaderDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderDestination4', nil, js.FHIRFactoryJs);
  defineMessageHeaderDestinationPropsJs(js, def);
end;


procedure defineMessageHeaderSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderSource4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource4', 'software', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource4', 'contact', 'ContactPoint4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderSource4', 'endpoint', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderSource4', nil, js.FHIRFactoryJs);
  defineMessageHeaderSourcePropsJs(js, def);
end;


procedure defineMessageHeaderResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderResponse4', 'identifier', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse4', 'details', 'Reference(OperationOutcome)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMessageHeaderResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderResponse4', nil, js.FHIRFactoryJs);
  defineMessageHeaderResponsePropsJs(js, def);
end;


procedure defineMessageHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MessageHeader4', 'eventCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'eventUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeader4', 'destination', 'MessageHeaderDestination4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader4', 'sender', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'enterer', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'author', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'source', 'MessageHeaderSource4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'responsible', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'reason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'response', 'MessageHeaderResponse4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader4', 'focus', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader4', 'definition', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeader4', nil, js.FHIRFactoryJs);
  defineMessageHeaderPropsJs(js, def);
end;


procedure defineMolecularSequenceReferenceSeqPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceReferenceSeq4', 'chromosome', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq4', 'genomeBuild', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq4', 'orientation', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq4', 'referenceSeqId', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq4', 'referenceSeqPointer', 'Reference(MolecularSequence)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq4', 'referenceSeqString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq4', 'strand', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq4', 'windowStart', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq4', 'windowEnd', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineMolecularSequenceReferenceSeqJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceReferenceSeq4', nil, js.FHIRFactoryJs);
  defineMolecularSequenceReferenceSeqPropsJs(js, def);
end;


procedure defineMolecularSequenceVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceVariant4', 'start', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceVariant4', 'end_', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceVariant4', 'observedAllele', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceVariant4', 'referenceAllele', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceVariant4', 'cigar', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceVariant4', 'variantPointer', 'Reference(Observation)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMolecularSequenceVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceVariant4', nil, js.FHIRFactoryJs);
  defineMolecularSequenceVariantPropsJs(js, def);
end;


procedure defineMolecularSequenceQualityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceQuality4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'standardSequence', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'start', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'end_', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'score', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'method', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'truthTP', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'queryTP', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'truthFN', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'queryFP', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'gtFP', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'precision', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'recall', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'fScore_', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality4', 'roc', 'MolecularSequenceQualityRoc4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMolecularSequenceQualityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceQuality4', nil, js.FHIRFactoryJs);
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
  def := js.defineClass('MolecularSequenceQualityRoc4', nil, js.FHIRFactoryJs);
  defineMolecularSequenceQualityRocPropsJs(js, def);
end;


procedure defineMolecularSequenceRepositoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceRepository4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceRepository4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceRepository4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceRepository4', 'datasetId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceRepository4', 'variantsetId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceRepository4', 'readsetId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMolecularSequenceRepositoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceRepository4', nil, js.FHIRFactoryJs);
  defineMolecularSequenceRepositoryPropsJs(js, def);
end;


procedure defineMolecularSequenceStructureVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceStructureVariant4', 'variantType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceStructureVariant4', 'exact', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MolecularSequenceStructureVariant4', 'length', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceStructureVariant4', 'outer', 'MolecularSequenceStructureVariantOuter4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceStructureVariant4', 'inner', 'MolecularSequenceStructureVariantInner4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMolecularSequenceStructureVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceStructureVariant4', nil, js.FHIRFactoryJs);
  defineMolecularSequenceStructureVariantPropsJs(js, def);
end;


procedure defineMolecularSequenceStructureVariantOuterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceStructureVariantOuter4', 'start', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceStructureVariantOuter4', 'end_', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineMolecularSequenceStructureVariantOuterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceStructureVariantOuter4', nil, js.FHIRFactoryJs);
  defineMolecularSequenceStructureVariantOuterPropsJs(js, def);
end;


procedure defineMolecularSequenceStructureVariantInnerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceStructureVariantInner4', 'start', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceStructureVariantInner4', 'end_', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineMolecularSequenceStructureVariantInnerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceStructureVariantInner4', nil, js.FHIRFactoryJs);
  defineMolecularSequenceStructureVariantInnerPropsJs(js, def);
end;


procedure defineMolecularSequencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MolecularSequence4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MolecularSequence4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequence4', 'coordinateSystem', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequence4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence4', 'specimen', 'Reference(Specimen)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence4', 'device', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence4', 'performer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence4', 'referenceSeq', 'MolecularSequenceReferenceSeq4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence4', 'variant', 'MolecularSequenceVariant4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MolecularSequence4', 'observedSeq', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequence4', 'quality', 'MolecularSequenceQuality4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MolecularSequence4', 'readCoverage', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequence4', 'repository', 'MolecularSequenceRepository4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MolecularSequence4', 'pointer', 'Reference(MolecularSequence)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MolecularSequence4', 'structureVariant', 'MolecularSequenceStructureVariant4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMolecularSequenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequence4', nil, js.FHIRFactoryJs);
  defineMolecularSequencePropsJs(js, def);
end;


procedure defineNamingSystemUniqueIdPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NamingSystemUniqueId4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId4', 'preferred', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'NamingSystemUniqueId4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNamingSystemUniqueIdJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystemUniqueId4', nil, js.FHIRFactoryJs);
  defineNamingSystemUniqueIdPropsJs(js, def);
end;


procedure defineNamingSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'NamingSystem4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'kind', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NamingSystem4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem4', 'responsible', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NamingSystem4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem4', 'usage', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem4', 'uniqueId', 'NamingSystemUniqueId4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineNamingSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystem4', nil, js.FHIRFactoryJs);
  defineNamingSystemPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'schedule', 'Timing4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'nutrient', 'NutritionOrderOralDietNutrient4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'texture', 'NutritionOrderOralDietTexture4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'fluidConsistencyType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet4', 'instruction', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderOralDietJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDiet4', nil, js.FHIRFactoryJs);
  defineNutritionOrderOralDietPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietNutrientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietNutrient4', 'modifier', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietNutrient4', 'amount', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietNutrientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietNutrient4', nil, js.FHIRFactoryJs);
  defineNutritionOrderOralDietNutrientPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietTexturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietTexture4', 'modifier', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietTexture4', 'foodType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietTextureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietTexture4', nil, js.FHIRFactoryJs);
  defineNutritionOrderOralDietTexturePropsJs(js, def);
end;


procedure defineNutritionOrderSupplementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderSupplement4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement4', 'productName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderSupplement4', 'schedule', 'Timing4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderSupplement4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement4', 'instruction', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderSupplementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderSupplement4', nil, js.FHIRFactoryJs);
  defineNutritionOrderSupplementPropsJs(js, def);
end;


procedure defineNutritionOrderEnteralFormulaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'baseFormulaType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'baseFormulaProductName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'additiveType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'additiveProductName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'caloricDensity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'routeofAdministration', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'administration', 'NutritionOrderEnteralFormulaAdministration4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'maxVolumeToDeliver', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula4', 'administrationInstruction', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderEnteralFormulaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormula4', nil, js.FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaPropsJs(js, def);
end;


procedure defineNutritionOrderEnteralFormulaAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration4', 'schedule', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration4', 'rateQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration4', 'rateRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderEnteralFormulaAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormulaAdministration4', nil, js.FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaAdministrationPropsJs(js, def);
end;


procedure defineNutritionOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'NutritionOrder4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrder4', 'intent', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrder4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder4', 'dateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NutritionOrder4', 'orderer', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder4', 'allergyIntolerance', 'Reference(AllergyIntolerance)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder4', 'foodPreferenceModifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder4', 'excludeFoodModifier', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder4', 'oralDiet', 'NutritionOrderOralDiet4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder4', 'supplement', 'NutritionOrderSupplement4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder4', 'enteralFormula', 'NutritionOrderEnteralFormula4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineNutritionOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrder4', nil, js.FHIRFactoryJs);
  defineNutritionOrderPropsJs(js, def);
end;


procedure defineObservationReferenceRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationReferenceRange4', 'low', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange4', 'high', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange4', 'appliesTo', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationReferenceRange4', 'age', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineObservationReferenceRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationReferenceRange4', nil, js.FHIRFactoryJs);
  defineObservationReferenceRangePropsJs(js, def);
end;


procedure defineObservationComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationComponent4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ObservationComponent4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ObservationComponent4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ObservationComponent4', 'valuePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'dataAbsentReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent4', 'interpretation', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationComponent4', 'referenceRange', '@Observation.referenceRange4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineObservationComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationComponent4', nil, js.FHIRFactoryJs);
  defineObservationComponentPropsJs(js, def);
end;


procedure defineObservationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Observation4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'basedOn', 'Reference(CarePlan)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'partOf', 'Reference(MedicationAdministration)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'focus', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'effectiveDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'effectiveTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'effectiveInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation4', 'issued', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation4', 'performer', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Observation4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Observation4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'valueRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'valueSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation4', 'valuePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'dataAbsentReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'interpretation', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'bodySite', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'method', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'specimen', 'Reference(Specimen)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'device', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation4', 'referenceRange', 'ObservationReferenceRange4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'hasMember', 'Reference(Observation)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'derivedFrom', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation4', 'component', 'ObservationComponent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineObservationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Observation4', nil, js.FHIRFactoryJs);
  defineObservationPropsJs(js, def);
end;


procedure defineObservationDefinitionQuantitativeDetailsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails4', 'customaryUnit', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails4', 'unit', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails4', 'conversionFactor', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails4', 'decimalPrecision', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineObservationDefinitionQuantitativeDetailsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinitionQuantitativeDetails4', nil, js.FHIRFactoryJs);
  defineObservationDefinitionQuantitativeDetailsPropsJs(js, def);
end;


procedure defineObservationDefinitionQualifiedIntervalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'category', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'range', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'context', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'appliesTo', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'gender', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'age', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'gestationalAge', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval4', 'condition', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineObservationDefinitionQualifiedIntervalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinitionQualifiedInterval4', nil, js.FHIRFactoryJs);
  defineObservationDefinitionQualifiedIntervalPropsJs(js, def);
end;


procedure defineObservationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ObservationDefinition4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition4', 'multipleResultsAllowed', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ObservationDefinition4', 'method', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition4', 'preferredReportName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition4', 'quantitativeDetails', 'ObservationDefinitionQuantitativeDetails4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition4', 'qualifiedInterval', 'ObservationDefinitionQualifiedInterval4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition4', 'validCodedValueSet', 'Reference(ValueSet)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition4', 'normalCodedValueSet', 'Reference(ValueSet)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition4', 'abnormalCodedValueSet', 'Reference(ValueSet)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition4', 'criticalCodedValueSet', 'Reference(ValueSet)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineObservationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinition4', nil, js.FHIRFactoryJs);
  defineObservationDefinitionPropsJs(js, def);
end;


procedure defineOperationDefinitionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameter4', 'name', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'use', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'min', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'max', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'documentation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'searchType', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'binding', 'OperationDefinitionParameterBinding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'referencedFrom', 'OperationDefinitionParameterReferencedFrom4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinitionParameter4', 'part', '@OperationDefinition.parameter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameter4', nil, js.FHIRFactoryJs);
  defineOperationDefinitionParameterPropsJs(js, def);
end;


procedure defineOperationDefinitionParameterBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameterBinding4', 'strength', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterBinding4', 'valueSet', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationDefinitionParameterBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameterBinding4', nil, js.FHIRFactoryJs);
  defineOperationDefinitionParameterBindingPropsJs(js, def);
end;


procedure defineOperationDefinitionParameterReferencedFromPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameterReferencedFrom4', 'source', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterReferencedFrom4', 'sourceId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationDefinitionParameterReferencedFromJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameterReferencedFrom4', nil, js.FHIRFactoryJs);
  defineOperationDefinitionParameterReferencedFromPropsJs(js, def);
end;


procedure defineOperationDefinitionOverloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionOverload4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationDefinitionOverloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionOverload4', nil, js.FHIRFactoryJs);
  defineOperationDefinitionOverloadPropsJs(js, def);
end;


procedure defineOperationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'OperationDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'kind', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OperationDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'affectsState', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'comment', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'base', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'system', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition4', 'type_', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition4', 'instance', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition4', 'inputProfile', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'outputProfile', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition4', 'parameter', 'OperationDefinitionParameter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition4', 'overload', 'OperationDefinitionOverload4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinition4', nil, js.FHIRFactoryJs);
  defineOperationDefinitionPropsJs(js, def);
end;


procedure defineOperationOutcomeIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationOutcomeIssue4', 'severity', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue4', 'details', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationOutcomeIssue4', 'diagnostics', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationOutcomeIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcomeIssue4', nil, js.FHIRFactoryJs);
  defineOperationOutcomeIssuePropsJs(js, def);
end;


procedure defineOperationOutcomePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OperationOutcome4', 'issue', 'OperationOutcomeIssue4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationOutcomeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcome4', nil, js.FHIRFactoryJs);
  defineOperationOutcomePropsJs(js, def);
end;


procedure defineOrganizationContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationContact4', 'purpose', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact4', 'name', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationContact4', 'address', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOrganizationContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationContact4', nil, js.FHIRFactoryJs);
  defineOrganizationContactPropsJs(js, def);
end;


procedure defineOrganizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Organization4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Organization4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Organization4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization4', 'address', 'Address4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization4', 'partOf', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Organization4', 'contact', 'OrganizationContact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization4', 'endpoint', 'Reference(Endpoint)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrganizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Organization4', nil, js.FHIRFactoryJs);
  defineOrganizationPropsJs(js, def);
end;


procedure defineOrganizationAffiliationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OrganizationAffiliation4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'organization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'participatingOrganization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'network', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'code', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'specialty', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'location', 'Reference(Location)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'healthcareService', 'Reference(HealthcareService)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation4', 'endpoint', 'Reference(Endpoint)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrganizationAffiliationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationAffiliation4', nil, js.FHIRFactoryJs);
  defineOrganizationAffiliationPropsJs(js, def);
end;


procedure definePatientContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientContact4', 'relationship', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PatientContact4', 'name', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PatientContact4', 'address', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact4', 'gender', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PatientContact4', 'organization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePatientContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientContact4', nil, js.FHIRFactoryJs);
  definePatientContactPropsJs(js, def);
end;


procedure definePatientCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientCommunication4', 'language', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientCommunication4', 'preferred', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure definePatientCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientCommunication4', nil, js.FHIRFactoryJs);
  definePatientCommunicationPropsJs(js, def);
end;


procedure definePatientLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientLink4', 'other', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientLink4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePatientLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientLink4', nil, js.FHIRFactoryJs);
  definePatientLinkPropsJs(js, def);
end;


procedure definePatientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Patient4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient4', 'name', 'HumanName4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'gender', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Patient4', 'birthDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Patient4', 'deceasedBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient4', 'deceasedDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Patient4', 'address', 'Address4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'maritalStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient4', 'multipleBirthBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient4', 'multipleBirthInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Patient4', 'photo', 'Attachment4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'contact', 'PatientContact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'communication', 'PatientCommunication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'generalPractitioner', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient4', 'managingOrganization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient4', 'link', 'PatientLink4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePatientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Patient4', nil, js.FHIRFactoryJs);
  definePatientPropsJs(js, def);
end;


procedure definePaymentNoticePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentNotice4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentNotice4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentNotice4', 'request', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'response', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice4', 'provider', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'payment', 'Reference(PaymentReconciliation)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'paymentDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice4', 'payee', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'recipient', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'amount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice4', 'paymentStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePaymentNoticeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentNotice4', nil, js.FHIRFactoryJs);
  definePaymentNoticePropsJs(js, def);
end;


procedure definePaymentReconciliationDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'predecessor', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'request', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'submitter', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'response', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'date', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'responsible', 'Reference(PractitionerRole)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'payee', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail4', 'amount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePaymentReconciliationDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationDetail4', nil, js.FHIRFactoryJs);
  definePaymentReconciliationDetailPropsJs(js, def);
end;


procedure definePaymentReconciliationProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationProcessNote4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliationProcessNote4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePaymentReconciliationProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationProcessNote4', nil, js.FHIRFactoryJs);
  definePaymentReconciliationProcessNotePropsJs(js, def);
end;


procedure definePaymentReconciliationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliation4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliation4', 'paymentIssuer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'request', 'Reference(Task)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'requestor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'outcome', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation4', 'disposition', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation4', 'paymentDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliation4', 'paymentAmount', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'paymentIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'detail', 'PaymentReconciliationDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation4', 'formCode', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation4', 'processNote', 'PaymentReconciliationProcessNote4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePaymentReconciliationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliation4', nil, js.FHIRFactoryJs);
  definePaymentReconciliationPropsJs(js, def);
end;


procedure definePersonLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PersonLink4', 'target', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PersonLink4', 'assurance', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePersonLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PersonLink4', nil, js.FHIRFactoryJs);
  definePersonLinkPropsJs(js, def);
end;


procedure definePersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Person4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person4', 'name', 'HumanName4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person4', 'gender', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Person4', 'birthDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Person4', 'address', 'Address4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person4', 'photo', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Person4', 'managingOrganization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Person4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Person4', 'link', 'PersonLink4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Person4', nil, js.FHIRFactoryJs);
  definePersonPropsJs(js, def);
end;


procedure definePlanDefinitionGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionGoal4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'description', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'priority', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'start', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'addresses', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'documentation', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionGoal4', 'target', 'PlanDefinitionGoalTarget4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionGoal4', nil, js.FHIRFactoryJs);
  definePlanDefinitionGoalPropsJs(js, def);
end;


procedure definePlanDefinitionGoalTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionGoalTarget4', 'measure', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget4', 'detailQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget4', 'detailRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget4', 'detailCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget4', 'due', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionGoalTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionGoalTarget4', nil, js.FHIRFactoryJs);
  definePlanDefinitionGoalTargetPropsJs(js, def);
end;


procedure definePlanDefinitionActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionAction4', 'prefix', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'textEquivalent', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'code', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'reason', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'documentation', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'subjectCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'subjectReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'trigger', 'TriggerDefinition4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'condition', 'PlanDefinitionActionCondition4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'input', 'DataRequirement4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'output', 'DataRequirement4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'relatedAction', 'PlanDefinitionActionRelatedAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'timingTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'participant', 'PlanDefinitionActionParticipant4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'groupingBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'selectionBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'requiredBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'precheckBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'cardinalityBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'definitionCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'definitionUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'transform', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'dynamicValue', 'PlanDefinitionActionDynamicValue4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction4', 'action', '@PlanDefinition.action4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionAction4', nil, js.FHIRFactoryJs);
  definePlanDefinitionActionPropsJs(js, def);
end;


procedure definePlanDefinitionActionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionCondition4', 'kind', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition4', 'expression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionCondition4', nil, js.FHIRFactoryJs);
  definePlanDefinitionActionConditionPropsJs(js, def);
end;


procedure definePlanDefinitionActionRelatedActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction4', 'actionId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction4', 'relationship', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction4', 'offsetDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction4', 'offsetRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionRelatedActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionRelatedAction4', nil, js.FHIRFactoryJs);
  definePlanDefinitionActionRelatedActionPropsJs(js, def);
end;


procedure definePlanDefinitionActionParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionParticipant4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionParticipant4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionParticipant4', nil, js.FHIRFactoryJs);
  definePlanDefinitionActionParticipantPropsJs(js, def);
end;


procedure definePlanDefinitionActionDynamicValuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue4', 'expression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionDynamicValueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionDynamicValue4', nil, js.FHIRFactoryJs);
  definePlanDefinitionActionDynamicValuePropsJs(js, def);
end;


procedure definePlanDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'PlanDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'subtitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PlanDefinition4', 'subjectCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition4', 'subjectReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'usage', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'goal', 'PlanDefinitionGoal4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition4', 'action', 'PlanDefinitionAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinition4', nil, js.FHIRFactoryJs);
  definePlanDefinitionPropsJs(js, def);
end;


procedure definePractitionerQualificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerQualification4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerQualification4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification4', 'issuer', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePractitionerQualificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerQualification4', nil, js.FHIRFactoryJs);
  definePractitionerQualificationPropsJs(js, def);
end;


procedure definePractitionerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Practitioner4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Practitioner4', 'name', 'HumanName4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'address', 'Address4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'gender', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Practitioner4', 'birthDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Practitioner4', 'photo', 'Attachment4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'qualification', 'PractitionerQualification4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner4', 'communication', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePractitionerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Practitioner4', nil, js.FHIRFactoryJs);
  definePractitionerPropsJs(js, def);
end;


procedure definePractitionerRoleAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerRoleAvailableTime4', 'allDay', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PractitionerRoleAvailableTime4', 'availableStartTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRoleAvailableTime4', 'availableEndTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePractitionerRoleAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRoleAvailableTime4', nil, js.FHIRFactoryJs);
  definePractitionerRoleAvailableTimePropsJs(js, def);
end;


procedure definePractitionerRoleNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerRoleNotAvailable4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRoleNotAvailable4', 'during', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePractitionerRoleNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRoleNotAvailable4', nil, js.FHIRFactoryJs);
  definePractitionerRoleNotAvailablePropsJs(js, def);
end;


procedure definePractitionerRolePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PractitionerRole4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PractitionerRole4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole4', 'practitioner', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole4', 'organization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole4', 'code', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'specialty', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'location', 'Reference(Location)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'healthcareService', 'Reference(HealthcareService)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'availableTime', 'PractitionerRoleAvailableTime4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'notAvailable', 'PractitionerRoleNotAvailable4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole4', 'availabilityExceptions', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRole4', 'endpoint', 'Reference(Endpoint)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePractitionerRoleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRole4', nil, js.FHIRFactoryJs);
  definePractitionerRolePropsJs(js, def);
end;


procedure defineProcedurePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedurePerformer4', 'function', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer4', 'actor', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer4', 'onBehalfOf', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedurePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedurePerformer4', nil, js.FHIRFactoryJs);
  defineProcedurePerformerPropsJs(js, def);
end;


procedure defineProcedureFocalDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedureFocalDevice4', 'action', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureFocalDevice4', 'manipulated', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedureFocalDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedureFocalDevice4', nil, js.FHIRFactoryJs);
  defineProcedureFocalDevicePropsJs(js, def);
end;


procedure defineProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Procedure4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'basedOn', 'Reference(CarePlan)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'partOf', 'Reference(Procedure)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Procedure4', 'statusReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'performedDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Procedure4', 'performedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'performedString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Procedure4', 'performedAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'performedRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'recorder', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'asserter', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'performer', 'ProcedurePerformer4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'bodySite', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'outcome', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure4', 'report', 'Reference(DiagnosticReport)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'complication', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'complicationDetail', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'followUp', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'focalDevice', 'ProcedureFocalDevice4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'usedReference', 'Reference(Device)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure4', 'usedCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Procedure4', nil, js.FHIRFactoryJs);
  defineProcedurePropsJs(js, def);
end;


procedure defineProvenanceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceAgent4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent4', 'role', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProvenanceAgent4', 'who', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent4', 'onBehalfOf', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProvenanceAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceAgent4', nil, js.FHIRFactoryJs);
  defineProvenanceAgentPropsJs(js, def);
end;


procedure defineProvenanceEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceEntity4', 'role', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity4', 'what', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity4', 'agent', '@Provenance.agent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProvenanceEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceEntity4', nil, js.FHIRFactoryJs);
  defineProvenanceEntityPropsJs(js, def);
end;


procedure defineProvenancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Provenance4', 'target', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance4', 'occurredPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance4', 'occurredDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance4', 'recorded', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance4', 'reason', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance4', 'activity', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance4', 'agent', 'ProvenanceAgent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance4', 'entity', 'ProvenanceEntity4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance4', 'signature', 'Signature4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProvenanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Provenance4', nil, js.FHIRFactoryJs);
  defineProvenancePropsJs(js, def);
end;


procedure defineQuestionnaireItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItem4', 'linkId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'definition', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'code', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem4', 'prefix', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'enableWhen', 'QuestionnaireItemEnableWhen4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem4', 'enableBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'required', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem4', 'repeats', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem4', 'readOnly', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem4', 'maxLength', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItem4', 'answerValueSet', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem4', 'answerOption', 'QuestionnaireItemAnswerOption4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem4', 'initial', 'QuestionnaireItemInitial4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem4', 'item', '@Questionnaire.item4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItem4', nil, js.FHIRFactoryJs);
  defineQuestionnaireItemPropsJs(js, def);
end;


procedure defineQuestionnaireItemEnableWhenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'question', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'operator', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen4', 'answerReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineQuestionnaireItemEnableWhenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemEnableWhen4', nil, js.FHIRFactoryJs);
  defineQuestionnaireItemEnableWhenPropsJs(js, def);
end;


procedure defineQuestionnaireItemAnswerOptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemAnswerOption4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption4', 'valueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption4', 'valueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption4', 'initialSelected', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineQuestionnaireItemAnswerOptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemAnswerOption4', nil, js.FHIRFactoryJs);
  defineQuestionnaireItemAnswerOptionPropsJs(js, def);
end;


procedure defineQuestionnaireItemInitialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemInitial4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineQuestionnaireItemInitialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemInitial4', nil, js.FHIRFactoryJs);
  defineQuestionnaireItemInitialPropsJs(js, def);
end;


procedure defineQuestionnairePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Questionnaire4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Questionnaire4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Questionnaire4', 'code', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire4', 'item', 'QuestionnaireItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Questionnaire4', nil, js.FHIRFactoryJs);
  defineQuestionnairePropsJs(js, def);
end;


procedure defineQuestionnaireResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'linkId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'definition', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'text', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'answer', 'QuestionnaireResponseItemAnswer4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponseItem4', 'item', '@QuestionnaireResponse.item4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseItem4', nil, js.FHIRFactoryJs);
  defineQuestionnaireResponseItemPropsJs(js, def);
end;


procedure defineQuestionnaireResponseItemAnswerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer4', 'item', '@QuestionnaireResponse.item4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseItemAnswerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseItemAnswer4', nil, js.FHIRFactoryJs);
  defineQuestionnaireResponseItemAnswerPropsJs(js, def);
end;


procedure defineQuestionnaireResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponse4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'basedOn', 'Reference(CarePlan)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'partOf', 'Reference(Observation)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'questionnaire', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'subject', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'authored', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'author', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'source', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse4', 'item', 'QuestionnaireResponseItem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponse4', nil, js.FHIRFactoryJs);
  defineQuestionnaireResponsePropsJs(js, def);
end;


procedure defineRelatedPersonCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RelatedPersonCommunication4', 'language', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPersonCommunication4', 'preferred', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineRelatedPersonCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedPersonCommunication4', nil, js.FHIRFactoryJs);
  defineRelatedPersonCommunicationPropsJs(js, def);
end;


procedure defineRelatedPersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RelatedPerson4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'RelatedPerson4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson4', 'relationship', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'name', 'HumanName4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'telecom', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'gender', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedPerson4', 'birthDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RelatedPerson4', 'address', 'Address4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'photo', 'Attachment4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson4', 'communication', 'RelatedPersonCommunication4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRelatedPersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedPerson4', nil, js.FHIRFactoryJs);
  defineRelatedPersonPropsJs(js, def);
end;


procedure defineRequestGroupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupAction4', 'prefix', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'textEquivalent', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'code', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction4', 'documentation', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction4', 'condition', 'RequestGroupActionCondition4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction4', 'relatedAction', 'RequestGroupActionRelatedAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'timingTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'participant', 'Reference(Patient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'groupingBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'selectionBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'requiredBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'precheckBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'cardinalityBehavior', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction4', 'resource', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction4', 'action', '@RequestGroup.action4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRequestGroupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupAction4', nil, js.FHIRFactoryJs);
  defineRequestGroupActionPropsJs(js, def);
end;


procedure defineRequestGroupActionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupActionCondition4', 'kind', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition4', 'expression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRequestGroupActionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupActionCondition4', nil, js.FHIRFactoryJs);
  defineRequestGroupActionConditionPropsJs(js, def);
end;


procedure defineRequestGroupActionRelatedActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupActionRelatedAction4', 'actionId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction4', 'relationship', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction4', 'offsetDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction4', 'offsetRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRequestGroupActionRelatedActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupActionRelatedAction4', nil, js.FHIRFactoryJs);
  defineRequestGroupActionRelatedActionPropsJs(js, def);
end;


procedure defineRequestGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RequestGroup4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'basedOn', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'replaces', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'groupIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup4', 'intent', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup4', 'authoredOn', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroup4', 'author', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup4', 'action', 'RequestGroupAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRequestGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroup4', nil, js.FHIRFactoryJs);
  defineRequestGroupPropsJs(js, def);
end;


procedure defineResearchDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'shortTitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'subtitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ResearchDefinition4', 'subjectCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition4', 'subjectReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'usage', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchDefinition4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchDefinition4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition4', 'population', 'Reference(ResearchElementDefinition)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition4', 'exposure', 'Reference(ResearchElementDefinition)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition4', 'exposureAlternative', 'Reference(ResearchElementDefinition)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition4', 'outcome', 'Reference(ResearchElementDefinition)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineResearchDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchDefinition4', nil, js.FHIRFactoryJs);
  defineResearchDefinitionPropsJs(js, def);
end;


procedure defineResearchElementDefinitionCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'definitionCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'definitionCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'definitionExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'definitionDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'usageContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'exclude', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'unitOfMeasure', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'studyEffectiveDescription', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'studyEffectiveDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'studyEffectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'studyEffectiveDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'studyEffectiveTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'studyEffectiveTimeFromStart', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'studyEffectiveGroupMeasure', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'participantEffectiveDescription', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'participantEffectiveDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'participantEffectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'participantEffectiveDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'participantEffectiveTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'participantEffectiveTimeFromStart', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic4', 'participantEffectiveGroupMeasure', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineResearchElementDefinitionCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchElementDefinitionCharacteristic4', nil, js.FHIRFactoryJs);
  defineResearchElementDefinitionCharacteristicPropsJs(js, def);
end;


procedure defineResearchElementDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchElementDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'shortTitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'subtitle', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'subjectCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'subjectReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'usage', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'variableType', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition4', 'characteristic', 'ResearchElementDefinitionCharacteristic4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineResearchElementDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchElementDefinition4', nil, js.FHIRFactoryJs);
  defineResearchElementDefinitionPropsJs(js, def);
end;


procedure defineResearchStudyArmPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchStudyArm4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudyArm4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudyArm4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineResearchStudyArmJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudyArm4', nil, js.FHIRFactoryJs);
  defineResearchStudyArmPropsJs(js, def);
end;


procedure defineResearchStudyObjectivePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchStudyObjective4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudyObjective4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineResearchStudyObjectiveJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudyObjective4', nil, js.FHIRFactoryJs);
  defineResearchStudyObjectivePropsJs(js, def);
end;


procedure defineResearchStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchStudy4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy4', 'protocol', 'Reference(PlanDefinition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'partOf', 'Reference(ResearchStudy)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy4', 'primaryPurposeType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'phase', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'focus', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'condition', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'keyword', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'location', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy4', 'enrollment', 'Reference(Group)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'sponsor', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'principalInvestigator', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'site', 'Reference(Location)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'reasonStopped', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'arm', 'ResearchStudyArm4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy4', 'objective', 'ResearchStudyObjective4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineResearchStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudy4', nil, js.FHIRFactoryJs);
  defineResearchStudyPropsJs(js, def);
end;


procedure defineResearchSubjectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchSubject4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchSubject4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject4', 'study', 'Reference(ResearchStudy)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject4', 'individual', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject4', 'assignedArm', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject4', 'actualArm', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject4', 'consent', 'Reference(Consent)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineResearchSubjectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchSubject4', nil, js.FHIRFactoryJs);
  defineResearchSubjectPropsJs(js, def);
end;


procedure defineRiskAssessmentPredictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'outcome', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'probabilityDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'probabilityRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'qualitativeRisk', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'relativeRisk', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'whenPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'whenRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction4', 'rationale', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRiskAssessmentPredictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessmentPrediction4', nil, js.FHIRFactoryJs);
  defineRiskAssessmentPredictionPropsJs(js, def);
end;


procedure defineRiskAssessmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RiskAssessment4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment4', 'basedOn', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'parent', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment4', 'method', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'occurrenceDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskAssessment4', 'occurrencePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'condition', 'Reference(Condition)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'performer', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment4', 'basis', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment4', 'prediction', 'RiskAssessmentPrediction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment4', 'mitigation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskAssessmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessment4', nil, js.FHIRFactoryJs);
  defineRiskAssessmentPropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisSampleSizePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesisSampleSize4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesisSampleSize4', 'numberOfStudies', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'RiskEvidenceSynthesisSampleSize4', 'numberOfParticipants', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineRiskEvidenceSynthesisSampleSizeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesisSampleSize4', nil, js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisSampleSizePropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisRiskEstimatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate4', 'value', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate4', 'unitOfMeasure', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate4', 'denominatorCount', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate4', 'numeratorCount', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate4', 'precisionEstimate', 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskEvidenceSynthesisRiskEstimateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesisRiskEstimate4', nil, js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisRiskEstimatePropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisRiskEstimatePrecisionEstimatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate4', 'level', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate4', 'from', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate4', 'to_', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineRiskEvidenceSynthesisRiskEstimatePrecisionEstimateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesisRiskEstimatePrecisionEstimate4', nil, js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisRiskEstimatePrecisionEstimatePropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisCertaintyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesisCertainty4', 'rating', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesisCertainty4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesisCertainty4', 'certaintySubcomponent', 'RiskEvidenceSynthesisCertaintyCertaintySubcomponent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskEvidenceSynthesisCertaintyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesisCertainty4', nil, js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisCertaintyPropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisCertaintyCertaintySubcomponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesisCertaintyCertaintySubcomponent4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesisCertaintyCertaintySubcomponent4', 'rating', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesisCertaintyCertaintySubcomponent4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskEvidenceSynthesisCertaintyCertaintySubcomponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesisCertaintyCertaintySubcomponent4', nil, js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisCertaintyCertaintySubcomponentPropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'approvalDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'lastReviewDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'effectivePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'topic', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'author', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'editor', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'reviewer', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'endorser', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'relatedArtifact', 'RelatedArtifact4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'synthesisType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'studyType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'population', 'Reference(EvidenceVariable)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'exposure', 'Reference(EvidenceVariable)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'outcome', 'Reference(EvidenceVariable)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'sampleSize', 'RiskEvidenceSynthesisSampleSize4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'riskEstimate', 'RiskEvidenceSynthesisRiskEstimate4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis4', 'certainty', 'RiskEvidenceSynthesisCertainty4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskEvidenceSynthesisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesis4', nil, js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisPropsJs(js, def);
end;


procedure defineSchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Schedule4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule4', 'active', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Schedule4', 'serviceCategory', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule4', 'serviceType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule4', 'specialty', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule4', 'actor', 'Reference(Patient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule4', 'planningHorizon', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Schedule4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineScheduleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Schedule4', nil, js.FHIRFactoryJs);
  defineSchedulePropsJs(js, def);
end;


procedure defineSearchParameterComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SearchParameterComponent4', 'definition', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameterComponent4', 'expression', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSearchParameterComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameterComponent4', nil, js.FHIRFactoryJs);
  defineSearchParameterComponentPropsJs(js, def);
end;


procedure defineSearchParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'SearchParameter4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'derivedFrom', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SearchParameter4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'expression', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'xpath', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'xpathUsage', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter4', 'multipleOr', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter4', 'multipleAnd', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter4', 'component', 'SearchParameterComponent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSearchParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameter4', nil, js.FHIRFactoryJs);
  defineSearchParameterPropsJs(js, def);
end;


procedure defineServiceRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ServiceRequest4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'basedOn', 'Reference(CarePlan)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'replaces', 'Reference(ServiceRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'requisition', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest4', 'intent', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest4', 'doNotPerform', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ServiceRequest4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'orderDetail', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'quantityQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'quantityRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'quantityRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'occurrenceDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceRequest4', 'occurrencePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'occurrenceTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'asNeededBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ServiceRequest4', 'asNeededCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'authoredOn', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceRequest4', 'requester', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'performerType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest4', 'performer', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'locationCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'locationReference', 'Reference(Location)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'insurance', 'Reference(Coverage)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'supportingInfo', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'specimen', 'Reference(Specimen)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'bodySite', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest4', 'patientInstruction', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest4', 'relevantHistory', 'Reference(Provenance)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineServiceRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ServiceRequest4', nil, js.FHIRFactoryJs);
  defineServiceRequestPropsJs(js, def);
end;


procedure defineSlotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Slot4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot4', 'serviceCategory', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot4', 'serviceType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot4', 'specialty', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot4', 'appointmentType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot4', 'schedule', 'Reference(Schedule)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Slot4', 'start', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Slot4', 'end_', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Slot4', 'overbooked', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Slot4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSlotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Slot4', nil, js.FHIRFactoryJs);
  defineSlotPropsJs(js, def);
end;


procedure defineSpecimenCollectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenCollection4', 'collector', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'collectedDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenCollection4', 'collectedPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'duration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'method', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'bodySite', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'fastingStatusCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection4', 'fastingStatusDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenCollectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenCollection4', nil, js.FHIRFactoryJs);
  defineSpecimenCollectionPropsJs(js, def);
end;


procedure defineSpecimenProcessingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenProcessing4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenProcessing4', 'procedure', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenProcessing4', 'additive', 'Reference(Substance)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenProcessing4', 'timeDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenProcessing4', 'timePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenProcessingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenProcessing4', nil, js.FHIRFactoryJs);
  defineSpecimenProcessingPropsJs(js, def);
end;


procedure defineSpecimenContainerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenContainer4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenContainer4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenContainer4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer4', 'capacity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer4', 'specimenQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer4', 'additiveCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer4', 'additiveReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenContainerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenContainer4', nil, js.FHIRFactoryJs);
  defineSpecimenContainerPropsJs(js, def);
end;


procedure defineSpecimenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Specimen4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'accessionIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Specimen4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen4', 'subject', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen4', 'receivedTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Specimen4', 'parent', 'Reference(Specimen)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'request', 'Reference(ServiceRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'collection', 'SpecimenCollection4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen4', 'processing', 'SpecimenProcessing4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'container', 'SpecimenContainer4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'condition', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Specimen4', nil, js.FHIRFactoryJs);
  defineSpecimenPropsJs(js, def);
end;


procedure defineSpecimenDefinitionTypeTestedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionTypeTested4', 'isDerived', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested4', 'preference', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested4', 'container', 'SpecimenDefinitionTypeTestedContainer4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested4', 'requirement', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested4', 'retentionTime', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested4', 'rejectionCriterion', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested4', 'handling', 'SpecimenDefinitionTypeTestedHandling4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenDefinitionTypeTestedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionTypeTested4', nil, js.FHIRFactoryJs);
  defineSpecimenDefinitionTypeTestedPropsJs(js, def);
end;


procedure defineSpecimenDefinitionTypeTestedContainerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer4', 'material', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer4', 'cap', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer4', 'capacity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer4', 'minimumVolumeQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer4', 'minimumVolumeString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer4', 'additive', 'SpecimenDefinitionTypeTestedContainerAdditive4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer4', 'preparation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSpecimenDefinitionTypeTestedContainerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionTypeTestedContainer4', nil, js.FHIRFactoryJs);
  defineSpecimenDefinitionTypeTestedContainerPropsJs(js, def);
end;


procedure defineSpecimenDefinitionTypeTestedContainerAdditivePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainerAdditive4', 'additiveCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainerAdditive4', 'additiveReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenDefinitionTypeTestedContainerAdditiveJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionTypeTestedContainerAdditive4', nil, js.FHIRFactoryJs);
  defineSpecimenDefinitionTypeTestedContainerAdditivePropsJs(js, def);
end;


procedure defineSpecimenDefinitionTypeTestedHandlingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedHandling4', 'temperatureQualifier', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedHandling4', 'temperatureRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedHandling4', 'maxDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedHandling4', 'instruction', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSpecimenDefinitionTypeTestedHandlingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionTypeTestedHandling4', nil, js.FHIRFactoryJs);
  defineSpecimenDefinitionTypeTestedHandlingPropsJs(js, def);
end;


procedure defineSpecimenDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinition4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinition4', 'typeCollected', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinition4', 'patientPreparation', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinition4', 'timeAspect', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinition4', 'collection', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinition4', 'typeTested', 'SpecimenDefinitionTypeTested4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinition4', nil, js.FHIRFactoryJs);
  defineSpecimenDefinitionPropsJs(js, def);
end;


procedure defineStructureDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionMapping4', 'identity', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping4', 'uri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionMapping4', nil, js.FHIRFactoryJs);
  defineStructureDefinitionMappingPropsJs(js, def);
end;


procedure defineStructureDefinitionContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionContext4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionContext4', 'expression', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureDefinitionContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionContext4', nil, js.FHIRFactoryJs);
  defineStructureDefinitionContextPropsJs(js, def);
end;


procedure defineStructureDefinitionSnapshotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionSnapshot4', 'element', 'ElementDefinition4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionSnapshotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionSnapshot4', nil, js.FHIRFactoryJs);
  defineStructureDefinitionSnapshotPropsJs(js, def);
end;


procedure defineStructureDefinitionDifferentialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionDifferential4', 'element', 'ElementDefinition4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionDifferentialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionDifferential4', nil, js.FHIRFactoryJs);
  defineStructureDefinitionDifferentialPropsJs(js, def);
end;


procedure defineStructureDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'StructureDefinition4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureDefinition4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'keyword', 'Coding4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'fhirVersion', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'mapping', 'StructureDefinitionMapping4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'kind', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'abstract', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition4', 'context', 'StructureDefinitionContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition4', 'type_', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'baseDefinition', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'derivation', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition4', 'snapshot', 'StructureDefinitionSnapshot4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureDefinition4', 'differential', 'StructureDefinitionDifferential4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineStructureDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinition4', nil, js.FHIRFactoryJs);
  defineStructureDefinitionPropsJs(js, def);
end;


procedure defineStructureMapStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapStructure4', 'url', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure4', 'alias', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure4', 'documentation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapStructure4', nil, js.FHIRFactoryJs);
  defineStructureMapStructurePropsJs(js, def);
end;


procedure defineStructureMapGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroup4', 'name', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup4', 'extends', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup4', 'typeMode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup4', 'documentation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup4', 'input', 'StructureMapGroupInput4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroup4', 'rule', 'StructureMapGroupRule4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroup4', nil, js.FHIRFactoryJs);
  defineStructureMapGroupPropsJs(js, def);
end;


procedure defineStructureMapGroupInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupInput4', 'name', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput4', 'type_', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput4', 'mode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput4', 'documentation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupInput4', nil, js.FHIRFactoryJs);
  defineStructureMapGroupInputPropsJs(js, def);
end;


procedure defineStructureMapGroupRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRule4', 'name', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRule4', 'source', 'StructureMapGroupRuleSource4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule4', 'target', 'StructureMapGroupRuleTarget4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule4', 'rule', '@StructureMap.group.rule4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule4', 'dependent', 'StructureMapGroupRuleDependent4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule4', 'documentation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRule4', nil, js.FHIRFactoryJs);
  defineStructureMapGroupRulePropsJs(js, def);
end;


procedure defineStructureMapGroupRuleSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'context', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'min', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'max', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'type_', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueMarkdown', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueOid', 'oid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValuePositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueUrl', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueUuid', 'uuid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueAnnotation', 'Annotation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueContactPoint', 'ContactPoint4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueCount', 'Count4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDistance', 'Distance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueHumanName', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValuePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueContactDetail', 'ContactDetail4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueContributor', 'Contributor4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueParameterDefinition', 'ParameterDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueRelatedArtifact', 'RelatedArtifact4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueTriggerDefinition', 'TriggerDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueUsageContext', 'UsageContext4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'defaultValueDosage', 'Dosage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'element', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'listMode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'variable', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'condition', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'check', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource4', 'logMessage', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleSource4', nil, js.FHIRFactoryJs);
  defineStructureMapGroupRuleSourcePropsJs(js, def);
end;


procedure defineStructureMapGroupRuleTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'context', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'contextType', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'element', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'variable', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'listRuleId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'transform', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget4', 'parameter', 'StructureMapGroupRuleTargetParameter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapGroupRuleTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleTarget4', nil, js.FHIRFactoryJs);
  defineStructureMapGroupRuleTargetPropsJs(js, def);
end;


procedure defineStructureMapGroupRuleTargetParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter4', 'valueId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineStructureMapGroupRuleTargetParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleTargetParameter4', nil, js.FHIRFactoryJs);
  defineStructureMapGroupRuleTargetParameterPropsJs(js, def);
end;


procedure defineStructureMapGroupRuleDependentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleDependent4', 'name', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleDependentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleDependent4', nil, js.FHIRFactoryJs);
  defineStructureMapGroupRuleDependentPropsJs(js, def);
end;


procedure defineStructureMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'StructureMap4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMap4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMap4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap4', 'structure', 'StructureMapStructure4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap4', 'group', 'StructureMapGroup4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMap4', nil, js.FHIRFactoryJs);
  defineStructureMapPropsJs(js, def);
end;


procedure defineSubscriptionChannelPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubscriptionChannel4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel4', 'endpoint', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel4', 'payload', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubscriptionChannelJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubscriptionChannel4', nil, js.FHIRFactoryJs);
  defineSubscriptionChannelPropsJs(js, def);
end;


procedure defineSubscriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Subscription4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription4', 'contact', 'ContactPoint4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Subscription4', 'end_', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Subscription4', 'reason', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription4', 'criteria', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription4', 'error', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription4', 'channel', 'SubscriptionChannel4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubscriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Subscription4', nil, js.FHIRFactoryJs);
  defineSubscriptionPropsJs(js, def);
end;


procedure defineSubstanceInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceInstance4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceInstance4', 'expiry', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceInstance4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceInstance4', nil, js.FHIRFactoryJs);
  defineSubstanceInstancePropsJs(js, def);
end;


procedure defineSubstanceIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceIngredient4', 'quantity', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient4', 'substanceCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient4', 'substanceReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceIngredient4', nil, js.FHIRFactoryJs);
  defineSubstanceIngredientPropsJs(js, def);
end;


procedure defineSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Substance4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Substance4', 'category', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Substance4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Substance4', 'instance', 'SubstanceInstance4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance4', 'ingredient', 'SubstanceIngredient4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Substance4', nil, js.FHIRFactoryJs);
  defineSubstancePropsJs(js, def);
end;


procedure defineSubstanceNucleicAcidSubunitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit4', 'subunit', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit4', 'sequence', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit4', 'length', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit4', 'sequenceAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit4', 'fivePrime', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit4', 'threePrime', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit4', 'linkage', 'SubstanceNucleicAcidSubunitLinkage4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit4', 'sugar', 'SubstanceNucleicAcidSubunitSugar4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceNucleicAcidSubunitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceNucleicAcidSubunit4', nil, js.FHIRFactoryJs);
  defineSubstanceNucleicAcidSubunitPropsJs(js, def);
end;


procedure defineSubstanceNucleicAcidSubunitLinkagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitLinkage4', 'connectivity', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitLinkage4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitLinkage4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitLinkage4', 'residueSite', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceNucleicAcidSubunitLinkageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceNucleicAcidSubunitLinkage4', nil, js.FHIRFactoryJs);
  defineSubstanceNucleicAcidSubunitLinkagePropsJs(js, def);
end;


procedure defineSubstanceNucleicAcidSubunitSugarPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitSugar4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitSugar4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitSugar4', 'residueSite', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceNucleicAcidSubunitSugarJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceNucleicAcidSubunitSugar4', nil, js.FHIRFactoryJs);
  defineSubstanceNucleicAcidSubunitSugarPropsJs(js, def);
end;


procedure defineSubstanceNucleicAcidPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceNucleicAcid4', 'sequenceType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcid4', 'numberOfSubunits', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceNucleicAcid4', 'areaOfHybridisation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceNucleicAcid4', 'oligoNucleotideType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcid4', 'subunit', 'SubstanceNucleicAcidSubunit4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceNucleicAcidJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceNucleicAcid4', nil, js.FHIRFactoryJs);
  defineSubstanceNucleicAcidPropsJs(js, def);
end;


procedure defineSubstancePolymerMonomerSetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerMonomerSet4', 'ratioType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSet4', 'startingMaterial', 'SubstancePolymerMonomerSetStartingMaterial4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerMonomerSetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerMonomerSet4', nil, js.FHIRFactoryJs);
  defineSubstancePolymerMonomerSetPropsJs(js, def);
end;


procedure defineSubstancePolymerMonomerSetStartingMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial4', 'material', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial4', 'isDefining', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial4', 'amount', 'SubstanceAmount4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstancePolymerMonomerSetStartingMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerMonomerSetStartingMaterial4', nil, js.FHIRFactoryJs);
  defineSubstancePolymerMonomerSetStartingMaterialPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeat4', 'numberOfUnits', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstancePolymerRepeat4', 'averageMolecularFormula', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeat4', 'repeatUnitAmountType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeat4', 'repeatUnit', 'SubstancePolymerRepeatRepeatUnit4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerRepeatJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeat4', nil, js.FHIRFactoryJs);
  defineSubstancePolymerRepeatPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit4', 'orientationOfPolymerisation', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit4', 'repeatUnit', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit4', 'amount', 'SubstanceAmount4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit4', 'degreeOfPolymerisation', 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit4', 'structuralRepresentation', 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnit4', nil, js.FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation4', 'degree', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation4', 'amount', 'SubstanceAmount4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation4', nil, js.FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation4', 'representation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation4', 'attachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnitStructuralRepresentation4', nil, js.FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationPropsJs(js, def);
end;


procedure defineSubstancePolymerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstancePolymer4', 'class', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymer4', 'geometry', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymer4', 'copolymerConnectivity', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymer4', 'monomerSet', 'SubstancePolymerMonomerSet4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymer4', 'repeat', 'SubstancePolymerRepeat4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymer4', nil, js.FHIRFactoryJs);
  defineSubstancePolymerPropsJs(js, def);
end;


procedure defineSubstanceProteinSubunitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceProteinSubunit4', 'subunit', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceProteinSubunit4', 'sequence', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceProteinSubunit4', 'length', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceProteinSubunit4', 'sequenceAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceProteinSubunit4', 'nTerminalModificationId', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceProteinSubunit4', 'nTerminalModification', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceProteinSubunit4', 'cTerminalModificationId', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceProteinSubunit4', 'cTerminalModification', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceProteinSubunitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceProteinSubunit4', nil, js.FHIRFactoryJs);
  defineSubstanceProteinSubunitPropsJs(js, def);
end;


procedure defineSubstanceProteinPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceProtein4', 'sequenceType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceProtein4', 'numberOfSubunits', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceProtein4', 'subunit', 'SubstanceProteinSubunit4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceProteinJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceProtein4', nil, js.FHIRFactoryJs);
  defineSubstanceProteinPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationGenePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationGene4', 'geneSequenceOrigin', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGene4', 'gene', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGene4', 'source', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationGeneJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationGene4', nil, js.FHIRFactoryJs);
  defineSubstanceReferenceInformationGenePropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationGeneElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement4', 'element', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement4', 'source', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationGeneElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationGeneElement4', nil, js.FHIRFactoryJs);
  defineSubstanceReferenceInformationGeneElementPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationClassificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationClassification4', 'domain', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification4', 'classification', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification4', 'subtype', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification4', 'source', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationClassificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationClassification4', nil, js.FHIRFactoryJs);
  defineSubstanceReferenceInformationClassificationPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'target', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'interaction', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'organism', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'organismType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'amountQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'amountRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'amountString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'amountType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget4', 'source', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationTarget4', nil, js.FHIRFactoryJs);
  defineSubstanceReferenceInformationTargetPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'gene', 'SubstanceReferenceInformationGene4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'geneElement', 'SubstanceReferenceInformationGeneElement4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'classification', 'SubstanceReferenceInformationClassification4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation4', 'target', 'SubstanceReferenceInformationTarget4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformation4', nil, js.FHIRFactoryJs);
  defineSubstanceReferenceInformationPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialFractionDescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialFractionDescription4', 'fraction', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialFractionDescription4', 'materialType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSourceMaterialFractionDescriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialFractionDescription4', nil, js.FHIRFactoryJs);
  defineSubstanceSourceMaterialFractionDescriptionPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialOrganismPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism4', 'family', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism4', 'genus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism4', 'species', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism4', 'intraspecificType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism4', 'intraspecificDescription', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism4', 'author', 'SubstanceSourceMaterialOrganismAuthor4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism4', 'hybrid', 'SubstanceSourceMaterialOrganismHybrid4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism4', 'organismGeneral', 'SubstanceSourceMaterialOrganismOrganismGeneral4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSourceMaterialOrganismJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialOrganism4', nil, js.FHIRFactoryJs);
  defineSubstanceSourceMaterialOrganismPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialOrganismAuthorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismAuthor4', 'authorType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismAuthor4', 'authorDescription', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSourceMaterialOrganismAuthorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialOrganismAuthor4', nil, js.FHIRFactoryJs);
  defineSubstanceSourceMaterialOrganismAuthorPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialOrganismHybridPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismHybrid4', 'maternalOrganismId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismHybrid4', 'maternalOrganismName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismHybrid4', 'paternalOrganismId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismHybrid4', 'paternalOrganismName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismHybrid4', 'hybridType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSourceMaterialOrganismHybridJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialOrganismHybrid4', nil, js.FHIRFactoryJs);
  defineSubstanceSourceMaterialOrganismHybridPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialOrganismOrganismGeneralPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismOrganismGeneral4', 'kingdom', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismOrganismGeneral4', 'phylum', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismOrganismGeneral4', 'class', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismOrganismGeneral4', 'order', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSourceMaterialOrganismOrganismGeneralJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialOrganismOrganismGeneral4', nil, js.FHIRFactoryJs);
  defineSubstanceSourceMaterialOrganismOrganismGeneralPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialPartDescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialPartDescription4', 'part', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialPartDescription4', 'partLocation', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSourceMaterialPartDescriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialPartDescription4', nil, js.FHIRFactoryJs);
  defineSubstanceSourceMaterialPartDescriptionPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'sourceMaterialClass', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'sourceMaterialType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'sourceMaterialState', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'organismId', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'organismName', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'parentSubstanceId', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'countryOfOrigin', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'developmentStage', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'fractionDescription', 'SubstanceSourceMaterialFractionDescription4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'organism', 'SubstanceSourceMaterialOrganism4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial4', 'partDescription', 'SubstanceSourceMaterialPartDescription4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSourceMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterial4', nil, js.FHIRFactoryJs);
  defineSubstanceSourceMaterialPropsJs(js, def);
end;


procedure defineSubstanceSpecificationMoietyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'role', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'stereochemistry', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'opticalActivity', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'molecularFormula', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'amountQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety4', 'amountString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSpecificationMoietyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationMoiety4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationMoietyPropsJs(js, def);
end;


procedure defineSubstanceSpecificationPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'parameters', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'definingSubstanceReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'definingSubstanceCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'amountQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty4', 'amountString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSpecificationPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationProperty4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationPropertyPropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'stereochemistry', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'opticalActivity', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'molecularFormula', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'molecularFormulaByMoiety', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'isotope', 'SubstanceSpecificationStructureIsotope4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'molecularWeight', '@SubstanceSpecification.structure.isotope.molecularWeight4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'source', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationStructure4', 'representation', 'SubstanceSpecificationStructureRepresentation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructure4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationStructurePropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureIsotopePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'name', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'substitution', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'halfLife', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope4', 'molecularWeight', 'SubstanceSpecificationStructureIsotopeMolecularWeight4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureIsotopeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureIsotope4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureIsotopePropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureIsotopeMolecularWeightPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight4', 'method', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight4', 'amount', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureIsotopeMolecularWeightJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureIsotopeMolecularWeight4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureIsotopeMolecularWeightPropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureRepresentationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureRepresentation4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureRepresentation4', 'representation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructureRepresentation4', 'attachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureRepresentationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureRepresentation4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureRepresentationPropsJs(js, def);
end;


procedure defineSubstanceSpecificationCodePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationCode4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationCode4', 'status', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationCode4', 'statusDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceSpecificationCode4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationCode4', 'source', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationCodeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationCode4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationCodePropsJs(js, def);
end;


procedure defineSubstanceSpecificationNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationName4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationName4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationName4', 'status', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationName4', 'preferred', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstanceSpecificationName4', 'language', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName4', 'domain', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName4', 'synonym', '@SubstanceSpecification.name4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName4', 'translation', '@SubstanceSpecification.name4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName4', 'official', 'SubstanceSpecificationNameOfficial4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName4', 'source', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationName4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationNamePropsJs(js, def);
end;


procedure defineSubstanceSpecificationNameOfficialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationNameOfficial4', 'authority', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationNameOfficial4', 'status', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationNameOfficial4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineSubstanceSpecificationNameOfficialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationNameOfficial4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationNameOfficialPropsJs(js, def);
end;


procedure defineSubstanceSpecificationRelationshipPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'substanceReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'substanceCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'relationship', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'isDefining', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'amountQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'amountRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'amountRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'amountString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'amountRatioLowLimit', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'amountType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship4', 'source', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationRelationshipJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationRelationship4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationRelationshipPropsJs(js, def);
end;


procedure defineSubstanceSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecification4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'status', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'domain', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecification4', 'source', 'Reference(DocumentReference)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'comment', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecification4', 'moiety', 'SubstanceSpecificationMoiety4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'property', 'SubstanceSpecificationProperty4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'referenceInformation', 'Reference(SubstanceReferenceInformation)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'structure', 'SubstanceSpecificationStructure4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'code', 'SubstanceSpecificationCode4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'name', 'SubstanceSpecificationName4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'molecularWeight', '@SubstanceSpecification.structure.isotope.molecularWeight4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'relationship', 'SubstanceSpecificationRelationship4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification4', 'nucleicAcid', 'Reference(SubstanceNucleicAcid)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'polymer', 'Reference(SubstancePolymer)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'protein', 'Reference(SubstanceProtein)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification4', 'sourceMaterial', 'Reference(SubstanceSourceMaterial)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecification4', nil, js.FHIRFactoryJs);
  defineSubstanceSpecificationPropsJs(js, def);
end;


procedure defineSupplyDeliverySuppliedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyDeliverySuppliedItem4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDeliverySuppliedItem4', 'itemCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDeliverySuppliedItem4', 'itemReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyDeliverySuppliedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDeliverySuppliedItem4', nil, js.FHIRFactoryJs);
  defineSupplyDeliverySuppliedItemPropsJs(js, def);
end;


procedure defineSupplyDeliveryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyDelivery4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery4', 'basedOn', 'Reference(SupplyRequest)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery4', 'partOf', 'Reference(SupplyDelivery)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyDelivery4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'suppliedItem', 'SupplyDeliverySuppliedItem4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'occurrenceDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyDelivery4', 'occurrencePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'occurrenceTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'supplier', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'destination', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery4', 'receiver', 'Reference(Practitioner)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSupplyDeliveryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDelivery4', nil, js.FHIRFactoryJs);
  defineSupplyDeliveryPropsJs(js, def);
end;


procedure defineSupplyRequestParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyRequestParameter4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineSupplyRequestParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequestParameter4', nil, js.FHIRFactoryJs);
  defineSupplyRequestParameterPropsJs(js, def);
end;


procedure defineSupplyRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyRequest4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest4', 'category', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest4', 'itemCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'itemReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'quantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'parameter', 'SupplyRequestParameter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest4', 'occurrenceDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest4', 'occurrencePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'occurrenceTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'authoredOn', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest4', 'requester', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'supplier', 'Reference(Organization)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest4', 'reasonCode', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest4', 'reasonReference', 'Reference(Condition)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest4', 'deliverFrom', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest4', 'deliverTo', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequest4', nil, js.FHIRFactoryJs);
  defineSupplyRequestPropsJs(js, def);
end;


procedure defineTaskRestrictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskRestriction4', 'repetitions', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskRestriction4', 'period', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskRestriction4', 'recipient', 'Reference(Patient)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTaskRestrictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskRestriction4', nil, js.FHIRFactoryJs);
  defineTaskRestrictionPropsJs(js, def);
end;


procedure defineTaskInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskInput4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'TaskInput4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TaskInput4', 'valueCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TaskInput4', 'valueId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskInput4', 'valueMarkdown', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueOid', 'oid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valuePositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskInput4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueUrl', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueUuid', 'uuid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput4', 'valueAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueAnnotation', 'Annotation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueContactPoint', 'ContactPoint4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueCount', 'Count4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueDistance', 'Distance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueHumanName', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valuePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueContactDetail', 'ContactDetail4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueContributor', 'Contributor4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueParameterDefinition', 'ParameterDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueRelatedArtifact', 'RelatedArtifact4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueTriggerDefinition', 'TriggerDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueUsageContext', 'UsageContext4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput4', 'valueDosage', 'Dosage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTaskInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskInput4', nil, js.FHIRFactoryJs);
  defineTaskInputPropsJs(js, def);
end;


procedure defineTaskOutputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskOutput4', 'type', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueBase64Binary', 'base64Binary4', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'TaskOutput4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TaskOutput4', 'valueCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TaskOutput4', 'valueId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueInstant', 'instant4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskOutput4', 'valueMarkdown', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueOid', 'oid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valuePositiveInt', 'positiveInt4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskOutput4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueTime', 'time4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueUnsignedInt', 'unsignedInt4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueUrl', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueUuid', 'uuid4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput4', 'valueAddress', 'Address4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueAge', 'Age4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueAnnotation', 'Annotation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueAttachment', 'Attachment4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueCodeableConcept', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueCoding', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueContactPoint', 'ContactPoint4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueCount', 'Count4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueDistance', 'Distance4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueDuration', 'Duration4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueHumanName', 'HumanName4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueMoney', 'Money4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valuePeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueQuantity', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueRange', 'Range4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueRatio', 'Ratio4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueReference', 'Reference4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueSampledData', 'SampledData4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueTiming', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueContactDetail', 'ContactDetail4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueContributor', 'Contributor4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueDataRequirement', 'DataRequirement4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueExpression', 'Expression4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueParameterDefinition', 'ParameterDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueRelatedArtifact', 'RelatedArtifact4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueTriggerDefinition', 'TriggerDefinition4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueUsageContext', 'UsageContext4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput4', 'valueDosage', 'Dosage4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTaskOutputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskOutput4', nil, js.FHIRFactoryJs);
  defineTaskOutputPropsJs(js, def);
end;


procedure defineTaskPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Task4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'instantiatesCanonical', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'instantiatesUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'basedOn', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'groupIdentifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'partOf', 'Reference(Task)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'statusReason', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'businessStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'intent', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'priority', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'code', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task4', 'focus', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'for', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'executionPeriod', 'Period4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'authoredOn', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Task4', 'lastModified', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Task4', 'requester', 'Reference(Device)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'performerType', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'owner', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'location', 'Reference(Location)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'reasonCode', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'reasonReference', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'insurance', 'Reference(Coverage)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'relevantHistory', 'Reference(Provenance)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'restriction', 'TaskRestriction4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task4', 'input', 'TaskInput4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task4', 'output', 'TaskOutput4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTaskJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Task4', nil, js.FHIRFactoryJs);
  defineTaskPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesSoftwarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesSoftware4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesSoftware4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesSoftwareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesSoftware4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesSoftwarePropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesImplementationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesImplementation4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesImplementation4', 'url', 'url4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesImplementationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesImplementation4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesImplementationPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem4', 'uri', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem4', 'version', 'TerminologyCapabilitiesCodeSystemVersion4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem4', 'subsumption', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystem4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion4', 'code', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion4', 'isDefault', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion4', 'compositional', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion4', 'filter', 'TerminologyCapabilitiesCodeSystemVersionFilter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystemVersion4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemVersionPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemVersionFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersionFilter4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemVersionFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystemVersionFilter4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemVersionFilterPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesExpansionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion4', 'hierarchical', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion4', 'paging', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion4', 'incomplete', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion4', 'parameter', 'TerminologyCapabilitiesExpansionParameter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion4', 'textFilter', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesExpansion4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesExpansionPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesExpansionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesExpansionParameter4', 'name', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansionParameter4', 'documentation', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesExpansionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesExpansionParameter4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesExpansionParameterPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesValidateCodePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesValidateCode4', 'translations', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesValidateCodeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesValidateCode4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesValidateCodePropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesTranslationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesTranslation4', 'needsMap', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesTranslationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesTranslation4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesTranslationPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesClosurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesClosure4', 'translation', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesClosureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesClosure4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesClosurePropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilities4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'kind', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'software', 'TerminologyCapabilitiesSoftware4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'implementation', 'TerminologyCapabilitiesImplementation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'lockedDate', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'codeSystem', 'TerminologyCapabilitiesCodeSystem4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'expansion', 'TerminologyCapabilitiesExpansion4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'codeSearch', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'validateCode', 'TerminologyCapabilitiesValidateCode4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'translation', 'TerminologyCapabilitiesTranslation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities4', 'closure', 'TerminologyCapabilitiesClosure4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTerminologyCapabilitiesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilities4', nil, js.FHIRFactoryJs);
  defineTerminologyCapabilitiesPropsJs(js, def);
end;


procedure defineTestReportParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportParticipant4', 'type_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportParticipant4', 'uri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportParticipant4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportParticipant4', nil, js.FHIRFactoryJs);
  defineTestReportParticipantPropsJs(js, def);
end;


procedure defineTestReportSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetup4', 'action', 'TestReportSetupAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetup4', nil, js.FHIRFactoryJs);
  defineTestReportSetupPropsJs(js, def);
end;


procedure defineTestReportSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupAction4', 'operation', 'TestReportSetupActionOperation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReportSetupAction4', 'assert', 'TestReportSetupActionAssert4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupAction4', nil, js.FHIRFactoryJs);
  defineTestReportSetupActionPropsJs(js, def);
end;


procedure defineTestReportSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupActionOperation4', 'result', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionOperation4', 'message', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionOperation4', 'detail', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupActionOperation4', nil, js.FHIRFactoryJs);
  defineTestReportSetupActionOperationPropsJs(js, def);
end;


procedure defineTestReportSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupActionAssert4', 'result', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionAssert4', 'message', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionAssert4', 'detail', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupActionAssert4', nil, js.FHIRFactoryJs);
  defineTestReportSetupActionAssertPropsJs(js, def);
end;


procedure defineTestReportTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTest4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportTest4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportTest4', 'action', 'TestReportTestAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTest4', nil, js.FHIRFactoryJs);
  defineTestReportTestPropsJs(js, def);
end;


procedure defineTestReportTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTestAction4', 'operation', '@TestReport.setup.action.operation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReportTestAction4', 'assert', '@TestReport.setup.action.assert4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTestAction4', nil, js.FHIRFactoryJs);
  defineTestReportTestActionPropsJs(js, def);
end;


procedure defineTestReportTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTeardown4', 'action', 'TestReportTeardownAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTeardown4', nil, js.FHIRFactoryJs);
  defineTestReportTeardownPropsJs(js, def);
end;


procedure defineTestReportTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTeardownAction4', 'operation', '@TestReport.setup.action.operation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTeardownAction4', nil, js.FHIRFactoryJs);
  defineTestReportTeardownActionPropsJs(js, def);
end;


procedure defineTestReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'TestReport4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport4', 'testScript', 'Reference(TestScript)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport4', 'result', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport4', 'score', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TestReport4', 'tester', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport4', 'issued', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TestReport4', 'participant', 'TestReportParticipant4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestReport4', 'setup', 'TestReportSetup4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport4', 'test', 'TestReportTest4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestReport4', 'teardown', 'TestReportTeardown4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReport4', nil, js.FHIRFactoryJs);
  defineTestReportPropsJs(js, def);
end;


procedure defineTestScriptOriginPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptOrigin4', 'index', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptOrigin4', 'profile', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptOriginJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptOrigin4', nil, js.FHIRFactoryJs);
  defineTestScriptOriginPropsJs(js, def);
end;


procedure defineTestScriptDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptDestination4', 'index', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptDestination4', 'profile', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptDestination4', nil, js.FHIRFactoryJs);
  defineTestScriptDestinationPropsJs(js, def);
end;


procedure defineTestScriptMetadataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadata4', 'link', 'TestScriptMetadataLink4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScriptMetadata4', 'capability', 'TestScriptMetadataCapability4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptMetadataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadata4', nil, js.FHIRFactoryJs);
  defineTestScriptMetadataPropsJs(js, def);
end;


procedure defineTestScriptMetadataLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataLink4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataLink4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptMetadataLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataLink4', nil, js.FHIRFactoryJs);
  defineTestScriptMetadataLinkPropsJs(js, def);
end;


procedure defineTestScriptMetadataCapabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataCapability4', 'required', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability4', 'validated', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataCapability4', 'destination', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptMetadataCapability4', 'capabilities', 'canonical4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptMetadataCapabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataCapability4', nil, js.FHIRFactoryJs);
  defineTestScriptMetadataCapabilityPropsJs(js, def);
end;


procedure defineTestScriptFixturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptFixture4', 'autocreate', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture4', 'autodelete', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture4', 'resource', 'Reference(Any)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptFixtureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptFixture4', nil, js.FHIRFactoryJs);
  defineTestScriptFixturePropsJs(js, def);
end;


procedure defineTestScriptVariablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptVariable4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'defaultValue', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'expression', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'headerField', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'hint', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable4', 'sourceId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptVariableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptVariable4', nil, js.FHIRFactoryJs);
  defineTestScriptVariablePropsJs(js, def);
end;


procedure defineTestScriptSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetup4', 'action', 'TestScriptSetupAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetup4', nil, js.FHIRFactoryJs);
  defineTestScriptSetupPropsJs(js, def);
end;


procedure defineTestScriptSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupAction4', 'operation', 'TestScriptSetupActionOperation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupAction4', 'assert', 'TestScriptSetupActionAssert4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupAction4', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionPropsJs(js, def);
end;


procedure defineTestScriptSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'type', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'resource', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'label_', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'accept', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'contentType', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'destination', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'encodeRequestUrl', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'method', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'origin', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'params', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'requestHeader', 'TestScriptSetupActionOperationRequestHeader4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'requestId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'responseId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'sourceId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'targetId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation4', 'url', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperation4', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionOperationPropsJs(js, def);
end;


procedure defineTestScriptSetupActionOperationRequestHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader4', 'field', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationRequestHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperationRequestHeader4', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionOperationRequestHeaderPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'label_', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'direction', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'compareToSourceId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'compareToSourceExpression', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'compareToSourcePath', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'contentType', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'expression', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'headerField', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'minimumId', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'navigationLinks', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'operator', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'path', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'requestMethod', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'requestURL', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'resource', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'response', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'responseCode', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'sourceId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'validateProfileId', 'id4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert4', 'warningOnly', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTestScriptSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssert4', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertPropsJs(js, def);
end;


procedure defineTestScriptTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTest4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest4', 'description', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest4', 'action', 'TestScriptTestAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTest4', nil, js.FHIRFactoryJs);
  defineTestScriptTestPropsJs(js, def);
end;


procedure defineTestScriptTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTestAction4', 'operation', '@TestScript.setup.action.operation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptTestAction4', 'assert', '@TestScript.setup.action.assert4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTestAction4', nil, js.FHIRFactoryJs);
  defineTestScriptTestActionPropsJs(js, def);
end;


procedure defineTestScriptTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardown4', 'action', 'TestScriptTeardownAction4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardown4', nil, js.FHIRFactoryJs);
  defineTestScriptTeardownPropsJs(js, def);
end;


procedure defineTestScriptTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardownAction4', 'operation', '@TestScript.setup.action.operation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardownAction4', nil, js.FHIRFactoryJs);
  defineTestScriptTeardownActionPropsJs(js, def);
end;


procedure defineTestScriptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'TestScript4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'identifier', 'Identifier4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScript4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TestScript4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript4', 'origin', 'TestScriptOrigin4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'destination', 'TestScriptDestination4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'metadata', 'TestScriptMetadata4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript4', 'fixture', 'TestScriptFixture4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'profile', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'variable', 'TestScriptVariable4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'setup', 'TestScriptSetup4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript4', 'test', 'TestScriptTest4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript4', 'teardown', 'TestScriptTeardown4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScript4', nil, js.FHIRFactoryJs);
  defineTestScriptPropsJs(js, def);
end;


procedure defineValueSetComposePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCompose4', 'lockedDate', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetCompose4', 'inactive', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetCompose4', 'include', 'ValueSetComposeInclude4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetCompose4', 'exclude', '@ValueSet.compose.include4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCompose4', nil, js.FHIRFactoryJs);
  defineValueSetComposePropsJs(js, def);
end;


procedure defineValueSetComposeIncludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeInclude4', 'system', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude4', 'concept', 'ValueSetComposeIncludeConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetComposeInclude4', 'filter', 'ValueSetComposeIncludeFilter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeInclude4', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludePropsJs(js, def);
end;


procedure defineValueSetComposeIncludeConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConcept4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept4', 'designation', 'ValueSetComposeIncludeConceptDesignation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConcept4', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludeConceptPropsJs(js, def);
end;


procedure defineValueSetComposeIncludeConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation4', 'language', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation4', 'use', 'Coding4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConceptDesignation4', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludeConceptDesignationPropsJs(js, def);
end;


procedure defineValueSetComposeIncludeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeFilter4', 'property_', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter4', 'op', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter4', 'value', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeFilter4', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludeFilterPropsJs(js, def);
end;


procedure defineValueSetExpansionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansion4', 'identifier', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansion4', 'timestamp', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetExpansion4', 'total', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion4', 'offset', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion4', 'parameter', 'ValueSetExpansionParameter4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansion4', 'contains', 'ValueSetExpansionContains4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansion4', nil, js.FHIRFactoryJs);
  defineValueSetExpansionPropsJs(js, def);
end;


procedure defineValueSetExpansionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueString', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueBoolean', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueInteger', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueDecimal', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueUri', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueCode', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter4', 'valueDateTime', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineValueSetExpansionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionParameter4', nil, js.FHIRFactoryJs);
  defineValueSetExpansionParameterPropsJs(js, def);
end;


procedure defineValueSetExpansionContainsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionContains4', 'system', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'abstract', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'inactive', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'code', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'display', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'designation', '@ValueSet.compose.include.concept.designation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansionContains4', 'contains', '@ValueSet.expansion.contains4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetExpansionContainsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionContains4', nil, js.FHIRFactoryJs);
  defineValueSetExpansionContainsPropsJs(js, def);
end;


procedure defineValueSetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ValueSet4', 'url', 'uri4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet4', 'version', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'name', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'title', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'experimental', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet4', 'date', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSet4', 'publisher', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'contact', 'ContactDetail4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet4', 'description', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'useContext', 'UsageContext4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet4', 'jurisdiction', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet4', 'immutable', 'boolean4', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet4', 'purpose', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'copyright', 'markdown4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet4', 'compose', 'ValueSetCompose4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSet4', 'expansion', 'ValueSetExpansion4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineValueSetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSet4', nil, js.FHIRFactoryJs);
  defineValueSetPropsJs(js, def);
end;


procedure defineVerificationResultPrimarySourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'who', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'type', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'communicationMethod', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'validationStatus', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'validationDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'canPushUpdates', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource4', 'pushTypeAvailable', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVerificationResultPrimarySourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultPrimarySource4', nil, js.FHIRFactoryJs);
  defineVerificationResultPrimarySourcePropsJs(js, def);
end;


procedure defineVerificationResultAttestationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultAttestation4', 'who', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'onBehalfOf', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'communicationMethod', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'date', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'sourceIdentityCertificate', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'proxyIdentityCertificate', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'proxySignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation4', 'sourceSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineVerificationResultAttestationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultAttestation4', nil, js.FHIRFactoryJs);
  defineVerificationResultAttestationPropsJs(js, def);
end;


procedure defineVerificationResultValidatorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultValidator4', 'organization', 'Reference(Organization)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultValidator4', 'identityCertificate', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultValidator4', 'attestationSignature', 'Signature4', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineVerificationResultValidatorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultValidator4', nil, js.FHIRFactoryJs);
  defineVerificationResultValidatorPropsJs(js, def);
end;


procedure defineVerificationResultPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VerificationResult4', 'target', 'Reference(Any)4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult4', 'need', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResult4', 'statusDate', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult4', 'validationType', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult4', 'validationProcess', 'CodeableConcept4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult4', 'frequency', 'Timing4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult4', 'lastPerformed', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult4', 'nextScheduled', 'date4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult4', 'failureAction', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult4', 'primarySource', 'VerificationResultPrimarySource4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult4', 'attestation', 'VerificationResultAttestation4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult4', 'validator', 'VerificationResultValidator4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVerificationResultJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResult4', nil, js.FHIRFactoryJs);
  defineVerificationResultPropsJs(js, def);
end;


procedure defineVisionPrescriptionLensSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'product', 'CodeableConcept4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'eye', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'sphere', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'cylinder', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'axis', 'integer4', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'prism', 'VisionPrescriptionLensSpecificationPrism4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'add', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'power', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'backCurve', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'diameter', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'duration', 'Quantity4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'color', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'brand', 'string4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification4', 'note', 'Annotation4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVisionPrescriptionLensSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescriptionLensSpecification4', nil, js.FHIRFactoryJs);
  defineVisionPrescriptionLensSpecificationPropsJs(js, def);
end;


procedure defineVisionPrescriptionLensSpecificationPrismPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionLensSpecificationPrism4', 'amount', 'decimal4', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecificationPrism4', 'base', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineVisionPrescriptionLensSpecificationPrismJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescriptionLensSpecificationPrism4', nil, js.FHIRFactoryJs);
  defineVisionPrescriptionLensSpecificationPrismPropsJs(js, def);
end;


procedure defineVisionPrescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VisionPrescription4', 'identifier', 'Identifier4', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VisionPrescription4', 'status', 'code4', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescription4', 'created', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VisionPrescription4', 'patient', 'Reference(Patient)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription4', 'encounter', 'Reference(Encounter)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription4', 'dateWritten', 'dateTime4', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VisionPrescription4', 'prescriber', 'Reference(Practitioner)4', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription4', 'lensSpecification', 'VisionPrescriptionLensSpecification4', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVisionPrescriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescription4', nil, js.FHIRFactoryJs);
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

