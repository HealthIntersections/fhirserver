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

// FHIR v3.2.0 generated 2018-01-13T10:54:00+11:00

uses
  Javascript, FHIRJavascript;

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
  js.registerElement(def, 'ParametersParameter', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Extension', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Extension', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
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
  js.registerElement(def, 'Extension', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Attachment', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'DataRequirementCodeFilter', 'valueSetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter', 'valueSetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineDataRequirementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirement', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirement', 'codeFilter', 'DataRequirementCodeFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataRequirement', 'dateFilter', 'DataRequirementDateFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataRequirementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirement', nil, 'DataRequirement', js.FHIRFactoryJs);
  defineDataRequirementPropsJs(js, def);
end;


procedure defineDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
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
  js.registerElement(def, 'Dosage', 'doseRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'maxDosePerPeriod', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'maxDosePerAdministration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'maxDosePerLifetime', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'rateRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Dosage', nil, 'Dosage', js.FHIRFactoryJs);
  defineDosagePropsJs(js, def);
end;


procedure defineMarketingStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
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
  defineElementPropsJs(js, def);
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


procedure defineSubstanceMoietyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceMoiety', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceMoiety', 'stereochemistry', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety', 'opticalActivity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety', 'molecularFormula', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceMoiety', 'amount', 'SubstanceAmount', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceMoietyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceMoiety', nil, 'SubstanceMoiety', js.FHIRFactoryJs);
  defineSubstanceMoietyPropsJs(js, def);
end;


procedure defineReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Reference', 'reference', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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


procedure defineTriggerDefinitionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TriggerDefinitionCondition', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinitionCondition', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinitionCondition', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTriggerDefinitionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TriggerDefinitionCondition', nil, 'TriggerDefinitionCondition', js.FHIRFactoryJs);
  defineTriggerDefinitionConditionPropsJs(js, def);
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
  js.registerElement(def, 'TriggerDefinition', 'data', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition', 'condition', 'TriggerDefinitionCondition', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'RelatedArtifact', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'citation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'document', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedArtifact', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Annotation', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  defineElementPropsJs(js, def);
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


procedure defineUsageContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'UsageContext', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Signature', 'whoUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature', 'whoReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature', 'onBehalfOfUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature', 'onBehalfOfReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature', 'targetFormat', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature', 'sigFormat', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature', 'blob', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
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
  defineElementPropsJs(js, def);
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
  js.registerElement(def, 'ParameterDefinition', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ElementDefinitionType', 'profile', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionType', 'targetProfile', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'ElementDefinitionExample', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ElementDefinitionConstraint', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'ElementDefinitionBinding', 'valueSetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding', 'valueSetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinition', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'sliceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'ElementDefinition', 'defaultValueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'meaningWhenMissing', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'orderMeaning', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition', 'fixedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
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
  js.registerElement(def, 'ElementDefinition', 'fixedDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition', 'patternBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
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
  js.registerElement(def, 'ElementDefinition', 'patternDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'TimingRepeat', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'countMax', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'duration', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'durationMax', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'durationUnit', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat', 'frequency', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'frequencyMax', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
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
  defineElementPropsJs(js, def);
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


procedure defineMoneyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineQuantityPropsJs(js, def);
end;

procedure defineMoneyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Money', nil, 'Money', js.FHIRFactoryJs);
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
  js.registerElement(def, 'AccountGuarantor', 'party', 'Reference(Patient|RelatedPerson|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Account', 'subject', 'Reference(Patient|Device|Practitioner|Location|HealthcareService|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account', 'active', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ActivityDefinitionDynamicValue', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'ActivityDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ActivityDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'library', 'Reference(Library)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ActivityDefinition', 'transform', 'Reference(StructureMap)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'AdverseEventSuspectEntity', 'instance', 'Reference(Substance|Medication|MedicationAdministration|MedicationStatement|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'AdverseEventSuspectEntityCausality', 'author', 'Reference(Practitioner|PractitionerRole)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'AdverseEvent', 'subject', 'Reference(Patient|Group|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AdverseEvent', 'resultingCondition', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'seriousness', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'severity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'recorder', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'eventParticipant', 'Reference(Practitioner|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEvent', 'suspectEntity', 'AdverseEventSuspectEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'subjectMedicalHistory', 'Reference(Condition|Observation|AllergyIntolerance|FamilyMemberHistory|Immunization|Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'AllergyIntolerance', 'clinicalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'verificationStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'criticality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'assertedDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'recorder', 'Reference(Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'asserter', 'Reference(Patient|RelatedPerson|Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'AppointmentParticipant', 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentParticipant', 'required', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentParticipant', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'Appointment', 'serviceCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'appointmentType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'indication', 'Reference(Condition|Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'Appointment', 'incomingReferral', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'AppointmentResponse', 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'AuditEventAgent', 'reference', 'Reference(PractitionerRole|Practitioner|Organization|Device|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'userId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'AuditEventSource', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'AuditEventEntity', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Basic', 'author', 'Reference(Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Binary', 'content', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
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
  js.registerElement(def, 'BiologicallyDerivedProductCollection', 'collector', 'Reference(Practitioner|PractitionerRole|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection', 'source', 'Reference(Patient|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'BiologicallyDerivedProduct', 'parent', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'CapabilityStatementImplementation', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'CapabilityStatementRest', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'CapabilityStatementRestSecurity', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity', 'certificate', 'CapabilityStatementRestSecurityCertificate', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestSecurityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestSecurity', nil, 'CapabilityStatementRestSecurity', js.FHIRFactoryJs);
  defineCapabilityStatementRestSecurityPropsJs(js, def);
end;


procedure defineCapabilityStatementRestSecurityCertificatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestSecurityCertificate', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestSecurityCertificate', 'blob', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineCapabilityStatementRestSecurityCertificateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestSecurityCertificate', nil, 'CapabilityStatementRestSecurityCertificate', js.FHIRFactoryJs);
  defineCapabilityStatementRestSecurityCertificatePropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResource', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'supportedProfile', 'Reference(StructureDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'definition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'CapabilityStatementRestResourceOperation', 'definition', 'Reference(OperationDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'CapabilityStatementRestInteraction', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'CapabilityStatementMessaging', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint', 'address', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage', 'definition', 'Reference(MessageDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'CapabilityStatementDocument', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'CapabilityStatement', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'acceptUnknown', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'CarePlanActivity', 'reference', 'Reference(Appointment|CommunicationRequest|DeviceRequest|MedicationRequest|NutritionOrder|Task|ServiceRequest|VisionPrescription|RequestGroup)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'CarePlanActivityDetail', 'instantiates', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'goal', 'Reference(Goal)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'statusReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'prohibited', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|RelatedPerson|Patient|CareTeam|HealthcareService|Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'CarePlan', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'author', 'Reference(Patient|Practitioner|PractitionerRole|Device|RelatedPerson|Organization|CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'CareTeamParticipant', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant', 'member', 'Reference(Practitioner|RelatedPerson|Patient|Organization|CareTeam)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'CareTeam', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineChargeItemParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemParticipant', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemParticipant', 'actor', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineChargeItemParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemParticipant', nil, 'ChargeItemParticipant', js.FHIRFactoryJs);
  defineChargeItemParticipantPropsJs(js, def);
end;


procedure defineChargeItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ChargeItem', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItem', 'partOf', 'Reference(ChargeItem)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'participant', 'ChargeItemParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'performingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'requestingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'bodysite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'factorOverride', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ChargeItem', 'priceOverride', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'overrideReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItem', 'enterer', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'enteredDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'service', 'Reference(DiagnosticReport|ImagingStudy|Immunization|MedicationAdministration|MedicationDispense|Observation|Procedure|SupplyDelivery)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ClaimPayee', 'resource', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee', 'party', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ClaimCareTeam', 'provider', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineClaimInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimInformation', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimInformation', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'timingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimInformation', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimInformation', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimInformation', nil, 'ClaimInformation', js.FHIRFactoryJs);
  defineClaimInformationPropsJs(js, def);
end;


procedure defineClaimDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimDiagnosis', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimDiagnosis', 'diagnosisCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis', 'diagnosisReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ClaimProcedure', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimProcedure', 'procedureCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimProcedure', 'procedureReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ClaimItem', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ClaimItemDetail', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Claim', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'billablePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Claim', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'fundsReserve', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'related', 'ClaimRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'prescription', 'Reference(MedicationRequest|VisionPrescription)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'originalPrescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'payee', 'ClaimPayee', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'referral', 'Reference(ServiceRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'careTeam', 'ClaimCareTeam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'information', 'ClaimInformation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'diagnosis', 'ClaimDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'procedure', 'ClaimProcedure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'insurance', 'ClaimInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'accident', 'ClaimAccident', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'employmentImpacted', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'hospitalization', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ClaimResponseAddItem', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'fee', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItem', nil, 'ClaimResponseAddItem', js.FHIRFactoryJs);
  defineClaimResponseAddItemPropsJs(js, def);
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


procedure defineClaimResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClaimResponse', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponse', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'request', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'payeeType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'item', 'ClaimResponseItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'addItem', 'ClaimResponseAddItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'error', 'ClaimResponseError', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'totalCost', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'unallocDeductable', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'totalBenefit', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'payment', 'ClaimResponsePayment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'reserved', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'processNote', 'ClaimResponseProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'communicationRequest', 'Reference(CommunicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'insurance', 'ClaimResponseInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ClinicalImpressionInvestigation', 'item', 'Reference(Observation|QuestionnaireResponse|FamilyMemberHistory|DiagnosticReport|RiskAssessment|ImagingStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ClinicalImpressionFinding', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ClinicalImpression', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression', 'assessor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'previous', 'Reference(ClinicalImpression)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'problem', 'Reference(Condition|AllergyIntolerance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'investigation', 'ClinicalImpressionInvestigation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'summary', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'finding', 'ClinicalImpressionFinding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'prognosisCodeableConcept', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'prognosisReference', 'Reference(RiskAssessment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'action', 'Reference(ServiceRequest|Procedure|MedicationRequest|Appointment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'CodeSystem', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'CodeSystem', 'valueSet', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'hierarchyMeaning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'compositional', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem', 'versionNeeded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem', 'content', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'supplements', 'Reference(CodeSystem)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Communication', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'recipient', 'Reference(Device|Organization|Patient|Practitioner|PractitionerRole|RelatedPerson|Group|CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'topic', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'about', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'sent', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication', 'received', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication', 'sender', 'Reference(Device|Organization|Patient|Practitioner|PractitionerRole|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'CommunicationRequest', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest', 'medium', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'recipient', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson|Group|CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'about', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'payload', 'CommunicationRequestPayload', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest', 'requester', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'sender', 'Reference(Device|Organization|Patient|Practitioner|PractitionerRole|RelatedPerson|HealthcareService)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'CompartmentDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompartmentDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'CompositionAttester', 'time', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompositionAttester', 'party', 'Reference(Patient|Practitioner|PractitionerRole|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Composition', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Composition', 'author', 'Reference(Practitioner|PractitionerRole|Device|Patient|RelatedPerson|Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn', 'code', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'ConceptMapGroupUnmapped', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'ConceptMap', 'sourceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap', 'targetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'targetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ConditionStage', 'assessment', 'Reference(ClinicalImpression|DiagnosticReport|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'Condition', 'clinicalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition', 'verificationStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'severity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Condition', 'assertedDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition', 'recorder', 'Reference(Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'asserter', 'Reference(Practitioner|PractitionerRole|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ConsentVerification', 'verifiedWith', 'Reference(Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ConsentProvisionActor', 'reference', 'Reference(Device|Group|CareTeam|Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Consent', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Consent', 'scope', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Consent', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'dateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Consent', 'consentingParty', 'Reference(Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'organization', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'sourceAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'sourceIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineContractTermPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTerm', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTerm', 'applies', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'subType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'offer', 'ContractTermOffer', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'asset', 'ContractTermAsset', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'agent', 'ContractTermAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'action', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'actionReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'group', '@Contract.term', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTerm', nil, 'ContractTerm', js.FHIRFactoryJs);
  defineContractTermPropsJs(js, def);
end;


procedure defineContractTermOfferPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermOffer', 'topic', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer', 'decision', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermOffer', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineContractTermOfferJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermOffer', nil, 'ContractTermOffer', js.FHIRFactoryJs);
  defineContractTermOfferPropsJs(js, def);
end;


procedure defineContractTermAssetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAsset', 'class', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset', 'dataPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset', 'data', 'ContractTermAssetData', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'valuedItem', 'ContractTermAssetValuedItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermAssetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAsset', nil, 'ContractTermAsset', js.FHIRFactoryJs);
  defineContractTermAssetPropsJs(js, def);
end;


procedure defineContractTermAssetDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAssetData', 'meaning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermAssetData', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermAssetDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAssetData', nil, 'ContractTermAssetData', js.FHIRFactoryJs);
  defineContractTermAssetDataPropsJs(js, def);
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
end;

procedure defineContractTermAssetValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAssetValuedItem', nil, 'ContractTermAssetValuedItem', js.FHIRFactoryJs);
  defineContractTermAssetValuedItemPropsJs(js, def);
end;


procedure defineContractTermAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAgent', 'actor', 'Reference(Contract|Device|Group|Location|Organization|Patient|Practitioner|RelatedPerson|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAgent', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAgent', nil, 'ContractTermAgent', js.FHIRFactoryJs);
  defineContractTermAgentPropsJs(js, def);
end;


procedure defineContractSignerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractSigner', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner', 'party', 'Reference(Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Contract', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract', 'contentDerivative', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Contract', 'applies', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'subject', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'authority', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'domain', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'term', 'ContractTerm', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'signer', 'ContractSigner', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'friendly', 'ContractFriendly', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'legal', 'ContractLegal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'rule', 'ContractRule', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'CoverageClass', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineCoverageGroupingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageGrouping', 'group', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'groupDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subGroup', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subGroupDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'plan', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'planDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subPlan', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subPlanDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'class', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'classDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subClass', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subClassDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCoverageGroupingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageGrouping', nil, 'CoverageGrouping', js.FHIRFactoryJs);
  defineCoverageGroupingPropsJs(js, def);
end;


procedure defineCoverageCopayPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageCopay', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageCopay', 'value', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCoverageCopayJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageCopay', nil, 'CoverageCopay', js.FHIRFactoryJs);
  defineCoverageCopayPropsJs(js, def);
end;


procedure defineCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Coverage', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'policyHolder', 'Reference(Patient|RelatedPerson|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'subscriber', 'Reference(Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'subscriberId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'beneficiary', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'dependent', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'payor', 'Reference(Organization|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'class', 'CoverageClass', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'grouping', 'CoverageGrouping', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'sequence', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'order', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Coverage', 'network', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'copay', 'CoverageCopay', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'contract', 'Reference(Contract)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coverage', nil, 'Coverage', js.FHIRFactoryJs);
  defineCoveragePropsJs(js, def);
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
  js.registerElement(def, 'DetectedIssue', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssue', 'author', 'Reference(Practitioner|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'implicated', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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


procedure defineDeviceUdiPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceUdi', 'deviceIdentifier', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi', 'jurisdiction', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi', 'carrierHRF', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi', 'carrierAIDC', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'DeviceUdi', 'issuer', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi', 'entryType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceUdiJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUdi', nil, 'DeviceUdi', js.FHIRFactoryJs);
  defineDeviceUdiPropsJs(js, def);
end;


procedure defineDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Device', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'udi', 'DeviceUdi', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'manufacturer', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'manufactureDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device', 'expirationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device', 'model', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device', 'owner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'safety', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Device', nil, 'Device', js.FHIRFactoryJs);
  defineDevicePropsJs(js, def);
end;


procedure defineDeviceComponentProductionSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceComponentProductionSpecification', 'specType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProductionSpecification', 'componentId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProductionSpecification', 'productionSpec', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceComponentProductionSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponentProductionSpecification', nil, 'DeviceComponentProductionSpecification', js.FHIRFactoryJs);
  defineDeviceComponentProductionSpecificationPropsJs(js, def);
end;


procedure defineDeviceComponentPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceComponentProperty', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProperty', 'valueQuantity', 'Quantity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponentProperty', 'valueCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceComponentPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponentProperty', nil, 'DeviceComponentProperty', js.FHIRFactoryJs);
  defineDeviceComponentPropertyPropsJs(js, def);
end;


procedure defineDeviceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceComponent', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'lastSystemChange', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceComponent', 'source', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'parent', 'Reference(DeviceComponent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'operationalStatus', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent', 'parameterGroup', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'measurementPrinciple', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceComponent', 'productionSpecification', 'DeviceComponentProductionSpecification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent', 'languageCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'property', 'DeviceComponentProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponent', nil, 'DeviceComponent', js.FHIRFactoryJs);
  defineDeviceComponentPropsJs(js, def);
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
  js.registerElement(def, 'DeviceMetric', 'parent', 'Reference(DeviceComponent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'DeviceRequest', 'intent', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest', 'codeReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'codeCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'parameter', 'DeviceRequestParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'subject', 'Reference(Patient|Group|Location|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest', 'requester', 'Reference(Device|Practitioner|PractitionerRole|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam|HealthcareService|Patient|Device|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'insurance', 'Reference(Coverage|ClaimResponse)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'DeviceUseStatement', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'derivedFrom', 'Reference(ServiceRequest|Procedure|Claim|Observation|QuestionnaireResponse|DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement', 'recordedOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement', 'source', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference|Media)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'DiagnosticReport', 'basedOn', 'Reference(CarePlan|ImmunizationRecommendation|MedicationRequest|NutritionOrder|ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'subject', 'Reference(Patient|Group|Device|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'resultsInterpreter', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'result', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'imagingStudy', 'Reference(ImagingStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'media', 'DiagnosticReportMedia', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'conclusion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport', 'codedDiagnosis', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'presentedForm', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDiagnosticReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReport', nil, 'DiagnosticReport', js.FHIRFactoryJs);
  defineDiagnosticReportPropsJs(js, def);
end;


procedure defineDocumentManifestAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestAgent', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestAgent', 'who', 'Reference(Practitioner|PractitionerRole|Organization|Device|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentManifestAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestAgent', nil, 'DocumentManifestAgent', js.FHIRFactoryJs);
  defineDocumentManifestAgentPropsJs(js, def);
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
  js.registerElement(def, 'DocumentManifest', 'subject', 'Reference(Patient|Practitioner|Group|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentManifest', 'agent', 'DocumentManifestAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'recipient', 'Reference(Patient|Practitioner|PractitionerRole|RelatedPerson|Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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


procedure defineDocumentReferenceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceAgent', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceAgent', 'who', 'Reference(Practitioner|PractitionerRole|Organization|Device|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceAgent', nil, 'DocumentReferenceAgent', js.FHIRFactoryJs);
  defineDocumentReferenceAgentPropsJs(js, def);
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
  js.registerElement(def, 'DocumentReferenceContext', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'event', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReferenceContext', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'facilityType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'practiceSetting', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'sourcePatientInfo', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'related', 'DocumentReferenceContextRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentReferenceContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContext', nil, 'DocumentReferenceContext', js.FHIRFactoryJs);
  defineDocumentReferenceContextPropsJs(js, def);
end;


procedure defineDocumentReferenceContextRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContextRelated', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContextRelated', 'ref', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceContextRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContextRelated', nil, 'DocumentReferenceContextRelated', js.FHIRFactoryJs);
  defineDocumentReferenceContextRelatedPropsJs(js, def);
end;


procedure defineDocumentReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentReference', 'masterIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference', 'docStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'subject', 'Reference(Patient|Practitioner|Group|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference', 'date', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference', 'agent', 'DocumentReferenceAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'authenticator', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineEligibilityRequestAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityRequestAuthorization', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'EligibilityRequestAuthorization', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequestAuthorization', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityRequestAuthorization', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequestAuthorization', 'facility', 'Reference(Location|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityRequestAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityRequestAuthorization', nil, 'EligibilityRequestAuthorization', js.FHIRFactoryJs);
  defineEligibilityRequestAuthorizationPropsJs(js, def);
end;


procedure defineEligibilityRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityRequest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityRequest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityRequest', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityRequest', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityRequest', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityRequest', 'benefitCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'benefitSubCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'authorization', 'EligibilityRequestAuthorization', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityRequest', nil, 'EligibilityRequest', js.FHIRFactoryJs);
  defineEligibilityRequestPropsJs(js, def);
end;


procedure defineEligibilityResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsurance', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsurance', 'contract', 'Reference(Contract)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsurance', 'benefitBalance', 'EligibilityResponseInsuranceBenefitBalance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsurance', nil, 'EligibilityResponseInsurance', js.FHIRFactoryJs);
  defineEligibilityResponseInsurancePropsJs(js, def);
end;


procedure defineEligibilityResponseInsuranceBenefitBalancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'subCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'excluded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'network', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'term', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'financial', 'EligibilityResponseInsuranceBenefitBalanceFinancial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseInsuranceBenefitBalanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsuranceBenefitBalance', nil, 'EligibilityResponseInsuranceBenefitBalance', js.FHIRFactoryJs);
  defineEligibilityResponseInsuranceBenefitBalancePropsJs(js, def);
end;


procedure defineEligibilityResponseInsuranceBenefitBalanceFinancialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'allowedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'allowedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'allowedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'usedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'usedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityResponseInsuranceBenefitBalanceFinancialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsuranceBenefitBalanceFinancial', nil, 'EligibilityResponseInsuranceBenefitBalanceFinancial', js.FHIRFactoryJs);
  defineEligibilityResponseInsuranceBenefitBalanceFinancialPropsJs(js, def);
end;


procedure defineEligibilityResponseAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseAuthorization', 'authorizationSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'EligibilityResponseAuthorization', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponseAuthorization', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseAuthorization', nil, 'EligibilityResponseAuthorization', js.FHIRFactoryJs);
  defineEligibilityResponseAuthorizationPropsJs(js, def);
end;


procedure defineEligibilityResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseError', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseError', nil, 'EligibilityResponseError', js.FHIRFactoryJs);
  defineEligibilityResponseErrorPropsJs(js, def);
end;


procedure defineEligibilityResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityResponse', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityResponse', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'request', 'Reference(EligibilityRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'inforce', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponse', 'insurance', 'EligibilityResponseInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse', 'preAuthRef', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse', 'authorization', 'EligibilityResponseAuthorization', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'error', 'EligibilityResponseError', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponse', nil, 'EligibilityResponse', js.FHIRFactoryJs);
  defineEligibilityResponsePropsJs(js, def);
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
  js.registerElement(def, 'EncounterParticipant', 'individual', 'Reference(Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'EncounterDiagnosis', 'condition', 'Reference(Condition|Procedure)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Encounter', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'episodeOfCare', 'Reference(EpisodeOfCare)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'incomingReferral', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'participant', 'EncounterParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'appointment', 'Reference(Appointment)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'length', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'Endpoint', 'address', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'EnrollmentRequest', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'EnrollmentResponse', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentResponse', nil, 'EnrollmentResponse', js.FHIRFactoryJs);
  defineEnrollmentResponsePropsJs(js, def);
end;


procedure defineEntryDefinitionRelatedEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EntryDefinitionRelatedEntry', 'relationtype', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinitionRelatedEntry', 'item', 'Reference(EntryDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEntryDefinitionRelatedEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EntryDefinitionRelatedEntry', nil, 'EntryDefinitionRelatedEntry', js.FHIRFactoryJs);
  defineEntryDefinitionRelatedEntryPropsJs(js, def);
end;


procedure defineEntryDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EntryDefinition', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'purpose', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'referencedItem', 'Reference(Medication|Device|Organization|Practitioner|HealthcareService|ActivityDefinition|PlanDefinition|SpecimenDefinition|ObservationDefinition|Binary)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'additionalIdentifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition', 'classification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'lastUpdated', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EntryDefinition', 'additionalCharacteristic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition', 'additionalClassification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition', 'relatedEntry', 'EntryDefinitionRelatedEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEntryDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EntryDefinition', nil, 'EntryDefinition', js.FHIRFactoryJs);
  defineEntryDefinitionPropsJs(js, def);
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
  js.registerElement(def, 'EventDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EventDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'trigger', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEventDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EventDefinition', nil, 'EventDefinition', js.FHIRFactoryJs);
  defineEventDefinitionPropsJs(js, def);
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
  js.registerElement(def, 'ExampleScenarioProcessStep', 'alternative', 'ExampleScenarioProcessStepAlternative', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative', 'option', 'ExampleScenarioProcessStepAlternativeOption', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessStepAlternativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepAlternative', nil, 'ExampleScenarioProcessStepAlternative', js.FHIRFactoryJs);
  defineExampleScenarioProcessStepAlternativePropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepAlternativeOptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternativeOption', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternativeOption', 'step', '@ExampleScenario.process.step', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessStepAlternativeOptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepAlternativeOption', nil, 'ExampleScenarioProcessStepAlternativeOption', js.FHIRFactoryJs);
  defineExampleScenarioProcessStepAlternativeOptionPropsJs(js, def);
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
  js.registerElement(def, 'ExampleScenario', 'workflow', 'Reference(ExampleScenario)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenario', nil, 'ExampleScenario', js.FHIRFactoryJs);
  defineExampleScenarioPropsJs(js, def);
end;


procedure defineExpansionProfileFixedVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileFixedVersion', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileFixedVersion', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileFixedVersion', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExpansionProfileFixedVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileFixedVersion', nil, 'ExpansionProfileFixedVersion', js.FHIRFactoryJs);
  defineExpansionProfileFixedVersionPropsJs(js, def);
end;


procedure defineExpansionProfileExcludedSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileExcludedSystem', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileExcludedSystem', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExpansionProfileExcludedSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileExcludedSystem', nil, 'ExpansionProfileExcludedSystem', js.FHIRFactoryJs);
  defineExpansionProfileExcludedSystemPropsJs(js, def);
end;


procedure defineExpansionProfileDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignation', 'include', 'ExpansionProfileDesignationInclude', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfileDesignation', 'exclude', 'ExpansionProfileDesignationExclude', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignation', nil, 'ExpansionProfileDesignation', js.FHIRFactoryJs);
  defineExpansionProfileDesignationPropsJs(js, def);
end;


procedure defineExpansionProfileDesignationIncludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationInclude', 'designation', 'ExpansionProfileDesignationIncludeDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExpansionProfileDesignationIncludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationInclude', nil, 'ExpansionProfileDesignationInclude', js.FHIRFactoryJs);
  defineExpansionProfileDesignationIncludePropsJs(js, def);
end;


procedure defineExpansionProfileDesignationIncludeDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationIncludeDesignation', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileDesignationIncludeDesignation', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationIncludeDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationIncludeDesignation', nil, 'ExpansionProfileDesignationIncludeDesignation', js.FHIRFactoryJs);
  defineExpansionProfileDesignationIncludeDesignationPropsJs(js, def);
end;


procedure defineExpansionProfileDesignationExcludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationExclude', 'designation', 'ExpansionProfileDesignationExcludeDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExpansionProfileDesignationExcludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationExclude', nil, 'ExpansionProfileDesignationExclude', js.FHIRFactoryJs);
  defineExpansionProfileDesignationExcludePropsJs(js, def);
end;


procedure defineExpansionProfileDesignationExcludeDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationExcludeDesignation', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileDesignationExcludeDesignation', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationExcludeDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationExcludeDesignation', nil, 'ExpansionProfileDesignationExcludeDesignation', js.FHIRFactoryJs);
  defineExpansionProfileDesignationExcludeDesignationPropsJs(js, def);
end;


procedure defineExpansionProfilePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ExpansionProfile', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExpansionProfile', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile', 'fixedVersion', 'ExpansionProfileFixedVersion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile', 'excludedSystem', 'ExpansionProfileExcludedSystem', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile', 'includeDesignations', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'designation', 'ExpansionProfileDesignation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile', 'includeDefinition', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'activeOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'excludeNested', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'excludeNotForUI', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'excludePostCoordinated', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'displayLanguage', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'limitedExpansion', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineExpansionProfileJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfile', nil, 'ExpansionProfile', js.FHIRFactoryJs);
  defineExpansionProfilePropsJs(js, def);
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
  js.registerElement(def, 'ExplanationOfBenefitPayee', 'resource', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayee', 'party', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayee', nil, 'ExplanationOfBenefitPayee', js.FHIRFactoryJs);
  defineExplanationOfBenefitPayeePropsJs(js, def);
end;


procedure defineExplanationOfBenefitInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'timingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'reason', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitInformation', nil, 'ExplanationOfBenefitInformation', js.FHIRFactoryJs);
  defineExplanationOfBenefitInformationPropsJs(js, def);
end;


procedure defineExplanationOfBenefitCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'provider', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineExplanationOfBenefitDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'diagnosisCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'diagnosisReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'procedureCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'procedureReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ExplanationOfBenefitItem', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'fee', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItem', nil, 'ExplanationOfBenefitAddItem', js.FHIRFactoryJs);
  defineExplanationOfBenefitAddItemPropsJs(js, def);
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
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'subCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ExplanationOfBenefit', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'billablePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'referral', 'Reference(ServiceRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'claim', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'related', 'ExplanationOfBenefitRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'prescription', 'Reference(MedicationRequest|VisionPrescription)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'originalPrescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'payee', 'ExplanationOfBenefitPayee', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'information', 'ExplanationOfBenefitInformation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'careTeam', 'ExplanationOfBenefitCareTeam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'diagnosis', 'ExplanationOfBenefitDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'procedure', 'ExplanationOfBenefitProcedure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'precedence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'insurance', 'ExplanationOfBenefitInsurance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'accident', 'ExplanationOfBenefitAccident', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'employmentImpacted', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'hospitalization', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'item', 'ExplanationOfBenefitItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'addItem', 'ExplanationOfBenefitAddItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'totalCost', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'unallocDeductable', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'totalBenefit', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'payment', 'ExplanationOfBenefitPayment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'processNote', 'ExplanationOfBenefitProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'FamilyMemberHistory', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'FamilyMemberHistory', 'reasonReference', 'Reference(Condition|Observation|AllergyIntolerance|QuestionnaireResponse|DiagnosticReport|DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'Flag', 'subject', 'Reference(Patient|Location|Group|Organization|Practitioner|PlanDefinition|Medication|Procedure)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'author', 'Reference(Device|Organization|Patient|Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Goal', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'description', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'subject', 'Reference(Patient|Group|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'startDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal', 'startCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'target', 'GoalTarget', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'statusDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal', 'statusReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal', 'expressedBy', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'addresses', 'Reference(Condition|Observation|MedicationStatement|NutritionOrder|ServiceRequest|RiskAssessment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'profile', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'GraphDefinition', 'profile', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'GroupMember', 'entity', 'Reference(Patient|Practitioner|Device|Medication|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'GuidanceResponse', 'requestId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'module', 'Reference(ServiceDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GuidanceResponse', 'performer', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse', 'evaluationMessage', 'Reference(OperationOutcome)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse', 'outputParameters', 'Reference(Parameters)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'result', 'Reference(CarePlan|RequestGroup)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'dataRequirement', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGuidanceResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GuidanceResponse', nil, 'GuidanceResponse', js.FHIRFactoryJs);
  defineGuidanceResponsePropsJs(js, def);
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
  js.registerElement(def, 'HealthcareService', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'extraDetails', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'photo', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'coverageArea', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'serviceProvisionCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'eligibility', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'eligibilityNote', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'characteristic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ImagingStudySeries', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'number', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'modality', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'numberOfInstances', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'availability', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries', 'bodySite', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'laterality', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries', 'started', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudySeries', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries', 'instance', 'ImagingStudySeriesInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudySeriesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeries', nil, 'ImagingStudySeries', js.FHIRFactoryJs);
  defineImagingStudySeriesPropsJs(js, def);
end;


procedure defineImagingStudySeriesInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'number', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'sopClass', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'ImagingStudy', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'accession', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'availability', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'modalityList', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'subject', 'Reference(Patient|Device|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'started', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudy', 'basedOn', 'Reference(CarePlan|ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'referrer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'interpreter', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'numberOfSeries', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'numberOfInstances', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'procedureReference', 'Reference(Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'procedureCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineImmunizationPractitionerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationPractitioner', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationPractitioner', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImmunizationPractitionerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationPractitioner', nil, 'ImmunizationPractitioner', js.FHIRFactoryJs);
  defineImmunizationPractitionerPropsJs(js, def);
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


procedure defineImmunizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Immunization', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization', 'vaccineCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization', 'primarySource', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization', 'reportOrigin', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization', 'expirationDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'practitioner', 'ImmunizationPractitioner', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'education', 'ImmunizationEducation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'programEligibility', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'fundingSource', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ImmunizationEvaluation', 'targetDisease', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'immunizationEvent', 'Reference(Immunization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'doseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'doseStatusReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'series', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'doseNumber', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'seriesDoses', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
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
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'doseNumber', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'seriesDoses', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'supportingImmunization', 'Reference(Immunization|ImmunizationEvaluation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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


procedure defineImplementationGuideDependencyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDependency', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDependency', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDependencyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDependency', nil, 'ImplementationGuideDependency', js.FHIRFactoryJs);
  defineImplementationGuideDependencyPropsJs(js, def);
end;


procedure defineImplementationGuidePackagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePackage', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackage', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackage', 'resource', 'ImplementationGuidePackageResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuidePackageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePackage', nil, 'ImplementationGuidePackage', js.FHIRFactoryJs);
  defineImplementationGuidePackagePropsJs(js, def);
end;


procedure defineImplementationGuidePackageResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'example', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'acronym', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'sourceUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'sourceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'exampleFor', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuidePackageResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePackageResource', nil, 'ImplementationGuidePackageResource', js.FHIRFactoryJs);
  defineImplementationGuidePackageResourcePropsJs(js, def);
end;


procedure defineImplementationGuideGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideGlobal', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideGlobal', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideGlobal', nil, 'ImplementationGuideGlobal', js.FHIRFactoryJs);
  defineImplementationGuideGlobalPropsJs(js, def);
end;


procedure defineImplementationGuidePagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePage', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage', 'format', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage', 'page', '@ImplementationGuide.page', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuidePageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePage', nil, 'ImplementationGuidePage', js.FHIRFactoryJs);
  defineImplementationGuidePagePropsJs(js, def);
end;


procedure defineImplementationGuidePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuide', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuide', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuide', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'dependency', 'ImplementationGuideDependency', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'package', 'ImplementationGuidePackage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'global', 'ImplementationGuideGlobal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'page', 'ImplementationGuidePage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuide', nil, 'ImplementationGuide', js.FHIRFactoryJs);
  defineImplementationGuidePropsJs(js, def);
end;


procedure defineImplementationGuideInputDependencyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInputDependency', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputDependency', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideInputDependencyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInputDependency', nil, 'ImplementationGuideInputDependency', js.FHIRFactoryJs);
  defineImplementationGuideInputDependencyPropsJs(js, def);
end;


procedure defineImplementationGuideInputPackagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInputPackage', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPackage', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPackage', 'resource', 'ImplementationGuideInputPackageResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideInputPackageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInputPackage', nil, 'ImplementationGuideInputPackage', js.FHIRFactoryJs);
  defineImplementationGuideInputPackagePropsJs(js, def);
end;


procedure defineImplementationGuideInputPackageResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInputPackageResource', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideInputPackageResource', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPackageResource', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPackageResource', 'exampleBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideInputPackageResource', 'exampleReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideInputPackageResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInputPackageResource', nil, 'ImplementationGuideInputPackageResource', js.FHIRFactoryJs);
  defineImplementationGuideInputPackageResourcePropsJs(js, def);
end;


procedure defineImplementationGuideInputGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInputGlobal', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputGlobal', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideInputGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInputGlobal', nil, 'ImplementationGuideInputGlobal', js.FHIRFactoryJs);
  defineImplementationGuideInputGlobalPropsJs(js, def);
end;


procedure defineImplementationGuideInputPagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInputPage', 'sourceUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPage', 'sourceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideInputPage', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPage', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPage', 'format', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPage', 'page', '@ImplementationGuideInput.page', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideInputPageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInputPage', nil, 'ImplementationGuideInputPage', js.FHIRFactoryJs);
  defineImplementationGuideInputPagePropsJs(js, def);
end;


procedure defineImplementationGuideInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInput', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideInput', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuideInput', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'dependency', 'ImplementationGuideInputDependency', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'package', 'ImplementationGuideInputPackage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'global', 'ImplementationGuideInputGlobal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'page', 'ImplementationGuideInputPage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInput', nil, 'ImplementationGuideInput', js.FHIRFactoryJs);
  defineImplementationGuideInputPropsJs(js, def);
end;


procedure defineImplementationGuideOutputDependencyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideOutputDependency', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutputDependency', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideOutputDependencyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideOutputDependency', nil, 'ImplementationGuideOutputDependency', js.FHIRFactoryJs);
  defineImplementationGuideOutputDependencyPropsJs(js, def);
end;


procedure defineImplementationGuideOutputResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideOutputResource', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideOutputResource', 'exampleBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideOutputResource', 'exampleReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideOutputResource', 'relativePath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideOutputResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideOutputResource', nil, 'ImplementationGuideOutputResource', js.FHIRFactoryJs);
  defineImplementationGuideOutputResourcePropsJs(js, def);
end;


procedure defineImplementationGuideOutputGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideOutputGlobal', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutputGlobal', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideOutputGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideOutputGlobal', nil, 'ImplementationGuideOutputGlobal', js.FHIRFactoryJs);
  defineImplementationGuideOutputGlobalPropsJs(js, def);
end;


procedure defineImplementationGuideOutputPagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideOutputPage', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutputPage', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideOutputPageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideOutputPage', nil, 'ImplementationGuideOutputPage', js.FHIRFactoryJs);
  defineImplementationGuideOutputPagePropsJs(js, def);
end;


procedure defineImplementationGuideOutputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideOutput', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'dependency', 'ImplementationGuideOutputDependency', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'resource', 'ImplementationGuideOutputResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'global', 'ImplementationGuideOutputGlobal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'rendering', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'page', 'ImplementationGuideOutputPage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideOutputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideOutput', nil, 'ImplementationGuideOutput', js.FHIRFactoryJs);
  defineImplementationGuideOutputPropsJs(js, def);
end;


procedure defineInvoiceParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceParticipant', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceParticipant', 'actor', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'InvoiceLineItem', 'chargeItem', 'Reference(ChargeItem)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'InvoiceLineItemPriceComponent', 'factor', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent', 'amount', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
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
  js.registerElement(def, 'Invoice', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'recipient', 'Reference(Organization|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineItemInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ItemInstance', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ItemInstance', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ItemInstance', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ItemInstance', 'manufactureDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ItemInstance', 'expiryDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ItemInstance', 'currentSWVersion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ItemInstance', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ItemInstance', 'serialNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ItemInstance', 'carrierAIDC', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ItemInstance', 'carrierHRF', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineItemInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ItemInstance', nil, 'ItemInstance', js.FHIRFactoryJs);
  defineItemInstancePropsJs(js, def);
end;


procedure defineLibraryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Library', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Library', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'Linkage', 'author', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'List', 'subject', 'Reference(Patient|Group|Device|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'List', 'source', 'Reference(Practitioner|PractitionerRole|Patient|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Location', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MeasureGroupPopulation', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'MeasureGroupStratifier', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupStratifier', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMeasureGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupStratifier', nil, 'MeasureGroupStratifier', js.FHIRFactoryJs);
  defineMeasureGroupStratifierPropsJs(js, def);
end;


procedure defineMeasureSupplementalDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureSupplementalData', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureSupplementalData', 'usage', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureSupplementalData', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureSupplementalData', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureSupplementalData', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'Measure', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Measure', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'subject', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'library', 'Reference(Library)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'disclaimer', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'scoring', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'compositeScoring', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'riskAdjustment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'rateAggregation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'rationale', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'clinicalRecommendationStatement', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'improvementNotation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'guidance', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'set', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'MeasureReportGroupPopulation', 'subjects', 'Reference(List)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MeasureReportGroupStratifier', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineMeasureReportGroupStratifierStratumPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation', 'subjects', 'Reference(List)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MeasureReport', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport', 'measure', 'Reference(Measure)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'subject', 'Reference(Patient|Practitioner|Location|Device|RelatedPerson|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MeasureReport', 'reportingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'group', 'MeasureReportGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReport', 'evaluatedResources', 'Reference(Bundle)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Media', 'basedOn', 'Reference(ServiceRequest|CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media', 'partOf', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media', 'modality', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'view', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'subject', 'Reference(Patient|Practitioner|Group|Device|Specimen|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'createdDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Media', 'createdPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Media', 'operator', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam|Patient|Device|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'deviceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media', 'device', 'Reference(Device|DeviceMetric|DeviceComponent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MedicationIngredient', 'amount', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Medication', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Medication', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MedicationAdministrationPerformer', 'actor', 'Reference(Practitioner|PractitionerRole|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MedicationAdministration', 'partOf', 'Reference(MedicationAdministration|Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministration', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationAdministration', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'performer', 'MedicationAdministrationPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'statusReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'MedicationDispensePerformer', 'actor', 'Reference(Practitioner|PractitionerRole|Organization|Patient|Device|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MedicationDispense', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MedicationDispense', 'receiver', 'Reference(Patient|Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'dosageInstruction', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'substitution', 'MedicationDispenseSubstitution', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'detectedIssue', 'Reference(DetectedIssue)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'statusReasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'statusReasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'eventHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispense', nil, 'MedicationDispense', js.FHIRFactoryJs);
  defineMedicationDispensePropsJs(js, def);
end;


procedure defineMedicationRequestDispenseRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
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


procedure defineMedicationRequestSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestSubstitution', 'allowed', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
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
  js.registerElement(def, 'MedicationRequest', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationRequest', 'requester', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|Patient|Device|RelatedPerson|CareTeam)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'recorder', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'reasonReference', 'Reference(Condition|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'basedOn', 'Reference(CarePlan|MedicationRequest|ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'insurance', 'Reference(Coverage|ClaimResponse)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'MedicationStatement', 'basedOn', 'Reference(MedicationRequest|CarePlan|ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'partOf', 'Reference(MedicationAdministration|MedicationDispense|MedicationStatement|Procedure|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement', 'statusReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'dateAsserted', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement', 'informationSource', 'Reference(Patient|Practitioner|RelatedPerson|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'derivedFrom', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'MedicinalProductName', 'fullName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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


procedure defineMedicinalProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProduct', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'combinedPharmaceuticalDoseForm', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'additionalMonitoringIndicator', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'paediatricUseIndicator', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'orphanDesignationStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'productClassification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'marketingAuthorization', 'Reference(MedicinalProductAuthorization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'packagedMedicinalProduct', 'Reference(MedicinalProductPackaged)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'pharmaceuticalProduct', 'Reference(MedicinalProductPharmaceutical)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'clinicalParticulars', 'Reference(MedicinalProductClinicals)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'attachedDocument', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'masterFile', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'name', 'MedicinalProductName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'crossReference', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'manufacturingBusinessOperation', 'MedicinalProductManufacturingBusinessOperation', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'country', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'jurisdiction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'number', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'legalStatusOfSupply', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'number', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'date', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'application', 'MedicinalProductAuthorizationProcedureApplication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductAuthorizationProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationProcedure', nil, 'MedicinalProductAuthorizationProcedure', js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationProcedurePropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationProcedureApplicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedureApplication', 'number', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedureApplication', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedureApplication', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineMedicinalProductAuthorizationProcedureApplicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationProcedureApplication', nil, 'MedicinalProductAuthorizationProcedureApplication', js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationProcedureApplicationPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorization', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'country', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'legalStatusOfSupply', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'statusDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'restoreDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'dataExclusivityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'dateOfFirstAuthorization', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'internationalBirthDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'jurisdictionalAuthorization', 'MedicinalProductAuthorizationJurisdictionalAuthorization', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'holder', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'regulator', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'procedure', 'MedicinalProductAuthorizationProcedure', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'marketingStatus', 'MarketingStatus', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorization', nil, 'MedicinalProductAuthorization', js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsUndesirableEffectsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects', 'symptomConditionEffect', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects', 'classification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects', 'frequencyOfOccurrence', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects', 'population', 'MedicinalProductClinicalsUndesirableEffectsPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsUndesirableEffectsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsUndesirableEffects', nil, 'MedicinalProductClinicalsUndesirableEffects', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsUndesirableEffectsPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsUndesirableEffectsPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation', 'ageRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation', 'ageCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation', 'gender', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation', 'race', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation', 'physiologicalCondition', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductClinicalsUndesirableEffectsPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsUndesirableEffectsPopulation', nil, 'MedicinalProductClinicalsUndesirableEffectsPopulation', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsUndesirableEffectsPopulationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsTherapeuticIndicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'diseaseSymptomProcedure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'diseaseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'comorbidity', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'intendedEffect', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'duration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'undesirableEffects', '@MedicinalProductClinicals.undesirableEffects', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'otherTherapy', 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'population', '@MedicinalProductClinicals.undesirableEffects.population', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsTherapeuticIndicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsTherapeuticIndication', nil, 'MedicinalProductClinicalsTherapeuticIndication', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsTherapeuticIndicationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsTherapeuticIndicationOtherTherapyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', 'therapyRelationshipType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductClinicalsTherapeuticIndicationOtherTherapyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', nil, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsTherapeuticIndicationOtherTherapyPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsContraindicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'disease', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'diseaseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'comorbidity', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'therapeuticIndication', '@MedicinalProductClinicals.therapeuticIndication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'otherTherapy', '@MedicinalProductClinicals.therapeuticIndication.otherTherapy', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'population', '@MedicinalProductClinicals.undesirableEffects.population', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsContraindicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsContraindication', nil, 'MedicinalProductClinicalsContraindication', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsContraindicationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsInteractionsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions', 'interactant', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions', 'effect', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions', 'incidence', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions', 'management', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductClinicalsInteractionsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsInteractions', nil, 'MedicinalProductClinicalsInteractions', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsInteractionsPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicals', 'undesirableEffects', 'MedicinalProductClinicalsUndesirableEffects', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicals', 'therapeuticIndication', 'MedicinalProductClinicalsTherapeuticIndication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicals', 'contraindication', 'MedicinalProductClinicalsContraindication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicals', 'interactions', 'MedicinalProductClinicalsInteractions', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicals', nil, 'MedicinalProductClinicals', js.FHIRFactoryJs);
  defineMedicinalProductClinicalsPropsJs(js, def);
end;


procedure defineMedicinalProductDeviceSpecMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductDeviceSpecMaterial', 'substance', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpecMaterial', 'alternate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicinalProductDeviceSpecMaterial', 'allergenicIndicator', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineMedicinalProductDeviceSpecMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductDeviceSpecMaterial', nil, 'MedicinalProductDeviceSpecMaterial', js.FHIRFactoryJs);
  defineMedicinalProductDeviceSpecMaterialPropsJs(js, def);
end;


procedure defineMedicinalProductDeviceSpecPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'tradeName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'listingNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'modelNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'sterilityIndicator', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'sterilisationRequirement', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'usage', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'nomenclature', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'shelfLife', 'ProductShelfLife', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'physicalCharacteristics', 'ProdCharacteristic', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'otherCharacteristics', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'batchIdentifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'material', 'MedicinalProductDeviceSpecMaterial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductDeviceSpecJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductDeviceSpec', nil, 'MedicinalProductDeviceSpec', js.FHIRFactoryJs);
  defineMedicinalProductDeviceSpecPropsJs(js, def);
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
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'concentration', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'device', 'Reference(MedicinalProductDeviceSpec)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'manufacturedItem', 'MedicinalProductPackagedPackageItemManufacturedItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'otherCharacteristics', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'packageItem', '@MedicinalProductPackaged.packageItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'physicalCharacteristics', 'ProdCharacteristic', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'shelfLifeStorage', 'ProductShelfLife', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPackagedPackageItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedPackageItem', nil, 'MedicinalProductPackagedPackageItem', js.FHIRFactoryJs);
  defineMedicinalProductPackagedPackageItemPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPackageItemManufacturedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'manufacturedDoseForm', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'unitOfPresentation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'xManufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'ingredient', 'Reference(MedicinalProductIngredient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'physicalCharacteristics', 'ProdCharacteristic', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductPackagedPackageItemManufacturedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedPackageItemManufacturedItem', nil, 'MedicinalProductPackagedPackageItemManufacturedItem', js.FHIRFactoryJs);
  defineMedicinalProductPackagedPackageItemManufacturedItemPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackaged', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'marketingStatus', 'MarketingStatus', js.getFHIRArrayProp, js.setFHIRArrayProp);
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


procedure defineMedicinalProductPharmaceuticalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'administrableDoseForm', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'unitOfPresentation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'routeOfAdministration', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'ingredient', 'Reference(MedicinalProductIngredient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'characteristics', 'MedicinalProductPharmaceuticalCharacteristics', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPharmaceuticalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceutical', nil, 'MedicinalProductPharmaceutical', js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalPropsJs(js, def);
end;


procedure defineMessageDefinitionFocusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionFocus', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MessageDefinitionAllowedResponse', 'message', 'Reference(MessageDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MessageDefinition', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MessageDefinition', 'base', 'Reference(MessageDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinition', 'parent', 'Reference(ActivityDefinition|PlanDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'replaces', 'Reference(MessageDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'event', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'focus', 'MessageDefinitionFocus', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'responseRequired', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
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
  js.registerElement(def, 'MessageHeaderDestination', 'endpoint', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination', 'receiver', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'MessageHeaderSource', 'endpoint', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'MessageHeader', 'event', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'destination', 'MessageHeaderDestination', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader', 'sender', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'source', 'MessageHeaderSource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'responsible', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'response', 'MessageHeaderResponse', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'focus', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader', 'definition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeader', nil, 'MessageHeader', js.FHIRFactoryJs);
  defineMessageHeaderPropsJs(js, def);
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
  js.registerElement(def, 'NutritionOrder', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'dateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NutritionOrder', 'orderer', 'Reference(Practitioner|PractitionerRole)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ObservationComponent', 'interpretation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Observation', 'basedOn', 'Reference(CarePlan|DeviceRequest|ImmunizationRecommendation|MedicationRequest|NutritionOrder|ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'partOf', 'Reference(MedicationAdministration|MedicationDispense|MedicationStatement|Procedure|Immunization|ImagingStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'subject', 'Reference(Patient|Group|Device|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'effectiveTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'Observation', 'interpretation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'specimen', 'Reference(Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'device', 'Reference(Device|DeviceComponent|DeviceMetric)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'referenceRange', 'ObservationReferenceRange', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'hasMember', 'Reference(Observation|QuestionnaireResponse|Sequence)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'derivedFrom', 'Reference(DocumentReference|ImagingStudy|Media|QuestionnaireResponse|Observation|Sequence)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails', 'customaryUnit', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails', 'unit', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'range', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'appliesTo', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ObservationDefinition', 'category', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'permittedDataType', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition', 'multipleResultsAllowed', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ObservationDefinition', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'preferredReportName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition', 'quantitativeDetails', 'ObservationDefinitionQuantitativeDetails', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'qualifiedInterval', 'ObservationDefinitionQualifiedInterval', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition', 'validCodedValueSet', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition', 'normalCodedValueSet', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition', 'abnormalCodedValueSet', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition', 'criticalCodedValueSet', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineObservationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinition', nil, 'ObservationDefinition', js.FHIRFactoryJs);
  defineObservationDefinitionPropsJs(js, def);
end;


procedure defineOccupationalDataEmploymentStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataEmploymentStatus', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataEmploymentStatus', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalDataEmploymentStatus', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOccupationalDataEmploymentStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataEmploymentStatus', nil, 'OccupationalDataEmploymentStatus', js.FHIRFactoryJs);
  defineOccupationalDataEmploymentStatusPropsJs(js, def);
end;


procedure defineOccupationalDataUsualOccupationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataUsualOccupation', 'occupationCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataUsualOccupation', 'industryCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataUsualOccupation', 'start', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalDataUsualOccupation', 'duration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOccupationalDataUsualOccupationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataUsualOccupation', nil, 'OccupationalDataUsualOccupation', js.FHIRFactoryJs);
  defineOccupationalDataUsualOccupationPropsJs(js, def);
end;


procedure defineOccupationalDataPastOrPresentOccupationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'occupationCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'industryCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'employer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'employmentType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'supervisoryLevel', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'workSchedule', 'OccupationalDataPastOrPresentOccupationWorkSchedule', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOccupationalDataPastOrPresentOccupationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataPastOrPresentOccupation', nil, 'OccupationalDataPastOrPresentOccupation', js.FHIRFactoryJs);
  defineOccupationalDataPastOrPresentOccupationPropsJs(js, def);
end;


procedure defineOccupationalDataPastOrPresentOccupationWorkSchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupationWorkSchedule', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupationWorkSchedule', 'weeklyWorkDays', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupationWorkSchedule', 'dailyWorkHours', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineOccupationalDataPastOrPresentOccupationWorkScheduleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataPastOrPresentOccupationWorkSchedule', nil, 'OccupationalDataPastOrPresentOccupationWorkSchedule', js.FHIRFactoryJs);
  defineOccupationalDataPastOrPresentOccupationWorkSchedulePropsJs(js, def);
end;


procedure defineOccupationalDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OccupationalData', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OccupationalData', 'subject', 'Reference(Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalData', 'author', 'Reference(Practitioner|PractitionerRole|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OccupationalData', 'employmentStatus', 'OccupationalDataEmploymentStatus', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData', 'combatZoneWork', 'Period', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OccupationalData', 'usualOccupation', 'OccupationalDataUsualOccupation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData', 'pastOrPresentOccupation', 'OccupationalDataPastOrPresentOccupation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOccupationalDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalData', nil, 'OccupationalData', js.FHIRFactoryJs);
  defineOccupationalDataPropsJs(js, def);
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
  js.registerElement(def, 'OperationDefinitionParameterBinding', 'valueSetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterBinding', 'valueSetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOperationDefinitionParameterBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameterBinding', nil, 'OperationDefinitionParameterBinding', js.FHIRFactoryJs);
  defineOperationDefinitionParameterBindingPropsJs(js, def);
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
  js.registerElement(def, 'OperationDefinition', 'base', 'Reference(OperationDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinition', 'system', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'type', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'instance', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'inputProfile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinition', 'outputProfile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineOrganizationRoleAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationRoleAvailableTime', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OrganizationRoleAvailableTime', 'availableStartTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OrganizationRoleAvailableTime', 'availableEndTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOrganizationRoleAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationRoleAvailableTime', nil, 'OrganizationRoleAvailableTime', js.FHIRFactoryJs);
  defineOrganizationRoleAvailableTimePropsJs(js, def);
end;


procedure defineOrganizationRoleNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationRoleNotAvailable', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OrganizationRoleNotAvailable', 'during', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOrganizationRoleNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationRoleNotAvailable', nil, 'OrganizationRoleNotAvailable', js.FHIRFactoryJs);
  defineOrganizationRoleNotAvailablePropsJs(js, def);
end;


procedure defineOrganizationRolePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OrganizationRole', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OrganizationRole', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationRole', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationRole', 'participatingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationRole', 'network', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'healthcareService', 'Reference(HealthcareService)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'availableTime', 'OrganizationRoleAvailableTime', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'notAvailable', 'OrganizationRoleNotAvailable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OrganizationRole', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrganizationRoleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationRole', nil, 'OrganizationRole', js.FHIRFactoryJs);
  defineOrganizationRolePropsJs(js, def);
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


procedure definePatientAnimalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientAnimal', 'species', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientAnimal', 'breed', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientAnimal', 'genderStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePatientAnimalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientAnimal', nil, 'PatientAnimal', js.FHIRFactoryJs);
  definePatientAnimalPropsJs(js, def);
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
  js.registerElement(def, 'PatientLink', 'other', 'Reference(Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Patient', 'animal', 'PatientAnimal', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient', 'communication', 'PatientCommunication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'generalPractitioner', 'Reference(Organization|Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'PaymentNotice', 'statusDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'PaymentReconciliationDetail', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'submitter', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'payee', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
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
  js.registerElement(def, 'PaymentReconciliation', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'request', 'Reference(ProcessRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'detail', 'PaymentReconciliationDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'total', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'PersonLink', 'target', 'Reference(Patient|Practitioner|RelatedPerson|Person)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'PlanDefinitionAction', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'documentation', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'triggerDefinition', 'TriggerDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'PlanDefinitionAction', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'groupingBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'selectionBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'requiredBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'precheckBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'cardinalityBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'definition', 'Reference(ActivityDefinition|PlanDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'transform', 'Reference(StructureMap)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'PlanDefinitionActionCondition', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'PlanDefinitionActionDynamicValue', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'PlanDefinition', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PlanDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'library', 'Reference(Library)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ProcedurePerformer', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer', 'actor', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Procedure', 'basedOn', 'Reference(CarePlan|ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'partOf', 'Reference(Procedure|Observation|MedicationAdministration)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Procedure', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Procedure', 'performedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Procedure', 'performedAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performedRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performer', 'ProcedurePerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'reasonReference', 'Reference(Condition|Observation|Procedure|DiagnosticReport|DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'report', 'Reference(DiagnosticReport)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'complication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'complicationDetail', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'followUp', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'focalDevice', 'ProcedureFocalDevice', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'usedReference', 'Reference(Device|Medication|Substance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'usedCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Procedure', nil, 'Procedure', js.FHIRFactoryJs);
  defineProcedurePropsJs(js, def);
end;


procedure defineProcessRequestItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessRequestItem', 'sequenceLinkId', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineProcessRequestItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessRequestItem', nil, 'ProcessRequestItem', js.FHIRFactoryJs);
  defineProcessRequestItemPropsJs(js, def);
end;


procedure defineProcessRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessRequest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest', 'action', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessRequest', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'nullify', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ProcessRequest', 'reference', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest', 'item', 'ProcessRequestItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcessRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessRequest', nil, 'ProcessRequest', js.FHIRFactoryJs);
  defineProcessRequestPropsJs(js, def);
end;


procedure defineProcessResponseProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessResponseProcessNote', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponseProcessNote', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineProcessResponseProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponseProcessNote', nil, 'ProcessResponseProcessNote', js.FHIRFactoryJs);
  defineProcessResponseProcessNotePropsJs(js, def);
end;


procedure defineProcessResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessResponse', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessResponse', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'processNote', 'ProcessResponseProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse', 'error', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse', 'communicationRequest', 'Reference(CommunicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcessResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponse', nil, 'ProcessResponse', js.FHIRFactoryJs);
  defineProcessResponsePropsJs(js, def);
end;


procedure defineProductPlanContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanContact', 'purpose', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanContact', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanContact', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanContact', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProductPlanContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanContact', nil, 'ProductPlanContact', js.FHIRFactoryJs);
  defineProductPlanContactPropsJs(js, def);
end;


procedure defineProductPlanCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanCoverage', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanCoverage', 'benefit', 'ProductPlanCoverageBenefit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanCoverage', nil, 'ProductPlanCoverage', js.FHIRFactoryJs);
  defineProductPlanCoveragePropsJs(js, def);
end;


procedure defineProductPlanCoverageBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanCoverageBenefit', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanCoverageBenefit', 'item', 'ProductPlanCoverageBenefitItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanCoverageBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanCoverageBenefit', nil, 'ProductPlanCoverageBenefit', js.FHIRFactoryJs);
  defineProductPlanCoverageBenefitPropsJs(js, def);
end;


procedure defineProductPlanCoverageBenefitItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanCoverageBenefitItem', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanCoverageBenefitItem', 'benefitValue', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProductPlanCoverageBenefitItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanCoverageBenefitItem', nil, 'ProductPlanCoverageBenefitItem', js.FHIRFactoryJs);
  defineProductPlanCoverageBenefitItemPropsJs(js, def);
end;


procedure defineProductPlanPlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlan', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlan', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProductPlanPlan', 'premium', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlan', 'category', 'ProductPlanPlanCategory', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanPlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlan', nil, 'ProductPlanPlan', js.FHIRFactoryJs);
  defineProductPlanPlanPropsJs(js, def);
end;


procedure defineProductPlanPlanCategoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlanCategory', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanCategory', 'benefit', 'ProductPlanPlanCategoryBenefit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanPlanCategoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlanCategory', nil, 'ProductPlanPlanCategory', js.FHIRFactoryJs);
  defineProductPlanPlanCategoryPropsJs(js, def);
end;


procedure defineProductPlanPlanCategoryBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlanCategoryBenefit', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanCategoryBenefit', 'cost', 'ProductPlanPlanCategoryBenefitCost', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanPlanCategoryBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlanCategoryBenefit', nil, 'ProductPlanPlanCategoryBenefit', js.FHIRFactoryJs);
  defineProductPlanPlanCategoryBenefitPropsJs(js, def);
end;


procedure defineProductPlanPlanCategoryBenefitCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlanCategoryBenefitCost', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanCategoryBenefitCost', 'applicability', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanPlanCategoryBenefitCost', 'value', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProductPlanPlanCategoryBenefitCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlanCategoryBenefitCost', nil, 'ProductPlanPlanCategoryBenefitCost', js.FHIRFactoryJs);
  defineProductPlanPlanCategoryBenefitCostPropsJs(js, def);
end;


procedure defineProductPlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProductPlan', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProductPlan', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProductPlan', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan', 'ownedBy', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan', 'administeredBy', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'coverageArea', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan', 'contact', 'ProductPlanContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'coverage', 'ProductPlanCoverage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'plan', 'ProductPlanPlan', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductPlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlan', nil, 'ProductPlan', js.FHIRFactoryJs);
  defineProductPlanPropsJs(js, def);
end;


procedure defineProvenanceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceAgent', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProvenanceAgent', 'whoIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'whoReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'onBehalfOfIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'onBehalfOfReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'ProvenanceEntity', 'whatIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity', 'whatReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'QuestionnaireItem', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'repeats', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'readOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'maxLength', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItem', 'options', 'Reference(ValueSet)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem', 'option', 'QuestionnaireItemOption', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'hasAnswer', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineQuestionnaireItemOptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemOption', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemOption', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemOption', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemOption', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemOption', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemOption', 'initialSelected', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineQuestionnaireItemOptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemOption', nil, 'QuestionnaireItemOption', js.FHIRFactoryJs);
  defineQuestionnaireItemOptionPropsJs(js, def);
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
  js.registerElement(def, 'Questionnaire', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'QuestionnaireResponseItem', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'QuestionnaireResponse', 'basedOn', 'Reference(CarePlan|ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse', 'partOf', 'Reference(Observation|Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse', 'questionnaire', 'Reference(Questionnaire)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'authored', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponse', 'author', 'Reference(Device|Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'source', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'item', 'QuestionnaireResponseItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponse', nil, 'QuestionnaireResponse', js.FHIRFactoryJs);
  defineQuestionnaireResponsePropsJs(js, def);
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
  js.registerElement(def, 'RequestGroupAction', 'participant', 'Reference(Patient|Person|Practitioner|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'RequestGroupActionCondition', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'RequestGroup', 'definition', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'replaces', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroup', 'author', 'Reference(Device|Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'reasonReference', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'ResearchStudy', 'condition', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'RiskAssessment', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'basedOn', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'parent', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskAssessment', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'condition', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'performer', 'Reference(Practitioner|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'basis', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'prediction', 'RiskAssessmentPrediction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'mitigation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRiskAssessmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessment', nil, 'RiskAssessment', js.FHIRFactoryJs);
  defineRiskAssessmentPropsJs(js, def);
end;


procedure defineSchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Schedule', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Schedule', 'serviceCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Schedule', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'actor', 'Reference(Patient|Practitioner|PractitionerRole|RelatedPerson|Device|HealthcareService|Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'SearchParameterComponent', 'definition', 'Reference(SearchParameter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'SearchParameter', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SearchParameter', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'derivedFrom', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'xpath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'xpathUsage', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'component', 'SearchParameterComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSearchParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameter', nil, 'SearchParameter', js.FHIRFactoryJs);
  defineSearchParameterPropsJs(js, def);
end;


procedure defineSequenceReferenceSeqPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceReferenceSeq', 'chromosome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'genomeBuild', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'referenceSeqId', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'referenceSeqPointer', 'Reference(Sequence)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'referenceSeqString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'strand', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'windowStart', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'windowEnd', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineSequenceReferenceSeqJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceReferenceSeq', nil, 'SequenceReferenceSeq', js.FHIRFactoryJs);
  defineSequenceReferenceSeqPropsJs(js, def);
end;


procedure defineSequenceVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceVariant', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceVariant', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceVariant', 'observedAllele', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant', 'referenceAllele', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant', 'cigar', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant', 'variantPointer', 'Reference(Observation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSequenceVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceVariant', nil, 'SequenceVariant', js.FHIRFactoryJs);
  defineSequenceVariantPropsJs(js, def);
end;


procedure defineSequenceQualityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceQuality', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceQuality', 'standardSequence', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceQuality', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceQuality', 'score', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality', 'truthTP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'queryTP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'truthFN', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'queryFP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'gtFP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'precision', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'recall', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'fScore', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'roc', 'SequenceQualityRoc', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSequenceQualityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceQuality', nil, 'SequenceQuality', js.FHIRFactoryJs);
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
  def := js.defineClass('SequenceQualityRoc', nil, 'SequenceQualityRoc', js.FHIRFactoryJs);
  defineSequenceQualityRocPropsJs(js, def);
end;


procedure defineSequenceRepositoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceRepository', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository', 'datasetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository', 'variantsetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository', 'readsetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSequenceRepositoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceRepository', nil, 'SequenceRepository', js.FHIRFactoryJs);
  defineSequenceRepositoryPropsJs(js, def);
end;


procedure defineSequenceStructureVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceStructureVariant', 'precision', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceStructureVariant', 'reportedaCGHRatio', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceStructureVariant', 'length', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceStructureVariant', 'outer', 'SequenceStructureVariantOuter', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceStructureVariant', 'inner', 'SequenceStructureVariantInner', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSequenceStructureVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceStructureVariant', nil, 'SequenceStructureVariant', js.FHIRFactoryJs);
  defineSequenceStructureVariantPropsJs(js, def);
end;


procedure defineSequenceStructureVariantOuterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceStructureVariantOuter', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceStructureVariantOuter', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineSequenceStructureVariantOuterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceStructureVariantOuter', nil, 'SequenceStructureVariantOuter', js.FHIRFactoryJs);
  defineSequenceStructureVariantOuterPropsJs(js, def);
end;


procedure defineSequenceStructureVariantInnerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceStructureVariantInner', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceStructureVariantInner', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineSequenceStructureVariantInnerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceStructureVariantInner', nil, 'SequenceStructureVariantInner', js.FHIRFactoryJs);
  defineSequenceStructureVariantInnerPropsJs(js, def);
end;


procedure defineSequencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Sequence', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Sequence', 'coordinateSystem', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Sequence', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'specimen', 'Reference(Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'performer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'referenceSeq', 'SequenceReferenceSeq', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'variant', 'SequenceVariant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence', 'observedSeq', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Sequence', 'quality', 'SequenceQuality', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence', 'readCoverage', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Sequence', 'repository', 'SequenceRepository', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence', 'pointer', 'Reference(Sequence)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence', 'structureVariant', 'SequenceStructureVariant', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSequenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Sequence', nil, 'Sequence', js.FHIRFactoryJs);
  defineSequencePropsJs(js, def);
end;


procedure defineServiceDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ServiceDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ServiceDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceDefinition', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceDefinition', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'trigger', 'TriggerDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'dataRequirement', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'operationDefinition', 'Reference(OperationDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineServiceDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ServiceDefinition', nil, 'ServiceDefinition', js.FHIRFactoryJs);
  defineServiceDefinitionPropsJs(js, def);
end;


procedure defineServiceRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ServiceRequest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'basedOn', 'Reference(CarePlan|ServiceRequest|MedicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'replaces', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'requisition', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ServiceRequest', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'orderDetail', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'subject', 'Reference(Patient|Group|Location|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceRequest', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ServiceRequest', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceRequest', 'requester', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|Patient|Device|RelatedPerson|HealthcareService|CareTeam)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'insurance', 'Reference(Coverage|ClaimResponse)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'Slot', 'serviceCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'SpecimenCollection', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Specimen', 'subject', 'Reference(Patient|Group|Device|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'receivedTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Specimen', 'parent', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'request', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'collection', 'SpecimenCollection', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'processing', 'SpecimenProcessing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'container', 'SpecimenContainer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Specimen', nil, 'Specimen', js.FHIRFactoryJs);
  defineSpecimenPropsJs(js, def);
end;


procedure defineSpecimenDefinitionSpecimenToLabPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'isDerived', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'preference', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerMaterial', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerCap', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerDescription', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerCapacity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerMinimumVolume', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerAdditive', 'SpecimenDefinitionSpecimenToLabContainerAdditive', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerPreparation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'requirement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'retentionTime', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'rejectionCriterion', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'handling', 'SpecimenDefinitionSpecimenToLabHandling', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenDefinitionSpecimenToLabJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionSpecimenToLab', nil, 'SpecimenDefinitionSpecimenToLab', js.FHIRFactoryJs);
  defineSpecimenDefinitionSpecimenToLabPropsJs(js, def);
end;


procedure defineSpecimenDefinitionSpecimenToLabContainerAdditivePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabContainerAdditive', 'additiveCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabContainerAdditive', 'additiveReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenDefinitionSpecimenToLabContainerAdditiveJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionSpecimenToLabContainerAdditive', nil, 'SpecimenDefinitionSpecimenToLabContainerAdditive', js.FHIRFactoryJs);
  defineSpecimenDefinitionSpecimenToLabContainerAdditivePropsJs(js, def);
end;


procedure defineSpecimenDefinitionSpecimenToLabHandlingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling', 'conditionSet', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling', 'tempRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling', 'maxDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling', 'lightExposure', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSpecimenDefinitionSpecimenToLabHandlingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionSpecimenToLabHandling', nil, 'SpecimenDefinitionSpecimenToLabHandling', js.FHIRFactoryJs);
  defineSpecimenDefinitionSpecimenToLabHandlingPropsJs(js, def);
end;


procedure defineSpecimenDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinition', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinition', 'typeCollected', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinition', 'patientPreparation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinition', 'timeAspect', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinition', 'collection', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinition', 'specimenToLab', 'SpecimenDefinitionSpecimenToLab', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'StructureDefinition', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'mapping', 'StructureDefinitionMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'abstract', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition', 'contextType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'baseDefinition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'StructureMapStructure', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'element', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'listMode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'variable', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'condition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'check', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'SubscriptionChannel', 'endpoint', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel', 'payload', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'Subscription', 'tag', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
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


procedure defineSubstanceReferenceInformationRelationshipPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'substanceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'substanceCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'interaction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'isDefining', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'amountQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'amountRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'amountString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'amountType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'amountText', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationRelationshipJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationRelationship', nil, 'SubstanceReferenceInformationRelationship', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationRelationshipPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'target', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'interaction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'organism', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'organismType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'SubstanceReferenceInformation', 'relationship', 'SubstanceReferenceInformationRelationship', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation', 'target', 'SubstanceReferenceInformationTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformation', nil, 'SubstanceReferenceInformation', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationPropsJs(js, def);
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
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'amount', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'SubstanceSpecificationProperty', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'name', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'parameters', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'substanceId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'substanceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'amount', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'SubstanceSpecificationStructure', 'referenceSource', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'structuralRepresentation', 'SubstanceSpecificationStructureStructuralRepresentation', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'nuclideId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'nuclideName', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'substitutionType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'nuclideHalfLife', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'amount', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight', 'amount', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSpecificationStructureIsotopeMolecularWeightJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureIsotopeMolecularWeight', nil, 'SubstanceSpecificationStructureIsotopeMolecularWeight', js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureIsotopeMolecularWeightPropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureStructuralRepresentationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureStructuralRepresentation', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureStructuralRepresentation', 'representation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructureStructuralRepresentation', 'attachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureStructuralRepresentationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureStructuralRepresentation', nil, 'SubstanceSpecificationStructureStructuralRepresentation', js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureStructuralRepresentationPropsJs(js, def);
end;


procedure defineSubstanceSpecificationSubstanceCodePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode', 'statusDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSpecificationSubstanceCodeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationSubstanceCode', nil, 'SubstanceSpecificationSubstanceCode', js.FHIRFactoryJs);
  defineSubstanceSpecificationSubstanceCodePropsJs(js, def);
end;


procedure defineSubstanceSpecificationSubstanceNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'language', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'domain', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'officialName', 'SubstanceSpecificationSubstanceNameOfficialName', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationSubstanceNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationSubstanceName', nil, 'SubstanceSpecificationSubstanceName', js.FHIRFactoryJs);
  defineSubstanceSpecificationSubstanceNamePropsJs(js, def);
end;


procedure defineSubstanceSpecificationSubstanceNameOfficialNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationSubstanceNameOfficialName', 'authority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceNameOfficialName', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceNameOfficialName', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineSubstanceSpecificationSubstanceNameOfficialNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationSubstanceNameOfficialName', nil, 'SubstanceSpecificationSubstanceNameOfficialName', js.FHIRFactoryJs);
  defineSubstanceSpecificationSubstanceNameOfficialNamePropsJs(js, def);
end;


procedure defineSubstanceSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecification', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecification', 'stoichiometric', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstanceSpecification', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'moiety', 'SubstanceSpecificationMoiety', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'property', 'SubstanceSpecificationProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'referenceInformation', 'Reference(SubstanceReferenceInformation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'structure', 'SubstanceSpecificationStructure', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'substanceCode', 'SubstanceSpecificationSubstanceCode', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'substanceName', 'SubstanceSpecificationSubstanceName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'molecularWeight', '@SubstanceSpecification.structure.isotope.molecularWeight', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'polymer', 'Reference(SubstancePolymer)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'SupplyDelivery', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'basedOn', 'Reference(SupplyRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery', 'partOf', 'Reference(SupplyDelivery|Contract)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyDelivery', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'suppliedItem', 'SupplyDeliverySuppliedItem', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyDelivery', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'supplier', 'Reference(Practitioner|PractitionerRole|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'SupplyRequest', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'SupplyRequest', 'requester', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'supplier', 'Reference(Organization|HealthcareService)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'deliverFrom', 'Reference(Organization|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'deliverTo', 'Reference(Organization|Location|Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'TaskRestriction', 'recipient', 'Reference(Patient|Practitioner|RelatedPerson|Group|Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'TaskInput', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'TaskOutput', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Task', 'instantiatesUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task', 'instantiatesReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
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
  js.registerElement(def, 'Task', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'executionPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Task', 'lastModified', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Task', 'requester', 'Reference(Device|Organization|Patient|Practitioner|PractitionerRole|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'performerType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task', 'owner', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam|HealthcareService|Patient|Device|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'reasonCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'reasonReference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineTerminologyCapabilitiesCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem', 'version', 'TerminologyCapabilitiesCodeSystemVersion', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'definition', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'profile', 'Reference(ExpansionProfile)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'textFilter', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesExpansion', nil, 'TerminologyCapabilitiesExpansion', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesExpansionPropsJs(js, def);
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
  js.registerElement(def, 'TestScriptMetadataCapability', 'capabilities', 'Reference(CapabilityStatement)', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineTestScriptRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRule', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptRule', 'param', 'TestScriptRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRule', nil, 'TestScriptRule', js.FHIRFactoryJs);
  defineTestScriptRulePropsJs(js, def);
end;


procedure defineTestScriptRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRuleParam', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptRuleParam', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRuleParam', nil, 'TestScriptRuleParam', js.FHIRFactoryJs);
  defineTestScriptRuleParamPropsJs(js, def);
end;


procedure defineTestScriptRulesetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRuleset', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptRuleset', 'rule', 'TestScriptRulesetRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptRulesetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRuleset', nil, 'TestScriptRuleset', js.FHIRFactoryJs);
  defineTestScriptRulesetPropsJs(js, def);
end;


procedure defineTestScriptRulesetRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRulesetRule', 'ruleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptRulesetRule', 'param', 'TestScriptRulesetRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptRulesetRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRulesetRule', nil, 'TestScriptRulesetRule', js.FHIRFactoryJs);
  defineTestScriptRulesetRulePropsJs(js, def);
end;


procedure defineTestScriptRulesetRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRulesetRuleParam', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptRulesetRuleParam', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptRulesetRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRulesetRuleParam', nil, 'TestScriptRulesetRuleParam', js.FHIRFactoryJs);
  defineTestScriptRulesetRuleParamPropsJs(js, def);
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
  js.registerElement(def, 'TestScriptSetupActionAssert', 'rule', 'TestScriptSetupActionAssertRule', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'ruleset', 'TestScriptSetupActionAssertRuleset', js.getFHIRObjectProp, js.setFHIRObjectProp);
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


procedure defineTestScriptSetupActionAssertRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRule', 'ruleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRule', 'param', 'TestScriptSetupActionAssertRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRule', nil, 'TestScriptSetupActionAssertRule', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulePropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleParam', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleParam', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionAssertRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRuleParam', nil, 'TestScriptSetupActionAssertRuleParam', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRuleParamPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulesetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleset', 'rulesetId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleset', 'rule', 'TestScriptSetupActionAssertRulesetRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRulesetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRuleset', nil, 'TestScriptSetupActionAssertRuleset', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulesetRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRule', 'ruleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRule', 'param', 'TestScriptSetupActionAssertRulesetRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRulesetRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRulesetRule', nil, 'TestScriptSetupActionAssertRulesetRule', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetRulePropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulesetRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRuleParam', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRuleParam', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionAssertRulesetRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRulesetRuleParam', nil, 'TestScriptSetupActionAssertRulesetRuleParam', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetRuleParamPropsJs(js, def);
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
  js.registerElement(def, 'TestScript', 'rule', 'TestScriptRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'ruleset', 'TestScriptRuleset', js.getFHIRArrayProp, js.setFHIRArrayProp);
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


procedure defineUserSessionStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'UserSessionStatus', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'UserSessionStatus', 'source', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineUserSessionStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UserSessionStatus', nil, 'UserSessionStatus', js.FHIRFactoryJs);
  defineUserSessionStatusPropsJs(js, def);
end;


procedure defineUserSessionContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'UserSessionContext', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'UserSessionContext', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UserSessionContext', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineUserSessionContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UserSessionContext', nil, 'UserSessionContext', js.FHIRFactoryJs);
  defineUserSessionContextPropsJs(js, def);
end;


procedure defineUserSessionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'UserSession', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UserSession', 'user', 'Reference(Device|Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UserSession', 'status', 'UserSessionStatus', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UserSession', 'workstation', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UserSession', 'focus', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'UserSession', 'created', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'UserSession', 'expires', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'UserSession', 'context', 'UserSessionContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineUserSessionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UserSession', nil, 'UserSession', js.FHIRFactoryJs);
  defineUserSessionPropsJs(js, def);
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
  js.registerElement(def, 'ValueSetComposeIncludeFilter', 'value', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'ValueSet', 'extensible', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
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
  js.registerElement(def, 'VerificationResultPrimarySource', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'validationProcess', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'validationStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'validationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'canPushUpdates', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'VerificationResultAttestation', 'source', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResultAttestation', 'sourceIdentityCertificate', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation', 'proxyIdentityCertificate', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
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
  js.registerElement(def, 'VerificationResultValidator', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultValidator', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultValidator', 'identityCertificate', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultValidator', 'dateValidated', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
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
  js.registerElement(def, 'VerificationResult', 'need', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResult', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResult', 'statusDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult', 'validationType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResult', 'validationProcess', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult', 'frequency', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult', 'lastPerformed', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult', 'nextScheduled', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult', 'failureAction', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
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


procedure defineVisionPrescriptionDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionDispense', 'product', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'eye', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'sphere', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'cylinder', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'axis', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'prism', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'base', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'add', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'power', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'backCurve', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'diameter', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'duration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'color', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'brand', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVisionPrescriptionDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescriptionDispense', nil, 'VisionPrescriptionDispense', js.FHIRFactoryJs);
  defineVisionPrescriptionDispensePropsJs(js, def);
end;


procedure defineVisionPrescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VisionPrescription', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VisionPrescription', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescription', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'dateWritten', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VisionPrescription', 'prescriber', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'dispense', 'VisionPrescriptionDispense', js.getFHIRArrayProp, js.setFHIRArrayProp);
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
  defineOccupationalDataUsualOccupationJs(js); 
  defineOccupationalDataPastOrPresentOccupationJs(js); 
  defineOccupationalDataPastOrPresentOccupationWorkScheduleJs(js); 
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

