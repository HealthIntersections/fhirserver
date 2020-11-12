unit fhir3_javascript;

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

{$I fhir.inc}
{$I fhir3.inc}

interface

// FHIR v3.0.1 generated 2018-06-12T19:15:59+10:00

uses
  fsl_javascript, fhir_javascript;

procedure registerFHIRTypes(js : TFHIRJavascript);

implementation

procedure defineElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Element3', 'id', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Element3', 'extension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineBackboneElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'BackboneElement3', 'modifierExtension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Resource3', 'id', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Resource3', 'meta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Resource3', 'implicitRules', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Resource3', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDomainResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'DomainResource3', 'text', 'Narrative', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DomainResource3', 'contained', 'Resource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DomainResource3', 'extension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DomainResource3', 'modifierExtension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ParametersParameter3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter3', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ParametersParameter3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ParametersParameter3', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter3', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter3', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter3', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ParametersParameter3', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter3', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter3', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParametersParameter3', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter3', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter3', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParametersParameter3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter3', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter3', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter3', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter3', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'valueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'resource', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter3', 'part', '@Parameters.parameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParametersParameter3', nil, js.FHIRFactoryJs);
  defineParametersParameterPropsJs(js, def);
end;

procedure defineParametersPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Parameters3', 'parameter', 'ParametersParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Parameters3', nil, js.FHIRFactoryJs);
  defineParametersPropsJs(js, def);
end;

procedure defineMetadataResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
end;

procedure defineExtensionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Extension3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension3', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Extension3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Extension3', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension3', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension3', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension3', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Extension3', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension3', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension3', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Extension3', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension3', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension3', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Extension3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension3', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension3', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension3', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension3', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension3', 'valueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExtensionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Extension3', nil, js.FHIRFactoryJs);
  defineExtensionPropsJs(js, def);
end;

procedure defineNarrativePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Narrative3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Narrative3', 'div', 'xhtml', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNarrativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Narrative3', nil, js.FHIRFactoryJs);
  defineNarrativePropsJs(js, def);
end;

procedure defineContributorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Contributor3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contributor3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contributor3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContributorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contributor3', nil, js.FHIRFactoryJs);
  defineContributorPropsJs(js, def);
end;

procedure defineAttachmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Attachment3', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment3', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment3', 'data', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Attachment3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment3', 'size', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment3', 'hash', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Attachment3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment3', 'creation', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineAttachmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Attachment3', nil, js.FHIRFactoryJs);
  defineAttachmentPropsJs(js, def);
end;

procedure defineDataRequirementCodeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementCodeFilter3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter3', 'valueSetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter3', 'valueSetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirementCodeFilter3', 'valueCoding', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataRequirementCodeFilter3', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataRequirementCodeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementCodeFilter3', nil, js.FHIRFactoryJs);
  defineDataRequirementCodeFilterPropsJs(js, def);
end;

procedure defineDataRequirementDateFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementDateFilter3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementDateFilter3', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DataRequirementDateFilter3', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirementDateFilter3', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDataRequirementDateFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementDateFilter3', nil, js.FHIRFactoryJs);
  defineDataRequirementDateFilterPropsJs(js, def);
end;

procedure defineDataRequirementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirement3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirement3', 'codeFilter', 'DataRequirementCodeFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataRequirement3', 'dateFilter', 'DataRequirementDateFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataRequirementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirement3', nil, js.FHIRFactoryJs);
  defineDataRequirementPropsJs(js, def);
end;

procedure defineDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Dosage3', 'sequence', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Dosage3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Dosage3', 'additionalInstruction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Dosage3', 'patientInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Dosage3', 'timing', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Dosage3', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'doseRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'maxDosePerPeriod', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'maxDosePerAdministration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'maxDosePerLifetime', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'rateRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage3', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Dosage3', nil, js.FHIRFactoryJs);
  defineDosagePropsJs(js, def);
end;

procedure defineIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Identifier3', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Identifier3', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Identifier3', 'assigner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Identifier3', nil, js.FHIRFactoryJs);
  defineIdentifierPropsJs(js, def);
end;

procedure defineCodingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Coding3', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding3', 'userSelected', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineCodingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coding3', nil, js.FHIRFactoryJs);
  defineCodingPropsJs(js, def);
end;

procedure defineSampledDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SampledData3', 'origin', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SampledData3', 'period', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData3', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData3', 'lowerLimit', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData3', 'upperLimit', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData3', 'dimensions', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SampledData3', 'data', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSampledDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SampledData3', nil, js.FHIRFactoryJs);
  defineSampledDataPropsJs(js, def);
end;

procedure defineRatioPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Ratio3', 'numerator', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Ratio3', 'denominator', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRatioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Ratio3', nil, js.FHIRFactoryJs);
  defineRatioPropsJs(js, def);
end;

procedure defineReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Reference3', 'reference', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Reference3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Reference3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Reference3', nil, js.FHIRFactoryJs);
  defineReferencePropsJs(js, def);
end;

procedure defineTriggerDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TriggerDefinition3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinition3', 'eventName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinition3', 'eventTimingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition3', 'eventTimingReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition3', 'eventTimingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TriggerDefinition3', 'eventTimingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TriggerDefinition3', 'eventData', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTriggerDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TriggerDefinition3', nil, js.FHIRFactoryJs);
  defineTriggerDefinitionPropsJs(js, def);
end;

procedure definePeriodPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Period3', 'start', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Period3', 'end', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure definePeriodJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Period3', nil, js.FHIRFactoryJs);
  definePeriodPropsJs(js, def);
end;

procedure defineQuantityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Quantity3', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Quantity3', 'comparator', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity3', 'unit', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity3', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineQuantityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Quantity3', nil, js.FHIRFactoryJs);
  defineQuantityPropsJs(js, def);
end;

procedure defineRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Range3', 'low', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Range3', 'high', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Range3', nil, js.FHIRFactoryJs);
  defineRangePropsJs(js, def);
end;

procedure defineRelatedArtifactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'RelatedArtifact3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact3', 'citation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact3', 'document', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedArtifact3', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRelatedArtifactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedArtifact3', nil, js.FHIRFactoryJs);
  defineRelatedArtifactPropsJs(js, def);
end;

procedure defineAnnotationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Annotation3', 'authorReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Annotation3', 'authorString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Annotation3', 'time', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Annotation3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAnnotationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Annotation3', nil, js.FHIRFactoryJs);
  defineAnnotationPropsJs(js, def);
end;

procedure defineContactDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactDetail3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactDetail3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContactDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactDetail3', nil, js.FHIRFactoryJs);
  defineContactDetailPropsJs(js, def);
end;

procedure defineUsageContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'UsageContext3', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext3', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext3', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineUsageContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UsageContext3', nil, js.FHIRFactoryJs);
  defineUsageContextPropsJs(js, def);
end;

procedure defineSignaturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Signature3', 'type', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Signature3', 'when', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Signature3', 'whoUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature3', 'whoReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature3', 'onBehalfOfUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature3', 'onBehalfOfReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature3', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature3', 'blob', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineSignatureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Signature3', nil, js.FHIRFactoryJs);
  defineSignaturePropsJs(js, def);
end;

procedure defineCodeableConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'CodeableConcept3', 'coding', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeableConcept3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeableConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeableConcept3', nil, js.FHIRFactoryJs);
  defineCodeableConceptPropsJs(js, def);
end;

procedure defineParameterDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ParameterDefinition3', 'name', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition3', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition3', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParameterDefinition3', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition3', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineParameterDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParameterDefinition3', nil, js.FHIRFactoryJs);
  defineParameterDefinitionPropsJs(js, def);
end;

procedure defineContactPointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactPoint3', 'system', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint3', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint3', 'rank', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ContactPoint3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContactPointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactPoint3', nil, js.FHIRFactoryJs);
  defineContactPointPropsJs(js, def);
end;

procedure defineHumanNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'HumanName3', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName3', 'family', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineHumanNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HumanName3', nil, js.FHIRFactoryJs);
  defineHumanNamePropsJs(js, def);
end;

procedure defineMetaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Meta3', 'versionId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Meta3', 'lastUpdated', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Meta3', 'security', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Meta3', 'tag', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMetaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Meta3', nil, js.FHIRFactoryJs);
  defineMetaPropsJs(js, def);
end;

procedure defineAddressPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Address3', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address3', 'city', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address3', 'district', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address3', 'state', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address3', 'postalCode', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address3', 'country', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAddressJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Address3', nil, js.FHIRFactoryJs);
  defineAddressPropsJs(js, def);
end;

procedure defineElementDefinitionSlicingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicing3', 'discriminator', 'ElementDefinitionSlicingDiscriminator', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinitionSlicing3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicing3', 'ordered', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionSlicing3', 'rules', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicing3', nil, js.FHIRFactoryJs);
  defineElementDefinitionSlicingPropsJs(js, def);
end;

procedure defineElementDefinitionSlicingDiscriminatorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicingDiscriminator3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicingDiscriminator3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingDiscriminatorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicingDiscriminator3', nil, js.FHIRFactoryJs);
  defineElementDefinitionSlicingDiscriminatorPropsJs(js, def);
end;

procedure defineElementDefinitionBasePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBase3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase3', 'min', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase3', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionBaseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBase3', nil, js.FHIRFactoryJs);
  defineElementDefinitionBasePropsJs(js, def);
end;

procedure defineElementDefinitionTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionType3', 'code', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionType3', 'profile', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionType3', 'targetProfile', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionType3', 'versioning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionTypeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionType3', nil, js.FHIRFactoryJs);
  defineElementDefinitionTypePropsJs(js, def);
end;

procedure defineElementDefinitionExamplePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionExample3', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample3', 'valueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineElementDefinitionExampleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionExample3', nil, js.FHIRFactoryJs);
  defineElementDefinitionExamplePropsJs(js, def);
end;

procedure defineElementDefinitionConstraintPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionConstraint3', 'key', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint3', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint3', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint3', 'human', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint3', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint3', 'xpath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint3', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionConstraintJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionConstraint3', nil, js.FHIRFactoryJs);
  defineElementDefinitionConstraintPropsJs(js, def);
end;

procedure defineElementDefinitionBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBinding3', 'strength', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding3', 'valueSetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding3', 'valueSetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineElementDefinitionBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBinding3', nil, js.FHIRFactoryJs);
  defineElementDefinitionBindingPropsJs(js, def);
end;

procedure defineElementDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionMapping3', 'identity', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping3', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping3', 'map', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionMapping3', nil, js.FHIRFactoryJs);
  defineElementDefinitionMappingPropsJs(js, def);
end;

procedure defineElementDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinition3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'sliceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition3', 'slicing', 'ElementDefinitionSlicing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'short', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'definition', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'comment', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'requirements', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'min', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'base', 'ElementDefinitionBase', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'contentReference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'type', 'ElementDefinitionType', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'defaultValueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'meaningWhenMissing', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'orderMeaning', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'fixedMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition3', 'patternBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition3', 'patternCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'patternDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'patternDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'patternDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition3', 'patternId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'patternInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'patternInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'patternMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'patternOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'patternPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'patternString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'patternTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'patternUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'patternUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'patternAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'patternMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'example', 'ElementDefinitionExample', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition3', 'minValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'minValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'minValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'minValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'minValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition3', 'minValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'minValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'minValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'minValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'maxValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'maxValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'maxValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition3', 'maxValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'maxValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition3', 'maxValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'maxValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'maxValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition3', 'maxValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'maxLength', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition3', 'constraint', 'ElementDefinitionConstraint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition3', 'mustSupport', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition3', 'isModifier', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition3', 'isSummary', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition3', 'binding', 'ElementDefinitionBinding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition3', 'mapping', 'ElementDefinitionMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineElementDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinition3', nil, js.FHIRFactoryJs);
  defineElementDefinitionPropsJs(js, def);
end;

procedure defineTimingRepeatPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TimingRepeat3', 'boundsDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat3', 'boundsRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat3', 'boundsPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat3', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat3', 'countMax', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat3', 'duration', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat3', 'durationMax', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat3', 'durationUnit', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat3', 'frequency', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat3', 'frequencyMax', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat3', 'period', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat3', 'periodMax', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat3', 'periodUnit', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat3', 'offset', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTimingRepeatJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TimingRepeat3', nil, js.FHIRFactoryJs);
  defineTimingRepeatPropsJs(js, def);
end;

procedure defineTimingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Timing3', 'repeat', 'TimingRepeat', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Timing3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTimingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Timing3', nil, js.FHIRFactoryJs);
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
  def := js.defineClass('Count3', nil, js.FHIRFactoryJs);
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
  def := js.defineClass('Money3', nil, js.FHIRFactoryJs);
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
  def := js.defineClass('Age3', nil, js.FHIRFactoryJs);
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
  def := js.defineClass('Distance3', nil, js.FHIRFactoryJs);
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
  def := js.defineClass('Duration3', nil, js.FHIRFactoryJs);
  defineDurationPropsJs(js, def);
end;

procedure defineAccountCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AccountCoverage3', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AccountCoverage3', 'priority', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineAccountCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AccountCoverage3', nil, js.FHIRFactoryJs);
  defineAccountCoveragePropsJs(js, def);
end;

procedure defineAccountGuarantorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AccountGuarantor3', 'party', 'Reference(Patient|RelatedPerson|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AccountGuarantor3', 'onHold', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'AccountGuarantor3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAccountGuarantorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AccountGuarantor3', nil, js.FHIRFactoryJs);
  defineAccountGuarantorPropsJs(js, def);
end;

procedure defineAccountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Account3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account3', 'subject', 'Reference(Patient|Device|Practitioner|Location|HealthcareService|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account3', 'active', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account3', 'balance', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account3', 'coverage', 'AccountCoverage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account3', 'owner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account3', 'guarantor', 'AccountGuarantor', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAccountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Account3', nil, js.FHIRFactoryJs);
  defineAccountPropsJs(js, def);
end;

procedure defineActivityDefinitionParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ActivityDefinitionParticipant3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionParticipant3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineActivityDefinitionParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinitionParticipant3', nil, js.FHIRFactoryJs);
  defineActivityDefinitionParticipantPropsJs(js, def);
end;

procedure defineActivityDefinitionDynamicValuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ActivityDefinitionDynamicValue3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue3', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue3', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineActivityDefinitionDynamicValueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinitionDynamicValue3', nil, js.FHIRFactoryJs);
  defineActivityDefinitionDynamicValuePropsJs(js, def);
end;

procedure defineActivityDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ActivityDefinition3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ActivityDefinition3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition3', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'library', 'Reference(Library)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition3', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition3', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition3', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition3', 'timingRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition3', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition3', 'participant', 'ActivityDefinitionParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'productReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition3', 'productCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition3', 'dosage', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition3', 'transform', 'Reference(StructureMap)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition3', 'dynamicValue', 'ActivityDefinitionDynamicValue', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineActivityDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinition3', nil, js.FHIRFactoryJs);
  defineActivityDefinitionPropsJs(js, def);
end;

procedure defineAdverseEventSuspectEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AdverseEventSuspectEntity3', 'instance', 'Reference(Substance|Medication|MedicationAdministration|MedicationStatement|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntity3', 'causality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEventSuspectEntity3', 'causalityAssessment', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntity3', 'causalityProductRelatedness', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEventSuspectEntity3', 'causalityMethod', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntity3', 'causalityAuthor', 'Reference(Practitioner|PractitionerRole)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntity3', 'causalityResult', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAdverseEventSuspectEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEventSuspectEntity3', nil, js.FHIRFactoryJs);
  defineAdverseEventSuspectEntityPropsJs(js, def);
end;

procedure defineAdverseEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AdverseEvent3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent3', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEvent3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent3', 'subject', 'Reference(Patient|ResearchSubject|Medication|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AdverseEvent3', 'reaction', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent3', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent3', 'seriousness', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent3', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent3', 'recorder', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent3', 'eventParticipant', 'Reference(Practitioner|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEvent3', 'suspectEntity', 'AdverseEventSuspectEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent3', 'subjectMedicalHistory', 'Reference(Condition|Observation|AllergyIntolerance|FamilyMemberHistory|Immunization|Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent3', 'referenceDocument', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent3', 'study', 'Reference(ResearchStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAdverseEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEvent3', nil, js.FHIRFactoryJs);
  defineAdverseEventPropsJs(js, def);
end;

procedure defineAllergyIntoleranceReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AllergyIntoleranceReaction3', 'substance', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction3', 'manifestation', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntoleranceReaction3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction3', 'onset', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntoleranceReaction3', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction3', 'exposureRoute', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntoleranceReaction3', nil, js.FHIRFactoryJs);
  defineAllergyIntoleranceReactionPropsJs(js, def);
end;

procedure defineAllergyIntolerancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AllergyIntolerance3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance3', 'clinicalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance3', 'verificationStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance3', 'criticality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance3', 'onsetDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance3', 'onsetAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance3', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance3', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance3', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance3', 'assertedDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance3', 'recorder', 'Reference(Practitioner|Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance3', 'asserter', 'Reference(Patient|RelatedPerson|Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance3', 'lastOccurrence', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance3', 'reaction', 'AllergyIntoleranceReaction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntolerance3', nil, js.FHIRFactoryJs);
  defineAllergyIntolerancePropsJs(js, def);
end;

procedure defineAppointmentParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AppointmentParticipant3', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentParticipant3', 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentParticipant3', 'required', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentParticipant3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAppointmentParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentParticipant3', nil, js.FHIRFactoryJs);
  defineAppointmentParticipantPropsJs(js, def);
end;

procedure defineAppointmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Appointment3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment3', 'serviceCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment3', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment3', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment3', 'appointmentType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment3', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment3', 'indication', 'Reference(Condition|Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment3', 'priority', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment3', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment3', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment3', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment3', 'minutesDuration', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Appointment3', 'slot', 'Reference(Slot)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment3', 'incomingReferral', 'Reference(ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment3', 'participant', 'AppointmentParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment3', 'requestedPeriod', 'Period', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAppointmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Appointment3', nil, js.FHIRFactoryJs);
  defineAppointmentPropsJs(js, def);
end;

procedure defineAppointmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AppointmentResponse3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse3', 'appointment', 'Reference(Appointment)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse3', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse3', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse3', 'participantType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse3', 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse3', 'participantStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentResponse3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAppointmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentResponse3', nil, js.FHIRFactoryJs);
  defineAppointmentResponsePropsJs(js, def);
end;

procedure defineAuditEventAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventAgent3', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventAgent3', 'reference', 'Reference(Practitioner|Organization|Device|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent3', 'userId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent3', 'altId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgent3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgent3', 'requestor', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'AuditEventAgent3', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent3', 'media', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent3', 'network', 'AuditEventAgentNetwork', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent3', 'purposeOfUse', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventAgent3', nil, js.FHIRFactoryJs);
  defineAuditEventAgentPropsJs(js, def);
end;

procedure defineAuditEventAgentNetworkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventAgentNetwork3', 'address', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgentNetwork3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAuditEventAgentNetworkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventAgentNetwork3', nil, js.FHIRFactoryJs);
  defineAuditEventAgentNetworkPropsJs(js, def);
end;

procedure defineAuditEventSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventSource3', 'site', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventSource3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventSource3', 'type', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventSource3', nil, js.FHIRFactoryJs);
  defineAuditEventSourcePropsJs(js, def);
end;

procedure defineAuditEventEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEntity3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity3', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity3', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity3', 'role', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity3', 'lifecycle', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity3', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventEntity3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntity3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntity3', 'query', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'AuditEventEntity3', 'detail', 'AuditEventEntityDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEntity3', nil, js.FHIRFactoryJs);
  defineAuditEventEntityPropsJs(js, def);
end;

procedure defineAuditEventEntityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEntityDetail3', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntityDetail3', 'value', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineAuditEventEntityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEntityDetail3', nil, js.FHIRFactoryJs);
  defineAuditEventEntityDetailPropsJs(js, def);
end;

procedure defineAuditEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AuditEvent3', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent3', 'subtype', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent3', 'action', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent3', 'recorded', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AuditEvent3', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent3', 'outcomeDesc', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent3', 'purposeOfEvent', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent3', 'agent', 'AuditEventAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent3', 'source', 'AuditEventSource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent3', 'entity', 'AuditEventEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEvent3', nil, js.FHIRFactoryJs);
  defineAuditEventPropsJs(js, def);
end;

procedure defineBasicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Basic3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Basic3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic3', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic3', 'created', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Basic3', 'author', 'Reference(Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBasicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Basic3', nil, js.FHIRFactoryJs);
  defineBasicPropsJs(js, def);
end;

procedure defineBinaryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Binary3', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Binary3', 'securityContext', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Binary3', 'content', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineBinaryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Binary3', nil, js.FHIRFactoryJs);
  defineBinaryPropsJs(js, def);
end;

procedure defineBodySitePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BodySite3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodySite3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'BodySite3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BodySite3', 'qualifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodySite3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BodySite3', 'image', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodySite3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBodySiteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BodySite3', nil, js.FHIRFactoryJs);
  defineBodySitePropsJs(js, def);
end;

procedure defineBundleLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleLink3', 'relation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleLink3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineBundleLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleLink3', nil, js.FHIRFactoryJs);
  defineBundleLinkPropsJs(js, def);
end;

procedure defineBundleEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntry3', 'link', '@Bundle.link', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BundleEntry3', 'fullUrl', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntry3', 'resource', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry3', 'search', 'BundleEntrySearch', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry3', 'request', 'BundleEntryRequest', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry3', 'response', 'BundleEntryResponse', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntry3', nil, js.FHIRFactoryJs);
  defineBundleEntryPropsJs(js, def);
end;

procedure defineBundleEntrySearchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntrySearch3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntrySearch3', 'score', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineBundleEntrySearchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntrySearch3', nil, js.FHIRFactoryJs);
  defineBundleEntrySearchPropsJs(js, def);
end;

procedure defineBundleEntryRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryRequest3', 'method', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest3', 'ifNoneMatch', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest3', 'ifModifiedSince', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryRequest3', 'ifMatch', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest3', 'ifNoneExist', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineBundleEntryRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryRequest3', nil, js.FHIRFactoryJs);
  defineBundleEntryRequestPropsJs(js, def);
end;

procedure defineBundleEntryResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryResponse3', 'status', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse3', 'location', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse3', 'etag', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse3', 'lastModified', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryResponse3', 'outcome', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleEntryResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryResponse3', nil, js.FHIRFactoryJs);
  defineBundleEntryResponsePropsJs(js, def);
end;

procedure defineBundlePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Bundle3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Bundle3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Bundle3', 'total', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Bundle3', 'link', 'BundleLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Bundle3', 'entry', 'BundleEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Bundle3', 'signature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Bundle3', nil, js.FHIRFactoryJs);
  defineBundlePropsJs(js, def);
end;

procedure defineCapabilityStatementSoftwarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementSoftware3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementSoftware3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementSoftware3', 'releaseDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineCapabilityStatementSoftwareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementSoftware3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementSoftwarePropsJs(js, def);
end;

procedure defineCapabilityStatementImplementationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementImplementation3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementImplementation3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementImplementationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementImplementation3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementImplementationPropsJs(js, def);
end;

procedure defineCapabilityStatementRestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRest3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRest3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRest3', 'security', 'CapabilityStatementRestSecurity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementRest3', 'resource', 'CapabilityStatementRestResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest3', 'interaction', 'CapabilityStatementRestInteraction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest3', 'searchParam', '@CapabilityStatement.rest.resource.searchParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest3', 'operation', 'CapabilityStatementRestOperation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRest3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestPropsJs(js, def);
end;

procedure defineCapabilityStatementRestSecurityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestSecurity3', 'cors', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity3', 'service', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity3', 'certificate', 'CapabilityStatementRestSecurityCertificate', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestSecurityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestSecurity3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestSecurityPropsJs(js, def);
end;

procedure defineCapabilityStatementRestSecurityCertificatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestSecurityCertificate3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestSecurityCertificate3', 'blob', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineCapabilityStatementRestSecurityCertificateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestSecurityCertificate3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestSecurityCertificatePropsJs(js, def);
end;

procedure defineCapabilityStatementRestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'interaction', 'CapabilityStatementRestResourceInteraction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'versioning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'readHistory', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'updateCreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'conditionalCreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'conditionalRead', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'conditionalUpdate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'conditionalDelete', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource3', 'searchParam', 'CapabilityStatementRestResourceSearchParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResource3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestResourcePropsJs(js, def);
end;

procedure defineCapabilityStatementRestResourceInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceInteraction3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceInteractionPropsJs(js, def);
end;

procedure defineCapabilityStatementRestResourceSearchParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam3', 'definition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceSearchParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceSearchParam3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceSearchParamPropsJs(js, def);
end;

procedure defineCapabilityStatementRestInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestInteraction3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestInteraction3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestInteraction3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestInteractionPropsJs(js, def);
end;

procedure defineCapabilityStatementRestOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestOperation3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestOperation3', 'definition', 'Reference(OperationDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCapabilityStatementRestOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestOperation3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementRestOperationPropsJs(js, def);
end;

procedure defineCapabilityStatementMessagingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessaging3', 'endpoint', 'CapabilityStatementMessagingEndpoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementMessaging3', 'reliableCache', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessaging3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessaging3', 'supportedMessage', 'CapabilityStatementMessagingSupportedMessage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementMessaging3', 'event', 'CapabilityStatementMessagingEvent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementMessagingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessaging3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementMessagingPropsJs(js, def);
end;

procedure defineCapabilityStatementMessagingEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint3', 'protocol', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint3', 'address', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementMessagingEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingEndpoint3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementMessagingEndpointPropsJs(js, def);
end;

procedure defineCapabilityStatementMessagingSupportedMessagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage3', 'definition', 'Reference(MessageDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCapabilityStatementMessagingSupportedMessageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingSupportedMessage3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementMessagingSupportedMessagePropsJs(js, def);
end;

procedure defineCapabilityStatementMessagingEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingEvent3', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementMessagingEvent3', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessagingEvent3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessagingEvent3', 'focus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessagingEvent3', 'request', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementMessagingEvent3', 'response', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementMessagingEvent3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementMessagingEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingEvent3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementMessagingEventPropsJs(js, def);
end;

procedure defineCapabilityStatementDocumentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementDocument3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument3', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCapabilityStatementDocumentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementDocument3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementDocumentPropsJs(js, def);
end;

procedure defineCapabilityStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CapabilityStatement3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatement3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CapabilityStatement3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'software', 'CapabilityStatementSoftware', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatement3', 'implementation', 'CapabilityStatementImplementation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatement3', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'acceptUnknown', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement3', 'profile', 'Reference(StructureDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement3', 'rest', 'CapabilityStatementRest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement3', 'messaging', 'CapabilityStatementMessaging', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement3', 'document', 'CapabilityStatementDocument', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatement3', nil, js.FHIRFactoryJs);
  defineCapabilityStatementPropsJs(js, def);
end;

procedure defineCarePlanActivityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivity3', 'outcomeCodeableConcept', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity3', 'outcomeReference', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity3', 'progress', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity3', 'reference', 'Reference(Appointment|CommunicationRequest|DeviceRequest|MedicationRequest|NutritionOrder|Task|ProcedureRequest|ReferralRequest|VisionPrescription|RequestGroup)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivity3', 'detail', 'CarePlanActivityDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCarePlanActivityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivity3', nil, js.FHIRFactoryJs);
  defineCarePlanActivityPropsJs(js, def);
end;

procedure defineCarePlanActivityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivityDetail3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'definition', 'Reference(PlanDefinition|ActivityDefinition|Questionnaire)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'goal', 'Reference(Goal)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'statusReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'prohibited', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'scheduledTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'scheduledPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'scheduledString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'performer', 'Reference(Practitioner|Organization|RelatedPerson|Patient|CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'productCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'productReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'dailyAmount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCarePlanActivityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivityDetail3', nil, js.FHIRFactoryJs);
  defineCarePlanActivityDetailPropsJs(js, def);
end;

procedure defineCarePlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CarePlan3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'definition', 'Reference(PlanDefinition|Questionnaire)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'replaces', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'partOf', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan3', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan3', 'author', 'Reference(Patient|Practitioner|RelatedPerson|Organization|CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'careTeam', 'Reference(CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'addresses', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'goal', 'Reference(Goal)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'activity', 'CarePlanActivity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCarePlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlan3', nil, js.FHIRFactoryJs);
  defineCarePlanPropsJs(js, def);
end;

procedure defineCareTeamParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CareTeamParticipant3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant3', 'member', 'Reference(Practitioner|RelatedPerson|Patient|Organization|CareTeam)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCareTeamParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CareTeamParticipant3', nil, js.FHIRFactoryJs);
  defineCareTeamParticipantPropsJs(js, def);
end;

procedure defineCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CareTeam3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CareTeam3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CareTeam3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam3', 'participant', 'CareTeamParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam3', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam3', 'managingOrganization', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CareTeam3', nil, js.FHIRFactoryJs);
  defineCareTeamPropsJs(js, def);
end;

procedure defineChargeItemParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemParticipant3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemParticipant3', 'actor', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineChargeItemParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemParticipant3', nil, js.FHIRFactoryJs);
  defineChargeItemParticipantPropsJs(js, def);
end;

procedure defineChargeItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ChargeItem3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItem3', 'partOf', 'Reference(ChargeItem)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem3', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'participant', 'ChargeItemParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem3', 'performingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'requestingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'bodysite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem3', 'factorOverride', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ChargeItem3', 'priceOverride', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'overrideReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItem3', 'enterer', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem3', 'enteredDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem3', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem3', 'service', 'Reference(DiagnosticReport|ImagingStudy|Immunization|MedicationAdministration|MedicationDispense|Observation|Procedure|SupplyDelivery)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem3', 'account', 'Reference(Account)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem3', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineChargeItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItem3', nil, js.FHIRFactoryJs);
  defineChargeItemPropsJs(js, def);
end;

procedure defineClaimRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimRelated3', 'claim', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimRelated3', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimRelated3', 'reference', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimRelated3', nil, js.FHIRFactoryJs);
  defineClaimRelatedPropsJs(js, def);
end;

procedure defineClaimPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimPayee3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee3', 'resourceType', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee3', 'party', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimPayee3', nil, js.FHIRFactoryJs);
  defineClaimPayeePropsJs(js, def);
end;

procedure defineClaimCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimCareTeam3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimCareTeam3', 'provider', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCareTeam3', 'responsible', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimCareTeam3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCareTeam3', 'qualification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimCareTeam3', nil, js.FHIRFactoryJs);
  defineClaimCareTeamPropsJs(js, def);
end;

procedure defineClaimInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimInformation3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimInformation3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation3', 'timingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimInformation3', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimInformation3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation3', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation3', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation3', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimInformation3', nil, js.FHIRFactoryJs);
  defineClaimInformationPropsJs(js, def);
end;

procedure defineClaimDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimDiagnosis3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimDiagnosis3', 'diagnosisCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis3', 'diagnosisReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis3', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimDiagnosis3', 'packageCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimDiagnosis3', nil, js.FHIRFactoryJs);
  defineClaimDiagnosisPropsJs(js, def);
end;

procedure defineClaimProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimProcedure3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimProcedure3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimProcedure3', 'procedureCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimProcedure3', 'procedureReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimProcedure3', nil, js.FHIRFactoryJs);
  defineClaimProcedurePropsJs(js, def);
end;

procedure defineClaimInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimInsurance3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimInsurance3', 'focal', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimInsurance3', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInsurance3', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimInsurance3', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimInsurance3', nil, js.FHIRFactoryJs);
  defineClaimInsurancePropsJs(js, def);
end;

procedure defineClaimAccidentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimAccident3', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimAccident3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimAccident3', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimAccident3', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimAccidentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimAccident3', nil, js.FHIRFactoryJs);
  defineClaimAccidentPropsJs(js, def);
end;

procedure defineClaimItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItem3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItem3', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem3', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem3', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimItem3', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'locationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItem3', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem3', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem3', 'subSite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem3', 'encounter', 'Reference(Encounter)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem3', 'detail', 'ClaimItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItem3', nil, js.FHIRFactoryJs);
  defineClaimItemPropsJs(js, def);
end;

procedure defineClaimItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetail3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetail3', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail3', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail3', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail3', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail3', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail3', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetail3', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail3', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail3', 'subDetail', 'ClaimItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetail3', nil, js.FHIRFactoryJs);
  defineClaimItemDetailPropsJs(js, def);
end;

procedure defineClaimItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail3', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetailSubDetail3', nil, js.FHIRFactoryJs);
  defineClaimItemDetailSubDetailPropsJs(js, def);
end;

procedure defineClaimPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Claim3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim3', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'billablePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Claim3', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'fundsReserve', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'related', 'ClaimRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim3', 'prescription', 'Reference(MedicationRequest|VisionPrescription)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'originalPrescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'payee', 'ClaimPayee', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'referral', 'Reference(ReferralRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'careTeam', 'ClaimCareTeam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim3', 'information', 'ClaimInformation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim3', 'diagnosis', 'ClaimDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim3', 'procedure', 'ClaimProcedure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim3', 'insurance', 'ClaimInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim3', 'accident', 'ClaimAccident', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'employmentImpacted', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'hospitalization', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim3', 'item', 'ClaimItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim3', 'total', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Claim3', nil, js.FHIRFactoryJs);
  defineClaimPropsJs(js, def);
end;

procedure defineClaimResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItem3', 'sequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItem3', 'adjudication', 'ClaimResponseItemAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItem3', 'detail', 'ClaimResponseItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItem3', nil, js.FHIRFactoryJs);
  defineClaimResponseItemPropsJs(js, def);
end;

procedure defineClaimResponseItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemAdjudication3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication3', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication3', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication3', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineClaimResponseItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemAdjudication3', nil, js.FHIRFactoryJs);
  defineClaimResponseItemAdjudicationPropsJs(js, def);
end;

procedure defineClaimResponseItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetail3', 'sequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetail3', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItemDetail3', 'subDetail', 'ClaimResponseItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetail3', nil, js.FHIRFactoryJs);
  defineClaimResponseItemDetailPropsJs(js, def);
end;

procedure defineClaimResponseItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail3', 'sequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail3', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailSubDetail3', nil, js.FHIRFactoryJs);
  defineClaimResponseItemDetailSubDetailPropsJs(js, def);
end;

procedure defineClaimResponseAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItem3', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem3', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem3', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem3', 'fee', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem3', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem3', 'detail', 'ClaimResponseAddItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItem3', nil, js.FHIRFactoryJs);
  defineClaimResponseAddItemPropsJs(js, def);
end;

procedure defineClaimResponseAddItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemDetail3', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail3', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail3', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail3', 'fee', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail3', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemDetail3', nil, js.FHIRFactoryJs);
  defineClaimResponseAddItemDetailPropsJs(js, def);
end;

procedure defineClaimResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseError3', 'sequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError3', 'detailSequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError3', 'subdetailSequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseError3', nil, js.FHIRFactoryJs);
  defineClaimResponseErrorPropsJs(js, def);
end;

procedure defineClaimResponsePaymentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponsePayment3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment3', 'adjustment', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment3', 'adjustmentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment3', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponsePayment3', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponsePaymentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponsePayment3', nil, js.FHIRFactoryJs);
  defineClaimResponsePaymentPropsJs(js, def);
end;

procedure defineClaimResponseProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseProcessNote3', 'number', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseProcessNote3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseProcessNote3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseProcessNote3', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseProcessNote3', nil, js.FHIRFactoryJs);
  defineClaimResponseProcessNotePropsJs(js, def);
end;

procedure defineClaimResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseInsurance3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseInsurance3', 'focal', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimResponseInsurance3', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseInsurance3', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseInsurance3', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseInsurance3', nil, js.FHIRFactoryJs);
  defineClaimResponseInsurancePropsJs(js, def);
end;

procedure defineClaimResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClaimResponse3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponse3', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'request', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse3', 'payeeType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'item', 'ClaimResponseItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse3', 'addItem', 'ClaimResponseAddItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse3', 'error', 'ClaimResponseError', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse3', 'totalCost', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'unallocDeductable', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'totalBenefit', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'payment', 'ClaimResponsePayment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'reserved', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse3', 'processNote', 'ClaimResponseProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse3', 'communicationRequest', 'Reference(CommunicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse3', 'insurance', 'ClaimResponseInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponse3', nil, js.FHIRFactoryJs);
  defineClaimResponsePropsJs(js, def);
end;

procedure defineClinicalImpressionInvestigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionInvestigation3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionInvestigation3', 'item', 'Reference(Observation|QuestionnaireResponse|FamilyMemberHistory|DiagnosticReport|RiskAssessment|ImagingStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClinicalImpressionInvestigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionInvestigation3', nil, js.FHIRFactoryJs);
  defineClinicalImpressionInvestigationPropsJs(js, def);
end;

procedure defineClinicalImpressionFindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionFinding3', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding3', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding3', 'basis', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineClinicalImpressionFindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionFinding3', nil, js.FHIRFactoryJs);
  defineClinicalImpressionFindingPropsJs(js, def);
end;

procedure defineClinicalImpressionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClinicalImpression3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression3', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression3', 'assessor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression3', 'previous', 'Reference(ClinicalImpression)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression3', 'problem', 'Reference(Condition|AllergyIntolerance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression3', 'investigation', 'ClinicalImpressionInvestigation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression3', 'summary', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression3', 'finding', 'ClinicalImpressionFinding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression3', 'prognosisCodeableConcept', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression3', 'prognosisReference', 'Reference(RiskAssessment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression3', 'action', 'Reference(ReferralRequest|ProcedureRequest|Procedure|MedicationRequest|Appointment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClinicalImpressionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpression3', nil, js.FHIRFactoryJs);
  defineClinicalImpressionPropsJs(js, def);
end;

procedure defineCodeSystemFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemFilter3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemFilter3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemFilter3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemFilter3', nil, js.FHIRFactoryJs);
  defineCodeSystemFilterPropsJs(js, def);
end;

procedure defineCodeSystemPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemProperty3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty3', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemProperty3', nil, js.FHIRFactoryJs);
  defineCodeSystemPropertyPropsJs(js, def);
end;

procedure defineCodeSystemConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConcept3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept3', 'definition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept3', 'designation', 'CodeSystemConceptDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystemConcept3', 'property', 'CodeSystemConceptProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystemConcept3', 'concept', '@CodeSystem.concept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCodeSystemConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConcept3', nil, js.FHIRFactoryJs);
  defineCodeSystemConceptPropsJs(js, def);
end;

procedure defineCodeSystemConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConceptDesignation3', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptDesignation3', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CodeSystemConceptDesignation3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConceptDesignation3', nil, js.FHIRFactoryJs);
  defineCodeSystemConceptDesignationPropsJs(js, def);
end;

procedure defineCodeSystemConceptPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConceptProperty3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty3', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty3', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CodeSystemConceptProperty3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty3', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'CodeSystemConceptProperty3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystemConceptProperty3', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineCodeSystemConceptPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConceptProperty3', nil, js.FHIRFactoryJs);
  defineCodeSystemConceptPropertyPropsJs(js, def);
end;

procedure defineCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CodeSystem3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CodeSystem3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CodeSystem3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'caseSensitive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem3', 'valueSet', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'hierarchyMeaning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'compositional', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem3', 'versionNeeded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem3', 'content', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'count', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem3', 'filter', 'CodeSystemFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem3', 'property', 'CodeSystemProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem3', 'concept', 'CodeSystemConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystem3', nil, js.FHIRFactoryJs);
  defineCodeSystemPropsJs(js, def);
end;

procedure defineCommunicationPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationPayload3', 'contentString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationPayload3', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationPayload3', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationPayload3', nil, js.FHIRFactoryJs);
  defineCommunicationPayloadPropsJs(js, def);
end;

procedure defineCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Communication3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'definition', 'Reference(PlanDefinition|ActivityDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'partOf', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Communication3', 'notDone', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Communication3', 'notDoneReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'medium', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication3', 'recipient', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson|Group)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'topic', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication3', 'sent', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication3', 'received', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication3', 'sender', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'reasonReference', 'Reference(Condition|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'payload', 'CommunicationPayload', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Communication3', nil, js.FHIRFactoryJs);
  defineCommunicationPropsJs(js, def);
end;

procedure defineCommunicationRequestPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationRequestPayload3', 'contentString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequestPayload3', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequestPayload3', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationRequestPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequestPayload3', nil, js.FHIRFactoryJs);
  defineCommunicationRequestPayloadPropsJs(js, def);
end;

procedure defineCommunicationRequestRequesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationRequestRequester3', 'agent', 'Reference(Practitioner|Organization|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequestRequester3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationRequestRequesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequestRequester3', nil, js.FHIRFactoryJs);
  defineCommunicationRequestRequesterPropsJs(js, def);
end;

procedure defineCommunicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CommunicationRequest3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest3', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest3', 'replaces', 'Reference(CommunicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest3', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest3', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest3', 'medium', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest3', 'recipient', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson|Group|CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest3', 'topic', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest3', 'payload', 'CommunicationRequestPayload', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest3', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest3', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest3', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest3', 'sender', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest3', 'requester', 'CommunicationRequestRequester', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest3', 'reasonReference', 'Reference(Condition|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCommunicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequest3', nil, js.FHIRFactoryJs);
  defineCommunicationRequestPropsJs(js, def);
end;

procedure defineCompartmentDefinitionResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompartmentDefinitionResource3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinitionResource3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCompartmentDefinitionResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompartmentDefinitionResource3', nil, js.FHIRFactoryJs);
  defineCompartmentDefinitionResourcePropsJs(js, def);
end;

procedure defineCompartmentDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CompartmentDefinition3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompartmentDefinition3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition3', 'search', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition3', 'resource', 'CompartmentDefinitionResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompartmentDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompartmentDefinition3', nil, js.FHIRFactoryJs);
  defineCompartmentDefinitionPropsJs(js, def);
end;

procedure defineCompositionAttesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionAttester3', 'time', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompositionAttester3', 'party', 'Reference(Patient|Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCompositionAttesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionAttester3', nil, js.FHIRFactoryJs);
  defineCompositionAttesterPropsJs(js, def);
end;

procedure defineCompositionRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionRelatesTo3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionRelatesTo3', 'targetIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionRelatesTo3', 'targetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCompositionRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionRelatesTo3', nil, js.FHIRFactoryJs);
  defineCompositionRelatesToPropsJs(js, def);
end;

procedure defineCompositionEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionEvent3', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionEvent3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionEvent3', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionEvent3', nil, js.FHIRFactoryJs);
  defineCompositionEventPropsJs(js, def);
end;

procedure defineCompositionSectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionSection3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionSection3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection3', 'text', 'Narrative', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionSection3', 'orderedBy', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection3', 'entry', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionSection3', 'emptyReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection3', 'section', '@Composition.section', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionSectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionSection3', nil, js.FHIRFactoryJs);
  defineCompositionSectionPropsJs(js, def);
end;

procedure defineCompositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Composition3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition3', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition3', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition3', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Composition3', 'author', 'Reference(Practitioner|Device|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition3', 'confidentiality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition3', 'attester', 'CompositionAttester', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition3', 'custodian', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition3', 'relatesTo', 'CompositionRelatesTo', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition3', 'event', 'CompositionEvent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition3', 'section', 'CompositionSection', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Composition3', nil, js.FHIRFactoryJs);
  defineCompositionPropsJs(js, def);
end;

procedure defineConceptMapGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroup3', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup3', 'sourceVersion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup3', 'target', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup3', 'targetVersion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup3', 'element', 'ConceptMapGroupElement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapGroup3', 'unmapped', 'ConceptMapGroupUnmapped', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConceptMapGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroup3', nil, js.FHIRFactoryJs);
  defineConceptMapGroupPropsJs(js, def);
end;

procedure defineConceptMapGroupElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElement3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElement3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElement3', 'target', 'ConceptMapGroupElementTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapGroupElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElement3', nil, js.FHIRFactoryJs);
  defineConceptMapGroupElementPropsJs(js, def);
end;

procedure defineConceptMapGroupElementTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElementTarget3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget3', 'equivalence', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget3', 'dependsOn', 'ConceptMapGroupElementTargetDependsOn', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget3', 'product', '@ConceptMap.group.element.target.dependsOn', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapGroupElementTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElementTarget3', nil, js.FHIRFactoryJs);
  defineConceptMapGroupElementTargetPropsJs(js, def);
end;

procedure defineConceptMapGroupElementTargetDependsOnPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn3', 'property', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn3', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn3', 'code', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConceptMapGroupElementTargetDependsOnJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElementTargetDependsOn3', nil, js.FHIRFactoryJs);
  defineConceptMapGroupElementTargetDependsOnPropsJs(js, def);
end;

procedure defineConceptMapGroupUnmappedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupUnmapped3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConceptMapGroupUnmappedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupUnmapped3', nil, js.FHIRFactoryJs);
  defineConceptMapGroupUnmappedPropsJs(js, def);
end;

procedure defineConceptMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ConceptMap3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConceptMap3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ConceptMap3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'sourceUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'sourceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap3', 'targetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap3', 'targetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap3', 'group', 'ConceptMapGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMap3', nil, js.FHIRFactoryJs);
  defineConceptMapPropsJs(js, def);
end;

procedure defineConditionStagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionStage3', 'summary', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConditionStage3', 'assessment', 'Reference(ClinicalImpression|DiagnosticReport|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionStageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionStage3', nil, js.FHIRFactoryJs);
  defineConditionStagePropsJs(js, def);
end;

procedure defineConditionEvidencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionEvidence3', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConditionEvidence3', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionEvidenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionEvidence3', nil, js.FHIRFactoryJs);
  defineConditionEvidencePropsJs(js, def);
end;

procedure defineConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Condition3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition3', 'clinicalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition3', 'verificationStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition3', 'severity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'onsetDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition3', 'onsetAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition3', 'abatementDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition3', 'abatementAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'abatementBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Condition3', 'abatementPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'abatementRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'abatementString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition3', 'assertedDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition3', 'asserter', 'Reference(Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'stage', 'ConditionStage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition3', 'evidence', 'ConditionEvidence', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Condition3', nil, js.FHIRFactoryJs);
  defineConditionPropsJs(js, def);
end;

procedure defineConsentActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentActor3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentActor3', 'reference', 'Reference(Device|Group|CareTeam|Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentActor3', nil, js.FHIRFactoryJs);
  defineConsentActorPropsJs(js, def);
end;

procedure defineConsentPolicyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentPolicy3', 'authority', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentPolicy3', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConsentPolicyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentPolicy3', nil, js.FHIRFactoryJs);
  defineConsentPolicyPropsJs(js, def);
end;

procedure defineConsentDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentData3', 'meaning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentData3', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentData3', nil, js.FHIRFactoryJs);
  defineConsentDataPropsJs(js, def);
end;

procedure defineConsentExceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentExcept3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentExcept3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentExcept3', 'actor', 'ConsentExceptActor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentExcept3', 'action', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentExcept3', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentExcept3', 'purpose', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentExcept3', 'class', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentExcept3', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentExcept3', 'dataPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentExcept3', 'data', 'ConsentExceptData', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConsentExceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentExcept3', nil, js.FHIRFactoryJs);
  defineConsentExceptPropsJs(js, def);
end;

procedure defineConsentExceptActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentExceptActor3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentExceptActor3', 'reference', 'Reference(Device|Group|CareTeam|Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentExceptActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentExceptActor3', nil, js.FHIRFactoryJs);
  defineConsentExceptActorPropsJs(js, def);
end;

procedure defineConsentExceptDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentExceptData3', 'meaning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentExceptData3', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentExceptDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentExceptData3', nil, js.FHIRFactoryJs);
  defineConsentExceptDataPropsJs(js, def);
end;

procedure defineConsentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Consent3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Consent3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent3', 'dateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Consent3', 'consentingParty', 'Reference(Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent3', 'actor', 'ConsentActor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent3', 'action', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent3', 'organization', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent3', 'sourceAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent3', 'sourceIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent3', 'sourceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent3', 'policy', 'ConsentPolicy', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent3', 'policyRule', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Consent3', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent3', 'purpose', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent3', 'dataPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent3', 'data', 'ConsentData', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent3', 'except', 'ConsentExcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConsentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Consent3', nil, js.FHIRFactoryJs);
  defineConsentPropsJs(js, def);
end;

procedure defineContractAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractAgent3', 'actor', 'Reference(Contract|Device|Group|Location|Organization|Patient|Practitioner|RelatedPerson|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractAgent3', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractAgent3', nil, js.FHIRFactoryJs);
  defineContractAgentPropsJs(js, def);
end;

procedure defineContractSignerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractSigner3', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner3', 'party', 'Reference(Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner3', 'signature', 'Signature', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractSignerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractSigner3', nil, js.FHIRFactoryJs);
  defineContractSignerPropsJs(js, def);
end;

procedure defineContractValuedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractValuedItem3', 'entityCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem3', 'entityReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem3', 'effectiveTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractValuedItem3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem3', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem3', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractValuedItem3', 'points', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractValuedItem3', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractValuedItem3', nil, js.FHIRFactoryJs);
  defineContractValuedItemPropsJs(js, def);
end;

procedure defineContractTermPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTerm3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm3', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTerm3', 'applies', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm3', 'subType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm3', 'topic', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm3', 'action', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm3', 'actionReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm3', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm3', 'agent', 'ContractTermAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTerm3', 'valuedItem', 'ContractTermValuedItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm3', 'group', '@Contract.term', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTerm3', nil, js.FHIRFactoryJs);
  defineContractTermPropsJs(js, def);
end;

procedure defineContractTermAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAgent3', 'actor', 'Reference(Contract|Device|Group|Location|Organization|Patient|Practitioner|RelatedPerson|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAgent3', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAgent3', nil, js.FHIRFactoryJs);
  defineContractTermAgentPropsJs(js, def);
end;

procedure defineContractTermValuedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermValuedItem3', 'entityCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem3', 'entityReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem3', 'effectiveTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermValuedItem3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem3', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem3', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermValuedItem3', 'points', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermValuedItem3', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermValuedItem3', nil, js.FHIRFactoryJs);
  defineContractTermValuedItemPropsJs(js, def);
end;

procedure defineContractFriendlyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractFriendly3', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractFriendly3', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractFriendlyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractFriendly3', nil, js.FHIRFactoryJs);
  defineContractFriendlyPropsJs(js, def);
end;

procedure defineContractLegalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractLegal3', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractLegal3', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractLegalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractLegal3', nil, js.FHIRFactoryJs);
  defineContractLegalPropsJs(js, def);
end;

procedure defineContractRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractRule3', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractRule3', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractRule3', nil, js.FHIRFactoryJs);
  defineContractRulePropsJs(js, def);
end;

procedure defineContractPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Contract3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract3', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Contract3', 'applies', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract3', 'subject', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'topic', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'authority', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'domain', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract3', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'action', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'actionReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'decisionType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract3', 'contentDerivative', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract3', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'agent', 'ContractAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'signer', 'ContractSigner', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'valuedItem', 'ContractValuedItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'term', 'ContractTerm', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'bindingAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract3', 'bindingReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract3', 'friendly', 'ContractFriendly', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'legal', 'ContractLegal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract3', 'rule', 'ContractRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contract3', nil, js.FHIRFactoryJs);
  defineContractPropsJs(js, def);
end;

procedure defineCoverageGroupingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageGrouping3', 'group', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'groupDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'subGroup', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'subGroupDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'plan', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'planDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'subPlan', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'subPlanDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'class', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'classDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'subClass', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping3', 'subClassDisplay', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCoverageGroupingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageGrouping3', nil, js.FHIRFactoryJs);
  defineCoverageGroupingPropsJs(js, def);
end;

procedure defineCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Coverage3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage3', 'policyHolder', 'Reference(Patient|RelatedPerson|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage3', 'subscriber', 'Reference(Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage3', 'subscriberId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage3', 'beneficiary', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage3', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage3', 'payor', 'Reference(Organization|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage3', 'grouping', 'CoverageGrouping', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage3', 'dependent', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage3', 'sequence', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage3', 'order', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Coverage3', 'network', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage3', 'contract', 'Reference(Contract)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coverage3', nil, js.FHIRFactoryJs);
  defineCoveragePropsJs(js, def);
end;

procedure defineDataElementMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DataElementMapping3', 'identity', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElementMapping3', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElementMapping3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElementMapping3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDataElementMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataElementMapping3', nil, js.FHIRFactoryJs);
  defineDataElementMappingPropsJs(js, def);
end;

procedure defineDataElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'DataElement3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataElement3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'DataElement3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DataElement3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataElement3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataElement3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataElement3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement3', 'stringency', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement3', 'mapping', 'DataElementMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataElement3', 'element', 'ElementDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataElement3', nil, js.FHIRFactoryJs);
  defineDataElementPropsJs(js, def);
end;

procedure defineDetectedIssueMitigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DetectedIssueMitigation3', 'action', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssueMitigation3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssueMitigation3', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDetectedIssueMitigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssueMitigation3', nil, js.FHIRFactoryJs);
  defineDetectedIssueMitigationPropsJs(js, def);
end;

procedure defineDetectedIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DetectedIssue3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue3', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssue3', 'author', 'Reference(Practitioner|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue3', 'implicated', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue3', 'detail', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue3', 'reference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue3', 'mitigation', 'DetectedIssueMitigation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDetectedIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssue3', nil, js.FHIRFactoryJs);
  defineDetectedIssuePropsJs(js, def);
end;

procedure defineDeviceUdiPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceUdi3', 'deviceIdentifier', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi3', 'jurisdiction', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi3', 'carrierHRF', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi3', 'carrierAIDC', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'DeviceUdi3', 'issuer', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi3', 'entryType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceUdiJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUdi3', nil, js.FHIRFactoryJs);
  defineDeviceUdiPropsJs(js, def);
end;

procedure defineDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Device3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device3', 'udi', 'DeviceUdi', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device3', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device3', 'manufacturer', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device3', 'manufactureDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device3', 'expirationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device3', 'model', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device3', 'owner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device3', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device3', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device3', 'safety', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Device3', nil, js.FHIRFactoryJs);
  defineDevicePropsJs(js, def);
end;

procedure defineDeviceComponentProductionSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceComponentProductionSpecification3', 'specType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProductionSpecification3', 'componentId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProductionSpecification3', 'productionSpec', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceComponentProductionSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponentProductionSpecification3', nil, js.FHIRFactoryJs);
  defineDeviceComponentProductionSpecificationPropsJs(js, def);
end;

procedure defineDeviceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceComponent3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent3', 'lastSystemChange', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceComponent3', 'source', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent3', 'parent', 'Reference(DeviceComponent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent3', 'operationalStatus', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent3', 'parameterGroup', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent3', 'measurementPrinciple', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceComponent3', 'productionSpecification', 'DeviceComponentProductionSpecification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent3', 'languageCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDeviceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponent3', nil, js.FHIRFactoryJs);
  defineDeviceComponentPropsJs(js, def);
end;

procedure defineDeviceMetricCalibrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceMetricCalibration3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration3', 'state', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration3', 'time', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineDeviceMetricCalibrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetricCalibration3', nil, js.FHIRFactoryJs);
  defineDeviceMetricCalibrationPropsJs(js, def);
end;

procedure defineDeviceMetricPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceMetric3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric3', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric3', 'source', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric3', 'parent', 'Reference(DeviceComponent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric3', 'operationalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric3', 'color', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric3', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric3', 'measurementPeriod', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric3', 'calibration', 'DeviceMetricCalibration', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceMetricJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetric3', nil, js.FHIRFactoryJs);
  defineDeviceMetricPropsJs(js, def);
end;

procedure defineDeviceRequestRequesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceRequestRequester3', 'agent', 'Reference(Device|Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestRequester3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDeviceRequestRequesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceRequestRequester3', nil, js.FHIRFactoryJs);
  defineDeviceRequestRequesterPropsJs(js, def);
end;

procedure defineDeviceRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceRequest3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest3', 'definition', 'Reference(ActivityDefinition|PlanDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest3', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest3', 'priorRequest', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest3', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest3', 'intent', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest3', 'codeReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'codeCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'subject', 'Reference(Patient|Group|Location|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest3', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest3', 'requester', 'DeviceRequestRequester', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'performer', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson|HealthcareService)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest3', 'reasonReference', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest3', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest3', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceRequest3', nil, js.FHIRFactoryJs);
  defineDeviceRequestPropsJs(js, def);
end;

procedure defineDeviceUseStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceUseStatement3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUseStatement3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement3', 'whenUsed', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement3', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement3', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement3', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement3', 'recordedOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement3', 'source', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement3', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement3', 'indication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement3', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceUseStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUseStatement3', nil, js.FHIRFactoryJs);
  defineDeviceUseStatementPropsJs(js, def);
end;

procedure defineDiagnosticReportPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticReportPerformer3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReportPerformer3', 'actor', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDiagnosticReportPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReportPerformer3', nil, js.FHIRFactoryJs);
  defineDiagnosticReportPerformerPropsJs(js, def);
end;

procedure defineDiagnosticReportImagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticReportImage3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReportImage3', 'link', 'Reference(Media)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDiagnosticReportImageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReportImage3', nil, js.FHIRFactoryJs);
  defineDiagnosticReportImagePropsJs(js, def);
end;

procedure defineDiagnosticReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DiagnosticReport3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport3', 'basedOn', 'Reference(CarePlan|ImmunizationRecommendation|MedicationRequest|NutritionOrder|ProcedureRequest|ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport3', 'subject', 'Reference(Patient|Group|Device|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport3', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport3', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport3', 'performer', 'DiagnosticReportPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport3', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport3', 'result', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport3', 'imagingStudy', 'Reference(ImagingStudy|ImagingManifest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport3', 'image', 'DiagnosticReportImage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport3', 'conclusion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport3', 'codedDiagnosis', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport3', 'presentedForm', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDiagnosticReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReport3', nil, js.FHIRFactoryJs);
  defineDiagnosticReportPropsJs(js, def);
end;

procedure defineDocumentManifestContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestContent3', 'pAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestContent3', 'pReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentManifestContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestContent3', nil, js.FHIRFactoryJs);
  defineDocumentManifestContentPropsJs(js, def);
end;

procedure defineDocumentManifestRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestRelated3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestRelated3', 'ref', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentManifestRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestRelated3', nil, js.FHIRFactoryJs);
  defineDocumentManifestRelatedPropsJs(js, def);
end;

procedure defineDocumentManifestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentManifest3', 'masterIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest3', 'subject', 'Reference(Patient|Practitioner|Group|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentManifest3', 'author', 'Reference(Practitioner|Organization|Device|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest3', 'recipient', 'Reference(Patient|Practitioner|RelatedPerson|Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest3', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest3', 'content', 'DocumentManifestContent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest3', 'related', 'DocumentManifestRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifest3', nil, js.FHIRFactoryJs);
  defineDocumentManifestPropsJs(js, def);
end;

procedure defineDocumentReferenceRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceRelatesTo3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReferenceRelatesTo3', 'target', 'Reference(DocumentReference)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceRelatesTo3', nil, js.FHIRFactoryJs);
  defineDocumentReferenceRelatesToPropsJs(js, def);
end;

procedure defineDocumentReferenceContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContent3', 'attachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContent3', 'format', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContent3', nil, js.FHIRFactoryJs);
  defineDocumentReferenceContentPropsJs(js, def);
end;

procedure defineDocumentReferenceContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContext3', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext3', 'event', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReferenceContext3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext3', 'facilityType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext3', 'practiceSetting', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext3', 'sourcePatientInfo', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext3', 'related', 'DocumentReferenceContextRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentReferenceContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContext3', nil, js.FHIRFactoryJs);
  defineDocumentReferenceContextPropsJs(js, def);
end;

procedure defineDocumentReferenceContextRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContextRelated3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContextRelated3', 'ref', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceContextRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContextRelated3', nil, js.FHIRFactoryJs);
  defineDocumentReferenceContextRelatedPropsJs(js, def);
end;

procedure defineDocumentReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentReference3', 'masterIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference3', 'docStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference3', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference3', 'subject', 'Reference(Patient|Practitioner|Group|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference3', 'indexed', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference3', 'author', 'Reference(Practitioner|Organization|Device|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference3', 'authenticator', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference3', 'custodian', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference3', 'relatesTo', 'DocumentReferenceRelatesTo', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference3', 'securityLabel', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference3', 'content', 'DocumentReferenceContent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference3', 'context', 'DocumentReferenceContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReference3', nil, js.FHIRFactoryJs);
  defineDocumentReferencePropsJs(js, def);
end;

procedure defineEligibilityRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityRequest3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityRequest3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityRequest3', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest3', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityRequest3', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityRequest3', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest3', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest3', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest3', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest3', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest3', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityRequest3', 'benefitCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest3', 'benefitSubCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityRequest3', nil, js.FHIRFactoryJs);
  defineEligibilityRequestPropsJs(js, def);
end;

procedure defineEligibilityResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsurance3', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsurance3', 'contract', 'Reference(Contract)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsurance3', 'benefitBalance', 'EligibilityResponseInsuranceBenefitBalance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsurance3', nil, js.FHIRFactoryJs);
  defineEligibilityResponseInsurancePropsJs(js, def);
end;

procedure defineEligibilityResponseInsuranceBenefitBalancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance3', 'subCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance3', 'excluded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance3', 'network', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance3', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance3', 'term', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance3', 'financial', 'EligibilityResponseInsuranceBenefitBalanceFinancial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseInsuranceBenefitBalanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsuranceBenefitBalance3', nil, js.FHIRFactoryJs);
  defineEligibilityResponseInsuranceBenefitBalancePropsJs(js, def);
end;

procedure defineEligibilityResponseInsuranceBenefitBalanceFinancialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial3', 'allowedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial3', 'allowedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial3', 'allowedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial3', 'usedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial3', 'usedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityResponseInsuranceBenefitBalanceFinancialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsuranceBenefitBalanceFinancial3', nil, js.FHIRFactoryJs);
  defineEligibilityResponseInsuranceBenefitBalanceFinancialPropsJs(js, def);
end;

procedure defineEligibilityResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseError3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseError3', nil, js.FHIRFactoryJs);
  defineEligibilityResponseErrorPropsJs(js, def);
end;

procedure defineEligibilityResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityResponse3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityResponse3', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse3', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse3', 'request', 'Reference(EligibilityRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse3', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse3', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse3', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse3', 'inforce', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponse3', 'insurance', 'EligibilityResponseInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse3', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse3', 'error', 'EligibilityResponseError', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEligibilityResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponse3', nil, js.FHIRFactoryJs);
  defineEligibilityResponsePropsJs(js, def);
end;

procedure defineEncounterStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterStatusHistory3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EncounterStatusHistory3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterStatusHistory3', nil, js.FHIRFactoryJs);
  defineEncounterStatusHistoryPropsJs(js, def);
end;

procedure defineEncounterClassHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterClassHistory3', 'class', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterClassHistory3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterClassHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterClassHistory3', nil, js.FHIRFactoryJs);
  defineEncounterClassHistoryPropsJs(js, def);
end;

procedure defineEncounterParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterParticipant3', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterParticipant3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterParticipant3', 'individual', 'Reference(Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterParticipant3', nil, js.FHIRFactoryJs);
  defineEncounterParticipantPropsJs(js, def);
end;

procedure defineEncounterDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterDiagnosis3', 'condition', 'Reference(Condition|Procedure)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis3', 'rank', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineEncounterDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterDiagnosis3', nil, js.FHIRFactoryJs);
  defineEncounterDiagnosisPropsJs(js, def);
end;

procedure defineEncounterHospitalizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterHospitalization3', 'preAdmissionIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization3', 'origin', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization3', 'admitSource', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization3', 'reAdmission', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization3', 'dietPreference', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization3', 'specialCourtesy', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization3', 'specialArrangement', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization3', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization3', 'dischargeDisposition', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterHospitalizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterHospitalization3', nil, js.FHIRFactoryJs);
  defineEncounterHospitalizationPropsJs(js, def);
end;

procedure defineEncounterLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterLocation3', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterLocation3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EncounterLocation3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterLocation3', nil, js.FHIRFactoryJs);
  defineEncounterLocationPropsJs(js, def);
end;

procedure defineEncounterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Encounter3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Encounter3', 'statusHistory', 'EncounterStatusHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'class', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter3', 'classHistory', 'EncounterClassHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter3', 'episodeOfCare', 'Reference(EpisodeOfCare)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'incomingReferral', 'Reference(ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'participant', 'EncounterParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'appointment', 'Reference(Appointment)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter3', 'length', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter3', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'diagnosis', 'EncounterDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'account', 'Reference(Account)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'hospitalization', 'EncounterHospitalization', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter3', 'location', 'EncounterLocation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter3', 'serviceProvider', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter3', 'partOf', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Encounter3', nil, js.FHIRFactoryJs);
  defineEncounterPropsJs(js, def);
end;

procedure defineEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Endpoint3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Endpoint3', 'connectionType', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Endpoint3', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint3', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint3', 'payloadType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint3', 'address', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Endpoint3', nil, js.FHIRFactoryJs);
  defineEndpointPropsJs(js, def);
end;

procedure defineEnrollmentRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentRequest3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentRequest3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentRequest3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentRequest3', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest3', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest3', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest3', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentRequest3', nil, js.FHIRFactoryJs);
  defineEnrollmentRequestPropsJs(js, def);
end;

procedure defineEnrollmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentResponse3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentResponse3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse3', 'request', 'Reference(EnrollmentRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse3', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse3', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentResponse3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse3', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse3', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentResponse3', nil, js.FHIRFactoryJs);
  defineEnrollmentResponsePropsJs(js, def);
end;

procedure defineEpisodeOfCareStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareStatusHistory3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCareStatusHistory3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEpisodeOfCareStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareStatusHistory3', nil, js.FHIRFactoryJs);
  defineEpisodeOfCareStatusHistoryPropsJs(js, def);
end;

procedure defineEpisodeOfCareDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareDiagnosis3', 'condition', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareDiagnosis3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareDiagnosis3', 'rank', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineEpisodeOfCareDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareDiagnosis3', nil, js.FHIRFactoryJs);
  defineEpisodeOfCareDiagnosisPropsJs(js, def);
end;

procedure defineEpisodeOfCarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCare3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCare3', 'statusHistory', 'EpisodeOfCareStatusHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare3', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare3', 'diagnosis', 'EpisodeOfCareDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare3', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare3', 'referralRequest', 'Reference(ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare3', 'careManager', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare3', 'team', 'Reference(CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare3', 'account', 'Reference(Account)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEpisodeOfCareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCare3', nil, js.FHIRFactoryJs);
  defineEpisodeOfCarePropsJs(js, def);
end;

procedure defineExpansionProfileFixedVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileFixedVersion3', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileFixedVersion3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileFixedVersion3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExpansionProfileFixedVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileFixedVersion3', nil, js.FHIRFactoryJs);
  defineExpansionProfileFixedVersionPropsJs(js, def);
end;

procedure defineExpansionProfileExcludedSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileExcludedSystem3', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileExcludedSystem3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExpansionProfileExcludedSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileExcludedSystem3', nil, js.FHIRFactoryJs);
  defineExpansionProfileExcludedSystemPropsJs(js, def);
end;

procedure defineExpansionProfileDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignation3', 'include', 'ExpansionProfileDesignationInclude', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfileDesignation3', 'exclude', 'ExpansionProfileDesignationExclude', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignation3', nil, js.FHIRFactoryJs);
  defineExpansionProfileDesignationPropsJs(js, def);
end;

procedure defineExpansionProfileDesignationIncludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationInclude3', 'designation', 'ExpansionProfileDesignationIncludeDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExpansionProfileDesignationIncludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationInclude3', nil, js.FHIRFactoryJs);
  defineExpansionProfileDesignationIncludePropsJs(js, def);
end;

procedure defineExpansionProfileDesignationIncludeDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationIncludeDesignation3', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileDesignationIncludeDesignation3', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationIncludeDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationIncludeDesignation3', nil, js.FHIRFactoryJs);
  defineExpansionProfileDesignationIncludeDesignationPropsJs(js, def);
end;

procedure defineExpansionProfileDesignationExcludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationExclude3', 'designation', 'ExpansionProfileDesignationExcludeDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExpansionProfileDesignationExcludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationExclude3', nil, js.FHIRFactoryJs);
  defineExpansionProfileDesignationExcludePropsJs(js, def);
end;

procedure defineExpansionProfileDesignationExcludeDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationExcludeDesignation3', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileDesignationExcludeDesignation3', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationExcludeDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationExcludeDesignation3', nil, js.FHIRFactoryJs);
  defineExpansionProfileDesignationExcludeDesignationPropsJs(js, def);
end;

procedure defineExpansionProfilePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ExpansionProfile3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExpansionProfile3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile3', 'fixedVersion', 'ExpansionProfileFixedVersion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile3', 'excludedSystem', 'ExpansionProfileExcludedSystem', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile3', 'includeDesignations', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile3', 'designation', 'ExpansionProfileDesignation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile3', 'includeDefinition', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile3', 'activeOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile3', 'excludeNested', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile3', 'excludeNotForUI', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile3', 'excludePostCoordinated', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile3', 'displayLanguage', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile3', 'limitedExpansion', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineExpansionProfileJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfile3', nil, js.FHIRFactoryJs);
  defineExpansionProfilePropsJs(js, def);
end;

procedure defineExplanationOfBenefitRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitRelated3', 'claim', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitRelated3', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitRelated3', 'reference', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitRelated3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitRelatedPropsJs(js, def);
end;

procedure defineExplanationOfBenefitPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitPayee3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayee3', 'resourceType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayee3', 'party', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayee3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitPayeePropsJs(js, def);
end;

procedure defineExplanationOfBenefitInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitInformation3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation3', 'timingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation3', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation3', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation3', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation3', 'reason', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitInformation3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitInformationPropsJs(js, def);
end;

procedure defineExplanationOfBenefitCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam3', 'provider', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam3', 'responsible', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam3', 'qualification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitCareTeam3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitCareTeamPropsJs(js, def);
end;

procedure defineExplanationOfBenefitDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis3', 'diagnosisCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis3', 'diagnosisReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis3', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis3', 'packageCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitDiagnosis3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitDiagnosisPropsJs(js, def);
end;

procedure defineExplanationOfBenefitProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitProcedure3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure3', 'procedureCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure3', 'procedureReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitProcedure3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitProcedurePropsJs(js, def);
end;

procedure defineExplanationOfBenefitInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitInsurance3', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitInsurance3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitInsurancePropsJs(js, def);
end;

procedure defineExplanationOfBenefitAccidentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAccident3', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident3', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident3', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitAccidentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAccident3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitAccidentPropsJs(js, def);
end;

procedure defineExplanationOfBenefitItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'locationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'subSite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'encounter', 'Reference(Encounter)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'adjudication', 'ExplanationOfBenefitItemAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem3', 'detail', 'ExplanationOfBenefitItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItem3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitItemPropsJs(js, def);
end;

procedure defineExplanationOfBenefitItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication3', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication3', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication3', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineExplanationOfBenefitItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemAdjudication3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitItemAdjudicationPropsJs(js, def);
end;

procedure defineExplanationOfBenefitItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail3', 'subDetail', 'ExplanationOfBenefitItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemDetail3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitItemDetailPropsJs(js, def);
end;

procedure defineExplanationOfBenefitItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail3', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemDetailSubDetail3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitItemDetailSubDetailPropsJs(js, def);
end;

procedure defineExplanationOfBenefitAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAddItem3', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem3', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem3', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem3', 'fee', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem3', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem3', 'detail', 'ExplanationOfBenefitAddItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItem3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitAddItemPropsJs(js, def);
end;

procedure defineExplanationOfBenefitAddItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail3', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail3', 'service', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail3', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail3', 'fee', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail3', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItemDetail3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitAddItemDetailPropsJs(js, def);
end;

procedure defineExplanationOfBenefitPaymentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitPayment3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment3', 'adjustment', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment3', 'adjustmentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment3', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment3', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPaymentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayment3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitPaymentPropsJs(js, def);
end;

procedure defineExplanationOfBenefitProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote3', 'number', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote3', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitProcessNote3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitProcessNotePropsJs(js, def);
end;

procedure defineExplanationOfBenefitBenefitBalancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance3', 'subCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance3', 'excluded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance3', 'network', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance3', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance3', 'term', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance3', 'financial', 'ExplanationOfBenefitBenefitBalanceFinancial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitBenefitBalanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitBenefitBalance3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitBenefitBalancePropsJs(js, def);
end;

procedure defineExplanationOfBenefitBenefitBalanceFinancialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial3', 'allowedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial3', 'allowedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial3', 'allowedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial3', 'usedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial3', 'usedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitBenefitBalanceFinancialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitBenefitBalanceFinancial3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitBenefitBalanceFinancialPropsJs(js, def);
end;

procedure defineExplanationOfBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefit3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'billablePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'referral', 'Reference(ReferralRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'claim', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'related', 'ExplanationOfBenefitRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'prescription', 'Reference(MedicationRequest|VisionPrescription)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'originalPrescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'payee', 'ExplanationOfBenefitPayee', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'information', 'ExplanationOfBenefitInformation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'careTeam', 'ExplanationOfBenefitCareTeam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'diagnosis', 'ExplanationOfBenefitDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'procedure', 'ExplanationOfBenefitProcedure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'precedence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'insurance', 'ExplanationOfBenefitInsurance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'accident', 'ExplanationOfBenefitAccident', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'employmentImpacted', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'hospitalization', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'item', 'ExplanationOfBenefitItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'addItem', 'ExplanationOfBenefitAddItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'totalCost', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'unallocDeductable', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'totalBenefit', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'payment', 'ExplanationOfBenefitPayment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'processNote', 'ExplanationOfBenefitProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit3', 'benefitBalance', 'ExplanationOfBenefitBenefitBalance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefit3', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitPropsJs(js, def);
end;

procedure defineFamilyMemberHistoryConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistoryCondition3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition3', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition3', 'onsetAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition3', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition3', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition3', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistoryCondition3', nil, js.FHIRFactoryJs);
  defineFamilyMemberHistoryConditionPropsJs(js, def);
end;

procedure defineFamilyMemberHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistory3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'definition', 'Reference(PlanDefinition|Questionnaire)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'notDone', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'notDoneReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'bornPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'bornDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'bornString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'ageAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'ageRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'ageString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'estimatedAge', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'deceasedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'deceasedAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'deceasedRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'deceasedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'deceasedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'reasonReference', 'Reference(Condition|Observation|AllergyIntolerance|QuestionnaireResponse)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory3', 'condition', 'FamilyMemberHistoryCondition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistory3', nil, js.FHIRFactoryJs);
  defineFamilyMemberHistoryPropsJs(js, def);
end;

procedure defineFlagPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Flag3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Flag3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Flag3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag3', 'subject', 'Reference(Patient|Location|Group|Organization|Practitioner|PlanDefinition|Medication|Procedure)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag3', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag3', 'author', 'Reference(Device|Organization|Patient|Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineFlagJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Flag3', nil, js.FHIRFactoryJs);
  defineFlagPropsJs(js, def);
end;

procedure defineGoalTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GoalTarget3', 'measure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget3', 'detailQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget3', 'detailRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget3', 'detailCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget3', 'dueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GoalTarget3', 'dueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineGoalTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GoalTarget3', nil, js.FHIRFactoryJs);
  defineGoalTargetPropsJs(js, def);
end;

procedure defineGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Goal3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal3', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal3', 'description', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal3', 'subject', 'Reference(Patient|Group|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal3', 'startDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal3', 'startCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal3', 'target', 'GoalTarget', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal3', 'statusDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal3', 'statusReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal3', 'expressedBy', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal3', 'addresses', 'Reference(Condition|Observation|MedicationStatement|NutritionOrder|ProcedureRequest|RiskAssessment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal3', 'outcomeCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal3', 'outcomeReference', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Goal3', nil, js.FHIRFactoryJs);
  defineGoalPropsJs(js, def);
end;

procedure defineGraphDefinitionLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLink3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink3', 'sliceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink3', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'GraphDefinitionLink3', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink3', 'target', 'GraphDefinitionLinkTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLink3', nil, js.FHIRFactoryJs);
  defineGraphDefinitionLinkPropsJs(js, def);
end;

procedure defineGraphDefinitionLinkTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLinkTarget3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget3', 'profile', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget3', 'compartment', 'GraphDefinitionLinkTargetCompartment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget3', 'link', '@GraphDefinition.link', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionLinkTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLinkTarget3', nil, js.FHIRFactoryJs);
  defineGraphDefinitionLinkTargetPropsJs(js, def);
end;

procedure defineGraphDefinitionLinkTargetCompartmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment3', 'rule', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment3', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineGraphDefinitionLinkTargetCompartmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLinkTargetCompartment3', nil, js.FHIRFactoryJs);
  defineGraphDefinitionLinkTargetCompartmentPropsJs(js, def);
end;

procedure defineGraphDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'GraphDefinition3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GraphDefinition3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GraphDefinition3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition3', 'start', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition3', 'profile', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition3', 'link', 'GraphDefinitionLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinition3', nil, js.FHIRFactoryJs);
  defineGraphDefinitionPropsJs(js, def);
end;

procedure defineGroupCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupCharacteristic3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic3', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic3', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic3', 'exclude', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineGroupCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupCharacteristic3', nil, js.FHIRFactoryJs);
  defineGroupCharacteristicPropsJs(js, def);
end;

procedure defineGroupMemberPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupMember3', 'entity', 'Reference(Patient|Practitioner|Device|Medication|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupMember3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupMember3', 'inactive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineGroupMemberJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupMember3', nil, js.FHIRFactoryJs);
  defineGroupMemberPropsJs(js, def);
end;

procedure defineGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Group3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Group3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Group3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group3', 'actual', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Group3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Group3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group3', 'quantity', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group3', 'characteristic', 'GroupCharacteristic', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Group3', 'member', 'GroupMember', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Group3', nil, js.FHIRFactoryJs);
  defineGroupPropsJs(js, def);
end;

procedure defineGuidanceResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'GuidanceResponse3', 'requestId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse3', 'module', 'Reference(ServiceDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse3', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GuidanceResponse3', 'performer', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse3', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse3', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse3', 'evaluationMessage', 'Reference(OperationOutcome)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse3', 'outputParameters', 'Reference(Parameters)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse3', 'result', 'Reference(CarePlan|RequestGroup)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse3', 'dataRequirement', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGuidanceResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GuidanceResponse3', nil, js.FHIRFactoryJs);
  defineGuidanceResponsePropsJs(js, def);
end;

procedure defineHealthcareServiceAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceAvailableTime3', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime3', 'availableStartTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime3', 'availableEndTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineHealthcareServiceAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceAvailableTime3', nil, js.FHIRFactoryJs);
  defineHealthcareServiceAvailableTimePropsJs(js, def);
end;

procedure defineHealthcareServiceNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceNotAvailable3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceNotAvailable3', 'during', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineHealthcareServiceNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceNotAvailable3', nil, js.FHIRFactoryJs);
  defineHealthcareServiceNotAvailablePropsJs(js, def);
end;

procedure defineHealthcareServicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'HealthcareService3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService3', 'providedBy', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService3', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService3', 'extraDetails', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService3', 'photo', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'coverageArea', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'serviceProvisionCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'eligibility', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService3', 'eligibilityNote', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService3', 'characteristic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'referralMethod', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'appointmentRequired', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService3', 'availableTime', 'HealthcareServiceAvailableTime', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'notAvailable', 'HealthcareServiceNotAvailable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService3', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService3', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineHealthcareServiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareService3', nil, js.FHIRFactoryJs);
  defineHealthcareServicePropsJs(js, def);
end;

procedure defineImagingManifestStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingManifestStudy3', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingManifestStudy3', 'imagingStudy', 'Reference(ImagingStudy)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingManifestStudy3', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingManifestStudy3', 'series', 'ImagingManifestStudySeries', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingManifestStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingManifestStudy3', nil, js.FHIRFactoryJs);
  defineImagingManifestStudyPropsJs(js, def);
end;

procedure defineImagingManifestStudySeriesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingManifestStudySeries3', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingManifestStudySeries3', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingManifestStudySeries3', 'instance', 'ImagingManifestStudySeriesInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingManifestStudySeriesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingManifestStudySeries3', nil, js.FHIRFactoryJs);
  defineImagingManifestStudySeriesPropsJs(js, def);
end;

procedure defineImagingManifestStudySeriesInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingManifestStudySeriesInstance3', 'sopClass', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingManifestStudySeriesInstance3', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImagingManifestStudySeriesInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingManifestStudySeriesInstance3', nil, js.FHIRFactoryJs);
  defineImagingManifestStudySeriesInstancePropsJs(js, def);
end;

procedure defineImagingManifestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImagingManifest3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingManifest3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingManifest3', 'authoringTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingManifest3', 'author', 'Reference(Practitioner|Device|Organization|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingManifest3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingManifest3', 'study', 'ImagingManifestStudy', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingManifest3', nil, js.FHIRFactoryJs);
  defineImagingManifestPropsJs(js, def);
end;

procedure defineImagingStudySeriesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeries3', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries3', 'number', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries3', 'modality', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries3', 'numberOfInstances', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries3', 'availability', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries3', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries3', 'bodySite', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries3', 'laterality', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries3', 'started', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudySeries3', 'performer', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries3', 'instance', 'ImagingStudySeriesInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudySeriesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeries3', nil, js.FHIRFactoryJs);
  defineImagingStudySeriesPropsJs(js, def);
end;

procedure defineImagingStudySeriesInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeriesInstance3', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance3', 'number', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance3', 'sopClass', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImagingStudySeriesInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeriesInstance3', nil, js.FHIRFactoryJs);
  defineImagingStudySeriesInstancePropsJs(js, def);
end;

procedure defineImagingStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImagingStudy3', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy3', 'accession', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy3', 'availability', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy3', 'modalityList', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy3', 'started', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudy3', 'basedOn', 'Reference(ReferralRequest|CarePlan|ProcedureRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy3', 'referrer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy3', 'interpreter', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy3', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy3', 'numberOfSeries', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy3', 'numberOfInstances', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy3', 'procedureReference', 'Reference(Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy3', 'procedureCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy3', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy3', 'series', 'ImagingStudySeries', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudy3', nil, js.FHIRFactoryJs);
  defineImagingStudyPropsJs(js, def);
end;

procedure defineImmunizationPractitionerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationPractitioner3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationPractitioner3', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImmunizationPractitionerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationPractitioner3', nil, js.FHIRFactoryJs);
  defineImmunizationPractitionerPropsJs(js, def);
end;

procedure defineImmunizationExplanationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationExplanation3', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationExplanation3', 'reasonNotGiven', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationExplanationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationExplanation3', nil, js.FHIRFactoryJs);
  defineImmunizationExplanationPropsJs(js, def);
end;

procedure defineImmunizationReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationReaction3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationReaction3', 'detail', 'Reference(Observation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationReaction3', 'reported', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineImmunizationReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationReaction3', nil, js.FHIRFactoryJs);
  defineImmunizationReactionPropsJs(js, def);
end;

procedure defineImmunizationVaccinationProtocolPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationVaccinationProtocol3', 'doseSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol3', 'authority', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol3', 'series', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol3', 'seriesDoses', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol3', 'targetDisease', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol3', 'doseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol3', 'doseStatusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImmunizationVaccinationProtocolJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationVaccinationProtocol3', nil, js.FHIRFactoryJs);
  defineImmunizationVaccinationProtocolPropsJs(js, def);
end;

procedure defineImmunizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Immunization3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization3', 'notGiven', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization3', 'vaccineCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization3', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization3', 'primarySource', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization3', 'reportOrigin', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization3', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization3', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization3', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization3', 'expirationDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization3', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization3', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization3', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization3', 'practitioner', 'ImmunizationPractitioner', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization3', 'explanation', 'ImmunizationExplanation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization3', 'reaction', 'ImmunizationReaction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization3', 'vaccinationProtocol', 'ImmunizationVaccinationProtocol', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Immunization3', nil, js.FHIRFactoryJs);
  defineImmunizationPropsJs(js, def);
end;

procedure defineImmunizationRecommendationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation3', 'vaccineCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation3', 'targetDisease', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation3', 'doseNumber', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation3', 'forecastStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation3', 'dateCriterion', 'ImmunizationRecommendationRecommendationDateCriterion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation3', 'protocol', 'ImmunizationRecommendationRecommendationProtocol', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation3', 'supportingImmunization', 'Reference(Immunization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation3', 'supportingPatientInformation', 'Reference(Observation|AllergyIntolerance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendation3', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationPropsJs(js, def);
end;

procedure defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion3', 'value', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineImmunizationRecommendationRecommendationDateCriterionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendationDateCriterion3', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js, def);
end;

procedure defineImmunizationRecommendationRecommendationProtocolPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol3', 'doseSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol3', 'authority', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol3', 'series', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImmunizationRecommendationRecommendationProtocolJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendationProtocol3', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationProtocolPropsJs(js, def);
end;

procedure defineImmunizationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendation3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendation3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation3', 'recommendation', 'ImmunizationRecommendationRecommendation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendation3', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationPropsJs(js, def);
end;

procedure defineImplementationGuideDependencyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDependency3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDependency3', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDependencyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDependency3', nil, js.FHIRFactoryJs);
  defineImplementationGuideDependencyPropsJs(js, def);
end;

procedure defineImplementationGuidePackagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePackage3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackage3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackage3', 'resource', 'ImplementationGuidePackageResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuidePackageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePackage3', nil, js.FHIRFactoryJs);
  defineImplementationGuidePackagePropsJs(js, def);
end;

procedure defineImplementationGuidePackageResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePackageResource3', 'example', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuidePackageResource3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource3', 'acronym', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource3', 'sourceUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource3', 'sourceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuidePackageResource3', 'exampleFor', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuidePackageResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePackageResource3', nil, js.FHIRFactoryJs);
  defineImplementationGuidePackageResourcePropsJs(js, def);
end;

procedure defineImplementationGuideGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideGlobal3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideGlobal3', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideGlobal3', nil, js.FHIRFactoryJs);
  defineImplementationGuideGlobalPropsJs(js, def);
end;

procedure defineImplementationGuidePagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePage3', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage3', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage3', 'format', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage3', 'page', '@ImplementationGuide.page', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuidePageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePage3', nil, js.FHIRFactoryJs);
  defineImplementationGuidePagePropsJs(js, def);
end;

procedure defineImplementationGuidePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuide3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuide3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuide3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide3', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide3', 'dependency', 'ImplementationGuideDependency', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide3', 'package', 'ImplementationGuidePackage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide3', 'global', 'ImplementationGuideGlobal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide3', 'page', 'ImplementationGuidePage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuide3', nil, js.FHIRFactoryJs);
  defineImplementationGuidePropsJs(js, def);
end;

procedure defineLibraryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Library3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Library3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library3', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library3', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library3', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library3', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library3', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library3', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library3', 'parameter', 'ParameterDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library3', 'dataRequirement', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library3', 'content', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLibraryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Library3', nil, js.FHIRFactoryJs);
  defineLibraryPropsJs(js, def);
end;

procedure defineLinkageItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LinkageItem3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'LinkageItem3', 'resource', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineLinkageItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LinkageItem3', nil, js.FHIRFactoryJs);
  defineLinkageItemPropsJs(js, def);
end;

procedure defineLinkagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Linkage3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Linkage3', 'author', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Linkage3', 'item', 'LinkageItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLinkageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Linkage3', nil, js.FHIRFactoryJs);
  defineLinkagePropsJs(js, def);
end;

procedure defineListEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ListEntry3', 'flag', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ListEntry3', 'deleted', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ListEntry3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ListEntry3', 'item', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineListEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ListEntry3', nil, js.FHIRFactoryJs);
  defineListEntryPropsJs(js, def);
end;

procedure defineListPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'List3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List3', 'subject', 'Reference(Patient|Group|Device|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List3', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'List3', 'source', 'Reference(Practitioner|Patient|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List3', 'orderedBy', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List3', 'entry', 'ListEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List3', 'emptyReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineListJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('List3', nil, js.FHIRFactoryJs);
  defineListPropsJs(js, def);
end;

procedure defineLocationPositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LocationPosition3', 'longitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition3', 'latitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition3', 'altitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineLocationPositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LocationPosition3', nil, js.FHIRFactoryJs);
  defineLocationPositionPropsJs(js, def);
end;

procedure defineLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Location3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location3', 'operationalStatus', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location3', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location3', 'physicalType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location3', 'position', 'LocationPosition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location3', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location3', 'partOf', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location3', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Location3', nil, js.FHIRFactoryJs);
  defineLocationPropsJs(js, def);
end;

procedure defineMeasureGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroup3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroup3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroup3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroup3', 'population', 'MeasureGroupPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureGroup3', 'stratifier', 'MeasureGroupStratifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroup3', nil, js.FHIRFactoryJs);
  defineMeasureGroupPropsJs(js, def);
end;

procedure defineMeasureGroupPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupPopulation3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupPopulation3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupPopulation3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupPopulation3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupPopulation3', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMeasureGroupPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupPopulation3', nil, js.FHIRFactoryJs);
  defineMeasureGroupPopulationPropsJs(js, def);
end;

procedure defineMeasureGroupStratifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupStratifier3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupStratifier3', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupStratifier3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMeasureGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupStratifier3', nil, js.FHIRFactoryJs);
  defineMeasureGroupStratifierPropsJs(js, def);
end;

procedure defineMeasureSupplementalDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureSupplementalData3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureSupplementalData3', 'usage', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureSupplementalData3', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureSupplementalData3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMeasureSupplementalDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureSupplementalData3', nil, js.FHIRFactoryJs);
  defineMeasureSupplementalDataPropsJs(js, def);
end;

procedure defineMeasurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Measure3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Measure3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure3', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure3', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure3', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure3', 'library', 'Reference(Library)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure3', 'disclaimer', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'scoring', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure3', 'compositeScoring', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure3', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure3', 'riskAdjustment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'rateAggregation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'rationale', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'clinicalRecommendationStatement', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'improvementNotation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'guidance', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'set', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure3', 'group', 'MeasureGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure3', 'supplementalData', 'MeasureSupplementalData', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Measure3', nil, js.FHIRFactoryJs);
  defineMeasurePropsJs(js, def);
end;

procedure defineMeasureReportGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroup3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroup3', 'population', 'MeasureReportGroupPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroup3', 'measureScore', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MeasureReportGroup3', 'stratifier', 'MeasureReportGroupStratifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureReportGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroup3', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupPropsJs(js, def);
end;

procedure defineMeasureReportGroupPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupPopulation3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupPopulation3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupPopulation3', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupPopulation3', 'patients', 'Reference(List)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupPopulation3', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupPopulationPropsJs(js, def);
end;

procedure defineMeasureReportGroupStratifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifier3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifier3', 'stratum', 'MeasureReportGroupStratifierStratum', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureReportGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifier3', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierPropsJs(js, def);
end;

procedure defineMeasureReportGroupStratifierStratumPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum3', 'population', 'MeasureReportGroupStratifierStratumPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum3', 'measureScore', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineMeasureReportGroupStratifierStratumJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratum3', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumPropsJs(js, def);
end;

procedure defineMeasureReportGroupStratifierStratumPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation3', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation3', 'patients', 'Reference(List)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratumPopulation3', nil, js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumPopulationPropsJs(js, def);
end;

procedure defineMeasureReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MeasureReport3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport3', 'measure', 'Reference(Measure)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MeasureReport3', 'reportingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport3', 'group', 'MeasureReportGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReport3', 'evaluatedResources', 'Reference(Bundle)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReport3', nil, js.FHIRFactoryJs);
  defineMeasureReportPropsJs(js, def);
end;

procedure defineMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Media3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media3', 'basedOn', 'Reference(ProcedureRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media3', 'subtype', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media3', 'view', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media3', 'subject', 'Reference(Patient|Practitioner|Group|Device|Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media3', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Media3', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media3', 'operator', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media3', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media3', 'device', 'Reference(Device|DeviceMetric)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media3', 'height', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media3', 'width', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media3', 'frames', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media3', 'duration', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media3', 'content', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Media3', nil, js.FHIRFactoryJs);
  defineMediaPropsJs(js, def);
end;

procedure defineMedicationIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationIngredient3', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationIngredient3', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationIngredient3', 'isActive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationIngredient3', 'amount', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationIngredient3', nil, js.FHIRFactoryJs);
  defineMedicationIngredientPropsJs(js, def);
end;

procedure defineMedicationPackagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationPackage3', 'container', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationPackage3', 'content', 'MedicationPackageContent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationPackage3', 'batch', 'MedicationPackageBatch', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationPackageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationPackage3', nil, js.FHIRFactoryJs);
  defineMedicationPackagePropsJs(js, def);
end;

procedure defineMedicationPackageContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationPackageContent3', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationPackageContent3', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationPackageContent3', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationPackageContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationPackageContent3', nil, js.FHIRFactoryJs);
  defineMedicationPackageContentPropsJs(js, def);
end;

procedure defineMedicationPackageBatchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationPackageBatch3', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationPackageBatch3', 'expirationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineMedicationPackageBatchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationPackageBatch3', nil, js.FHIRFactoryJs);
  defineMedicationPackageBatchPropsJs(js, def);
end;

procedure defineMedicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Medication3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Medication3', 'isBrand', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Medication3', 'isOverTheCounter', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Medication3', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication3', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication3', 'ingredient', 'MedicationIngredient', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Medication3', 'package', 'MedicationPackage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication3', 'image', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Medication3', nil, js.FHIRFactoryJs);
  defineMedicationPropsJs(js, def);
end;

procedure defineMedicationAdministrationPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationPerformer3', 'actor', 'Reference(Practitioner|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationPerformer3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationAdministrationPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationPerformer3', nil, js.FHIRFactoryJs);
  defineMedicationAdministrationPerformerPropsJs(js, def);
end;

procedure defineMedicationAdministrationDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationDosage3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministrationDosage3', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage3', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage3', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage3', 'dose', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage3', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage3', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationAdministrationDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationDosage3', nil, js.FHIRFactoryJs);
  defineMedicationAdministrationDosagePropsJs(js, def);
end;

procedure defineMedicationAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationAdministration3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration3', 'definition', 'Reference(PlanDefinition|ActivityDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration3', 'partOf', 'Reference(MedicationAdministration|Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministration3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration3', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration3', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration3', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration3', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationAdministration3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration3', 'performer', 'MedicationAdministrationPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration3', 'notGiven', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationAdministration3', 'reasonNotGiven', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration3', 'reasonReference', 'Reference(Condition|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration3', 'prescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration3', 'device', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration3', 'dosage', 'MedicationAdministrationDosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration3', 'eventHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministration3', nil, js.FHIRFactoryJs);
  defineMedicationAdministrationPropsJs(js, def);
end;

procedure defineMedicationDispensePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispensePerformer3', 'actor', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispensePerformer3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationDispensePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispensePerformer3', nil, js.FHIRFactoryJs);
  defineMedicationDispensePerformerPropsJs(js, def);
end;

procedure defineMedicationDispenseSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispenseSubstitution3', 'wasSubstituted', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationDispenseSubstitution3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseSubstitution3', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispenseSubstitution3', 'responsibleParty', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationDispenseSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispenseSubstitution3', nil, js.FHIRFactoryJs);
  defineMedicationDispenseSubstitutionPropsJs(js, def);
end;

procedure defineMedicationDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationDispense3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense3', 'partOf', 'Reference(Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationDispense3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense3', 'performer', 'MedicationDispensePerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense3', 'authorizingPrescription', 'Reference(MedicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'daysSupply', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'whenPrepared', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense3', 'whenHandedOver', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense3', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'receiver', 'Reference(Patient|Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense3', 'dosageInstruction', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense3', 'substitution', 'MedicationDispenseSubstitution', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'detectedIssue', 'Reference(DetectedIssue)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense3', 'notDone', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationDispense3', 'notDoneReasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'notDoneReasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense3', 'eventHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispense3', nil, js.FHIRFactoryJs);
  defineMedicationDispensePropsJs(js, def);
end;

procedure defineMedicationRequestRequesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestRequester3', 'agent', 'Reference(Practitioner|Organization|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestRequester3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestRequesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestRequester3', nil, js.FHIRFactoryJs);
  defineMedicationRequestRequesterPropsJs(js, def);
end;

procedure defineMedicationRequestDispenseRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestDispenseRequest3', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest3', 'numberOfRepeatsAllowed', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest3', 'expectedSupplyDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest3', 'performer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestDispenseRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestDispenseRequest3', nil, js.FHIRFactoryJs);
  defineMedicationRequestDispenseRequestPropsJs(js, def);
end;

procedure defineMedicationRequestSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestSubstitution3', 'allowed', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationRequestSubstitution3', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestSubstitution3', nil, js.FHIRFactoryJs);
  defineMedicationRequestSubstitutionPropsJs(js, def);
end;

procedure defineMedicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationRequest3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest3', 'definition', 'Reference(ActivityDefinition|PlanDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest3', 'basedOn', 'Reference(CarePlan|MedicationRequest|ProcedureRequest|ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest3', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest3', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest3', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest3', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationRequest3', 'requester', 'MedicationRequestRequester', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'recorder', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest3', 'reasonReference', 'Reference(Condition|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest3', 'dosageInstruction', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest3', 'dispenseRequest', 'MedicationRequestDispenseRequest', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'substitution', 'MedicationRequestSubstitution', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'priorPrescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest3', 'detectedIssue', 'Reference(DetectedIssue)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest3', 'eventHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequest3', nil, js.FHIRFactoryJs);
  defineMedicationRequestPropsJs(js, def);
end;

procedure defineMedicationStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationStatement3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement3', 'basedOn', 'Reference(MedicationRequest|CarePlan|ProcedureRequest|ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement3', 'partOf', 'Reference(MedicationAdministration|MedicationDispense|MedicationStatement|Procedure|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement3', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement3', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement3', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement3', 'dateAsserted', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement3', 'informationSource', 'Reference(Patient|Practitioner|RelatedPerson|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement3', 'derivedFrom', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement3', 'taken', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement3', 'reasonNotTaken', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement3', 'reasonReference', 'Reference(Condition|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement3', 'dosage', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationStatement3', nil, js.FHIRFactoryJs);
  defineMedicationStatementPropsJs(js, def);
end;

procedure defineMessageDefinitionFocusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionFocus3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus3', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinitionFocus3', 'min', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus3', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageDefinitionFocusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinitionFocus3', nil, js.FHIRFactoryJs);
  defineMessageDefinitionFocusPropsJs(js, def);
end;

procedure defineMessageDefinitionAllowedResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionAllowedResponse3', 'message', 'Reference(MessageDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinitionAllowedResponse3', 'situation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageDefinitionAllowedResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinitionAllowedResponse3', nil, js.FHIRFactoryJs);
  defineMessageDefinitionAllowedResponsePropsJs(js, def);
end;

procedure defineMessageDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'MessageDefinition3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinition3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MessageDefinition3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MessageDefinition3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition3', 'base', 'Reference(MessageDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinition3', 'parent', 'Reference(ActivityDefinition|PlanDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition3', 'replaces', 'Reference(MessageDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition3', 'event', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinition3', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition3', 'focus', 'MessageDefinitionFocus', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition3', 'responseRequired', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MessageDefinition3', 'allowedResponse', 'MessageDefinitionAllowedResponse', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMessageDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinition3', nil, js.FHIRFactoryJs);
  defineMessageDefinitionPropsJs(js, def);
end;

procedure defineMessageHeaderDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderDestination3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination3', 'target', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderDestination3', 'endpoint', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderDestination3', nil, js.FHIRFactoryJs);
  defineMessageHeaderDestinationPropsJs(js, def);
end;

procedure defineMessageHeaderSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderSource3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource3', 'software', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource3', 'contact', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderSource3', 'endpoint', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderSource3', nil, js.FHIRFactoryJs);
  defineMessageHeaderSourcePropsJs(js, def);
end;

procedure defineMessageHeaderResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderResponse3', 'identifier', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse3', 'details', 'Reference(OperationOutcome)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMessageHeaderResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderResponse3', nil, js.FHIRFactoryJs);
  defineMessageHeaderResponsePropsJs(js, def);
end;

procedure defineMessageHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MessageHeader3', 'event', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader3', 'destination', 'MessageHeaderDestination', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader3', 'receiver', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader3', 'sender', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader3', 'timestamp', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MessageHeader3', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader3', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader3', 'source', 'MessageHeaderSource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader3', 'responsible', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader3', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader3', 'response', 'MessageHeaderResponse', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader3', 'focus', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMessageHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeader3', nil, js.FHIRFactoryJs);
  defineMessageHeaderPropsJs(js, def);
end;

procedure defineNamingSystemUniqueIdPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NamingSystemUniqueId3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId3', 'preferred', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'NamingSystemUniqueId3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNamingSystemUniqueIdJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystemUniqueId3', nil, js.FHIRFactoryJs);
  defineNamingSystemUniqueIdPropsJs(js, def);
end;

procedure defineNamingSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'NamingSystem3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem3', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NamingSystem3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem3', 'responsible', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NamingSystem3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem3', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem3', 'uniqueId', 'NamingSystemUniqueId', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem3', 'replacedBy', 'Reference(NamingSystem)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNamingSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystem3', nil, js.FHIRFactoryJs);
  defineNamingSystemPropsJs(js, def);
end;

procedure defineNutritionOrderOralDietPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDiet3', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet3', 'schedule', 'Timing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet3', 'nutrient', 'NutritionOrderOralDietNutrient', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet3', 'texture', 'NutritionOrderOralDietTexture', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet3', 'fluidConsistencyType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet3', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderOralDietJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDiet3', nil, js.FHIRFactoryJs);
  defineNutritionOrderOralDietPropsJs(js, def);
end;

procedure defineNutritionOrderOralDietNutrientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietNutrient3', 'modifier', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietNutrient3', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietNutrientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietNutrient3', nil, js.FHIRFactoryJs);
  defineNutritionOrderOralDietNutrientPropsJs(js, def);
end;

procedure defineNutritionOrderOralDietTexturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietTexture3', 'modifier', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietTexture3', 'foodType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietTextureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietTexture3', nil, js.FHIRFactoryJs);
  defineNutritionOrderOralDietTexturePropsJs(js, def);
end;

procedure defineNutritionOrderSupplementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderSupplement3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement3', 'productName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderSupplement3', 'schedule', 'Timing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderSupplement3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement3', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderSupplementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderSupplement3', nil, js.FHIRFactoryJs);
  defineNutritionOrderSupplementPropsJs(js, def);
end;

procedure defineNutritionOrderEnteralFormulaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormula3', 'baseFormulaType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula3', 'baseFormulaProductName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula3', 'additiveType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula3', 'additiveProductName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula3', 'caloricDensity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula3', 'routeofAdministration', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula3', 'administration', 'NutritionOrderEnteralFormulaAdministration', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula3', 'maxVolumeToDeliver', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula3', 'administrationInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderEnteralFormulaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormula3', nil, js.FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaPropsJs(js, def);
end;

procedure defineNutritionOrderEnteralFormulaAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration3', 'schedule', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration3', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration3', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderEnteralFormulaAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormulaAdministration3', nil, js.FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaAdministrationPropsJs(js, def);
end;

procedure defineNutritionOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'NutritionOrder3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrder3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder3', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder3', 'dateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NutritionOrder3', 'orderer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder3', 'allergyIntolerance', 'Reference(AllergyIntolerance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder3', 'foodPreferenceModifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder3', 'excludeFoodModifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder3', 'oralDiet', 'NutritionOrderOralDiet', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder3', 'supplement', 'NutritionOrderSupplement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder3', 'enteralFormula', 'NutritionOrderEnteralFormula', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrder3', nil, js.FHIRFactoryJs);
  defineNutritionOrderPropsJs(js, def);
end;

procedure defineObservationReferenceRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationReferenceRange3', 'low', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange3', 'high', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange3', 'appliesTo', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationReferenceRange3', 'age', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineObservationReferenceRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationReferenceRange3', nil, js.FHIRFactoryJs);
  defineObservationReferenceRangePropsJs(js, def);
end;

procedure defineObservationRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationRelated3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationRelated3', 'target', 'Reference(Observation|QuestionnaireResponse|Sequence)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineObservationRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationRelated3', nil, js.FHIRFactoryJs);
  defineObservationRelatedPropsJs(js, def);
end;

procedure defineObservationComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationComponent3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent3', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent3', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent3', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent3', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent3', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent3', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent3', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ObservationComponent3', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent3', 'dataAbsentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent3', 'interpretation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent3', 'referenceRange', '@Observation.referenceRange', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineObservationComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationComponent3', nil, js.FHIRFactoryJs);
  defineObservationComponentPropsJs(js, def);
end;

procedure defineObservationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Observation3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation3', 'basedOn', 'Reference(CarePlan|DeviceRequest|ImmunizationRecommendation|MedicationRequest|NutritionOrder|ProcedureRequest|ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'subject', 'Reference(Patient|Group|Device|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation3', 'performer', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Observation3', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation3', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation3', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'dataAbsentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'interpretation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation3', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'specimen', 'Reference(Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'device', 'Reference(Device|DeviceMetric)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation3', 'referenceRange', 'ObservationReferenceRange', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation3', 'related', 'ObservationRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation3', 'component', 'ObservationComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineObservationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Observation3', nil, js.FHIRFactoryJs);
  defineObservationPropsJs(js, def);
end;

procedure defineOperationDefinitionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameter3', 'name', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter3', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter3', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'OperationDefinitionParameter3', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter3', 'searchType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter3', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinitionParameter3', 'binding', 'OperationDefinitionParameterBinding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinitionParameter3', 'part', '@OperationDefinition.parameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameter3', nil, js.FHIRFactoryJs);
  defineOperationDefinitionParameterPropsJs(js, def);
end;

procedure defineOperationDefinitionParameterBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameterBinding3', 'strength', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterBinding3', 'valueSetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterBinding3', 'valueSetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOperationDefinitionParameterBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameterBinding3', nil, js.FHIRFactoryJs);
  defineOperationDefinitionParameterBindingPropsJs(js, def);
end;

procedure defineOperationDefinitionOverloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionOverload3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationDefinitionOverloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionOverload3', nil, js.FHIRFactoryJs);
  defineOperationDefinitionOverloadPropsJs(js, def);
end;

procedure defineOperationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'OperationDefinition3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition3', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OperationDefinition3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition3', 'idempotent', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition3', 'base', 'Reference(OperationDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinition3', 'system', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition3', 'type', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition3', 'instance', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition3', 'parameter', 'OperationDefinitionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition3', 'overload', 'OperationDefinitionOverload', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinition3', nil, js.FHIRFactoryJs);
  defineOperationDefinitionPropsJs(js, def);
end;

procedure defineOperationOutcomeIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationOutcomeIssue3', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue3', 'details', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationOutcomeIssue3', 'diagnostics', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationOutcomeIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcomeIssue3', nil, js.FHIRFactoryJs);
  defineOperationOutcomeIssuePropsJs(js, def);
end;

procedure defineOperationOutcomePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OperationOutcome3', 'issue', 'OperationOutcomeIssue', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationOutcomeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcome3', nil, js.FHIRFactoryJs);
  defineOperationOutcomePropsJs(js, def);
end;

procedure defineOrganizationContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationContact3', 'purpose', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact3', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationContact3', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOrganizationContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationContact3', nil, js.FHIRFactoryJs);
  defineOrganizationContactPropsJs(js, def);
end;

procedure defineOrganizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Organization3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Organization3', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Organization3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization3', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization3', 'partOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Organization3', 'contact', 'OrganizationContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization3', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrganizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Organization3', nil, js.FHIRFactoryJs);
  defineOrganizationPropsJs(js, def);
end;

procedure definePatientContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientContact3', 'relationship', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PatientContact3', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PatientContact3', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact3', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PatientContact3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePatientContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientContact3', nil, js.FHIRFactoryJs);
  definePatientContactPropsJs(js, def);
end;

procedure definePatientAnimalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientAnimal3', 'species', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientAnimal3', 'breed', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientAnimal3', 'genderStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePatientAnimalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientAnimal3', nil, js.FHIRFactoryJs);
  definePatientAnimalPropsJs(js, def);
end;

procedure definePatientCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientCommunication3', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientCommunication3', 'preferred', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure definePatientCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientCommunication3', nil, js.FHIRFactoryJs);
  definePatientCommunicationPropsJs(js, def);
end;

procedure definePatientLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientLink3', 'other', 'Reference(Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientLink3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePatientLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientLink3', nil, js.FHIRFactoryJs);
  definePatientLinkPropsJs(js, def);
end;

procedure definePatientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Patient3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient3', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient3', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Patient3', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Patient3', 'deceasedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient3', 'deceasedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Patient3', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient3', 'maritalStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient3', 'multipleBirthBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient3', 'multipleBirthInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Patient3', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient3', 'contact', 'PatientContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient3', 'animal', 'PatientAnimal', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient3', 'communication', 'PatientCommunication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient3', 'generalPractitioner', 'Reference(Organization|Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient3', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient3', 'link', 'PatientLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePatientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Patient3', nil, js.FHIRFactoryJs);
  definePatientPropsJs(js, def);
end;

procedure definePaymentNoticePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentNotice3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentNotice3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentNotice3', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice3', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice3', 'statusDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice3', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice3', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice3', 'paymentStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePaymentNoticeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentNotice3', nil, js.FHIRFactoryJs);
  definePaymentNoticePropsJs(js, def);
end;

procedure definePaymentReconciliationDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationDetail3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail3', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail3', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail3', 'submitter', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail3', 'payee', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail3', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliationDetail3', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePaymentReconciliationDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationDetail3', nil, js.FHIRFactoryJs);
  definePaymentReconciliationDetailPropsJs(js, def);
end;

procedure definePaymentReconciliationProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationProcessNote3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationProcessNote3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePaymentReconciliationProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationProcessNote3', nil, js.FHIRFactoryJs);
  definePaymentReconciliationProcessNotePropsJs(js, def);
end;

procedure definePaymentReconciliationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliation3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliation3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation3', 'request', 'Reference(ProcessRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation3', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation3', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation3', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation3', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation3', 'detail', 'PaymentReconciliationDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation3', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation3', 'total', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation3', 'processNote', 'PaymentReconciliationProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePaymentReconciliationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliation3', nil, js.FHIRFactoryJs);
  definePaymentReconciliationPropsJs(js, def);
end;

procedure definePersonLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PersonLink3', 'target', 'Reference(Patient|Practitioner|RelatedPerson|Person)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PersonLink3', 'assurance', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePersonLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PersonLink3', nil, js.FHIRFactoryJs);
  definePersonLinkPropsJs(js, def);
end;

procedure definePersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Person3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person3', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person3', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Person3', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Person3', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person3', 'photo', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Person3', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Person3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Person3', 'link', 'PersonLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Person3', nil, js.FHIRFactoryJs);
  definePersonPropsJs(js, def);
end;

procedure definePlanDefinitionGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionGoal3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal3', 'description', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal3', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal3', 'start', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal3', 'addresses', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionGoal3', 'documentation', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionGoal3', 'target', 'PlanDefinitionGoalTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionGoal3', nil, js.FHIRFactoryJs);
  definePlanDefinitionGoalPropsJs(js, def);
end;

procedure definePlanDefinitionGoalTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionGoalTarget3', 'measure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget3', 'detailQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget3', 'detailRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget3', 'detailCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget3', 'due', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionGoalTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionGoalTarget3', nil, js.FHIRFactoryJs);
  definePlanDefinitionGoalTargetPropsJs(js, def);
end;

procedure definePlanDefinitionActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionAction3', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'textEquivalent', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'documentation', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'triggerDefinition', 'TriggerDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'condition', 'PlanDefinitionActionCondition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'input', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'output', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'relatedAction', 'PlanDefinitionActionRelatedAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'timingDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'timingRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'participant', 'PlanDefinitionActionParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'groupingBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'selectionBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'requiredBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'precheckBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'cardinalityBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'definition', 'Reference(ActivityDefinition|PlanDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'transform', 'Reference(StructureMap)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'dynamicValue', 'PlanDefinitionActionDynamicValue', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction3', 'action', '@PlanDefinition.action', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionAction3', nil, js.FHIRFactoryJs);
  definePlanDefinitionActionPropsJs(js, def);
end;

procedure definePlanDefinitionActionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionCondition3', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition3', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition3', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePlanDefinitionActionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionCondition3', nil, js.FHIRFactoryJs);
  definePlanDefinitionActionConditionPropsJs(js, def);
end;

procedure definePlanDefinitionActionRelatedActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction3', 'actionId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction3', 'relationship', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction3', 'offsetDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction3', 'offsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionRelatedActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionRelatedAction3', nil, js.FHIRFactoryJs);
  definePlanDefinitionActionRelatedActionPropsJs(js, def);
end;

procedure definePlanDefinitionActionParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionParticipant3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionParticipant3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionParticipant3', nil, js.FHIRFactoryJs);
  definePlanDefinitionActionParticipantPropsJs(js, def);
end;

procedure definePlanDefinitionActionDynamicValuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue3', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue3', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePlanDefinitionActionDynamicValueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionDynamicValue3', nil, js.FHIRFactoryJs);
  definePlanDefinitionActionDynamicValuePropsJs(js, def);
end;

procedure definePlanDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'PlanDefinition3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PlanDefinition3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition3', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition3', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition3', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition3', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition3', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition3', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition3', 'library', 'Reference(Library)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition3', 'goal', 'PlanDefinitionGoal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition3', 'action', 'PlanDefinitionAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinition3', nil, js.FHIRFactoryJs);
  definePlanDefinitionPropsJs(js, def);
end;

procedure definePractitionerQualificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerQualification3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerQualification3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification3', 'issuer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePractitionerQualificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerQualification3', nil, js.FHIRFactoryJs);
  definePractitionerQualificationPropsJs(js, def);
end;

procedure definePractitionerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Practitioner3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Practitioner3', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner3', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner3', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Practitioner3', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Practitioner3', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner3', 'qualification', 'PractitionerQualification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner3', 'communication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePractitionerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Practitioner3', nil, js.FHIRFactoryJs);
  definePractitionerPropsJs(js, def);
end;

procedure definePractitionerRoleAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerRoleAvailableTime3', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PractitionerRoleAvailableTime3', 'availableStartTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRoleAvailableTime3', 'availableEndTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePractitionerRoleAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRoleAvailableTime3', nil, js.FHIRFactoryJs);
  definePractitionerRoleAvailableTimePropsJs(js, def);
end;

procedure definePractitionerRoleNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerRoleNotAvailable3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRoleNotAvailable3', 'during', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePractitionerRoleNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRoleNotAvailable3', nil, js.FHIRFactoryJs);
  definePractitionerRoleNotAvailablePropsJs(js, def);
end;

procedure definePractitionerRolePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PractitionerRole3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PractitionerRole3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole3', 'practitioner', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole3', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole3', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole3', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole3', 'healthcareService', 'Reference(HealthcareService)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole3', 'availableTime', 'PractitionerRoleAvailableTime', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole3', 'notAvailable', 'PractitionerRoleNotAvailable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole3', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRole3', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePractitionerRoleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRole3', nil, js.FHIRFactoryJs);
  definePractitionerRolePropsJs(js, def);
end;

procedure defineProcedurePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedurePerformer3', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer3', 'actor', 'Reference(Practitioner|Organization|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedurePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedurePerformer3', nil, js.FHIRFactoryJs);
  defineProcedurePerformerPropsJs(js, def);
end;

procedure defineProcedureFocalDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedureFocalDevice3', 'action', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureFocalDevice3', 'manipulated', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedureFocalDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedureFocalDevice3', nil, js.FHIRFactoryJs);
  defineProcedureFocalDevicePropsJs(js, def);
end;

procedure defineProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Procedure3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'definition', 'Reference(PlanDefinition|ActivityDefinition|HealthcareService)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'basedOn', 'Reference(CarePlan|ProcedureRequest|ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'partOf', 'Reference(Procedure|Observation|MedicationAdministration)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Procedure3', 'notDone', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Procedure3', 'notDoneReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure3', 'performedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Procedure3', 'performedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure3', 'performer', 'ProcedurePerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'reasonReference', 'Reference(Condition|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure3', 'report', 'Reference(DiagnosticReport)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'complication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'complicationDetail', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'followUp', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'focalDevice', 'ProcedureFocalDevice', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'usedReference', 'Reference(Device|Medication|Substance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure3', 'usedCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Procedure3', nil, js.FHIRFactoryJs);
  defineProcedurePropsJs(js, def);
end;

procedure defineProcedureRequestRequesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedureRequestRequester3', 'agent', 'Reference(Device|Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequestRequester3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedureRequestRequesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedureRequestRequester3', nil, js.FHIRFactoryJs);
  defineProcedureRequestRequesterPropsJs(js, def);
end;

procedure defineProcedureRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcedureRequest3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'definition', 'Reference(ActivityDefinition|PlanDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'replaces', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'requisition', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcedureRequest3', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcedureRequest3', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcedureRequest3', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ProcedureRequest3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest3', 'subject', 'Reference(Patient|Group|Location|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest3', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcedureRequest3', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest3', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest3', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ProcedureRequest3', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest3', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcedureRequest3', 'requester', 'ProcedureRequestRequester', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest3', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest3', 'performer', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson|HealthcareService)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'reasonReference', 'Reference(Condition|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest3', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcedureRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedureRequest3', nil, js.FHIRFactoryJs);
  defineProcedureRequestPropsJs(js, def);
end;

procedure defineProcessRequestItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessRequestItem3', 'sequenceLinkId', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineProcessRequestItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessRequestItem3', nil, js.FHIRFactoryJs);
  defineProcessRequestItemPropsJs(js, def);
end;

procedure defineProcessRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessRequest3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest3', 'action', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest3', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessRequest3', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest3', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest3', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest3', 'nullify', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ProcessRequest3', 'reference', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest3', 'item', 'ProcessRequestItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcessRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessRequest3', nil, js.FHIRFactoryJs);
  defineProcessRequestPropsJs(js, def);
end;

procedure defineProcessResponseProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessResponseProcessNote3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponseProcessNote3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineProcessResponseProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponseProcessNote3', nil, js.FHIRFactoryJs);
  defineProcessResponseProcessNotePropsJs(js, def);
end;

procedure defineProcessResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessResponse3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse3', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessResponse3', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse3', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse3', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse3', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse3', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse3', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse3', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse3', 'processNote', 'ProcessResponseProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse3', 'error', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse3', 'communicationRequest', 'Reference(CommunicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcessResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponse3', nil, js.FHIRFactoryJs);
  defineProcessResponsePropsJs(js, def);
end;

procedure defineProvenanceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceAgent3', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProvenanceAgent3', 'whoUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProvenanceAgent3', 'whoReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent3', 'onBehalfOfUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProvenanceAgent3', 'onBehalfOfReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent3', 'relatedAgentType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProvenanceAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceAgent3', nil, js.FHIRFactoryJs);
  defineProvenanceAgentPropsJs(js, def);
end;

procedure defineProvenanceEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceEntity3', 'role', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity3', 'whatUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity3', 'whatReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity3', 'whatIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity3', 'agent', '@Provenance.agent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProvenanceEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceEntity3', nil, js.FHIRFactoryJs);
  defineProvenanceEntityPropsJs(js, def);
end;

procedure defineProvenancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Provenance3', 'target', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance3', 'recorded', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance3', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance3', 'reason', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance3', 'activity', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance3', 'agent', 'ProvenanceAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance3', 'entity', 'ProvenanceEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance3', 'signature', 'Signature', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProvenanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Provenance3', nil, js.FHIRFactoryJs);
  defineProvenancePropsJs(js, def);
end;

procedure defineQuestionnaireItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItem3', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem3', 'definition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem3', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem3', 'prefix', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem3', 'enableWhen', 'QuestionnaireItemEnableWhen', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem3', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem3', 'repeats', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem3', 'readOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem3', 'maxLength', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItem3', 'options', 'Reference(ValueSet)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem3', 'option', 'QuestionnaireItemOption', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem3', 'initialReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem3', 'item', '@Questionnaire.item', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItem3', nil, js.FHIRFactoryJs);
  defineQuestionnaireItemPropsJs(js, def);
end;

procedure defineQuestionnaireItemEnableWhenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'question', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'hasAnswer', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen3', 'answerReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineQuestionnaireItemEnableWhenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemEnableWhen3', nil, js.FHIRFactoryJs);
  defineQuestionnaireItemEnableWhenPropsJs(js, def);
end;

procedure defineQuestionnaireItemOptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemOption3', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemOption3', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemOption3', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemOption3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemOption3', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineQuestionnaireItemOptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemOption3', nil, js.FHIRFactoryJs);
  defineQuestionnaireItemOptionPropsJs(js, def);
end;

procedure defineQuestionnairePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Questionnaire3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Questionnaire3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire3', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire3', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Questionnaire3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire3', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire3', 'item', 'QuestionnaireItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Questionnaire3', nil, js.FHIRFactoryJs);
  defineQuestionnairePropsJs(js, def);
end;

procedure defineQuestionnaireResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseItem3', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem3', 'definition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem3', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem3', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItem3', 'answer', 'QuestionnaireResponseItemAnswer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponseItem3', 'item', '@QuestionnaireResponse.item', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseItem3', nil, js.FHIRFactoryJs);
  defineQuestionnaireResponseItemPropsJs(js, def);
end;

procedure defineQuestionnaireResponseItemAnswerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer3', 'item', '@QuestionnaireResponse.item', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseItemAnswerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseItemAnswer3', nil, js.FHIRFactoryJs);
  defineQuestionnaireResponseItemAnswerPropsJs(js, def);
end;

procedure defineQuestionnaireResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponse3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse3', 'basedOn', 'Reference(ReferralRequest|CarePlan|ProcedureRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse3', 'parent', 'Reference(Observation|Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse3', 'questionnaire', 'Reference(Questionnaire)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse3', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse3', 'authored', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponse3', 'author', 'Reference(Device|Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse3', 'source', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse3', 'item', 'QuestionnaireResponseItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponse3', nil, js.FHIRFactoryJs);
  defineQuestionnaireResponsePropsJs(js, def);
end;

procedure defineReferralRequestRequesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ReferralRequestRequester3', 'agent', 'Reference(Practitioner|Organization|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequestRequester3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineReferralRequestRequesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ReferralRequestRequester3', nil, js.FHIRFactoryJs);
  defineReferralRequestRequesterPropsJs(js, def);
end;

procedure defineReferralRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ReferralRequest3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest3', 'definition', 'Reference(ActivityDefinition|PlanDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest3', 'basedOn', 'Reference(ReferralRequest|CarePlan|ProcedureRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest3', 'replaces', 'Reference(ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest3', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ReferralRequest3', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ReferralRequest3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest3', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ReferralRequest3', 'serviceRequested', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest3', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ReferralRequest3', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest3', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ReferralRequest3', 'requester', 'ReferralRequestRequester', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest3', 'specialty', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest3', 'recipient', 'Reference(Practitioner|Organization|HealthcareService)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest3', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest3', 'reasonReference', 'Reference(Condition|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ReferralRequest3', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest3', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineReferralRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ReferralRequest3', nil, js.FHIRFactoryJs);
  defineReferralRequestPropsJs(js, def);
end;

procedure defineRelatedPersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RelatedPerson3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'RelatedPerson3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson3', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson3', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson3', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson3', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedPerson3', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RelatedPerson3', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson3', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRelatedPersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedPerson3', nil, js.FHIRFactoryJs);
  defineRelatedPersonPropsJs(js, def);
end;

procedure defineRequestGroupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupAction3', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction3', 'textEquivalent', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction3', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction3', 'documentation', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction3', 'condition', 'RequestGroupActionCondition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction3', 'relatedAction', 'RequestGroupActionRelatedAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction3', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroupAction3', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction3', 'timingDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction3', 'timingRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction3', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction3', 'participant', 'Reference(Patient|Person|Practitioner|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction3', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction3', 'groupingBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction3', 'selectionBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction3', 'requiredBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction3', 'precheckBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction3', 'cardinalityBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction3', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction3', 'action', '@RequestGroup.action', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRequestGroupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupAction3', nil, js.FHIRFactoryJs);
  defineRequestGroupActionPropsJs(js, def);
end;

procedure defineRequestGroupActionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupActionCondition3', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition3', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition3', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRequestGroupActionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupActionCondition3', nil, js.FHIRFactoryJs);
  defineRequestGroupActionConditionPropsJs(js, def);
end;

procedure defineRequestGroupActionRelatedActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupActionRelatedAction3', 'actionId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction3', 'relationship', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction3', 'offsetDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction3', 'offsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRequestGroupActionRelatedActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupActionRelatedAction3', nil, js.FHIRFactoryJs);
  defineRequestGroupActionRelatedActionPropsJs(js, def);
end;

procedure defineRequestGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RequestGroup3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup3', 'definition', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup3', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup3', 'replaces', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup3', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup3', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup3', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup3', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroup3', 'author', 'Reference(Device|Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup3', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup3', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup3', 'action', 'RequestGroupAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRequestGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroup3', nil, js.FHIRFactoryJs);
  defineRequestGroupPropsJs(js, def);
end;

procedure defineResearchStudyArmPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchStudyArm3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudyArm3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudyArm3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineResearchStudyArmJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudyArm3', nil, js.FHIRFactoryJs);
  defineResearchStudyArmPropsJs(js, def);
end;

procedure defineResearchStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchStudy3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy3', 'protocol', 'Reference(PlanDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'partOf', 'Reference(ResearchStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'focus', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'keyword', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy3', 'enrollment', 'Reference(Group)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy3', 'sponsor', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy3', 'principalInvestigator', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy3', 'site', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'reasonStopped', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy3', 'arm', 'ResearchStudyArm', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineResearchStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudy3', nil, js.FHIRFactoryJs);
  defineResearchStudyPropsJs(js, def);
end;

procedure defineResearchSubjectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchSubject3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject3', 'study', 'Reference(ResearchStudy)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject3', 'individual', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject3', 'assignedArm', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject3', 'actualArm', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject3', 'consent', 'Reference(Consent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineResearchSubjectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchSubject3', nil, js.FHIRFactoryJs);
  defineResearchSubjectPropsJs(js, def);
end;

procedure defineRiskAssessmentPredictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskAssessmentPrediction3', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction3', 'probabilityDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction3', 'probabilityRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction3', 'qualitativeRisk', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction3', 'relativeRisk', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction3', 'whenPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction3', 'whenRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction3', 'rationale', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRiskAssessmentPredictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessmentPrediction3', nil, js.FHIRFactoryJs);
  defineRiskAssessmentPredictionPropsJs(js, def);
end;

procedure defineRiskAssessmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RiskAssessment3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'basedOn', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'parent', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment3', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskAssessment3', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'condition', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'performer', 'Reference(Practitioner|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment3', 'basis', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment3', 'prediction', 'RiskAssessmentPrediction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment3', 'mitigation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRiskAssessmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessment3', nil, js.FHIRFactoryJs);
  defineRiskAssessmentPropsJs(js, def);
end;

procedure defineSchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Schedule3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule3', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Schedule3', 'serviceCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Schedule3', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule3', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule3', 'actor', 'Reference(Patient|Practitioner|PractitionerRole|RelatedPerson|Device|HealthcareService|Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule3', 'planningHorizon', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Schedule3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineScheduleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Schedule3', nil, js.FHIRFactoryJs);
  defineSchedulePropsJs(js, def);
end;

procedure defineSearchParameterComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SearchParameterComponent3', 'definition', 'Reference(SearchParameter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SearchParameterComponent3', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSearchParameterComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameterComponent3', nil, js.FHIRFactoryJs);
  defineSearchParameterComponentPropsJs(js, def);
end;

procedure defineSearchParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'SearchParameter3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SearchParameter3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'derivedFrom', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'xpath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'xpathUsage', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter3', 'component', 'SearchParameterComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSearchParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameter3', nil, js.FHIRFactoryJs);
  defineSearchParameterPropsJs(js, def);
end;

procedure defineSequenceReferenceSeqPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceReferenceSeq3', 'chromosome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq3', 'genomeBuild', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceReferenceSeq3', 'referenceSeqId', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq3', 'referenceSeqPointer', 'Reference(Sequence)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq3', 'referenceSeqString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceReferenceSeq3', 'strand', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceReferenceSeq3', 'windowStart', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceReferenceSeq3', 'windowEnd', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineSequenceReferenceSeqJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceReferenceSeq3', nil, js.FHIRFactoryJs);
  defineSequenceReferenceSeqPropsJs(js, def);
end;

procedure defineSequenceVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceVariant3', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceVariant3', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceVariant3', 'observedAllele', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant3', 'referenceAllele', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant3', 'cigar', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant3', 'variantPointer', 'Reference(Observation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSequenceVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceVariant3', nil, js.FHIRFactoryJs);
  defineSequenceVariantPropsJs(js, def);
end;

procedure defineSequenceQualityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceQuality3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceQuality3', 'standardSequence', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality3', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceQuality3', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SequenceQuality3', 'score', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality3', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality3', 'truthTP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality3', 'queryTP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality3', 'truthFN', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality3', 'queryFP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality3', 'gtFP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality3', 'precision', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality3', 'recall', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality3', 'fScore', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineSequenceQualityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceQuality3', nil, js.FHIRFactoryJs);
  defineSequenceQualityPropsJs(js, def);
end;

procedure defineSequenceRepositoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceRepository3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository3', 'datasetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository3', 'variantsetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository3', 'readsetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSequenceRepositoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceRepository3', nil, js.FHIRFactoryJs);
  defineSequenceRepositoryPropsJs(js, def);
end;

procedure defineSequencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Sequence3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Sequence3', 'coordinateSystem', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Sequence3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence3', 'specimen', 'Reference(Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence3', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence3', 'performer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence3', 'referenceSeq', 'SequenceReferenceSeq', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Sequence3', 'variant', 'SequenceVariant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence3', 'observedSeq', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Sequence3', 'quality', 'SequenceQuality', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence3', 'readCoverage', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Sequence3', 'repository', 'SequenceRepository', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Sequence3', 'pointer', 'Reference(Sequence)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSequenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Sequence3', nil, js.FHIRFactoryJs);
  defineSequencePropsJs(js, def);
end;

procedure defineServiceDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ServiceDefinition3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ServiceDefinition3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceDefinition3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition3', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition3', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceDefinition3', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceDefinition3', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceDefinition3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition3', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition3', 'contributor', 'Contributor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition3', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition3', 'trigger', 'TriggerDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition3', 'dataRequirement', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition3', 'operationDefinition', 'Reference(OperationDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineServiceDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ServiceDefinition3', nil, js.FHIRFactoryJs);
  defineServiceDefinitionPropsJs(js, def);
end;

procedure defineSlotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Slot3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot3', 'serviceCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot3', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot3', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot3', 'appointmentType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot3', 'schedule', 'Reference(Schedule)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Slot3', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Slot3', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Slot3', 'overbooked', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Slot3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSlotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Slot3', nil, js.FHIRFactoryJs);
  defineSlotPropsJs(js, def);
end;

procedure defineSpecimenCollectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenCollection3', 'collector', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection3', 'collectedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenCollection3', 'collectedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection3', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection3', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenCollectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenCollection3', nil, js.FHIRFactoryJs);
  defineSpecimenCollectionPropsJs(js, def);
end;

procedure defineSpecimenProcessingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenProcessing3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenProcessing3', 'procedure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenProcessing3', 'additive', 'Reference(Substance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenProcessing3', 'timeDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenProcessing3', 'timePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenProcessingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenProcessing3', nil, js.FHIRFactoryJs);
  defineSpecimenProcessingPropsJs(js, def);
end;

procedure defineSpecimenContainerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenContainer3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenContainer3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenContainer3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer3', 'capacity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer3', 'specimenQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer3', 'additiveCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer3', 'additiveReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenContainerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenContainer3', nil, js.FHIRFactoryJs);
  defineSpecimenContainerPropsJs(js, def);
end;

procedure defineSpecimenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Specimen3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen3', 'accessionIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Specimen3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen3', 'subject', 'Reference(Patient|Group|Device|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen3', 'receivedTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Specimen3', 'parent', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen3', 'request', 'Reference(ProcedureRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen3', 'collection', 'SpecimenCollection', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen3', 'processing', 'SpecimenProcessing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen3', 'container', 'SpecimenContainer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Specimen3', nil, js.FHIRFactoryJs);
  defineSpecimenPropsJs(js, def);
end;

procedure defineStructureDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionMapping3', 'identity', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping3', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping3', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionMapping3', nil, js.FHIRFactoryJs);
  defineStructureDefinitionMappingPropsJs(js, def);
end;

procedure defineStructureDefinitionSnapshotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionSnapshot3', 'element', 'ElementDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionSnapshotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionSnapshot3', nil, js.FHIRFactoryJs);
  defineStructureDefinitionSnapshotPropsJs(js, def);
end;

procedure defineStructureDefinitionDifferentialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionDifferential3', 'element', 'ElementDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionDifferentialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionDifferential3', nil, js.FHIRFactoryJs);
  defineStructureDefinitionDifferentialPropsJs(js, def);
end;

procedure defineStructureDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'StructureDefinition3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureDefinition3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'keyword', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition3', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'mapping', 'StructureDefinitionMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition3', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'abstract', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition3', 'contextType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'baseDefinition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'derivation', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition3', 'snapshot', 'StructureDefinitionSnapshot', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureDefinition3', 'differential', 'StructureDefinitionDifferential', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineStructureDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinition3', nil, js.FHIRFactoryJs);
  defineStructureDefinitionPropsJs(js, def);
end;

procedure defineStructureMapStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapStructure3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure3', 'alias', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapStructure3', nil, js.FHIRFactoryJs);
  defineStructureMapStructurePropsJs(js, def);
end;

procedure defineStructureMapGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroup3', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup3', 'extends', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup3', 'typeMode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup3', 'input', 'StructureMapGroupInput', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroup3', 'rule', 'StructureMapGroupRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroup3', nil, js.FHIRFactoryJs);
  defineStructureMapGroupPropsJs(js, def);
end;

procedure defineStructureMapGroupInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupInput3', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput3', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput3', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupInput3', nil, js.FHIRFactoryJs);
  defineStructureMapGroupInputPropsJs(js, def);
end;

procedure defineStructureMapGroupRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRule3', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRule3', 'source', 'StructureMapGroupRuleSource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule3', 'target', 'StructureMapGroupRuleTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule3', 'rule', '@StructureMap.group.rule', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule3', 'dependent', 'StructureMapGroupRuleDependent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule3', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRule3', nil, js.FHIRFactoryJs);
  defineStructureMapGroupRulePropsJs(js, def);
end;

procedure defineStructureMapGroupRuleSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'context', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'defaultValueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'element', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'listMode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'variable', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'condition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource3', 'check', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleSource3', nil, js.FHIRFactoryJs);
  defineStructureMapGroupRuleSourcePropsJs(js, def);
end;

procedure defineStructureMapGroupRuleTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleTarget3', 'context', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget3', 'contextType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget3', 'element', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget3', 'variable', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget3', 'listRuleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget3', 'transform', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget3', 'parameter', 'StructureMapGroupRuleTargetParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapGroupRuleTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleTarget3', nil, js.FHIRFactoryJs);
  defineStructureMapGroupRuleTargetPropsJs(js, def);
end;

procedure defineStructureMapGroupRuleTargetParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter3', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter3', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter3', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineStructureMapGroupRuleTargetParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleTargetParameter3', nil, js.FHIRFactoryJs);
  defineStructureMapGroupRuleTargetParameterPropsJs(js, def);
end;

procedure defineStructureMapGroupRuleDependentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleDependent3', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleDependentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleDependent3', nil, js.FHIRFactoryJs);
  defineStructureMapGroupRuleDependentPropsJs(js, def);
end;

procedure defineStructureMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'StructureMap3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMap3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMap3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap3', 'structure', 'StructureMapStructure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap3', 'group', 'StructureMapGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMap3', nil, js.FHIRFactoryJs);
  defineStructureMapPropsJs(js, def);
end;

procedure defineSubscriptionChannelPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubscriptionChannel3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel3', 'endpoint', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel3', 'payload', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubscriptionChannelJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubscriptionChannel3', nil, js.FHIRFactoryJs);
  defineSubscriptionChannelPropsJs(js, def);
end;

procedure defineSubscriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Subscription3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription3', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Subscription3', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Subscription3', 'reason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription3', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription3', 'error', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription3', 'channel', 'SubscriptionChannel', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Subscription3', 'tag', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubscriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Subscription3', nil, js.FHIRFactoryJs);
  defineSubscriptionPropsJs(js, def);
end;

procedure defineSubstanceInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceInstance3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceInstance3', 'expiry', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceInstance3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceInstance3', nil, js.FHIRFactoryJs);
  defineSubstanceInstancePropsJs(js, def);
end;

procedure defineSubstanceIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceIngredient3', 'quantity', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient3', 'substanceCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient3', 'substanceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceIngredient3', nil, js.FHIRFactoryJs);
  defineSubstanceIngredientPropsJs(js, def);
end;

procedure defineSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Substance3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Substance3', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Substance3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Substance3', 'instance', 'SubstanceInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance3', 'ingredient', 'SubstanceIngredient', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Substance3', nil, js.FHIRFactoryJs);
  defineSubstancePropsJs(js, def);
end;

procedure defineSupplyDeliverySuppliedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyDeliverySuppliedItem3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDeliverySuppliedItem3', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDeliverySuppliedItem3', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyDeliverySuppliedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDeliverySuppliedItem3', nil, js.FHIRFactoryJs);
  defineSupplyDeliverySuppliedItemPropsJs(js, def);
end;

procedure defineSupplyDeliveryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyDelivery3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery3', 'basedOn', 'Reference(SupplyRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery3', 'partOf', 'Reference(SupplyDelivery|Contract)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyDelivery3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery3', 'suppliedItem', 'SupplyDeliverySuppliedItem', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery3', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyDelivery3', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery3', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery3', 'supplier', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery3', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery3', 'receiver', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSupplyDeliveryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDelivery3', nil, js.FHIRFactoryJs);
  defineSupplyDeliveryPropsJs(js, def);
end;

procedure defineSupplyRequestOrderedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyRequestOrderedItem3', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestOrderedItem3', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestOrderedItem3', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyRequestOrderedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequestOrderedItem3', nil, js.FHIRFactoryJs);
  defineSupplyRequestOrderedItemPropsJs(js, def);
end;

procedure defineSupplyRequestRequesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyRequestRequester3', 'agent', 'Reference(Practitioner|Organization|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestRequester3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyRequestRequesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequestRequester3', nil, js.FHIRFactoryJs);
  defineSupplyRequestRequesterPropsJs(js, def);
end;

procedure defineSupplyRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyRequest3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest3', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest3', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest3', 'orderedItem', 'SupplyRequestOrderedItem', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest3', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest3', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest3', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest3', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest3', 'requester', 'SupplyRequestRequester', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest3', 'supplier', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest3', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest3', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest3', 'deliverFrom', 'Reference(Organization|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest3', 'deliverTo', 'Reference(Organization|Location|Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequest3', nil, js.FHIRFactoryJs);
  defineSupplyRequestPropsJs(js, def);
end;

procedure defineTaskRequesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskRequester3', 'agent', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskRequester3', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTaskRequesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskRequester3', nil, js.FHIRFactoryJs);
  defineTaskRequesterPropsJs(js, def);
end;

procedure defineTaskRestrictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskRestriction3', 'repetitions', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskRestriction3', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskRestriction3', 'recipient', 'Reference(Patient|Practitioner|RelatedPerson|Group|Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTaskRestrictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskRestriction3', nil, js.FHIRFactoryJs);
  defineTaskRestrictionPropsJs(js, def);
end;

procedure defineTaskInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskInput3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'TaskInput3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TaskInput3', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput3', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput3', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput3', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TaskInput3', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput3', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput3', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskInput3', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput3', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput3', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskInput3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput3', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput3', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput3', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput3', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput3', 'valueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTaskInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskInput3', nil, js.FHIRFactoryJs);
  defineTaskInputPropsJs(js, def);
end;

procedure defineTaskOutputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskOutput3', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'TaskOutput3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TaskOutput3', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput3', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput3', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput3', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TaskOutput3', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput3', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput3', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskOutput3', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput3', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput3', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskOutput3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput3', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput3', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput3', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput3', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput3', 'valueMeta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTaskOutputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskOutput3', nil, js.FHIRFactoryJs);
  defineTaskOutputPropsJs(js, def);
end;

procedure defineTaskPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Task3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task3', 'definitionUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task3', 'definitionReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task3', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'partOf', 'Reference(Task)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task3', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'businessStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task3', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task3', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task3', 'focus', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'for', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'executionPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Task3', 'lastModified', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Task3', 'requester', 'TaskRequester', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'performerType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task3', 'owner', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task3', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task3', 'restriction', 'TaskRestriction', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task3', 'input', 'TaskInput', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task3', 'output', 'TaskOutput', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTaskJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Task3', nil, js.FHIRFactoryJs);
  defineTaskPropsJs(js, def);
end;

procedure defineTestReportParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportParticipant3', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportParticipant3', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportParticipant3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportParticipant3', nil, js.FHIRFactoryJs);
  defineTestReportParticipantPropsJs(js, def);
end;

procedure defineTestReportSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetup3', 'action', 'TestReportSetupAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetup3', nil, js.FHIRFactoryJs);
  defineTestReportSetupPropsJs(js, def);
end;

procedure defineTestReportSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupAction3', 'operation', 'TestReportSetupActionOperation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReportSetupAction3', 'assert', 'TestReportSetupActionAssert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupAction3', nil, js.FHIRFactoryJs);
  defineTestReportSetupActionPropsJs(js, def);
end;

procedure defineTestReportSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupActionOperation3', 'result', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionOperation3', 'message', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionOperation3', 'detail', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupActionOperation3', nil, js.FHIRFactoryJs);
  defineTestReportSetupActionOperationPropsJs(js, def);
end;

procedure defineTestReportSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupActionAssert3', 'result', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionAssert3', 'message', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionAssert3', 'detail', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupActionAssert3', nil, js.FHIRFactoryJs);
  defineTestReportSetupActionAssertPropsJs(js, def);
end;

procedure defineTestReportTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTest3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportTest3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportTest3', 'action', 'TestReportTestAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTest3', nil, js.FHIRFactoryJs);
  defineTestReportTestPropsJs(js, def);
end;

procedure defineTestReportTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTestAction3', 'operation', '@TestReport.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReportTestAction3', 'assert', '@TestReport.setup.action.assert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTestAction3', nil, js.FHIRFactoryJs);
  defineTestReportTestActionPropsJs(js, def);
end;

procedure defineTestReportTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTeardown3', 'action', 'TestReportTeardownAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTeardown3', nil, js.FHIRFactoryJs);
  defineTestReportTeardownPropsJs(js, def);
end;

procedure defineTestReportTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTeardownAction3', 'operation', '@TestReport.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTeardownAction3', nil, js.FHIRFactoryJs);
  defineTestReportTeardownActionPropsJs(js, def);
end;

procedure defineTestReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'TestReport3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport3', 'testScript', 'Reference(TestScript)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport3', 'result', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport3', 'score', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TestReport3', 'tester', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport3', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TestReport3', 'participant', 'TestReportParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestReport3', 'setup', 'TestReportSetup', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport3', 'test', 'TestReportTest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestReport3', 'teardown', 'TestReportTeardown', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReport3', nil, js.FHIRFactoryJs);
  defineTestReportPropsJs(js, def);
end;

procedure defineTestScriptOriginPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptOrigin3', 'index', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptOrigin3', 'profile', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptOriginJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptOrigin3', nil, js.FHIRFactoryJs);
  defineTestScriptOriginPropsJs(js, def);
end;

procedure defineTestScriptDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptDestination3', 'index', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptDestination3', 'profile', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptDestination3', nil, js.FHIRFactoryJs);
  defineTestScriptDestinationPropsJs(js, def);
end;

procedure defineTestScriptMetadataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadata3', 'link', 'TestScriptMetadataLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScriptMetadata3', 'capability', 'TestScriptMetadataCapability', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptMetadataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadata3', nil, js.FHIRFactoryJs);
  defineTestScriptMetadataPropsJs(js, def);
end;

procedure defineTestScriptMetadataLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataLink3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataLink3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptMetadataLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataLink3', nil, js.FHIRFactoryJs);
  defineTestScriptMetadataLinkPropsJs(js, def);
end;

procedure defineTestScriptMetadataCapabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataCapability3', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability3', 'validated', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataCapability3', 'destination', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptMetadataCapability3', 'capabilities', 'Reference(CapabilityStatement)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptMetadataCapabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataCapability3', nil, js.FHIRFactoryJs);
  defineTestScriptMetadataCapabilityPropsJs(js, def);
end;

procedure defineTestScriptFixturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptFixture3', 'autocreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture3', 'autodelete', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture3', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptFixtureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptFixture3', nil, js.FHIRFactoryJs);
  defineTestScriptFixturePropsJs(js, def);
end;

procedure defineTestScriptVariablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptVariable3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable3', 'defaultValue', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable3', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable3', 'headerField', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable3', 'hint', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable3', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptVariableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptVariable3', nil, js.FHIRFactoryJs);
  defineTestScriptVariablePropsJs(js, def);
end;

procedure defineTestScriptRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRule3', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptRule3', 'param', 'TestScriptRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRule3', nil, js.FHIRFactoryJs);
  defineTestScriptRulePropsJs(js, def);
end;

procedure defineTestScriptRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRuleParam3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptRuleParam3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRuleParam3', nil, js.FHIRFactoryJs);
  defineTestScriptRuleParamPropsJs(js, def);
end;

procedure defineTestScriptRulesetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRuleset3', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptRuleset3', 'rule', 'TestScriptRulesetRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptRulesetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRuleset3', nil, js.FHIRFactoryJs);
  defineTestScriptRulesetPropsJs(js, def);
end;

procedure defineTestScriptRulesetRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRulesetRule3', 'ruleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptRulesetRule3', 'param', 'TestScriptRulesetRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptRulesetRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRulesetRule3', nil, js.FHIRFactoryJs);
  defineTestScriptRulesetRulePropsJs(js, def);
end;

procedure defineTestScriptRulesetRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRulesetRuleParam3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptRulesetRuleParam3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptRulesetRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRulesetRuleParam3', nil, js.FHIRFactoryJs);
  defineTestScriptRulesetRuleParamPropsJs(js, def);
end;

procedure defineTestScriptSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetup3', 'action', 'TestScriptSetupAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetup3', nil, js.FHIRFactoryJs);
  defineTestScriptSetupPropsJs(js, def);
end;

procedure defineTestScriptSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupAction3', 'operation', 'TestScriptSetupActionOperation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupAction3', 'assert', 'TestScriptSetupActionAssert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupAction3', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionPropsJs(js, def);
end;

procedure defineTestScriptSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'resource', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'accept', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'destination', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'encodeRequestUrl', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'origin', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'params', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'requestHeader', 'TestScriptSetupActionOperationRequestHeader', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'requestId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'responseId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'targetId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation3', 'url', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperation3', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionOperationPropsJs(js, def);
end;

procedure defineTestScriptSetupActionOperationRequestHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader3', 'field', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationRequestHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperationRequestHeader3', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionOperationRequestHeaderPropsJs(js, def);
end;

procedure defineTestScriptSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'direction', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'compareToSourceId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'compareToSourceExpression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'compareToSourcePath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'headerField', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'minimumId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'navigationLinks', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'operator', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'requestMethod', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'requestURL', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'resource', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'response', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'responseCode', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'rule', 'TestScriptSetupActionAssertRule', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'ruleset', 'TestScriptSetupActionAssertRuleset', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'validateProfileId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert3', 'warningOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTestScriptSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssert3', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertPropsJs(js, def);
end;

procedure defineTestScriptSetupActionAssertRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRule3', 'ruleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRule3', 'param', 'TestScriptSetupActionAssertRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRule3', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulePropsJs(js, def);
end;

procedure defineTestScriptSetupActionAssertRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleParam3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleParam3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionAssertRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRuleParam3', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRuleParamPropsJs(js, def);
end;

procedure defineTestScriptSetupActionAssertRulesetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleset3', 'rulesetId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleset3', 'rule', 'TestScriptSetupActionAssertRulesetRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRulesetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRuleset3', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetPropsJs(js, def);
end;

procedure defineTestScriptSetupActionAssertRulesetRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRule3', 'ruleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRule3', 'param', 'TestScriptSetupActionAssertRulesetRuleParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRulesetRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRulesetRule3', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetRulePropsJs(js, def);
end;

procedure defineTestScriptSetupActionAssertRulesetRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRuleParam3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRuleParam3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionAssertRulesetRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRulesetRuleParam3', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetRuleParamPropsJs(js, def);
end;

procedure defineTestScriptTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTest3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest3', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest3', 'action', 'TestScriptTestAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTest3', nil, js.FHIRFactoryJs);
  defineTestScriptTestPropsJs(js, def);
end;

procedure defineTestScriptTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTestAction3', 'operation', '@TestScript.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptTestAction3', 'assert', '@TestScript.setup.action.assert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTestAction3', nil, js.FHIRFactoryJs);
  defineTestScriptTestActionPropsJs(js, def);
end;

procedure defineTestScriptTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardown3', 'action', 'TestScriptTeardownAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardown3', nil, js.FHIRFactoryJs);
  defineTestScriptTeardownPropsJs(js, def);
end;

procedure defineTestScriptTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardownAction3', 'operation', '@TestScript.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardownAction3', nil, js.FHIRFactoryJs);
  defineTestScriptTeardownActionPropsJs(js, def);
end;

procedure defineTestScriptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'TestScript3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript3', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScript3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TestScript3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript3', 'origin', 'TestScriptOrigin', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'destination', 'TestScriptDestination', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'metadata', 'TestScriptMetadata', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript3', 'fixture', 'TestScriptFixture', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'profile', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'variable', 'TestScriptVariable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'rule', 'TestScriptRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'ruleset', 'TestScriptRuleset', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'setup', 'TestScriptSetup', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript3', 'test', 'TestScriptTest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript3', 'teardown', 'TestScriptTeardown', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScript3', nil, js.FHIRFactoryJs);
  defineTestScriptPropsJs(js, def);
end;

procedure defineValueSetComposePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCompose3', 'lockedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetCompose3', 'inactive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetCompose3', 'include', 'ValueSetComposeInclude', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetCompose3', 'exclude', '@ValueSet.compose.include', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCompose3', nil, js.FHIRFactoryJs);
  defineValueSetComposePropsJs(js, def);
end;

procedure defineValueSetComposeIncludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeInclude3', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude3', 'concept', 'ValueSetComposeIncludeConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetComposeInclude3', 'filter', 'ValueSetComposeIncludeFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeInclude3', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludePropsJs(js, def);
end;

procedure defineValueSetComposeIncludeConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConcept3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept3', 'designation', 'ValueSetComposeIncludeConceptDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConcept3', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludeConceptPropsJs(js, def);
end;

procedure defineValueSetComposeIncludeConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation3', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation3', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation3', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConceptDesignation3', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludeConceptDesignationPropsJs(js, def);
end;

procedure defineValueSetComposeIncludeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeFilter3', 'property', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter3', 'op', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter3', 'value', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeFilter3', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludeFilterPropsJs(js, def);
end;

procedure defineValueSetExpansionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansion3', 'identifier', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansion3', 'timestamp', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetExpansion3', 'total', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion3', 'offset', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion3', 'parameter', 'ValueSetExpansionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansion3', 'contains', 'ValueSetExpansionContains', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansion3', nil, js.FHIRFactoryJs);
  defineValueSetExpansionPropsJs(js, def);
end;

procedure defineValueSetExpansionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionParameter3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter3', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter3', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionParameter3', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansionParameter3', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ValueSetExpansionParameter3', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter3', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetExpansionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionParameter3', nil, js.FHIRFactoryJs);
  defineValueSetExpansionParameterPropsJs(js, def);
end;

procedure defineValueSetExpansionContainsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionContains3', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains3', 'abstract', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains3', 'inactive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains3', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains3', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains3', 'designation', '@ValueSet.compose.include.concept.designation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansionContains3', 'contains', '@ValueSet.expansion.contains', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetExpansionContainsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionContains3', nil, js.FHIRFactoryJs);
  defineValueSetExpansionContainsPropsJs(js, def);
end;

procedure defineValueSetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ValueSet3', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet3', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet3', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet3', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet3', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet3', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSet3', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet3', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet3', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet3', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet3', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet3', 'immutable', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet3', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet3', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet3', 'extensible', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet3', 'compose', 'ValueSetCompose', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSet3', 'expansion', 'ValueSetExpansion', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineValueSetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSet3', nil, js.FHIRFactoryJs);
  defineValueSetPropsJs(js, def);
end;

procedure defineVisionPrescriptionDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'product', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'eye', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'sphere', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'cylinder', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'axis', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'prism', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'base', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'add', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'power', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'backCurve', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'diameter', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'duration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'color', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'brand', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense3', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVisionPrescriptionDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescriptionDispense3', nil, js.FHIRFactoryJs);
  defineVisionPrescriptionDispensePropsJs(js, def);
end;

procedure defineVisionPrescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VisionPrescription3', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VisionPrescription3', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescription3', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription3', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription3', 'dateWritten', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VisionPrescription3', 'prescriber', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription3', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription3', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription3', 'dispense', 'VisionPrescriptionDispense', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVisionPrescriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescription3', nil, js.FHIRFactoryJs);
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
  defineIdentifierJs(js); 
  defineCodingJs(js); 
  defineSampledDataJs(js); 
  defineRatioJs(js); 
  defineReferenceJs(js); 
  defineTriggerDefinitionJs(js); 
  definePeriodJs(js); 
  defineQuantityJs(js); 
  defineRangeJs(js); 
  defineRelatedArtifactJs(js); 
  defineAnnotationJs(js); 
  defineContactDetailJs(js); 
  defineUsageContextJs(js); 
  defineSignatureJs(js); 
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
  defineBodySiteJs(js); 
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
  defineCapabilityStatementRestInteractionJs(js); 
  defineCapabilityStatementRestOperationJs(js); 
  defineCapabilityStatementMessagingJs(js); 
  defineCapabilityStatementMessagingEndpointJs(js); 
  defineCapabilityStatementMessagingSupportedMessageJs(js); 
  defineCapabilityStatementMessagingEventJs(js); 
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
  defineClaimResponseAddItemDetailJs(js); 
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
  defineCommunicationRequestRequesterJs(js); 
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
  defineConsentActorJs(js); 
  defineConsentPolicyJs(js); 
  defineConsentDataJs(js); 
  defineConsentExceptJs(js); 
  defineConsentExceptActorJs(js); 
  defineConsentExceptDataJs(js); 
  defineConsentJs(js); 
  defineContractAgentJs(js); 
  defineContractSignerJs(js); 
  defineContractValuedItemJs(js); 
  defineContractTermJs(js); 
  defineContractTermAgentJs(js); 
  defineContractTermValuedItemJs(js); 
  defineContractFriendlyJs(js); 
  defineContractLegalJs(js); 
  defineContractRuleJs(js); 
  defineContractJs(js); 
  defineCoverageGroupingJs(js); 
  defineCoverageJs(js); 
  defineDataElementMappingJs(js); 
  defineDataElementJs(js); 
  defineDetectedIssueMitigationJs(js); 
  defineDetectedIssueJs(js); 
  defineDeviceUdiJs(js); 
  defineDeviceJs(js); 
  defineDeviceComponentProductionSpecificationJs(js); 
  defineDeviceComponentJs(js); 
  defineDeviceMetricCalibrationJs(js); 
  defineDeviceMetricJs(js); 
  defineDeviceRequestRequesterJs(js); 
  defineDeviceRequestJs(js); 
  defineDeviceUseStatementJs(js); 
  defineDiagnosticReportPerformerJs(js); 
  defineDiagnosticReportImageJs(js); 
  defineDiagnosticReportJs(js); 
  defineDocumentManifestContentJs(js); 
  defineDocumentManifestRelatedJs(js); 
  defineDocumentManifestJs(js); 
  defineDocumentReferenceRelatesToJs(js); 
  defineDocumentReferenceContentJs(js); 
  defineDocumentReferenceContextJs(js); 
  defineDocumentReferenceContextRelatedJs(js); 
  defineDocumentReferenceJs(js); 
  defineEligibilityRequestJs(js); 
  defineEligibilityResponseInsuranceJs(js); 
  defineEligibilityResponseInsuranceBenefitBalanceJs(js); 
  defineEligibilityResponseInsuranceBenefitBalanceFinancialJs(js); 
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
  defineEpisodeOfCareStatusHistoryJs(js); 
  defineEpisodeOfCareDiagnosisJs(js); 
  defineEpisodeOfCareJs(js); 
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
  defineExplanationOfBenefitAddItemDetailJs(js); 
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
  defineImagingManifestStudyJs(js); 
  defineImagingManifestStudySeriesJs(js); 
  defineImagingManifestStudySeriesInstanceJs(js); 
  defineImagingManifestJs(js); 
  defineImagingStudySeriesJs(js); 
  defineImagingStudySeriesInstanceJs(js); 
  defineImagingStudyJs(js); 
  defineImmunizationPractitionerJs(js); 
  defineImmunizationExplanationJs(js); 
  defineImmunizationReactionJs(js); 
  defineImmunizationVaccinationProtocolJs(js); 
  defineImmunizationJs(js); 
  defineImmunizationRecommendationRecommendationJs(js); 
  defineImmunizationRecommendationRecommendationDateCriterionJs(js); 
  defineImmunizationRecommendationRecommendationProtocolJs(js); 
  defineImmunizationRecommendationJs(js); 
  defineImplementationGuideDependencyJs(js); 
  defineImplementationGuidePackageJs(js); 
  defineImplementationGuidePackageResourceJs(js); 
  defineImplementationGuideGlobalJs(js); 
  defineImplementationGuidePageJs(js); 
  defineImplementationGuideJs(js); 
  defineLibraryJs(js); 
  defineLinkageItemJs(js); 
  defineLinkageJs(js); 
  defineListEntryJs(js); 
  defineListJs(js); 
  defineLocationPositionJs(js); 
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
  defineMedicationPackageJs(js); 
  defineMedicationPackageContentJs(js); 
  defineMedicationPackageBatchJs(js); 
  defineMedicationJs(js); 
  defineMedicationAdministrationPerformerJs(js); 
  defineMedicationAdministrationDosageJs(js); 
  defineMedicationAdministrationJs(js); 
  defineMedicationDispensePerformerJs(js); 
  defineMedicationDispenseSubstitutionJs(js); 
  defineMedicationDispenseJs(js); 
  defineMedicationRequestRequesterJs(js); 
  defineMedicationRequestDispenseRequestJs(js); 
  defineMedicationRequestSubstitutionJs(js); 
  defineMedicationRequestJs(js); 
  defineMedicationStatementJs(js); 
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
  defineObservationRelatedJs(js); 
  defineObservationComponentJs(js); 
  defineObservationJs(js); 
  defineOperationDefinitionParameterJs(js); 
  defineOperationDefinitionParameterBindingJs(js); 
  defineOperationDefinitionOverloadJs(js); 
  defineOperationDefinitionJs(js); 
  defineOperationOutcomeIssueJs(js); 
  defineOperationOutcomeJs(js); 
  defineOrganizationContactJs(js); 
  defineOrganizationJs(js); 
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
  defineProcedureRequestRequesterJs(js); 
  defineProcedureRequestJs(js); 
  defineProcessRequestItemJs(js); 
  defineProcessRequestJs(js); 
  defineProcessResponseProcessNoteJs(js); 
  defineProcessResponseJs(js); 
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
  defineReferralRequestRequesterJs(js); 
  defineReferralRequestJs(js); 
  defineRelatedPersonJs(js); 
  defineRequestGroupActionJs(js); 
  defineRequestGroupActionConditionJs(js); 
  defineRequestGroupActionRelatedActionJs(js); 
  defineRequestGroupJs(js); 
  defineResearchStudyArmJs(js); 
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
  defineSequenceRepositoryJs(js); 
  defineSequenceJs(js); 
  defineServiceDefinitionJs(js); 
  defineSlotJs(js); 
  defineSpecimenCollectionJs(js); 
  defineSpecimenProcessingJs(js); 
  defineSpecimenContainerJs(js); 
  defineSpecimenJs(js); 
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
  defineSupplyDeliverySuppliedItemJs(js); 
  defineSupplyDeliveryJs(js); 
  defineSupplyRequestOrderedItemJs(js); 
  defineSupplyRequestRequesterJs(js); 
  defineSupplyRequestJs(js); 
  defineTaskRequesterJs(js); 
  defineTaskRestrictionJs(js); 
  defineTaskInputJs(js); 
  defineTaskOutputJs(js); 
  defineTaskJs(js); 
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
  defineValueSetComposeJs(js); 
  defineValueSetComposeIncludeJs(js); 
  defineValueSetComposeIncludeConceptJs(js); 
  defineValueSetComposeIncludeConceptDesignationJs(js); 
  defineValueSetComposeIncludeFilterJs(js); 
  defineValueSetExpansionJs(js); 
  defineValueSetExpansionParameterJs(js); 
  defineValueSetExpansionContainsJs(js); 
  defineValueSetJs(js); 
  defineVisionPrescriptionDispenseJs(js); 
  defineVisionPrescriptionJs(js); 

end;

end.

