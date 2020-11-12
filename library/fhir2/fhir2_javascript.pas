unit fhir2_javascript;

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
{$I fhir2.inc}

interface

// FHIR v1.0.2 generated 2015-10-24T07:41:03+11:00

uses
  fsl_javascript, fhir_javascript;

procedure registerFHIRTypes(js : TFHIRJavascript);

implementation

procedure defineElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Element2', 'id', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Element2', 'extension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineBackboneElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'BackboneElement2', 'modifierExtension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Resource2', 'id', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Resource2', 'meta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Resource2', 'implicitRules', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Resource2', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineParametersParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ParametersParameter2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter2', 'value*', '*', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter2', 'resource', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter2', 'part', '@Parameters.parameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParametersParameter2', nil, js.FHIRFactoryJs);
  defineParametersParameterPropsJs(js, def);
end;

procedure defineParametersPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Parameters2', 'parameter', 'ParametersParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Parameters2', nil, js.FHIRFactoryJs);
  defineParametersPropsJs(js, def);
end;

procedure defineDomainResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'DomainResource2', 'text', 'Narrative', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DomainResource2', 'contained', 'Resource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DomainResource2', 'extension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DomainResource2', 'modifierExtension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExtensionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Extension2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension2', 'value*', '*', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExtensionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Extension2', nil, js.FHIRFactoryJs);
  defineExtensionPropsJs(js, def);
end;

procedure defineNarrativePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Narrative2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Narrative2', 'div', 'xhtml', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNarrativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Narrative2', nil, js.FHIRFactoryJs);
  defineNarrativePropsJs(js, def);
end;

procedure defineIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Identifier2', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Identifier2', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier2', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Identifier2', 'assigner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Identifier2', nil, js.FHIRFactoryJs);
  defineIdentifierPropsJs(js, def);
end;

procedure defineCodingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Coding2', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding2', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding2', 'userSelected', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineCodingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coding2', nil, js.FHIRFactoryJs);
  defineCodingPropsJs(js, def);
end;

procedure defineReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Reference2', 'reference', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Reference2', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Reference2', nil, js.FHIRFactoryJs);
  defineReferencePropsJs(js, def);
end;

procedure defineSignaturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Signature2', 'type', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Signature2', 'when', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Signature2', 'whoUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature2', 'whoReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature2', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature2', 'blob', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineSignatureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Signature2', nil, js.FHIRFactoryJs);
  defineSignaturePropsJs(js, def);
end;

procedure defineSampledDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SampledData2', 'origin', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SampledData2', 'period', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData2', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData2', 'lowerLimit', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData2', 'upperLimit', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData2', 'dimensions', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SampledData2', 'data', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSampledDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SampledData2', nil, js.FHIRFactoryJs);
  defineSampledDataPropsJs(js, def);
end;

procedure definePeriodPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Period2', 'start', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Period2', 'end', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure definePeriodJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Period2', nil, js.FHIRFactoryJs);
  definePeriodPropsJs(js, def);
end;

procedure defineQuantityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Quantity2', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Quantity2', 'comparator', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity2', 'unit', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity2', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineQuantityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Quantity2', nil, js.FHIRFactoryJs);
  defineQuantityPropsJs(js, def);
end;

procedure defineAttachmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Attachment2', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment2', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment2', 'data', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Attachment2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment2', 'size', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment2', 'hash', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Attachment2', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment2', 'creation', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineAttachmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Attachment2', nil, js.FHIRFactoryJs);
  defineAttachmentPropsJs(js, def);
end;

procedure defineRatioPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Ratio2', 'numerator', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Ratio2', 'denominator', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRatioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Ratio2', nil, js.FHIRFactoryJs);
  defineRatioPropsJs(js, def);
end;

procedure defineRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Range2', 'low', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Range2', 'high', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Range2', nil, js.FHIRFactoryJs);
  defineRangePropsJs(js, def);
end;

procedure defineAnnotationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Annotation2', 'authorReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Annotation2', 'authorString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Annotation2', 'time', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Annotation2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAnnotationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Annotation2', nil, js.FHIRFactoryJs);
  defineAnnotationPropsJs(js, def);
end;

procedure defineCodeableConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'CodeableConcept2', 'coding', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeableConcept2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeableConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeableConcept2', nil, js.FHIRFactoryJs);
  defineCodeableConceptPropsJs(js, def);
end;

procedure defineHumanNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'HumanName2', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineHumanNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HumanName2', nil, js.FHIRFactoryJs);
  defineHumanNamePropsJs(js, def);
end;

procedure defineMetaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Meta2', 'versionId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Meta2', 'lastUpdated', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Meta2', 'security', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Meta2', 'tag', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMetaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Meta2', nil, js.FHIRFactoryJs);
  defineMetaPropsJs(js, def);
end;

procedure defineContactPointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactPoint2', 'system', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint2', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint2', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint2', 'rank', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ContactPoint2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContactPointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactPoint2', nil, js.FHIRFactoryJs);
  defineContactPointPropsJs(js, def);
end;

procedure defineAddressPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Address2', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address2', 'city', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address2', 'district', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address2', 'state', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address2', 'postalCode', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address2', 'country', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAddressJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Address2', nil, js.FHIRFactoryJs);
  defineAddressPropsJs(js, def);
end;

procedure defineElementDefinitionSlicingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicing2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicing2', 'ordered', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionSlicing2', 'rules', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicing2', nil, js.FHIRFactoryJs);
  defineElementDefinitionSlicingPropsJs(js, def);
end;

procedure defineElementDefinitionBasePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBase2', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase2', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionBase2', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionBaseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBase2', nil, js.FHIRFactoryJs);
  defineElementDefinitionBasePropsJs(js, def);
end;

procedure defineElementDefinitionTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionType2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionTypeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionType2', nil, js.FHIRFactoryJs);
  defineElementDefinitionTypePropsJs(js, def);
end;

procedure defineElementDefinitionConstraintPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionConstraint2', 'key', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint2', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint2', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint2', 'human', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint2', 'xpath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionConstraintJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionConstraint2', nil, js.FHIRFactoryJs);
  defineElementDefinitionConstraintPropsJs(js, def);
end;

procedure defineElementDefinitionBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBinding2', 'strength', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding2', 'valueSetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding2', 'valueSetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineElementDefinitionBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBinding2', nil, js.FHIRFactoryJs);
  defineElementDefinitionBindingPropsJs(js, def);
end;

procedure defineElementDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionMapping2', 'identity', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping2', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping2', 'map', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionMapping2', nil, js.FHIRFactoryJs);
  defineElementDefinitionMappingPropsJs(js, def);
end;

procedure defineElementDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinition2', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition2', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition2', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition2', 'slicing', 'ElementDefinitionSlicing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition2', 'short', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition2', 'definition', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition2', 'comments', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition2', 'requirements', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition2', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition2', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition2', 'base', 'ElementDefinitionBase', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition2', 'type', 'ElementDefinitionType', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition2', 'nameReference', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition2', 'defaultValue*', '*', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition2', 'meaningWhenMissing', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition2', 'fixed*', '*', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition2', 'pattern*', '*', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition2', 'example*', '*', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition2', 'minValue*', '*', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition2', 'maxValue*', '*', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition2', 'maxLength', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition2', 'constraint', 'ElementDefinitionConstraint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition2', 'mustSupport', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition2', 'isModifier', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition2', 'isSummary', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition2', 'binding', 'ElementDefinitionBinding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition2', 'mapping', 'ElementDefinitionMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineElementDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinition2', nil, js.FHIRFactoryJs);
  defineElementDefinitionPropsJs(js, def);
end;

procedure defineTimingRepeatPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TimingRepeat2', 'boundsQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat2', 'boundsRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat2', 'boundsPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat2', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat2', 'duration', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat2', 'durationMax', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat2', 'durationUnits', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat2', 'frequency', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat2', 'frequencyMax', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat2', 'period', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat2', 'periodMax', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat2', 'periodUnits', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat2', 'when', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTimingRepeatJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TimingRepeat2', nil, js.FHIRFactoryJs);
  defineTimingRepeatPropsJs(js, def);
end;

procedure defineTimingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Timing2', 'repeat', 'TimingRepeat', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Timing2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTimingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Timing2', nil, js.FHIRFactoryJs);
  defineTimingPropsJs(js, def);
end;

procedure defineAccountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Account2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account2', 'activePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account2', 'currency', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account2', 'balance', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account2', 'coveragePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account2', 'subject', 'Reference(Patient|Device|Practitioner|Location|HealthcareService|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account2', 'owner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAccountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Account2', nil, js.FHIRFactoryJs);
  defineAccountPropsJs(js, def);
end;

procedure defineAllergyIntoleranceReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AllergyIntoleranceReaction2', 'substance', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction2', 'certainty', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction2', 'manifestation', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntoleranceReaction2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction2', 'onset', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntoleranceReaction2', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction2', 'exposureRoute', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction2', 'note', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAllergyIntoleranceReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntoleranceReaction2', nil, js.FHIRFactoryJs);
  defineAllergyIntoleranceReactionPropsJs(js, def);
end;

procedure defineAllergyIntolerancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AllergyIntolerance2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance2', 'onset', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance2', 'recordedDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance2', 'recorder', 'Reference(Practitioner|Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance2', 'reporter', 'Reference(Patient|RelatedPerson|Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance2', 'substance', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance2', 'criticality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance2', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance2', 'lastOccurence', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance2', 'note', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance2', 'reaction', 'AllergyIntoleranceReaction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntolerance2', nil, js.FHIRFactoryJs);
  defineAllergyIntolerancePropsJs(js, def);
end;

procedure defineAppointmentParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AppointmentParticipant2', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentParticipant2', 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentParticipant2', 'required', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentParticipant2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAppointmentParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentParticipant2', nil, js.FHIRFactoryJs);
  defineAppointmentParticipantPropsJs(js, def);
end;

procedure defineAppointmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Appointment2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment2', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment2', 'priority', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment2', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment2', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment2', 'minutesDuration', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Appointment2', 'slot', 'Reference(Slot)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment2', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment2', 'participant', 'AppointmentParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAppointmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Appointment2', nil, js.FHIRFactoryJs);
  defineAppointmentPropsJs(js, def);
end;

procedure defineAppointmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AppointmentResponse2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse2', 'appointment', 'Reference(Appointment)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse2', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse2', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse2', 'participantType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse2', 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse2', 'participantStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentResponse2', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAppointmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentResponse2', nil, js.FHIRFactoryJs);
  defineAppointmentResponsePropsJs(js, def);
end;

procedure defineAuditEventEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEvent2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEvent2', 'subtype', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventEvent2', 'action', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEvent2', 'dateTime', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AuditEventEvent2', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEvent2', 'outcomeDesc', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEvent2', 'purposeOfEvent', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEvent2', nil, js.FHIRFactoryJs);
  defineAuditEventEventPropsJs(js, def);
end;

procedure defineAuditEventParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventParticipant2', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventParticipant2', 'reference', 'Reference(Practitioner|Organization|Device|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventParticipant2', 'userId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventParticipant2', 'altId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventParticipant2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventParticipant2', 'requestor', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'AuditEventParticipant2', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventParticipant2', 'media', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventParticipant2', 'network', 'AuditEventParticipantNetwork', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventParticipant2', 'purposeOfUse', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventParticipant2', nil, js.FHIRFactoryJs);
  defineAuditEventParticipantPropsJs(js, def);
end;

procedure defineAuditEventParticipantNetworkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventParticipantNetwork2', 'address', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventParticipantNetwork2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAuditEventParticipantNetworkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventParticipantNetwork2', nil, js.FHIRFactoryJs);
  defineAuditEventParticipantNetworkPropsJs(js, def);
end;

procedure defineAuditEventSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventSource2', 'site', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventSource2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventSource2', 'type', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventSource2', nil, js.FHIRFactoryJs);
  defineAuditEventSourcePropsJs(js, def);
end;

procedure defineAuditEventObjectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventObject2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventObject2', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventObject2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventObject2', 'role', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventObject2', 'lifecycle', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventObject2', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventObject2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventObject2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventObject2', 'query', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'AuditEventObject2', 'detail', 'AuditEventObjectDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventObjectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventObject2', nil, js.FHIRFactoryJs);
  defineAuditEventObjectPropsJs(js, def);
end;

procedure defineAuditEventObjectDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventObjectDetail2', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventObjectDetail2', 'value', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineAuditEventObjectDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventObjectDetail2', nil, js.FHIRFactoryJs);
  defineAuditEventObjectDetailPropsJs(js, def);
end;

procedure defineAuditEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AuditEvent2', 'event', 'AuditEventEvent', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent2', 'participant', 'AuditEventParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent2', 'source', 'AuditEventSource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent2', 'object', 'AuditEventObject', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEvent2', nil, js.FHIRFactoryJs);
  defineAuditEventPropsJs(js, def);
end;

procedure defineBasicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Basic2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Basic2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic2', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic2', 'author', 'Reference(Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic2', 'created', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineBasicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Basic2', nil, js.FHIRFactoryJs);
  defineBasicPropsJs(js, def);
end;

procedure defineBinaryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Binary2', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Binary2', 'content', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineBinaryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Binary2', nil, js.FHIRFactoryJs);
  defineBinaryPropsJs(js, def);
end;

procedure defineBodySitePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BodySite2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BodySite2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodySite2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BodySite2', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodySite2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BodySite2', 'image', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineBodySiteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BodySite2', nil, js.FHIRFactoryJs);
  defineBodySitePropsJs(js, def);
end;

procedure defineBundleLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleLink2', 'relation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleLink2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineBundleLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleLink2', nil, js.FHIRFactoryJs);
  defineBundleLinkPropsJs(js, def);
end;

procedure defineBundleEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntry2', 'link', '@Bundle.link', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BundleEntry2', 'fullUrl', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntry2', 'resource', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry2', 'search', 'BundleEntrySearch', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry2', 'request', 'BundleEntryRequest', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry2', 'response', 'BundleEntryResponse', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntry2', nil, js.FHIRFactoryJs);
  defineBundleEntryPropsJs(js, def);
end;

procedure defineBundleEntrySearchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntrySearch2', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntrySearch2', 'score', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineBundleEntrySearchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntrySearch2', nil, js.FHIRFactoryJs);
  defineBundleEntrySearchPropsJs(js, def);
end;

procedure defineBundleEntryRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryRequest2', 'method', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest2', 'ifNoneMatch', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest2', 'ifModifiedSince', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryRequest2', 'ifMatch', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest2', 'ifNoneExist', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineBundleEntryRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryRequest2', nil, js.FHIRFactoryJs);
  defineBundleEntryRequestPropsJs(js, def);
end;

procedure defineBundleEntryResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryResponse2', 'status', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse2', 'location', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse2', 'etag', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse2', 'lastModified', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineBundleEntryResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryResponse2', nil, js.FHIRFactoryJs);
  defineBundleEntryResponsePropsJs(js, def);
end;

procedure defineBundlePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Bundle2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Bundle2', 'total', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Bundle2', 'link', 'BundleLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Bundle2', 'entry', 'BundleEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Bundle2', 'signature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Bundle2', nil, js.FHIRFactoryJs);
  defineBundlePropsJs(js, def);
end;

procedure defineCarePlanRelatedPlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanRelatedPlan2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanRelatedPlan2', 'plan', 'Reference(CarePlan)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCarePlanRelatedPlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanRelatedPlan2', nil, js.FHIRFactoryJs);
  defineCarePlanRelatedPlanPropsJs(js, def);
end;

procedure defineCarePlanParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanParticipant2', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanParticipant2', 'member', 'Reference(Practitioner|RelatedPerson|Patient|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCarePlanParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanParticipant2', nil, js.FHIRFactoryJs);
  defineCarePlanParticipantPropsJs(js, def);
end;

procedure defineCarePlanActivityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivity2', 'actionResulting', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity2', 'progress', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity2', 'reference', 'Reference(Appointment|CommunicationRequest|DeviceUseRequest|DiagnosticOrder|MedicationOrder|NutritionOrder|Order|ProcedureRequest|ProcessRequest|ReferralRequest|SupplyRequest|VisionPrescription)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivity2', 'detail', 'CarePlanActivityDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCarePlanActivityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivity2', nil, js.FHIRFactoryJs);
  defineCarePlanActivityPropsJs(js, def);
end;

procedure defineCarePlanActivityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivityDetail2', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'goal', 'Reference(Goal)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'prohibited', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'scheduledTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'scheduledPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'scheduledString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'performer', 'Reference(Practitioner|Organization|RelatedPerson|Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'productCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'productReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'dailyAmount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCarePlanActivityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivityDetail2', nil, js.FHIRFactoryJs);
  defineCarePlanActivityDetailPropsJs(js, def);
end;

procedure defineCarePlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CarePlan2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan2', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan2', 'context', 'Reference(Encounter|EpisodeOfCare)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan2', 'author', 'Reference(Patient|Practitioner|RelatedPerson|Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan2', 'modified', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CarePlan2', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan2', 'addresses', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan2', 'support', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan2', 'relatedPlan', 'CarePlanRelatedPlan', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan2', 'participant', 'CarePlanParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan2', 'goal', 'Reference(Goal)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan2', 'activity', 'CarePlanActivity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan2', 'note', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCarePlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlan2', nil, js.FHIRFactoryJs);
  defineCarePlanPropsJs(js, def);
end;

procedure defineClaimPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimPayee2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee2', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee2', 'person', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimPayee2', nil, js.FHIRFactoryJs);
  defineClaimPayeePropsJs(js, def);
end;

procedure defineClaimDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimDiagnosis2', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimDiagnosis2', 'diagnosis', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimDiagnosis2', nil, js.FHIRFactoryJs);
  defineClaimDiagnosisPropsJs(js, def);
end;

procedure defineClaimCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimCoverage2', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimCoverage2', 'focal', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimCoverage2', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCoverage2', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimCoverage2', 'relationship', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCoverage2', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCoverage2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimCoverage2', nil, js.FHIRFactoryJs);
  defineClaimCoveragePropsJs(js, def);
end;

procedure defineClaimItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItem2', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItem2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem2', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem2', 'service', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem2', 'serviceDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimItem2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem2', 'unitPrice', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem2', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItem2', 'points', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItem2', 'net', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem2', 'udi', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem2', 'bodySite', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem2', 'subSite', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem2', 'modifier', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem2', 'detail', 'ClaimItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem2', 'prosthesis', 'ClaimItemProsthesis', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItem2', nil, js.FHIRFactoryJs);
  defineClaimItemPropsJs(js, def);
end;

procedure defineClaimItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetail2', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetail2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail2', 'service', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail2', 'unitPrice', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail2', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetail2', 'points', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetail2', 'net', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail2', 'udi', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail2', 'subDetail', 'ClaimItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetail2', nil, js.FHIRFactoryJs);
  defineClaimItemDetailPropsJs(js, def);
end;

procedure defineClaimItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetailSubDetail2', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail2', 'service', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail2', 'unitPrice', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail2', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail2', 'points', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail2', 'net', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail2', 'udi', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetailSubDetail2', nil, js.FHIRFactoryJs);
  defineClaimItemDetailSubDetailPropsJs(js, def);
end;

procedure defineClaimItemProsthesisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemProsthesis2', 'initial', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimItemProsthesis2', 'priorDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimItemProsthesis2', 'priorMaterial', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimItemProsthesisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemProsthesis2', nil, js.FHIRFactoryJs);
  defineClaimItemProsthesisPropsJs(js, def);
end;

procedure defineClaimMissingTeethPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimMissingTeeth2', 'tooth', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimMissingTeeth2', 'reason', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimMissingTeeth2', 'extractionDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineClaimMissingTeethJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimMissingTeeth2', nil, js.FHIRFactoryJs);
  defineClaimMissingTeethPropsJs(js, def);
end;

procedure defineClaimPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Claim2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Claim2', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim2', 'priority', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'fundsReserve', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'prescription', 'Reference(MedicationOrder|VisionPrescription)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'originalPrescription', 'Reference(MedicationOrder)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'payee', 'ClaimPayee', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'referral', 'Reference(ReferralRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'diagnosis', 'ClaimDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim2', 'condition', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'coverage', 'ClaimCoverage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim2', 'exception', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim2', 'school', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim2', 'accident', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Claim2', 'accidentType', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim2', 'interventionException', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim2', 'item', 'ClaimItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim2', 'additionalMaterials', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim2', 'missingTeeth', 'ClaimMissingTeeth', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Claim2', nil, js.FHIRFactoryJs);
  defineClaimPropsJs(js, def);
end;

procedure defineClaimResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItem2', 'sequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItem2', 'adjudication', 'ClaimResponseItemAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItem2', 'detail', 'ClaimResponseItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItem2', nil, js.FHIRFactoryJs);
  defineClaimResponseItemPropsJs(js, def);
end;

procedure defineClaimResponseItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemAdjudication2', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication2', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication2', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineClaimResponseItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemAdjudication2', nil, js.FHIRFactoryJs);
  defineClaimResponseItemAdjudicationPropsJs(js, def);
end;

procedure defineClaimResponseItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetail2', 'sequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetail2', 'adjudication', 'ClaimResponseItemDetailAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItemDetail2', 'subDetail', 'ClaimResponseItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetail2', nil, js.FHIRFactoryJs);
  defineClaimResponseItemDetailPropsJs(js, def);
end;

procedure defineClaimResponseItemDetailAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailAdjudication2', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemDetailAdjudication2', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemDetailAdjudication2', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineClaimResponseItemDetailAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailAdjudication2', nil, js.FHIRFactoryJs);
  defineClaimResponseItemDetailAdjudicationPropsJs(js, def);
end;

procedure defineClaimResponseItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail2', 'sequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail2', 'adjudication', 'ClaimResponseItemDetailSubDetailAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailSubDetail2', nil, js.FHIRFactoryJs);
  defineClaimResponseItemDetailSubDetailPropsJs(js, def);
end;

procedure defineClaimResponseItemDetailSubDetailAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetailAdjudication2', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetailAdjudication2', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetailAdjudication2', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineClaimResponseItemDetailSubDetailAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailSubDetailAdjudication2', nil, js.FHIRFactoryJs);
  defineClaimResponseItemDetailSubDetailAdjudicationPropsJs(js, def);
end;

procedure defineClaimResponseAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItem2', 'service', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem2', 'fee', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem2', 'adjudication', 'ClaimResponseAddItemAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem2', 'detail', 'ClaimResponseAddItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItem2', nil, js.FHIRFactoryJs);
  defineClaimResponseAddItemPropsJs(js, def);
end;

procedure defineClaimResponseAddItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemAdjudication2', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemAdjudication2', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemAdjudication2', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineClaimResponseAddItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemAdjudication2', nil, js.FHIRFactoryJs);
  defineClaimResponseAddItemAdjudicationPropsJs(js, def);
end;

procedure defineClaimResponseAddItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemDetail2', 'service', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail2', 'fee', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail2', 'adjudication', 'ClaimResponseAddItemDetailAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemDetail2', nil, js.FHIRFactoryJs);
  defineClaimResponseAddItemDetailPropsJs(js, def);
end;

procedure defineClaimResponseAddItemDetailAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemDetailAdjudication2', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailAdjudication2', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailAdjudication2', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineClaimResponseAddItemDetailAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemDetailAdjudication2', nil, js.FHIRFactoryJs);
  defineClaimResponseAddItemDetailAdjudicationPropsJs(js, def);
end;

procedure defineClaimResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseError2', 'sequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError2', 'detailSequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError2', 'subdetailSequenceLinkId', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError2', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseError2', nil, js.FHIRFactoryJs);
  defineClaimResponseErrorPropsJs(js, def);
end;

procedure defineClaimResponseNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseNote2', 'number', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseNote2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseNote2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineClaimResponseNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseNote2', nil, js.FHIRFactoryJs);
  defineClaimResponseNotePropsJs(js, def);
end;

procedure defineClaimResponseCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseCoverage2', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseCoverage2', 'focal', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimResponseCoverage2', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseCoverage2', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseCoverage2', 'relationship', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseCoverage2', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseCoverage2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseCoverage2', nil, js.FHIRFactoryJs);
  defineClaimResponseCoveragePropsJs(js, def);
end;

procedure defineClaimResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClaimResponse2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse2', 'request', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponse2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse2', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse2', 'payeeType', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'item', 'ClaimResponseItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse2', 'addItem', 'ClaimResponseAddItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse2', 'error', 'ClaimResponseError', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse2', 'totalCost', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'unallocDeductable', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'totalBenefit', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'paymentAdjustment', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'paymentAdjustmentReason', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'paymentDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponse2', 'paymentAmount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'paymentRef', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'reserved', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'form', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse2', 'note', 'ClaimResponseNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse2', 'coverage', 'ClaimResponseCoverage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponse2', nil, js.FHIRFactoryJs);
  defineClaimResponsePropsJs(js, def);
end;

procedure defineClinicalImpressionInvestigationsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionInvestigations2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionInvestigations2', 'item', 'Reference(Observation|QuestionnaireResponse|FamilyMemberHistory|DiagnosticReport)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClinicalImpressionInvestigationsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionInvestigations2', nil, js.FHIRFactoryJs);
  defineClinicalImpressionInvestigationsPropsJs(js, def);
end;

procedure defineClinicalImpressionFindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionFinding2', 'item', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding2', 'cause', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineClinicalImpressionFindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionFinding2', nil, js.FHIRFactoryJs);
  defineClinicalImpressionFindingPropsJs(js, def);
end;

procedure defineClinicalImpressionRuledOutPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionRuledOut2', 'item', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionRuledOut2', 'reason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineClinicalImpressionRuledOutJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionRuledOut2', nil, js.FHIRFactoryJs);
  defineClinicalImpressionRuledOutPropsJs(js, def);
end;

procedure defineClinicalImpressionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClinicalImpression2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression2', 'assessor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression2', 'previous', 'Reference(ClinicalImpression)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression2', 'problem', 'Reference(Condition|AllergyIntolerance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression2', 'triggerCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression2', 'triggerReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression2', 'investigations', 'ClinicalImpressionInvestigations', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression2', 'protocol', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression2', 'summary', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression2', 'finding', 'ClinicalImpressionFinding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression2', 'resolved', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression2', 'ruledOut', 'ClinicalImpressionRuledOut', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression2', 'prognosis', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression2', 'plan', 'Reference(CarePlan|Appointment|CommunicationRequest|DeviceUseRequest|DiagnosticOrder|MedicationOrder|NutritionOrder|Order|ProcedureRequest|ProcessRequest|ReferralRequest|SupplyRequest|VisionPrescription)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression2', 'action', 'Reference(ReferralRequest|ProcedureRequest|Procedure|MedicationOrder|DiagnosticOrder|NutritionOrder|SupplyRequest|Appointment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClinicalImpressionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpression2', nil, js.FHIRFactoryJs);
  defineClinicalImpressionPropsJs(js, def);
end;

procedure defineCommunicationPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationPayload2', 'contentString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationPayload2', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationPayload2', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationPayload2', nil, js.FHIRFactoryJs);
  defineCommunicationPayloadPropsJs(js, def);
end;

procedure defineCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Communication2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication2', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication2', 'sender', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication2', 'recipient', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson|Group)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication2', 'payload', 'CommunicationPayload', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication2', 'medium', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Communication2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication2', 'sent', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication2', 'received', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication2', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication2', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication2', 'requestDetail', 'Reference(CommunicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Communication2', nil, js.FHIRFactoryJs);
  defineCommunicationPropsJs(js, def);
end;

procedure defineCommunicationRequestPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationRequestPayload2', 'contentString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequestPayload2', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequestPayload2', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationRequestPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequestPayload2', nil, js.FHIRFactoryJs);
  defineCommunicationRequestPayloadPropsJs(js, def);
end;

procedure defineCommunicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CommunicationRequest2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest2', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest2', 'sender', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest2', 'recipient', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest2', 'payload', 'CommunicationRequestPayload', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest2', 'medium', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest2', 'requester', 'Reference(Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest2', 'scheduledDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest2', 'scheduledPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest2', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest2', 'requestedOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest2', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest2', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequest2', nil, js.FHIRFactoryJs);
  defineCommunicationRequestPropsJs(js, def);
end;

procedure defineCompositionAttesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionAttester2', 'time', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompositionAttester2', 'party', 'Reference(Patient|Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCompositionAttesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionAttester2', nil, js.FHIRFactoryJs);
  defineCompositionAttesterPropsJs(js, def);
end;

procedure defineCompositionEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionEvent2', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionEvent2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionEvent2', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionEvent2', nil, js.FHIRFactoryJs);
  defineCompositionEventPropsJs(js, def);
end;

procedure defineCompositionSectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionSection2', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionSection2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection2', 'text', 'Narrative', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection2', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionSection2', 'orderedBy', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection2', 'entry', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionSection2', 'emptyReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection2', 'section', '@Composition.section', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionSectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionSection2', nil, js.FHIRFactoryJs);
  defineCompositionSectionPropsJs(js, def);
end;

procedure defineCompositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Composition2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Composition2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition2', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition2', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition2', 'confidentiality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition2', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition2', 'author', 'Reference(Practitioner|Device|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition2', 'attester', 'CompositionAttester', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition2', 'custodian', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition2', 'event', 'CompositionEvent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition2', 'section', 'CompositionSection', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Composition2', nil, js.FHIRFactoryJs);
  defineCompositionPropsJs(js, def);
end;

procedure defineConceptMapContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapContact2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapContact2', nil, js.FHIRFactoryJs);
  defineConceptMapContactPropsJs(js, def);
end;

procedure defineConceptMapElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapElement2', 'codeSystem', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElement2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElement2', 'target', 'ConceptMapElementTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapElement2', nil, js.FHIRFactoryJs);
  defineConceptMapElementPropsJs(js, def);
end;

procedure defineConceptMapElementTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapElementTarget2', 'codeSystem', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTarget2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTarget2', 'equivalence', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTarget2', 'comments', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTarget2', 'dependsOn', 'ConceptMapElementTargetDependsOn', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapElementTarget2', 'product', '@ConceptMap.element.target.dependsOn', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapElementTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapElementTarget2', nil, js.FHIRFactoryJs);
  defineConceptMapElementTargetPropsJs(js, def);
end;

procedure defineConceptMapElementTargetDependsOnPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapElementTargetDependsOn2', 'element', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTargetDependsOn2', 'codeSystem', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTargetDependsOn2', 'code', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConceptMapElementTargetDependsOnJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapElementTargetDependsOn2', nil, js.FHIRFactoryJs);
  defineConceptMapElementTargetDependsOnPropsJs(js, def);
end;

procedure defineConceptMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ConceptMap2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap2', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConceptMap2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap2', 'contact', 'ConceptMapContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ConceptMap2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap2', 'useContext', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap2', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap2', 'copyright', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap2', 'sourceUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap2', 'sourceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap2', 'targetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap2', 'targetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap2', 'element', 'ConceptMapElement', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMap2', nil, js.FHIRFactoryJs);
  defineConceptMapPropsJs(js, def);
end;

procedure defineConditionStagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionStage2', 'summary', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConditionStage2', 'assessment', 'Reference(ClinicalImpression|DiagnosticReport|Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionStageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionStage2', nil, js.FHIRFactoryJs);
  defineConditionStagePropsJs(js, def);
end;

procedure defineConditionEvidencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionEvidence2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConditionEvidence2', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionEvidenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionEvidence2', nil, js.FHIRFactoryJs);
  defineConditionEvidencePropsJs(js, def);
end;

procedure defineConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Condition2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'asserter', 'Reference(Practitioner|Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'dateRecorded', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'clinicalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition2', 'verificationStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition2', 'severity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'onsetDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition2', 'onsetQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition2', 'abatementDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition2', 'abatementQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'abatementBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Condition2', 'abatementPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'abatementRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'abatementString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition2', 'stage', 'ConditionStage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition2', 'evidence', 'ConditionEvidence', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition2', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition2', 'notes', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Condition2', nil, js.FHIRFactoryJs);
  defineConditionPropsJs(js, def);
end;

procedure defineConformanceContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceContact2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConformanceContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceContact2', nil, js.FHIRFactoryJs);
  defineConformanceContactPropsJs(js, def);
end;

procedure defineConformanceSoftwarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceSoftware2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceSoftware2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceSoftware2', 'releaseDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineConformanceSoftwareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceSoftware2', nil, js.FHIRFactoryJs);
  defineConformanceSoftwarePropsJs(js, def);
end;

procedure defineConformanceImplementationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceImplementation2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceImplementation2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConformanceImplementationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceImplementation2', nil, js.FHIRFactoryJs);
  defineConformanceImplementationPropsJs(js, def);
end;

procedure defineConformanceRestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRest2', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRest2', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRest2', 'security', 'ConformanceRestSecurity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConformanceRest2', 'resource', 'ConformanceRestResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConformanceRest2', 'interaction', 'ConformanceRestInteraction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConformanceRest2', 'transactionMode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRest2', 'searchParam', '@Conformance.rest.resource.searchParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConformanceRest2', 'operation', 'ConformanceRestOperation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConformanceRestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRest2', nil, js.FHIRFactoryJs);
  defineConformanceRestPropsJs(js, def);
end;

procedure defineConformanceRestSecurityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestSecurity2', 'cors', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConformanceRestSecurity2', 'service', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConformanceRestSecurity2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestSecurity2', 'certificate', 'ConformanceRestSecurityCertificate', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConformanceRestSecurityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestSecurity2', nil, js.FHIRFactoryJs);
  defineConformanceRestSecurityPropsJs(js, def);
end;

procedure defineConformanceRestSecurityCertificatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestSecurityCertificate2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestSecurityCertificate2', 'blob', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineConformanceRestSecurityCertificateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestSecurityCertificate2', nil, js.FHIRFactoryJs);
  defineConformanceRestSecurityCertificatePropsJs(js, def);
end;

procedure defineConformanceRestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestResource2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResource2', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConformanceRestResource2', 'interaction', 'ConformanceRestResourceInteraction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConformanceRestResource2', 'versioning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResource2', 'readHistory', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConformanceRestResource2', 'updateCreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConformanceRestResource2', 'conditionalCreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConformanceRestResource2', 'conditionalUpdate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConformanceRestResource2', 'conditionalDelete', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResource2', 'searchParam', 'ConformanceRestResourceSearchParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConformanceRestResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestResource2', nil, js.FHIRFactoryJs);
  defineConformanceRestResourcePropsJs(js, def);
end;

procedure defineConformanceRestResourceInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestResourceInteraction2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResourceInteraction2', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConformanceRestResourceInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestResourceInteraction2', nil, js.FHIRFactoryJs);
  defineConformanceRestResourceInteractionPropsJs(js, def);
end;

procedure defineConformanceRestResourceSearchParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestResourceSearchParam2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResourceSearchParam2', 'definition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResourceSearchParam2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResourceSearchParam2', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConformanceRestResourceSearchParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestResourceSearchParam2', nil, js.FHIRFactoryJs);
  defineConformanceRestResourceSearchParamPropsJs(js, def);
end;

procedure defineConformanceRestInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestInteraction2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestInteraction2', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConformanceRestInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestInteraction2', nil, js.FHIRFactoryJs);
  defineConformanceRestInteractionPropsJs(js, def);
end;

procedure defineConformanceRestOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestOperation2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestOperation2', 'definition', 'Reference(OperationDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConformanceRestOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestOperation2', nil, js.FHIRFactoryJs);
  defineConformanceRestOperationPropsJs(js, def);
end;

procedure defineConformanceMessagingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceMessaging2', 'endpoint', 'ConformanceMessagingEndpoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConformanceMessaging2', 'reliableCache', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceMessaging2', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceMessaging2', 'event', 'ConformanceMessagingEvent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConformanceMessagingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceMessaging2', nil, js.FHIRFactoryJs);
  defineConformanceMessagingPropsJs(js, def);
end;

procedure defineConformanceMessagingEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceMessagingEndpoint2', 'protocol', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConformanceMessagingEndpoint2', 'address', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConformanceMessagingEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceMessagingEndpoint2', nil, js.FHIRFactoryJs);
  defineConformanceMessagingEndpointPropsJs(js, def);
end;

procedure defineConformanceMessagingEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceMessagingEvent2', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConformanceMessagingEvent2', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceMessagingEvent2', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceMessagingEvent2', 'focus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceMessagingEvent2', 'request', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConformanceMessagingEvent2', 'response', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConformanceMessagingEvent2', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConformanceMessagingEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceMessagingEvent2', nil, js.FHIRFactoryJs);
  defineConformanceMessagingEventPropsJs(js, def);
end;

procedure defineConformanceDocumentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceDocument2', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceDocument2', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConformanceDocument2', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConformanceDocumentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceDocument2', nil, js.FHIRFactoryJs);
  defineConformanceDocumentPropsJs(js, def);
end;

procedure defineConformancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Conformance2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Conformance2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'contact', 'ConformanceContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Conformance2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Conformance2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'copyright', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'software', 'ConformanceSoftware', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Conformance2', 'implementation', 'ConformanceImplementation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Conformance2', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'acceptUnknown', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Conformance2', 'profile', 'Reference(StructureDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Conformance2', 'rest', 'ConformanceRest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Conformance2', 'messaging', 'ConformanceMessaging', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Conformance2', 'document', 'ConformanceDocument', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConformanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Conformance2', nil, js.FHIRFactoryJs);
  defineConformancePropsJs(js, def);
end;

procedure defineContractActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractActor2', 'entity', 'Reference(Contract|Device|Group|Location|Organization|Patient|Practitioner|RelatedPerson|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractActor2', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractActor2', nil, js.FHIRFactoryJs);
  defineContractActorPropsJs(js, def);
end;

procedure defineContractValuedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractValuedItem2', 'entityCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem2', 'entityReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem2', 'effectiveTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractValuedItem2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem2', 'unitPrice', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem2', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractValuedItem2', 'points', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractValuedItem2', 'net', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractValuedItem2', nil, js.FHIRFactoryJs);
  defineContractValuedItemPropsJs(js, def);
end;

procedure defineContractSignerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractSigner2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner2', 'party', 'Reference(Organization|Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner2', 'signature', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineContractSignerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractSigner2', nil, js.FHIRFactoryJs);
  defineContractSignerPropsJs(js, def);
end;

procedure defineContractTermPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTerm2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm2', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTerm2', 'applies', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm2', 'subType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm2', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm2', 'action', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm2', 'actionReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm2', 'actor', 'ContractTermActor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTerm2', 'valuedItem', 'ContractTermValuedItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm2', 'group', '@Contract.term', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTerm2', nil, js.FHIRFactoryJs);
  defineContractTermPropsJs(js, def);
end;

procedure defineContractTermActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermActor2', 'entity', 'Reference(Contract|Device|Group|Location|Organization|Patient|Practitioner|RelatedPerson|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermActor2', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermActor2', nil, js.FHIRFactoryJs);
  defineContractTermActorPropsJs(js, def);
end;

procedure defineContractTermValuedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermValuedItem2', 'entityCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem2', 'entityReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem2', 'effectiveTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermValuedItem2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem2', 'unitPrice', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem2', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermValuedItem2', 'points', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermValuedItem2', 'net', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermValuedItem2', nil, js.FHIRFactoryJs);
  defineContractTermValuedItemPropsJs(js, def);
end;

procedure defineContractFriendlyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractFriendly2', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractFriendly2', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractFriendlyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractFriendly2', nil, js.FHIRFactoryJs);
  defineContractFriendlyPropsJs(js, def);
end;

procedure defineContractLegalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractLegal2', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractLegal2', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractLegalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractLegal2', nil, js.FHIRFactoryJs);
  defineContractLegalPropsJs(js, def);
end;

procedure defineContractRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractRule2', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractRule2', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractRule2', nil, js.FHIRFactoryJs);
  defineContractRulePropsJs(js, def);
end;

procedure defineContractPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Contract2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract2', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Contract2', 'applies', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract2', 'subject', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'authority', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'domain', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract2', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'action', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'actionReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'actor', 'ContractActor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'valuedItem', 'ContractValuedItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'signer', 'ContractSigner', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'term', 'ContractTerm', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'bindingAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract2', 'bindingReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract2', 'friendly', 'ContractFriendly', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'legal', 'ContractLegal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract2', 'rule', 'ContractRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contract2', nil, js.FHIRFactoryJs);
  defineContractPropsJs(js, def);
end;

procedure defineCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Coverage2', 'issuer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage2', 'bin', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage2', 'subscriberId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage2', 'group', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage2', 'plan', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage2', 'subPlan', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage2', 'dependent', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Coverage2', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Coverage2', 'subscriber', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage2', 'network', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage2', 'contract', 'Reference(Contract)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coverage2', nil, js.FHIRFactoryJs);
  defineCoveragePropsJs(js, def);
end;

procedure defineDataElementContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DataElementContact2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElementContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataElementContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataElementContact2', nil, js.FHIRFactoryJs);
  defineDataElementContactPropsJs(js, def);
end;

procedure defineDataElementMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DataElementMapping2', 'identity', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElementMapping2', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElementMapping2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElementMapping2', 'comments', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDataElementMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataElementMapping2', nil, js.FHIRFactoryJs);
  defineDataElementMappingPropsJs(js, def);
end;

procedure defineDataElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DataElement2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataElement2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement2', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'DataElement2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement2', 'contact', 'DataElementContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataElement2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DataElement2', 'useContext', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataElement2', 'copyright', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement2', 'stringency', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataElement2', 'mapping', 'DataElementMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataElement2', 'element', 'ElementDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataElement2', nil, js.FHIRFactoryJs);
  defineDataElementPropsJs(js, def);
end;

procedure defineDetectedIssueMitigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DetectedIssueMitigation2', 'action', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssueMitigation2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssueMitigation2', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDetectedIssueMitigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssueMitigation2', nil, js.FHIRFactoryJs);
  defineDetectedIssueMitigationPropsJs(js, def);
end;

procedure defineDetectedIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DetectedIssue2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue2', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue2', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue2', 'implicated', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue2', 'detail', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssue2', 'author', 'Reference(Practitioner|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue2', 'reference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue2', 'mitigation', 'DetectedIssueMitigation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDetectedIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssue2', nil, js.FHIRFactoryJs);
  defineDetectedIssuePropsJs(js, def);
end;

procedure defineDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Device2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device2', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device2', 'manufacturer', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device2', 'model', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device2', 'manufactureDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device2', 'expiry', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device2', 'udi', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device2', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device2', 'owner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device2', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device2', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Device2', nil, js.FHIRFactoryJs);
  defineDevicePropsJs(js, def);
end;

procedure defineDeviceComponentProductionSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceComponentProductionSpecification2', 'specType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProductionSpecification2', 'componentId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProductionSpecification2', 'productionSpec', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceComponentProductionSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponentProductionSpecification2', nil, js.FHIRFactoryJs);
  defineDeviceComponentProductionSpecificationPropsJs(js, def);
end;

procedure defineDeviceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceComponent2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent2', 'lastSystemChange', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceComponent2', 'source', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent2', 'parent', 'Reference(DeviceComponent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent2', 'operationalStatus', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent2', 'parameterGroup', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent2', 'measurementPrinciple', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceComponent2', 'productionSpecification', 'DeviceComponentProductionSpecification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent2', 'languageCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDeviceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponent2', nil, js.FHIRFactoryJs);
  defineDeviceComponentPropsJs(js, def);
end;

procedure defineDeviceMetricCalibrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceMetricCalibration2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration2', 'state', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration2', 'time', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineDeviceMetricCalibrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetricCalibration2', nil, js.FHIRFactoryJs);
  defineDeviceMetricCalibrationPropsJs(js, def);
end;

procedure defineDeviceMetricPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceMetric2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric2', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric2', 'source', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric2', 'parent', 'Reference(DeviceComponent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric2', 'operationalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric2', 'color', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric2', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric2', 'measurementPeriod', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric2', 'calibration', 'DeviceMetricCalibration', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceMetricJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetric2', nil, js.FHIRFactoryJs);
  defineDeviceMetricPropsJs(js, def);
end;

procedure defineDeviceUseRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceUseRequest2', 'bodySiteCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest2', 'bodySiteReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUseRequest2', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseRequest2', 'indication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseRequest2', 'prnReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseRequest2', 'orderedOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseRequest2', 'recordedOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseRequest2', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest2', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest2', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest2', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseRequest2', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceUseRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUseRequest2', nil, js.FHIRFactoryJs);
  defineDeviceUseRequestPropsJs(js, def);
end;

procedure defineDeviceUseStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceUseStatement2', 'bodySiteCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement2', 'bodySiteReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement2', 'whenUsed', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement2', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement2', 'indication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement2', 'recordedOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement2', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement2', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement2', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement2', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineDeviceUseStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUseStatement2', nil, js.FHIRFactoryJs);
  defineDeviceUseStatementPropsJs(js, def);
end;

procedure defineDiagnosticOrderEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticOrderEvent2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticOrderEvent2', 'description', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrderEvent2', 'dateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticOrderEvent2', 'actor', 'Reference(Practitioner|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDiagnosticOrderEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticOrderEvent2', nil, js.FHIRFactoryJs);
  defineDiagnosticOrderEventPropsJs(js, def);
end;

procedure defineDiagnosticOrderItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticOrderItem2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrderItem2', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrderItem2', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrderItem2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticOrderItem2', 'event', '@DiagnosticOrder.event', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDiagnosticOrderItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticOrderItem2', nil, js.FHIRFactoryJs);
  defineDiagnosticOrderItemPropsJs(js, def);
end;

procedure defineDiagnosticOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DiagnosticOrder2', 'subject', 'Reference(Patient|Group|Location|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrder2', 'orderer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrder2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrder2', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder2', 'supportingInformation', 'Reference(Observation|Condition|DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder2', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticOrder2', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticOrder2', 'event', 'DiagnosticOrderEvent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder2', 'item', 'DiagnosticOrderItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder2', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDiagnosticOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticOrder2', nil, js.FHIRFactoryJs);
  defineDiagnosticOrderPropsJs(js, def);
end;

procedure defineDiagnosticReportImagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticReportImage2', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReportImage2', 'link', 'Reference(Media)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDiagnosticReportImageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReportImage2', nil, js.FHIRFactoryJs);
  defineDiagnosticReportImagePropsJs(js, def);
end;

procedure defineDiagnosticReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DiagnosticReport2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport2', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport2', 'subject', 'Reference(Patient|Group|Device|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport2', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport2', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport2', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport2', 'performer', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport2', 'request', 'Reference(DiagnosticOrder|ProcedureRequest|ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport2', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport2', 'result', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport2', 'imagingStudy', 'Reference(ImagingStudy|ImagingObjectSelection)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport2', 'image', 'DiagnosticReportImage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport2', 'conclusion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport2', 'codedDiagnosis', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport2', 'presentedForm', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDiagnosticReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReport2', nil, js.FHIRFactoryJs);
  defineDiagnosticReportPropsJs(js, def);
end;

procedure defineDocumentManifestContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestContent2', 'pAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestContent2', 'pReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentManifestContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestContent2', nil, js.FHIRFactoryJs);
  defineDocumentManifestContentPropsJs(js, def);
end;

procedure defineDocumentManifestRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestRelated2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestRelated2', 'ref', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentManifestRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestRelated2', nil, js.FHIRFactoryJs);
  defineDocumentManifestRelatedPropsJs(js, def);
end;

procedure defineDocumentManifestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentManifest2', 'masterIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest2', 'subject', 'Reference(Patient|Practitioner|Group|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest2', 'recipient', 'Reference(Patient|Practitioner|RelatedPerson|Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest2', 'author', 'Reference(Practitioner|Organization|Device|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentManifest2', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest2', 'content', 'DocumentManifestContent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest2', 'related', 'DocumentManifestRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifest2', nil, js.FHIRFactoryJs);
  defineDocumentManifestPropsJs(js, def);
end;

procedure defineDocumentReferenceRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceRelatesTo2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReferenceRelatesTo2', 'target', 'Reference(DocumentReference)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceRelatesTo2', nil, js.FHIRFactoryJs);
  defineDocumentReferenceRelatesToPropsJs(js, def);
end;

procedure defineDocumentReferenceContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContent2', 'attachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContent2', 'format', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentReferenceContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContent2', nil, js.FHIRFactoryJs);
  defineDocumentReferenceContentPropsJs(js, def);
end;

procedure defineDocumentReferenceContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContext2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext2', 'event', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReferenceContext2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext2', 'facilityType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext2', 'practiceSetting', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext2', 'sourcePatientInfo', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext2', 'related', 'DocumentReferenceContextRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentReferenceContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContext2', nil, js.FHIRFactoryJs);
  defineDocumentReferenceContextPropsJs(js, def);
end;

procedure defineDocumentReferenceContextRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContextRelated2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContextRelated2', 'ref', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceContextRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContextRelated2', nil, js.FHIRFactoryJs);
  defineDocumentReferenceContextRelatedPropsJs(js, def);
end;

procedure defineDocumentReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentReference2', 'masterIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference2', 'subject', 'Reference(Patient|Practitioner|Group|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference2', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference2', 'author', 'Reference(Practitioner|Organization|Device|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference2', 'custodian', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference2', 'authenticator', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference2', 'indexed', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference2', 'docStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference2', 'relatesTo', 'DocumentReferenceRelatesTo', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference2', 'securityLabel', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference2', 'content', 'DocumentReferenceContent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference2', 'context', 'DocumentReferenceContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReference2', nil, js.FHIRFactoryJs);
  defineDocumentReferencePropsJs(js, def);
end;

procedure defineEligibilityRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityRequest2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityRequest2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityRequest2', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest2', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityRequest2', nil, js.FHIRFactoryJs);
  defineEligibilityRequestPropsJs(js, def);
end;

procedure defineEligibilityResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityResponse2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse2', 'request', 'Reference(EligibilityRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse2', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse2', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityResponse2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse2', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse2', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEligibilityResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponse2', nil, js.FHIRFactoryJs);
  defineEligibilityResponsePropsJs(js, def);
end;

procedure defineEncounterStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterStatusHistory2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EncounterStatusHistory2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterStatusHistory2', nil, js.FHIRFactoryJs);
  defineEncounterStatusHistoryPropsJs(js, def);
end;

procedure defineEncounterParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterParticipant2', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterParticipant2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterParticipant2', 'individual', 'Reference(Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterParticipant2', nil, js.FHIRFactoryJs);
  defineEncounterParticipantPropsJs(js, def);
end;

procedure defineEncounterHospitalizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterHospitalization2', 'preAdmissionIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization2', 'origin', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization2', 'admitSource', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization2', 'admittingDiagnosis', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization2', 'reAdmission', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization2', 'dietPreference', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization2', 'specialCourtesy', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization2', 'specialArrangement', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization2', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization2', 'dischargeDisposition', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization2', 'dischargeDiagnosis', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEncounterHospitalizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterHospitalization2', nil, js.FHIRFactoryJs);
  defineEncounterHospitalizationPropsJs(js, def);
end;

procedure defineEncounterLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterLocation2', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterLocation2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EncounterLocation2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterLocation2', nil, js.FHIRFactoryJs);
  defineEncounterLocationPropsJs(js, def);
end;

procedure defineEncounterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Encounter2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Encounter2', 'statusHistory', 'EncounterStatusHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter2', 'class', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Encounter2', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter2', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter2', 'episodeOfCare', 'Reference(EpisodeOfCare)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter2', 'incomingReferral', 'Reference(ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter2', 'participant', 'EncounterParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter2', 'appointment', 'Reference(Appointment)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter2', 'length', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter2', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter2', 'indication', 'Reference(Condition|Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter2', 'hospitalization', 'EncounterHospitalization', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter2', 'location', 'EncounterLocation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter2', 'serviceProvider', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter2', 'partOf', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Encounter2', nil, js.FHIRFactoryJs);
  defineEncounterPropsJs(js, def);
end;

procedure defineEnrollmentRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentRequest2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentRequest2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentRequest2', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest2', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest2', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest2', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest2', 'relationship', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentRequest2', nil, js.FHIRFactoryJs);
  defineEnrollmentRequestPropsJs(js, def);
end;

procedure defineEnrollmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentResponse2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentResponse2', 'request', 'Reference(EnrollmentRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse2', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse2', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentResponse2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse2', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse2', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentResponse2', nil, js.FHIRFactoryJs);
  defineEnrollmentResponsePropsJs(js, def);
end;

procedure defineEpisodeOfCareStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareStatusHistory2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCareStatusHistory2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEpisodeOfCareStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareStatusHistory2', nil, js.FHIRFactoryJs);
  defineEpisodeOfCareStatusHistoryPropsJs(js, def);
end;

procedure defineEpisodeOfCareCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareCareTeam2', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCareCareTeam2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareCareTeam2', 'member', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEpisodeOfCareCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareCareTeam2', nil, js.FHIRFactoryJs);
  defineEpisodeOfCareCareTeamPropsJs(js, def);
end;

procedure defineEpisodeOfCarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCare2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCare2', 'statusHistory', 'EpisodeOfCareStatusHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare2', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare2', 'condition', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare2', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare2', 'referralRequest', 'Reference(ReferralRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare2', 'careManager', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare2', 'careTeam', 'EpisodeOfCareCareTeam', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEpisodeOfCareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCare2', nil, js.FHIRFactoryJs);
  defineEpisodeOfCarePropsJs(js, def);
end;

procedure defineExplanationOfBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefit2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit2', 'request', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit2', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit2', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefit2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit2', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit2', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefit2', nil, js.FHIRFactoryJs);
  defineExplanationOfBenefitPropsJs(js, def);
end;

procedure defineFamilyMemberHistoryConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistoryCondition2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition2', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition2', 'onsetQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition2', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition2', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition2', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition2', 'note', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineFamilyMemberHistoryConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistoryCondition2', nil, js.FHIRFactoryJs);
  defineFamilyMemberHistoryConditionPropsJs(js, def);
end;

procedure defineFamilyMemberHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistory2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'bornPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'bornDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'bornString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'ageQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'ageRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'ageString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'deceasedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'deceasedQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'deceasedRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'deceasedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'deceasedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'note', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory2', 'condition', 'FamilyMemberHistoryCondition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistory2', nil, js.FHIRFactoryJs);
  defineFamilyMemberHistoryPropsJs(js, def);
end;

procedure defineFlagPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Flag2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Flag2', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Flag2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag2', 'subject', 'Reference(Patient|Location|Group|Organization|Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag2', 'author', 'Reference(Device|Organization|Patient|Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineFlagJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Flag2', nil, js.FHIRFactoryJs);
  defineFlagPropsJs(js, def);
end;

procedure defineGoalOutcomePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GoalOutcome2', 'resultCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalOutcome2', 'resultReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineGoalOutcomeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GoalOutcome2', nil, js.FHIRFactoryJs);
  defineGoalOutcomePropsJs(js, def);
end;

procedure defineGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Goal2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal2', 'subject', 'Reference(Patient|Group|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal2', 'startDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal2', 'startCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal2', 'targetDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal2', 'targetQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal2', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal2', 'statusDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal2', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal2', 'author', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal2', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal2', 'addresses', 'Reference(Condition|Observation|MedicationStatement|NutritionOrder|ProcedureRequest|RiskAssessment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal2', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal2', 'outcome', 'GoalOutcome', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Goal2', nil, js.FHIRFactoryJs);
  defineGoalPropsJs(js, def);
end;

procedure defineGroupCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupCharacteristic2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic2', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic2', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic2', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic2', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic2', 'exclude', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineGroupCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupCharacteristic2', nil, js.FHIRFactoryJs);
  defineGroupCharacteristicPropsJs(js, def);
end;

procedure defineGroupMemberPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupMember2', 'entity', 'Reference(Patient|Practitioner|Device|Medication|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupMember2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupMember2', 'inactive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineGroupMemberJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupMember2', nil, js.FHIRFactoryJs);
  defineGroupMemberPropsJs(js, def);
end;

procedure defineGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Group2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Group2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group2', 'actual', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Group2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Group2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group2', 'quantity', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group2', 'characteristic', 'GroupCharacteristic', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Group2', 'member', 'GroupMember', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Group2', nil, js.FHIRFactoryJs);
  defineGroupPropsJs(js, def);
end;

procedure defineHealthcareServiceServiceTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceServiceType2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareServiceServiceType2', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineHealthcareServiceServiceTypeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceServiceType2', nil, js.FHIRFactoryJs);
  defineHealthcareServiceServiceTypePropsJs(js, def);
end;

procedure defineHealthcareServiceAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceAvailableTime2', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime2', 'availableStartTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime2', 'availableEndTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineHealthcareServiceAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceAvailableTime2', nil, js.FHIRFactoryJs);
  defineHealthcareServiceAvailableTimePropsJs(js, def);
end;

procedure defineHealthcareServiceNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceNotAvailable2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceNotAvailable2', 'during', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineHealthcareServiceNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceNotAvailable2', nil, js.FHIRFactoryJs);
  defineHealthcareServiceNotAvailablePropsJs(js, def);
end;

procedure defineHealthcareServicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'HealthcareService2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService2', 'providedBy', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService2', 'serviceCategory', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService2', 'serviceType', 'HealthcareServiceServiceType', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService2', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService2', 'serviceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService2', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService2', 'extraDetails', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService2', 'photo', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService2', 'coverageArea', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService2', 'serviceProvisionCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService2', 'eligibility', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService2', 'eligibilityNote', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService2', 'characteristic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService2', 'referralMethod', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService2', 'publicKey', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService2', 'appointmentRequired', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService2', 'availableTime', 'HealthcareServiceAvailableTime', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService2', 'notAvailable', 'HealthcareServiceNotAvailable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService2', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineHealthcareServiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareService2', nil, js.FHIRFactoryJs);
  defineHealthcareServicePropsJs(js, def);
end;

procedure defineImagingObjectSelectionStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingObjectSelectionStudy2', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudy2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudy2', 'imagingStudy', 'Reference(ImagingStudy)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingObjectSelectionStudy2', 'series', 'ImagingObjectSelectionStudySeries', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingObjectSelectionStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingObjectSelectionStudy2', nil, js.FHIRFactoryJs);
  defineImagingObjectSelectionStudyPropsJs(js, def);
end;

procedure defineImagingObjectSelectionStudySeriesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingObjectSelectionStudySeries2', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudySeries2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudySeries2', 'instance', 'ImagingObjectSelectionStudySeriesInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingObjectSelectionStudySeriesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingObjectSelectionStudySeries2', nil, js.FHIRFactoryJs);
  defineImagingObjectSelectionStudySeriesPropsJs(js, def);
end;

procedure defineImagingObjectSelectionStudySeriesInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingObjectSelectionStudySeriesInstance2', 'sopClass', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudySeriesInstance2', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudySeriesInstance2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudySeriesInstance2', 'frames', 'ImagingObjectSelectionStudySeriesInstanceFrames', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingObjectSelectionStudySeriesInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingObjectSelectionStudySeriesInstance2', nil, js.FHIRFactoryJs);
  defineImagingObjectSelectionStudySeriesInstancePropsJs(js, def);
end;

procedure defineImagingObjectSelectionStudySeriesInstanceFramesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingObjectSelectionStudySeriesInstanceFrames2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImagingObjectSelectionStudySeriesInstanceFramesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingObjectSelectionStudySeriesInstanceFrames2', nil, js.FHIRFactoryJs);
  defineImagingObjectSelectionStudySeriesInstanceFramesPropsJs(js, def);
end;

procedure defineImagingObjectSelectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImagingObjectSelection2', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelection2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingObjectSelection2', 'title', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingObjectSelection2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelection2', 'author', 'Reference(Practitioner|Device|Organization|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingObjectSelection2', 'authoringTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingObjectSelection2', 'study', 'ImagingObjectSelectionStudy', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingObjectSelectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingObjectSelection2', nil, js.FHIRFactoryJs);
  defineImagingObjectSelectionPropsJs(js, def);
end;

procedure defineImagingStudySeriesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeries2', 'number', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries2', 'modality', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries2', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries2', 'numberOfInstances', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries2', 'availability', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries2', 'bodySite', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries2', 'laterality', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries2', 'started', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudySeries2', 'instance', 'ImagingStudySeriesInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudySeriesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeries2', nil, js.FHIRFactoryJs);
  defineImagingStudySeriesPropsJs(js, def);
end;

procedure defineImagingStudySeriesInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeriesInstance2', 'number', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance2', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance2', 'sopClass', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance2', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance2', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance2', 'content', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudySeriesInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeriesInstance2', nil, js.FHIRFactoryJs);
  defineImagingStudySeriesInstancePropsJs(js, def);
end;

procedure defineImagingStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImagingStudy2', 'started', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudy2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy2', 'uid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy2', 'accession', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy2', 'order', 'Reference(DiagnosticOrder)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy2', 'modalityList', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy2', 'referrer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy2', 'availability', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy2', 'numberOfSeries', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy2', 'numberOfInstances', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy2', 'procedure', 'Reference(Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy2', 'interpreter', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy2', 'series', 'ImagingStudySeries', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudy2', nil, js.FHIRFactoryJs);
  defineImagingStudyPropsJs(js, def);
end;

procedure defineImmunizationExplanationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationExplanation2', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationExplanation2', 'reasonNotGiven', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationExplanationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationExplanation2', nil, js.FHIRFactoryJs);
  defineImmunizationExplanationPropsJs(js, def);
end;

procedure defineImmunizationReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationReaction2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationReaction2', 'detail', 'Reference(Observation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationReaction2', 'reported', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineImmunizationReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationReaction2', nil, js.FHIRFactoryJs);
  defineImmunizationReactionPropsJs(js, def);
end;

procedure defineImmunizationVaccinationProtocolPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationVaccinationProtocol2', 'doseSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol2', 'authority', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol2', 'series', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol2', 'seriesDoses', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol2', 'targetDisease', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol2', 'doseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol2', 'doseStatusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImmunizationVaccinationProtocolJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationVaccinationProtocol2', nil, js.FHIRFactoryJs);
  defineImmunizationVaccinationProtocolPropsJs(js, def);
end;

procedure defineImmunizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Immunization2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization2', 'vaccineCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'wasNotGiven', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization2', 'reported', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization2', 'performer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'requester', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization2', 'expirationDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization2', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization2', 'explanation', 'ImmunizationExplanation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization2', 'reaction', 'ImmunizationReaction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization2', 'vaccinationProtocol', 'ImmunizationVaccinationProtocol', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Immunization2', nil, js.FHIRFactoryJs);
  defineImmunizationPropsJs(js, def);
end;

procedure defineImmunizationRecommendationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation2', 'vaccineCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation2', 'doseNumber', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation2', 'forecastStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation2', 'dateCriterion', 'ImmunizationRecommendationRecommendationDateCriterion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation2', 'protocol', 'ImmunizationRecommendationRecommendationProtocol', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation2', 'supportingImmunization', 'Reference(Immunization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation2', 'supportingPatientInformation', 'Reference(Observation|AllergyIntolerance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendation2', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationPropsJs(js, def);
end;

procedure defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion2', 'value', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineImmunizationRecommendationRecommendationDateCriterionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendationDateCriterion2', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js, def);
end;

procedure defineImmunizationRecommendationRecommendationProtocolPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol2', 'doseSequence', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol2', 'authority', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol2', 'series', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImmunizationRecommendationRecommendationProtocolJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendationProtocol2', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationProtocolPropsJs(js, def);
end;

procedure defineImmunizationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendation2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendation2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation2', 'recommendation', 'ImmunizationRecommendationRecommendation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendation2', nil, js.FHIRFactoryJs);
  defineImmunizationRecommendationPropsJs(js, def);
end;

procedure defineImplementationGuideContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideContact2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideContact2', nil, js.FHIRFactoryJs);
  defineImplementationGuideContactPropsJs(js, def);
end;

procedure defineImplementationGuideDependencyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDependency2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDependency2', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDependencyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDependency2', nil, js.FHIRFactoryJs);
  defineImplementationGuideDependencyPropsJs(js, def);
end;

procedure defineImplementationGuidePackagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePackage2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackage2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackage2', 'resource', 'ImplementationGuidePackageResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuidePackageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePackage2', nil, js.FHIRFactoryJs);
  defineImplementationGuidePackagePropsJs(js, def);
end;

procedure defineImplementationGuidePackageResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePackageResource2', 'purpose', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource2', 'acronym', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource2', 'sourceUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource2', 'sourceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuidePackageResource2', 'exampleFor', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuidePackageResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePackageResource2', nil, js.FHIRFactoryJs);
  defineImplementationGuidePackageResourcePropsJs(js, def);
end;

procedure defineImplementationGuideGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideGlobal2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideGlobal2', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideGlobal2', nil, js.FHIRFactoryJs);
  defineImplementationGuideGlobalPropsJs(js, def);
end;

procedure defineImplementationGuidePagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePage2', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage2', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage2', 'format', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage2', 'page', '@ImplementationGuide.page', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuidePageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePage2', nil, js.FHIRFactoryJs);
  defineImplementationGuidePagePropsJs(js, def);
end;

procedure defineImplementationGuidePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuide2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide2', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuide2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide2', 'contact', 'ImplementationGuideContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuide2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide2', 'useContext', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide2', 'copyright', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide2', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide2', 'dependency', 'ImplementationGuideDependency', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide2', 'package', 'ImplementationGuidePackage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide2', 'global', 'ImplementationGuideGlobal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide2', 'page', 'ImplementationGuidePage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuide2', nil, js.FHIRFactoryJs);
  defineImplementationGuidePropsJs(js, def);
end;

procedure defineListEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ListEntry2', 'flag', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ListEntry2', 'deleted', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ListEntry2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ListEntry2', 'item', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineListEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ListEntry2', nil, js.FHIRFactoryJs);
  defineListEntryPropsJs(js, def);
end;

procedure defineListPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'List2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List2', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List2', 'subject', 'Reference(Patient|Group|Device|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List2', 'source', 'Reference(Practitioner|Patient|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'List2', 'orderedBy', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List2', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List2', 'note', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List2', 'entry', 'ListEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List2', 'emptyReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineListJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('List2', nil, js.FHIRFactoryJs);
  defineListPropsJs(js, def);
end;

procedure defineLocationPositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LocationPosition2', 'longitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition2', 'latitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition2', 'altitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineLocationPositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LocationPosition2', nil, js.FHIRFactoryJs);
  defineLocationPositionPropsJs(js, def);
end;

procedure defineLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Location2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location2', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location2', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location2', 'physicalType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location2', 'position', 'LocationPosition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location2', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location2', 'partOf', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Location2', nil, js.FHIRFactoryJs);
  defineLocationPropsJs(js, def);
end;

procedure defineMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Media2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media2', 'subtype', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media2', 'subject', 'Reference(Patient|Practitioner|Group|Device|Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media2', 'operator', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media2', 'view', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media2', 'deviceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media2', 'height', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media2', 'width', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media2', 'frames', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media2', 'duration', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media2', 'content', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Media2', nil, js.FHIRFactoryJs);
  defineMediaPropsJs(js, def);
end;

procedure defineMedicationProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationProduct2', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationProduct2', 'ingredient', 'MedicationProductIngredient', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationProduct2', 'batch', 'MedicationProductBatch', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationProductJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationProduct2', nil, js.FHIRFactoryJs);
  defineMedicationProductPropsJs(js, def);
end;

procedure defineMedicationProductIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationProductIngredient2', 'item', 'Reference(Substance|Medication)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationProductIngredient2', 'amount', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationProductIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationProductIngredient2', nil, js.FHIRFactoryJs);
  defineMedicationProductIngredientPropsJs(js, def);
end;

procedure defineMedicationProductBatchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationProductBatch2', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationProductBatch2', 'expirationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineMedicationProductBatchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationProductBatch2', nil, js.FHIRFactoryJs);
  defineMedicationProductBatchPropsJs(js, def);
end;

procedure defineMedicationPackagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationPackage2', 'container', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationPackage2', 'content', 'MedicationPackageContent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationPackageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationPackage2', nil, js.FHIRFactoryJs);
  defineMedicationPackagePropsJs(js, def);
end;

procedure defineMedicationPackageContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationPackageContent2', 'item', 'Reference(Medication)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationPackageContent2', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationPackageContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationPackageContent2', nil, js.FHIRFactoryJs);
  defineMedicationPackageContentPropsJs(js, def);
end;

procedure defineMedicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Medication2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication2', 'isBrand', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Medication2', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication2', 'product', 'MedicationProduct', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication2', 'package', 'MedicationPackage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Medication2', nil, js.FHIRFactoryJs);
  defineMedicationPropsJs(js, def);
end;

procedure defineMedicationAdministrationDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationDosage2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministrationDosage2', 'siteCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage2', 'siteReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage2', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage2', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage2', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage2', 'rateRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationAdministrationDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationDosage2', nil, js.FHIRFactoryJs);
  defineMedicationAdministrationDosagePropsJs(js, def);
end;

procedure defineMedicationAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationAdministration2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministration2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration2', 'practitioner', 'Reference(Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration2', 'prescription', 'Reference(MedicationOrder)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration2', 'wasNotGiven', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationAdministration2', 'reasonNotGiven', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration2', 'reasonGiven', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration2', 'effectiveTimeDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationAdministration2', 'effectiveTimePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration2', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration2', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration2', 'device', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration2', 'note', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministration2', 'dosage', 'MedicationAdministrationDosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministration2', nil, js.FHIRFactoryJs);
  defineMedicationAdministrationPropsJs(js, def);
end;

procedure defineMedicationDispenseDosageInstructionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'additionalInstructions', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'timing', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'siteCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'siteReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'doseRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'rateRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction2', 'maxDosePerPeriod', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationDispenseDosageInstructionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispenseDosageInstruction2', nil, js.FHIRFactoryJs);
  defineMedicationDispenseDosageInstructionPropsJs(js, def);
end;

procedure defineMedicationDispenseSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispenseSubstitution2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseSubstitution2', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispenseSubstitution2', 'responsibleParty', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationDispenseSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispenseSubstitution2', nil, js.FHIRFactoryJs);
  defineMedicationDispenseSubstitutionPropsJs(js, def);
end;

procedure defineMedicationDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationDispense2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationDispense2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense2', 'dispenser', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense2', 'authorizingPrescription', 'Reference(MedicationOrder)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense2', 'daysSupply', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense2', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense2', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense2', 'whenPrepared', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense2', 'whenHandedOver', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense2', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense2', 'receiver', 'Reference(Patient|Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense2', 'note', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationDispense2', 'dosageInstruction', 'MedicationDispenseDosageInstruction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense2', 'substitution', 'MedicationDispenseSubstitution', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispense2', nil, js.FHIRFactoryJs);
  defineMedicationDispensePropsJs(js, def);
end;

procedure defineMedicationOrderDosageInstructionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'additionalInstructions', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'timing', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'siteCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'siteReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'doseRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'rateRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction2', 'maxDosePerPeriod', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationOrderDosageInstructionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationOrderDosageInstruction2', nil, js.FHIRFactoryJs);
  defineMedicationOrderDosageInstructionPropsJs(js, def);
end;

procedure defineMedicationOrderDispenseRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationOrderDispenseRequest2', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDispenseRequest2', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDispenseRequest2', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDispenseRequest2', 'numberOfRepeatsAllowed', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MedicationOrderDispenseRequest2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDispenseRequest2', 'expectedSupplyDuration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationOrderDispenseRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationOrderDispenseRequest2', nil, js.FHIRFactoryJs);
  defineMedicationOrderDispenseRequestPropsJs(js, def);
end;

procedure defineMedicationOrderSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationOrderSubstitution2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderSubstitution2', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationOrderSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationOrderSubstitution2', nil, js.FHIRFactoryJs);
  defineMedicationOrderSubstitutionPropsJs(js, def);
end;

procedure defineMedicationOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationOrder2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationOrder2', 'dateWritten', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationOrder2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationOrder2', 'dateEnded', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationOrder2', 'reasonEnded', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder2', 'prescriber', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder2', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder2', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder2', 'note', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationOrder2', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder2', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder2', 'dosageInstruction', 'MedicationOrderDosageInstruction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationOrder2', 'dispenseRequest', 'MedicationOrderDispenseRequest', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder2', 'substitution', 'MedicationOrderSubstitution', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder2', 'priorPrescription', 'Reference(MedicationOrder)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationOrder2', nil, js.FHIRFactoryJs);
  defineMedicationOrderPropsJs(js, def);
end;

procedure defineMedicationStatementDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationStatementDosage2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'timing', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'siteCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'siteReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'quantityQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'quantityRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'rateRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage2', 'maxDosePerPeriod', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationStatementDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationStatementDosage2', nil, js.FHIRFactoryJs);
  defineMedicationStatementDosagePropsJs(js, def);
end;

procedure defineMedicationStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationStatement2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement2', 'informationSource', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement2', 'dateAsserted', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement2', 'wasNotTaken', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationStatement2', 'reasonNotTaken', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement2', 'reasonForUseCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement2', 'reasonForUseReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement2', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement2', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement2', 'note', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement2', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement2', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement2', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement2', 'dosage', 'MedicationStatementDosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationStatement2', nil, js.FHIRFactoryJs);
  defineMedicationStatementPropsJs(js, def);
end;

procedure defineMessageHeaderResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderResponse2', 'identifier', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse2', 'details', 'Reference(OperationOutcome)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMessageHeaderResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderResponse2', nil, js.FHIRFactoryJs);
  defineMessageHeaderResponsePropsJs(js, def);
end;

procedure defineMessageHeaderSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderSource2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource2', 'software', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource2', 'contact', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderSource2', 'endpoint', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderSource2', nil, js.FHIRFactoryJs);
  defineMessageHeaderSourcePropsJs(js, def);
end;

procedure defineMessageHeaderDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderDestination2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination2', 'target', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderDestination2', 'endpoint', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderDestination2', nil, js.FHIRFactoryJs);
  defineMessageHeaderDestinationPropsJs(js, def);
end;

procedure defineMessageHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MessageHeader2', 'timestamp', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MessageHeader2', 'event', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader2', 'response', 'MessageHeaderResponse', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader2', 'source', 'MessageHeaderSource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader2', 'destination', 'MessageHeaderDestination', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader2', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader2', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader2', 'receiver', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader2', 'responsible', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader2', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader2', 'data', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMessageHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeader2', nil, js.FHIRFactoryJs);
  defineMessageHeaderPropsJs(js, def);
end;

procedure defineNamingSystemContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NamingSystemContact2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineNamingSystemContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystemContact2', nil, js.FHIRFactoryJs);
  defineNamingSystemContactPropsJs(js, def);
end;

procedure defineNamingSystemUniqueIdPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NamingSystemUniqueId2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId2', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId2', 'preferred', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'NamingSystemUniqueId2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNamingSystemUniqueIdJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystemUniqueId2', nil, js.FHIRFactoryJs);
  defineNamingSystemUniqueIdPropsJs(js, def);
end;

procedure defineNamingSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'NamingSystem2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem2', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem2', 'contact', 'NamingSystemContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem2', 'responsible', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NamingSystem2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NamingSystem2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem2', 'useContext', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem2', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem2', 'uniqueId', 'NamingSystemUniqueId', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem2', 'replacedBy', 'Reference(NamingSystem)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNamingSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystem2', nil, js.FHIRFactoryJs);
  defineNamingSystemPropsJs(js, def);
end;

procedure defineNutritionOrderOralDietPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDiet2', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet2', 'schedule', 'Timing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet2', 'nutrient', 'NutritionOrderOralDietNutrient', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet2', 'texture', 'NutritionOrderOralDietTexture', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet2', 'fluidConsistencyType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet2', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderOralDietJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDiet2', nil, js.FHIRFactoryJs);
  defineNutritionOrderOralDietPropsJs(js, def);
end;

procedure defineNutritionOrderOralDietNutrientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietNutrient2', 'modifier', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietNutrient2', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietNutrientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietNutrient2', nil, js.FHIRFactoryJs);
  defineNutritionOrderOralDietNutrientPropsJs(js, def);
end;

procedure defineNutritionOrderOralDietTexturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietTexture2', 'modifier', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietTexture2', 'foodType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietTextureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietTexture2', nil, js.FHIRFactoryJs);
  defineNutritionOrderOralDietTexturePropsJs(js, def);
end;

procedure defineNutritionOrderSupplementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderSupplement2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement2', 'productName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderSupplement2', 'schedule', 'Timing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderSupplement2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement2', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderSupplementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderSupplement2', nil, js.FHIRFactoryJs);
  defineNutritionOrderSupplementPropsJs(js, def);
end;

procedure defineNutritionOrderEnteralFormulaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormula2', 'baseFormulaType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula2', 'baseFormulaProductName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula2', 'additiveType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula2', 'additiveProductName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula2', 'caloricDensity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula2', 'routeofAdministration', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula2', 'administration', 'NutritionOrderEnteralFormulaAdministration', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula2', 'maxVolumeToDeliver', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula2', 'administrationInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderEnteralFormulaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormula2', nil, js.FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaPropsJs(js, def);
end;

procedure defineNutritionOrderEnteralFormulaAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration2', 'schedule', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration2', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration2', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderEnteralFormulaAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormulaAdministration2', nil, js.FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaAdministrationPropsJs(js, def);
end;

procedure defineNutritionOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'NutritionOrder2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder2', 'orderer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder2', 'dateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NutritionOrder2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrder2', 'allergyIntolerance', 'Reference(AllergyIntolerance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder2', 'foodPreferenceModifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder2', 'excludeFoodModifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder2', 'oralDiet', 'NutritionOrderOralDiet', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder2', 'supplement', 'NutritionOrderSupplement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder2', 'enteralFormula', 'NutritionOrderEnteralFormula', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrder2', nil, js.FHIRFactoryJs);
  defineNutritionOrderPropsJs(js, def);
end;

procedure defineObservationReferenceRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationReferenceRange2', 'low', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange2', 'high', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange2', 'meaning', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange2', 'age', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineObservationReferenceRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationReferenceRange2', nil, js.FHIRFactoryJs);
  defineObservationReferenceRangePropsJs(js, def);
end;

procedure defineObservationRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationRelated2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationRelated2', 'target', 'Reference(Observation|QuestionnaireResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineObservationRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationRelated2', nil, js.FHIRFactoryJs);
  defineObservationRelatedPropsJs(js, def);
end;

procedure defineObservationComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationComponent2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent2', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent2', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent2', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent2', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent2', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent2', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent2', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent2', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent2', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ObservationComponent2', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent2', 'dataAbsentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent2', 'referenceRange', '@Observation.referenceRange', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineObservationComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationComponent2', nil, js.FHIRFactoryJs);
  defineObservationComponentPropsJs(js, def);
end;

procedure defineObservationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Observation2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation2', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'subject', 'Reference(Patient|Group|Device|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation2', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation2', 'performer', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation2', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation2', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation2', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation2', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'dataAbsentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'interpretation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'comments', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation2', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'specimen', 'Reference(Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'device', 'Reference(Device|DeviceMetric)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation2', 'referenceRange', 'ObservationReferenceRange', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation2', 'related', 'ObservationRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation2', 'component', 'ObservationComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineObservationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Observation2', nil, js.FHIRFactoryJs);
  defineObservationPropsJs(js, def);
end;

procedure defineOperationDefinitionContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionContact2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionContact2', nil, js.FHIRFactoryJs);
  defineOperationDefinitionContactPropsJs(js, def);
end;

procedure defineOperationDefinitionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameter2', 'name', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter2', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter2', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'OperationDefinitionParameter2', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter2', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter2', 'profile', 'Reference(StructureDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinitionParameter2', 'binding', 'OperationDefinitionParameterBinding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinitionParameter2', 'part', '@OperationDefinition.parameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameter2', nil, js.FHIRFactoryJs);
  defineOperationDefinitionParameterPropsJs(js, def);
end;

procedure defineOperationDefinitionParameterBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameterBinding2', 'strength', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterBinding2', 'valueSetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterBinding2', 'valueSetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOperationDefinitionParameterBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameterBinding2', nil, js.FHIRFactoryJs);
  defineOperationDefinitionParameterBindingPropsJs(js, def);
end;

procedure defineOperationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OperationDefinition2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition2', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition2', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition2', 'contact', 'OperationDefinitionContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OperationDefinition2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition2', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition2', 'idempotent', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition2', 'notes', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition2', 'base', 'Reference(OperationDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinition2', 'system', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition2', 'instance', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition2', 'parameter', 'OperationDefinitionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinition2', nil, js.FHIRFactoryJs);
  defineOperationDefinitionPropsJs(js, def);
end;

procedure defineOperationOutcomeIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationOutcomeIssue2', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue2', 'details', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationOutcomeIssue2', 'diagnostics', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationOutcomeIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcomeIssue2', nil, js.FHIRFactoryJs);
  defineOperationOutcomeIssuePropsJs(js, def);
end;

procedure defineOperationOutcomePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OperationOutcome2', 'issue', 'OperationOutcomeIssue', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationOutcomeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcome2', nil, js.FHIRFactoryJs);
  defineOperationOutcomePropsJs(js, def);
end;

procedure defineOrderWhenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrderWhen2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrderWhen2', 'schedule', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOrderWhenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrderWhen2', nil, js.FHIRFactoryJs);
  defineOrderWhenPropsJs(js, def);
end;

procedure defineOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Order2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Order2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Order2', 'subject', 'Reference(Patient|Group|Device|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Order2', 'source', 'Reference(Practitioner|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Order2', 'target', 'Reference(Organization|Device|Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Order2', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Order2', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Order2', 'when', 'OrderWhen', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Order2', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Order2', nil, js.FHIRFactoryJs);
  defineOrderPropsJs(js, def);
end;

procedure defineOrderResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OrderResponse2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrderResponse2', 'request', 'Reference(Order)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrderResponse2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OrderResponse2', 'who', 'Reference(Practitioner|Organization|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrderResponse2', 'orderStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OrderResponse2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OrderResponse2', 'fulfillment', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrderResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrderResponse2', nil, js.FHIRFactoryJs);
  defineOrderResponsePropsJs(js, def);
end;

procedure defineOrganizationContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationContact2', 'purpose', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact2', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationContact2', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOrganizationContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationContact2', nil, js.FHIRFactoryJs);
  defineOrganizationContactPropsJs(js, def);
end;

procedure defineOrganizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Organization2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization2', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Organization2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Organization2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Organization2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization2', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization2', 'partOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Organization2', 'contact', 'OrganizationContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrganizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Organization2', nil, js.FHIRFactoryJs);
  defineOrganizationPropsJs(js, def);
end;

procedure definePatientContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientContact2', 'relationship', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PatientContact2', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PatientContact2', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact2', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PatientContact2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePatientContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientContact2', nil, js.FHIRFactoryJs);
  definePatientContactPropsJs(js, def);
end;

procedure definePatientAnimalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientAnimal2', 'species', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientAnimal2', 'breed', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientAnimal2', 'genderStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePatientAnimalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientAnimal2', nil, js.FHIRFactoryJs);
  definePatientAnimalPropsJs(js, def);
end;

procedure definePatientCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientCommunication2', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientCommunication2', 'preferred', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure definePatientCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientCommunication2', nil, js.FHIRFactoryJs);
  definePatientCommunicationPropsJs(js, def);
end;

procedure definePatientLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientLink2', 'other', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientLink2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePatientLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientLink2', nil, js.FHIRFactoryJs);
  definePatientLinkPropsJs(js, def);
end;

procedure definePatientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Patient2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient2', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient2', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient2', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Patient2', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Patient2', 'deceasedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient2', 'deceasedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Patient2', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient2', 'maritalStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient2', 'multipleBirthBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient2', 'multipleBirthInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Patient2', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient2', 'contact', 'PatientContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient2', 'animal', 'PatientAnimal', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient2', 'communication', 'PatientCommunication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient2', 'careProvider', 'Reference(Organization|Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient2', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient2', 'link', 'PatientLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePatientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Patient2', nil, js.FHIRFactoryJs);
  definePatientPropsJs(js, def);
end;

procedure definePaymentNoticePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentNotice2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentNotice2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice2', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice2', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice2', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice2', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice2', 'paymentStatus', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePaymentNoticeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentNotice2', nil, js.FHIRFactoryJs);
  definePaymentNoticePropsJs(js, def);
end;

procedure definePaymentReconciliationDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationDetail2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail2', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail2', 'responce', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail2', 'submitter', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail2', 'payee', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail2', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliationDetail2', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePaymentReconciliationDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationDetail2', nil, js.FHIRFactoryJs);
  definePaymentReconciliationDetailPropsJs(js, def);
end;

procedure definePaymentReconciliationNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationNote2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationNote2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePaymentReconciliationNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationNote2', nil, js.FHIRFactoryJs);
  definePaymentReconciliationNotePropsJs(js, def);
end;

procedure definePaymentReconciliationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliation2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation2', 'request', 'Reference(ProcessRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation2', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation2', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliation2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation2', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation2', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation2', 'detail', 'PaymentReconciliationDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation2', 'form', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation2', 'total', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation2', 'note', 'PaymentReconciliationNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePaymentReconciliationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliation2', nil, js.FHIRFactoryJs);
  definePaymentReconciliationPropsJs(js, def);
end;

procedure definePersonLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PersonLink2', 'target', 'Reference(Patient|Practitioner|RelatedPerson|Person)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PersonLink2', 'assurance', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePersonLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PersonLink2', nil, js.FHIRFactoryJs);
  definePersonLinkPropsJs(js, def);
end;

procedure definePersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Person2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person2', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person2', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Person2', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Person2', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person2', 'photo', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Person2', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Person2', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Person2', 'link', 'PersonLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Person2', nil, js.FHIRFactoryJs);
  definePersonPropsJs(js, def);
end;

procedure definePractitionerPractitionerRolePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerPractitionerRole2', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerPractitionerRole2', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerPractitionerRole2', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerPractitionerRole2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerPractitionerRole2', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerPractitionerRole2', 'healthcareService', 'Reference(HealthcareService)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePractitionerPractitionerRoleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerPractitionerRole2', nil, js.FHIRFactoryJs);
  definePractitionerPractitionerRolePropsJs(js, def);
end;

procedure definePractitionerQualificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerQualification2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerQualification2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification2', 'issuer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePractitionerQualificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerQualification2', nil, js.FHIRFactoryJs);
  definePractitionerQualificationPropsJs(js, def);
end;

procedure definePractitionerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Practitioner2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner2', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Practitioner2', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Practitioner2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner2', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner2', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Practitioner2', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Practitioner2', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner2', 'practitionerRole', 'PractitionerPractitionerRole', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner2', 'qualification', 'PractitionerQualification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner2', 'communication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePractitionerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Practitioner2', nil, js.FHIRFactoryJs);
  definePractitionerPropsJs(js, def);
end;

procedure defineProcedurePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedurePerformer2', 'actor', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer2', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedurePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedurePerformer2', nil, js.FHIRFactoryJs);
  defineProcedurePerformerPropsJs(js, def);
end;

procedure defineProcedureFocalDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedureFocalDevice2', 'action', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureFocalDevice2', 'manipulated', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedureFocalDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedureFocalDevice2', nil, js.FHIRFactoryJs);
  defineProcedureFocalDevicePropsJs(js, def);
end;

procedure defineProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Procedure2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure2', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Procedure2', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure2', 'notPerformed', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Procedure2', 'reasonNotPerformed', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure2', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure2', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure2', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure2', 'performer', 'ProcedurePerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure2', 'performedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Procedure2', 'performedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure2', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure2', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure2', 'report', 'Reference(DiagnosticReport)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure2', 'complication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure2', 'followUp', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure2', 'request', 'Reference(CarePlan|DiagnosticOrder|ProcedureRequest|ReferralRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure2', 'notes', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure2', 'focalDevice', 'ProcedureFocalDevice', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure2', 'used', 'Reference(Device|Medication|Substance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Procedure2', nil, js.FHIRFactoryJs);
  defineProcedurePropsJs(js, def);
end;

procedure defineProcedureRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcedureRequest2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest2', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest2', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest2', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest2', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest2', 'scheduledDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcedureRequest2', 'scheduledPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest2', 'scheduledTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest2', 'performer', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcedureRequest2', 'notes', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest2', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ProcedureRequest2', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest2', 'orderedOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcedureRequest2', 'orderer', 'Reference(Practitioner|Patient|RelatedPerson|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest2', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineProcedureRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedureRequest2', nil, js.FHIRFactoryJs);
  defineProcedureRequestPropsJs(js, def);
end;

procedure defineProcessRequestItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessRequestItem2', 'sequenceLinkId', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineProcessRequestItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessRequestItem2', nil, js.FHIRFactoryJs);
  defineProcessRequestItemPropsJs(js, def);
end;

procedure defineProcessRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessRequest2', 'action', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessRequest2', 'target', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest2', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest2', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest2', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest2', 'nullify', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ProcessRequest2', 'reference', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest2', 'item', 'ProcessRequestItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcessRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessRequest2', nil, js.FHIRFactoryJs);
  defineProcessRequestPropsJs(js, def);
end;

procedure defineProcessResponseNotesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessResponseNotes2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponseNotes2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineProcessResponseNotesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponseNotes2', nil, js.FHIRFactoryJs);
  defineProcessResponseNotesPropsJs(js, def);
end;

procedure defineProcessResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessResponse2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse2', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse2', 'outcome', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse2', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse2', 'ruleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse2', 'originalRuleset', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse2', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessResponse2', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse2', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse2', 'requestOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse2', 'form', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse2', 'notes', 'ProcessResponseNotes', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse2', 'error', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcessResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponse2', nil, js.FHIRFactoryJs);
  defineProcessResponsePropsJs(js, def);
end;

procedure defineProvenanceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceAgent2', 'role', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent2', 'actor', 'Reference(Practitioner|RelatedPerson|Patient|Device|Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent2', 'userId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent2', 'relatedAgent', 'ProvenanceAgentRelatedAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProvenanceAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceAgent2', nil, js.FHIRFactoryJs);
  defineProvenanceAgentPropsJs(js, def);
end;

procedure defineProvenanceAgentRelatedAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceAgentRelatedAgent2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgentRelatedAgent2', 'target', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineProvenanceAgentRelatedAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceAgentRelatedAgent2', nil, js.FHIRFactoryJs);
  defineProvenanceAgentRelatedAgentPropsJs(js, def);
end;

procedure defineProvenanceEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceEntity2', 'role', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity2', 'reference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity2', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity2', 'agent', '@Provenance.agent', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProvenanceEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceEntity2', nil, js.FHIRFactoryJs);
  defineProvenanceEntityPropsJs(js, def);
end;

procedure defineProvenancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Provenance2', 'target', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance2', 'recorded', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance2', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance2', 'activity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance2', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance2', 'agent', 'ProvenanceAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance2', 'entity', 'ProvenanceEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance2', 'signature', 'Signature', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProvenanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Provenance2', nil, js.FHIRFactoryJs);
  defineProvenancePropsJs(js, def);
end;

procedure defineQuestionnaireGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireGroup2', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroup2', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroup2', 'concept', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireGroup2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroup2', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireGroup2', 'repeats', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireGroup2', 'group', '@Questionnaire.group', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireGroup2', 'question', 'QuestionnaireGroupQuestion', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireGroup2', nil, js.FHIRFactoryJs);
  defineQuestionnaireGroupPropsJs(js, def);
end;

procedure defineQuestionnaireGroupQuestionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireGroupQuestion2', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion2', 'concept', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion2', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion2', 'repeats', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion2', 'options', 'Reference(ValueSet)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion2', 'option', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion2', 'group', '@Questionnaire.group', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireGroupQuestionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireGroupQuestion2', nil, js.FHIRFactoryJs);
  defineQuestionnaireGroupQuestionPropsJs(js, def);
end;

procedure defineQuestionnairePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Questionnaire2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire2', 'group', 'QuestionnaireGroup', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineQuestionnaireJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Questionnaire2', nil, js.FHIRFactoryJs);
  defineQuestionnairePropsJs(js, def);
end;

procedure defineQuestionnaireResponseGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseGroup2', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroup2', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroup2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroup2', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseGroup2', 'group', '@QuestionnaireResponse.group', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponseGroup2', 'question', 'QuestionnaireResponseGroupQuestion', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseGroup2', nil, js.FHIRFactoryJs);
  defineQuestionnaireResponseGroupPropsJs(js, def);
end;

procedure defineQuestionnaireResponseGroupQuestionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestion2', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestion2', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestion2', 'answer', 'QuestionnaireResponseGroupQuestionAnswer', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseGroupQuestionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseGroupQuestion2', nil, js.FHIRFactoryJs);
  defineQuestionnaireResponseGroupQuestionPropsJs(js, def);
end;

procedure defineQuestionnaireResponseGroupQuestionAnswerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer2', 'group', '@QuestionnaireResponse.group', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseGroupQuestionAnswerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseGroupQuestionAnswer2', nil, js.FHIRFactoryJs);
  defineQuestionnaireResponseGroupQuestionAnswerPropsJs(js, def);
end;

procedure defineQuestionnaireResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponse2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse2', 'questionnaire', 'Reference(Questionnaire)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse2', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse2', 'author', 'Reference(Device|Practitioner|Patient|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse2', 'authored', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponse2', 'source', 'Reference(Patient|Practitioner|RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse2', 'group', 'QuestionnaireResponseGroup', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineQuestionnaireResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponse2', nil, js.FHIRFactoryJs);
  defineQuestionnaireResponsePropsJs(js, def);
end;

procedure defineReferralRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ReferralRequest2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ReferralRequest2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ReferralRequest2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest2', 'specialty', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest2', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest2', 'requester', 'Reference(Practitioner|Organization|Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest2', 'recipient', 'Reference(Practitioner|Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest2', 'dateSent', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ReferralRequest2', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ReferralRequest2', 'serviceRequested', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest2', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest2', 'fulfillmentTime', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineReferralRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ReferralRequest2', nil, js.FHIRFactoryJs);
  defineReferralRequestPropsJs(js, def);
end;

procedure defineRelatedPersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RelatedPerson2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson2', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson2', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson2', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedPerson2', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RelatedPerson2', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson2', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson2', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRelatedPersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedPerson2', nil, js.FHIRFactoryJs);
  defineRelatedPersonPropsJs(js, def);
end;

procedure defineRiskAssessmentPredictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskAssessmentPrediction2', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction2', 'probabilityDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction2', 'probabilityRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction2', 'probabilityCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction2', 'relativeRisk', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction2', 'whenPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction2', 'whenRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction2', 'rationale', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRiskAssessmentPredictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessmentPrediction2', nil, js.FHIRFactoryJs);
  defineRiskAssessmentPredictionPropsJs(js, def);
end;

procedure defineRiskAssessmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RiskAssessment2', 'subject', 'Reference(Patient|Group)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskAssessment2', 'condition', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment2', 'performer', 'Reference(Practitioner|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment2', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment2', 'basis', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment2', 'prediction', 'RiskAssessmentPrediction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment2', 'mitigation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRiskAssessmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessment2', nil, js.FHIRFactoryJs);
  defineRiskAssessmentPropsJs(js, def);
end;

procedure defineSchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Schedule2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule2', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule2', 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Schedule2', 'planningHorizon', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Schedule2', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineScheduleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Schedule2', nil, js.FHIRFactoryJs);
  defineSchedulePropsJs(js, def);
end;

procedure defineSearchParameterContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SearchParameterContact2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameterContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSearchParameterContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameterContact2', nil, js.FHIRFactoryJs);
  defineSearchParameterContactPropsJs(js, def);
end;

procedure defineSearchParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SearchParameter2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter2', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter2', 'contact', 'SearchParameterContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SearchParameter2', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter2', 'base', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter2', 'xpath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter2', 'xpathUsage', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSearchParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameter2', nil, js.FHIRFactoryJs);
  defineSearchParameterPropsJs(js, def);
end;

procedure defineSlotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Slot2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot2', 'schedule', 'Reference(Schedule)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot2', 'freeBusyType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Slot2', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Slot2', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Slot2', 'overbooked', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Slot2', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSlotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Slot2', nil, js.FHIRFactoryJs);
  defineSlotPropsJs(js, def);
end;

procedure defineSpecimenCollectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenCollection2', 'collector', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection2', 'collectedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenCollection2', 'collectedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection2', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection2', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenCollectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenCollection2', nil, js.FHIRFactoryJs);
  defineSpecimenCollectionPropsJs(js, def);
end;

procedure defineSpecimenTreatmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenTreatment2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenTreatment2', 'procedure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenTreatment2', 'additive', 'Reference(Substance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenTreatmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenTreatment2', nil, js.FHIRFactoryJs);
  defineSpecimenTreatmentPropsJs(js, def);
end;

procedure defineSpecimenContainerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenContainer2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenContainer2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenContainer2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer2', 'capacity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer2', 'specimenQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer2', 'additiveCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer2', 'additiveReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenContainerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenContainer2', nil, js.FHIRFactoryJs);
  defineSpecimenContainerPropsJs(js, def);
end;

procedure defineSpecimenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Specimen2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Specimen2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen2', 'parent', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen2', 'subject', 'Reference(Patient|Group|Device|Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen2', 'accessionIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen2', 'receivedTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Specimen2', 'collection', 'SpecimenCollection', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen2', 'treatment', 'SpecimenTreatment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen2', 'container', 'SpecimenContainer', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Specimen2', nil, js.FHIRFactoryJs);
  defineSpecimenPropsJs(js, def);
end;

procedure defineStructureDefinitionContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionContact2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionContact2', nil, js.FHIRFactoryJs);
  defineStructureDefinitionContactPropsJs(js, def);
end;

procedure defineStructureDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionMapping2', 'identity', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping2', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping2', 'comments', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionMapping2', nil, js.FHIRFactoryJs);
  defineStructureDefinitionMappingPropsJs(js, def);
end;

procedure defineStructureDefinitionSnapshotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionSnapshot2', 'element', 'ElementDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionSnapshotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionSnapshot2', nil, js.FHIRFactoryJs);
  defineStructureDefinitionSnapshotPropsJs(js, def);
end;

procedure defineStructureDefinitionDifferentialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionDifferential2', 'element', 'ElementDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionDifferentialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionDifferential2', nil, js.FHIRFactoryJs);
  defineStructureDefinitionDifferentialPropsJs(js, def);
end;

procedure defineStructureDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'StructureDefinition2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'contact', 'StructureDefinitionContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureDefinition2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'useContext', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition2', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'copyright', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition2', 'fhirVersion', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'mapping', 'StructureDefinitionMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition2', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'constrainedType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'abstract', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition2', 'contextType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'base', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition2', 'snapshot', 'StructureDefinitionSnapshot', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureDefinition2', 'differential', 'StructureDefinitionDifferential', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineStructureDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinition2', nil, js.FHIRFactoryJs);
  defineStructureDefinitionPropsJs(js, def);
end;

procedure defineSubscriptionChannelPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubscriptionChannel2', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel2', 'endpoint', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel2', 'payload', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel2', 'header', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubscriptionChannelJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubscriptionChannel2', nil, js.FHIRFactoryJs);
  defineSubscriptionChannelPropsJs(js, def);
end;

procedure defineSubscriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Subscription2', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription2', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Subscription2', 'reason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription2', 'error', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription2', 'channel', 'SubscriptionChannel', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Subscription2', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Subscription2', 'tag', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubscriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Subscription2', nil, js.FHIRFactoryJs);
  defineSubscriptionPropsJs(js, def);
end;

procedure defineSubstanceInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceInstance2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceInstance2', 'expiry', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceInstance2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceInstance2', nil, js.FHIRFactoryJs);
  defineSubstanceInstancePropsJs(js, def);
end;

procedure defineSubstanceIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceIngredient2', 'quantity', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient2', 'substance', 'Reference(Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceIngredient2', nil, js.FHIRFactoryJs);
  defineSubstanceIngredientPropsJs(js, def);
end;

procedure defineSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Substance2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance2', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Substance2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Substance2', 'instance', 'SubstanceInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance2', 'ingredient', 'SubstanceIngredient', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Substance2', nil, js.FHIRFactoryJs);
  defineSubstancePropsJs(js, def);
end;

procedure defineSupplyDeliveryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyDelivery2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyDelivery2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery2', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery2', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery2', 'suppliedItem', 'Reference(Medication|Substance|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery2', 'supplier', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery2', 'whenPrepared', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery2', 'time', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyDelivery2', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery2', 'receiver', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSupplyDeliveryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDelivery2', nil, js.FHIRFactoryJs);
  defineSupplyDeliveryPropsJs(js, def);
end;

procedure defineSupplyRequestWhenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyRequestWhen2', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestWhen2', 'schedule', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyRequestWhenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequestWhen2', nil, js.FHIRFactoryJs);
  defineSupplyRequestWhenPropsJs(js, def);
end;

procedure defineSupplyRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyRequest2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest2', 'source', 'Reference(Practitioner|Organization|Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest2', 'kind', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest2', 'orderedItem', 'Reference(Medication|Substance|Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest2', 'supplier', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest2', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest2', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest2', 'when', 'SupplyRequestWhen', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequest2', nil, js.FHIRFactoryJs);
  defineSupplyRequestPropsJs(js, def);
end;

procedure defineTestScriptContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptContact2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptContact2', nil, js.FHIRFactoryJs);
  defineTestScriptContactPropsJs(js, def);
end;

procedure defineTestScriptMetadataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadata2', 'link', 'TestScriptMetadataLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScriptMetadata2', 'capability', 'TestScriptMetadataCapability', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptMetadataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadata2', nil, js.FHIRFactoryJs);
  defineTestScriptMetadataPropsJs(js, def);
end;

procedure defineTestScriptMetadataLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataLink2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataLink2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptMetadataLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataLink2', nil, js.FHIRFactoryJs);
  defineTestScriptMetadataLinkPropsJs(js, def);
end;

procedure defineTestScriptMetadataCapabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataCapability2', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability2', 'validated', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataCapability2', 'destination', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptMetadataCapability2', 'conformance', 'Reference(Conformance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptMetadataCapabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataCapability2', nil, js.FHIRFactoryJs);
  defineTestScriptMetadataCapabilityPropsJs(js, def);
end;

procedure defineTestScriptFixturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptFixture2', 'autocreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture2', 'autodelete', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture2', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptFixtureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptFixture2', nil, js.FHIRFactoryJs);
  defineTestScriptFixturePropsJs(js, def);
end;

procedure defineTestScriptVariablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptVariable2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable2', 'headerField', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable2', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable2', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptVariableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptVariable2', nil, js.FHIRFactoryJs);
  defineTestScriptVariablePropsJs(js, def);
end;

procedure defineTestScriptSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetup2', 'metadata', '@TestScript.metadata', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetup2', 'action', 'TestScriptSetupAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetup2', nil, js.FHIRFactoryJs);
  defineTestScriptSetupPropsJs(js, def);
end;

procedure defineTestScriptSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupAction2', 'operation', 'TestScriptSetupActionOperation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupAction2', 'assert', 'TestScriptSetupActionAssert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupAction2', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionPropsJs(js, def);
end;

procedure defineTestScriptSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'resource', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'accept', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'destination', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'encodeRequestUrl', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'params', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'requestHeader', 'TestScriptSetupActionOperationRequestHeader', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'responseId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'targetId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation2', 'url', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperation2', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionOperationPropsJs(js, def);
end;

procedure defineTestScriptSetupActionOperationRequestHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader2', 'field', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader2', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationRequestHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperationRequestHeader2', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionOperationRequestHeaderPropsJs(js, def);
end;

procedure defineTestScriptSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'direction', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'compareToSourceId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'compareToSourcePath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'headerField', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'minimumId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'navigationLinks', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'operator', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'resource', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'response', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'responseCode', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'validateProfileId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert2', 'warningOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTestScriptSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssert2', nil, js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertPropsJs(js, def);
end;

procedure defineTestScriptTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTest2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest2', 'metadata', '@TestScript.metadata', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptTest2', 'action', 'TestScriptTestAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTest2', nil, js.FHIRFactoryJs);
  defineTestScriptTestPropsJs(js, def);
end;

procedure defineTestScriptTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTestAction2', 'operation', '@TestScript.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptTestAction2', 'assert', '@TestScript.setup.action.assert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTestAction2', nil, js.FHIRFactoryJs);
  defineTestScriptTestActionPropsJs(js, def);
end;

procedure defineTestScriptTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardown2', 'action', 'TestScriptTeardownAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardown2', nil, js.FHIRFactoryJs);
  defineTestScriptTeardownPropsJs(js, def);
end;

procedure defineTestScriptTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardownAction2', 'operation', '@TestScript.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardownAction2', nil, js.FHIRFactoryJs);
  defineTestScriptTeardownActionPropsJs(js, def);
end;

procedure defineTestScriptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'TestScript2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript2', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScript2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript2', 'contact', 'TestScriptContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TestScript2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript2', 'useContext', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript2', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript2', 'copyright', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript2', 'metadata', 'TestScriptMetadata', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript2', 'multiserver', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScript2', 'fixture', 'TestScriptFixture', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript2', 'profile', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript2', 'variable', 'TestScriptVariable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript2', 'setup', 'TestScriptSetup', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript2', 'test', 'TestScriptTest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript2', 'teardown', 'TestScriptTeardown', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScript2', nil, js.FHIRFactoryJs);
  defineTestScriptPropsJs(js, def);
end;

procedure defineValueSetContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetContact2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetContact2', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetContact2', nil, js.FHIRFactoryJs);
  defineValueSetContactPropsJs(js, def);
end;

procedure defineValueSetCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCodeSystem2', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystem2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystem2', 'caseSensitive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetCodeSystem2', 'concept', 'ValueSetCodeSystemConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCodeSystem2', nil, js.FHIRFactoryJs);
  defineValueSetCodeSystemPropsJs(js, def);
end;

procedure defineValueSetCodeSystemConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCodeSystemConcept2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystemConcept2', 'abstract', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetCodeSystemConcept2', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystemConcept2', 'definition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystemConcept2', 'designation', 'ValueSetCodeSystemConceptDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetCodeSystemConcept2', 'concept', '@ValueSet.codeSystem.concept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetCodeSystemConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCodeSystemConcept2', nil, js.FHIRFactoryJs);
  defineValueSetCodeSystemConceptPropsJs(js, def);
end;

procedure defineValueSetCodeSystemConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCodeSystemConceptDesignation2', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystemConceptDesignation2', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSetCodeSystemConceptDesignation2', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetCodeSystemConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCodeSystemConceptDesignation2', nil, js.FHIRFactoryJs);
  defineValueSetCodeSystemConceptDesignationPropsJs(js, def);
end;

procedure defineValueSetComposePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCompose2', 'include', 'ValueSetComposeInclude', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetCompose2', 'exclude', '@ValueSet.compose.include', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCompose2', nil, js.FHIRFactoryJs);
  defineValueSetComposePropsJs(js, def);
end;

procedure defineValueSetComposeIncludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeInclude2', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude2', 'concept', 'ValueSetComposeIncludeConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetComposeInclude2', 'filter', 'ValueSetComposeIncludeFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeInclude2', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludePropsJs(js, def);
end;

procedure defineValueSetComposeIncludeConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConcept2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept2', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept2', 'designation', '@ValueSet.codeSystem.concept.designation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConcept2', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludeConceptPropsJs(js, def);
end;

procedure defineValueSetComposeIncludeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeFilter2', 'property', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter2', 'op', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter2', 'value', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeFilter2', nil, js.FHIRFactoryJs);
  defineValueSetComposeIncludeFilterPropsJs(js, def);
end;

procedure defineValueSetExpansionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansion2', 'identifier', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansion2', 'timestamp', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetExpansion2', 'total', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion2', 'offset', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion2', 'parameter', 'ValueSetExpansionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansion2', 'contains', 'ValueSetExpansionContains', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansion2', nil, js.FHIRFactoryJs);
  defineValueSetExpansionPropsJs(js, def);
end;

procedure defineValueSetExpansionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionParameter2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter2', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter2', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionParameter2', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansionParameter2', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ValueSetExpansionParameter2', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter2', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetExpansionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionParameter2', nil, js.FHIRFactoryJs);
  defineValueSetExpansionParameterPropsJs(js, def);
end;

procedure defineValueSetExpansionContainsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionContains2', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains2', 'abstract', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains2', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains2', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains2', 'contains', '@ValueSet.expansion.contains', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetExpansionContainsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionContains2', nil, js.FHIRFactoryJs);
  defineValueSetExpansionContainsPropsJs(js, def);
end;

procedure defineValueSetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ValueSet2', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet2', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSet2', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet2', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet2', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet2', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet2', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet2', 'contact', 'ValueSetContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet2', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSet2', 'lockedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSet2', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet2', 'useContext', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet2', 'immutable', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet2', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet2', 'copyright', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet2', 'extensible', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet2', 'codeSystem', 'ValueSetCodeSystem', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSet2', 'compose', 'ValueSetCompose', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSet2', 'expansion', 'ValueSetExpansion', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineValueSetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSet2', nil, js.FHIRFactoryJs);
  defineValueSetPropsJs(js, def);
end;

procedure defineVisionPrescriptionDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'product', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'eye', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'sphere', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'cylinder', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'axis', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'prism', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'base', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'add', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'power', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'backCurve', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'diameter', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'duration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'color', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'brand', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense2', 'notes', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineVisionPrescriptionDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescriptionDispense2', nil, js.FHIRFactoryJs);
  defineVisionPrescriptionDispensePropsJs(js, def);
end;

procedure defineVisionPrescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VisionPrescription2', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VisionPrescription2', 'dateWritten', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VisionPrescription2', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription2', 'prescriber', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription2', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription2', 'reasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription2', 'reasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription2', 'dispense', 'VisionPrescriptionDispense', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVisionPrescriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescription2', nil, js.FHIRFactoryJs);
  defineVisionPrescriptionPropsJs(js, def);
end;

procedure registerFHIRTypes(js : TFHIRJavascript);

begin

  defineParametersParameterJs(js); 
  defineParametersJs(js); 
  defineExtensionJs(js); 
  defineNarrativeJs(js); 
  defineIdentifierJs(js); 
  defineCodingJs(js); 
  defineReferenceJs(js); 
  defineSignatureJs(js); 
  defineSampledDataJs(js); 
  definePeriodJs(js); 
  defineQuantityJs(js); 
  defineAttachmentJs(js); 
  defineRatioJs(js); 
  defineRangeJs(js); 
  defineAnnotationJs(js); 
  defineCodeableConceptJs(js); 
  defineHumanNameJs(js); 
  defineMetaJs(js); 
  defineContactPointJs(js); 
  defineAddressJs(js); 
  defineElementDefinitionSlicingJs(js); 
  defineElementDefinitionBaseJs(js); 
  defineElementDefinitionTypeJs(js); 
  defineElementDefinitionConstraintJs(js); 
  defineElementDefinitionBindingJs(js); 
  defineElementDefinitionMappingJs(js); 
  defineElementDefinitionJs(js); 
  defineTimingRepeatJs(js); 
  defineTimingJs(js); 
  defineAccountJs(js); 
  defineAllergyIntoleranceReactionJs(js); 
  defineAllergyIntoleranceJs(js); 
  defineAppointmentParticipantJs(js); 
  defineAppointmentJs(js); 
  defineAppointmentResponseJs(js); 
  defineAuditEventEventJs(js); 
  defineAuditEventParticipantJs(js); 
  defineAuditEventParticipantNetworkJs(js); 
  defineAuditEventSourceJs(js); 
  defineAuditEventObjectJs(js); 
  defineAuditEventObjectDetailJs(js); 
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
  defineCarePlanRelatedPlanJs(js); 
  defineCarePlanParticipantJs(js); 
  defineCarePlanActivityJs(js); 
  defineCarePlanActivityDetailJs(js); 
  defineCarePlanJs(js); 
  defineClaimPayeeJs(js); 
  defineClaimDiagnosisJs(js); 
  defineClaimCoverageJs(js); 
  defineClaimItemJs(js); 
  defineClaimItemDetailJs(js); 
  defineClaimItemDetailSubDetailJs(js); 
  defineClaimItemProsthesisJs(js); 
  defineClaimMissingTeethJs(js); 
  defineClaimJs(js); 
  defineClaimResponseItemJs(js); 
  defineClaimResponseItemAdjudicationJs(js); 
  defineClaimResponseItemDetailJs(js); 
  defineClaimResponseItemDetailAdjudicationJs(js); 
  defineClaimResponseItemDetailSubDetailJs(js); 
  defineClaimResponseItemDetailSubDetailAdjudicationJs(js); 
  defineClaimResponseAddItemJs(js); 
  defineClaimResponseAddItemAdjudicationJs(js); 
  defineClaimResponseAddItemDetailJs(js); 
  defineClaimResponseAddItemDetailAdjudicationJs(js); 
  defineClaimResponseErrorJs(js); 
  defineClaimResponseNoteJs(js); 
  defineClaimResponseCoverageJs(js); 
  defineClaimResponseJs(js); 
  defineClinicalImpressionInvestigationsJs(js); 
  defineClinicalImpressionFindingJs(js); 
  defineClinicalImpressionRuledOutJs(js); 
  defineClinicalImpressionJs(js); 
  defineCommunicationPayloadJs(js); 
  defineCommunicationJs(js); 
  defineCommunicationRequestPayloadJs(js); 
  defineCommunicationRequestJs(js); 
  defineCompositionAttesterJs(js); 
  defineCompositionEventJs(js); 
  defineCompositionSectionJs(js); 
  defineCompositionJs(js); 
  defineConceptMapContactJs(js); 
  defineConceptMapElementJs(js); 
  defineConceptMapElementTargetJs(js); 
  defineConceptMapElementTargetDependsOnJs(js); 
  defineConceptMapJs(js); 
  defineConditionStageJs(js); 
  defineConditionEvidenceJs(js); 
  defineConditionJs(js); 
  defineConformanceContactJs(js); 
  defineConformanceSoftwareJs(js); 
  defineConformanceImplementationJs(js); 
  defineConformanceRestJs(js); 
  defineConformanceRestSecurityJs(js); 
  defineConformanceRestSecurityCertificateJs(js); 
  defineConformanceRestResourceJs(js); 
  defineConformanceRestResourceInteractionJs(js); 
  defineConformanceRestResourceSearchParamJs(js); 
  defineConformanceRestInteractionJs(js); 
  defineConformanceRestOperationJs(js); 
  defineConformanceMessagingJs(js); 
  defineConformanceMessagingEndpointJs(js); 
  defineConformanceMessagingEventJs(js); 
  defineConformanceDocumentJs(js); 
  defineConformanceJs(js); 
  defineContractActorJs(js); 
  defineContractValuedItemJs(js); 
  defineContractSignerJs(js); 
  defineContractTermJs(js); 
  defineContractTermActorJs(js); 
  defineContractTermValuedItemJs(js); 
  defineContractFriendlyJs(js); 
  defineContractLegalJs(js); 
  defineContractRuleJs(js); 
  defineContractJs(js); 
  defineCoverageJs(js); 
  defineDataElementContactJs(js); 
  defineDataElementMappingJs(js); 
  defineDataElementJs(js); 
  defineDetectedIssueMitigationJs(js); 
  defineDetectedIssueJs(js); 
  defineDeviceJs(js); 
  defineDeviceComponentProductionSpecificationJs(js); 
  defineDeviceComponentJs(js); 
  defineDeviceMetricCalibrationJs(js); 
  defineDeviceMetricJs(js); 
  defineDeviceUseRequestJs(js); 
  defineDeviceUseStatementJs(js); 
  defineDiagnosticOrderEventJs(js); 
  defineDiagnosticOrderItemJs(js); 
  defineDiagnosticOrderJs(js); 
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
  defineEligibilityResponseJs(js); 
  defineEncounterStatusHistoryJs(js); 
  defineEncounterParticipantJs(js); 
  defineEncounterHospitalizationJs(js); 
  defineEncounterLocationJs(js); 
  defineEncounterJs(js); 
  defineEnrollmentRequestJs(js); 
  defineEnrollmentResponseJs(js); 
  defineEpisodeOfCareStatusHistoryJs(js); 
  defineEpisodeOfCareCareTeamJs(js); 
  defineEpisodeOfCareJs(js); 
  defineExplanationOfBenefitJs(js); 
  defineFamilyMemberHistoryConditionJs(js); 
  defineFamilyMemberHistoryJs(js); 
  defineFlagJs(js); 
  defineGoalOutcomeJs(js); 
  defineGoalJs(js); 
  defineGroupCharacteristicJs(js); 
  defineGroupMemberJs(js); 
  defineGroupJs(js); 
  defineHealthcareServiceServiceTypeJs(js); 
  defineHealthcareServiceAvailableTimeJs(js); 
  defineHealthcareServiceNotAvailableJs(js); 
  defineHealthcareServiceJs(js); 
  defineImagingObjectSelectionStudyJs(js); 
  defineImagingObjectSelectionStudySeriesJs(js); 
  defineImagingObjectSelectionStudySeriesInstanceJs(js); 
  defineImagingObjectSelectionStudySeriesInstanceFramesJs(js); 
  defineImagingObjectSelectionJs(js); 
  defineImagingStudySeriesJs(js); 
  defineImagingStudySeriesInstanceJs(js); 
  defineImagingStudyJs(js); 
  defineImmunizationExplanationJs(js); 
  defineImmunizationReactionJs(js); 
  defineImmunizationVaccinationProtocolJs(js); 
  defineImmunizationJs(js); 
  defineImmunizationRecommendationRecommendationJs(js); 
  defineImmunizationRecommendationRecommendationDateCriterionJs(js); 
  defineImmunizationRecommendationRecommendationProtocolJs(js); 
  defineImmunizationRecommendationJs(js); 
  defineImplementationGuideContactJs(js); 
  defineImplementationGuideDependencyJs(js); 
  defineImplementationGuidePackageJs(js); 
  defineImplementationGuidePackageResourceJs(js); 
  defineImplementationGuideGlobalJs(js); 
  defineImplementationGuidePageJs(js); 
  defineImplementationGuideJs(js); 
  defineListEntryJs(js); 
  defineListJs(js); 
  defineLocationPositionJs(js); 
  defineLocationJs(js); 
  defineMediaJs(js); 
  defineMedicationProductJs(js); 
  defineMedicationProductIngredientJs(js); 
  defineMedicationProductBatchJs(js); 
  defineMedicationPackageJs(js); 
  defineMedicationPackageContentJs(js); 
  defineMedicationJs(js); 
  defineMedicationAdministrationDosageJs(js); 
  defineMedicationAdministrationJs(js); 
  defineMedicationDispenseDosageInstructionJs(js); 
  defineMedicationDispenseSubstitutionJs(js); 
  defineMedicationDispenseJs(js); 
  defineMedicationOrderDosageInstructionJs(js); 
  defineMedicationOrderDispenseRequestJs(js); 
  defineMedicationOrderSubstitutionJs(js); 
  defineMedicationOrderJs(js); 
  defineMedicationStatementDosageJs(js); 
  defineMedicationStatementJs(js); 
  defineMessageHeaderResponseJs(js); 
  defineMessageHeaderSourceJs(js); 
  defineMessageHeaderDestinationJs(js); 
  defineMessageHeaderJs(js); 
  defineNamingSystemContactJs(js); 
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
  defineOperationDefinitionContactJs(js); 
  defineOperationDefinitionParameterJs(js); 
  defineOperationDefinitionParameterBindingJs(js); 
  defineOperationDefinitionJs(js); 
  defineOperationOutcomeIssueJs(js); 
  defineOperationOutcomeJs(js); 
  defineOrderWhenJs(js); 
  defineOrderJs(js); 
  defineOrderResponseJs(js); 
  defineOrganizationContactJs(js); 
  defineOrganizationJs(js); 
  definePatientContactJs(js); 
  definePatientAnimalJs(js); 
  definePatientCommunicationJs(js); 
  definePatientLinkJs(js); 
  definePatientJs(js); 
  definePaymentNoticeJs(js); 
  definePaymentReconciliationDetailJs(js); 
  definePaymentReconciliationNoteJs(js); 
  definePaymentReconciliationJs(js); 
  definePersonLinkJs(js); 
  definePersonJs(js); 
  definePractitionerPractitionerRoleJs(js); 
  definePractitionerQualificationJs(js); 
  definePractitionerJs(js); 
  defineProcedurePerformerJs(js); 
  defineProcedureFocalDeviceJs(js); 
  defineProcedureJs(js); 
  defineProcedureRequestJs(js); 
  defineProcessRequestItemJs(js); 
  defineProcessRequestJs(js); 
  defineProcessResponseNotesJs(js); 
  defineProcessResponseJs(js); 
  defineProvenanceAgentJs(js); 
  defineProvenanceAgentRelatedAgentJs(js); 
  defineProvenanceEntityJs(js); 
  defineProvenanceJs(js); 
  defineQuestionnaireGroupJs(js); 
  defineQuestionnaireGroupQuestionJs(js); 
  defineQuestionnaireJs(js); 
  defineQuestionnaireResponseGroupJs(js); 
  defineQuestionnaireResponseGroupQuestionJs(js); 
  defineQuestionnaireResponseGroupQuestionAnswerJs(js); 
  defineQuestionnaireResponseJs(js); 
  defineReferralRequestJs(js); 
  defineRelatedPersonJs(js); 
  defineRiskAssessmentPredictionJs(js); 
  defineRiskAssessmentJs(js); 
  defineScheduleJs(js); 
  defineSearchParameterContactJs(js); 
  defineSearchParameterJs(js); 
  defineSlotJs(js); 
  defineSpecimenCollectionJs(js); 
  defineSpecimenTreatmentJs(js); 
  defineSpecimenContainerJs(js); 
  defineSpecimenJs(js); 
  defineStructureDefinitionContactJs(js); 
  defineStructureDefinitionMappingJs(js); 
  defineStructureDefinitionSnapshotJs(js); 
  defineStructureDefinitionDifferentialJs(js); 
  defineStructureDefinitionJs(js); 
  defineSubscriptionChannelJs(js); 
  defineSubscriptionJs(js); 
  defineSubstanceInstanceJs(js); 
  defineSubstanceIngredientJs(js); 
  defineSubstanceJs(js); 
  defineSupplyDeliveryJs(js); 
  defineSupplyRequestWhenJs(js); 
  defineSupplyRequestJs(js); 
  defineTestScriptContactJs(js); 
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
  defineValueSetContactJs(js); 
  defineValueSetCodeSystemJs(js); 
  defineValueSetCodeSystemConceptJs(js); 
  defineValueSetCodeSystemConceptDesignationJs(js); 
  defineValueSetComposeJs(js); 
  defineValueSetComposeIncludeJs(js); 
  defineValueSetComposeIncludeConceptJs(js); 
  defineValueSetComposeIncludeFilterJs(js); 
  defineValueSetExpansionJs(js); 
  defineValueSetExpansionParameterJs(js); 
  defineValueSetExpansionContainsJs(js); 
  defineValueSetJs(js); 
  defineVisionPrescriptionDispenseJs(js); 
  defineVisionPrescriptionJs(js); 

end;

end.

