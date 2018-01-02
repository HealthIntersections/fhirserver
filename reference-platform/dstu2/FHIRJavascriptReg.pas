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

{$IFNDEF FHIR2}
This is the dstu2 version of the FHIR code
{$ENDIF}


interface

// FHIR v1.0.2 generated 2015-10-24T07:41:03+11:00

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


procedure defineParametersParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ParametersParameter', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'value*', '*', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineDomainResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'DomainResource', 'text', 'Narrative', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DomainResource', 'contained', 'Resource', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DomainResource', 'extension', 'Extension', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DomainResource', 'modifierExtension', 'Extension', getFHIRArrayProp, setFHIRArrayProp);
end;


procedure defineExtensionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Extension', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Extension', 'value*', '*', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'Narrative', 'div', 'xhtml', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineNarrativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Narrative', nil, 'Narrative', FHIRFactoryJs);
  defineNarrativePropsJs(js, def);
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


procedure defineReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Reference', 'reference', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Reference', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Reference', nil, 'Reference', FHIRFactoryJs);
  defineReferencePropsJs(js, def);
end;


procedure defineSignaturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Signature', 'type', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Signature', 'when', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Signature', 'whoUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Signature', 'whoReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Signature', 'contentType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Signature', 'blob', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
end;

procedure defineSignatureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Signature', nil, 'Signature', FHIRFactoryJs);
  defineSignaturePropsJs(js, def);
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


procedure defineHumanNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'HumanName', 'use', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HumanName', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
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


procedure defineElementDefinitionBasePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBase', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase', 'min', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
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
  js.registerElement(def, 'ElementDefinitionType', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineElementDefinitionTypeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionType', nil, 'ElementDefinitionType', FHIRFactoryJs);
  defineElementDefinitionTypePropsJs(js, def);
end;


procedure defineElementDefinitionConstraintPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionConstraint', 'key', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'requirements', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'severity', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'human', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'xpath', 'string', getFHIRStringProp, setFHIRStringProp);
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
  js.registerElement(def, 'ElementDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'label', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'code', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition', 'slicing', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'short', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'definition', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'comments', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'requirements', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'min', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'max', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'base', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'type', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition', 'nameReference', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValue*', '*', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'meaningWhenMissing', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixed*', '*', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'pattern*', '*', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'example*', '*', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'minValue*', '*', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'maxValue*', '*', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'TimingRepeat', 'boundsQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat', 'boundsRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat', 'boundsPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat', 'count', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'duration', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'durationMax', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'durationUnits', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat', 'frequency', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'frequencyMax', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'period', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'periodMax', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'periodUnits', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat', 'when', 'code', getFHIRStringProp, setFHIRStringProp);
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


procedure defineAccountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Account', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Account', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Account', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Account', 'activePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'currency', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'balance', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'coveragePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'subject', 'Reference(Patient|Device|Practitioner|Location|HealthcareService|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'owner', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineAccountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Account', nil, 'Account', FHIRFactoryJs);
  defineAccountPropsJs(js, def);
end;


procedure defineAllergyIntoleranceReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'substance', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'certainty', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'manifestation', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'onset', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'severity', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'exposureRoute', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'note', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'AllergyIntolerance', 'onset', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'recordedDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'recorder', 'Reference(Practitioner|Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'reporter', 'Reference(Patient|RelatedPerson|Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'substance', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'criticality', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'category', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'lastOccurence', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'note', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'Appointment', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Appointment', 'reason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Appointment', 'priority', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'start', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment', 'end', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment', 'minutesDuration', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Appointment', 'slot', 'Reference(Slot)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
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


procedure defineAuditEventEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEvent', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEvent', 'subtype', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AuditEventEvent', 'action', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventEvent', 'dateTime', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AuditEventEvent', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventEvent', 'outcomeDesc', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventEvent', 'purposeOfEvent', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAuditEventEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEvent', nil, 'AuditEventEvent', FHIRFactoryJs);
  defineAuditEventEventPropsJs(js, def);
end;


procedure defineAuditEventParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventParticipant', 'role', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AuditEventParticipant', 'reference', 'Reference(Practitioner|Organization|Device|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventParticipant', 'userId', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventParticipant', 'altId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventParticipant', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventParticipant', 'requestor', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'AuditEventParticipant', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventParticipant', 'media', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventParticipant', 'network', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventParticipant', 'purposeOfUse', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAuditEventParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventParticipant', nil, 'AuditEventParticipant', FHIRFactoryJs);
  defineAuditEventParticipantPropsJs(js, def);
end;


procedure defineAuditEventParticipantNetworkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventParticipantNetwork', 'address', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventParticipantNetwork', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineAuditEventParticipantNetworkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventParticipantNetwork', nil, 'AuditEventParticipantNetwork', FHIRFactoryJs);
  defineAuditEventParticipantNetworkPropsJs(js, def);
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


procedure defineAuditEventObjectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventObject', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventObject', 'reference', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventObject', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventObject', 'role', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventObject', 'lifecycle', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventObject', 'securityLabel', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AuditEventObject', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventObject', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventObject', 'query', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'AuditEventObject', 'detail', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAuditEventObjectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventObject', nil, 'AuditEventObject', FHIRFactoryJs);
  defineAuditEventObjectPropsJs(js, def);
end;


procedure defineAuditEventObjectDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventObjectDetail', 'type', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventObjectDetail', 'value', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
end;

procedure defineAuditEventObjectDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventObjectDetail', nil, 'AuditEventObjectDetail', FHIRFactoryJs);
  defineAuditEventObjectDetailPropsJs(js, def);
end;


procedure defineAuditEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AuditEvent', 'event', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent', 'source', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent', 'object', '', getFHIRArrayProp, setFHIRArrayProp);
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
  js.registerElement(def, 'Basic', 'author', 'Reference(Practitioner|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Basic', 'created', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
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
  js.registerElement(def, 'Binary', 'content', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
end;

procedure defineBinaryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Binary', nil, 'Binary', FHIRFactoryJs);
  defineBinaryPropsJs(js, def);
end;


procedure defineBodySitePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BodySite', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BodySite', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'BodySite', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BodySite', 'modifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'BodySite', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BodySite', 'image', 'Attachment', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineBodySiteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BodySite', nil, 'BodySite', FHIRFactoryJs);
  defineBodySitePropsJs(js, def);
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
  js.registerElement(def, 'Bundle', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
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


procedure defineCarePlanRelatedPlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanRelatedPlan', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlanRelatedPlan', 'plan', 'Reference(CarePlan)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCarePlanRelatedPlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanRelatedPlan', nil, 'CarePlanRelatedPlan', FHIRFactoryJs);
  defineCarePlanRelatedPlanPropsJs(js, def);
end;


procedure defineCarePlanParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanParticipant', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanParticipant', 'member', 'Reference(Practitioner|RelatedPerson|Patient|Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCarePlanParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanParticipant', nil, 'CarePlanParticipant', FHIRFactoryJs);
  defineCarePlanParticipantPropsJs(js, def);
end;


procedure defineCarePlanActivityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivity', 'actionResulting', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity', 'progress', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity', 'reference', 'Reference(Appointment|CommunicationRequest|DeviceUseRequest|DiagnosticOrder|MedicationOrder|NutritionOrder|Order|ProcedureRequest|ProcessRequest|ReferralRequest|SupplyRequest|VisionPrescription)', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'CarePlanActivityDetail', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'reasonReference', 'Reference(Condition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'goal', 'Reference(Goal)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'statusReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'prohibited', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'performer', 'Reference(Practitioner|Organization|RelatedPerson|Patient)', getFHIRArrayProp, setFHIRArrayProp);
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
  js.registerElement(def, 'CarePlan', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlan', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'author', 'Reference(Patient|Practitioner|RelatedPerson|Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'modified', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'CarePlan', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlan', 'addresses', 'Reference(Condition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'support', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'relatedPlan', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'goal', 'Reference(Goal)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'activity', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'note', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCarePlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlan', nil, 'CarePlan', FHIRFactoryJs);
  defineCarePlanPropsJs(js, def);
end;


procedure defineClaimPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimPayee', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee', 'person', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimPayee', nil, 'ClaimPayee', FHIRFactoryJs);
  defineClaimPayeePropsJs(js, def);
end;


procedure defineClaimDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimDiagnosis', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimDiagnosis', 'diagnosis', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimDiagnosis', nil, 'ClaimDiagnosis', FHIRFactoryJs);
  defineClaimDiagnosisPropsJs(js, def);
end;


procedure defineClaimCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimCoverage', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimCoverage', 'focal', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ClaimCoverage', 'coverage', 'Reference(Coverage)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimCoverage', 'businessArrangement', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimCoverage', 'relationship', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimCoverage', 'claimResponse', 'Reference(ClaimResponse)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimCoverage', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimCoverage', nil, 'ClaimCoverage', FHIRFactoryJs);
  defineClaimCoveragePropsJs(js, def);
end;


procedure defineClaimItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItem', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItem', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'service', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'serviceDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimItem', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'unitPrice', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItem', 'points', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItem', 'net', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'udi', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'bodySite', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'subSite', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'modifier', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'detail', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'prosthesis', '', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'ClaimItemDetail', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'service', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'unitPrice', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetail', 'points', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetail', 'net', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'udi', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'service', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'unitPrice', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'points', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'net', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'udi', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetailSubDetail', nil, 'ClaimItemDetailSubDetail', FHIRFactoryJs);
  defineClaimItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimItemProsthesisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemProsthesis', 'initial', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ClaimItemProsthesis', 'priorDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimItemProsthesis', 'priorMaterial', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimItemProsthesisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemProsthesis', nil, 'ClaimItemProsthesis', FHIRFactoryJs);
  defineClaimItemProsthesisPropsJs(js, def);
end;


procedure defineClaimMissingTeethPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimMissingTeeth', 'tooth', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimMissingTeeth', 'reason', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimMissingTeeth', 'extractionDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineClaimMissingTeethJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimMissingTeeth', nil, 'ClaimMissingTeeth', FHIRFactoryJs);
  defineClaimMissingTeethPropsJs(js, def);
end;


procedure defineClaimPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Claim', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Claim', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Claim', 'target', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'use', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Claim', 'priority', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'fundsReserve', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'enterer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'facility', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'prescription', 'Reference(MedicationOrder|VisionPrescription)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'originalPrescription', 'Reference(MedicationOrder)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'payee', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'referral', 'Reference(ReferralRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'diagnosis', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'condition', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'coverage', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'exception', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'school', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Claim', 'accident', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Claim', 'accidentType', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'interventionException', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'additionalMaterials', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'missingTeeth', '', getFHIRArrayProp, setFHIRArrayProp);
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
  js.registerElement(def, 'ClaimResponseItem', 'sequenceLinkId', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
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
  js.registerElement(def, 'ClaimResponseItemAdjudication', 'code', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication', 'amount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'ClaimResponseItemDetail', 'sequenceLinkId', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetail', 'adjudication', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItemDetail', 'subDetail', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetail', nil, 'ClaimResponseItemDetail', FHIRFactoryJs);
  defineClaimResponseItemDetailPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailAdjudication', 'code', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemDetailAdjudication', 'amount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemDetailAdjudication', 'value', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
end;

procedure defineClaimResponseItemDetailAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailAdjudication', nil, 'ClaimResponseItemDetailAdjudication', FHIRFactoryJs);
  defineClaimResponseItemDetailAdjudicationPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail', 'sequenceLinkId', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail', 'adjudication', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailSubDetail', nil, 'ClaimResponseItemDetailSubDetail', FHIRFactoryJs);
  defineClaimResponseItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailSubDetailAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetailAdjudication', 'code', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetailAdjudication', 'amount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetailAdjudication', 'value', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
end;

procedure defineClaimResponseItemDetailSubDetailAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailSubDetailAdjudication', nil, 'ClaimResponseItemDetailSubDetailAdjudication', FHIRFactoryJs);
  defineClaimResponseItemDetailSubDetailAdjudicationPropsJs(js, def);
end;


procedure defineClaimResponseAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItem', 'service', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'fee', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'adjudication', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'detail', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItem', nil, 'ClaimResponseAddItem', FHIRFactoryJs);
  defineClaimResponseAddItemPropsJs(js, def);
end;


procedure defineClaimResponseAddItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemAdjudication', 'code', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemAdjudication', 'amount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemAdjudication', 'value', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
end;

procedure defineClaimResponseAddItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemAdjudication', nil, 'ClaimResponseAddItemAdjudication', FHIRFactoryJs);
  defineClaimResponseAddItemAdjudicationPropsJs(js, def);
end;


procedure defineClaimResponseAddItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'service', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'fee', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'adjudication', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemDetail', nil, 'ClaimResponseAddItemDetail', FHIRFactoryJs);
  defineClaimResponseAddItemDetailPropsJs(js, def);
end;


procedure defineClaimResponseAddItemDetailAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemDetailAdjudication', 'code', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailAdjudication', 'amount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailAdjudication', 'value', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
end;

procedure defineClaimResponseAddItemDetailAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemDetailAdjudication', nil, 'ClaimResponseAddItemDetailAdjudication', FHIRFactoryJs);
  defineClaimResponseAddItemDetailAdjudicationPropsJs(js, def);
end;


procedure defineClaimResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseError', 'sequenceLinkId', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError', 'detailSequenceLinkId', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError', 'subdetailSequenceLinkId', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError', 'code', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseError', nil, 'ClaimResponseError', FHIRFactoryJs);
  defineClaimResponseErrorPropsJs(js, def);
end;


procedure defineClaimResponseNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseNote', 'number', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseNote', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseNote', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineClaimResponseNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseNote', nil, 'ClaimResponseNote', FHIRFactoryJs);
  defineClaimResponseNotePropsJs(js, def);
end;


procedure defineClaimResponseCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseCoverage', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseCoverage', 'focal', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ClaimResponseCoverage', 'coverage', 'Reference(Coverage)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseCoverage', 'businessArrangement', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseCoverage', 'relationship', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseCoverage', 'claimResponse', 'Reference(ClaimResponse)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseCoverage', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimResponseCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseCoverage', nil, 'ClaimResponseCoverage', FHIRFactoryJs);
  defineClaimResponseCoveragePropsJs(js, def);
end;


procedure defineClaimResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClaimResponse', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'request', 'Reference(Claim)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponse', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'requestProvider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'requestOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'payeeType', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'addItem', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'error', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'totalCost', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'unallocDeductable', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'totalBenefit', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'paymentAdjustment', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'paymentAdjustmentReason', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'paymentDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponse', 'paymentAmount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'paymentRef', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'reserved', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'form', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'note', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'coverage', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponse', nil, 'ClaimResponse', FHIRFactoryJs);
  defineClaimResponsePropsJs(js, def);
end;


procedure defineClinicalImpressionInvestigationsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionInvestigations', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionInvestigations', 'item', 'Reference(Observation|QuestionnaireResponse|FamilyMemberHistory|DiagnosticReport)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClinicalImpressionInvestigationsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionInvestigations', nil, 'ClinicalImpressionInvestigations', FHIRFactoryJs);
  defineClinicalImpressionInvestigationsPropsJs(js, def);
end;


procedure defineClinicalImpressionFindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionFinding', 'item', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding', 'cause', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineClinicalImpressionFindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionFinding', nil, 'ClinicalImpressionFinding', FHIRFactoryJs);
  defineClinicalImpressionFindingPropsJs(js, def);
end;


procedure defineClinicalImpressionRuledOutPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionRuledOut', 'item', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionRuledOut', 'reason', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineClinicalImpressionRuledOutJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionRuledOut', nil, 'ClinicalImpressionRuledOut', FHIRFactoryJs);
  defineClinicalImpressionRuledOutPropsJs(js, def);
end;


procedure defineClinicalImpressionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClinicalImpression', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'assessor', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'previous', 'Reference(ClinicalImpression)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'problem', 'Reference(Condition|AllergyIntolerance)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'triggerCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'triggerReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'investigations', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'protocol', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'summary', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'finding', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'resolved', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'ruledOut', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'prognosis', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'plan', 'Reference(CarePlan|Appointment|CommunicationRequest|DeviceUseRequest|DiagnosticOrder|MedicationOrder|NutritionOrder|Order|ProcedureRequest|ProcessRequest|ReferralRequest|SupplyRequest|VisionPrescription)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'action', 'Reference(ReferralRequest|ProcedureRequest|Procedure|MedicationOrder|DiagnosticOrder|NutritionOrder|SupplyRequest|Appointment)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClinicalImpressionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpression', nil, 'ClinicalImpression', FHIRFactoryJs);
  defineClinicalImpressionPropsJs(js, def);
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
  js.registerElement(def, 'Communication', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'sender', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'recipient', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson|Group)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'payload', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'medium', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Communication', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'sent', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Communication', 'received', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Communication', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'subject', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'requestDetail', 'Reference(CommunicationRequest)', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'CommunicationRequest', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'sender', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'recipient', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'payload', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'medium', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'requester', 'Reference(Practitioner|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'scheduledDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest', 'scheduledPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'requestedOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest', 'subject', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'priority', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCommunicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequest', nil, 'CommunicationRequest', FHIRFactoryJs);
  defineCommunicationRequestPropsJs(js, def);
end;


procedure defineCompositionAttesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionAttester', 'time', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'CompositionAttester', 'party', 'Reference(Patient|Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCompositionAttesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionAttester', nil, 'CompositionAttester', FHIRFactoryJs);
  defineCompositionAttesterPropsJs(js, def);
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
  js.registerElement(def, 'Composition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Composition', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'class', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Composition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Composition', 'confidentiality', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Composition', 'subject', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'author', 'Reference(Practitioner|Device|Patient|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'attester', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'custodian', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'event', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'section', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCompositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Composition', nil, 'Composition', FHIRFactoryJs);
  defineCompositionPropsJs(js, def);
end;


procedure defineConceptMapContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapContact', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConceptMapContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapContact', nil, 'ConceptMapContact', FHIRFactoryJs);
  defineConceptMapContactPropsJs(js, def);
end;


procedure defineConceptMapElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapElement', 'codeSystem', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElement', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElement', 'target', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConceptMapElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapElement', nil, 'ConceptMapElement', FHIRFactoryJs);
  defineConceptMapElementPropsJs(js, def);
end;


procedure defineConceptMapElementTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapElementTarget', 'codeSystem', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTarget', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTarget', 'equivalence', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTarget', 'comments', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTarget', 'dependsOn', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapElementTarget', 'product', '@ConceptMap.element.target.dependsOn', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConceptMapElementTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapElementTarget', nil, 'ConceptMapElementTarget', FHIRFactoryJs);
  defineConceptMapElementTargetPropsJs(js, def);
end;


procedure defineConceptMapElementTargetDependsOnPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapElementTargetDependsOn', 'element', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTargetDependsOn', 'codeSystem', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapElementTargetDependsOn', 'code', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConceptMapElementTargetDependsOnJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapElementTargetDependsOn', nil, 'ConceptMapElementTargetDependsOn', FHIRFactoryJs);
  defineConceptMapElementTargetDependsOnPropsJs(js, def);
end;


procedure defineConceptMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ConceptMap', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ConceptMap', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ConceptMap', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'useContext', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap', 'requirements', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'copyright', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'sourceUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'sourceReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap', 'targetUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'targetReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap', 'element', '', getFHIRArrayProp, setFHIRArrayProp);
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
  js.registerElement(def, 'ConditionEvidence', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'Condition', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'asserter', 'Reference(Practitioner|Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'dateRecorded', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Condition', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'clinicalStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Condition', 'verificationStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Condition', 'severity', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Condition', 'onsetQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Condition', 'abatementDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Condition', 'abatementQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'abatementBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Condition', 'abatementPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'abatementRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'abatementString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Condition', 'stage', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'evidence', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'bodySite', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'notes', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Condition', nil, 'Condition', FHIRFactoryJs);
  defineConditionPropsJs(js, def);
end;


procedure defineConformanceContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceContact', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConformanceContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceContact', nil, 'ConformanceContact', FHIRFactoryJs);
  defineConformanceContactPropsJs(js, def);
end;


procedure defineConformanceSoftwarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceSoftware', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceSoftware', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceSoftware', 'releaseDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineConformanceSoftwareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceSoftware', nil, 'ConformanceSoftware', FHIRFactoryJs);
  defineConformanceSoftwarePropsJs(js, def);
end;


procedure defineConformanceImplementationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceImplementation', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceImplementation', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConformanceImplementationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceImplementation', nil, 'ConformanceImplementation', FHIRFactoryJs);
  defineConformanceImplementationPropsJs(js, def);
end;


procedure defineConformanceRestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRest', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRest', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRest', 'security', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConformanceRest', 'resource', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConformanceRest', 'interaction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConformanceRest', 'transactionMode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRest', 'searchParam', '@Conformance.rest.resource.searchParam', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConformanceRest', 'operation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConformanceRestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRest', nil, 'ConformanceRest', FHIRFactoryJs);
  defineConformanceRestPropsJs(js, def);
end;


procedure defineConformanceRestSecurityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestSecurity', 'cors', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ConformanceRestSecurity', 'service', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConformanceRestSecurity', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestSecurity', 'certificate', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConformanceRestSecurityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestSecurity', nil, 'ConformanceRestSecurity', FHIRFactoryJs);
  defineConformanceRestSecurityPropsJs(js, def);
end;


procedure defineConformanceRestSecurityCertificatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestSecurityCertificate', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestSecurityCertificate', 'blob', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
end;

procedure defineConformanceRestSecurityCertificateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestSecurityCertificate', nil, 'ConformanceRestSecurityCertificate', FHIRFactoryJs);
  defineConformanceRestSecurityCertificatePropsJs(js, def);
end;


procedure defineConformanceRestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestResource', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResource', 'profile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConformanceRestResource', 'interaction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConformanceRestResource', 'versioning', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResource', 'readHistory', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ConformanceRestResource', 'updateCreate', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ConformanceRestResource', 'conditionalCreate', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ConformanceRestResource', 'conditionalUpdate', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ConformanceRestResource', 'conditionalDelete', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResource', 'searchParam', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConformanceRestResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestResource', nil, 'ConformanceRestResource', FHIRFactoryJs);
  defineConformanceRestResourcePropsJs(js, def);
end;


procedure defineConformanceRestResourceInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestResourceInteraction', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResourceInteraction', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConformanceRestResourceInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestResourceInteraction', nil, 'ConformanceRestResourceInteraction', FHIRFactoryJs);
  defineConformanceRestResourceInteractionPropsJs(js, def);
end;


procedure defineConformanceRestResourceSearchParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestResourceSearchParam', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResourceSearchParam', 'definition', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResourceSearchParam', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestResourceSearchParam', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConformanceRestResourceSearchParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestResourceSearchParam', nil, 'ConformanceRestResourceSearchParam', FHIRFactoryJs);
  defineConformanceRestResourceSearchParamPropsJs(js, def);
end;


procedure defineConformanceRestInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestInteraction', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestInteraction', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConformanceRestInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestInteraction', nil, 'ConformanceRestInteraction', FHIRFactoryJs);
  defineConformanceRestInteractionPropsJs(js, def);
end;


procedure defineConformanceRestOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceRestOperation', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceRestOperation', 'definition', 'Reference(OperationDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineConformanceRestOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceRestOperation', nil, 'ConformanceRestOperation', FHIRFactoryJs);
  defineConformanceRestOperationPropsJs(js, def);
end;


procedure defineConformanceMessagingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceMessaging', 'endpoint', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConformanceMessaging', 'reliableCache', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceMessaging', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceMessaging', 'event', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConformanceMessagingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceMessaging', nil, 'ConformanceMessaging', FHIRFactoryJs);
  defineConformanceMessagingPropsJs(js, def);
end;


procedure defineConformanceMessagingEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceMessagingEndpoint', 'protocol', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConformanceMessagingEndpoint', 'address', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConformanceMessagingEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceMessagingEndpoint', nil, 'ConformanceMessagingEndpoint', FHIRFactoryJs);
  defineConformanceMessagingEndpointPropsJs(js, def);
end;


procedure defineConformanceMessagingEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceMessagingEvent', 'code', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConformanceMessagingEvent', 'category', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceMessagingEvent', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceMessagingEvent', 'focus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceMessagingEvent', 'request', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConformanceMessagingEvent', 'response', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConformanceMessagingEvent', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConformanceMessagingEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceMessagingEvent', nil, 'ConformanceMessagingEvent', FHIRFactoryJs);
  defineConformanceMessagingEventPropsJs(js, def);
end;


procedure defineConformanceDocumentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConformanceDocument', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceDocument', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConformanceDocument', 'profile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineConformanceDocumentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConformanceDocument', nil, 'ConformanceDocument', FHIRFactoryJs);
  defineConformanceDocumentPropsJs(js, def);
end;


procedure defineConformancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Conformance', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Conformance', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Conformance', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Conformance', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'requirements', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'copyright', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'software', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Conformance', 'implementation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Conformance', 'fhirVersion', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'acceptUnknown', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Conformance', 'profile', 'Reference(StructureDefinition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Conformance', 'rest', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Conformance', 'messaging', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Conformance', 'document', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConformanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Conformance', nil, 'Conformance', FHIRFactoryJs);
  defineConformancePropsJs(js, def);
end;


procedure defineContractActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractActor', 'entity', 'Reference(Contract|Device|Group|Location|Organization|Patient|Practitioner|RelatedPerson|Substance)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractActor', 'role', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineContractActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractActor', nil, 'ContractActor', FHIRFactoryJs);
  defineContractActorPropsJs(js, def);
end;


procedure defineContractValuedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractValuedItem', 'entityCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem', 'entityReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem', 'effectiveTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ContractValuedItem', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem', 'unitPrice', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractValuedItem', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ContractValuedItem', 'points', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ContractValuedItem', 'net', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineContractValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractValuedItem', nil, 'ContractValuedItem', FHIRFactoryJs);
  defineContractValuedItemPropsJs(js, def);
end;


procedure defineContractSignerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractSigner', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner', 'party', 'Reference(Organization|Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner', 'signature', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineContractSignerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractSigner', nil, 'ContractSigner', FHIRFactoryJs);
  defineContractSignerPropsJs(js, def);
end;


procedure defineContractTermPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTerm', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'issued', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTerm', 'applies', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'subType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'subject', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'action', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'actionReason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'actor', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ContractTerm', 'valuedItem', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'group', '@Contract.term', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineContractTermJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTerm', nil, 'ContractTerm', FHIRFactoryJs);
  defineContractTermPropsJs(js, def);
end;


procedure defineContractTermActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermActor', 'entity', 'Reference(Contract|Device|Group|Location|Organization|Patient|Practitioner|RelatedPerson|Substance)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermActor', 'role', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineContractTermActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermActor', nil, 'ContractTermActor', FHIRFactoryJs);
  defineContractTermActorPropsJs(js, def);
end;


procedure defineContractTermValuedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermValuedItem', 'entityCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem', 'entityReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem', 'effectiveTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermValuedItem', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem', 'unitPrice', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermValuedItem', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermValuedItem', 'points', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermValuedItem', 'net', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineContractTermValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermValuedItem', nil, 'ContractTermValuedItem', FHIRFactoryJs);
  defineContractTermValuedItemPropsJs(js, def);
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
  js.registerElement(def, 'Contract', 'issued', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Contract', 'applies', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'subject', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'authority', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'domain', 'Reference(Location)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'subType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'action', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'actionReason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'actor', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'valuedItem', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'signer', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'term', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'bindingAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'bindingReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'friendly', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'legal', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'rule', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineContractJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contract', nil, 'Contract', FHIRFactoryJs);
  defineContractPropsJs(js, def);
end;


procedure defineCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Coverage', 'issuer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'bin', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'subscriberId', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'group', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'plan', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'subPlan', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'dependent', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Coverage', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Coverage', 'subscriber', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'network', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'contract', 'Reference(Contract)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coverage', nil, 'Coverage', FHIRFactoryJs);
  defineCoveragePropsJs(js, def);
end;


procedure defineDataElementContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DataElementContact', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElementContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDataElementContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataElementContact', nil, 'DataElementContact', FHIRFactoryJs);
  defineDataElementContactPropsJs(js, def);
end;


procedure defineDataElementMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DataElementMapping', 'identity', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElementMapping', 'uri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElementMapping', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElementMapping', 'comments', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineDataElementMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataElementMapping', nil, 'DataElementMapping', FHIRFactoryJs);
  defineDataElementMappingPropsJs(js, def);
end;


procedure defineDataElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DataElement', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElement', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DataElement', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElement', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElement', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElement', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'DataElement', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElement', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DataElement', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DataElement', 'useContext', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DataElement', 'copyright', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElement', 'stringency', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataElement', 'mapping', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DataElement', 'element', 'ElementDefinition', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDataElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataElement', nil, 'DataElement', FHIRFactoryJs);
  defineDataElementPropsJs(js, def);
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
  js.registerElement(def, 'DetectedIssue', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'severity', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'implicated', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue', 'detail', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssue', 'author', 'Reference(Practitioner|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Device', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Device', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Device', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Device', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'manufacturer', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'model', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'manufactureDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Device', 'expiry', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Device', 'udi', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'lotNumber', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'owner', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Device', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Device', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Device', 'contact', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Device', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
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


procedure defineDeviceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceComponent', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'lastSystemChange', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceComponent', 'source', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'parent', 'Reference(DeviceComponent)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'operationalStatus', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent', 'parameterGroup', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'measurementPrinciple', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceComponent', 'productionSpecification', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent', 'languageCode', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'DeviceMetric', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineDeviceUseRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceUseRequest', 'bodySiteCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest', 'bodySiteReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceUseRequest', 'device', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseRequest', 'indication', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseRequest', 'prnReason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseRequest', 'orderedOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseRequest', 'recordedOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseRequest', 'subject', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest', 'timingTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest', 'timingPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseRequest', 'timingDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseRequest', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineDeviceUseRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUseRequest', nil, 'DeviceUseRequest', FHIRFactoryJs);
  defineDeviceUseRequestPropsJs(js, def);
end;


procedure defineDeviceUseStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceUseStatement', 'bodySiteCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'bodySiteReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'whenUsed', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'device', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'indication', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'recordedOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement', 'subject', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineDeviceUseStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUseStatement', nil, 'DeviceUseStatement', FHIRFactoryJs);
  defineDeviceUseStatementPropsJs(js, def);
end;


procedure defineDiagnosticOrderEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticOrderEvent', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DiagnosticOrderEvent', 'description', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrderEvent', 'dateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticOrderEvent', 'actor', 'Reference(Practitioner|Device)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDiagnosticOrderEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticOrderEvent', nil, 'DiagnosticOrderEvent', FHIRFactoryJs);
  defineDiagnosticOrderEventPropsJs(js, def);
end;


procedure defineDiagnosticOrderItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticOrderItem', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrderItem', 'specimen', 'Reference(Specimen)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrderItem', 'bodySite', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrderItem', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DiagnosticOrderItem', 'event', '@DiagnosticOrder.event', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDiagnosticOrderItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticOrderItem', nil, 'DiagnosticOrderItem', FHIRFactoryJs);
  defineDiagnosticOrderItemPropsJs(js, def);
end;


procedure defineDiagnosticOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DiagnosticOrder', 'subject', 'Reference(Patient|Group|Location|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrder', 'orderer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrder', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticOrder', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder', 'supportingInformation', 'Reference(Observation|Condition|DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder', 'specimen', 'Reference(Specimen)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DiagnosticOrder', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DiagnosticOrder', 'event', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticOrder', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDiagnosticOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticOrder', nil, 'DiagnosticOrder', FHIRFactoryJs);
  defineDiagnosticOrderPropsJs(js, def);
end;


procedure defineDiagnosticReportImagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticReportImage', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReportImage', 'link', 'Reference(Media)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDiagnosticReportImageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReportImage', nil, 'DiagnosticReportImage', FHIRFactoryJs);
  defineDiagnosticReportImagePropsJs(js, def);
end;


procedure defineDiagnosticReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DiagnosticReport', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'subject', 'Reference(Patient|Group|Device|Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'issued', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport', 'performer', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'request', 'Reference(DiagnosticOrder|ProcedureRequest|ReferralRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'specimen', 'Reference(Specimen)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'result', 'Reference(Observation)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'imagingStudy', 'Reference(ImagingStudy|ImagingObjectSelection)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'image', '', getFHIRArrayProp, setFHIRArrayProp);
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


procedure defineDocumentManifestContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestContent', 'pAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestContent', 'pReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDocumentManifestContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestContent', nil, 'DocumentManifestContent', FHIRFactoryJs);
  defineDocumentManifestContentPropsJs(js, def);
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
  js.registerElement(def, 'DocumentManifest', 'subject', 'Reference(Patient|Practitioner|Group|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest', 'recipient', 'Reference(Patient|Practitioner|RelatedPerson|Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest', 'author', 'Reference(Practitioner|Organization|Device|Patient|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentManifest', 'source', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest', 'content', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'related', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDocumentManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifest', nil, 'DocumentManifest', FHIRFactoryJs);
  defineDocumentManifestPropsJs(js, def);
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
  js.registerElement(def, 'DocumentReferenceContent', 'format', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
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
  js.registerElement(def, 'DocumentReference', 'subject', 'Reference(Patient|Practitioner|Group|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'class', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'author', 'Reference(Practitioner|Organization|Device|Patient|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'custodian', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'authenticator', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference', 'indexed', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentReference', 'docStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineEligibilityRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EligibilityRequest', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityRequest', 'target', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEligibilityRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityRequest', nil, 'EligibilityRequest', FHIRFactoryJs);
  defineEligibilityRequestPropsJs(js, def);
end;


procedure defineEligibilityResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityResponse', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse', 'request', 'Reference(EligibilityRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityResponse', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'requestProvider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'requestOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineEncounterHospitalizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterHospitalization', 'preAdmissionIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'origin', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'admitSource', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'admittingDiagnosis', 'Reference(Condition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization', 'reAdmission', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'dietPreference', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization', 'specialCourtesy', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization', 'specialArrangement', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization', 'destination', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'dischargeDisposition', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'dischargeDiagnosis', 'Reference(Condition)', getFHIRArrayProp, setFHIRArrayProp);
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
  js.registerElement(def, 'Encounter', 'class', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Encounter', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'priority', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'episodeOfCare', 'Reference(EpisodeOfCare)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'incomingReferral', 'Reference(ReferralRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'appointment', 'Reference(Appointment)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'length', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'indication', 'Reference(Condition|Procedure)', getFHIRArrayProp, setFHIRArrayProp);
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


procedure defineEnrollmentRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentRequest', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentRequest', 'target', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'subject', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'coverage', 'Reference(Coverage)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'relationship', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'EnrollmentResponse', 'request', 'Reference(EnrollmentRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineEpisodeOfCareCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareCareTeam', 'role', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCareCareTeam', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareCareTeam', 'member', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEpisodeOfCareCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareCareTeam', nil, 'EpisodeOfCareCareTeam', FHIRFactoryJs);
  defineEpisodeOfCareCareTeamPropsJs(js, def);
end;


procedure defineEpisodeOfCarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCare', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCare', 'statusHistory', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'condition', 'Reference(Condition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'managingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'referralRequest', 'Reference(ReferralRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'careManager', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'careTeam', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineEpisodeOfCareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCare', nil, 'EpisodeOfCare', FHIRFactoryJs);
  defineEpisodeOfCarePropsJs(js, def);
end;


procedure defineExplanationOfBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefit', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'request', 'Reference(Claim)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'requestProvider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'requestOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'note', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'FamilyMemberHistory', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'relationship', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'gender', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'bornPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'bornDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory', 'bornString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'ageQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'ageRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'ageString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'note', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'Flag', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Flag', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'subject', 'Reference(Patient|Location|Group|Organization|Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'author', 'Reference(Device|Organization|Patient|Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineFlagJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Flag', nil, 'Flag', FHIRFactoryJs);
  defineFlagPropsJs(js, def);
end;


procedure defineGoalOutcomePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GoalOutcome', 'resultCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GoalOutcome', 'resultReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineGoalOutcomeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GoalOutcome', nil, 'GoalOutcome', FHIRFactoryJs);
  defineGoalOutcomePropsJs(js, def);
end;


procedure defineGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Goal', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'subject', 'Reference(Patient|Group|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'startDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Goal', 'startCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'targetDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Goal', 'targetQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Goal', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Goal', 'statusDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Goal', 'statusReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'author', 'Reference(Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'priority', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'addresses', 'Reference(Condition|Observation|MedicationStatement|NutritionOrder|ProcedureRequest|RiskAssessment)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'outcome', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Goal', nil, 'Goal', FHIRFactoryJs);
  defineGoalPropsJs(js, def);
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


procedure defineHealthcareServiceServiceTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceServiceType', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'HealthcareServiceServiceType', 'specialty', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineHealthcareServiceServiceTypeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceServiceType', nil, 'HealthcareServiceServiceType', FHIRFactoryJs);
  defineHealthcareServiceServiceTypePropsJs(js, def);
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
  js.registerElement(def, 'HealthcareService', 'providedBy', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'serviceCategory', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'serviceType', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'serviceName', 'string', getFHIRStringProp, setFHIRStringProp);
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
  js.registerElement(def, 'HealthcareService', 'publicKey', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'appointmentRequired', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService', 'availableTime', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'notAvailable', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'availabilityExceptions', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineHealthcareServiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareService', nil, 'HealthcareService', FHIRFactoryJs);
  defineHealthcareServicePropsJs(js, def);
end;


procedure defineImagingObjectSelectionStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingObjectSelectionStudy', 'uid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudy', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudy', 'imagingStudy', 'Reference(ImagingStudy)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingObjectSelectionStudy', 'series', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImagingObjectSelectionStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingObjectSelectionStudy', nil, 'ImagingObjectSelectionStudy', FHIRFactoryJs);
  defineImagingObjectSelectionStudyPropsJs(js, def);
end;


procedure defineImagingObjectSelectionStudySeriesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingObjectSelectionStudySeries', 'uid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudySeries', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudySeries', 'instance', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImagingObjectSelectionStudySeriesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingObjectSelectionStudySeries', nil, 'ImagingObjectSelectionStudySeries', FHIRFactoryJs);
  defineImagingObjectSelectionStudySeriesPropsJs(js, def);
end;


procedure defineImagingObjectSelectionStudySeriesInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingObjectSelectionStudySeriesInstance', 'sopClass', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudySeriesInstance', 'uid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudySeriesInstance', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelectionStudySeriesInstance', 'frames', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImagingObjectSelectionStudySeriesInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingObjectSelectionStudySeriesInstance', nil, 'ImagingObjectSelectionStudySeriesInstance', FHIRFactoryJs);
  defineImagingObjectSelectionStudySeriesInstancePropsJs(js, def);
end;


procedure defineImagingObjectSelectionStudySeriesInstanceFramesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingObjectSelectionStudySeriesInstanceFrames', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineImagingObjectSelectionStudySeriesInstanceFramesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingObjectSelectionStudySeriesInstanceFrames', nil, 'ImagingObjectSelectionStudySeriesInstanceFrames', FHIRFactoryJs);
  defineImagingObjectSelectionStudySeriesInstanceFramesPropsJs(js, def);
end;


procedure defineImagingObjectSelectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImagingObjectSelection', 'uid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelection', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingObjectSelection', 'title', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingObjectSelection', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingObjectSelection', 'author', 'Reference(Practitioner|Device|Organization|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingObjectSelection', 'authoringTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingObjectSelection', 'study', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImagingObjectSelectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingObjectSelection', nil, 'ImagingObjectSelection', FHIRFactoryJs);
  defineImagingObjectSelectionPropsJs(js, def);
end;


procedure defineImagingStudySeriesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeries', 'number', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'modality', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'uid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'numberOfInstances', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'availability', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'bodySite', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'laterality', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'started', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
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
  js.registerElement(def, 'ImagingStudySeriesInstance', 'number', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'uid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'sopClass', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'type', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'content', 'Attachment', getFHIRArrayProp, setFHIRArrayProp);
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
  js.registerElement(def, 'ImagingStudy', 'started', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudy', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'uid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'accession', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'order', 'Reference(DiagnosticOrder)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'modalityList', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'referrer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'availability', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'numberOfSeries', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'numberOfInstances', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'procedure', 'Reference(Procedure)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'interpreter', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineImmunizationExplanationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationExplanation', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationExplanation', 'reasonNotGiven', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImmunizationExplanationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationExplanation', nil, 'ImmunizationExplanation', FHIRFactoryJs);
  defineImmunizationExplanationPropsJs(js, def);
end;


procedure defineImmunizationReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationReaction', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationReaction', 'detail', 'Reference(Observation)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationReaction', 'reported', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineImmunizationReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationReaction', nil, 'ImmunizationReaction', FHIRFactoryJs);
  defineImmunizationReactionPropsJs(js, def);
end;


procedure defineImmunizationVaccinationProtocolPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationVaccinationProtocol', 'doseSequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol', 'authority', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol', 'series', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol', 'seriesDoses', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol', 'targetDisease', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol', 'doseStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationVaccinationProtocol', 'doseStatusReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineImmunizationVaccinationProtocolJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationVaccinationProtocol', nil, 'ImmunizationVaccinationProtocol', FHIRFactoryJs);
  defineImmunizationVaccinationProtocolPropsJs(js, def);
end;


procedure defineImmunizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Immunization', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Immunization', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization', 'vaccineCode', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'wasNotGiven', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Immunization', 'reported', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Immunization', 'performer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'requester', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'manufacturer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'lotNumber', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Immunization', 'expirationDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization', 'site', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'route', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'doseQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'explanation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'reaction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'vaccinationProtocol', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImmunizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Immunization', nil, 'Immunization', FHIRFactoryJs);
  defineImmunizationPropsJs(js, def);
end;


procedure defineImmunizationRecommendationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'vaccineCode', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'doseNumber', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'forecastStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'dateCriterion', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'protocol', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'supportingImmunization', 'Reference(Immunization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'supportingPatientInformation', 'Reference(Observation|AllergyIntolerance)', getFHIRArrayProp, setFHIRArrayProp);
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


procedure defineImmunizationRecommendationRecommendationProtocolPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol', 'doseSequence', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol', 'authority', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationProtocol', 'series', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineImmunizationRecommendationRecommendationProtocolJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendationProtocol', nil, 'ImmunizationRecommendationRecommendationProtocol', FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationProtocolPropsJs(js, def);
end;


procedure defineImmunizationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendation', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendation', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation', 'recommendation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendation', nil, 'ImmunizationRecommendation', FHIRFactoryJs);
  defineImmunizationRecommendationPropsJs(js, def);
end;


procedure defineImplementationGuideContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideContact', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImplementationGuideContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideContact', nil, 'ImplementationGuideContact', FHIRFactoryJs);
  defineImplementationGuideContactPropsJs(js, def);
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
  js.registerElement(def, 'ImplementationGuidePackageResource', 'purpose', 'code', getFHIRStringProp, setFHIRStringProp);
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
  js.registerElement(def, 'ImplementationGuidePage', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
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
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuide', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuide', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuide', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'useContext', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'copyright', 'string', getFHIRStringProp, setFHIRStringProp);
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
  js.registerElement(def, 'List', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'List', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'List', 'subject', 'Reference(Patient|Group|Device|Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'List', 'source', 'Reference(Practitioner|Patient|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'List', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'List', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'List', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'List', 'orderedBy', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'List', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'List', 'note', 'string', getFHIRStringProp, setFHIRStringProp);
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


procedure defineLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Location', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Location', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
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
end;

procedure defineLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Location', nil, 'Location', FHIRFactoryJs);
  defineLocationPropsJs(js, def);
end;


procedure defineMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Media', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Media', 'subtype', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Media', 'subject', 'Reference(Patient|Practitioner|Group|Device|Specimen)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'operator', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'view', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'deviceName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Media', 'height', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Media', 'width', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Media', 'frames', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Media', 'duration', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Media', 'content', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Media', nil, 'Media', FHIRFactoryJs);
  defineMediaPropsJs(js, def);
end;


procedure defineMedicationProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationProduct', 'form', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationProduct', 'ingredient', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationProduct', 'batch', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicationProductJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationProduct', nil, 'MedicationProduct', FHIRFactoryJs);
  defineMedicationProductPropsJs(js, def);
end;


procedure defineMedicationProductIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationProductIngredient', 'item', 'Reference(Substance|Medication)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationProductIngredient', 'amount', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationProductIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationProductIngredient', nil, 'MedicationProductIngredient', FHIRFactoryJs);
  defineMedicationProductIngredientPropsJs(js, def);
end;


procedure defineMedicationProductBatchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationProductBatch', 'lotNumber', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationProductBatch', 'expirationDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineMedicationProductBatchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationProductBatch', nil, 'MedicationProductBatch', FHIRFactoryJs);
  defineMedicationProductBatchPropsJs(js, def);
end;


procedure defineMedicationPackagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationPackage', 'container', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationPackage', 'content', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicationPackageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationPackage', nil, 'MedicationPackage', FHIRFactoryJs);
  defineMedicationPackagePropsJs(js, def);
end;


procedure defineMedicationPackageContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationPackageContent', 'item', 'Reference(Medication)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationPackageContent', 'amount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationPackageContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationPackageContent', nil, 'MedicationPackageContent', FHIRFactoryJs);
  defineMedicationPackageContentPropsJs(js, def);
end;


procedure defineMedicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Medication', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'isBrand', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Medication', 'manufacturer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'product', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'package', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Medication', nil, 'Medication', FHIRFactoryJs);
  defineMedicationPropsJs(js, def);
end;


procedure defineMedicationAdministrationDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationDosage', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'siteCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'siteReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'route', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'rateRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'rateRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'MedicationAdministration', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministration', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'practitioner', 'Reference(Practitioner|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'prescription', 'Reference(MedicationOrder)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'wasNotGiven', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MedicationAdministration', 'reasonNotGiven', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'reasonGiven', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'effectiveTimeDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationAdministration', 'effectiveTimePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'medicationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'medicationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'device', 'Reference(Device)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'note', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministration', 'dosage', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministration', nil, 'MedicationAdministration', FHIRFactoryJs);
  defineMedicationAdministrationPropsJs(js, def);
end;


procedure defineMedicationDispenseDosageInstructionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'additionalInstructions', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'timing', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'asNeededBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'asNeededCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'siteCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'siteReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'route', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'doseRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'doseQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'rateRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'rateRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseDosageInstruction', 'maxDosePerPeriod', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationDispenseDosageInstructionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispenseDosageInstruction', nil, 'MedicationDispenseDosageInstruction', FHIRFactoryJs);
  defineMedicationDispenseDosageInstructionPropsJs(js, def);
end;


procedure defineMedicationDispenseSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
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
  js.registerElement(def, 'MedicationDispense', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationDispense', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'dispenser', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'authorizingPrescription', 'Reference(MedicationOrder)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'daysSupply', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'medicationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'medicationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'whenPrepared', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense', 'whenHandedOver', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense', 'destination', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'receiver', 'Reference(Patient|Practitioner)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'note', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationDispense', 'dosageInstruction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'substitution', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispense', nil, 'MedicationDispense', FHIRFactoryJs);
  defineMedicationDispensePropsJs(js, def);
end;


procedure defineMedicationOrderDosageInstructionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'additionalInstructions', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'timing', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'asNeededBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'asNeededCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'siteCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'siteReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'route', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'doseRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'doseQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'rateRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'rateRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDosageInstruction', 'maxDosePerPeriod', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationOrderDosageInstructionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationOrderDosageInstruction', nil, 'MedicationOrderDosageInstruction', FHIRFactoryJs);
  defineMedicationOrderDosageInstructionPropsJs(js, def);
end;


procedure defineMedicationOrderDispenseRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationOrderDispenseRequest', 'medicationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDispenseRequest', 'medicationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDispenseRequest', 'validityPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDispenseRequest', 'numberOfRepeatsAllowed', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'MedicationOrderDispenseRequest', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderDispenseRequest', 'expectedSupplyDuration', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationOrderDispenseRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationOrderDispenseRequest', nil, 'MedicationOrderDispenseRequest', FHIRFactoryJs);
  defineMedicationOrderDispenseRequestPropsJs(js, def);
end;


procedure defineMedicationOrderSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationOrderSubstitution', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrderSubstitution', 'reason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationOrderSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationOrderSubstitution', nil, 'MedicationOrderSubstitution', FHIRFactoryJs);
  defineMedicationOrderSubstitutionPropsJs(js, def);
end;


procedure defineMedicationOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationOrder', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationOrder', 'dateWritten', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationOrder', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationOrder', 'dateEnded', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationOrder', 'reasonEnded', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder', 'prescriber', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder', 'reasonCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder', 'reasonReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder', 'note', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationOrder', 'medicationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder', 'medicationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder', 'dosageInstruction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationOrder', 'dispenseRequest', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder', 'substitution', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationOrder', 'priorPrescription', 'Reference(MedicationOrder)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationOrder', nil, 'MedicationOrder', FHIRFactoryJs);
  defineMedicationOrderPropsJs(js, def);
end;


procedure defineMedicationStatementDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationStatementDosage', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationStatementDosage', 'timing', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage', 'asNeededBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MedicationStatementDosage', 'asNeededCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage', 'siteCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage', 'siteReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage', 'route', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage', 'quantityQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage', 'quantityRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage', 'rateRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage', 'rateRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatementDosage', 'maxDosePerPeriod', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationStatementDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationStatementDosage', nil, 'MedicationStatementDosage', FHIRFactoryJs);
  defineMedicationStatementDosagePropsJs(js, def);
end;


procedure defineMedicationStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationStatement', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'informationSource', 'Reference(Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'dateAsserted', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement', 'wasNotTaken', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MedicationStatement', 'reasonNotTaken', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'reasonForUseCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'reasonForUseReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'note', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement', 'supportingInformation', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'medicationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'medicationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'dosage', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicationStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationStatement', nil, 'MedicationStatement', FHIRFactoryJs);
  defineMedicationStatementPropsJs(js, def);
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


procedure defineMessageHeaderDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderDestination', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination', 'target', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderDestination', 'endpoint', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineMessageHeaderDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderDestination', nil, 'MessageHeaderDestination', FHIRFactoryJs);
  defineMessageHeaderDestinationPropsJs(js, def);
end;


procedure defineMessageHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MessageHeader', 'timestamp', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MessageHeader', 'event', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'response', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'source', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'destination', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader', 'enterer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'author', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'receiver', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'responsible', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'reason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'data', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMessageHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeader', nil, 'MessageHeader', FHIRFactoryJs);
  defineMessageHeaderPropsJs(js, def);
end;


procedure defineNamingSystemContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NamingSystemContact', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystemContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineNamingSystemContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystemContact', nil, 'NamingSystemContact', FHIRFactoryJs);
  defineNamingSystemContactPropsJs(js, def);
end;


procedure defineNamingSystemUniqueIdPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NamingSystemUniqueId', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId', 'preferred', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
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
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'NamingSystem', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem', 'responsible', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'NamingSystem', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NamingSystem', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'useContext', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem', 'usage', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'uniqueId', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem', 'replacedBy', 'Reference(NamingSystem)', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'NutritionOrder', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'orderer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'dateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'NutritionOrder', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NutritionOrder', 'allergyIntolerance', 'Reference(AllergyIntolerance)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'foodPreferenceModifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'excludeFoodModifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'oralDiet', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'supplement', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'enteralFormula', '', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'ObservationReferenceRange', 'meaning', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineObservationRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationRelated', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ObservationRelated', 'target', 'Reference(Observation|QuestionnaireResponse)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineObservationRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationRelated', nil, 'ObservationRelated', FHIRFactoryJs);
  defineObservationRelatedPropsJs(js, def);
end;


procedure defineObservationComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationComponent', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ObservationComponent', 'valuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'dataAbsentReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'Observation', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Observation', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'subject', 'Reference(Patient|Group|Device|Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'issued', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'performer', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Observation', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Observation', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'valuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'dataAbsentReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'interpretation', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'comments', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Observation', 'bodySite', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'specimen', 'Reference(Specimen)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'device', 'Reference(Device|DeviceMetric)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'referenceRange', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'related', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'component', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineObservationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Observation', nil, 'Observation', FHIRFactoryJs);
  defineObservationPropsJs(js, def);
end;


procedure defineOperationDefinitionContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionContact', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineOperationDefinitionContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionContact', nil, 'OperationDefinitionContact', FHIRFactoryJs);
  defineOperationDefinitionContactPropsJs(js, def);
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
  js.registerElement(def, 'OperationDefinitionParameter', 'profile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineOperationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OperationDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'OperationDefinition', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'requirements', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'idempotent', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'notes', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'base', 'Reference(OperationDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinition', 'system', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'instance', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'parameter', '', getFHIRArrayProp, setFHIRArrayProp);
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


procedure defineOrderWhenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrderWhen', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OrderWhen', 'schedule', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOrderWhenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrderWhen', nil, 'OrderWhen', FHIRFactoryJs);
  defineOrderWhenPropsJs(js, def);
end;


procedure defineOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Order', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Order', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Order', 'subject', 'Reference(Patient|Group|Device|Substance)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Order', 'source', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Order', 'target', 'Reference(Organization|Device|Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Order', 'reasonCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Order', 'reasonReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Order', 'when', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Order', 'detail', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Order', nil, 'Order', FHIRFactoryJs);
  defineOrderPropsJs(js, def);
end;


procedure defineOrderResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OrderResponse', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrderResponse', 'request', 'Reference(Order)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OrderResponse', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'OrderResponse', 'who', 'Reference(Practitioner|Organization|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OrderResponse', 'orderStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OrderResponse', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OrderResponse', 'fulfillment', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineOrderResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrderResponse', nil, 'OrderResponse', FHIRFactoryJs);
  defineOrderResponsePropsJs(js, def);
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
  js.registerElement(def, 'Organization', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Organization', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Organization', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'address', 'Address', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'partOf', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Organization', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineOrganizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Organization', nil, 'Organization', FHIRFactoryJs);
  defineOrganizationPropsJs(js, def);
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
  js.registerElement(def, 'PatientLink', 'other', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'Patient', 'careProvider', 'Reference(Organization|Practitioner)', getFHIRArrayProp, setFHIRArrayProp);
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
  js.registerElement(def, 'PaymentNotice', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice', 'target', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'request', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'response', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'paymentStatus', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'PaymentReconciliationDetail', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'request', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'responce', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'submitter', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'payee', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'date', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'amount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure definePaymentReconciliationDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationDetail', nil, 'PaymentReconciliationDetail', FHIRFactoryJs);
  definePaymentReconciliationDetailPropsJs(js, def);
end;


procedure definePaymentReconciliationNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationNote', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationNote', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure definePaymentReconciliationNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationNote', nil, 'PaymentReconciliationNote', FHIRFactoryJs);
  definePaymentReconciliationNotePropsJs(js, def);
end;


procedure definePaymentReconciliationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliation', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation', 'request', 'Reference(ProcessRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliation', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'requestProvider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'requestOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'detail', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation', 'form', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'total', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'note', '', getFHIRArrayProp, setFHIRArrayProp);
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


procedure definePractitionerPractitionerRolePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerPractitionerRole', 'managingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PractitionerPractitionerRole', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PractitionerPractitionerRole', 'specialty', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerPractitionerRole', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PractitionerPractitionerRole', 'location', 'Reference(Location)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerPractitionerRole', 'healthcareService', 'Reference(HealthcareService)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure definePractitionerPractitionerRoleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerPractitionerRole', nil, 'PractitionerPractitionerRole', FHIRFactoryJs);
  definePractitionerPractitionerRolePropsJs(js, def);
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
  js.registerElement(def, 'Practitioner', 'name', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Practitioner', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'address', 'Address', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'gender', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Practitioner', 'birthDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Practitioner', 'photo', 'Attachment', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'practitionerRole', '', getFHIRArrayProp, setFHIRArrayProp);
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


procedure defineProcedurePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedurePerformer', 'actor', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'Procedure', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Procedure', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'notPerformed', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Procedure', 'reasonNotPerformed', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'bodySite', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'reasonCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'reasonReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performer', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'performedDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Procedure', 'performedPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'outcome', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'report', 'Reference(DiagnosticReport)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'complication', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'followUp', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'request', 'Reference(CarePlan|DiagnosticOrder|ProcedureRequest|ReferralRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'notes', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'focalDevice', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'used', 'Reference(Device|Medication|Substance)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Procedure', nil, 'Procedure', FHIRFactoryJs);
  defineProcedurePropsJs(js, def);
end;


procedure defineProcedureRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcedureRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest', 'bodySite', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest', 'reasonCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest', 'reasonReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest', 'scheduledDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ProcedureRequest', 'scheduledPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest', 'scheduledTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest', 'performer', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProcedureRequest', 'notes', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcedureRequest', 'asNeededBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ProcedureRequest', 'asNeededCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest', 'orderedOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ProcedureRequest', 'orderer', 'Reference(Practitioner|Patient|RelatedPerson|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureRequest', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineProcedureRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedureRequest', nil, 'ProcedureRequest', FHIRFactoryJs);
  defineProcedureRequestPropsJs(js, def);
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
  js.registerElement(def, 'ProcessRequest', 'action', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessRequest', 'target', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineProcessResponseNotesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessResponseNotes', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponseNotes', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineProcessResponseNotesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponseNotes', nil, 'ProcessResponseNotes', FHIRFactoryJs);
  defineProcessResponseNotesPropsJs(js, def);
end;


procedure defineProcessResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessResponse', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse', 'request', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'outcome', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse', 'ruleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'originalRuleset', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessResponse', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'requestProvider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'requestOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'form', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'notes', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse', 'error', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProcessResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponse', nil, 'ProcessResponse', FHIRFactoryJs);
  defineProcessResponsePropsJs(js, def);
end;


procedure defineProvenanceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceAgent', 'role', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'actor', 'Reference(Practitioner|RelatedPerson|Patient|Device|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'userId', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'relatedAgent', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProvenanceAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceAgent', nil, 'ProvenanceAgent', FHIRFactoryJs);
  defineProvenanceAgentPropsJs(js, def);
end;


procedure defineProvenanceAgentRelatedAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceAgentRelatedAgent', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgentRelatedAgent', 'target', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineProvenanceAgentRelatedAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceAgentRelatedAgent', nil, 'ProvenanceAgentRelatedAgent', FHIRFactoryJs);
  defineProvenanceAgentRelatedAgentPropsJs(js, def);
end;


procedure defineProvenanceEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceEntity', 'role', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity', 'reference', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity', 'agent', '@Provenance.agent', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'Provenance', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Provenance', 'recorded', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Provenance', 'activity', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Provenance', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineQuestionnaireGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireGroup', 'linkId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroup', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroup', 'concept', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireGroup', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroup', 'required', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireGroup', 'repeats', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireGroup', 'group', '@Questionnaire.group', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireGroup', 'question', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineQuestionnaireGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireGroup', nil, 'QuestionnaireGroup', FHIRFactoryJs);
  defineQuestionnaireGroupPropsJs(js, def);
end;


procedure defineQuestionnaireGroupQuestionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireGroupQuestion', 'linkId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion', 'concept', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion', 'required', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion', 'repeats', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion', 'options', 'Reference(ValueSet)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion', 'option', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireGroupQuestion', 'group', '@Questionnaire.group', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineQuestionnaireGroupQuestionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireGroupQuestion', nil, 'QuestionnaireGroupQuestion', FHIRFactoryJs);
  defineQuestionnaireGroupQuestionPropsJs(js, def);
end;


procedure defineQuestionnairePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Questionnaire', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'group', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineQuestionnaireJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Questionnaire', nil, 'Questionnaire', FHIRFactoryJs);
  defineQuestionnairePropsJs(js, def);
end;


procedure defineQuestionnaireResponseGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseGroup', 'linkId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroup', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroup', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroup', 'subject', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseGroup', 'group', '@QuestionnaireResponse.group', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponseGroup', 'question', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseGroup', nil, 'QuestionnaireResponseGroup', FHIRFactoryJs);
  defineQuestionnaireResponseGroupPropsJs(js, def);
end;


procedure defineQuestionnaireResponseGroupQuestionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestion', 'linkId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestion', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestion', 'answer', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseGroupQuestionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseGroupQuestion', nil, 'QuestionnaireResponseGroupQuestion', FHIRFactoryJs);
  defineQuestionnaireResponseGroupQuestionPropsJs(js, def);
end;


procedure defineQuestionnaireResponseGroupQuestionAnswerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'valueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseGroupQuestionAnswer', 'group', '@QuestionnaireResponse.group', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseGroupQuestionAnswerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseGroupQuestionAnswer', nil, 'QuestionnaireResponseGroupQuestionAnswer', FHIRFactoryJs);
  defineQuestionnaireResponseGroupQuestionAnswerPropsJs(js, def);
end;


procedure defineQuestionnaireResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponse', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'questionnaire', 'Reference(Questionnaire)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse', 'subject', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'author', 'Reference(Device|Practitioner|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'authored', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponse', 'source', 'Reference(Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'group', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineQuestionnaireResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponse', nil, 'QuestionnaireResponse', FHIRFactoryJs);
  defineQuestionnaireResponsePropsJs(js, def);
end;


procedure defineReferralRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ReferralRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ReferralRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ReferralRequest', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest', 'specialty', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest', 'priority', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest', 'requester', 'Reference(Practitioner|Organization|Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest', 'recipient', 'Reference(Practitioner|Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest', 'dateSent', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ReferralRequest', 'reason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ReferralRequest', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ReferralRequest', 'serviceRequested', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest', 'supportingInformation', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ReferralRequest', 'fulfillmentTime', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineReferralRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ReferralRequest', nil, 'ReferralRequest', FHIRFactoryJs);
  defineReferralRequestPropsJs(js, def);
end;


procedure defineRelatedPersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RelatedPerson', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson', 'relationship', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson', 'name', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineRiskAssessmentPredictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskAssessmentPrediction', 'outcome', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'probabilityDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'probabilityRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'probabilityCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'RiskAssessment', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'RiskAssessment', 'condition', 'Reference(Condition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'performer', 'Reference(Practitioner|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'basis', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'prediction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'mitigation', 'string', getFHIRStringProp, setFHIRStringProp);
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
  js.registerElement(def, 'Schedule', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineSearchParameterContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SearchParameterContact', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameterContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSearchParameterContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameterContact', nil, 'SearchParameterContact', FHIRFactoryJs);
  defineSearchParameterContactPropsJs(js, def);
end;


procedure defineSearchParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SearchParameter', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'SearchParameter', 'requirements', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'base', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'xpath', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'xpathUsage', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSearchParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameter', nil, 'SearchParameter', FHIRFactoryJs);
  defineSearchParameterPropsJs(js, def);
end;


procedure defineSlotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Slot', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Slot', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Slot', 'schedule', 'Reference(Schedule)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Slot', 'freeBusyType', 'code', getFHIRStringProp, setFHIRStringProp);
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


procedure defineSpecimenTreatmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenTreatment', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SpecimenTreatment', 'procedure', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenTreatment', 'additive', 'Reference(Substance)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSpecimenTreatmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenTreatment', nil, 'SpecimenTreatment', FHIRFactoryJs);
  defineSpecimenTreatmentPropsJs(js, def);
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
  js.registerElement(def, 'Specimen', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Specimen', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'parent', 'Reference(Specimen)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'subject', 'Reference(Patient|Group|Device|Substance)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'accessionIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'receivedTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Specimen', 'collection', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'treatment', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'container', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSpecimenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Specimen', nil, 'Specimen', FHIRFactoryJs);
  defineSpecimenPropsJs(js, def);
end;


procedure defineStructureDefinitionContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionContact', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineStructureDefinitionContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionContact', nil, 'StructureDefinitionContact', FHIRFactoryJs);
  defineStructureDefinitionContactPropsJs(js, def);
end;


procedure defineStructureDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionMapping', 'identity', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping', 'uri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping', 'comments', 'string', getFHIRStringProp, setFHIRStringProp);
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
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'StructureDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'StructureDefinition', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'useContext', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'requirements', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'copyright', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'code', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'fhirVersion', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'mapping', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'constrainedType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'abstract', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition', 'contextType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'base', 'uri', getFHIRStringProp, setFHIRStringProp);
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


procedure defineSubscriptionChannelPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubscriptionChannel', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel', 'endpoint', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel', 'payload', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel', 'header', 'string', getFHIRStringProp, setFHIRStringProp);
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
  js.registerElement(def, 'Subscription', 'criteria', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'contact', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Subscription', 'reason', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'error', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'channel', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Subscription', 'end', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
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
  js.registerElement(def, 'SubstanceIngredient', 'substance', 'Reference(Substance)', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineSupplyDeliveryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyDelivery', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SupplyDelivery', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'suppliedItem', 'Reference(Medication|Substance|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'supplier', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'whenPrepared', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'time', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
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


procedure defineSupplyRequestWhenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyRequestWhen', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestWhen', 'schedule', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSupplyRequestWhenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequestWhen', nil, 'SupplyRequestWhen', FHIRFactoryJs);
  defineSupplyRequestWhenPropsJs(js, def);
end;


procedure defineSupplyRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyRequest', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'source', 'Reference(Practitioner|Organization|Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest', 'kind', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'orderedItem', 'Reference(Medication|Substance|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'supplier', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'reasonCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'reasonReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'when', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSupplyRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequest', nil, 'SupplyRequest', FHIRFactoryJs);
  defineSupplyRequestPropsJs(js, def);
end;


procedure defineTestScriptContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptContact', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptContact', nil, 'TestScriptContact', FHIRFactoryJs);
  defineTestScriptContactPropsJs(js, def);
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
  js.registerElement(def, 'TestScriptMetadataCapability', 'conformance', 'Reference(Conformance)', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'TestScriptVariable', 'headerField', 'string', getFHIRStringProp, setFHIRStringProp);
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


procedure defineTestScriptSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetup', 'metadata', '@TestScript.metadata', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'TestScriptSetupActionOperation', 'params', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'requestHeader', '', getFHIRArrayProp, setFHIRArrayProp);
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
  js.registerElement(def, 'TestScriptSetupActionAssert', 'compareToSourcePath', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'contentType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'headerField', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'minimumId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'navigationLinks', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'operator', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'resource', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'response', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'responseCode', 'string', getFHIRStringProp, setFHIRStringProp);
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


procedure defineTestScriptTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTest', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest', 'metadata', '@TestScript.metadata', getFHIRObjectProp, setFHIRObjectProp);
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
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'TestScript', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScript', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TestScript', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TestScript', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'useContext', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'requirements', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'copyright', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'metadata', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScript', 'multiserver', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TestScript', 'fixture', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'profile', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'variable', '', getFHIRArrayProp, setFHIRArrayProp);
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


procedure defineValueSetContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetContact', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineValueSetContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetContact', nil, 'ValueSetContact', FHIRFactoryJs);
  defineValueSetContactPropsJs(js, def);
end;


procedure defineValueSetCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCodeSystem', 'system', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystem', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystem', 'caseSensitive', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetCodeSystem', 'concept', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineValueSetCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCodeSystem', nil, 'ValueSetCodeSystem', FHIRFactoryJs);
  defineValueSetCodeSystemPropsJs(js, def);
end;


procedure defineValueSetCodeSystemConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCodeSystemConcept', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystemConcept', 'abstract', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetCodeSystemConcept', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystemConcept', 'definition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystemConcept', 'designation', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSetCodeSystemConcept', 'concept', '@ValueSet.codeSystem.concept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineValueSetCodeSystemConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCodeSystemConcept', nil, 'ValueSetCodeSystemConcept', FHIRFactoryJs);
  defineValueSetCodeSystemConceptPropsJs(js, def);
end;


procedure defineValueSetCodeSystemConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCodeSystemConceptDesignation', 'language', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetCodeSystemConceptDesignation', 'use', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ValueSetCodeSystemConceptDesignation', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineValueSetCodeSystemConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCodeSystemConceptDesignation', nil, 'ValueSetCodeSystemConceptDesignation', FHIRFactoryJs);
  defineValueSetCodeSystemConceptDesignationPropsJs(js, def);
end;


procedure defineValueSetComposePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
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
  js.registerElement(def, 'ValueSetComposeIncludeConcept', 'designation', '@ValueSet.codeSystem.concept.designation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConcept', nil, 'ValueSetComposeIncludeConcept', FHIRFactoryJs);
  defineValueSetComposeIncludeConceptPropsJs(js, def);
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
  js.registerElement(def, 'ValueSetExpansionContains', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
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
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ValueSet', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ValueSet', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSet', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSet', 'lockedDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSet', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'useContext', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSet', 'immutable', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet', 'requirements', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'copyright', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'extensible', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet', 'codeSystem', '', getFHIRObjectProp, setFHIRObjectProp);
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


procedure defineVisionPrescriptionDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionDispense', 'product', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
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
  js.registerElement(def, 'VisionPrescriptionDispense', 'notes', 'string', getFHIRStringProp, setFHIRStringProp);
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
  js.registerElement(def, 'VisionPrescription', 'dateWritten', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'VisionPrescription', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'prescriber', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
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

