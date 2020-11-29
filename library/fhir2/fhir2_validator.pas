Unit fhir2_validator;

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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

interface


Uses
  SysUtils, Classes, Character, {$IFDEF DELPHI} RegularExpressions, {$ENDIF}
  fsl_base, fsl_utilities, fsl_stream, fsl_fpc,
  fsl_collections, fsl_xml, fsl_json,
  fhir_objects,  fhir_xhtml, fhir_factory, fhir_common,
  fhir2_pathnode, fhir2_context, fhir2_resources, fhir2_types, fhir2_pathengine, fhir2_elementmodel,
  fhir2_resources_base, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_canonical, fhir2_resources_other;


Type
  TNodeStack = class(TFslObject)
  private
    parent: TNodeStack;
    literalPath: String; // fhir path format
    logicalPaths: TStringList; // dotted format, various entry points
    element: TFHIRMMElement;
    definition: TFHIRElementDefinition;
    type_: TFHIRElementDefinition;
    // extension : TFHIRElementDefinition;
    function push(element: TFHIRMMElement; count: integer; definition: TFHIRElementDefinition; type_: TFHIRElementDefinition): TNodeStack;
    function addToLiteralPath(path: Array of String): String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Overload; Override;
    constructor Create(element : TFHIRMMElement); Overload;
    destructor Destroy; Override;
  end;

  TElementInfo = class(TFslObject)
  private
    name: String;
    element: TFHIRMMElement;
    path: String;
    definition: TFHIRElementDefinition;
    count: integer;
    index : integer;

    function locStart: TSourceLocation;
    function locEnd: TSourceLocation;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(name: String; element: TFHIRMMElement; path: String; count: integer);
  end;

  TChildIterator = class(TFslObject)
  private
    parent: TFHIRMMElement;
    basePath: String;
    cursor, lastCount: integer;

  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(path: String; element: TFHIRMMElement);
    destructor Destroy; override;
    function next: boolean;
    function name: String;
    function element: TFHIRMMElement;
    function path: String;
    function count: integer;
  end;

  TValidationProfileSet = class (TFslObject)
  private
    FCanonical : TStringList;
    FDefinitions : TFHIRStructureDefinitionList;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(profile : String); overload;
    constructor Create(profile : TFHIRStructureDefinition); overload;
    destructor Destroy; override;
  end;

  TFHIRValidator2 = class(TFHIRValidatorV)
  private
    // configuration items
    FExtensionDomains: TStringList;
    FPathEngine : TFHIRPathEngine;

//    function LoadDoc(name: String; isFree: boolean = false): IXMLDomDocument2;

    function rule(ctxt : TFHIRValidatorContext; t: TFhirIssueTypeEnum; locStart, locEnd: TSourceLocation; path: String; thePass: boolean; msg: String): boolean; overload;
    function rule(ctxt : TFHIRValidatorContext; t: TFhirIssueTypeEnum; path: String; thePass: boolean; msg: String): boolean; overload;
    function warning(ctxt : TFHIRValidatorContext; t: TFhirIssueTypeEnum; locStart, locEnd: TSourceLocation; path: String; thePass: boolean; msg: String): boolean;
    function hint(ctxt : TFHIRValidatorContext; t: TFhirIssueTypeEnum; locStart, locEnd: TSourceLocation; path: String; thePass: boolean; msg: String): boolean;
    procedure bpCheck(ctxt : TFHIRValidatorContext; t: TFhirIssueTypeEnum; locStart, locEnd: TSourceLocation; literalPath: String; test: boolean; message: String);

    // --- first phase : work with the json or xml directly ---------------------------------------------------------------------------------------------------------

    // function isKnownType(code : String) : boolean;
    function genFullUrl(bundleBase, entryBase, ty, id: String): String;
    Function resolveInBundle(entries: TFslList<TFHIRMMElement>; ref, fullUrl, type_, id: String): TFHIRMMElement;
    function getProfileForType(ctxt : TFHIRValidatorContext; type_: String): TFHIRStructureDefinition;
    function sliceMatches(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; path: String; slice, ed: TFHIRElementDefinition; profile: TFHIRStructureDefinition): boolean;
    function resolveType(ctxt : TFHIRValidatorContext; t: String): TFHIRElementDefinition;
    function getCriteriaForDiscriminator(ctxt : TFHIRValidatorContext; path: String; ed: TFHIRElementDefinition; discriminator: String; profile: TFHIRStructureDefinition): TFHIRElementDefinition;
    function getValueForDiscriminator(element: TFHIRMMElement; discriminator: String; criteria: TFHIRElementDefinition): TFhirElement;
    function valueMatchesCriteria(value: TFhirElement; criteria: TFHIRElementDefinition): boolean;
    function resolve(ref: String; stack: TNodeStack): TFHIRMMElement;
    function tryParse(ctxt : TFHIRValidatorContext; ref: String): String;
    function checkResourceType(ctxt : TFHIRValidatorContext; ty: String): String;
    function getBaseType(ctxt : TFHIRValidatorContext; profile: TFHIRStructureDefinition; pr: String): String;
    function getFromBundle(bundle: TFHIRMMElement; ref: String): TFHIRMMElement;
    function getContainedById(container: TFHIRMMElement; id: String): TFHIRMMElement;
    function resolveProfile(ctxt : TFHIRValidatorContext; profile: TFHIRStructureDefinition; pr: String): TFHIRStructureDefinition;
    function checkExtensionContext(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; definition: TFHIRStructureDefinition; stack: TNodeStack;
      extensionParent: String): boolean;
    // function getElementByPath(definition : TFHIRStructureDefinition; path : String) : TFHIRElementDefinition;
    function findElement(profile: TFHIRStructureDefinition; name: String): TFHIRElementDefinition;
    // function getDefinitionByTailNameChoice(children : TFHIRElementDefinitionList; name : String) : TFHIRElementDefinition;
    function resolveBindingReference(ctxt : TFHIRValidatorContext; context : TFHIRDomainResource; reference: TFHIRType): TFHIRValueSet;
    function getExtensionByUrl(extensions: TFslList<TFHIRMMElement>; url: String): TFHIRMMElement;

    procedure checkQuantityValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRQuantity);
    procedure checkAddressValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRAddress);
    procedure checkContactPointValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRContactPoint);
    procedure checkAttachmentValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRAttachment);
    procedure checkIdentifierValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRIdentifier);
    procedure checkCodingValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRCoding);
    procedure checkHumanNameValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRHumanName);
    procedure checkCodeableConceptValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRCodeableConcept);
    procedure checkTimingValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRTiming);
    procedure checkPeriodValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRPeriod);
    procedure checkRangeValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRRange);
    procedure checkRatioValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRRatio);
    procedure checkSampledDataValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRSampledData);

    procedure checkFixedValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFhirElement; propName: String);

    function checkCode(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; path: String; code, System, version, display: String): boolean;
    procedure checkQuantity(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; context: TFHIRElementDefinition);
    procedure checkPrimitiveBinding(ctxt : TFHIRValidatorContext; path: String; ty: String; context: TFHIRElementDefinition; element: TFHIRMMElement; profile : TFhirStructureDefinition);
    procedure checkPrimitive(ctxt : TFHIRValidatorContext; path, ty: String; context: TFHIRElementDefinition; e: TFHIRMMElement; profile : TFhirStructureDefinition);
    procedure checkIdentifier(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; context: TFHIRElementDefinition);
    procedure checkCoding(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; profile: TFHIRStructureDefinition; context: TFHIRElementDefinition; inCodeableConcept: boolean);
    procedure checkCodeableConcept(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; profile: TFHIRStructureDefinition; context: TFHIRElementDefinition);
    procedure checkReference(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; profile: TFHIRStructureDefinition; container: TFHIRElementDefinition; parentType: String; stack: TNodeStack);
    function checkExtension(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; def: TFHIRElementDefinition; profile: TFHIRStructureDefinition; stack: TNodeStack): TFHIRStructureDefinition;
    procedure validateContains(ctxt : TFHIRValidatorContext; path: String; child: TFHIRElementDefinition; context: TFHIRElementDefinition; resource, element: TFHIRMMElement; stack: TNodeStack; idRule: TResourceIdStatus);
    function allowUnknownExtension(ctxt : TFHIRValidatorContext; url: String): boolean;

    function idStatusForEntry(ep : TFHIRMMElement; ei : TElementInfo): TResourceIdStatus;
    procedure checkInvariants(ctxt : TFHIRValidatorContext; path : String; profile: TFHIRStructureDefinition; ed: TFhirElementDefinition; typename, typeProfile : String; resource, element: TFHIRMMElement);
    procedure validateSections(ctxt : TFHIRValidatorContext; entries: TFslList<TFHIRMMElement>; focus: TFHIRMMElement; stack: TNodeStack; fullUrl, id: String);
    procedure validateBundleReference(ctxt : TFHIRValidatorContext; entries: TFslList<TFHIRMMElement>; ref: TFHIRMMElement; name: String; stack: TNodeStack; fullUrl, type_, id: String);
    procedure validateDocument(ctxt : TFHIRValidatorContext; entries: TFslList<TFHIRMMElement>; composition: TFHIRMMElement; stack: TNodeStack; fullUrl, id: String);
    procedure validateElement(ctxt : TFHIRValidatorContext; profile: TFHIRStructureDefinition; definition: TFHIRElementDefinition; cprofile: TFHIRStructureDefinition; context: TFHIRElementDefinition; resource, element: TFHIRMMElement; actualType: String; stack: TNodeStack; inCodeableConcept: boolean);
    procedure validateMessage(ctxt : TFHIRValidatorContext; bundle: TFHIRMMElement);
    procedure validateBundle(ctxt : TFHIRValidatorContext; bundle: TFHIRMMElement; stack: TNodeStack);
    procedure validateObservation(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; stack: TNodeStack);

{$IFDEF DSTU21}
    function findQuestionnaireItem(qsrc : TFhirQuestionnaire; linkId : String; var qItem : TFhirQuestionnaireItem) : boolean;
    procedure validateAnswerCode(ctxt : TFHIRValidatorContext; value: TFHIRMMElement; stack: TNodeStack; optionList : TFhirCodingList); overload;
    procedure validateAnswerCode(ctxt : TFHIRValidatorContext; value: TFHIRMMElement; stack: TNodeStack; qSrc : TFhirQuestionnaire; vsRef : TFhirReference); overload;
    procedure validateAnswerCode(ctxt : TFHIRValidatorContext; answer: TFHIRMMElement; stack: TNodeStack; qSrc : TFhirQuestionnaire; qitem : TFhirQuestionnaireItem); overload;
    procedure validateQuestionnaireResponseItemQuantity(ctxt : TFHIRValidatorContext; answer: TFHIRMMElement; stack: TNodeStack);
    function validateQuestionnaireResponseItemType(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; stack: TNodeStack; types: array of String) : string;
    procedure validateQuestionannaireResponseItem(qsrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; ctxt : TFHIRValidatorContext; element: TFHIRMMElement; stack: TNodeStack; inProgress : boolean); overload;
    procedure validateQuestionannaireResponseItem(qsrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; ctxt : TFHIRValidatorContext; elements: TFslList<TFHIRMMElement>; stack: TNodeStack; inProgress : boolean); overload;
    procedure validateQuestionannaireResponseItems(qsrc : TFhirQuestionnaire; qItems : TFhirQuestionnaireItemList; ctxt : TFHIRValidatorContext; element: TFHIRMMElement; stack: TNodeStack; inProgress : boolean);
    procedure validateQuestionannaireResponse(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; stack: TNodeStack);
{$ENDIF}
    procedure checkDeclaredProfiles(ctxt : TFHIRValidatorContext; resource, element: TFHIRMMElement; stack: TNodeStack);
    procedure start(ctxt : TFHIRValidatorContext; resource, element: TFHIRMMElement; profile: TFHIRStructureDefinition; stack: TNodeStack);
    procedure validateResource(ctxt : TFHIRValidatorContext; resource, element: TFHIRMMElement; profile: TFHIRStructureDefinition; idRule: TResourceIdStatus; stack: TNodeStack);
    procedure checkInnerNS(ctxt: TFHIRValidatorContext; e: TFHIRMMElement; path: String; list: TFhirXHtmlNodeList);
    procedure checkInnerNames(ctxt: TFHIRValidatorContext; e: TFHIRMMElement; path: String; list: TFhirXHtmlNodeList);

    function GetContext : TFHIRWorkerContext;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(context: TFHIRWorkerContextWithFactory); override;
    destructor Destroy; Override;

    Property Context : TFHIRWorkerContext read GetContext;
    Property ValContext : TFHIRWorkerContext read GetContext;

    procedure validate(ctxt : TFHIRValidatorContext; element : TFHIRMMElement); overload;
    procedure validate(ctxt : TFHIRValidatorContext; element : TFHIRMMElement; profiles: TValidationProfileSet); overload;
    procedure validate(ctxt : TFHIRValidatorContext; element : TFHIRMMElement; profile: String); overload;

    procedure validate(ctxt : TFHIRValidatorContext; obj: TJsonObject); overload; override;
    procedure validate(ctxt : TFHIRValidatorContext; obj: TJsonObject; profiles: TValidationProfileSet); overload;
    procedure validate(ctxt : TFHIRValidatorContext; obj: TJsonObject; profile: String); overload; override;

    procedure validate(ctxt : TFHIRValidatorContext; element: TMXmlElement); overload; override;
    procedure validate(ctxt : TFHIRValidatorContext; element: TMXmlElement; profile: String); overload; override;
    procedure validate(ctxt : TFHIRValidatorContext; element: TMXmlElement; profiles: TValidationProfileSet); overload;

    procedure validate(ctxt : TFHIRValidatorContext; document: TMXmlDocument); overload; override;
    procedure validate(ctxt : TFHIRValidatorContext; document: TMXmlDocument; profile: String); overload; override;
    procedure validate(ctxt : TFHIRValidatorContext; document: TMXmlDocument; profiles: TValidationProfileSet); overload;

    procedure validate(ctxt : TFHIRValidatorContext; source : TFslBuffer; format : TFHIRFormat); overload; override;
    procedure validate(ctxt : TFHIRValidatorContext; source : TFslBuffer; format : TFHIRFormat; profile : String); overload; override;
    procedure validate(ctxt : TFHIRValidatorContext; source : TFslBuffer; format : TFHIRFormat; profiles : TValidationProfileSet); overload;

    procedure validate(ctxt : TFHIRValidatorContext; resource : TFhirResourceV); overload; override;
    procedure validate(ctxt : TFHIRValidatorContext; resource : TFhirResourceV; profile : string); overload; override;
    procedure validate(ctxt : TFHIRValidatorContext; resource : TFhirResourceV; profiles : TValidationProfileSet); overload;

    function  describe(ctxt : TFHIRValidatorContext): TFHIROperationOutcomeW; override;
  end;

  TFHIRValidator = TFHIRValidator2;

implementation

uses
  fhir_parser,
  fhir2_common, fhir2_utilities, fhir2_profiles, fhir2_narrative;

function nameMatches(name, tail: String): boolean;
begin
  if (tail.endsWith('[x]')) then
    result := name.startsWith(tail.substring(0, tail.length - 3))
  else
    result := (name = tail);
end;

{ TElementInfo }

Constructor TElementInfo.Create(name: String; element: TFHIRMMElement; path: String; count: integer);
begin
  inherited Create;
  self.name := name;
  self.element := element;
  self.path := path;
  self.count := count;
end;

function TElementInfo.locStart: TSourceLocation;
begin
  result := element.locStart;
end;

function TElementInfo.locEnd: TSourceLocation;
begin
  result := element.locEnd;
end;

function codeInExpansion(cnt: TFhirValueSetExpansionContains; System, code: String): boolean; overload;
var
  c: TFhirValueSetExpansionContains;
begin
  for c in cnt.containsList do
  begin
    if (code = c.code) and ((System = '') or (System = c.System)) then
    begin
      result := true;
      exit;
    end;
    if (codeInExpansion(c, System, code)) then
    begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function codeInExpansion(vs: TFHIRValueSet; System, code: String): boolean; overload;
var
  c: TFhirValueSetExpansionContains;
begin
  for c in vs.Expansion.containsList do
  begin
    if (code = c.code) and ((System = '') or (System = c.System)) then
    begin
      result := true;
      exit;
    end;
    if (codeInExpansion(c, System, code)) then
    begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TElementInfo.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (name.length * sizeof(char)) + 12);
  inc(result, element.sizeInBytes);
  inc(result, (path.length * sizeof(char)) + 12);
  inc(result, definition.sizeInBytes);
end;

{ TNodeStack }

constructor TNodeStack.Create();
begin
  inherited Create;
  logicalPaths := TStringList.Create;
end;

constructor TNodeStack.Create(element: TFHIRMMElement);
begin
  inherited Create;
  logicalPaths := TStringList.Create;
  self.element := element;
  literalPath := element.Name;
end;

destructor TNodeStack.Destroy;
begin
  logicalPaths.Free;
  inherited;
end;

function tail(path: String): String;
begin
  result := path.substring(path.lastIndexOf('.') + 1);
end;

function TNodeStack.push(element: TFHIRMMElement; count: integer; definition: TFHIRElementDefinition; type_: TFHIRElementDefinition): TNodeStack;
var
  t, lp: String;
  n : String;
begin
  result := TNodeStack.Create();
  try
    result.parent := self;
    result.element := element;
    result.definition := definition;
    n := element.Name;
    if LiteralPath = '' then
      result.literalPath := n
    else
      result.literalPath := literalPath + '.' + n;
    if (count > -1) then
      result.literalPath := result.literalPath + '[' + integer.toString(count-1) + ']';
    if (type_ <> nil) then
    begin
      // type will be bull if we on a stitching point of a contained resource, or if....
      result.type_ := type_;
      t := tail(definition.path);
      for lp in logicalPaths do
      begin
        result.logicalPaths.Add(lp + '.' + t);
        if (t.endsWith('[x]')) then
          result.logicalPaths.Add(lp + '.' + t.substring(0, t.length - 3) + type_.path);
      end;
      result.logicalPaths.Add(type_.path);
    end
    else if (definition <> nil) then
    begin
      for lp in logicalPaths do
        result.logicalPaths.Add(lp + '.' + element.Name);
    end
    else
      result.logicalPaths.AddStrings(logicalPaths);
    result.Link;
  finally
    result.Free;
  end;
end;

function TNodeStack.addToLiteralPath(path: Array of String): String;
var
  b: TStringBuilder;
  p: String;
begin
  b := TStringBuilder.Create;
  try
    b.append(literalPath);
    for p in path do
    begin
      if (p.startsWith(':')) then
      begin
        b.append('.item(');
        b.append(p.substring(1));
        b.append(')');
      end
      else
      begin
        b.append('.');
        b.append(p);
      end
    end;
    result := b.toString();
  finally
    b.Free;
  end;
end;

function TNodeStack.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, parent.sizeInBytes);
  inc(result, (literalPath.length * sizeof(char)) + 12);
  inc(result, logicalPaths.sizeInBytes);
  inc(result, element.sizeInBytes);
  inc(result, definition.sizeInBytes);
  inc(result, type_.sizeInBytes);
end;

{ TFHIRValidator }

constructor TFHIRValidator.Create(context: TFHIRWorkerContextWithFactory);
begin
  inherited Create(context);
  FPathEngine := TFHIRPathEngine.create((Context as TFHIRWorkerContext).link, nil);
end;

destructor TFHIRValidator.Destroy;
begin
  FPathEngine.Free;
  inherited;
end;


procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; element: TMXmlElement);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create;
  try
    validate(ctxt, element, profiles);
  finally
    profiles.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; obj: TJsonObject);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create;
  try
    validate(ctxt, obj, profiles);
  finally
    profiles.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; element: TMXmlElement; profile: String);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create(profile);
  try
    validate(ctxt, element, profiles);
  finally
    profiles.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; element: TMXmlElement; profiles: TValidationProfileSet);
var
  w : TFHIRMMElement;
  x : TFHIRMMXmlParser;
begin
  x := TFHIRMMXmlParser.create(ValContext.Link);
  try
    x.setupValidation(fvpEverything, ctxt.Issues.Link);
    w := x.Parse(element);
    try
      validate(ctxt, w, profiles);
    finally
      w.free;
    end;
  finally
    x.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; obj: TJsonObject; profiles: TValidationProfileSet);
var
  w : TFHIRMMElement;
  j : TFHIRMMJsonParser;
begin
  j := TFHIRMMJsonParser.create(ValContext.Link);
  try
    j.setupValidation(fvpEverything, ctxt.Issues.Link);
    w := j.Parse(obj);
    try
        validate(ctxt, w, profiles);
    finally
      w.free;
    end;
  finally
    j.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; obj: TJsonObject; profile: String);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create(profile);
  try
    validate(ctxt, obj, profiles);
  finally
    profiles.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; document: TMXmlDocument);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create;
  try
    validate(ctxt, document, profiles);
  finally
    profiles.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; document: TMXmlDocument; profile: String);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create(profile);
  try
    validate(ctxt, document, profiles);
  finally
    profiles.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; document: TMXmlDocument; profiles: TValidationProfileSet);
begin
  validate(ctxt, document.document, profiles);
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; profiles: TValidationProfileSet);
var
  profile : TFHIRStructureDefinition;
begin
  // all the other entry points end up here
  // dstu2, the validator is not re-written for the mutlt-profile approach in stu3, but the API is kept consistent
  profile := nil;
  if (profiles <> nil) then
    if profiles.FDefinitions.count > 0 then
      profile := profiles.FDefinitions[0]
    else if profiles.FCanonical.count > 0 then
    begin
      profile := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, profiles.FCanonical[0]));
      ctxt.Owned.add(profile);
      if (profile = nil) then
        raise EFHIRException.create('StructureDefinition "' + profiles.FCanonical[0] + '" not found');
    end;
  validateResource(ctxt, element, element, profile, ctxt.resourceIdRule, nil);
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; element: TFHIRMMElement);
begin
  validate(ctxt, element, nil);
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; profile: String);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create(profile);
  try
    validate(ctxt, element, profiles);
  finally
    profiles.free;
  end;
end;

function describeReference(reference: TFHIRType): String;
begin
  if (reference = nil) then
    result := 'nil'
  else if (reference is TFHIRUri) then
    result := TFHIRUri(reference).value
  else if (reference is TFHIRReference) then
    result := TFHIRReference(reference).reference
  else
    result := '??';
end;

function readAsCoding(item: TFHIRMMElement): TFHIRCoding;
var
  c: TFHIRCoding;
begin
  c := TFHIRCoding.Create;
  try
    c.System := item.getNamedChildValue('system');
    c.Version := item.getNamedChildValue('version');
    c.code := item.getNamedChildValue('code');
    c.display := item.getNamedChildValue('display');
    result := c.Link;
  finally
    c.Free;
  end;
end;
{$IFDEF DSTU21}
function TFHIRValidator.findQuestionnaireItem(qsrc: TFhirQuestionnaire; linkId: String; var qItem: TFhirQuestionnaireItem): boolean;
  procedure FindItem(list : TFhirQuestionnaireItemList);
  var
    item : TFhirQuestionnaireItem;
  begin
    for item in list do
    begin
      if item.linkId = linkId then
      begin
        result := true;
        qItem := item;
      end
      else
        FindItem(item.itemList);
      if result then
        break;
    end;
  end;
begin
  findItem(qsrc.itemList);
end;

procedure TFHIRValidator.validateAnswerCode(ctxt : TFHIRValidatorContext; value: TFHIRMMElement; stack: TNodeStack; optionList: TFhirCodingList);
var
  system, code : String;
  found : boolean;
  c : TFhirCoding;
begin
  system := value.getNamedChildValue('system');
  code := value.getNamedChildValue('code');
  found := false;
  for c in optionList do
    if (c.system = system) and (c.code = code) then
    begin
      found := true;
      break;
    end;
  rule(ctxt, IssueTypeStructure, value.locStart, value.locEnd, stack.literalPath, found, 'The code '+system+'::'+code+' is not a valid option');
end;

procedure TFHIRValidator.validateAnswerCode(ctxt : TFHIRValidatorContext; value: TFHIRMMElement; stack: TNodeStack; qSrc : TFhirQuestionnaire; vsRef: TFhirReference);
var
  vs : TFhirValueSet;
  c : TFHIRCoding;
  res: TValidationResult;
begin
  vs := resolveBindingReference(qSrc, vsRef);
  if (warning(ctxt, IssueTypeCODEINVALID, value.locStart, value.locEnd, stack.literalPath, vs <> nil, 'ValueSet ' + describeReference(vsRef) + ' not found')) then
  begin
    try
      c := readAsCoding(value);
      try
        res := ValContext.validateCode(c, vs);
        try
          if (not res.isOk()) then
            rule(ctxt, IssueTypeCODEINVALID, value.locStart, value.locEnd, stack.literalPath, false, 'The value provided ('+c.system+'::'+c.code+') is not in the options value set in the questionnaire');
        finally
          res.free;
        end;
      finally
        c.Free;
      end;
    except
      on e: Exception do
        warning(ctxt, IssueTypeCODEINVALID, value.locStart, value.locEnd, stack.literalPath, false, 'Error ' + e.message + ' validating Coding against Questionnaire Options');
    end;
  end;
end;

procedure TFHIRValidator.validateAnswerCode(ctxt : TFHIRValidatorContext; answer: TFHIRMMElement; stack: TNodeStack; qSrc : TFhirQuestionnaire; qitem: TFhirQuestionnaireItem);
var
  v : TFHIRMMElement;
  ns : TNodeStack;
begin
  v := answer.getNamedChild('valueCoding');
  ns := stack.push(v, -1, nil, nil);
  try
    if qItem.optionList.Count > 0 then
      validateAnswerCode(ctxt, v, stack, qitem.optionList)
    else if qItem.options <> nil then
      validateAnswerCode(ctxt, v, stack, qSrc, qitem.options)
    else
      hint(ctxt, IssueTypeStructure, v.locStart, v.locEnd, stack.literalPath, false, 'Cannot validate options because no option or options are provided');
  finally
    ns.free;
  end;
end;

procedure TFHIRValidator.validateQuestionannaireResponse(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; stack: TNodeStack);
var
  q : TFHIRMMElement;
  qsrc : TFhirQuestionnaire;
  inProgress : boolean;
begin
  q := element.getNamedChild('questionnaire');
  if hint(ctxt, IssueTypeRequired, element.locStart, element.locEnd, stack.literalPath, q <> nil, 'No questionnaire is identified, so no validation can be performed against the base questionnaire') then
  begin
    qsrc := TFhirQuestionnaire(ValContext.fetchResource(frtQuestionnaire, q.getNamedChildValue('reference')));
    try
      if warning(ctxt, IssueTypeRequired, q.locStart, q.locEnd, stack.literalPath, qsrc <> nil, 'The questionnaire could not be resolved, so no validation can be performed against the base questionnaire') then
      begin
        inProgress := element.getNamedChildValue('status') = 'in-progress';
        validateQuestionannaireResponseItems(qsrc, qsrc.itemList, errors, element, stack, inProgress);
      end;
    finally
      qsrc.free;
    end;
  end;
end;

procedure TFHIRValidator.validateQuestionannaireResponseItem(qsrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; ctxt : TFHIRValidatorContext; element: TFHIRMMElement; stack: TNodeStack; inProgress : boolean);
var
  answers, items : TFslList<TFHIRMMElement>;
  answer, item : TFHIRMMElement;
  ns : TNodeStack;
  text : String;
begin
  text := element.getNamedChildValue('text');
  rule(ctxt, IssueTypeInvalid, element.locStart, element.locEnd, stack.literalPath, (text = '') or (text = qItem.text), 'If text exists, it must match the questionnaire definition for linkId '+qItem.linkId);

  answers := TFslList<TFHIRMMElement>.create;
  try
    element.getNamedChildren('answer', answers);
    if inProgress then
      warning(ctxt, IssueTypeRequired, element.locStart, element.locEnd, stack.literalPath, (answers.Count > 0) or not qItem.required, 'No response answer found for required item '+qItem.linkId)
    else
      rule(ctxt, IssueTypeRequired, element.locStart, element.locEnd, stack.literalPath, (answers.Count > 0) or not qItem.required, 'No response answer found for required item '+qItem.linkId);
    if (answers.Count > 1) then
      rule(ctxt, IssueTypeInvalid, answers[1].locStart, answers[1].locEnd, stack.literalPath, qItem.repeats, 'Only one response answer item with this linkId allowed');

    for answer in answers do
    begin
      ns := stack.push(answer, -1, nil, nil);
      try
        case qitem.type_ of
          ItemTypeGroup: rule(ctxt, IssueTypeStructure, answer.locStart, answer.locEnd, stack.literalPath, false, 'Items of type group should not have answers');
          ItemTypeDisplay: ; // nothing
          ItemTypeBoolean:       validateQuestionnaireResponseItemType(ctxt, answer, ns, ['boolean']);
          ItemTypeDecimal:       validateQuestionnaireResponseItemType(ctxt, answer, ns, ['decimal']);
          ItemTypeInteger:       validateQuestionnaireResponseItemType(ctxt, answer, ns, ['integer']);
          ItemTypeDate:          validateQuestionnaireResponseItemType(ctxt, answer, ns, ['date']);
          ItemTypeDateTime:      validateQuestionnaireResponseItemType(ctxt, answer, ns, ['dateTime']);
          ItemTypeInstant:       validateQuestionnaireResponseItemType(ctxt, answer, ns, ['instant']);
          ItemTypeTime:          validateQuestionnaireResponseItemType(ctxt, answer, ns, ['time']);
          ItemTypeString:        validateQuestionnaireResponseItemType(ctxt, answer, ns, ['string']);
          ItemTypeText:          validateQuestionnaireResponseItemType(ctxt, answer, ns, ['text']);
          ItemTypeUrl:           validateQuestionnaireResponseItemType(ctxt, answer, ns, ['uri']);
          ItemTypeAttachment:    validateQuestionnaireResponseItemType(ctxt, answer, ns, ['Attachment']);
          ItemTypeReference:     validateQuestionnaireResponseItemType(ctxt, answer, ns, ['Reference']);
          ItemTypeQuantity:   if validateQuestionnaireResponseItemType(ctxt, answer, ns, ['Quantity']) = 'Quantity' then
            if qItem.hasExtension('???') then
              validateQuestionnaireResponseItemQuantity(ctxt, answer, ns);
          ItemTypeChoice:     if validateQuestionnaireResponseItemType(ctxt, answer, ns, ['Coding']) = 'Coding' then
            validateAnswerCode(ctxt, answer, ns, qsrc, qitem);
          ItemTypeOpenChoice: if validateQuestionnaireResponseItemType(ctxt, answer, ns, ['Coding', 'string']) = 'Coding' then
            validateAnswerCode(ctxt, answer, ns, qsrc, qitem);
        end;
        validateQuestionannaireResponseItems(qsrc, qitem.itemList, errors, answer, stack, inProgress);
      finally
        ns.free;
      end;
    end;
  finally
    answers.Free;
  end;
  if qitem.type_ = ItemTypeGroup then
    validateQuestionannaireResponseItems(qsrc, qitem.itemList, errors, element, stack, inProgress)
  else
  begin
    items := TFslList<TFHIRMMElement>.create;
    try
      element.getNamedChildren('item', items);
      for item in items do
      begin
        ns := stack.push(item, -1, nil, nil);
        try
          rule(ctxt, IssueTypeStructure, item.locStart, item.locEnd, stack.literalPath, false, 'Items not of type group should not have items');
        finally
          ns.free;
        end;
      end;
    finally
      answers.Free;
    end;
  end;
end;

procedure TFHIRValidator.validateQuestionannaireResponseItem(qsrc: TFhirQuestionnaire; qItem: TFhirQuestionnaireItem; ctxt : TFHIRValidatorContext; elements: TFslList<TFHIRMMElement>; stack: TNodeStack; inProgress : boolean);
var
  ns : TNodeStack;
  element : TFHIRMMElement;
begin
  if (elements.Count > 1) then
    rule(ctxt, IssueTypeInvalid, elements[1].locStart, elements[1].locEnd, stack.literalPath, qItem.repeats, 'Only one response item with this linkId allowed');
  for element in elements do
  begin
    ns := stack.push(element, -1, nil, nil);
    try
      validateQuestionannaireResponseItem(qsrc, qitem, errors, element, ns, inProgress);
    finally
      ns.free;
    end;
  end;
end;

procedure TFHIRValidator.validateQuestionannaireResponseItems(qsrc : TFhirQuestionnaire; qItems : TFhirQuestionnaireItemList; ctxt : TFHIRValidatorContext; element: TFHIRMMElement; stack: TNodeStack; inProgress : boolean);
var
  items, mapItem : TFslList<TFHIRMMElement>;
  map : TFslMap<TFslList<TFHIRMMElement>>;
  index, lastIndex : integer;
  item : TFHIRMMElement;
  ns : TNodeStack;
  linkId : String;
  qItem : TFhirQuestionnaireItem;
  function getLinkIdIndex(linkId : String) : integer;
  var
    i : integer;
  begin
    result := -1;
    for i := 0 to qItems.Count -1 do
      if (qItems[i].linkId = linkid) then
        exit(i);
  end;
begin
  items := TFslList<TFHIRMMElement>.create;
  try
    element.getNamedChildren('item', items);
    // now, sort into stacks
    map := TFslMap<TFslList<TFHIRMMElement>>.create;
    try
      lastIndex := -1;
      for item in items do
      begin
        linkId := item.getNamedChildValue('linkId');
        if rule(ctxt, IssueTypeRequired, item.locStart, item.locEnd, stack.literalPath, linkId <> '', 'No LinkId, so can''t be validated') then
        begin
          index := getLinkIdIndex(linkId);
          if index = -1 then
          begin
            if findQuestionnaireItem(qsrc, linkId, qitem) then
            begin
              rule(ctxt, IssueTypeStructure, item.locStart, item.locEnd, stack.literalPath, index > -1, 'Structural Error: item is in the wrong place');
              ns := stack.push(item, -1, nil, nil);
              try
                validateQuestionannaireResponseItem(qsrc, qitem, errors, element, ns, inProgress);
              finally
                ns.free;
              end;
            end
            else
              rule(ctxt, IssueTypeNotFound, item.locStart, item.locEnd, stack.literalPath, index > -1, 'LinkId "'+linkId+'" not found in questionnaire');
          end
          else
          begin
            rule(ctxt, IssueTypeStructure, item.locStart, item.locEnd, stack.literalPath, index >= lastIndex, 'Structural Error: items are out of order');
            lastIndex := index;
            if not map.TryGetValue(linkId, mapItem) then
            begin
              mapItem := TFslList<TFHIRMMElement>.create;
              map.Add(linkId, mapitem);
            end;
            mapItem.Add(item.Link);
          end;
        end;
      end;

      // ok, now we have a list of known items, grouped by linkId. We've made an error for anything out of order
      for qItem in qItems do
      begin
        if map.TryGetValue(qItem.linkId, mapItem) then
          validateQuestionannaireResponseItem(qsrc, qItem, errors, mapItem, stack, inProgress)
        else
          rule(ctxt, IssueTypeRequired, element.locStart, element.locEnd, stack.literalPath, not qItem.required, 'No response found for required item '+qItem.linkId);
      end;
    finally
      map.Free;
    end;
  finally
    items.Free;
  end;
end;

procedure TFHIRValidator.validateQuestionnaireResponseItemQuantity(ctxt : TFHIRValidatorContext; answer: TFHIRMMElement; stack: TNodeStack);
begin

end;

function TFHIRValidator.validateQuestionnaireResponseItemType(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; stack: TNodeStack; types: array of String) : string;
var
  values : TFslList<TFHIRMMElement>;
  ns : TNodeStack;
  s, l : String;
begin
  result := '';
  values := TFslList<TFHIRMMElement>.create;
  try
    element.getNamedChildrenWithWildcard('value[x]', values);
    if values.Count > 0 then
    begin
      ns := stack.push(values[0], -1, nil, nil);
      try
        l := '';
        for s in types do
        begin
          commaAdd(l, s);
          if values[0].getName = 'value'+capitalize(s) then
          begin
            result := s;
            break;
          end;
        end;
        if length(types) = 1 then
          rule(ctxt, IssueTypeStructure, values[0].locStart, values[0].locEnd, ns.literalPath, result <> '', 'Answer value must be of type '+types[0])
        else
          rule(ctxt, IssueTypeStructure, values[0].locStart, values[0].locEnd, ns.literalPath, result <> '', 'Answer value must be one of the types '+l);
      finally
        ns.free;
      end;
    end;
  finally
    values.Free;
  end;
end;
{$ENDIF}

function getFirstEntry(bundle: TFHIRMMElement): TFHIRMMElement;
var
  list: TFslList<TFHIRMMElement>;
  resource: TFHIRMMElement;
begin
  list := TFslList<TFHIRMMElement>.Create;
  try
    bundle.getNamedChildren('entry', list);
    if (list.count = 0) then
      result := nil
    else
    begin
      resource := list[0].getNamedChild('resource');
      if (resource = nil) then
        result := nil
      else
        result := resource.children[0];
    end;
  finally
    list.Free;
  end;
end;

{
  * The actual base entry point
}
procedure TFHIRValidator.validateResource(ctxt : TFHIRValidatorContext; resource, element: TFHIRMMElement; profile: TFHIRStructureDefinition; idRule: TResourceIdStatus; stack: TNodeStack);
var
  resourceName: String;
  type_: String;
  first: TFHIRMMElement;
  result : boolean;
  stack1 : TNodeStack;
begin
  result := false;
  if resource = nil then
    resource := element;

  if (stack = nil) then
    stack := TNodeStack.Create()
  else
    stack.Link;
  try

    // getting going - either we got a profile, or not.
    resourceName := element.type_;
    if (profile = nil) then
    begin
      profile := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/' + resourceName));
      ctxt.Owned.add(profile);
      rule(ctxt, IssueTypeINVALID, element.locStart, element.locEnd, stack.addToLiteralPath(resourceName), profile <> nil, 'No profile found for resource type "' + resourceName + '"');
    end;
    if (profile <> nil) then
    begin
      if (profile.BaseType <> '') then
        type_ := profile.baseType
      else
        type_ := profile.name;

      // special case: we have a bundle, and the profile is not for a bundle. We'll try the first entry instead
      if (type_ = resourceName) and (resourceName = 'Bundle') then
      begin
        first := getFirstEntry(element);
        if (first <> nil) and (first.Type_ = type_) then
        begin
          element := first;
          resourceName := element.Type_;
          idRule := risOptional; // why is this?
        end;
      end;

      result := rule(ctxt, IssueTypeINVALID, TSourceLocation.CreateNull, TSourceLocation.CreateNull, stack.addToLiteralPath(resourceName), type_ = resourceName, 'Specified profile type was "' + profile.baseType +
        '", but resource type was "' + resourceName + '"');
    end;

    if (result) then
    begin
      stack1 := stack.push(element, -1, profile.Snapshot.ElementList[0], profile.Snapshot.ElementList[0]);
      try
        if (idRule = risRequired) and ((element.getNamedChild('id') = nil)) then
          rule(ctxt, IssueTypeINVALID, element.locStart, element.locEnd, stack1.literalPath, false, 'Resource requires an id, but none is present')
        else if (idRule = risProhibited) and ((element.getNamedChild('id') <> nil)) then
          rule(ctxt, IssueTypeINVALID, element.locStart, element.locEnd, stack1.literalPath, false, 'Resource has an id, but none is allowed');
        start(ctxt, resource, element, profile, stack1); // root is both definition and type
      finally
        stack1.Free;
      end;
    end;
  finally
    stack.Free;
  end;
end;

// we assume that the following things are true:
// the instance at root is valid against the schema and schematron
// the instance validator had no issues against the base resource profile
// profile is valid, and matches the resource name
procedure TFHIRValidator.start(ctxt : TFHIRValidatorContext; resource, element: TFHIRMMElement; profile: TFHIRStructureDefinition; stack: TNodeStack);
begin
  if (rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, stack.literalPath, profile.Snapshot <> nil,
    'StructureDefinition has no snapshot - validation is against the snapshot, so it must be provided')) then
  begin
    validateElement(ctxt, profile, profile.Snapshot.ElementList[0], nil, nil, resource, element, element.Name, stack, false);
    checkDeclaredProfiles(ctxt, resource, element, stack);

    // specific known special validations
    if (element.Type_ = 'Bundle') then
      validateBundle(ctxt, element, stack);
    if (element.Type_ = 'Observation') then
      validateObservation(ctxt, element, stack);
{$IFDEF DSTU21}
    if (element.Type_ = 'QuestionnaireResponse') then
      validateQuestionannaireResponse(ctxt, element, stack);
{$ENDIF}
  end;
end;

procedure TFHIRValidator.checkDeclaredProfiles(ctxt : TFHIRValidatorContext; resource, element: TFHIRMMElement; stack: TNodeStack);
var
  meta: TFHIRMMElement;
  profiles: TFslList<TFHIRMMElement>;
  i: integer;
  profile: TFHIRMMElement;
  ref, p: String;
  pr: TFHIRStructureDefinition;
begin
  meta := element.getNamedChild('meta');
  if (meta <> nil) then
  begin
    profiles := TFslList<TFHIRMMElement>.Create();
    try
      meta.getNamedChildren('profile', profiles);
      i := 0;
      for profile in profiles do
      begin
        ref := profile.value;
        p := stack.addToLiteralPath(['meta', 'profile', ':' + inttostr(i)]);
        if (rule(ctxt, IssueTypeINVALID, element.locStart, element.locEnd, p, ref <> '', 'StructureDefinition reference invalid')) then
        begin
          pr := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, ref));
          ctxt.Owned.add(pr);
          if (warning(ctxt, IssueTypeINVALID, element.locStart, element.locEnd, p, pr <> nil, 'StructureDefinition reference could not be resolved')) then
          begin
            if (rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, p, pr.Snapshot <> nil,
              'StructureDefinition has no snapshot - validation is against the snapshot, so it must be provided')) then
            begin
              validateElement(ctxt, pr, pr.Snapshot.ElementList[0], nil, nil, resource, element, element.Name, stack, false);
            end;
          end;
          inc(i);
        end;
      end;
    finally
      profiles.free;
    end;
  end;
end;

procedure TFHIRValidator.validateBundle(ctxt : TFHIRValidatorContext; bundle: TFHIRMMElement; stack: TNodeStack);
var
  entries: TFslList<TFHIRMMElement>;
  type_, id: String;
  firstEntry: TFHIRMMElement;
  firstStack: TNodeStack;
  fullUrl: String;
  resource, res: TFHIRMMElement;
  localStack : TNodeStack;
begin
  entries := TFslList<TFHIRMMElement>.Create();
  try
    bundle.getNamedChildren('entry', entries);
    type_ := bundle.getNamedChildValue('type');
    if (entries.count = 0) then
    begin
      rule(ctxt, IssueTypeINVALID, stack.literalPath, (type_ <> 'document') and (type_ <> 'message'), 'Documents or Messages must contain at least one entry');
    end
    else
    begin
      firstEntry := entries[0];
      firstStack := stack.push(firstEntry, 0, nil, nil);
      try
        fullUrl := firstEntry.getNamedChildValue('fullUrl');

        if (type_ = 'document') then
        begin
          res := firstEntry.getNamedChild('resource');
          localStack := firstStack.push(res, -1, nil, nil);
          try
            resource := res.children[0];
            id := resource.getNamedChildValue('id');
            if (rule(ctxt, IssueTypeINVALID, firstEntry.locStart, firstEntry.locEnd, stack.addToLiteralPath(['entry', ':0']), res <> nil, 'No resource on first entry')) then
              validateDocument(ctxt, entries, res, localStack, fullUrl, id);
          finally
            localstack.free;
          end;
        end;
        if (type_ = 'message') then
          validateMessage(ctxt, bundle);
      finally
        firstStack.Free;
      end;
    end;
  finally
    entries.free;
  end;
end;

procedure TFHIRValidator.validateMessage(ctxt : TFHIRValidatorContext; bundle: TFHIRMMElement);
begin
end;

procedure TFHIRValidator.validateDocument(ctxt : TFHIRValidatorContext; entries: TFslList<TFHIRMMElement>; composition: TFHIRMMElement; stack: TNodeStack; fullUrl, id: String);
var
  ns : TNodeStack;
  elem : TFHIRMMElement;
begin
  // first entry must be a composition
  if (rule(ctxt, IssueTypeINVALID, composition.locStart, composition.locEnd, stack.literalPath, composition.Type_ = 'Composition',
    'The first entry in a document must be a composition')) then
  begin
    elem := composition.getNamedChild('subject');
    if rule(ctxt, IssueTypeINVALID, composition.locStart, composition.locEnd, stack.literalPath, elem <> nil, 'In a document, a compsosition must have a subject') then
    begin
    // the composition subject and section references must resolve in the bundle
      ns := stack.push(elem, -1, nil, nil);
    try
      validateBundleReference(ctxt, entries, composition.getNamedChild('subject'), 'Composition Subject', ns, fullUrl, 'Composition', id);
    finally
      ns.Free;
    end;
    validateSections(ctxt, entries, composition, stack, fullUrl, id);
  end;
end;
end;
// rule(ctxt, IssueTypeINVALID, bundle.locStart, bundle.locEnd, 'Bundle', !'urn:guid:' = base), 'The base "urn:guid:" is not valid (use urn:uuid:)');
// rule(ctxt, IssueTypeINVALID, entry.locStart, entry.locEnd, localStack.literalPath, !'urn:guid:' = ebase), 'The base "urn:guid:" is not valid');
// rule(ctxt, IssueTypeINVALID, entry.locStart, entry.locEnd, localStack.literalPath, !Utilities.noString(base) ) or ( !Utilities.noString(ebase), 'entry does not have a base');
// String firstBase := nil;
// firstBase := ebase = nil ? base : ebase;

procedure TFHIRValidator.validateSections(ctxt : TFHIRValidatorContext; entries: TFslList<TFHIRMMElement>; focus: TFHIRMMElement; stack: TNodeStack;
  fullUrl, id: String);
var
  sections: TFslList<TFHIRMMElement>;
  section: TFHIRMMElement;
  localStack: TNodeStack;
begin
  sections := TFslList<TFHIRMMElement>.Create();
  try
    focus.getNamedChildren('entry', sections);
    for section in sections do
    begin
      localStack := stack.push(section, 1, nil, nil);
      try
        validateBundleReference(ctxt, entries, section.getNamedChild('content'), 'Section Content', localStack, fullUrl, 'Composition', id);
        validateSections(ctxt, entries, section, localStack, fullUrl, id);
      finally
        localStack.Free;
      end;
    end;
  finally
    sections.free;
  end;
end;

procedure TFHIRValidator.validateBundleReference(ctxt : TFHIRValidatorContext; entries: TFslList<TFHIRMMElement>; ref: TFHIRMMElement; name: String;
  stack: TNodeStack; fullUrl, type_, id: String);
var
  target: TFHIRMMElement;
begin
  if (ref <> nil) and (ref.getNamedChildValue('reference') <> '') then
  begin
    target := resolveInBundle(entries, ref.getNamedChildValue('reference'), fullUrl, type_, id);
    rule(ctxt, IssueTypeINVALID, ref.locStart, ref.locEnd, stack.addToLiteralPath(['reference']), target <> nil,
      'Unable to resolve the target of the reference in the bundle (' + name + ')');
  end;
end;

Function TFHIRValidator.resolveInBundle(entries: TFslList<TFHIRMMElement>; ref, fullUrl, type_, id: String): TFHIRMMElement;
var
  entry, res : TFHIRMMElement;
  fu, u, t, i, et, eid: String;
  parts: TArray<String>;
begin
  result := nil;
  if (isAbsoluteUrl(ref)) then
  begin
    // if the reference is absolute, then you resolve by fullUrl. No other thinking is required.
    for entry in entries do
    begin
      fu := entry.getNamedChildValue('fullUrl');
      if (ref = fu) then
      begin
        result := entry;
        exit;
      end;
    end;
  end
  else
  begin
    // split into base, type, and id
    u := '';
    if (fullUrl <> '') and (fullUrl.endsWith(type_ + '/' + id)) then
      // fullUrl := complex
      u := fullUrl.substring((type_ + '/' + id).length) + ref;
    parts := ref.split(['/']);
    if (length(parts) >= 2) then
    begin
      t := parts[0];
      i := parts[1];
      for entry in entries do
      begin
        fu := entry.getNamedChildValue('fullUrl');
        if (u <> '') and (fullUrl = u) then
        begin
          result := entry;
          exit;
        end;
        if (u = '') then
        begin
          res := entry.getNamedChild('resource');
          et := res.Type_;
          eid := res.getNamedChildValue('id');
          if (t = et) and (i = eid) then
          begin
            result := entry;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

function TFHIRValidator.getProfileForType(ctxt : TFHIRValidatorContext; type_: String): TFHIRStructureDefinition;
begin
  result := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/' + type_));
  ctxt.Owned.add(result);
end;

procedure TFHIRValidator.validateObservation(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; stack: TNodeStack);
begin
  // all observations should have a subject, a performer, and a time
  bpCheck(ctxt, IssueTypeINVALID, element.locStart, element.locEnd, stack.literalPath, element.getNamedChild('subject') <> nil, 'All observations should have a subject');
  bpCheck(ctxt, IssueTypeINVALID, element.locStart, element.locEnd, stack.literalPath, element.getNamedChild('performer') <> nil, 'All observations should have a performer');
  bpCheck(ctxt, IssueTypeINVALID, element.locStart, element.locEnd, stack.literalPath, (element.getNamedChild('effectiveDateTime') <> nil) or
    (element.getNamedChild('effectivePeriod') <> nil), 'All observations should have an effectiveDateTime or an effectivePeriod');
end;

procedure TFHIRValidator.bpCheck(ctxt : TFHIRValidatorContext; t: TFhirIssueTypeEnum; locStart, locEnd: TSourceLocation; literalPath: String; test: boolean; message: String);
begin
  case ctxt.BPWarnings of
    bpwlHint:
      hint(ctxt, t, locStart, locEnd, literalPath, test, message);
    bpwlWarning:
      warning(ctxt, t, locStart, locEnd, literalPath, test, message);
    bpwlError:
      rule(ctxt, t, locStart, locEnd, literalPath, test, message);
    bpwlIgnore:
      ; // do nothing
  end;
end;

function isPrimitiveType(t: String): boolean;
begin
  result := SameText(t, 'boolean') or SameText(t, 'integer') or SameText(t, 'string') or SameText(t, 'decimal') or SameText(t, 'uri') or SameText(t, 'base64Binary') or
    SameText(t, 'instant') or SameText(t, 'date') or SameText(t, 'uuid') or SameText(t, 'id') or SameText(t, 'xhtml') or SameText(t, 'markdown') or SameText(t, 'dateTime') or
    SameText(t, 'time') or SameText(t, 'code') or SameText(t, 'oid') or SameText(t, 'id');
end;

function describeTypes(types: TFhirElementDefinitionTypeList): String;
var
  tc: TFhirElementDefinitionType;
begin
  result := '';
  for tc in types do
    CommaAdd(result, tc.code);
end;

function resolveNameReference(Snapshot: TFhirStructureDefinitionSnapshot; name: String): TFHIRElementDefinition;
var
  ed: TFHIRElementDefinition;
begin
  result := nil;
  for ed in Snapshot.ElementList do
    if (name = '#'+ed.id) then
    begin
      result := ed;
      exit;
    end;
end;

function findElement(profile: TFHIRStructureDefinition; name: String): TFHIRElementDefinition;
var
  c: TFHIRElementDefinition;
begin
  result := nil;
  for c in profile.Snapshot.ElementList do
  begin
    if (c.path = name) then
    begin
      result := c;
      exit;
    end;
  end;
end;

function TFHIRValidator.resolveType(ctxt : TFHIRValidatorContext; t: String): TFHIRElementDefinition;
var
  url: String;
  sd: TFHIRStructureDefinition;
begin
  url := 'http://hl7.org/fhir/StructureDefinition/' + t;
  sd := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, url));
  ctxt.Owned.add(sd);
  if (sd = nil) or (sd.Snapshot = nil) then
    result := nil
  else
    result := sd.Snapshot.ElementList[0];
end;

function TFHIRValidator.rule(ctxt : TFHIRValidatorContext; t: TFhirIssueTypeEnum; path: String; thePass: boolean; msg: String): boolean;
var
  vm: TFhirOperationOutcomeIssue;
begin
  if not thePass then
  begin
    vm := TFhirOperationOutcomeIssue.Create;
    try
      vm.severity := IssueSeverityError;
      vm.locationList.append.value := path;
      vm.code := t;
      vm.details := TFHIRCodeableConcept.Create;
      vm.details.text := msg;
      ctxt.Issues.Add(TFHIROperationOutcomeIssue2.Create(vm.Link));
    finally
      vm.Free;
    end;
  end;
  result := thePass;
end;

function TFHIRValidator.rule(ctxt : TFHIRValidatorContext; t: TFhirIssueTypeEnum; locStart, locEnd: TSourceLocation; path: String; thePass: boolean; msg: String): boolean;
var
  vm: TFhirOperationOutcomeIssue;
begin
  if not thePass then
  begin
    vm := TFhirOperationOutcomeIssue.Create;
    try
      vm.Tags['s-l'] := inttostr(locStart.lineForHuman);
      vm.Tags['s-c'] := inttostr(locStart.colForHuman);
      vm.Tags['e-l'] := inttostr(locEnd.lineForHuman);
      vm.Tags['e-c'] := inttostr(locEnd.colForHuman);
      vm.severity := IssueSeverityError;
      vm.locationList.append.value := path + ' (@ ' + locStart.describe + ')';
      vm.code := t;
      vm.details := TFHIRCodeableConcept.Create;
      vm.details.text := msg;
      ctxt.Issues.Add(TFHIROperationOutcomeIssue2.Create(vm.Link));
    finally
      vm.Free;
    end;
  end;
  result := thePass;
end;

function isBundleEntry(path: String): boolean;
var
  parts: TArray<String>;
begin
  parts := path.split(['.']);
  result := (length(parts) > 3) and (parts[length(parts) - 1].startsWith('resource')) and ((parts[length(parts) - 3] = 'entry')) or (parts[length(parts) - 2] = 'entry');
end;

function isParametersEntry(path: String): boolean;
var
  parts: TArray<String>;
begin
  parts := path.split(['.']);
  result := (length(parts) > 2) and (parts[length(parts) - 1].startsWith('resource')) and ((parts[length(parts) - 2] = 'parameter')) or (parts[length(parts) - 2].startsWith('entry['));
end;

procedure TFHIRValidator.validateElement(ctxt : TFHIRValidatorContext; profile: TFHIRStructureDefinition; definition: TFHIRElementDefinition;
  cprofile: TFHIRStructureDefinition; context: TFHIRElementDefinition; resource, element: TFHIRMMElement; actualType: String; stack: TNodeStack; inCodeableConcept: boolean);
var
  childDefinitions: TFHIRElementDefinitionList;
  children: TFslList<TElementInfo>;
  iter: TChildIterator;
  slice, ed, td: TFHIRElementDefinition;
  process, match: boolean;
  ei: TElementInfo;
  count, i: integer;
  t, prefix: String;
  tc, trc: TFhirElementDefinitionType;
  p: TFHIRStructureDefinition;
  localStack: TNodeStack;
  thisIsCodeableConcept: boolean;
  last : integer;
begin
  assert(profile.snapshot.elementList.ExistsByReference(definition));
  doProgress(stack.literalPath);
//  // for later re-use
//  element.Definition := definition;
//  element.Profile := profile;

  checkInvariants(ctxt, stack.literalPath, profile, definition, '', '', resource, element);

  // get the list of direct defined children, including slices
  children := TFslList<TElementInfo>.Create();
  childDefinitions := getChildMap(profile, definition.name, definition.path, definition.ContentReference);
  try

    // 1. List the children, and remember their exact path (convenience)
    iter := TChildIterator.Create(stack.literalPath, element.link);
    try
      while (iter.next()) do
        children.Add(TElementInfo.Create(iter.name(), iter.element(), iter.path(), iter.count()));
    finally
      iter.Free;
    end;

    // 2. assign children to a definition
    // for each definition, for each child, check whether it belongs in the slice
    slice := nil;
    for i := 0 to childDefinitions.count - 1 do
    begin
      ed := childDefinitions[i];
      process := true;
      // where are we with slicing
      if (ed.Slicing <> nil) then
      begin
        if (slice <> nil) and (slice.path = ed.path) then
          raise EDefinitionException.create('Slice encountered midway through path on ' + slice.path);
        slice := ed;
        process := false;
      end
      else if (slice <> nil) and (slice.path <> ed.path) then
        slice := nil;

      if (process) then
      begin
        for ei in children do
        begin
          match := false;
          if (slice = nil) then
          begin
            match := nameMatches(ei.name, tail(ed.path));
          end
          else
          begin
            if nameMatches(ei.name, tail(ed.path)) then
              match := sliceMatches(ctxt, ei.element, ei.path, slice, ed, profile);
          end;
          if (match) then
          begin
            if (rule(ctxt, IssueTypeINVALID, ei.locStart, ei.locEnd, ei.path, ei.definition = nil, 'Element matches more than one slice')) then
            begin
              ei.definition := ed;
              ei.index := i;
            end;
          end;
        end;
      end;
    end;

    last := -1;
    for ei in children do
    begin
      if (ei.path.endsWith('.extension')) then
        rule(ctxt, IssueTypeINVALID, ei.locStart, ei.locEnd, ei.path, ei.definition <> nil, 'Element is unknown or does not match any slice (url:="' + ei.element.getNamedChildValue('url') + '")')
      else
        rule(ctxt, IssueTypeINVALID, ei.locStart, ei.locEnd, ei.path, (ei.definition <> nil), 'Element is unknown or does not match any slice');
       rule(ctxt, IssueTypeINVALID, ei.locStart, ei.locEnd, ei.path, (ei.definition = nil) or (ei.index >= last), 'Element is out of order');
      last := ei.index;
    end;

    // 3. report any definitions that have a cardinality problem
    for ed in childDefinitions do
    begin
      if (ed.representation = []) then
      begin // ignore xml attributes
        count := 0;
        for ei in children do
          if (ei.definition = ed) then
            inc(count);
        if (ed.Min <> '0') then
          rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, stack.literalPath, count >= StrToInt(ed.Min), 'Element "' + stack.literalPath + '.' + tail(ed.path) +
            '": minimum required = ' + ed.Min + ', but only found ' + inttostr(count));
        if (ed.max <> '*') then
          rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, stack.literalPath, count <= StrToInt(ed.max), 'Element ' + tail(ed.path) + ' @ ' + stack.literalPath
            + ': max allowed = ' + ed.max + ', but found ' + inttostr(count));

      end;
    end;
    // 4. check order if any slices are orderd. (todo)

    // 5. inspect each child for validity
    for ei in children do
    begin
      if (ei.definition <> nil) then
      begin
        t := '';
        td := nil;
        if (ei.definition.Type_List.count = 1) and (ei.definition.Type_List[0].code <> '') and (ei.definition.Type_List[0].code <> 'Element') and
          (ei.definition.Type_List[0].code <> 'BackboneElement') then
          t := ei.definition.Type_List[0].code
        else if (ei.definition.Type_List.count = 1) and (ei.definition.Type_List[0].code = '') then
        begin
          prefix := tail(ei.definition.path);
          assert(prefix.endsWith('[x]'));
          t := ei.name.substring(prefix.length - 3);
          if (isPrimitiveType(t)) then
            t := uncapitalize(t);
        end
        else if (ei.definition.Type_List.count > 1) then
        begin
          prefix := tail(ei.definition.path);
          prefix := prefix.substring(0, prefix.length - 3);
          for tc in ei.definition.Type_List do
            if ((prefix + capitalize(tc.code)) = ei.name) then
              t := tc.code;
          if (t = '') then
          begin
            trc := ei.definition.Type_List[0];
            if (trc.code = 'Reference') then
              t := 'Reference'
            else
            begin
              rule(ctxt, IssueTypeSTRUCTURE, ei.locStart, ei.locEnd, stack.literalPath, false, 'The element ' + ei.name + ' is illegal. Valid types at this point are ' +
                describeTypes(ei.definition.Type_List));
            end;
          end;
        end
        else if (ei.definition.ContentReference <> '') then
          td := resolveNameReference(profile.Snapshot, ei.definition.ContentReference);

        if (t <> '') then
        begin
          if (t.startsWith('@')) then
          begin
            ei.definition := findElement(profile, t.substring(1));
            t := '';
          end;
        end;
        if (t = '') then
          localStack := stack.push(ei.element, ei.count, ei.definition, td)
        else
          localStack := stack.push(ei.element, ei.count, ei.definition, resolveType(ctxt, t));
        try
          if ei.path <> localStack.literalPath then
            raise EDefinitionException.create('paths differ: ' + ei.path + ' vs ' + localStack.literalPath);

          assert(ei.path = localStack.literalPath);
          thisIsCodeableConcept := false;

          if (t <> '') then
          begin
            if (isPrimitiveType(t)) then
              checkPrimitive(ctxt, ei.path, t, ei.definition, ei.element, profile)
            else
            begin
              if (t = 'Identifier') then
                checkIdentifier(ctxt, ei.path, ei.element, ei.definition)
              else if (t = 'Coding') then
                checkCoding(ctxt, ei.path, ei.element, profile, ei.definition, inCodeableConcept)
              else if (t = 'Quantity') then
                checkQuantity(ctxt, ei.path, ei.element, ei.definition)
              else if (t = 'CodeableConcept') then
              begin
                checkCodeableConcept(ctxt, ei.path, ei.element, profile, ei.definition);
                thisIsCodeableConcept := true;
              end
              else if (t = 'Reference') then
                checkReference(ctxt, ei.path, ei.element, profile, ei.definition, actualType, localStack);

              if (t = 'Extension') then
                checkExtension(ctxt, ei.path, ei.element, ei.definition, profile, localStack)
              else if (t = 'Resource') then
                validateContains(ctxt, ei.path, ei.definition, definition, resource, ei.element, localStack, idStatusForEntry(element, ei))
              else
              begin
                p := getProfileForType(ctxt, t);
                if (rule(ctxt, IssueTypeSTRUCTURE, ei.locStart, ei.locEnd, ei.path, p <> nil, 'Unknown type ' + t)) then
                  validateElement(ctxt, p, p.Snapshot.ElementList[0], profile, ei.definition, resource, ei.element, t, localStack, thisIsCodeableConcept);
              end;
            end;
          end
          else
          begin
            if (rule(ctxt, IssueTypeSTRUCTURE, ei.locStart, ei.locEnd, stack.literalPath, ei.definition <> nil, 'Unrecognised Content ' + ei.name)) then
              validateElement(ctxt, profile, ei.definition, nil, nil, resource, ei.element, t, localStack, false);
          end;
        finally
          localStack.Free;
        end;
      end;
    end;
  finally
    childDefinitions.Free;
    children.Free;
  end;
end;

{ *
  *
  * @param element - the candidate that might be in the slice
  * @param path - for reporting any errors. the XPath for the element
  * @param slice - the definition of how slicing is determined
  * @param ed - the slice for which to test membership
  * @return
  * @;
}
function TFHIRValidator.sliceMatches(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; path: String; slice, ed: TFHIRElementDefinition; profile: TFHIRStructureDefinition): boolean;
var
  s: TFhirString;
  discriminator: String;
  criteria: TFHIRElementDefinition;
  value: TFhirElement;
begin
  result := true;
  if (slice.Slicing.DiscriminatorList.count = 0) then
    result := false // cannot validate in this case
  else
    for s in slice.Slicing.DiscriminatorList do
    begin
      discriminator := s.value;
      criteria := getCriteriaForDiscriminator(ctxt, path, ed, discriminator, profile);
      if (discriminator = 'url') and criteria.path.endsWith('xtension.url') then
      begin
        if (element.getNamedChildValue('url') <> TFHIRUri(criteria.fixed).value) then
        begin
          result := false;
          exit;
        end;
      end
      else
      begin
        value := getValueForDiscriminator(element, discriminator, criteria);
        if (not valueMatchesCriteria(value, criteria)) then
        begin
          result := false;
          exit;
        end;
      end;
    end;
end;

function TFHIRValidator.valueMatchesCriteria(value: TFhirElement; criteria: TFHIRElementDefinition): boolean;
begin
  result := false;
end;

function TFHIRValidator.warning(ctxt : TFHIRValidatorContext; t: TFhirIssueTypeEnum; locStart, locEnd: TSourceLocation; path: String; thePass: boolean; msg: String): boolean;
var
  vm: TFhirOperationOutcomeIssue;
begin
  if not thePass then
  begin
    vm := TFhirOperationOutcomeIssue.Create;
    try
      vm.Tags['s-l'] := inttostr(locStart.lineForHuman);
      vm.Tags['s-c'] := inttostr(locStart.colForHuman);
      vm.Tags['e-l'] := inttostr(locEnd.lineForHuman);
      vm.Tags['e-c'] := inttostr(locEnd.colForHuman);
      vm.severity := IssueSeverityWarning;
      vm.locationList.append.value := path + ' (@ ' + locStart.describe + ')';
      vm.code := t;
      vm.details := TFHIRCodeableConcept.Create;
      vm.details.text := msg;
      ctxt.Issues.Add(TFHIROperationOutcomeIssue2.Create(vm.link));
    finally
      vm.Free;
    end;
  end;
  result := thePass;
end;

function TFHIRValidator.getValueForDiscriminator(element: TFHIRMMElement; discriminator: String; criteria: TFHIRElementDefinition): TFhirElement;
begin
  result := nil;
end;

function TFHIRValidator.hint(ctxt : TFHIRValidatorContext; t: TFhirIssueTypeEnum; locStart, locEnd: TSourceLocation; path: String; thePass: boolean; msg: String): boolean;
var
  vm: TFhirOperationOutcomeIssue;
begin
  if not thePass then
  begin
    vm := TFhirOperationOutcomeIssue.Create;
    try
      vm.Tags['s-l'] := inttostr(locStart.lineForHuman);
      vm.Tags['s-c'] := inttostr(locStart.colForHuman);
      vm.Tags['e-l'] := inttostr(locEnd.lineForHuman);
      vm.Tags['e-c'] := inttostr(locEnd.colForHuman);
      vm.severity := IssueSeverityInformation;
      vm.locationList.append.value := path + ' (@ ' + locStart.describe + ')';
      vm.code := t;
      vm.details := TFHIRCodeableConcept.Create;
      vm.details.text := msg;
      ctxt.Issues.Add(TFHIROperationOutcomeIssue2.Create(vm));
    finally
      vm.Free;
    end;
  end;
  result := thePass;
end;

function TFHIRValidator.idStatusForEntry(ep : TFHIRMMElement; ei: TElementInfo): TResourceIdStatus;
var
  e : TFHIRMMElement;
  s : String;
begin
  if (isBundleEntry(ei.path)) then
  begin
    e := ep.getNamedChild('request');
    if (e <> nil) then
      e := e.getNamedChild('method');
    if (e = nil) then
      result := risRequired
    else
    begin
      s := e.value;
      if (s = 'PUT') then
        result := risRequired
      else if (s = 'POST') then
        result := risProhibited
      else // actually, we should never get to here; a bundle entry with method get/delete should not have a resource
        result := risOptional;
    end
  end
  else if (isParametersEntry(ei.path)) then
    result := risOptional
  else
    result := risRequired;
end;

function TrimBof(const s : String):String;
begin
  result := s;
  while (result[1] <> '<') do
    delete(result, 1, 1);
end;


//function TFHIRValidator.LoadDoc(name : String; isFree : boolean) : IXMLDomDocument2;
//Var
//  LVariant: Variant;
//  buf : TFslNameBuffer;
//Begin
//  buf := ValContext.GetSourceByName(name);
//  LVariant := LoadMsXMLDomV(isfree);
//  Result := IUnknown(TVarData(LVariant).VDispatch) as IXMLDomDocument2;
//  result.async := false;
//  if isFree then
//    result.resolveExternals := true;
//  if not result.loadXML(TrimBof(buf.AsUnicode)) then
//    raise EDefinitionException.create('unable to parse XML because '+result.parseError.reason);
//end;
//
function TFHIRValidator.getCriteriaForDiscriminator(ctxt : TFHIRValidatorContext; path: String; ed: TFHIRElementDefinition; discriminator: String; profile: TFHIRStructureDefinition)
  : TFHIRElementDefinition;
var
  childDefinitions, Snapshot: TFHIRElementDefinitionList;
  // t : TFHIRStructureDefinition;
  originalPath, goal: String;
  ty: TFHIRStructureDefinition;
  index: integer;
begin
  childDefinitions := getChildMap(profile, ed.name, ed.path, '');
  try
    if (childDefinitions.count = 0) then
    begin
      // going to look at the type
      if (ed.Type_List.count = 0) then
        raise EDefinitionException.create('Error in profile for ' + path + ' no children, no type');
      if (ed.Type_List.count > 1) then
        raise EDefinitionException.create('Error in profile for ' + path + ' multiple types defined in slice discriminator');
      if (ed.Type_List[0].profileList.count > 0) then
      begin
        // need to do some special processing for reference here...
        if (ed.Type_List[0].code = 'Reference') then
          discriminator := discriminator.substring(discriminator.indexOf('.') + 1);
        ty := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, ed.Type_List[0].profileList[0].value));
      end
      else
        ty := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/' + ed.Type_List[0].code));
      ctxt.Owned.add(ty);
      Snapshot := ty.Snapshot.ElementList;
      ed := Snapshot[0];
      index := 0;
    end
    else
    begin
      Snapshot := ChildDefinitions;
      index := -1;
    end;
    originalPath := ed.path;
    goal := originalPath + '.' + discriminator;

    inc(index);
    while (index < Snapshot.count) and (Snapshot[index].path <> originalPath) do
    begin
      if (Snapshot[index].path = goal) then
      begin
        result := Snapshot[index];
        exit;
      end;
      inc(index);
    end;
    raise EDefinitionException.create('Unable to find discriminator definition for ' + goal + ' in ' + discriminator + ' at ' + path);
  finally
    childDefinitions.Free;
  end;
end;

function TFHIRValidator.checkResourceType(ctxt : TFHIRValidatorContext; ty: String): String;
var
  t : TFHIRResource;
begin
  t := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/' + ty));
  ctxt.Owned.add(t);
  if (t <> nil) then
    result := ty
  else
    result := '';
end;

function TFHIRValidator.tryParse(ctxt : TFHIRValidatorContext; ref: String): String;
var
  parts: TArray<String>;
begin
  parts := ref.split(['/']);
  case (length(parts)) of
    1:
      result := '';
    2:
      result := checkResourceType(ctxt, parts[0]);
  else
    if (parts[length(parts) - 2] = '_history') then
      result := checkResourceType(ctxt, parts[length(parts) - 4])
    else
      result := checkResourceType(ctxt, parts[length(parts) - 2]);
  end;
end;

procedure TFHIRValidator.checkReference(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; profile: TFHIRStructureDefinition;
  container: TFHIRElementDefinition; parentType: String; stack: TNodeStack);
var
  ref: String;
  we: TFHIRMMElement;
  ft, pr, bt: String;
  ok: boolean;
  b: String;
  ty: TFhirElementDefinitionType;
begin
  ref := element.getNamedChildValue('reference');
  if (ref = '') then
  begin
    // todo - what should we do in this case?
    hint(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, path, element.getNamedChildValue('display') <> '',
      'A Reference without an actual reference should have a display');
    exit;
  end;

  we := resolve(ref, stack);
  if (we <> nil) then
    ft := we.Type_
  else
    ft := tryParse(ctxt, ref);
  if (hint(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, path, ft <> '', 'Unable to determine type of target resource')) then
  begin
    ok := false;
    b := '';
    for ty in container.Type_List do
    begin
      if (not ok) and (ty.code = 'Reference') then
      begin
        // we validate as much as we can. First, can we infer a type from the profile?
        if (ty.profileList.count = 0) or (ty.profileList[0].value = 'http://hl7.org/fhir/StructureDefinition/Resource') then
          ok := true
        else
        begin
          pr := ty.profileList[0].value;
          bt := getBaseType(ctxt, profile, pr);
          if (rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, path, bt <> '', 'Unable to resolve the profile reference "' + pr + '"')) then
          begin
            if (b <> '') then
              b := b + ', ';
            b := b + bt;
            ok := bt = ft;
          end
          else
            ok := true; // suppress following check
        end;
      end;
      if (not ok) and (ty.code = '') then
      begin
        ok := true; // can refer to anything
      end;
    end;
    rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, path, ok, 'Invalid Resource target type. Found ' + ft + ', but expected one of (' + b + ')');
  end;
end;

function TFHIRValidator.resolve(ref: String; stack: TNodeStack): TFHIRMMElement;
var
  res: TFHIRMMElement;
begin
  if (ref.startsWith('#')) then
  begin
    // work back through the contained list.
    // really, there should only be one level for this (contained resources cannot contain
    // contained resources), but we"ll leave that to some other code to worry about
    while (stack <> nil) and (stack.element <> nil) do
    begin
      res := getContainedById(stack.element, ref.substring(1));
      if (res <> nil) then
      begin
        result := res;
        exit;
      end;
      stack := stack.parent;
    end;
    result := nil;
  end
  else
  begin
    // work back through the contained list - if any of them are bundles, try to resolve
    // the resource in the bundle
    while (stack <> nil) and (stack.element <> nil) do
    begin
      if ('Bundle' = stack.element.Type_) then
      begin
        res := getFromBundle(stack.element, ref.substring(1));
        if (res <> nil) then
        begin
          result := res;
          exit;
        end;
      end;
      stack := stack.parent;
    end;
    // todo: consult the external host for resolution
    result := nil;
  end;
end;

function TFHIRValidator.getFromBundle(bundle: TFHIRMMElement; ref: String): TFHIRMMElement;
var
  entries: TFslList<TFHIRMMElement>;
  we, res: TFHIRMMElement;
  url: String;
begin
  entries := TFslList<TFHIRMMElement>.Create();
  try
    bundle.getNamedChildren('entry', entries);
    for we in entries do
    begin
      res := we.getNamedChild('resource').children[0];
      if (res <> nil) then
      begin
        url := genFullUrl(bundle.getNamedChildValue('base'), we.getNamedChildValue('base'), res.Name, res.getNamedChildValue('id'));
        if (url.endsWith(ref)) then
        begin
          result := res;
          exit;
        end;
      end;
    end;
    result := nil;
  finally
    entries.free;
  end;
end;

function TFHIRValidator.genFullUrl(bundleBase, entryBase, ty, id: String): String;
var
  base: String;
begin
  if entryBase = '' then
    base := bundleBase
  else
    base := entryBase;
  if (base = '') then
    result := ty + '/' + id
  else if ('urn:uuid' = base) or ('urn:oid' = base) then
    result := base + id
  else
    result := base + '/' + ty + '/' + id;
end;

function TFHIRValidator.getContainedById(container: TFHIRMMElement; id: String): TFHIRMMElement;
var
  contained: TFslList<TFHIRMMElement>;
  we : TFHIRMMElement;
begin
  contained := TFslList<TFHIRMMElement>.Create();
  try
    container.getNamedChildren('contained', contained);
    for we in contained do
    begin
      if (id = we.getNamedChildValue('id')) then
      begin
        result := we;
        exit;
      end;
    end;
    result := nil;
  finally
    contained.free;
  end;
end;

function TFHIRValidator.getBaseType(ctxt : TFHIRValidatorContext; profile: TFHIRStructureDefinition; pr: String): String;
var
  p: TFHIRStructureDefinition;
begin
  // if (pr.startsWith('http://hl7.org/fhir/StructureDefinition/')) begin
  // // this just has to be a base type
  // return pr.substring(40);
  // end; else begin
  p := resolveProfile(ctxt, profile, pr);
  if (p = nil) then
    result := ''
  else if (p.Kind = StructureDefinitionKindRESOURCE) then
    result := p.Snapshot.ElementList[0].path
  else
    result := p.Snapshot.ElementList[0].Type_List[0].code;
  // end;
end;

function TFHIRValidator.resolveProfile(ctxt : TFHIRValidatorContext; profile: TFHIRStructureDefinition; pr: String): TFHIRStructureDefinition;
var
  r: TFHIRResource;
begin
  if (pr.startsWith('#')) then
  begin
    for r in profile.containedList do
    begin
      if (r.id = pr.substring(1)) and (r is TFHIRStructureDefinition) then
      begin
        exit(r as TFHIRStructureDefinition);
      end;
    end;
    result := nil;
  end
  else
  begin
    result := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, pr));
    ctxt.Owned.add(result);
  end;
end;

function TFHIRValidator.checkExtension(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; def: TFHIRElementDefinition;
  profile: TFHIRStructureDefinition; stack: TNodeStack): TFHIRStructureDefinition;
var
  url: String;
  isModifier: boolean;
  ex: TFHIRStructureDefinition;
begin
  url := element.getNamedChildValue('url');
  isModifier := element.name = 'modifierExtension';

  ex := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, url));
  if (ex = nil) then
  begin
    if (not rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, path, allowUnknownExtension(ctxt, url), 'The extension ' + url + ' is unknown, and not allowed here'))
    then
      warning(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, path, allowUnknownExtension(ctxt, url), 'Unknown extension ' + url);
  end
  else
  begin
    ctxt.Owned.add(ex);
    if (def.isModifier) then
      rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, path + '[url:="' + url + '"]', ex.Snapshot.ElementList[0].isModifier,
        'Extension modifier mismatch: the extension element is labelled as a modifier, but the underlying extension is not')
    else
      rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, path + '[url:="' + url + '"]', not ex.Snapshot.ElementList[0].isModifier,
        'Extension modifier mismatch: the extension element is not labelled as a modifier, but the underlying extension is');

    // two questions
    // 1. can this extension be used here?
    checkExtensionContext(ctxt, element, { path+'[url:="'+url+'"]', } ex, stack, ex.url);

    if (isModifier) then
      rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, path + '[url:="' + url + '"]', ex.Snapshot.ElementList[0].isModifier,
        'The Extension "' + url + '" must be used as a modifierExtension')
    else
      rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, path + '[url:="' + url + '"]', not ex.Snapshot.ElementList[0].isModifier,
        'The Extension "' + url + '" must not be used as an extension (it"s a modifierExtension)');

    // 2. is the content of the extension valid?

  end;
  result := ex;
end;

function TFHIRValidator.allowUnknownExtension(ctxt : TFHIRValidatorContext; url: String): boolean;
var
  s: String;
begin
  result := ctxt.IsAnyExtensionsAllowed;
  if (url.contains('example.org')) or (url.contains('acme.com')) or (url.contains('nema.org')) then
    result := true
  else if FExtensionDomains <> nil then
    for s in FExtensionDomains do
      if (url.startsWith(s)) then
        result := true;
end;

// function TFHIRValidator.isKnownType(code : String) : boolean;
// begin
// result := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, code.toLower)) <> nil;
// end;

// function TFHIRValidator.getElementByPath(definition : TFHIRStructureDefinition; path : String) : TFHIRElementDefinition;
// var
// e : TFHIRElementDefinition;
// begin
// for e in definition.SnapShot.ElementList do
// begin
// if (e.Path = path) then
// begin
// result := e;
// exit;
// end;
// end;
// result := nil;
// end;

function TFHIRValidator.checkExtensionContext(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; definition: TFHIRStructureDefinition; stack: TNodeStack;
  extensionParent: String): boolean;
var
  extUrl: String;
  b, c, p, lp, pe: String;
  ct: TFhirString;
  ok: boolean;
begin
  extUrl := definition.url;
  if definition.snapshot.elementList[0].isModifier then
    pe := '.modifierExtension'
  else
    pe := '.extension';

  p := '';
  for lp in stack.logicalPaths do
  begin
    if p <> '' then
      p := p + ', ';
    p := p + lp;
  end;
  if (definition.ContextType = ExtensionContextDATATYPE) then
  begin
    ok := false;
    b := '';
    for ct in definition.contextList do
    begin
      if b <> '' then
        b := b + ', ';
      if ct.value = '*' then
        b := b + ct.value
      else
        b := b + ct.value+pe;
      if (ct.value = '*') or (stack.logicalPaths.indexOf(ct.value + pe) > -1) then
        ok := true;
    end;
    result := rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, stack.literalPath, ok,
      'The extension ' + extUrl + ' is not allowed to be used on the logical path set [' + p + '] (allowed: datatype:=' + b + ')');
  end
  else if (definition.ContextType = ExtensionContextEXTENSION) then
  begin
    ok := false;
    for ct in definition.contextList do
      if (ct.value = '*') or (ct.value = extensionParent) then
        ok := true;
    result := rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, stack.literalPath, ok,
      'The extension ' + extUrl + ' is not allowed to be used with the extension "' + extensionParent + '"');
  end
  else if (definition.ContextType = ExtensionContextRESOURCE) then
  begin
    ok := false;
    // String simplePath := container.Path;
    // System.out.println(simplePath);
    // if (effetive.endsWith('.extension') ) or ( simplePath.endsWith('.modifierExtension'))
    // simplePath := simplePath.substring(0, simplePath.lastIndexOf("."));
    b := '';
    for ct in definition.contextList do
    begin
      if b <> '' then
        b := b + ', ';
      if ct.value = '*' then
        b := b + ct.value
      else
        b := b + ct.value+pe;
      c := ct.value;
      if (c = '*') or (stack.logicalPaths.indexOf(c + pe) >= 0) or (c.startsWith('@') and (stack.logicalPaths.indexOf(c.substring(1) + pe) >= 0)) then
        ok := true;
    end;
    result := rule(ctxt, IssueTypeSTRUCTURE, element.locStart, element.locEnd, stack.literalPath, ok,
      'The extension ' + extUrl + ' is not allowed to be used on the logical path set ' + p + ' (allowed: resource:=' + b + ')');
  end
  else
    raise EDefinitionException.create('Unknown context type');
end;

//
// private String simplifyPath(String path) begin
// String s := path.replace('/f:', '.');
// while (s.contains('['))
// s := s.substring(0, s.indexOf('['))+s.substring(s.indexOf(']')+1);
// TArray<String> parts := s.split('\\.');
// int i := 0;
// while (i < parts.length ) and ( !context.getProfiles().containsKey(parts[i].toLowerCase()))
// i++;
// if (i >= parts.length)
// raise EDefinitionException.create('Unable to process part '+path);
// int j := parts.length - 1;
// while (j > 0 ) and ( (parts[j] = 'extension') ) or ( parts[j] = 'modifierExtension')))
// j--;
// StringBuilder b := new StringBuilder();
// boolean first := true;
// for (int k := i; k <= j; k++) begin
// if (k = j ) or ( !parts[k] = parts[k+1])) begin
// if (first)
// first := false;
// else
// b.append('.');
// b.append(parts[k]);
// end;
// end;
// return b.toString();
// end;
//


function TFHIRValidator.findElement(profile: TFHIRStructureDefinition; name: String): TFHIRElementDefinition;
var
  c: TFHIRElementDefinition;
begin
  result := nil;
  for c in profile.Snapshot.ElementList do
  begin
    if (c.path = name) then
    begin
      result := c;
      exit;
    end;
  end;
end;



procedure TFHIRValidator.validateContains(ctxt : TFHIRValidatorContext; path: String; child: TFHIRElementDefinition; context: TFHIRElementDefinition; resource, element: TFHIRMMElement; stack: TNodeStack; idRule: TResourceIdStatus);
var
  resourceName: String;
  profile: TFHIRStructureDefinition;
begin
  resourceName := element.Type_;
  profile := TFHIRStructureDefinition(ValContext.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/' + resourceName));
  ctxt.Owned.add(profile);
  if (rule(ctxt, IssueTypeINVALID, element.locStart, element.locEnd, stack.addToLiteralPath(resourceName), profile <> nil,
    'No profile found for contained resource of type "' + resourceName + '"')) then
    validateResource(ctxt, resource, element, profile, idRule, stack);
end;

function passesCodeWhitespaceRules(v: String): boolean;
var
  lastWasSpace: boolean;
  c: char;
begin
  if (v.trim() <> v) then
    exit(false)
  else
  begin
    lastWasSpace := true;
    for c in v do
    begin
      if (c = ' ') then
      begin
        if (lastWasSpace) then
        begin
          result := false;
          exit;
        end
        else
          lastWasSpace := true;
      end
      else if c.isWhitespace then
      begin
        result := false;
        exit;
      end
      else
        lastWasSpace := false;
    end;
  end;
  result := true;
end;

function yearIsValid(v: String): boolean;
var
  i: integer;
begin
  if (v = '') then
    result := false
  else
  begin
    i := StrToIntDef(v.substring(0, Integermin(4, v.length)), 0);
    result := (i >= 1800) and (i <= 2100);
  end;
end;

function hasTimeZone(fmt: String): boolean;
begin
  result := (fmt.length > 10) and (fmt.substring(10).contains('-') or fmt.substring(10).contains('+') or fmt.substring(10).contains('Z'));
end;

function hasTime(fmt: String): boolean;
begin
  result := fmt.contains('T');
end;

procedure TFHIRValidator.checkPrimitive(ctxt : TFHIRValidatorContext; path: String; ty: String; context: TFHIRElementDefinition; e: TFHIRMMElement; profile : TFhirStructureDefinition);
var
  regex: TRegEx;
  xhtml : TFhirXHtmlNode;
  ns : String;
begin
  if (ty = 'boolean') then
    rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, (e.primitiveValue() ='true') or (e.primitiveValue() = 'false'), 'boolean values must be "true" or "false"');
  if (ty = 'uri') then
  begin
    rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, not e.primitiveValue.startsWith('oid:'), 'URI values cannot start with oid:');
    rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, not e.primitiveValue.startsWith('uuid:'), 'URI values cannot start with uuid:');
    rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, e.primitiveValue = e.primitiveValue.trim(), 'URI values cannot have leading or trailing whitespace');
  end;
  if (not SameText(ty, 'string')) and (e.hasPrimitiveValue) then
  begin
    if (rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, e.primitiveValue.length > 0, '@value cannot be empty')) then
      warning(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, e.primitiveValue.trim() = e.primitiveValue, 'value should not start or finish with whitespace');
  end;
  if (ty = 'dateTime') then
  begin
    rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, yearIsValid(e.primitiveValue), 'The value "' + e.primitiveValue + '" does not have a valid year');
    regex := TRegEx.Create('-?[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?)?)?)?', [roCompiled]);
    rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, regex.isMatch(e.primitiveValue), 'Not a valid date time');
    rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, not hasTime(e.primitiveValue) or hasTimeZone(e.primitiveValue),
      'if a date has a time, it must have a timezone');
  end;
  if (ty = 'instant') then
  begin
    regex := TRegEx.Create('-?[0-9]{4}-(0[1-9]|1[0-2])-(0[0-9]|[1-2][0-9]|3[0-1])T([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))',[roCompiled]);
    rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, regex.isMatch(e.primitiveValue), 'The instant "' + e.primitiveValue + '" is not valid (by regex)');
    rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, yearIsValid(e.primitiveValue), 'The value "' + e.primitiveValue + '" does not have a valid year');
  end;

  if (ty = 'code') then
  begin
    // Technically, a code is restricted to string which has at least one character and no leading or trailing whitespace, and where there is no whitespace other than single spaces in the contents
    rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, passesCodeWhitespaceRules(e.primitiveValue),
      'The code "' + e.primitiveValue + '" is not valid (whitespace rules)');
  end;

  if (context.Binding <> nil) then
  begin
    checkPrimitiveBinding(ctxt, path, ty, context, e, profile);
  end;

  if (ty = 'xhtml') then
  begin
    xhtml := e.Xhtml;
    if (xhtml <> nil) then
    begin
     // if it is null, this is an error already noted in the parsers
      // check that the namespace is there and correct.
      ns := xhtml.NsDecl;
      rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, XHTML_NS = ns, 'Wrong namespace on the XHTML ("'+ns+'")');
      // check that inner namespaces are all correct
      checkInnerNS(ctxt, e, path, xhtml.ChildNodes);
      rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, 'div' = xhtml.Name, 'Wrong name on the XHTML ("'+xhtml.name+'") - must start with "div"');
      // check that no illegal elements and attributes have been used
      checkInnerNames(ctxt, e, path, xhtml.ChildNodes);
    end;
  end;
end;

procedure TFHIRValidator.checkInnerNames(ctxt : TFHIRValidatorContext; e: TFHIRMMElement; path: String; list : TFhirXHtmlNodeList);
var
  node : TFhirXHtmlNode;
  attr : TFHIRAttribute;
begin
  for node in list do
  begin
    if (node.NodeType = fhntElement) then
    begin
      rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path,  TFHIRXhtmlParser.elementIsOk(xppDrop, [], node.Name), 'Illegal element name in the XHTML("'+node.name+'")');
      if node.HasAttributes then
        for attr in node.Attributes do
          rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path,  TFHIRXhtmlParser.attributeIsOk(xppDrop, [], node.Name, attr.Name, attr.Value), 'Illegal attribute name in the XHTML("'+node.name+'.@'+attr.name+'")');
      checkInnerNS(ctxt, e, path, node.ChildNodes);
end;
  end;
end;

procedure TFHIRValidator.checkInnerNS(ctxt : TFHIRValidatorContext; e: TFHIRMMElement; path: String; list : TFhirXHtmlNodeList);
var
  node : TFhirXHtmlNode;
  ns : String;
begin
  for node in list do
  begin
    if (node.NodeType = fhntElement) then
    begin
      ns := node.NsDecl;
      rule(ctxt, IssueTypeINVALID, e.locStart, e.locEnd, path, (ns = '') or (XHTML_NS = ns), 'Wrong namespace on the XHTML ("'+ns+'")');
      checkInnerNS(ctxt, e, path, node.ChildNodes);
    end;
  end;
end;


// note that we don"t check the type here; it could be string, uri or code.
procedure TFHIRValidator.checkPrimitiveBinding(ctxt : TFHIRValidatorContext; path: String; ty: String; context: TFHIRElementDefinition; element: TFHIRMMElement; profile : TFhirStructureDefinition);
var
  value: String;
  Binding: TFhirElementDefinitionBinding;
  vs: TFHIRValueSet;
  res: TValidationResult;
begin
  if (not element.hasPrimitiveValue) then
    exit;
  value := element.primitiveValue;

  // System.out.println('check '+value+' in '+path);

  // firstly, resolve the value set
  Binding := context.Binding;
  if (Binding.ValueSet <> nil) and (Binding.ValueSet is TFHIRReference) then
  begin
    vs := resolveBindingReference(ctxt, profile, Binding.ValueSet);
    if (warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, vs <> nil, 'ValueSet ' + describeReference(Binding.ValueSet) + ' not found')) then
    begin
      res := ValContext.validateCode(SYSTEM_NOT_APPLICABLE, value, '', vs);
      try
        if (not res.isOk()) then
        begin
          if (Binding.Strength = BindingStrengthREQUIRED) then
            warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'The value provided ('+value+') is not in the value set ' +
              describeReference(Binding.ValueSet) + ' (' + vs.url + ', and a code is required from this value set')
          else if (Binding.Strength = BindingStrengthEXTENSIBLE) then
            warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'The value provided ('+value+') is not in the value set ' +
              describeReference(Binding.ValueSet) + ' (' + vs.url + ', and a code should come from this value set unless it has no suitable code')
          else if (Binding.Strength = BindingStrengthPREFERRED) then
            hint(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'The value provided ('+value+') is not in the value set ' + describeReference(Binding.ValueSet)
              + ' (' + vs.url + ', and a code is recommended to come from this value set');
        end;
      finally
        res.free;
      end;
    end;
  end
  else
    hint(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, ty <> 'code', 'Binding has no source, so can''t be checked');
end;

function isValidFHIRUrn(uri: String): boolean;
begin
  result := (uri = 'urn:x-fhir:uk:id:nhs-number');
end;

function isAbsolute(uri: String): boolean;
begin
  result := (uri = '') or uri.startsWith('http:') or uri.startsWith('https:') or uri.startsWith('urn:uuid:') or uri.startsWith('urn:oid:') or uri.startsWith('urn:ietf:') or
    uri.startsWith('urn:iso:') or uri.startsWith('urn:std:') or isValidFHIRUrn(uri);
end;

procedure TFHIRValidator.checkIdentifier(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; context: TFHIRElementDefinition);
var
  System: String;
begin
  System := element.getNamedChildValue('system');
  rule(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, isAbsolute(System), 'Identifier.system must be an absolute reference, not a local reference ('+system+')');
end;

procedure TFHIRValidator.checkQuantity(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; context: TFHIRElementDefinition);
var
  code: String;
  System, version: String;
  units: String;
begin
  code := element.getNamedChildValue('code');
  System := element.getNamedChildValue('system');
  version := element.getNamedChildValue('version');
  units := element.getNamedChildValue('units');

  if (System <> '') and (code <> '') then
    checkCode(ctxt, element, path, code, System, version, units);
end;

procedure TFHIRValidator.checkCoding(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; profile: TFHIRStructureDefinition;
  context: TFHIRElementDefinition; inCodeableConcept: boolean);
var
  code: String;
  System, version: String;
  display: String;
  Binding: TFhirElementDefinitionBinding;
  vs: TFHIRValueSet;
  c: TFHIRCoding;
  res: TValidationResult;
begin
  code := element.getNamedChildValue('code');
  System := element.getNamedChildValue('system');
  version := element.getNamedChildValue('version');
  display := element.getNamedChildValue('display');

  rule(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, isAbsolute(System), 'Coding.system must be an absolute reference, not a local reference ('+system+')');

  if (System <> '') and (code <> '') then
  begin
    if (checkCode(ctxt, element, path, code, System, version, display)) then
      if (context <> nil) and (context.Binding <> nil) then
      begin
        Binding := context.Binding;
        if (warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, Binding <> nil, 'Binding for ' + path + ' missing')) then
        begin
          if (Binding.ValueSet <> nil) then
          begin
            vs := resolveBindingReference(ctxt, profile, Binding.ValueSet);
            if (warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, vs <> nil, 'ValueSet ' + describeReference(Binding.ValueSet) + ' not found')) then
            begin
              try
                c := readAsCoding(element);
                try
                  res := ValContext.validateCode(c, vs);
                  try
                    if (not res.isOk()) then
                      if (Binding.Strength = BindingStrengthREQUIRED) then
                        warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'The value provided ('+c.system+'::'+c.code+') is not in the value set ' +
                          describeReference(Binding.ValueSet) + ' (' + vs.url + ', and a code is required from this value set')
                      else if (Binding.Strength = BindingStrengthEXTENSIBLE) then
                        warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'The value provided ('+c.system+'::'+c.code+') is not in the value set ' +
                          describeReference(Binding.ValueSet) + ' (' + vs.url + ', and a code should come from this value set unless it has no suitable code')
                      else if (Binding.Strength = BindingStrengthPREFERRED) then
                        hint(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'The value provided ('+c.system+'::'+c.code+') is not in the value set ' +
                          describeReference(Binding.ValueSet) + ' (' + vs.url + ', and a code is recommended to come from this value set');
                  finally
                    res.free;
                  end;
                finally
                  c.Free;
                end;
              except
                on e: Exception do
                  warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'Error ' + e.message + ' validating Coding');
              end;
            end
            else if (Binding.ValueSet <> nil) then
              hint(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'Binding by URI TFHIRReference cannot be checked')
            else if not inCodeableConcept then
              hint(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'Binding has no source, so can''t be checked');
          end;
        end;
      end;
  end;
end;

function TFHIRValidator.resolveBindingReference(ctxt : TFHIRValidatorContext; context : TFHIRDomainResource; reference: TFHIRType): TFHIRValueSet;
var
  s : String;
  c : TFHIRResource;
begin
  if (reference is TFHIRUri) then
    result := TFHIRValueSet(ValContext.fetchResource(frtValueSet, TFHIRUri(reference).value))
  else if (reference is TFHIRReference) then
  begin
    s := TFHIRReference(reference).reference;
    if s.StartsWith('#') then
    begin
      for c in context.containedList do
        if (c.id = s.Substring(1)) and (c is TFHIRValueSet) then
          exit(TFHIRValueSet(c).link);
      result := nil;
    end
    else
    begin
      result := TFHIRValueSet(ValContext.fetchResource(frtValueSet, s));
      ctxt.Owned.add(result);
    end;
  end
  else
    result := nil;
end;

function readAsCodeableConcept(element: TFHIRMMElement): TFHIRCodeableConcept;
var
  cc: TFHIRCodeableConcept;
  list: TFslList<TFHIRMMElement>;
  item: TFHIRMMElement;
begin
  cc := TFHIRCodeableConcept.Create;
  list := TFslList<TFHIRMMElement>.Create;
  try
    element.getNamedChildren('coding', list);
    for item in list do
      cc.CodingList.Add(readAsCoding(item));
    cc.text := element.getNamedChildValue('text');
    result := cc.Link;
  finally
    cc.Free;
    list.Free;
  end;
End;

procedure TFHIRValidator.checkCodeableConcept(ctxt : TFHIRValidatorContext; path: String; element: TFHIRMMElement; profile: TFHIRStructureDefinition;
  context: TFHIRElementDefinition);
var
  Binding: TFhirElementDefinitionBinding;
  vs: TFHIRValueSet;
  res: TValidationResult;
  cc: TFHIRCodeableConcept;
begin
  if (context <> nil) and (context.Binding <> nil) then
  begin
    Binding := context.Binding;
    if (warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, Binding <> nil, 'Binding for ' + path + ' missing (cc)')) then
    begin
      if (Binding.ValueSet <> nil) and (Binding.ValueSet is TFHIRReference) then
      begin
        vs := resolveBindingReference(ctxt, profile, Binding.ValueSet);
        if (warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, vs <> nil, 'ValueSet ' + describeReference(Binding.ValueSet) + ' not found')) then
        begin
          try
            cc := readAsCodeableConcept(element);
            try
              if (cc.CodingList.IsEmpty) then
              begin
                if (Binding.Strength = BindingStrengthREQUIRED) then
                  rule(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'No code provided, and a code is required from the value set ' +
                    describeReference(Binding.ValueSet) + ' (' + vs.url)
                else if (Binding.Strength = BindingStrengthEXTENSIBLE) then
                  warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'No code provided, and a code should be provided from the value set ' +
                    describeReference(Binding.ValueSet) + ' (' + vs.url);
              end
              else
              begin
                res := ValContext.validateCode(cc, vs);
                try
                  if (not res.isOk) then
                  begin
                    if (Binding.Strength = BindingStrengthREQUIRED) then
                      rule(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'None of the codes provided are in the value set ' +
                        describeReference(Binding.ValueSet) + ' (' + vs.url + ', and a code from this value set is required')
                    else if (Binding.Strength = BindingStrengthEXTENSIBLE) then
                      warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'None of the codes provided are in the value set ' +
                        describeReference(Binding.ValueSet) + ' (' + vs.url + ', and a code should come from this value set unless it has no suitable code')
                    else if (Binding.Strength = BindingStrengthPREFERRED) then
                      hint(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'None of the codes provided are in the value set ' +
                        describeReference(Binding.ValueSet) + ' (' + vs.url + ', and a code is recommended to come from this value set');
                  end;
                finally
                  res.free;
                end;
              end;
            finally
              cc.free;
            end;
          except
            on e: Exception do
              warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'Error ' + e.message + ' validating CodeableConcept');
          end;
        end;
      end
      else if (Binding.ValueSet <> nil) then
        hint(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'Binding by URI rReference cannot be checked')
      else
        hint(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, false, 'Binding has no source, so can''t be checked');
    end
  end;
end;

function TFHIRValidator.checkCode(ctxt : TFHIRValidatorContext; element: TFHIRMMElement; path: String; code, System, version, display: String): boolean;
var
  s: TValidationResult;
begin
  result := true;
  if (ValContext.supportsSystem(System, version)) then
  begin
    s := ValContext.validateCode(System, version, code, display);
    try
      if (s = nil) or (s.isOk()) then
        result := true
      else if (s.severity = isInformation) then
        hint(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, s = nil, s.message)
      else if (s.severity = isWarning) then
        warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, s = nil, s.message)
      else
        result := rule(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, s = nil, s.message);
    finally
      s.Free;
    end;
  end
  // else if (system.startsWith('http://hl7.org/fhir')) then
  // begin
  // if (system = 'http://hl7.org/fhir/sid/icd-10') then
  // result := true // else don"t check ICD-10 (for now)
  // else
  // begin
  // vs := ValContext.fetchCodeSystem(system);
  // if (vs <> nil) then
  // check vlaue set uri hasn't been used directly
  // if (warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, vs <> nil, 'Unknown Code System '+system))
  // else begin
  // ConceptDefinitionComponent def := getCodeDefinition(vs, code);
  // if (warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, def <> nil, 'Unknown Code ('+system+'#'+code+')'))
  // return warning(ctxt, IssueTypeCODEINVALID, element.locStart, element.locEnd, path, display = nil ) or ( display = def.getDisplay()), 'Display should be "'+def.getDisplay()+'"');
  // end;
  // return false;
  // end;
  // end; else if (system.startsWith('http://loinc.org')) begin
  // return true;
  // end; else if (system.startsWith('http://unitsofmeasure.org')) begin
  // return true;
  // end;
  else
    result := true;
end;

// function getCodeDefinition(c TFHIRConceptDefinition; code : String ) : TFHIRConceptDefinition;
// begin
// if (code = c.Code))
// return c;
// for (ConceptDefinitionComponent g : c.getConcept()) begin
// ConceptDefinitionComponent r := getCodeDefinition(g, code);
// if (r <> nil)
// return r;
// end;
// return nil;
// end;
//
// private ConceptDefinitionComponent getCodeDefinition(vs : TFHIRValueSet, String code) begin
// for (ConceptDefinitionComponent c : vs.getCodeSystem().getConcept()) begin
// ConceptDefinitionComponent r := getCodeDefinition(c, code);
// if (r <> nil)
// return r;
// end;
// return nil;
// end;



// public class ProfileStructureIterator begin
//
// private profile : TFHIRStructureDefinition;
// private TFHIRElementDefinition elementDefn;
// private List<String> names := new ArrayList<String>();
// private Map<String, List<TFHIRElementDefinition>> children := new HashMap<String, List<TFHIRElementDefinition>>();
// private int cursor;
//
// public ProfileStructureIterator(profile : TFHIRStructureDefinition, TFHIRElementDefinition elementDefn) begin
// this.profile := profile;
// this.elementDefn := elementDefn;
// loadMap();
// cursor := -1;
// end;
//
// procedure TFHIRValidator.loadMap() begin
// int i := profile.SnapShot.getElement().indexOf(elementDefn) + 1;
// String lead := elementDefn.Path;
// while (i < profile.SnapShot.getElement().Count) begin
// String name := profile.Snapshot.ElementList[i).Path;
// if (name.length() <= lead.length())
// return; // cause we"ve got to the end of the possible matches
// String tail := name.substring(lead.length()+1);
// if (Utilities.isToken(tail) ) and ( name.substring(0, lead.length()) = lead)) begin
// List<TFHIRElementDefinition> list := children[tail);
// if (list = nil) begin
// list := new ArrayList<TFHIRElementDefinition>();
// names.add(tail);
// children.put(tail, list);
// end;
// list.add(profile.Snapshot.ElementList[i));
// end;
// i++;
// end;
// end;
//
// public boolean more() begin
// cursor++;
// return cursor < names.Count;
// end;
//
// public List<TFHIRElementDefinition> current() begin
// return children[name());
// end;
//
// public String name() begin
// return names[cursor);
// end;
//
// end;
//
// procedure TFHIRValidator.checkByProfile(errors : TFhirOperationOutcomeIssueList; path : String; focus : TFHIRMMElement, profile : TFHIRStructureDefinition, TFHIRElementDefinition elementDefn) ; begin
// // we have an element, and the structure that describes it.
// // we know that"s it"s valid against the underlying spec - is it valid against this one?
// // in the instance validator above, we assume that schema or schmeatron has taken care of cardinalities, but here, we have no such reliance.
// // so the walking algorithm is different: we"re going to walk the definitions
// String type;
// if (elementDefn.Path.endsWith('[x]')) begin
// String tail := elementDefn.Path.substring(elementDefn.Path.lastIndexOf('.')+1, elementDefn.Path.length()-3);
// type := focus.name.substring(tail.length());
// rule(ctxt, IssueTypeSTRUCTURE, focus.locStart, focus.locEnd, path, typeAllowed(type, elementDefn.Type_List), 'The type "'+type+'" is not allowed at this point (must be one of "'+typeSummary(elementDefn)+')');
// end; else begin
// if (elementDefn.Type_List.Count = 1) begin
// type := elementDefn.Type_List.Count = 0 ? nil : elementDefn.Type_List[0].Code;
// end; else
// type := nil;
// end;
// // constraints:
// for (ElementDefinitionConstraintComponent c : elementDefn.getConstraint())
// checkConstraint(ctxt, path, focus, c);
// if (elementDefn.Binding <> nil ) and ( type <> nil)
// checkBinding(ctxt, path, focus, profile, elementDefn, type);
//
// // type specific checking:
// if (type <> nil ) and ( typeIsPrimitive(type)) begin
// checkPrimitiveByProfile(ctxt, path, focus, elementDefn);
// end; else begin
// if (elementDefn.hasFixed())
// checkFixedValue(ctxt, path, focus, elementDefn.getFixed(), '');
//
// ProfileStructureIterator walker := new ProfileStructureIterator(profile, elementDefn);
// while (walker.more()) begin
// // collect all the slices for the path
// List<TFHIRElementDefinition> childset := walker.current();
// // collect all the elements that match it by name
// TFslList<TFHIRMMElement> children := TFslList<TFHIRMMElement>.create();
// focus.getNamedChildrenWithWildcard(walker.name(), children);
//
// if (children.Count = 0) begin
// // well, there"s no children - should there be?
// for (TFHIRElementDefinition defn : childset) begin
// if (not rule(ctxt, IssueTypeREQUIRED, focus.locStart, focus.locEnd, path, defn.getMin() = 0, 'Required Element "'+walker.name()+'" missing'))
// break; // no point complaining about missing ones after the first one
// end;
// end; else if (childset.Count = 1) begin
// // simple case: one possible definition, and one or more children.
// rule(ctxt, IssueTypeSTRUCTURE, focus.locStart, focus.locEnd, path, childset[0).Max = '*') ) or ( StrToInt(childset[0).Max) >= children.Count,
// 'Too many elements for "'+walker.name()+'"'); // todo: sort out structure
// for (TFHIRMMElement child : children) begin
// checkByProfile(ctxt, childset[0).Path, child, profile, childset[0));
// end;
// end; else begin
// // ok, this is the full case - we have a list of definitions, and a list of candidates for meeting those definitions.
// // we need to decide *if* that match a given definition
// end;
// end;
// end;
// end;
// procedure TFHIRValidator.checkBinding(errors : TFhirOperationOutcomeIssueList; path : String; focus : TFHIRMMElement, profile : TFHIRStructureDefinition, TFHIRElementDefinition elementDefn, String type) throws EOperationOutcome, Exception begin
// ElementDefinitionBindingComponent bc := elementDefn.Binding;
//
// if (bc <> nil ) and ( bc.ValueSet() <> nil ) and ( bc.ValueSet is TFHIRReference) begin
// String url := ((TFHIRReference) bc.ValueSet).getReference();
// vs : TFHIRValueSet := resolveValueSetReference(profile, (TFHIRReference) bc.ValueSet);
// if (vs = nil) begin
// rule(ctxt, IssueTypeSTRUCTURE, focus.locStart, focus.locEnd, path, false, 'Cannot check binding on type "'+type+'" as the value set "'+url+'" could not be located');
// end; else if (ty = 'code'))
// checkBindingCode(ctxt, path, focus, vs);
// else if (ty = 'Coding'))
// checkBindingCoding(ctxt, path, focus, vs);
// else if (ty = 'CodeableConcept'))
// checkBindingCodeableConcept(ctxt, path, focus, vs);
// else
// rule(ctxt, IssueTypeSTRUCTURE, focus.locStart, focus.locEnd, path, false, 'Cannot check binding on type "'+type+'"');
// end;
// end;
//
// private ValueSet resolveValueSetReference(profile : TFHIRStructureDefinition, TFHIRReference TFHIRReference) throws EOperationOutcome, Exception begin
// if (TFHIRReference.getReference().startsWith('#')) begin
// for (Resource r : profile.getContained()) begin
// if (r is ValueSet ) and ( r.getId() = TFHIRReference.getReference().substring(1)))
// return (ValueSet) r;
// end;
// return nil;
// end; else
// return resolveBindingReference(TFHIRReference);
//
// end;

// procedure TFHIRValidator.checkBindingCode(errors : TFhirOperationOutcomeIssueList; path : String; focus : TFHIRMMElement, vs : TFHIRValueSet) begin
// // rule(ctxt, 'exception', path, false, 'checkBindingCode not done yet');
// end;
//
// procedure TFHIRValidator.checkBindingCoding(errors : TFhirOperationOutcomeIssueList; path : String; focus : TFHIRMMElement, vs : TFHIRValueSet) begin
// // rule(ctxt, 'exception', path, false, 'checkBindingCoding not done yet');
// end;
//
// procedure TFHIRValidator.checkBindingCodeableConcept(errors : TFhirOperationOutcomeIssueList; path : String; focus : TFHIRMMElement, vs : TFHIRValueSet) begin
// // rule(ctxt, 'exception', path, false, 'checkBindingCodeableConcept not done yet');
// end;
//
// private String typeSummary(TFHIRElementDefinition elementDefn) begin
// StringBuilder b := new StringBuilder();
// for (TypeRefComponent t : elementDefn.Type_List) begin
// b.append('"'+t.Code);
// end;
// return b.toString().substring(1);
// end;
//
// private boolean typeAllowed(String t, List<TypeRefComponent> types) begin
// for (TypeRefComponent type : types) begin
// if (t = Utilities.capitalize(ty.Code)))
// return true;
// if (t = 'Resource') ) and ( Utilities.capitalize(ty.Code) = 'TFHIRReference'))
// return true;
// end;
// return false;
// end;
//
// procedure TFHIRValidator.checkConstraint(errors : TFhirOperationOutcomeIssueList; path : String; focus : TFHIRMMElement, ElementDefinitionConstraintComponent c) ; begin

// try
// begin
// XPathFactory xpf := new net.sf.saxon.xpath.XPathFactoryImpl();
// NamespaceContext context := new NamespaceContextMap('f', 'http://hl7.org/fhir', 'h', 'http://www.w3.org/1999/xhtml');
//
// XPath xpath := xpf.newXPath();
// xpath.setNamespaceContext(context);
// ok : boolean := (Boolean) xpath.evaluate(c.getXpath(), focus, XPathConstants.BOOLEAN);
// if (ok = nil ) or ( not ok) begin
// if (c.getSeverity() = ConstraintSeverity.warning)
// warning(ctxt, 'invariant', path, false, c.getHuman());
// else
// rule(ctxt, 'invariant', path, false, c.getHuman());
// end;
// end;
// catch (XPathExpressionException e) begin
// rule(ctxt, 'invariant', path, false, 'error executing invariant: '+e.Message);
// end;
// end;
//
// procedure TFHIRValidator.checkPrimitiveByProfile(errors : TFhirOperationOutcomeIssueList; path : String; focus : TFHIRMMElement, TFHIRElementDefinition elementDefn) begin
// // two things to check - length, and fixed value
// String value := focus.primitiveValue;
// if (elementDefn.hasMaxLengthElement()) begin
// rule(ctxt, IssueTypeTOOLONG, focus.locStart, focus.locEnd, path, value.length() <= elementDefn.getMaxLength(), 'The value "'+value+'" exceeds the allow length limit of '+inttostr(elementDefn.getMaxLength()));
// end;
// if (elementDefn.hasFixed()) begin
// checkFixedValue(ctxt, path, focus, elementDefn.getFixed(), '');
// end;
// end;
//

procedure TFHIRValidator.checkFixedValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFhirElement; propName: String);
var
  value: String;
  extensions: TFslList<TFHIRMMElement>;
  e: TFhirExtension;
  ex: TFHIRMMElement;
begin
  if (fixed = nil) and (focus = nil) then
    exit; // this is all good

  if (fixed = nil) and (focus <> nil) then
    rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, false, 'Unexpected element ' + focus.name)
  else if (fixed <> nil) and (focus = nil) then
    rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, false, 'Mising element ' + propName)
  else
  begin
    value := focus.primitiveValue;
    if (fixed is TFHIRBoolean) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRBoolean(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIRBoolean(fixed)
        .StringValue + '"')
    else if (fixed is TFHIRInteger) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRInteger(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIRInteger(fixed)
        .StringValue + '"')
    else if (fixed is TFHIRDecimal) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRDecimal(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIRDecimal(fixed)
        .StringValue + '"')
    else if (fixed is TFHIRBase64Binary) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRBase64Binary(fixed).StringValue = value,
        'Value is "' + value + '" but must be "' + TFHIRBase64Binary(fixed).StringValue + '"')
    else if (fixed is TFHIRInstant) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRInstant(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIRInstant(fixed)
        .StringValue + '"')
    else if (fixed is TFhirString) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFhirString(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFhirString(fixed)
        .StringValue + '"')
    else if (fixed is TFHIRUri) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRUri(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIRUri(fixed)
        .StringValue + '"')
    else if (fixed is TFHIRDate) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRDate(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIRDate(fixed)
        .StringValue + '"')
    else if (fixed is TFHIRDateTime) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRDateTime(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIRDateTime(fixed)
        .StringValue + '"')
    else if (fixed is TFHIROid) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIROid(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIROid(fixed)
        .StringValue + '"')
    else if (fixed is TFHIRUuid) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRUuid(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIRUuid(fixed)
        .StringValue + '"')
    else if (fixed is TFHIRCode) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRCode(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIRCode(fixed)
        .StringValue + '"')
    else if (fixed is TFHIRId) then
      rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, TFHIRId(fixed).StringValue = value, 'Value is "' + value + '" but must be "' + TFHIRId(fixed).StringValue + '"')
    else if (fixed is TFHIRQuantity) then
      checkQuantityValue(ctxt, path, focus, TFHIRQuantity(fixed))
    else if (fixed is TFHIRAddress) then
      checkAddressValue(ctxt, path, focus, TFHIRAddress(fixed))
    else if (fixed is TFHIRContactPoint) then
      checkContactPointValue(ctxt, path, focus, TFHIRContactPoint(fixed))
    else if (fixed is TFHIRAttachment) then
      checkAttachmentValue(ctxt, path, focus, TFHIRAttachment(fixed))
    else if (fixed is TFHIRIdentifier) then
      checkIdentifierValue(ctxt, path, focus, TFHIRIdentifier(fixed))
    else if (fixed is TFHIRCoding) then
      checkCodingValue(ctxt, path, focus, TFHIRCoding(fixed))
    else if (fixed is TFHIRHumanName) then
      checkHumanNameValue(ctxt, path, focus, TFHIRHumanName(fixed))
    else if (fixed is TFHIRCodeableConcept) then
      checkCodeableConceptValue(ctxt, path, focus, TFHIRCodeableConcept(fixed))
    else if (fixed is TFHIRTiming) then
      checkTimingValue(ctxt, path, focus, TFHIRTiming(fixed))
    else if (fixed is TFHIRPeriod) then
      checkPeriodValue(ctxt, path, focus, TFHIRPeriod(fixed))
    else if (fixed is TFHIRRange) then
      checkRangeValue(ctxt, path, focus, TFHIRRange(fixed))
    else if (fixed is TFHIRRatio) then
      checkRatioValue(ctxt, path, focus, TFHIRRatio(fixed))
    else if (fixed is TFHIRSampledData) then
      checkSampledDataValue(ctxt, path, focus, TFHIRSampledData(fixed))
    else
      rule(ctxt, IssueTypeException, focus.locStart, focus.locEnd, path, false, 'Unhandled fixed value type ' + fixed.ClassName);
    extensions := TFslList<TFHIRMMElement>.Create();
    try
      focus.getNamedChildren('extension', extensions);
      if (fixed.extensionList.count = 0) then
      begin
        rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, extensions.count = 0, 'No extensions allowed');
      end
      else if (rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, extensions.count = fixed.extensionList.count,
        'Extensions count mismatch: expected ' + inttostr(fixed.extensionList.count) + ' but found ' + inttostr(extensions.count))) then
      begin
        for e in fixed.extensionList do
        begin
          ex := getExtensionByUrl(extensions, e.url);
          if (rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, ex <> nil, 'Extension count mismatch: unable to find extension: ' + e.url)) then
            checkFixedValue(ctxt, path, ex.getNamedChild('extension').getNamedChild('value'), e.value, 'extension.value');
        end;
      end;
    finally
      extensions.free;
    end;
  end;
end;

procedure TFHIRValidator.checkAddressValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRAddress);
var
  lines: TFslList<TFHIRMMElement>;
  i: integer;
begin
  checkFixedValue(ctxt, path + '.use', focus.getNamedChild('use'), fixed.UseElement, 'use');
  checkFixedValue(ctxt, path + '.text', focus.getNamedChild('text'), fixed.TextElement, 'text');
  checkFixedValue(ctxt, path + '.city', focus.getNamedChild('city'), fixed.CityElement, 'city');
  checkFixedValue(ctxt, path + '.state', focus.getNamedChild('state'), fixed.StateElement, 'state');
  checkFixedValue(ctxt, path + '.country', focus.getNamedChild('country'), fixed.CountryElement, 'country');
  checkFixedValue(ctxt, path + '.zip', focus.getNamedChild('zip'), fixed.PostalCodeElement, 'postalCode');

  lines := TFslList<TFHIRMMElement>.Create();
  try
    focus.getNamedChildren('line', lines);
    if (rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, lines.count = fixed.lineList.count, 'Expected ' + inttostr(fixed.lineList.count) + ' but found ' +
      inttostr(lines.count) + ' line elements')) then
    begin
      for i := 0 to lines.count - 1 do
        checkFixedValue(ctxt, path + '.coding', lines[i], fixed.lineList[i], 'coding');
    end;
  finally
    lines.free;
  end;
end;

procedure TFHIRValidator.checkContactPointValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRContactPoint);
begin
  checkFixedValue(ctxt, path + '.system', focus.getNamedChild('system'), fixed.SystemElement, 'system');
  checkFixedValue(ctxt, path + '.value', focus.getNamedChild('value'), fixed.ValueElement, 'value');
  checkFixedValue(ctxt, path + '.use', focus.getNamedChild('use'), fixed.UseElement, 'use');
  checkFixedValue(ctxt, path + '.period', focus.getNamedChild('period'), fixed.Period, 'period');
end;

procedure TFHIRValidator.checkAttachmentValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRAttachment);
begin
  checkFixedValue(ctxt, path + '.contentType', focus.getNamedChild('contentType'), fixed.ContentTypeElement, 'contentType');
  checkFixedValue(ctxt, path + '.language', focus.getNamedChild('language'), fixed.LanguageElement, 'language');
  checkFixedValue(ctxt, path + '.data', focus.getNamedChild('data'), fixed.DataElement, 'data');
  checkFixedValue(ctxt, path + '.url', focus.getNamedChild('url'), fixed.UrlElement, 'url');
  checkFixedValue(ctxt, path + '.size', focus.getNamedChild('size'), fixed.SizeElement, 'size');
  checkFixedValue(ctxt, path + '.hash', focus.getNamedChild('hash'), fixed.HashElement, 'hash');
  checkFixedValue(ctxt, path + '.title', focus.getNamedChild('title'), fixed.TitleElement, 'title');
end;

procedure TFHIRValidator.checkIdentifierValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRIdentifier);
begin
  checkFixedValue(ctxt, path + '.use', focus.getNamedChild('use'), fixed.UseElement, 'use');
  checkFixedValue(ctxt, path + '.label', focus.getNamedChild('type'), fixed.type_, 'type');
  checkFixedValue(ctxt, path + '.system', focus.getNamedChild('system'), fixed.SystemElement, 'system');
  checkFixedValue(ctxt, path + '.value', focus.getNamedChild('value'), fixed.ValueElement, 'value');
  checkFixedValue(ctxt, path + '.period', focus.getNamedChild('period'), fixed.Period, 'period');
  checkFixedValue(ctxt, path + '.assigner', focus.getNamedChild('assigner'), fixed.Assigner, 'assigner');
end;

procedure TFHIRValidator.checkInvariants(ctxt : TFHIRValidatorContext; path : String; profile: TFHIRStructureDefinition; ed: TFhirElementDefinition; typename, typeProfile : String; resource, element: TFHIRMMElement);
var
  inv : TFhirElementDefinitionConstraint;
  ok : boolean;
  msg : String;
  expr : TFHIRPathExpressionNode;
begin
  ok := false;
  for inv in ed.constraintList do
  begin
    if inv.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-expression') <> '' then
    begin
      expr := inv.Tag as TFHIRPathExpressionNode;
      if (expr = nil) then
      begin
        expr := FPathEngine.parse(inv.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-expression'));
        inv.Tag := expr;
      end;
      try
        ok := FPathEngine.evaluateToBoolean(nil, resource, element, expr);
        msg := '';
      except
        on e : Exception do
        begin
          ok := false;
          msg := e.message;
        end;
      end;
      if not ok then
        case inv.severity of
          ConstraintSeverityError: rule(ctxt, IssueTypeInvariant, element.LocStart, element.LocEnd, path, ok, inv.human+FPathEngine.UseLog+' ('+msg+') '+inv.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-expression'));
          ConstraintSeverityWarning: warning(ctxt, IssueTypeInvariant, element.LocStart, element.LocEnd, path, ok, inv.human+FPathEngine.UseLog+' ('+msg+') '+inv.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-expression'));
        end;
    end;
  end;
end;

procedure TFHIRValidator.checkCodingValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRCoding);
begin
  checkFixedValue(ctxt, path + '.system', focus.getNamedChild('system'), fixed.SystemElement, 'system');
  checkFixedValue(ctxt, path + '.code', focus.getNamedChild('code'), fixed.CodeElement, 'code');
  checkFixedValue(ctxt, path + '.display', focus.getNamedChild('display'), fixed.DisplayElement, 'display');
  checkFixedValue(ctxt, path + '.userSelected', focus.getNamedChild('userSelected'), fixed.UserSelectedElement, 'userSelected');
end;

procedure TFHIRValidator.checkHumanNameValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRHumanName);
var
  parts: TFslList<TFHIRMMElement>;
  i: integer;
begin
  checkFixedValue(ctxt, path + '.use', focus.getNamedChild('use'), fixed.UseElement, 'use');
  checkFixedValue(ctxt, path + '.text', focus.getNamedChild('text'), fixed.TextElement, 'text');
  checkFixedValue(ctxt, path + '.period', focus.getNamedChild('period'), fixed.Period, 'period');

  parts := TFslList<TFHIRMMElement>.Create();
  try
    focus.getNamedChildren('family', parts);
    if (rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, parts.count = fixed.familyList.count, 'Expected ' + inttostr(fixed.familyList.count) + ' but found ' +
      inttostr(parts.count) + ' family elements')) then
    begin
      for i := 0 to parts.count - 1 do
        checkFixedValue(ctxt, path + '.family', parts[i], fixed.familyList[i], 'family');
    end;
    focus.getNamedChildren('given', parts);
    if (rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, parts.count = fixed.GivenList.count, 'Expected ' + inttostr(fixed.GivenList.count) + ' but found ' +
      inttostr(parts.count) + ' given elements')) then
    begin
      for i := 0 to parts.count - 1 do
        checkFixedValue(ctxt, path + '.given', parts[i], fixed.GivenList[i], 'given');
    end;
    focus.getNamedChildren('prefix', parts);
    if (rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, parts.count = fixed.prefixList.count, 'Expected ' + inttostr(fixed.prefixList.count) + ' but found ' +
      inttostr(parts.count) + ' prefix elements')) then
    begin
      for i := 0 to parts.count - 1 do
        checkFixedValue(ctxt, path + '.prefix', parts[i], fixed.prefixList[i], 'prefix');
    end;
    focus.getNamedChildren('suffix', parts);
    if (rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, parts.count = fixed.suffixList.count, 'Expected ' + inttostr(fixed.suffixList.count) + ' but found ' +
      inttostr(parts.count) + ' suffix elements')) then
    begin
      for i := 0 to parts.count - 1 do
        checkFixedValue(ctxt, path + '.suffix', parts[i], fixed.suffixList[i], 'suffix');
    end;
  finally
    parts.free;
  end;
end;

procedure TFHIRValidator.checkCodeableConceptValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRCodeableConcept);
var
  codings: TFslList<TFHIRMMElement>;
  i: integer;
begin
  checkFixedValue(ctxt, path + '.text', focus.getNamedChild('text'), fixed.TextElement, 'text');
  codings := TFslList<TFHIRMMElement>.Create();
  try
    focus.getNamedChildren('coding', codings);
    if (rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, codings.count = fixed.CodingList.count, 'Expected ' + inttostr(fixed.CodingList.count) + ' but found ' +
      inttostr(codings.count) + ' coding elements')) then
    begin
      for i := 0 to codings.count - 1 do
        checkFixedValue(ctxt, path + '.coding', codings[i], fixed.CodingList[i], 'coding');
    end;
  finally
    codings.free;
  end;
end;

procedure TFHIRValidator.checkTimingValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRTiming);
var
  events: TFslList<TFHIRMMElement>;
  i: integer;
begin
  checkFixedValue(ctxt, path + '.repeat', focus.getNamedChild('repeat'), fixed.repeat_, 'value');

  events := TFslList<TFHIRMMElement>.Create();
  try
    focus.getNamedChildren('event', events);
    if (rule(ctxt, IssueTypeVALUE, focus.locStart, focus.locEnd, path, events.count = fixed.eventList.count, 'Expected ' + inttostr(fixed.eventList.count) + ' but found ' +
      inttostr(events.count) + ' event elements')) then
    begin
      for i := 0 to events.count - 1 do
        checkFixedValue(ctxt, path + '.event', events[i], fixed.eventList[i], 'event');
    end;
  finally
    events.free;
  end;
end;

procedure TFHIRValidator.checkPeriodValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRPeriod);
begin
  checkFixedValue(ctxt, path + '.start', focus.getNamedChild('start'), fixed.StartElement, 'start');
  checkFixedValue(ctxt, path + '.end', focus.getNamedChild('end'), fixed.End_Element, 'end');
end;

procedure TFHIRValidator.checkRangeValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRRange);
begin
  checkFixedValue(ctxt, path + '.low', focus.getNamedChild('low'), fixed.Low, 'low');
  checkFixedValue(ctxt, path + '.high', focus.getNamedChild('high'), fixed.High, 'high');
end;

procedure TFHIRValidator.checkRatioValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRRatio);
begin
  checkFixedValue(ctxt, path + '.numerator', focus.getNamedChild('numerator'), fixed.Numerator, 'numerator');
  checkFixedValue(ctxt, path + '.denominator', focus.getNamedChild('denominator'), fixed.Denominator, 'denominator');
end;

procedure TFHIRValidator.checkSampledDataValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRSampledData);
begin
  checkFixedValue(ctxt, path + '.origin', focus.getNamedChild('origin'), fixed.Origin, 'origin');
  checkFixedValue(ctxt, path + '.period', focus.getNamedChild('period'), fixed.PeriodElement, 'period');
  checkFixedValue(ctxt, path + '.factor', focus.getNamedChild('factor'), fixed.FactorElement, 'factor');
  checkFixedValue(ctxt, path + '.lowerLimit', focus.getNamedChild('lowerLimit'), fixed.LowerLimitElement, 'lowerLimit');
  checkFixedValue(ctxt, path + '.upperLimit', focus.getNamedChild('upperLimit'), fixed.UpperLimitElement, 'upperLimit');
  checkFixedValue(ctxt, path + '.dimensions', focus.getNamedChild('dimensions'), fixed.DimensionsElement, 'dimensions');
  checkFixedValue(ctxt, path + '.data', focus.getNamedChild('data'), fixed.DataElement, 'data');
end;

procedure TFHIRValidator.checkQuantityValue(ctxt : TFHIRValidatorContext; path: String; focus: TFHIRMMElement; fixed: TFHIRQuantity);
begin
  checkFixedValue(ctxt, path + '.value', focus.getNamedChild('value'), fixed.ValueElement, 'value');
  checkFixedValue(ctxt, path + '.comparator', focus.getNamedChild('comparator'), fixed.ComparatorElement, 'comparator');
  checkFixedValue(ctxt, path + '.units', focus.getNamedChild('unit'), fixed.Unit_Element, 'units');
  checkFixedValue(ctxt, path + '.system', focus.getNamedChild('system'), fixed.SystemElement, 'system');
  checkFixedValue(ctxt, path + '.code', focus.getNamedChild('code'), fixed.CodeElement, 'code');
end;

function TFHIRValidator.getExtensionByUrl(extensions: TFslList<TFHIRMMElement>; url: String): TFHIRMMElement;
var
  e: TFHIRMMElement;
begin
  result := nil;
  for e in extensions do
  begin
    if (url = e.getNamedChildValue('url')) then
    begin
      result := e;
    end;
  end;
end;

procedure TFHIRValidator.validate(ctxt : TFHIRValidatorContext; source: TFslBuffer; format: TFHIRFormat; profiles : TValidationProfileSet);
var
  p : TFHIRMMParserBase;
  element : TFHIRMMElement;
begin
  if (profiles = nil) then
    profiles := TValidationProfileSet.create
  else
    profiles.link;
  try
    p := TFHIRMMManager.makeParser(context, format);
    try
      p.setupValidation(fvpEVERYTHING, ctxt.Issues.Link);
      element := p.parse(source);
      try
        if (element <> nil) then
          validate(ctxt, element, profiles);
      finally
        element.Free;
      end;
    finally
      p.Free;
    end;
  finally
    profiles.free;
  end;
end;

function TFHIRValidator.describe(ctxt : TFHIRValidatorContext): TFHIROperationOutcomeW;
var
  o : TFhirOperationOutcomeIssueW;
  gen : TFHIRNarrativeGenerator;
begin
  result := context.Factory.wrapOperationOutcome(context.factory.makeResource('OperationOutcome'));
  try
    for o in ctxt.Issues do
      result.addIssue(o.Link, false);
    gen := TFHIRNarrativeGenerator.create(Context.Link);
    try
      gen.description := ctxt.OperationDescription;
      gen.generate(result.Resource);
    finally
      gen.Free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;


procedure TFHIRValidator.validate(ctxt: TFHIRValidatorContext; source: TFslBuffer; format: TFHIRFormat; profile: String);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create(profile);
  try
    validate(ctxt, source, format, profiles);
  finally
    profiles.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt: TFHIRValidatorContext; source: TFslBuffer; format: TFHIRFormat);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create;
  try
    validate(ctxt, source, format, profiles);
  finally
    profiles.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt: TFHIRValidatorContext; resource: TFhirResourceV);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create;
  try
    validate(ctxt, resource, profiles);
  finally
    profiles.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt: TFHIRValidatorContext; resource: TFhirResourceV; profiles: TValidationProfileSet);
var
  loader : TFHIRMMResourceLoader;
  e : TFHIRMMElement;
begin
  loader := TFHIRMMResourceLoader.create(ValContext.Link);
  try
    e := loader.parse(resource);
    try
      validate(ctxt, e, profiles);
    finally
      e.free;
    end;
  finally
    loader.free;
  end;
end;

procedure TFHIRValidator.validate(ctxt: TFHIRValidatorContext; resource: TFhirResourceV; profile: string);
var
  profiles : TValidationProfileSet;
begin
  profiles := TValidationProfileSet.create(profile);
  try
    validate(ctxt, resource, profiles);
  finally
    profiles.free;
  end;
end;

function TFHIRValidator.GetContext : TFHIRWorkerContext;
begin
  result := (inherited Context) as TFHIRWorkerContext;
end;

{ TChildIterator }

constructor TChildIterator.Create(path: String; element: TFHIRMMElement);
begin
  inherited create;
  parent := element;
  basePath := path;
  cursor := -1;
end;

destructor TChildIterator.Destroy;
begin
  parent.free;
  inherited;
end;

function TChildIterator.count: integer;
var
  na, nb : String;
begin
  if cursor = 0 then
    nb :=  '--'
  else
    nb := parent.Children[cursor-1].Name;
  if cursor >= parent.Children.count - 1 then
    na := '--'
  else
    na := parent.Children[cursor+1].Name;
  if (name = nb) or (name = na) then
    result := lastCount + 1
  else
    result := -1;
end;

function TChildIterator.element: TFHIRMMElement;
begin
  result := parent.Children[cursor];
end;

function TChildIterator.name: String;
begin
  result := element.Name;
end;

function TChildIterator.next: boolean;
var
  lastName : String;
begin
  if (cursor = -1) then
  begin
    inc(cursor);
    lastCount := 0;
  end
  else
  begin
    lastName := name;
    inc(cursor);
    if (cursor < parent.Children.count) and (name = lastName) then
      inc(lastCount)
    else
      lastCount := 0;
  end;
  result := cursor < parent.Children.count;
end;

function TChildIterator.path: String;
var
  i : integer;
  sfx : String;
begin
  i := count;
  if (i > -1) then
    sfx := '[' + inttostr(lastCount) + ']'
  else
    sfx := '';
  result := basePath + '.' + name + sfx;
end;


function TChildIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, parent.sizeInBytes);
  inc(result, (basePath.length * sizeof(char)) + 12);
end;

{ TValidationProfileSet }

constructor TValidationProfileSet.create;
begin
  inherited;
  FCanonical := TStringList.create;
  FDefinitions := TFHIRStructureDefinitionList.create;

end;

constructor TValidationProfileSet.create(profile: String);
begin
  Create;
  FCanonical.add(profile);
end;

constructor TValidationProfileSet.create(profile: TFHIRStructureDefinition);
begin
  Create;
  FDefinitions.add(profile.link);
end;

destructor TValidationProfileSet.Destroy;
begin
  FCanonical.Free;
  FDefinitions.Free;
  inherited;
end;


function TValidationProfileSet.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCanonical.sizeInBytes);
  inc(result, FDefinitions.sizeInBytes);
end;

function TFHIRValidator2.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FExtensionDomains.sizeInBytes);
  inc(result, FPathEngine.sizeInBytes);
end;

end.
