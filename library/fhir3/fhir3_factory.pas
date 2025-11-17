unit fhir3_factory;

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

// FHIR v3.3.0 generated 2018-05-15T06:38:00+10:00

uses
  SysUtils, Classes, System.NetEncoding,
  fsl_base, fsl_utilities, fsl_stream, fsl_http, fsl_npm, fsl_threads, fsl_i18n,
  fsl_ucum, fsl_web_stream,
  fhir_objects, fhir_parser, fhir_validator, fhir_narrative, fhir_factory, fhir_pathengine, fhir_xhtml, fhir_common,  fhir_elementmodel,
  fhir_client, fhir_client_threaded, fhir_uris;

type

  { TFHIRFactoryR3 }

  TFHIRFactoryR3 = class (TFHIRFactory)
  public
    function version : TFHIRVersion; override;
    function versionString : String; override;
    function versionName : String; override;
    function corePackage : String; override;
    function specUrl : String; override;
    function description : String; override;
    function resourceNames : TArray<String>; override;
    function resCategory(name: String) : TTokenCategory; override;
    function canonicalResources : TArray<String>; override;
    function URLs : TCommonURLs; override;
    function makeParser(worker : TFHIRWorkerContextV; format : TFHIRFormat; langList : THTTPLanguageList) : TFHIRParser; override;
    function makeComposer(worker : TFHIRWorkerContextV; format : TFHIRFormat; langList : THTTPLanguageList; style: TFHIROutputStyle) : TFHIRComposer; override;
    function makeValidator(worker : TFHIRWorkerContextV) : TFHIRValidatorV; override;
    function makeGenerator(worker : TFHIRWorkerContextV) : TFHIRNarrativeGeneratorBase; override;
    function makePathEngine(worker : TFHIRWorkerContextV; ucum : TUcumServiceInterface) : TFHIRPathEngineV; override;
    function makeElementModelManager : TFHIRBaseMMManager; override;
    function createFromProfile(worker : TFHIRWorkerContextV; profile : TFhirStructureDefinitionW) : TFHIRResourceV; override;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal; proxy : String) : TFhirClientV; overload; override;
    function makeClientThreaded(worker : TFHIRWorkerContextV; internal : TFhirClientV; event : TThreadManagementEvent) : TFhirClientV; overload; override;
    function makeClientInt(worker : TFHIRWorkerContextV; langList : THTTPLanguageList; comm : TFHIRClientCommunicator) : TFhirClientV; overload; override;
    function makeHealthcareCard : THealthcareCard; override;

    function makeProxy(packageId : String; pi : TNpmPackageResource; worker : TFHIRWorkerContextV; lock : TFslLock) : TFHIRResourceProxyV; override;
    function makeProxy(packageId : String; resource : TFHIRResourceV) : TFHIRResourceProxyV; override;

    function getXhtml(res : TFHIRResourceV) : TFHIRXhtmlNode; override;
    function resetXhtml(res : TFHIRResourceV) : TFHIRXhtmlNode; override;
    procedure clearXhtml(res : TFHIRResourceV); override;
    procedure setXhtml(res : TFHIRResourceV; x : TFHIRXhtmlNode); override;
    function getContained(r : TFHIRResourceV) : TFslList<TFHIRResourceV>; override;
    function describe(r : TFHIRResourceV) : String; override;
    procedure markWithTag(r : TFHIRResourceV; systemUri, code, display : String); override;

    procedure checkNoModifiers(res : TFHIRObject; method, param : string; allowed : TArray<String> = nil); override;
    function buildOperationOutcome(langList : THTTPLanguageList; e : Exception; issueCode : TFhirIssueType = itNull) : TFhirResourceV; overload; override;
    Function buildOperationOutcome(langList : THTTPLanguageList; message : String; issueCode : TFhirIssueType = itNull) : TFhirResourceV; overload; override;
    function buildOperationOutcome(i18n : TI18nSupport; langList : THTTPLanguageList; exception : EFHIROperationException) : TFhirResourceV; overload; override;

    function makeByName(const name : String) : TFHIRObject; override;
    function makeBoolean(b : boolean): TFHIRObject; override;
    function makeCode(s : string) : TFHIRObject; override;
    function makeString(s : string) : TFHIRObject; override;
    function makeUri(s : string) : TFHIRObject; override;
    function makeInteger(s : string) : TFHIRObject; override;
    function makeDecimal(s : string) : TFHIRObject; override;
    function makeBase64Binary(s : string) : TFHIRObject; override;
    function makeParameters : TFHIRParametersW; override;
    function makeDate(value : TFslDateTime) : TFHIRObject; override;
    function makeDateTime(value : TFslDateTime) : TFHIRObject; override;
    function makeDuration(dt : TDateTime) : TFHIRObject; override;
    function wrapPrimitive(p : TFHIRObject) : TFHIRPrimitiveW; override;
    function wrapCapabilityStatement(r : TFHIRResourceV) : TFHIRCapabilityStatementW; override;
    function wrapStructureDefinition(r : TFHIRResourceV) : TFhirStructureDefinitionW; override;
    function wrapValueSet(r : TFHIRResourceV) : TFhirValueSetW; override;
    function wrapCodeSystem(r : TFHIRResourceV) : TFhirCodeSystemW; override;
    function wrapExtension(o : TFHIRObject) : TFhirExtensionW; override;
    function wrapCoding(o : TFHIRObject) : TFhirCodingW; override;
    function wrapIdentifier(o : TFHIRObject) : TFhirIdentifierW; override;
    function wrapOperationOutcome(r : TFHIRResourceV) : TFhirOperationOutcomeW; override;
    function wrapBundle(r : TFHIRResourceV) : TFhirBundleW; override;
    function wrapConceptMap(r : TFHIRResourceV) : TFhirConceptMapW; override;
    function wrapParams(r : TFHIRResourceV) : TFHIRParametersW; override;
    function wrapAuditEvent(r : TFHIRResourceV) : TFhirAuditEventW; override;
    function wrapBinary(r : TFHIRResourceV) : TFhirBinaryW; override;
    function makeBinary(content : TBytes; contentType : String) : TFHIRResourceV; override;
    function wrapSubscription(r : TFHIRResourceV) : TFhirSubscriptionW; override;
    function wrapSubscriptionTopic(r : TFHIRResourceV) : TFhirSubscriptionTopicW; override;
    function wrapMeta(r : TFHIRResourceV) : TFhirMetaW; overload; override;
    function wrapMeta(r : TFHIRObject) : TFhirMetaW; overload; override;
    function wrapObservation(r : TFHIRResourceV) : TFhirObservationW; override;
    function wrapQuantity(r : TFHIRObject) : TFhirQuantityW; override;
    function wrapPeriod(r : TFHIRObject) : TFhirPeriodW; override;
    function wrapAttachment(r : TFHIRObject) : TFHIRAttachmentW; override;
    function makeOpReqLookup : TFHIRLookupOpRequestW; override;
    function makeOpRespLookup : TFHIRLookupOpResponseW; override;
    function makeOpReqSubsumes : TFHIRSubsumesOpRequestW; override;
    function makeOpRespSubsumes : TFHIRSubsumesOpResponseW; override;
    function wrapCodeableConcept(o : TFHIRObject) : TFhirCodeableConceptW; override;
    function wrapGroup(r : TFHIRResourceV) : TFhirGroupW; override;
    function wrapPatient(r : TFHIRResourceV) : TFhirPatientW; override;
    function wrapEncounter(r : TFHIRResourceV) : TFhirEncounterW; override;
    function makeIssue(level : TIssueSeverity; issue: TFhirIssueType; location, message: String) : TFhirOperationOutcomeIssueW; override;
    function wrapBundleEntry(o : TFHIRObject) : TFhirBundleEntryW; override;
    function wrapNamingSystem(o : TFHIRResourceV) : TFHIRNamingSystemW; override;
    function wrapStructureMap(o : TFHIRResourceV) : TFHIRStructureMapW; override;
    function wrapEventDefinition(o : TFHIRResourceV) : TFHIREventDefinitionW; override;
    function wrapConsent(o : TFHIRResourceV) : TFHIRConsentW; override;
    function wrapTestScript(o : TFHIRResourceV) : TFHIRTestScriptW; override;
    function wrapProvenance(o : TFHIRResourceV) : TFHIRProvenanceW; override;
    function makeParamsFromForm(s : TStream) : TFHIRResourceV; override;
    function makeDtFromForm(part : TMimePart; langList : THTTPLanguageList; name : String; type_ : string) : TFHIRXVersionElementWrapper; override;
    function makeCoding(system, version, code, display : String) : TFHIRObject; override;
    function makeCodeableConcept(coding : TFHIRCodingW) : TFHIRObject; override;
    function makeTerminologyCapablities : TFhirTerminologyCapabilitiesW; override;
    function makeValueSetContains : TFhirValueSetExpansionContainsW; override;
    function makeBundle(list : TFslList<TFHIRResourceV>) : TFHIRBundleW; override;
    function wrapImmunization(o : TFHIRResourceV) : TFhirImmunizationW; override;
  end;
  TFHIRFactoryX = TFHIRFactoryR3;

implementation

uses
  fhir_client_http,
  fhir3_types, fhir3_resources, fhir3_parser, fhir3_context, fhir3_validator, fhir3_profiles, fhir3_operations, fhir3_elementmodel,
  fhir3_narrative, fhir3_pathengine, fhir3_constants, fhir3_client, fhir3_common, fhir3_utilities, fhir3_authmap, fhir3_resources_base;

{ TFHIRFactoryR3 }

function TFHIRFactoryR3.buildOperationOutcome(langList : THTTPLanguageList; message: String; issueCode: TFhirIssueType): TFhirResourceV;
begin
  result := fhir3_utilities.BuildOperationOutcome(langList, message, ExceptionTypeTranslations[issueCode]);
end;

function TFHIRFactoryR3.buildOperationOutcome(i18n: TI18nSupport; langList: THTTPLanguageList; exception: EFHIROperationException): TFhirResourceV;
var
  op : TFHIROperationOutcome;
  iss : TFHIROperationOutcomeIssue;
begin
  op := TFHIROperationOutcome.create;
  try
    iss := TFHIROperationOutcomeIssue.create;
    try
      iss.severity := ISSUE_SEVERITY_MAP2[exception.level];
      iss.code := ExceptionTypeTranslations[exception.Cause];
      iss.expressionList.Add(exception.Path);
      iss.details := TFHIRCodeableConcept.create;
      iss.details.addCoding('http://hl7.org/fhir/tools/CodeSystem/tx-issue-type', '', CODES_TOpIssueCode[exception.Code], '');
      if (exception.MsgId <> '') then
        iss.details.text := i18n.translate(exception.msgId, langlist, exception.Params)
      else
        iss.details.text := exception.message;
      op.issueList.add(iss.link);
    finally
      iss.free;
    end;
    result := op.link;
  finally
    op.free;
  end;
end;

function TFHIRFactoryR3.buildOperationOutcome(langList : THTTPLanguageList; e: Exception; issueCode: TFhirIssueType): TFhirResourceV;
begin
  result := fhir3_utilities.BuildOperationOutcome(langList, e, ExceptionTypeTranslations[issueCode]);
end;

function TFHIRFactoryR3.canonicalResources: TArray<String>;
var
  i : integer;
  a : TFhirResourceType;
begin
  i := 0;
  for a in ALL_RESOURCE_TYPES do
    if a in CANONICAL_URL_RESOURCE_TYPES then
      inc(i);
  setLength(result, i);
  i := 0;
  for a in ALL_RESOURCE_TYPES do
    if a in CANONICAL_URL_RESOURCE_TYPES then
    begin
      result[i] := CODES_TFhirResourceType[a];
      inc(i);
    end;
end;

procedure TFHIRFactoryR3.checkNoModifiers(res: TFHIRObject; method, param: string; allowed : TArray<String> = nil);
begin
  if res is TFHIRDomainResource then
    TFHIRDomainResource(res).checkNoModifiers(method, param)
  else if res is TFHIRBackboneElement then
    TFHIRBackboneElement(res).checkNoModifiers(method, param)
  else if res is TFHIRXVersionElementWrapper then 
    checkNoModifiers((res as TFHIRXVersionElementWrapper).Element, method, param, allowed) 
  else if res is TFHIRXVersionResourceWrapper then 
    checkNoModifiers((res as TFHIRXVersionResourceWrapper).Resource, method, param, allowed) 
end;

function TFHIRFactoryR3.corePackage: String;
begin
  result := 'hl7.fhir.r3.core';
end;

function TFHIRFactoryR3.createFromProfile(worker: TFHIRWorkerContextV; profile: TFhirStructureDefinitionW): TFHIRResourceV;
var
  pu : TProfileUtilities;
begin
  pu := TProfileUtilities.Create(worker.Link as TFHIRWorkerContext, nil);
  try
    result := pu.populateByProfile(profile.Resource as TFhirStructureDefinition);
  finally
    pu.free;
  end;
end;

function TFHIRFactoryR3.describe(r: TFHIRResourceV): String;
begin
  result := describeResource(r as TFHIRResource);
end;

function TFHIRFactoryR3.description: String;
begin
  result := 'R3 ('+FHIR_GENERATED_VERSION+')';
end;

function TFHIRFactoryR3.getContained(r: TFHIRResourceV): TFslList<TFHIRResourceV>;
var
  res : TFHIRResource;
begin
  result := TFslList<TFHIRResourceV>.Create;
  if (r is TFHIRDomainResource) then
  begin
    for res in (r as TFHIRDomainResource).containedList do
      result.add(r.link);
  end;
end;

function TFHIRFactoryR3.getXhtml(res: TFHIRResourceV): TFHIRXhtmlNode;
var
  r : TFHIRDomainResource;
begin
  if res = nil then
    exit(nil);
  if not (res is TFHIRDomainResource) then
    exit(nil);
  r := res as TFHIRDomainResource;
  if (r.text = nil) then
    result := nil
  else
    result := r.text.div_;
end;

function TFHIRFactoryR3.makeClient(worker: TFHIRWorkerContextV; url: String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout: cardinal; proxy: String): TFhirClientV;
var
  http : TFHIRHTTPCommunicator;
begin
  http := TFHIRHTTPCommunicator.Create(url);
  try
    if kind = fctCrossPlatform then
      http.UseIndy := true;
    http.timeout := timeout;
    http.proxy := proxy;
    result := TFhirClient3.Create(worker, nil, http.link);
    try
      result.format := fmt;
      result.link;
    finally
      result.free;
    end;
  finally
    http.free;
  end;
end;

function TFHIRFactoryR3.makeClientInt(worker: TFHIRWorkerContextV; langList : THTTPLanguageList; comm: TFHIRClientCommunicator): TFhirClientV;
begin
  result := TFhirClient3.Create(worker, nil, comm);
end;

function TFHIRFactoryR3.makeClientThreaded(worker: TFHIRWorkerContextV; internal: TFhirClientV; event: TThreadManagementEvent): TFhirClientV;
var
  c : TFhirThreadedCommunicator;
begin
  c := TFhirThreadedCommunicator.Create(internal, event);
  try
    result := TFhirClient3.Create(worker, nil, c.link);
    try
      result.format := internal.format;
      result.link;
    finally
      result.free;
    end;
  finally
    c.free;
  end;
end;

function TFHIRFactoryR3.makeCode(s: string): TFHIRObject;
begin
  result := TFhirCode.Create(s);
end;

function TFHIRFactoryR3.makeCoding(system, version, code, display: String): TFHIRObject;
begin
  result := TFHIRCoding.Create(system, code);
  if version <> '' then
    TFHIRCoding(result).version := version;
  if display <> '' then
    TFHIRCoding(result).display := display;
end;

function TFHIRFactoryR3.makeCodeableConcept(coding: TFHIRCodingW): TFHIRObject;
begin
  result := TFHIRCodeableConcept.Create;
  if (coding <> nil) then
    TFHIRCodeableConcept(result).codingList.add(coding.element.link);
end;

function TFHIRFactoryR3.makeComposer(worker: TFHIRWorkerContextV; format: TFHIRFormat; langList : THTTPLanguageList; style: TFHIROutputStyle): TFHIRComposer;
begin
  result := TFHIRParsers3.composer(worker.link as TFHIRWorkerContext, format, langList.link, style);
end;

function TFHIRFactoryR3.makeDateTime(value: TFslDateTime): TFHIRObject;
begin
  result := TFhirDateTime.Create(value);
end;

function TFHIRFactoryR3.makeDate(value: TFslDateTime): TFHIRObject;
begin
  result := TFhirDate.Create(value);
end;

function TFHIRFactoryR3.makeDecimal(s: string): TFHIRObject;
begin
  result := TFhirDecimal.Create(s);
end;

function TFHIRFactoryR3.makeDtFromForm(part: TMimePart; langList : THTTPLanguageList; name: String; type_: string): TFHIRXVersionElementWrapper;
begin
  if type_ = 'Coding' then
    result := wrapCoding(LoadDTFromFormParam(nil, part, langList, name, TFhirCoding))
  else if type_ = 'CodeableConcept' then
    result := wrapCodeableConcept(LoadDTFromFormParam(nil, part, langList, name, TFhirCodeableConcept))
  else
    raise EFHIRException.Create('Unknown Supported Data Type '+type_);
end;

function TFHIRFactoryR3.makeDuration(dt: TDateTime): TFHIRObject;
begin
  result := TFhirQuantity.fromDuration(dt);
end;

function TFHIRFactoryR3.wrapPrimitive(p: TFHIRObject): TFHIRPrimitiveW;
begin
  if (p = nil) then
    result := nil
  else
    result := TFHIRPrimitive3.Create(p);
end;

function TFHIRFactoryR3.makeElementModelManager: TFHIRBaseMMManager;
begin
  result := TFHIRMMManager.Create;
end;

function TFHIRFactoryR3.makeGenerator(worker: TFHIRWorkerContextV): TFHIRNarrativeGeneratorBase;
begin
  result := TFHIRNarrativeGenerator.Create(worker);
end;

function TFHIRFactoryR3.makeHealthcareCard: THealthcareCard;
begin
  raise EFslException.Create('Healthcare Cards are not supported in version '+versionString);
end;

function TFHIRFactoryR3.makeInteger(s: string): TFHIRObject;
begin
  result := TFhirInteger.Create(s);
end;

function TFHIRFactoryR3.makeProxy(packageId : String; pi: TNpmPackageResource; worker : TFHIRWorkerContextV; lock: TFslLock): TFHIRResourceProxyV;
begin
  result := TFHIRResourceProxy.Create(packageId, self.link, lock, worker, pi);
end;

function TFHIRFactoryR3.makeProxy(packageId : String; resource : TFHIRResourceV) : TFHIRResourceProxyV;
begin
  result := TFHIRResourceProxy.Create(packageId, self.link, resource as TFHIRResource);
end;

function TFHIRFactoryR3.makeIssue(level : TIssueSeverity; issue: TFhirIssueType; location, message: String): TFhirOperationOutcomeIssueW;
var
  iss : TFhirOperationOutcomeIssue;
begin
  iss := TFhirOperationOutcomeIssue.Create;
  try
    iss.severity := ISSUE_SEVERITY_MAP2[level];
    iss.code := ExceptionTypeTranslations[issue];
    iss.details := TFhirCodeableConcept.Create;
    iss.details.text := message;
    iss.locationList.add(location);
    result := TFhirOperationOutcomeIssue3.Create(iss.Link);
  finally
    iss.free;
  end;
end;

function TFHIRFactoryR3.makeOpReqLookup: TFHIRLookupOpRequestW;
begin
  result := TFHIRLookupOpRequest3.Create(TFHIRLookupOpRequest.create);
end;

function TFHIRFactoryR3.makeOpReqSubsumes: TFHIRSubsumesOpRequestW;
begin
  result := TFHIRSubsumesOpRequest3.Create(TFHIRSubsumesOpRequest.create);
end;

function TFHIRFactoryR3.makeOpRespLookup: TFHIRLookupOpResponseW;
begin
  result := TFHIRLookupOpResponse3.Create(TFHIRLookupOpResponse.create);
end;

function TFHIRFactoryR3.makeOpRespSubsumes: TFHIRSubsumesOpResponseW;
begin
  result := TFHIRSubsumesOpResponse3.Create(TFHIRSubsumesOpResponse.create);
end;

function TFHIRFactoryR3.makeParameters: TFHIRParametersW;
begin
  result := TFHIRParameters3.Create(TFHIRParameters.Create);
end;

function TFHIRFactoryR3.makeParamsFromForm(s: TStream): TFHIRResourceV;
begin
  result := parseParamsFromForm(s);
end;

function TFHIRFactoryR3.makeParser(worker: TFHIRWorkerContextV; format: TFHIRFormat; langList : THTTPLanguageList): TFHIRParser;
begin
  result := TFHIRParsers3.parser(worker.link as TFHIRWorkerContext, format, langList.link);
end;

function TFHIRFactoryR3.makePathEngine(worker: TFHIRWorkerContextV; ucum : TUcumServiceInterface): TFHIRPathEngineV;
begin
  result := TFHIRPathEngine.Create(worker as TFHIRWorkerContext, ucum);
end;

function TFHIRFactoryR3.makeString(s: string): TFHIRObject;
begin
  if (s = '') then
    result := nil
  else
    result := TFhirString.Create(s);
end;

function TFHIRFactoryR3.makeTerminologyCapablities: TFhirTerminologyCapabilitiesW;
begin
  result := TFhirTerminologyCapabilities3.Create(TFHIRParameters.create);
end;

function TFHIRFactoryR3.makeUri(s: string): TFHIRObject;
begin
  result := TFhirUri.Create(s);
end;

function TFHIRFactoryR3.makeValidator(worker: TFHIRWorkerContextV): TFHIRValidatorV;
begin
  result := TFHIRValidator3.Create(worker as TFHIRWorkerContext);
end;

function TFHIRFactoryR3.makeValueSetContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains3.Create(TFhirValueSetExpansionContains.create);
end;

procedure TFHIRFactoryR3.markWithTag(r: TFHIRResourceV; systemUri, code, display: String);
var
  res : TFHIRResource;
  tag : TFHIRCoding;
begin
  res := r as TFhirResource;
  if (res.meta = nil) then
    res.meta := TFHIRMeta.Create;
  for tag in res.meta.tagList do
    if (tag.system = systemUri) and (tag.code = code) then
    begin
      if (display <> '') then
      begin
        tag.display := display;
      end;
      exit;
    end;
  tag := res.meta.tagList.Append;
  tag.system := systemUri;
  tag.code := code;
  tag.display := display;
end;

function TFHIRFactoryR3.resCategory(name: String): TTokenCategory;
var
  a : TFhirResourceType;
begin
  for a in ALL_RESOURCE_TYPES do
    if CODES_TFhirResourceType[a] = name then
      exit(RESOURCE_CATEGORY[a]);
  result := tcOther;
end;

procedure TFHIRFactoryR3.clearXhtml(res : TFHIRResourceV);
var
  r : TFHIRDomainResource;
begin
  if res = nil then
    exit;
  if not (res is TFHIRDomainResource) then
    exit;
  r := res as TFHIRDomainResource;
  r.text := nil;
end;


function TFHIRFactoryR3.resetXhtml(res: TFHIRResourceV): TFHIRXhtmlNode;
var
  r : TFHIRDomainResource;
begin
  if res = nil then
    exit(nil);
  if not (res is TFHIRDomainResource) then
    exit(nil);
  r := res as TFHIRDomainResource;
  r.text := TFHIRNarrative.Create;
  r.text.status := NarrativeStatusGenerated;
  r.text.div_ := TFhirXHtmlNode.Create('div');
  result := r.text.div_;
end;

function TFHIRFactoryR3.resourceNames: TArray<String>;
var
  a : TFhirResourceType;
begin
  SetLength(result, length(CODES_TFhirResourceType)-2);
  for a in ALL_RESOURCE_TYPES do
    if not (a in [frtNull, frtCustom]) then
      result[ord(a)-1] := CODES_TFhirResourceType[a];
end;

procedure TFHIRFactoryR3.setXhtml(res: TFHIRResourceV; x: TFHIRXhtmlNode);
var
  r : TFHIRDomainResource;
begin
  if res = nil then
  begin
    x.free;
    raise EFHIRException.Create('Unable to set xhtml on nil resource');
  end;
  if not (res is TFHIRDomainResource) then
  begin
    x.free;
    raise EFHIRException.Create('Unable to set xhtml on non-domain resource');
  end;
  r := res as TFHIRDomainResource;
  if (r.text = nil) then
  begin
    r.text := TFHIRNarrative.Create;
    r.text.status := NarrativeStatusGenerated;
  end;
  r.text.div_ := x;
end;

function TFHIRFactoryR3.specUrl: String;
begin
  result := 'http://build.fhir.org';
end;

function TFHIRFactoryR3.URLs: TCommonURLs;
begin
  result.SecuritySourceType := URI_FHIR_SECURITY_SOURCE_TYPE_R3;
end;

function TFHIRFactoryR3.version: TFHIRVersion;
begin
  result := fhirVersionRelease3;
end;

function TFHIRFactoryR3.versionName: String;
begin
  result := 'R3';
end;

function TFHIRFactoryR3.versionString: String;
begin
  result := FHIR_GENERATED_VERSION;
end;

function TFHIRFactoryR3.wrapAttachment(r : TFHIRObject) : TFHIRAttachmentW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirAttachment3.Create(r);
end;

function TFHIRFactoryR3.wrapAuditEvent(r: TFHIRResourceV): TFhirAuditEventW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirAuditEvent3.Create(r);
end;

function TFHIRFactoryR3.wrapBinary(r: TFHIRResourceV): TFhirBinaryW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirBinary3.Create(r);
end;

function TFHIRFactoryR3.wrapBundle(r: TFHIRResourceV): TFhirBundleW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirBundle3.Create(r);
end;

function TFHIRFactoryR3.wrapBundleEntry(o: TFHIRObject): TFhirBundleEntryW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRBundleEntry3.Create(o);
end;

function TFHIRFactoryR3.wrapCapabilityStatement(r: TFHIRResourceV): TFHIRCapabilityStatementW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRCapabilityStatement3.Create(r);
end;

function TFHIRFactoryR3.wrapCodeableConcept(o: TFHIRObject): TFhirCodeableConceptW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirCodeableConcept3.Create(o);
end;

function TFHIRFactoryR3.wrapCodeSystem(r: TFHIRResourceV): TFhirCodeSystemW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRCodeSystem3.Create(r);
end;

function TFHIRFactoryR3.wrapCoding(o: TFHIRObject): TFhirCodingW;
begin
  if o = nil then
    result := nil
  else
  result := TFhirCoding3.Create(o);
end;

function TFHIRFactoryR3.wrapConceptMap(r: TFHIRResourceV): TFhirConceptMapW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirConceptMap3.Create(r);
end;

function TFHIRFactoryR3.wrapConsent(o: TFHIRResourceV): TFHIRConsentW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRConsent3.Create(o);
end;

function TFHIRFactoryR3.wrapEncounter(r: TFHIRResourceV): TFhirEncounterW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirEncounter3.Create(r);
end;

function TFHIRFactoryR3.wrapEventDefinition(o: TFHIRResourceV): TFHIREventDefinitionW;
begin
  raise EFHIRException.Create('Not supported in R3');
end;

function TFHIRFactoryR3.wrapExtension(o: TFHIRObject): TFhirExtensionW;
begin
  result := TFhirExtension3.Create(o);
end;

function TFHIRFactoryR3.wrapGroup(r: TFHIRResourceV): TFhirGroupW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirGroup3.Create(r);
end;

function TFHIRFactoryR3.wrapIdentifier(o: TFHIRObject): TFhirIdentifierW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirIdentifier3.Create(o);
end;

function TFHIRFactoryR3.wrapImmunization(o: TFHIRResourceV): TFhirImmunizationW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirImmunization3.Create(o);
end;

function TFHIRFactoryR3.wrapMeta(r: TFHIRObject): TFhirMetaW;
begin
  if r = nil then
    result := nil
  else if r.isResource then
  begin
    result := TFHIRMeta3.Create((r as TFHIRResource).meta.link);
    TFHIRMeta3(result).resource := (r as TFHIRResource).link;
  end
  else
    result := TFHIRMeta3.Create((r as TFhirMeta))
end;

function TFHIRFactoryR3.wrapNamingSystem(o: TFHIRResourceV): TFHIRNamingSystemW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRNamingSystem3.Create(o);
end;

function TFHIRFactoryR3.wrapMeta(r: TFHIRResourceV): TFhirMetaW;
begin
  result := TFHIRMeta3.Create((r as TFHIRResource).meta.link);
  TFHIRMeta3(result).resource := (r as TFHIRResource).link;
end;

function TFHIRFactoryR3.wrapObservation(r: TFHIRResourceV): TFhirObservationW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirObservation3.Create(r);
end;

function TFHIRFactoryR3.wrapOperationOutcome(r: TFHIRResourceV): TFhirOperationOutcomeW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirOperationOutcome3.Create(r);
end;

function TFHIRFactoryR3.wrapParams(r: TFHIRResourceV): TFHIRParametersW;
begin
  if r = nil then
    result := nil
  else if r is TFHIRExpansionProfile then
    result := TFHIRExpansionProfile3.Create(r)
  else
    result := TFhirParameters3.Create(r);
end;

function TFHIRFactoryR3.wrapPatient(r: TFHIRResourceV): TFhirPatientW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirPatient3.Create(r);
end;

function TFHIRFactoryR3.wrapPeriod(r: TFHIRObject): TFhirPeriodW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirPeriod3.Create(r);
end;

function TFHIRFactoryR3.wrapProvenance(o: TFHIRResourceV): TFHIRProvenanceW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRProvenance3.Create(o);
end;

function TFHIRFactoryR3.wrapQuantity(r: TFHIRObject): TFhirQuantityW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirQuantity3.Create(r);
end;

function TFHIRFactoryR3.wrapStructureDefinition(r: TFHIRResourceV): TFhirStructureDefinitionW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRStructureDefinition3.Create(r);
end;

function TFHIRFactoryR3.wrapStructureMap(o: TFHIRResourceV): TFHIRStructureMapW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRStructureMap3.Create(o);
end;

function TFHIRFactoryR3.wrapSubscription(r: TFHIRResourceV): TFhirSubscriptionW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirSubscription3.Create(r);
end;

function TFHIRFactoryR3.wrapSubscriptionTopic(r: TFHIRResourceV): TFhirSubscriptionTopicW;
begin
  result := nil;
end;

function TFHIRFactoryR3.wrapTestScript(o: TFHIRResourceV): TFHIRTestScriptW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirTestScript3.Create(o);
end;

function TFHIRFactoryR3.wrapValueSet(r: TFHIRResourceV): TFhirValueSetW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRValueSet3.Create(r);
end;

function TFHIRFactoryR3.makeBase64Binary(s: string): TFHIRObject;
begin
  result := TFhirBase64Binary.Create(decodeBase64(AnsiString(s)));
end;

function TFHIRFactoryR3.makeBinary(content: TBytes; contentType: String): TFHIRResourceV;
begin
  result := TFhirBinary.Create;
  try
    TFhirBinary(result).content := content;
    TFhirBinary(result).contentType := contentType;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactoryR3.makeBoolean(b: boolean): TFHIRObject;
begin
  result := TFhirBoolean.Create(b);
end;

function TFHIRFactoryR3.makeBundle(list: TFslList<TFHIRResourceV>): TFHIRBundleW;
var
  bnd : TFHIRBundle;
  r : TFhirResourceV;
begin
  bnd := TFHIRBundle.Create(BundleTypeCollection);
  try
    if list <> nil then
    begin
      for r in list do
        bnd.entryList.Append.resource := r.link as TFhirResource;
    end;
    result := TFHIRBundle3.Create(bnd.link);
  finally
    bnd.free;
  end;
end;

function TFHIRFactoryR3.makeByName(const name : String) : TFHIRObject;
begin
  if name = 'enum' then
    result := TFhirEnum.Create()
  else if name = 'date' then
    result := TFhirDate.Create()
  else if name = 'dateTime' then
    result := TFhirDateTime.Create()
  else if name = 'string' then
    result := TFhirString.Create()
  else if name = 'integer' then
    result := TFhirInteger.Create()
  else if name = 'uri' then
    result := TFhirUri.Create()
  else if name = 'instant' then
    result := TFhirInstant.Create()
  else if name = 'xhtml' then
    result := TFhirXhtml.Create()
  else if name = 'boolean' then
    result := TFhirBoolean.Create()
  else if name = 'base64Binary' then
    result := TFhirBase64Binary.Create()
  else if name = 'time' then
    result := TFhirTime.Create()
  else if name = 'decimal' then
    result := TFhirDecimal.Create()
  else if name = 'code' then
    result := TFhirCode.Create()
  else if name = 'oid' then
    result := TFhirOid.Create()
  else if name = 'uuid' then
    result := TFhirUuid.Create()
  else if name = 'markdown' then
    result := TFhirMarkdown.Create()
  else if name = 'unsignedInt' then
    result := TFhirUnsignedInt.Create()
  else if name = 'id' then
    result := TFhirId.Create()
  else if name = 'positiveInt' then
    result := TFhirPositiveInt.Create()
{$IFDEF FHIR_PARAMETERS}
  else if name = 'Parameters.parameter' then
    result := TFhirParametersParameter.Create()
  else if name = 'Parameters' then
    result := TFhirParameters.Create()
{$ENDIF FHIR_PARAMETERS}
  else if name = 'Extension' then
    result := TFhirExtension.Create()
  else if name = 'Narrative' then
    result := TFhirNarrative.Create()
  else if name = 'Contributor' then
    result := TFhirContributor.Create()
  else if name = 'Attachment' then
    result := TFhirAttachment.Create()
  else if name = 'DataRequirement.codeFilter' then
    result := TFhirDataRequirementCodeFilter.Create()
  else if name = 'DataRequirement.dateFilter' then
    result := TFhirDataRequirementDateFilter.Create()
  else if name = 'DataRequirement' then
    result := TFhirDataRequirement.Create()
  else if name = 'Dosage' then
    result := TFhirDosage.Create()
  else if name = 'Identifier' then
    result := TFhirIdentifier.Create()
  else if name = 'Coding' then
    result := TFhirCoding.Create()
  else if name = 'SampledData' then
    result := TFhirSampledData.Create()
  else if name = 'Ratio' then
    result := TFhirRatio.Create()
  else if name = 'Reference' then
    result := TFhirReference.Create()
  else if name = 'TriggerDefinition' then
    result := TFhirTriggerDefinition.Create()
  else if name = 'Period' then
    result := TFhirPeriod.Create()
  else if name = 'Quantity' then
    result := TFhirQuantity.Create()
  else if name = 'Range' then
    result := TFhirRange.Create()
  else if name = 'RelatedArtifact' then
    result := TFhirRelatedArtifact.Create()
  else if name = 'Annotation' then
    result := TFhirAnnotation.Create()
  else if name = 'ContactDetail' then
    result := TFhirContactDetail.Create()
  else if name = 'UsageContext' then
    result := TFhirUsageContext.Create()
  else if name = 'Signature' then
    result := TFhirSignature.Create()
  else if name = 'CodeableConcept' then
    result := TFhirCodeableConcept.Create()
  else if name = 'ParameterDefinition' then
    result := TFhirParameterDefinition.Create()
  else if name = 'ContactPoint' then
    result := TFhirContactPoint.Create()
  else if name = 'HumanName' then
    result := TFhirHumanName.Create()
  else if name = 'Meta' then
    result := TFhirMeta.Create()
  else if name = 'Address' then
    result := TFhirAddress.Create()
  else if name = 'ElementDefinition.slicing' then
    result := TFhirElementDefinitionSlicing.Create()
  else if name = 'ElementDefinition.slicing.discriminator' then
    result := TFhirElementDefinitionSlicingDiscriminator.Create()
  else if name = 'ElementDefinition.base' then
    result := TFhirElementDefinitionBase.Create()
  else if name = 'ElementDefinition.type' then
    result := TFhirElementDefinitionType.Create()
  else if name = 'ElementDefinition.example' then
    result := TFhirElementDefinitionExample.Create()
  else if name = 'ElementDefinition.constraint' then
    result := TFhirElementDefinitionConstraint.Create()
  else if name = 'ElementDefinition.binding' then
    result := TFhirElementDefinitionBinding.Create()
  else if name = 'ElementDefinition.mapping' then
    result := TFhirElementDefinitionMapping.Create()
  else if name = 'ElementDefinition' then
    result := TFhirElementDefinition.Create()
  else if name = 'Timing.repeat' then
    result := TFhirTimingRepeat.Create()
  else if name = 'Timing' then
    result := TFhirTiming.Create()
  else if name = 'Count' then
    result := TFhirCount.Create()
  else if name = 'Money' then
    result := TFhirMoney.Create()
  else if name = 'Age' then
    result := TFhirAge.Create()
  else if name = 'Distance' then
    result := TFhirDistance.Create()
  else if name = 'Duration' then
    result := TFhirDuration.Create()
{$IFDEF FHIR_ACCOUNT}
  else if name = 'Account.coverage' then
    result := TFhirAccountCoverage.Create()
  else if name = 'Account.guarantor' then
    result := TFhirAccountGuarantor.Create()
  else if name = 'Account' then
    result := TFhirAccount.Create()
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  else if name = 'ActivityDefinition.participant' then
    result := TFhirActivityDefinitionParticipant.Create()
  else if name = 'ActivityDefinition.dynamicValue' then
    result := TFhirActivityDefinitionDynamicValue.Create()
  else if name = 'ActivityDefinition' then
    result := TFhirActivityDefinition.Create()
{$ENDIF FHIR_ACTIVITYDEFINITION}
{$IFDEF FHIR_ADVERSEEVENT}
  else if name = 'AdverseEvent.suspectEntity' then
    result := TFhirAdverseEventSuspectEntity.Create()
  else if name = 'AdverseEvent' then
    result := TFhirAdverseEvent.Create()
{$ENDIF FHIR_ADVERSEEVENT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  else if name = 'AllergyIntolerance.reaction' then
    result := TFhirAllergyIntoleranceReaction.Create()
  else if name = 'AllergyIntolerance' then
    result := TFhirAllergyIntolerance.Create()
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  else if name = 'Appointment.participant' then
    result := TFhirAppointmentParticipant.Create()
  else if name = 'Appointment' then
    result := TFhirAppointment.Create()
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  else if name = 'AppointmentResponse' then
    result := TFhirAppointmentResponse.Create()
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_AUDITEVENT}
  else if name = 'AuditEvent.agent' then
    result := TFhirAuditEventAgent.Create()
  else if name = 'AuditEvent.agent.network' then
    result := TFhirAuditEventAgentNetwork.Create()
  else if name = 'AuditEvent.source' then
    result := TFhirAuditEventSource.Create()
  else if name = 'AuditEvent.entity' then
    result := TFhirAuditEventEntity.Create()
  else if name = 'AuditEvent.entity.detail' then
    result := TFhirAuditEventEntityDetail.Create()
  else if name = 'AuditEvent' then
    result := TFhirAuditEvent.Create()
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BASIC}
  else if name = 'Basic' then
    result := TFhirBasic.Create()
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BINARY}
  else if name = 'Binary' then
    result := TFhirBinary.Create()
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BODYSITE}
  else if name = 'BodySite' then
    result := TFhirBodySite.Create()
{$ENDIF FHIR_BODYSITE}
{$IFDEF FHIR_BUNDLE}
  else if name = 'Bundle.link' then
    result := TFhirBundleLink.Create()
  else if name = 'Bundle.entry' then
    result := TFhirBundleEntry.Create()
  else if name = 'Bundle.entry.search' then
    result := TFhirBundleEntrySearch.Create()
  else if name = 'Bundle.entry.request' then
    result := TFhirBundleEntryRequest.Create()
  else if name = 'Bundle.entry.response' then
    result := TFhirBundleEntryResponse.Create()
  else if name = 'Bundle' then
    result := TFhirBundle.Create()
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  else if name = 'CapabilityStatement.software' then
    result := TFhirCapabilityStatementSoftware.Create()
  else if name = 'CapabilityStatement.implementation' then
    result := TFhirCapabilityStatementImplementation.Create()
  else if name = 'CapabilityStatement.rest' then
    result := TFhirCapabilityStatementRest.Create()
  else if name = 'CapabilityStatement.rest.security' then
    result := TFhirCapabilityStatementRestSecurity.Create()
  else if name = 'CapabilityStatement.rest.security.certificate' then
    result := TFhirCapabilityStatementRestSecurityCertificate.Create()
  else if name = 'CapabilityStatement.rest.resource' then
    result := TFhirCapabilityStatementRestResource.Create()
  else if name = 'CapabilityStatement.rest.resource.interaction' then
    result := TFhirCapabilityStatementRestResourceInteraction.Create()
  else if name = 'CapabilityStatement.rest.resource.searchParam' then
    result := TFhirCapabilityStatementRestResourceSearchParam.Create()
  else if name = 'CapabilityStatement.rest.interaction' then
    result := TFhirCapabilityStatementRestInteraction.Create()
  else if name = 'CapabilityStatement.rest.operation' then
    result := TFhirCapabilityStatementRestOperation.Create()
  else if name = 'CapabilityStatement.messaging' then
    result := TFhirCapabilityStatementMessaging.Create()
  else if name = 'CapabilityStatement.messaging.endpoint' then
    result := TFhirCapabilityStatementMessagingEndpoint.Create()
  else if name = 'CapabilityStatement.messaging.supportedMessage' then
    result := TFhirCapabilityStatementMessagingSupportedMessage.Create()
  else if name = 'CapabilityStatement.messaging.event' then
    result := TFhirCapabilityStatementMessagingEvent.Create()
  else if name = 'CapabilityStatement.document' then
    result := TFhirCapabilityStatementDocument.Create()
  else if name = 'CapabilityStatement' then
    result := TFhirCapabilityStatement.Create()
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CAREPLAN}
  else if name = 'CarePlan.activity' then
    result := TFhirCarePlanActivity.Create()
  else if name = 'CarePlan.activity.detail' then
    result := TFhirCarePlanActivityDetail.Create()
  else if name = 'CarePlan' then
    result := TFhirCarePlan.Create()
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CARETEAM}
  else if name = 'CareTeam.participant' then
    result := TFhirCareTeamParticipant.Create()
  else if name = 'CareTeam' then
    result := TFhirCareTeam.Create()
{$ENDIF FHIR_CARETEAM}
{$IFDEF FHIR_CHARGEITEM}
  else if name = 'ChargeItem.participant' then
    result := TFhirChargeItemParticipant.Create()
  else if name = 'ChargeItem' then
    result := TFhirChargeItem.Create()
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CLAIM}
  else if name = 'Claim.related' then
    result := TFhirClaimRelated.Create()
  else if name = 'Claim.payee' then
    result := TFhirClaimPayee.Create()
  else if name = 'Claim.careTeam' then
    result := TFhirClaimCareTeam.Create()
  else if name = 'Claim.information' then
    result := TFhirClaimInformation.Create()
  else if name = 'Claim.diagnosis' then
    result := TFhirClaimDiagnosis.Create()
  else if name = 'Claim.procedure' then
    result := TFhirClaimProcedure.Create()
  else if name = 'Claim.insurance' then
    result := TFhirClaimInsurance.Create()
  else if name = 'Claim.accident' then
    result := TFhirClaimAccident.Create()
  else if name = 'Claim.item' then
    result := TFhirClaimItem.Create()
  else if name = 'Claim.item.detail' then
    result := TFhirClaimItemDetail.Create()
  else if name = 'Claim.item.detail.subDetail' then
    result := TFhirClaimItemDetailSubDetail.Create()
  else if name = 'Claim' then
    result := TFhirClaim.Create()
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  else if name = 'ClaimResponse.item' then
    result := TFhirClaimResponseItem.Create()
  else if name = 'ClaimResponse.item.adjudication' then
    result := TFhirClaimResponseItemAdjudication.Create()
  else if name = 'ClaimResponse.item.detail' then
    result := TFhirClaimResponseItemDetail.Create()
  else if name = 'ClaimResponse.item.detail.subDetail' then
    result := TFhirClaimResponseItemDetailSubDetail.Create()
  else if name = 'ClaimResponse.addItem' then
    result := TFhirClaimResponseAddItem.Create()
  else if name = 'ClaimResponse.addItem.detail' then
    result := TFhirClaimResponseAddItemDetail.Create()
  else if name = 'ClaimResponse.error' then
    result := TFhirClaimResponseError.Create()
  else if name = 'ClaimResponse.payment' then
    result := TFhirClaimResponsePayment.Create()
  else if name = 'ClaimResponse.processNote' then
    result := TFhirClaimResponseProcessNote.Create()
  else if name = 'ClaimResponse.insurance' then
    result := TFhirClaimResponseInsurance.Create()
  else if name = 'ClaimResponse' then
    result := TFhirClaimResponse.Create()
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  else if name = 'ClinicalImpression.investigation' then
    result := TFhirClinicalImpressionInvestigation.Create()
  else if name = 'ClinicalImpression.finding' then
    result := TFhirClinicalImpressionFinding.Create()
  else if name = 'ClinicalImpression' then
    result := TFhirClinicalImpression.Create()
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_CODESYSTEM}
  else if name = 'CodeSystem.filter' then
    result := TFhirCodeSystemFilter.Create()
  else if name = 'CodeSystem.property' then
    result := TFhirCodeSystemProperty.Create()
  else if name = 'CodeSystem.concept' then
    result := TFhirCodeSystemConcept.Create()
  else if name = 'CodeSystem.concept.designation' then
    result := TFhirCodeSystemConceptDesignation.Create()
  else if name = 'CodeSystem.concept.property' then
    result := TFhirCodeSystemConceptProperty.Create()
  else if name = 'CodeSystem' then
    result := TFhirCodeSystem.Create()
{$ENDIF FHIR_CODESYSTEM}
{$IFDEF FHIR_COMMUNICATION}
  else if name = 'Communication.payload' then
    result := TFhirCommunicationPayload.Create()
  else if name = 'Communication' then
    result := TFhirCommunication.Create()
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  else if name = 'CommunicationRequest.payload' then
    result := TFhirCommunicationRequestPayload.Create()
  else if name = 'CommunicationRequest.requester' then
    result := TFhirCommunicationRequestRequester.Create()
  else if name = 'CommunicationRequest' then
    result := TFhirCommunicationRequest.Create()
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  else if name = 'CompartmentDefinition.resource' then
    result := TFhirCompartmentDefinitionResource.Create()
  else if name = 'CompartmentDefinition' then
    result := TFhirCompartmentDefinition.Create()
{$ENDIF FHIR_COMPARTMENTDEFINITION}
{$IFDEF FHIR_COMPOSITION}
  else if name = 'Composition.attester' then
    result := TFhirCompositionAttester.Create()
  else if name = 'Composition.relatesTo' then
    result := TFhirCompositionRelatesTo.Create()
  else if name = 'Composition.event' then
    result := TFhirCompositionEvent.Create()
  else if name = 'Composition.section' then
    result := TFhirCompositionSection.Create()
  else if name = 'Composition' then
    result := TFhirComposition.Create()
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  else if name = 'ConceptMap.group' then
    result := TFhirConceptMapGroup.Create()
  else if name = 'ConceptMap.group.element' then
    result := TFhirConceptMapGroupElement.Create()
  else if name = 'ConceptMap.group.element.target' then
    result := TFhirConceptMapGroupElementTarget.Create()
  else if name = 'ConceptMap.group.element.target.dependsOn' then
    result := TFhirConceptMapGroupElementTargetDependsOn.Create()
  else if name = 'ConceptMap.group.unmapped' then
    result := TFhirConceptMapGroupUnmapped.Create()
  else if name = 'ConceptMap' then
    result := TFhirConceptMap.Create()
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_CONDITION}
  else if name = 'Condition.stage' then
    result := TFhirConditionStage.Create()
  else if name = 'Condition.evidence' then
    result := TFhirConditionEvidence.Create()
  else if name = 'Condition' then
    result := TFhirCondition.Create()
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_CONSENT}
  else if name = 'Consent.actor' then
    result := TFhirConsentActor.Create()
  else if name = 'Consent.policy' then
    result := TFhirConsentPolicy.Create()
  else if name = 'Consent.data' then
    result := TFhirConsentData.Create()
  else if name = 'Consent.except' then
    result := TFhirConsentExcept.Create()
  else if name = 'Consent.except.actor' then
    result := TFhirConsentExceptActor.Create()
  else if name = 'Consent.except.data' then
    result := TFhirConsentExceptData.Create()
  else if name = 'Consent' then
    result := TFhirConsent.Create()
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  else if name = 'Contract.agent' then
    result := TFhirContractAgent.Create()
  else if name = 'Contract.signer' then
    result := TFhirContractSigner.Create()
  else if name = 'Contract.valuedItem' then
    result := TFhirContractValuedItem.Create()
  else if name = 'Contract.term' then
    result := TFhirContractTerm.Create()
  else if name = 'Contract.term.agent' then
    result := TFhirContractTermAgent.Create()
  else if name = 'Contract.term.valuedItem' then
    result := TFhirContractTermValuedItem.Create()
  else if name = 'Contract.friendly' then
    result := TFhirContractFriendly.Create()
  else if name = 'Contract.legal' then
    result := TFhirContractLegal.Create()
  else if name = 'Contract.rule' then
    result := TFhirContractRule.Create()
  else if name = 'Contract' then
    result := TFhirContract.Create()
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_COVERAGE}
  else if name = 'Coverage.grouping' then
    result := TFhirCoverageGrouping.Create()
  else if name = 'Coverage' then
    result := TFhirCoverage.Create()
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_DATAELEMENT}
  else if name = 'DataElement.mapping' then
    result := TFhirDataElementMapping.Create()
  else if name = 'DataElement' then
    result := TFhirDataElement.Create()
{$ENDIF FHIR_DATAELEMENT}
{$IFDEF FHIR_DETECTEDISSUE}
  else if name = 'DetectedIssue.mitigation' then
    result := TFhirDetectedIssueMitigation.Create()
  else if name = 'DetectedIssue' then
    result := TFhirDetectedIssue.Create()
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  else if name = 'Device.udi' then
    result := TFhirDeviceUdi.Create()
  else if name = 'Device' then
    result := TFhirDevice.Create()
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICECOMPONENT}
  else if name = 'DeviceComponent.productionSpecification' then
    result := TFhirDeviceComponentProductionSpecification.Create()
  else if name = 'DeviceComponent' then
    result := TFhirDeviceComponent.Create()
{$ENDIF FHIR_DEVICECOMPONENT}
{$IFDEF FHIR_DEVICEMETRIC}
  else if name = 'DeviceMetric.calibration' then
    result := TFhirDeviceMetricCalibration.Create()
  else if name = 'DeviceMetric' then
    result := TFhirDeviceMetric.Create()
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEREQUEST}
  else if name = 'DeviceRequest.requester' then
    result := TFhirDeviceRequestRequester.Create()
  else if name = 'DeviceRequest' then
    result := TFhirDeviceRequest.Create()
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  else if name = 'DeviceUseStatement' then
    result := TFhirDeviceUseStatement.Create()
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  else if name = 'DiagnosticReport.performer' then
    result := TFhirDiagnosticReportPerformer.Create()
  else if name = 'DiagnosticReport.image' then
    result := TFhirDiagnosticReportImage.Create()
  else if name = 'DiagnosticReport' then
    result := TFhirDiagnosticReport.Create()
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  else if name = 'DocumentManifest.content' then
    result := TFhirDocumentManifestContent.Create()
  else if name = 'DocumentManifest.related' then
    result := TFhirDocumentManifestRelated.Create()
  else if name = 'DocumentManifest' then
    result := TFhirDocumentManifest.Create()
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  else if name = 'DocumentReference.relatesTo' then
    result := TFhirDocumentReferenceRelatesTo.Create()
  else if name = 'DocumentReference.content' then
    result := TFhirDocumentReferenceContent.Create()
  else if name = 'DocumentReference.context' then
    result := TFhirDocumentReferenceContext.Create()
  else if name = 'DocumentReference.context.related' then
    result := TFhirDocumentReferenceContextRelated.Create()
  else if name = 'DocumentReference' then
    result := TFhirDocumentReference.Create()
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_ELIGIBILITYREQUEST}
  else if name = 'EligibilityRequest' then
    result := TFhirEligibilityRequest.Create()
{$ENDIF FHIR_ELIGIBILITYREQUEST}
{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  else if name = 'EligibilityResponse.insurance' then
    result := TFhirEligibilityResponseInsurance.Create()
  else if name = 'EligibilityResponse.insurance.benefitBalance' then
    result := TFhirEligibilityResponseInsuranceBenefitBalance.Create()
  else if name = 'EligibilityResponse.insurance.benefitBalance.financial' then
    result := TFhirEligibilityResponseInsuranceBenefitBalanceFinancial.Create()
  else if name = 'EligibilityResponse.error' then
    result := TFhirEligibilityResponseError.Create()
  else if name = 'EligibilityResponse' then
    result := TFhirEligibilityResponse.Create()
{$ENDIF FHIR_ELIGIBILITYRESPONSE}
{$IFDEF FHIR_ENCOUNTER}
  else if name = 'Encounter.statusHistory' then
    result := TFhirEncounterStatusHistory.Create()
  else if name = 'Encounter.classHistory' then
    result := TFhirEncounterClassHistory.Create()
  else if name = 'Encounter.participant' then
    result := TFhirEncounterParticipant.Create()
  else if name = 'Encounter.diagnosis' then
    result := TFhirEncounterDiagnosis.Create()
  else if name = 'Encounter.hospitalization' then
    result := TFhirEncounterHospitalization.Create()
  else if name = 'Encounter.location' then
    result := TFhirEncounterLocation.Create()
  else if name = 'Encounter' then
    result := TFhirEncounter.Create()
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENDPOINT}
  else if name = 'Endpoint' then
    result := TFhirEndpoint.Create()
{$ENDIF FHIR_ENDPOINT}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  else if name = 'EnrollmentRequest' then
    result := TFhirEnrollmentRequest.Create()
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  else if name = 'EnrollmentResponse' then
    result := TFhirEnrollmentResponse.Create()
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EPISODEOFCARE}
  else if name = 'EpisodeOfCare.statusHistory' then
    result := TFhirEpisodeOfCareStatusHistory.Create()
  else if name = 'EpisodeOfCare.diagnosis' then
    result := TFhirEpisodeOfCareDiagnosis.Create()
  else if name = 'EpisodeOfCare' then
    result := TFhirEpisodeOfCare.Create()
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EXPANSIONPROFILE}
  else if name = 'ExpansionProfile.fixedVersion' then
    result := TFhirExpansionProfileFixedVersion.Create()
  else if name = 'ExpansionProfile.excludedSystem' then
    result := TFhirExpansionProfileExcludedSystem.Create()
  else if name = 'ExpansionProfile.designation' then
    result := TFhirExpansionProfileDesignation.Create()
  else if name = 'ExpansionProfile.designation.include' then
    result := TFhirExpansionProfileDesignationInclude.Create()
  else if name = 'ExpansionProfile.designation.include.designation' then
    result := TFhirExpansionProfileDesignationIncludeDesignation.Create()
  else if name = 'ExpansionProfile.designation.exclude' then
    result := TFhirExpansionProfileDesignationExclude.Create()
  else if name = 'ExpansionProfile.designation.exclude.designation' then
    result := TFhirExpansionProfileDesignationExcludeDesignation.Create()
  else if name = 'ExpansionProfile' then
    result := TFhirExpansionProfile.Create()
{$ENDIF FHIR_EXPANSIONPROFILE}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  else if name = 'ExplanationOfBenefit.related' then
    result := TFhirExplanationOfBenefitRelated.Create()
  else if name = 'ExplanationOfBenefit.payee' then
    result := TFhirExplanationOfBenefitPayee.Create()
  else if name = 'ExplanationOfBenefit.information' then
    result := TFhirExplanationOfBenefitInformation.Create()
  else if name = 'ExplanationOfBenefit.careTeam' then
    result := TFhirExplanationOfBenefitCareTeam.Create()
  else if name = 'ExplanationOfBenefit.diagnosis' then
    result := TFhirExplanationOfBenefitDiagnosis.Create()
  else if name = 'ExplanationOfBenefit.procedure' then
    result := TFhirExplanationOfBenefitProcedure.Create()
  else if name = 'ExplanationOfBenefit.insurance' then
    result := TFhirExplanationOfBenefitInsurance.Create()
  else if name = 'ExplanationOfBenefit.accident' then
    result := TFhirExplanationOfBenefitAccident.Create()
  else if name = 'ExplanationOfBenefit.item' then
    result := TFhirExplanationOfBenefitItem.Create()
  else if name = 'ExplanationOfBenefit.item.adjudication' then
    result := TFhirExplanationOfBenefitItemAdjudication.Create()
  else if name = 'ExplanationOfBenefit.item.detail' then
    result := TFhirExplanationOfBenefitItemDetail.Create()
  else if name = 'ExplanationOfBenefit.item.detail.subDetail' then
    result := TFhirExplanationOfBenefitItemDetailSubDetail.Create()
  else if name = 'ExplanationOfBenefit.addItem' then
    result := TFhirExplanationOfBenefitAddItem.Create()
  else if name = 'ExplanationOfBenefit.addItem.detail' then
    result := TFhirExplanationOfBenefitAddItemDetail.Create()
  else if name = 'ExplanationOfBenefit.payment' then
    result := TFhirExplanationOfBenefitPayment.Create()
  else if name = 'ExplanationOfBenefit.processNote' then
    result := TFhirExplanationOfBenefitProcessNote.Create()
  else if name = 'ExplanationOfBenefit.benefitBalance' then
    result := TFhirExplanationOfBenefitBenefitBalance.Create()
  else if name = 'ExplanationOfBenefit.benefitBalance.financial' then
    result := TFhirExplanationOfBenefitBenefitBalanceFinancial.Create()
  else if name = 'ExplanationOfBenefit' then
    result := TFhirExplanationOfBenefit.Create()
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  else if name = 'FamilyMemberHistory.condition' then
    result := TFhirFamilyMemberHistoryCondition.Create()
  else if name = 'FamilyMemberHistory' then
    result := TFhirFamilyMemberHistory.Create()
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  else if name = 'Flag' then
    result := TFhirFlag.Create()
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  else if name = 'Goal.target' then
    result := TFhirGoalTarget.Create()
  else if name = 'Goal' then
    result := TFhirGoal.Create()
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GRAPHDEFINITION}
  else if name = 'GraphDefinition.link' then
    result := TFhirGraphDefinitionLink.Create()
  else if name = 'GraphDefinition.link.target' then
    result := TFhirGraphDefinitionLinkTarget.Create()
  else if name = 'GraphDefinition.link.target.compartment' then
    result := TFhirGraphDefinitionLinkTargetCompartment.Create()
  else if name = 'GraphDefinition' then
    result := TFhirGraphDefinition.Create()
{$ENDIF FHIR_GRAPHDEFINITION}
{$IFDEF FHIR_GROUP}
  else if name = 'Group.characteristic' then
    result := TFhirGroupCharacteristic.Create()
  else if name = 'Group.member' then
    result := TFhirGroupMember.Create()
  else if name = 'Group' then
    result := TFhirGroup.Create()
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_GUIDANCERESPONSE}
  else if name = 'GuidanceResponse' then
    result := TFhirGuidanceResponse.Create()
{$ENDIF FHIR_GUIDANCERESPONSE}
{$IFDEF FHIR_HEALTHCARESERVICE}
  else if name = 'HealthcareService.availableTime' then
    result := TFhirHealthcareServiceAvailableTime.Create()
  else if name = 'HealthcareService.notAvailable' then
    result := TFhirHealthcareServiceNotAvailable.Create()
  else if name = 'HealthcareService' then
    result := TFhirHealthcareService.Create()
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGMANIFEST}
  else if name = 'ImagingManifest.study' then
    result := TFhirImagingManifestStudy.Create()
  else if name = 'ImagingManifest.study.series' then
    result := TFhirImagingManifestStudySeries.Create()
  else if name = 'ImagingManifest.study.series.instance' then
    result := TFhirImagingManifestStudySeriesInstance.Create()
  else if name = 'ImagingManifest' then
    result := TFhirImagingManifest.Create()
{$ENDIF FHIR_IMAGINGMANIFEST}
{$IFDEF FHIR_IMAGINGSTUDY}
  else if name = 'ImagingStudy.series' then
    result := TFhirImagingStudySeries.Create()
  else if name = 'ImagingStudy.series.instance' then
    result := TFhirImagingStudySeriesInstance.Create()
  else if name = 'ImagingStudy' then
    result := TFhirImagingStudy.Create()
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  else if name = 'Immunization.practitioner' then
    result := TFhirImmunizationPractitioner.Create()
  else if name = 'Immunization.explanation' then
    result := TFhirImmunizationExplanation.Create()
  else if name = 'Immunization.reaction' then
    result := TFhirImmunizationReaction.Create()
  else if name = 'Immunization.vaccinationProtocol' then
    result := TFhirImmunizationVaccinationProtocol.Create()
  else if name = 'Immunization' then
    result := TFhirImmunization.Create()
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  else if name = 'ImmunizationRecommendation.recommendation' then
    result := TFhirImmunizationRecommendationRecommendation.Create()
  else if name = 'ImmunizationRecommendation.recommendation.dateCriterion' then
    result := TFhirImmunizationRecommendationRecommendationDateCriterion.Create()
  else if name = 'ImmunizationRecommendation.recommendation.protocol' then
    result := TFhirImmunizationRecommendationRecommendationProtocol.Create()
  else if name = 'ImmunizationRecommendation' then
    result := TFhirImmunizationRecommendation.Create()
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  else if name = 'ImplementationGuide.dependency' then
    result := TFhirImplementationGuideDependency.Create()
  else if name = 'ImplementationGuide.package' then
    result := TFhirImplementationGuidePackage.Create()
  else if name = 'ImplementationGuide.package.resource' then
    result := TFhirImplementationGuidePackageResource.Create()
  else if name = 'ImplementationGuide.global' then
    result := TFhirImplementationGuideGlobal.Create()
  else if name = 'ImplementationGuide.page' then
    result := TFhirImplementationGuidePage.Create()
  else if name = 'ImplementationGuide' then
    result := TFhirImplementationGuide.Create()
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_LIBRARY}
  else if name = 'Library' then
    result := TFhirLibrary.Create()
{$ENDIF FHIR_LIBRARY}
{$IFDEF FHIR_LINKAGE}
  else if name = 'Linkage.item' then
    result := TFhirLinkageItem.Create()
  else if name = 'Linkage' then
    result := TFhirLinkage.Create()
{$ENDIF FHIR_LINKAGE}
{$IFDEF FHIR_LIST}
  else if name = 'List.entry' then
    result := TFhirListEntry.Create()
  else if name = 'List' then
    result := TFhirList.Create()
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_LOCATION}
  else if name = 'Location.position' then
    result := TFhirLocationPosition.Create()
  else if name = 'Location' then
    result := TFhirLocation.Create()
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_MEASURE}
  else if name = 'Measure.group' then
    result := TFhirMeasureGroup.Create()
  else if name = 'Measure.group.population' then
    result := TFhirMeasureGroupPopulation.Create()
  else if name = 'Measure.group.stratifier' then
    result := TFhirMeasureGroupStratifier.Create()
  else if name = 'Measure.supplementalData' then
    result := TFhirMeasureSupplementalData.Create()
  else if name = 'Measure' then
    result := TFhirMeasure.Create()
{$ENDIF FHIR_MEASURE}
{$IFDEF FHIR_MEASUREREPORT}
  else if name = 'MeasureReport.group' then
    result := TFhirMeasureReportGroup.Create()
  else if name = 'MeasureReport.group.population' then
    result := TFhirMeasureReportGroupPopulation.Create()
  else if name = 'MeasureReport.group.stratifier' then
    result := TFhirMeasureReportGroupStratifier.Create()
  else if name = 'MeasureReport.group.stratifier.stratum' then
    result := TFhirMeasureReportGroupStratifierStratum.Create()
  else if name = 'MeasureReport.group.stratifier.stratum.population' then
    result := TFhirMeasureReportGroupStratifierStratumPopulation.Create()
  else if name = 'MeasureReport' then
    result := TFhirMeasureReport.Create()
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MEDIA}
  else if name = 'Media' then
    result := TFhirMedia.Create()
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATION}
  else if name = 'Medication.ingredient' then
    result := TFhirMedicationIngredient.Create()
  else if name = 'Medication.package' then
    result := TFhirMedicationPackage.Create()
  else if name = 'Medication.package.content' then
    result := TFhirMedicationPackageContent.Create()
  else if name = 'Medication.package.batch' then
    result := TFhirMedicationPackageBatch.Create()
  else if name = 'Medication' then
    result := TFhirMedication.Create()
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  else if name = 'MedicationAdministration.performer' then
    result := TFhirMedicationAdministrationPerformer.Create()
  else if name = 'MedicationAdministration.dosage' then
    result := TFhirMedicationAdministrationDosage.Create()
  else if name = 'MedicationAdministration' then
    result := TFhirMedicationAdministration.Create()
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  else if name = 'MedicationDispense.performer' then
    result := TFhirMedicationDispensePerformer.Create()
  else if name = 'MedicationDispense.substitution' then
    result := TFhirMedicationDispenseSubstitution.Create()
  else if name = 'MedicationDispense' then
    result := TFhirMedicationDispense.Create()
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  else if name = 'MedicationRequest.requester' then
    result := TFhirMedicationRequestRequester.Create()
  else if name = 'MedicationRequest.dispenseRequest' then
    result := TFhirMedicationRequestDispenseRequest.Create()
  else if name = 'MedicationRequest.substitution' then
    result := TFhirMedicationRequestSubstitution.Create()
  else if name = 'MedicationRequest' then
    result := TFhirMedicationRequest.Create()
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  else if name = 'MedicationStatement' then
    result := TFhirMedicationStatement.Create()
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_MESSAGEDEFINITION}
  else if name = 'MessageDefinition.focus' then
    result := TFhirMessageDefinitionFocus.Create()
  else if name = 'MessageDefinition.allowedResponse' then
    result := TFhirMessageDefinitionAllowedResponse.Create()
  else if name = 'MessageDefinition' then
    result := TFhirMessageDefinition.Create()
{$ENDIF FHIR_MESSAGEDEFINITION}
{$IFDEF FHIR_MESSAGEHEADER}
  else if name = 'MessageHeader.destination' then
    result := TFhirMessageHeaderDestination.Create()
  else if name = 'MessageHeader.source' then
    result := TFhirMessageHeaderSource.Create()
  else if name = 'MessageHeader.response' then
    result := TFhirMessageHeaderResponse.Create()
  else if name = 'MessageHeader' then
    result := TFhirMessageHeader.Create()
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_NAMINGSYSTEM}
  else if name = 'NamingSystem.uniqueId' then
    result := TFhirNamingSystemUniqueId.Create()
  else if name = 'NamingSystem' then
    result := TFhirNamingSystem.Create()
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_NUTRITIONORDER}
  else if name = 'NutritionOrder.oralDiet' then
    result := TFhirNutritionOrderOralDiet.Create()
  else if name = 'NutritionOrder.oralDiet.nutrient' then
    result := TFhirNutritionOrderOralDietNutrient.Create()
  else if name = 'NutritionOrder.oralDiet.texture' then
    result := TFhirNutritionOrderOralDietTexture.Create()
  else if name = 'NutritionOrder.supplement' then
    result := TFhirNutritionOrderSupplement.Create()
  else if name = 'NutritionOrder.enteralFormula' then
    result := TFhirNutritionOrderEnteralFormula.Create()
  else if name = 'NutritionOrder.enteralFormula.administration' then
    result := TFhirNutritionOrderEnteralFormulaAdministration.Create()
  else if name = 'NutritionOrder' then
    result := TFhirNutritionOrder.Create()
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_OBSERVATION}
  else if name = 'Observation.referenceRange' then
    result := TFhirObservationReferenceRange.Create()
  else if name = 'Observation.related' then
    result := TFhirObservationRelated.Create()
  else if name = 'Observation.component' then
    result := TFhirObservationComponent.Create()
  else if name = 'Observation' then
    result := TFhirObservation.Create()
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  else if name = 'OperationDefinition.parameter' then
    result := TFhirOperationDefinitionParameter.Create()
  else if name = 'OperationDefinition.parameter.binding' then
    result := TFhirOperationDefinitionParameterBinding.Create()
  else if name = 'OperationDefinition.overload' then
    result := TFhirOperationDefinitionOverload.Create()
  else if name = 'OperationDefinition' then
    result := TFhirOperationDefinition.Create()
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  else if name = 'OperationOutcome.issue' then
    result := TFhirOperationOutcomeIssue.Create()
  else if name = 'OperationOutcome' then
    result := TFhirOperationOutcome.Create()
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_ORGANIZATION}
  else if name = 'Organization.contact' then
    result := TFhirOrganizationContact.Create()
  else if name = 'Organization' then
    result := TFhirOrganization.Create()
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_PATIENT}
  else if name = 'Patient.contact' then
    result := TFhirPatientContact.Create()
  else if name = 'Patient.animal' then
    result := TFhirPatientAnimal.Create()
  else if name = 'Patient.communication' then
    result := TFhirPatientCommunication.Create()
  else if name = 'Patient.link' then
    result := TFhirPatientLink.Create()
  else if name = 'Patient' then
    result := TFhirPatient.Create()
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PAYMENTNOTICE}
  else if name = 'PaymentNotice' then
    result := TFhirPaymentNotice.Create()
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  else if name = 'PaymentReconciliation.detail' then
    result := TFhirPaymentReconciliationDetail.Create()
  else if name = 'PaymentReconciliation.processNote' then
    result := TFhirPaymentReconciliationProcessNote.Create()
  else if name = 'PaymentReconciliation' then
    result := TFhirPaymentReconciliation.Create()
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERSON}
  else if name = 'Person.link' then
    result := TFhirPersonLink.Create()
  else if name = 'Person' then
    result := TFhirPerson.Create()
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PLANDEFINITION}
  else if name = 'PlanDefinition.goal' then
    result := TFhirPlanDefinitionGoal.Create()
  else if name = 'PlanDefinition.goal.target' then
    result := TFhirPlanDefinitionGoalTarget.Create()
  else if name = 'PlanDefinition.action' then
    result := TFhirPlanDefinitionAction.Create()
  else if name = 'PlanDefinition.action.condition' then
    result := TFhirPlanDefinitionActionCondition.Create()
  else if name = 'PlanDefinition.action.relatedAction' then
    result := TFhirPlanDefinitionActionRelatedAction.Create()
  else if name = 'PlanDefinition.action.participant' then
    result := TFhirPlanDefinitionActionParticipant.Create()
  else if name = 'PlanDefinition.action.dynamicValue' then
    result := TFhirPlanDefinitionActionDynamicValue.Create()
  else if name = 'PlanDefinition' then
    result := TFhirPlanDefinition.Create()
{$ENDIF FHIR_PLANDEFINITION}
{$IFDEF FHIR_PRACTITIONER}
  else if name = 'Practitioner.qualification' then
    result := TFhirPractitionerQualification.Create()
  else if name = 'Practitioner' then
    result := TFhirPractitioner.Create()
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PRACTITIONERROLE}
  else if name = 'PractitionerRole.availableTime' then
    result := TFhirPractitionerRoleAvailableTime.Create()
  else if name = 'PractitionerRole.notAvailable' then
    result := TFhirPractitionerRoleNotAvailable.Create()
  else if name = 'PractitionerRole' then
    result := TFhirPractitionerRole.Create()
{$ENDIF FHIR_PRACTITIONERROLE}
{$IFDEF FHIR_PROCEDURE}
  else if name = 'Procedure.performer' then
    result := TFhirProcedurePerformer.Create()
  else if name = 'Procedure.focalDevice' then
    result := TFhirProcedureFocalDevice.Create()
  else if name = 'Procedure' then
    result := TFhirProcedure.Create()
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROCEDUREREQUEST}
  else if name = 'ProcedureRequest.requester' then
    result := TFhirProcedureRequestRequester.Create()
  else if name = 'ProcedureRequest' then
    result := TFhirProcedureRequest.Create()
{$ENDIF FHIR_PROCEDUREREQUEST}
{$IFDEF FHIR_PROCESSREQUEST}
  else if name = 'ProcessRequest.item' then
    result := TFhirProcessRequestItem.Create()
  else if name = 'ProcessRequest' then
    result := TFhirProcessRequest.Create()
{$ENDIF FHIR_PROCESSREQUEST}
{$IFDEF FHIR_PROCESSRESPONSE}
  else if name = 'ProcessResponse.processNote' then
    result := TFhirProcessResponseProcessNote.Create()
  else if name = 'ProcessResponse' then
    result := TFhirProcessResponse.Create()
{$ENDIF FHIR_PROCESSRESPONSE}
{$IFDEF FHIR_PROVENANCE}
  else if name = 'Provenance.agent' then
    result := TFhirProvenanceAgent.Create()
  else if name = 'Provenance.entity' then
    result := TFhirProvenanceEntity.Create()
  else if name = 'Provenance' then
    result := TFhirProvenance.Create()
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  else if name = 'Questionnaire.item' then
    result := TFhirQuestionnaireItem.Create()
  else if name = 'Questionnaire.item.enableWhen' then
    result := TFhirQuestionnaireItemEnableWhen.Create()
  else if name = 'Questionnaire.item.option' then
    result := TFhirQuestionnaireItemOption.Create()
  else if name = 'Questionnaire' then
    result := TFhirQuestionnaire.Create()
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  else if name = 'QuestionnaireResponse.item' then
    result := TFhirQuestionnaireResponseItem.Create()
  else if name = 'QuestionnaireResponse.item.answer' then
    result := TFhirQuestionnaireResponseItemAnswer.Create()
  else if name = 'QuestionnaireResponse' then
    result := TFhirQuestionnaireResponse.Create()
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REFERRALREQUEST}
  else if name = 'ReferralRequest.requester' then
    result := TFhirReferralRequestRequester.Create()
  else if name = 'ReferralRequest' then
    result := TFhirReferralRequest.Create()
{$ENDIF FHIR_REFERRALREQUEST}
{$IFDEF FHIR_RELATEDPERSON}
  else if name = 'RelatedPerson' then
    result := TFhirRelatedPerson.Create()
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_REQUESTGROUP}
  else if name = 'RequestGroup.action' then
    result := TFhirRequestGroupAction.Create()
  else if name = 'RequestGroup.action.condition' then
    result := TFhirRequestGroupActionCondition.Create()
  else if name = 'RequestGroup.action.relatedAction' then
    result := TFhirRequestGroupActionRelatedAction.Create()
  else if name = 'RequestGroup' then
    result := TFhirRequestGroup.Create()
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_RESEARCHSTUDY}
  else if name = 'ResearchStudy.arm' then
    result := TFhirResearchStudyArm.Create()
  else if name = 'ResearchStudy' then
    result := TFhirResearchStudy.Create()
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  else if name = 'ResearchSubject' then
    result := TFhirResearchSubject.Create()
{$ENDIF FHIR_RESEARCHSUBJECT}
{$IFDEF FHIR_RISKASSESSMENT}
  else if name = 'RiskAssessment.prediction' then
    result := TFhirRiskAssessmentPrediction.Create()
  else if name = 'RiskAssessment' then
    result := TFhirRiskAssessment.Create()
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_SCHEDULE}
  else if name = 'Schedule' then
    result := TFhirSchedule.Create()
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SEARCHPARAMETER}
  else if name = 'SearchParameter.component' then
    result := TFhirSearchParameterComponent.Create()
  else if name = 'SearchParameter' then
    result := TFhirSearchParameter.Create()
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SEQUENCE}
  else if name = 'Sequence.referenceSeq' then
    result := TFhirSequenceReferenceSeq.Create()
  else if name = 'Sequence.variant' then
    result := TFhirSequenceVariant.Create()
  else if name = 'Sequence.quality' then
    result := TFhirSequenceQuality.Create()
  else if name = 'Sequence.repository' then
    result := TFhirSequenceRepository.Create()
  else if name = 'Sequence' then
    result := TFhirSequence.Create()
{$ENDIF FHIR_SEQUENCE}
{$IFDEF FHIR_SERVICEDEFINITION}
  else if name = 'ServiceDefinition' then
    result := TFhirServiceDefinition.Create()
{$ENDIF FHIR_SERVICEDEFINITION}
{$IFDEF FHIR_SLOT}
  else if name = 'Slot' then
    result := TFhirSlot.Create()
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  else if name = 'Specimen.collection' then
    result := TFhirSpecimenCollection.Create()
  else if name = 'Specimen.processing' then
    result := TFhirSpecimenProcessing.Create()
  else if name = 'Specimen.container' then
    result := TFhirSpecimenContainer.Create()
  else if name = 'Specimen' then
    result := TFhirSpecimen.Create()
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  else if name = 'StructureDefinition.mapping' then
    result := TFhirStructureDefinitionMapping.Create()
  else if name = 'StructureDefinition.snapshot' then
    result := TFhirStructureDefinitionSnapshot.Create()
  else if name = 'StructureDefinition.differential' then
    result := TFhirStructureDefinitionDifferential.Create()
  else if name = 'StructureDefinition' then
    result := TFhirStructureDefinition.Create()
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_STRUCTUREMAP}
  else if name = 'StructureMap.structure' then
    result := TFhirStructureMapStructure.Create()
  else if name = 'StructureMap.group' then
    result := TFhirStructureMapGroup.Create()
  else if name = 'StructureMap.group.input' then
    result := TFhirStructureMapGroupInput.Create()
  else if name = 'StructureMap.group.rule' then
    result := TFhirStructureMapGroupRule.Create()
  else if name = 'StructureMap.group.rule.source' then
    result := TFhirStructureMapGroupRuleSource.Create()
  else if name = 'StructureMap.group.rule.target' then
    result := TFhirStructureMapGroupRuleTarget.Create()
  else if name = 'StructureMap.group.rule.target.parameter' then
    result := TFhirStructureMapGroupRuleTargetParameter.Create()
  else if name = 'StructureMap.group.rule.dependent' then
    result := TFhirStructureMapGroupRuleDependent.Create()
  else if name = 'StructureMap' then
    result := TFhirStructureMap.Create()
{$ENDIF FHIR_STRUCTUREMAP}
{$IFDEF FHIR_SUBSCRIPTION}
  else if name = 'Subscription.channel' then
    result := TFhirSubscriptionChannel.Create()
  else if name = 'Subscription' then
    result := TFhirSubscription.Create()
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSTANCE}
  else if name = 'Substance.instance' then
    result := TFhirSubstanceInstance.Create()
  else if name = 'Substance.ingredient' then
    result := TFhirSubstanceIngredient.Create()
  else if name = 'Substance' then
    result := TFhirSubstance.Create()
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUPPLYDELIVERY}
  else if name = 'SupplyDelivery.suppliedItem' then
    result := TFhirSupplyDeliverySuppliedItem.Create()
  else if name = 'SupplyDelivery' then
    result := TFhirSupplyDelivery.Create()
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  else if name = 'SupplyRequest.orderedItem' then
    result := TFhirSupplyRequestOrderedItem.Create()
  else if name = 'SupplyRequest.requester' then
    result := TFhirSupplyRequestRequester.Create()
  else if name = 'SupplyRequest' then
    result := TFhirSupplyRequest.Create()
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TASK}
  else if name = 'Task.requester' then
    result := TFhirTaskRequester.Create()
  else if name = 'Task.restriction' then
    result := TFhirTaskRestriction.Create()
  else if name = 'Task.input' then
    result := TFhirTaskInput.Create()
  else if name = 'Task.output' then
    result := TFhirTaskOutput.Create()
  else if name = 'Task' then
    result := TFhirTask.Create()
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TESTREPORT}
  else if name = 'TestReport.participant' then
    result := TFhirTestReportParticipant.Create()
  else if name = 'TestReport.setup' then
    result := TFhirTestReportSetup.Create()
  else if name = 'TestReport.setup.action' then
    result := TFhirTestReportSetupAction.Create()
  else if name = 'TestReport.setup.action.operation' then
    result := TFhirTestReportSetupActionOperation.Create()
  else if name = 'TestReport.setup.action.assert' then
    result := TFhirTestReportSetupActionAssert.Create()
  else if name = 'TestReport.test' then
    result := TFhirTestReportTest.Create()
  else if name = 'TestReport.test.action' then
    result := TFhirTestReportTestAction.Create()
  else if name = 'TestReport.teardown' then
    result := TFhirTestReportTeardown.Create()
  else if name = 'TestReport.teardown.action' then
    result := TFhirTestReportTeardownAction.Create()
  else if name = 'TestReport' then
    result := TFhirTestReport.Create()
{$ENDIF FHIR_TESTREPORT}
{$IFDEF FHIR_TESTSCRIPT}
  else if name = 'TestScript.origin' then
    result := TFhirTestScriptOrigin.Create()
  else if name = 'TestScript.destination' then
    result := TFhirTestScriptDestination.Create()
  else if name = 'TestScript.metadata' then
    result := TFhirTestScriptMetadata.Create()
  else if name = 'TestScript.metadata.link' then
    result := TFhirTestScriptMetadataLink.Create()
  else if name = 'TestScript.metadata.capability' then
    result := TFhirTestScriptMetadataCapability.Create()
  else if name = 'TestScript.fixture' then
    result := TFhirTestScriptFixture.Create()
  else if name = 'TestScript.variable' then
    result := TFhirTestScriptVariable.Create()
  else if name = 'TestScript.rule' then
    result := TFhirTestScriptRule.Create()
  else if name = 'TestScript.rule.param' then
    result := TFhirTestScriptRuleParam.Create()
  else if name = 'TestScript.ruleset' then
    result := TFhirTestScriptRuleset.Create()
  else if name = 'TestScript.ruleset.rule' then
    result := TFhirTestScriptRulesetRule.Create()
  else if name = 'TestScript.ruleset.rule.param' then
    result := TFhirTestScriptRulesetRuleParam.Create()
  else if name = 'TestScript.setup' then
    result := TFhirTestScriptSetup.Create()
  else if name = 'TestScript.setup.action' then
    result := TFhirTestScriptSetupAction.Create()
  else if name = 'TestScript.setup.action.operation' then
    result := TFhirTestScriptSetupActionOperation.Create()
  else if name = 'TestScript.setup.action.operation.requestHeader' then
    result := TFhirTestScriptSetupActionOperationRequestHeader.Create()
  else if name = 'TestScript.setup.action.assert' then
    result := TFhirTestScriptSetupActionAssert.Create()
  else if name = 'TestScript.setup.action.assert.rule' then
    result := TFhirTestScriptSetupActionAssertRule.Create()
  else if name = 'TestScript.setup.action.assert.rule.param' then
    result := TFhirTestScriptSetupActionAssertRuleParam.Create()
  else if name = 'TestScript.setup.action.assert.ruleset' then
    result := TFhirTestScriptSetupActionAssertRuleset.Create()
  else if name = 'TestScript.setup.action.assert.ruleset.rule' then
    result := TFhirTestScriptSetupActionAssertRulesetRule.Create()
  else if name = 'TestScript.setup.action.assert.ruleset.rule.param' then
    result := TFhirTestScriptSetupActionAssertRulesetRuleParam.Create()
  else if name = 'TestScript.test' then
    result := TFhirTestScriptTest.Create()
  else if name = 'TestScript.test.action' then
    result := TFhirTestScriptTestAction.Create()
  else if name = 'TestScript.teardown' then
    result := TFhirTestScriptTeardown.Create()
  else if name = 'TestScript.teardown.action' then
    result := TFhirTestScriptTeardownAction.Create()
  else if name = 'TestScript' then
    result := TFhirTestScript.Create()
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  else if name = 'ValueSet.compose' then
    result := TFhirValueSetCompose.Create()
  else if name = 'ValueSet.compose.include' then
    result := TFhirValueSetComposeInclude.Create()
  else if name = 'ValueSet.compose.include.concept' then
    result := TFhirValueSetComposeIncludeConcept.Create()
  else if name = 'ValueSet.compose.include.concept.designation' then
    result := TFhirValueSetComposeIncludeConceptDesignation.Create()
  else if name = 'ValueSet.compose.include.filter' then
    result := TFhirValueSetComposeIncludeFilter.Create()
  else if name = 'ValueSet.expansion' then
    result := TFhirValueSetExpansion.Create()
  else if name = 'ValueSet.expansion.parameter' then
    result := TFhirValueSetExpansionParameter.Create()
  else if name = 'ValueSet.expansion.contains' then
    result := TFhirValueSetExpansionContains.Create()
  else if name = 'ValueSet' then
    result := TFhirValueSet.Create()
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  else if name = 'VisionPrescription.dispense' then
    result := TFhirVisionPrescriptionDispense.Create()
  else if name = 'VisionPrescription' then
    result := TFhirVisionPrescription.Create()
{$ENDIF FHIR_VISIONPRESCRIPTION}

  else
    result := nil;
end;



end.

