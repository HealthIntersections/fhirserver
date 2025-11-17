unit fhir5_factory;

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
{$I fhir5.inc}

interface

// FHIR v5.0.0 generated 2018-05-15T06:58:00+10:00

uses
  SysUtils, Classes, System.NetEncoding,
  fsl_base, fsl_utilities, fsl_stream, fsl_http, fsl_i18n,
  fsl_ucum, fsl_npm, fsl_threads, fsl_web_stream,
  fhir_objects, fhir_parser, fhir_validator, fhir_narrative, fhir_factory, fhir_pathengine, fhir_xhtml, fhir_common,  fhir_elementmodel,
  fhir_client, fhir_client_threaded, fhir_uris;

type

  { TFHIRFactoryR5 }

  TFHIRFactoryR5 = class (TFHIRFactory)
  public
    function link : TFHIRFactoryR5; overload;
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
    function makeProxy(packageId : string; resource : TFHIRResourceV) : TFHIRResourceProxyV; override;

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
    function makeDate(value : TFslDateTime) : TFHIRObject; override;
    function makeDateTime(value : TFslDateTime) : TFHIRObject; override;
    function makeParameters : TFHIRParametersW; override;
    function wrapPrimitive(p :TFHIRObject) : TFHIRPrimitiveW; override;
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
    function makeDuration(dt : TDateTime) : TFHIRObject; override;
    function wrapPeriod(r : TFHIRObject) : TFhirPeriodW; override;
    function wrapAttachment(r : TFHIRObject) : TFHIRAttachmentW; override;
    function makeValueSetContains : TFhirValueSetExpansionContainsW; override;
    function makeBundle(list : TFslList<TFHIRResourceV>) : TFHIRBundleW; override;
    function wrapImmunization(o : TFHIRResourceV) : TFhirImmunizationW; override;
  end;
  TFHIRFactoryX = TFHIRFactoryR5;

implementation

uses
  fhir_client_http,
  fhir5_enums, fhir5_types, fhir5_resources, fhir5_resources_base, fhir5_parser, fhir5_context, fhir5_validator, fhir5_profiles, fhir5_operations,
  fhir5_narrative, fhir5_pathengine, fhir5_constants, fhir5_client, fhir5_common, fhir5_utilities, fhir5_authmap,
  fhir5_elementmodel;

{ TFHIRFactoryR5 }

function TFHIRFactoryR5.buildOperationOutcome(langList : THTTPLanguageList; message: String; issueCode: TFhirIssueType): TFhirResourceV;
begin
  result := fhir5_utilities.BuildOperationOutcome(langList, message, ExceptionTypeTranslations[issueCode]);
end;

function TFHIRFactoryR5.buildOperationOutcome(i18n: TI18nSupport; langList: THTTPLanguageList; exception: EFHIROperationException): TFhirResourceV;
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
      begin
        exception.Message := i18n.translate(exception.msgId, langlist, exception.Params);
        iss.details.text := exception.Message;
      end
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

function TFHIRFactoryR5.buildOperationOutcome(langList : THTTPLanguageList; e: Exception; issueCode: TFhirIssueType): TFhirResourceV;
begin
  result := fhir5_utilities.BuildOperationOutcome(langList, e, ExceptionTypeTranslations[issueCode]);
end;

function TFHIRFactoryR5.canonicalResources: TArray<String>;
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

procedure TFHIRFactoryR5.checkNoModifiers(res: TFHIRObject; method, param: string; allowed : TArray<String> = nil);
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

function TFHIRFactoryR5.corePackage: String;
begin
  result := 'hl7.fhir.r5.core';
end;

function TFHIRFactoryR5.createFromProfile(worker: TFHIRWorkerContextV; profile: TFhirStructureDefinitionW): TFHIRResourceV;
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

function TFHIRFactoryR5.describe(r: TFHIRResourceV): String;
begin
  result := describeResource(r as TFHIRResource);
end;

function TFHIRFactoryR5.description: String;
begin
  result := 'R5 ('+FHIR_GENERATED_VERSION+')';
end;

function TFHIRFactoryR5.getContained(r: TFHIRResourceV): TFslList<TFHIRResourceV>;
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

function TFHIRFactoryR5.getXhtml(res: TFHIRResourceV): TFHIRXhtmlNode;
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

function TFHIRFactoryR5.link: TFHIRFactoryR5;
begin
  result := TFHIRFactoryR5(inherited link);
end;

function TFHIRFactoryR5.makeClient(worker: TFHIRWorkerContextV; url: String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout: cardinal; proxy: String): TFhirClientV;
var
  http : TFHIRHTTPCommunicator;
begin
  http := TFHIRHTTPCommunicator.Create(url);
  try
    if kind = fctCrossPlatform then
      http.UseIndy := true;
    http.timeout := timeout;
    http.proxy := proxy;
    result := TFhirClient5.Create(worker, nil, http.link);
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

function TFHIRFactoryR5.makeClientInt(worker: TFHIRWorkerContextV; langList : THTTPLanguageList; comm: TFHIRClientCommunicator): TFhirClientV;
begin
  result := TFhirClient5.Create(worker, nil, comm);
end;

function TFHIRFactoryR5.makeClientThreaded(worker: TFHIRWorkerContextV; internal: TFhirClientV; event: TThreadManagementEvent): TFhirClientV;
var
  c : TFhirThreadedCommunicator;
begin
  c := TFhirThreadedCommunicator.Create(internal, event);
  try
    result := TFhirClient5.Create(worker, nil, c.link);
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

function TFHIRFactoryR5.makeCode(s: string): TFHIRObject;
begin
  result := TFhirCode.Create(s);
end;

function TFHIRFactoryR5.makeCoding(system, version, code, display: String): TFHIRObject;
begin
  result := TFHIRCoding.Create(system, code);
  if version <> '' then
    TFHIRCoding(result).version := version;
  if display <> '' then
    TFHIRCoding(result).display := display;
end;

function TFHIRFactoryR5.makeCodeableConcept(coding: TFHIRCodingW): TFHIRObject;
begin
  result := TFHIRCodeableConcept.Create;
  if (coding <> nil) then
    TFHIRCodeableConcept(result).codingList.add(coding.element.link);
end;

function TFHIRFactoryR5.makeComposer(worker: TFHIRWorkerContextV; format: TFHIRFormat; langList : THTTPLanguageList; style: TFHIROutputStyle): TFHIRComposer;
begin
  result := TFHIRParsers5.composer(worker.link as TFHIRWorkerContext, format, langList.link, style);
end;

function TFHIRFactoryR5.makeDate(value: TFslDateTime): TFHIRObject;
begin
  result := TFhirDate.Create(value);
end;

function TFHIRFactoryR5.makeDateTime(value: TFslDateTime): TFHIRObject;
begin
  result := TFhirDateTime.Create(value);
end;

function TFHIRFactoryR5.makeDecimal(s: string): TFHIRObject;
begin
  result := TFhirDecimal.Create(s);
end;

function TFHIRFactoryR5.makeDtFromForm(part: TMimePart; langList : THTTPLanguageList; name: String; type_: string): TFHIRXVersionElementWrapper;
begin
  if type_ = 'Coding' then
    result := wrapCoding(LoadDTFromFormParam(nil, part, langList, name, TFhirCoding))
  else if type_ = 'CodeableConcept' then
    result := wrapCodeableConcept(LoadDTFromFormParam(nil, part, langList, name, TFhirCodeableConcept))
  else
    raise EFHIRException.Create('Unknown Supported Data Type '+type_);
end;

function TFHIRFactoryR5.makeDuration(dt: TDateTime): TFHIRObject;
begin
  result := TFhirQuantity.fromDuration(dt);
end;

function TFHIRFactoryR5.makeElementModelManager: TFHIRBaseMMManager;
begin
  result := TFHIRMMManager.Create;
end;

function TFHIRFactoryR5.makeGenerator(worker: TFHIRWorkerContextV): TFHIRNarrativeGeneratorBase;
begin
  result := TFHIRNarrativeGenerator.Create(worker);
end;

function TFHIRFactoryR5.makeHealthcareCard: THealthcareCard;
begin
  raise EFslException.Create('Healthcare Cards are not supported in version '+versionString);
end;

function TFHIRFactoryR5.makeProxy(packageId : String; pi: TNpmPackageResource; worker : TFHIRWorkerContextV; lock: TFslLock): TFHIRResourceProxyV;
begin
  result := TNpmResourceProxy.Create(packageId, self.link, lock, worker, pi);
end;

function TFHIRFactoryR5.makeProxy(packageId : String; resource : TFHIRResourceV) : TFHIRResourceProxyV;
begin
  result := TFHIRResourceProxy.Create(packageId, self.link, resource as TFHIRResource);
end;

function TFHIRFactoryR5.makeInteger(s: string): TFHIRObject;
begin
  result := TFhirInteger.Create(s);
end;

function TFHIRFactoryR5.makeIssue(level : TIssueSeverity; issue: TFhirIssueType; location, message: String): TFhirOperationOutcomeIssueW;
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
    result := TFhirOperationOutcomeIssue5.Create(iss.Link);
  finally
    iss.free;
  end;
end;

function TFHIRFactoryR5.makeOpReqLookup: TFHIRLookupOpRequestW;
begin
  result := TFHIRLookupOpRequest5.Create(TFHIRLookupOpRequest.create);
end;

function TFHIRFactoryR5.makeOpReqSubsumes: TFHIRSubsumesOpRequestW;
begin
  result := TFHIRSubsumesOpRequest5.Create(TFHIRSubsumesOpRequest.create);
end;

function TFHIRFactoryR5.makeOpRespLookup: TFHIRLookupOpResponseW;
begin
  result := TFHIRLookupOpResponse5.Create(TFHIRLookupOpResponse.create);
end;

function TFHIRFactoryR5.makeOpRespSubsumes: TFHIRSubsumesOpResponseW;
begin
  result := TFHIRSubsumesOpResponse5.Create(TFHIRSubsumesOpResponse.create);
end;

function TFHIRFactoryR5.makeParameters: TFHIRParametersW;
begin
  result := TFHIRParameters5.Create(TFHIRParameters.Create);
end;

function TFHIRFactoryR5.wrapPrimitive(p: TFHIRObject): TFHIRPrimitiveW;
begin
  if (p = nil) then
    result := nil
  else
    result := TFHIRPrimitive5.Create(p);
end;

function TFHIRFactoryR5.makeParamsFromForm(s: TStream): TFHIRResourceV;
begin
  result := parseParamsFromForm(s);
end;

function TFHIRFactoryR5.makeParser(worker: TFHIRWorkerContextV; format: TFHIRFormat; langList : THTTPLanguageList): TFHIRParser;
begin
  result := TFHIRParsers5.parser(worker.link as TFHIRWorkerContext, format, langList.link);
end;

function TFHIRFactoryR5.makePathEngine(worker: TFHIRWorkerContextV; ucum : TUcumServiceInterface): TFHIRPathEngineV;
begin
  result := TFHIRPathEngine.Create(worker as TFHIRWorkerContext, ucum);
end;

function TFHIRFactoryR5.makeString(s: string): TFHIRObject;
begin
  if (s = '') then
    result := nil
  else
    result := TFhirString.Create(s);
end;

function TFHIRFactoryR5.makeTerminologyCapablities: TFhirTerminologyCapabilitiesW;
begin
  result := TFhirTerminologyCapabilities5.Create(TFhirTerminologyCapabilities.create);
end;

function TFHIRFactoryR5.makeUri(s: string): TFHIRObject;
begin
  result := TFhirUri.Create(s);
end;

function TFHIRFactoryR5.makeValidator(worker: TFHIRWorkerContextV): TFHIRValidatorV;
begin
  result := TFHIRValidator5.Create(worker as TFHIRWorkerContext);
end;

function TFHIRFactoryR5.makeValueSetContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains5.Create(TFhirValueSetExpansionContains.create);
end;

procedure TFHIRFactoryR5.markWithTag(r : TFHIRResourceV; systemUri, code, display: String);
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

function TFHIRFactoryR5.resCategory(name: String): TTokenCategory;
var
  a : TFhirResourceType;
begin
  for a in ALL_RESOURCE_TYPES do
    if CODES_TFhirResourceType[a] = name then
      exit(RESOURCE_CATEGORY[a]);
  result := tcOther;
end;

procedure TFHIRFactoryR5.clearXhtml(res : TFHIRResourceV);
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

function TFHIRFactoryR5.resetXhtml(res: TFHIRResourceV): TFHIRXhtmlNode;
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

function TFHIRFactoryR5.resourceNames: TArray<String>;
var
  a : TFhirResourceType;
begin
  SetLength(result, length(CODES_TFhirResourceType)-2);
  for a in ALL_RESOURCE_TYPES do
    if not (a in [frtNull, frtCustom]) then
      result[ord(a)-1] := CODES_TFhirResourceType[a];
end;

procedure TFHIRFactoryR5.setXhtml(res: TFHIRResourceV; x: TFHIRXhtmlNode);
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

function TFHIRFactoryR5.specUrl: String;
begin
  result := 'http://build.fhir.org';
end;

function TFHIRFactoryR5.URLs: TCommonURLs;
begin
  result.SecuritySourceType := URI_FHIR_SECURITY_SOURCE_TYPE_R4;
end;

function TFHIRFactoryR5.version: TFHIRVersion;
begin
  result := fhirVersionRelease5;
end;

function TFHIRFactoryR5.versionName: String;
begin
  result := 'R5';
end;

function TFHIRFactoryR5.versionString: String;
begin
  result := FHIR_GENERATED_VERSION;
end;

function TFHIRFactoryR5.wrapAttachment(r : TFHIRObject) : TFHIRAttachmentW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirAttachment5.Create(r);
end;

function TFHIRFactoryR5.wrapAuditEvent(r: TFHIRResourceV): TFhirAuditEventW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirAuditEvent5.Create(r);
end;

function TFHIRFactoryR5.wrapBinary(r: TFHIRResourceV): TFhirBinaryW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirBinary5.Create(r);
end;

function TFHIRFactoryR5.wrapBundle(r: TFHIRResourceV): TFhirBundleW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirBundle5.Create(r);
end;

function TFHIRFactoryR5.wrapBundleEntry(o: TFHIRObject): TFhirBundleEntryW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRBundleEntry5.Create(o);
end;

function TFHIRFactoryR5.wrapCapabilityStatement(r: TFHIRResourceV): TFHIRCapabilityStatementW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRCapabilityStatement5.Create(r);
end;

function TFHIRFactoryR5.wrapCodeableConcept(o: TFHIRObject): TFhirCodeableConceptW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirCodeableConcept5.Create(o);
end;

function TFHIRFactoryR5.wrapCodeSystem(r: TFHIRResourceV): TFhirCodeSystemW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRCodeSystem5.Create(r);
end;

function TFHIRFactoryR5.wrapCoding(o: TFHIRObject): TFhirCodingW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirCoding5.Create(o);
end;

function TFHIRFactoryR5.wrapConceptMap(r: TFHIRResourceV): TFhirConceptMapW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirConceptMap5.Create(r);
end;

function TFHIRFactoryR5.wrapConsent(o: TFHIRResourceV): TFHIRConsentW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRConsent5.Create(o);
end;

function TFHIRFactoryR5.wrapEncounter(r: TFHIRResourceV): TFhirEncounterW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirEncounter5.Create(r);
end;

function TFHIRFactoryR5.wrapEventDefinition(o: TFHIRResourceV): TFHIREventDefinitionW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIREventDefinition5.Create(o);
end;

function TFHIRFactoryR5.wrapExtension(o: TFHIRObject): TFhirExtensionW;
begin
  result := TFhirExtension5.Create(o);
end;

function TFHIRFactoryR5.wrapGroup(r: TFHIRResourceV): TFhirGroupW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirGroup5.Create(r);
end;

function TFHIRFactoryR5.wrapIdentifier(o: TFHIRObject): TFhirIdentifierW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirIdentifier5.Create(o);
end;

function TFHIRFactoryR5.wrapImmunization(o: TFHIRResourceV): TFhirImmunizationW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirImmunization5.Create(o);
end;

function TFHIRFactoryR5.wrapMeta(r: TFHIRObject): TFhirMetaW;
begin
  if r = nil then
    result := nil
  else if r.isResource then
  begin
    result := TFHIRMeta5.Create((r as TFHIRResource).meta.link);
    TFHIRMeta5(result).resource := (r as TFHIRResource).link;
  end
  else
    result := TFHIRMeta5.Create((r as TFhirMeta))
end;

function TFHIRFactoryR5.wrapNamingSystem(o: TFHIRResourceV): TFHIRNamingSystemW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRNamingSystem5.Create(o);
end;

function TFHIRFactoryR5.wrapMeta(r: TFHIRResourceV): TFhirMetaW;
begin
  result := TFHIRMeta5.Create((r as TFHIRResource).meta.link);
  TFHIRMeta5(result).resource := (r as TFHIRResource).link;
end;

function TFHIRFactoryR5.wrapObservation(r: TFHIRResourceV): TFhirObservationW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirObservation5.Create(r);
end;

function TFHIRFactoryR5.wrapOperationOutcome(r: TFHIRResourceV): TFhirOperationOutcomeW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirOperationOutcome5.Create(r);
end;

function TFHIRFactoryR5.wrapParams(r: TFHIRResourceV): TFHIRParametersW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirParameters5.Create(r);
end;

function TFHIRFactoryR5.wrapPatient(r: TFHIRResourceV): TFhirPatientW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirPatient5.Create(r);
end;

function TFHIRFactoryR5.wrapPeriod(r: TFHIRObject): TFhirPeriodW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirPeriod5.Create(r);
end;

function TFHIRFactoryR5.wrapProvenance(o: TFHIRResourceV): TFHIRProvenanceW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRProvenance5.Create(o);
end;

function TFHIRFactoryR5.wrapQuantity(r: TFHIRObject): TFhirQuantityW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirQuantity5.Create(r);
end;

function TFHIRFactoryR5.wrapStructureDefinition(r: TFHIRResourceV): TFhirStructureDefinitionW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRStructureDefinition5.Create(r);
end;

function TFHIRFactoryR5.wrapStructureMap(o: TFHIRResourceV): TFHIRStructureMapW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRStructureMap5.Create(o);
end;

function TFHIRFactoryR5.wrapSubscription(r: TFHIRResourceV): TFhirSubscriptionW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirSubscription5.Create(r);
end;

function TFHIRFactoryR5.wrapSubscriptionTopic(r: TFHIRResourceV): TFhirSubscriptionTopicW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirSubscriptionTopic5.Create(r as TFhirSubscriptionTopic);
end;

function TFHIRFactoryR5.wrapTestScript(o: TFHIRResourceV): TFHIRTestScriptW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirTestScript5.Create(o);
end;

function TFHIRFactoryR5.wrapValueSet(r: TFHIRResourceV): TFhirValueSetW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRValueSet5.Create(r);
end;

function TFHIRFactoryR5.makeBase64Binary(s: string): TFHIRObject;
begin
  result := TFhirBase64Binary.Create(decodeBase64(AnsiString(s)));
end;

function TFHIRFactoryR5.makeBinary(content: TBytes; contentType: String): TFHIRResourceV;
begin
  result := TFhirBinary.Create;
  try
    TFhirBinary(result).data := content;
    TFhirBinary(result).contentType := contentType;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactoryR5.makeBoolean(b: boolean): TFHIRObject;
begin
  result := TFhirBoolean.Create(b);
end;

function TFHIRFactoryR5.makeBundle(list: TFslList<TFHIRResourceV>): TFHIRBundleW;
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
    result := TFHIRBundle5.Create(bnd.link);
  finally
    bnd.free;
  end;
end;

function TFHIRFactoryR5.makeByName(const name : String) : TFHIRObject;
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
  else if name = 'integer64' then
    result := TFhirInteger64.Create()
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
  else if name = 'canonical' then
    result := TFhirCanonical.Create()
  else if name = 'oid' then
    result := TFhirOid.Create()
  else if name = 'uuid' then
    result := TFhirUuid.Create()
  else if name = 'url' then
    result := TFhirUrl.Create()
  else if name = 'markdown' then
    result := TFhirMarkdown.Create()
  else if name = 'unsignedInt' then
    result := TFhirUnsignedInt.Create()
  else if name = 'id' then
    result := TFhirId.Create()
  else if name = 'positiveInt' then
    result := TFhirPositiveInt.Create()
{gen-factory-start}
  else if name = 'Address' then
    result := TFhirAddress.Create()
  else if name = 'Annotation' then
    result := TFhirAnnotation.Create()
  else if name = 'Attachment' then
    result := TFhirAttachment.Create()
  else if name = 'Availability' then
    result := TFhirAvailability.Create()
  else if name = 'CodeableConcept' then
    result := TFhirCodeableConcept.Create()
  else if name = 'CodeableReference' then
    result := TFhirCodeableReference.Create()
  else if name = 'Coding' then
    result := TFhirCoding.Create()
  else if name = 'ContactDetail' then
    result := TFhirContactDetail.Create()
  else if name = 'ContactPoint' then
    result := TFhirContactPoint.Create()
  else if name = 'Contributor' then
    result := TFhirContributor.Create()
  else if name = 'DataRequirement' then
    result := TFhirDataRequirement.Create()
  else if name = 'Expression' then
    result := TFhirExpression.Create()
  else if name = 'ExtendedContactDetail' then
    result := TFhirExtendedContactDetail.Create()
  else if name = 'Extension' then
    result := TFhirExtension.Create()
  else if name = 'HumanName' then
    result := TFhirHumanName.Create()
  else if name = 'Identifier' then
    result := TFhirIdentifier.Create()
  else if name = 'Meta' then
    result := TFhirMeta.Create()
  else if name = 'MonetaryComponent' then
    result := TFhirMonetaryComponent.Create()
  else if name = 'Money' then
    result := TFhirMoney.Create()
  else if name = 'Narrative' then
    result := TFhirNarrative.Create()
  else if name = 'ParameterDefinition' then
    result := TFhirParameterDefinition.Create()
  else if name = 'Period' then
    result := TFhirPeriod.Create()
  else if name = 'Quantity' then
    result := TFhirQuantity.Create()
  else if name = 'Range' then
    result := TFhirRange.Create()
  else if name = 'Ratio' then
    result := TFhirRatio.Create()
  else if name = 'RatioRange' then
    result := TFhirRatioRange.Create()
  else if name = 'Reference' then
    result := TFhirReference.Create()
  else if name = 'RelatedArtifact' then
    result := TFhirRelatedArtifact.Create()
  else if name = 'SampledData' then
    result := TFhirSampledData.Create()
  else if name = 'Signature' then
    result := TFhirSignature.Create()
  else if name = 'TriggerDefinition' then
    result := TFhirTriggerDefinition.Create()
  else if name = 'UsageContext' then
    result := TFhirUsageContext.Create()
  else if name = 'VirtualServiceDetail' then
    result := TFhirVirtualServiceDetail.Create()
  else if name = 'Age' then
    result := TFhirAge.Create()
  else if name = 'Count' then
    result := TFhirCount.Create()
  else if name = 'Distance' then
    result := TFhirDistance.Create()
  else if name = 'Dosage' then
    result := TFhirDosage.Create()
  else if name = 'Duration' then
    result := TFhirDuration.Create()
  else if name = 'ElementDefinition' then
    result := TFhirElementDefinition.Create()
  else if name = 'MarketingStatus' then
    result := TFhirMarketingStatus.Create()
  else if name = 'Population' then
    result := TFhirPopulation.Create()
  else if name = 'ProductShelfLife' then
    result := TFhirProductShelfLife.Create()
  else if name = 'Timing' then
    result := TFhirTiming.Create()
{$IFDEF FHIR_RESOURCE}
{$ENDIF FHIR_RESOURCE}
{$IFDEF FHIR_DOMAINRESOURCE}
{$ENDIF FHIR_DOMAINRESOURCE}
{$IFDEF FHIR_CANONICALRESOURCE}
{$ENDIF FHIR_CANONICALRESOURCE}
{$IFDEF FHIR_METADATARESOURCE}
{$ENDIF FHIR_METADATARESOURCE}
{$IFDEF FHIR_ACCOUNT}
  else if name = 'Account.coverage' then
    result := TFhirAccountCoverage.Create()
  else if name = 'Account.guarantor' then
    result := TFhirAccountGuarantor.Create()
  else if name = 'Account.relatedAccount' then
    result := TFhirAccountRelatedAccount.Create()
  else if name = 'Account.balance' then
    result := TFhirAccountBalance.Create()
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
{$IFDEF FHIR_ACTORDEFINITION}
  else if name = 'ActorDefinition' then
    result := TFhirActorDefinition.Create()
{$ENDIF FHIR_ACTORDEFINITION}
{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
  else if name = 'AdministrableProductDefinition.property' then
    result := TFhirAdministrableProductDefinitionProperty.Create()
  else if name = 'AdministrableProductDefinition.routeOfAdministration' then
    result := TFhirAdministrableProductDefinitionRouteOfAdministration.Create()
  else if name = 'AdministrableProductDefinition.routeOfAdministration.targetSpecies' then
    result := TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpecies.Create()
  else if name = 'AdministrableProductDefinition.routeOfAdministration.targetSpecies.withdrawalPeriod' then
    result := TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriod.Create()
  else if name = 'AdministrableProductDefinition' then
    result := TFhirAdministrableProductDefinition.Create()
{$ENDIF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
{$IFDEF FHIR_ADVERSEEVENT}
  else if name = 'AdverseEvent.participant' then
    result := TFhirAdverseEventParticipant.Create()
  else if name = 'AdverseEvent.suspectEntity' then
    result := TFhirAdverseEventSuspectEntity.Create()
  else if name = 'AdverseEvent.suspectEntity.causality' then
    result := TFhirAdverseEventSuspectEntityCausality.Create()
  else if name = 'AdverseEvent.contributingFactor' then
    result := TFhirAdverseEventContributingFactor.Create()
  else if name = 'AdverseEvent.preventiveAction' then
    result := TFhirAdverseEventPreventiveAction.Create()
  else if name = 'AdverseEvent.mitigatingAction' then
    result := TFhirAdverseEventMitigatingAction.Create()
  else if name = 'AdverseEvent.supportingInfo' then
    result := TFhirAdverseEventSupportingInfo.Create()
  else if name = 'AdverseEvent' then
    result := TFhirAdverseEvent.Create()
{$ENDIF FHIR_ADVERSEEVENT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  else if name = 'AllergyIntolerance.participant' then
    result := TFhirAllergyIntoleranceParticipant.Create()
  else if name = 'AllergyIntolerance.reaction' then
    result := TFhirAllergyIntoleranceReaction.Create()
  else if name = 'AllergyIntolerance' then
    result := TFhirAllergyIntolerance.Create()
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  else if name = 'Appointment.participant' then
    result := TFhirAppointmentParticipant.Create()
  else if name = 'Appointment.recurrenceTemplate' then
    result := TFhirAppointmentRecurrenceTemplate.Create()
  else if name = 'Appointment.recurrenceTemplate.weeklyTemplate' then
    result := TFhirAppointmentRecurrenceTemplateWeeklyTemplate.Create()
  else if name = 'Appointment.recurrenceTemplate.monthlyTemplate' then
    result := TFhirAppointmentRecurrenceTemplateMonthlyTemplate.Create()
  else if name = 'Appointment.recurrenceTemplate.yearlyTemplate' then
    result := TFhirAppointmentRecurrenceTemplateYearlyTemplate.Create()
  else if name = 'Appointment' then
    result := TFhirAppointment.Create()
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  else if name = 'AppointmentResponse' then
    result := TFhirAppointmentResponse.Create()
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_ARTIFACTASSESSMENT}
  else if name = 'ArtifactAssessment.content' then
    result := TFhirArtifactAssessmentContent.Create()
  else if name = 'ArtifactAssessment' then
    result := TFhirArtifactAssessment.Create()
{$ENDIF FHIR_ARTIFACTASSESSMENT}
{$IFDEF FHIR_AUDITEVENT}
  else if name = 'AuditEvent.outcome' then
    result := TFhirAuditEventOutcome.Create()
  else if name = 'AuditEvent.agent' then
    result := TFhirAuditEventAgent.Create()
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
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  else if name = 'BiologicallyDerivedProduct.collection' then
    result := TFhirBiologicallyDerivedProductCollection.Create()
  else if name = 'BiologicallyDerivedProduct.property' then
    result := TFhirBiologicallyDerivedProductProperty.Create()
  else if name = 'BiologicallyDerivedProduct' then
    result := TFhirBiologicallyDerivedProduct.Create()
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
{$IFDEF FHIR_BODYSTRUCTURE}
  else if name = 'BodyStructure.includedStructure' then
    result := TFhirBodyStructureIncludedStructure.Create()
  else if name = 'BodyStructure.excludedStructure' then
    result := TFhirBodyStructureExcludedStructure.Create()
  else if name = 'BodyStructure' then
    result := TFhirBodyStructure.Create()
{$ENDIF FHIR_BODYSTRUCTURE}
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
  else if name = 'CapabilityStatement.rest.resource' then
    result := TFhirCapabilityStatementRestResource.Create()
  else if name = 'CapabilityStatement.rest.resource.interaction' then
    result := TFhirCapabilityStatementRestResourceInteraction.Create()
  else if name = 'CapabilityStatement.rest.resource.searchParam' then
    result := TFhirCapabilityStatementRestResourceSearchParam.Create()
  else if name = 'CapabilityStatement.rest.resource.operation' then
    result := TFhirCapabilityStatementRestResourceOperation.Create()
  else if name = 'CapabilityStatement.rest.interaction' then
    result := TFhirCapabilityStatementRestInteraction.Create()
  else if name = 'CapabilityStatement.messaging' then
    result := TFhirCapabilityStatementMessaging.Create()
  else if name = 'CapabilityStatement.messaging.endpoint' then
    result := TFhirCapabilityStatementMessagingEndpoint.Create()
  else if name = 'CapabilityStatement.messaging.supportedMessage' then
    result := TFhirCapabilityStatementMessagingSupportedMessage.Create()
  else if name = 'CapabilityStatement.document' then
    result := TFhirCapabilityStatementDocument.Create()
  else if name = 'CapabilityStatement' then
    result := TFhirCapabilityStatement.Create()
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CAREPLAN}
  else if name = 'CarePlan.activity' then
    result := TFhirCarePlanActivity.Create()
  else if name = 'CarePlan.activity.plannedActivityDetail' then
    result := TFhirCarePlanActivityPlannedActivityDetail.Create()
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
  else if name = 'ChargeItem.performer' then
    result := TFhirChargeItemPerformer.Create()
  else if name = 'ChargeItem' then
    result := TFhirChargeItem.Create()
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CHARGEITEMDEFINITION}
  else if name = 'ChargeItemDefinition.applicability' then
    result := TFhirChargeItemDefinitionApplicability.Create()
  else if name = 'ChargeItemDefinition.propertyGroup' then
    result := TFhirChargeItemDefinitionPropertyGroup.Create()
  else if name = 'ChargeItemDefinition' then
    result := TFhirChargeItemDefinition.Create()
{$ENDIF FHIR_CHARGEITEMDEFINITION}
{$IFDEF FHIR_CITATION}
  else if name = 'Citation.summary' then
    result := TFhirCitationSummary.Create()
  else if name = 'Citation.classification' then
    result := TFhirCitationClassification.Create()
  else if name = 'Citation.statusDate' then
    result := TFhirCitationStatusDate.Create()
  else if name = 'Citation.citedArtifact' then
    result := TFhirCitationCitedArtifact.Create()
  else if name = 'Citation.citedArtifact.version' then
    result := TFhirCitationCitedArtifactVersion.Create()
  else if name = 'Citation.citedArtifact.statusDate' then
    result := TFhirCitationCitedArtifactStatusDate.Create()
  else if name = 'Citation.citedArtifact.title' then
    result := TFhirCitationCitedArtifactTitle.Create()
  else if name = 'Citation.citedArtifact.abstract' then
    result := TFhirCitationCitedArtifactAbstract.Create()
  else if name = 'Citation.citedArtifact.part' then
    result := TFhirCitationCitedArtifactPart.Create()
  else if name = 'Citation.citedArtifact.relatesTo' then
    result := TFhirCitationCitedArtifactRelatesTo.Create()
  else if name = 'Citation.citedArtifact.publicationForm' then
    result := TFhirCitationCitedArtifactPublicationForm.Create()
  else if name = 'Citation.citedArtifact.publicationForm.publishedIn' then
    result := TFhirCitationCitedArtifactPublicationFormPublishedIn.Create()
  else if name = 'Citation.citedArtifact.webLocation' then
    result := TFhirCitationCitedArtifactWebLocation.Create()
  else if name = 'Citation.citedArtifact.classification' then
    result := TFhirCitationCitedArtifactClassification.Create()
  else if name = 'Citation.citedArtifact.contributorship' then
    result := TFhirCitationCitedArtifactContributorship.Create()
  else if name = 'Citation.citedArtifact.contributorship.entry' then
    result := TFhirCitationCitedArtifactContributorshipEntry.Create()
  else if name = 'Citation.citedArtifact.contributorship.entry.contributionInstance' then
    result := TFhirCitationCitedArtifactContributorshipEntryContributionInstance.Create()
  else if name = 'Citation.citedArtifact.contributorship.summary' then
    result := TFhirCitationCitedArtifactContributorshipSummary.Create()
  else if name = 'Citation' then
    result := TFhirCitation.Create()
{$ENDIF FHIR_CITATION}
{$IFDEF FHIR_CLAIM}
  else if name = 'Claim.related' then
    result := TFhirClaimRelated.Create()
  else if name = 'Claim.payee' then
    result := TFhirClaimPayee.Create()
  else if name = 'Claim.careTeam' then
    result := TFhirClaimCareTeam.Create()
  else if name = 'Claim.supportingInfo' then
    result := TFhirClaimSupportingInfo.Create()
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
  else if name = 'Claim.item.bodySite' then
    result := TFhirClaimItemBodySite.Create()
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
  else if name = 'ClaimResponse.addItem.bodySite' then
    result := TFhirClaimResponseAddItemBodySite.Create()
  else if name = 'ClaimResponse.addItem.detail' then
    result := TFhirClaimResponseAddItemDetail.Create()
  else if name = 'ClaimResponse.addItem.detail.subDetail' then
    result := TFhirClaimResponseAddItemDetailSubDetail.Create()
  else if name = 'ClaimResponse.total' then
    result := TFhirClaimResponseTotal.Create()
  else if name = 'ClaimResponse.payment' then
    result := TFhirClaimResponsePayment.Create()
  else if name = 'ClaimResponse.processNote' then
    result := TFhirClaimResponseProcessNote.Create()
  else if name = 'ClaimResponse.insurance' then
    result := TFhirClaimResponseInsurance.Create()
  else if name = 'ClaimResponse.error' then
    result := TFhirClaimResponseError.Create()
  else if name = 'ClaimResponse' then
    result := TFhirClaimResponse.Create()
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  else if name = 'ClinicalImpression.finding' then
    result := TFhirClinicalImpressionFinding.Create()
  else if name = 'ClinicalImpression' then
    result := TFhirClinicalImpression.Create()
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_CLINICALUSEDEFINITION}
  else if name = 'ClinicalUseDefinition.contraindication' then
    result := TFhirClinicalUseDefinitionContraindication.Create()
  else if name = 'ClinicalUseDefinition.contraindication.otherTherapy' then
    result := TFhirClinicalUseDefinitionContraindicationOtherTherapy.Create()
  else if name = 'ClinicalUseDefinition.indication' then
    result := TFhirClinicalUseDefinitionIndication.Create()
  else if name = 'ClinicalUseDefinition.interaction' then
    result := TFhirClinicalUseDefinitionInteraction.Create()
  else if name = 'ClinicalUseDefinition.interaction.interactant' then
    result := TFhirClinicalUseDefinitionInteractionInteractant.Create()
  else if name = 'ClinicalUseDefinition.undesirableEffect' then
    result := TFhirClinicalUseDefinitionUndesirableEffect.Create()
  else if name = 'ClinicalUseDefinition.warning' then
    result := TFhirClinicalUseDefinitionWarning.Create()
  else if name = 'ClinicalUseDefinition' then
    result := TFhirClinicalUseDefinition.Create()
{$ENDIF FHIR_CLINICALUSEDEFINITION}
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
  else if name = 'Condition.participant' then
    result := TFhirConditionParticipant.Create()
  else if name = 'Condition.stage' then
    result := TFhirConditionStage.Create()
  else if name = 'Condition' then
    result := TFhirCondition.Create()
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_CONDITIONDEFINITION}
  else if name = 'ConditionDefinition.observation' then
    result := TFhirConditionDefinitionObservation.Create()
  else if name = 'ConditionDefinition.medication' then
    result := TFhirConditionDefinitionMedication.Create()
  else if name = 'ConditionDefinition.precondition' then
    result := TFhirConditionDefinitionPrecondition.Create()
  else if name = 'ConditionDefinition.questionnaire' then
    result := TFhirConditionDefinitionQuestionnaire.Create()
  else if name = 'ConditionDefinition.plan' then
    result := TFhirConditionDefinitionPlan.Create()
  else if name = 'ConditionDefinition' then
    result := TFhirConditionDefinition.Create()
{$ENDIF FHIR_CONDITIONDEFINITION}
{$IFDEF FHIR_CONSENT}
  else if name = 'Consent.policyBasis' then
    result := TFhirConsentPolicyBasis.Create()
  else if name = 'Consent.verification' then
    result := TFhirConsentVerification.Create()
  else if name = 'Consent.provision' then
    result := TFhirConsentProvision.Create()
  else if name = 'Consent.provision.actor' then
    result := TFhirConsentProvisionActor.Create()
  else if name = 'Consent.provision.data' then
    result := TFhirConsentProvisionData.Create()
  else if name = 'Consent' then
    result := TFhirConsent.Create()
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  else if name = 'Contract.contentDefinition' then
    result := TFhirContractContentDefinition.Create()
  else if name = 'Contract.term' then
    result := TFhirContractTerm.Create()
  else if name = 'Contract.term.securityLabel' then
    result := TFhirContractTermSecurityLabel.Create()
  else if name = 'Contract.term.offer' then
    result := TFhirContractTermOffer.Create()
  else if name = 'Contract.term.offer.party' then
    result := TFhirContractTermOfferParty.Create()
  else if name = 'Contract.term.offer.answer' then
    result := TFhirContractTermOfferAnswer.Create()
  else if name = 'Contract.term.asset' then
    result := TFhirContractTermAsset.Create()
  else if name = 'Contract.term.asset.context' then
    result := TFhirContractTermAssetContext.Create()
  else if name = 'Contract.term.asset.valuedItem' then
    result := TFhirContractTermAssetValuedItem.Create()
  else if name = 'Contract.term.action' then
    result := TFhirContractTermAction.Create()
  else if name = 'Contract.term.action.subject' then
    result := TFhirContractTermActionSubject.Create()
  else if name = 'Contract.signer' then
    result := TFhirContractSigner.Create()
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
  else if name = 'Coverage.paymentBy' then
    result := TFhirCoveragePaymentBy.Create()
  else if name = 'Coverage.class' then
    result := TFhirCoverageClass.Create()
  else if name = 'Coverage.costToBeneficiary' then
    result := TFhirCoverageCostToBeneficiary.Create()
  else if name = 'Coverage.costToBeneficiary.exception' then
    result := TFhirCoverageCostToBeneficiaryException.Create()
  else if name = 'Coverage' then
    result := TFhirCoverage.Create()
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  else if name = 'CoverageEligibilityRequest.supportingInfo' then
    result := TFhirCoverageEligibilityRequestSupportingInfo.Create()
  else if name = 'CoverageEligibilityRequest.insurance' then
    result := TFhirCoverageEligibilityRequestInsurance.Create()
  else if name = 'CoverageEligibilityRequest.item' then
    result := TFhirCoverageEligibilityRequestItem.Create()
  else if name = 'CoverageEligibilityRequest.item.diagnosis' then
    result := TFhirCoverageEligibilityRequestItemDiagnosis.Create()
  else if name = 'CoverageEligibilityRequest' then
    result := TFhirCoverageEligibilityRequest.Create()
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}
{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  else if name = 'CoverageEligibilityResponse.insurance' then
    result := TFhirCoverageEligibilityResponseInsurance.Create()
  else if name = 'CoverageEligibilityResponse.insurance.item' then
    result := TFhirCoverageEligibilityResponseInsuranceItem.Create()
  else if name = 'CoverageEligibilityResponse.insurance.item.benefit' then
    result := TFhirCoverageEligibilityResponseInsuranceItemBenefit.Create()
  else if name = 'CoverageEligibilityResponse.error' then
    result := TFhirCoverageEligibilityResponseError.Create()
  else if name = 'CoverageEligibilityResponse' then
    result := TFhirCoverageEligibilityResponse.Create()
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}
{$IFDEF FHIR_DETECTEDISSUE}
  else if name = 'DetectedIssue.evidence' then
    result := TFhirDetectedIssueEvidence.Create()
  else if name = 'DetectedIssue.mitigation' then
    result := TFhirDetectedIssueMitigation.Create()
  else if name = 'DetectedIssue' then
    result := TFhirDetectedIssue.Create()
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  else if name = 'Device.udiCarrier' then
    result := TFhirDeviceUdiCarrier.Create()
  else if name = 'Device.deviceName' then
    result := TFhirDeviceDeviceName.Create()
  else if name = 'Device.version' then
    result := TFhirDeviceVersion.Create()
  else if name = 'Device.specialization' then
    result := TFhirDeviceSpecialization.Create()
  else if name = 'Device.property' then
    result := TFhirDeviceProperty.Create()
  else if name = 'Device.operation' then
    result := TFhirDeviceOperation.Create()
  else if name = 'Device.association' then
    result := TFhirDeviceAssociation.Create()
  else if name = 'Device' then
    result := TFhirDevice.Create()
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICEDEFINITION}
  else if name = 'DeviceDefinition.udiDeviceIdentifier' then
    result := TFhirDeviceDefinitionUdiDeviceIdentifier.Create()
  else if name = 'DeviceDefinition.udiDeviceIdentifier.marketDistribution' then
    result := TFhirDeviceDefinitionUdiDeviceIdentifierMarketDistribution.Create()
  else if name = 'DeviceDefinition.regulatoryIdentifier' then
    result := TFhirDeviceDefinitionRegulatoryIdentifier.Create()
  else if name = 'DeviceDefinition.deviceName' then
    result := TFhirDeviceDefinitionDeviceName.Create()
  else if name = 'DeviceDefinition.classification' then
    result := TFhirDeviceDefinitionClassification.Create()
  else if name = 'DeviceDefinition.hasPart' then
    result := TFhirDeviceDefinitionHasPart.Create()
  else if name = 'DeviceDefinition.packaging' then
    result := TFhirDeviceDefinitionPackaging.Create()
  else if name = 'DeviceDefinition.packaging.distributor' then
    result := TFhirDeviceDefinitionPackagingDistributor.Create()
  else if name = 'DeviceDefinition.version' then
    result := TFhirDeviceDefinitionVersion.Create()
  else if name = 'DeviceDefinition.property' then
    result := TFhirDeviceDefinitionProperty.Create()
  else if name = 'DeviceDefinition.link' then
    result := TFhirDeviceDefinitionLink.Create()
  else if name = 'DeviceDefinition.material' then
    result := TFhirDeviceDefinitionMaterial.Create()
  else if name = 'DeviceDefinition.guideline' then
    result := TFhirDeviceDefinitionGuideline.Create()
  else if name = 'DeviceDefinition.correctiveAction' then
    result := TFhirDeviceDefinitionCorrectiveAction.Create()
  else if name = 'DeviceDefinition.chargeItem' then
    result := TFhirDeviceDefinitionChargeItem.Create()
  else if name = 'DeviceDefinition' then
    result := TFhirDeviceDefinition.Create()
{$ENDIF FHIR_DEVICEDEFINITION}
{$IFDEF FHIR_DEVICEDISPENSE}
  else if name = 'DeviceDispense.performer' then
    result := TFhirDeviceDispensePerformer.Create()
  else if name = 'DeviceDispense' then
    result := TFhirDeviceDispense.Create()
{$ENDIF FHIR_DEVICEDISPENSE}
{$IFDEF FHIR_DEVICEMETRIC}
  else if name = 'DeviceMetric.calibration' then
    result := TFhirDeviceMetricCalibration.Create()
  else if name = 'DeviceMetric' then
    result := TFhirDeviceMetric.Create()
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEREQUEST}
  else if name = 'DeviceRequest.parameter' then
    result := TFhirDeviceRequestParameter.Create()
  else if name = 'DeviceRequest' then
    result := TFhirDeviceRequest.Create()
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSAGE}
  else if name = 'DeviceUsage.adherence' then
    result := TFhirDeviceUsageAdherence.Create()
  else if name = 'DeviceUsage' then
    result := TFhirDeviceUsage.Create()
{$ENDIF FHIR_DEVICEUSAGE}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  else if name = 'DiagnosticReport.supportingInfo' then
    result := TFhirDiagnosticReportSupportingInfo.Create()
  else if name = 'DiagnosticReport.media' then
    result := TFhirDiagnosticReportMedia.Create()
  else if name = 'DiagnosticReport' then
    result := TFhirDiagnosticReport.Create()
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  else if name = 'DocumentManifest.related' then
    result := TFhirDocumentManifestRelated.Create()
  else if name = 'DocumentManifest' then
    result := TFhirDocumentManifest.Create()
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  else if name = 'DocumentReference.attester' then
    result := TFhirDocumentReferenceAttester.Create()
  else if name = 'DocumentReference.relatesTo' then
    result := TFhirDocumentReferenceRelatesTo.Create()
  else if name = 'DocumentReference.content' then
    result := TFhirDocumentReferenceContent.Create()
  else if name = 'DocumentReference.content.profile' then
    result := TFhirDocumentReferenceContentProfile.Create()
  else if name = 'DocumentReference' then
    result := TFhirDocumentReference.Create()
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_ENCOUNTER}
  else if name = 'Encounter.statusHistory' then
    result := TFhirEncounterStatusHistory.Create()
  else if name = 'Encounter.classHistory' then
    result := TFhirEncounterClassHistory.Create()
  else if name = 'Encounter.participant' then
    result := TFhirEncounterParticipant.Create()
  else if name = 'Encounter.diagnosis' then
    result := TFhirEncounterDiagnosis.Create()
  else if name = 'Encounter.admission' then
    result := TFhirEncounterAdmission.Create()
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
{$IFDEF FHIR_EVENTDEFINITION}
  else if name = 'EventDefinition' then
    result := TFhirEventDefinition.Create()
{$ENDIF FHIR_EVENTDEFINITION}
{$IFDEF FHIR_EVIDENCE}
  else if name = 'Evidence.variableDefinition' then
    result := TFhirEvidenceVariableDefinition.Create()
  else if name = 'Evidence.statistic' then
    result := TFhirEvidenceStatistic.Create()
  else if name = 'Evidence.statistic.sampleSize' then
    result := TFhirEvidenceStatisticSampleSize.Create()
  else if name = 'Evidence.statistic.attributeEstimate' then
    result := TFhirEvidenceStatisticAttributeEstimate.Create()
  else if name = 'Evidence.statistic.modelCharacteristic' then
    result := TFhirEvidenceStatisticModelCharacteristic.Create()
  else if name = 'Evidence.statistic.modelCharacteristic.variable' then
    result := TFhirEvidenceStatisticModelCharacteristicVariable.Create()
  else if name = 'Evidence.certainty' then
    result := TFhirEvidenceCertainty.Create()
  else if name = 'Evidence' then
    result := TFhirEvidence.Create()
{$ENDIF FHIR_EVIDENCE}
{$IFDEF FHIR_EVIDENCEREPORT}
  else if name = 'EvidenceReport.subject' then
    result := TFhirEvidenceReportSubject.Create()
  else if name = 'EvidenceReport.subject.characteristic' then
    result := TFhirEvidenceReportSubjectCharacteristic.Create()
  else if name = 'EvidenceReport.relatesTo' then
    result := TFhirEvidenceReportRelatesTo.Create()
  else if name = 'EvidenceReport.relatesTo.target' then
    result := TFhirEvidenceReportRelatesToTarget.Create()
  else if name = 'EvidenceReport.section' then
    result := TFhirEvidenceReportSection.Create()
  else if name = 'EvidenceReport' then
    result := TFhirEvidenceReport.Create()
{$ENDIF FHIR_EVIDENCEREPORT}
{$IFDEF FHIR_EVIDENCEVARIABLE}
  else if name = 'EvidenceVariable.characteristic' then
    result := TFhirEvidenceVariableCharacteristic.Create()
  else if name = 'EvidenceVariable.characteristic.definitionByTypeAndValue' then
    result := TFhirEvidenceVariableCharacteristicDefinitionByTypeAndValue.Create()
  else if name = 'EvidenceVariable.characteristic.definitionByCombination' then
    result := TFhirEvidenceVariableCharacteristicDefinitionByCombination.Create()
  else if name = 'EvidenceVariable.characteristic.timeFromEvent' then
    result := TFhirEvidenceVariableCharacteristicTimeFromEvent.Create()
  else if name = 'EvidenceVariable.category' then
    result := TFhirEvidenceVariableCategory.Create()
  else if name = 'EvidenceVariable' then
    result := TFhirEvidenceVariable.Create()
{$ENDIF FHIR_EVIDENCEVARIABLE}
{$IFDEF FHIR_EXAMPLESCENARIO}
  else if name = 'ExampleScenario.actor' then
    result := TFhirExampleScenarioActor.Create()
  else if name = 'ExampleScenario.instance' then
    result := TFhirExampleScenarioInstance.Create()
  else if name = 'ExampleScenario.instance.version' then
    result := TFhirExampleScenarioInstanceVersion.Create()
  else if name = 'ExampleScenario.instance.containedInstance' then
    result := TFhirExampleScenarioInstanceContainedInstance.Create()
  else if name = 'ExampleScenario.process' then
    result := TFhirExampleScenarioProcess.Create()
  else if name = 'ExampleScenario.process.step' then
    result := TFhirExampleScenarioProcessStep.Create()
  else if name = 'ExampleScenario.process.step.operation' then
    result := TFhirExampleScenarioProcessStepOperation.Create()
  else if name = 'ExampleScenario.process.step.alternative' then
    result := TFhirExampleScenarioProcessStepAlternative.Create()
  else if name = 'ExampleScenario' then
    result := TFhirExampleScenario.Create()
{$ENDIF FHIR_EXAMPLESCENARIO}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  else if name = 'ExplanationOfBenefit.related' then
    result := TFhirExplanationOfBenefitRelated.Create()
  else if name = 'ExplanationOfBenefit.payee' then
    result := TFhirExplanationOfBenefitPayee.Create()
  else if name = 'ExplanationOfBenefit.careTeam' then
    result := TFhirExplanationOfBenefitCareTeam.Create()
  else if name = 'ExplanationOfBenefit.supportingInfo' then
    result := TFhirExplanationOfBenefitSupportingInfo.Create()
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
  else if name = 'ExplanationOfBenefit.item.bodySite' then
    result := TFhirExplanationOfBenefitItemBodySite.Create()
  else if name = 'ExplanationOfBenefit.item.adjudication' then
    result := TFhirExplanationOfBenefitItemAdjudication.Create()
  else if name = 'ExplanationOfBenefit.item.detail' then
    result := TFhirExplanationOfBenefitItemDetail.Create()
  else if name = 'ExplanationOfBenefit.item.detail.subDetail' then
    result := TFhirExplanationOfBenefitItemDetailSubDetail.Create()
  else if name = 'ExplanationOfBenefit.addItem' then
    result := TFhirExplanationOfBenefitAddItem.Create()
  else if name = 'ExplanationOfBenefit.addItem.bodySite' then
    result := TFhirExplanationOfBenefitAddItemBodySite.Create()
  else if name = 'ExplanationOfBenefit.addItem.detail' then
    result := TFhirExplanationOfBenefitAddItemDetail.Create()
  else if name = 'ExplanationOfBenefit.addItem.detail.subDetail' then
    result := TFhirExplanationOfBenefitAddItemDetailSubDetail.Create()
  else if name = 'ExplanationOfBenefit.total' then
    result := TFhirExplanationOfBenefitTotal.Create()
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
  else if name = 'FamilyMemberHistory.procedure' then
    result := TFhirFamilyMemberHistoryProcedure.Create()
  else if name = 'FamilyMemberHistory' then
    result := TFhirFamilyMemberHistory.Create()
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  else if name = 'Flag' then
    result := TFhirFlag.Create()
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_FORMULARYITEM}
  else if name = 'FormularyItem' then
    result := TFhirFormularyItem.Create()
{$ENDIF FHIR_FORMULARYITEM}
{$IFDEF FHIR_GENOMICSTUDY}
  else if name = 'GenomicStudy.analysis' then
    result := TFhirGenomicStudyAnalysis.Create()
  else if name = 'GenomicStudy.analysis.input' then
    result := TFhirGenomicStudyAnalysisInput.Create()
  else if name = 'GenomicStudy.analysis.output' then
    result := TFhirGenomicStudyAnalysisOutput.Create()
  else if name = 'GenomicStudy.analysis.performer' then
    result := TFhirGenomicStudyAnalysisPerformer.Create()
  else if name = 'GenomicStudy.analysis.device' then
    result := TFhirGenomicStudyAnalysisDevice.Create()
  else if name = 'GenomicStudy' then
    result := TFhirGenomicStudy.Create()
{$ENDIF FHIR_GENOMICSTUDY}
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
  else if name = 'HealthcareService.eligibility' then
    result := TFhirHealthcareServiceEligibility.Create()
  else if name = 'HealthcareService' then
    result := TFhirHealthcareService.Create()
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGSELECTION}
  else if name = 'ImagingSelection.performer' then
    result := TFhirImagingSelectionPerformer.Create()
  else if name = 'ImagingSelection.instance' then
    result := TFhirImagingSelectionInstance.Create()
  else if name = 'ImagingSelection.instance.imageRegion' then
    result := TFhirImagingSelectionInstanceImageRegion.Create()
  else if name = 'ImagingSelection.imageRegion' then
    result := TFhirImagingSelectionImageRegion.Create()
  else if name = 'ImagingSelection' then
    result := TFhirImagingSelection.Create()
{$ENDIF FHIR_IMAGINGSELECTION}
{$IFDEF FHIR_IMAGINGSTUDY}
  else if name = 'ImagingStudy.series' then
    result := TFhirImagingStudySeries.Create()
  else if name = 'ImagingStudy.series.performer' then
    result := TFhirImagingStudySeriesPerformer.Create()
  else if name = 'ImagingStudy.series.instance' then
    result := TFhirImagingStudySeriesInstance.Create()
  else if name = 'ImagingStudy' then
    result := TFhirImagingStudy.Create()
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  else if name = 'Immunization.performer' then
    result := TFhirImmunizationPerformer.Create()
  else if name = 'Immunization.programEligibility' then
    result := TFhirImmunizationProgramEligibility.Create()
  else if name = 'Immunization.reaction' then
    result := TFhirImmunizationReaction.Create()
  else if name = 'Immunization.protocolApplied' then
    result := TFhirImmunizationProtocolApplied.Create()
  else if name = 'Immunization' then
    result := TFhirImmunization.Create()
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  else if name = 'ImmunizationEvaluation' then
    result := TFhirImmunizationEvaluation.Create()
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  else if name = 'ImmunizationRecommendation.recommendation' then
    result := TFhirImmunizationRecommendationRecommendation.Create()
  else if name = 'ImmunizationRecommendation.recommendation.dateCriterion' then
    result := TFhirImmunizationRecommendationRecommendationDateCriterion.Create()
  else if name = 'ImmunizationRecommendation' then
    result := TFhirImmunizationRecommendation.Create()
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  else if name = 'ImplementationGuide.dependsOn' then
    result := TFhirImplementationGuideDependsOn.Create()
  else if name = 'ImplementationGuide.global' then
    result := TFhirImplementationGuideGlobal.Create()
  else if name = 'ImplementationGuide.definition' then
    result := TFhirImplementationGuideDefinition.Create()
  else if name = 'ImplementationGuide.definition.grouping' then
    result := TFhirImplementationGuideDefinitionGrouping.Create()
  else if name = 'ImplementationGuide.definition.resource' then
    result := TFhirImplementationGuideDefinitionResource.Create()
  else if name = 'ImplementationGuide.definition.page' then
    result := TFhirImplementationGuideDefinitionPage.Create()
  else if name = 'ImplementationGuide.definition.parameter' then
    result := TFhirImplementationGuideDefinitionParameter.Create()
  else if name = 'ImplementationGuide.definition.template' then
    result := TFhirImplementationGuideDefinitionTemplate.Create()
  else if name = 'ImplementationGuide.manifest' then
    result := TFhirImplementationGuideManifest.Create()
  else if name = 'ImplementationGuide.manifest.resource' then
    result := TFhirImplementationGuideManifestResource.Create()
  else if name = 'ImplementationGuide.manifest.page' then
    result := TFhirImplementationGuideManifestPage.Create()
  else if name = 'ImplementationGuide' then
    result := TFhirImplementationGuide.Create()
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_INGREDIENT}
  else if name = 'Ingredient.manufacturer' then
    result := TFhirIngredientManufacturer.Create()
  else if name = 'Ingredient.substance' then
    result := TFhirIngredientSubstance.Create()
  else if name = 'Ingredient.substance.strength' then
    result := TFhirIngredientSubstanceStrength.Create()
  else if name = 'Ingredient.substance.strength.referenceStrength' then
    result := TFhirIngredientSubstanceStrengthReferenceStrength.Create()
  else if name = 'Ingredient' then
    result := TFhirIngredient.Create()
{$ENDIF FHIR_INGREDIENT}
{$IFDEF FHIR_INSURANCEPLAN}
  else if name = 'InsurancePlan.coverage' then
    result := TFhirInsurancePlanCoverage.Create()
  else if name = 'InsurancePlan.coverage.benefit' then
    result := TFhirInsurancePlanCoverageBenefit.Create()
  else if name = 'InsurancePlan.coverage.benefit.limit' then
    result := TFhirInsurancePlanCoverageBenefitLimit.Create()
  else if name = 'InsurancePlan.plan' then
    result := TFhirInsurancePlanPlan.Create()
  else if name = 'InsurancePlan.plan.generalCost' then
    result := TFhirInsurancePlanPlanGeneralCost.Create()
  else if name = 'InsurancePlan.plan.specificCost' then
    result := TFhirInsurancePlanPlanSpecificCost.Create()
  else if name = 'InsurancePlan.plan.specificCost.benefit' then
    result := TFhirInsurancePlanPlanSpecificCostBenefit.Create()
  else if name = 'InsurancePlan.plan.specificCost.benefit.cost' then
    result := TFhirInsurancePlanPlanSpecificCostBenefitCost.Create()
  else if name = 'InsurancePlan' then
    result := TFhirInsurancePlan.Create()
{$ENDIF FHIR_INSURANCEPLAN}
{$IFDEF FHIR_INVENTORYREPORT}
  else if name = 'InventoryReport.inventoryListing' then
    result := TFhirInventoryReportInventoryListing.Create()
  else if name = 'InventoryReport.inventoryListing.items' then
    result := TFhirInventoryReportInventoryListingItems.Create()
  else if name = 'InventoryReport' then
    result := TFhirInventoryReport.Create()
{$ENDIF FHIR_INVENTORYREPORT}
{$IFDEF FHIR_INVOICE}
  else if name = 'Invoice.participant' then
    result := TFhirInvoiceParticipant.Create()
  else if name = 'Invoice.lineItem' then
    result := TFhirInvoiceLineItem.Create()
  else if name = 'Invoice' then
    result := TFhirInvoice.Create()
{$ENDIF FHIR_INVOICE}
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
{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
  else if name = 'ManufacturedItemDefinition.property' then
    result := TFhirManufacturedItemDefinitionProperty.Create()
  else if name = 'ManufacturedItemDefinition.component' then
    result := TFhirManufacturedItemDefinitionComponent.Create()
  else if name = 'ManufacturedItemDefinition.component.constituent' then
    result := TFhirManufacturedItemDefinitionComponentConstituent.Create()
  else if name = 'ManufacturedItemDefinition' then
    result := TFhirManufacturedItemDefinition.Create()
{$ENDIF FHIR_MANUFACTUREDITEMDEFINITION}
{$IFDEF FHIR_MEASURE}
  else if name = 'Measure.term' then
    result := TFhirMeasureTerm.Create()
  else if name = 'Measure.group' then
    result := TFhirMeasureGroup.Create()
  else if name = 'Measure.group.population' then
    result := TFhirMeasureGroupPopulation.Create()
  else if name = 'Measure.group.stratifier' then
    result := TFhirMeasureGroupStratifier.Create()
  else if name = 'Measure.group.stratifier.component' then
    result := TFhirMeasureGroupStratifierComponent.Create()
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
  else if name = 'MeasureReport.group.stratifier.stratum.component' then
    result := TFhirMeasureReportGroupStratifierStratumComponent.Create()
  else if name = 'MeasureReport.group.stratifier.stratum.population' then
    result := TFhirMeasureReportGroupStratifierStratumPopulation.Create()
  else if name = 'MeasureReport' then
    result := TFhirMeasureReport.Create()
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MEDICATION}
  else if name = 'Medication.ingredient' then
    result := TFhirMedicationIngredient.Create()
  else if name = 'Medication.batch' then
    result := TFhirMedicationBatch.Create()
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
{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  else if name = 'MedicationKnowledge.relatedMedicationKnowledge' then
    result := TFhirMedicationKnowledgeRelatedMedicationKnowledge.Create()
  else if name = 'MedicationKnowledge.monograph' then
    result := TFhirMedicationKnowledgeMonograph.Create()
  else if name = 'MedicationKnowledge.cost' then
    result := TFhirMedicationKnowledgeCost.Create()
  else if name = 'MedicationKnowledge.monitoringProgram' then
    result := TFhirMedicationKnowledgeMonitoringProgram.Create()
  else if name = 'MedicationKnowledge.indicationGuideline' then
    result := TFhirMedicationKnowledgeIndicationGuideline.Create()
  else if name = 'MedicationKnowledge.indicationGuideline.dosingGuideline' then
    result := TFhirMedicationKnowledgeIndicationGuidelineDosingGuideline.Create()
  else if name = 'MedicationKnowledge.indicationGuideline.dosingGuideline.dosage' then
    result := TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelineDosage.Create()
  else if name = 'MedicationKnowledge.indicationGuideline.dosingGuideline.patientCharacteristic' then
    result := TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelinePatientCharacteristic.Create()
  else if name = 'MedicationKnowledge.medicineClassification' then
    result := TFhirMedicationKnowledgeMedicineClassification.Create()
  else if name = 'MedicationKnowledge.packaging' then
    result := TFhirMedicationKnowledgePackaging.Create()
  else if name = 'MedicationKnowledge.storageGuideline' then
    result := TFhirMedicationKnowledgeStorageGuideline.Create()
  else if name = 'MedicationKnowledge.storageGuideline.environmentalSetting' then
    result := TFhirMedicationKnowledgeStorageGuidelineEnvironmentalSetting.Create()
  else if name = 'MedicationKnowledge.regulatory' then
    result := TFhirMedicationKnowledgeRegulatory.Create()
  else if name = 'MedicationKnowledge.regulatory.substitution' then
    result := TFhirMedicationKnowledgeRegulatorySubstitution.Create()
  else if name = 'MedicationKnowledge.regulatory.maxDispense' then
    result := TFhirMedicationKnowledgeRegulatoryMaxDispense.Create()
  else if name = 'MedicationKnowledge.definitional' then
    result := TFhirMedicationKnowledgeDefinitional.Create()
  else if name = 'MedicationKnowledge.definitional.ingredient' then
    result := TFhirMedicationKnowledgeDefinitionalIngredient.Create()
  else if name = 'MedicationKnowledge.definitional.drugCharacteristic' then
    result := TFhirMedicationKnowledgeDefinitionalDrugCharacteristic.Create()
  else if name = 'MedicationKnowledge' then
    result := TFhirMedicationKnowledge.Create()
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  else if name = 'MedicationRequest.dose' then
    result := TFhirMedicationRequestDose.Create()
  else if name = 'MedicationRequest.dispenseRequest' then
    result := TFhirMedicationRequestDispenseRequest.Create()
  else if name = 'MedicationRequest.dispenseRequest.initialFill' then
    result := TFhirMedicationRequestDispenseRequestInitialFill.Create()
  else if name = 'MedicationRequest.substitution' then
    result := TFhirMedicationRequestSubstitution.Create()
  else if name = 'MedicationRequest' then
    result := TFhirMedicationRequest.Create()
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONUSAGE}
  else if name = 'MedicationUsage.adherence' then
    result := TFhirMedicationUsageAdherence.Create()
  else if name = 'MedicationUsage' then
    result := TFhirMedicationUsage.Create()
{$ENDIF FHIR_MEDICATIONUSAGE}
{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
  else if name = 'MedicinalProductDefinition.contact' then
    result := TFhirMedicinalProductDefinitionContact.Create()
  else if name = 'MedicinalProductDefinition.name' then
    result := TFhirMedicinalProductDefinitionName.Create()
  else if name = 'MedicinalProductDefinition.name.part' then
    result := TFhirMedicinalProductDefinitionNamePart.Create()
  else if name = 'MedicinalProductDefinition.name.usage' then
    result := TFhirMedicinalProductDefinitionNameUsage.Create()
  else if name = 'MedicinalProductDefinition.crossReference' then
    result := TFhirMedicinalProductDefinitionCrossReference.Create()
  else if name = 'MedicinalProductDefinition.operation' then
    result := TFhirMedicinalProductDefinitionOperation.Create()
  else if name = 'MedicinalProductDefinition.characteristic' then
    result := TFhirMedicinalProductDefinitionCharacteristic.Create()
  else if name = 'MedicinalProductDefinition' then
    result := TFhirMedicinalProductDefinition.Create()
{$ENDIF FHIR_MEDICINALPRODUCTDEFINITION}
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
{$IFDEF FHIR_MOLECULARSEQUENCE}
  else if name = 'MolecularSequence.relative' then
    result := TFhirMolecularSequenceRelative.Create()
  else if name = 'MolecularSequence.relative.startingSequence' then
    result := TFhirMolecularSequenceRelativeStartingSequence.Create()
  else if name = 'MolecularSequence.relative.edit' then
    result := TFhirMolecularSequenceRelativeEdit.Create()
  else if name = 'MolecularSequence' then
    result := TFhirMolecularSequence.Create()
{$ENDIF FHIR_MOLECULARSEQUENCE}
{$IFDEF FHIR_NAMINGSYSTEM}
  else if name = 'NamingSystem.uniqueId' then
    result := TFhirNamingSystemUniqueId.Create()
  else if name = 'NamingSystem' then
    result := TFhirNamingSystem.Create()
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_NUTRITIONINTAKE}
  else if name = 'NutritionIntake.consumedItem' then
    result := TFhirNutritionIntakeConsumedItem.Create()
  else if name = 'NutritionIntake.ingredientLabel' then
    result := TFhirNutritionIntakeIngredientLabel.Create()
  else if name = 'NutritionIntake.performer' then
    result := TFhirNutritionIntakePerformer.Create()
  else if name = 'NutritionIntake' then
    result := TFhirNutritionIntake.Create()
{$ENDIF FHIR_NUTRITIONINTAKE}
{$IFDEF FHIR_NUTRITIONORDER}
  else if name = 'NutritionOrder.oralDiet' then
    result := TFhirNutritionOrderOralDiet.Create()
  else if name = 'NutritionOrder.oralDiet.schedule' then
    result := TFhirNutritionOrderOralDietSchedule.Create()
  else if name = 'NutritionOrder.oralDiet.nutrient' then
    result := TFhirNutritionOrderOralDietNutrient.Create()
  else if name = 'NutritionOrder.oralDiet.texture' then
    result := TFhirNutritionOrderOralDietTexture.Create()
  else if name = 'NutritionOrder.supplement' then
    result := TFhirNutritionOrderSupplement.Create()
  else if name = 'NutritionOrder.supplement.schedule' then
    result := TFhirNutritionOrderSupplementSchedule.Create()
  else if name = 'NutritionOrder.enteralFormula' then
    result := TFhirNutritionOrderEnteralFormula.Create()
  else if name = 'NutritionOrder.enteralFormula.additive' then
    result := TFhirNutritionOrderEnteralFormulaAdditive.Create()
  else if name = 'NutritionOrder.enteralFormula.administration' then
    result := TFhirNutritionOrderEnteralFormulaAdministration.Create()
  else if name = 'NutritionOrder.enteralFormula.administration.schedule' then
    result := TFhirNutritionOrderEnteralFormulaAdministrationSchedule.Create()
  else if name = 'NutritionOrder' then
    result := TFhirNutritionOrder.Create()
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_NUTRITIONPRODUCT}
  else if name = 'NutritionProduct.nutrient' then
    result := TFhirNutritionProductNutrient.Create()
  else if name = 'NutritionProduct.ingredient' then
    result := TFhirNutritionProductIngredient.Create()
  else if name = 'NutritionProduct.characteristic' then
    result := TFhirNutritionProductCharacteristic.Create()
  else if name = 'NutritionProduct.instance' then
    result := TFhirNutritionProductInstance.Create()
  else if name = 'NutritionProduct' then
    result := TFhirNutritionProduct.Create()
{$ENDIF FHIR_NUTRITIONPRODUCT}
{$IFDEF FHIR_OBSERVATION}
  else if name = 'Observation.triggeredBy' then
    result := TFhirObservationTriggeredBy.Create()
  else if name = 'Observation.referenceRange' then
    result := TFhirObservationReferenceRange.Create()
  else if name = 'Observation.component' then
    result := TFhirObservationComponent.Create()
  else if name = 'Observation' then
    result := TFhirObservation.Create()
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OBSERVATIONDEFINITION}
  else if name = 'ObservationDefinition.quantitativeDetails' then
    result := TFhirObservationDefinitionQuantitativeDetails.Create()
  else if name = 'ObservationDefinition.qualifiedValue' then
    result := TFhirObservationDefinitionQualifiedValue.Create()
  else if name = 'ObservationDefinition.component' then
    result := TFhirObservationDefinitionComponent.Create()
  else if name = 'ObservationDefinition' then
    result := TFhirObservationDefinition.Create()
{$ENDIF FHIR_OBSERVATIONDEFINITION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  else if name = 'OperationDefinition.parameter' then
    result := TFhirOperationDefinitionParameter.Create()
  else if name = 'OperationDefinition.parameter.binding' then
    result := TFhirOperationDefinitionParameterBinding.Create()
  else if name = 'OperationDefinition.parameter.referencedFrom' then
    result := TFhirOperationDefinitionParameterReferencedFrom.Create()
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
  else if name = 'Organization.qualification' then
    result := TFhirOrganizationQualification.Create()
  else if name = 'Organization' then
    result := TFhirOrganization.Create()
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  else if name = 'OrganizationAffiliation' then
    result := TFhirOrganizationAffiliation.Create()
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}
{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
  else if name = 'PackagedProductDefinition.legalStatusOfSupply' then
    result := TFhirPackagedProductDefinitionLegalStatusOfSupply.Create()
  else if name = 'PackagedProductDefinition.packaging' then
    result := TFhirPackagedProductDefinitionPackaging.Create()
  else if name = 'PackagedProductDefinition.packaging.property' then
    result := TFhirPackagedProductDefinitionPackagingProperty.Create()
  else if name = 'PackagedProductDefinition.packaging.containedItem' then
    result := TFhirPackagedProductDefinitionPackagingContainedItem.Create()
  else if name = 'PackagedProductDefinition' then
    result := TFhirPackagedProductDefinition.Create()
{$ENDIF FHIR_PACKAGEDPRODUCTDEFINITION}
{$IFDEF FHIR_PARAMETERS}
  else if name = 'Parameters.parameter' then
    result := TFhirParametersParameter.Create()
  else if name = 'Parameters' then
    result := TFhirParameters.Create()
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_PATIENT}
  else if name = 'Patient.contact' then
    result := TFhirPatientContact.Create()
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
  else if name = 'PaymentReconciliation.allocation' then
    result := TFhirPaymentReconciliationAllocation.Create()
  else if name = 'PaymentReconciliation.processNote' then
    result := TFhirPaymentReconciliationProcessNote.Create()
  else if name = 'PaymentReconciliation' then
    result := TFhirPaymentReconciliation.Create()
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERMISSION}
  else if name = 'Permission.justification' then
    result := TFhirPermissionJustification.Create()
  else if name = 'Permission.rule' then
    result := TFhirPermissionRule.Create()
  else if name = 'Permission.rule.data' then
    result := TFhirPermissionRuleData.Create()
  else if name = 'Permission.rule.data.resource' then
    result := TFhirPermissionRuleDataResource.Create()
  else if name = 'Permission.rule.activity' then
    result := TFhirPermissionRuleActivity.Create()
  else if name = 'Permission' then
    result := TFhirPermission.Create()
{$ENDIF FHIR_PERMISSION}
{$IFDEF FHIR_PERSON}
  else if name = 'Person.communication' then
    result := TFhirPersonCommunication.Create()
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
  else if name = 'PlanDefinition.actor' then
    result := TFhirPlanDefinitionActor.Create()
  else if name = 'PlanDefinition.actor.option' then
    result := TFhirPlanDefinitionActorOption.Create()
  else if name = 'PlanDefinition.action' then
    result := TFhirPlanDefinitionAction.Create()
  else if name = 'PlanDefinition.action.condition' then
    result := TFhirPlanDefinitionActionCondition.Create()
  else if name = 'PlanDefinition.action.input' then
    result := TFhirPlanDefinitionActionInput.Create()
  else if name = 'PlanDefinition.action.output' then
    result := TFhirPlanDefinitionActionOutput.Create()
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
  else if name = 'Questionnaire.item.answerOption' then
    result := TFhirQuestionnaireItemAnswerOption.Create()
  else if name = 'Questionnaire.item.initial' then
    result := TFhirQuestionnaireItemInitial.Create()
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
{$IFDEF FHIR_REGULATEDAUTHORIZATION}
  else if name = 'RegulatedAuthorization.case' then
    result := TFhirRegulatedAuthorizationCase.Create()
  else if name = 'RegulatedAuthorization' then
    result := TFhirRegulatedAuthorization.Create()
{$ENDIF FHIR_REGULATEDAUTHORIZATION}
{$IFDEF FHIR_RELATEDPERSON}
  else if name = 'RelatedPerson.communication' then
    result := TFhirRelatedPersonCommunication.Create()
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
  else if name = 'RequestGroup.action.participant' then
    result := TFhirRequestGroupActionParticipant.Create()
  else if name = 'RequestGroup' then
    result := TFhirRequestGroup.Create()
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_REQUESTORCHESTRATION}
  else if name = 'RequestOrchestration.action' then
    result := TFhirRequestOrchestrationAction.Create()
  else if name = 'RequestOrchestration.action.condition' then
    result := TFhirRequestOrchestrationActionCondition.Create()
  else if name = 'RequestOrchestration.action.input' then
    result := TFhirRequestOrchestrationActionInput.Create()
  else if name = 'RequestOrchestration.action.output' then
    result := TFhirRequestOrchestrationActionOutput.Create()
  else if name = 'RequestOrchestration.action.relatedAction' then
    result := TFhirRequestOrchestrationActionRelatedAction.Create()
  else if name = 'RequestOrchestration.action.participant' then
    result := TFhirRequestOrchestrationActionParticipant.Create()
  else if name = 'RequestOrchestration.action.dynamicValue' then
    result := TFhirRequestOrchestrationActionDynamicValue.Create()
  else if name = 'RequestOrchestration' then
    result := TFhirRequestOrchestration.Create()
{$ENDIF FHIR_REQUESTORCHESTRATION}
{$IFDEF FHIR_REQUIREMENTS}
  else if name = 'Requirements.statement' then
    result := TFhirRequirementsStatement.Create()
  else if name = 'Requirements' then
    result := TFhirRequirements.Create()
{$ENDIF FHIR_REQUIREMENTS}
{$IFDEF FHIR_RESEARCHSTUDY}
  else if name = 'ResearchStudy.label' then
    result := TFhirResearchStudyLabel.Create()
  else if name = 'ResearchStudy.focus' then
    result := TFhirResearchStudyFocus.Create()
  else if name = 'ResearchStudy.associatedParty' then
    result := TFhirResearchStudyAssociatedParty.Create()
  else if name = 'ResearchStudy.progressStatus' then
    result := TFhirResearchStudyProgressStatus.Create()
  else if name = 'ResearchStudy.recruitment' then
    result := TFhirResearchStudyRecruitment.Create()
  else if name = 'ResearchStudy.comparisonGroup' then
    result := TFhirResearchStudyComparisonGroup.Create()
  else if name = 'ResearchStudy.objective' then
    result := TFhirResearchStudyObjective.Create()
  else if name = 'ResearchStudy.outcomeMeasure' then
    result := TFhirResearchStudyOutcomeMeasure.Create()
  else if name = 'ResearchStudy.webLocation' then
    result := TFhirResearchStudyWebLocation.Create()
  else if name = 'ResearchStudy' then
    result := TFhirResearchStudy.Create()
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  else if name = 'ResearchSubject.progress' then
    result := TFhirResearchSubjectProgress.Create()
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
{$IFDEF FHIR_SERVICEREQUEST}
  else if name = 'ServiceRequest' then
    result := TFhirServiceRequest.Create()
{$ENDIF FHIR_SERVICEREQUEST}
{$IFDEF FHIR_SLOT}
  else if name = 'Slot' then
    result := TFhirSlot.Create()
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  else if name = 'Specimen.feature' then
    result := TFhirSpecimenFeature.Create()
  else if name = 'Specimen.collection' then
    result := TFhirSpecimenCollection.Create()
  else if name = 'Specimen.processing' then
    result := TFhirSpecimenProcessing.Create()
  else if name = 'Specimen.container' then
    result := TFhirSpecimenContainer.Create()
  else if name = 'Specimen' then
    result := TFhirSpecimen.Create()
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_SPECIMENDEFINITION}
  else if name = 'SpecimenDefinition.typeTested' then
    result := TFhirSpecimenDefinitionTypeTested.Create()
  else if name = 'SpecimenDefinition.typeTested.container' then
    result := TFhirSpecimenDefinitionTypeTestedContainer.Create()
  else if name = 'SpecimenDefinition.typeTested.container.additive' then
    result := TFhirSpecimenDefinitionTypeTestedContainerAdditive.Create()
  else if name = 'SpecimenDefinition.typeTested.handling' then
    result := TFhirSpecimenDefinitionTypeTestedHandling.Create()
  else if name = 'SpecimenDefinition' then
    result := TFhirSpecimenDefinition.Create()
{$ENDIF FHIR_SPECIMENDEFINITION}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  else if name = 'StructureDefinition.mapping' then
    result := TFhirStructureDefinitionMapping.Create()
  else if name = 'StructureDefinition.context' then
    result := TFhirStructureDefinitionContext.Create()
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
  else if name = 'Subscription.filterBy' then
    result := TFhirSubscriptionFilterBy.Create()
  else if name = 'Subscription' then
    result := TFhirSubscription.Create()
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
  else if name = 'SubscriptionStatus.notificationEvent' then
    result := TFhirSubscriptionStatusNotificationEvent.Create()
  else if name = 'SubscriptionStatus' then
    result := TFhirSubscriptionStatus.Create()
{$ENDIF FHIR_SUBSCRIPTIONSTATUS}
{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
  else if name = 'SubscriptionTopic.resourceTrigger' then
    result := TFhirSubscriptionTopicResourceTrigger.Create()
  else if name = 'SubscriptionTopic.resourceTrigger.queryCriteria' then
    result := TFhirSubscriptionTopicResourceTriggerQueryCriteria.Create()
  else if name = 'SubscriptionTopic.eventTrigger' then
    result := TFhirSubscriptionTopicEventTrigger.Create()
  else if name = 'SubscriptionTopic.canFilterBy' then
    result := TFhirSubscriptionTopicCanFilterBy.Create()
  else if name = 'SubscriptionTopic.notificationShape' then
    result := TFhirSubscriptionTopicNotificationShape.Create()
  else if name = 'SubscriptionTopic' then
    result := TFhirSubscriptionTopic.Create()
{$ENDIF FHIR_SUBSCRIPTIONTOPIC}
{$IFDEF FHIR_SUBSTANCE}
  else if name = 'Substance.ingredient' then
    result := TFhirSubstanceIngredient.Create()
  else if name = 'Substance' then
    result := TFhirSubstance.Create()
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUBSTANCEDEFINITION}
  else if name = 'SubstanceDefinition.moiety' then
    result := TFhirSubstanceDefinitionMoiety.Create()
  else if name = 'SubstanceDefinition.property' then
    result := TFhirSubstanceDefinitionProperty.Create()
  else if name = 'SubstanceDefinition.molecularWeight' then
    result := TFhirSubstanceDefinitionMolecularWeight.Create()
  else if name = 'SubstanceDefinition.structure' then
    result := TFhirSubstanceDefinitionStructure.Create()
  else if name = 'SubstanceDefinition.structure.representation' then
    result := TFhirSubstanceDefinitionStructureRepresentation.Create()
  else if name = 'SubstanceDefinition.code' then
    result := TFhirSubstanceDefinitionCode.Create()
  else if name = 'SubstanceDefinition.name' then
    result := TFhirSubstanceDefinitionName.Create()
  else if name = 'SubstanceDefinition.name.official' then
    result := TFhirSubstanceDefinitionNameOfficial.Create()
  else if name = 'SubstanceDefinition.relationship' then
    result := TFhirSubstanceDefinitionRelationship.Create()
  else if name = 'SubstanceDefinition.sourceMaterial' then
    result := TFhirSubstanceDefinitionSourceMaterial.Create()
  else if name = 'SubstanceDefinition' then
    result := TFhirSubstanceDefinition.Create()
{$ENDIF FHIR_SUBSTANCEDEFINITION}
{$IFDEF FHIR_SUBSTANCENUCLEICACID}
  else if name = 'SubstanceNucleicAcid.subunit' then
    result := TFhirSubstanceNucleicAcidSubunit.Create()
  else if name = 'SubstanceNucleicAcid.subunit.linkage' then
    result := TFhirSubstanceNucleicAcidSubunitLinkage.Create()
  else if name = 'SubstanceNucleicAcid.subunit.sugar' then
    result := TFhirSubstanceNucleicAcidSubunitSugar.Create()
  else if name = 'SubstanceNucleicAcid' then
    result := TFhirSubstanceNucleicAcid.Create()
{$ENDIF FHIR_SUBSTANCENUCLEICACID}
{$IFDEF FHIR_SUBSTANCEPOLYMER}
  else if name = 'SubstancePolymer.monomerSet' then
    result := TFhirSubstancePolymerMonomerSet.Create()
  else if name = 'SubstancePolymer.monomerSet.startingMaterial' then
    result := TFhirSubstancePolymerMonomerSetStartingMaterial.Create()
  else if name = 'SubstancePolymer.repeat' then
    result := TFhirSubstancePolymerRepeat.Create()
  else if name = 'SubstancePolymer.repeat.repeatUnit' then
    result := TFhirSubstancePolymerRepeatRepeatUnit.Create()
  else if name = 'SubstancePolymer.repeat.repeatUnit.degreeOfPolymerisation' then
    result := TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation.Create()
  else if name = 'SubstancePolymer.repeat.repeatUnit.structuralRepresentation' then
    result := TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentation.Create()
  else if name = 'SubstancePolymer' then
    result := TFhirSubstancePolymer.Create()
{$ENDIF FHIR_SUBSTANCEPOLYMER}
{$IFDEF FHIR_SUBSTANCEPROTEIN}
  else if name = 'SubstanceProtein.subunit' then
    result := TFhirSubstanceProteinSubunit.Create()
  else if name = 'SubstanceProtein' then
    result := TFhirSubstanceProtein.Create()
{$ENDIF FHIR_SUBSTANCEPROTEIN}
{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  else if name = 'SubstanceReferenceInformation.gene' then
    result := TFhirSubstanceReferenceInformationGene.Create()
  else if name = 'SubstanceReferenceInformation.geneElement' then
    result := TFhirSubstanceReferenceInformationGeneElement.Create()
  else if name = 'SubstanceReferenceInformation.target' then
    result := TFhirSubstanceReferenceInformationTarget.Create()
  else if name = 'SubstanceReferenceInformation' then
    result := TFhirSubstanceReferenceInformation.Create()
{$ENDIF FHIR_SUBSTANCEREFERENCEINFORMATION}
{$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
  else if name = 'SubstanceSourceMaterial.fractionDescription' then
    result := TFhirSubstanceSourceMaterialFractionDescription.Create()
  else if name = 'SubstanceSourceMaterial.organism' then
    result := TFhirSubstanceSourceMaterialOrganism.Create()
  else if name = 'SubstanceSourceMaterial.organism.author' then
    result := TFhirSubstanceSourceMaterialOrganismAuthor.Create()
  else if name = 'SubstanceSourceMaterial.organism.hybrid' then
    result := TFhirSubstanceSourceMaterialOrganismHybrid.Create()
  else if name = 'SubstanceSourceMaterial.organism.organismGeneral' then
    result := TFhirSubstanceSourceMaterialOrganismOrganismGeneral.Create()
  else if name = 'SubstanceSourceMaterial.partDescription' then
    result := TFhirSubstanceSourceMaterialPartDescription.Create()
  else if name = 'SubstanceSourceMaterial' then
    result := TFhirSubstanceSourceMaterial.Create()
{$ENDIF FHIR_SUBSTANCESOURCEMATERIAL}
{$IFDEF FHIR_SUPPLYDELIVERY}
  else if name = 'SupplyDelivery.suppliedItem' then
    result := TFhirSupplyDeliverySuppliedItem.Create()
  else if name = 'SupplyDelivery' then
    result := TFhirSupplyDelivery.Create()
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  else if name = 'SupplyRequest.parameter' then
    result := TFhirSupplyRequestParameter.Create()
  else if name = 'SupplyRequest' then
    result := TFhirSupplyRequest.Create()
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TASK}
  else if name = 'Task.restriction' then
    result := TFhirTaskRestriction.Create()
  else if name = 'Task.input' then
    result := TFhirTaskInput.Create()
  else if name = 'Task.output' then
    result := TFhirTaskOutput.Create()
  else if name = 'Task' then
    result := TFhirTask.Create()
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  else if name = 'TerminologyCapabilities.software' then
    result := TFhirTerminologyCapabilitiesSoftware.Create()
  else if name = 'TerminologyCapabilities.implementation' then
    result := TFhirTerminologyCapabilitiesImplementation.Create()
  else if name = 'TerminologyCapabilities.codeSystem' then
    result := TFhirTerminologyCapabilitiesCodeSystem.Create()
  else if name = 'TerminologyCapabilities.codeSystem.version' then
    result := TFhirTerminologyCapabilitiesCodeSystemVersion.Create()
  else if name = 'TerminologyCapabilities.codeSystem.version.filter' then
    result := TFhirTerminologyCapabilitiesCodeSystemVersionFilter.Create()
  else if name = 'TerminologyCapabilities.expansion' then
    result := TFhirTerminologyCapabilitiesExpansion.Create()
  else if name = 'TerminologyCapabilities.expansion.parameter' then
    result := TFhirTerminologyCapabilitiesExpansionParameter.Create()
  else if name = 'TerminologyCapabilities.validateCode' then
    result := TFhirTerminologyCapabilitiesValidateCode.Create()
  else if name = 'TerminologyCapabilities.translation' then
    result := TFhirTerminologyCapabilitiesTranslation.Create()
  else if name = 'TerminologyCapabilities.closure' then
    result := TFhirTerminologyCapabilitiesClosure.Create()
  else if name = 'TerminologyCapabilities' then
    result := TFhirTerminologyCapabilities.Create()
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}
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
  else if name = 'TestScript.scope' then
    result := TFhirTestScriptScope.Create()
  else if name = 'TestScript.fixture' then
    result := TFhirTestScriptFixture.Create()
  else if name = 'TestScript.variable' then
    result := TFhirTestScriptVariable.Create()
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
{$IFDEF FHIR_TRANSPORT}
  else if name = 'Transport.restriction' then
    result := TFhirTransportRestriction.Create()
  else if name = 'Transport.input' then
    result := TFhirTransportInput.Create()
  else if name = 'Transport.output' then
    result := TFhirTransportOutput.Create()
  else if name = 'Transport' then
    result := TFhirTransport.Create()
{$ENDIF FHIR_TRANSPORT}
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
  else if name = 'ValueSet.expansion.property' then
    result := TFhirValueSetExpansionProperty.Create()
  else if name = 'ValueSet.expansion.contains' then
    result := TFhirValueSetExpansionContains.Create()
  else if name = 'ValueSet.expansion.contains.property' then
    result := TFhirValueSetExpansionContainsProperty.Create()
  else if name = 'ValueSet.expansion.contains.property.subProperty' then
    result := TFhirValueSetExpansionContainsPropertySubProperty.Create()
  else if name = 'ValueSet.scope' then
    result := TFhirValueSetScope.Create()
  else if name = 'ValueSet' then
    result := TFhirValueSet.Create()
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VERIFICATIONRESULT}
  else if name = 'VerificationResult.primarySource' then
    result := TFhirVerificationResultPrimarySource.Create()
  else if name = 'VerificationResult.attestation' then
    result := TFhirVerificationResultAttestation.Create()
  else if name = 'VerificationResult.validator' then
    result := TFhirVerificationResultValidator.Create()
  else if name = 'VerificationResult' then
    result := TFhirVerificationResult.Create()
{$ENDIF FHIR_VERIFICATIONRESULT}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  else if name = 'VisionPrescription.lensSpecification' then
    result := TFhirVisionPrescriptionLensSpecification.Create()
  else if name = 'VisionPrescription.lensSpecification.prism' then
    result := TFhirVisionPrescriptionLensSpecificationPrism.Create()
  else if name = 'VisionPrescription' then
    result := TFhirVisionPrescription.Create()
{$ENDIF FHIR_VISIONPRESCRIPTION}
{gen-factory-end}
  else
    result := nil;
end;

end.
