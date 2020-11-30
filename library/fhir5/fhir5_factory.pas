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
  fsl_base, fsl_utilities, fsl_stream, fsl_http,
  fsl_ucum,
  fhir_objects, fhir_parser, fhir_validator, fhir_narrative, fhir_factory, fhir_pathengine, fhir_xhtml, fhir_common,  fhir_elementmodel,
  fhir_client, fhir_client_threaded;

type
  TFHIRFactoryR5 = class (TFHIRFactory)
  public
    function link : TFHIRFactoryR5; overload;
    function version : TFHIRVersion; override;
    function versionString : String; override;
    function versionName : String; override;
    function corePackage : String; override;
    function txPackage : String; override;
    function txSupportPackage : String; override;
    function specUrl : String; override;
    function description : String; override;
    function resourceNames : TArray<String>; override;
    function resCategory(name: String) : TTokenCategory; override;
    function canonicalResources : TArray<String>; override;
    function makeParser(worker : TFHIRWorkerContextV; format : TFHIRFormat; const lang : THTTPLanguages) : TFHIRParser; override;
    function makeComposer(worker : TFHIRWorkerContextV; format : TFHIRFormat; const lang : THTTPLanguages; style: TFHIROutputStyle) : TFHIRComposer; override;
    function makeValidator(worker : TFHIRWorkerContextV) : TFHIRValidatorV; override;
    function makeGenerator(worker : TFHIRWorkerContextV) : TFHIRNarrativeGeneratorBase; override;
    function makePathEngine(worker : TFHIRWorkerContextV; ucum : TUcumServiceInterface) : TFHIRPathEngineV; override;
    function makeElementModelManager : TFHIRBaseMMManager; override;
    function createFromProfile(worker : TFHIRWorkerContextV; profile : TFhirStructureDefinitionW) : TFHIRResourceV; override;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal; proxy : String) : TFhirClientV; overload; override;
    function makeClientThreaded(worker : TFHIRWorkerContextV; internal : TFhirClientV; event : TThreadManagementEvent) : TFhirClientV; overload; override;
    function makeClientInt(worker : TFHIRWorkerContextV; const lang : THTTPLanguages; comm : TFHIRClientCommunicator) : TFhirClientV; overload; override;

    function getXhtml(res : TFHIRResourceV) : TFHIRXhtmlNode; override;
    function resetXhtml(res : TFHIRResourceV) : TFHIRXhtmlNode; override;
    procedure setXhtml(res : TFHIRResourceV; x : TFHIRXhtmlNode); override;
    function getContained(r : TFHIRResourceV) : TFslList<TFHIRResourceV>; override;
    procedure markWithTag(r : TFHIRResourceV; systemUri, code, display : String); override;

    procedure checkNoModifiers(res : TFHIRObject; method, param : string; allowed : TArray<String> = nil); override;
    function buildOperationOutcome(const lang : THTTPLanguages; e : Exception; issueCode : TFhirIssueType = itNull) : TFhirResourceV; overload; override;
    Function buildOperationOutcome(const lang : THTTPLanguages; message : String; issueCode : TFhirIssueType = itNull) : TFhirResourceV; overload; override;

    function makeByName(const name : String) : TFHIRObject; override;
    function makeBoolean(b : boolean): TFHIRObject; override;
    function makeCode(s : string) : TFHIRObject; override;
    function makeString(s : string) : TFHIRObject; override;
    function makeInteger(s : string) : TFHIRObject; override;
    function makeDecimal(s : string) : TFHIRObject; override;
    function makeBase64Binary(s : string) : TFHIRObject; override;
    function makeDateTime(value : TFslDateTime) : TFHIRObject; override;
    function makeParameters : TFHIRParametersW; override;
    function wrapCapabilityStatement(r : TFHIRResourceV) : TFHIRCapabilityStatementW; override;
    function wrapStructureDefinition(r : TFHIRResourceV) : TFhirStructureDefinitionW; override;
    function wrapValueSet(r : TFHIRResourceV) : TFhirValueSetW; override;
    function wrapCodeSystem(r : TFHIRResourceV) : TFhirCodeSystemW; override;
    function wrapExtension(o : TFHIRObject) : TFhirExtensionW; override;
    function wrapCoding(o : TFHIRObject) : TFhirCodingW; override;
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
    function makeDtFromForm(part : TMimePart; const lang : THTTPLanguages; name : String; type_ : string) : TFHIRXVersionElementWrapper; override;
    function makeCoding(system, version, code, display : String) : TFHIRObject; override;
    function makeTerminologyCapablities : TFhirTerminologyCapabilitiesW; override;
    function makeDuration(dt : TDateTime) : TFHIRObject; override;
    function wrapPeriod(r : TFHIRObject) : TFhirPeriodW; override;
    function makeValueSetContains : TFhirValueSetExpansionContainsW; override;
    function makeBundle(list : TFslList<TFHIRResourceV>) : TFHIRBundleW; override;
  end;
  TFHIRFactoryX = TFHIRFactoryR5;

implementation

uses
  fhir_client_http,
  fhir5_enums, fhir5_types, fhir5_resources, fhir5_resources_base, fhir5_parser, fhir5_context, fhir5_validator, fhir5_profiles, fhir5_operations,
  fhir5_narrative, fhir5_pathengine, fhir5_constants, fhir5_client, fhir5_common, fhir5_utilities, fhir5_authmap,
  fhir5_elementmodel;

{ TFHIRFactoryR5 }

function TFHIRFactoryR5.buildOperationOutcome(const lang : THTTPLanguages; message: String; issueCode: TFhirIssueType): TFhirResourceV;
begin
  result := fhir5_utilities.BuildOperationOutcome(lang, message, ExceptionTypeTranslations[issueCode]);
end;

function TFHIRFactoryR5.buildOperationOutcome(const lang : THTTPLanguages; e: Exception; issueCode: TFhirIssueType): TFhirResourceV;
begin
  result := fhir5_utilities.BuildOperationOutcome(lang, e, ExceptionTypeTranslations[issueCode]);
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
end;

function TFHIRFactoryR5.corePackage: String;
begin
  result := 'hl7.fhir.r5.core';
end;

function TFHIRFactoryR5.createFromProfile(worker: TFHIRWorkerContextV; profile: TFhirStructureDefinitionW): TFHIRResourceV;
var
  pu : TProfileUtilities;
begin
  pu := TProfileUtilities.create(worker.Link as TFHIRWorkerContext, nil);
  try
    result := pu.populateByProfile(profile.Resource as TFhirStructureDefinition);
  finally
    pu.Free;
  end;
end;

function TFHIRFactoryR5.description: String;
begin
  result := 'R5 ('+FHIR_GENERATED_VERSION+')';
end;

function TFHIRFactoryR5.getContained(r: TFHIRResourceV): TFslList<TFHIRResourceV>;
var
  res : TFHIRResource;
begin
  result := TFslList<TFHIRResourceV>.create;
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
    result := TFhirClient5.create(worker, THTTPLanguages.create('en'), http.link);
    try
      result.format := fmt;
      result.link;
    finally
      result.Free;
    end;
  finally
    http.free;
  end;
end;

function TFHIRFactoryR5.makeClientInt(worker: TFHIRWorkerContextV; const lang : THTTPLanguages; comm: TFHIRClientCommunicator): TFhirClientV;
begin
  result := TFhirClient5.create(worker, THTTPLanguages.create('en'), comm);
end;

function TFHIRFactoryR5.makeClientThreaded(worker: TFHIRWorkerContextV; internal: TFhirClientV; event: TThreadManagementEvent): TFhirClientV;
var
  c : TFhirThreadedCommunicator;
begin
  c := TFhirThreadedCommunicator.Create(internal, event);
  try
    result := TFhirClient5.create(worker, THTTPLanguages.create('en'), c.link);
    try
      result.format := internal.format;
      result.link;
    finally
      result.Free;
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
  result := TFHIRCoding.create(system, code);
  if version <> '' then
    TFHIRCoding(result).version := version;
  if display <> '' then
    TFHIRCoding(result).version := display;
end;

function TFHIRFactoryR5.makeComposer(worker: TFHIRWorkerContextV; format: TFHIRFormat; const lang : THTTPLanguages; style: TFHIROutputStyle): TFHIRComposer;
begin
  result := TFHIRParsers5.composer(worker as TFHIRWorkerContext, format, lang, style);
end;

function TFHIRFactoryR5.makeDateTime(value: TFslDateTime): TFHIRObject;
begin
  result := TFhirDateTime.Create(value);
end;

function TFHIRFactoryR5.makeDecimal(s: string): TFHIRObject;
begin
  result := TFhirDecimal.Create(s);
end;

function TFHIRFactoryR5.makeDtFromForm(part: TMimePart; const lang : THTTPLanguages; name: String; type_: string): TFHIRXVersionElementWrapper;
begin
  if type_ = 'Coding' then
    result := wrapCoding(LoadDTFromFormParam(nil, part, lang, name, TFhirCoding))
  else if type_ = 'CodeableConcept' then
    result := wrapCodeableConcept(LoadDTFromFormParam(nil, part, lang, name, TFhirCodeableConcept))
  else
    raise EFHIRException.create('Unknown Supported Data Type '+type_);
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
  result := TFHIRNarrativeGenerator.create(worker);
end;

function TFHIRFactoryR5.makeInteger(s: string): TFHIRObject;
begin
  result := TFhirInteger.Create(s);
end;

function TFHIRFactoryR5.makeIssue(level : TIssueSeverity; issue: TFhirIssueType; location, message: String): TFhirOperationOutcomeIssueW;
var
  iss : TFhirOperationOutcomeIssue;
begin
  iss := TFhirOperationOutcomeIssue.create;
  try
    iss.severity := ISSUE_SEVERITY_MAP2[level];
    iss.code := ExceptionTypeTranslations[issue];
    iss.details := TFhirCodeableConcept.Create;
    iss.details.text := message;
    iss.locationList.add(location);
    result := TFhirOperationOutcomeIssue5.create(iss.Link);
  finally
    iss.Free;
  end;
end;

function TFHIRFactoryR5.makeOpReqLookup: TFHIRLookupOpRequestW;
begin
  result := TFHIRLookupOpRequest5.create(TFHIRLookupOpRequest.create);
end;

function TFHIRFactoryR5.makeOpReqSubsumes: TFHIRSubsumesOpRequestW;
begin
  result := TFHIRSubsumesOpRequest5.Create(TFHIRSubsumesOpRequest.create);
end;

function TFHIRFactoryR5.makeOpRespLookup: TFHIRLookupOpResponseW;
begin
  result := TFHIRLookupOpResponse5.create(TFHIRLookupOpResponse.create);
end;

function TFHIRFactoryR5.makeOpRespSubsumes: TFHIRSubsumesOpResponseW;
begin
  result := TFHIRSubsumesOpResponse5.Create(TFHIRSubsumesOpResponse.create);
end;

function TFHIRFactoryR5.makeParameters: TFHIRParametersW;
begin
  result := TFHIRParameters5.Create(TFHIRParameters.Create);
end;

function TFHIRFactoryR5.makeParamsFromForm(s: TStream): TFHIRResourceV;
begin
  result := parseParamsFromForm(s);
end;

function TFHIRFactoryR5.makeParser(worker: TFHIRWorkerContextV; format: TFHIRFormat; const lang : THTTPLanguages): TFHIRParser;
begin
  result := TFHIRParsers5.parser(worker as TFHIRWorkerContext, format, lang);
end;

function TFHIRFactoryR5.makePathEngine(worker: TFHIRWorkerContextV; ucum : TUcumServiceInterface): TFHIRPathEngineV;
begin
  result := TFHIRPathEngine.Create(worker as TFHIRWorkerContext, ucum);
end;

function TFHIRFactoryR5.makeString(s: string): TFHIRObject;
begin
  result := TFhirString.Create(s);
end;

function TFHIRFactoryR5.makeTerminologyCapablities: TFhirTerminologyCapabilitiesW;
begin
  result := TFhirTerminologyCapabilities5.create(TFhirTerminologyCapabilities.create);
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
    res.meta := TFHIRMeta.create;
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
    raise EFHIRException.create('Unable to set xhtml on nil resource');
  end;
  if not (res is TFHIRDomainResource) then
  begin
    x.free;
    raise EFHIRException.create('Unable to set xhtml on non-domain resource');
  end;
  r := res as TFHIRDomainResource;
  if (r.text = nil) then
  begin
    r.text := TFHIRNarrative.create;
    r.text.status := NarrativeStatusGenerated;
  end;
  r.text.div_ := x;
end;

function TFHIRFactoryR5.specUrl: String;
begin
  result := 'http://build.fhir.org';
end;

function TFHIRFactoryR5.txPackage: String;
begin
  result := 'hl7.terminology.r4';
end;

function TFHIRFactoryR5.txSupportPackage: String;
begin
  result := 'fhir.tx.support.r4';
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
    result := TFHIRCapabilityStatement5.create(r);
end;

function TFHIRFactoryR5.wrapCodeableConcept(o: TFHIRObject): TFhirCodeableConceptW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirCodeableConcept5.create(o);
end;

function TFHIRFactoryR5.wrapCodeSystem(r: TFHIRResourceV): TFhirCodeSystemW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRCodeSystem5.create(r);
end;

function TFHIRFactoryR5.wrapCoding(o: TFHIRObject): TFhirCodingW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirCoding5.create(o);
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
  result := TFhirExtension5.create(o);
end;

function TFHIRFactoryR5.wrapGroup(r: TFHIRResourceV): TFhirGroupW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirGroup5.Create(r);
end;

function TFHIRFactoryR5.wrapMeta(r: TFHIRObject): TFhirMetaW;
begin
  if r = nil then
    result := nil
  else if r.isResource then
  begin
    result := TFHIRMeta5.create((r as TFHIRResource).meta.link);
    TFHIRMeta5(result).resource := (r as TFHIRResource).link;
  end
  else
    result := TFHIRMeta5.create((r as TFhirMeta))
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
  result := TFHIRMeta5.create((r as TFHIRResource).meta.link);
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
    result := TFHIRStructureDefinition5.create(r);
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
    result := TFHIRValueSet5.create(r);
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
    result.Free;
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
    for r in list do
    begin
      bnd.entryList.Append.resource := r.link as TFhirResource;
    end;
    result := TFHIRBundle5.Create(bnd.link);
  finally
    bnd.Free;
  end;
end;

function TFHIRFactoryR5.makeByName(const name : String) : TFHIRObject;
begin
  if name = 'enum' then
    result := TFhirEnum.create()
  else if name = 'date' then
    result := TFhirDate.create()
  else if name = 'dateTime' then
    result := TFhirDateTime.create()
  else if name = 'string' then
    result := TFhirString.create()
  else if name = 'integer' then
    result := TFhirInteger.create()
  else if name = 'integer64' then
    result := TFhirInteger64.create()
  else if name = 'uri' then
    result := TFhirUri.create()
  else if name = 'instant' then
    result := TFhirInstant.create()
  else if name = 'xhtml' then
    result := TFhirXhtml.create()
  else if name = 'boolean' then
    result := TFhirBoolean.create()
  else if name = 'base64Binary' then
    result := TFhirBase64Binary.create()
  else if name = 'time' then
    result := TFhirTime.create()
  else if name = 'decimal' then
    result := TFhirDecimal.create()
  else if name = 'code' then
    result := TFhirCode.create()
  else if name = 'canonical' then
    result := TFhirCanonical.create()
  else if name = 'oid' then
    result := TFhirOid.create()
  else if name = 'uuid' then
    result := TFhirUuid.create()
  else if name = 'url' then
    result := TFhirUrl.create()
  else if name = 'markdown' then
    result := TFhirMarkdown.create()
  else if name = 'unsignedInt' then
    result := TFhirUnsignedInt.create()
  else if name = 'id' then
    result := TFhirId.create()
  else if name = 'positiveInt' then
    result := TFhirPositiveInt.create()
{gen-factory-start}
  else if name = 'Address' then
    result := TFhirAddress.create()
  else if name = 'Annotation' then
    result := TFhirAnnotation.create()
  else if name = 'Attachment' then
    result := TFhirAttachment.create()
  else if name = 'CodeableConcept' then
    result := TFhirCodeableConcept.create()
  else if name = 'CodeableReference' then
    result := TFhirCodeableReference.create()
  else if name = 'Coding' then
    result := TFhirCoding.create()
  else if name = 'ContactDetail' then
    result := TFhirContactDetail.create()
  else if name = 'ContactPoint' then
    result := TFhirContactPoint.create()
  else if name = 'Contributor' then
    result := TFhirContributor.create()
  else if name = 'DataRequirement' then
    result := TFhirDataRequirement.create()
  else if name = 'Expression' then
    result := TFhirExpression.create()
  else if name = 'Extension' then
    result := TFhirExtension.create()
  else if name = 'HumanName' then
    result := TFhirHumanName.create()
  else if name = 'Identifier' then
    result := TFhirIdentifier.create()
  else if name = 'Meta' then
    result := TFhirMeta.create()
  else if name = 'Money' then
    result := TFhirMoney.create()
  else if name = 'Narrative' then
    result := TFhirNarrative.create()
  else if name = 'ParameterDefinition' then
    result := TFhirParameterDefinition.create()
  else if name = 'Period' then
    result := TFhirPeriod.create()
  else if name = 'Quantity' then
    result := TFhirQuantity.create()
  else if name = 'Range' then
    result := TFhirRange.create()
  else if name = 'Ratio' then
    result := TFhirRatio.create()
  else if name = 'Reference' then
    result := TFhirReference.create()
  else if name = 'RelatedArtifact' then
    result := TFhirRelatedArtifact.create()
  else if name = 'SampledData' then
    result := TFhirSampledData.create()
  else if name = 'Signature' then
    result := TFhirSignature.create()
  else if name = 'TriggerDefinition' then
    result := TFhirTriggerDefinition.create()
  else if name = 'UsageContext' then
    result := TFhirUsageContext.create()
  else if name = 'Age' then
    result := TFhirAge.create()
  else if name = 'Count' then
    result := TFhirCount.create()
  else if name = 'Distance' then
    result := TFhirDistance.create()
  else if name = 'Dosage' then
    result := TFhirDosage.create()
  else if name = 'Duration' then
    result := TFhirDuration.create()
  else if name = 'ElementDefinition' then
    result := TFhirElementDefinition.create()
  else if name = 'MarketingStatus' then
    result := TFhirMarketingStatus.create()
  else if name = 'OrderedDistribution' then
    result := TFhirOrderedDistribution.create()
  else if name = 'Population' then
    result := TFhirPopulation.create()
  else if name = 'ProdCharacteristic' then
    result := TFhirProdCharacteristic.create()
  else if name = 'ProductShelfLife' then
    result := TFhirProductShelfLife.create()
  else if name = 'Statistic' then
    result := TFhirStatistic.create()
  else if name = 'Timing' then
    result := TFhirTiming.create()
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
    result := TFhirAccountCoverage.create()
  else if name = 'Account.guarantor' then
    result := TFhirAccountGuarantor.create()
  else if name = 'Account' then
    result := TFhirAccount.create()
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  else if name = 'ActivityDefinition.participant' then
    result := TFhirActivityDefinitionParticipant.create()
  else if name = 'ActivityDefinition.dynamicValue' then
    result := TFhirActivityDefinitionDynamicValue.create()
  else if name = 'ActivityDefinition' then
    result := TFhirActivityDefinition.create()
{$ENDIF FHIR_ACTIVITYDEFINITION}
{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
  else if name = 'AdministrableProductDefinition.property' then
    result := TFhirAdministrableProductDefinitionProperty.create()
  else if name = 'AdministrableProductDefinition.routeOfAdministration' then
    result := TFhirAdministrableProductDefinitionRouteOfAdministration.create()
  else if name = 'AdministrableProductDefinition.routeOfAdministration.targetSpecies' then
    result := TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpecies.create()
  else if name = 'AdministrableProductDefinition.routeOfAdministration.targetSpecies.withdrawalPeriod' then
    result := TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriod.create()
  else if name = 'AdministrableProductDefinition' then
    result := TFhirAdministrableProductDefinition.create()
{$ENDIF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
{$IFDEF FHIR_ADVERSEEVENT}
  else if name = 'AdverseEvent.participant' then
    result := TFhirAdverseEventParticipant.create()
  else if name = 'AdverseEvent.suspectEntity' then
    result := TFhirAdverseEventSuspectEntity.create()
  else if name = 'AdverseEvent.suspectEntity.causality' then
    result := TFhirAdverseEventSuspectEntityCausality.create()
  else if name = 'AdverseEvent.contributingFactor' then
    result := TFhirAdverseEventContributingFactor.create()
  else if name = 'AdverseEvent.preventiveAction' then
    result := TFhirAdverseEventPreventiveAction.create()
  else if name = 'AdverseEvent.mitigatingAction' then
    result := TFhirAdverseEventMitigatingAction.create()
  else if name = 'AdverseEvent.supportingInfo' then
    result := TFhirAdverseEventSupportingInfo.create()
  else if name = 'AdverseEvent' then
    result := TFhirAdverseEvent.create()
{$ENDIF FHIR_ADVERSEEVENT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  else if name = 'AllergyIntolerance.reaction' then
    result := TFhirAllergyIntoleranceReaction.create()
  else if name = 'AllergyIntolerance' then
    result := TFhirAllergyIntolerance.create()
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  else if name = 'Appointment.participant' then
    result := TFhirAppointmentParticipant.create()
  else if name = 'Appointment' then
    result := TFhirAppointment.create()
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  else if name = 'AppointmentResponse' then
    result := TFhirAppointmentResponse.create()
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_AUDITEVENT}
  else if name = 'AuditEvent.agent' then
    result := TFhirAuditEventAgent.create()
  else if name = 'AuditEvent.agent.network' then
    result := TFhirAuditEventAgentNetwork.create()
  else if name = 'AuditEvent.source' then
    result := TFhirAuditEventSource.create()
  else if name = 'AuditEvent.entity' then
    result := TFhirAuditEventEntity.create()
  else if name = 'AuditEvent.entity.detail' then
    result := TFhirAuditEventEntityDetail.create()
  else if name = 'AuditEvent' then
    result := TFhirAuditEvent.create()
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BASIC}
  else if name = 'Basic' then
    result := TFhirBasic.create()
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BINARY}
  else if name = 'Binary' then
    result := TFhirBinary.create()
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  else if name = 'BiologicallyDerivedProduct.collection' then
    result := TFhirBiologicallyDerivedProductCollection.create()
  else if name = 'BiologicallyDerivedProduct.processing' then
    result := TFhirBiologicallyDerivedProductProcessing.create()
  else if name = 'BiologicallyDerivedProduct.manipulation' then
    result := TFhirBiologicallyDerivedProductManipulation.create()
  else if name = 'BiologicallyDerivedProduct.storage' then
    result := TFhirBiologicallyDerivedProductStorage.create()
  else if name = 'BiologicallyDerivedProduct' then
    result := TFhirBiologicallyDerivedProduct.create()
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
{$IFDEF FHIR_BODYSTRUCTURE}
  else if name = 'BodyStructure' then
    result := TFhirBodyStructure.create()
{$ENDIF FHIR_BODYSTRUCTURE}
{$IFDEF FHIR_BUNDLE}
  else if name = 'Bundle.link' then
    result := TFhirBundleLink.create()
  else if name = 'Bundle.entry' then
    result := TFhirBundleEntry.create()
  else if name = 'Bundle.entry.search' then
    result := TFhirBundleEntrySearch.create()
  else if name = 'Bundle.entry.request' then
    result := TFhirBundleEntryRequest.create()
  else if name = 'Bundle.entry.response' then
    result := TFhirBundleEntryResponse.create()
  else if name = 'Bundle' then
    result := TFhirBundle.create()
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  else if name = 'CapabilityStatement.software' then
    result := TFhirCapabilityStatementSoftware.create()
  else if name = 'CapabilityStatement.implementation' then
    result := TFhirCapabilityStatementImplementation.create()
  else if name = 'CapabilityStatement.rest' then
    result := TFhirCapabilityStatementRest.create()
  else if name = 'CapabilityStatement.rest.security' then
    result := TFhirCapabilityStatementRestSecurity.create()
  else if name = 'CapabilityStatement.rest.resource' then
    result := TFhirCapabilityStatementRestResource.create()
  else if name = 'CapabilityStatement.rest.resource.interaction' then
    result := TFhirCapabilityStatementRestResourceInteraction.create()
  else if name = 'CapabilityStatement.rest.resource.searchParam' then
    result := TFhirCapabilityStatementRestResourceSearchParam.create()
  else if name = 'CapabilityStatement.rest.resource.operation' then
    result := TFhirCapabilityStatementRestResourceOperation.create()
  else if name = 'CapabilityStatement.rest.interaction' then
    result := TFhirCapabilityStatementRestInteraction.create()
  else if name = 'CapabilityStatement.messaging' then
    result := TFhirCapabilityStatementMessaging.create()
  else if name = 'CapabilityStatement.messaging.endpoint' then
    result := TFhirCapabilityStatementMessagingEndpoint.create()
  else if name = 'CapabilityStatement.messaging.supportedMessage' then
    result := TFhirCapabilityStatementMessagingSupportedMessage.create()
  else if name = 'CapabilityStatement.document' then
    result := TFhirCapabilityStatementDocument.create()
  else if name = 'CapabilityStatement' then
    result := TFhirCapabilityStatement.create()
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CAPABILITYSTATEMENT2}
  else if name = 'CapabilityStatement2.software' then
    result := TFhirCapabilityStatement2Software.create()
  else if name = 'CapabilityStatement2.implementation' then
    result := TFhirCapabilityStatement2Implementation.create()
  else if name = 'CapabilityStatement2.rest' then
    result := TFhirCapabilityStatement2Rest.create()
  else if name = 'CapabilityStatement2.rest.resource' then
    result := TFhirCapabilityStatement2RestResource.create()
  else if name = 'CapabilityStatement2.rest.resource.interaction' then
    result := TFhirCapabilityStatement2RestResourceInteraction.create()
  else if name = 'CapabilityStatement2.rest.resource.searchParam' then
    result := TFhirCapabilityStatement2RestResourceSearchParam.create()
  else if name = 'CapabilityStatement2.rest.resource.operation' then
    result := TFhirCapabilityStatement2RestResourceOperation.create()
  else if name = 'CapabilityStatement2.rest.interaction' then
    result := TFhirCapabilityStatement2RestInteraction.create()
  else if name = 'CapabilityStatement2' then
    result := TFhirCapabilityStatement2.create()
{$ENDIF FHIR_CAPABILITYSTATEMENT2}
{$IFDEF FHIR_CAREPLAN}
  else if name = 'CarePlan.activity' then
    result := TFhirCarePlanActivity.create()
  else if name = 'CarePlan.activity.detail' then
    result := TFhirCarePlanActivityDetail.create()
  else if name = 'CarePlan' then
    result := TFhirCarePlan.create()
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CARETEAM}
  else if name = 'CareTeam.participant' then
    result := TFhirCareTeamParticipant.create()
  else if name = 'CareTeam' then
    result := TFhirCareTeam.create()
{$ENDIF FHIR_CARETEAM}
{$IFDEF FHIR_CATALOGENTRY}
  else if name = 'CatalogEntry.relatedEntry' then
    result := TFhirCatalogEntryRelatedEntry.create()
  else if name = 'CatalogEntry' then
    result := TFhirCatalogEntry.create()
{$ENDIF FHIR_CATALOGENTRY}
{$IFDEF FHIR_CHARGEITEM}
  else if name = 'ChargeItem.performer' then
    result := TFhirChargeItemPerformer.create()
  else if name = 'ChargeItem' then
    result := TFhirChargeItem.create()
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CHARGEITEMDEFINITION}
  else if name = 'ChargeItemDefinition.applicability' then
    result := TFhirChargeItemDefinitionApplicability.create()
  else if name = 'ChargeItemDefinition.propertyGroup' then
    result := TFhirChargeItemDefinitionPropertyGroup.create()
  else if name = 'ChargeItemDefinition.propertyGroup.priceComponent' then
    result := TFhirChargeItemDefinitionPropertyGroupPriceComponent.create()
  else if name = 'ChargeItemDefinition' then
    result := TFhirChargeItemDefinition.create()
{$ENDIF FHIR_CHARGEITEMDEFINITION}
{$IFDEF FHIR_CITATION}
  else if name = 'Citation.summary' then
    result := TFhirCitationSummary.create()
  else if name = 'Citation.variantCitation' then
    result := TFhirCitationVariantCitation.create()
  else if name = 'Citation.journal' then
    result := TFhirCitationJournal.create()
  else if name = 'Citation.journal.journalIssue' then
    result := TFhirCitationJournalJournalIssue.create()
  else if name = 'Citation.journal.journalIssue.publicationDate' then
    result := TFhirCitationJournalJournalIssuePublicationDate.create()
  else if name = 'Citation.publicationInfo' then
    result := TFhirCitationPublicationInfo.create()
  else if name = 'Citation.publicationInfo.publishedIn' then
    result := TFhirCitationPublicationInfoPublishedIn.create()
  else if name = 'Citation.alternativeTitle' then
    result := TFhirCitationAlternativeTitle.create()
  else if name = 'Citation.pagination' then
    result := TFhirCitationPagination.create()
  else if name = 'Citation.articleUrl' then
    result := TFhirCitationArticleUrl.create()
  else if name = 'Citation.alternativeAbstract' then
    result := TFhirCitationAlternativeAbstract.create()
  else if name = 'Citation.contributorship' then
    result := TFhirCitationContributorship.create()
  else if name = 'Citation.contributorship.entry' then
    result := TFhirCitationContributorshipEntry.create()
  else if name = 'Citation.contributorship.entry.affiliationInfo' then
    result := TFhirCitationContributorshipEntryAffiliationInfo.create()
  else if name = 'Citation.contributorship.summary' then
    result := TFhirCitationContributorshipSummary.create()
  else if name = 'Citation.alternativeForm' then
    result := TFhirCitationAlternativeForm.create()
  else if name = 'Citation.alternativeForm.journalIssue' then
    result := TFhirCitationAlternativeFormJournalIssue.create()
  else if name = 'Citation.alternativeForm.journalIssue.publicationDate' then
    result := TFhirCitationAlternativeFormJournalIssuePublicationDate.create()
  else if name = 'Citation.alternativeForm.pagination' then
    result := TFhirCitationAlternativeFormPagination.create()
  else if name = 'Citation.alternativeForm.publicationInfo' then
    result := TFhirCitationAlternativeFormPublicationInfo.create()
  else if name = 'Citation.alternativeForm.publicationInfo.publishedIn' then
    result := TFhirCitationAlternativeFormPublicationInfoPublishedIn.create()
  else if name = 'Citation.keywordList' then
    result := TFhirCitationKeywordList.create()
  else if name = 'Citation.keywordList.keyword' then
    result := TFhirCitationKeywordListKeyword.create()
  else if name = 'Citation.medlinePubMed' then
    result := TFhirCitationMedlinePubMed.create()
  else if name = 'Citation.medlinePubMed.pubMedPubDate' then
    result := TFhirCitationMedlinePubMedPubMedPubDate.create()
  else if name = 'Citation.medlinePubMed.relatedArticle' then
    result := TFhirCitationMedlinePubMedRelatedArticle.create()
  else if name = 'Citation' then
    result := TFhirCitation.create()
{$ENDIF FHIR_CITATION}
{$IFDEF FHIR_CLAIM}
  else if name = 'Claim.related' then
    result := TFhirClaimRelated.create()
  else if name = 'Claim.payee' then
    result := TFhirClaimPayee.create()
  else if name = 'Claim.careTeam' then
    result := TFhirClaimCareTeam.create()
  else if name = 'Claim.supportingInfo' then
    result := TFhirClaimSupportingInfo.create()
  else if name = 'Claim.diagnosis' then
    result := TFhirClaimDiagnosis.create()
  else if name = 'Claim.procedure' then
    result := TFhirClaimProcedure.create()
  else if name = 'Claim.insurance' then
    result := TFhirClaimInsurance.create()
  else if name = 'Claim.accident' then
    result := TFhirClaimAccident.create()
  else if name = 'Claim.item' then
    result := TFhirClaimItem.create()
  else if name = 'Claim.item.detail' then
    result := TFhirClaimItemDetail.create()
  else if name = 'Claim.item.detail.subDetail' then
    result := TFhirClaimItemDetailSubDetail.create()
  else if name = 'Claim' then
    result := TFhirClaim.create()
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  else if name = 'ClaimResponse.item' then
    result := TFhirClaimResponseItem.create()
  else if name = 'ClaimResponse.item.adjudication' then
    result := TFhirClaimResponseItemAdjudication.create()
  else if name = 'ClaimResponse.item.detail' then
    result := TFhirClaimResponseItemDetail.create()
  else if name = 'ClaimResponse.item.detail.subDetail' then
    result := TFhirClaimResponseItemDetailSubDetail.create()
  else if name = 'ClaimResponse.addItem' then
    result := TFhirClaimResponseAddItem.create()
  else if name = 'ClaimResponse.addItem.detail' then
    result := TFhirClaimResponseAddItemDetail.create()
  else if name = 'ClaimResponse.addItem.detail.subDetail' then
    result := TFhirClaimResponseAddItemDetailSubDetail.create()
  else if name = 'ClaimResponse.total' then
    result := TFhirClaimResponseTotal.create()
  else if name = 'ClaimResponse.payment' then
    result := TFhirClaimResponsePayment.create()
  else if name = 'ClaimResponse.processNote' then
    result := TFhirClaimResponseProcessNote.create()
  else if name = 'ClaimResponse.insurance' then
    result := TFhirClaimResponseInsurance.create()
  else if name = 'ClaimResponse.error' then
    result := TFhirClaimResponseError.create()
  else if name = 'ClaimResponse' then
    result := TFhirClaimResponse.create()
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  else if name = 'ClinicalImpression.finding' then
    result := TFhirClinicalImpressionFinding.create()
  else if name = 'ClinicalImpression' then
    result := TFhirClinicalImpression.create()
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_CLINICALUSEISSUE}
  else if name = 'ClinicalUseIssue.contraindication' then
    result := TFhirClinicalUseIssueContraindication.create()
  else if name = 'ClinicalUseIssue.contraindication.otherTherapy' then
    result := TFhirClinicalUseIssueContraindicationOtherTherapy.create()
  else if name = 'ClinicalUseIssue.indication' then
    result := TFhirClinicalUseIssueIndication.create()
  else if name = 'ClinicalUseIssue.interaction' then
    result := TFhirClinicalUseIssueInteraction.create()
  else if name = 'ClinicalUseIssue.interaction.interactant' then
    result := TFhirClinicalUseIssueInteractionInteractant.create()
  else if name = 'ClinicalUseIssue.undesirableEffect' then
    result := TFhirClinicalUseIssueUndesirableEffect.create()
  else if name = 'ClinicalUseIssue' then
    result := TFhirClinicalUseIssue.create()
{$ENDIF FHIR_CLINICALUSEISSUE}
{$IFDEF FHIR_CODESYSTEM}
  else if name = 'CodeSystem.filter' then
    result := TFhirCodeSystemFilter.create()
  else if name = 'CodeSystem.property' then
    result := TFhirCodeSystemProperty.create()
  else if name = 'CodeSystem.concept' then
    result := TFhirCodeSystemConcept.create()
  else if name = 'CodeSystem.concept.designation' then
    result := TFhirCodeSystemConceptDesignation.create()
  else if name = 'CodeSystem.concept.property' then
    result := TFhirCodeSystemConceptProperty.create()
  else if name = 'CodeSystem' then
    result := TFhirCodeSystem.create()
{$ENDIF FHIR_CODESYSTEM}
{$IFDEF FHIR_COMMUNICATION}
  else if name = 'Communication.payload' then
    result := TFhirCommunicationPayload.create()
  else if name = 'Communication' then
    result := TFhirCommunication.create()
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  else if name = 'CommunicationRequest.payload' then
    result := TFhirCommunicationRequestPayload.create()
  else if name = 'CommunicationRequest' then
    result := TFhirCommunicationRequest.create()
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  else if name = 'CompartmentDefinition.resource' then
    result := TFhirCompartmentDefinitionResource.create()
  else if name = 'CompartmentDefinition' then
    result := TFhirCompartmentDefinition.create()
{$ENDIF FHIR_COMPARTMENTDEFINITION}
{$IFDEF FHIR_COMPOSITION}
  else if name = 'Composition.attester' then
    result := TFhirCompositionAttester.create()
  else if name = 'Composition.relatesTo' then
    result := TFhirCompositionRelatesTo.create()
  else if name = 'Composition.event' then
    result := TFhirCompositionEvent.create()
  else if name = 'Composition.section' then
    result := TFhirCompositionSection.create()
  else if name = 'Composition' then
    result := TFhirComposition.create()
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  else if name = 'ConceptMap.group' then
    result := TFhirConceptMapGroup.create()
  else if name = 'ConceptMap.group.element' then
    result := TFhirConceptMapGroupElement.create()
  else if name = 'ConceptMap.group.element.target' then
    result := TFhirConceptMapGroupElementTarget.create()
  else if name = 'ConceptMap.group.element.target.dependsOn' then
    result := TFhirConceptMapGroupElementTargetDependsOn.create()
  else if name = 'ConceptMap.group.unmapped' then
    result := TFhirConceptMapGroupUnmapped.create()
  else if name = 'ConceptMap' then
    result := TFhirConceptMap.create()
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_CONDITION}
  else if name = 'Condition.stage' then
    result := TFhirConditionStage.create()
  else if name = 'Condition.evidence' then
    result := TFhirConditionEvidence.create()
  else if name = 'Condition' then
    result := TFhirCondition.create()
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_CONDITIONDEFINITION}
  else if name = 'ConditionDefinition.observation' then
    result := TFhirConditionDefinitionObservation.create()
  else if name = 'ConditionDefinition.medication' then
    result := TFhirConditionDefinitionMedication.create()
  else if name = 'ConditionDefinition.precondition' then
    result := TFhirConditionDefinitionPrecondition.create()
  else if name = 'ConditionDefinition.questionnaire' then
    result := TFhirConditionDefinitionQuestionnaire.create()
  else if name = 'ConditionDefinition.plan' then
    result := TFhirConditionDefinitionPlan.create()
  else if name = 'ConditionDefinition' then
    result := TFhirConditionDefinition.create()
{$ENDIF FHIR_CONDITIONDEFINITION}
{$IFDEF FHIR_CONSENT}
  else if name = 'Consent.policy' then
    result := TFhirConsentPolicy.create()
  else if name = 'Consent.verification' then
    result := TFhirConsentVerification.create()
  else if name = 'Consent.provision' then
    result := TFhirConsentProvision.create()
  else if name = 'Consent.provision.actor' then
    result := TFhirConsentProvisionActor.create()
  else if name = 'Consent.provision.data' then
    result := TFhirConsentProvisionData.create()
  else if name = 'Consent' then
    result := TFhirConsent.create()
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  else if name = 'Contract.contentDefinition' then
    result := TFhirContractContentDefinition.create()
  else if name = 'Contract.term' then
    result := TFhirContractTerm.create()
  else if name = 'Contract.term.securityLabel' then
    result := TFhirContractTermSecurityLabel.create()
  else if name = 'Contract.term.offer' then
    result := TFhirContractTermOffer.create()
  else if name = 'Contract.term.offer.party' then
    result := TFhirContractTermOfferParty.create()
  else if name = 'Contract.term.offer.answer' then
    result := TFhirContractTermOfferAnswer.create()
  else if name = 'Contract.term.asset' then
    result := TFhirContractTermAsset.create()
  else if name = 'Contract.term.asset.context' then
    result := TFhirContractTermAssetContext.create()
  else if name = 'Contract.term.asset.valuedItem' then
    result := TFhirContractTermAssetValuedItem.create()
  else if name = 'Contract.term.action' then
    result := TFhirContractTermAction.create()
  else if name = 'Contract.term.action.subject' then
    result := TFhirContractTermActionSubject.create()
  else if name = 'Contract.signer' then
    result := TFhirContractSigner.create()
  else if name = 'Contract.friendly' then
    result := TFhirContractFriendly.create()
  else if name = 'Contract.legal' then
    result := TFhirContractLegal.create()
  else if name = 'Contract.rule' then
    result := TFhirContractRule.create()
  else if name = 'Contract' then
    result := TFhirContract.create()
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_COVERAGE}
  else if name = 'Coverage.class' then
    result := TFhirCoverageClass.create()
  else if name = 'Coverage.costToBeneficiary' then
    result := TFhirCoverageCostToBeneficiary.create()
  else if name = 'Coverage.costToBeneficiary.exception' then
    result := TFhirCoverageCostToBeneficiaryException.create()
  else if name = 'Coverage' then
    result := TFhirCoverage.create()
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  else if name = 'CoverageEligibilityRequest.supportingInfo' then
    result := TFhirCoverageEligibilityRequestSupportingInfo.create()
  else if name = 'CoverageEligibilityRequest.insurance' then
    result := TFhirCoverageEligibilityRequestInsurance.create()
  else if name = 'CoverageEligibilityRequest.item' then
    result := TFhirCoverageEligibilityRequestItem.create()
  else if name = 'CoverageEligibilityRequest.item.diagnosis' then
    result := TFhirCoverageEligibilityRequestItemDiagnosis.create()
  else if name = 'CoverageEligibilityRequest' then
    result := TFhirCoverageEligibilityRequest.create()
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}
{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  else if name = 'CoverageEligibilityResponse.insurance' then
    result := TFhirCoverageEligibilityResponseInsurance.create()
  else if name = 'CoverageEligibilityResponse.insurance.item' then
    result := TFhirCoverageEligibilityResponseInsuranceItem.create()
  else if name = 'CoverageEligibilityResponse.insurance.item.benefit' then
    result := TFhirCoverageEligibilityResponseInsuranceItemBenefit.create()
  else if name = 'CoverageEligibilityResponse.error' then
    result := TFhirCoverageEligibilityResponseError.create()
  else if name = 'CoverageEligibilityResponse' then
    result := TFhirCoverageEligibilityResponse.create()
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}
{$IFDEF FHIR_DETECTEDISSUE}
  else if name = 'DetectedIssue.evidence' then
    result := TFhirDetectedIssueEvidence.create()
  else if name = 'DetectedIssue.mitigation' then
    result := TFhirDetectedIssueMitigation.create()
  else if name = 'DetectedIssue' then
    result := TFhirDetectedIssue.create()
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  else if name = 'Device.udiCarrier' then
    result := TFhirDeviceUdiCarrier.create()
  else if name = 'Device.deviceName' then
    result := TFhirDeviceDeviceName.create()
  else if name = 'Device.specialization' then
    result := TFhirDeviceSpecialization.create()
  else if name = 'Device.version' then
    result := TFhirDeviceVersion.create()
  else if name = 'Device.property' then
    result := TFhirDeviceProperty.create()
  else if name = 'Device.operationalStatus' then
    result := TFhirDeviceOperationalStatus.create()
  else if name = 'Device.associationStatus' then
    result := TFhirDeviceAssociationStatus.create()
  else if name = 'Device' then
    result := TFhirDevice.create()
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICEDEFINITION}
  else if name = 'DeviceDefinition.udiDeviceIdentifier' then
    result := TFhirDeviceDefinitionUdiDeviceIdentifier.create()
  else if name = 'DeviceDefinition.deviceName' then
    result := TFhirDeviceDefinitionDeviceName.create()
  else if name = 'DeviceDefinition.specialization' then
    result := TFhirDeviceDefinitionSpecialization.create()
  else if name = 'DeviceDefinition.capability' then
    result := TFhirDeviceDefinitionCapability.create()
  else if name = 'DeviceDefinition.property' then
    result := TFhirDeviceDefinitionProperty.create()
  else if name = 'DeviceDefinition.material' then
    result := TFhirDeviceDefinitionMaterial.create()
  else if name = 'DeviceDefinition' then
    result := TFhirDeviceDefinition.create()
{$ENDIF FHIR_DEVICEDEFINITION}
{$IFDEF FHIR_DEVICEMETRIC}
  else if name = 'DeviceMetric.calibration' then
    result := TFhirDeviceMetricCalibration.create()
  else if name = 'DeviceMetric' then
    result := TFhirDeviceMetric.create()
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEREQUEST}
  else if name = 'DeviceRequest.parameter' then
    result := TFhirDeviceRequestParameter.create()
  else if name = 'DeviceRequest' then
    result := TFhirDeviceRequest.create()
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  else if name = 'DeviceUseStatement' then
    result := TFhirDeviceUseStatement.create()
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  else if name = 'DiagnosticReport.media' then
    result := TFhirDiagnosticReportMedia.create()
  else if name = 'DiagnosticReport' then
    result := TFhirDiagnosticReport.create()
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  else if name = 'DocumentManifest.related' then
    result := TFhirDocumentManifestRelated.create()
  else if name = 'DocumentManifest' then
    result := TFhirDocumentManifest.create()
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  else if name = 'DocumentReference.attester' then
    result := TFhirDocumentReferenceAttester.create()
  else if name = 'DocumentReference.relatesTo' then
    result := TFhirDocumentReferenceRelatesTo.create()
  else if name = 'DocumentReference.content' then
    result := TFhirDocumentReferenceContent.create()
  else if name = 'DocumentReference' then
    result := TFhirDocumentReference.create()
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_ENCOUNTER}
  else if name = 'Encounter.statusHistory' then
    result := TFhirEncounterStatusHistory.create()
  else if name = 'Encounter.classHistory' then
    result := TFhirEncounterClassHistory.create()
  else if name = 'Encounter.participant' then
    result := TFhirEncounterParticipant.create()
  else if name = 'Encounter.diagnosis' then
    result := TFhirEncounterDiagnosis.create()
  else if name = 'Encounter.hospitalization' then
    result := TFhirEncounterHospitalization.create()
  else if name = 'Encounter.location' then
    result := TFhirEncounterLocation.create()
  else if name = 'Encounter' then
    result := TFhirEncounter.create()
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENDPOINT}
  else if name = 'Endpoint' then
    result := TFhirEndpoint.create()
{$ENDIF FHIR_ENDPOINT}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  else if name = 'EnrollmentRequest' then
    result := TFhirEnrollmentRequest.create()
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  else if name = 'EnrollmentResponse' then
    result := TFhirEnrollmentResponse.create()
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EPISODEOFCARE}
  else if name = 'EpisodeOfCare.statusHistory' then
    result := TFhirEpisodeOfCareStatusHistory.create()
  else if name = 'EpisodeOfCare.diagnosis' then
    result := TFhirEpisodeOfCareDiagnosis.create()
  else if name = 'EpisodeOfCare' then
    result := TFhirEpisodeOfCare.create()
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EVENTDEFINITION}
  else if name = 'EventDefinition' then
    result := TFhirEventDefinition.create()
{$ENDIF FHIR_EVENTDEFINITION}
{$IFDEF FHIR_EVIDENCE}
  else if name = 'Evidence.variableDefinition' then
    result := TFhirEvidenceVariableDefinition.create()
  else if name = 'Evidence.certainty' then
    result := TFhirEvidenceCertainty.create()
  else if name = 'Evidence.certainty.certaintySubcomponent' then
    result := TFhirEvidenceCertaintyCertaintySubcomponent.create()
  else if name = 'Evidence' then
    result := TFhirEvidence.create()
{$ENDIF FHIR_EVIDENCE}
{$IFDEF FHIR_EVIDENCEREPORT}
  else if name = 'EvidenceReport.subject' then
    result := TFhirEvidenceReportSubject.create()
  else if name = 'EvidenceReport.subject.characteristic' then
    result := TFhirEvidenceReportSubjectCharacteristic.create()
  else if name = 'EvidenceReport.relatesTo' then
    result := TFhirEvidenceReportRelatesTo.create()
  else if name = 'EvidenceReport.section' then
    result := TFhirEvidenceReportSection.create()
  else if name = 'EvidenceReport' then
    result := TFhirEvidenceReport.create()
{$ENDIF FHIR_EVIDENCEREPORT}
{$IFDEF FHIR_EVIDENCEVARIABLE}
  else if name = 'EvidenceVariable.characteristic' then
    result := TFhirEvidenceVariableCharacteristic.create()
  else if name = 'EvidenceVariable.characteristic.timeFromStart' then
    result := TFhirEvidenceVariableCharacteristicTimeFromStart.create()
  else if name = 'EvidenceVariable.category' then
    result := TFhirEvidenceVariableCategory.create()
  else if name = 'EvidenceVariable' then
    result := TFhirEvidenceVariable.create()
{$ENDIF FHIR_EVIDENCEVARIABLE}
{$IFDEF FHIR_EXAMPLESCENARIO}
  else if name = 'ExampleScenario.actor' then
    result := TFhirExampleScenarioActor.create()
  else if name = 'ExampleScenario.instance' then
    result := TFhirExampleScenarioInstance.create()
  else if name = 'ExampleScenario.instance.version' then
    result := TFhirExampleScenarioInstanceVersion.create()
  else if name = 'ExampleScenario.instance.containedInstance' then
    result := TFhirExampleScenarioInstanceContainedInstance.create()
  else if name = 'ExampleScenario.process' then
    result := TFhirExampleScenarioProcess.create()
  else if name = 'ExampleScenario.process.step' then
    result := TFhirExampleScenarioProcessStep.create()
  else if name = 'ExampleScenario.process.step.operation' then
    result := TFhirExampleScenarioProcessStepOperation.create()
  else if name = 'ExampleScenario.process.step.alternative' then
    result := TFhirExampleScenarioProcessStepAlternative.create()
  else if name = 'ExampleScenario' then
    result := TFhirExampleScenario.create()
{$ENDIF FHIR_EXAMPLESCENARIO}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  else if name = 'ExplanationOfBenefit.related' then
    result := TFhirExplanationOfBenefitRelated.create()
  else if name = 'ExplanationOfBenefit.payee' then
    result := TFhirExplanationOfBenefitPayee.create()
  else if name = 'ExplanationOfBenefit.careTeam' then
    result := TFhirExplanationOfBenefitCareTeam.create()
  else if name = 'ExplanationOfBenefit.supportingInfo' then
    result := TFhirExplanationOfBenefitSupportingInfo.create()
  else if name = 'ExplanationOfBenefit.diagnosis' then
    result := TFhirExplanationOfBenefitDiagnosis.create()
  else if name = 'ExplanationOfBenefit.procedure' then
    result := TFhirExplanationOfBenefitProcedure.create()
  else if name = 'ExplanationOfBenefit.insurance' then
    result := TFhirExplanationOfBenefitInsurance.create()
  else if name = 'ExplanationOfBenefit.accident' then
    result := TFhirExplanationOfBenefitAccident.create()
  else if name = 'ExplanationOfBenefit.item' then
    result := TFhirExplanationOfBenefitItem.create()
  else if name = 'ExplanationOfBenefit.item.adjudication' then
    result := TFhirExplanationOfBenefitItemAdjudication.create()
  else if name = 'ExplanationOfBenefit.item.detail' then
    result := TFhirExplanationOfBenefitItemDetail.create()
  else if name = 'ExplanationOfBenefit.item.detail.subDetail' then
    result := TFhirExplanationOfBenefitItemDetailSubDetail.create()
  else if name = 'ExplanationOfBenefit.addItem' then
    result := TFhirExplanationOfBenefitAddItem.create()
  else if name = 'ExplanationOfBenefit.addItem.detail' then
    result := TFhirExplanationOfBenefitAddItemDetail.create()
  else if name = 'ExplanationOfBenefit.addItem.detail.subDetail' then
    result := TFhirExplanationOfBenefitAddItemDetailSubDetail.create()
  else if name = 'ExplanationOfBenefit.total' then
    result := TFhirExplanationOfBenefitTotal.create()
  else if name = 'ExplanationOfBenefit.payment' then
    result := TFhirExplanationOfBenefitPayment.create()
  else if name = 'ExplanationOfBenefit.processNote' then
    result := TFhirExplanationOfBenefitProcessNote.create()
  else if name = 'ExplanationOfBenefit.benefitBalance' then
    result := TFhirExplanationOfBenefitBenefitBalance.create()
  else if name = 'ExplanationOfBenefit.benefitBalance.financial' then
    result := TFhirExplanationOfBenefitBenefitBalanceFinancial.create()
  else if name = 'ExplanationOfBenefit' then
    result := TFhirExplanationOfBenefit.create()
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  else if name = 'FamilyMemberHistory.condition' then
    result := TFhirFamilyMemberHistoryCondition.create()
  else if name = 'FamilyMemberHistory.procedure' then
    result := TFhirFamilyMemberHistoryProcedure.create()
  else if name = 'FamilyMemberHistory' then
    result := TFhirFamilyMemberHistory.create()
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  else if name = 'Flag' then
    result := TFhirFlag.create()
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  else if name = 'Goal.target' then
    result := TFhirGoalTarget.create()
  else if name = 'Goal' then
    result := TFhirGoal.create()
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GRAPHDEFINITION}
  else if name = 'GraphDefinition.link' then
    result := TFhirGraphDefinitionLink.create()
  else if name = 'GraphDefinition.link.target' then
    result := TFhirGraphDefinitionLinkTarget.create()
  else if name = 'GraphDefinition.link.target.compartment' then
    result := TFhirGraphDefinitionLinkTargetCompartment.create()
  else if name = 'GraphDefinition' then
    result := TFhirGraphDefinition.create()
{$ENDIF FHIR_GRAPHDEFINITION}
{$IFDEF FHIR_GROUP}
  else if name = 'Group.characteristic' then
    result := TFhirGroupCharacteristic.create()
  else if name = 'Group.member' then
    result := TFhirGroupMember.create()
  else if name = 'Group' then
    result := TFhirGroup.create()
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_GUIDANCERESPONSE}
  else if name = 'GuidanceResponse' then
    result := TFhirGuidanceResponse.create()
{$ENDIF FHIR_GUIDANCERESPONSE}
{$IFDEF FHIR_HEALTHCARESERVICE}
  else if name = 'HealthcareService.eligibility' then
    result := TFhirHealthcareServiceEligibility.create()
  else if name = 'HealthcareService.availableTime' then
    result := TFhirHealthcareServiceAvailableTime.create()
  else if name = 'HealthcareService.notAvailable' then
    result := TFhirHealthcareServiceNotAvailable.create()
  else if name = 'HealthcareService' then
    result := TFhirHealthcareService.create()
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGSTUDY}
  else if name = 'ImagingStudy.procedure' then
    result := TFhirImagingStudyProcedure.create()
  else if name = 'ImagingStudy.series' then
    result := TFhirImagingStudySeries.create()
  else if name = 'ImagingStudy.series.performer' then
    result := TFhirImagingStudySeriesPerformer.create()
  else if name = 'ImagingStudy.series.instance' then
    result := TFhirImagingStudySeriesInstance.create()
  else if name = 'ImagingStudy' then
    result := TFhirImagingStudy.create()
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  else if name = 'Immunization.performer' then
    result := TFhirImmunizationPerformer.create()
  else if name = 'Immunization.education' then
    result := TFhirImmunizationEducation.create()
  else if name = 'Immunization.reaction' then
    result := TFhirImmunizationReaction.create()
  else if name = 'Immunization.protocolApplied' then
    result := TFhirImmunizationProtocolApplied.create()
  else if name = 'Immunization' then
    result := TFhirImmunization.create()
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  else if name = 'ImmunizationEvaluation' then
    result := TFhirImmunizationEvaluation.create()
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  else if name = 'ImmunizationRecommendation.recommendation' then
    result := TFhirImmunizationRecommendationRecommendation.create()
  else if name = 'ImmunizationRecommendation.recommendation.dateCriterion' then
    result := TFhirImmunizationRecommendationRecommendationDateCriterion.create()
  else if name = 'ImmunizationRecommendation' then
    result := TFhirImmunizationRecommendation.create()
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  else if name = 'ImplementationGuide.dependsOn' then
    result := TFhirImplementationGuideDependsOn.create()
  else if name = 'ImplementationGuide.global' then
    result := TFhirImplementationGuideGlobal.create()
  else if name = 'ImplementationGuide.definition' then
    result := TFhirImplementationGuideDefinition.create()
  else if name = 'ImplementationGuide.definition.grouping' then
    result := TFhirImplementationGuideDefinitionGrouping.create()
  else if name = 'ImplementationGuide.definition.resource' then
    result := TFhirImplementationGuideDefinitionResource.create()
  else if name = 'ImplementationGuide.definition.page' then
    result := TFhirImplementationGuideDefinitionPage.create()
  else if name = 'ImplementationGuide.definition.parameter' then
    result := TFhirImplementationGuideDefinitionParameter.create()
  else if name = 'ImplementationGuide.definition.template' then
    result := TFhirImplementationGuideDefinitionTemplate.create()
  else if name = 'ImplementationGuide.manifest' then
    result := TFhirImplementationGuideManifest.create()
  else if name = 'ImplementationGuide.manifest.resource' then
    result := TFhirImplementationGuideManifestResource.create()
  else if name = 'ImplementationGuide.manifest.page' then
    result := TFhirImplementationGuideManifestPage.create()
  else if name = 'ImplementationGuide' then
    result := TFhirImplementationGuide.create()
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_INGREDIENT}
  else if name = 'Ingredient.substance' then
    result := TFhirIngredientSubstance.create()
  else if name = 'Ingredient.substance.strength' then
    result := TFhirIngredientSubstanceStrength.create()
  else if name = 'Ingredient.substance.strength.referenceStrength' then
    result := TFhirIngredientSubstanceStrengthReferenceStrength.create()
  else if name = 'Ingredient.specifiedSubstance' then
    result := TFhirIngredientSpecifiedSubstance.create()
  else if name = 'Ingredient' then
    result := TFhirIngredient.create()
{$ENDIF FHIR_INGREDIENT}
{$IFDEF FHIR_INSURANCEPLAN}
  else if name = 'InsurancePlan.contact' then
    result := TFhirInsurancePlanContact.create()
  else if name = 'InsurancePlan.coverage' then
    result := TFhirInsurancePlanCoverage.create()
  else if name = 'InsurancePlan.coverage.benefit' then
    result := TFhirInsurancePlanCoverageBenefit.create()
  else if name = 'InsurancePlan.coverage.benefit.limit' then
    result := TFhirInsurancePlanCoverageBenefitLimit.create()
  else if name = 'InsurancePlan.plan' then
    result := TFhirInsurancePlanPlan.create()
  else if name = 'InsurancePlan.plan.generalCost' then
    result := TFhirInsurancePlanPlanGeneralCost.create()
  else if name = 'InsurancePlan.plan.specificCost' then
    result := TFhirInsurancePlanPlanSpecificCost.create()
  else if name = 'InsurancePlan.plan.specificCost.benefit' then
    result := TFhirInsurancePlanPlanSpecificCostBenefit.create()
  else if name = 'InsurancePlan.plan.specificCost.benefit.cost' then
    result := TFhirInsurancePlanPlanSpecificCostBenefitCost.create()
  else if name = 'InsurancePlan' then
    result := TFhirInsurancePlan.create()
{$ENDIF FHIR_INSURANCEPLAN}
{$IFDEF FHIR_INVOICE}
  else if name = 'Invoice.participant' then
    result := TFhirInvoiceParticipant.create()
  else if name = 'Invoice.lineItem' then
    result := TFhirInvoiceLineItem.create()
  else if name = 'Invoice.lineItem.priceComponent' then
    result := TFhirInvoiceLineItemPriceComponent.create()
  else if name = 'Invoice' then
    result := TFhirInvoice.create()
{$ENDIF FHIR_INVOICE}
{$IFDEF FHIR_LIBRARY}
  else if name = 'Library' then
    result := TFhirLibrary.create()
{$ENDIF FHIR_LIBRARY}
{$IFDEF FHIR_LINKAGE}
  else if name = 'Linkage.item' then
    result := TFhirLinkageItem.create()
  else if name = 'Linkage' then
    result := TFhirLinkage.create()
{$ENDIF FHIR_LINKAGE}
{$IFDEF FHIR_LIST}
  else if name = 'List.entry' then
    result := TFhirListEntry.create()
  else if name = 'List' then
    result := TFhirList.create()
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_LOCATION}
  else if name = 'Location.position' then
    result := TFhirLocationPosition.create()
  else if name = 'Location.hoursOfOperation' then
    result := TFhirLocationHoursOfOperation.create()
  else if name = 'Location' then
    result := TFhirLocation.create()
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
  else if name = 'ManufacturedItemDefinition.property' then
    result := TFhirManufacturedItemDefinitionProperty.create()
  else if name = 'ManufacturedItemDefinition' then
    result := TFhirManufacturedItemDefinition.create()
{$ENDIF FHIR_MANUFACTUREDITEMDEFINITION}
{$IFDEF FHIR_MEASURE}
  else if name = 'Measure.group' then
    result := TFhirMeasureGroup.create()
  else if name = 'Measure.group.population' then
    result := TFhirMeasureGroupPopulation.create()
  else if name = 'Measure.group.stratifier' then
    result := TFhirMeasureGroupStratifier.create()
  else if name = 'Measure.group.stratifier.component' then
    result := TFhirMeasureGroupStratifierComponent.create()
  else if name = 'Measure.supplementalData' then
    result := TFhirMeasureSupplementalData.create()
  else if name = 'Measure' then
    result := TFhirMeasure.create()
{$ENDIF FHIR_MEASURE}
{$IFDEF FHIR_MEASUREREPORT}
  else if name = 'MeasureReport.group' then
    result := TFhirMeasureReportGroup.create()
  else if name = 'MeasureReport.group.population' then
    result := TFhirMeasureReportGroupPopulation.create()
  else if name = 'MeasureReport.group.stratifier' then
    result := TFhirMeasureReportGroupStratifier.create()
  else if name = 'MeasureReport.group.stratifier.stratum' then
    result := TFhirMeasureReportGroupStratifierStratum.create()
  else if name = 'MeasureReport.group.stratifier.stratum.component' then
    result := TFhirMeasureReportGroupStratifierStratumComponent.create()
  else if name = 'MeasureReport.group.stratifier.stratum.population' then
    result := TFhirMeasureReportGroupStratifierStratumPopulation.create()
  else if name = 'MeasureReport' then
    result := TFhirMeasureReport.create()
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MEDICATION}
  else if name = 'Medication.ingredient' then
    result := TFhirMedicationIngredient.create()
  else if name = 'Medication.batch' then
    result := TFhirMedicationBatch.create()
  else if name = 'Medication' then
    result := TFhirMedication.create()
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  else if name = 'MedicationAdministration.performer' then
    result := TFhirMedicationAdministrationPerformer.create()
  else if name = 'MedicationAdministration.dosage' then
    result := TFhirMedicationAdministrationDosage.create()
  else if name = 'MedicationAdministration' then
    result := TFhirMedicationAdministration.create()
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  else if name = 'MedicationDispense.performer' then
    result := TFhirMedicationDispensePerformer.create()
  else if name = 'MedicationDispense.substitution' then
    result := TFhirMedicationDispenseSubstitution.create()
  else if name = 'MedicationDispense' then
    result := TFhirMedicationDispense.create()
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  else if name = 'MedicationKnowledge.relatedMedicationKnowledge' then
    result := TFhirMedicationKnowledgeRelatedMedicationKnowledge.create()
  else if name = 'MedicationKnowledge.monograph' then
    result := TFhirMedicationKnowledgeMonograph.create()
  else if name = 'MedicationKnowledge.ingredient' then
    result := TFhirMedicationKnowledgeIngredient.create()
  else if name = 'MedicationKnowledge.cost' then
    result := TFhirMedicationKnowledgeCost.create()
  else if name = 'MedicationKnowledge.monitoringProgram' then
    result := TFhirMedicationKnowledgeMonitoringProgram.create()
  else if name = 'MedicationKnowledge.administrationGuideline' then
    result := TFhirMedicationKnowledgeAdministrationGuideline.create()
  else if name = 'MedicationKnowledge.administrationGuideline.dosage' then
    result := TFhirMedicationKnowledgeAdministrationGuidelineDosage.create()
  else if name = 'MedicationKnowledge.administrationGuideline.patientCharacteristic' then
    result := TFhirMedicationKnowledgeAdministrationGuidelinePatientCharacteristic.create()
  else if name = 'MedicationKnowledge.medicineClassification' then
    result := TFhirMedicationKnowledgeMedicineClassification.create()
  else if name = 'MedicationKnowledge.packaging' then
    result := TFhirMedicationKnowledgePackaging.create()
  else if name = 'MedicationKnowledge.drugCharacteristic' then
    result := TFhirMedicationKnowledgeDrugCharacteristic.create()
  else if name = 'MedicationKnowledge.regulatory' then
    result := TFhirMedicationKnowledgeRegulatory.create()
  else if name = 'MedicationKnowledge.regulatory.substitution' then
    result := TFhirMedicationKnowledgeRegulatorySubstitution.create()
  else if name = 'MedicationKnowledge.regulatory.maxDispense' then
    result := TFhirMedicationKnowledgeRegulatoryMaxDispense.create()
  else if name = 'MedicationKnowledge.kineticCharacteristic' then
    result := TFhirMedicationKnowledgeKineticCharacteristic.create()
  else if name = 'MedicationKnowledge' then
    result := TFhirMedicationKnowledge.create()
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  else if name = 'MedicationRequest.dispenseRequest' then
    result := TFhirMedicationRequestDispenseRequest.create()
  else if name = 'MedicationRequest.dispenseRequest.initialFill' then
    result := TFhirMedicationRequestDispenseRequestInitialFill.create()
  else if name = 'MedicationRequest.substitution' then
    result := TFhirMedicationRequestSubstitution.create()
  else if name = 'MedicationRequest' then
    result := TFhirMedicationRequest.create()
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONUSAGE}
  else if name = 'MedicationUsage' then
    result := TFhirMedicationUsage.create()
{$ENDIF FHIR_MEDICATIONUSAGE}
{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
  else if name = 'MedicinalProductDefinition.contact' then
    result := TFhirMedicinalProductDefinitionContact.create()
  else if name = 'MedicinalProductDefinition.name' then
    result := TFhirMedicinalProductDefinitionName.create()
  else if name = 'MedicinalProductDefinition.name.namePart' then
    result := TFhirMedicinalProductDefinitionNameNamePart.create()
  else if name = 'MedicinalProductDefinition.name.countryLanguage' then
    result := TFhirMedicinalProductDefinitionNameCountryLanguage.create()
  else if name = 'MedicinalProductDefinition.crossReference' then
    result := TFhirMedicinalProductDefinitionCrossReference.create()
  else if name = 'MedicinalProductDefinition.manufacturingBusinessOperation' then
    result := TFhirMedicinalProductDefinitionManufacturingBusinessOperation.create()
  else if name = 'MedicinalProductDefinition' then
    result := TFhirMedicinalProductDefinition.create()
{$ENDIF FHIR_MEDICINALPRODUCTDEFINITION}
{$IFDEF FHIR_MESSAGEDEFINITION}
  else if name = 'MessageDefinition.focus' then
    result := TFhirMessageDefinitionFocus.create()
  else if name = 'MessageDefinition.allowedResponse' then
    result := TFhirMessageDefinitionAllowedResponse.create()
  else if name = 'MessageDefinition' then
    result := TFhirMessageDefinition.create()
{$ENDIF FHIR_MESSAGEDEFINITION}
{$IFDEF FHIR_MESSAGEHEADER}
  else if name = 'MessageHeader.destination' then
    result := TFhirMessageHeaderDestination.create()
  else if name = 'MessageHeader.source' then
    result := TFhirMessageHeaderSource.create()
  else if name = 'MessageHeader.response' then
    result := TFhirMessageHeaderResponse.create()
  else if name = 'MessageHeader' then
    result := TFhirMessageHeader.create()
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_MOLECULARSEQUENCE}
  else if name = 'MolecularSequence.referenceSeq' then
    result := TFhirMolecularSequenceReferenceSeq.create()
  else if name = 'MolecularSequence.variant' then
    result := TFhirMolecularSequenceVariant.create()
  else if name = 'MolecularSequence.quality' then
    result := TFhirMolecularSequenceQuality.create()
  else if name = 'MolecularSequence.quality.roc' then
    result := TFhirMolecularSequenceQualityRoc.create()
  else if name = 'MolecularSequence.repository' then
    result := TFhirMolecularSequenceRepository.create()
  else if name = 'MolecularSequence.structureVariant' then
    result := TFhirMolecularSequenceStructureVariant.create()
  else if name = 'MolecularSequence.structureVariant.outer' then
    result := TFhirMolecularSequenceStructureVariantOuter.create()
  else if name = 'MolecularSequence.structureVariant.inner' then
    result := TFhirMolecularSequenceStructureVariantInner.create()
  else if name = 'MolecularSequence' then
    result := TFhirMolecularSequence.create()
{$ENDIF FHIR_MOLECULARSEQUENCE}
{$IFDEF FHIR_NAMINGSYSTEM}
  else if name = 'NamingSystem.uniqueId' then
    result := TFhirNamingSystemUniqueId.create()
  else if name = 'NamingSystem' then
    result := TFhirNamingSystem.create()
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_NUTRITIONINTAKE}
  else if name = 'NutritionIntake.consumedItem' then
    result := TFhirNutritionIntakeConsumedItem.create()
  else if name = 'NutritionIntake.ingredientLabel' then
    result := TFhirNutritionIntakeIngredientLabel.create()
  else if name = 'NutritionIntake.performer' then
    result := TFhirNutritionIntakePerformer.create()
  else if name = 'NutritionIntake' then
    result := TFhirNutritionIntake.create()
{$ENDIF FHIR_NUTRITIONINTAKE}
{$IFDEF FHIR_NUTRITIONORDER}
  else if name = 'NutritionOrder.oralDiet' then
    result := TFhirNutritionOrderOralDiet.create()
  else if name = 'NutritionOrder.oralDiet.nutrient' then
    result := TFhirNutritionOrderOralDietNutrient.create()
  else if name = 'NutritionOrder.oralDiet.texture' then
    result := TFhirNutritionOrderOralDietTexture.create()
  else if name = 'NutritionOrder.supplement' then
    result := TFhirNutritionOrderSupplement.create()
  else if name = 'NutritionOrder.enteralFormula' then
    result := TFhirNutritionOrderEnteralFormula.create()
  else if name = 'NutritionOrder.enteralFormula.administration' then
    result := TFhirNutritionOrderEnteralFormulaAdministration.create()
  else if name = 'NutritionOrder' then
    result := TFhirNutritionOrder.create()
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_NUTRITIONPRODUCT}
  else if name = 'NutritionProduct.nutrient' then
    result := TFhirNutritionProductNutrient.create()
  else if name = 'NutritionProduct.ingredient' then
    result := TFhirNutritionProductIngredient.create()
  else if name = 'NutritionProduct.productCharacteristic' then
    result := TFhirNutritionProductProductCharacteristic.create()
  else if name = 'NutritionProduct.instance' then
    result := TFhirNutritionProductInstance.create()
  else if name = 'NutritionProduct' then
    result := TFhirNutritionProduct.create()
{$ENDIF FHIR_NUTRITIONPRODUCT}
{$IFDEF FHIR_OBSERVATION}
  else if name = 'Observation.referenceRange' then
    result := TFhirObservationReferenceRange.create()
  else if name = 'Observation.component' then
    result := TFhirObservationComponent.create()
  else if name = 'Observation' then
    result := TFhirObservation.create()
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OBSERVATIONDEFINITION}
  else if name = 'ObservationDefinition.quantitativeDetails' then
    result := TFhirObservationDefinitionQuantitativeDetails.create()
  else if name = 'ObservationDefinition.qualifiedInterval' then
    result := TFhirObservationDefinitionQualifiedInterval.create()
  else if name = 'ObservationDefinition.component' then
    result := TFhirObservationDefinitionComponent.create()
  else if name = 'ObservationDefinition' then
    result := TFhirObservationDefinition.create()
{$ENDIF FHIR_OBSERVATIONDEFINITION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  else if name = 'OperationDefinition.parameter' then
    result := TFhirOperationDefinitionParameter.create()
  else if name = 'OperationDefinition.parameter.binding' then
    result := TFhirOperationDefinitionParameterBinding.create()
  else if name = 'OperationDefinition.parameter.referencedFrom' then
    result := TFhirOperationDefinitionParameterReferencedFrom.create()
  else if name = 'OperationDefinition.overload' then
    result := TFhirOperationDefinitionOverload.create()
  else if name = 'OperationDefinition' then
    result := TFhirOperationDefinition.create()
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  else if name = 'OperationOutcome.issue' then
    result := TFhirOperationOutcomeIssue.create()
  else if name = 'OperationOutcome' then
    result := TFhirOperationOutcome.create()
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_ORGANIZATION}
  else if name = 'Organization.contact' then
    result := TFhirOrganizationContact.create()
  else if name = 'Organization' then
    result := TFhirOrganization.create()
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  else if name = 'OrganizationAffiliation' then
    result := TFhirOrganizationAffiliation.create()
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}
{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
  else if name = 'PackagedProductDefinition.batchIdentifier' then
    result := TFhirPackagedProductDefinitionBatchIdentifier.create()
  else if name = 'PackagedProductDefinition.package' then
    result := TFhirPackagedProductDefinitionPackage.create()
  else if name = 'PackagedProductDefinition.package.property' then
    result := TFhirPackagedProductDefinitionPackageProperty.create()
  else if name = 'PackagedProductDefinition.package.containedItem' then
    result := TFhirPackagedProductDefinitionPackageContainedItem.create()
  else if name = 'PackagedProductDefinition' then
    result := TFhirPackagedProductDefinition.create()
{$ENDIF FHIR_PACKAGEDPRODUCTDEFINITION}
{$IFDEF FHIR_PARAMETERS}
  else if name = 'Parameters.parameter' then
    result := TFhirParametersParameter.create()
  else if name = 'Parameters' then
    result := TFhirParameters.create()
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_PATIENT}
  else if name = 'Patient.contact' then
    result := TFhirPatientContact.create()
  else if name = 'Patient.communication' then
    result := TFhirPatientCommunication.create()
  else if name = 'Patient.link' then
    result := TFhirPatientLink.create()
  else if name = 'Patient' then
    result := TFhirPatient.create()
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PAYMENTNOTICE}
  else if name = 'PaymentNotice' then
    result := TFhirPaymentNotice.create()
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  else if name = 'PaymentReconciliation.detail' then
    result := TFhirPaymentReconciliationDetail.create()
  else if name = 'PaymentReconciliation.processNote' then
    result := TFhirPaymentReconciliationProcessNote.create()
  else if name = 'PaymentReconciliation' then
    result := TFhirPaymentReconciliation.create()
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERMISSION}
  else if name = 'Permission.processingActivity' then
    result := TFhirPermissionProcessingActivity.create()
  else if name = 'Permission.justification' then
    result := TFhirPermissionJustification.create()
  else if name = 'Permission' then
    result := TFhirPermission.create()
{$ENDIF FHIR_PERMISSION}
{$IFDEF FHIR_PERSON}
  else if name = 'Person.link' then
    result := TFhirPersonLink.create()
  else if name = 'Person' then
    result := TFhirPerson.create()
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PLANDEFINITION}
  else if name = 'PlanDefinition.goal' then
    result := TFhirPlanDefinitionGoal.create()
  else if name = 'PlanDefinition.goal.target' then
    result := TFhirPlanDefinitionGoalTarget.create()
  else if name = 'PlanDefinition.action' then
    result := TFhirPlanDefinitionAction.create()
  else if name = 'PlanDefinition.action.condition' then
    result := TFhirPlanDefinitionActionCondition.create()
  else if name = 'PlanDefinition.action.relatedAction' then
    result := TFhirPlanDefinitionActionRelatedAction.create()
  else if name = 'PlanDefinition.action.participant' then
    result := TFhirPlanDefinitionActionParticipant.create()
  else if name = 'PlanDefinition.action.dynamicValue' then
    result := TFhirPlanDefinitionActionDynamicValue.create()
  else if name = 'PlanDefinition' then
    result := TFhirPlanDefinition.create()
{$ENDIF FHIR_PLANDEFINITION}
{$IFDEF FHIR_PRACTITIONER}
  else if name = 'Practitioner.qualification' then
    result := TFhirPractitionerQualification.create()
  else if name = 'Practitioner' then
    result := TFhirPractitioner.create()
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PRACTITIONERROLE}
  else if name = 'PractitionerRole.availableTime' then
    result := TFhirPractitionerRoleAvailableTime.create()
  else if name = 'PractitionerRole.notAvailable' then
    result := TFhirPractitionerRoleNotAvailable.create()
  else if name = 'PractitionerRole' then
    result := TFhirPractitionerRole.create()
{$ENDIF FHIR_PRACTITIONERROLE}
{$IFDEF FHIR_PROCEDURE}
  else if name = 'Procedure.performer' then
    result := TFhirProcedurePerformer.create()
  else if name = 'Procedure.focalDevice' then
    result := TFhirProcedureFocalDevice.create()
  else if name = 'Procedure' then
    result := TFhirProcedure.create()
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROVENANCE}
  else if name = 'Provenance.agent' then
    result := TFhirProvenanceAgent.create()
  else if name = 'Provenance.entity' then
    result := TFhirProvenanceEntity.create()
  else if name = 'Provenance' then
    result := TFhirProvenance.create()
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  else if name = 'Questionnaire.item' then
    result := TFhirQuestionnaireItem.create()
  else if name = 'Questionnaire.item.enableWhen' then
    result := TFhirQuestionnaireItemEnableWhen.create()
  else if name = 'Questionnaire.item.answerOption' then
    result := TFhirQuestionnaireItemAnswerOption.create()
  else if name = 'Questionnaire.item.initial' then
    result := TFhirQuestionnaireItemInitial.create()
  else if name = 'Questionnaire' then
    result := TFhirQuestionnaire.create()
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  else if name = 'QuestionnaireResponse.item' then
    result := TFhirQuestionnaireResponseItem.create()
  else if name = 'QuestionnaireResponse.item.answer' then
    result := TFhirQuestionnaireResponseItemAnswer.create()
  else if name = 'QuestionnaireResponse' then
    result := TFhirQuestionnaireResponse.create()
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REGULATEDAUTHORIZATION}
  else if name = 'RegulatedAuthorization.relatedDate' then
    result := TFhirRegulatedAuthorizationRelatedDate.create()
  else if name = 'RegulatedAuthorization.case' then
    result := TFhirRegulatedAuthorizationCase.create()
  else if name = 'RegulatedAuthorization' then
    result := TFhirRegulatedAuthorization.create()
{$ENDIF FHIR_REGULATEDAUTHORIZATION}
{$IFDEF FHIR_RELATEDPERSON}
  else if name = 'RelatedPerson.communication' then
    result := TFhirRelatedPersonCommunication.create()
  else if name = 'RelatedPerson' then
    result := TFhirRelatedPerson.create()
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_REQUESTGROUP}
  else if name = 'RequestGroup.action' then
    result := TFhirRequestGroupAction.create()
  else if name = 'RequestGroup.action.condition' then
    result := TFhirRequestGroupActionCondition.create()
  else if name = 'RequestGroup.action.relatedAction' then
    result := TFhirRequestGroupActionRelatedAction.create()
  else if name = 'RequestGroup' then
    result := TFhirRequestGroup.create()
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_RESEARCHSTUDY}
  else if name = 'ResearchStudy.arm' then
    result := TFhirResearchStudyArm.create()
  else if name = 'ResearchStudy.objective' then
    result := TFhirResearchStudyObjective.create()
  else if name = 'ResearchStudy' then
    result := TFhirResearchStudy.create()
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  else if name = 'ResearchSubject.progress' then
    result := TFhirResearchSubjectProgress.create()
  else if name = 'ResearchSubject' then
    result := TFhirResearchSubject.create()
{$ENDIF FHIR_RESEARCHSUBJECT}
{$IFDEF FHIR_RISKASSESSMENT}
  else if name = 'RiskAssessment.prediction' then
    result := TFhirRiskAssessmentPrediction.create()
  else if name = 'RiskAssessment' then
    result := TFhirRiskAssessment.create()
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_SCHEDULE}
  else if name = 'Schedule' then
    result := TFhirSchedule.create()
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SEARCHPARAMETER}
  else if name = 'SearchParameter.component' then
    result := TFhirSearchParameterComponent.create()
  else if name = 'SearchParameter' then
    result := TFhirSearchParameter.create()
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SERVICEREQUEST}
  else if name = 'ServiceRequest' then
    result := TFhirServiceRequest.create()
{$ENDIF FHIR_SERVICEREQUEST}
{$IFDEF FHIR_SLOT}
  else if name = 'Slot' then
    result := TFhirSlot.create()
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  else if name = 'Specimen.collection' then
    result := TFhirSpecimenCollection.create()
  else if name = 'Specimen.processing' then
    result := TFhirSpecimenProcessing.create()
  else if name = 'Specimen.container' then
    result := TFhirSpecimenContainer.create()
  else if name = 'Specimen' then
    result := TFhirSpecimen.create()
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_SPECIMENDEFINITION}
  else if name = 'SpecimenDefinition.typeTested' then
    result := TFhirSpecimenDefinitionTypeTested.create()
  else if name = 'SpecimenDefinition.typeTested.container' then
    result := TFhirSpecimenDefinitionTypeTestedContainer.create()
  else if name = 'SpecimenDefinition.typeTested.container.additive' then
    result := TFhirSpecimenDefinitionTypeTestedContainerAdditive.create()
  else if name = 'SpecimenDefinition.typeTested.handling' then
    result := TFhirSpecimenDefinitionTypeTestedHandling.create()
  else if name = 'SpecimenDefinition' then
    result := TFhirSpecimenDefinition.create()
{$ENDIF FHIR_SPECIMENDEFINITION}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  else if name = 'StructureDefinition.mapping' then
    result := TFhirStructureDefinitionMapping.create()
  else if name = 'StructureDefinition.context' then
    result := TFhirStructureDefinitionContext.create()
  else if name = 'StructureDefinition.snapshot' then
    result := TFhirStructureDefinitionSnapshot.create()
  else if name = 'StructureDefinition.differential' then
    result := TFhirStructureDefinitionDifferential.create()
  else if name = 'StructureDefinition' then
    result := TFhirStructureDefinition.create()
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_STRUCTUREMAP}
  else if name = 'StructureMap.structure' then
    result := TFhirStructureMapStructure.create()
  else if name = 'StructureMap.group' then
    result := TFhirStructureMapGroup.create()
  else if name = 'StructureMap.group.input' then
    result := TFhirStructureMapGroupInput.create()
  else if name = 'StructureMap.group.rule' then
    result := TFhirStructureMapGroupRule.create()
  else if name = 'StructureMap.group.rule.source' then
    result := TFhirStructureMapGroupRuleSource.create()
  else if name = 'StructureMap.group.rule.target' then
    result := TFhirStructureMapGroupRuleTarget.create()
  else if name = 'StructureMap.group.rule.target.parameter' then
    result := TFhirStructureMapGroupRuleTargetParameter.create()
  else if name = 'StructureMap.group.rule.dependent' then
    result := TFhirStructureMapGroupRuleDependent.create()
  else if name = 'StructureMap' then
    result := TFhirStructureMap.create()
{$ENDIF FHIR_STRUCTUREMAP}
{$IFDEF FHIR_SUBSCRIPTION}
  else if name = 'Subscription.filterBy' then
    result := TFhirSubscriptionFilterBy.create()
  else if name = 'Subscription' then
    result := TFhirSubscription.create()
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
  else if name = 'SubscriptionStatus' then
    result := TFhirSubscriptionStatus.create()
{$ENDIF FHIR_SUBSCRIPTIONSTATUS}
{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
  else if name = 'SubscriptionTopic.resourceTrigger' then
    result := TFhirSubscriptionTopicResourceTrigger.create()
  else if name = 'SubscriptionTopic.resourceTrigger.queryCriteria' then
    result := TFhirSubscriptionTopicResourceTriggerQueryCriteria.create()
  else if name = 'SubscriptionTopic.canFilterBy' then
    result := TFhirSubscriptionTopicCanFilterBy.create()
  else if name = 'SubscriptionTopic' then
    result := TFhirSubscriptionTopic.create()
{$ENDIF FHIR_SUBSCRIPTIONTOPIC}
{$IFDEF FHIR_SUBSTANCE}
  else if name = 'Substance.instance' then
    result := TFhirSubstanceInstance.create()
  else if name = 'Substance.ingredient' then
    result := TFhirSubstanceIngredient.create()
  else if name = 'Substance' then
    result := TFhirSubstance.create()
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUBSTANCEDEFINITION}
  else if name = 'SubstanceDefinition.moiety' then
    result := TFhirSubstanceDefinitionMoiety.create()
  else if name = 'SubstanceDefinition.property' then
    result := TFhirSubstanceDefinitionProperty.create()
  else if name = 'SubstanceDefinition.structure' then
    result := TFhirSubstanceDefinitionStructure.create()
  else if name = 'SubstanceDefinition.structure.isotope' then
    result := TFhirSubstanceDefinitionStructureIsotope.create()
  else if name = 'SubstanceDefinition.structure.isotope.molecularWeight' then
    result := TFhirSubstanceDefinitionStructureIsotopeMolecularWeight.create()
  else if name = 'SubstanceDefinition.structure.representation' then
    result := TFhirSubstanceDefinitionStructureRepresentation.create()
  else if name = 'SubstanceDefinition.code' then
    result := TFhirSubstanceDefinitionCode.create()
  else if name = 'SubstanceDefinition.name' then
    result := TFhirSubstanceDefinitionName.create()
  else if name = 'SubstanceDefinition.name.official' then
    result := TFhirSubstanceDefinitionNameOfficial.create()
  else if name = 'SubstanceDefinition.relationship' then
    result := TFhirSubstanceDefinitionRelationship.create()
  else if name = 'SubstanceDefinition' then
    result := TFhirSubstanceDefinition.create()
{$ENDIF FHIR_SUBSTANCEDEFINITION}
{$IFDEF FHIR_SUBSTANCENUCLEICACID}
  else if name = 'SubstanceNucleicAcid.subunit' then
    result := TFhirSubstanceNucleicAcidSubunit.create()
  else if name = 'SubstanceNucleicAcid.subunit.linkage' then
    result := TFhirSubstanceNucleicAcidSubunitLinkage.create()
  else if name = 'SubstanceNucleicAcid.subunit.sugar' then
    result := TFhirSubstanceNucleicAcidSubunitSugar.create()
  else if name = 'SubstanceNucleicAcid' then
    result := TFhirSubstanceNucleicAcid.create()
{$ENDIF FHIR_SUBSTANCENUCLEICACID}
{$IFDEF FHIR_SUBSTANCEPOLYMER}
  else if name = 'SubstancePolymer.monomerSet' then
    result := TFhirSubstancePolymerMonomerSet.create()
  else if name = 'SubstancePolymer.monomerSet.startingMaterial' then
    result := TFhirSubstancePolymerMonomerSetStartingMaterial.create()
  else if name = 'SubstancePolymer.repeat' then
    result := TFhirSubstancePolymerRepeat.create()
  else if name = 'SubstancePolymer.repeat.repeatUnit' then
    result := TFhirSubstancePolymerRepeatRepeatUnit.create()
  else if name = 'SubstancePolymer.repeat.repeatUnit.degreeOfPolymerisation' then
    result := TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation.create()
  else if name = 'SubstancePolymer.repeat.repeatUnit.structuralRepresentation' then
    result := TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentation.create()
  else if name = 'SubstancePolymer' then
    result := TFhirSubstancePolymer.create()
{$ENDIF FHIR_SUBSTANCEPOLYMER}
{$IFDEF FHIR_SUBSTANCEPROTEIN}
  else if name = 'SubstanceProtein.subunit' then
    result := TFhirSubstanceProteinSubunit.create()
  else if name = 'SubstanceProtein' then
    result := TFhirSubstanceProtein.create()
{$ENDIF FHIR_SUBSTANCEPROTEIN}
{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  else if name = 'SubstanceReferenceInformation.gene' then
    result := TFhirSubstanceReferenceInformationGene.create()
  else if name = 'SubstanceReferenceInformation.geneElement' then
    result := TFhirSubstanceReferenceInformationGeneElement.create()
  else if name = 'SubstanceReferenceInformation.target' then
    result := TFhirSubstanceReferenceInformationTarget.create()
  else if name = 'SubstanceReferenceInformation' then
    result := TFhirSubstanceReferenceInformation.create()
{$ENDIF FHIR_SUBSTANCEREFERENCEINFORMATION}
{$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
  else if name = 'SubstanceSourceMaterial.fractionDescription' then
    result := TFhirSubstanceSourceMaterialFractionDescription.create()
  else if name = 'SubstanceSourceMaterial.organism' then
    result := TFhirSubstanceSourceMaterialOrganism.create()
  else if name = 'SubstanceSourceMaterial.organism.author' then
    result := TFhirSubstanceSourceMaterialOrganismAuthor.create()
  else if name = 'SubstanceSourceMaterial.organism.hybrid' then
    result := TFhirSubstanceSourceMaterialOrganismHybrid.create()
  else if name = 'SubstanceSourceMaterial.organism.organismGeneral' then
    result := TFhirSubstanceSourceMaterialOrganismOrganismGeneral.create()
  else if name = 'SubstanceSourceMaterial.partDescription' then
    result := TFhirSubstanceSourceMaterialPartDescription.create()
  else if name = 'SubstanceSourceMaterial' then
    result := TFhirSubstanceSourceMaterial.create()
{$ENDIF FHIR_SUBSTANCESOURCEMATERIAL}
{$IFDEF FHIR_SUPPLYDELIVERY}
  else if name = 'SupplyDelivery.suppliedItem' then
    result := TFhirSupplyDeliverySuppliedItem.create()
  else if name = 'SupplyDelivery' then
    result := TFhirSupplyDelivery.create()
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  else if name = 'SupplyRequest.parameter' then
    result := TFhirSupplyRequestParameter.create()
  else if name = 'SupplyRequest' then
    result := TFhirSupplyRequest.create()
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TASK}
  else if name = 'Task.restriction' then
    result := TFhirTaskRestriction.create()
  else if name = 'Task.input' then
    result := TFhirTaskInput.create()
  else if name = 'Task.output' then
    result := TFhirTaskOutput.create()
  else if name = 'Task' then
    result := TFhirTask.create()
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  else if name = 'TerminologyCapabilities.software' then
    result := TFhirTerminologyCapabilitiesSoftware.create()
  else if name = 'TerminologyCapabilities.implementation' then
    result := TFhirTerminologyCapabilitiesImplementation.create()
  else if name = 'TerminologyCapabilities.codeSystem' then
    result := TFhirTerminologyCapabilitiesCodeSystem.create()
  else if name = 'TerminologyCapabilities.codeSystem.version' then
    result := TFhirTerminologyCapabilitiesCodeSystemVersion.create()
  else if name = 'TerminologyCapabilities.codeSystem.version.filter' then
    result := TFhirTerminologyCapabilitiesCodeSystemVersionFilter.create()
  else if name = 'TerminologyCapabilities.expansion' then
    result := TFhirTerminologyCapabilitiesExpansion.create()
  else if name = 'TerminologyCapabilities.expansion.parameter' then
    result := TFhirTerminologyCapabilitiesExpansionParameter.create()
  else if name = 'TerminologyCapabilities.validateCode' then
    result := TFhirTerminologyCapabilitiesValidateCode.create()
  else if name = 'TerminologyCapabilities.translation' then
    result := TFhirTerminologyCapabilitiesTranslation.create()
  else if name = 'TerminologyCapabilities.closure' then
    result := TFhirTerminologyCapabilitiesClosure.create()
  else if name = 'TerminologyCapabilities' then
    result := TFhirTerminologyCapabilities.create()
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}
{$IFDEF FHIR_TESTREPORT}
  else if name = 'TestReport.participant' then
    result := TFhirTestReportParticipant.create()
  else if name = 'TestReport.setup' then
    result := TFhirTestReportSetup.create()
  else if name = 'TestReport.setup.action' then
    result := TFhirTestReportSetupAction.create()
  else if name = 'TestReport.setup.action.operation' then
    result := TFhirTestReportSetupActionOperation.create()
  else if name = 'TestReport.setup.action.assert' then
    result := TFhirTestReportSetupActionAssert.create()
  else if name = 'TestReport.test' then
    result := TFhirTestReportTest.create()
  else if name = 'TestReport.test.action' then
    result := TFhirTestReportTestAction.create()
  else if name = 'TestReport.teardown' then
    result := TFhirTestReportTeardown.create()
  else if name = 'TestReport.teardown.action' then
    result := TFhirTestReportTeardownAction.create()
  else if name = 'TestReport' then
    result := TFhirTestReport.create()
{$ENDIF FHIR_TESTREPORT}
{$IFDEF FHIR_TESTSCRIPT}
  else if name = 'TestScript.origin' then
    result := TFhirTestScriptOrigin.create()
  else if name = 'TestScript.destination' then
    result := TFhirTestScriptDestination.create()
  else if name = 'TestScript.metadata' then
    result := TFhirTestScriptMetadata.create()
  else if name = 'TestScript.metadata.link' then
    result := TFhirTestScriptMetadataLink.create()
  else if name = 'TestScript.metadata.capability' then
    result := TFhirTestScriptMetadataCapability.create()
  else if name = 'TestScript.fixture' then
    result := TFhirTestScriptFixture.create()
  else if name = 'TestScript.variable' then
    result := TFhirTestScriptVariable.create()
  else if name = 'TestScript.setup' then
    result := TFhirTestScriptSetup.create()
  else if name = 'TestScript.setup.action' then
    result := TFhirTestScriptSetupAction.create()
  else if name = 'TestScript.setup.action.operation' then
    result := TFhirTestScriptSetupActionOperation.create()
  else if name = 'TestScript.setup.action.operation.requestHeader' then
    result := TFhirTestScriptSetupActionOperationRequestHeader.create()
  else if name = 'TestScript.setup.action.assert' then
    result := TFhirTestScriptSetupActionAssert.create()
  else if name = 'TestScript.test' then
    result := TFhirTestScriptTest.create()
  else if name = 'TestScript.test.action' then
    result := TFhirTestScriptTestAction.create()
  else if name = 'TestScript.teardown' then
    result := TFhirTestScriptTeardown.create()
  else if name = 'TestScript.teardown.action' then
    result := TFhirTestScriptTeardownAction.create()
  else if name = 'TestScript' then
    result := TFhirTestScript.create()
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  else if name = 'ValueSet.compose' then
    result := TFhirValueSetCompose.create()
  else if name = 'ValueSet.compose.include' then
    result := TFhirValueSetComposeInclude.create()
  else if name = 'ValueSet.compose.include.concept' then
    result := TFhirValueSetComposeIncludeConcept.create()
  else if name = 'ValueSet.compose.include.concept.designation' then
    result := TFhirValueSetComposeIncludeConceptDesignation.create()
  else if name = 'ValueSet.compose.include.filter' then
    result := TFhirValueSetComposeIncludeFilter.create()
  else if name = 'ValueSet.expansion' then
    result := TFhirValueSetExpansion.create()
  else if name = 'ValueSet.expansion.parameter' then
    result := TFhirValueSetExpansionParameter.create()
  else if name = 'ValueSet.expansion.property' then
    result := TFhirValueSetExpansionProperty.create()
  else if name = 'ValueSet.expansion.contains' then
    result := TFhirValueSetExpansionContains.create()
  else if name = 'ValueSet.expansion.contains.property' then
    result := TFhirValueSetExpansionContainsProperty.create()
  else if name = 'ValueSet' then
    result := TFhirValueSet.create()
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VERIFICATIONRESULT}
  else if name = 'VerificationResult.primarySource' then
    result := TFhirVerificationResultPrimarySource.create()
  else if name = 'VerificationResult.attestation' then
    result := TFhirVerificationResultAttestation.create()
  else if name = 'VerificationResult.validator' then
    result := TFhirVerificationResultValidator.create()
  else if name = 'VerificationResult' then
    result := TFhirVerificationResult.create()
{$ENDIF FHIR_VERIFICATIONRESULT}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  else if name = 'VisionPrescription.lensSpecification' then
    result := TFhirVisionPrescriptionLensSpecification.create()
  else if name = 'VisionPrescription.lensSpecification.prism' then
    result := TFhirVisionPrescriptionLensSpecificationPrism.create()
  else if name = 'VisionPrescription' then
    result := TFhirVisionPrescription.create()
{$ENDIF FHIR_VISIONPRESCRIPTION}
{gen-factory-end}
  else
    result := nil;
end;

end.
