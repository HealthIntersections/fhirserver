unit fhir2_factory;

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

// FHIR v3.2.0 generated 2018-05-15T06:28:00+10:00

uses
  SysUtils, Classes, System.NetEncoding,
  fsl_base, fsl_utilities, fsl_stream, fsl_http,
  fsl_ucum,
  fhir_objects, fhir_parser, fhir_validator, fhir_narrative, fhir_factory, fhir_pathengine, fhir_xhtml, fhir_common,  fhir_elementmodel,
  fhir_client, fhir_client_threaded;

type
  TFHIRFactoryR2 = class (TFHIRFactory)
  public
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
    function makeParameters : TFHIRParametersW; override;
    function makeDateTime(value : TFslDateTime) : TFHIRObject; override;
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
  TFHIRFactoryX = TFHIRFactoryR2;

implementation

uses
  fhir_client_http,
  fhir2_types, fhir2_resources, fhir2_parser, fhir2_context, fhir2_validator, fhir2_profiles, fhir2_operations, fhir2_elementmodel,
  fhir2_narrative, fhir2_pathengine, fhir2_constants, fhir2_client, fhir2_common, fhir2_utilities, fhir2_authmap,
  fhir2_resources_base, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_canonical, fhir2_resources_other;

{ TFHIRFactoryR2 }

function TFHIRFactoryR2.buildOperationOutcome(const lang : THTTPLanguages; message: String; issueCode: TFhirIssueType): TFhirResourceV;
begin
  result := fhir2_utilities.BuildOperationOutcome(lang, message, ExceptionTypeTranslations[issueCode]);
end;

function TFHIRFactoryR2.buildOperationOutcome(const lang : THTTPLanguages; e: Exception; issueCode: TFhirIssueType): TFhirResourceV;
begin
  result := fhir2_utilities.BuildOperationOutcome(lang, e, ExceptionTypeTranslations[issueCode]);
end;

function TFHIRFactoryR2.canonicalResources: TArray<String>;
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

procedure TFHIRFactoryR2.checkNoModifiers(res: TFHIRObject; method, param: string; allowed : TArray<String> = nil);
begin
  if res is TFHIRDomainResource then
    TFHIRDomainResource(res).checkNoModifiers(method, param)
  else if res is TFHIRBackboneElement then
    TFHIRBackboneElement(res).checkNoModifiers(method, param)
end;

function TFHIRFactoryR2.corePackage: String;
begin
  result := 'hl7.fhir.r2.core';
end;

function TFHIRFactoryR2.createFromProfile(worker: TFHIRWorkerContextV; profile: TFhirStructureDefinitionW): TFHIRResourceV;
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

function TFHIRFactoryR2.description: String;
begin
  result := 'R2 ('+FHIR_GENERATED_VERSION+')';
end;

function TFHIRFactoryR2.getContained(r: TFHIRResourceV): TFslList<TFHIRResourceV>;
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

function TFHIRFactoryR2.getXhtml(res: TFHIRResourceV): TFHIRXhtmlNode;
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

function TFHIRFactoryR2.makeClient(worker: TFHIRWorkerContextV; url: String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout: cardinal; proxy: String): TFhirClientV;
var
  http : TFHIRHTTPCommunicator;
begin
  http := TFHIRHTTPCommunicator.Create(url);
  try
    if kind = fctCrossPlatform then
      http.UseIndy := true;
    http.timeout := timeout;
    http.proxy := proxy;
    result := TFhirClient2.create(worker, THTTPLanguages.create('en'), http.link);
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

function TFHIRFactoryR2.makeClientInt(worker: TFHIRWorkerContextV; const lang : THTTPLanguages; comm: TFHIRClientCommunicator): TFhirClientV;
begin
  result := TFhirClient2.create(worker, THTTPLanguages.create('en'), comm);
end;

function TFHIRFactoryR2.makeClientThreaded(worker: TFHIRWorkerContextV; internal: TFhirClientV; event: TThreadManagementEvent): TFhirClientV;
var
  c : TFhirThreadedCommunicator;
begin
  c := TFhirThreadedCommunicator.Create(internal, event);
  try
    result := TFhirClient2.create(worker, THTTPLanguages.create('en'), c.link);
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

function TFHIRFactoryR2.makeCode(s: string): TFHIRObject;
begin
  result := TFhirCode.Create(s);
end;

function TFHIRFactoryR2.makeCoding(system, version, code, display: String): TFHIRObject;
begin
  result := TFHIRCoding.create(system, code);
  if version <> '' then
    TFHIRCoding(result).version := version;
  if display <> '' then
    TFHIRCoding(result).version := display;
end;

function TFHIRFactoryR2.makeComposer(worker: TFHIRWorkerContextV; format: TFHIRFormat; const lang : THTTPLanguages; style: TFHIROutputStyle): TFHIRComposer;
begin
  result := TFHIRParsers2.composer(worker as TFHIRWorkerContext, format, lang, style);
end;

function TFHIRFactoryR2.makeDateTime(value: TFslDateTime): TFHIRObject;
begin
  result := TFhirDateTime.Create(value);
end;

function TFHIRFactoryR2.makeDecimal(s: string): TFHIRObject;
begin
  result := TFhirDecimal.Create(s);
end;

function TFHIRFactoryR2.makeDtFromForm(part: TMimePart; const lang : THTTPLanguages; name: String; type_: string): TFHIRXVersionElementWrapper;
begin
  if type_ = 'Coding' then
    result := wrapCoding(LoadDTFromFormParam(nil, part, lang, name, TFhirCoding))
  else if type_ = 'CodeableConcept' then
    result := wrapCodeableConcept(LoadDTFromFormParam(nil, part, lang, name, TFhirCodeableConcept))
  else
    raise EFHIRException.create('Unknown Supported Data Type '+type_);
end;

function TFHIRFactoryR2.makeDuration(dt: TDateTime): TFHIRObject;
begin
  result := TFhirQuantity.fromDuration(dt);
end;

function TFHIRFactoryR2.makeElementModelManager: TFHIRBaseMMManager;
begin
  result := TFHIRMMManager.Create;
end;

function TFHIRFactoryR2.makeGenerator(worker: TFHIRWorkerContextV): TFHIRNarrativeGeneratorBase;
begin
  result := TFHIRNarrativeGenerator.create(worker);
end;

function TFHIRFactoryR2.makeInteger(s: string): TFHIRObject;
begin
  result := TFhirInteger.Create(s);
end;

function TFHIRFactoryR2.makeIssue(level : TIssueSeverity; issue: TFhirIssueType; location, message: String): TFhirOperationOutcomeIssueW;
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
    result := TFhirOperationOutcomeIssue2.create(iss.Link);
  finally
    iss.Free;
  end;
end;

function TFHIRFactoryR2.makeOpReqLookup: TFHIRLookupOpRequestW;
begin
  result := TFHIRLookupOpRequest2.create(TFHIRLookupOpRequest.create);
end;

function TFHIRFactoryR2.makeOpReqSubsumes: TFHIRSubsumesOpRequestW;
begin
  raise EFslException.Create('Not supported in R2');
end;

function TFHIRFactoryR2.makeOpRespLookup: TFHIRLookupOpResponseW;
begin
  result := TFHIRLookupOpResponse2.create(TFHIRLookupOpResponse.create);
end;

function TFHIRFactoryR2.makeOpRespSubsumes: TFHIRSubsumesOpResponseW;
begin
  raise EFslException.Create('Not supported in R2');
end;

function TFHIRFactoryR2.makeParameters: TFHIRParametersW;
begin
  result := TFHIRParameters2.Create(TFHIRParameters.Create);
end;

function TFHIRFactoryR2.makeParamsFromForm(s: TStream): TFHIRResourceV;
begin
  result := parseParamsFromForm(s);
end;

function TFHIRFactoryR2.makeParser(worker: TFHIRWorkerContextV; format: TFHIRFormat; const lang : THTTPLanguages): TFHIRParser;
begin
  result := TFHIRParsers2.parser(worker as TFHIRWorkerContext, format, lang);
end;

function TFHIRFactoryR2.makePathEngine(worker: TFHIRWorkerContextV; ucum : TUcumServiceInterface): TFHIRPathEngineV;
begin
  result := TFHIRPathEngine.Create(worker as TFHIRWorkerContext, ucum);
end;

function TFHIRFactoryR2.makeString(s: string): TFHIRObject;
begin
  result := TFhirString.Create(s);
end;

function TFHIRFactoryR2.makeTerminologyCapablities: TFhirTerminologyCapabilitiesW;
begin
  result := TFhirTerminologyCapabilities2.create(TFHIRParameters.create);
end;

function TFHIRFactoryR2.makeValidator(worker: TFHIRWorkerContextV): TFHIRValidatorV;
begin
  result := TFHIRValidator2.Create(worker as TFHIRWorkerContext);
end;

function TFHIRFactoryR2.makeValueSetContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains2.Create(TFhirValueSetExpansionContains.create);
end;

procedure TFHIRFactoryR2.markWithTag(r: TFHIRResourceV; systemUri, code, display: String);
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

function TFHIRFactoryR2.resCategory(name: String): TTokenCategory;
var
  a : TFhirResourceType;
begin
  for a in ALL_RESOURCE_TYPES do
    if CODES_TFhirResourceType[a] = name then
      exit(RESOURCE_CATEGORY[a]);
  result := tcOther;
end;

function TFHIRFactoryR2.resetXhtml(res: TFHIRResourceV): TFHIRXhtmlNode;
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

function TFHIRFactoryR2.resourceNames: TArray<String>;
var
  a : TFhirResourceType;
begin
  SetLength(result, length(CODES_TFhirResourceType)-2);
  for a in ALL_RESOURCE_TYPES do
    if not (a in [frtNull, frtCustom]) then
      result[ord(a)-1] := CODES_TFhirResourceType[a];
end;

procedure TFHIRFactoryR2.setXhtml(res: TFHIRResourceV; x: TFHIRXhtmlNode);
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

function TFHIRFactoryR2.specUrl: String;
begin
  result := 'http://build.fhir.org';
end;

function TFHIRFactoryR2.txPackage: String;
begin
  result := '';
end;

function TFHIRFactoryR2.txSupportPackage: String;
begin
  result := 'fhir.tx.support.r2';

end;

function TFHIRFactoryR2.version: TFHIRVersion;
begin
  result := fhirVersionRelease2;
end;

function TFHIRFactoryR2.versionName: String;
begin
  result := 'R2';
end;

function TFHIRFactoryR2.versionString: String;
begin
  result := FHIR_GENERATED_VERSION;
end;

function TFHIRFactoryR2.wrapAuditEvent(r: TFHIRResourceV): TFhirAuditEventW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirAuditEvent2.Create(r);
end;

function TFHIRFactoryR2.wrapBinary(r: TFHIRResourceV): TFhirBinaryW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirBinary2.Create(r);
end;

function TFHIRFactoryR2.wrapBundle(r: TFHIRResourceV): TFhirBundleW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirBundle2.Create(r);
end;

function TFHIRFactoryR2.wrapBundleEntry(o: TFHIRObject): TFhirBundleEntryW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRBundleEntry2.Create(o);
end;

function TFHIRFactoryR2.wrapCapabilityStatement(r: TFHIRResourceV): TFHIRCapabilityStatementW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRCapabilityStatement2.create(r);
end;

function TFHIRFactoryR2.wrapCodeableConcept(o: TFHIRObject): TFhirCodeableConceptW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirCodeableConcept2.create(o);
end;

function TFHIRFactoryR2.wrapCodeSystem(r: TFHIRResourceV): TFhirCodeSystemW;
begin
  if r = nil then
    result := nil
  else
    result := fhir2_common.TFHIRCodeSystem2.create(r);
end;

function TFHIRFactoryR2.wrapCoding(o: TFHIRObject): TFhirCodingW;
begin
  if o = nil then
    result := nil
  else
  result := TFhirCoding2.create(o);
end;

function TFHIRFactoryR2.wrapConceptMap(r: TFHIRResourceV): TFhirConceptMapW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirConceptMap2.Create(r);
end;

function TFHIRFactoryR2.wrapConsent(o: TFHIRResourceV): TFHIRConsentW;
begin
  raise EFslException.Create('Not supported in R2');
end;

function TFHIRFactoryR2.wrapEncounter(r: TFHIRResourceV): TFhirEncounterW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirEncounter2.Create(r);
end;

function TFHIRFactoryR2.wrapEventDefinition(o: TFHIRResourceV): TFHIREventDefinitionW;
begin
  raise EFslException.Create('Not supported in R2');
end;

function TFHIRFactoryR2.wrapExtension(o: TFHIRObject): TFhirExtensionW;
begin
  result := TFhirExtension2.create(o);
end;

function TFHIRFactoryR2.wrapGroup(r: TFHIRResourceV): TFhirGroupW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirGroup2.Create(r);
end;

function TFHIRFactoryR2.wrapMeta(r: TFHIRObject): TFhirMetaW;
begin
  if r = nil then
    result := nil
  else if r.isResource then
  begin
    result := TFHIRMeta2.create((r as TFHIRResource).meta.link);
    TFHIRMeta2(result).resource := (r as TFHIRResource).link;
  end
  else
    result := TFHIRMeta2.create((r as TFhirMeta))
end;

function TFHIRFactoryR2.wrapNamingSystem(o: TFHIRResourceV): TFHIRNamingSystemW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRNamingSystem2.Create(o);
end;

function TFHIRFactoryR2.wrapMeta(r: TFHIRResourceV): TFhirMetaW;
begin
  result := TFHIRMeta2.create((r as TFHIRResource).meta.link);
  TFHIRMeta2(result).resource := (r as TFHIRResource).link;
end;

function TFHIRFactoryR2.wrapObservation(r: TFHIRResourceV): TFhirObservationW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirObservation2.Create(r);
end;

function TFHIRFactoryR2.wrapOperationOutcome(r: TFHIRResourceV): TFhirOperationOutcomeW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirOperationOutcome2.Create(r);
end;

function TFHIRFactoryR2.wrapParams(r: TFHIRResourceV): TFHIRParametersW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirParameters2.Create(r);
end;

function TFHIRFactoryR2.wrapPatient(r: TFHIRResourceV): TFhirPatientW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirPatient2.Create(r);
end;

function TFHIRFactoryR2.wrapPeriod(r: TFHIRObject): TFhirPeriodW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirPeriod2.Create(r);
end;

function TFHIRFactoryR2.wrapProvenance(o: TFHIRResourceV): TFHIRProvenanceW;
begin
  if o = nil then
    result := nil
  else
    result := TFHIRProvenance2.Create(o);
end;

function TFHIRFactoryR2.wrapQuantity(r: TFHIRObject): TFhirQuantityW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirQuantity2.Create(r);
end;

function TFHIRFactoryR2.wrapStructureDefinition(r: TFHIRResourceV): TFhirStructureDefinitionW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRStructureDefinition2.create(r);
end;

function TFHIRFactoryR2.wrapStructureMap(o: TFHIRResourceV): TFHIRStructureMapW;
begin
  if o = nil then
    result := nil
  else
  raise EFslException.Create('Not supported in R2');
end;

function TFHIRFactoryR2.wrapSubscription(r: TFHIRResourceV): TFhirSubscriptionW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirSubscription2.Create(r);
end;

function TFHIRFactoryR2.wrapSubscriptionTopic(r: TFHIRResourceV): TFhirSubscriptionTopicW;
begin
  result := nil;
end;

function TFHIRFactoryR2.wrapTestScript(o: TFHIRResourceV): TFHIRTestScriptW;
begin
  if o = nil then
    result := nil
  else
    result := TFhirTestScript2.Create(o);
end;

function TFHIRFactoryR2.wrapValueSet(r: TFHIRResourceV): TFhirValueSetW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRValueSet2.create(r);
end;

function TFHIRFactoryR2.makeBase64Binary(s: string): TFHIRObject;
begin
  result := TFhirBase64Binary.Create(decodeBase64(AnsiString(s)));
end;

function TFHIRFactoryR2.makeBinary(content: TBytes; contentType: String): TFHIRResourceV;
begin
  result := TFhirBinary.Create;
  try
    TFhirBinary(result).content := content;
    TFhirBinary(result).contentType := contentType;
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRFactoryR2.makeBoolean(b: boolean): TFHIRObject;
begin
  result := TFhirBoolean.Create(b);
end;

function TFHIRFactoryR2.makeBundle(list: TFslList<TFHIRResourceV>): TFHIRBundleW;
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
    result := TFHIRBundle2.Create(bnd.link);
  finally
    bnd.Free;
  end;
end;

function TFHIRFactoryR2.makeByName(const name : String) : TFHIRObject;
begin
  if name = 'enum' then
    result := TFhirEnum.create()
  else if name = 'dateTime' then
    result := TFhirDateTime.create()
  else if name = 'date' then
    result := TFhirDate.create()
  else if name = 'string' then
    result := TFhirString.create()
  else if name = 'integer' then
    result := TFhirInteger.create()
  else if name = 'uri' then
    result := TFhirUri.create()
  else if name = 'instant' then
    result := TFhirInstant.create()
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
  else if name = 'oid' then
    result := TFhirOid.create()
  else if name = 'uuid' then
    result := TFhirUuid.create()
  else if name = 'markdown' then
    result := TFhirMarkdown.create()
  else if name = 'unsignedInt' then
    result := TFhirUnsignedInt.create()
  else if name = 'id' then
    result := TFhirId.create()
  else if name = 'positiveInt' then
    result := TFhirPositiveInt.create()
{$IFDEF FHIR_PARAMETERS}
  else if name = 'Parameters.parameter' then
    result := TFhirParametersParameter.create()
  else if name = 'Parameters' then
    result := TFhirParameters.create()
{$ENDIF FHIR_PARAMETERS}
  else if name = 'Extension' then
    result := TFhirExtension.create()
  else if name = 'Narrative' then
    result := TFhirNarrative.create()
  else if name = 'Identifier' then
    result := TFhirIdentifier.create()
  else if name = 'Coding' then
    result := TFhirCoding.create()
  else if name = 'Reference' then
    result := TFhirReference.create()
  else if name = 'Signature' then
    result := TFhirSignature.create()
  else if name = 'SampledData' then
    result := TFhirSampledData.create()
  else if name = 'Period' then
    result := TFhirPeriod.create()
  else if name = 'Quantity' then
    result := TFhirQuantity.create()
  else if name = 'Attachment' then
    result := TFhirAttachment.create()
  else if name = 'Ratio' then
    result := TFhirRatio.create()
  else if name = 'Range' then
    result := TFhirRange.create()
  else if name = 'Annotation' then
    result := TFhirAnnotation.create()
  else if name = 'CodeableConcept' then
    result := TFhirCodeableConcept.create()
  else if name = 'HumanName' then
    result := TFhirHumanName.create()
  else if name = 'Meta' then
    result := TFhirMeta.create()
  else if name = 'ContactPoint' then
    result := TFhirContactPoint.create()
  else if name = 'Address' then
    result := TFhirAddress.create()
  else if name = 'ElementDefinition.slicing' then
    result := TFhirElementDefinitionSlicing.create()
  else if name = 'ElementDefinition.base' then
    result := TFhirElementDefinitionBase.create()
  else if name = 'ElementDefinition.type' then
    result := TFhirElementDefinitionType.create()
  else if name = 'ElementDefinition.constraint' then
    result := TFhirElementDefinitionConstraint.create()
  else if name = 'ElementDefinition.binding' then
    result := TFhirElementDefinitionBinding.create()
  else if name = 'ElementDefinition.mapping' then
    result := TFhirElementDefinitionMapping.create()
  else if name = 'ElementDefinition' then
    result := TFhirElementDefinition.create()
  else if name = 'Timing.repeat' then
    result := TFhirTimingRepeat.create()
  else if name = 'Timing' then
    result := TFhirTiming.create()
{$IFDEF FHIR_ACCOUNT}
  else if name = 'Account' then
    result := TFhirAccount.create()
{$ENDIF FHIR_ACCOUNT}
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
  else if name = 'AuditEvent.event' then
    result := TFhirAuditEventEvent.create()
  else if name = 'AuditEvent.participant' then
    result := TFhirAuditEventParticipant.create()
  else if name = 'AuditEvent.participant.network' then
    result := TFhirAuditEventParticipantNetwork.create()
  else if name = 'AuditEvent.source' then
    result := TFhirAuditEventSource.create()
  else if name = 'AuditEvent.object' then
    result := TFhirAuditEventObject.create()
  else if name = 'AuditEvent.object.detail' then
    result := TFhirAuditEventObjectDetail.create()
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
{$IFDEF FHIR_BODYSITE}
  else if name = 'BodySite' then
    result := TFhirBodySite.create()
{$ENDIF FHIR_BODYSITE}
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
{$IFDEF FHIR_CAREPLAN}
  else if name = 'CarePlan.relatedPlan' then
    result := TFhirCarePlanRelatedPlan.create()
  else if name = 'CarePlan.participant' then
    result := TFhirCarePlanParticipant.create()
  else if name = 'CarePlan.activity' then
    result := TFhirCarePlanActivity.create()
  else if name = 'CarePlan.activity.detail' then
    result := TFhirCarePlanActivityDetail.create()
  else if name = 'CarePlan' then
    result := TFhirCarePlan.create()
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CLAIM}
  else if name = 'Claim.payee' then
    result := TFhirClaimPayee.create()
  else if name = 'Claim.diagnosis' then
    result := TFhirClaimDiagnosis.create()
  else if name = 'Claim.coverage' then
    result := TFhirClaimCoverage.create()
  else if name = 'Claim.item' then
    result := TFhirClaimItem.create()
  else if name = 'Claim.item.detail' then
    result := TFhirClaimItemDetail.create()
  else if name = 'Claim.item.detail.subDetail' then
    result := TFhirClaimItemDetailSubDetail.create()
  else if name = 'Claim.item.prosthesis' then
    result := TFhirClaimItemProsthesis.create()
  else if name = 'Claim.missingTeeth' then
    result := TFhirClaimMissingTeeth.create()
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
  else if name = 'ClaimResponse.item.detail.adjudication' then
    result := TFhirClaimResponseItemDetailAdjudication.create()
  else if name = 'ClaimResponse.item.detail.subDetail' then
    result := TFhirClaimResponseItemDetailSubDetail.create()
  else if name = 'ClaimResponse.item.detail.subDetail.adjudication' then
    result := TFhirClaimResponseItemDetailSubDetailAdjudication.create()
  else if name = 'ClaimResponse.addItem' then
    result := TFhirClaimResponseAddItem.create()
  else if name = 'ClaimResponse.addItem.adjudication' then
    result := TFhirClaimResponseAddItemAdjudication.create()
  else if name = 'ClaimResponse.addItem.detail' then
    result := TFhirClaimResponseAddItemDetail.create()
  else if name = 'ClaimResponse.addItem.detail.adjudication' then
    result := TFhirClaimResponseAddItemDetailAdjudication.create()
  else if name = 'ClaimResponse.error' then
    result := TFhirClaimResponseError.create()
  else if name = 'ClaimResponse.note' then
    result := TFhirClaimResponseNote.create()
  else if name = 'ClaimResponse.coverage' then
    result := TFhirClaimResponseCoverage.create()
  else if name = 'ClaimResponse' then
    result := TFhirClaimResponse.create()
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  else if name = 'ClinicalImpression.investigations' then
    result := TFhirClinicalImpressionInvestigations.create()
  else if name = 'ClinicalImpression.finding' then
    result := TFhirClinicalImpressionFinding.create()
  else if name = 'ClinicalImpression.ruledOut' then
    result := TFhirClinicalImpressionRuledOut.create()
  else if name = 'ClinicalImpression' then
    result := TFhirClinicalImpression.create()
{$ENDIF FHIR_CLINICALIMPRESSION}
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
{$IFDEF FHIR_COMPOSITION}
  else if name = 'Composition.attester' then
    result := TFhirCompositionAttester.create()
  else if name = 'Composition.event' then
    result := TFhirCompositionEvent.create()
  else if name = 'Composition.section' then
    result := TFhirCompositionSection.create()
  else if name = 'Composition' then
    result := TFhirComposition.create()
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  else if name = 'ConceptMap.contact' then
    result := TFhirConceptMapContact.create()
  else if name = 'ConceptMap.element' then
    result := TFhirConceptMapElement.create()
  else if name = 'ConceptMap.element.target' then
    result := TFhirConceptMapElementTarget.create()
  else if name = 'ConceptMap.element.target.dependsOn' then
    result := TFhirConceptMapElementTargetDependsOn.create()
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
{$IFDEF FHIR_CONFORMANCE}
  else if name = 'Conformance.contact' then
    result := TFhirConformanceContact.create()
  else if name = 'Conformance.software' then
    result := TFhirConformanceSoftware.create()
  else if name = 'Conformance.implementation' then
    result := TFhirConformanceImplementation.create()
  else if name = 'Conformance.rest' then
    result := TFhirConformanceRest.create()
  else if name = 'Conformance.rest.security' then
    result := TFhirConformanceRestSecurity.create()
  else if name = 'Conformance.rest.security.certificate' then
    result := TFhirConformanceRestSecurityCertificate.create()
  else if name = 'Conformance.rest.resource' then
    result := TFhirConformanceRestResource.create()
  else if name = 'Conformance.rest.resource.interaction' then
    result := TFhirConformanceRestResourceInteraction.create()
  else if name = 'Conformance.rest.resource.searchParam' then
    result := TFhirConformanceRestResourceSearchParam.create()
  else if name = 'Conformance.rest.interaction' then
    result := TFhirConformanceRestInteraction.create()
  else if name = 'Conformance.rest.operation' then
    result := TFhirConformanceRestOperation.create()
  else if name = 'Conformance.messaging' then
    result := TFhirConformanceMessaging.create()
  else if name = 'Conformance.messaging.endpoint' then
    result := TFhirConformanceMessagingEndpoint.create()
  else if name = 'Conformance.messaging.event' then
    result := TFhirConformanceMessagingEvent.create()
  else if name = 'Conformance.document' then
    result := TFhirConformanceDocument.create()
  else if name = 'Conformance' then
    result := TFhirConformance.create()
{$ENDIF FHIR_CONFORMANCE}
{$IFDEF FHIR_CONTRACT}
  else if name = 'Contract.actor' then
    result := TFhirContractActor.create()
  else if name = 'Contract.valuedItem' then
    result := TFhirContractValuedItem.create()
  else if name = 'Contract.signer' then
    result := TFhirContractSigner.create()
  else if name = 'Contract.term' then
    result := TFhirContractTerm.create()
  else if name = 'Contract.term.actor' then
    result := TFhirContractTermActor.create()
  else if name = 'Contract.term.valuedItem' then
    result := TFhirContractTermValuedItem.create()
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
  else if name = 'Coverage' then
    result := TFhirCoverage.create()
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_DATAELEMENT}
  else if name = 'DataElement.contact' then
    result := TFhirDataElementContact.create()
  else if name = 'DataElement.mapping' then
    result := TFhirDataElementMapping.create()
  else if name = 'DataElement' then
    result := TFhirDataElement.create()
{$ENDIF FHIR_DATAELEMENT}
{$IFDEF FHIR_DETECTEDISSUE}
  else if name = 'DetectedIssue.mitigation' then
    result := TFhirDetectedIssueMitigation.create()
  else if name = 'DetectedIssue' then
    result := TFhirDetectedIssue.create()
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  else if name = 'Device' then
    result := TFhirDevice.create()
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICECOMPONENT}
  else if name = 'DeviceComponent.productionSpecification' then
    result := TFhirDeviceComponentProductionSpecification.create()
  else if name = 'DeviceComponent' then
    result := TFhirDeviceComponent.create()
{$ENDIF FHIR_DEVICECOMPONENT}
{$IFDEF FHIR_DEVICEMETRIC}
  else if name = 'DeviceMetric.calibration' then
    result := TFhirDeviceMetricCalibration.create()
  else if name = 'DeviceMetric' then
    result := TFhirDeviceMetric.create()
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEUSEREQUEST}
  else if name = 'DeviceUseRequest' then
    result := TFhirDeviceUseRequest.create()
{$ENDIF FHIR_DEVICEUSEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  else if name = 'DeviceUseStatement' then
    result := TFhirDeviceUseStatement.create()
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICORDER}
  else if name = 'DiagnosticOrder.event' then
    result := TFhirDiagnosticOrderEvent.create()
  else if name = 'DiagnosticOrder.item' then
    result := TFhirDiagnosticOrderItem.create()
  else if name = 'DiagnosticOrder' then
    result := TFhirDiagnosticOrder.create()
{$ENDIF FHIR_DIAGNOSTICORDER}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  else if name = 'DiagnosticReport.image' then
    result := TFhirDiagnosticReportImage.create()
  else if name = 'DiagnosticReport' then
    result := TFhirDiagnosticReport.create()
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  else if name = 'DocumentManifest.content' then
    result := TFhirDocumentManifestContent.create()
  else if name = 'DocumentManifest.related' then
    result := TFhirDocumentManifestRelated.create()
  else if name = 'DocumentManifest' then
    result := TFhirDocumentManifest.create()
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  else if name = 'DocumentReference.relatesTo' then
    result := TFhirDocumentReferenceRelatesTo.create()
  else if name = 'DocumentReference.content' then
    result := TFhirDocumentReferenceContent.create()
  else if name = 'DocumentReference.context' then
    result := TFhirDocumentReferenceContext.create()
  else if name = 'DocumentReference.context.related' then
    result := TFhirDocumentReferenceContextRelated.create()
  else if name = 'DocumentReference' then
    result := TFhirDocumentReference.create()
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_ELIGIBILITYREQUEST}
  else if name = 'EligibilityRequest' then
    result := TFhirEligibilityRequest.create()
{$ENDIF FHIR_ELIGIBILITYREQUEST}
{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  else if name = 'EligibilityResponse' then
    result := TFhirEligibilityResponse.create()
{$ENDIF FHIR_ELIGIBILITYRESPONSE}
{$IFDEF FHIR_ENCOUNTER}
  else if name = 'Encounter.statusHistory' then
    result := TFhirEncounterStatusHistory.create()
  else if name = 'Encounter.participant' then
    result := TFhirEncounterParticipant.create()
  else if name = 'Encounter.hospitalization' then
    result := TFhirEncounterHospitalization.create()
  else if name = 'Encounter.location' then
    result := TFhirEncounterLocation.create()
  else if name = 'Encounter' then
    result := TFhirEncounter.create()
{$ENDIF FHIR_ENCOUNTER}
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
  else if name = 'EpisodeOfCare.careTeam' then
    result := TFhirEpisodeOfCareCareTeam.create()
  else if name = 'EpisodeOfCare' then
    result := TFhirEpisodeOfCare.create()
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  else if name = 'ExplanationOfBenefit' then
    result := TFhirExplanationOfBenefit.create()
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  else if name = 'FamilyMemberHistory.condition' then
    result := TFhirFamilyMemberHistoryCondition.create()
  else if name = 'FamilyMemberHistory' then
    result := TFhirFamilyMemberHistory.create()
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  else if name = 'Flag' then
    result := TFhirFlag.create()
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  else if name = 'Goal.outcome' then
    result := TFhirGoalOutcome.create()
  else if name = 'Goal' then
    result := TFhirGoal.create()
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GROUP}
  else if name = 'Group.characteristic' then
    result := TFhirGroupCharacteristic.create()
  else if name = 'Group.member' then
    result := TFhirGroupMember.create()
  else if name = 'Group' then
    result := TFhirGroup.create()
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_HEALTHCARESERVICE}
  else if name = 'HealthcareService.serviceType' then
    result := TFhirHealthcareServiceServiceType.create()
  else if name = 'HealthcareService.availableTime' then
    result := TFhirHealthcareServiceAvailableTime.create()
  else if name = 'HealthcareService.notAvailable' then
    result := TFhirHealthcareServiceNotAvailable.create()
  else if name = 'HealthcareService' then
    result := TFhirHealthcareService.create()
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGOBJECTSELECTION}
  else if name = 'ImagingObjectSelection.study' then
    result := TFhirImagingObjectSelectionStudy.create()
  else if name = 'ImagingObjectSelection.study.series' then
    result := TFhirImagingObjectSelectionStudySeries.create()
  else if name = 'ImagingObjectSelection.study.series.instance' then
    result := TFhirImagingObjectSelectionStudySeriesInstance.create()
  else if name = 'ImagingObjectSelection.study.series.instance.frames' then
    result := TFhirImagingObjectSelectionStudySeriesInstanceFrames.create()
  else if name = 'ImagingObjectSelection' then
    result := TFhirImagingObjectSelection.create()
{$ENDIF FHIR_IMAGINGOBJECTSELECTION}
{$IFDEF FHIR_IMAGINGSTUDY}
  else if name = 'ImagingStudy.series' then
    result := TFhirImagingStudySeries.create()
  else if name = 'ImagingStudy.series.instance' then
    result := TFhirImagingStudySeriesInstance.create()
  else if name = 'ImagingStudy' then
    result := TFhirImagingStudy.create()
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  else if name = 'Immunization.explanation' then
    result := TFhirImmunizationExplanation.create()
  else if name = 'Immunization.reaction' then
    result := TFhirImmunizationReaction.create()
  else if name = 'Immunization.vaccinationProtocol' then
    result := TFhirImmunizationVaccinationProtocol.create()
  else if name = 'Immunization' then
    result := TFhirImmunization.create()
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  else if name = 'ImmunizationRecommendation.recommendation' then
    result := TFhirImmunizationRecommendationRecommendation.create()
  else if name = 'ImmunizationRecommendation.recommendation.dateCriterion' then
    result := TFhirImmunizationRecommendationRecommendationDateCriterion.create()
  else if name = 'ImmunizationRecommendation.recommendation.protocol' then
    result := TFhirImmunizationRecommendationRecommendationProtocol.create()
  else if name = 'ImmunizationRecommendation' then
    result := TFhirImmunizationRecommendation.create()
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  else if name = 'ImplementationGuide.contact' then
    result := TFhirImplementationGuideContact.create()
  else if name = 'ImplementationGuide.dependency' then
    result := TFhirImplementationGuideDependency.create()
  else if name = 'ImplementationGuide.package' then
    result := TFhirImplementationGuidePackage.create()
  else if name = 'ImplementationGuide.package.resource' then
    result := TFhirImplementationGuidePackageResource.create()
  else if name = 'ImplementationGuide.global' then
    result := TFhirImplementationGuideGlobal.create()
  else if name = 'ImplementationGuide.page' then
    result := TFhirImplementationGuidePage.create()
  else if name = 'ImplementationGuide' then
    result := TFhirImplementationGuide.create()
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_LIST}
  else if name = 'List.entry' then
    result := TFhirListEntry.create()
  else if name = 'List' then
    result := TFhirList.create()
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_LOCATION}
  else if name = 'Location.position' then
    result := TFhirLocationPosition.create()
  else if name = 'Location' then
    result := TFhirLocation.create()
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_MEDIA}
  else if name = 'Media' then
    result := TFhirMedia.create()
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATION}
  else if name = 'Medication.product' then
    result := TFhirMedicationProduct.create()
  else if name = 'Medication.product.ingredient' then
    result := TFhirMedicationProductIngredient.create()
  else if name = 'Medication.product.batch' then
    result := TFhirMedicationProductBatch.create()
  else if name = 'Medication.package' then
    result := TFhirMedicationPackage.create()
  else if name = 'Medication.package.content' then
    result := TFhirMedicationPackageContent.create()
  else if name = 'Medication' then
    result := TFhirMedication.create()
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  else if name = 'MedicationAdministration.dosage' then
    result := TFhirMedicationAdministrationDosage.create()
  else if name = 'MedicationAdministration' then
    result := TFhirMedicationAdministration.create()
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  else if name = 'MedicationDispense.dosageInstruction' then
    result := TFhirMedicationDispenseDosageInstruction.create()
  else if name = 'MedicationDispense.substitution' then
    result := TFhirMedicationDispenseSubstitution.create()
  else if name = 'MedicationDispense' then
    result := TFhirMedicationDispense.create()
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONORDER}
  else if name = 'MedicationOrder.dosageInstruction' then
    result := TFhirMedicationOrderDosageInstruction.create()
  else if name = 'MedicationOrder.dispenseRequest' then
    result := TFhirMedicationOrderDispenseRequest.create()
  else if name = 'MedicationOrder.substitution' then
    result := TFhirMedicationOrderSubstitution.create()
  else if name = 'MedicationOrder' then
    result := TFhirMedicationOrder.create()
{$ENDIF FHIR_MEDICATIONORDER}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  else if name = 'MedicationStatement.dosage' then
    result := TFhirMedicationStatementDosage.create()
  else if name = 'MedicationStatement' then
    result := TFhirMedicationStatement.create()
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_MESSAGEHEADER}
  else if name = 'MessageHeader.response' then
    result := TFhirMessageHeaderResponse.create()
  else if name = 'MessageHeader.source' then
    result := TFhirMessageHeaderSource.create()
  else if name = 'MessageHeader.destination' then
    result := TFhirMessageHeaderDestination.create()
  else if name = 'MessageHeader' then
    result := TFhirMessageHeader.create()
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_NAMINGSYSTEM}
  else if name = 'NamingSystem.contact' then
    result := TFhirNamingSystemContact.create()
  else if name = 'NamingSystem.uniqueId' then
    result := TFhirNamingSystemUniqueId.create()
  else if name = 'NamingSystem' then
    result := TFhirNamingSystem.create()
{$ENDIF FHIR_NAMINGSYSTEM}
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
{$IFDEF FHIR_OBSERVATION}
  else if name = 'Observation.referenceRange' then
    result := TFhirObservationReferenceRange.create()
  else if name = 'Observation.related' then
    result := TFhirObservationRelated.create()
  else if name = 'Observation.component' then
    result := TFhirObservationComponent.create()
  else if name = 'Observation' then
    result := TFhirObservation.create()
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  else if name = 'OperationDefinition.contact' then
    result := TFhirOperationDefinitionContact.create()
  else if name = 'OperationDefinition.parameter' then
    result := TFhirOperationDefinitionParameter.create()
  else if name = 'OperationDefinition.parameter.binding' then
    result := TFhirOperationDefinitionParameterBinding.create()
  else if name = 'OperationDefinition' then
    result := TFhirOperationDefinition.create()
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  else if name = 'OperationOutcome.issue' then
    result := TFhirOperationOutcomeIssue.create()
  else if name = 'OperationOutcome' then
    result := TFhirOperationOutcome.create()
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_ORDER}
  else if name = 'Order.when' then
    result := TFhirOrderWhen.create()
  else if name = 'Order' then
    result := TFhirOrder.create()
{$ENDIF FHIR_ORDER}
{$IFDEF FHIR_ORDERRESPONSE}
  else if name = 'OrderResponse' then
    result := TFhirOrderResponse.create()
{$ENDIF FHIR_ORDERRESPONSE}
{$IFDEF FHIR_ORGANIZATION}
  else if name = 'Organization.contact' then
    result := TFhirOrganizationContact.create()
  else if name = 'Organization' then
    result := TFhirOrganization.create()
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_PATIENT}
  else if name = 'Patient.contact' then
    result := TFhirPatientContact.create()
  else if name = 'Patient.animal' then
    result := TFhirPatientAnimal.create()
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
  else if name = 'PaymentReconciliation.note' then
    result := TFhirPaymentReconciliationNote.create()
  else if name = 'PaymentReconciliation' then
    result := TFhirPaymentReconciliation.create()
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERSON}
  else if name = 'Person.link' then
    result := TFhirPersonLink.create()
  else if name = 'Person' then
    result := TFhirPerson.create()
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PRACTITIONER}
  else if name = 'Practitioner.practitionerRole' then
    result := TFhirPractitionerPractitionerRole.create()
  else if name = 'Practitioner.qualification' then
    result := TFhirPractitionerQualification.create()
  else if name = 'Practitioner' then
    result := TFhirPractitioner.create()
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PROCEDURE}
  else if name = 'Procedure.performer' then
    result := TFhirProcedurePerformer.create()
  else if name = 'Procedure.focalDevice' then
    result := TFhirProcedureFocalDevice.create()
  else if name = 'Procedure' then
    result := TFhirProcedure.create()
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROCEDUREREQUEST}
  else if name = 'ProcedureRequest' then
    result := TFhirProcedureRequest.create()
{$ENDIF FHIR_PROCEDUREREQUEST}
{$IFDEF FHIR_PROCESSREQUEST}
  else if name = 'ProcessRequest.item' then
    result := TFhirProcessRequestItem.create()
  else if name = 'ProcessRequest' then
    result := TFhirProcessRequest.create()
{$ENDIF FHIR_PROCESSREQUEST}
{$IFDEF FHIR_PROCESSRESPONSE}
  else if name = 'ProcessResponse.notes' then
    result := TFhirProcessResponseNotes.create()
  else if name = 'ProcessResponse' then
    result := TFhirProcessResponse.create()
{$ENDIF FHIR_PROCESSRESPONSE}
{$IFDEF FHIR_PROVENANCE}
  else if name = 'Provenance.agent' then
    result := TFhirProvenanceAgent.create()
  else if name = 'Provenance.agent.relatedAgent' then
    result := TFhirProvenanceAgentRelatedAgent.create()
  else if name = 'Provenance.entity' then
    result := TFhirProvenanceEntity.create()
  else if name = 'Provenance' then
    result := TFhirProvenance.create()
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  else if name = 'Questionnaire.group' then
    result := TFhirQuestionnaireGroup.create()
  else if name = 'Questionnaire.group.question' then
    result := TFhirQuestionnaireGroupQuestion.create()
  else if name = 'Questionnaire' then
    result := TFhirQuestionnaire.create()
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  else if name = 'QuestionnaireResponse.group' then
    result := TFhirQuestionnaireResponseGroup.create()
  else if name = 'QuestionnaireResponse.group.question' then
    result := TFhirQuestionnaireResponseGroupQuestion.create()
  else if name = 'QuestionnaireResponse.group.question.answer' then
    result := TFhirQuestionnaireResponseGroupQuestionAnswer.create()
  else if name = 'QuestionnaireResponse' then
    result := TFhirQuestionnaireResponse.create()
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REFERRALREQUEST}
  else if name = 'ReferralRequest' then
    result := TFhirReferralRequest.create()
{$ENDIF FHIR_REFERRALREQUEST}
{$IFDEF FHIR_RELATEDPERSON}
  else if name = 'RelatedPerson' then
    result := TFhirRelatedPerson.create()
{$ENDIF FHIR_RELATEDPERSON}
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
  else if name = 'SearchParameter.contact' then
    result := TFhirSearchParameterContact.create()
  else if name = 'SearchParameter' then
    result := TFhirSearchParameter.create()
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SLOT}
  else if name = 'Slot' then
    result := TFhirSlot.create()
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  else if name = 'Specimen.collection' then
    result := TFhirSpecimenCollection.create()
  else if name = 'Specimen.treatment' then
    result := TFhirSpecimenTreatment.create()
  else if name = 'Specimen.container' then
    result := TFhirSpecimenContainer.create()
  else if name = 'Specimen' then
    result := TFhirSpecimen.create()
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  else if name = 'StructureDefinition.contact' then
    result := TFhirStructureDefinitionContact.create()
  else if name = 'StructureDefinition.mapping' then
    result := TFhirStructureDefinitionMapping.create()
  else if name = 'StructureDefinition.snapshot' then
    result := TFhirStructureDefinitionSnapshot.create()
  else if name = 'StructureDefinition.differential' then
    result := TFhirStructureDefinitionDifferential.create()
  else if name = 'StructureDefinition' then
    result := TFhirStructureDefinition.create()
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_SUBSCRIPTION}
  else if name = 'Subscription.channel' then
    result := TFhirSubscriptionChannel.create()
  else if name = 'Subscription' then
    result := TFhirSubscription.create()
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSTANCE}
  else if name = 'Substance.instance' then
    result := TFhirSubstanceInstance.create()
  else if name = 'Substance.ingredient' then
    result := TFhirSubstanceIngredient.create()
  else if name = 'Substance' then
    result := TFhirSubstance.create()
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUPPLYDELIVERY}
  else if name = 'SupplyDelivery' then
    result := TFhirSupplyDelivery.create()
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  else if name = 'SupplyRequest.when' then
    result := TFhirSupplyRequestWhen.create()
  else if name = 'SupplyRequest' then
    result := TFhirSupplyRequest.create()
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TESTSCRIPT}
  else if name = 'TestScript.contact' then
    result := TFhirTestScriptContact.create()
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
  else if name = 'ValueSet.contact' then
    result := TFhirValueSetContact.create()
  else if name = 'ValueSet.codeSystem' then
    result := TFhirValueSetCodeSystem.create()
  else if name = 'ValueSet.codeSystem.concept' then
    result := TFhirValueSetCodeSystemConcept.create()
  else if name = 'ValueSet.codeSystem.concept.designation' then
    result := TFhirValueSetCodeSystemConceptDesignation.create()
  else if name = 'ValueSet.compose' then
    result := TFhirValueSetCompose.create()
  else if name = 'ValueSet.compose.include' then
    result := TFhirValueSetComposeInclude.create()
  else if name = 'ValueSet.compose.include.concept' then
    result := TFhirValueSetComposeIncludeConcept.create()
  else if name = 'ValueSet.compose.include.filter' then
    result := TFhirValueSetComposeIncludeFilter.create()
  else if name = 'ValueSet.expansion' then
    result := TFhirValueSetExpansion.create()
  else if name = 'ValueSet.expansion.parameter' then
    result := TFhirValueSetExpansionParameter.create()
  else if name = 'ValueSet.expansion.contains' then
    result := TFhirValueSetExpansionContains.create()
  else if name = 'ValueSet' then
    result := TFhirValueSet.create()
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  else if name = 'VisionPrescription.dispense' then
    result := TFhirVisionPrescriptionDispense.create()
  else if name = 'VisionPrescription' then
    result := TFhirVisionPrescription.create()
{$ENDIF FHIR_VISIONPRESCRIPTION}

  else if name = 'CapabilityStatement' then
    result := TFhirConformance.create()
  else
    result := nil;
end;



end.
