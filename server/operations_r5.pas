unit operations_r5;

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

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_json,
  fsl_http,
  fdb_manager,
  fhir_objects, fhir_factory, fhir_common,  fhir_xhtml, fhir_validator, fhir_parser, fhir_utilities,
  fhir5_enums, fhir5_types, fhir5_resources_base, fhir5_resources, fhir5_constants, fhir5_utilities, fhir5_opbase, fhir5_operations, fhir5_pathengine, fhir5_pathnode,
  fhir5_common, fhir5_questionnaire, fhir5_validator, fhir5_context, fhir5_profiles, fhir5_narrative, fhir5_graphdefinition, fhir5_maputils,
  fhir_codegen, fhir_diff,
  tx_operations, ftx_ucum_services,
  operations,
  session, tags, storage, database, obsservation_stats, search,
  bundlebuilder, validator_r5, security, subscriptions, server_context;

type
  TFhirNativeOperationEngineR5 = class (TFhirNativeOperationEngine)
  protected
    procedure registerOperations; override;
    procedure adjustReferences(request : TFHIRRequest; resp : TFHIRResponse; te : TFHIRTransactionEntry; base : String; entry : TFHIRBundleEntryW; ids : TFHIRTransactionEntryList); override;
    function PerformQuery(context: TFHIRObject; path: String): TFHIRObjectList; override;
    function readRef(ref : TFHIRObject) : string; override;
    function getOpException(op : TFHIRResourceV) : String; override;
    procedure doAuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode : Integer; name, message : String; patientId : String); override;
    procedure checkProposedContent(session : TFhirSession; request : TFHIRRequest; resource : TFHIRResourceV; tags : TFHIRTagList); override;
    procedure checkProposedDeletion(session : TFHIRSession; request : TFHIRRequest; resource : TFHIRResourceV; tags : TFHIRTagList); override;
  public
    Procedure CollectIncludes(session : TFhirSession; includes : TReferenceList; resource : TFHIRResourceV; path : String); override;
    function patientIds(request : TFHIRRequest; res : TFHIRResourceV) : TArray<String>; override;
  end;

  TFhirNativeOperationR5 = class (TFhirNativeOperation)
  protected
    function makeParamsV(request : TFHIRRequest) : TFHIRParameters;
    function vc(manager : TFHIROperationEngine) :TBaseWorkerContextR5; // native(manager).ServerContext.ValidatorContext
  end;

  TFhirGenerateQAOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function HandlesRequest(request : TFHIRRequest) : boolean; override;
  end;

  TFhirGenerateJWTOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function HandlesRequest(request : TFHIRRequest) : boolean; override;
  end;

  TFhirGraphFetchOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function formalURL : String; override;
  end;

  TFhirGenerateCodeOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function HandlesRequest(request : TFHIRRequest) : boolean; override;
  end;


  TFhirHandleQAPostOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
  end;

  TFhirQuestionnaireGenerationOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
  end;

  TFhirEverythingOperation = class (TFhirNativeOperationR5)
  protected
    function resourceName : String; virtual; abstract;
    function isPrimaryResource(request: TFHIRRequest; rtype, id : String) : boolean; virtual;
  public
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
  end;

  TFhirPatientEverythingOperation = class (TFhirEverythingOperation)
  protected
    FIsExport : boolean;
    function isWrite : boolean; override;
    function owningResource : String; override;
    function resourceName : String; override;
    function isPrimaryResource(request: TFHIRRequest; rtype, id : String) : boolean; override;
  public
    constructor Create(factory : TFhirFactory; isExport : boolean);
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function formalURL : String; override;
  end;

  TFhirEncounterEverythingOperation = class (TFhirEverythingOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
    function resourceName : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function formalURL : String; override;
  end;

  TFhirGroupEverythingOperation = class (TFhirEverythingOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
    function resourceName : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function formalURL : String; override;
  end;

  TFhirGenerateDocumentOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  private
    procedure addResource(manager: TFHIROperationEngine; secure : boolean; request : TFHIRRequest; bundle : TFHIRBundle; source : TFHIRResourceV; reference : TFhirReference; required : boolean; patIds : TPatientIdTracker);
    procedure addSections(manager: TFHIROperationEngine; secure : boolean; request : TFHIRRequest; bundle : TFHIRBundle; composition : TFhirComposition; sections : TFhirCompositionSectionList; patIds : TPatientIdTracker);
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
  end;

  TFhirValidationOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function formalURL : String; override;
  end;

  TFhirProcessClaimOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
  end;

  TFhirGenerateSnapshotOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
  end;

  TFhirGenerateTemplateOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
  end;

  TFhirGenerateNarrativeOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
  end;

  TFhirSuggestKeyWordsOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
  end;

  TFhirGetMetaDataOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function formalURL : String; override;
  end;

  TFhirAddMetaDataOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function formalURL : String; override;
  end;

  TFhirDeleteMetaDataOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function formalURL : String; override;
  end;

  TFhirDiffOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function formalURL : String; override;
  end;

  TFhirConvertOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function formalURL : String; override;
  end;

  TFhirTransformOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function formalURL : String; override;
  end;

  TFhirObservationStatsOperation = class (TFhirNativeOperationR5)
  private
    function resolveParameter(const lang : THTTPLanguages; code : String): TObservationStatsParameter;
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function formalURL : String; override;
  end;

  TFhirObservationLastNOperation = class (TFhirNativeOperationR5)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    function formalURL : String; override;
  end;

  TServerTransformerServices = class (TTransformerServices)
  private
    FServerContext : TFHIRServerContext;
  public
    constructor Create(ServerContext: TFHIRServerContext);
    destructor Destroy; override;
    function translate(appInfo : TFslObject; src : TFHIRCoding; conceptMapUrl : String) : TFHIRCoding; override;
    procedure log(s : String); override;
    function performSearch(appInfo : TFslObject; url : String) : TFslList<TFHIRObject>; override;
    function createType(appInfo : TFslObject; tn : String) : TFHIRObject; override;
    procedure createResource(appInfo : TFslObject; res : TFHIRObject; atRootofTransform : boolean); override;
  end;


  TFHIRNativeStorageServiceR5 = class (TFHIRNativeStorageService)
  protected
    function vc : TFHIRServerWorkerContextR5;
    procedure checkDefinitions; override;
  public
    procedure RegisterConsentRecord(session: TFhirSession); override;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;
    procedure checkProposedResource(session : TFhirSession; needsSecure, created : boolean; request : TFHIRRequest; res : TFHIRResourceV; tags : TFHIRTagList); override;
    procedure SeeResource(key, vkey, pvkey: integer; id: string; needsSecure, created : boolean; res : TFHIRResourceV; conn: TFDBConnection; reload: Boolean; session: TFhirSession; const lang : THTTPLanguages; src : TBytes); override;
    function createOperationContext(const lang : THTTPLanguages) : TFHIROperationEngine; override;
    Procedure Yield(op : TFHIROperationEngine; e : Exception); override;
    function engineFactory(const lang : THTTPLanguages; usage : String) : TFHIRNativeOperationEngine; override;
    Procedure SetUpRecording(session : TFhirSession); override;
    procedure RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception); override;
    procedure FinishRecording(); override;
  end;


implementation


{ TFhirNativeOperationEngineR5 }

procedure TFhirNativeOperationEngineR5.adjustReferences(request: TFHIRRequest; resp: TFHIRResponse; te: TFHIRTransactionEntry; base: String;entry: TFHIRBundleEntryW; ids: TFHIRTransactionEntryList);
var
  refs : TFhirReferenceList;
  ref : TFhirReference;
  url : String;
  vhist : String;
  i, j, k : integer;
  attachments : TFhirAttachmentList;
  attachment : TFhirAttachment;
  extension : TFhirExtension;
  cl : TFhirDocumentReferenceContent;
begin
  if entry.resource = nil then
    exit;
  refs := TFhirReferenceList.create;
  try
    listReferences(entry.resource as TFhirResource, refs);
    for i := 0 to refs.count - 1 do
    begin
      ref := refs[i];
      url := fullResourceUri(base, ref);

      if url.contains('?') then
        ref.reference := resolveConditionalURL(request, resp, ref.reference)
      else if (isHistoryURL(url)) then
        splitHistoryUrl(url, vhist)
      else
        vHist := '';

      j := ids.IndexByName(url);
      k := 0;
      while (j = -1) and (k < ServerContext.Globals.Bases.Count - 1) do
      begin
        j := ids.IndexByName(ServerContext.Globals.Bases[k]+ref.reference);
        inc(k);
      end;
      if (j <> -1) then
      begin
        if (vhist = '') then
          ref.reference := ids[j].resType+'/'+ids[j].id
        else if (ids[j].version <> '') and (ids[j].version <> vHist) then
          raise EFHIRException.create(StringFormat(GetFhirMessage('Version ID Mismatch for '+url+': reference to version '+vHist+', reference is '+ids[j].version, lang), [ref.reference]))
        else
          ref.reference :=  ids[j].resType+'/'+ids[j].id+'/_history/'+inttostr(ids[j].outcomeVersion);
      end;
    end;
  finally
    refs.free;
  end;
  attachments := TFhirAttachmentList.create;
  try
    ListAttachments(entry.resource as TFhirResource, attachments);
    for i := 0 to attachments.count - 1 do
    begin
      attachment := attachments[i];
      j := ids.IndexByName(attachment.url);
      if (j > -1) then
        attachment.url := ids[j].resType+'/'+ids[j].id
      else if isLogicalReference(attachment.url) then
        raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [attachment.url]));
    end;
  finally
    attachments.free;
  end;
  if (entry.resource is TFhirDomainResource) then
  begin
    for i := 0 to TFhirDomainResource(entry.resource).extensionList.count - 1 do
    begin
      extension := TFhirDomainResource(entry.resource).extensionList[i];
      j := ids.IndexByName(extension.url);
      if (j > -1) then
        extension.url := base+ids[j].resType+'/'+ids[j].id
      else if isLogicalReference(extension.url) then
        raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [extension.url]));
    end;
  end;
  // special case: XdsEntry
  if (entry.resource.fhirType = 'DocumentReference') then
    for cl in TFhirDocumentReference(entry.resource).contentList do
    begin
      attachment := cl.attachment;
      if (attachment <> nil) then
      begin
        j := ids.IndexByName(attachment.url);
        if (j > -1) then
          attachment.url := base+ids[j].resType+'/'+ids[j].id
        else if isLogicalReference(Attachment.url) then
          raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [attachment.url]));
      end;
    end;

  if entry.resource.isDomainResource and (TFhirDomainResource(entry.resource).text <> nil) then
    FixXhtmlUrls(lang, base, ids, TFhirDomainResource(entry.resource).text.div_);

end;

procedure TFhirNativeOperationEngineR5.checkProposedContent(session: TFhirSession; request: TFHIRRequest; resource: TFHIRResourceV; tags: TFHIRTagList);
//var
//  l, r : String;
begin
  if resource is TFhirSubscription then
  begin
//    if (TFhirSubscription(resource).status <> SubscriptionStatusRequested) and (request.origin = roRest) then // nil = from the internal system, which is allowed to
//      raise EFHIRException.create('Subscription status must be "requested", not '+TFhirSubscription(resource).statusElement.value);
//    if (TFhirSubscription(resource).channel = nil) then
//      raise EFHIRException.create('Subscription must have a channel');
//    if (TFhirSubscription(resource).channel.type_ = SubscriptionChannelTypeWebsocket) and not ((TFhirSubscription(resource).channel.payload = '') or StringArrayExistsSensitive(['application/xml+fhir', 'application/fhir+xml', 'application/xml', 'application/json+fhir', 'application/fhir+json', 'application/json'], TFhirSubscription(resource).channel.payload)) then
//      raise EFHIRException.create('A websocket subscription must have a no payload, or the payload must be application/xml+fhir or application/json+fhir');
//    if (TFhirSubscription(resource).status = SubscriptionStatusRequested) then
//      TFhirSubscription(resource).status := SubscriptionStatusActive; // well, it will be, or it will be rejected later
//    StringSplit(TFhirSubscription(resource).criteria, '?', l, r);
//    if (StringArrayIndexOfSensitive(CODES_TFhirResourceType, l) < 1) or (r = '') then
//      raise EFHIRException.create('Criteria is not valid');
  end;
  if (resource is TFHIROperationDefinition) then
  begin
    if resource.id.StartsWith('fso-') and (resource.tags['internal'] <> '1') then
      raise EFHIRException.create('operation Definitions that start with "fso-" are managed by the system directly, and cannot be changed through the REST API');
  end;
  if (resource is TFHIRStructureDefinition) then
  begin
//    if ServerContext.ValidatorContext.hasCustomResourceDefinition(TFHIRStructureDefinition(resource)) then
//      raise EFHIRException.create('Cannot update a structure definition that is in use as a custom resource');
  end;
end;

procedure TFhirNativeOperationEngineR5.checkProposedDeletion(session: TFHIRSession; request: TFHIRRequest; resource: TFHIRResourceV; tags: TFHIRTagList);
begin
  if (resource is TFHIROperationDefinition) then
  begin
    if resource.id.StartsWith('fso-') then
      raise EFHIRException.create('operation Definitions that start with "fso-" are managed by the system directly, and cannot be changed through the REST API');
  end;
  if (resource is TFHIRStructureDefinition) then
  begin
//    if ServerContext.ValidatorContext.hasCustomResourceDefinition(TFHIRStructureDefinition(resource)) then
//      raise EFHIRException.create('Cannot delete a structure definition that is in use as a custom resource');
  end;
end;

procedure TFhirNativeOperationEngineR5.CollectIncludes(session: TFhirSession; includes: TReferenceList; resource: TFHIRResourceV; path: String);
var
  s : String;
  matches : TFHIRObjectList;
  i : integer;
begin
  if resource = nil then
    exit;

  while path <> '' do
  begin
    StringSplit(path, ';', s, path);
    matches := PerformQuery(resource, s);
    try
      for i := 0 to matches.count - 1 do
        if (matches[i] is TFhirReference) and (TFhirReference(matches[i]).reference <> '') and (Session.canRead(typeForReference(TFhirReference(matches[i]).reference))) then
          includes.seeReference(TFhirReference(matches[i]).reference);
    finally
      matches.free;
    end;
  end;
end;


function TFhirNativeOperationEngineR5.getOpException(op: TFHIRResourceV): String;
begin
  result := TFHIROperationOutcome(op).asExceptionMessage
end;

function TFhirNativeOperationEngineR5.patientIds(request : TFHIRRequest; res: TFHIRResourceV): TArray<String>;
var
  ctxt : TFHIRServerWorkerContextR5;
  r : TFHIRResource;
  expression : TFHIRPathExpressionNode;
  path : TFHIRPathEngine;
  st : TStringList;
  matches : TFHIRSelectionList;
//  m : TFHIRSelection;
//  ref : String;
begin
  CreateIndexer;

  ctxt := ServerContext.ValidatorContext as TFHIRServerWorkerContextR5;
  r := res as TFhirResource;
  if r = nil then
    expression := nil
  else
    expression := ctxt.PatientIdExpression(r.ResourceType);
  if expression <> nil then
  begin
    path := Indexer.Engine as TFHIRPathEngine;
    matches := path.evaluate(request, r, r, expression);
    st := TStringList.Create;
    try
//      for m in matches do
//      begin
//        if m.value is TFhirReference then
//          ref := (m.value as TFhirReference).reference
//        else
//          raise Exception.Create('Unexpected type');
//        if ref.StartsWith('Patient/') then
//          st.Add(ref.Substring(9));
//      end;
      result := st.ToStringArray;
    finally
      st.Free;
      matches.free;
    end;
  end
  else
    result := [];
end;

function TFhirNativeOperationEngineR5.PerformQuery(context: TFHIRObject; path: String): TFHIRObjectList;
var
  qry : TFHIRPathEngine;
  list : TFHIRSelectionList;
begin
  qry := TFHIRPathEngine.create(nil, nil);
  try
    list := qry.evaluate(nil, context, path);
    try
      result := list.asValues;
    finally
      list.Free;
    end;
  finally
    qry.free;
  end;
end;

function TFhirNativeOperationEngineR5.readRef(ref: TFHIRObject): string;
begin
  result := TFhirReference(ref).reference;
end;

procedure TFhirNativeOperationEngineR5.registerOperations;
begin
  FOperations.add(TFhirExpandValueSetOperation.create(Factory.link, ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirLookupCodeSystemOperation.create(Factory.link, ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirValueSetValidationOperation.create(Factory.link, ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirConceptMapTranslationOperation.create(Factory.link, ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirConceptMapClosureOperation.create(Factory.link, ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirValidationOperation.create(Factory.link));
  FOperations.add(TFhirGenerateDocumentOperation.create(Factory.link));
  FOperations.add(TFhirPatientEverythingOperation.create(Factory.link, true));
  FOperations.add(TFhirPatientEverythingOperation.create(Factory.link, false));
  FOperations.add(TFhirEncounterEverythingOperation.create(Factory.link));
  FOperations.add(TFhirGroupEverythingOperation.create(Factory.link));
  FOperations.add(TFhirGenerateQAOperation.create(Factory.link));
  FOperations.add(TFhirGenerateJWTOperation.create(Factory.link));
  FOperations.add(TFhirGenerateCodeOperation.create(Factory.link));
  FOperations.add(TFhirHandleQAPostOperation.create(Factory.link));
  FOperations.add(TFhirQuestionnaireGenerationOperation.create(Factory.link));
  FOperations.add(TFhirVersionsOperation.create(Factory.link));
  FOperations.add(TFhirProcessClaimOperation.create(Factory.link));
  FOperations.add(TFhirGenerateSnapshotOperation.create(Factory.link));
  FOperations.add(TFhirGenerateTemplateOperation.create(Factory.link));
  FOperations.add(TFhirGenerateNarrativeOperation.create(Factory.link));
  FOperations.add(TFhirSuggestKeyWordsOperation.create(Factory.link));
  FOperations.add(TFhirGetMetaDataOperation.create(Factory.link));
  FOperations.add(TFhirAddMetaDataOperation.create(Factory.link));
  FOperations.add(TFhirDeleteMetaDataOperation.create(Factory.link));
  FOperations.add(TFhirDiffOperation.create(Factory.link));
  FOperations.add(TFhirConvertOperation.create(Factory.link));
  FOperations.add(TFhirTransformOperation.create(Factory.link));
  FOperations.add(TFhirSubsumesOperation.create(Factory.link, ServerContext.TerminologyServer.Link));
//    FOperations.add(TFhirCodeSystemComposeOperation.create(Factory.link, ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirObservationStatsOperation.create(Factory.link));
  FOperations.add(TFhirObservationLastNOperation.create(Factory.link));
end;

procedure TFhirNativeOperationEngineR5.doAuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirProvenanceW; opName: String; httpCode: Integer; name, message: String; patientId : String);
var
  se : TFhirAuditEvent;
  c : TFhirCoding;
  p : TFhirAuditEventParticipant;
  o : TFhirAuditEventObject;
  procedure event(t, ts, td, s, sc : String; a : TFhirAuditEventActionEnum);
  begin
    se.event.type_ := TFhirCoding.create;
    c := se.event.type_;
    c.code := t;
    c.system := ts;
    c.display := td;
    c := se.event.subtypeList.append;
    c.code := s;
    c.system := sc;
    c.display := s;
    se.event.action := a;
  end;
begin
  if not ServerContext.DoAudit then
    exit;
  se := TFhirAuditEvent.create;
  try
    if verkey <> 0 then
      se.Tags['verkey'] := inttostr(verkey);
    if intreqid = '' then
      raise EFHIRException.create('Unidentified request');
    se.id := intreqid;

    if extreqid <> '' then
      with se.entityList.Append do
      begin
        type_ := TFhirCoding.Create('', 'X-Request-Id');
        what := TFhirReference.Create;
        what.identifier := TFhirIdentifier.Create(extreqid);
      end;

    se.event := TFhirAuditEventEvent.create;
    case op of
      fcmdRead:            event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'read',    'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdVersionRead:     event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'vread',   'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdUpdate:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'update',  'http://hl7.org/fhir/restful-operation', AuditEventActionU);
      fcmdDelete:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'delete',  'http://hl7.org/fhir/restful-operation', AuditEventActionD);
      fcmdHistoryInstance: event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'history-instance', 'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdCreate:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'create',  'http://hl7.org/fhir/restful-operation', AuditEventActionC);
      fcmdSearch:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'search',  'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdHistoryType:     event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'history-type', 'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdValidate:        event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'validate', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdMetadata:        event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'conformance',    'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdTransaction:     event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'transaction', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdBatch:           event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'batch', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdPatch:           event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'patch', 'http://hl7.org/fhir/restful-operation', AuditEventActionU);
      fcmdHistorySystem:   event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'history-system', 'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdUpload:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'upload', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdOperation:       event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'operation', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
    else // fcmdUnknown
      raise EFHIRException.create('unknown operation');
    end;
    if op = fcmdOperation then
    begin
      c := se.event.subtypeList.Append;
      c.code := opName;
      c.system := 'http://healthintersections.com.au/fhir/operation-name';
    end;
    if httpCode < 400 then
      se.event.outcome := TFhirCodeableConcept.Create('http://terminology.hl7.org/CodeSystem/audit-event-outcome', '0')
    else if httpCode < 500 then
      se.event.outcome := TFhirCodeableConcept.Create('http://terminology.hl7.org/CodeSystem/audit-event-outcome', '4')
    else
      se.event.outcome := TFhirCodeableConcept.Create('http://terminology.hl7.org/CodeSystem/audit-event-outcome', '8'); // no way we are going down...
    se.event.dateTime := TFslDateTime.makeUTC;
    se.Tag := TFslDateTimeWrapper.Create(se.event.dateTime);

    se.source := TFhirAuditEventSource.create;
    se.source.site := ServerContext.Globals.OwnerName;
    se.source.observer := TFhirReference.Create;
    se.source.observer.identifier := TFhirIdentifier.Create;
    se.source.observer.identifier.value := ServerContext.DatabaseId;
    se.source.observer.identifier.system := 'urn:ietf:rfc:4986';

    c := se.source.type_List.Append;
    c.code := '4';
    c.display := 'Web Server';
    c.system := 'http://hl7.org/fhir/security-source-type';

    // participant - the web browser / user proxy
    p := se.participantList.Append;
    if session = nil then
      p.name := 'Server'
    else
    begin
      p.who := TFhirReference.Create;
      p.who.identifier := TFhirIdentifier.Create;
      p.who.identifier.value := inttostr(session.Key);
      p.who.identifier.system := ServerContext.DatabaseId;
      p.altId := session.Id;
      p.name := session.SessionName;
    end;
    p.requestor := true;
    p.network := TFhirAuditEventParticipantNetwork.create;
    p.network.address := ip;
    p.network.type_ := AuditEventAgentNetworkType2;

    if (patientId <> '') then
      with se.entityList.Append do
      begin
        what := TFhirReference.Create('Patient/'+patientId);
        type_ := TFhirCoding.Create('http://terminology.hl7.org/CodeSystem/audit-entity-type', '1');
        role := TFhirCoding.Create('http://terminology.hl7.org/CodeSystem/object-role', '1');
      end;

    if resourceName <> '' then
    begin
      o := se.object_List.Append;
      o.what := TFhirReference.create;
      if ver <> '' then
        o.what.reference := resourceName+'/'+id+'/_history/'+ver
      else if id <> '' then
        o.what.reference := resourceName+'/'+id;
      o.type_ := TFhirCoding.Create;
      o.type_.system := 'http://hl7.org/fhir/security-source-type';
      o.type_.code := '2';
      o.lifecycle := TFhirCoding.Create;
      o.lifecycle.system := 'http://hl7.org/fhir/object-lifecycle';
      case op of
        fcmdRead:            o.lifecycle.code := '6';
        fcmdVersionRead:     o.lifecycle.code := '6';
        fcmdUpdate:          o.lifecycle.code := '4';
        fcmdDelete:          o.lifecycle.code := '14';
        fcmdHistoryInstance: o.lifecycle.code := '9';
        fcmdCreate:          o.lifecycle.code := '1';
        fcmdSearch:          o.lifecycle.code := '6';
        fcmdHistoryType:     o.lifecycle.code := '9';
        fcmdValidate:        o.lifecycle.code := '4';
        fcmdMetadata:        o.lifecycle.code := '6';
        fcmdTransaction:     o.lifecycle.code := '4';
        fcmdHistorySystem:   o.lifecycle.code := '9';
        fcmdUpload:          o.lifecycle.code := '9';
      end;
      if op = fcmdSearch then
        o.query := StringAsBytes(name)
      else
        o.name := name;
    end;
    Repository.queueResource(session, se);
  finally
    se.Free;
  end;
end;

{ TFhirNativeOperationR5 }

function TFhirNativeOperationR5.makeParamsV(request: TFHIRRequest): TFHIRParameters;
var
  i : integer;
begin
  if (request.Resource <> nil) and (request.Resource.fhirType = 'Parameters') then
    result := request.Resource.Link as TFHIRParameters
  else
    result := TFhirParameters.create;
  try
    for i := 0 to request.Parameters.Count - 1 do
      result.AddParameter(request.Parameters.Name[i], TFhirString.Create(request.Parameters[request.Parameters.Name[i]]));
    result.link;
  finally
    result.Free;
  end;
end;

function TFhirNativeOperationR5.vc(manager : TFHIROperationEngine): TBaseWorkerContextR5;
begin
  result := native(manager).ServerContext.ValidatorContext as TBaseWorkerContextR5;
end;

{ TFhirGenerateQAOperation }

function TFhirGenerateQAOperation.Name: String;
begin
  result := 'qa-edit';
end;

function TFhirGenerateQAOperation.owningResource: String;
begin
  result := '';
end;

function TFhirGenerateQAOperation.Types: TArray<String>;
begin
  result := FFactory.ResourceNames; { - resource names}
end;

function TFhirGenerateQAOperation.HandlesRequest(request: TFHIRRequest): boolean;
begin
  result := inherited HandlesRequest(request) and (request.id <> '') and (request.SubId = '');
end;

function TFhirGenerateQAOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateQAOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirGenerateQAOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
begin
  result := '??';
end;

{ TFhirJWTOperation }

function TFhirGenerateJWTOperation.Name: String;
begin
  result := 'jwt';
end;

function TFhirGenerateJWTOperation.owningResource: String;
begin
  result := '';
end;

function TFhirGenerateJWTOperation.Types: TArray<String>;
begin
  result := [];
end;

function TFhirGenerateJWTOperation.HandlesRequest(request: TFHIRRequest): boolean;
begin
  result := inherited HandlesRequest(request); // and (request.secure);
end;

function TFhirGenerateJWTOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateJWTOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirGenerateJWTOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  jwt : String;
  pIn : TFhirParameters;
  pOut : TFhirParameters;
begin
  result := '??';
  try
    pIn := makeParamsV(request);
    try
      if pIn.hasParameter('for') then
      begin
        jwt := native(manager).ServerContext.JWTServices.makeJWT(pIn.str['for']);
      end
      else
      begin
        if not pIn.hasParameter('source') then
          raise EFHIRException.createLang('JWT_NO_SOURCE', request.lang);
        jwt := native(manager).ServerContext.JWTServices.makeJWT;
      end;
      response.HTTPCode := 200;
      response.Message := 'OK';
      if request.PostFormat <> ffUnspecified then
      begin
        pOut := TFhirParameters.Create;
        response.Resource := pOut;
        pOut.AddParameter('jwt', jwt);
      end
      else
      begin
        response.ContentType := 'application/jwt';
        response.Body := jwt;
      end;
    finally
      pIn.Free;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;


{ TFhirGenerateCodeOperation }

function TFhirGenerateCodeOperation.Name: String;
begin
  result := 'codegen';
end;

function TFhirGenerateCodeOperation.owningResource: String;
begin
  result := '';
end;

function TFhirGenerateCodeOperation.Types: TArray<String>;
begin
  result := FFactory.ResourceNames;
end;

function TFhirGenerateCodeOperation.HandlesRequest(request: TFHIRRequest): boolean;
begin
  result := inherited HandlesRequest(request);
end;

function TFhirGenerateCodeOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateCodeOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirGenerateCodeOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  res : TFHIRResourceV;
  codegen : TFHIRCodeGenerator;
  code, genlang : string;
  resourceKey, versionKey : integer;
  needSecure : boolean;
  params : TFhirParameters;
  oo : TFHIROperationOutcome;
  issue : TFhirOperationOutcomeIssue;
begin
  result := '??';
  try
    manager.NotFound(request, response);
    if native(manager).check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        params := makeParamsV(request);
        try
          if request.Id <> '' then // and it must exist, because of the check above
            res := native(manager).GetResourceById(request, request.ResourceName, request.Id, request.baseUrl, needSecure)
          else if (request.Resource <> nil) and (request.Resource is TFHIRValueSet) then
            res := request.Resource.Link
          else if params.hasParameter('resource') then
            res := params.res['resource'].Link
          else
            raise EFHIRException.createLang('OP_NO_RESOURCE', request.lang, ['ValueSet']);
          try
            genlang := params.str['language'];
            codegen := makeCodeGenerator(genlang);
            try
              codegen.Resource := res.Link;
              codegen.Context := vc(manager).Link;
              code := codegen.generate;
            finally
              codegen.free;
            end;
          finally
            res.Free;
          end;
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.Body := '';
          response.LastModifiedDate := now;
          oo := TFHIROperationOutcome.create;
          response.Resource := oo;
          oo.text := TFhirNarrative.Create;
          code := '<div><pre>'+FormatCodeToXML(code)+'</pre></div>';
          oo.text.div_ := TFHIRXhtmlParser.parse(THTTPLanguages.create('en'), xppReject, [], code);
          issue := oo.issueList.Append;
          issue.severity := IssueSeverityInformation;
          issue.code := IssueTypeInformational;
        finally
          params.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

{ TFhirHandleQAPostOperation }

function TFhirHandleQAPostOperation.Name: String;
begin
  result := 'qa-post';
end;

function TFhirHandleQAPostOperation.owningResource: String;
begin
  result := '';
end;

function TFhirHandleQAPostOperation.Types: TArray<String>;
begin
  result := FFactory.ResourceNames; // - [frtStructureDefinition];
end;

function TFhirHandleQAPostOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirHandleQAPostOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
begin
  result := '??';
end;

function TFhirHandleQAPostOperation.isWrite: boolean;
begin
  result := true;
end;

{ TFhirQuestionnaireGenerationOperation }

function TFhirQuestionnaireGenerationOperation.Name: String;
begin
  result := 'questionnaire';
end;

function TFhirQuestionnaireGenerationOperation.owningResource: String;
begin
  result := 'StructureDefinition';
end;

function TFhirQuestionnaireGenerationOperation.Types: TArray<String>;
begin
  result := ['StructureDefinition'];
end;

function TFhirQuestionnaireGenerationOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirQuestionnaireGenerationOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  profile : TFHirStructureDefinition;
  op : TFHIROperationOutcomeW;
  resourceKey, versionKey : integer;
  id, fid : String;
  builder : TQuestionnaireBuilder;
  questionnaire : TFHIRQuestionnaireW;
  q : TFHIRQuestionnaire;
  needSecure : boolean;
  ctxt : TFHIRValidatorContext;
begin
  result := '??';
  try
    manager.NotFound(request, response);
    if manager.check(response, request.Session.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, request.CommandType), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource('StructureDefinition', request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        profile := nil;
        try
          // first, we have to identify the structure definition
          id := request.Id;
          if request.Id <> '' then // and it must exist, because of the check above
            profile := native(manager).GetResourceById(request, 'StructureDefinition', request.Id, request.baseUrl, needSecure) as TFhirStructureDefinition
          else if request.Parameters.has('identifier') then
            profile := native(manager).GetResourceByURL('StructureDefinition', request.Parameters['identifier'], '', false, needSecure) as TFhirStructureDefinition
          else if (request.form <> nil) and request.form.hasParam('profile') then
            profile := LoadFromFormParam(request.Context as TFHIRWorkerContext, request.form.getparam('profile'), request.Lang) as TFHirStructureDefinition
          else if (request.Resource <> nil) and (request.Resource is TFHirStructureDefinition) then
            profile := request.Resource.Link as TFHirStructureDefinition
          else
            raise EFHIRException.createLang('OP_NO_RESOURCE', request.lang, ['Profile']);

          profile.checkNoImplicitRules('QuestionnaireGeneration', 'profile');
          profile.checkNoModifiers('QuestionnaireGeneration', 'profile');

          if id <> '' then
          begin
            fid := request.baseUrl+'StructureDefinition/'+id+'/$questionnaire';
            questionnaire := native(manager).ServerContext.QuestionnaireCache.getQuestionnaire('StructureDefinition', id);
            q := questionnaire.Resource as TFhirQuestionnaire;
            q.checkNoImplicitRules('QuestionnaireGeneration', 'questionnaire');
            q.checkNoModifiers('QuestionnaireGeneration', 'questionnaire');
          end
          else
          begin
            fid := newGUIDUrn;
            questionnaire := nil;
          end;

          try
            if questionnaire = nil then
            begin
              builder := TQuestionnaireBuilder.Create(request.Lang);
              try
                builder.Profile := profile.link;
                builder.OnExpand := native(manager).Repository.ExpandVS;
                builder.onLookupCode := native(manager).Repository.LookupCode;
                builder.Context := request.Link;
                builder.onLookupReference := native(manager).LookupReferenceS;
                builder.QuestionnaireId := fid;
// todo...                builder.Profiles := vc(manager).Profiles.Link;
                builder.build;
// todo...                questionnaire := FFactory.wrap builder.questionnaire.Link;
                if id <> '' then
                  native(manager).ServerContext.QuestionnaireCache.putQuestionnaire('StructureDefinition', id, questionnaire, builder.dependencies);
              finally
                builder.Free;
              end;
            end;
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.Resource := questionnaire.Resource.Link;
          finally
            questionnaire.Free;
          end;
        finally
          profile.free;
        end;
        ctxt := TFHIRValidatorContext.Create;
        try
          ctxt.ResourceIdRule := risOptional;
          ctxt.IsAnyExtensionsAllowed := true;
          ctxt.OperationDescription := 'Produce Questionnaire';
          native(manager).ServerContext.Validator.validate(ctxt, response.Resource);
          op := native(manager).ServerContext.Validator.describe(ctxt);
        finally
          ctxt.Free;
        end;
        try
          if (op.hasErrors) then
          begin
            response.HTTPCode := 500;
            response.Message := 'Questionnaire Generation Failed';
            response.Resource.Id := 'src';
            (op.Resource as TFhirDomainResource).containedList.Add(response.Resource.Link);
            response.Resource := op.Resource.link;
          end;
        finally
          op.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirQuestionnaireGenerationOperation.isWrite: boolean;
begin
  result := false;
end;


{ TFhirValidationOperation }

function TFhirValidationOperation.Name: String;
begin
  result := 'validate';
end;

function TFhirValidationOperation.owningResource: String;
begin
  result := '';
end;

function TFhirValidationOperation.Types: TArray<String>;
begin
  result := FFactory.ResourceNames;
end;

function TFhirValidationOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirValidationOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;

type TValidationOperationMode = (vomGeneral, vomCreate, vomUpdate, vomDelete);
var
  outcome : TFHIROperationOutcomeW;
  profileId : String;
  profile : TFHirStructureDefinition;
  profiles : TValidationProfileSet;
  opDesc : string;
  res : boolean;
  needSecure : boolean;
  ctxt : TFHIRValidatorContext;
  function getParam(name : String) : String;
  var
    params : TFhirParameters;
  begin
    if request.Resource is TFhirParameters then
    begin
      params := request.Resource as TFhirParameters;
      if params.hasParameter(name) then
      begin
        result := (params.NamedParameter[name] as TFHIRPrimitiveType).StringValue;
        exit;
      end;
    end;
    result := request.Parameters[name];
  end;
begin
  result := '??';
  profileId := '';
  profile := nil;
  try
    profileId := getParam('profile');
    // reject mode - we don't know what to do with it
    if getParam('mode') <> '' then
      raise EFHIRException.create('Mode parameter is not (yet) supported');

    if StringStartsWith(ProfileId, 'http://localhost/StructureDefinition/') then
      profile := native(manager).GetResourceById(request, 'StructureDefinition', copy(ProfileId, 27, $FF), request.baseUrl, needSecure) as TFhirStructureDefinition
    else if StringStartsWith(ProfileId, 'StructureDefinition/') then
      profile := native(manager).GetResourceById(request, 'StructureDefinition', copy(ProfileId, 9, $FF), request.baseUrl, needSecure) as TFhirStructureDefinition
    else if StringStartsWith(ProfileId, request.baseUrl+'StructureDefinition/') then
      profile := native(manager).GetResourceById(request, 'StructureDefinition', copy(ProfileId, length(request.baseUrl)+9, $FF), request.baseUrl, needSecure) as TFhirStructureDefinition
    else if (profileId <> '') then
      profile := native(manager).GetResourceByURL('StructureDefinition', profileId, '', false, needSecure) as TFhirStructureDefinition;

    if Profile <> nil then
      opDesc := 'Validate resource '+request.id+' against profile '+profileId
    else if (profileId <> '') then
      raise EFHIRException.createLang('MSG_NO_MATCH', request.lang, [profileId])
    else
      opDesc := 'Validate resource '+request.id;

    ctxt := TFHIRValidatorContext.Create;
    try
      ctxt.ResourceIdRule := risOptional;
      ctxt.IsAnyExtensionsAllowed := true;
      ctxt.OperationDescription := opDesc;
      if (request.Source <> nil) and not (request.Resource is TFhirParameters) then
      begin
        profiles := TValidationProfileSet.create(profile);
        try
          (native(manager).ServerContext.Validator as TFHIRValidatoR5).validate(ctxt, request.Source, request.PostFormat, profiles)
        finally
          profiles.Free;
        end;
      end
      else
      begin
        if request.resource = nil then
          request.resource := native(manager).GetResourceById(request, request.ResourceName, request.Id, '', needSecure);
        profiles := TValidationProfileSet.create(profile);
        try
          (native(manager).ServerContext.Validator as TFHIRValidatoR5).validate(ctxt, request.Resource, profiles);
        finally
          profiles.Free;
        end;
      end;
      outcome := native(manager).ServerContext.Validator.describe(ctxt);
    finally
      ctxt.Free;
    end;

    // todo: check version id integrity
    // todo: check version integrity

    response.Resource := outcome.Resource.link;
    res := not outcome.hasErrors;
    if res then
      response.HTTPCode := 200
    else
      response.HTTPCode := 400;
    if request.ResourceName <> 'AuditEvent' then
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      if request.ResourceName <> 'AuditEvent' then
        manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;


function TFhirValidationOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/ValueSet-validate-code';
end;

function TFhirValidationOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirEverythingOperation }

function TFhirEverythingOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  bundle : TFHIRBundleBuilder;
  entry : TFHIRBundleEntryW;
  includes : TReferenceList;
  id, link, base, sql, field : String;
  total : Integer;
  rkey, versionKey : integer;
  reverse : boolean;
  wantsummary : TFHIRSummaryOption;
  title: string;
  keys : TKeyList;
  params : THTTPParameters;
  prsrFmt : TFhirFormat;
  needsObject : boolean;
  sId, type_ : String;
  first : boolean;
  conn : TFDBConnection;
  patIds : TPatientIdTracker;
begin
  result := '??';
  patIds := TPatientIdTracker.Create;
  try
  try
    // first, we have to convert from the patient id to a compartment id
    if (id = '') or manager.FindResource(resourceName, request.Id, [], rkey, versionKey, request, response, nil) then
    begin
      if id = '' then
        request.compartment := TFHIRCompartmentId.Create(request.ResourceName, '*')
      else
        begin
        request.compartment := TFHIRCompartmentId.Create(request.ResourceName, request.Id);
          if request.ResourceName = 'Patient' then
            patIds.seeIds([request.Id]);
        end;
      response.OnCreateBuilder(request, response, btCollection, bundle);
      includes := TReferenceList.create;
      keys := TKeyList.Create;
      params := THTTPParameters.Create('');
      try
        if native(manager).FindSavedSearch(request.parameters.value[SEARCH_PARAM_NAME_ID], request.Session, 1, id, link, sql, title, base, total, wantSummary, request.strictSearch, reverse) then
          link := SEARCH_PARAM_NAME_ID+'='+request.parameters.value[SEARCH_PARAM_NAME_ID]
        else
          id := native(manager).BuildSearchResultSet(0, request.Session, request.resourceName, params, request.baseUrl, request.compartment, request.SessionCompartments, nil, link, sql, total, wantSummary, request.strictSearch, reverse);
        bundle.setTotal(total);
        bundle.tag('sql', sql);
        bundle.addLink('self', 'todo');

        native(manager).chooseField(response.Format, wantsummary, request.loadObjects, field, prsrFmt, needsObject);
        if (not needsObject) then
          prsrFmt := ffUnspecified;

        conn := native(manager).Connection;
        conn.SQL := 'Select Ids.ResourceKey, Types.ResourceName, Ids.Id, VersionId, Secure, StatedDate, Name, Versions.Status, Tags, '+field+' from Versions, Ids, Sessions, SearchEntries, Types '+
            'where Ids.Deleted = 0 and SearchEntries.ResourceVersionKey = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey '+'and SearchEntries.ResourceKey = Ids.ResourceKey and Types.ResourceTypeKey = Ids.ResourceTypeKey and SearchEntries.SearchKey = '+id+' '+
            'order by SortValue ASC';
        conn.Prepare;
        try
          conn.Execute;
          while conn.FetchNext do
          Begin
            sId := conn.ColStringByName['Id'];
            type_ := conn.colStringByName['ResourceName'];
            first := isPrimaryResource(request, type_, sId);

              entry := native(manager).AddResourceTobundle(request, bundle, request.secure, request.baseUrl, field, prsrFmt, smUnknown, false, request.parameters.has('_summary'), type_, patIds, first);
            keys.Add(TKeyPair.create(type_, conn.ColStringByName['ResourceKey']));

            if request.Parameters.has('_include') then
              native(manager).CollectIncludes(request.session, includes, entry.resource, request.Parameters['_include']);
          End;
        finally
          conn.Terminate;
        end;

        // process reverse includes
//          if request.Parameters.has('_reverseInclude') then
//            native(manager).CollectReverseIncludes(request.Session, includes, keys, request.Parameters['_reverseInclude'), bundle, request, response, wantsummary);

//          //now, add the includes
//          if includes.Count > 0 then
//          begin
//            native(manager).Connection.SQL := 'Select ResourceTypeKey, Ids.Id, VersionId, Secure, StatedDate, Name, Versions.Status, Tags, '+field+' from Versions, Sessions, Ids '+
//                'where Ids.Deleted = 0 and Ids.MostRecent = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey '+
//                'and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and '+includes.asSql+') order by ResourceVersionKey DESC';
//            native(manager).Connection.Prepare;
//            try
//              native(manager).Connection.Execute;
//              while native(manager).Connection.FetchNext do
//                native(manager).AddResourceTobundle(bundle, baseUrlrequest.request.request., field, prsr);
//            finally
//              native(manager).Connection.Terminate;
//            end;
//          end;

        bundle.setLastUpdated(TFslDateTime.makeUTC);
        bundle.setId(NewGuidId);
        response.HTTPCode := 200;
        response.Message := 'OK';
        response.Body := '';
        response.resource := bundle.getBundle;
        if (request.Parameters['email'] <> '') then
          native(manager).ServerContext.SubscriptionManager.sendByEmail(response.Resource, request.Parameters['email'], false)
        else if (request.Parameters['direct'] <> '') then
          native(manager).ServerContext.SubscriptionManager.sendByEmail(response.Resource, request.Parameters['direct'], true);
      finally
        params.free;
        includes.free;
        keys.Free;
        bundle.Free;
      end;
    end;
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message, patIds.ids);
  except
    on e: exception do
    begin
        manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message, patIds.ids);
      recordStack(e);
      raise;
    end;
  end;
  finally
    patIds.Free;
  end;
end;


function TFhirEverythingOperation.isPrimaryResource(request: TFHIRRequest; rtype, id: String): boolean;
begin
  result := false;
end;


{ TFhirProcessClaimOperation }

function TFhirProcessClaimOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirProcessClaimOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  resourceKey, versionKey : integer;
  params : TFhirParametersW;
  claim : TFhirClaim;
  resp : TFhirClaimResponse;
  needSecure : boolean;
begin
  result := '??';
  claim := nil;
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        params := makeParams(request);
        claim := nil;
        try
          // first, we have to identify the value set.
          if request.Id <> '' then // and it must exist, because of the check above
            claim := native(manager).GetResourceById(request, 'Claim', request.Id, request.baseUrl, needSecure) as TFhirClaim
//          else if request.Parameters.has('identifier') then
//          begin
//            url := request.Parameters['identifier'];
//            if (url.startsWith('ValueSet/')) then
//              vs := native(manager).GetValueSetById(request, url.substring(9), baseUrlrequest.request.request.request.request.)
//            else if (url.startsWith(baseURLrequest.request.request.request.request.request.+'ValueSet/')) then
//              vs := native(manager).GetValueSetById(request, url.substring(9), baseUrlrequest.request.request.request.request.)
//            else if not native(manager).FRepository.TerminologyServer.isKnownValueSet(url, vs) then
//              vs := native(manager).GetValueSetByIdentity(request.Parameters['identifier'], request.Parameters.getvar('version'));
//            cacheId := vs.url;
//          end
          else if (request.form <> nil) and request.form.hasParam('claim') then
            claim := LoadFromFormParam(request.Context as TFHIRWorkerContext, request.form.getparam('valueSet'), request.Lang) as TFhirClaim
          else if (request.Resource <> nil) and (request.Resource is TFHIRClaim) then
            claim := request.Resource.Link as TFHIRClaim
          else
            raise EFHIRException.createLang('OP_NO_RESOURCE', request.lang, ['Claim']);

          claim.checkNoImplicitRules('ProcessClaim', 'claim');
          claim.checkNoModifiers('ProcessClaim', 'claim');
          resp := nil;
// todo          resp := native(manager).Repository.GenerateClaimResponse(claim);
          try
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.Resource := resp.Link;
            // response.categories.... no tags to go on this resource
          finally
            resp.free;
          end;
        finally
          claim.free;
          params.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, manager.patientIds(request, claim));
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, manager.patientIds(request, claim));
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirProcessClaimOperation.isWrite: boolean;
begin
  result := true;
end;

function TFhirProcessClaimOperation.Name: String;
begin
  result := 'process';
end;

function TFhirProcessClaimOperation.owningResource: String;
begin
  result := 'Claim';
end;

function TFhirProcessClaimOperation.Types: TArray<String>;
begin
  result := ['Claim'];
end;

{ TFhirGenerateSnapshotOperation }

function TFhirGenerateSnapshotOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirGenerateSnapshotOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  params : TFhirParameters;
  sdParam, sdBase : TFhirStructureDefinition;
  utils : TProfileUtilities;
  op : TFHIROperationOutcome;
begin
  result := '??';
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      sdParam := nil;
      sdBase := nil;
      params := makeParamsV(request);
      try
        if params.hasParameter('profile') then
          sdParam := params['profile'] as TFhirStructureDefinition
        else if (request.Resource <> nil) and (request.Resource is TFhirStructureDefinition) then
          sdParam := request.Resource.Link as TFhirStructureDefinition
        else
          raise EFHIRException.createLang('OP_NO_RESOURCE', request.Lang, ['Profile']);

        sdParam.checkNoImplicitRules('GenerateSnapshot', 'profile');
        sdParam.checkNoModifiers('GenerateSnapshot', 'profile');
        if sdParam.baseDefinition <> '' then
        begin
          if not vc(manager).Profiles.getProfileStructure(nil, sdParam.baseDefinition, sdBase) then
          raise EFHIRException.createLang('MSG_NO_MATCH', request.Lang, ['base profile "'+sdParam.baseDefinition+'"']);
        end
        else if params.hasParameter('base') then
        begin
          if not vc(manager).Profiles.getProfileStructure(nil, params.str['base'], sdBase) then
          raise EFHIRException.createLang('MSG_NO_MATCH', request.Lang, ['base profile "'+params.str['base']+'"']);
        end
        else
        begin
          if not vc(manager).Profiles.getProfileStructure(nil, sdBase.baseDefinition, sdBase) then
           raise EFHIRException.createLang('MSG_NO_MATCH', request.Lang, ['Implicit base definition "'+sdBase.baseDefinition+'"']);
        end;

        op := TFHIROperationOutcome.Create;
        utils := TProfileUtilities.create(vc(manager).link, op.issueList.Link);
        try
          try
            raise EFslException.Create('not implemented yet');
//            utils.generateSnapshot(sdBase, sdParam, sdParam.url, sdParam.name);
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.Resource := sdParam.Link;
          except
            on e : exception do
            begin
              op.text := TFhirNarrative.Create;
              op.text.status := NarrativeStatusGenerated;
              op.text.div_ := TFhirXHtmlNode.Create('div');
              op.text.div_.AddText(e.Message);
              response.HTTPCode := 400;
              response.Message := 'Bad Request';
              response.Body := '';
              response.LastModifiedDate := now;
              response.Resource := op.Link;
            end;
          end;
        finally
          op.Free;
          utils.Free;
        end;
      finally
        sdBase.Free;
        sdParam.free;
        params.Free;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirGenerateSnapshotOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateSnapshotOperation.Name: String;
begin
  result := 'snapshot';
end;

function TFhirGenerateSnapshotOperation.owningResource: String;
begin
  result := 'StructureDefinition';
end;

function TFhirGenerateSnapshotOperation.Types: TArray<String>;
begin
  result := ['StructureDefinition'];
end;

{ TFhirGenerateTemplateOperation }

function TFhirGenerateTemplateOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirGenerateTemplateOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  profile : TFHirStructureDefinition;
  resourceKey, versionKey : integer;
  id : String;
  builder : TProfileUtilities;
  template : TFHIRResourceV;
  narr : TFHIRNarrativeGenerator;
  needSecure : boolean;
begin
  result := '??';
  try
    manager.NotFound(request, response);
    if manager.check(response, request.Session.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, request.CommandType), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource('StructureDefinition', request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        profile := nil;
        try
          // first, we have to identify the structure definition
          id := request.Id;
          if request.Id <> '' then // and it must exist, because of the check above
            profile := native(manager).GetResourceById(request, 'StructureDefinition', request.Id, request.baseUrl, needSecure) as TFhirStructureDefinition
          else if request.Parameters.has('identifier') then
            profile := native(manager).GetResourceByURL('StructureDefinition', request.Parameters['identifier'], '', false, needSecure) as TFhirStructureDefinition
          else if (request.form <> nil) and request.form.hasParam('profile') then
            profile := LoadFromFormParam(request.Context as TFHIRWorkerContext, request.form.getparam('profile'), request.Lang) as TFHirStructureDefinition
          else if (request.Resource <> nil) and (request.Resource is TFHirStructureDefinition) then
            profile := request.Resource.Link as TFHirStructureDefinition
          else
            raise EFHIRException.createLang('OP_NO_RESOURCE', request.lang, ['profile']);

          profile.checkNoImplicitRules('GenerateTemplate', 'profile');
          profile.checkNoModifiers('GenerateTemplate', 'profile');

          template := nil;
          try
            builder := TProfileUtilities.create(vc(manager).Link, nil);
            try
              template := builder.populateByProfile(profile);
              if template is TFhirDomainResource then
              begin
                narr := TFHIRNarrativeGenerator.create(vc(manager).Link);
                try
                  narr.generate(template as TFhirDomainResource);
                finally
                  narr.Free;
                end;
              end;
            finally
              builder.Free;
            end;
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.Resource := template.Link;
          finally
            template.Free;
          end;
        finally
          profile.free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirGenerateTemplateOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateTemplateOperation.Name: String;
begin
  result := 'generate-template';
end;

function TFhirGenerateTemplateOperation.owningResource: String;
begin
  result := 'StructureDefinition';
end;

function TFhirGenerateTemplateOperation.Types: TArray<String>;
begin
  result := ['StructureDefinition'];
end;

{ TFhirGenerateNarrativeOperation }

function TFhirGenerateNarrativeOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirGenerateNarrativeOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  narr : TFHIRNarrativeGenerator;
  r : TFHIRResourceV;
begin
  result := '??';
  try
    r := request.Resource;
    if (r = nil) then
      raise EFHIRException.create('No resource found');
    if r is TFhirDomainResource then
    begin
      r.checkNoImplicitRules('GenerateNarrative', 'resource');
      TFhirDomainResource(r).checkNoModifiers('GenerateNarrative', 'resource');
      (r as TFhirDomainResource).text := nil;
      narr := TFHIRNarrativeGenerator.create(vc(manager).Link);
      try
        narr.generate(r as TFhirDomainResource);
      finally
        narr.Free;
      end;
    end;
    response.HTTPCode := 200;
    response.Message := 'OK';
    response.Body := '';
    response.LastModifiedDate := now;
    response.Resource := r.Link;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirGenerateNarrativeOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateNarrativeOperation.Name: String;
begin
  result := 'generate-narrative';
end;

function TFhirGenerateNarrativeOperation.owningResource: String;
begin
  result := '';
end;

function TFhirGenerateNarrativeOperation.Types: TArray<String>;
begin
  result := [''];
end;

{ TFhirSuggestKeyWordsOperation }

function TFhirSuggestKeyWordsOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirSuggestKeyWordsOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
begin
  result := '??';
  raise EFHIRException.CreateLang('NOT_DONE_YET', Request.lang);
end;

function TFhirSuggestKeyWordsOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirSuggestKeyWordsOperation.Name: String;
begin
  result := 'suggest-keywords';
end;

function TFhirSuggestKeyWordsOperation.owningResource: String;
begin
  result := 'SearchParameter';
end;

function TFhirSuggestKeyWordsOperation.Types: TArray<String>;
begin
  result := ['SearchParameter'];
end;

{ TFhirGetMetaDataOperation }

function TFhirGetMetaDataOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil
end;

function TFhirGetMetaDataOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  ok : boolean;
  meta : TFHIRMeta;
  coding : TFhirCoding;
  uri : TFhirUri;
  params : TFhirParameters;
  conn : TFDBConnection;
begin
  result := '??';
  conn := native(manager).Connection;
  try
    ok := true;
    if request.ResourceName = '' then
    begin
    // well, this operation is always allowed?
      conn.sql := 'Select Kind, Uri, Code, Display, (select count(*) from VersionTags where Tags.TagKey = VersionTags.TagKey and ResourceVersionKey in (select MostRecent from Ids)) as usecount from Tags where TagKey in (Select '+'TagKey from VersionTags where ResourceVersionKey in (select MostRecent from Ids)) order by Kind, Uri, Code'
    end
    else if request.Id = '' then
    begin
      if not manager.check(response, request.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdRead) and native(manager).ServerContext.ResConfig[request.ResourceName].Supported, 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
        ok := false
      else
        conn.sql := 'Select Kind, Uri, Code, Display, (select count(*) from VersionTags where Tags.TagKey = VersionTags.TagKey and ResourceVersionKey in (select MostRecent from Ids where ResourceTypeKey = '+inttostr(native(manager).ServerContext.ResConfig[request.ResourceName].Key)+')) as usecount  from Tags where TagKey in (Select TagKey from VersionTags where ResourceVersionKey in (select MostRecent from Ids where ResourceTypeKey = '+inttostr(native(manager).ServerContext.ResConfig[request.ResourceName].Key)+')) order by Kind, Uri, Code'
    end
    else if request.SubId <> '' then
    begin
      manager.check(response, false, 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden);
      Ok := false;
    end
    else
    begin
      if not manager.check(response, request.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdVersionRead), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
        ok := false
      else
        conn.sql := 'Select Kind, Uri, Code, VersionTags.Display, 1 as UseCount  from Tags, VersionTags '+
        'where Tags.TagKey = VersionTags.TagKey and VersionTags.ResourceVersionKey in (Select ResourceVersionKey from Versions where VersionId = :vid and ResourceKey in (select ResourceKey from Ids where Id = :id and ResourceTypeKey = '+inttostr(native(manager).ServerContext.ResConfig[request.ResourceName].Key)+')) order by Kind, Uri, Code';
    end;

    if ok then
    begin
      conn.Prepare;
      if request.Id <> '' then
      begin
        conn.BindString('id', request.Id);
        if request.SubId <> '' then
          conn.BindString('vid', request.SubId);
      end;
      conn.execute;
      meta := TFHIRMeta.Create;
      try
        while conn.FetchNext do
        begin
          if TFHIRTagCategory(conn.ColIntegerByName['Kind']) = tcProfile then
          begin
            uri := meta.profileList.Append;
            uri.value := conn.ColStringByName['Code'];
            if request.Id = '' then
              uri.AddExtension('http://www.healthintersections.com.au/fhir/ExtensionDefinition/usecount', TFhirInteger.Create(conn.ColStringByName['UseCount']));
          end
          else
          begin
            coding := TFhirCoding.create;
            try
              coding.system := conn.ColStringByName['Uri'];
              coding.code := conn.ColStringByName['Code'];
              coding.display := conn.ColStringByName['Display'];
              if request.Id = '' then
                coding.AddExtension('http://www.healthintersections.com.au/fhir/ExtensionDefinition/usecount', TFhirInteger.Create(conn.ColStringByName['UseCount']));
              if TFHIRTagCategory(conn.ColIntegerByName['Kind']) = tcTag then
                meta.tagList.add(coding.Link)
              else
                meta.securityList.add(coding.Link)
            finally
              coding.Free;
            end;
          end;
        end;
        params := TFhirParameters.create;
        try
          if meta.tagList.count + meta.securityList.count + meta.profileList.count = 0 then
            params.AddParameter('return')
          else
            params.AddParameter('return', meta.link);
          response.resource := params.Link;
        finally
          params.Free;
        end;
      finally
        meta.Free;
      end;

      conn.terminate;
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Body := '';
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message, []);
      recordStack(e);
 raise;
    end;
  end;
end;

function TFhirGetMetaDataOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-meta';
end;

function TFhirGetMetaDataOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGetMetaDataOperation.Name: String;
begin
  result := 'meta';
end;

function TFhirGetMetaDataOperation.owningResource: String;
begin
  result := '';
end;

function TFhirGetMetaDataOperation.Types: TArray<String>;
begin
  result := FFactory.ResourceNames + [''];
end;

{ TFhirAddMetaDataOperation }

function TFhirAddMetaDataOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirAddMetaDataOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  resourceKey : Integer;
  resourceVersionKey : Integer;
  versionKey : Integer;
  i : integer;
  tags : TFHIRTagList;
  t : string;
  ok : boolean;
  blob : TBytes;
  parser : TFHIRParser;
  deleted : boolean;
  meta : TFHIRMeta;
  mw : TFHIRMetaW;
  p : TFHIRResource;
begin
  result := '??';
  meta := nil;

  try
    ok := true;
    if not manager.check(response, request.canWrite(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdUpdate), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
      ok := false;
    if ok then
      manager.NotFound(request, response);
    if ok and not manager.FindResource(request.ResourceName, request.Id, [froFindDeletedResource, froForCommit], resourceKey, versionKey, request, response, nil) then
      ok := false;
    if not ok then
      // nothing
    else
    begin
      if request.SubId <> '' then
      begin
        if not manager.check(response, manager.opAllowed(request.ResourceName, fcmdUpdate), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
          ok := false;
       if ok and Not native(manager).FindResourceVersion(request.ResourceName, request.Id, request.SubId, false, resourceVersionKey, request, response) then
         ok := false;
      end
      else
        resourceVersionKey := versionKey;
    end;

    if ok then
      if (request.Resource is TFhirParameters) and (TFhirParameters(request.Resource).hasParameter('meta')) and (TFhirParameters(request.Resource)['meta'] is TFHIRMeta) then
        meta := TFhirParameters(request.Resource)['meta'] as TFHIRMeta
      else
        ok := false;

    if ok then
    begin
      mw := FFactory.wrapMeta(meta.Link);
      try
        tags := TFHIRTagList.create(FFactory.link);
        try
          native(manager).LoadTags(tags, ResourceKey);
          tags.readTags(mw);
          for i := 0 to tags.count - 1 do
            native(manager).Repository.RegisterTag(tags[i], native(manager).Connection);

          native(manager).Connection.ExecSQL('delete from VersionTags where ResourceVersionKey = '+inttostr(resourceVersionKey));
          native(manager).CommitTags(tags, resourceVersionKey);

          native(manager).Connection.SQL := 'Select Status, JsonContent from Versions where ResourceVersionKey = '+inttostr(resourceVersionKey);
          native(manager).Connection.prepare;
          native(manager).Connection.Execute;
          if not native(manager).Connection.FetchNext then
            raise EFHIRException.create('Internal Error fetching current content');
          blob := native(manager).Connection.ColBlobByName['JsonContent'];
          deleted := native(manager).Connection.ColIntegerByName['Status'] = 2;
          native(manager).Connection.Terminate;
          parser := FFactory.makeParser(request.Context, ffJson, THTTPLanguages.create('en'));
          try
            p := parser.parseResource(blob) as TFhirResource;
            try
              native(manager).Connection.SQL := 'Update Versions set XmlContent = :xc, XmlSummary = :xs, JsonContent = :jc, JsonSummary = :js, Tags = :tb where ResourceVersionKey = '+inttostr(resourceVersionKey);
              native(manager).Connection.prepare;
              native(manager).Connection.BindBlob('tb', tags.json);
              response.resource := TFhirParameters.create;
              if deleted then
              begin
                native(manager).Connection.BindNull('xc');
                native(manager).Connection.BindNull('jc');
                native(manager).Connection.BindNull('xs');
                native(manager).Connection.BindNull('js');
                meta := TFHIRMeta.Create;
                TFhirParameters(response.Resource).AddParameter('return', meta);
                tags.writeTags(mw);
              end
              else
              begin
                if p.meta = nil then
                  p.meta := TFHIRMeta.Create;
                tags.writeTags(mw);
                native(manager).Connection.BindBlob('xc', native(manager).EncodeResource(p, true, soFull));
                native(manager).Connection.BindBlob('jc', native(manager).EncodeResource(p, false, soFull));
                native(manager).markSubsetted(mw);
                native(manager).Connection.BindBlob('xs', native(manager).EncodeResource(p, true, soSummary));
                native(manager).Connection.BindBlob('js', native(manager).EncodeResource(p, false, soSummary));
                native(manager).unmarkSubsetted(mw);
                TFhirParameters(response.Resource).AddParameter('return', p.meta.link);
              end;
              native(manager).Connection.Execute;
              native(manager).Connection.Terminate;
              if not deleted and (resourceVersionKey = versionKey) then
              begin
                native(manager).CreateIndexer;
                native(manager).Indexer.execute(resourceKey, request.Id, p, tags, request).free;
              end;
            finally
              p.Free;
            end;
          finally
            parser.free;
          end;
          response.HTTPCode := 200;
          response.Message := 'OK';
        finally
          tags.free;
        end;
      finally
        mw.free;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, response.httpCode, t, response.message, manager.patientIds(request, response.Resource));
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, 500, t, e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirAddMetaDataOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-meta-add';
end;

function TFhirAddMetaDataOperation.isWrite: boolean;
begin
  result := true;
end;

function TFhirAddMetaDataOperation.Name: String;
begin
  result := 'meta-add';
end;

function TFhirAddMetaDataOperation.owningResource: String;
begin
  result := '';
end;

function TFhirAddMetaDataOperation.Types: TArray<String>;
begin
  result := FFactory.ResourceNames;
end;

{ TFhirDeleteMetaDataOperation }

function TFhirDeleteMetaDataOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirDeleteMetaDataOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  resourceKey : Integer;
  resourceVersionKey : Integer;
  versionKey : Integer;
  i : integer;
  tags : TFHIRTagList;
  t : string;
  ok : boolean;
  blob : TBytes;
  parser : TFHIRParser;
  deleted : boolean;
  meta : TFHIRMeta;
  c : TFhirCoding;
  mw : TFHIRMetaW;
  p : TFhirResource;
begin
  result := '??';
  meta := nil;
  try
    ok := true;
    if not manager.check(response, request.canWrite(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdUpdate), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
      ok := false;
    if ok then
      manager.NotFound(request, response);
    if ok and not manager.FindResource(request.ResourceName, request.Id, [froFindDeletedResource, froForCommit], resourceKey, versionKey, request, response, nil) then
      ok := false;
    if not ok then
      // nothing
    else
    begin
      if request.SubId <> '' then
      begin
        if not manager.check(response, manager.opAllowed(request.ResourceName, fcmdUpdate), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
          ok := false;
       if ok and Not native(manager).FindResourceVersion(request.ResourceName, request.Id, request.SubId, false, resourceVersionKey, request, response) then
         ok := false;
      end
      else
        resourceVersionKey := versionKey;
    end;

    if ok then
      if (request.Resource is TFhirParameters) and (TFhirParameters(request.Resource).hasParameter('meta')) and (TFhirParameters(request.Resource)['meta'] is TFHIRMeta) then
        meta := TFhirParameters(request.Resource)['meta'] as TFHIRMeta
      else
        ok := false;

    if ok then
      for c in meta.securityList do
        if ok and not native(manager).isOkToDeleteSecurityLabel(request, response, c.system, c.code) then
          ok := false;

    if ok then
    begin
      mw := FFactory.wrapMeta(meta.Link);
      try
        tags := TFHIRTagList.create(FFactory.link);
        try
          native(manager).LoadTags(tags, ResourceKey);
          tags.removeTags(mw);
          for i := 0 to tags.count - 1 do
            native(manager).Repository.RegisterTag(tags[i], native(manager).Connection);

          native(manager).Connection.ExecSQL('delete from VersionTags where ResourceVersionKey = '+inttostr(resourceVersionKey));
          native(manager).CommitTags(tags, resourceVersionKey);

          native(manager).Connection.SQL := 'Select Status, JsonContent from Versions where ResourceVersionKey = '+inttostr(resourceVersionKey);
          native(manager).Connection.prepare;
          native(manager).Connection.Execute;
          if not native(manager).Connection.FetchNext then
            raise EFHIRException.create('Internal Error fetching current content');
          blob := native(manager).Connection.ColBlobByName['JsonContent'];
          deleted := native(manager).Connection.ColIntegerByName['Status'] = 2;
          native(manager).Connection.Terminate;
          parser := FFactory.MakeParser(request.Context, ffJson, THTTPLanguages.create('en'));
          try
            p := parser.parseResource(blob) as TFhirResource;
            try
              native(manager).Connection.SQL := 'Update Versions set XmlContent = :xc, XmlSummary = :xs, JsonContent = :jc, JsonSummary = :js, Tags = :tb where ResourceVersionKey = '+inttostr(resourceVersionKey);
              native(manager).Connection.prepare;
              native(manager).Connection.BindBlob('tb', tags.json);
              response.resource := TFhirParameters.create;
              if deleted then
              begin
                native(manager).Connection.BindNull('xc');
                native(manager).Connection.BindNull('jc');
                native(manager).Connection.BindNull('xs');
                native(manager).Connection.BindNull('js');
                meta := TFHIRMeta.Create;
                TFhirParameters(response.Resource).AddParameter('return', meta);
                tags.writeTags(mw);
              end
              else
              begin
                if p.meta = nil then
                  p.meta := TFHIRMeta.Create;
                tags.writeTags(mw);
                native(manager).Connection.BindBlob('xc', native(manager).EncodeResource(p, true, soFull));
                native(manager).Connection.BindBlob('jc', native(manager).EncodeResource(p, false, soFull));
                native(manager).markSubsetted(mw);
                native(manager).Connection.BindBlob('xs', native(manager).EncodeResource(p, true, soSummary));
                native(manager).Connection.BindBlob('js', native(manager).EncodeResource(p, false, soSummary));
                native(manager).unmarkSubsetted(mw);
                TFhirParameters(response.Resource).AddParameter('return', p.meta.link);
              end;
              native(manager).Connection.Execute;
              native(manager).Connection.Terminate;
              if not deleted and (resourceVersionKey = versionKey) then
              begin
                native(manager).CreateIndexer;
                native(manager).Indexer.execute(resourceKey, request.Id, parser.resource as TFHIRResourceV, tags, request);
              end;
            finally
              p.Free;
            end;
          finally
            parser.free;
          end;
          response.HTTPCode := 200;
          response.Message := 'OK';
        finally
          tags.free;
        end;
      finally
        mw.Free;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, response.httpCode, t, response.message, manager.patientIds(request, response.resource));
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, 500, t, e.message, nil);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirDeleteMetaDataOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-meta-add';
end;

function TFhirDeleteMetaDataOperation.isWrite: boolean;
begin
  result := true;
end;

function TFhirDeleteMetaDataOperation.Name: String;
begin
  result := 'meta-delete';
end;

function TFhirDeleteMetaDataOperation.owningResource: String;
begin
  result := '';
end;

function TFhirDeleteMetaDataOperation.Types: TArray<String>;
begin
  result := FFactory.ResourceNames;
end;

{ TFhirDiffOperation }

function TFhirDiffOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirDiffOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  resourceKey : Integer;
  versionKey : Integer;
//  i : integer;
//  tags : TFHIRTagList;
//  t : string;
  ok : boolean;
  blob : TBytes;
  diff : TDifferenceEngine;
  parser : TFHIRParser;
  html : String;
//  deleted : boolean;
//  meta : TFHIRMetaW;
//  c : TFhirCoding;
begin
  result := '??';
  try
    ok := true;
    if not manager.check(response, request.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdRead), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
      ok := false;
    if ok then
      manager.NotFound(request, response);
    if ok and not manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil) then
      ok := false;
    if ok and not manager.check(response, request.Resource <> nil, 400, request.lang, 'A resource to compare must be posted', itRequired) then
      ok := false;
    if not ok then
      // nothing
    else
    begin
      native(manager).Connection.SQL := 'Select Status, JsonContent from Versions where ResourceVersionKey = '+inttostr(versionKey);
      native(manager).Connection.prepare;
      native(manager).Connection.Execute;
      if not native(manager).Connection.FetchNext then
        raise EFHIRException.create('Internal Error fetching content');
      blob := native(manager).Connection.ColBlobByName['JsonContent'];
      native(manager).Connection.Terminate;
      parser := FFactory.makeParser(request.Context, ffJson, THTTPLanguages.create('en'));
      try
        diff := TDifferenceEngine.create(vc(manager).Link, native(manager).ServerContext.Factory.link);
        try
          response.Resource := diff.generateDifference(parser.parseresource(blob), request.Resource, html).Resource as TFHIRResourceV;
          response.HTTPCode := 200;
          response.Message := 'OK';
        finally
          diff.Free;
        end;
      finally
        parser.free;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, response.httpCode, 'diff', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, 500, 'diff', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirDiffOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-diff';
end;

function TFhirDiffOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirDiffOperation.Name: String;
begin
  result := 'diff';
end;

function TFhirDiffOperation.owningResource: String;
begin
  result := '';
end;

function TFhirDiffOperation.Types: TArray<String>;
begin
  result := FFactory.ResourceNames;
end;

{ TFhirConvertOperation }

function TFhirConvertOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirConvertOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
begin
  result := '??';
  try
    response.Resource := request.Resource.link;
    response.HTTPCode := 200;
    response.Message := 'OK';
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, response.httpCode, 'diff', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, 500, 'diff', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirConvertOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-Convert';
end;

function TFhirConvertOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirConvertOperation.Name: String;
begin
  result := 'convert';
end;

function TFhirConvertOperation.owningResource: String;
begin
  result := '';
end;

function TFhirConvertOperation.Types: TArray<String>;
begin
  result := [''] + FFactory.ResourceNames;
end;

{ TFhirObservationStatsOperation }

function TFhirObservationStatsOperation.Name: String;
begin
  result := 'stats';
end;

function TFhirObservationStatsOperation.owningResource: String;
begin
  result := 'Observation';
end;


function TFhirObservationStatsOperation.resolveParameter(const lang : THTTPLanguages; code: String): TObservationStatsParameter;
var
  i  : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TObservationStatsParameter, code);
  if i = -1 then
    raise EFHIRException.createLang('MSG_PARAM_UNKNOWN', lang, [code]);
  result := TObservationStatsParameter(i);
end;


function TFhirObservationStatsOperation.Types: TArray<String>;
begin
  result := ['Observation'];
end;

function TFhirObservationStatsOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirObservationStatsOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  req : TFHIRStatsOpRequest;
  s : string;
  ose : TObservationStatsEvaluator;
  c : TFhirCoding;
  list : TFslList<TFHIRResourceV>;
  res : TFHIRResourceV;
begin
  result := '??';
  try
    manager.NotFound(request, response);
    req := TFHIRStatsOpRequest.create();
    try
      if (request.Resource <> nil) and (request.Resource is TFhirParameters) then
        req.load(request.Resource as TFhirParameters)
      else
        req.load(request.Parameters);

      ose := TObservationStatsEvaluator.create(FFactory.link, native(manager).Connection, TFHIRStatsOpResponse5.create(TFHIRStatsOpResponse.Create));
      try
        ose.subject := req.subject;
        ose.subjectKey := resolvePatient(manager, request, req.subject);
        for s in req.codeList do
          ose.concepts.add(TFHIRCoding5.Create(TFHIRCoding.Create(req.system, s)));
        for c in req.codingList do
          ose.concepts.add(TFHIRCoding5.Create(c.Link));
        if (ose.concepts.empty) then
          raise EFHIRException.create('no code or coding found');
        if (req.duration <> '') then
        begin
          ose.start := TFslDateTime.makeUTC.DateTime - DATETIME_HOUR_ONE * StrToFloat(req.duration);
          ose.finish := TFslDateTime.makeUTC.DateTime;
        end
        else if (req.period <> nil) then
        begin
          if (req.period.start.null) then
            raise EFHIRException.create('Period.start is required');
          ose.start := req.period.start.UTC.DateTime;
          if (req.period.end_.null) then
            raise EFHIRException.create('Period.end is required');
          ose.finish := req.period.end_.UTC.DateTime;
        end
        else
          raise EFHIRException.create('duration or period is required');
        if (req.statisticList.Count = 0) then
          raise EFHIRException.create('at least one parameter is required');

        for s in req.statisticList do
          ose.parameters := ose.parameters + [resolveParameter(request.Lang, s)];

        ose.execute;
        if (req.include) then
        begin
          list := native(manager).loadResources(ose.Observations);
          try
            for res in list do
              ose.Resp.AddObs(res.link)
          finally
            list.Free;
          end;
        end;

        response.Resource := ose.resp.asParams;
      finally
        ose.Free;
      end;

    finally
      req.Free;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, [req.subject]);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirObservationStatsOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Observation-Stats';
end;

function TFhirObservationStatsOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirObservationLastNOperation }

function TFhirObservationLastNOperation.Name: String;
begin
  result := 'lastn';
end;

function TFhirObservationLastNOperation.owningResource: String;
begin
  result := 'Observation';
end;

function TFhirObservationLastNOperation.Types: TArray<String>;
begin
  result := ['Observation'];
end;

function TFhirObservationLastNOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirObservationLastNOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  sp : TSearchProcessor;
  conn : TFDBConnection;
  base, field, type_ : String;
  bundle : TFHIRBundleBuilder;
  op : TFHIROperationOutcome;
  keys : TKeyList;
  summaryStatus : TFHIRSummaryOption;
  needsObject : boolean;
  prsrFmt : TFhirFormat;
  patIds : TPatientIdTracker;
begin
  result := '??';
  patIds := TPatientIdTracker.Create;
  try
  conn := native(manager).Connection;
  try
    sp := TSearchProcessor.create(native(manager).ServerContext);
    try
      sp.resConfig := native(manager).ServerContext.ResConfig.Link;
      sp.typeKey := native(manager).Connection.CountSQL('select ResourceTypeKey from Types where ResourceName = ''Observation''');
      sp.type_ := 'Observation';
      sp.compartment := request.compartment.Link;
      sp.sessionCompartments := request.SessionCompartments.link;
      sp.baseURL := request.baseURL;
      sp.lang := request.lang;
      sp.params := request.Parameters;
      native(manager).CreateIndexer;
      sp.indexes := native(manager).ServerContext.Indexes.Link;
      sp.countAllowed := false;
      sp.Connection := conn.link;
      sp.build;

      response.OnCreateBuilder(request, response, btSearchset, bundle);
      op := TFHIROperationOutcome.Create;
      keys := TKeyList.Create;
      try
        bundle.setId(FhirGUIDToString(CreateGUID));
        bundle.setLastUpdated(TFslDateTime.makeUTC);
        summaryStatus := request.Summary;

        base := AppendForwardSlash(Request.baseUrl)+request.ResourceName+'/$lastn?';
        if response.Format <> ffUnspecified then
          base := base + '_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
        bundle.addLink('self', base+sp.link_);
        native(manager).chooseField(response.Format, summaryStatus, request.loadObjects, field, prsrFmt, needsObject);
        if (not needsObject) then
          prsrFmt := ffUnspecified;

        conn.SQL :=
          'Select '+#14#10+
          '  ResourceKey, ResourceName, Id, 0 as Score1, 0 as Score2, VersionId, Secure, StatedDate, Status, CodeList, Tags, '+field+' '+#14#10+
          'from ( '+#14#10+
          'Select '+#14#10+
          '  Ids.ResourceKey, Types.ResourceName, Ids.Id, 0 as Score1, 0 as Score2, VersionId, Secure, StatedDate, Versions.Status, CodeList, Tags, '+field+', '+#14#10+
          '  ROW_NUMBER() OVER (PARTITION BY CodeList '+#14#10+
          '                              ORDER BY StatedDate DESC '+#14#10+
          '                             ) '+#14#10+
          '             AS rn '+#14#10+
          'from '+#14#10+
          '  Versions, Ids, Types, Observations '+#14#10+
          'where '+#14#10+
          '  Observations.ResourceKey = Ids.ResourceKey and Observations.isComponent = 0 and Types.ResourceTypeKey = Ids.ResourceTypeKey and '+#14#10+
          '  Ids.MostRecent = Versions.ResourceVersionKey and Ids.resourceKey in ( '+#14#10+
          '    select ResourceKey from Ids where Ids.Deleted = 0 and '+sp.filter+#14#10+
          '   ) '+#14#10+
          ') tmp '+#14#10+
          'WHERE rn <= 4 '+#14#10+
          'ORDER BY CodeList, StatedDate Desc, rn'+#14#10;
        bundle.tag('sql', conn.SQL);
        conn.Prepare;
        try
          conn.Execute;
          while conn.FetchNext do
          Begin
            native(manager).AddResourceTobundle(request, bundle, request.secure, request.baseUrl, field, prsrFmt, smUnknown, false, request.parameters.has('_summary'), type_, patIds);
            keys.Add(TKeyPair.Create(type_, conn.ColStringByName['ResourceKey']));
          end;
        finally
          conn.Terminate;
        end;
        native(manager).processIncludes(request, request.session, request.secure, request.Parameters['_include'], request.Parameters['_revinclude'], bundle, keys, request.baseUrl, request.Lang, field, prsrFmt, patIds);

//          if (op.issueList.Count > 0) then
//          begin
//            be := bundle.entryList.Append;
//            be.resource := op.Link;
//            be.search := TFhirBundleEntrySearch.Create;
//            be.search.mode := SearchEntryModeOutcome;
//          end;

          //bundle.link_List['self'] := request.url;
        response.HTTPCode := 200;
        response.Message := 'OK';
        response.Body := '';
        response.Resource := nil;
        response.resource := bundle.getBundle;
      finally
        keys.Free;
        bundle.Free;
        op.Free;
      end;
    finally
      sp.Free;
    end;

      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, patIds.ids);
  except
    on e: exception do
    begin
        manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, patIds.ids);
      recordStack(e);
      raise;
    end;
  end;
  finally
    patIds.Free;
  end;
end;

function TFhirObservationLastNOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Observation-lastn';
end;

function TFhirObservationLastNOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirEncounterEverythingOperation }

function TFhirEncounterEverythingOperation.Name: String;
begin
  result := 'everything';
end;

function TFhirEncounterEverythingOperation.owningResource: String;
begin
  result := 'Encounter';
end;


function TFhirEncounterEverythingOperation.resourceName: String;
begin
  result := 'Encounter';
end;

function TFhirEncounterEverythingOperation.Types: TArray<String>;
begin
  result := ['Encounter'];
end;

function TFhirEncounterEverythingOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
//  result := CreateBaseDefinition(base);
//  try
//    result.!{$IFDEF FHIR5}notes{$ELSE}comment{$ENDIF} := 'This server has little idea what a valid Encounter record is; it returns everything in the Encounter compartment, and any resource directly referred to from one of these';
//    result.system := False;
//    result.resourceList.AddItem('Encounter');
//    result.type_ := true;
//    result.instance := true;
//    with result.parameterList.Append do
//    begin
//      name := 'return';
//      use := OperationParameterUseOut;
//      min := '1';
//      max := '1';
//      documentation := 'Encounter record as a bundle';
//      type_ := !{$IFNDEF FHIR5}AllTypesBundle {$ELSE}OperationParameterTypeBundle{$ENDIF};
//    end;
//    result.Link;
//  finally
//    result.Free;
//  end;
  result := nil;
end;

function TFhirEncounterEverythingOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Encounter-everything';
end;

function TFhirEncounterEverythingOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirGroupEverythingOperation }

function TFhirGroupEverythingOperation.Name: String;
begin
  result := 'everything';
end;

function TFhirGroupEverythingOperation.owningResource: String;
begin
  result := 'Group';
end;


function TFhirGroupEverythingOperation.resourceName: String;
begin
  result := 'Group';
end;

function TFhirGroupEverythingOperation.Types: TArray<String>;
begin
  result := ['Group'];
end;

function TFhirGroupEverythingOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
//  result := CreateBaseDefinition(base);
//  try
//    result.system := False;
//    result.resourceList.AddItem('Group');
//    result.type_ := true;
//    result.instance := true;
//    with result.parameterList.Append do
//    begin
//      name := 'return';
//      use := OperationParameterUseOut;
//      min := '1';
//      max := '1';
//      documentation := 'Bundle with information for all patients in the group';
//      type_ := !{$IFNDEF FHIR5}AllTypesBundle {$ELSE}OperationParameterTypeBundle{$ENDIF};
//    end;
//    result.Link;
//  finally
//    result.Free;
//  end;
  result := nil;
end;

function TFhirGroupEverythingOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Group-everything';
end;

function TFhirGroupEverythingOperation.isWrite: boolean;
begin
  result := false;
end;


{ TFhirGraphFetchOperation }

function TFhirGraphFetchOperation.Name: String;
begin
  result := 'graph';
end;

function TFhirGraphFetchOperation.owningResource: String;
begin
  result := '';
end;

function TFhirGraphFetchOperation.Types: TArray<String>;
begin
  result := FFactory.ResourceNames;
end;

function TFhirGraphFetchOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirGraphFetchOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  gd : TFHIRGraphDefinition;
  resourceKey, versionKey : integer;
//  vs, dst : TFHIRValueSet;
//  resourceKey, versionKey : integer;
//  url, cacheId, filter : String;
//  profile : TFhirExpansionProfile;
//  limit, count, offset : integer;
  params : TFhirParameters;
  needSecure : boolean;
  engine : TFHIRGraphDefinitionEngine4;
  p : TFHIRGraphDefinitionParseR5;
begin
  result := '??';
  try
    manager.NotFound(request, response);
    if (request.Id <> '') and manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        params := makeParamsV(request);
        try
          if params.hasParameter('graph') then
            gd := native(manager).GetResourceById(request, 'GraphDefinition', params.str['graph'], request.baseUrl, needSecure) as TFHIRGraphDefinition
          else if params.hasParameter('definition') then
          begin
            p := TFHIRGraphDefinitionParseR5.Create;
            try
              gd := p.parseV(params.str['definition']) as TFHIRGraphDefinition
            finally
              p.Free;
            end;
          end
          else
            raise EFHIRException.create('No Graph definition found');
          try

            engine := TFHIRGraphDefinitionEngine4.Create(vc(manager).Link);
            try
              engine.OnFollowReference := native(manager).GraphFollowReference;
              engine.OnListResources := native(manager).GraphListResources;

              engine.baseURL := request.baseUrl;
              engine.definition := gd.link;
              engine.start := native(manager).GetResourceById(request, request.ResourceName, request.Id, request.baseUrl, needSecure) as TFhirResource;
              engine.bundle := TFHIRBundle.Create;
              engine.bundle.id := NewGuidId;
              engine.bundle.type_ := BundleTypeSearchset;
              engine.depthLimit := 25;
              engine.validating := false;
              engine.appInfo := request.link;

              engine.execute;
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
              response.Resource := engine.bundle.Link;
            finally
              engine.Free;
            end;
          finally
            gd.Free;
          end;
        finally
          params.free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirGraphFetchOperation.formalURL: String;
begin
  result := 'http://healthintersections.com.au/fhir/OperationDefinition/graph-fetch';
end;

function TFhirGraphFetchOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirPatientEverythingOperation }

function TFhirPatientEverythingOperation.Name: String;
begin
  if FIsExport then
    result := 'export'
  else
    result := 'everything';
end;

function TFhirPatientEverythingOperation.owningResource: String;
begin
  result := 'Patient';
end;


function TFhirPatientEverythingOperation.resourceName: String;
begin
  result := 'Patient';
end;

function TFhirPatientEverythingOperation.Types: TArray<String>;
begin
  result := ['Patient'];
end;

constructor TFhirPatientEverythingOperation.create(factory : TFhirFactory; isExport: boolean);
begin
  inherited Create(factory);
  FIsExport := isExport;
end;

function TFhirPatientEverythingOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
//  result := CreateBaseDefinition(base);
//  try
//    result.!{$IFDEF FHIR5}notes{$ELSE}comment{$ENDIF} := 'This server has little idea what a valid patient record is; it returns everything in the patient compartment, and any resource directly referred to from one of these';
//    result.system := False;
//    result.resourceList.AddItem('Patient');
//    result.type_ := true;
//    result.instance := true;
//    with result.parameterList.Append do
//    begin
//      name := 'return';
//      use := OperationParameterUseOut;
//      min := '1';
//      max := '1';
//      documentation := 'Patient record as a bundle';
//      type_ := !{$IFNDEF FHIR5}AllTypesBundle {$ELSE}OperationParameterTypeBundle{$ENDIF};
//    end;
//    result.Link;
//  finally
//    result.Free;
//  end;
  result := nil;
end;

function TFhirPatientEverythingOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Patient-everything';
end;

function TFhirPatientEverythingOperation.isPrimaryResource(request: TFHIRRequest; rtype, id: String): boolean;
begin
  result := (rtype = 'Patient') and (id = request.Id);
end;

function TFhirPatientEverythingOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirGenerateDocumentOperation }

procedure TFhirGenerateDocumentOperation.addResource(manager: TFHIROperationEngine; secure : boolean; request : TFHIRRequest; bundle : TFHIRBundle; source : TFHIRResourceV; reference : TFhirReference; required : boolean; patIds : TPatientIdTracker);
var
  res : TFHIRResourceV;
  needSecure : boolean;
  entry : TFHIRBundleEntry;
  exists : boolean;
  url : String;
begin
  if reference = nil then
    exit;
  res := native(manager).getResourceByReference(source, reference.reference, request, true, needSecure);
  try
    if res <> nil then
    begin
      if needSecure and not secure then
      begin
        if required then
          raise ERestfulException.Create('TFhirGenerateDocumentOperation.Execute', 404, itSuppressed, 'This document contains resources labelled with a security tag that means this server will only send it if the connection is secure', manager.lang);
      end
      else
      begin
        patIds.seeIds(manager.patientIds(request, res));
        url := native(manager).ServerContext.FormalURLPlain+'/'+res.fhirType+'/'+res.id;
        exists := false;
        for entry in bundle.entryList do
          if entry.fullUrl = url then
            exists := true;
        if not exists then
        begin
          entry := bundle.entryList.Append;
          entry.resource := res.Link as TFhirResource;
          entry.fullUrl := native(manager).ServerContext.FormalURLPlain+'/'+res.fhirType+'/'+res.id;
        end;
      end
    end
    else if required then
      raise EFHIRException.createLang('MSG_NO_MATCH', request.lang, [reference.reference]);
  finally
    res.Free;
  end;
end;

procedure TFhirGenerateDocumentOperation.addSections(manager: TFHIROperationEngine; secure : boolean; request : TFHIRRequest; bundle: TFHIRBundle; composition : TFhirComposition; sections: TFhirCompositionSectionList; patIds : TPatientIdTracker);
var
  i, j : integer;
begin
  for i := 0 to sections.Count - 1 do
  begin
    for j := 0 to sections[i].entryList.Count - 1 do
      addResource(manager, secure, request, bundle, composition, sections[i].entryList[j], true, patIds);
    if (sections[i].hasSectionList) then
      addSections(manager, secure, request, bundle, composition, sections[i].sectionList, patIds);
  end;
end;

function TFhirGenerateDocumentOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
//  result := CreateBaseDefinition(base);
//  try
//    result.system := False;
//    result.resourceList.AddItem('Composition');
//    result.type_ := true;
//    result.instance := true;
//    with result.parameterList.Append do
//    begin
//      name := 'return';
//      use := OperationParameterUseOut;
//      min := '1';
//      max := '1';
//      documentation := 'Composition as a bundle (document)';
//      type_ := !{$IFNDEF FHIR5}AllTypesBundle {$ELSE}OperationParameterTypeBundle{$ENDIF};
//    end;
//    result.Link;
//  finally
//    result.Free;
//  end;
  result := nil
end;

function TFhirGenerateDocumentOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  composition : TFhirComposition;
  bundle : TFHIRBundle;
  resourceKey, versionKey : integer;
  entry : TFHIRBundleEntry;
  i, j : integer;
  needSecure : boolean;
  gd : TFhirGraphDefinition;
  engine : TFHIRGraphDefinitionEngine4;
  p : TFHIRGraphDefinitionParseR5;
  patIds : TPatientIdTracker;
begin
  result := '??';
  patIds := TPatientIdTracker.create;
  try
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil) then
      begin
        composition := native(manager).GetResourceByKey(resourceKey, needSecure) as TFhirComposition;
        try
          composition.checkNoImplicitRules('GenerateDocument', 'composition');
          composition.checkNoModifiers('GenerateDocument', 'composition');
          if needSecure and not request.secure then
            raise ERestfulException.Create('TFhirGenerateDocumentOperation.Execute', 404, itSuppressed, 'This document contains resources labelled with a security tag that means this server will only send it if the connection is secure', manager.lang);

          bundle := TFHIRBundle.Create(BundleTypeDocument);
          try
            bundle.id := copy(GUIDToString(CreateGUID), 2, 46).ToLower;
            bundle.meta := TFHIRMeta.Create;
            bundle.meta.lastUpdated := TFslDateTime.makeUTC;
//            bundle.base := native(manager).ServerContext.FormalURLPlain;
            bundle.identifier := TFhirIdentifier.Create;
            bundle.identifier.system := 'urn:ietf:rfc:4986';
            bundle.identifier.value := NewGuidURN;
            entry := bundle.entryList.Append;
            entry.resource := composition.Link;
            entry.fullUrl := native(manager).ServerContext.FormalURLPlain+'/Composition/'+composition.id;
            if request.Parameters['graph'] <> '' then
              gd := native(manager).GetResourceById(request, 'GraphDefinition', request.Parameters['graph'], request.baseUrl, needSecure) as TFHIRGraphDefinition
            else if request.Parameters['definition'] <> '' then
            begin
              p := TFHIRGraphDefinitionParseR5.Create;
              try
                gd := p.parseV(request.Parameters['definition']) as TFHIRGraphDefinition
              finally
                p.Free;
              end;
            end
            else
              gd := nil;

            if gd <> nil then
            begin
              try
                engine := TFHIRGraphDefinitionEngine4.Create(vc(manager).Link);
                try
                  engine.OnFollowReference := native(manager).GraphFollowReference;
                  engine.OnListResources := native(manager).GraphListResources;

                  engine.baseURL := 'http://hl7.org/fhir/test';
                  engine.definition := gd.link;
                  engine.start := composition.link;
                  engine.bundle := TFHIRBundle.Create;
                  engine.bundle.id := NewGuidId;
                  engine.bundle.type_ := BundleTypeSearchset;
                  engine.depthLimit := 25;
                  engine.validating := false;
                  engine.appInfo := request.link;

                  engine.execute;
                finally
                  engine.Free;
                end;
              finally
                gd.Free;
              end;
            end
            else
            begin
              addResource(manager, request.secure, request, bundle, composition, composition.subject, true, patIds);
              addSections(manager, request.secure, request, bundle, composition, composition.sectionList, patIds);

              for i := 0 to composition.authorList.Count - 1 do
                addResource(manager, request.secure, request, bundle, composition, composition.authorList[i], false, patIds);
              for i := 0 to composition.attesterList.Count - 1 do
                addResource(manager, request.secure, request, bundle, composition, composition.attesterList[i].party, false, patIds);
              addResource(manager, request.secure, request, bundle, composition, composition.custodian, false, patIds);
              for i := 0 to composition.eventList.Count - 1 do
                for j := 0 to composition.eventList[i].detailList.Count - 1 do
                  addResource(manager, request.secure, request, bundle, composition, composition.eventList[i].detailList[j], false, patIds);
              addResource(manager, request.secure, request, bundle, composition, composition.encounter, false, patIds);
            end;

            if request.Parameters['persist'] = 'true' then
            begin
              request.ResourceName := bundle.FhirType;
              request.CommandType := fcmdUpdate;
              request.Id := bundle.id;
              request.Resource := bundle.link;
              manager.Execute(context, request, response);
            end
            else
            begin
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
              response.Resource := bundle.Link;
            end;
          finally
            bundle.Free;
          end;
        finally
          composition.Free;
        end;
      end;
    end;
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, patIds.ids);
  except
    on e: exception do
    begin
        manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, patIds.ids);
      recordStack(e);
      raise;
    end;
  end;
  finally
    patIds.Free;
  end;
end;

function TFhirGenerateDocumentOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateDocumentOperation.Name: String;
begin
  result := 'document';
end;

function TFhirGenerateDocumentOperation.owningResource: String;
begin
  result := 'Composition';
end;

function TFhirGenerateDocumentOperation.Types: TArray<String>;
begin
  result := ['Composition'];
end;

{ TFhirTransformOperation }

function TFhirTransformOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirTransformOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  params : TFHIRTransformOpRequest;
  rkey, versionKey : integer;
  needSecure : boolean;
  libw : TFslMap<TFHIRStructureMapW>;
  lib : TFslMap<TFHIRStructureMap>;
  lw : TFHIRStructureMapW;
  map : TFHIRStructureMap;
  utils : TFHIRStructureMapUtilities;
  outcome : TFHIRObject;
//  params : TFhirParametersW;
//  sdParam, sdBase : TFhirStructureDefinition;
//  utils : TProfileUtilities;
//  op : TFHIROperationOutcomeW;
begin
  result := '??';
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      params := TFHIRTransformOpRequest.Create;
      try
        if (request.Resource <> nil) and (request.Resource is TFhirParameters) then
          params.load(request.Resource.Link as TFhirParameters)
        else
        begin
          params.load(request.Parameters);
          if (request.Resource <> nil) then
            params.content := request.Resource.Link as TFhirResource;
        end;

        lib := TFslMap<TFHIRStructureMap>.create('op.transform');
        try
          libw := native(manager).ServerContext.getMaps;
          try
            for lw in libw.Values do
              lib.Add(lw.url, lw.Resource.link as TFHIRStructureMap);
          finally
            libw.Free;
          end;
          map := nil;
          if request.Id <> '' then
          begin
            if manager.FindResource('StructureMap', request.Id, [], rkey, versionKey, request, response, nil) then
            begin
              map := native(manager).GetResourceByKey(rkey, needSecure) as TFHIRStructureMap;
              if needSecure and not request.secure then
                raise ERestfulException.Create('TFhirGenerateDocumentOperation.Execute', 404, itSuppressed, 'This document contains resources labelled with a security tag that means this server will only send it if the connection is secure', manager.lang);
            end;
          end
          else if params.source <> '' then
          begin
            map := lib[params.source].link as TFHIRStructureMap;
            manager.check(response, map <> nil, 404 , manager.lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', manager.lang), [params.source]), itNotFound);
          end
          else
            manager.check(response, false, 404, manager.lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', manager.lang), ['no id provided']), itNotFound);
          if (map <> nil) then
          begin
            try
              outcome := TFHIRServerContext(native(manager).ServerContext).Factory.makeByName(map.targetType);
              try
                utils := TFHIRStructureMapUtilities.Create(vc(manager).link, lib.Link, TServerTransformerServices.create(native(manager).ServerContext.link), nil);
                try
                  try
                    utils.transform(nil, params.content, map, outcome);
                    response.HTTPCode := 200;
                    response.Message := 'OK';
                    response.Body := '';
                    response.LastModifiedDate := now;
                    if outcome is TFHIRResourceV then
                      response.Resource := (outcome as TFHIRResourceV).link
                    else
                      raise EFslException.Create('not supported yet');
//                      response.Resource := TFHIRCustomResource.createFromBase(vc, outcome);
                  except
                    on e : exception do
                      native(manager).check(response, false, 500, manager.lang, e.Message, itProcessing);
                  end;
                finally
                  utils.Free;
                end;
              finally
                outcome.Free;
              end;
            finally
              map.Free;
            end;
          end;
        finally
          lib.Free;
        end;
      finally
        params.Free
      end;

    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirTransformOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/StructureMap-transform';
end;

function TFhirTransformOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirTransformOperation.Name: String;
begin
  result := 'transform';

end;

function TFhirTransformOperation.owningResource: String;
begin
  result := 'StructureMap';
end;

function TFhirTransformOperation.Types: TArray<String>;
begin
  result := ['StructureMap'];
end;

{ TServerTransformerServices }

constructor TServerTransformerServices.create(ServerContext : TFHIRServerContext);
begin
  Inherited Create;
  FServerContext := ServerContext;
end;

procedure TServerTransformerServices.createResource(appInfo: TFslObject; res: TFHIRObject; atRootofTransform: boolean);
begin
  raise Exception.Create('Not done yet');
end;

function TServerTransformerServices.createType(appInfo: TFslObject; tn: String): TFHIRObject;
begin
  raise Exception.Create('Not done yet');
end;

destructor TServerTransformerServices.Destroy;
begin
  FServerContext.Free;
  inherited;
end;

procedure TServerTransformerServices.log(s: String);
begin
  // nothing right now
end;

function TServerTransformerServices.performSearch(appInfo: TFslObject;
  url: String): TFslList<TFHIRObject>;
begin
  raise Exception.Create('Not done yet');
end;

function TServerTransformerServices.translate(appInfo: TFslObject; src: TFHIRCoding; conceptMapUrl: String): TFHIRCoding;
begin
  raise EFHIRException.CreateLang('NOT_DONE_YET', THTTPLanguages.create('en'){?});
end;

{ TFHIRNativeStorageServiceR5 }

procedure TFHIRNativeStorageServiceR5.checkDefinitions;
var
  s, sx : string;
  fpe : TFHIRPathEngine;
  sd : TFhirStructureDefinition;
  ed: TFhirElementDefinition;
  inv : TFhirElementDefinitionConstraint;
  td : TFHIRTypeDetails;
  expr : TFHIRPathExpressionNode;
begin
  s := '';
  fpe := TFHIRPathEngine.create(vc, TUcumServiceImplementation.Create(ServerContext.TerminologyServer.CommonTerminologies.Ucum.Link));
  try
    for sd in vc.Profiles.ProfilesByURL.Values do

      if sd.snapshot <> nil then
      begin
        for ed in sd.snapshot.elementList do
          for inv in ed.constraintList do
          begin
            sx := inv.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-expression'); //inv.expression
            if (sx <> '') and not sx.contains('$parent') then
            begin
              try
                expr := fpe.parse(sx);
                try
                  if sd.kind = StructureDefinitionKindResource then
                    td := fpe.check(nil, sd.id, ed.path, '', expr, false)
                  else
                    td := fpe.check(nil, 'DomainResource', ed.path, '', expr, false);
                  try
                    if (td.hasNoTypes) then
                      s := s + inv.key+' @ '+ed.path+' ('+sd.name+'): no possible result from '+sx + #13#10
                  finally
                    td.free;
                  end;
                finally
                  expr.Free;

                end;
              except
                on e : Exception do
                  s := s + inv.key+' @ '+ed.path+' ('+sd.name+'): exception "'+e.message+'" ('+sx+')' + #13#10;
              end;
            end;
          end;
        end;
  finally
    fpe.Free;
  end;
end;

procedure TFHIRNativeStorageServiceR5.checkProposedResource(session: TFhirSession; needsSecure, created: boolean; request: TFHIRRequest; res: TFHIRResourceV; tags: TFHIRTagList);
var
  vs : TFHIRValueset;
  resource : TFHIRResource;
  ed : TFhirEventDefinitionW;
begin
  resource := res as TFHIRResource;

  if (resource.ResourceType in [frtValueSet, frtConceptMap, frtStructureDefinition, frtQuestionnaire, frtSubscription]) and (needsSecure or ((resource.meta <> nil) and not resource.meta.securityList.IsEmpty)) then
    raise ERestfulException.Create('TFHIRNativeStorageService.SeeResource', 400, itBusinessRule, 'Resources of type '+CODES_TFHIRResourceType[resource.ResourceType]+' are not allowed to have a security label on them', request.lang);

  if resource.ResourceType = frtValueSet then
  begin
    vs := TFHIRValueSet(resource);
    ServerContext.TerminologyServer.checkTerminologyResource(vs)
  end
  else if resource.ResourceType in [frtConceptMap, frtCodeSystem] then
    ServerContext.TerminologyServer.checkTerminologyResource(resource)
  else if resource.ResourceType = frtStructureDefinition then
    vc.checkResource(resource as TFhirStructureDefinition)
  else if resource.ResourceType = frtQuestionnaire then
    vc.checkResource(resource as TFhirQuestionnaire)
  else if resource.ResourceType = frtEventDefinition then
  begin
    ed := factory.wrapEventDefinition(resource.Link);
    try
      {$IFNDEF NO_JS}
      ServerContext.EventScriptRegistry.checkResource(ed);
      {$ENDIF}
    finally
      ed.Free;
    end;
  end;
end;

function TFHIRNativeStorageServiceR5.createOperationContext(const lang : THTTPLanguages): TFHIROperationEngine;
begin
  result := TFhirNativeOperationEngineR5.Create(lang, ServerContext, self.Link, DB.GetConnection('Operation'));
end;

function TFHIRNativeStorageServiceR5.engineFactory(const lang : THTTPLanguages; usage: String): TFHIRNativeOperationEngine;
begin
  result := TFHIRNativeOperationEngineR5.create(lang, ServerContext, self.Link, DB.GetConnection(usage));
end;

procedure TFHIRNativeStorageServiceR5.FinishRecording();
begin
 // nothing
end;

procedure TFHIRNativeStorageServiceR5.RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception);
var
  op: TFhirTestScriptSetupActionOperation;
  ts : TFhirTestScript;
begin
  if req.Session = nil then
    exit;
  if req.Session.TestScript = nil then
    exit;
  if (req.CommandType in NON_STD_COMMANDS) then
    exit;
  op := TFhirTestScriptSetupActionOperation.Create;
  ts := req.Session.TestScript.Resource as TFHIRTestScript;
  ts.testList.Append.actionList.Append.operation := op;
  if req.CommandType = fcmdOperation then
    op.type_ := TFHIRCoding.Create('http://hl7.org/fhir/testscript-operation-codes', req.OperationName)
  else
    op.type_ := TFHIRCoding.Create('http://hl7.org/fhir/testscript-operation-codes', CODES_TFHIRCommandType[req.CommandType].ToLower);
  op.resource := TFhirFHIRDefinedTypeEnum(StringArrayIndexOfSensitive(CODES_TFhirFHIRDefinedTypeEnum, req.ResourceName));
  if resp.format = ffJson then
    op.Accept := 'application/fhir+json'
  else
    op.Accept := 'application/fhir+xml';
  op.params := req.Parameters.Source;
  op.requestHeaderList.Add('Host', req.baseUrl);
  op.requestHeaderList.Add('Content-Type', MIMETYPES_TFHIRFormat[req.PostFormat]);
  if (req.lastModifiedDate <> 0) then
    op.requestHeaderList.Add('Last-Modified', DateTimeToXMLDateTimeTimeZoneString(req.lastModifiedDate, TimeZoneBias));
  op.requestHeaderList.Add('Language', req.lang.header);
  op.requestHeaderList.Add('if-match', req.IfMatch);
  op.requestHeaderList.Add('if-none-match', req.IfNoneMatch);
  if (req.IfModifiedSince <> 0) then
    op.requestHeaderList.Add('if-modified-since', DateTimeToXMLDateTimeTimeZoneString(req.IfModifiedSince, TimeZoneBias));
  op.requestHeaderList.Add('if-none-exist', req.IfNoneExist);
  if req.provenance <> nil then
    op.requestHeaderList.Add('X-Provenance', ComposeJson(ServerContext.ValidatorContext as TFHIRWorkerContext, req.provenance.Resource as TFhirProvenance));
  op.url := req.url;
end;

procedure TFHIRNativeStorageServiceR5.RegisterAuditEvent(session: TFhirSession; ip: String);
var
  se: TFhirAuditEvent;
  C: TFHIRCoding;
  p: TFhirAuditEventParticipant;
begin
  se := TFhirAuditEvent.Create;
  try
    se.event := TFhirAuditEventEvent.Create;
    se.event.type_ := TFHIRCoding.Create;
    C := se.event.type_;
    C.code := '110114';
    C.system := 'http://nema.org/dicom/dcid';
    C.Display := 'User Authentication';
    C := se.event.subtypeList.append;
    C.code := '110144';
    C.system := 'http://nema.org/dicom/dcid';
    C.Display := 'Login';
    se.event.action := AuditEventActionE;
    se.event.outcome := TFhirCodeableConcept.Create('http://terminology.hl7.org/CodeSystem/audit-event-outcome', '0');
    se.event.dateTime := TFslDateTime.makeUTC;
    se.source := TFhirAuditEventSource.Create;
    se.source.site := ServerContext.Globals.OwnerName;
    se.source.observer := TFhirReference.Create;
    se.source.observer.identifier := TFhirIdentifier.Create;
    se.source.observer.identifier.system := 'urn:ietf:rfc:3986';
    se.source.observer.identifier.value := ServerContext.DatabaseId;
    C := se.source.type_List.append;
    C.code := '3';
    C.Display := 'Web Server';
    C.system := 'http://hl7.org/fhir/security-source-type';

    // participant - the web browser / user proxy
    p := se.participantList.append;
    p.who := TFhirReference.Create;
    p.who.identifier := TFhirIdentifier.Create;
    p.who.identifier.system := ServerContext.DatabaseId;
    p.who.identifier.value := inttostr(session.key);
    p.altId := session.id;
    p.name := session.SessionName;
    if (ip <> '') then
    begin
      p.network := TFhirAuditEventParticipantNetwork.Create;
      p.network.address := ip;
      p.network.type_ := AuditEventAgentNetworkType4;
      p.requestor := true;
    end;

    QueueResource(session, se, se.event.dateTime);
  finally
    se.free;
  end;
end;

procedure TFHIRNativeStorageServiceR5.RegisterConsentRecord(session: TFhirSession);
var
  pc: TFhirConsent;
begin
  if session.Compartments.Count = 1 then
  begin
    pc := TFhirConsent.Create;
    try
      pc.status := ConsentStateActive;
      with pc.categoryList.Append.codingList.append do
      begin
        system := 'http://hl7.org/fhir/consentcategorycodes';
        code := 'smart-on-fhir';
      end;
      pc.dateTime := TFslDateTime.makeUTC;
//      pc.period := TFHIRPeriod.Create;
//      pc.period.start := pc.dateTime.Link;
//      pc.period.end_ := TFslDateTime.CreateUTC(session.expires);
      pc.subject := TFHIRReference.Create;
      pc.subject.reference := session.Compartments[0].ToString;
      // todo: do we have a reference for the consentor?
      // todo: do we have an identity for the organization?
  //    for
  //
  //    with pc.except_List.Append do
  //    begin
  //      type_ := ConsentExceptTypePermit;
  //      action := TFHIRCodeableConcept.Create;
  //      action.codingList.add(TFHIRCoding.Create('http://hl7.org/fhir/consentaction', 'read')));
  //    end;
  //  finally
  //
  //  end;
    finally
      pc.Free;
    end;
  end;
end;

procedure TFHIRNativeStorageServiceR5.SeeResource(key, vkey, pvkey: integer; id: string; needsSecure, created: boolean; res: TFHIRResourceV; conn: TFDBConnection; reload: Boolean; session: TFhirSession; const lang : THTTPLanguages; src: TBytes);
var
  vs : TFHIRValueSet;
  resource : TFHIRResource;
begin
  resource := res as TFHIRResource;
  if (resource.ResourceType in [frtValueSet, frtConceptMap, frtStructureDefinition, frtQuestionnaire, frtSubscription]) and (needsSecure or ((resource.meta <> nil) and not resource.meta.securityList.IsEmpty)) then
    raise ERestfulException.Create('TFHIRNativeStorageService.SeeResource', 400, itBusinessRule, 'Resources of type '+CODES_TFHIRResourceType[resource.ResourceType]+' are not allowed to have a security label on them', lang);

  if resource.ResourceType = frtValueSet then
  begin
    vs := TFHIRValueSet(resource);
    vs.Tags['tracker'] := inttostr(TrackValueSet(vs.url, conn, reload));
    ServerContext.TerminologyServer.SeeTerminologyResource(resource)
  end
  else if resource.ResourceType in [frtConceptMap, frtCodeSystem] then
    ServerContext.TerminologyServer.SeeTerminologyResource(resource)
  else if resource.ResourceType = frtStructureDefinition then
    ServerContext.ValidatorContext.seeResource(resource as TFhirStructureDefinition)
  else if resource.ResourceType = frtQuestionnaire then
  {$IFNDEF NO_JS}
    ServerContext.ValidatorContext.seeResource(resource as TFhirQuestionnaire)
  else if resource.ResourceType = frtEventDefinition then
    ServerContext.EventScriptRegistry.seeResource(Factory.wrapEventDefinition(resource.Link)){$ENDIF};

  if created then
    ServerContext.SubscriptionManager.SeeResource(key, vkey, pvkey, id, subscriptionCreate, resource, conn, reload, session)
  else
    ServerContext.SubscriptionManager.SeeResource(key, vkey, pvkey, id, subscriptionUpdate, resource, conn, reload, session);

  FLock.Lock('SeeResource');
  try
    ServerContext.QuestionnaireCache.clear(resource.fhirType, id);
    if resource.ResourceType = frtValueSet then
      ServerContext.QuestionnaireCache.clearVS(TFHIRValueSet(resource).url);
    if resource.ResourceType = frtStructureMap then
      ServerContext.seeMap(factory.wrapStructureMap(resource.link));
    if resource.ResourceType = frtNamingSystem then
      ServerContext.seeNamingSystem(key, factory.wrapNamingSystem(resource.link));
    if not reload and (resource.ResourceType = frtObservation) then
      StoreObservation(conn, key);
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRNativeStorageServiceR5.SetupRecording(session : TFHIRSession);
begin
  session.TestScript := TFhirTestScript5.create(TFhirTestScript.Create);
end;

function TFHIRNativeStorageServiceR5.vc: TFHIRServerWorkerContextR5;
begin
  result := ServerContext.ValidatorContext.Link as TFHIRServerWorkerContextR5;
end;

procedure TFHIRNativeStorageServiceR5.Yield(op: TFHIROperationEngine; e: Exception);
begin
  try
    if e = nil then
      TFHIRNativeOperationEngine(op).Connection.Release
    else
      TFHIRNativeOperationEngine(op).Connection.Error(e);
  finally
    op.Free;
  end;
end;

end.
