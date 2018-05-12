unit FHIR.R2.Factory;

{$I fhir.inc}

interface

uses
  FHIR.Ucum.IFace,
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.Base.Validator, FHIR.Base.Narrative, FHIR.Base.Factory, FHIR.Base.PathEngine,
  FHIR.XVersion.Resources,
  FHIR.Client.Base, FHIR.Client.Threaded;

type
  TFHIRFactoryR2 = class (TFHIRFactory)
  public
    function version : TFHIRVersion; override;
    function versionString : String; override;
    function description : String; override;
    function makeParser(worker : TFHIRWorkerContextV; format : TFHIRFormat; lang : String) : TFHIRParser; override;
    function makeComposer(worker : TFHIRWorkerContextV; format : TFHIRFormat; lang : String; style: TFHIROutputStyle) : TFHIRComposer; override;
    function makeValidator(worker : TFHIRWorkerContextV) : TFHIRValidatorV; override;
    function makeGenerator(worker : TFHIRWorkerContextV) : TFHIRNarrativeGeneratorBase; override;
    function makePathEngine(worker : TFHIRWorkerContextV; ucum : TUcumServiceInterface) : TFHIRPathEngineV; override;
    function makeClientHTTP(worker : TFHIRWorkerContextV; url : String; fmt : TFHIRFormat; timeout : cardinal; proxy : String) : TFhirClientV; overload; override;
    function makeClientThreaded(worker : TFHIRWorkerContextV; internal : TFhirClientV; event : TThreadManagementEvent) : TFhirClientV; overload; override;

    function makeByName(const name : String) : TFHIRObject; override;
    function makeBoolean(b : boolean): TFHIRObject; override;
    function makeCode(s : string) : TFHIRObject; override;
    function makeString(s : string) : TFHIRObject; override;
    function makeInteger(s : string) : TFHIRObject; override;
    function makeDecimal(s : string) : TFHIRObject; override;
    function makeBase64Binary(s : string) : TFHIRObject; override;
    function makeParameters : TFHIRParametersW; override;
    function wrapCapabilityStatement(r : TFHIRResourceV) : TFHIRCapabilityStatementW; override;
  end;

implementation

uses
  Soap.EncdDecd,
  FHIR.Client.HTTP,
  FHIR.R2.Types, FHIR.R2.Resources, FHIR.R2.Parser, FHIR.R2.Context, FHIR.R2.Validator, FHIR.R2.Narrative,
  FHIR.R2.PathEngine, FHIR.R2.Constants, FHIR.R2.Client, FHIR.R2.Common;

{ TFHIRFactoryR2 }

function TFHIRFactoryR2.description: String;
begin
  result := 'R2 ('+FHIR_GENERATED_VERSION+')';
end;

function TFHIRFactoryR2.makeBase64Binary(s: string): TFHIRObject;
begin
  result := TFhirBase64Binary.Create(decodeBase64(s));
end;

function TFHIRFactoryR2.makeBoolean(b: boolean): TFHIRObject;
begin
  result := TFHIRBoolean.create(b);
end;

function TFHIRFactoryR2.makeClientHTTP(worker: TFHIRWorkerContextV; url: String; fmt : TFHIRFormat; timeout: cardinal; proxy: String): TFhirClientV;
var
  http : TFHIRHTTPCommunicator;
begin
  http := TFHIRHTTPCommunicator.Create(url);
  try
    http.timeout := timeout;
    http.proxy := proxy;
    result := TFhirClient2.create(worker, 'en', http.link);
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

function TFHIRFactoryR2.makeClientThreaded(worker: TFHIRWorkerContextV;
  internal: TFhirClientV; event: TThreadManagementEvent): TFhirClientV;
begin

end;

function TFHIRFactoryR2.makeCode(s: string): TFHIRObject;
begin
  result := TFhirCode.Create(s);
end;

function TFHIRFactoryR2.makeComposer(worker: TFHIRWorkerContextV; format: TFHIRFormat; lang: String; style: TFHIROutputStyle): TFHIRComposer;
begin
  result := TFHIRParsers2.composer(worker as TFHIRWorkerContext, format, lang, style);
end;

function TFHIRFactoryR2.makeDecimal(s: string): TFHIRObject;
begin
  result := TFhirDecimal.Create(s);
end;

function TFHIRFactoryR2.makeGenerator(worker: TFHIRWorkerContextV): TFHIRNarrativeGeneratorBase;
begin
  result := TFHIRNarrativeGenerator.create(worker);
end;

function TFHIRFactoryR2.makeInteger(s: string): TFHIRObject;
begin
  result := TFhirInteger.Create(s);
end;

function TFHIRFactoryR2.makeParameters: TFHIRParametersW;
begin
  result := TFHIRParameters2.Create(TFhirParameters.create);
end;

function TFHIRFactoryR2.makeParser(worker: TFHIRWorkerContextV; format: TFHIRFormat; lang: String): TFHIRParser;
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

function TFHIRFactoryR2.makeValidator(worker: TFHIRWorkerContextV): TFHIRValidatorV;
begin
  result := TFHIRValidator2.Create(worker as TFHIRWorkerContext);
end;

function TFHIRFactoryR2.version: TFHIRVersion;
begin
  result := fhirVersionRelease2;
end;

function TFHIRFactoryR2.versionString: String;
begin
  result := FHIR_GENERATED_VERSION;
end;

function TFHIRFactoryR2.wrapCapabilityStatement(r: TFHIRResourceV): TFHIRCapabilityStatementW;
begin
  result := TFHIRCapabilityStatement2.create(r);
end;

function TFHIRFactoryR2.makeByName(const name : String) : TFHIRObject;
begin
  if name = 'enum' then
    result := TFHIREnum.create()
  else if name = 'dateTime' then
    result := TFHIRDateTime.create()
  else if name = 'date' then
    result := TFHIRDate.create()
  else if name = 'string' then
    result := TFHIRString.create()
  else if name = 'integer' then
    result := TFHIRInteger.create()
  else if name = 'uri' then
    result := TFHIRUri.create()
  else if name = 'instant' then
    result := TFHIRInstant.create()
  else if name = 'boolean' then
    result := TFHIRBoolean.create()
  else if name = 'base64Binary' then
    result := TFHIRBase64Binary.create()
  else if name = 'time' then
    result := TFHIRTime.create()
  else if name = 'decimal' then
    result := TFHIRDecimal.create()
  else if name = 'code' then
    result := TFHIRCode.create()
  else if name = 'oid' then
    result := TFHIROid.create()
  else if name = 'uuid' then
    result := TFHIRUuid.create()
  else if name = 'markdown' then
    result := TFHIRMarkdown.create()
  else if name = 'unsignedInt' then
    result := TFHIRUnsignedInt.create()
  else if name = 'id' then
    result := TFHIRId.create()
  else if name = 'positiveInt' then
    result := TFHIRPositiveInt.create()
{$IFDEF FHIR_PARAMETERS}
  else if name = 'Parameters.parameter' then
    result := TFHIRParametersParameter.create()
  else if name = 'Parameters' then
    result := TFHIRParameters.create()
{$ENDIF FHIR_PARAMETERS}
  else if name = 'Extension' then
    result := TFHIRExtension.create()
  else if name = 'Narrative' then
    result := TFHIRNarrative.create()
  else if name = 'Identifier' then
    result := TFHIRIdentifier.create()
  else if name = 'Coding' then
    result := TFHIRCoding.create()
  else if name = 'Reference' then
    result := TFHIRReference.create()
  else if name = 'Signature' then
    result := TFHIRSignature.create()
  else if name = 'SampledData' then
    result := TFHIRSampledData.create()
  else if name = 'Period' then
    result := TFHIRPeriod.create()
  else if name = 'Quantity' then
    result := TFHIRQuantity.create()
  else if name = 'Attachment' then
    result := TFHIRAttachment.create()
  else if name = 'Ratio' then
    result := TFHIRRatio.create()
  else if name = 'Range' then
    result := TFHIRRange.create()
  else if name = 'Annotation' then
    result := TFHIRAnnotation.create()
  else if name = 'CodeableConcept' then
    result := TFHIRCodeableConcept.create()
  else if name = 'HumanName' then
    result := TFHIRHumanName.create()
  else if name = 'Meta' then
    result := TFHIRMeta.create()
  else if name = 'ContactPoint' then
    result := TFHIRContactPoint.create()
  else if name = 'Address' then
    result := TFHIRAddress.create()
  else if name = 'ElementDefinition.slicing' then
    result := TFHIRElementDefinitionSlicing.create()
  else if name = 'ElementDefinition.base' then
    result := TFHIRElementDefinitionBase.create()
  else if name = 'ElementDefinition.type' then
    result := TFHIRElementDefinitionType.create()
  else if name = 'ElementDefinition.constraint' then
    result := TFHIRElementDefinitionConstraint.create()
  else if name = 'ElementDefinition.binding' then
    result := TFHIRElementDefinitionBinding.create()
  else if name = 'ElementDefinition.mapping' then
    result := TFHIRElementDefinitionMapping.create()
  else if name = 'ElementDefinition' then
    result := TFHIRElementDefinition.create()
  else if name = 'Timing.repeat' then
    result := TFHIRTimingRepeat.create()
  else if name = 'Timing' then
    result := TFHIRTiming.create()
{$IFDEF FHIR_ACCOUNT}
  else if name = 'Account' then
    result := TFHIRAccount.create()
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  else if name = 'AllergyIntolerance.reaction' then
    result := TFHIRAllergyIntoleranceReaction.create()
  else if name = 'AllergyIntolerance' then
    result := TFHIRAllergyIntolerance.create()
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  else if name = 'Appointment.participant' then
    result := TFHIRAppointmentParticipant.create()
  else if name = 'Appointment' then
    result := TFHIRAppointment.create()
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  else if name = 'AppointmentResponse' then
    result := TFHIRAppointmentResponse.create()
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_AUDITEVENT}
  else if name = 'AuditEvent.event' then
    result := TFHIRAuditEventEvent.create()
  else if name = 'AuditEvent.participant' then
    result := TFHIRAuditEventParticipant.create()
  else if name = 'AuditEvent.participant.network' then
    result := TFHIRAuditEventParticipantNetwork.create()
  else if name = 'AuditEvent.source' then
    result := TFHIRAuditEventSource.create()
  else if name = 'AuditEvent.object' then
    result := TFHIRAuditEventObject.create()
  else if name = 'AuditEvent.object.detail' then
    result := TFHIRAuditEventObjectDetail.create()
  else if name = 'AuditEvent' then
    result := TFHIRAuditEvent.create()
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BASIC}
  else if name = 'Basic' then
    result := TFHIRBasic.create()
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BINARY}
  else if name = 'Binary' then
    result := TFHIRBinary.create()
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BODYSITE}
  else if name = 'BodySite' then
    result := TFHIRBodySite.create()
{$ENDIF FHIR_BODYSITE}
{$IFDEF FHIR_BUNDLE}
  else if name = 'Bundle.link' then
    result := TFHIRBundleLink.create()
  else if name = 'Bundle.entry' then
    result := TFHIRBundleEntry.create()
  else if name = 'Bundle.entry.search' then
    result := TFHIRBundleEntrySearch.create()
  else if name = 'Bundle.entry.request' then
    result := TFHIRBundleEntryRequest.create()
  else if name = 'Bundle.entry.response' then
    result := TFHIRBundleEntryResponse.create()
  else if name = 'Bundle' then
    result := TFHIRBundle.create()
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CAREPLAN}
  else if name = 'CarePlan.relatedPlan' then
    result := TFHIRCarePlanRelatedPlan.create()
  else if name = 'CarePlan.participant' then
    result := TFHIRCarePlanParticipant.create()
  else if name = 'CarePlan.activity' then
    result := TFHIRCarePlanActivity.create()
  else if name = 'CarePlan.activity.detail' then
    result := TFHIRCarePlanActivityDetail.create()
  else if name = 'CarePlan' then
    result := TFHIRCarePlan.create()
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CLAIM}
  else if name = 'Claim.payee' then
    result := TFHIRClaimPayee.create()
  else if name = 'Claim.diagnosis' then
    result := TFHIRClaimDiagnosis.create()
  else if name = 'Claim.coverage' then
    result := TFHIRClaimCoverage.create()
  else if name = 'Claim.item' then
    result := TFHIRClaimItem.create()
  else if name = 'Claim.item.detail' then
    result := TFHIRClaimItemDetail.create()
  else if name = 'Claim.item.detail.subDetail' then
    result := TFHIRClaimItemDetailSubDetail.create()
  else if name = 'Claim.item.prosthesis' then
    result := TFHIRClaimItemProsthesis.create()
  else if name = 'Claim.missingTeeth' then
    result := TFHIRClaimMissingTeeth.create()
  else if name = 'Claim' then
    result := TFHIRClaim.create()
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  else if name = 'ClaimResponse.item' then
    result := TFHIRClaimResponseItem.create()
  else if name = 'ClaimResponse.item.adjudication' then
    result := TFHIRClaimResponseItemAdjudication.create()
  else if name = 'ClaimResponse.item.detail' then
    result := TFHIRClaimResponseItemDetail.create()
  else if name = 'ClaimResponse.item.detail.adjudication' then
    result := TFHIRClaimResponseItemDetailAdjudication.create()
  else if name = 'ClaimResponse.item.detail.subDetail' then
    result := TFHIRClaimResponseItemDetailSubDetail.create()
  else if name = 'ClaimResponse.item.detail.subDetail.adjudication' then
    result := TFHIRClaimResponseItemDetailSubDetailAdjudication.create()
  else if name = 'ClaimResponse.addItem' then
    result := TFHIRClaimResponseAddItem.create()
  else if name = 'ClaimResponse.addItem.adjudication' then
    result := TFHIRClaimResponseAddItemAdjudication.create()
  else if name = 'ClaimResponse.addItem.detail' then
    result := TFHIRClaimResponseAddItemDetail.create()
  else if name = 'ClaimResponse.addItem.detail.adjudication' then
    result := TFHIRClaimResponseAddItemDetailAdjudication.create()
  else if name = 'ClaimResponse.error' then
    result := TFHIRClaimResponseError.create()
  else if name = 'ClaimResponse.note' then
    result := TFHIRClaimResponseNote.create()
  else if name = 'ClaimResponse.coverage' then
    result := TFHIRClaimResponseCoverage.create()
  else if name = 'ClaimResponse' then
    result := TFHIRClaimResponse.create()
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  else if name = 'ClinicalImpression.investigations' then
    result := TFHIRClinicalImpressionInvestigations.create()
  else if name = 'ClinicalImpression.finding' then
    result := TFHIRClinicalImpressionFinding.create()
  else if name = 'ClinicalImpression.ruledOut' then
    result := TFHIRClinicalImpressionRuledOut.create()
  else if name = 'ClinicalImpression' then
    result := TFHIRClinicalImpression.create()
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_COMMUNICATION}
  else if name = 'Communication.payload' then
    result := TFHIRCommunicationPayload.create()
  else if name = 'Communication' then
    result := TFHIRCommunication.create()
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  else if name = 'CommunicationRequest.payload' then
    result := TFHIRCommunicationRequestPayload.create()
  else if name = 'CommunicationRequest' then
    result := TFHIRCommunicationRequest.create()
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPOSITION}
  else if name = 'Composition.attester' then
    result := TFHIRCompositionAttester.create()
  else if name = 'Composition.event' then
    result := TFHIRCompositionEvent.create()
  else if name = 'Composition.section' then
    result := TFHIRCompositionSection.create()
  else if name = 'Composition' then
    result := TFHIRComposition.create()
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  else if name = 'ConceptMap.contact' then
    result := TFHIRConceptMapContact.create()
  else if name = 'ConceptMap.element' then
    result := TFHIRConceptMapElement.create()
  else if name = 'ConceptMap.element.target' then
    result := TFHIRConceptMapElementTarget.create()
  else if name = 'ConceptMap.element.target.dependsOn' then
    result := TFHIRConceptMapElementTargetDependsOn.create()
  else if name = 'ConceptMap' then
    result := TFHIRConceptMap.create()
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_CONDITION}
  else if name = 'Condition.stage' then
    result := TFHIRConditionStage.create()
  else if name = 'Condition.evidence' then
    result := TFHIRConditionEvidence.create()
  else if name = 'Condition' then
    result := TFHIRCondition.create()
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_CONFORMANCE}
  else if name = 'Conformance.contact' then
    result := TFHIRConformanceContact.create()
  else if name = 'Conformance.software' then
    result := TFHIRConformanceSoftware.create()
  else if name = 'Conformance.implementation' then
    result := TFHIRConformanceImplementation.create()
  else if name = 'Conformance.rest' then
    result := TFHIRConformanceRest.create()
  else if name = 'Conformance.rest.security' then
    result := TFHIRConformanceRestSecurity.create()
  else if name = 'Conformance.rest.security.certificate' then
    result := TFHIRConformanceRestSecurityCertificate.create()
  else if name = 'Conformance.rest.resource' then
    result := TFHIRConformanceRestResource.create()
  else if name = 'Conformance.rest.resource.interaction' then
    result := TFHIRConformanceRestResourceInteraction.create()
  else if name = 'Conformance.rest.resource.searchParam' then
    result := TFHIRConformanceRestResourceSearchParam.create()
  else if name = 'Conformance.rest.interaction' then
    result := TFHIRConformanceRestInteraction.create()
  else if name = 'Conformance.rest.operation' then
    result := TFHIRConformanceRestOperation.create()
  else if name = 'Conformance.messaging' then
    result := TFHIRConformanceMessaging.create()
  else if name = 'Conformance.messaging.endpoint' then
    result := TFHIRConformanceMessagingEndpoint.create()
  else if name = 'Conformance.messaging.event' then
    result := TFHIRConformanceMessagingEvent.create()
  else if name = 'Conformance.document' then
    result := TFHIRConformanceDocument.create()
  else if name = 'Conformance' then
    result := TFHIRConformance.create()
{$ENDIF FHIR_CONFORMANCE}
{$IFDEF FHIR_CONTRACT}
  else if name = 'Contract.actor' then
    result := TFHIRContractActor.create()
  else if name = 'Contract.valuedItem' then
    result := TFHIRContractValuedItem.create()
  else if name = 'Contract.signer' then
    result := TFHIRContractSigner.create()
  else if name = 'Contract.term' then
    result := TFHIRContractTerm.create()
  else if name = 'Contract.term.actor' then
    result := TFHIRContractTermActor.create()
  else if name = 'Contract.term.valuedItem' then
    result := TFHIRContractTermValuedItem.create()
  else if name = 'Contract.friendly' then
    result := TFHIRContractFriendly.create()
  else if name = 'Contract.legal' then
    result := TFHIRContractLegal.create()
  else if name = 'Contract.rule' then
    result := TFHIRContractRule.create()
  else if name = 'Contract' then
    result := TFHIRContract.create()
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_COVERAGE}
  else if name = 'Coverage' then
    result := TFHIRCoverage.create()
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_DATAELEMENT}
  else if name = 'DataElement.contact' then
    result := TFHIRDataElementContact.create()
  else if name = 'DataElement.mapping' then
    result := TFHIRDataElementMapping.create()
  else if name = 'DataElement' then
    result := TFHIRDataElement.create()
{$ENDIF FHIR_DATAELEMENT}
{$IFDEF FHIR_DETECTEDISSUE}
  else if name = 'DetectedIssue.mitigation' then
    result := TFHIRDetectedIssueMitigation.create()
  else if name = 'DetectedIssue' then
    result := TFHIRDetectedIssue.create()
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  else if name = 'Device' then
    result := TFHIRDevice.create()
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICECOMPONENT}
  else if name = 'DeviceComponent.productionSpecification' then
    result := TFHIRDeviceComponentProductionSpecification.create()
  else if name = 'DeviceComponent' then
    result := TFHIRDeviceComponent.create()
{$ENDIF FHIR_DEVICECOMPONENT}
{$IFDEF FHIR_DEVICEMETRIC}
  else if name = 'DeviceMetric.calibration' then
    result := TFHIRDeviceMetricCalibration.create()
  else if name = 'DeviceMetric' then
    result := TFHIRDeviceMetric.create()
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEUSEREQUEST}
  else if name = 'DeviceUseRequest' then
    result := TFHIRDeviceUseRequest.create()
{$ENDIF FHIR_DEVICEUSEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  else if name = 'DeviceUseStatement' then
    result := TFHIRDeviceUseStatement.create()
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICORDER}
  else if name = 'DiagnosticOrder.event' then
    result := TFHIRDiagnosticOrderEvent.create()
  else if name = 'DiagnosticOrder.item' then
    result := TFHIRDiagnosticOrderItem.create()
  else if name = 'DiagnosticOrder' then
    result := TFHIRDiagnosticOrder.create()
{$ENDIF FHIR_DIAGNOSTICORDER}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  else if name = 'DiagnosticReport.image' then
    result := TFHIRDiagnosticReportImage.create()
  else if name = 'DiagnosticReport' then
    result := TFHIRDiagnosticReport.create()
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  else if name = 'DocumentManifest.content' then
    result := TFHIRDocumentManifestContent.create()
  else if name = 'DocumentManifest.related' then
    result := TFHIRDocumentManifestRelated.create()
  else if name = 'DocumentManifest' then
    result := TFHIRDocumentManifest.create()
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  else if name = 'DocumentReference.relatesTo' then
    result := TFHIRDocumentReferenceRelatesTo.create()
  else if name = 'DocumentReference.content' then
    result := TFHIRDocumentReferenceContent.create()
  else if name = 'DocumentReference.context' then
    result := TFHIRDocumentReferenceContext.create()
  else if name = 'DocumentReference.context.related' then
    result := TFHIRDocumentReferenceContextRelated.create()
  else if name = 'DocumentReference' then
    result := TFHIRDocumentReference.create()
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_ELIGIBILITYREQUEST}
  else if name = 'EligibilityRequest' then
    result := TFHIREligibilityRequest.create()
{$ENDIF FHIR_ELIGIBILITYREQUEST}
{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  else if name = 'EligibilityResponse' then
    result := TFHIREligibilityResponse.create()
{$ENDIF FHIR_ELIGIBILITYRESPONSE}
{$IFDEF FHIR_ENCOUNTER}
  else if name = 'Encounter.statusHistory' then
    result := TFHIREncounterStatusHistory.create()
  else if name = 'Encounter.participant' then
    result := TFHIREncounterParticipant.create()
  else if name = 'Encounter.hospitalization' then
    result := TFHIREncounterHospitalization.create()
  else if name = 'Encounter.location' then
    result := TFHIREncounterLocation.create()
  else if name = 'Encounter' then
    result := TFHIREncounter.create()
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  else if name = 'EnrollmentRequest' then
    result := TFHIREnrollmentRequest.create()
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  else if name = 'EnrollmentResponse' then
    result := TFHIREnrollmentResponse.create()
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EPISODEOFCARE}
  else if name = 'EpisodeOfCare.statusHistory' then
    result := TFHIREpisodeOfCareStatusHistory.create()
  else if name = 'EpisodeOfCare.careTeam' then
    result := TFHIREpisodeOfCareCareTeam.create()
  else if name = 'EpisodeOfCare' then
    result := TFHIREpisodeOfCare.create()
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  else if name = 'ExplanationOfBenefit' then
    result := TFHIRExplanationOfBenefit.create()
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  else if name = 'FamilyMemberHistory.condition' then
    result := TFHIRFamilyMemberHistoryCondition.create()
  else if name = 'FamilyMemberHistory' then
    result := TFHIRFamilyMemberHistory.create()
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  else if name = 'Flag' then
    result := TFHIRFlag.create()
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  else if name = 'Goal.outcome' then
    result := TFHIRGoalOutcome.create()
  else if name = 'Goal' then
    result := TFHIRGoal.create()
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GROUP}
  else if name = 'Group.characteristic' then
    result := TFHIRGroupCharacteristic.create()
  else if name = 'Group.member' then
    result := TFHIRGroupMember.create()
  else if name = 'Group' then
    result := TFHIRGroup.create()
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_HEALTHCARESERVICE}
  else if name = 'HealthcareService.serviceType' then
    result := TFHIRHealthcareServiceServiceType.create()
  else if name = 'HealthcareService.availableTime' then
    result := TFHIRHealthcareServiceAvailableTime.create()
  else if name = 'HealthcareService.notAvailable' then
    result := TFHIRHealthcareServiceNotAvailable.create()
  else if name = 'HealthcareService' then
    result := TFHIRHealthcareService.create()
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGOBJECTSELECTION}
  else if name = 'ImagingObjectSelection.study' then
    result := TFHIRImagingObjectSelectionStudy.create()
  else if name = 'ImagingObjectSelection.study.series' then
    result := TFHIRImagingObjectSelectionStudySeries.create()
  else if name = 'ImagingObjectSelection.study.series.instance' then
    result := TFHIRImagingObjectSelectionStudySeriesInstance.create()
  else if name = 'ImagingObjectSelection.study.series.instance.frames' then
    result := TFHIRImagingObjectSelectionStudySeriesInstanceFrames.create()
  else if name = 'ImagingObjectSelection' then
    result := TFHIRImagingObjectSelection.create()
{$ENDIF FHIR_IMAGINGOBJECTSELECTION}
{$IFDEF FHIR_IMAGINGSTUDY}
  else if name = 'ImagingStudy.series' then
    result := TFHIRImagingStudySeries.create()
  else if name = 'ImagingStudy.series.instance' then
    result := TFHIRImagingStudySeriesInstance.create()
  else if name = 'ImagingStudy' then
    result := TFHIRImagingStudy.create()
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  else if name = 'Immunization.explanation' then
    result := TFHIRImmunizationExplanation.create()
  else if name = 'Immunization.reaction' then
    result := TFHIRImmunizationReaction.create()
  else if name = 'Immunization.vaccinationProtocol' then
    result := TFHIRImmunizationVaccinationProtocol.create()
  else if name = 'Immunization' then
    result := TFHIRImmunization.create()
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  else if name = 'ImmunizationRecommendation.recommendation' then
    result := TFHIRImmunizationRecommendationRecommendation.create()
  else if name = 'ImmunizationRecommendation.recommendation.dateCriterion' then
    result := TFHIRImmunizationRecommendationRecommendationDateCriterion.create()
  else if name = 'ImmunizationRecommendation.recommendation.protocol' then
    result := TFHIRImmunizationRecommendationRecommendationProtocol.create()
  else if name = 'ImmunizationRecommendation' then
    result := TFHIRImmunizationRecommendation.create()
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  else if name = 'ImplementationGuide.contact' then
    result := TFHIRImplementationGuideContact.create()
  else if name = 'ImplementationGuide.dependency' then
    result := TFHIRImplementationGuideDependency.create()
  else if name = 'ImplementationGuide.package' then
    result := TFHIRImplementationGuidePackage.create()
  else if name = 'ImplementationGuide.package.resource' then
    result := TFHIRImplementationGuidePackageResource.create()
  else if name = 'ImplementationGuide.global' then
    result := TFHIRImplementationGuideGlobal.create()
  else if name = 'ImplementationGuide.page' then
    result := TFHIRImplementationGuidePage.create()
  else if name = 'ImplementationGuide' then
    result := TFHIRImplementationGuide.create()
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_LIST}
  else if name = 'List.entry' then
    result := TFHIRListEntry.create()
  else if name = 'List' then
    result := TFHIRList.create()
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_LOCATION}
  else if name = 'Location.position' then
    result := TFHIRLocationPosition.create()
  else if name = 'Location' then
    result := TFHIRLocation.create()
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_MEDIA}
  else if name = 'Media' then
    result := TFHIRMedia.create()
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATION}
  else if name = 'Medication.product' then
    result := TFHIRMedicationProduct.create()
  else if name = 'Medication.product.ingredient' then
    result := TFHIRMedicationProductIngredient.create()
  else if name = 'Medication.product.batch' then
    result := TFHIRMedicationProductBatch.create()
  else if name = 'Medication.package' then
    result := TFHIRMedicationPackage.create()
  else if name = 'Medication.package.content' then
    result := TFHIRMedicationPackageContent.create()
  else if name = 'Medication' then
    result := TFHIRMedication.create()
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  else if name = 'MedicationAdministration.dosage' then
    result := TFHIRMedicationAdministrationDosage.create()
  else if name = 'MedicationAdministration' then
    result := TFHIRMedicationAdministration.create()
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  else if name = 'MedicationDispense.dosageInstruction' then
    result := TFHIRMedicationDispenseDosageInstruction.create()
  else if name = 'MedicationDispense.substitution' then
    result := TFHIRMedicationDispenseSubstitution.create()
  else if name = 'MedicationDispense' then
    result := TFHIRMedicationDispense.create()
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONORDER}
  else if name = 'MedicationOrder.dosageInstruction' then
    result := TFHIRMedicationOrderDosageInstruction.create()
  else if name = 'MedicationOrder.dispenseRequest' then
    result := TFHIRMedicationOrderDispenseRequest.create()
  else if name = 'MedicationOrder.substitution' then
    result := TFHIRMedicationOrderSubstitution.create()
  else if name = 'MedicationOrder' then
    result := TFHIRMedicationOrder.create()
{$ENDIF FHIR_MEDICATIONORDER}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  else if name = 'MedicationStatement.dosage' then
    result := TFHIRMedicationStatementDosage.create()
  else if name = 'MedicationStatement' then
    result := TFHIRMedicationStatement.create()
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_MESSAGEHEADER}
  else if name = 'MessageHeader.response' then
    result := TFHIRMessageHeaderResponse.create()
  else if name = 'MessageHeader.source' then
    result := TFHIRMessageHeaderSource.create()
  else if name = 'MessageHeader.destination' then
    result := TFHIRMessageHeaderDestination.create()
  else if name = 'MessageHeader' then
    result := TFHIRMessageHeader.create()
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_NAMINGSYSTEM}
  else if name = 'NamingSystem.contact' then
    result := TFHIRNamingSystemContact.create()
  else if name = 'NamingSystem.uniqueId' then
    result := TFHIRNamingSystemUniqueId.create()
  else if name = 'NamingSystem' then
    result := TFHIRNamingSystem.create()
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_NUTRITIONORDER}
  else if name = 'NutritionOrder.oralDiet' then
    result := TFHIRNutritionOrderOralDiet.create()
  else if name = 'NutritionOrder.oralDiet.nutrient' then
    result := TFHIRNutritionOrderOralDietNutrient.create()
  else if name = 'NutritionOrder.oralDiet.texture' then
    result := TFHIRNutritionOrderOralDietTexture.create()
  else if name = 'NutritionOrder.supplement' then
    result := TFHIRNutritionOrderSupplement.create()
  else if name = 'NutritionOrder.enteralFormula' then
    result := TFHIRNutritionOrderEnteralFormula.create()
  else if name = 'NutritionOrder.enteralFormula.administration' then
    result := TFHIRNutritionOrderEnteralFormulaAdministration.create()
  else if name = 'NutritionOrder' then
    result := TFHIRNutritionOrder.create()
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_OBSERVATION}
  else if name = 'Observation.referenceRange' then
    result := TFHIRObservationReferenceRange.create()
  else if name = 'Observation.related' then
    result := TFHIRObservationRelated.create()
  else if name = 'Observation.component' then
    result := TFHIRObservationComponent.create()
  else if name = 'Observation' then
    result := TFHIRObservation.create()
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  else if name = 'OperationDefinition.contact' then
    result := TFHIROperationDefinitionContact.create()
  else if name = 'OperationDefinition.parameter' then
    result := TFHIROperationDefinitionParameter.create()
  else if name = 'OperationDefinition.parameter.binding' then
    result := TFHIROperationDefinitionParameterBinding.create()
  else if name = 'OperationDefinition' then
    result := TFHIROperationDefinition.create()
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  else if name = 'OperationOutcome.issue' then
    result := TFHIROperationOutcomeIssue.create()
  else if name = 'OperationOutcome' then
    result := TFHIROperationOutcome.create()
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_ORDER}
  else if name = 'Order.when' then
    result := TFHIROrderWhen.create()
  else if name = 'Order' then
    result := TFHIROrder.create()
{$ENDIF FHIR_ORDER}
{$IFDEF FHIR_ORDERRESPONSE}
  else if name = 'OrderResponse' then
    result := TFHIROrderResponse.create()
{$ENDIF FHIR_ORDERRESPONSE}
{$IFDEF FHIR_ORGANIZATION}
  else if name = 'Organization.contact' then
    result := TFHIROrganizationContact.create()
  else if name = 'Organization' then
    result := TFHIROrganization.create()
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_PATIENT}
  else if name = 'Patient.contact' then
    result := TFHIRPatientContact.create()
  else if name = 'Patient.animal' then
    result := TFHIRPatientAnimal.create()
  else if name = 'Patient.communication' then
    result := TFHIRPatientCommunication.create()
  else if name = 'Patient.link' then
    result := TFHIRPatientLink.create()
  else if name = 'Patient' then
    result := TFHIRPatient.create()
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PAYMENTNOTICE}
  else if name = 'PaymentNotice' then
    result := TFHIRPaymentNotice.create()
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  else if name = 'PaymentReconciliation.detail' then
    result := TFHIRPaymentReconciliationDetail.create()
  else if name = 'PaymentReconciliation.note' then
    result := TFHIRPaymentReconciliationNote.create()
  else if name = 'PaymentReconciliation' then
    result := TFHIRPaymentReconciliation.create()
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERSON}
  else if name = 'Person.link' then
    result := TFHIRPersonLink.create()
  else if name = 'Person' then
    result := TFHIRPerson.create()
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PRACTITIONER}
  else if name = 'Practitioner.practitionerRole' then
    result := TFHIRPractitionerPractitionerRole.create()
  else if name = 'Practitioner.qualification' then
    result := TFHIRPractitionerQualification.create()
  else if name = 'Practitioner' then
    result := TFHIRPractitioner.create()
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PROCEDURE}
  else if name = 'Procedure.performer' then
    result := TFHIRProcedurePerformer.create()
  else if name = 'Procedure.focalDevice' then
    result := TFHIRProcedureFocalDevice.create()
  else if name = 'Procedure' then
    result := TFHIRProcedure.create()
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROCEDUREREQUEST}
  else if name = 'ProcedureRequest' then
    result := TFHIRProcedureRequest.create()
{$ENDIF FHIR_PROCEDUREREQUEST}
{$IFDEF FHIR_PROCESSREQUEST}
  else if name = 'ProcessRequest.item' then
    result := TFHIRProcessRequestItem.create()
  else if name = 'ProcessRequest' then
    result := TFHIRProcessRequest.create()
{$ENDIF FHIR_PROCESSREQUEST}
{$IFDEF FHIR_PROCESSRESPONSE}
  else if name = 'ProcessResponse.notes' then
    result := TFHIRProcessResponseNotes.create()
  else if name = 'ProcessResponse' then
    result := TFHIRProcessResponse.create()
{$ENDIF FHIR_PROCESSRESPONSE}
{$IFDEF FHIR_PROVENANCE}
  else if name = 'Provenance.agent' then
    result := TFHIRProvenanceAgent.create()
  else if name = 'Provenance.agent.relatedAgent' then
    result := TFHIRProvenanceAgentRelatedAgent.create()
  else if name = 'Provenance.entity' then
    result := TFHIRProvenanceEntity.create()
  else if name = 'Provenance' then
    result := TFHIRProvenance.create()
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  else if name = 'Questionnaire.group' then
    result := TFHIRQuestionnaireGroup.create()
  else if name = 'Questionnaire.group.question' then
    result := TFHIRQuestionnaireGroupQuestion.create()
  else if name = 'Questionnaire' then
    result := TFHIRQuestionnaire.create()
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  else if name = 'QuestionnaireResponse.group' then
    result := TFHIRQuestionnaireResponseGroup.create()
  else if name = 'QuestionnaireResponse.group.question' then
    result := TFHIRQuestionnaireResponseGroupQuestion.create()
  else if name = 'QuestionnaireResponse.group.question.answer' then
    result := TFHIRQuestionnaireResponseGroupQuestionAnswer.create()
  else if name = 'QuestionnaireResponse' then
    result := TFHIRQuestionnaireResponse.create()
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REFERRALREQUEST}
  else if name = 'ReferralRequest' then
    result := TFHIRReferralRequest.create()
{$ENDIF FHIR_REFERRALREQUEST}
{$IFDEF FHIR_RELATEDPERSON}
  else if name = 'RelatedPerson' then
    result := TFHIRRelatedPerson.create()
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_RISKASSESSMENT}
  else if name = 'RiskAssessment.prediction' then
    result := TFHIRRiskAssessmentPrediction.create()
  else if name = 'RiskAssessment' then
    result := TFHIRRiskAssessment.create()
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_SCHEDULE}
  else if name = 'Schedule' then
    result := TFHIRSchedule.create()
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SEARCHPARAMETER}
  else if name = 'SearchParameter.contact' then
    result := TFHIRSearchParameterContact.create()
  else if name = 'SearchParameter' then
    result := TFHIRSearchParameter.create()
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SLOT}
  else if name = 'Slot' then
    result := TFHIRSlot.create()
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  else if name = 'Specimen.collection' then
    result := TFHIRSpecimenCollection.create()
  else if name = 'Specimen.treatment' then
    result := TFHIRSpecimenTreatment.create()
  else if name = 'Specimen.container' then
    result := TFHIRSpecimenContainer.create()
  else if name = 'Specimen' then
    result := TFHIRSpecimen.create()
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  else if name = 'StructureDefinition.contact' then
    result := TFHIRStructureDefinitionContact.create()
  else if name = 'StructureDefinition.mapping' then
    result := TFHIRStructureDefinitionMapping.create()
  else if name = 'StructureDefinition.snapshot' then
    result := TFHIRStructureDefinitionSnapshot.create()
  else if name = 'StructureDefinition.differential' then
    result := TFHIRStructureDefinitionDifferential.create()
  else if name = 'StructureDefinition' then
    result := TFHIRStructureDefinition.create()
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_SUBSCRIPTION}
  else if name = 'Subscription.channel' then
    result := TFHIRSubscriptionChannel.create()
  else if name = 'Subscription' then
    result := TFHIRSubscription.create()
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSTANCE}
  else if name = 'Substance.instance' then
    result := TFHIRSubstanceInstance.create()
  else if name = 'Substance.ingredient' then
    result := TFHIRSubstanceIngredient.create()
  else if name = 'Substance' then
    result := TFHIRSubstance.create()
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUPPLYDELIVERY}
  else if name = 'SupplyDelivery' then
    result := TFHIRSupplyDelivery.create()
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  else if name = 'SupplyRequest.when' then
    result := TFHIRSupplyRequestWhen.create()
  else if name = 'SupplyRequest' then
    result := TFHIRSupplyRequest.create()
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TESTSCRIPT}
  else if name = 'TestScript.contact' then
    result := TFHIRTestScriptContact.create()
  else if name = 'TestScript.metadata' then
    result := TFHIRTestScriptMetadata.create()
  else if name = 'TestScript.metadata.link' then
    result := TFHIRTestScriptMetadataLink.create()
  else if name = 'TestScript.metadata.capability' then
    result := TFHIRTestScriptMetadataCapability.create()
  else if name = 'TestScript.fixture' then
    result := TFHIRTestScriptFixture.create()
  else if name = 'TestScript.variable' then
    result := TFHIRTestScriptVariable.create()
  else if name = 'TestScript.setup' then
    result := TFHIRTestScriptSetup.create()
  else if name = 'TestScript.setup.action' then
    result := TFHIRTestScriptSetupAction.create()
  else if name = 'TestScript.setup.action.operation' then
    result := TFHIRTestScriptSetupActionOperation.create()
  else if name = 'TestScript.setup.action.operation.requestHeader' then
    result := TFHIRTestScriptSetupActionOperationRequestHeader.create()
  else if name = 'TestScript.setup.action.assert' then
    result := TFHIRTestScriptSetupActionAssert.create()
  else if name = 'TestScript.test' then
    result := TFHIRTestScriptTest.create()
  else if name = 'TestScript.test.action' then
    result := TFHIRTestScriptTestAction.create()
  else if name = 'TestScript.teardown' then
    result := TFHIRTestScriptTeardown.create()
  else if name = 'TestScript.teardown.action' then
    result := TFHIRTestScriptTeardownAction.create()
  else if name = 'TestScript' then
    result := TFHIRTestScript.create()
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  else if name = 'ValueSet.contact' then
    result := TFHIRValueSetContact.create()
  else if name = 'ValueSet.codeSystem' then
    result := TFHIRValueSetCodeSystem.create()
  else if name = 'ValueSet.codeSystem.concept' then
    result := TFHIRValueSetCodeSystemConcept.create()
  else if name = 'ValueSet.codeSystem.concept.designation' then
    result := TFHIRValueSetCodeSystemConceptDesignation.create()
  else if name = 'ValueSet.compose' then
    result := TFHIRValueSetCompose.create()
  else if name = 'ValueSet.compose.include' then
    result := TFHIRValueSetComposeInclude.create()
  else if name = 'ValueSet.compose.include.concept' then
    result := TFHIRValueSetComposeIncludeConcept.create()
  else if name = 'ValueSet.compose.include.filter' then
    result := TFHIRValueSetComposeIncludeFilter.create()
  else if name = 'ValueSet.expansion' then
    result := TFHIRValueSetExpansion.create()
  else if name = 'ValueSet.expansion.parameter' then
    result := TFHIRValueSetExpansionParameter.create()
  else if name = 'ValueSet.expansion.contains' then
    result := TFHIRValueSetExpansionContains.create()
  else if name = 'ValueSet' then
    result := TFHIRValueSet.create()
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  else if name = 'VisionPrescription.dispense' then
    result := TFHIRVisionPrescriptionDispense.create()
  else if name = 'VisionPrescription' then
    result := TFHIRVisionPrescription.create()
{$ENDIF FHIR_VISIONPRESCRIPTION}
  else
    result := nil;
end;


end.
