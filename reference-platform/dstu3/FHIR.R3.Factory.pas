unit FHIR.R3.Factory;

interface

uses
  FHIR.Ucum.IFace,
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.Base.Validator, FHIR.Base.Narrative, FHIR.Base.Factory, FHIR.Base.PathEngine,
  FHIR.XVersion.Resources,
  FHIR.Client.Base, FHIR.Client.Threaded;

type
  TFHIRFactoryR3 = class (TFHIRFactory)
  public
    function version : TFHIRVersion; override;
    function description : String; virtual;
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
  FHIR.R3.Types, FHIR.R3.Resources, FHIR.R3.Parser, FHIR.R3.Context, FHIR.R3.Validator,
  FHIR.R3.Narrative, FHIR.R3.PathEngine, FHIR.R3.Constants, FHIR.R3.Client, FHIR.R3.Common;

{ TFHIRFactoryR3 }

function TFHIRFactoryR3.description: String;
begin
  result := 'R3 ('+FHIR_GENERATED_VERSION+')';
end;

function TFHIRFactoryR3.makeClientHTTP(worker: TFHIRWorkerContextV; url: String; fmt : TFHIRFormat; timeout: cardinal; proxy: String): TFhirClientV;
var
  http : TFHIRHTTPCommunicator;
begin
  http := TFHIRHTTPCommunicator.Create(url);
  try
    http.timeout := timeout;
    http.proxy := proxy;
    result := TFhirClient3.create(worker, 'en', http.link);
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

function TFHIRFactoryR3.makeClientThreaded(worker: TFHIRWorkerContextV;
  internal: TFhirClientV; event: TThreadManagementEvent): TFhirClientV;
begin

end;

function TFHIRFactoryR3.makeCode(s: string): TFHIRObject;
begin
  result := TFhirCode.Create(s);
end;

function TFHIRFactoryR3.makeComposer(worker: TFHIRWorkerContextV; format: TFHIRFormat; lang: String; style: TFHIROutputStyle): TFHIRComposer;
begin
  result := TFHIRParsers3.composer(worker as TFHIRWorkerContext, format, lang, style);
end;

function TFHIRFactoryR3.makeDecimal(s: string): TFHIRObject;
begin
  result := TFhirDecimal.Create(s);
end;

function TFHIRFactoryR3.makeGenerator(worker: TFHIRWorkerContextV): TFHIRNarrativeGeneratorBase;
begin
  result := TFHIRNarrativeGenerator.create(worker);
end;

function TFHIRFactoryR3.makeInteger(s: string): TFHIRObject;
begin
  result := TFhirInteger.Create(s);
end;

function TFHIRFactoryR3.makeParameters: TFHIRParametersW;
begin

end;

function TFHIRFactoryR3.makeParser(worker: TFHIRWorkerContextV; format: TFHIRFormat; lang: String): TFHIRParser;
begin
  result := TFHIRParsers3.parser(worker as TFHIRWorkerContext, format, lang);
end;

function TFHIRFactoryR3.makePathEngine(worker: TFHIRWorkerContextV; ucum : TUcumServiceInterface): TFHIRPathEngineV;
begin
  result := TFHIRPathEngine.Create(worker as TFHIRWorkerContext, ucum);
end;

function TFHIRFactoryR3.makeString(s: string): TFHIRObject;
begin
  result := TFhirString.Create(s);
end;

function TFHIRFactoryR3.makeValidator(worker: TFHIRWorkerContextV): TFHIRValidatorV;
begin
  result := TFHIRValidator3.Create(worker as TFHIRWorkerContext);
end;

function TFHIRFactoryR3.version: TFHIRVersion;
begin
  result := fhirVersionRelease3;
end;

function TFHIRFactoryR3.wrapCapabilityStatement(r: TFHIRResourceV): TFHIRCapabilityStatementW;
begin
  result := TFHIRCapabilityStatement3.create(r);
end;

function TFHIRFactoryR3.makeBase64Binary(s: string): TFHIRObject;
begin
  result := TFhirBase64Binary.Create(decodeBase64(s));
end;

function TFHIRFactoryR3.makeBoolean(b: boolean): TFHIRObject;
begin
  result := TFhirBoolean.Create(b);
end;

function TFHIRFactoryR3.makeByName(const name : String) : TFHIRObject;
begin
  if name = 'Enum' then
    result := TFHIREnum.create()
  else if name = 'Date' then
    result := TFHIRDate.create()
  else if name = 'DateTime' then
    result := TFHIRDateTime.create()
  else if name = 'String' then
    result := TFHIRString.create()
  else if name = 'Integer' then
    result := TFHIRInteger.create()
  else if name = 'Uri' then
    result := TFHIRUri.create()
  else if name = 'Instant' then
    result := TFHIRInstant.create()
  else if name = 'Xhtml' then
    result := TFHIRXhtml.create()
  else if name = 'Boolean' then
    result := TFHIRBoolean.create()
  else if name = 'Base64Binary' then
    result := TFHIRBase64Binary.create()
  else if name = 'Time' then
    result := TFHIRTime.create()
  else if name = 'Decimal' then
    result := TFHIRDecimal.create()
  else if name = 'Code' then
    result := TFHIRCode.create()
  else if name = 'Oid' then
    result := TFHIROid.create()
  else if name = 'Uuid' then
    result := TFHIRUuid.create()
  else if name = 'Markdown' then
    result := TFHIRMarkdown.create()
  else if name = 'UnsignedInt' then
    result := TFHIRUnsignedInt.create()
  else if name = 'Id' then
    result := TFHIRId.create()
  else if name = 'PositiveInt' then
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
  else if name = 'Contributor' then
    result := TFHIRContributor.create()
  else if name = 'Attachment' then
    result := TFHIRAttachment.create()
  else if name = 'DataRequirement.codeFilter' then
    result := TFHIRDataRequirementCodeFilter.create()
  else if name = 'DataRequirement.dateFilter' then
    result := TFHIRDataRequirementDateFilter.create()
  else if name = 'DataRequirement' then
    result := TFHIRDataRequirement.create()
  else if name = 'Dosage' then
    result := TFHIRDosage.create()
  else if name = 'Identifier' then
    result := TFHIRIdentifier.create()
  else if name = 'Coding' then
    result := TFHIRCoding.create()
  else if name = 'SampledData' then
    result := TFHIRSampledData.create()
  else if name = 'Ratio' then
    result := TFHIRRatio.create()
  else if name = 'Reference' then
    result := TFHIRReference.create()
  else if name = 'TriggerDefinition' then
    result := TFHIRTriggerDefinition.create()
  else if name = 'Period' then
    result := TFHIRPeriod.create()
  else if name = 'Quantity' then
    result := TFHIRQuantity.create()
  else if name = 'Range' then
    result := TFHIRRange.create()
  else if name = 'RelatedArtifact' then
    result := TFHIRRelatedArtifact.create()
  else if name = 'Annotation' then
    result := TFHIRAnnotation.create()
  else if name = 'ContactDetail' then
    result := TFHIRContactDetail.create()
  else if name = 'UsageContext' then
    result := TFHIRUsageContext.create()
  else if name = 'Signature' then
    result := TFHIRSignature.create()
  else if name = 'CodeableConcept' then
    result := TFHIRCodeableConcept.create()
  else if name = 'ParameterDefinition' then
    result := TFHIRParameterDefinition.create()
  else if name = 'ContactPoint' then
    result := TFHIRContactPoint.create()
  else if name = 'HumanName' then
    result := TFHIRHumanName.create()
  else if name = 'Meta' then
    result := TFHIRMeta.create()
  else if name = 'Address' then
    result := TFHIRAddress.create()
  else if name = 'ElementDefinition.slicing' then
    result := TFHIRElementDefinitionSlicing.create()
  else if name = 'ElementDefinition.slicing.discriminator' then
    result := TFHIRElementDefinitionSlicingDiscriminator.create()
  else if name = 'ElementDefinition.base' then
    result := TFHIRElementDefinitionBase.create()
  else if name = 'ElementDefinition.type' then
    result := TFHIRElementDefinitionType.create()
  else if name = 'ElementDefinition.example' then
    result := TFHIRElementDefinitionExample.create()
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
  else if name = 'Count' then
    result := TFHIRCount.create()
  else if name = 'Money' then
    result := TFHIRMoney.create()
  else if name = 'Age' then
    result := TFHIRAge.create()
  else if name = 'Distance' then
    result := TFHIRDistance.create()
  else if name = 'Duration' then
    result := TFHIRDuration.create()
{$IFDEF FHIR_ACCOUNT}
  else if name = 'Account.coverage' then
    result := TFHIRAccountCoverage.create()
  else if name = 'Account.guarantor' then
    result := TFHIRAccountGuarantor.create()
  else if name = 'Account' then
    result := TFHIRAccount.create()
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  else if name = 'ActivityDefinition.participant' then
    result := TFHIRActivityDefinitionParticipant.create()
  else if name = 'ActivityDefinition.dynamicValue' then
    result := TFHIRActivityDefinitionDynamicValue.create()
  else if name = 'ActivityDefinition' then
    result := TFHIRActivityDefinition.create()
{$ENDIF FHIR_ACTIVITYDEFINITION}
{$IFDEF FHIR_ADVERSEEVENT}
  else if name = 'AdverseEvent.suspectEntity' then
    result := TFHIRAdverseEventSuspectEntity.create()
  else if name = 'AdverseEvent' then
    result := TFHIRAdverseEvent.create()
{$ENDIF FHIR_ADVERSEEVENT}
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
  else if name = 'AuditEvent.agent' then
    result := TFHIRAuditEventAgent.create()
  else if name = 'AuditEvent.agent.network' then
    result := TFHIRAuditEventAgentNetwork.create()
  else if name = 'AuditEvent.source' then
    result := TFHIRAuditEventSource.create()
  else if name = 'AuditEvent.entity' then
    result := TFHIRAuditEventEntity.create()
  else if name = 'AuditEvent.entity.detail' then
    result := TFHIRAuditEventEntityDetail.create()
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
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  else if name = 'CapabilityStatement.software' then
    result := TFHIRCapabilityStatementSoftware.create()
  else if name = 'CapabilityStatement.implementation' then
    result := TFHIRCapabilityStatementImplementation.create()
  else if name = 'CapabilityStatement.rest' then
    result := TFHIRCapabilityStatementRest.create()
  else if name = 'CapabilityStatement.rest.security' then
    result := TFHIRCapabilityStatementRestSecurity.create()
  else if name = 'CapabilityStatement.rest.security.certificate' then
    result := TFHIRCapabilityStatementRestSecurityCertificate.create()
  else if name = 'CapabilityStatement.rest.resource' then
    result := TFHIRCapabilityStatementRestResource.create()
  else if name = 'CapabilityStatement.rest.resource.interaction' then
    result := TFHIRCapabilityStatementRestResourceInteraction.create()
  else if name = 'CapabilityStatement.rest.resource.searchParam' then
    result := TFHIRCapabilityStatementRestResourceSearchParam.create()
  else if name = 'CapabilityStatement.rest.interaction' then
    result := TFHIRCapabilityStatementRestInteraction.create()
  else if name = 'CapabilityStatement.rest.operation' then
    result := TFHIRCapabilityStatementRestOperation.create()
  else if name = 'CapabilityStatement.messaging' then
    result := TFHIRCapabilityStatementMessaging.create()
  else if name = 'CapabilityStatement.messaging.endpoint' then
    result := TFHIRCapabilityStatementMessagingEndpoint.create()
  else if name = 'CapabilityStatement.messaging.supportedMessage' then
    result := TFHIRCapabilityStatementMessagingSupportedMessage.create()
  else if name = 'CapabilityStatement.messaging.event' then
    result := TFHIRCapabilityStatementMessagingEvent.create()
  else if name = 'CapabilityStatement.document' then
    result := TFHIRCapabilityStatementDocument.create()
  else if name = 'CapabilityStatement' then
    result := TFHIRCapabilityStatement.create()
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CAREPLAN}
  else if name = 'CarePlan.activity' then
    result := TFHIRCarePlanActivity.create()
  else if name = 'CarePlan.activity.detail' then
    result := TFHIRCarePlanActivityDetail.create()
  else if name = 'CarePlan' then
    result := TFHIRCarePlan.create()
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CARETEAM}
  else if name = 'CareTeam.participant' then
    result := TFHIRCareTeamParticipant.create()
  else if name = 'CareTeam' then
    result := TFHIRCareTeam.create()
{$ENDIF FHIR_CARETEAM}
{$IFDEF FHIR_CHARGEITEM}
  else if name = 'ChargeItem.participant' then
    result := TFHIRChargeItemParticipant.create()
  else if name = 'ChargeItem' then
    result := TFHIRChargeItem.create()
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CLAIM}
  else if name = 'Claim.related' then
    result := TFHIRClaimRelated.create()
  else if name = 'Claim.payee' then
    result := TFHIRClaimPayee.create()
  else if name = 'Claim.careTeam' then
    result := TFHIRClaimCareTeam.create()
  else if name = 'Claim.information' then
    result := TFHIRClaimInformation.create()
  else if name = 'Claim.diagnosis' then
    result := TFHIRClaimDiagnosis.create()
  else if name = 'Claim.procedure' then
    result := TFHIRClaimProcedure.create()
  else if name = 'Claim.insurance' then
    result := TFHIRClaimInsurance.create()
  else if name = 'Claim.accident' then
    result := TFHIRClaimAccident.create()
  else if name = 'Claim.item' then
    result := TFHIRClaimItem.create()
  else if name = 'Claim.item.detail' then
    result := TFHIRClaimItemDetail.create()
  else if name = 'Claim.item.detail.subDetail' then
    result := TFHIRClaimItemDetailSubDetail.create()
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
  else if name = 'ClaimResponse.item.detail.subDetail' then
    result := TFHIRClaimResponseItemDetailSubDetail.create()
  else if name = 'ClaimResponse.addItem' then
    result := TFHIRClaimResponseAddItem.create()
  else if name = 'ClaimResponse.addItem.detail' then
    result := TFHIRClaimResponseAddItemDetail.create()
  else if name = 'ClaimResponse.error' then
    result := TFHIRClaimResponseError.create()
  else if name = 'ClaimResponse.payment' then
    result := TFHIRClaimResponsePayment.create()
  else if name = 'ClaimResponse.processNote' then
    result := TFHIRClaimResponseProcessNote.create()
  else if name = 'ClaimResponse.insurance' then
    result := TFHIRClaimResponseInsurance.create()
  else if name = 'ClaimResponse' then
    result := TFHIRClaimResponse.create()
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  else if name = 'ClinicalImpression.investigation' then
    result := TFHIRClinicalImpressionInvestigation.create()
  else if name = 'ClinicalImpression.finding' then
    result := TFHIRClinicalImpressionFinding.create()
  else if name = 'ClinicalImpression' then
    result := TFHIRClinicalImpression.create()
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_CODESYSTEM}
  else if name = 'CodeSystem.filter' then
    result := TFHIRCodeSystemFilter.create()
  else if name = 'CodeSystem.property' then
    result := TFHIRCodeSystemProperty.create()
  else if name = 'CodeSystem.concept' then
    result := TFHIRCodeSystemConcept.create()
  else if name = 'CodeSystem.concept.designation' then
    result := TFHIRCodeSystemConceptDesignation.create()
  else if name = 'CodeSystem.concept.property' then
    result := TFHIRCodeSystemConceptProperty.create()
  else if name = 'CodeSystem' then
    result := TFHIRCodeSystem.create()
{$ENDIF FHIR_CODESYSTEM}
{$IFDEF FHIR_COMMUNICATION}
  else if name = 'Communication.payload' then
    result := TFHIRCommunicationPayload.create()
  else if name = 'Communication' then
    result := TFHIRCommunication.create()
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  else if name = 'CommunicationRequest.payload' then
    result := TFHIRCommunicationRequestPayload.create()
  else if name = 'CommunicationRequest.requester' then
    result := TFHIRCommunicationRequestRequester.create()
  else if name = 'CommunicationRequest' then
    result := TFHIRCommunicationRequest.create()
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  else if name = 'CompartmentDefinition.resource' then
    result := TFHIRCompartmentDefinitionResource.create()
  else if name = 'CompartmentDefinition' then
    result := TFHIRCompartmentDefinition.create()
{$ENDIF FHIR_COMPARTMENTDEFINITION}
{$IFDEF FHIR_COMPOSITION}
  else if name = 'Composition.attester' then
    result := TFHIRCompositionAttester.create()
  else if name = 'Composition.relatesTo' then
    result := TFHIRCompositionRelatesTo.create()
  else if name = 'Composition.event' then
    result := TFHIRCompositionEvent.create()
  else if name = 'Composition.section' then
    result := TFHIRCompositionSection.create()
  else if name = 'Composition' then
    result := TFHIRComposition.create()
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  else if name = 'ConceptMap.group' then
    result := TFHIRConceptMapGroup.create()
  else if name = 'ConceptMap.group.element' then
    result := TFHIRConceptMapGroupElement.create()
  else if name = 'ConceptMap.group.element.target' then
    result := TFHIRConceptMapGroupElementTarget.create()
  else if name = 'ConceptMap.group.element.target.dependsOn' then
    result := TFHIRConceptMapGroupElementTargetDependsOn.create()
  else if name = 'ConceptMap.group.unmapped' then
    result := TFHIRConceptMapGroupUnmapped.create()
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
{$IFDEF FHIR_CONSENT}
  else if name = 'Consent.actor' then
    result := TFHIRConsentActor.create()
  else if name = 'Consent.policy' then
    result := TFHIRConsentPolicy.create()
  else if name = 'Consent.data' then
    result := TFHIRConsentData.create()
  else if name = 'Consent.except' then
    result := TFHIRConsentExcept.create()
  else if name = 'Consent.except.actor' then
    result := TFHIRConsentExceptActor.create()
  else if name = 'Consent.except.data' then
    result := TFHIRConsentExceptData.create()
  else if name = 'Consent' then
    result := TFHIRConsent.create()
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  else if name = 'Contract.agent' then
    result := TFHIRContractAgent.create()
  else if name = 'Contract.signer' then
    result := TFHIRContractSigner.create()
  else if name = 'Contract.valuedItem' then
    result := TFHIRContractValuedItem.create()
  else if name = 'Contract.term' then
    result := TFHIRContractTerm.create()
  else if name = 'Contract.term.agent' then
    result := TFHIRContractTermAgent.create()
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
  else if name = 'Coverage.grouping' then
    result := TFHIRCoverageGrouping.create()
  else if name = 'Coverage' then
    result := TFHIRCoverage.create()
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_DATAELEMENT}
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
  else if name = 'Device.udi' then
    result := TFHIRDeviceUdi.create()
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
{$IFDEF FHIR_DEVICEREQUEST}
  else if name = 'DeviceRequest.requester' then
    result := TFHIRDeviceRequestRequester.create()
  else if name = 'DeviceRequest' then
    result := TFHIRDeviceRequest.create()
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  else if name = 'DeviceUseStatement' then
    result := TFHIRDeviceUseStatement.create()
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  else if name = 'DiagnosticReport.performer' then
    result := TFHIRDiagnosticReportPerformer.create()
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
  else if name = 'EligibilityResponse.insurance' then
    result := TFHIREligibilityResponseInsurance.create()
  else if name = 'EligibilityResponse.insurance.benefitBalance' then
    result := TFHIREligibilityResponseInsuranceBenefitBalance.create()
  else if name = 'EligibilityResponse.insurance.benefitBalance.financial' then
    result := TFHIREligibilityResponseInsuranceBenefitBalanceFinancial.create()
  else if name = 'EligibilityResponse.error' then
    result := TFHIREligibilityResponseError.create()
  else if name = 'EligibilityResponse' then
    result := TFHIREligibilityResponse.create()
{$ENDIF FHIR_ELIGIBILITYRESPONSE}
{$IFDEF FHIR_ENCOUNTER}
  else if name = 'Encounter.statusHistory' then
    result := TFHIREncounterStatusHistory.create()
  else if name = 'Encounter.classHistory' then
    result := TFHIREncounterClassHistory.create()
  else if name = 'Encounter.participant' then
    result := TFHIREncounterParticipant.create()
  else if name = 'Encounter.diagnosis' then
    result := TFHIREncounterDiagnosis.create()
  else if name = 'Encounter.hospitalization' then
    result := TFHIREncounterHospitalization.create()
  else if name = 'Encounter.location' then
    result := TFHIREncounterLocation.create()
  else if name = 'Encounter' then
    result := TFHIREncounter.create()
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENDPOINT}
  else if name = 'Endpoint' then
    result := TFHIREndpoint.create()
{$ENDIF FHIR_ENDPOINT}
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
  else if name = 'EpisodeOfCare.diagnosis' then
    result := TFHIREpisodeOfCareDiagnosis.create()
  else if name = 'EpisodeOfCare' then
    result := TFHIREpisodeOfCare.create()
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EXPANSIONPROFILE}
  else if name = 'ExpansionProfile.fixedVersion' then
    result := TFHIRExpansionProfileFixedVersion.create()
  else if name = 'ExpansionProfile.excludedSystem' then
    result := TFHIRExpansionProfileExcludedSystem.create()
  else if name = 'ExpansionProfile.designation' then
    result := TFHIRExpansionProfileDesignation.create()
  else if name = 'ExpansionProfile.designation.include' then
    result := TFHIRExpansionProfileDesignationInclude.create()
  else if name = 'ExpansionProfile.designation.include.designation' then
    result := TFHIRExpansionProfileDesignationIncludeDesignation.create()
  else if name = 'ExpansionProfile.designation.exclude' then
    result := TFHIRExpansionProfileDesignationExclude.create()
  else if name = 'ExpansionProfile.designation.exclude.designation' then
    result := TFHIRExpansionProfileDesignationExcludeDesignation.create()
  else if name = 'ExpansionProfile' then
    result := TFHIRExpansionProfile.create()
{$ENDIF FHIR_EXPANSIONPROFILE}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  else if name = 'ExplanationOfBenefit.related' then
    result := TFHIRExplanationOfBenefitRelated.create()
  else if name = 'ExplanationOfBenefit.payee' then
    result := TFHIRExplanationOfBenefitPayee.create()
  else if name = 'ExplanationOfBenefit.information' then
    result := TFHIRExplanationOfBenefitInformation.create()
  else if name = 'ExplanationOfBenefit.careTeam' then
    result := TFHIRExplanationOfBenefitCareTeam.create()
  else if name = 'ExplanationOfBenefit.diagnosis' then
    result := TFHIRExplanationOfBenefitDiagnosis.create()
  else if name = 'ExplanationOfBenefit.procedure' then
    result := TFHIRExplanationOfBenefitProcedure.create()
  else if name = 'ExplanationOfBenefit.insurance' then
    result := TFHIRExplanationOfBenefitInsurance.create()
  else if name = 'ExplanationOfBenefit.accident' then
    result := TFHIRExplanationOfBenefitAccident.create()
  else if name = 'ExplanationOfBenefit.item' then
    result := TFHIRExplanationOfBenefitItem.create()
  else if name = 'ExplanationOfBenefit.item.adjudication' then
    result := TFHIRExplanationOfBenefitItemAdjudication.create()
  else if name = 'ExplanationOfBenefit.item.detail' then
    result := TFHIRExplanationOfBenefitItemDetail.create()
  else if name = 'ExplanationOfBenefit.item.detail.subDetail' then
    result := TFHIRExplanationOfBenefitItemDetailSubDetail.create()
  else if name = 'ExplanationOfBenefit.addItem' then
    result := TFHIRExplanationOfBenefitAddItem.create()
  else if name = 'ExplanationOfBenefit.addItem.detail' then
    result := TFHIRExplanationOfBenefitAddItemDetail.create()
  else if name = 'ExplanationOfBenefit.payment' then
    result := TFHIRExplanationOfBenefitPayment.create()
  else if name = 'ExplanationOfBenefit.processNote' then
    result := TFHIRExplanationOfBenefitProcessNote.create()
  else if name = 'ExplanationOfBenefit.benefitBalance' then
    result := TFHIRExplanationOfBenefitBenefitBalance.create()
  else if name = 'ExplanationOfBenefit.benefitBalance.financial' then
    result := TFHIRExplanationOfBenefitBenefitBalanceFinancial.create()
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
  else if name = 'Goal.target' then
    result := TFHIRGoalTarget.create()
  else if name = 'Goal' then
    result := TFHIRGoal.create()
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GRAPHDEFINITION}
  else if name = 'GraphDefinition.link' then
    result := TFHIRGraphDefinitionLink.create()
  else if name = 'GraphDefinition.link.target' then
    result := TFHIRGraphDefinitionLinkTarget.create()
  else if name = 'GraphDefinition.link.target.compartment' then
    result := TFHIRGraphDefinitionLinkTargetCompartment.create()
  else if name = 'GraphDefinition' then
    result := TFHIRGraphDefinition.create()
{$ENDIF FHIR_GRAPHDEFINITION}
{$IFDEF FHIR_GROUP}
  else if name = 'Group.characteristic' then
    result := TFHIRGroupCharacteristic.create()
  else if name = 'Group.member' then
    result := TFHIRGroupMember.create()
  else if name = 'Group' then
    result := TFHIRGroup.create()
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_GUIDANCERESPONSE}
  else if name = 'GuidanceResponse' then
    result := TFHIRGuidanceResponse.create()
{$ENDIF FHIR_GUIDANCERESPONSE}
{$IFDEF FHIR_HEALTHCARESERVICE}
  else if name = 'HealthcareService.availableTime' then
    result := TFHIRHealthcareServiceAvailableTime.create()
  else if name = 'HealthcareService.notAvailable' then
    result := TFHIRHealthcareServiceNotAvailable.create()
  else if name = 'HealthcareService' then
    result := TFHIRHealthcareService.create()
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGMANIFEST}
  else if name = 'ImagingManifest.study' then
    result := TFHIRImagingManifestStudy.create()
  else if name = 'ImagingManifest.study.series' then
    result := TFHIRImagingManifestStudySeries.create()
  else if name = 'ImagingManifest.study.series.instance' then
    result := TFHIRImagingManifestStudySeriesInstance.create()
  else if name = 'ImagingManifest' then
    result := TFHIRImagingManifest.create()
{$ENDIF FHIR_IMAGINGMANIFEST}
{$IFDEF FHIR_IMAGINGSTUDY}
  else if name = 'ImagingStudy.series' then
    result := TFHIRImagingStudySeries.create()
  else if name = 'ImagingStudy.series.instance' then
    result := TFHIRImagingStudySeriesInstance.create()
  else if name = 'ImagingStudy' then
    result := TFHIRImagingStudy.create()
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  else if name = 'Immunization.practitioner' then
    result := TFHIRImmunizationPractitioner.create()
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
{$IFDEF FHIR_LIBRARY}
  else if name = 'Library' then
    result := TFHIRLibrary.create()
{$ENDIF FHIR_LIBRARY}
{$IFDEF FHIR_LINKAGE}
  else if name = 'Linkage.item' then
    result := TFHIRLinkageItem.create()
  else if name = 'Linkage' then
    result := TFHIRLinkage.create()
{$ENDIF FHIR_LINKAGE}
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
{$IFDEF FHIR_MEASURE}
  else if name = 'Measure.group' then
    result := TFHIRMeasureGroup.create()
  else if name = 'Measure.group.population' then
    result := TFHIRMeasureGroupPopulation.create()
  else if name = 'Measure.group.stratifier' then
    result := TFHIRMeasureGroupStratifier.create()
  else if name = 'Measure.supplementalData' then
    result := TFHIRMeasureSupplementalData.create()
  else if name = 'Measure' then
    result := TFHIRMeasure.create()
{$ENDIF FHIR_MEASURE}
{$IFDEF FHIR_MEASUREREPORT}
  else if name = 'MeasureReport.group' then
    result := TFHIRMeasureReportGroup.create()
  else if name = 'MeasureReport.group.population' then
    result := TFHIRMeasureReportGroupPopulation.create()
  else if name = 'MeasureReport.group.stratifier' then
    result := TFHIRMeasureReportGroupStratifier.create()
  else if name = 'MeasureReport.group.stratifier.stratum' then
    result := TFHIRMeasureReportGroupStratifierStratum.create()
  else if name = 'MeasureReport.group.stratifier.stratum.population' then
    result := TFHIRMeasureReportGroupStratifierStratumPopulation.create()
  else if name = 'MeasureReport' then
    result := TFHIRMeasureReport.create()
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MEDIA}
  else if name = 'Media' then
    result := TFHIRMedia.create()
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATION}
  else if name = 'Medication.ingredient' then
    result := TFHIRMedicationIngredient.create()
  else if name = 'Medication.package' then
    result := TFHIRMedicationPackage.create()
  else if name = 'Medication.package.content' then
    result := TFHIRMedicationPackageContent.create()
  else if name = 'Medication.package.batch' then
    result := TFHIRMedicationPackageBatch.create()
  else if name = 'Medication' then
    result := TFHIRMedication.create()
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  else if name = 'MedicationAdministration.performer' then
    result := TFHIRMedicationAdministrationPerformer.create()
  else if name = 'MedicationAdministration.dosage' then
    result := TFHIRMedicationAdministrationDosage.create()
  else if name = 'MedicationAdministration' then
    result := TFHIRMedicationAdministration.create()
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  else if name = 'MedicationDispense.performer' then
    result := TFHIRMedicationDispensePerformer.create()
  else if name = 'MedicationDispense.substitution' then
    result := TFHIRMedicationDispenseSubstitution.create()
  else if name = 'MedicationDispense' then
    result := TFHIRMedicationDispense.create()
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  else if name = 'MedicationRequest.requester' then
    result := TFHIRMedicationRequestRequester.create()
  else if name = 'MedicationRequest.dispenseRequest' then
    result := TFHIRMedicationRequestDispenseRequest.create()
  else if name = 'MedicationRequest.substitution' then
    result := TFHIRMedicationRequestSubstitution.create()
  else if name = 'MedicationRequest' then
    result := TFHIRMedicationRequest.create()
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  else if name = 'MedicationStatement' then
    result := TFHIRMedicationStatement.create()
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_MESSAGEDEFINITION}
  else if name = 'MessageDefinition.focus' then
    result := TFHIRMessageDefinitionFocus.create()
  else if name = 'MessageDefinition.allowedResponse' then
    result := TFHIRMessageDefinitionAllowedResponse.create()
  else if name = 'MessageDefinition' then
    result := TFHIRMessageDefinition.create()
{$ENDIF FHIR_MESSAGEDEFINITION}
{$IFDEF FHIR_MESSAGEHEADER}
  else if name = 'MessageHeader.destination' then
    result := TFHIRMessageHeaderDestination.create()
  else if name = 'MessageHeader.source' then
    result := TFHIRMessageHeaderSource.create()
  else if name = 'MessageHeader.response' then
    result := TFHIRMessageHeaderResponse.create()
  else if name = 'MessageHeader' then
    result := TFHIRMessageHeader.create()
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_NAMINGSYSTEM}
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
  else if name = 'OperationDefinition.parameter' then
    result := TFHIROperationDefinitionParameter.create()
  else if name = 'OperationDefinition.parameter.binding' then
    result := TFHIROperationDefinitionParameterBinding.create()
  else if name = 'OperationDefinition.overload' then
    result := TFHIROperationDefinitionOverload.create()
  else if name = 'OperationDefinition' then
    result := TFHIROperationDefinition.create()
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  else if name = 'OperationOutcome.issue' then
    result := TFHIROperationOutcomeIssue.create()
  else if name = 'OperationOutcome' then
    result := TFHIROperationOutcome.create()
{$ENDIF FHIR_OPERATIONOUTCOME}
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
  else if name = 'PaymentReconciliation.processNote' then
    result := TFHIRPaymentReconciliationProcessNote.create()
  else if name = 'PaymentReconciliation' then
    result := TFHIRPaymentReconciliation.create()
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERSON}
  else if name = 'Person.link' then
    result := TFHIRPersonLink.create()
  else if name = 'Person' then
    result := TFHIRPerson.create()
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PLANDEFINITION}
  else if name = 'PlanDefinition.goal' then
    result := TFHIRPlanDefinitionGoal.create()
  else if name = 'PlanDefinition.goal.target' then
    result := TFHIRPlanDefinitionGoalTarget.create()
  else if name = 'PlanDefinition.action' then
    result := TFHIRPlanDefinitionAction.create()
  else if name = 'PlanDefinition.action.condition' then
    result := TFHIRPlanDefinitionActionCondition.create()
  else if name = 'PlanDefinition.action.relatedAction' then
    result := TFHIRPlanDefinitionActionRelatedAction.create()
  else if name = 'PlanDefinition.action.participant' then
    result := TFHIRPlanDefinitionActionParticipant.create()
  else if name = 'PlanDefinition.action.dynamicValue' then
    result := TFHIRPlanDefinitionActionDynamicValue.create()
  else if name = 'PlanDefinition' then
    result := TFHIRPlanDefinition.create()
{$ENDIF FHIR_PLANDEFINITION}
{$IFDEF FHIR_PRACTITIONER}
  else if name = 'Practitioner.qualification' then
    result := TFHIRPractitionerQualification.create()
  else if name = 'Practitioner' then
    result := TFHIRPractitioner.create()
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PRACTITIONERROLE}
  else if name = 'PractitionerRole.availableTime' then
    result := TFHIRPractitionerRoleAvailableTime.create()
  else if name = 'PractitionerRole.notAvailable' then
    result := TFHIRPractitionerRoleNotAvailable.create()
  else if name = 'PractitionerRole' then
    result := TFHIRPractitionerRole.create()
{$ENDIF FHIR_PRACTITIONERROLE}
{$IFDEF FHIR_PROCEDURE}
  else if name = 'Procedure.performer' then
    result := TFHIRProcedurePerformer.create()
  else if name = 'Procedure.focalDevice' then
    result := TFHIRProcedureFocalDevice.create()
  else if name = 'Procedure' then
    result := TFHIRProcedure.create()
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROCEDUREREQUEST}
  else if name = 'ProcedureRequest.requester' then
    result := TFHIRProcedureRequestRequester.create()
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
  else if name = 'ProcessResponse.processNote' then
    result := TFHIRProcessResponseProcessNote.create()
  else if name = 'ProcessResponse' then
    result := TFHIRProcessResponse.create()
{$ENDIF FHIR_PROCESSRESPONSE}
{$IFDEF FHIR_PROVENANCE}
  else if name = 'Provenance.agent' then
    result := TFHIRProvenanceAgent.create()
  else if name = 'Provenance.entity' then
    result := TFHIRProvenanceEntity.create()
  else if name = 'Provenance' then
    result := TFHIRProvenance.create()
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  else if name = 'Questionnaire.item' then
    result := TFHIRQuestionnaireItem.create()
  else if name = 'Questionnaire.item.enableWhen' then
    result := TFHIRQuestionnaireItemEnableWhen.create()
  else if name = 'Questionnaire.item.option' then
    result := TFHIRQuestionnaireItemOption.create()
  else if name = 'Questionnaire' then
    result := TFHIRQuestionnaire.create()
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  else if name = 'QuestionnaireResponse.item' then
    result := TFHIRQuestionnaireResponseItem.create()
  else if name = 'QuestionnaireResponse.item.answer' then
    result := TFHIRQuestionnaireResponseItemAnswer.create()
  else if name = 'QuestionnaireResponse' then
    result := TFHIRQuestionnaireResponse.create()
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REFERRALREQUEST}
  else if name = 'ReferralRequest.requester' then
    result := TFHIRReferralRequestRequester.create()
  else if name = 'ReferralRequest' then
    result := TFHIRReferralRequest.create()
{$ENDIF FHIR_REFERRALREQUEST}
{$IFDEF FHIR_RELATEDPERSON}
  else if name = 'RelatedPerson' then
    result := TFHIRRelatedPerson.create()
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_REQUESTGROUP}
  else if name = 'RequestGroup.action' then
    result := TFHIRRequestGroupAction.create()
  else if name = 'RequestGroup.action.condition' then
    result := TFHIRRequestGroupActionCondition.create()
  else if name = 'RequestGroup.action.relatedAction' then
    result := TFHIRRequestGroupActionRelatedAction.create()
  else if name = 'RequestGroup' then
    result := TFHIRRequestGroup.create()
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_RESEARCHSTUDY}
  else if name = 'ResearchStudy.arm' then
    result := TFHIRResearchStudyArm.create()
  else if name = 'ResearchStudy' then
    result := TFHIRResearchStudy.create()
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  else if name = 'ResearchSubject' then
    result := TFHIRResearchSubject.create()
{$ENDIF FHIR_RESEARCHSUBJECT}
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
  else if name = 'SearchParameter.component' then
    result := TFHIRSearchParameterComponent.create()
  else if name = 'SearchParameter' then
    result := TFHIRSearchParameter.create()
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SEQUENCE}
  else if name = 'Sequence.referenceSeq' then
    result := TFHIRSequenceReferenceSeq.create()
  else if name = 'Sequence.variant' then
    result := TFHIRSequenceVariant.create()
  else if name = 'Sequence.quality' then
    result := TFHIRSequenceQuality.create()
  else if name = 'Sequence.repository' then
    result := TFHIRSequenceRepository.create()
  else if name = 'Sequence' then
    result := TFHIRSequence.create()
{$ENDIF FHIR_SEQUENCE}
{$IFDEF FHIR_SERVICEDEFINITION}
  else if name = 'ServiceDefinition' then
    result := TFHIRServiceDefinition.create()
{$ENDIF FHIR_SERVICEDEFINITION}
{$IFDEF FHIR_SLOT}
  else if name = 'Slot' then
    result := TFHIRSlot.create()
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  else if name = 'Specimen.collection' then
    result := TFHIRSpecimenCollection.create()
  else if name = 'Specimen.processing' then
    result := TFHIRSpecimenProcessing.create()
  else if name = 'Specimen.container' then
    result := TFHIRSpecimenContainer.create()
  else if name = 'Specimen' then
    result := TFHIRSpecimen.create()
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  else if name = 'StructureDefinition.mapping' then
    result := TFHIRStructureDefinitionMapping.create()
  else if name = 'StructureDefinition.snapshot' then
    result := TFHIRStructureDefinitionSnapshot.create()
  else if name = 'StructureDefinition.differential' then
    result := TFHIRStructureDefinitionDifferential.create()
  else if name = 'StructureDefinition' then
    result := TFHIRStructureDefinition.create()
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_STRUCTUREMAP}
  else if name = 'StructureMap.structure' then
    result := TFHIRStructureMapStructure.create()
  else if name = 'StructureMap.group' then
    result := TFHIRStructureMapGroup.create()
  else if name = 'StructureMap.group.input' then
    result := TFHIRStructureMapGroupInput.create()
  else if name = 'StructureMap.group.rule' then
    result := TFHIRStructureMapGroupRule.create()
  else if name = 'StructureMap.group.rule.source' then
    result := TFHIRStructureMapGroupRuleSource.create()
  else if name = 'StructureMap.group.rule.target' then
    result := TFHIRStructureMapGroupRuleTarget.create()
  else if name = 'StructureMap.group.rule.target.parameter' then
    result := TFHIRStructureMapGroupRuleTargetParameter.create()
  else if name = 'StructureMap.group.rule.dependent' then
    result := TFHIRStructureMapGroupRuleDependent.create()
  else if name = 'StructureMap' then
    result := TFHIRStructureMap.create()
{$ENDIF FHIR_STRUCTUREMAP}
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
  else if name = 'SupplyDelivery.suppliedItem' then
    result := TFHIRSupplyDeliverySuppliedItem.create()
  else if name = 'SupplyDelivery' then
    result := TFHIRSupplyDelivery.create()
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  else if name = 'SupplyRequest.orderedItem' then
    result := TFHIRSupplyRequestOrderedItem.create()
  else if name = 'SupplyRequest.requester' then
    result := TFHIRSupplyRequestRequester.create()
  else if name = 'SupplyRequest' then
    result := TFHIRSupplyRequest.create()
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TASK}
  else if name = 'Task.requester' then
    result := TFHIRTaskRequester.create()
  else if name = 'Task.restriction' then
    result := TFHIRTaskRestriction.create()
  else if name = 'Task.input' then
    result := TFHIRTaskInput.create()
  else if name = 'Task.output' then
    result := TFHIRTaskOutput.create()
  else if name = 'Task' then
    result := TFHIRTask.create()
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TESTREPORT}
  else if name = 'TestReport.participant' then
    result := TFHIRTestReportParticipant.create()
  else if name = 'TestReport.setup' then
    result := TFHIRTestReportSetup.create()
  else if name = 'TestReport.setup.action' then
    result := TFHIRTestReportSetupAction.create()
  else if name = 'TestReport.setup.action.operation' then
    result := TFHIRTestReportSetupActionOperation.create()
  else if name = 'TestReport.setup.action.assert' then
    result := TFHIRTestReportSetupActionAssert.create()
  else if name = 'TestReport.test' then
    result := TFHIRTestReportTest.create()
  else if name = 'TestReport.test.action' then
    result := TFHIRTestReportTestAction.create()
  else if name = 'TestReport.teardown' then
    result := TFHIRTestReportTeardown.create()
  else if name = 'TestReport.teardown.action' then
    result := TFHIRTestReportTeardownAction.create()
  else if name = 'TestReport' then
    result := TFHIRTestReport.create()
{$ENDIF FHIR_TESTREPORT}
{$IFDEF FHIR_TESTSCRIPT}
  else if name = 'TestScript.origin' then
    result := TFHIRTestScriptOrigin.create()
  else if name = 'TestScript.destination' then
    result := TFHIRTestScriptDestination.create()
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
  else if name = 'TestScript.rule' then
    result := TFHIRTestScriptRule.create()
  else if name = 'TestScript.rule.param' then
    result := TFHIRTestScriptRuleParam.create()
  else if name = 'TestScript.ruleset' then
    result := TFHIRTestScriptRuleset.create()
  else if name = 'TestScript.ruleset.rule' then
    result := TFHIRTestScriptRulesetRule.create()
  else if name = 'TestScript.ruleset.rule.param' then
    result := TFHIRTestScriptRulesetRuleParam.create()
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
  else if name = 'TestScript.setup.action.assert.rule' then
    result := TFHIRTestScriptSetupActionAssertRule.create()
  else if name = 'TestScript.setup.action.assert.rule.param' then
    result := TFHIRTestScriptSetupActionAssertRuleParam.create()
  else if name = 'TestScript.setup.action.assert.ruleset' then
    result := TFHIRTestScriptSetupActionAssertRuleset.create()
  else if name = 'TestScript.setup.action.assert.ruleset.rule' then
    result := TFHIRTestScriptSetupActionAssertRulesetRule.create()
  else if name = 'TestScript.setup.action.assert.ruleset.rule.param' then
    result := TFHIRTestScriptSetupActionAssertRulesetRuleParam.create()
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
  else if name = 'ValueSet.compose' then
    result := TFHIRValueSetCompose.create()
  else if name = 'ValueSet.compose.include' then
    result := TFHIRValueSetComposeInclude.create()
  else if name = 'ValueSet.compose.include.concept' then
    result := TFHIRValueSetComposeIncludeConcept.create()
  else if name = 'ValueSet.compose.include.concept.designation' then
    result := TFHIRValueSetComposeIncludeConceptDesignation.create()
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
