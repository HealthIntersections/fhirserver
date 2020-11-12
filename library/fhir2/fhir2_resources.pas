unit fhir2_resources;

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
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_stream,
  fhir_objects, fhir_utilities, 
  fhir2_base, fhir2_types,
  fhir2_resources_base, fhir2_resources_canonical, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_other;

Type
  TFhirResourceClass = fhir2_resources_base.TFhirResourceClass;
  TFhirResourceType = fhir2_resources_base.TFhirResourceType;
  TFhirResourceTypeSet = fhir2_resources_base.TFhirResourceTypeSet;
  TFhirResource = fhir2_resources_base.TFhirResource;
  TFhirDomainResource = fhir2_resources_base.TFhirDomainResource;
  TFHIRMetadataResource = fhir2_resources_canonical.TFHIRMetadataResource;
  TFhirResourceList = fhir2_resources_base.TFhirResourceList;
  TFhirDomainResourceList = fhir2_resources_base.TFhirDomainResourceList;
{$IFDEF FHIR_PARAMETERS}
  TFhirParametersParameter = fhir2_resources_other.TFhirParametersParameter;
  TFhirParametersParameterList = fhir2_resources_other.TFhirParametersParameterList;
  TFhirParametersList = fhir2_resources_other.TFhirParametersList;
  TFhirParameters = fhir2_resources_other.TFhirParameters;
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_ACCOUNT}
  TFhirAccountList = fhir2_resources_clinical.TFhirAccountList;
  TFhirAccount = fhir2_resources_clinical.TFhirAccount;
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  TFhirAllergyIntoleranceReaction = fhir2_resources_clinical.TFhirAllergyIntoleranceReaction;
  TFhirAllergyIntoleranceReactionList = fhir2_resources_clinical.TFhirAllergyIntoleranceReactionList;
  TFhirAllergyIntoleranceList = fhir2_resources_clinical.TFhirAllergyIntoleranceList;
  TFhirAllergyIntolerance = fhir2_resources_clinical.TFhirAllergyIntolerance;
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  TFhirAppointmentParticipant = fhir2_resources_clinical.TFhirAppointmentParticipant;
  TFhirAppointmentParticipantList = fhir2_resources_clinical.TFhirAppointmentParticipantList;
  TFhirAppointmentList = fhir2_resources_clinical.TFhirAppointmentList;
  TFhirAppointment = fhir2_resources_clinical.TFhirAppointment;
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  TFhirAppointmentResponseList = fhir2_resources_clinical.TFhirAppointmentResponseList;
  TFhirAppointmentResponse = fhir2_resources_clinical.TFhirAppointmentResponse;
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_AUDITEVENT}
  TFhirAuditEventEvent = fhir2_resources_other.TFhirAuditEventEvent;
  TFhirAuditEventEventList = fhir2_resources_other.TFhirAuditEventEventList;
  TFhirAuditEventParticipant = fhir2_resources_other.TFhirAuditEventParticipant;
  TFhirAuditEventParticipantList = fhir2_resources_other.TFhirAuditEventParticipantList;
  TFhirAuditEventParticipantNetwork = fhir2_resources_other.TFhirAuditEventParticipantNetwork;
  TFhirAuditEventParticipantNetworkList = fhir2_resources_other.TFhirAuditEventParticipantNetworkList;
  TFhirAuditEventSource = fhir2_resources_other.TFhirAuditEventSource;
  TFhirAuditEventSourceList = fhir2_resources_other.TFhirAuditEventSourceList;
  TFhirAuditEventObject = fhir2_resources_other.TFhirAuditEventObject;
  TFhirAuditEventObjectList = fhir2_resources_other.TFhirAuditEventObjectList;
  TFhirAuditEventObjectDetail = fhir2_resources_other.TFhirAuditEventObjectDetail;
  TFhirAuditEventObjectDetailList = fhir2_resources_other.TFhirAuditEventObjectDetailList;
  TFhirAuditEventList = fhir2_resources_other.TFhirAuditEventList;
  TFhirAuditEvent = fhir2_resources_other.TFhirAuditEvent;
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BASIC}
  TFhirBasicList = fhir2_resources_clinical.TFhirBasicList;
  TFhirBasic = fhir2_resources_clinical.TFhirBasic;
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BINARY}
  TFhirBinaryList = fhir2_resources_other.TFhirBinaryList;
  TFhirBinary = fhir2_resources_other.TFhirBinary;
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BODYSITE}
  TFhirBodySiteList = fhir2_resources_clinical.TFhirBodySiteList;
  TFhirBodySite = fhir2_resources_clinical.TFhirBodySite;
{$ENDIF FHIR_BODYSITE}
{$IFDEF FHIR_BUNDLE}
  TFhirBundleLink = fhir2_resources_other.TFhirBundleLink;
  TFhirBundleLinkList = fhir2_resources_other.TFhirBundleLinkList;
  TFhirBundleEntry = fhir2_resources_other.TFhirBundleEntry;
  TFhirBundleEntryList = fhir2_resources_other.TFhirBundleEntryList;
  TFhirBundleEntrySearch = fhir2_resources_other.TFhirBundleEntrySearch;
  TFhirBundleEntrySearchList = fhir2_resources_other.TFhirBundleEntrySearchList;
  TFhirBundleEntryRequest = fhir2_resources_other.TFhirBundleEntryRequest;
  TFhirBundleEntryRequestList = fhir2_resources_other.TFhirBundleEntryRequestList;
  TFhirBundleEntryResponse = fhir2_resources_other.TFhirBundleEntryResponse;
  TFhirBundleEntryResponseList = fhir2_resources_other.TFhirBundleEntryResponseList;
  TFhirBundleList = fhir2_resources_other.TFhirBundleList;
  TFhirBundle = fhir2_resources_other.TFhirBundle;
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CAREPLAN}
  TFhirCarePlanRelatedPlan = fhir2_resources_clinical.TFhirCarePlanRelatedPlan;
  TFhirCarePlanRelatedPlanList = fhir2_resources_clinical.TFhirCarePlanRelatedPlanList;
  TFhirCarePlanParticipant = fhir2_resources_clinical.TFhirCarePlanParticipant;
  TFhirCarePlanParticipantList = fhir2_resources_clinical.TFhirCarePlanParticipantList;
  TFhirCarePlanActivity = fhir2_resources_clinical.TFhirCarePlanActivity;
  TFhirCarePlanActivityList = fhir2_resources_clinical.TFhirCarePlanActivityList;
  TFhirCarePlanActivityDetail = fhir2_resources_clinical.TFhirCarePlanActivityDetail;
  TFhirCarePlanActivityDetailList = fhir2_resources_clinical.TFhirCarePlanActivityDetailList;
  TFhirCarePlanList = fhir2_resources_clinical.TFhirCarePlanList;
  TFhirCarePlan = fhir2_resources_clinical.TFhirCarePlan;
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CLAIM}
  TFhirClaimPayee = fhir2_resources_other.TFhirClaimPayee;
  TFhirClaimPayeeList = fhir2_resources_other.TFhirClaimPayeeList;
  TFhirClaimDiagnosis = fhir2_resources_other.TFhirClaimDiagnosis;
  TFhirClaimDiagnosisList = fhir2_resources_other.TFhirClaimDiagnosisList;
  TFhirClaimCoverage = fhir2_resources_other.TFhirClaimCoverage;
  TFhirClaimCoverageList = fhir2_resources_other.TFhirClaimCoverageList;
  TFhirClaimItem = fhir2_resources_other.TFhirClaimItem;
  TFhirClaimItemList = fhir2_resources_other.TFhirClaimItemList;
  TFhirClaimItemDetail = fhir2_resources_other.TFhirClaimItemDetail;
  TFhirClaimItemDetailList = fhir2_resources_other.TFhirClaimItemDetailList;
  TFhirClaimItemDetailSubDetail = fhir2_resources_other.TFhirClaimItemDetailSubDetail;
  TFhirClaimItemDetailSubDetailList = fhir2_resources_other.TFhirClaimItemDetailSubDetailList;
  TFhirClaimItemProsthesis = fhir2_resources_other.TFhirClaimItemProsthesis;
  TFhirClaimItemProsthesisList = fhir2_resources_other.TFhirClaimItemProsthesisList;
  TFhirClaimMissingTeeth = fhir2_resources_other.TFhirClaimMissingTeeth;
  TFhirClaimMissingTeethList = fhir2_resources_other.TFhirClaimMissingTeethList;
  TFhirClaimList = fhir2_resources_other.TFhirClaimList;
  TFhirClaim = fhir2_resources_other.TFhirClaim;
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  TFhirClaimResponseItem = fhir2_resources_other.TFhirClaimResponseItem;
  TFhirClaimResponseItemList = fhir2_resources_other.TFhirClaimResponseItemList;
  TFhirClaimResponseItemAdjudication = fhir2_resources_other.TFhirClaimResponseItemAdjudication;
  TFhirClaimResponseItemAdjudicationList = fhir2_resources_other.TFhirClaimResponseItemAdjudicationList;
  TFhirClaimResponseItemDetail = fhir2_resources_other.TFhirClaimResponseItemDetail;
  TFhirClaimResponseItemDetailList = fhir2_resources_other.TFhirClaimResponseItemDetailList;
  TFhirClaimResponseItemDetailAdjudication = fhir2_resources_other.TFhirClaimResponseItemDetailAdjudication;
  TFhirClaimResponseItemDetailAdjudicationList = fhir2_resources_other.TFhirClaimResponseItemDetailAdjudicationList;
  TFhirClaimResponseItemDetailSubDetail = fhir2_resources_other.TFhirClaimResponseItemDetailSubDetail;
  TFhirClaimResponseItemDetailSubDetailList = fhir2_resources_other.TFhirClaimResponseItemDetailSubDetailList;
  TFhirClaimResponseItemDetailSubDetailAdjudication = fhir2_resources_other.TFhirClaimResponseItemDetailSubDetailAdjudication;
  TFhirClaimResponseItemDetailSubDetailAdjudicationList = fhir2_resources_other.TFhirClaimResponseItemDetailSubDetailAdjudicationList;
  TFhirClaimResponseAddItem = fhir2_resources_other.TFhirClaimResponseAddItem;
  TFhirClaimResponseAddItemList = fhir2_resources_other.TFhirClaimResponseAddItemList;
  TFhirClaimResponseAddItemAdjudication = fhir2_resources_other.TFhirClaimResponseAddItemAdjudication;
  TFhirClaimResponseAddItemAdjudicationList = fhir2_resources_other.TFhirClaimResponseAddItemAdjudicationList;
  TFhirClaimResponseAddItemDetail = fhir2_resources_other.TFhirClaimResponseAddItemDetail;
  TFhirClaimResponseAddItemDetailList = fhir2_resources_other.TFhirClaimResponseAddItemDetailList;
  TFhirClaimResponseAddItemDetailAdjudication = fhir2_resources_other.TFhirClaimResponseAddItemDetailAdjudication;
  TFhirClaimResponseAddItemDetailAdjudicationList = fhir2_resources_other.TFhirClaimResponseAddItemDetailAdjudicationList;
  TFhirClaimResponseError = fhir2_resources_other.TFhirClaimResponseError;
  TFhirClaimResponseErrorList = fhir2_resources_other.TFhirClaimResponseErrorList;
  TFhirClaimResponseNote = fhir2_resources_other.TFhirClaimResponseNote;
  TFhirClaimResponseNoteList = fhir2_resources_other.TFhirClaimResponseNoteList;
  TFhirClaimResponseCoverage = fhir2_resources_other.TFhirClaimResponseCoverage;
  TFhirClaimResponseCoverageList = fhir2_resources_other.TFhirClaimResponseCoverageList;
  TFhirClaimResponseList = fhir2_resources_other.TFhirClaimResponseList;
  TFhirClaimResponse = fhir2_resources_other.TFhirClaimResponse;
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  TFhirClinicalImpressionInvestigations = fhir2_resources_clinical.TFhirClinicalImpressionInvestigations;
  TFhirClinicalImpressionInvestigationsList = fhir2_resources_clinical.TFhirClinicalImpressionInvestigationsList;
  TFhirClinicalImpressionFinding = fhir2_resources_clinical.TFhirClinicalImpressionFinding;
  TFhirClinicalImpressionFindingList = fhir2_resources_clinical.TFhirClinicalImpressionFindingList;
  TFhirClinicalImpressionRuledOut = fhir2_resources_clinical.TFhirClinicalImpressionRuledOut;
  TFhirClinicalImpressionRuledOutList = fhir2_resources_clinical.TFhirClinicalImpressionRuledOutList;
  TFhirClinicalImpressionList = fhir2_resources_clinical.TFhirClinicalImpressionList;
  TFhirClinicalImpression = fhir2_resources_clinical.TFhirClinicalImpression;
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_COMMUNICATION}
  TFhirCommunicationPayload = fhir2_resources_clinical.TFhirCommunicationPayload;
  TFhirCommunicationPayloadList = fhir2_resources_clinical.TFhirCommunicationPayloadList;
  TFhirCommunicationList = fhir2_resources_clinical.TFhirCommunicationList;
  TFhirCommunication = fhir2_resources_clinical.TFhirCommunication;
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  TFhirCommunicationRequestPayload = fhir2_resources_clinical.TFhirCommunicationRequestPayload;
  TFhirCommunicationRequestPayloadList = fhir2_resources_clinical.TFhirCommunicationRequestPayloadList;
  TFhirCommunicationRequestList = fhir2_resources_clinical.TFhirCommunicationRequestList;
  TFhirCommunicationRequest = fhir2_resources_clinical.TFhirCommunicationRequest;
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPOSITION}
  TFhirCompositionAttester = fhir2_resources_clinical.TFhirCompositionAttester;
  TFhirCompositionAttesterList = fhir2_resources_clinical.TFhirCompositionAttesterList;
  TFhirCompositionEvent = fhir2_resources_clinical.TFhirCompositionEvent;
  TFhirCompositionEventList = fhir2_resources_clinical.TFhirCompositionEventList;
  TFhirCompositionSection = fhir2_resources_clinical.TFhirCompositionSection;
  TFhirCompositionSectionList = fhir2_resources_clinical.TFhirCompositionSectionList;
  TFhirCompositionList = fhir2_resources_clinical.TFhirCompositionList;
  TFhirComposition = fhir2_resources_clinical.TFhirComposition;
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  TFhirConceptMapContact = fhir2_resources_canonical.TFhirConceptMapContact;
  TFhirConceptMapContactList = fhir2_resources_canonical.TFhirConceptMapContactList;
  TFhirConceptMapElement = fhir2_resources_canonical.TFhirConceptMapElement;
  TFhirConceptMapElementList = fhir2_resources_canonical.TFhirConceptMapElementList;
  TFhirConceptMapElementTarget = fhir2_resources_canonical.TFhirConceptMapElementTarget;
  TFhirConceptMapElementTargetList = fhir2_resources_canonical.TFhirConceptMapElementTargetList;
  TFhirConceptMapElementTargetDependsOn = fhir2_resources_canonical.TFhirConceptMapElementTargetDependsOn;
  TFhirConceptMapElementTargetDependsOnList = fhir2_resources_canonical.TFhirConceptMapElementTargetDependsOnList;
  TFhirConceptMapList = fhir2_resources_canonical.TFhirConceptMapList;
  TFhirConceptMap = fhir2_resources_canonical.TFhirConceptMap;
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_CONDITION}
  TFhirConditionStage = fhir2_resources_clinical.TFhirConditionStage;
  TFhirConditionStageList = fhir2_resources_clinical.TFhirConditionStageList;
  TFhirConditionEvidence = fhir2_resources_clinical.TFhirConditionEvidence;
  TFhirConditionEvidenceList = fhir2_resources_clinical.TFhirConditionEvidenceList;
  TFhirConditionList = fhir2_resources_clinical.TFhirConditionList;
  TFhirCondition = fhir2_resources_clinical.TFhirCondition;
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_CONFORMANCE}
  TFhirConformanceContact = fhir2_resources_canonical.TFhirConformanceContact;
  TFhirConformanceContactList = fhir2_resources_canonical.TFhirConformanceContactList;
  TFhirConformanceSoftware = fhir2_resources_canonical.TFhirConformanceSoftware;
  TFhirConformanceSoftwareList = fhir2_resources_canonical.TFhirConformanceSoftwareList;
  TFhirConformanceImplementation = fhir2_resources_canonical.TFhirConformanceImplementation;
  TFhirConformanceImplementationList = fhir2_resources_canonical.TFhirConformanceImplementationList;
  TFhirConformanceRest = fhir2_resources_canonical.TFhirConformanceRest;
  TFhirConformanceRestList = fhir2_resources_canonical.TFhirConformanceRestList;
  TFhirConformanceRestSecurity = fhir2_resources_canonical.TFhirConformanceRestSecurity;
  TFhirConformanceRestSecurityList = fhir2_resources_canonical.TFhirConformanceRestSecurityList;
  TFhirConformanceRestSecurityCertificate = fhir2_resources_canonical.TFhirConformanceRestSecurityCertificate;
  TFhirConformanceRestSecurityCertificateList = fhir2_resources_canonical.TFhirConformanceRestSecurityCertificateList;
  TFhirConformanceRestResource = fhir2_resources_canonical.TFhirConformanceRestResource;
  TFhirConformanceRestResourceList = fhir2_resources_canonical.TFhirConformanceRestResourceList;
  TFhirConformanceRestResourceInteraction = fhir2_resources_canonical.TFhirConformanceRestResourceInteraction;
  TFhirConformanceRestResourceInteractionList = fhir2_resources_canonical.TFhirConformanceRestResourceInteractionList;
  TFhirConformanceRestResourceSearchParam = fhir2_resources_canonical.TFhirConformanceRestResourceSearchParam;
  TFhirConformanceRestResourceSearchParamList = fhir2_resources_canonical.TFhirConformanceRestResourceSearchParamList;
  TFhirConformanceRestInteraction = fhir2_resources_canonical.TFhirConformanceRestInteraction;
  TFhirConformanceRestInteractionList = fhir2_resources_canonical.TFhirConformanceRestInteractionList;
  TFhirConformanceRestOperation = fhir2_resources_canonical.TFhirConformanceRestOperation;
  TFhirConformanceRestOperationList = fhir2_resources_canonical.TFhirConformanceRestOperationList;
  TFhirConformanceMessaging = fhir2_resources_canonical.TFhirConformanceMessaging;
  TFhirConformanceMessagingList = fhir2_resources_canonical.TFhirConformanceMessagingList;
  TFhirConformanceMessagingEndpoint = fhir2_resources_canonical.TFhirConformanceMessagingEndpoint;
  TFhirConformanceMessagingEndpointList = fhir2_resources_canonical.TFhirConformanceMessagingEndpointList;
  TFhirConformanceMessagingEvent = fhir2_resources_canonical.TFhirConformanceMessagingEvent;
  TFhirConformanceMessagingEventList = fhir2_resources_canonical.TFhirConformanceMessagingEventList;
  TFhirConformanceDocument = fhir2_resources_canonical.TFhirConformanceDocument;
  TFhirConformanceDocumentList = fhir2_resources_canonical.TFhirConformanceDocumentList;
  TFhirConformanceList = fhir2_resources_canonical.TFhirConformanceList;
  TFhirConformance = fhir2_resources_canonical.TFhirConformance;
{$ENDIF FHIR_CONFORMANCE}
{$IFDEF FHIR_CONTRACT}
  TFhirContractActor = fhir2_resources_other.TFhirContractActor;
  TFhirContractActorList = fhir2_resources_other.TFhirContractActorList;
  TFhirContractValuedItem = fhir2_resources_other.TFhirContractValuedItem;
  TFhirContractValuedItemList = fhir2_resources_other.TFhirContractValuedItemList;
  TFhirContractSigner = fhir2_resources_other.TFhirContractSigner;
  TFhirContractSignerList = fhir2_resources_other.TFhirContractSignerList;
  TFhirContractTerm = fhir2_resources_other.TFhirContractTerm;
  TFhirContractTermList = fhir2_resources_other.TFhirContractTermList;
  TFhirContractTermActor = fhir2_resources_other.TFhirContractTermActor;
  TFhirContractTermActorList = fhir2_resources_other.TFhirContractTermActorList;
  TFhirContractTermValuedItem = fhir2_resources_other.TFhirContractTermValuedItem;
  TFhirContractTermValuedItemList = fhir2_resources_other.TFhirContractTermValuedItemList;
  TFhirContractFriendly = fhir2_resources_other.TFhirContractFriendly;
  TFhirContractFriendlyList = fhir2_resources_other.TFhirContractFriendlyList;
  TFhirContractLegal = fhir2_resources_other.TFhirContractLegal;
  TFhirContractLegalList = fhir2_resources_other.TFhirContractLegalList;
  TFhirContractRule = fhir2_resources_other.TFhirContractRule;
  TFhirContractRuleList = fhir2_resources_other.TFhirContractRuleList;
  TFhirContractList = fhir2_resources_other.TFhirContractList;
  TFhirContract = fhir2_resources_other.TFhirContract;
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_COVERAGE}
  TFhirCoverageList = fhir2_resources_clinical.TFhirCoverageList;
  TFhirCoverage = fhir2_resources_clinical.TFhirCoverage;
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_DATAELEMENT}
  TFhirDataElementContact = fhir2_resources_canonical.TFhirDataElementContact;
  TFhirDataElementContactList = fhir2_resources_canonical.TFhirDataElementContactList;
  TFhirDataElementMapping = fhir2_resources_canonical.TFhirDataElementMapping;
  TFhirDataElementMappingList = fhir2_resources_canonical.TFhirDataElementMappingList;
  TFhirDataElementList = fhir2_resources_canonical.TFhirDataElementList;
  TFhirDataElement = fhir2_resources_canonical.TFhirDataElement;
{$ENDIF FHIR_DATAELEMENT}
{$IFDEF FHIR_DETECTEDISSUE}
  TFhirDetectedIssueMitigation = fhir2_resources_clinical.TFhirDetectedIssueMitigation;
  TFhirDetectedIssueMitigationList = fhir2_resources_clinical.TFhirDetectedIssueMitigationList;
  TFhirDetectedIssueList = fhir2_resources_clinical.TFhirDetectedIssueList;
  TFhirDetectedIssue = fhir2_resources_clinical.TFhirDetectedIssue;
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  TFhirDeviceList = fhir2_resources_admin.TFhirDeviceList;
  TFhirDevice = fhir2_resources_admin.TFhirDevice;
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICECOMPONENT}
  TFhirDeviceComponentProductionSpecification = fhir2_resources_admin.TFhirDeviceComponentProductionSpecification;
  TFhirDeviceComponentProductionSpecificationList = fhir2_resources_admin.TFhirDeviceComponentProductionSpecificationList;
  TFhirDeviceComponentList = fhir2_resources_admin.TFhirDeviceComponentList;
  TFhirDeviceComponent = fhir2_resources_admin.TFhirDeviceComponent;
{$ENDIF FHIR_DEVICECOMPONENT}
{$IFDEF FHIR_DEVICEMETRIC}
  TFhirDeviceMetricCalibration = fhir2_resources_admin.TFhirDeviceMetricCalibration;
  TFhirDeviceMetricCalibrationList = fhir2_resources_admin.TFhirDeviceMetricCalibrationList;
  TFhirDeviceMetricList = fhir2_resources_admin.TFhirDeviceMetricList;
  TFhirDeviceMetric = fhir2_resources_admin.TFhirDeviceMetric;
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEUSEREQUEST}
  TFhirDeviceUseRequestList = fhir2_resources_clinical.TFhirDeviceUseRequestList;
  TFhirDeviceUseRequest = fhir2_resources_clinical.TFhirDeviceUseRequest;
{$ENDIF FHIR_DEVICEUSEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  TFhirDeviceUseStatementList = fhir2_resources_clinical.TFhirDeviceUseStatementList;
  TFhirDeviceUseStatement = fhir2_resources_clinical.TFhirDeviceUseStatement;
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICORDER}
  TFhirDiagnosticOrderEvent = fhir2_resources_clinical.TFhirDiagnosticOrderEvent;
  TFhirDiagnosticOrderEventList = fhir2_resources_clinical.TFhirDiagnosticOrderEventList;
  TFhirDiagnosticOrderItem = fhir2_resources_clinical.TFhirDiagnosticOrderItem;
  TFhirDiagnosticOrderItemList = fhir2_resources_clinical.TFhirDiagnosticOrderItemList;
  TFhirDiagnosticOrderList = fhir2_resources_clinical.TFhirDiagnosticOrderList;
  TFhirDiagnosticOrder = fhir2_resources_clinical.TFhirDiagnosticOrder;
{$ENDIF FHIR_DIAGNOSTICORDER}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  TFhirDiagnosticReportImage = fhir2_resources_clinical.TFhirDiagnosticReportImage;
  TFhirDiagnosticReportImageList = fhir2_resources_clinical.TFhirDiagnosticReportImageList;
  TFhirDiagnosticReportList = fhir2_resources_clinical.TFhirDiagnosticReportList;
  TFhirDiagnosticReport = fhir2_resources_clinical.TFhirDiagnosticReport;
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  TFhirDocumentManifestContent = fhir2_resources_clinical.TFhirDocumentManifestContent;
  TFhirDocumentManifestContentList = fhir2_resources_clinical.TFhirDocumentManifestContentList;
  TFhirDocumentManifestRelated = fhir2_resources_clinical.TFhirDocumentManifestRelated;
  TFhirDocumentManifestRelatedList = fhir2_resources_clinical.TFhirDocumentManifestRelatedList;
  TFhirDocumentManifestList = fhir2_resources_clinical.TFhirDocumentManifestList;
  TFhirDocumentManifest = fhir2_resources_clinical.TFhirDocumentManifest;
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  TFhirDocumentReferenceRelatesTo = fhir2_resources_clinical.TFhirDocumentReferenceRelatesTo;
  TFhirDocumentReferenceRelatesToList = fhir2_resources_clinical.TFhirDocumentReferenceRelatesToList;
  TFhirDocumentReferenceContent = fhir2_resources_clinical.TFhirDocumentReferenceContent;
  TFhirDocumentReferenceContentList = fhir2_resources_clinical.TFhirDocumentReferenceContentList;
  TFhirDocumentReferenceContext = fhir2_resources_clinical.TFhirDocumentReferenceContext;
  TFhirDocumentReferenceContextList = fhir2_resources_clinical.TFhirDocumentReferenceContextList;
  TFhirDocumentReferenceContextRelated = fhir2_resources_clinical.TFhirDocumentReferenceContextRelated;
  TFhirDocumentReferenceContextRelatedList = fhir2_resources_clinical.TFhirDocumentReferenceContextRelatedList;
  TFhirDocumentReferenceList = fhir2_resources_clinical.TFhirDocumentReferenceList;
  TFhirDocumentReference = fhir2_resources_clinical.TFhirDocumentReference;
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_ELIGIBILITYREQUEST}
  TFhirEligibilityRequestList = fhir2_resources_other.TFhirEligibilityRequestList;
  TFhirEligibilityRequest = fhir2_resources_other.TFhirEligibilityRequest;
{$ENDIF FHIR_ELIGIBILITYREQUEST}
{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  TFhirEligibilityResponseList = fhir2_resources_other.TFhirEligibilityResponseList;
  TFhirEligibilityResponse = fhir2_resources_other.TFhirEligibilityResponse;
{$ENDIF FHIR_ELIGIBILITYRESPONSE}
{$IFDEF FHIR_ENCOUNTER}
  TFhirEncounterStatusHistory = fhir2_resources_admin.TFhirEncounterStatusHistory;
  TFhirEncounterStatusHistoryList = fhir2_resources_admin.TFhirEncounterStatusHistoryList;
  TFhirEncounterParticipant = fhir2_resources_admin.TFhirEncounterParticipant;
  TFhirEncounterParticipantList = fhir2_resources_admin.TFhirEncounterParticipantList;
  TFhirEncounterHospitalization = fhir2_resources_admin.TFhirEncounterHospitalization;
  TFhirEncounterHospitalizationList = fhir2_resources_admin.TFhirEncounterHospitalizationList;
  TFhirEncounterLocation = fhir2_resources_admin.TFhirEncounterLocation;
  TFhirEncounterLocationList = fhir2_resources_admin.TFhirEncounterLocationList;
  TFhirEncounterList = fhir2_resources_admin.TFhirEncounterList;
  TFhirEncounter = fhir2_resources_admin.TFhirEncounter;
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  TFhirEnrollmentRequestList = fhir2_resources_other.TFhirEnrollmentRequestList;
  TFhirEnrollmentRequest = fhir2_resources_other.TFhirEnrollmentRequest;
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  TFhirEnrollmentResponseList = fhir2_resources_other.TFhirEnrollmentResponseList;
  TFhirEnrollmentResponse = fhir2_resources_other.TFhirEnrollmentResponse;
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EPISODEOFCARE}
  TFhirEpisodeOfCareStatusHistory = fhir2_resources_admin.TFhirEpisodeOfCareStatusHistory;
  TFhirEpisodeOfCareStatusHistoryList = fhir2_resources_admin.TFhirEpisodeOfCareStatusHistoryList;
  TFhirEpisodeOfCareCareTeam = fhir2_resources_admin.TFhirEpisodeOfCareCareTeam;
  TFhirEpisodeOfCareCareTeamList = fhir2_resources_admin.TFhirEpisodeOfCareCareTeamList;
  TFhirEpisodeOfCareList = fhir2_resources_admin.TFhirEpisodeOfCareList;
  TFhirEpisodeOfCare = fhir2_resources_admin.TFhirEpisodeOfCare;
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  TFhirExplanationOfBenefitList = fhir2_resources_other.TFhirExplanationOfBenefitList;
  TFhirExplanationOfBenefit = fhir2_resources_other.TFhirExplanationOfBenefit;
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  TFhirFamilyMemberHistoryCondition = fhir2_resources_clinical.TFhirFamilyMemberHistoryCondition;
  TFhirFamilyMemberHistoryConditionList = fhir2_resources_clinical.TFhirFamilyMemberHistoryConditionList;
  TFhirFamilyMemberHistoryList = fhir2_resources_clinical.TFhirFamilyMemberHistoryList;
  TFhirFamilyMemberHistory = fhir2_resources_clinical.TFhirFamilyMemberHistory;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  TFhirFlagList = fhir2_resources_clinical.TFhirFlagList;
  TFhirFlag = fhir2_resources_clinical.TFhirFlag;
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  TFhirGoalOutcome = fhir2_resources_clinical.TFhirGoalOutcome;
  TFhirGoalOutcomeList = fhir2_resources_clinical.TFhirGoalOutcomeList;
  TFhirGoalList = fhir2_resources_clinical.TFhirGoalList;
  TFhirGoal = fhir2_resources_clinical.TFhirGoal;
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GROUP}
  TFhirGroupCharacteristic = fhir2_resources_admin.TFhirGroupCharacteristic;
  TFhirGroupCharacteristicList = fhir2_resources_admin.TFhirGroupCharacteristicList;
  TFhirGroupMember = fhir2_resources_admin.TFhirGroupMember;
  TFhirGroupMemberList = fhir2_resources_admin.TFhirGroupMemberList;
  TFhirGroupList = fhir2_resources_admin.TFhirGroupList;
  TFhirGroup = fhir2_resources_admin.TFhirGroup;
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_HEALTHCARESERVICE}
  TFhirHealthcareServiceServiceType = fhir2_resources_admin.TFhirHealthcareServiceServiceType;
  TFhirHealthcareServiceServiceTypeList = fhir2_resources_admin.TFhirHealthcareServiceServiceTypeList;
  TFhirHealthcareServiceAvailableTime = fhir2_resources_admin.TFhirHealthcareServiceAvailableTime;
  TFhirHealthcareServiceAvailableTimeList = fhir2_resources_admin.TFhirHealthcareServiceAvailableTimeList;
  TFhirHealthcareServiceNotAvailable = fhir2_resources_admin.TFhirHealthcareServiceNotAvailable;
  TFhirHealthcareServiceNotAvailableList = fhir2_resources_admin.TFhirHealthcareServiceNotAvailableList;
  TFhirHealthcareServiceList = fhir2_resources_admin.TFhirHealthcareServiceList;
  TFhirHealthcareService = fhir2_resources_admin.TFhirHealthcareService;
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGOBJECTSELECTION}
  TFhirImagingObjectSelectionStudy = fhir2_resources_clinical.TFhirImagingObjectSelectionStudy;
  TFhirImagingObjectSelectionStudyList = fhir2_resources_clinical.TFhirImagingObjectSelectionStudyList;
  TFhirImagingObjectSelectionStudySeries = fhir2_resources_clinical.TFhirImagingObjectSelectionStudySeries;
  TFhirImagingObjectSelectionStudySeriesList = fhir2_resources_clinical.TFhirImagingObjectSelectionStudySeriesList;
  TFhirImagingObjectSelectionStudySeriesInstance = fhir2_resources_clinical.TFhirImagingObjectSelectionStudySeriesInstance;
  TFhirImagingObjectSelectionStudySeriesInstanceList = fhir2_resources_clinical.TFhirImagingObjectSelectionStudySeriesInstanceList;
  TFhirImagingObjectSelectionStudySeriesInstanceFrames = fhir2_resources_clinical.TFhirImagingObjectSelectionStudySeriesInstanceFrames;
  TFhirImagingObjectSelectionStudySeriesInstanceFramesList = fhir2_resources_clinical.TFhirImagingObjectSelectionStudySeriesInstanceFramesList;
  TFhirImagingObjectSelectionList = fhir2_resources_clinical.TFhirImagingObjectSelectionList;
  TFhirImagingObjectSelection = fhir2_resources_clinical.TFhirImagingObjectSelection;
{$ENDIF FHIR_IMAGINGOBJECTSELECTION}
{$IFDEF FHIR_IMAGINGSTUDY}
  TFhirImagingStudySeries = fhir2_resources_clinical.TFhirImagingStudySeries;
  TFhirImagingStudySeriesList = fhir2_resources_clinical.TFhirImagingStudySeriesList;
  TFhirImagingStudySeriesInstance = fhir2_resources_clinical.TFhirImagingStudySeriesInstance;
  TFhirImagingStudySeriesInstanceList = fhir2_resources_clinical.TFhirImagingStudySeriesInstanceList;
  TFhirImagingStudyList = fhir2_resources_clinical.TFhirImagingStudyList;
  TFhirImagingStudy = fhir2_resources_clinical.TFhirImagingStudy;
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  TFhirImmunizationExplanation = fhir2_resources_clinical.TFhirImmunizationExplanation;
  TFhirImmunizationExplanationList = fhir2_resources_clinical.TFhirImmunizationExplanationList;
  TFhirImmunizationReaction = fhir2_resources_clinical.TFhirImmunizationReaction;
  TFhirImmunizationReactionList = fhir2_resources_clinical.TFhirImmunizationReactionList;
  TFhirImmunizationVaccinationProtocol = fhir2_resources_clinical.TFhirImmunizationVaccinationProtocol;
  TFhirImmunizationVaccinationProtocolList = fhir2_resources_clinical.TFhirImmunizationVaccinationProtocolList;
  TFhirImmunizationList = fhir2_resources_clinical.TFhirImmunizationList;
  TFhirImmunization = fhir2_resources_clinical.TFhirImmunization;
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  TFhirImmunizationRecommendationRecommendation = fhir2_resources_clinical.TFhirImmunizationRecommendationRecommendation;
  TFhirImmunizationRecommendationRecommendationList = fhir2_resources_clinical.TFhirImmunizationRecommendationRecommendationList;
  TFhirImmunizationRecommendationRecommendationDateCriterion = fhir2_resources_clinical.TFhirImmunizationRecommendationRecommendationDateCriterion;
  TFhirImmunizationRecommendationRecommendationDateCriterionList = fhir2_resources_clinical.TFhirImmunizationRecommendationRecommendationDateCriterionList;
  TFhirImmunizationRecommendationRecommendationProtocol = fhir2_resources_clinical.TFhirImmunizationRecommendationRecommendationProtocol;
  TFhirImmunizationRecommendationRecommendationProtocolList = fhir2_resources_clinical.TFhirImmunizationRecommendationRecommendationProtocolList;
  TFhirImmunizationRecommendationList = fhir2_resources_clinical.TFhirImmunizationRecommendationList;
  TFhirImmunizationRecommendation = fhir2_resources_clinical.TFhirImmunizationRecommendation;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  TFhirImplementationGuideContact = fhir2_resources_canonical.TFhirImplementationGuideContact;
  TFhirImplementationGuideContactList = fhir2_resources_canonical.TFhirImplementationGuideContactList;
  TFhirImplementationGuideDependency = fhir2_resources_canonical.TFhirImplementationGuideDependency;
  TFhirImplementationGuideDependencyList = fhir2_resources_canonical.TFhirImplementationGuideDependencyList;
  TFhirImplementationGuidePackage = fhir2_resources_canonical.TFhirImplementationGuidePackage;
  TFhirImplementationGuidePackageList = fhir2_resources_canonical.TFhirImplementationGuidePackageList;
  TFhirImplementationGuidePackageResource = fhir2_resources_canonical.TFhirImplementationGuidePackageResource;
  TFhirImplementationGuidePackageResourceList = fhir2_resources_canonical.TFhirImplementationGuidePackageResourceList;
  TFhirImplementationGuideGlobal = fhir2_resources_canonical.TFhirImplementationGuideGlobal;
  TFhirImplementationGuideGlobalList = fhir2_resources_canonical.TFhirImplementationGuideGlobalList;
  TFhirImplementationGuidePage = fhir2_resources_canonical.TFhirImplementationGuidePage;
  TFhirImplementationGuidePageList = fhir2_resources_canonical.TFhirImplementationGuidePageList;
  TFhirImplementationGuideList = fhir2_resources_canonical.TFhirImplementationGuideList;
  TFhirImplementationGuide = fhir2_resources_canonical.TFhirImplementationGuide;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_LIST}
  TFhirListEntry = fhir2_resources_other.TFhirListEntry;
  TFhirListEntryList = fhir2_resources_other.TFhirListEntryList;
  TFhirListList = fhir2_resources_other.TFhirListList;
  TFhirList = fhir2_resources_other.TFhirList;
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_LOCATION}
  TFhirLocationPosition = fhir2_resources_admin.TFhirLocationPosition;
  TFhirLocationPositionList = fhir2_resources_admin.TFhirLocationPositionList;
  TFhirLocationList = fhir2_resources_admin.TFhirLocationList;
  TFhirLocation = fhir2_resources_admin.TFhirLocation;
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_MEDIA}
  TFhirMediaList = fhir2_resources_clinical.TFhirMediaList;
  TFhirMedia = fhir2_resources_clinical.TFhirMedia;
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATION}
  TFhirMedicationProduct = fhir2_resources_other.TFhirMedicationProduct;
  TFhirMedicationProductList = fhir2_resources_other.TFhirMedicationProductList;
  TFhirMedicationProductIngredient = fhir2_resources_other.TFhirMedicationProductIngredient;
  TFhirMedicationProductIngredientList = fhir2_resources_other.TFhirMedicationProductIngredientList;
  TFhirMedicationProductBatch = fhir2_resources_other.TFhirMedicationProductBatch;
  TFhirMedicationProductBatchList = fhir2_resources_other.TFhirMedicationProductBatchList;
  TFhirMedicationPackage = fhir2_resources_other.TFhirMedicationPackage;
  TFhirMedicationPackageList = fhir2_resources_other.TFhirMedicationPackageList;
  TFhirMedicationPackageContent = fhir2_resources_other.TFhirMedicationPackageContent;
  TFhirMedicationPackageContentList = fhir2_resources_other.TFhirMedicationPackageContentList;
  TFhirMedicationList = fhir2_resources_other.TFhirMedicationList;
  TFhirMedication = fhir2_resources_other.TFhirMedication;
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  TFhirMedicationAdministrationDosage = fhir2_resources_clinical.TFhirMedicationAdministrationDosage;
  TFhirMedicationAdministrationDosageList = fhir2_resources_clinical.TFhirMedicationAdministrationDosageList;
  TFhirMedicationAdministrationList = fhir2_resources_clinical.TFhirMedicationAdministrationList;
  TFhirMedicationAdministration = fhir2_resources_clinical.TFhirMedicationAdministration;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  TFhirMedicationDispenseDosageInstruction = fhir2_resources_clinical.TFhirMedicationDispenseDosageInstruction;
  TFhirMedicationDispenseDosageInstructionList = fhir2_resources_clinical.TFhirMedicationDispenseDosageInstructionList;
  TFhirMedicationDispenseSubstitution = fhir2_resources_clinical.TFhirMedicationDispenseSubstitution;
  TFhirMedicationDispenseSubstitutionList = fhir2_resources_clinical.TFhirMedicationDispenseSubstitutionList;
  TFhirMedicationDispenseList = fhir2_resources_clinical.TFhirMedicationDispenseList;
  TFhirMedicationDispense = fhir2_resources_clinical.TFhirMedicationDispense;
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONORDER}
  TFhirMedicationOrderDosageInstruction = fhir2_resources_clinical.TFhirMedicationOrderDosageInstruction;
  TFhirMedicationOrderDosageInstructionList = fhir2_resources_clinical.TFhirMedicationOrderDosageInstructionList;
  TFhirMedicationOrderDispenseRequest = fhir2_resources_clinical.TFhirMedicationOrderDispenseRequest;
  TFhirMedicationOrderDispenseRequestList = fhir2_resources_clinical.TFhirMedicationOrderDispenseRequestList;
  TFhirMedicationOrderSubstitution = fhir2_resources_clinical.TFhirMedicationOrderSubstitution;
  TFhirMedicationOrderSubstitutionList = fhir2_resources_clinical.TFhirMedicationOrderSubstitutionList;
  TFhirMedicationOrderList = fhir2_resources_clinical.TFhirMedicationOrderList;
  TFhirMedicationOrder = fhir2_resources_clinical.TFhirMedicationOrder;
{$ENDIF FHIR_MEDICATIONORDER}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  TFhirMedicationStatementDosage = fhir2_resources_clinical.TFhirMedicationStatementDosage;
  TFhirMedicationStatementDosageList = fhir2_resources_clinical.TFhirMedicationStatementDosageList;
  TFhirMedicationStatementList = fhir2_resources_clinical.TFhirMedicationStatementList;
  TFhirMedicationStatement = fhir2_resources_clinical.TFhirMedicationStatement;
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_MESSAGEHEADER}
  TFhirMessageHeaderResponse = fhir2_resources_other.TFhirMessageHeaderResponse;
  TFhirMessageHeaderResponseList = fhir2_resources_other.TFhirMessageHeaderResponseList;
  TFhirMessageHeaderSource = fhir2_resources_other.TFhirMessageHeaderSource;
  TFhirMessageHeaderSourceList = fhir2_resources_other.TFhirMessageHeaderSourceList;
  TFhirMessageHeaderDestination = fhir2_resources_other.TFhirMessageHeaderDestination;
  TFhirMessageHeaderDestinationList = fhir2_resources_other.TFhirMessageHeaderDestinationList;
  TFhirMessageHeaderList = fhir2_resources_other.TFhirMessageHeaderList;
  TFhirMessageHeader = fhir2_resources_other.TFhirMessageHeader;
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_NAMINGSYSTEM}
  TFhirNamingSystemContact = fhir2_resources_canonical.TFhirNamingSystemContact;
  TFhirNamingSystemContactList = fhir2_resources_canonical.TFhirNamingSystemContactList;
  TFhirNamingSystemUniqueId = fhir2_resources_canonical.TFhirNamingSystemUniqueId;
  TFhirNamingSystemUniqueIdList = fhir2_resources_canonical.TFhirNamingSystemUniqueIdList;
  TFhirNamingSystemList = fhir2_resources_canonical.TFhirNamingSystemList;
  TFhirNamingSystem = fhir2_resources_canonical.TFhirNamingSystem;
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_NUTRITIONORDER}
  TFhirNutritionOrderOralDiet = fhir2_resources_clinical.TFhirNutritionOrderOralDiet;
  TFhirNutritionOrderOralDietList = fhir2_resources_clinical.TFhirNutritionOrderOralDietList;
  TFhirNutritionOrderOralDietNutrient = fhir2_resources_clinical.TFhirNutritionOrderOralDietNutrient;
  TFhirNutritionOrderOralDietNutrientList = fhir2_resources_clinical.TFhirNutritionOrderOralDietNutrientList;
  TFhirNutritionOrderOralDietTexture = fhir2_resources_clinical.TFhirNutritionOrderOralDietTexture;
  TFhirNutritionOrderOralDietTextureList = fhir2_resources_clinical.TFhirNutritionOrderOralDietTextureList;
  TFhirNutritionOrderSupplement = fhir2_resources_clinical.TFhirNutritionOrderSupplement;
  TFhirNutritionOrderSupplementList = fhir2_resources_clinical.TFhirNutritionOrderSupplementList;
  TFhirNutritionOrderEnteralFormula = fhir2_resources_clinical.TFhirNutritionOrderEnteralFormula;
  TFhirNutritionOrderEnteralFormulaList = fhir2_resources_clinical.TFhirNutritionOrderEnteralFormulaList;
  TFhirNutritionOrderEnteralFormulaAdministration = fhir2_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministration;
  TFhirNutritionOrderEnteralFormulaAdministrationList = fhir2_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministrationList;
  TFhirNutritionOrderList = fhir2_resources_clinical.TFhirNutritionOrderList;
  TFhirNutritionOrder = fhir2_resources_clinical.TFhirNutritionOrder;
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_OBSERVATION}
  TFhirObservationReferenceRange = fhir2_resources_clinical.TFhirObservationReferenceRange;
  TFhirObservationReferenceRangeList = fhir2_resources_clinical.TFhirObservationReferenceRangeList;
  TFhirObservationRelated = fhir2_resources_clinical.TFhirObservationRelated;
  TFhirObservationRelatedList = fhir2_resources_clinical.TFhirObservationRelatedList;
  TFhirObservationComponent = fhir2_resources_clinical.TFhirObservationComponent;
  TFhirObservationComponentList = fhir2_resources_clinical.TFhirObservationComponentList;
  TFhirObservationList = fhir2_resources_clinical.TFhirObservationList;
  TFhirObservation = fhir2_resources_clinical.TFhirObservation;
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  TFhirOperationDefinitionContact = fhir2_resources_canonical.TFhirOperationDefinitionContact;
  TFhirOperationDefinitionContactList = fhir2_resources_canonical.TFhirOperationDefinitionContactList;
  TFhirOperationDefinitionParameter = fhir2_resources_canonical.TFhirOperationDefinitionParameter;
  TFhirOperationDefinitionParameterList = fhir2_resources_canonical.TFhirOperationDefinitionParameterList;
  TFhirOperationDefinitionParameterBinding = fhir2_resources_canonical.TFhirOperationDefinitionParameterBinding;
  TFhirOperationDefinitionParameterBindingList = fhir2_resources_canonical.TFhirOperationDefinitionParameterBindingList;
  TFhirOperationDefinitionList = fhir2_resources_canonical.TFhirOperationDefinitionList;
  TFhirOperationDefinition = fhir2_resources_canonical.TFhirOperationDefinition;
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  TFhirOperationOutcomeIssue = fhir2_resources_other.TFhirOperationOutcomeIssue;
  TFhirOperationOutcomeIssueList = fhir2_resources_other.TFhirOperationOutcomeIssueList;
  TFhirOperationOutcomeList = fhir2_resources_other.TFhirOperationOutcomeList;
  TFhirOperationOutcome = fhir2_resources_other.TFhirOperationOutcome;
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_ORDER}
  TFhirOrderWhen = fhir2_resources_other.TFhirOrderWhen;
  TFhirOrderWhenList = fhir2_resources_other.TFhirOrderWhenList;
  TFhirOrderList = fhir2_resources_other.TFhirOrderList;
  TFhirOrder = fhir2_resources_other.TFhirOrder;
{$ENDIF FHIR_ORDER}
{$IFDEF FHIR_ORDERRESPONSE}
  TFhirOrderResponseList = fhir2_resources_other.TFhirOrderResponseList;
  TFhirOrderResponse = fhir2_resources_other.TFhirOrderResponse;
{$ENDIF FHIR_ORDERRESPONSE}
{$IFDEF FHIR_ORGANIZATION}
  TFhirOrganizationContact = fhir2_resources_admin.TFhirOrganizationContact;
  TFhirOrganizationContactList = fhir2_resources_admin.TFhirOrganizationContactList;
  TFhirOrganizationList = fhir2_resources_admin.TFhirOrganizationList;
  TFhirOrganization = fhir2_resources_admin.TFhirOrganization;
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_PATIENT}
  TFhirPatientContact = fhir2_resources_admin.TFhirPatientContact;
  TFhirPatientContactList = fhir2_resources_admin.TFhirPatientContactList;
  TFhirPatientAnimal = fhir2_resources_admin.TFhirPatientAnimal;
  TFhirPatientAnimalList = fhir2_resources_admin.TFhirPatientAnimalList;
  TFhirPatientCommunication = fhir2_resources_admin.TFhirPatientCommunication;
  TFhirPatientCommunicationList = fhir2_resources_admin.TFhirPatientCommunicationList;
  TFhirPatientLink = fhir2_resources_admin.TFhirPatientLink;
  TFhirPatientLinkList = fhir2_resources_admin.TFhirPatientLinkList;
  TFhirPatientList = fhir2_resources_admin.TFhirPatientList;
  TFhirPatient = fhir2_resources_admin.TFhirPatient;
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PAYMENTNOTICE}
  TFhirPaymentNoticeList = fhir2_resources_other.TFhirPaymentNoticeList;
  TFhirPaymentNotice = fhir2_resources_other.TFhirPaymentNotice;
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  TFhirPaymentReconciliationDetail = fhir2_resources_other.TFhirPaymentReconciliationDetail;
  TFhirPaymentReconciliationDetailList = fhir2_resources_other.TFhirPaymentReconciliationDetailList;
  TFhirPaymentReconciliationNote = fhir2_resources_other.TFhirPaymentReconciliationNote;
  TFhirPaymentReconciliationNoteList = fhir2_resources_other.TFhirPaymentReconciliationNoteList;
  TFhirPaymentReconciliationList = fhir2_resources_other.TFhirPaymentReconciliationList;
  TFhirPaymentReconciliation = fhir2_resources_other.TFhirPaymentReconciliation;
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERSON}
  TFhirPersonLink = fhir2_resources_admin.TFhirPersonLink;
  TFhirPersonLinkList = fhir2_resources_admin.TFhirPersonLinkList;
  TFhirPersonList = fhir2_resources_admin.TFhirPersonList;
  TFhirPerson = fhir2_resources_admin.TFhirPerson;
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PRACTITIONER}
  TFhirPractitionerPractitionerRole = fhir2_resources_admin.TFhirPractitionerPractitionerRole;
  TFhirPractitionerPractitionerRoleList = fhir2_resources_admin.TFhirPractitionerPractitionerRoleList;
  TFhirPractitionerQualification = fhir2_resources_admin.TFhirPractitionerQualification;
  TFhirPractitionerQualificationList = fhir2_resources_admin.TFhirPractitionerQualificationList;
  TFhirPractitionerList = fhir2_resources_admin.TFhirPractitionerList;
  TFhirPractitioner = fhir2_resources_admin.TFhirPractitioner;
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PROCEDURE}
  TFhirProcedurePerformer = fhir2_resources_clinical.TFhirProcedurePerformer;
  TFhirProcedurePerformerList = fhir2_resources_clinical.TFhirProcedurePerformerList;
  TFhirProcedureFocalDevice = fhir2_resources_clinical.TFhirProcedureFocalDevice;
  TFhirProcedureFocalDeviceList = fhir2_resources_clinical.TFhirProcedureFocalDeviceList;
  TFhirProcedureList = fhir2_resources_clinical.TFhirProcedureList;
  TFhirProcedure = fhir2_resources_clinical.TFhirProcedure;
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROCEDUREREQUEST}
  TFhirProcedureRequestList = fhir2_resources_clinical.TFhirProcedureRequestList;
  TFhirProcedureRequest = fhir2_resources_clinical.TFhirProcedureRequest;
{$ENDIF FHIR_PROCEDUREREQUEST}
{$IFDEF FHIR_PROCESSREQUEST}
  TFhirProcessRequestItem = fhir2_resources_other.TFhirProcessRequestItem;
  TFhirProcessRequestItemList = fhir2_resources_other.TFhirProcessRequestItemList;
  TFhirProcessRequestList = fhir2_resources_other.TFhirProcessRequestList;
  TFhirProcessRequest = fhir2_resources_other.TFhirProcessRequest;
{$ENDIF FHIR_PROCESSREQUEST}
{$IFDEF FHIR_PROCESSRESPONSE}
  TFhirProcessResponseNotes = fhir2_resources_other.TFhirProcessResponseNotes;
  TFhirProcessResponseNotesList = fhir2_resources_other.TFhirProcessResponseNotesList;
  TFhirProcessResponseList = fhir2_resources_other.TFhirProcessResponseList;
  TFhirProcessResponse = fhir2_resources_other.TFhirProcessResponse;
{$ENDIF FHIR_PROCESSRESPONSE}
{$IFDEF FHIR_PROVENANCE}
  TFhirProvenanceAgent = fhir2_resources_other.TFhirProvenanceAgent;
  TFhirProvenanceAgentList = fhir2_resources_other.TFhirProvenanceAgentList;
  TFhirProvenanceAgentRelatedAgent = fhir2_resources_other.TFhirProvenanceAgentRelatedAgent;
  TFhirProvenanceAgentRelatedAgentList = fhir2_resources_other.TFhirProvenanceAgentRelatedAgentList;
  TFhirProvenanceEntity = fhir2_resources_other.TFhirProvenanceEntity;
  TFhirProvenanceEntityList = fhir2_resources_other.TFhirProvenanceEntityList;
  TFhirProvenanceList = fhir2_resources_other.TFhirProvenanceList;
  TFhirProvenance = fhir2_resources_other.TFhirProvenance;
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  TFhirQuestionnaireGroup = fhir2_resources_canonical.TFhirQuestionnaireGroup;
  TFhirQuestionnaireGroupList = fhir2_resources_canonical.TFhirQuestionnaireGroupList;
  TFhirQuestionnaireGroupQuestion = fhir2_resources_canonical.TFhirQuestionnaireGroupQuestion;
  TFhirQuestionnaireGroupQuestionList = fhir2_resources_canonical.TFhirQuestionnaireGroupQuestionList;
  TFhirQuestionnaireList = fhir2_resources_canonical.TFhirQuestionnaireList;
  TFhirQuestionnaire = fhir2_resources_canonical.TFhirQuestionnaire;
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  TFhirQuestionnaireResponseGroup = fhir2_resources_clinical.TFhirQuestionnaireResponseGroup;
  TFhirQuestionnaireResponseGroupList = fhir2_resources_clinical.TFhirQuestionnaireResponseGroupList;
  TFhirQuestionnaireResponseGroupQuestion = fhir2_resources_clinical.TFhirQuestionnaireResponseGroupQuestion;
  TFhirQuestionnaireResponseGroupQuestionList = fhir2_resources_clinical.TFhirQuestionnaireResponseGroupQuestionList;
  TFhirQuestionnaireResponseGroupQuestionAnswer = fhir2_resources_clinical.TFhirQuestionnaireResponseGroupQuestionAnswer;
  TFhirQuestionnaireResponseGroupQuestionAnswerList = fhir2_resources_clinical.TFhirQuestionnaireResponseGroupQuestionAnswerList;
  TFhirQuestionnaireResponseList = fhir2_resources_clinical.TFhirQuestionnaireResponseList;
  TFhirQuestionnaireResponse = fhir2_resources_clinical.TFhirQuestionnaireResponse;
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REFERRALREQUEST}
  TFhirReferralRequestList = fhir2_resources_clinical.TFhirReferralRequestList;
  TFhirReferralRequest = fhir2_resources_clinical.TFhirReferralRequest;
{$ENDIF FHIR_REFERRALREQUEST}
{$IFDEF FHIR_RELATEDPERSON}
  TFhirRelatedPersonList = fhir2_resources_admin.TFhirRelatedPersonList;
  TFhirRelatedPerson = fhir2_resources_admin.TFhirRelatedPerson;
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_RISKASSESSMENT}
  TFhirRiskAssessmentPrediction = fhir2_resources_clinical.TFhirRiskAssessmentPrediction;
  TFhirRiskAssessmentPredictionList = fhir2_resources_clinical.TFhirRiskAssessmentPredictionList;
  TFhirRiskAssessmentList = fhir2_resources_clinical.TFhirRiskAssessmentList;
  TFhirRiskAssessment = fhir2_resources_clinical.TFhirRiskAssessment;
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_SCHEDULE}
  TFhirScheduleList = fhir2_resources_admin.TFhirScheduleList;
  TFhirSchedule = fhir2_resources_admin.TFhirSchedule;
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SEARCHPARAMETER}
  TFhirSearchParameterContact = fhir2_resources_canonical.TFhirSearchParameterContact;
  TFhirSearchParameterContactList = fhir2_resources_canonical.TFhirSearchParameterContactList;
  TFhirSearchParameterList = fhir2_resources_canonical.TFhirSearchParameterList;
  TFhirSearchParameter = fhir2_resources_canonical.TFhirSearchParameter;
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SLOT}
  TFhirSlotList = fhir2_resources_admin.TFhirSlotList;
  TFhirSlot = fhir2_resources_admin.TFhirSlot;
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  TFhirSpecimenCollection = fhir2_resources_clinical.TFhirSpecimenCollection;
  TFhirSpecimenCollectionList = fhir2_resources_clinical.TFhirSpecimenCollectionList;
  TFhirSpecimenTreatment = fhir2_resources_clinical.TFhirSpecimenTreatment;
  TFhirSpecimenTreatmentList = fhir2_resources_clinical.TFhirSpecimenTreatmentList;
  TFhirSpecimenContainer = fhir2_resources_clinical.TFhirSpecimenContainer;
  TFhirSpecimenContainerList = fhir2_resources_clinical.TFhirSpecimenContainerList;
  TFhirSpecimenList = fhir2_resources_clinical.TFhirSpecimenList;
  TFhirSpecimen = fhir2_resources_clinical.TFhirSpecimen;
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  TFhirStructureDefinitionContact = fhir2_resources_canonical.TFhirStructureDefinitionContact;
  TFhirStructureDefinitionContactList = fhir2_resources_canonical.TFhirStructureDefinitionContactList;
  TFhirStructureDefinitionMapping = fhir2_resources_canonical.TFhirStructureDefinitionMapping;
  TFhirStructureDefinitionMappingList = fhir2_resources_canonical.TFhirStructureDefinitionMappingList;
  TFhirStructureDefinitionSnapshot = fhir2_resources_canonical.TFhirStructureDefinitionSnapshot;
  TFhirStructureDefinitionSnapshotList = fhir2_resources_canonical.TFhirStructureDefinitionSnapshotList;
  TFhirStructureDefinitionDifferential = fhir2_resources_canonical.TFhirStructureDefinitionDifferential;
  TFhirStructureDefinitionDifferentialList = fhir2_resources_canonical.TFhirStructureDefinitionDifferentialList;
  TFhirStructureDefinitionList = fhir2_resources_canonical.TFhirStructureDefinitionList;
  TFhirStructureDefinition = fhir2_resources_canonical.TFhirStructureDefinition;
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_SUBSCRIPTION}
  TFhirSubscriptionChannel = fhir2_resources_other.TFhirSubscriptionChannel;
  TFhirSubscriptionChannelList = fhir2_resources_other.TFhirSubscriptionChannelList;
  TFhirSubscriptionList = fhir2_resources_other.TFhirSubscriptionList;
  TFhirSubscription = fhir2_resources_other.TFhirSubscription;
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSTANCE}
  TFhirSubstanceInstance = fhir2_resources_admin.TFhirSubstanceInstance;
  TFhirSubstanceInstanceList = fhir2_resources_admin.TFhirSubstanceInstanceList;
  TFhirSubstanceIngredient = fhir2_resources_admin.TFhirSubstanceIngredient;
  TFhirSubstanceIngredientList = fhir2_resources_admin.TFhirSubstanceIngredientList;
  TFhirSubstanceList = fhir2_resources_admin.TFhirSubstanceList;
  TFhirSubstance = fhir2_resources_admin.TFhirSubstance;
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUPPLYDELIVERY}
  TFhirSupplyDeliveryList = fhir2_resources_clinical.TFhirSupplyDeliveryList;
  TFhirSupplyDelivery = fhir2_resources_clinical.TFhirSupplyDelivery;
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  TFhirSupplyRequestWhen = fhir2_resources_clinical.TFhirSupplyRequestWhen;
  TFhirSupplyRequestWhenList = fhir2_resources_clinical.TFhirSupplyRequestWhenList;
  TFhirSupplyRequestList = fhir2_resources_clinical.TFhirSupplyRequestList;
  TFhirSupplyRequest = fhir2_resources_clinical.TFhirSupplyRequest;
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TESTSCRIPT}
  TFhirTestScriptContact = fhir2_resources_canonical.TFhirTestScriptContact;
  TFhirTestScriptContactList = fhir2_resources_canonical.TFhirTestScriptContactList;
  TFhirTestScriptMetadata = fhir2_resources_canonical.TFhirTestScriptMetadata;
  TFhirTestScriptMetadataList = fhir2_resources_canonical.TFhirTestScriptMetadataList;
  TFhirTestScriptMetadataLink = fhir2_resources_canonical.TFhirTestScriptMetadataLink;
  TFhirTestScriptMetadataLinkList = fhir2_resources_canonical.TFhirTestScriptMetadataLinkList;
  TFhirTestScriptMetadataCapability = fhir2_resources_canonical.TFhirTestScriptMetadataCapability;
  TFhirTestScriptMetadataCapabilityList = fhir2_resources_canonical.TFhirTestScriptMetadataCapabilityList;
  TFhirTestScriptFixture = fhir2_resources_canonical.TFhirTestScriptFixture;
  TFhirTestScriptFixtureList = fhir2_resources_canonical.TFhirTestScriptFixtureList;
  TFhirTestScriptVariable = fhir2_resources_canonical.TFhirTestScriptVariable;
  TFhirTestScriptVariableList = fhir2_resources_canonical.TFhirTestScriptVariableList;
  TFhirTestScriptSetup = fhir2_resources_canonical.TFhirTestScriptSetup;
  TFhirTestScriptSetupList = fhir2_resources_canonical.TFhirTestScriptSetupList;
  TFhirTestScriptSetupAction = fhir2_resources_canonical.TFhirTestScriptSetupAction;
  TFhirTestScriptSetupActionList = fhir2_resources_canonical.TFhirTestScriptSetupActionList;
  TFhirTestScriptSetupActionOperation = fhir2_resources_canonical.TFhirTestScriptSetupActionOperation;
  TFhirTestScriptSetupActionOperationList = fhir2_resources_canonical.TFhirTestScriptSetupActionOperationList;
  TFhirTestScriptSetupActionOperationRequestHeader = fhir2_resources_canonical.TFhirTestScriptSetupActionOperationRequestHeader;
  TFhirTestScriptSetupActionOperationRequestHeaderList = fhir2_resources_canonical.TFhirTestScriptSetupActionOperationRequestHeaderList;
  TFhirTestScriptSetupActionAssert = fhir2_resources_canonical.TFhirTestScriptSetupActionAssert;
  TFhirTestScriptSetupActionAssertList = fhir2_resources_canonical.TFhirTestScriptSetupActionAssertList;
  TFhirTestScriptTest = fhir2_resources_canonical.TFhirTestScriptTest;
  TFhirTestScriptTestList = fhir2_resources_canonical.TFhirTestScriptTestList;
  TFhirTestScriptTestAction = fhir2_resources_canonical.TFhirTestScriptTestAction;
  TFhirTestScriptTestActionList = fhir2_resources_canonical.TFhirTestScriptTestActionList;
  TFhirTestScriptTeardown = fhir2_resources_canonical.TFhirTestScriptTeardown;
  TFhirTestScriptTeardownList = fhir2_resources_canonical.TFhirTestScriptTeardownList;
  TFhirTestScriptTeardownAction = fhir2_resources_canonical.TFhirTestScriptTeardownAction;
  TFhirTestScriptTeardownActionList = fhir2_resources_canonical.TFhirTestScriptTeardownActionList;
  TFhirTestScriptList = fhir2_resources_canonical.TFhirTestScriptList;
  TFhirTestScript = fhir2_resources_canonical.TFhirTestScript;
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  TFhirValueSetContact = fhir2_resources_canonical.TFhirValueSetContact;
  TFhirValueSetContactList = fhir2_resources_canonical.TFhirValueSetContactList;
  TFhirValueSetCodeSystem = fhir2_resources_canonical.TFhirValueSetCodeSystem;
  TFhirValueSetCodeSystemList = fhir2_resources_canonical.TFhirValueSetCodeSystemList;
  TFhirValueSetCodeSystemConcept = fhir2_resources_canonical.TFhirValueSetCodeSystemConcept;
  TFhirValueSetCodeSystemConceptList = fhir2_resources_canonical.TFhirValueSetCodeSystemConceptList;
  TFhirValueSetCodeSystemConceptDesignation = fhir2_resources_canonical.TFhirValueSetCodeSystemConceptDesignation;
  TFhirValueSetCodeSystemConceptDesignationList = fhir2_resources_canonical.TFhirValueSetCodeSystemConceptDesignationList;
  TFhirValueSetCompose = fhir2_resources_canonical.TFhirValueSetCompose;
  TFhirValueSetComposeList = fhir2_resources_canonical.TFhirValueSetComposeList;
  TFhirValueSetComposeInclude = fhir2_resources_canonical.TFhirValueSetComposeInclude;
  TFhirValueSetComposeIncludeList = fhir2_resources_canonical.TFhirValueSetComposeIncludeList;
  TFhirValueSetComposeIncludeConcept = fhir2_resources_canonical.TFhirValueSetComposeIncludeConcept;
  TFhirValueSetComposeIncludeConceptList = fhir2_resources_canonical.TFhirValueSetComposeIncludeConceptList;
  TFhirValueSetComposeIncludeFilter = fhir2_resources_canonical.TFhirValueSetComposeIncludeFilter;
  TFhirValueSetComposeIncludeFilterList = fhir2_resources_canonical.TFhirValueSetComposeIncludeFilterList;
  TFhirValueSetExpansion = fhir2_resources_canonical.TFhirValueSetExpansion;
  TFhirValueSetExpansionList = fhir2_resources_canonical.TFhirValueSetExpansionList;
  TFhirValueSetExpansionParameter = fhir2_resources_canonical.TFhirValueSetExpansionParameter;
  TFhirValueSetExpansionParameterList = fhir2_resources_canonical.TFhirValueSetExpansionParameterList;
  TFhirValueSetExpansionContains = fhir2_resources_canonical.TFhirValueSetExpansionContains;
  TFhirValueSetExpansionContainsList = fhir2_resources_canonical.TFhirValueSetExpansionContainsList;
  TFhirValueSetList = fhir2_resources_canonical.TFhirValueSetList;
  TFhirValueSet = fhir2_resources_canonical.TFhirValueSet;
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  TFhirVisionPrescriptionDispense = fhir2_resources_clinical.TFhirVisionPrescriptionDispense;
  TFhirVisionPrescriptionDispenseList = fhir2_resources_clinical.TFhirVisionPrescriptionDispenseList;
  TFhirVisionPrescriptionList = fhir2_resources_clinical.TFhirVisionPrescriptionList;
  TFhirVisionPrescription = fhir2_resources_clinical.TFhirVisionPrescription;
{$ENDIF FHIR_VISIONPRESCRIPTION}

implementation

end.

