unit FHIR.R2.Resources;

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

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
{$I fhir.r2.inc}

interface

// FHIR v1.0.2 generated 2015-10-24T07:41:03+11:00

uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Utilities, FHIR.Base.Lang,
  FHIR.R2.Base, FHIR.R2.Types,
  FHIR.R2.Resources.Base, FHIR.R2.Resources.Canonical, FHIR.R2.Resources.Admin, FHIR.R2.Resources.Clinical, FHIR.R2.Resources.Other;

Type
  TFhirResourceClass = FHIR.R2.Resources.Base.TFhirResourceClass;
  TFhirResourceType = FHIR.R2.Resources.Base.TFhirResourceType;
  TFhirResourceTypeSet = FHIR.R2.Resources.Base.TFhirResourceTypeSet;
  TFhirResource = FHIR.R2.Resources.Base.TFhirResource;
  TFhirDomainResource = FHIR.R2.Resources.Base.TFhirDomainResource;
  TFHIRMetadataResource = FHIR.R2.Resources.Canonical.TFHIRMetadataResource;
  TFhirResourceList = FHIR.R2.Resources.Base.TFhirResourceList;
  TFhirDomainResourceList = FHIR.R2.Resources.Base.TFhirDomainResourceList;
{$IFDEF FHIR_PARAMETERS}
  TFhirParametersParameter = FHIR.R2.Resources.Other.TFhirParametersParameter;
  TFhirParametersParameterList = FHIR.R2.Resources.Other.TFhirParametersParameterList;
  TFhirParametersList = FHIR.R2.Resources.Other.TFhirParametersList;
  TFhirParameters = FHIR.R2.Resources.Other.TFhirParameters;
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_ACCOUNT}
  TFhirAccountList = FHIR.R2.Resources.Clinical.TFhirAccountList;
  TFhirAccount = FHIR.R2.Resources.Clinical.TFhirAccount;
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  TFhirAllergyIntoleranceReaction = FHIR.R2.Resources.Clinical.TFhirAllergyIntoleranceReaction;
  TFhirAllergyIntoleranceReactionList = FHIR.R2.Resources.Clinical.TFhirAllergyIntoleranceReactionList;
  TFhirAllergyIntoleranceList = FHIR.R2.Resources.Clinical.TFhirAllergyIntoleranceList;
  TFhirAllergyIntolerance = FHIR.R2.Resources.Clinical.TFhirAllergyIntolerance;
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  TFhirAppointmentParticipant = FHIR.R2.Resources.Clinical.TFhirAppointmentParticipant;
  TFhirAppointmentParticipantList = FHIR.R2.Resources.Clinical.TFhirAppointmentParticipantList;
  TFhirAppointmentList = FHIR.R2.Resources.Clinical.TFhirAppointmentList;
  TFhirAppointment = FHIR.R2.Resources.Clinical.TFhirAppointment;
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  TFhirAppointmentResponseList = FHIR.R2.Resources.Clinical.TFhirAppointmentResponseList;
  TFhirAppointmentResponse = FHIR.R2.Resources.Clinical.TFhirAppointmentResponse;
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_AUDITEVENT}
  TFhirAuditEventEvent = FHIR.R2.Resources.Other.TFhirAuditEventEvent;
  TFhirAuditEventEventList = FHIR.R2.Resources.Other.TFhirAuditEventEventList;
  TFhirAuditEventParticipant = FHIR.R2.Resources.Other.TFhirAuditEventParticipant;
  TFhirAuditEventParticipantList = FHIR.R2.Resources.Other.TFhirAuditEventParticipantList;
  TFhirAuditEventParticipantNetwork = FHIR.R2.Resources.Other.TFhirAuditEventParticipantNetwork;
  TFhirAuditEventParticipantNetworkList = FHIR.R2.Resources.Other.TFhirAuditEventParticipantNetworkList;
  TFhirAuditEventSource = FHIR.R2.Resources.Other.TFhirAuditEventSource;
  TFhirAuditEventSourceList = FHIR.R2.Resources.Other.TFhirAuditEventSourceList;
  TFhirAuditEventObject = FHIR.R2.Resources.Other.TFhirAuditEventObject;
  TFhirAuditEventObjectList = FHIR.R2.Resources.Other.TFhirAuditEventObjectList;
  TFhirAuditEventObjectDetail = FHIR.R2.Resources.Other.TFhirAuditEventObjectDetail;
  TFhirAuditEventObjectDetailList = FHIR.R2.Resources.Other.TFhirAuditEventObjectDetailList;
  TFhirAuditEventList = FHIR.R2.Resources.Other.TFhirAuditEventList;
  TFhirAuditEvent = FHIR.R2.Resources.Other.TFhirAuditEvent;
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BASIC}
  TFhirBasicList = FHIR.R2.Resources.Clinical.TFhirBasicList;
  TFhirBasic = FHIR.R2.Resources.Clinical.TFhirBasic;
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BINARY}
  TFhirBinaryList = FHIR.R2.Resources.Other.TFhirBinaryList;
  TFhirBinary = FHIR.R2.Resources.Other.TFhirBinary;
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BODYSITE}
  TFhirBodySiteList = FHIR.R2.Resources.Clinical.TFhirBodySiteList;
  TFhirBodySite = FHIR.R2.Resources.Clinical.TFhirBodySite;
{$ENDIF FHIR_BODYSITE}
{$IFDEF FHIR_BUNDLE}
  TFhirBundleLink = FHIR.R2.Resources.Other.TFhirBundleLink;
  TFhirBundleLinkList = FHIR.R2.Resources.Other.TFhirBundleLinkList;
  TFhirBundleEntry = FHIR.R2.Resources.Other.TFhirBundleEntry;
  TFhirBundleEntryList = FHIR.R2.Resources.Other.TFhirBundleEntryList;
  TFhirBundleEntrySearch = FHIR.R2.Resources.Other.TFhirBundleEntrySearch;
  TFhirBundleEntrySearchList = FHIR.R2.Resources.Other.TFhirBundleEntrySearchList;
  TFhirBundleEntryRequest = FHIR.R2.Resources.Other.TFhirBundleEntryRequest;
  TFhirBundleEntryRequestList = FHIR.R2.Resources.Other.TFhirBundleEntryRequestList;
  TFhirBundleEntryResponse = FHIR.R2.Resources.Other.TFhirBundleEntryResponse;
  TFhirBundleEntryResponseList = FHIR.R2.Resources.Other.TFhirBundleEntryResponseList;
  TFhirBundleList = FHIR.R2.Resources.Other.TFhirBundleList;
  TFhirBundle = FHIR.R2.Resources.Other.TFhirBundle;
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CAREPLAN}
  TFhirCarePlanRelatedPlan = FHIR.R2.Resources.Clinical.TFhirCarePlanRelatedPlan;
  TFhirCarePlanRelatedPlanList = FHIR.R2.Resources.Clinical.TFhirCarePlanRelatedPlanList;
  TFhirCarePlanParticipant = FHIR.R2.Resources.Clinical.TFhirCarePlanParticipant;
  TFhirCarePlanParticipantList = FHIR.R2.Resources.Clinical.TFhirCarePlanParticipantList;
  TFhirCarePlanActivity = FHIR.R2.Resources.Clinical.TFhirCarePlanActivity;
  TFhirCarePlanActivityList = FHIR.R2.Resources.Clinical.TFhirCarePlanActivityList;
  TFhirCarePlanActivityDetail = FHIR.R2.Resources.Clinical.TFhirCarePlanActivityDetail;
  TFhirCarePlanActivityDetailList = FHIR.R2.Resources.Clinical.TFhirCarePlanActivityDetailList;
  TFhirCarePlanList = FHIR.R2.Resources.Clinical.TFhirCarePlanList;
  TFhirCarePlan = FHIR.R2.Resources.Clinical.TFhirCarePlan;
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CLAIM}
  TFhirClaimPayee = FHIR.R2.Resources.Other.TFhirClaimPayee;
  TFhirClaimPayeeList = FHIR.R2.Resources.Other.TFhirClaimPayeeList;
  TFhirClaimDiagnosis = FHIR.R2.Resources.Other.TFhirClaimDiagnosis;
  TFhirClaimDiagnosisList = FHIR.R2.Resources.Other.TFhirClaimDiagnosisList;
  TFhirClaimCoverage = FHIR.R2.Resources.Other.TFhirClaimCoverage;
  TFhirClaimCoverageList = FHIR.R2.Resources.Other.TFhirClaimCoverageList;
  TFhirClaimItem = FHIR.R2.Resources.Other.TFhirClaimItem;
  TFhirClaimItemList = FHIR.R2.Resources.Other.TFhirClaimItemList;
  TFhirClaimItemDetail = FHIR.R2.Resources.Other.TFhirClaimItemDetail;
  TFhirClaimItemDetailList = FHIR.R2.Resources.Other.TFhirClaimItemDetailList;
  TFhirClaimItemDetailSubDetail = FHIR.R2.Resources.Other.TFhirClaimItemDetailSubDetail;
  TFhirClaimItemDetailSubDetailList = FHIR.R2.Resources.Other.TFhirClaimItemDetailSubDetailList;
  TFhirClaimItemProsthesis = FHIR.R2.Resources.Other.TFhirClaimItemProsthesis;
  TFhirClaimItemProsthesisList = FHIR.R2.Resources.Other.TFhirClaimItemProsthesisList;
  TFhirClaimMissingTeeth = FHIR.R2.Resources.Other.TFhirClaimMissingTeeth;
  TFhirClaimMissingTeethList = FHIR.R2.Resources.Other.TFhirClaimMissingTeethList;
  TFhirClaimList = FHIR.R2.Resources.Other.TFhirClaimList;
  TFhirClaim = FHIR.R2.Resources.Other.TFhirClaim;
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  TFhirClaimResponseItem = FHIR.R2.Resources.Other.TFhirClaimResponseItem;
  TFhirClaimResponseItemList = FHIR.R2.Resources.Other.TFhirClaimResponseItemList;
  TFhirClaimResponseItemAdjudication = FHIR.R2.Resources.Other.TFhirClaimResponseItemAdjudication;
  TFhirClaimResponseItemAdjudicationList = FHIR.R2.Resources.Other.TFhirClaimResponseItemAdjudicationList;
  TFhirClaimResponseItemDetail = FHIR.R2.Resources.Other.TFhirClaimResponseItemDetail;
  TFhirClaimResponseItemDetailList = FHIR.R2.Resources.Other.TFhirClaimResponseItemDetailList;
  TFhirClaimResponseItemDetailAdjudication = FHIR.R2.Resources.Other.TFhirClaimResponseItemDetailAdjudication;
  TFhirClaimResponseItemDetailAdjudicationList = FHIR.R2.Resources.Other.TFhirClaimResponseItemDetailAdjudicationList;
  TFhirClaimResponseItemDetailSubDetail = FHIR.R2.Resources.Other.TFhirClaimResponseItemDetailSubDetail;
  TFhirClaimResponseItemDetailSubDetailList = FHIR.R2.Resources.Other.TFhirClaimResponseItemDetailSubDetailList;
  TFhirClaimResponseItemDetailSubDetailAdjudication = FHIR.R2.Resources.Other.TFhirClaimResponseItemDetailSubDetailAdjudication;
  TFhirClaimResponseItemDetailSubDetailAdjudicationList = FHIR.R2.Resources.Other.TFhirClaimResponseItemDetailSubDetailAdjudicationList;
  TFhirClaimResponseAddItem = FHIR.R2.Resources.Other.TFhirClaimResponseAddItem;
  TFhirClaimResponseAddItemList = FHIR.R2.Resources.Other.TFhirClaimResponseAddItemList;
  TFhirClaimResponseAddItemAdjudication = FHIR.R2.Resources.Other.TFhirClaimResponseAddItemAdjudication;
  TFhirClaimResponseAddItemAdjudicationList = FHIR.R2.Resources.Other.TFhirClaimResponseAddItemAdjudicationList;
  TFhirClaimResponseAddItemDetail = FHIR.R2.Resources.Other.TFhirClaimResponseAddItemDetail;
  TFhirClaimResponseAddItemDetailList = FHIR.R2.Resources.Other.TFhirClaimResponseAddItemDetailList;
  TFhirClaimResponseAddItemDetailAdjudication = FHIR.R2.Resources.Other.TFhirClaimResponseAddItemDetailAdjudication;
  TFhirClaimResponseAddItemDetailAdjudicationList = FHIR.R2.Resources.Other.TFhirClaimResponseAddItemDetailAdjudicationList;
  TFhirClaimResponseError = FHIR.R2.Resources.Other.TFhirClaimResponseError;
  TFhirClaimResponseErrorList = FHIR.R2.Resources.Other.TFhirClaimResponseErrorList;
  TFhirClaimResponseNote = FHIR.R2.Resources.Other.TFhirClaimResponseNote;
  TFhirClaimResponseNoteList = FHIR.R2.Resources.Other.TFhirClaimResponseNoteList;
  TFhirClaimResponseCoverage = FHIR.R2.Resources.Other.TFhirClaimResponseCoverage;
  TFhirClaimResponseCoverageList = FHIR.R2.Resources.Other.TFhirClaimResponseCoverageList;
  TFhirClaimResponseList = FHIR.R2.Resources.Other.TFhirClaimResponseList;
  TFhirClaimResponse = FHIR.R2.Resources.Other.TFhirClaimResponse;
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  TFhirClinicalImpressionInvestigations = FHIR.R2.Resources.Clinical.TFhirClinicalImpressionInvestigations;
  TFhirClinicalImpressionInvestigationsList = FHIR.R2.Resources.Clinical.TFhirClinicalImpressionInvestigationsList;
  TFhirClinicalImpressionFinding = FHIR.R2.Resources.Clinical.TFhirClinicalImpressionFinding;
  TFhirClinicalImpressionFindingList = FHIR.R2.Resources.Clinical.TFhirClinicalImpressionFindingList;
  TFhirClinicalImpressionRuledOut = FHIR.R2.Resources.Clinical.TFhirClinicalImpressionRuledOut;
  TFhirClinicalImpressionRuledOutList = FHIR.R2.Resources.Clinical.TFhirClinicalImpressionRuledOutList;
  TFhirClinicalImpressionList = FHIR.R2.Resources.Clinical.TFhirClinicalImpressionList;
  TFhirClinicalImpression = FHIR.R2.Resources.Clinical.TFhirClinicalImpression;
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_COMMUNICATION}
  TFhirCommunicationPayload = FHIR.R2.Resources.Clinical.TFhirCommunicationPayload;
  TFhirCommunicationPayloadList = FHIR.R2.Resources.Clinical.TFhirCommunicationPayloadList;
  TFhirCommunicationList = FHIR.R2.Resources.Clinical.TFhirCommunicationList;
  TFhirCommunication = FHIR.R2.Resources.Clinical.TFhirCommunication;
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  TFhirCommunicationRequestPayload = FHIR.R2.Resources.Clinical.TFhirCommunicationRequestPayload;
  TFhirCommunicationRequestPayloadList = FHIR.R2.Resources.Clinical.TFhirCommunicationRequestPayloadList;
  TFhirCommunicationRequestList = FHIR.R2.Resources.Clinical.TFhirCommunicationRequestList;
  TFhirCommunicationRequest = FHIR.R2.Resources.Clinical.TFhirCommunicationRequest;
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPOSITION}
  TFhirCompositionAttester = FHIR.R2.Resources.Clinical.TFhirCompositionAttester;
  TFhirCompositionAttesterList = FHIR.R2.Resources.Clinical.TFhirCompositionAttesterList;
  TFhirCompositionEvent = FHIR.R2.Resources.Clinical.TFhirCompositionEvent;
  TFhirCompositionEventList = FHIR.R2.Resources.Clinical.TFhirCompositionEventList;
  TFhirCompositionSection = FHIR.R2.Resources.Clinical.TFhirCompositionSection;
  TFhirCompositionSectionList = FHIR.R2.Resources.Clinical.TFhirCompositionSectionList;
  TFhirCompositionList = FHIR.R2.Resources.Clinical.TFhirCompositionList;
  TFhirComposition = FHIR.R2.Resources.Clinical.TFhirComposition;
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  TFhirConceptMapContact = FHIR.R2.Resources.Canonical.TFhirConceptMapContact;
  TFhirConceptMapContactList = FHIR.R2.Resources.Canonical.TFhirConceptMapContactList;
  TFhirConceptMapElement = FHIR.R2.Resources.Canonical.TFhirConceptMapElement;
  TFhirConceptMapElementList = FHIR.R2.Resources.Canonical.TFhirConceptMapElementList;
  TFhirConceptMapElementTarget = FHIR.R2.Resources.Canonical.TFhirConceptMapElementTarget;
  TFhirConceptMapElementTargetList = FHIR.R2.Resources.Canonical.TFhirConceptMapElementTargetList;
  TFhirConceptMapElementTargetDependsOn = FHIR.R2.Resources.Canonical.TFhirConceptMapElementTargetDependsOn;
  TFhirConceptMapElementTargetDependsOnList = FHIR.R2.Resources.Canonical.TFhirConceptMapElementTargetDependsOnList;
  TFhirConceptMapList = FHIR.R2.Resources.Canonical.TFhirConceptMapList;
  TFhirConceptMap = FHIR.R2.Resources.Canonical.TFhirConceptMap;
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_CONDITION}
  TFhirConditionStage = FHIR.R2.Resources.Clinical.TFhirConditionStage;
  TFhirConditionStageList = FHIR.R2.Resources.Clinical.TFhirConditionStageList;
  TFhirConditionEvidence = FHIR.R2.Resources.Clinical.TFhirConditionEvidence;
  TFhirConditionEvidenceList = FHIR.R2.Resources.Clinical.TFhirConditionEvidenceList;
  TFhirConditionList = FHIR.R2.Resources.Clinical.TFhirConditionList;
  TFhirCondition = FHIR.R2.Resources.Clinical.TFhirCondition;
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_CONFORMANCE}
  TFhirConformanceContact = FHIR.R2.Resources.Canonical.TFhirConformanceContact;
  TFhirConformanceContactList = FHIR.R2.Resources.Canonical.TFhirConformanceContactList;
  TFhirConformanceSoftware = FHIR.R2.Resources.Canonical.TFhirConformanceSoftware;
  TFhirConformanceSoftwareList = FHIR.R2.Resources.Canonical.TFhirConformanceSoftwareList;
  TFhirConformanceImplementation = FHIR.R2.Resources.Canonical.TFhirConformanceImplementation;
  TFhirConformanceImplementationList = FHIR.R2.Resources.Canonical.TFhirConformanceImplementationList;
  TFhirConformanceRest = FHIR.R2.Resources.Canonical.TFhirConformanceRest;
  TFhirConformanceRestList = FHIR.R2.Resources.Canonical.TFhirConformanceRestList;
  TFhirConformanceRestSecurity = FHIR.R2.Resources.Canonical.TFhirConformanceRestSecurity;
  TFhirConformanceRestSecurityList = FHIR.R2.Resources.Canonical.TFhirConformanceRestSecurityList;
  TFhirConformanceRestSecurityCertificate = FHIR.R2.Resources.Canonical.TFhirConformanceRestSecurityCertificate;
  TFhirConformanceRestSecurityCertificateList = FHIR.R2.Resources.Canonical.TFhirConformanceRestSecurityCertificateList;
  TFhirConformanceRestResource = FHIR.R2.Resources.Canonical.TFhirConformanceRestResource;
  TFhirConformanceRestResourceList = FHIR.R2.Resources.Canonical.TFhirConformanceRestResourceList;
  TFhirConformanceRestResourceInteraction = FHIR.R2.Resources.Canonical.TFhirConformanceRestResourceInteraction;
  TFhirConformanceRestResourceInteractionList = FHIR.R2.Resources.Canonical.TFhirConformanceRestResourceInteractionList;
  TFhirConformanceRestResourceSearchParam = FHIR.R2.Resources.Canonical.TFhirConformanceRestResourceSearchParam;
  TFhirConformanceRestResourceSearchParamList = FHIR.R2.Resources.Canonical.TFhirConformanceRestResourceSearchParamList;
  TFhirConformanceRestInteraction = FHIR.R2.Resources.Canonical.TFhirConformanceRestInteraction;
  TFhirConformanceRestInteractionList = FHIR.R2.Resources.Canonical.TFhirConformanceRestInteractionList;
  TFhirConformanceRestOperation = FHIR.R2.Resources.Canonical.TFhirConformanceRestOperation;
  TFhirConformanceRestOperationList = FHIR.R2.Resources.Canonical.TFhirConformanceRestOperationList;
  TFhirConformanceMessaging = FHIR.R2.Resources.Canonical.TFhirConformanceMessaging;
  TFhirConformanceMessagingList = FHIR.R2.Resources.Canonical.TFhirConformanceMessagingList;
  TFhirConformanceMessagingEndpoint = FHIR.R2.Resources.Canonical.TFhirConformanceMessagingEndpoint;
  TFhirConformanceMessagingEndpointList = FHIR.R2.Resources.Canonical.TFhirConformanceMessagingEndpointList;
  TFhirConformanceMessagingEvent = FHIR.R2.Resources.Canonical.TFhirConformanceMessagingEvent;
  TFhirConformanceMessagingEventList = FHIR.R2.Resources.Canonical.TFhirConformanceMessagingEventList;
  TFhirConformanceDocument = FHIR.R2.Resources.Canonical.TFhirConformanceDocument;
  TFhirConformanceDocumentList = FHIR.R2.Resources.Canonical.TFhirConformanceDocumentList;
  TFhirConformanceList = FHIR.R2.Resources.Canonical.TFhirConformanceList;
  TFhirConformance = FHIR.R2.Resources.Canonical.TFhirConformance;
{$ENDIF FHIR_CONFORMANCE}
{$IFDEF FHIR_CONTRACT}
  TFhirContractActor = FHIR.R2.Resources.Other.TFhirContractActor;
  TFhirContractActorList = FHIR.R2.Resources.Other.TFhirContractActorList;
  TFhirContractValuedItem = FHIR.R2.Resources.Other.TFhirContractValuedItem;
  TFhirContractValuedItemList = FHIR.R2.Resources.Other.TFhirContractValuedItemList;
  TFhirContractSigner = FHIR.R2.Resources.Other.TFhirContractSigner;
  TFhirContractSignerList = FHIR.R2.Resources.Other.TFhirContractSignerList;
  TFhirContractTerm = FHIR.R2.Resources.Other.TFhirContractTerm;
  TFhirContractTermList = FHIR.R2.Resources.Other.TFhirContractTermList;
  TFhirContractTermActor = FHIR.R2.Resources.Other.TFhirContractTermActor;
  TFhirContractTermActorList = FHIR.R2.Resources.Other.TFhirContractTermActorList;
  TFhirContractTermValuedItem = FHIR.R2.Resources.Other.TFhirContractTermValuedItem;
  TFhirContractTermValuedItemList = FHIR.R2.Resources.Other.TFhirContractTermValuedItemList;
  TFhirContractFriendly = FHIR.R2.Resources.Other.TFhirContractFriendly;
  TFhirContractFriendlyList = FHIR.R2.Resources.Other.TFhirContractFriendlyList;
  TFhirContractLegal = FHIR.R2.Resources.Other.TFhirContractLegal;
  TFhirContractLegalList = FHIR.R2.Resources.Other.TFhirContractLegalList;
  TFhirContractRule = FHIR.R2.Resources.Other.TFhirContractRule;
  TFhirContractRuleList = FHIR.R2.Resources.Other.TFhirContractRuleList;
  TFhirContractList = FHIR.R2.Resources.Other.TFhirContractList;
  TFhirContract = FHIR.R2.Resources.Other.TFhirContract;
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_COVERAGE}
  TFhirCoverageList = FHIR.R2.Resources.Clinical.TFhirCoverageList;
  TFhirCoverage = FHIR.R2.Resources.Clinical.TFhirCoverage;
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_DATAELEMENT}
  TFhirDataElementContact = FHIR.R2.Resources.Canonical.TFhirDataElementContact;
  TFhirDataElementContactList = FHIR.R2.Resources.Canonical.TFhirDataElementContactList;
  TFhirDataElementMapping = FHIR.R2.Resources.Canonical.TFhirDataElementMapping;
  TFhirDataElementMappingList = FHIR.R2.Resources.Canonical.TFhirDataElementMappingList;
  TFhirDataElementList = FHIR.R2.Resources.Canonical.TFhirDataElementList;
  TFhirDataElement = FHIR.R2.Resources.Canonical.TFhirDataElement;
{$ENDIF FHIR_DATAELEMENT}
{$IFDEF FHIR_DETECTEDISSUE}
  TFhirDetectedIssueMitigation = FHIR.R2.Resources.Clinical.TFhirDetectedIssueMitigation;
  TFhirDetectedIssueMitigationList = FHIR.R2.Resources.Clinical.TFhirDetectedIssueMitigationList;
  TFhirDetectedIssueList = FHIR.R2.Resources.Clinical.TFhirDetectedIssueList;
  TFhirDetectedIssue = FHIR.R2.Resources.Clinical.TFhirDetectedIssue;
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  TFhirDeviceList = FHIR.R2.Resources.Admin.TFhirDeviceList;
  TFhirDevice = FHIR.R2.Resources.Admin.TFhirDevice;
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICECOMPONENT}
  TFhirDeviceComponentProductionSpecification = FHIR.R2.Resources.Admin.TFhirDeviceComponentProductionSpecification;
  TFhirDeviceComponentProductionSpecificationList = FHIR.R2.Resources.Admin.TFhirDeviceComponentProductionSpecificationList;
  TFhirDeviceComponentList = FHIR.R2.Resources.Admin.TFhirDeviceComponentList;
  TFhirDeviceComponent = FHIR.R2.Resources.Admin.TFhirDeviceComponent;
{$ENDIF FHIR_DEVICECOMPONENT}
{$IFDEF FHIR_DEVICEMETRIC}
  TFhirDeviceMetricCalibration = FHIR.R2.Resources.Admin.TFhirDeviceMetricCalibration;
  TFhirDeviceMetricCalibrationList = FHIR.R2.Resources.Admin.TFhirDeviceMetricCalibrationList;
  TFhirDeviceMetricList = FHIR.R2.Resources.Admin.TFhirDeviceMetricList;
  TFhirDeviceMetric = FHIR.R2.Resources.Admin.TFhirDeviceMetric;
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEUSEREQUEST}
  TFhirDeviceUseRequestList = FHIR.R2.Resources.Clinical.TFhirDeviceUseRequestList;
  TFhirDeviceUseRequest = FHIR.R2.Resources.Clinical.TFhirDeviceUseRequest;
{$ENDIF FHIR_DEVICEUSEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  TFhirDeviceUseStatementList = FHIR.R2.Resources.Clinical.TFhirDeviceUseStatementList;
  TFhirDeviceUseStatement = FHIR.R2.Resources.Clinical.TFhirDeviceUseStatement;
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICORDER}
  TFhirDiagnosticOrderEvent = FHIR.R2.Resources.Clinical.TFhirDiagnosticOrderEvent;
  TFhirDiagnosticOrderEventList = FHIR.R2.Resources.Clinical.TFhirDiagnosticOrderEventList;
  TFhirDiagnosticOrderItem = FHIR.R2.Resources.Clinical.TFhirDiagnosticOrderItem;
  TFhirDiagnosticOrderItemList = FHIR.R2.Resources.Clinical.TFhirDiagnosticOrderItemList;
  TFhirDiagnosticOrderList = FHIR.R2.Resources.Clinical.TFhirDiagnosticOrderList;
  TFhirDiagnosticOrder = FHIR.R2.Resources.Clinical.TFhirDiagnosticOrder;
{$ENDIF FHIR_DIAGNOSTICORDER}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  TFhirDiagnosticReportImage = FHIR.R2.Resources.Clinical.TFhirDiagnosticReportImage;
  TFhirDiagnosticReportImageList = FHIR.R2.Resources.Clinical.TFhirDiagnosticReportImageList;
  TFhirDiagnosticReportList = FHIR.R2.Resources.Clinical.TFhirDiagnosticReportList;
  TFhirDiagnosticReport = FHIR.R2.Resources.Clinical.TFhirDiagnosticReport;
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  TFhirDocumentManifestContent = FHIR.R2.Resources.Clinical.TFhirDocumentManifestContent;
  TFhirDocumentManifestContentList = FHIR.R2.Resources.Clinical.TFhirDocumentManifestContentList;
  TFhirDocumentManifestRelated = FHIR.R2.Resources.Clinical.TFhirDocumentManifestRelated;
  TFhirDocumentManifestRelatedList = FHIR.R2.Resources.Clinical.TFhirDocumentManifestRelatedList;
  TFhirDocumentManifestList = FHIR.R2.Resources.Clinical.TFhirDocumentManifestList;
  TFhirDocumentManifest = FHIR.R2.Resources.Clinical.TFhirDocumentManifest;
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  TFhirDocumentReferenceRelatesTo = FHIR.R2.Resources.Clinical.TFhirDocumentReferenceRelatesTo;
  TFhirDocumentReferenceRelatesToList = FHIR.R2.Resources.Clinical.TFhirDocumentReferenceRelatesToList;
  TFhirDocumentReferenceContent = FHIR.R2.Resources.Clinical.TFhirDocumentReferenceContent;
  TFhirDocumentReferenceContentList = FHIR.R2.Resources.Clinical.TFhirDocumentReferenceContentList;
  TFhirDocumentReferenceContext = FHIR.R2.Resources.Clinical.TFhirDocumentReferenceContext;
  TFhirDocumentReferenceContextList = FHIR.R2.Resources.Clinical.TFhirDocumentReferenceContextList;
  TFhirDocumentReferenceContextRelated = FHIR.R2.Resources.Clinical.TFhirDocumentReferenceContextRelated;
  TFhirDocumentReferenceContextRelatedList = FHIR.R2.Resources.Clinical.TFhirDocumentReferenceContextRelatedList;
  TFhirDocumentReferenceList = FHIR.R2.Resources.Clinical.TFhirDocumentReferenceList;
  TFhirDocumentReference = FHIR.R2.Resources.Clinical.TFhirDocumentReference;
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_ELIGIBILITYREQUEST}
  TFhirEligibilityRequestList = FHIR.R2.Resources.Other.TFhirEligibilityRequestList;
  TFhirEligibilityRequest = FHIR.R2.Resources.Other.TFhirEligibilityRequest;
{$ENDIF FHIR_ELIGIBILITYREQUEST}
{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  TFhirEligibilityResponseList = FHIR.R2.Resources.Other.TFhirEligibilityResponseList;
  TFhirEligibilityResponse = FHIR.R2.Resources.Other.TFhirEligibilityResponse;
{$ENDIF FHIR_ELIGIBILITYRESPONSE}
{$IFDEF FHIR_ENCOUNTER}
  TFhirEncounterStatusHistory = FHIR.R2.Resources.Admin.TFhirEncounterStatusHistory;
  TFhirEncounterStatusHistoryList = FHIR.R2.Resources.Admin.TFhirEncounterStatusHistoryList;
  TFhirEncounterParticipant = FHIR.R2.Resources.Admin.TFhirEncounterParticipant;
  TFhirEncounterParticipantList = FHIR.R2.Resources.Admin.TFhirEncounterParticipantList;
  TFhirEncounterHospitalization = FHIR.R2.Resources.Admin.TFhirEncounterHospitalization;
  TFhirEncounterHospitalizationList = FHIR.R2.Resources.Admin.TFhirEncounterHospitalizationList;
  TFhirEncounterLocation = FHIR.R2.Resources.Admin.TFhirEncounterLocation;
  TFhirEncounterLocationList = FHIR.R2.Resources.Admin.TFhirEncounterLocationList;
  TFhirEncounterList = FHIR.R2.Resources.Admin.TFhirEncounterList;
  TFhirEncounter = FHIR.R2.Resources.Admin.TFhirEncounter;
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  TFhirEnrollmentRequestList = FHIR.R2.Resources.Other.TFhirEnrollmentRequestList;
  TFhirEnrollmentRequest = FHIR.R2.Resources.Other.TFhirEnrollmentRequest;
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  TFhirEnrollmentResponseList = FHIR.R2.Resources.Other.TFhirEnrollmentResponseList;
  TFhirEnrollmentResponse = FHIR.R2.Resources.Other.TFhirEnrollmentResponse;
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EPISODEOFCARE}
  TFhirEpisodeOfCareStatusHistory = FHIR.R2.Resources.Admin.TFhirEpisodeOfCareStatusHistory;
  TFhirEpisodeOfCareStatusHistoryList = FHIR.R2.Resources.Admin.TFhirEpisodeOfCareStatusHistoryList;
  TFhirEpisodeOfCareCareTeam = FHIR.R2.Resources.Admin.TFhirEpisodeOfCareCareTeam;
  TFhirEpisodeOfCareCareTeamList = FHIR.R2.Resources.Admin.TFhirEpisodeOfCareCareTeamList;
  TFhirEpisodeOfCareList = FHIR.R2.Resources.Admin.TFhirEpisodeOfCareList;
  TFhirEpisodeOfCare = FHIR.R2.Resources.Admin.TFhirEpisodeOfCare;
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  TFhirExplanationOfBenefitList = FHIR.R2.Resources.Other.TFhirExplanationOfBenefitList;
  TFhirExplanationOfBenefit = FHIR.R2.Resources.Other.TFhirExplanationOfBenefit;
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  TFhirFamilyMemberHistoryCondition = FHIR.R2.Resources.Clinical.TFhirFamilyMemberHistoryCondition;
  TFhirFamilyMemberHistoryConditionList = FHIR.R2.Resources.Clinical.TFhirFamilyMemberHistoryConditionList;
  TFhirFamilyMemberHistoryList = FHIR.R2.Resources.Clinical.TFhirFamilyMemberHistoryList;
  TFhirFamilyMemberHistory = FHIR.R2.Resources.Clinical.TFhirFamilyMemberHistory;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  TFhirFlagList = FHIR.R2.Resources.Clinical.TFhirFlagList;
  TFhirFlag = FHIR.R2.Resources.Clinical.TFhirFlag;
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  TFhirGoalOutcome = FHIR.R2.Resources.Clinical.TFhirGoalOutcome;
  TFhirGoalOutcomeList = FHIR.R2.Resources.Clinical.TFhirGoalOutcomeList;
  TFhirGoalList = FHIR.R2.Resources.Clinical.TFhirGoalList;
  TFhirGoal = FHIR.R2.Resources.Clinical.TFhirGoal;
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GROUP}
  TFhirGroupCharacteristic = FHIR.R2.Resources.Admin.TFhirGroupCharacteristic;
  TFhirGroupCharacteristicList = FHIR.R2.Resources.Admin.TFhirGroupCharacteristicList;
  TFhirGroupMember = FHIR.R2.Resources.Admin.TFhirGroupMember;
  TFhirGroupMemberList = FHIR.R2.Resources.Admin.TFhirGroupMemberList;
  TFhirGroupList = FHIR.R2.Resources.Admin.TFhirGroupList;
  TFhirGroup = FHIR.R2.Resources.Admin.TFhirGroup;
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_HEALTHCARESERVICE}
  TFhirHealthcareServiceServiceType = FHIR.R2.Resources.Admin.TFhirHealthcareServiceServiceType;
  TFhirHealthcareServiceServiceTypeList = FHIR.R2.Resources.Admin.TFhirHealthcareServiceServiceTypeList;
  TFhirHealthcareServiceAvailableTime = FHIR.R2.Resources.Admin.TFhirHealthcareServiceAvailableTime;
  TFhirHealthcareServiceAvailableTimeList = FHIR.R2.Resources.Admin.TFhirHealthcareServiceAvailableTimeList;
  TFhirHealthcareServiceNotAvailable = FHIR.R2.Resources.Admin.TFhirHealthcareServiceNotAvailable;
  TFhirHealthcareServiceNotAvailableList = FHIR.R2.Resources.Admin.TFhirHealthcareServiceNotAvailableList;
  TFhirHealthcareServiceList = FHIR.R2.Resources.Admin.TFhirHealthcareServiceList;
  TFhirHealthcareService = FHIR.R2.Resources.Admin.TFhirHealthcareService;
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGOBJECTSELECTION}
  TFhirImagingObjectSelectionStudy = FHIR.R2.Resources.Clinical.TFhirImagingObjectSelectionStudy;
  TFhirImagingObjectSelectionStudyList = FHIR.R2.Resources.Clinical.TFhirImagingObjectSelectionStudyList;
  TFhirImagingObjectSelectionStudySeries = FHIR.R2.Resources.Clinical.TFhirImagingObjectSelectionStudySeries;
  TFhirImagingObjectSelectionStudySeriesList = FHIR.R2.Resources.Clinical.TFhirImagingObjectSelectionStudySeriesList;
  TFhirImagingObjectSelectionStudySeriesInstance = FHIR.R2.Resources.Clinical.TFhirImagingObjectSelectionStudySeriesInstance;
  TFhirImagingObjectSelectionStudySeriesInstanceList = FHIR.R2.Resources.Clinical.TFhirImagingObjectSelectionStudySeriesInstanceList;
  TFhirImagingObjectSelectionStudySeriesInstanceFrames = FHIR.R2.Resources.Clinical.TFhirImagingObjectSelectionStudySeriesInstanceFrames;
  TFhirImagingObjectSelectionStudySeriesInstanceFramesList = FHIR.R2.Resources.Clinical.TFhirImagingObjectSelectionStudySeriesInstanceFramesList;
  TFhirImagingObjectSelectionList = FHIR.R2.Resources.Clinical.TFhirImagingObjectSelectionList;
  TFhirImagingObjectSelection = FHIR.R2.Resources.Clinical.TFhirImagingObjectSelection;
{$ENDIF FHIR_IMAGINGOBJECTSELECTION}
{$IFDEF FHIR_IMAGINGSTUDY}
  TFhirImagingStudySeries = FHIR.R2.Resources.Clinical.TFhirImagingStudySeries;
  TFhirImagingStudySeriesList = FHIR.R2.Resources.Clinical.TFhirImagingStudySeriesList;
  TFhirImagingStudySeriesInstance = FHIR.R2.Resources.Clinical.TFhirImagingStudySeriesInstance;
  TFhirImagingStudySeriesInstanceList = FHIR.R2.Resources.Clinical.TFhirImagingStudySeriesInstanceList;
  TFhirImagingStudyList = FHIR.R2.Resources.Clinical.TFhirImagingStudyList;
  TFhirImagingStudy = FHIR.R2.Resources.Clinical.TFhirImagingStudy;
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  TFhirImmunizationExplanation = FHIR.R2.Resources.Clinical.TFhirImmunizationExplanation;
  TFhirImmunizationExplanationList = FHIR.R2.Resources.Clinical.TFhirImmunizationExplanationList;
  TFhirImmunizationReaction = FHIR.R2.Resources.Clinical.TFhirImmunizationReaction;
  TFhirImmunizationReactionList = FHIR.R2.Resources.Clinical.TFhirImmunizationReactionList;
  TFhirImmunizationVaccinationProtocol = FHIR.R2.Resources.Clinical.TFhirImmunizationVaccinationProtocol;
  TFhirImmunizationVaccinationProtocolList = FHIR.R2.Resources.Clinical.TFhirImmunizationVaccinationProtocolList;
  TFhirImmunizationList = FHIR.R2.Resources.Clinical.TFhirImmunizationList;
  TFhirImmunization = FHIR.R2.Resources.Clinical.TFhirImmunization;
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  TFhirImmunizationRecommendationRecommendation = FHIR.R2.Resources.Clinical.TFhirImmunizationRecommendationRecommendation;
  TFhirImmunizationRecommendationRecommendationList = FHIR.R2.Resources.Clinical.TFhirImmunizationRecommendationRecommendationList;
  TFhirImmunizationRecommendationRecommendationDateCriterion = FHIR.R2.Resources.Clinical.TFhirImmunizationRecommendationRecommendationDateCriterion;
  TFhirImmunizationRecommendationRecommendationDateCriterionList = FHIR.R2.Resources.Clinical.TFhirImmunizationRecommendationRecommendationDateCriterionList;
  TFhirImmunizationRecommendationRecommendationProtocol = FHIR.R2.Resources.Clinical.TFhirImmunizationRecommendationRecommendationProtocol;
  TFhirImmunizationRecommendationRecommendationProtocolList = FHIR.R2.Resources.Clinical.TFhirImmunizationRecommendationRecommendationProtocolList;
  TFhirImmunizationRecommendationList = FHIR.R2.Resources.Clinical.TFhirImmunizationRecommendationList;
  TFhirImmunizationRecommendation = FHIR.R2.Resources.Clinical.TFhirImmunizationRecommendation;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  TFhirImplementationGuideContact = FHIR.R2.Resources.Canonical.TFhirImplementationGuideContact;
  TFhirImplementationGuideContactList = FHIR.R2.Resources.Canonical.TFhirImplementationGuideContactList;
  TFhirImplementationGuideDependency = FHIR.R2.Resources.Canonical.TFhirImplementationGuideDependency;
  TFhirImplementationGuideDependencyList = FHIR.R2.Resources.Canonical.TFhirImplementationGuideDependencyList;
  TFhirImplementationGuidePackage = FHIR.R2.Resources.Canonical.TFhirImplementationGuidePackage;
  TFhirImplementationGuidePackageList = FHIR.R2.Resources.Canonical.TFhirImplementationGuidePackageList;
  TFhirImplementationGuidePackageResource = FHIR.R2.Resources.Canonical.TFhirImplementationGuidePackageResource;
  TFhirImplementationGuidePackageResourceList = FHIR.R2.Resources.Canonical.TFhirImplementationGuidePackageResourceList;
  TFhirImplementationGuideGlobal = FHIR.R2.Resources.Canonical.TFhirImplementationGuideGlobal;
  TFhirImplementationGuideGlobalList = FHIR.R2.Resources.Canonical.TFhirImplementationGuideGlobalList;
  TFhirImplementationGuidePage = FHIR.R2.Resources.Canonical.TFhirImplementationGuidePage;
  TFhirImplementationGuidePageList = FHIR.R2.Resources.Canonical.TFhirImplementationGuidePageList;
  TFhirImplementationGuideList = FHIR.R2.Resources.Canonical.TFhirImplementationGuideList;
  TFhirImplementationGuide = FHIR.R2.Resources.Canonical.TFhirImplementationGuide;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_LIST}
  TFhirListEntry = FHIR.R2.Resources.Other.TFhirListEntry;
  TFhirListEntryList = FHIR.R2.Resources.Other.TFhirListEntryList;
  TFhirListList = FHIR.R2.Resources.Other.TFhirListList;
  TFhirList = FHIR.R2.Resources.Other.TFhirList;
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_LOCATION}
  TFhirLocationPosition = FHIR.R2.Resources.Admin.TFhirLocationPosition;
  TFhirLocationPositionList = FHIR.R2.Resources.Admin.TFhirLocationPositionList;
  TFhirLocationList = FHIR.R2.Resources.Admin.TFhirLocationList;
  TFhirLocation = FHIR.R2.Resources.Admin.TFhirLocation;
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_MEDIA}
  TFhirMediaList = FHIR.R2.Resources.Clinical.TFhirMediaList;
  TFhirMedia = FHIR.R2.Resources.Clinical.TFhirMedia;
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATION}
  TFhirMedicationProduct = FHIR.R2.Resources.Other.TFhirMedicationProduct;
  TFhirMedicationProductList = FHIR.R2.Resources.Other.TFhirMedicationProductList;
  TFhirMedicationProductIngredient = FHIR.R2.Resources.Other.TFhirMedicationProductIngredient;
  TFhirMedicationProductIngredientList = FHIR.R2.Resources.Other.TFhirMedicationProductIngredientList;
  TFhirMedicationProductBatch = FHIR.R2.Resources.Other.TFhirMedicationProductBatch;
  TFhirMedicationProductBatchList = FHIR.R2.Resources.Other.TFhirMedicationProductBatchList;
  TFhirMedicationPackage = FHIR.R2.Resources.Other.TFhirMedicationPackage;
  TFhirMedicationPackageList = FHIR.R2.Resources.Other.TFhirMedicationPackageList;
  TFhirMedicationPackageContent = FHIR.R2.Resources.Other.TFhirMedicationPackageContent;
  TFhirMedicationPackageContentList = FHIR.R2.Resources.Other.TFhirMedicationPackageContentList;
  TFhirMedicationList = FHIR.R2.Resources.Other.TFhirMedicationList;
  TFhirMedication = FHIR.R2.Resources.Other.TFhirMedication;
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  TFhirMedicationAdministrationDosage = FHIR.R2.Resources.Clinical.TFhirMedicationAdministrationDosage;
  TFhirMedicationAdministrationDosageList = FHIR.R2.Resources.Clinical.TFhirMedicationAdministrationDosageList;
  TFhirMedicationAdministrationList = FHIR.R2.Resources.Clinical.TFhirMedicationAdministrationList;
  TFhirMedicationAdministration = FHIR.R2.Resources.Clinical.TFhirMedicationAdministration;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  TFhirMedicationDispenseDosageInstruction = FHIR.R2.Resources.Clinical.TFhirMedicationDispenseDosageInstruction;
  TFhirMedicationDispenseDosageInstructionList = FHIR.R2.Resources.Clinical.TFhirMedicationDispenseDosageInstructionList;
  TFhirMedicationDispenseSubstitution = FHIR.R2.Resources.Clinical.TFhirMedicationDispenseSubstitution;
  TFhirMedicationDispenseSubstitutionList = FHIR.R2.Resources.Clinical.TFhirMedicationDispenseSubstitutionList;
  TFhirMedicationDispenseList = FHIR.R2.Resources.Clinical.TFhirMedicationDispenseList;
  TFhirMedicationDispense = FHIR.R2.Resources.Clinical.TFhirMedicationDispense;
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONORDER}
  TFhirMedicationOrderDosageInstruction = FHIR.R2.Resources.Clinical.TFhirMedicationOrderDosageInstruction;
  TFhirMedicationOrderDosageInstructionList = FHIR.R2.Resources.Clinical.TFhirMedicationOrderDosageInstructionList;
  TFhirMedicationOrderDispenseRequest = FHIR.R2.Resources.Clinical.TFhirMedicationOrderDispenseRequest;
  TFhirMedicationOrderDispenseRequestList = FHIR.R2.Resources.Clinical.TFhirMedicationOrderDispenseRequestList;
  TFhirMedicationOrderSubstitution = FHIR.R2.Resources.Clinical.TFhirMedicationOrderSubstitution;
  TFhirMedicationOrderSubstitutionList = FHIR.R2.Resources.Clinical.TFhirMedicationOrderSubstitutionList;
  TFhirMedicationOrderList = FHIR.R2.Resources.Clinical.TFhirMedicationOrderList;
  TFhirMedicationOrder = FHIR.R2.Resources.Clinical.TFhirMedicationOrder;
{$ENDIF FHIR_MEDICATIONORDER}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  TFhirMedicationStatementDosage = FHIR.R2.Resources.Clinical.TFhirMedicationStatementDosage;
  TFhirMedicationStatementDosageList = FHIR.R2.Resources.Clinical.TFhirMedicationStatementDosageList;
  TFhirMedicationStatementList = FHIR.R2.Resources.Clinical.TFhirMedicationStatementList;
  TFhirMedicationStatement = FHIR.R2.Resources.Clinical.TFhirMedicationStatement;
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_MESSAGEHEADER}
  TFhirMessageHeaderResponse = FHIR.R2.Resources.Other.TFhirMessageHeaderResponse;
  TFhirMessageHeaderResponseList = FHIR.R2.Resources.Other.TFhirMessageHeaderResponseList;
  TFhirMessageHeaderSource = FHIR.R2.Resources.Other.TFhirMessageHeaderSource;
  TFhirMessageHeaderSourceList = FHIR.R2.Resources.Other.TFhirMessageHeaderSourceList;
  TFhirMessageHeaderDestination = FHIR.R2.Resources.Other.TFhirMessageHeaderDestination;
  TFhirMessageHeaderDestinationList = FHIR.R2.Resources.Other.TFhirMessageHeaderDestinationList;
  TFhirMessageHeaderList = FHIR.R2.Resources.Other.TFhirMessageHeaderList;
  TFhirMessageHeader = FHIR.R2.Resources.Other.TFhirMessageHeader;
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_NAMINGSYSTEM}
  TFhirNamingSystemContact = FHIR.R2.Resources.Canonical.TFhirNamingSystemContact;
  TFhirNamingSystemContactList = FHIR.R2.Resources.Canonical.TFhirNamingSystemContactList;
  TFhirNamingSystemUniqueId = FHIR.R2.Resources.Canonical.TFhirNamingSystemUniqueId;
  TFhirNamingSystemUniqueIdList = FHIR.R2.Resources.Canonical.TFhirNamingSystemUniqueIdList;
  TFhirNamingSystemList = FHIR.R2.Resources.Canonical.TFhirNamingSystemList;
  TFhirNamingSystem = FHIR.R2.Resources.Canonical.TFhirNamingSystem;
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_NUTRITIONORDER}
  TFhirNutritionOrderOralDiet = FHIR.R2.Resources.Clinical.TFhirNutritionOrderOralDiet;
  TFhirNutritionOrderOralDietList = FHIR.R2.Resources.Clinical.TFhirNutritionOrderOralDietList;
  TFhirNutritionOrderOralDietNutrient = FHIR.R2.Resources.Clinical.TFhirNutritionOrderOralDietNutrient;
  TFhirNutritionOrderOralDietNutrientList = FHIR.R2.Resources.Clinical.TFhirNutritionOrderOralDietNutrientList;
  TFhirNutritionOrderOralDietTexture = FHIR.R2.Resources.Clinical.TFhirNutritionOrderOralDietTexture;
  TFhirNutritionOrderOralDietTextureList = FHIR.R2.Resources.Clinical.TFhirNutritionOrderOralDietTextureList;
  TFhirNutritionOrderSupplement = FHIR.R2.Resources.Clinical.TFhirNutritionOrderSupplement;
  TFhirNutritionOrderSupplementList = FHIR.R2.Resources.Clinical.TFhirNutritionOrderSupplementList;
  TFhirNutritionOrderEnteralFormula = FHIR.R2.Resources.Clinical.TFhirNutritionOrderEnteralFormula;
  TFhirNutritionOrderEnteralFormulaList = FHIR.R2.Resources.Clinical.TFhirNutritionOrderEnteralFormulaList;
  TFhirNutritionOrderEnteralFormulaAdministration = FHIR.R2.Resources.Clinical.TFhirNutritionOrderEnteralFormulaAdministration;
  TFhirNutritionOrderEnteralFormulaAdministrationList = FHIR.R2.Resources.Clinical.TFhirNutritionOrderEnteralFormulaAdministrationList;
  TFhirNutritionOrderList = FHIR.R2.Resources.Clinical.TFhirNutritionOrderList;
  TFhirNutritionOrder = FHIR.R2.Resources.Clinical.TFhirNutritionOrder;
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_OBSERVATION}
  TFhirObservationReferenceRange = FHIR.R2.Resources.Clinical.TFhirObservationReferenceRange;
  TFhirObservationReferenceRangeList = FHIR.R2.Resources.Clinical.TFhirObservationReferenceRangeList;
  TFhirObservationRelated = FHIR.R2.Resources.Clinical.TFhirObservationRelated;
  TFhirObservationRelatedList = FHIR.R2.Resources.Clinical.TFhirObservationRelatedList;
  TFhirObservationComponent = FHIR.R2.Resources.Clinical.TFhirObservationComponent;
  TFhirObservationComponentList = FHIR.R2.Resources.Clinical.TFhirObservationComponentList;
  TFhirObservationList = FHIR.R2.Resources.Clinical.TFhirObservationList;
  TFhirObservation = FHIR.R2.Resources.Clinical.TFhirObservation;
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  TFhirOperationDefinitionContact = FHIR.R2.Resources.Canonical.TFhirOperationDefinitionContact;
  TFhirOperationDefinitionContactList = FHIR.R2.Resources.Canonical.TFhirOperationDefinitionContactList;
  TFhirOperationDefinitionParameter = FHIR.R2.Resources.Canonical.TFhirOperationDefinitionParameter;
  TFhirOperationDefinitionParameterList = FHIR.R2.Resources.Canonical.TFhirOperationDefinitionParameterList;
  TFhirOperationDefinitionParameterBinding = FHIR.R2.Resources.Canonical.TFhirOperationDefinitionParameterBinding;
  TFhirOperationDefinitionParameterBindingList = FHIR.R2.Resources.Canonical.TFhirOperationDefinitionParameterBindingList;
  TFhirOperationDefinitionList = FHIR.R2.Resources.Canonical.TFhirOperationDefinitionList;
  TFhirOperationDefinition = FHIR.R2.Resources.Canonical.TFhirOperationDefinition;
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  TFhirOperationOutcomeIssue = FHIR.R2.Resources.Other.TFhirOperationOutcomeIssue;
  TFhirOperationOutcomeIssueList = FHIR.R2.Resources.Other.TFhirOperationOutcomeIssueList;
  TFhirOperationOutcomeList = FHIR.R2.Resources.Other.TFhirOperationOutcomeList;
  TFhirOperationOutcome = FHIR.R2.Resources.Other.TFhirOperationOutcome;
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_ORDER}
  TFhirOrderWhen = FHIR.R2.Resources.Other.TFhirOrderWhen;
  TFhirOrderWhenList = FHIR.R2.Resources.Other.TFhirOrderWhenList;
  TFhirOrderList = FHIR.R2.Resources.Other.TFhirOrderList;
  TFhirOrder = FHIR.R2.Resources.Other.TFhirOrder;
{$ENDIF FHIR_ORDER}
{$IFDEF FHIR_ORDERRESPONSE}
  TFhirOrderResponseList = FHIR.R2.Resources.Other.TFhirOrderResponseList;
  TFhirOrderResponse = FHIR.R2.Resources.Other.TFhirOrderResponse;
{$ENDIF FHIR_ORDERRESPONSE}
{$IFDEF FHIR_ORGANIZATION}
  TFhirOrganizationContact = FHIR.R2.Resources.Admin.TFhirOrganizationContact;
  TFhirOrganizationContactList = FHIR.R2.Resources.Admin.TFhirOrganizationContactList;
  TFhirOrganizationList = FHIR.R2.Resources.Admin.TFhirOrganizationList;
  TFhirOrganization = FHIR.R2.Resources.Admin.TFhirOrganization;
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_PATIENT}
  TFhirPatientContact = FHIR.R2.Resources.Admin.TFhirPatientContact;
  TFhirPatientContactList = FHIR.R2.Resources.Admin.TFhirPatientContactList;
  TFhirPatientAnimal = FHIR.R2.Resources.Admin.TFhirPatientAnimal;
  TFhirPatientAnimalList = FHIR.R2.Resources.Admin.TFhirPatientAnimalList;
  TFhirPatientCommunication = FHIR.R2.Resources.Admin.TFhirPatientCommunication;
  TFhirPatientCommunicationList = FHIR.R2.Resources.Admin.TFhirPatientCommunicationList;
  TFhirPatientLink = FHIR.R2.Resources.Admin.TFhirPatientLink;
  TFhirPatientLinkList = FHIR.R2.Resources.Admin.TFhirPatientLinkList;
  TFhirPatientList = FHIR.R2.Resources.Admin.TFhirPatientList;
  TFhirPatient = FHIR.R2.Resources.Admin.TFhirPatient;
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PAYMENTNOTICE}
  TFhirPaymentNoticeList = FHIR.R2.Resources.Other.TFhirPaymentNoticeList;
  TFhirPaymentNotice = FHIR.R2.Resources.Other.TFhirPaymentNotice;
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  TFhirPaymentReconciliationDetail = FHIR.R2.Resources.Other.TFhirPaymentReconciliationDetail;
  TFhirPaymentReconciliationDetailList = FHIR.R2.Resources.Other.TFhirPaymentReconciliationDetailList;
  TFhirPaymentReconciliationNote = FHIR.R2.Resources.Other.TFhirPaymentReconciliationNote;
  TFhirPaymentReconciliationNoteList = FHIR.R2.Resources.Other.TFhirPaymentReconciliationNoteList;
  TFhirPaymentReconciliationList = FHIR.R2.Resources.Other.TFhirPaymentReconciliationList;
  TFhirPaymentReconciliation = FHIR.R2.Resources.Other.TFhirPaymentReconciliation;
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERSON}
  TFhirPersonLink = FHIR.R2.Resources.Admin.TFhirPersonLink;
  TFhirPersonLinkList = FHIR.R2.Resources.Admin.TFhirPersonLinkList;
  TFhirPersonList = FHIR.R2.Resources.Admin.TFhirPersonList;
  TFhirPerson = FHIR.R2.Resources.Admin.TFhirPerson;
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PRACTITIONER}
  TFhirPractitionerPractitionerRole = FHIR.R2.Resources.Admin.TFhirPractitionerPractitionerRole;
  TFhirPractitionerPractitionerRoleList = FHIR.R2.Resources.Admin.TFhirPractitionerPractitionerRoleList;
  TFhirPractitionerQualification = FHIR.R2.Resources.Admin.TFhirPractitionerQualification;
  TFhirPractitionerQualificationList = FHIR.R2.Resources.Admin.TFhirPractitionerQualificationList;
  TFhirPractitionerList = FHIR.R2.Resources.Admin.TFhirPractitionerList;
  TFhirPractitioner = FHIR.R2.Resources.Admin.TFhirPractitioner;
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PROCEDURE}
  TFhirProcedurePerformer = FHIR.R2.Resources.Clinical.TFhirProcedurePerformer;
  TFhirProcedurePerformerList = FHIR.R2.Resources.Clinical.TFhirProcedurePerformerList;
  TFhirProcedureFocalDevice = FHIR.R2.Resources.Clinical.TFhirProcedureFocalDevice;
  TFhirProcedureFocalDeviceList = FHIR.R2.Resources.Clinical.TFhirProcedureFocalDeviceList;
  TFhirProcedureList = FHIR.R2.Resources.Clinical.TFhirProcedureList;
  TFhirProcedure = FHIR.R2.Resources.Clinical.TFhirProcedure;
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROCEDUREREQUEST}
  TFhirProcedureRequestList = FHIR.R2.Resources.Clinical.TFhirProcedureRequestList;
  TFhirProcedureRequest = FHIR.R2.Resources.Clinical.TFhirProcedureRequest;
{$ENDIF FHIR_PROCEDUREREQUEST}
{$IFDEF FHIR_PROCESSREQUEST}
  TFhirProcessRequestItem = FHIR.R2.Resources.Other.TFhirProcessRequestItem;
  TFhirProcessRequestItemList = FHIR.R2.Resources.Other.TFhirProcessRequestItemList;
  TFhirProcessRequestList = FHIR.R2.Resources.Other.TFhirProcessRequestList;
  TFhirProcessRequest = FHIR.R2.Resources.Other.TFhirProcessRequest;
{$ENDIF FHIR_PROCESSREQUEST}
{$IFDEF FHIR_PROCESSRESPONSE}
  TFhirProcessResponseNotes = FHIR.R2.Resources.Other.TFhirProcessResponseNotes;
  TFhirProcessResponseNotesList = FHIR.R2.Resources.Other.TFhirProcessResponseNotesList;
  TFhirProcessResponseList = FHIR.R2.Resources.Other.TFhirProcessResponseList;
  TFhirProcessResponse = FHIR.R2.Resources.Other.TFhirProcessResponse;
{$ENDIF FHIR_PROCESSRESPONSE}
{$IFDEF FHIR_PROVENANCE}
  TFhirProvenanceAgent = FHIR.R2.Resources.Other.TFhirProvenanceAgent;
  TFhirProvenanceAgentList = FHIR.R2.Resources.Other.TFhirProvenanceAgentList;
  TFhirProvenanceAgentRelatedAgent = FHIR.R2.Resources.Other.TFhirProvenanceAgentRelatedAgent;
  TFhirProvenanceAgentRelatedAgentList = FHIR.R2.Resources.Other.TFhirProvenanceAgentRelatedAgentList;
  TFhirProvenanceEntity = FHIR.R2.Resources.Other.TFhirProvenanceEntity;
  TFhirProvenanceEntityList = FHIR.R2.Resources.Other.TFhirProvenanceEntityList;
  TFhirProvenanceList = FHIR.R2.Resources.Other.TFhirProvenanceList;
  TFhirProvenance = FHIR.R2.Resources.Other.TFhirProvenance;
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  TFhirQuestionnaireGroup = FHIR.R2.Resources.Canonical.TFhirQuestionnaireGroup;
  TFhirQuestionnaireGroupList = FHIR.R2.Resources.Canonical.TFhirQuestionnaireGroupList;
  TFhirQuestionnaireGroupQuestion = FHIR.R2.Resources.Canonical.TFhirQuestionnaireGroupQuestion;
  TFhirQuestionnaireGroupQuestionList = FHIR.R2.Resources.Canonical.TFhirQuestionnaireGroupQuestionList;
  TFhirQuestionnaireList = FHIR.R2.Resources.Canonical.TFhirQuestionnaireList;
  TFhirQuestionnaire = FHIR.R2.Resources.Canonical.TFhirQuestionnaire;
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  TFhirQuestionnaireResponseGroup = FHIR.R2.Resources.Clinical.TFhirQuestionnaireResponseGroup;
  TFhirQuestionnaireResponseGroupList = FHIR.R2.Resources.Clinical.TFhirQuestionnaireResponseGroupList;
  TFhirQuestionnaireResponseGroupQuestion = FHIR.R2.Resources.Clinical.TFhirQuestionnaireResponseGroupQuestion;
  TFhirQuestionnaireResponseGroupQuestionList = FHIR.R2.Resources.Clinical.TFhirQuestionnaireResponseGroupQuestionList;
  TFhirQuestionnaireResponseGroupQuestionAnswer = FHIR.R2.Resources.Clinical.TFhirQuestionnaireResponseGroupQuestionAnswer;
  TFhirQuestionnaireResponseGroupQuestionAnswerList = FHIR.R2.Resources.Clinical.TFhirQuestionnaireResponseGroupQuestionAnswerList;
  TFhirQuestionnaireResponseList = FHIR.R2.Resources.Clinical.TFhirQuestionnaireResponseList;
  TFhirQuestionnaireResponse = FHIR.R2.Resources.Clinical.TFhirQuestionnaireResponse;
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REFERRALREQUEST}
  TFhirReferralRequestList = FHIR.R2.Resources.Clinical.TFhirReferralRequestList;
  TFhirReferralRequest = FHIR.R2.Resources.Clinical.TFhirReferralRequest;
{$ENDIF FHIR_REFERRALREQUEST}
{$IFDEF FHIR_RELATEDPERSON}
  TFhirRelatedPersonList = FHIR.R2.Resources.Admin.TFhirRelatedPersonList;
  TFhirRelatedPerson = FHIR.R2.Resources.Admin.TFhirRelatedPerson;
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_RISKASSESSMENT}
  TFhirRiskAssessmentPrediction = FHIR.R2.Resources.Clinical.TFhirRiskAssessmentPrediction;
  TFhirRiskAssessmentPredictionList = FHIR.R2.Resources.Clinical.TFhirRiskAssessmentPredictionList;
  TFhirRiskAssessmentList = FHIR.R2.Resources.Clinical.TFhirRiskAssessmentList;
  TFhirRiskAssessment = FHIR.R2.Resources.Clinical.TFhirRiskAssessment;
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_SCHEDULE}
  TFhirScheduleList = FHIR.R2.Resources.Admin.TFhirScheduleList;
  TFhirSchedule = FHIR.R2.Resources.Admin.TFhirSchedule;
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SEARCHPARAMETER}
  TFhirSearchParameterContact = FHIR.R2.Resources.Canonical.TFhirSearchParameterContact;
  TFhirSearchParameterContactList = FHIR.R2.Resources.Canonical.TFhirSearchParameterContactList;
  TFhirSearchParameterList = FHIR.R2.Resources.Canonical.TFhirSearchParameterList;
  TFhirSearchParameter = FHIR.R2.Resources.Canonical.TFhirSearchParameter;
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SLOT}
  TFhirSlotList = FHIR.R2.Resources.Admin.TFhirSlotList;
  TFhirSlot = FHIR.R2.Resources.Admin.TFhirSlot;
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  TFhirSpecimenCollection = FHIR.R2.Resources.Clinical.TFhirSpecimenCollection;
  TFhirSpecimenCollectionList = FHIR.R2.Resources.Clinical.TFhirSpecimenCollectionList;
  TFhirSpecimenTreatment = FHIR.R2.Resources.Clinical.TFhirSpecimenTreatment;
  TFhirSpecimenTreatmentList = FHIR.R2.Resources.Clinical.TFhirSpecimenTreatmentList;
  TFhirSpecimenContainer = FHIR.R2.Resources.Clinical.TFhirSpecimenContainer;
  TFhirSpecimenContainerList = FHIR.R2.Resources.Clinical.TFhirSpecimenContainerList;
  TFhirSpecimenList = FHIR.R2.Resources.Clinical.TFhirSpecimenList;
  TFhirSpecimen = FHIR.R2.Resources.Clinical.TFhirSpecimen;
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  TFhirStructureDefinitionContact = FHIR.R2.Resources.Canonical.TFhirStructureDefinitionContact;
  TFhirStructureDefinitionContactList = FHIR.R2.Resources.Canonical.TFhirStructureDefinitionContactList;
  TFhirStructureDefinitionMapping = FHIR.R2.Resources.Canonical.TFhirStructureDefinitionMapping;
  TFhirStructureDefinitionMappingList = FHIR.R2.Resources.Canonical.TFhirStructureDefinitionMappingList;
  TFhirStructureDefinitionSnapshot = FHIR.R2.Resources.Canonical.TFhirStructureDefinitionSnapshot;
  TFhirStructureDefinitionSnapshotList = FHIR.R2.Resources.Canonical.TFhirStructureDefinitionSnapshotList;
  TFhirStructureDefinitionDifferential = FHIR.R2.Resources.Canonical.TFhirStructureDefinitionDifferential;
  TFhirStructureDefinitionDifferentialList = FHIR.R2.Resources.Canonical.TFhirStructureDefinitionDifferentialList;
  TFhirStructureDefinitionList = FHIR.R2.Resources.Canonical.TFhirStructureDefinitionList;
  TFhirStructureDefinition = FHIR.R2.Resources.Canonical.TFhirStructureDefinition;
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_SUBSCRIPTION}
  TFhirSubscriptionChannel = FHIR.R2.Resources.Other.TFhirSubscriptionChannel;
  TFhirSubscriptionChannelList = FHIR.R2.Resources.Other.TFhirSubscriptionChannelList;
  TFhirSubscriptionList = FHIR.R2.Resources.Other.TFhirSubscriptionList;
  TFhirSubscription = FHIR.R2.Resources.Other.TFhirSubscription;
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSTANCE}
  TFhirSubstanceInstance = FHIR.R2.Resources.Admin.TFhirSubstanceInstance;
  TFhirSubstanceInstanceList = FHIR.R2.Resources.Admin.TFhirSubstanceInstanceList;
  TFhirSubstanceIngredient = FHIR.R2.Resources.Admin.TFhirSubstanceIngredient;
  TFhirSubstanceIngredientList = FHIR.R2.Resources.Admin.TFhirSubstanceIngredientList;
  TFhirSubstanceList = FHIR.R2.Resources.Admin.TFhirSubstanceList;
  TFhirSubstance = FHIR.R2.Resources.Admin.TFhirSubstance;
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUPPLYDELIVERY}
  TFhirSupplyDeliveryList = FHIR.R2.Resources.Clinical.TFhirSupplyDeliveryList;
  TFhirSupplyDelivery = FHIR.R2.Resources.Clinical.TFhirSupplyDelivery;
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  TFhirSupplyRequestWhen = FHIR.R2.Resources.Clinical.TFhirSupplyRequestWhen;
  TFhirSupplyRequestWhenList = FHIR.R2.Resources.Clinical.TFhirSupplyRequestWhenList;
  TFhirSupplyRequestList = FHIR.R2.Resources.Clinical.TFhirSupplyRequestList;
  TFhirSupplyRequest = FHIR.R2.Resources.Clinical.TFhirSupplyRequest;
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TESTSCRIPT}
  TFhirTestScriptContact = FHIR.R2.Resources.Canonical.TFhirTestScriptContact;
  TFhirTestScriptContactList = FHIR.R2.Resources.Canonical.TFhirTestScriptContactList;
  TFhirTestScriptMetadata = FHIR.R2.Resources.Canonical.TFhirTestScriptMetadata;
  TFhirTestScriptMetadataList = FHIR.R2.Resources.Canonical.TFhirTestScriptMetadataList;
  TFhirTestScriptMetadataLink = FHIR.R2.Resources.Canonical.TFhirTestScriptMetadataLink;
  TFhirTestScriptMetadataLinkList = FHIR.R2.Resources.Canonical.TFhirTestScriptMetadataLinkList;
  TFhirTestScriptMetadataCapability = FHIR.R2.Resources.Canonical.TFhirTestScriptMetadataCapability;
  TFhirTestScriptMetadataCapabilityList = FHIR.R2.Resources.Canonical.TFhirTestScriptMetadataCapabilityList;
  TFhirTestScriptFixture = FHIR.R2.Resources.Canonical.TFhirTestScriptFixture;
  TFhirTestScriptFixtureList = FHIR.R2.Resources.Canonical.TFhirTestScriptFixtureList;
  TFhirTestScriptVariable = FHIR.R2.Resources.Canonical.TFhirTestScriptVariable;
  TFhirTestScriptVariableList = FHIR.R2.Resources.Canonical.TFhirTestScriptVariableList;
  TFhirTestScriptSetup = FHIR.R2.Resources.Canonical.TFhirTestScriptSetup;
  TFhirTestScriptSetupList = FHIR.R2.Resources.Canonical.TFhirTestScriptSetupList;
  TFhirTestScriptSetupAction = FHIR.R2.Resources.Canonical.TFhirTestScriptSetupAction;
  TFhirTestScriptSetupActionList = FHIR.R2.Resources.Canonical.TFhirTestScriptSetupActionList;
  TFhirTestScriptSetupActionOperation = FHIR.R2.Resources.Canonical.TFhirTestScriptSetupActionOperation;
  TFhirTestScriptSetupActionOperationList = FHIR.R2.Resources.Canonical.TFhirTestScriptSetupActionOperationList;
  TFhirTestScriptSetupActionOperationRequestHeader = FHIR.R2.Resources.Canonical.TFhirTestScriptSetupActionOperationRequestHeader;
  TFhirTestScriptSetupActionOperationRequestHeaderList = FHIR.R2.Resources.Canonical.TFhirTestScriptSetupActionOperationRequestHeaderList;
  TFhirTestScriptSetupActionAssert = FHIR.R2.Resources.Canonical.TFhirTestScriptSetupActionAssert;
  TFhirTestScriptSetupActionAssertList = FHIR.R2.Resources.Canonical.TFhirTestScriptSetupActionAssertList;
  TFhirTestScriptTest = FHIR.R2.Resources.Canonical.TFhirTestScriptTest;
  TFhirTestScriptTestList = FHIR.R2.Resources.Canonical.TFhirTestScriptTestList;
  TFhirTestScriptTestAction = FHIR.R2.Resources.Canonical.TFhirTestScriptTestAction;
  TFhirTestScriptTestActionList = FHIR.R2.Resources.Canonical.TFhirTestScriptTestActionList;
  TFhirTestScriptTeardown = FHIR.R2.Resources.Canonical.TFhirTestScriptTeardown;
  TFhirTestScriptTeardownList = FHIR.R2.Resources.Canonical.TFhirTestScriptTeardownList;
  TFhirTestScriptTeardownAction = FHIR.R2.Resources.Canonical.TFhirTestScriptTeardownAction;
  TFhirTestScriptTeardownActionList = FHIR.R2.Resources.Canonical.TFhirTestScriptTeardownActionList;
  TFhirTestScriptList = FHIR.R2.Resources.Canonical.TFhirTestScriptList;
  TFhirTestScript = FHIR.R2.Resources.Canonical.TFhirTestScript;
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  TFhirValueSetContact = FHIR.R2.Resources.Canonical.TFhirValueSetContact;
  TFhirValueSetContactList = FHIR.R2.Resources.Canonical.TFhirValueSetContactList;
  TFhirValueSetCodeSystem = FHIR.R2.Resources.Canonical.TFhirValueSetCodeSystem;
  TFhirValueSetCodeSystemList = FHIR.R2.Resources.Canonical.TFhirValueSetCodeSystemList;
  TFhirValueSetCodeSystemConcept = FHIR.R2.Resources.Canonical.TFhirValueSetCodeSystemConcept;
  TFhirValueSetCodeSystemConceptList = FHIR.R2.Resources.Canonical.TFhirValueSetCodeSystemConceptList;
  TFhirValueSetCodeSystemConceptDesignation = FHIR.R2.Resources.Canonical.TFhirValueSetCodeSystemConceptDesignation;
  TFhirValueSetCodeSystemConceptDesignationList = FHIR.R2.Resources.Canonical.TFhirValueSetCodeSystemConceptDesignationList;
  TFhirValueSetCompose = FHIR.R2.Resources.Canonical.TFhirValueSetCompose;
  TFhirValueSetComposeList = FHIR.R2.Resources.Canonical.TFhirValueSetComposeList;
  TFhirValueSetComposeInclude = FHIR.R2.Resources.Canonical.TFhirValueSetComposeInclude;
  TFhirValueSetComposeIncludeList = FHIR.R2.Resources.Canonical.TFhirValueSetComposeIncludeList;
  TFhirValueSetComposeIncludeConcept = FHIR.R2.Resources.Canonical.TFhirValueSetComposeIncludeConcept;
  TFhirValueSetComposeIncludeConceptList = FHIR.R2.Resources.Canonical.TFhirValueSetComposeIncludeConceptList;
  TFhirValueSetComposeIncludeFilter = FHIR.R2.Resources.Canonical.TFhirValueSetComposeIncludeFilter;
  TFhirValueSetComposeIncludeFilterList = FHIR.R2.Resources.Canonical.TFhirValueSetComposeIncludeFilterList;
  TFhirValueSetExpansion = FHIR.R2.Resources.Canonical.TFhirValueSetExpansion;
  TFhirValueSetExpansionList = FHIR.R2.Resources.Canonical.TFhirValueSetExpansionList;
  TFhirValueSetExpansionParameter = FHIR.R2.Resources.Canonical.TFhirValueSetExpansionParameter;
  TFhirValueSetExpansionParameterList = FHIR.R2.Resources.Canonical.TFhirValueSetExpansionParameterList;
  TFhirValueSetExpansionContains = FHIR.R2.Resources.Canonical.TFhirValueSetExpansionContains;
  TFhirValueSetExpansionContainsList = FHIR.R2.Resources.Canonical.TFhirValueSetExpansionContainsList;
  TFhirValueSetList = FHIR.R2.Resources.Canonical.TFhirValueSetList;
  TFhirValueSet = FHIR.R2.Resources.Canonical.TFhirValueSet;
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  TFhirVisionPrescriptionDispense = FHIR.R2.Resources.Clinical.TFhirVisionPrescriptionDispense;
  TFhirVisionPrescriptionDispenseList = FHIR.R2.Resources.Clinical.TFhirVisionPrescriptionDispenseList;
  TFhirVisionPrescriptionList = FHIR.R2.Resources.Clinical.TFhirVisionPrescriptionList;
  TFhirVisionPrescription = FHIR.R2.Resources.Clinical.TFhirVisionPrescription;
{$ENDIF FHIR_VISIONPRESCRIPTION}

implementation

end.

