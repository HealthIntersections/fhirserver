unit fhir4_resources;

{$I fhir4.inc}

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

interface

// FHIR v4.0.0 generated 2019-01-21T22:41:56+11:00

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_stream,
  fhir_objects, fhir_utilities, 
  fhir4_base, fhir4_types, fhir4_resources_base,
  fhir4_resources_admin, fhir4_resources_canonical, fhir4_resources_clinical, fhir4_resources_financial, fhir4_resources_medications, fhir4_resources_other;

Type
  TFhirResourceType = fhir4_resources_base.TFhirResourceType;
  TFhirResourceTypeSet = fhir4_resources_base.TFhirResourceTypeSet;
  TFhirResource = fhir4_resources_base.TFhirResource;
  TFhirResourceList = fhir4_resources_base.TFhirResourceList;
  TFhirDomainResource = fhir4_resources_base.TFhirDomainResource;
  TFhirDomainResourceList = fhir4_resources_base.TFhirDomainResourceList;
  TFhirMetadataResource = fhir4_resources_canonical.TFhirMetadataResource;
  TFhirMetadataResourceList = fhir4_resources_canonical.TFhirMetadataResourceList;

{$IFDEF FHIR_PARAMETERS}
  TFhirParametersParameter = fhir4_resources_other.TFhirParametersParameter;
  TFhirParametersParameterList = fhir4_resources_other.TFhirParametersParameterList;
  TFhirParameters = fhir4_resources_other.TFhirParameters;
  TFhirParametersList = fhir4_resources_other.TFhirParametersList;
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_ACCOUNT}
  TFhirAccountCoverage = fhir4_resources_clinical.TFhirAccountCoverage;
  TFhirAccountCoverageList = fhir4_resources_clinical.TFhirAccountCoverageList;
  TFhirAccountGuarantor = fhir4_resources_clinical.TFhirAccountGuarantor;
  TFhirAccountGuarantorList = fhir4_resources_clinical.TFhirAccountGuarantorList;
  TFhirAccount = fhir4_resources_clinical.TFhirAccount;
  TFhirAccountList = fhir4_resources_clinical.TFhirAccountList;
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  TFhirActivityDefinitionParticipant = fhir4_resources_canonical.TFhirActivityDefinitionParticipant;
  TFhirActivityDefinitionParticipantList = fhir4_resources_canonical.TFhirActivityDefinitionParticipantList;
  TFhirActivityDefinitionDynamicValue = fhir4_resources_canonical.TFhirActivityDefinitionDynamicValue;
  TFhirActivityDefinitionDynamicValueList = fhir4_resources_canonical.TFhirActivityDefinitionDynamicValueList;
  TFhirActivityDefinition = fhir4_resources_canonical.TFhirActivityDefinition;
  TFhirActivityDefinitionList = fhir4_resources_canonical.TFhirActivityDefinitionList;
{$ENDIF FHIR_ACTIVITYDEFINITION}
{$IFDEF FHIR_ADVERSEEVENT}
  TFhirAdverseEventSuspectEntity = fhir4_resources_clinical.TFhirAdverseEventSuspectEntity;
  TFhirAdverseEventSuspectEntityList = fhir4_resources_clinical.TFhirAdverseEventSuspectEntityList;
  TFhirAdverseEventSuspectEntityCausality = fhir4_resources_clinical.TFhirAdverseEventSuspectEntityCausality;
  TFhirAdverseEventSuspectEntityCausalityList = fhir4_resources_clinical.TFhirAdverseEventSuspectEntityCausalityList;
  TFhirAdverseEvent = fhir4_resources_clinical.TFhirAdverseEvent;
  TFhirAdverseEventList = fhir4_resources_clinical.TFhirAdverseEventList;
{$ENDIF FHIR_ADVERSEEVENT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  TFhirAllergyIntoleranceReaction = fhir4_resources_clinical.TFhirAllergyIntoleranceReaction;
  TFhirAllergyIntoleranceReactionList = fhir4_resources_clinical.TFhirAllergyIntoleranceReactionList;
  TFhirAllergyIntolerance = fhir4_resources_clinical.TFhirAllergyIntolerance;
  TFhirAllergyIntoleranceList = fhir4_resources_clinical.TFhirAllergyIntoleranceList;
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  TFhirAppointmentParticipant = fhir4_resources_clinical.TFhirAppointmentParticipant;
  TFhirAppointmentParticipantList = fhir4_resources_clinical.TFhirAppointmentParticipantList;
  TFhirAppointment = fhir4_resources_clinical.TFhirAppointment;
  TFhirAppointmentList = fhir4_resources_clinical.TFhirAppointmentList;
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  TFhirAppointmentResponse = fhir4_resources_clinical.TFhirAppointmentResponse;
  TFhirAppointmentResponseList = fhir4_resources_clinical.TFhirAppointmentResponseList;
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_AUDITEVENT}
  TFhirAuditEventAgent = fhir4_resources_other.TFhirAuditEventAgent;
  TFhirAuditEventAgentList = fhir4_resources_other.TFhirAuditEventAgentList;
  TFhirAuditEventAgentNetwork = fhir4_resources_other.TFhirAuditEventAgentNetwork;
  TFhirAuditEventAgentNetworkList = fhir4_resources_other.TFhirAuditEventAgentNetworkList;
  TFhirAuditEventSource = fhir4_resources_other.TFhirAuditEventSource;
  TFhirAuditEventSourceList = fhir4_resources_other.TFhirAuditEventSourceList;
  TFhirAuditEventEntity = fhir4_resources_other.TFhirAuditEventEntity;
  TFhirAuditEventEntityList = fhir4_resources_other.TFhirAuditEventEntityList;
  TFhirAuditEventEntityDetail = fhir4_resources_other.TFhirAuditEventEntityDetail;
  TFhirAuditEventEntityDetailList = fhir4_resources_other.TFhirAuditEventEntityDetailList;
  TFhirAuditEvent = fhir4_resources_other.TFhirAuditEvent;
  TFhirAuditEventList = fhir4_resources_other.TFhirAuditEventList;
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BASIC}
  TFhirBasic = fhir4_resources_clinical.TFhirBasic;
  TFhirBasicList = fhir4_resources_clinical.TFhirBasicList;
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BINARY}
  TFhirBinary = fhir4_resources_other.TFhirBinary;
  TFhirBinaryList = fhir4_resources_other.TFhirBinaryList;
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  TFhirBiologicallyDerivedProductCollection = fhir4_resources_admin.TFhirBiologicallyDerivedProductCollection;
  TFhirBiologicallyDerivedProductCollectionList = fhir4_resources_admin.TFhirBiologicallyDerivedProductCollectionList;
  TFhirBiologicallyDerivedProductProcessing = fhir4_resources_admin.TFhirBiologicallyDerivedProductProcessing;
  TFhirBiologicallyDerivedProductProcessingList = fhir4_resources_admin.TFhirBiologicallyDerivedProductProcessingList;
  TFhirBiologicallyDerivedProductManipulation = fhir4_resources_admin.TFhirBiologicallyDerivedProductManipulation;
  TFhirBiologicallyDerivedProductManipulationList = fhir4_resources_admin.TFhirBiologicallyDerivedProductManipulationList;
  TFhirBiologicallyDerivedProductStorage = fhir4_resources_admin.TFhirBiologicallyDerivedProductStorage;
  TFhirBiologicallyDerivedProductStorageList = fhir4_resources_admin.TFhirBiologicallyDerivedProductStorageList;
  TFhirBiologicallyDerivedProduct = fhir4_resources_admin.TFhirBiologicallyDerivedProduct;
  TFhirBiologicallyDerivedProductList = fhir4_resources_admin.TFhirBiologicallyDerivedProductList;
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
{$IFDEF FHIR_BODYSTRUCTURE}
  TFhirBodyStructure = fhir4_resources_clinical.TFhirBodyStructure;
  TFhirBodyStructureList = fhir4_resources_clinical.TFhirBodyStructureList;
{$ENDIF FHIR_BODYSTRUCTURE}
{$IFDEF FHIR_BUNDLE}
  TFhirBundleLink = fhir4_resources_other.TFhirBundleLink;
  TFhirBundleLinkList = fhir4_resources_other.TFhirBundleLinkList;
  TFhirBundleEntry = fhir4_resources_other.TFhirBundleEntry;
  TFhirBundleEntryList = fhir4_resources_other.TFhirBundleEntryList;
  TFhirBundleEntrySearch = fhir4_resources_other.TFhirBundleEntrySearch;
  TFhirBundleEntrySearchList = fhir4_resources_other.TFhirBundleEntrySearchList;
  TFhirBundleEntryRequest = fhir4_resources_other.TFhirBundleEntryRequest;
  TFhirBundleEntryRequestList = fhir4_resources_other.TFhirBundleEntryRequestList;
  TFhirBundleEntryResponse = fhir4_resources_other.TFhirBundleEntryResponse;
  TFhirBundleEntryResponseList = fhir4_resources_other.TFhirBundleEntryResponseList;
  TFhirBundle = fhir4_resources_other.TFhirBundle;
  TFhirBundleList = fhir4_resources_other.TFhirBundleList;
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  TFhirCapabilityStatementSoftware = fhir4_resources_canonical.TFhirCapabilityStatementSoftware;
  TFhirCapabilityStatementSoftwareList = fhir4_resources_canonical.TFhirCapabilityStatementSoftwareList;
  TFhirCapabilityStatementImplementation = fhir4_resources_canonical.TFhirCapabilityStatementImplementation;
  TFhirCapabilityStatementImplementationList = fhir4_resources_canonical.TFhirCapabilityStatementImplementationList;
  TFhirCapabilityStatementRest = fhir4_resources_canonical.TFhirCapabilityStatementRest;
  TFhirCapabilityStatementRestList = fhir4_resources_canonical.TFhirCapabilityStatementRestList;
  TFhirCapabilityStatementRestSecurity = fhir4_resources_canonical.TFhirCapabilityStatementRestSecurity;
  TFhirCapabilityStatementRestSecurityList = fhir4_resources_canonical.TFhirCapabilityStatementRestSecurityList;
  TFhirCapabilityStatementRestResource = fhir4_resources_canonical.TFhirCapabilityStatementRestResource;
  TFhirCapabilityStatementRestResourceList = fhir4_resources_canonical.TFhirCapabilityStatementRestResourceList;
  TFhirCapabilityStatementRestResourceInteraction = fhir4_resources_canonical.TFhirCapabilityStatementRestResourceInteraction;
  TFhirCapabilityStatementRestResourceInteractionList = fhir4_resources_canonical.TFhirCapabilityStatementRestResourceInteractionList;
  TFhirCapabilityStatementRestResourceSearchParam = fhir4_resources_canonical.TFhirCapabilityStatementRestResourceSearchParam;
  TFhirCapabilityStatementRestResourceSearchParamList = fhir4_resources_canonical.TFhirCapabilityStatementRestResourceSearchParamList;
  TFhirCapabilityStatementRestResourceOperation = fhir4_resources_canonical.TFhirCapabilityStatementRestResourceOperation;
  TFhirCapabilityStatementRestResourceOperationList = fhir4_resources_canonical.TFhirCapabilityStatementRestResourceOperationList;
  TFhirCapabilityStatementRestInteraction = fhir4_resources_canonical.TFhirCapabilityStatementRestInteraction;
  TFhirCapabilityStatementRestInteractionList = fhir4_resources_canonical.TFhirCapabilityStatementRestInteractionList;
  TFhirCapabilityStatementMessaging = fhir4_resources_canonical.TFhirCapabilityStatementMessaging;
  TFhirCapabilityStatementMessagingList = fhir4_resources_canonical.TFhirCapabilityStatementMessagingList;
  TFhirCapabilityStatementMessagingEndpoint = fhir4_resources_canonical.TFhirCapabilityStatementMessagingEndpoint;
  TFhirCapabilityStatementMessagingEndpointList = fhir4_resources_canonical.TFhirCapabilityStatementMessagingEndpointList;
  TFhirCapabilityStatementMessagingSupportedMessage = fhir4_resources_canonical.TFhirCapabilityStatementMessagingSupportedMessage;
  TFhirCapabilityStatementMessagingSupportedMessageList = fhir4_resources_canonical.TFhirCapabilityStatementMessagingSupportedMessageList;
  TFhirCapabilityStatementDocument = fhir4_resources_canonical.TFhirCapabilityStatementDocument;
  TFhirCapabilityStatementDocumentList = fhir4_resources_canonical.TFhirCapabilityStatementDocumentList;
  TFhirCapabilityStatement = fhir4_resources_canonical.TFhirCapabilityStatement;
  TFhirCapabilityStatementList = fhir4_resources_canonical.TFhirCapabilityStatementList;
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CAREPLAN}
  TFhirCarePlanActivity = fhir4_resources_clinical.TFhirCarePlanActivity;
  TFhirCarePlanActivityList = fhir4_resources_clinical.TFhirCarePlanActivityList;
  TFhirCarePlanActivityDetail = fhir4_resources_clinical.TFhirCarePlanActivityDetail;
  TFhirCarePlanActivityDetailList = fhir4_resources_clinical.TFhirCarePlanActivityDetailList;
  TFhirCarePlan = fhir4_resources_clinical.TFhirCarePlan;
  TFhirCarePlanList = fhir4_resources_clinical.TFhirCarePlanList;
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CARETEAM}
  TFhirCareTeamParticipant = fhir4_resources_clinical.TFhirCareTeamParticipant;
  TFhirCareTeamParticipantList = fhir4_resources_clinical.TFhirCareTeamParticipantList;
  TFhirCareTeam = fhir4_resources_clinical.TFhirCareTeam;
  TFhirCareTeamList = fhir4_resources_clinical.TFhirCareTeamList;
{$ENDIF FHIR_CARETEAM}
{$IFDEF FHIR_CATALOGENTRY}
  TFhirCatalogEntryRelatedEntry = fhir4_resources_admin.TFhirCatalogEntryRelatedEntry;
  TFhirCatalogEntryRelatedEntryList = fhir4_resources_admin.TFhirCatalogEntryRelatedEntryList;
  TFhirCatalogEntry = fhir4_resources_admin.TFhirCatalogEntry;
  TFhirCatalogEntryList = fhir4_resources_admin.TFhirCatalogEntryList;
{$ENDIF FHIR_CATALOGENTRY}
{$IFDEF FHIR_CHARGEITEM}
  TFhirChargeItemPerformer = fhir4_resources_financial.TFhirChargeItemPerformer;
  TFhirChargeItemPerformerList = fhir4_resources_financial.TFhirChargeItemPerformerList;
  TFhirChargeItem = fhir4_resources_financial.TFhirChargeItem;
  TFhirChargeItemList = fhir4_resources_financial.TFhirChargeItemList;
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CHARGEITEMDEFINITION}
  TFhirChargeItemDefinitionApplicability = fhir4_resources_canonical.TFhirChargeItemDefinitionApplicability;
  TFhirChargeItemDefinitionApplicabilityList = fhir4_resources_canonical.TFhirChargeItemDefinitionApplicabilityList;
  TFhirChargeItemDefinitionPropertyGroup = fhir4_resources_canonical.TFhirChargeItemDefinitionPropertyGroup;
  TFhirChargeItemDefinitionPropertyGroupList = fhir4_resources_canonical.TFhirChargeItemDefinitionPropertyGroupList;
  TFhirChargeItemDefinitionPropertyGroupPriceComponent = fhir4_resources_canonical.TFhirChargeItemDefinitionPropertyGroupPriceComponent;
  TFhirChargeItemDefinitionPropertyGroupPriceComponentList = fhir4_resources_canonical.TFhirChargeItemDefinitionPropertyGroupPriceComponentList;
  TFhirChargeItemDefinition = fhir4_resources_canonical.TFhirChargeItemDefinition;
  TFhirChargeItemDefinitionList = fhir4_resources_canonical.TFhirChargeItemDefinitionList;
{$ENDIF FHIR_CHARGEITEMDEFINITION}
{$IFDEF FHIR_CLAIM}
  TFhirClaimRelated = fhir4_resources_financial.TFhirClaimRelated;
  TFhirClaimRelatedList = fhir4_resources_financial.TFhirClaimRelatedList;
  TFhirClaimPayee = fhir4_resources_financial.TFhirClaimPayee;
  TFhirClaimPayeeList = fhir4_resources_financial.TFhirClaimPayeeList;
  TFhirClaimCareTeam = fhir4_resources_financial.TFhirClaimCareTeam;
  TFhirClaimCareTeamList = fhir4_resources_financial.TFhirClaimCareTeamList;
  TFhirClaimSupportingInfo = fhir4_resources_financial.TFhirClaimSupportingInfo;
  TFhirClaimSupportingInfoList = fhir4_resources_financial.TFhirClaimSupportingInfoList;
  TFhirClaimDiagnosis = fhir4_resources_financial.TFhirClaimDiagnosis;
  TFhirClaimDiagnosisList = fhir4_resources_financial.TFhirClaimDiagnosisList;
  TFhirClaimProcedure = fhir4_resources_financial.TFhirClaimProcedure;
  TFhirClaimProcedureList = fhir4_resources_financial.TFhirClaimProcedureList;
  TFhirClaimInsurance = fhir4_resources_financial.TFhirClaimInsurance;
  TFhirClaimInsuranceList = fhir4_resources_financial.TFhirClaimInsuranceList;
  TFhirClaimAccident = fhir4_resources_financial.TFhirClaimAccident;
  TFhirClaimAccidentList = fhir4_resources_financial.TFhirClaimAccidentList;
  TFhirClaimItem = fhir4_resources_financial.TFhirClaimItem;
  TFhirClaimItemList = fhir4_resources_financial.TFhirClaimItemList;
  TFhirClaimItemDetail = fhir4_resources_financial.TFhirClaimItemDetail;
  TFhirClaimItemDetailList = fhir4_resources_financial.TFhirClaimItemDetailList;
  TFhirClaimItemDetailSubDetail = fhir4_resources_financial.TFhirClaimItemDetailSubDetail;
  TFhirClaimItemDetailSubDetailList = fhir4_resources_financial.TFhirClaimItemDetailSubDetailList;
  TFhirClaim = fhir4_resources_financial.TFhirClaim;
  TFhirClaimList = fhir4_resources_financial.TFhirClaimList;
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  TFhirClaimResponseItem = fhir4_resources_financial.TFhirClaimResponseItem;
  TFhirClaimResponseItemList = fhir4_resources_financial.TFhirClaimResponseItemList;
  TFhirClaimResponseItemAdjudication = fhir4_resources_financial.TFhirClaimResponseItemAdjudication;
  TFhirClaimResponseItemAdjudicationList = fhir4_resources_financial.TFhirClaimResponseItemAdjudicationList;
  TFhirClaimResponseItemDetail = fhir4_resources_financial.TFhirClaimResponseItemDetail;
  TFhirClaimResponseItemDetailList = fhir4_resources_financial.TFhirClaimResponseItemDetailList;
  TFhirClaimResponseItemDetailSubDetail = fhir4_resources_financial.TFhirClaimResponseItemDetailSubDetail;
  TFhirClaimResponseItemDetailSubDetailList = fhir4_resources_financial.TFhirClaimResponseItemDetailSubDetailList;
  TFhirClaimResponseAddItem = fhir4_resources_financial.TFhirClaimResponseAddItem;
  TFhirClaimResponseAddItemList = fhir4_resources_financial.TFhirClaimResponseAddItemList;
  TFhirClaimResponseAddItemDetail = fhir4_resources_financial.TFhirClaimResponseAddItemDetail;
  TFhirClaimResponseAddItemDetailList = fhir4_resources_financial.TFhirClaimResponseAddItemDetailList;
  TFhirClaimResponseAddItemDetailSubDetail = fhir4_resources_financial.TFhirClaimResponseAddItemDetailSubDetail;
  TFhirClaimResponseAddItemDetailSubDetailList = fhir4_resources_financial.TFhirClaimResponseAddItemDetailSubDetailList;
  TFhirClaimResponseTotal = fhir4_resources_financial.TFhirClaimResponseTotal;
  TFhirClaimResponseTotalList = fhir4_resources_financial.TFhirClaimResponseTotalList;
  TFhirClaimResponsePayment = fhir4_resources_financial.TFhirClaimResponsePayment;
  TFhirClaimResponsePaymentList = fhir4_resources_financial.TFhirClaimResponsePaymentList;
  TFhirClaimResponseProcessNote = fhir4_resources_financial.TFhirClaimResponseProcessNote;
  TFhirClaimResponseProcessNoteList = fhir4_resources_financial.TFhirClaimResponseProcessNoteList;
  TFhirClaimResponseInsurance = fhir4_resources_financial.TFhirClaimResponseInsurance;
  TFhirClaimResponseInsuranceList = fhir4_resources_financial.TFhirClaimResponseInsuranceList;
  TFhirClaimResponseError = fhir4_resources_financial.TFhirClaimResponseError;
  TFhirClaimResponseErrorList = fhir4_resources_financial.TFhirClaimResponseErrorList;
  TFhirClaimResponse = fhir4_resources_financial.TFhirClaimResponse;
  TFhirClaimResponseList = fhir4_resources_financial.TFhirClaimResponseList;
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  TFhirClinicalImpressionInvestigation = fhir4_resources_clinical.TFhirClinicalImpressionInvestigation;
  TFhirClinicalImpressionInvestigationList = fhir4_resources_clinical.TFhirClinicalImpressionInvestigationList;
  TFhirClinicalImpressionFinding = fhir4_resources_clinical.TFhirClinicalImpressionFinding;
  TFhirClinicalImpressionFindingList = fhir4_resources_clinical.TFhirClinicalImpressionFindingList;
  TFhirClinicalImpression = fhir4_resources_clinical.TFhirClinicalImpression;
  TFhirClinicalImpressionList = fhir4_resources_clinical.TFhirClinicalImpressionList;
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_CODESYSTEM}
  TFhirCodeSystemFilter = fhir4_resources_canonical.TFhirCodeSystemFilter;
  TFhirCodeSystemFilterList = fhir4_resources_canonical.TFhirCodeSystemFilterList;
  TFhirCodeSystemProperty = fhir4_resources_canonical.TFhirCodeSystemProperty;
  TFhirCodeSystemPropertyList = fhir4_resources_canonical.TFhirCodeSystemPropertyList;
  TFhirCodeSystemConcept = fhir4_resources_canonical.TFhirCodeSystemConcept;
  TFhirCodeSystemConceptList = fhir4_resources_canonical.TFhirCodeSystemConceptList;
  TFhirCodeSystemConceptDesignation = fhir4_resources_canonical.TFhirCodeSystemConceptDesignation;
  TFhirCodeSystemConceptDesignationList = fhir4_resources_canonical.TFhirCodeSystemConceptDesignationList;
  TFhirCodeSystemConceptProperty = fhir4_resources_canonical.TFhirCodeSystemConceptProperty;
  TFhirCodeSystemConceptPropertyList = fhir4_resources_canonical.TFhirCodeSystemConceptPropertyList;
  TFhirCodeSystem = fhir4_resources_canonical.TFhirCodeSystem;
  TFhirCodeSystemList = fhir4_resources_canonical.TFhirCodeSystemList;
{$ENDIF FHIR_CODESYSTEM}
{$IFDEF FHIR_COMMUNICATION}
  TFhirCommunicationPayload = fhir4_resources_clinical.TFhirCommunicationPayload;
  TFhirCommunicationPayloadList = fhir4_resources_clinical.TFhirCommunicationPayloadList;
  TFhirCommunication = fhir4_resources_clinical.TFhirCommunication;
  TFhirCommunicationList = fhir4_resources_clinical.TFhirCommunicationList;
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  TFhirCommunicationRequestPayload = fhir4_resources_clinical.TFhirCommunicationRequestPayload;
  TFhirCommunicationRequestPayloadList = fhir4_resources_clinical.TFhirCommunicationRequestPayloadList;
  TFhirCommunicationRequest = fhir4_resources_clinical.TFhirCommunicationRequest;
  TFhirCommunicationRequestList = fhir4_resources_clinical.TFhirCommunicationRequestList;
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  TFhirCompartmentDefinitionResource = fhir4_resources_canonical.TFhirCompartmentDefinitionResource;
  TFhirCompartmentDefinitionResourceList = fhir4_resources_canonical.TFhirCompartmentDefinitionResourceList;
  TFhirCompartmentDefinition = fhir4_resources_canonical.TFhirCompartmentDefinition;
  TFhirCompartmentDefinitionList = fhir4_resources_canonical.TFhirCompartmentDefinitionList;
{$ENDIF FHIR_COMPARTMENTDEFINITION}
{$IFDEF FHIR_COMPOSITION}
  TFhirCompositionAttester = fhir4_resources_clinical.TFhirCompositionAttester;
  TFhirCompositionAttesterList = fhir4_resources_clinical.TFhirCompositionAttesterList;
  TFhirCompositionRelatesTo = fhir4_resources_clinical.TFhirCompositionRelatesTo;
  TFhirCompositionRelatesToList = fhir4_resources_clinical.TFhirCompositionRelatesToList;
  TFhirCompositionEvent = fhir4_resources_clinical.TFhirCompositionEvent;
  TFhirCompositionEventList = fhir4_resources_clinical.TFhirCompositionEventList;
  TFhirCompositionSection = fhir4_resources_clinical.TFhirCompositionSection;
  TFhirCompositionSectionList = fhir4_resources_clinical.TFhirCompositionSectionList;
  TFhirComposition = fhir4_resources_clinical.TFhirComposition;
  TFhirCompositionList = fhir4_resources_clinical.TFhirCompositionList;
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  TFhirConceptMapGroup = fhir4_resources_canonical.TFhirConceptMapGroup;
  TFhirConceptMapGroupList = fhir4_resources_canonical.TFhirConceptMapGroupList;
  TFhirConceptMapGroupElement = fhir4_resources_canonical.TFhirConceptMapGroupElement;
  TFhirConceptMapGroupElementList = fhir4_resources_canonical.TFhirConceptMapGroupElementList;
  TFhirConceptMapGroupElementTarget = fhir4_resources_canonical.TFhirConceptMapGroupElementTarget;
  TFhirConceptMapGroupElementTargetList = fhir4_resources_canonical.TFhirConceptMapGroupElementTargetList;
  TFhirConceptMapGroupElementTargetDependsOn = fhir4_resources_canonical.TFhirConceptMapGroupElementTargetDependsOn;
  TFhirConceptMapGroupElementTargetDependsOnList = fhir4_resources_canonical.TFhirConceptMapGroupElementTargetDependsOnList;
  TFhirConceptMapGroupUnmapped = fhir4_resources_canonical.TFhirConceptMapGroupUnmapped;
  TFhirConceptMapGroupUnmappedList = fhir4_resources_canonical.TFhirConceptMapGroupUnmappedList;
  TFhirConceptMap = fhir4_resources_canonical.TFhirConceptMap;
  TFhirConceptMapList = fhir4_resources_canonical.TFhirConceptMapList;
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_CONDITION}
  TFhirConditionStage = fhir4_resources_clinical.TFhirConditionStage;
  TFhirConditionStageList = fhir4_resources_clinical.TFhirConditionStageList;
  TFhirConditionEvidence = fhir4_resources_clinical.TFhirConditionEvidence;
  TFhirConditionEvidenceList = fhir4_resources_clinical.TFhirConditionEvidenceList;
  TFhirCondition = fhir4_resources_clinical.TFhirCondition;
  TFhirConditionList = fhir4_resources_clinical.TFhirConditionList;
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_CONSENT}
  TFhirConsentPolicy = fhir4_resources_other.TFhirConsentPolicy;
  TFhirConsentPolicyList = fhir4_resources_other.TFhirConsentPolicyList;
  TFhirConsentVerification = fhir4_resources_other.TFhirConsentVerification;
  TFhirConsentVerificationList = fhir4_resources_other.TFhirConsentVerificationList;
  TFhirConsentProvision = fhir4_resources_other.TFhirConsentProvision;
  TFhirConsentProvisionList = fhir4_resources_other.TFhirConsentProvisionList;
  TFhirConsentProvisionActor = fhir4_resources_other.TFhirConsentProvisionActor;
  TFhirConsentProvisionActorList = fhir4_resources_other.TFhirConsentProvisionActorList;
  TFhirConsentProvisionData = fhir4_resources_other.TFhirConsentProvisionData;
  TFhirConsentProvisionDataList = fhir4_resources_other.TFhirConsentProvisionDataList;
  TFhirConsent = fhir4_resources_other.TFhirConsent;
  TFhirConsentList = fhir4_resources_other.TFhirConsentList;
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  TFhirContractContentDefinition = fhir4_resources_other.TFhirContractContentDefinition;
  TFhirContractContentDefinitionList = fhir4_resources_other.TFhirContractContentDefinitionList;
  TFhirContractTerm = fhir4_resources_other.TFhirContractTerm;
  TFhirContractTermList = fhir4_resources_other.TFhirContractTermList;
  TFhirContractTermSecurityLabel = fhir4_resources_other.TFhirContractTermSecurityLabel;
  TFhirContractTermSecurityLabelList = fhir4_resources_other.TFhirContractTermSecurityLabelList;
  TFhirContractTermOffer = fhir4_resources_other.TFhirContractTermOffer;
  TFhirContractTermOfferList = fhir4_resources_other.TFhirContractTermOfferList;
  TFhirContractTermOfferParty = fhir4_resources_other.TFhirContractTermOfferParty;
  TFhirContractTermOfferPartyList = fhir4_resources_other.TFhirContractTermOfferPartyList;
  TFhirContractTermOfferAnswer = fhir4_resources_other.TFhirContractTermOfferAnswer;
  TFhirContractTermOfferAnswerList = fhir4_resources_other.TFhirContractTermOfferAnswerList;
  TFhirContractTermAsset = fhir4_resources_other.TFhirContractTermAsset;
  TFhirContractTermAssetList = fhir4_resources_other.TFhirContractTermAssetList;
  TFhirContractTermAssetContext = fhir4_resources_other.TFhirContractTermAssetContext;
  TFhirContractTermAssetContextList = fhir4_resources_other.TFhirContractTermAssetContextList;
  TFhirContractTermAssetValuedItem = fhir4_resources_other.TFhirContractTermAssetValuedItem;
  TFhirContractTermAssetValuedItemList = fhir4_resources_other.TFhirContractTermAssetValuedItemList;
  TFhirContractTermAction = fhir4_resources_other.TFhirContractTermAction;
  TFhirContractTermActionList = fhir4_resources_other.TFhirContractTermActionList;
  TFhirContractTermActionSubject = fhir4_resources_other.TFhirContractTermActionSubject;
  TFhirContractTermActionSubjectList = fhir4_resources_other.TFhirContractTermActionSubjectList;
  TFhirContractSigner = fhir4_resources_other.TFhirContractSigner;
  TFhirContractSignerList = fhir4_resources_other.TFhirContractSignerList;
  TFhirContractFriendly = fhir4_resources_other.TFhirContractFriendly;
  TFhirContractFriendlyList = fhir4_resources_other.TFhirContractFriendlyList;
  TFhirContractLegal = fhir4_resources_other.TFhirContractLegal;
  TFhirContractLegalList = fhir4_resources_other.TFhirContractLegalList;
  TFhirContractRule = fhir4_resources_other.TFhirContractRule;
  TFhirContractRuleList = fhir4_resources_other.TFhirContractRuleList;
  TFhirContract = fhir4_resources_other.TFhirContract;
  TFhirContractList = fhir4_resources_other.TFhirContractList;
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_COVERAGE}
  TFhirCoverageClass = fhir4_resources_financial.TFhirCoverageClass;
  TFhirCoverageClassList = fhir4_resources_financial.TFhirCoverageClassList;
  TFhirCoverageCostToBeneficiary = fhir4_resources_financial.TFhirCoverageCostToBeneficiary;
  TFhirCoverageCostToBeneficiaryList = fhir4_resources_financial.TFhirCoverageCostToBeneficiaryList;
  TFhirCoverageCostToBeneficiaryException = fhir4_resources_financial.TFhirCoverageCostToBeneficiaryException;
  TFhirCoverageCostToBeneficiaryExceptionList = fhir4_resources_financial.TFhirCoverageCostToBeneficiaryExceptionList;
  TFhirCoverage = fhir4_resources_financial.TFhirCoverage;
  TFhirCoverageList = fhir4_resources_financial.TFhirCoverageList;
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  TFhirCoverageEligibilityRequestSupportingInfo = fhir4_resources_financial.TFhirCoverageEligibilityRequestSupportingInfo;
  TFhirCoverageEligibilityRequestSupportingInfoList = fhir4_resources_financial.TFhirCoverageEligibilityRequestSupportingInfoList;
  TFhirCoverageEligibilityRequestInsurance = fhir4_resources_financial.TFhirCoverageEligibilityRequestInsurance;
  TFhirCoverageEligibilityRequestInsuranceList = fhir4_resources_financial.TFhirCoverageEligibilityRequestInsuranceList;
  TFhirCoverageEligibilityRequestItem = fhir4_resources_financial.TFhirCoverageEligibilityRequestItem;
  TFhirCoverageEligibilityRequestItemList = fhir4_resources_financial.TFhirCoverageEligibilityRequestItemList;
  TFhirCoverageEligibilityRequestItemDiagnosis = fhir4_resources_financial.TFhirCoverageEligibilityRequestItemDiagnosis;
  TFhirCoverageEligibilityRequestItemDiagnosisList = fhir4_resources_financial.TFhirCoverageEligibilityRequestItemDiagnosisList;
  TFhirCoverageEligibilityRequest = fhir4_resources_financial.TFhirCoverageEligibilityRequest;
  TFhirCoverageEligibilityRequestList = fhir4_resources_financial.TFhirCoverageEligibilityRequestList;
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}
{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  TFhirCoverageEligibilityResponseInsurance = fhir4_resources_financial.TFhirCoverageEligibilityResponseInsurance;
  TFhirCoverageEligibilityResponseInsuranceList = fhir4_resources_financial.TFhirCoverageEligibilityResponseInsuranceList;
  TFhirCoverageEligibilityResponseInsuranceItem = fhir4_resources_financial.TFhirCoverageEligibilityResponseInsuranceItem;
  TFhirCoverageEligibilityResponseInsuranceItemList = fhir4_resources_financial.TFhirCoverageEligibilityResponseInsuranceItemList;
  TFhirCoverageEligibilityResponseInsuranceItemBenefit = fhir4_resources_financial.TFhirCoverageEligibilityResponseInsuranceItemBenefit;
  TFhirCoverageEligibilityResponseInsuranceItemBenefitList = fhir4_resources_financial.TFhirCoverageEligibilityResponseInsuranceItemBenefitList;
  TFhirCoverageEligibilityResponseError = fhir4_resources_financial.TFhirCoverageEligibilityResponseError;
  TFhirCoverageEligibilityResponseErrorList = fhir4_resources_financial.TFhirCoverageEligibilityResponseErrorList;
  TFhirCoverageEligibilityResponse = fhir4_resources_financial.TFhirCoverageEligibilityResponse;
  TFhirCoverageEligibilityResponseList = fhir4_resources_financial.TFhirCoverageEligibilityResponseList;
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}
{$IFDEF FHIR_DETECTEDISSUE}
  TFhirDetectedIssueEvidence = fhir4_resources_clinical.TFhirDetectedIssueEvidence;
  TFhirDetectedIssueEvidenceList = fhir4_resources_clinical.TFhirDetectedIssueEvidenceList;
  TFhirDetectedIssueMitigation = fhir4_resources_clinical.TFhirDetectedIssueMitigation;
  TFhirDetectedIssueMitigationList = fhir4_resources_clinical.TFhirDetectedIssueMitigationList;
  TFhirDetectedIssue = fhir4_resources_clinical.TFhirDetectedIssue;
  TFhirDetectedIssueList = fhir4_resources_clinical.TFhirDetectedIssueList;
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  TFhirDeviceUdiCarrier = fhir4_resources_admin.TFhirDeviceUdiCarrier;
  TFhirDeviceUdiCarrierList = fhir4_resources_admin.TFhirDeviceUdiCarrierList;
  TFhirDeviceDeviceName = fhir4_resources_admin.TFhirDeviceDeviceName;
  TFhirDeviceDeviceNameList = fhir4_resources_admin.TFhirDeviceDeviceNameList;
  TFhirDeviceSpecialization = fhir4_resources_admin.TFhirDeviceSpecialization;
  TFhirDeviceSpecializationList = fhir4_resources_admin.TFhirDeviceSpecializationList;
  TFhirDeviceVersion = fhir4_resources_admin.TFhirDeviceVersion;
  TFhirDeviceVersionList = fhir4_resources_admin.TFhirDeviceVersionList;
  TFhirDeviceProperty = fhir4_resources_admin.TFhirDeviceProperty;
  TFhirDevicePropertyList = fhir4_resources_admin.TFhirDevicePropertyList;
  TFhirDevice = fhir4_resources_admin.TFhirDevice;
  TFhirDeviceList = fhir4_resources_admin.TFhirDeviceList;
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICEDEFINITION}
  TFhirDeviceDefinitionUdiDeviceIdentifier = fhir4_resources_canonical.TFhirDeviceDefinitionUdiDeviceIdentifier;
  TFhirDeviceDefinitionUdiDeviceIdentifierList = fhir4_resources_canonical.TFhirDeviceDefinitionUdiDeviceIdentifierList;
  TFhirDeviceDefinitionDeviceName = fhir4_resources_canonical.TFhirDeviceDefinitionDeviceName;
  TFhirDeviceDefinitionDeviceNameList = fhir4_resources_canonical.TFhirDeviceDefinitionDeviceNameList;
  TFhirDeviceDefinitionSpecialization = fhir4_resources_canonical.TFhirDeviceDefinitionSpecialization;
  TFhirDeviceDefinitionSpecializationList = fhir4_resources_canonical.TFhirDeviceDefinitionSpecializationList;
  TFhirDeviceDefinitionCapability = fhir4_resources_canonical.TFhirDeviceDefinitionCapability;
  TFhirDeviceDefinitionCapabilityList = fhir4_resources_canonical.TFhirDeviceDefinitionCapabilityList;
  TFhirDeviceDefinitionProperty = fhir4_resources_canonical.TFhirDeviceDefinitionProperty;
  TFhirDeviceDefinitionPropertyList = fhir4_resources_canonical.TFhirDeviceDefinitionPropertyList;
  TFhirDeviceDefinitionMaterial = fhir4_resources_canonical.TFhirDeviceDefinitionMaterial;
  TFhirDeviceDefinitionMaterialList = fhir4_resources_canonical.TFhirDeviceDefinitionMaterialList;
  TFhirDeviceDefinition = fhir4_resources_canonical.TFhirDeviceDefinition;
  TFhirDeviceDefinitionList = fhir4_resources_canonical.TFhirDeviceDefinitionList;
{$ENDIF FHIR_DEVICEDEFINITION}
{$IFDEF FHIR_DEVICEMETRIC}
  TFhirDeviceMetricCalibration = fhir4_resources_admin.TFhirDeviceMetricCalibration;
  TFhirDeviceMetricCalibrationList = fhir4_resources_admin.TFhirDeviceMetricCalibrationList;
  TFhirDeviceMetric = fhir4_resources_admin.TFhirDeviceMetric;
  TFhirDeviceMetricList = fhir4_resources_admin.TFhirDeviceMetricList;
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEREQUEST}
  TFhirDeviceRequestParameter = fhir4_resources_clinical.TFhirDeviceRequestParameter;
  TFhirDeviceRequestParameterList = fhir4_resources_clinical.TFhirDeviceRequestParameterList;
  TFhirDeviceRequest = fhir4_resources_clinical.TFhirDeviceRequest;
  TFhirDeviceRequestList = fhir4_resources_clinical.TFhirDeviceRequestList;
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  TFhirDeviceUseStatement = fhir4_resources_clinical.TFhirDeviceUseStatement;
  TFhirDeviceUseStatementList = fhir4_resources_clinical.TFhirDeviceUseStatementList;
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  TFhirDiagnosticReportMedia = fhir4_resources_clinical.TFhirDiagnosticReportMedia;
  TFhirDiagnosticReportMediaList = fhir4_resources_clinical.TFhirDiagnosticReportMediaList;
  TFhirDiagnosticReport = fhir4_resources_clinical.TFhirDiagnosticReport;
  TFhirDiagnosticReportList = fhir4_resources_clinical.TFhirDiagnosticReportList;
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  TFhirDocumentManifestRelated = fhir4_resources_clinical.TFhirDocumentManifestRelated;
  TFhirDocumentManifestRelatedList = fhir4_resources_clinical.TFhirDocumentManifestRelatedList;
  TFhirDocumentManifest = fhir4_resources_clinical.TFhirDocumentManifest;
  TFhirDocumentManifestList = fhir4_resources_clinical.TFhirDocumentManifestList;
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  TFhirDocumentReferenceRelatesTo = fhir4_resources_clinical.TFhirDocumentReferenceRelatesTo;
  TFhirDocumentReferenceRelatesToList = fhir4_resources_clinical.TFhirDocumentReferenceRelatesToList;
  TFhirDocumentReferenceContent = fhir4_resources_clinical.TFhirDocumentReferenceContent;
  TFhirDocumentReferenceContentList = fhir4_resources_clinical.TFhirDocumentReferenceContentList;
  TFhirDocumentReferenceContext = fhir4_resources_clinical.TFhirDocumentReferenceContext;
  TFhirDocumentReferenceContextList = fhir4_resources_clinical.TFhirDocumentReferenceContextList;
  TFhirDocumentReference = fhir4_resources_clinical.TFhirDocumentReference;
  TFhirDocumentReferenceList = fhir4_resources_clinical.TFhirDocumentReferenceList;
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_EFFECTEVIDENCESYNTHESIS}
  TFhirEffectEvidenceSynthesisSampleSize = fhir4_resources_other.TFhirEffectEvidenceSynthesisSampleSize;
  TFhirEffectEvidenceSynthesisSampleSizeList = fhir4_resources_other.TFhirEffectEvidenceSynthesisSampleSizeList;
  TFhirEffectEvidenceSynthesisResultsByExposure = fhir4_resources_other.TFhirEffectEvidenceSynthesisResultsByExposure;
  TFhirEffectEvidenceSynthesisResultsByExposureList = fhir4_resources_other.TFhirEffectEvidenceSynthesisResultsByExposureList;
  TFhirEffectEvidenceSynthesisEffectEstimate = fhir4_resources_other.TFhirEffectEvidenceSynthesisEffectEstimate;
  TFhirEffectEvidenceSynthesisEffectEstimateList = fhir4_resources_other.TFhirEffectEvidenceSynthesisEffectEstimateList;
  TFhirEffectEvidenceSynthesisEffectEstimatePrecisionEstimate = fhir4_resources_other.TFhirEffectEvidenceSynthesisEffectEstimatePrecisionEstimate;
  TFhirEffectEvidenceSynthesisEffectEstimatePrecisionEstimateList = fhir4_resources_other.TFhirEffectEvidenceSynthesisEffectEstimatePrecisionEstimateList;
  TFhirEffectEvidenceSynthesisCertainty = fhir4_resources_other.TFhirEffectEvidenceSynthesisCertainty;
  TFhirEffectEvidenceSynthesisCertaintyList = fhir4_resources_other.TFhirEffectEvidenceSynthesisCertaintyList;
  TFhirEffectEvidenceSynthesisCertaintyCertaintySubcomponent = fhir4_resources_other.TFhirEffectEvidenceSynthesisCertaintyCertaintySubcomponent;
  TFhirEffectEvidenceSynthesisCertaintyCertaintySubcomponentList = fhir4_resources_other.TFhirEffectEvidenceSynthesisCertaintyCertaintySubcomponentList;
  TFhirEffectEvidenceSynthesis = fhir4_resources_other.TFhirEffectEvidenceSynthesis;
  TFhirEffectEvidenceSynthesisList = fhir4_resources_other.TFhirEffectEvidenceSynthesisList;
{$ENDIF FHIR_EFFECTEVIDENCESYNTHESIS}
{$IFDEF FHIR_ENCOUNTER}
  TFhirEncounterStatusHistory = fhir4_resources_admin.TFhirEncounterStatusHistory;
  TFhirEncounterStatusHistoryList = fhir4_resources_admin.TFhirEncounterStatusHistoryList;
  TFhirEncounterClassHistory = fhir4_resources_admin.TFhirEncounterClassHistory;
  TFhirEncounterClassHistoryList = fhir4_resources_admin.TFhirEncounterClassHistoryList;
  TFhirEncounterParticipant = fhir4_resources_admin.TFhirEncounterParticipant;
  TFhirEncounterParticipantList = fhir4_resources_admin.TFhirEncounterParticipantList;
  TFhirEncounterDiagnosis = fhir4_resources_admin.TFhirEncounterDiagnosis;
  TFhirEncounterDiagnosisList = fhir4_resources_admin.TFhirEncounterDiagnosisList;
  TFhirEncounterHospitalization = fhir4_resources_admin.TFhirEncounterHospitalization;
  TFhirEncounterHospitalizationList = fhir4_resources_admin.TFhirEncounterHospitalizationList;
  TFhirEncounterLocation = fhir4_resources_admin.TFhirEncounterLocation;
  TFhirEncounterLocationList = fhir4_resources_admin.TFhirEncounterLocationList;
  TFhirEncounter = fhir4_resources_admin.TFhirEncounter;
  TFhirEncounterList = fhir4_resources_admin.TFhirEncounterList;
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENDPOINT}
  TFhirEndpoint = fhir4_resources_admin.TFhirEndpoint;
  TFhirEndpointList = fhir4_resources_admin.TFhirEndpointList;
{$ENDIF FHIR_ENDPOINT}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  TFhirEnrollmentRequest = fhir4_resources_financial.TFhirEnrollmentRequest;
  TFhirEnrollmentRequestList = fhir4_resources_financial.TFhirEnrollmentRequestList;
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  TFhirEnrollmentResponse = fhir4_resources_financial.TFhirEnrollmentResponse;
  TFhirEnrollmentResponseList = fhir4_resources_financial.TFhirEnrollmentResponseList;
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EPISODEOFCARE}
  TFhirEpisodeOfCareStatusHistory = fhir4_resources_admin.TFhirEpisodeOfCareStatusHistory;
  TFhirEpisodeOfCareStatusHistoryList = fhir4_resources_admin.TFhirEpisodeOfCareStatusHistoryList;
  TFhirEpisodeOfCareDiagnosis = fhir4_resources_admin.TFhirEpisodeOfCareDiagnosis;
  TFhirEpisodeOfCareDiagnosisList = fhir4_resources_admin.TFhirEpisodeOfCareDiagnosisList;
  TFhirEpisodeOfCare = fhir4_resources_admin.TFhirEpisodeOfCare;
  TFhirEpisodeOfCareList = fhir4_resources_admin.TFhirEpisodeOfCareList;
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EVENTDEFINITION}
  TFhirEventDefinition = fhir4_resources_canonical.TFhirEventDefinition;
  TFhirEventDefinitionList = fhir4_resources_canonical.TFhirEventDefinitionList;
{$ENDIF FHIR_EVENTDEFINITION}
{$IFDEF FHIR_EVIDENCE}
  TFhirEvidence = fhir4_resources_other.TFhirEvidence;
  TFhirEvidenceList = fhir4_resources_other.TFhirEvidenceList;
{$ENDIF FHIR_EVIDENCE}
{$IFDEF FHIR_EVIDENCEVARIABLE}
  TFhirEvidenceVariableCharacteristic = fhir4_resources_other.TFhirEvidenceVariableCharacteristic;
  TFhirEvidenceVariableCharacteristicList = fhir4_resources_other.TFhirEvidenceVariableCharacteristicList;
  TFhirEvidenceVariable = fhir4_resources_other.TFhirEvidenceVariable;
  TFhirEvidenceVariableList = fhir4_resources_other.TFhirEvidenceVariableList;
{$ENDIF FHIR_EVIDENCEVARIABLE}
{$IFDEF FHIR_EXAMPLESCENARIO}
  TFhirExampleScenarioActor = fhir4_resources_canonical.TFhirExampleScenarioActor;
  TFhirExampleScenarioActorList = fhir4_resources_canonical.TFhirExampleScenarioActorList;
  TFhirExampleScenarioInstance = fhir4_resources_canonical.TFhirExampleScenarioInstance;
  TFhirExampleScenarioInstanceList = fhir4_resources_canonical.TFhirExampleScenarioInstanceList;
  TFhirExampleScenarioInstanceVersion = fhir4_resources_canonical.TFhirExampleScenarioInstanceVersion;
  TFhirExampleScenarioInstanceVersionList = fhir4_resources_canonical.TFhirExampleScenarioInstanceVersionList;
  TFhirExampleScenarioInstanceContainedInstance = fhir4_resources_canonical.TFhirExampleScenarioInstanceContainedInstance;
  TFhirExampleScenarioInstanceContainedInstanceList = fhir4_resources_canonical.TFhirExampleScenarioInstanceContainedInstanceList;
  TFhirExampleScenarioProcess = fhir4_resources_canonical.TFhirExampleScenarioProcess;
  TFhirExampleScenarioProcessList = fhir4_resources_canonical.TFhirExampleScenarioProcessList;
  TFhirExampleScenarioProcessStep = fhir4_resources_canonical.TFhirExampleScenarioProcessStep;
  TFhirExampleScenarioProcessStepList = fhir4_resources_canonical.TFhirExampleScenarioProcessStepList;
  TFhirExampleScenarioProcessStepOperation = fhir4_resources_canonical.TFhirExampleScenarioProcessStepOperation;
  TFhirExampleScenarioProcessStepOperationList = fhir4_resources_canonical.TFhirExampleScenarioProcessStepOperationList;
  TFhirExampleScenarioProcessStepAlternative = fhir4_resources_canonical.TFhirExampleScenarioProcessStepAlternative;
  TFhirExampleScenarioProcessStepAlternativeList = fhir4_resources_canonical.TFhirExampleScenarioProcessStepAlternativeList;
  TFhirExampleScenario = fhir4_resources_canonical.TFhirExampleScenario;
  TFhirExampleScenarioList = fhir4_resources_canonical.TFhirExampleScenarioList;
{$ENDIF FHIR_EXAMPLESCENARIO}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  TFhirExplanationOfBenefitRelated = fhir4_resources_financial.TFhirExplanationOfBenefitRelated;
  TFhirExplanationOfBenefitRelatedList = fhir4_resources_financial.TFhirExplanationOfBenefitRelatedList;
  TFhirExplanationOfBenefitPayee = fhir4_resources_financial.TFhirExplanationOfBenefitPayee;
  TFhirExplanationOfBenefitPayeeList = fhir4_resources_financial.TFhirExplanationOfBenefitPayeeList;
  TFhirExplanationOfBenefitCareTeam = fhir4_resources_financial.TFhirExplanationOfBenefitCareTeam;
  TFhirExplanationOfBenefitCareTeamList = fhir4_resources_financial.TFhirExplanationOfBenefitCareTeamList;
  TFhirExplanationOfBenefitSupportingInfo = fhir4_resources_financial.TFhirExplanationOfBenefitSupportingInfo;
  TFhirExplanationOfBenefitSupportingInfoList = fhir4_resources_financial.TFhirExplanationOfBenefitSupportingInfoList;
  TFhirExplanationOfBenefitDiagnosis = fhir4_resources_financial.TFhirExplanationOfBenefitDiagnosis;
  TFhirExplanationOfBenefitDiagnosisList = fhir4_resources_financial.TFhirExplanationOfBenefitDiagnosisList;
  TFhirExplanationOfBenefitProcedure = fhir4_resources_financial.TFhirExplanationOfBenefitProcedure;
  TFhirExplanationOfBenefitProcedureList = fhir4_resources_financial.TFhirExplanationOfBenefitProcedureList;
  TFhirExplanationOfBenefitInsurance = fhir4_resources_financial.TFhirExplanationOfBenefitInsurance;
  TFhirExplanationOfBenefitInsuranceList = fhir4_resources_financial.TFhirExplanationOfBenefitInsuranceList;
  TFhirExplanationOfBenefitAccident = fhir4_resources_financial.TFhirExplanationOfBenefitAccident;
  TFhirExplanationOfBenefitAccidentList = fhir4_resources_financial.TFhirExplanationOfBenefitAccidentList;
  TFhirExplanationOfBenefitItem = fhir4_resources_financial.TFhirExplanationOfBenefitItem;
  TFhirExplanationOfBenefitItemList = fhir4_resources_financial.TFhirExplanationOfBenefitItemList;
  TFhirExplanationOfBenefitItemAdjudication = fhir4_resources_financial.TFhirExplanationOfBenefitItemAdjudication;
  TFhirExplanationOfBenefitItemAdjudicationList = fhir4_resources_financial.TFhirExplanationOfBenefitItemAdjudicationList;
  TFhirExplanationOfBenefitItemDetail = fhir4_resources_financial.TFhirExplanationOfBenefitItemDetail;
  TFhirExplanationOfBenefitItemDetailList = fhir4_resources_financial.TFhirExplanationOfBenefitItemDetailList;
  TFhirExplanationOfBenefitItemDetailSubDetail = fhir4_resources_financial.TFhirExplanationOfBenefitItemDetailSubDetail;
  TFhirExplanationOfBenefitItemDetailSubDetailList = fhir4_resources_financial.TFhirExplanationOfBenefitItemDetailSubDetailList;
  TFhirExplanationOfBenefitAddItem = fhir4_resources_financial.TFhirExplanationOfBenefitAddItem;
  TFhirExplanationOfBenefitAddItemList = fhir4_resources_financial.TFhirExplanationOfBenefitAddItemList;
  TFhirExplanationOfBenefitAddItemDetail = fhir4_resources_financial.TFhirExplanationOfBenefitAddItemDetail;
  TFhirExplanationOfBenefitAddItemDetailList = fhir4_resources_financial.TFhirExplanationOfBenefitAddItemDetailList;
  TFhirExplanationOfBenefitAddItemDetailSubDetail = fhir4_resources_financial.TFhirExplanationOfBenefitAddItemDetailSubDetail;
  TFhirExplanationOfBenefitAddItemDetailSubDetailList = fhir4_resources_financial.TFhirExplanationOfBenefitAddItemDetailSubDetailList;
  TFhirExplanationOfBenefitTotal = fhir4_resources_financial.TFhirExplanationOfBenefitTotal;
  TFhirExplanationOfBenefitTotalList = fhir4_resources_financial.TFhirExplanationOfBenefitTotalList;
  TFhirExplanationOfBenefitPayment = fhir4_resources_financial.TFhirExplanationOfBenefitPayment;
  TFhirExplanationOfBenefitPaymentList = fhir4_resources_financial.TFhirExplanationOfBenefitPaymentList;
  TFhirExplanationOfBenefitProcessNote = fhir4_resources_financial.TFhirExplanationOfBenefitProcessNote;
  TFhirExplanationOfBenefitProcessNoteList = fhir4_resources_financial.TFhirExplanationOfBenefitProcessNoteList;
  TFhirExplanationOfBenefitBenefitBalance = fhir4_resources_financial.TFhirExplanationOfBenefitBenefitBalance;
  TFhirExplanationOfBenefitBenefitBalanceList = fhir4_resources_financial.TFhirExplanationOfBenefitBenefitBalanceList;
  TFhirExplanationOfBenefitBenefitBalanceFinancial = fhir4_resources_financial.TFhirExplanationOfBenefitBenefitBalanceFinancial;
  TFhirExplanationOfBenefitBenefitBalanceFinancialList = fhir4_resources_financial.TFhirExplanationOfBenefitBenefitBalanceFinancialList;
  TFhirExplanationOfBenefit = fhir4_resources_financial.TFhirExplanationOfBenefit;
  TFhirExplanationOfBenefitList = fhir4_resources_financial.TFhirExplanationOfBenefitList;
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  TFhirFamilyMemberHistoryCondition = fhir4_resources_clinical.TFhirFamilyMemberHistoryCondition;
  TFhirFamilyMemberHistoryConditionList = fhir4_resources_clinical.TFhirFamilyMemberHistoryConditionList;
  TFhirFamilyMemberHistory = fhir4_resources_clinical.TFhirFamilyMemberHistory;
  TFhirFamilyMemberHistoryList = fhir4_resources_clinical.TFhirFamilyMemberHistoryList;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  TFhirFlag = fhir4_resources_clinical.TFhirFlag;
  TFhirFlagList = fhir4_resources_clinical.TFhirFlagList;
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  TFhirGoalTarget = fhir4_resources_clinical.TFhirGoalTarget;
  TFhirGoalTargetList = fhir4_resources_clinical.TFhirGoalTargetList;
  TFhirGoal = fhir4_resources_clinical.TFhirGoal;
  TFhirGoalList = fhir4_resources_clinical.TFhirGoalList;
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GRAPHDEFINITION}
  TFhirGraphDefinitionLink = fhir4_resources_canonical.TFhirGraphDefinitionLink;
  TFhirGraphDefinitionLinkList = fhir4_resources_canonical.TFhirGraphDefinitionLinkList;
  TFhirGraphDefinitionLinkTarget = fhir4_resources_canonical.TFhirGraphDefinitionLinkTarget;
  TFhirGraphDefinitionLinkTargetList = fhir4_resources_canonical.TFhirGraphDefinitionLinkTargetList;
  TFhirGraphDefinitionLinkTargetCompartment = fhir4_resources_canonical.TFhirGraphDefinitionLinkTargetCompartment;
  TFhirGraphDefinitionLinkTargetCompartmentList = fhir4_resources_canonical.TFhirGraphDefinitionLinkTargetCompartmentList;
  TFhirGraphDefinition = fhir4_resources_canonical.TFhirGraphDefinition;
  TFhirGraphDefinitionList = fhir4_resources_canonical.TFhirGraphDefinitionList;
{$ENDIF FHIR_GRAPHDEFINITION}
{$IFDEF FHIR_GROUP}
  TFhirGroupCharacteristic = fhir4_resources_admin.TFhirGroupCharacteristic;
  TFhirGroupCharacteristicList = fhir4_resources_admin.TFhirGroupCharacteristicList;
  TFhirGroupMember = fhir4_resources_admin.TFhirGroupMember;
  TFhirGroupMemberList = fhir4_resources_admin.TFhirGroupMemberList;
  TFhirGroup = fhir4_resources_admin.TFhirGroup;
  TFhirGroupList = fhir4_resources_admin.TFhirGroupList;
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_GUIDANCERESPONSE}
  TFhirGuidanceResponse = fhir4_resources_other.TFhirGuidanceResponse;
  TFhirGuidanceResponseList = fhir4_resources_other.TFhirGuidanceResponseList;
{$ENDIF FHIR_GUIDANCERESPONSE}
{$IFDEF FHIR_HEALTHCARESERVICE}
  TFhirHealthcareServiceEligibility = fhir4_resources_admin.TFhirHealthcareServiceEligibility;
  TFhirHealthcareServiceEligibilityList = fhir4_resources_admin.TFhirHealthcareServiceEligibilityList;
  TFhirHealthcareServiceAvailableTime = fhir4_resources_admin.TFhirHealthcareServiceAvailableTime;
  TFhirHealthcareServiceAvailableTimeList = fhir4_resources_admin.TFhirHealthcareServiceAvailableTimeList;
  TFhirHealthcareServiceNotAvailable = fhir4_resources_admin.TFhirHealthcareServiceNotAvailable;
  TFhirHealthcareServiceNotAvailableList = fhir4_resources_admin.TFhirHealthcareServiceNotAvailableList;
  TFhirHealthcareService = fhir4_resources_admin.TFhirHealthcareService;
  TFhirHealthcareServiceList = fhir4_resources_admin.TFhirHealthcareServiceList;
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGSTUDY}
  TFhirImagingStudySeries = fhir4_resources_clinical.TFhirImagingStudySeries;
  TFhirImagingStudySeriesList = fhir4_resources_clinical.TFhirImagingStudySeriesList;
  TFhirImagingStudySeriesPerformer = fhir4_resources_clinical.TFhirImagingStudySeriesPerformer;
  TFhirImagingStudySeriesPerformerList = fhir4_resources_clinical.TFhirImagingStudySeriesPerformerList;
  TFhirImagingStudySeriesInstance = fhir4_resources_clinical.TFhirImagingStudySeriesInstance;
  TFhirImagingStudySeriesInstanceList = fhir4_resources_clinical.TFhirImagingStudySeriesInstanceList;
  TFhirImagingStudy = fhir4_resources_clinical.TFhirImagingStudy;
  TFhirImagingStudyList = fhir4_resources_clinical.TFhirImagingStudyList;
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  TFhirImmunizationPerformer = fhir4_resources_clinical.TFhirImmunizationPerformer;
  TFhirImmunizationPerformerList = fhir4_resources_clinical.TFhirImmunizationPerformerList;
  TFhirImmunizationEducation = fhir4_resources_clinical.TFhirImmunizationEducation;
  TFhirImmunizationEducationList = fhir4_resources_clinical.TFhirImmunizationEducationList;
  TFhirImmunizationReaction = fhir4_resources_clinical.TFhirImmunizationReaction;
  TFhirImmunizationReactionList = fhir4_resources_clinical.TFhirImmunizationReactionList;
  TFhirImmunizationProtocolApplied = fhir4_resources_clinical.TFhirImmunizationProtocolApplied;
  TFhirImmunizationProtocolAppliedList = fhir4_resources_clinical.TFhirImmunizationProtocolAppliedList;
  TFhirImmunization = fhir4_resources_clinical.TFhirImmunization;
  TFhirImmunizationList = fhir4_resources_clinical.TFhirImmunizationList;
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  TFhirImmunizationEvaluation = fhir4_resources_clinical.TFhirImmunizationEvaluation;
  TFhirImmunizationEvaluationList = fhir4_resources_clinical.TFhirImmunizationEvaluationList;
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  TFhirImmunizationRecommendationRecommendation = fhir4_resources_clinical.TFhirImmunizationRecommendationRecommendation;
  TFhirImmunizationRecommendationRecommendationList = fhir4_resources_clinical.TFhirImmunizationRecommendationRecommendationList;
  TFhirImmunizationRecommendationRecommendationDateCriterion = fhir4_resources_clinical.TFhirImmunizationRecommendationRecommendationDateCriterion;
  TFhirImmunizationRecommendationRecommendationDateCriterionList = fhir4_resources_clinical.TFhirImmunizationRecommendationRecommendationDateCriterionList;
  TFhirImmunizationRecommendation = fhir4_resources_clinical.TFhirImmunizationRecommendation;
  TFhirImmunizationRecommendationList = fhir4_resources_clinical.TFhirImmunizationRecommendationList;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  TFhirImplementationGuideDependsOn = fhir4_resources_canonical.TFhirImplementationGuideDependsOn;
  TFhirImplementationGuideDependsOnList = fhir4_resources_canonical.TFhirImplementationGuideDependsOnList;
  TFhirImplementationGuideGlobal = fhir4_resources_canonical.TFhirImplementationGuideGlobal;
  TFhirImplementationGuideGlobalList = fhir4_resources_canonical.TFhirImplementationGuideGlobalList;
  TFhirImplementationGuideDefinition = fhir4_resources_canonical.TFhirImplementationGuideDefinition;
  TFhirImplementationGuideDefinitionList = fhir4_resources_canonical.TFhirImplementationGuideDefinitionList;
  TFhirImplementationGuideDefinitionGrouping = fhir4_resources_canonical.TFhirImplementationGuideDefinitionGrouping;
  TFhirImplementationGuideDefinitionGroupingList = fhir4_resources_canonical.TFhirImplementationGuideDefinitionGroupingList;
  TFhirImplementationGuideDefinitionResource = fhir4_resources_canonical.TFhirImplementationGuideDefinitionResource;
  TFhirImplementationGuideDefinitionResourceList = fhir4_resources_canonical.TFhirImplementationGuideDefinitionResourceList;
  TFhirImplementationGuideDefinitionPage = fhir4_resources_canonical.TFhirImplementationGuideDefinitionPage;
  TFhirImplementationGuideDefinitionPageList = fhir4_resources_canonical.TFhirImplementationGuideDefinitionPageList;
  TFhirImplementationGuideDefinitionParameter = fhir4_resources_canonical.TFhirImplementationGuideDefinitionParameter;
  TFhirImplementationGuideDefinitionParameterList = fhir4_resources_canonical.TFhirImplementationGuideDefinitionParameterList;
  TFhirImplementationGuideDefinitionTemplate = fhir4_resources_canonical.TFhirImplementationGuideDefinitionTemplate;
  TFhirImplementationGuideDefinitionTemplateList = fhir4_resources_canonical.TFhirImplementationGuideDefinitionTemplateList;
  TFhirImplementationGuideManifest = fhir4_resources_canonical.TFhirImplementationGuideManifest;
  TFhirImplementationGuideManifestList = fhir4_resources_canonical.TFhirImplementationGuideManifestList;
  TFhirImplementationGuideManifestResource = fhir4_resources_canonical.TFhirImplementationGuideManifestResource;
  TFhirImplementationGuideManifestResourceList = fhir4_resources_canonical.TFhirImplementationGuideManifestResourceList;
  TFhirImplementationGuideManifestPage = fhir4_resources_canonical.TFhirImplementationGuideManifestPage;
  TFhirImplementationGuideManifestPageList = fhir4_resources_canonical.TFhirImplementationGuideManifestPageList;
  TFhirImplementationGuide = fhir4_resources_canonical.TFhirImplementationGuide;
  TFhirImplementationGuideList = fhir4_resources_canonical.TFhirImplementationGuideList;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_INSURANCEPLAN}
  TFhirInsurancePlanContact = fhir4_resources_financial.TFhirInsurancePlanContact;
  TFhirInsurancePlanContactList = fhir4_resources_financial.TFhirInsurancePlanContactList;
  TFhirInsurancePlanCoverage = fhir4_resources_financial.TFhirInsurancePlanCoverage;
  TFhirInsurancePlanCoverageList = fhir4_resources_financial.TFhirInsurancePlanCoverageList;
  TFhirInsurancePlanCoverageBenefit = fhir4_resources_financial.TFhirInsurancePlanCoverageBenefit;
  TFhirInsurancePlanCoverageBenefitList = fhir4_resources_financial.TFhirInsurancePlanCoverageBenefitList;
  TFhirInsurancePlanCoverageBenefitLimit = fhir4_resources_financial.TFhirInsurancePlanCoverageBenefitLimit;
  TFhirInsurancePlanCoverageBenefitLimitList = fhir4_resources_financial.TFhirInsurancePlanCoverageBenefitLimitList;
  TFhirInsurancePlanPlan = fhir4_resources_financial.TFhirInsurancePlanPlan;
  TFhirInsurancePlanPlanList = fhir4_resources_financial.TFhirInsurancePlanPlanList;
  TFhirInsurancePlanPlanGeneralCost = fhir4_resources_financial.TFhirInsurancePlanPlanGeneralCost;
  TFhirInsurancePlanPlanGeneralCostList = fhir4_resources_financial.TFhirInsurancePlanPlanGeneralCostList;
  TFhirInsurancePlanPlanSpecificCost = fhir4_resources_financial.TFhirInsurancePlanPlanSpecificCost;
  TFhirInsurancePlanPlanSpecificCostList = fhir4_resources_financial.TFhirInsurancePlanPlanSpecificCostList;
  TFhirInsurancePlanPlanSpecificCostBenefit = fhir4_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefit;
  TFhirInsurancePlanPlanSpecificCostBenefitList = fhir4_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefitList;
  TFhirInsurancePlanPlanSpecificCostBenefitCost = fhir4_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefitCost;
  TFhirInsurancePlanPlanSpecificCostBenefitCostList = fhir4_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefitCostList;
  TFhirInsurancePlan = fhir4_resources_financial.TFhirInsurancePlan;
  TFhirInsurancePlanList = fhir4_resources_financial.TFhirInsurancePlanList;
{$ENDIF FHIR_INSURANCEPLAN}
{$IFDEF FHIR_INVOICE}
  TFhirInvoiceParticipant = fhir4_resources_financial.TFhirInvoiceParticipant;
  TFhirInvoiceParticipantList = fhir4_resources_financial.TFhirInvoiceParticipantList;
  TFhirInvoiceLineItem = fhir4_resources_financial.TFhirInvoiceLineItem;
  TFhirInvoiceLineItemList = fhir4_resources_financial.TFhirInvoiceLineItemList;
  TFhirInvoiceLineItemPriceComponent = fhir4_resources_financial.TFhirInvoiceLineItemPriceComponent;
  TFhirInvoiceLineItemPriceComponentList = fhir4_resources_financial.TFhirInvoiceLineItemPriceComponentList;
  TFhirInvoice = fhir4_resources_financial.TFhirInvoice;
  TFhirInvoiceList = fhir4_resources_financial.TFhirInvoiceList;
{$ENDIF FHIR_INVOICE}
{$IFDEF FHIR_LIBRARY}
  TFhirLibrary = fhir4_resources_canonical.TFhirLibrary;
  TFhirLibraryList = fhir4_resources_canonical.TFhirLibraryList;
{$ENDIF FHIR_LIBRARY}
{$IFDEF FHIR_LINKAGE}
  TFhirLinkageItem = fhir4_resources_other.TFhirLinkageItem;
  TFhirLinkageItemList = fhir4_resources_other.TFhirLinkageItemList;
  TFhirLinkage = fhir4_resources_other.TFhirLinkage;
  TFhirLinkageList = fhir4_resources_other.TFhirLinkageList;
{$ENDIF FHIR_LINKAGE}
{$IFDEF FHIR_LIST}
  TFhirListEntry = fhir4_resources_other.TFhirListEntry;
  TFhirListEntryList = fhir4_resources_other.TFhirListEntryList;
  TFhirList = fhir4_resources_other.TFhirList;
  TFhirListList = fhir4_resources_other.TFhirListList;
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_LOCATION}
  TFhirLocationPosition = fhir4_resources_admin.TFhirLocationPosition;
  TFhirLocationPositionList = fhir4_resources_admin.TFhirLocationPositionList;
  TFhirLocationHoursOfOperation = fhir4_resources_admin.TFhirLocationHoursOfOperation;
  TFhirLocationHoursOfOperationList = fhir4_resources_admin.TFhirLocationHoursOfOperationList;
  TFhirLocation = fhir4_resources_admin.TFhirLocation;
  TFhirLocationList = fhir4_resources_admin.TFhirLocationList;
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_MEASURE}
  TFhirMeasureGroup = fhir4_resources_canonical.TFhirMeasureGroup;
  TFhirMeasureGroupList = fhir4_resources_canonical.TFhirMeasureGroupList;
  TFhirMeasureGroupPopulation = fhir4_resources_canonical.TFhirMeasureGroupPopulation;
  TFhirMeasureGroupPopulationList = fhir4_resources_canonical.TFhirMeasureGroupPopulationList;
  TFhirMeasureGroupStratifier = fhir4_resources_canonical.TFhirMeasureGroupStratifier;
  TFhirMeasureGroupStratifierList = fhir4_resources_canonical.TFhirMeasureGroupStratifierList;
  TFhirMeasureGroupStratifierComponent = fhir4_resources_canonical.TFhirMeasureGroupStratifierComponent;
  TFhirMeasureGroupStratifierComponentList = fhir4_resources_canonical.TFhirMeasureGroupStratifierComponentList;
  TFhirMeasureSupplementalData = fhir4_resources_canonical.TFhirMeasureSupplementalData;
  TFhirMeasureSupplementalDataList = fhir4_resources_canonical.TFhirMeasureSupplementalDataList;
  TFhirMeasure = fhir4_resources_canonical.TFhirMeasure;
  TFhirMeasureList = fhir4_resources_canonical.TFhirMeasureList;
{$ENDIF FHIR_MEASURE}
{$IFDEF FHIR_MEASUREREPORT}
  TFhirMeasureReportGroup = fhir4_resources_other.TFhirMeasureReportGroup;
  TFhirMeasureReportGroupList = fhir4_resources_other.TFhirMeasureReportGroupList;
  TFhirMeasureReportGroupPopulation = fhir4_resources_other.TFhirMeasureReportGroupPopulation;
  TFhirMeasureReportGroupPopulationList = fhir4_resources_other.TFhirMeasureReportGroupPopulationList;
  TFhirMeasureReportGroupStratifier = fhir4_resources_other.TFhirMeasureReportGroupStratifier;
  TFhirMeasureReportGroupStratifierList = fhir4_resources_other.TFhirMeasureReportGroupStratifierList;
  TFhirMeasureReportGroupStratifierStratum = fhir4_resources_other.TFhirMeasureReportGroupStratifierStratum;
  TFhirMeasureReportGroupStratifierStratumList = fhir4_resources_other.TFhirMeasureReportGroupStratifierStratumList;
  TFhirMeasureReportGroupStratifierStratumComponent = fhir4_resources_other.TFhirMeasureReportGroupStratifierStratumComponent;
  TFhirMeasureReportGroupStratifierStratumComponentList = fhir4_resources_other.TFhirMeasureReportGroupStratifierStratumComponentList;
  TFhirMeasureReportGroupStratifierStratumPopulation = fhir4_resources_other.TFhirMeasureReportGroupStratifierStratumPopulation;
  TFhirMeasureReportGroupStratifierStratumPopulationList = fhir4_resources_other.TFhirMeasureReportGroupStratifierStratumPopulationList;
  TFhirMeasureReport = fhir4_resources_other.TFhirMeasureReport;
  TFhirMeasureReportList = fhir4_resources_other.TFhirMeasureReportList;
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MEDIA}
  TFhirMedia = fhir4_resources_clinical.TFhirMedia;
  TFhirMediaList = fhir4_resources_clinical.TFhirMediaList;
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATION}
  TFhirMedicationIngredient = fhir4_resources_medications.TFhirMedicationIngredient;
  TFhirMedicationIngredientList = fhir4_resources_medications.TFhirMedicationIngredientList;
  TFhirMedicationBatch = fhir4_resources_medications.TFhirMedicationBatch;
  TFhirMedicationBatchList = fhir4_resources_medications.TFhirMedicationBatchList;
  TFhirMedication = fhir4_resources_medications.TFhirMedication;
  TFhirMedicationList = fhir4_resources_medications.TFhirMedicationList;
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  TFhirMedicationAdministrationPerformer = fhir4_resources_clinical.TFhirMedicationAdministrationPerformer;
  TFhirMedicationAdministrationPerformerList = fhir4_resources_clinical.TFhirMedicationAdministrationPerformerList;
  TFhirMedicationAdministrationDosage = fhir4_resources_clinical.TFhirMedicationAdministrationDosage;
  TFhirMedicationAdministrationDosageList = fhir4_resources_clinical.TFhirMedicationAdministrationDosageList;
  TFhirMedicationAdministration = fhir4_resources_clinical.TFhirMedicationAdministration;
  TFhirMedicationAdministrationList = fhir4_resources_clinical.TFhirMedicationAdministrationList;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  TFhirMedicationDispensePerformer = fhir4_resources_clinical.TFhirMedicationDispensePerformer;
  TFhirMedicationDispensePerformerList = fhir4_resources_clinical.TFhirMedicationDispensePerformerList;
  TFhirMedicationDispenseSubstitution = fhir4_resources_clinical.TFhirMedicationDispenseSubstitution;
  TFhirMedicationDispenseSubstitutionList = fhir4_resources_clinical.TFhirMedicationDispenseSubstitutionList;
  TFhirMedicationDispense = fhir4_resources_clinical.TFhirMedicationDispense;
  TFhirMedicationDispenseList = fhir4_resources_clinical.TFhirMedicationDispenseList;
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  TFhirMedicationKnowledgeRelatedMedicationKnowledge = fhir4_resources_medications.TFhirMedicationKnowledgeRelatedMedicationKnowledge;
  TFhirMedicationKnowledgeRelatedMedicationKnowledgeList = fhir4_resources_medications.TFhirMedicationKnowledgeRelatedMedicationKnowledgeList;
  TFhirMedicationKnowledgeMonograph = fhir4_resources_medications.TFhirMedicationKnowledgeMonograph;
  TFhirMedicationKnowledgeMonographList = fhir4_resources_medications.TFhirMedicationKnowledgeMonographList;
  TFhirMedicationKnowledgeIngredient = fhir4_resources_medications.TFhirMedicationKnowledgeIngredient;
  TFhirMedicationKnowledgeIngredientList = fhir4_resources_medications.TFhirMedicationKnowledgeIngredientList;
  TFhirMedicationKnowledgeCost = fhir4_resources_medications.TFhirMedicationKnowledgeCost;
  TFhirMedicationKnowledgeCostList = fhir4_resources_medications.TFhirMedicationKnowledgeCostList;
  TFhirMedicationKnowledgeMonitoringProgram = fhir4_resources_medications.TFhirMedicationKnowledgeMonitoringProgram;
  TFhirMedicationKnowledgeMonitoringProgramList = fhir4_resources_medications.TFhirMedicationKnowledgeMonitoringProgramList;
  TFhirMedicationKnowledgeAdministrationGuidelines = fhir4_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelines;
  TFhirMedicationKnowledgeAdministrationGuidelinesList = fhir4_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelinesList;
  TFhirMedicationKnowledgeAdministrationGuidelinesDosage = fhir4_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelinesDosage;
  TFhirMedicationKnowledgeAdministrationGuidelinesDosageList = fhir4_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelinesDosageList;
  TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristics = fhir4_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristics;
  TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsList = fhir4_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsList;
  TFhirMedicationKnowledgeMedicineClassification = fhir4_resources_medications.TFhirMedicationKnowledgeMedicineClassification;
  TFhirMedicationKnowledgeMedicineClassificationList = fhir4_resources_medications.TFhirMedicationKnowledgeMedicineClassificationList;
  TFhirMedicationKnowledgePackaging = fhir4_resources_medications.TFhirMedicationKnowledgePackaging;
  TFhirMedicationKnowledgePackagingList = fhir4_resources_medications.TFhirMedicationKnowledgePackagingList;
  TFhirMedicationKnowledgeDrugCharacteristic = fhir4_resources_medications.TFhirMedicationKnowledgeDrugCharacteristic;
  TFhirMedicationKnowledgeDrugCharacteristicList = fhir4_resources_medications.TFhirMedicationKnowledgeDrugCharacteristicList;
  TFhirMedicationKnowledgeRegulatory = fhir4_resources_medications.TFhirMedicationKnowledgeRegulatory;
  TFhirMedicationKnowledgeRegulatoryList = fhir4_resources_medications.TFhirMedicationKnowledgeRegulatoryList;
  TFhirMedicationKnowledgeRegulatorySubstitution = fhir4_resources_medications.TFhirMedicationKnowledgeRegulatorySubstitution;
  TFhirMedicationKnowledgeRegulatorySubstitutionList = fhir4_resources_medications.TFhirMedicationKnowledgeRegulatorySubstitutionList;
  TFhirMedicationKnowledgeRegulatorySchedule = fhir4_resources_medications.TFhirMedicationKnowledgeRegulatorySchedule;
  TFhirMedicationKnowledgeRegulatoryScheduleList = fhir4_resources_medications.TFhirMedicationKnowledgeRegulatoryScheduleList;
  TFhirMedicationKnowledgeRegulatoryMaxDispense = fhir4_resources_medications.TFhirMedicationKnowledgeRegulatoryMaxDispense;
  TFhirMedicationKnowledgeRegulatoryMaxDispenseList = fhir4_resources_medications.TFhirMedicationKnowledgeRegulatoryMaxDispenseList;
  TFhirMedicationKnowledgeKinetics = fhir4_resources_medications.TFhirMedicationKnowledgeKinetics;
  TFhirMedicationKnowledgeKineticsList = fhir4_resources_medications.TFhirMedicationKnowledgeKineticsList;
  TFhirMedicationKnowledge = fhir4_resources_medications.TFhirMedicationKnowledge;
  TFhirMedicationKnowledgeList = fhir4_resources_medications.TFhirMedicationKnowledgeList;
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  TFhirMedicationRequestDispenseRequest = fhir4_resources_clinical.TFhirMedicationRequestDispenseRequest;
  TFhirMedicationRequestDispenseRequestList = fhir4_resources_clinical.TFhirMedicationRequestDispenseRequestList;
  TFhirMedicationRequestDispenseRequestInitialFill = fhir4_resources_clinical.TFhirMedicationRequestDispenseRequestInitialFill;
  TFhirMedicationRequestDispenseRequestInitialFillList = fhir4_resources_clinical.TFhirMedicationRequestDispenseRequestInitialFillList;
  TFhirMedicationRequestSubstitution = fhir4_resources_clinical.TFhirMedicationRequestSubstitution;
  TFhirMedicationRequestSubstitutionList = fhir4_resources_clinical.TFhirMedicationRequestSubstitutionList;
  TFhirMedicationRequest = fhir4_resources_clinical.TFhirMedicationRequest;
  TFhirMedicationRequestList = fhir4_resources_clinical.TFhirMedicationRequestList;
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  TFhirMedicationStatement = fhir4_resources_clinical.TFhirMedicationStatement;
  TFhirMedicationStatementList = fhir4_resources_clinical.TFhirMedicationStatementList;
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_MEDICINALPRODUCT}
  TFhirMedicinalProductName = fhir4_resources_medications.TFhirMedicinalProductName;
  TFhirMedicinalProductNameList = fhir4_resources_medications.TFhirMedicinalProductNameList;
  TFhirMedicinalProductNameNamePart = fhir4_resources_medications.TFhirMedicinalProductNameNamePart;
  TFhirMedicinalProductNameNamePartList = fhir4_resources_medications.TFhirMedicinalProductNameNamePartList;
  TFhirMedicinalProductNameCountryLanguage = fhir4_resources_medications.TFhirMedicinalProductNameCountryLanguage;
  TFhirMedicinalProductNameCountryLanguageList = fhir4_resources_medications.TFhirMedicinalProductNameCountryLanguageList;
  TFhirMedicinalProductManufacturingBusinessOperation = fhir4_resources_medications.TFhirMedicinalProductManufacturingBusinessOperation;
  TFhirMedicinalProductManufacturingBusinessOperationList = fhir4_resources_medications.TFhirMedicinalProductManufacturingBusinessOperationList;
  TFhirMedicinalProductSpecialDesignation = fhir4_resources_medications.TFhirMedicinalProductSpecialDesignation;
  TFhirMedicinalProductSpecialDesignationList = fhir4_resources_medications.TFhirMedicinalProductSpecialDesignationList;
  TFhirMedicinalProduct = fhir4_resources_medications.TFhirMedicinalProduct;
  TFhirMedicinalProductList = fhir4_resources_medications.TFhirMedicinalProductList;
{$ENDIF FHIR_MEDICINALPRODUCT}
{$IFDEF FHIR_MEDICINALPRODUCTAUTHORIZATION}
  TFhirMedicinalProductAuthorizationJurisdictionalAuthorization = fhir4_resources_medications.TFhirMedicinalProductAuthorizationJurisdictionalAuthorization;
  TFhirMedicinalProductAuthorizationJurisdictionalAuthorizationList = fhir4_resources_medications.TFhirMedicinalProductAuthorizationJurisdictionalAuthorizationList;
  TFhirMedicinalProductAuthorizationProcedure = fhir4_resources_medications.TFhirMedicinalProductAuthorizationProcedure;
  TFhirMedicinalProductAuthorizationProcedureList = fhir4_resources_medications.TFhirMedicinalProductAuthorizationProcedureList;
  TFhirMedicinalProductAuthorization = fhir4_resources_medications.TFhirMedicinalProductAuthorization;
  TFhirMedicinalProductAuthorizationList = fhir4_resources_medications.TFhirMedicinalProductAuthorizationList;
{$ENDIF FHIR_MEDICINALPRODUCTAUTHORIZATION}
{$IFDEF FHIR_MEDICINALPRODUCTCONTRAINDICATION}
  TFhirMedicinalProductContraindicationOtherTherapy = fhir4_resources_medications.TFhirMedicinalProductContraindicationOtherTherapy;
  TFhirMedicinalProductContraindicationOtherTherapyList = fhir4_resources_medications.TFhirMedicinalProductContraindicationOtherTherapyList;
  TFhirMedicinalProductContraindication = fhir4_resources_medications.TFhirMedicinalProductContraindication;
  TFhirMedicinalProductContraindicationList = fhir4_resources_medications.TFhirMedicinalProductContraindicationList;
{$ENDIF FHIR_MEDICINALPRODUCTCONTRAINDICATION}
{$IFDEF FHIR_MEDICINALPRODUCTINDICATION}
  TFhirMedicinalProductIndicationOtherTherapy = fhir4_resources_medications.TFhirMedicinalProductIndicationOtherTherapy;
  TFhirMedicinalProductIndicationOtherTherapyList = fhir4_resources_medications.TFhirMedicinalProductIndicationOtherTherapyList;
  TFhirMedicinalProductIndication = fhir4_resources_medications.TFhirMedicinalProductIndication;
  TFhirMedicinalProductIndicationList = fhir4_resources_medications.TFhirMedicinalProductIndicationList;
{$ENDIF FHIR_MEDICINALPRODUCTINDICATION}
{$IFDEF FHIR_MEDICINALPRODUCTINGREDIENT}
  TFhirMedicinalProductIngredientSpecifiedSubstance = fhir4_resources_medications.TFhirMedicinalProductIngredientSpecifiedSubstance;
  TFhirMedicinalProductIngredientSpecifiedSubstanceList = fhir4_resources_medications.TFhirMedicinalProductIngredientSpecifiedSubstanceList;
  TFhirMedicinalProductIngredientSpecifiedSubstanceStrength = fhir4_resources_medications.TFhirMedicinalProductIngredientSpecifiedSubstanceStrength;
  TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthList = fhir4_resources_medications.TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthList;
  TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength = fhir4_resources_medications.TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength;
  TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthList = fhir4_resources_medications.TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthList;
  TFhirMedicinalProductIngredientSubstance = fhir4_resources_medications.TFhirMedicinalProductIngredientSubstance;
  TFhirMedicinalProductIngredientSubstanceList = fhir4_resources_medications.TFhirMedicinalProductIngredientSubstanceList;
  TFhirMedicinalProductIngredient = fhir4_resources_medications.TFhirMedicinalProductIngredient;
  TFhirMedicinalProductIngredientList = fhir4_resources_medications.TFhirMedicinalProductIngredientList;
{$ENDIF FHIR_MEDICINALPRODUCTINGREDIENT}
{$IFDEF FHIR_MEDICINALPRODUCTINTERACTION}
  TFhirMedicinalProductInteractionInteractant = fhir4_resources_medications.TFhirMedicinalProductInteractionInteractant;
  TFhirMedicinalProductInteractionInteractantList = fhir4_resources_medications.TFhirMedicinalProductInteractionInteractantList;
  TFhirMedicinalProductInteraction = fhir4_resources_medications.TFhirMedicinalProductInteraction;
  TFhirMedicinalProductInteractionList = fhir4_resources_medications.TFhirMedicinalProductInteractionList;
{$ENDIF FHIR_MEDICINALPRODUCTINTERACTION}
{$IFDEF FHIR_MEDICINALPRODUCTMANUFACTURED}
  TFhirMedicinalProductManufactured = fhir4_resources_medications.TFhirMedicinalProductManufactured;
  TFhirMedicinalProductManufacturedList = fhir4_resources_medications.TFhirMedicinalProductManufacturedList;
{$ENDIF FHIR_MEDICINALPRODUCTMANUFACTURED}
{$IFDEF FHIR_MEDICINALPRODUCTPACKAGED}
  TFhirMedicinalProductPackagedBatchIdentifier = fhir4_resources_medications.TFhirMedicinalProductPackagedBatchIdentifier;
  TFhirMedicinalProductPackagedBatchIdentifierList = fhir4_resources_medications.TFhirMedicinalProductPackagedBatchIdentifierList;
  TFhirMedicinalProductPackagedPackageItem = fhir4_resources_medications.TFhirMedicinalProductPackagedPackageItem;
  TFhirMedicinalProductPackagedPackageItemList = fhir4_resources_medications.TFhirMedicinalProductPackagedPackageItemList;
  TFhirMedicinalProductPackaged = fhir4_resources_medications.TFhirMedicinalProductPackaged;
  TFhirMedicinalProductPackagedList = fhir4_resources_medications.TFhirMedicinalProductPackagedList;
{$ENDIF FHIR_MEDICINALPRODUCTPACKAGED}
{$IFDEF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}
  TFhirMedicinalProductPharmaceuticalCharacteristics = fhir4_resources_medications.TFhirMedicinalProductPharmaceuticalCharacteristics;
  TFhirMedicinalProductPharmaceuticalCharacteristicsList = fhir4_resources_medications.TFhirMedicinalProductPharmaceuticalCharacteristicsList;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministration = fhir4_resources_medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministration;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministrationList = fhir4_resources_medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministrationList;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies = fhir4_resources_medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesList = fhir4_resources_medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesList;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod = fhir4_resources_medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodList = fhir4_resources_medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodList;
  TFhirMedicinalProductPharmaceutical = fhir4_resources_medications.TFhirMedicinalProductPharmaceutical;
  TFhirMedicinalProductPharmaceuticalList = fhir4_resources_medications.TFhirMedicinalProductPharmaceuticalList;
{$ENDIF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}
{$IFDEF FHIR_MEDICINALPRODUCTUNDESIRABLEEFFECT}
  TFhirMedicinalProductUndesirableEffect = fhir4_resources_medications.TFhirMedicinalProductUndesirableEffect;
  TFhirMedicinalProductUndesirableEffectList = fhir4_resources_medications.TFhirMedicinalProductUndesirableEffectList;
{$ENDIF FHIR_MEDICINALPRODUCTUNDESIRABLEEFFECT}
{$IFDEF FHIR_MESSAGEDEFINITION}
  TFhirMessageDefinitionFocus = fhir4_resources_canonical.TFhirMessageDefinitionFocus;
  TFhirMessageDefinitionFocusList = fhir4_resources_canonical.TFhirMessageDefinitionFocusList;
  TFhirMessageDefinitionAllowedResponse = fhir4_resources_canonical.TFhirMessageDefinitionAllowedResponse;
  TFhirMessageDefinitionAllowedResponseList = fhir4_resources_canonical.TFhirMessageDefinitionAllowedResponseList;
  TFhirMessageDefinition = fhir4_resources_canonical.TFhirMessageDefinition;
  TFhirMessageDefinitionList = fhir4_resources_canonical.TFhirMessageDefinitionList;
{$ENDIF FHIR_MESSAGEDEFINITION}
{$IFDEF FHIR_MESSAGEHEADER}
  TFhirMessageHeaderDestination = fhir4_resources_other.TFhirMessageHeaderDestination;
  TFhirMessageHeaderDestinationList = fhir4_resources_other.TFhirMessageHeaderDestinationList;
  TFhirMessageHeaderSource = fhir4_resources_other.TFhirMessageHeaderSource;
  TFhirMessageHeaderSourceList = fhir4_resources_other.TFhirMessageHeaderSourceList;
  TFhirMessageHeaderResponse = fhir4_resources_other.TFhirMessageHeaderResponse;
  TFhirMessageHeaderResponseList = fhir4_resources_other.TFhirMessageHeaderResponseList;
  TFhirMessageHeader = fhir4_resources_other.TFhirMessageHeader;
  TFhirMessageHeaderList = fhir4_resources_other.TFhirMessageHeaderList;
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_MOLECULARSEQUENCE}
  TFhirMolecularSequenceReferenceSeq = fhir4_resources_clinical.TFhirMolecularSequenceReferenceSeq;
  TFhirMolecularSequenceReferenceSeqList = fhir4_resources_clinical.TFhirMolecularSequenceReferenceSeqList;
  TFhirMolecularSequenceVariant = fhir4_resources_clinical.TFhirMolecularSequenceVariant;
  TFhirMolecularSequenceVariantList = fhir4_resources_clinical.TFhirMolecularSequenceVariantList;
  TFhirMolecularSequenceQuality = fhir4_resources_clinical.TFhirMolecularSequenceQuality;
  TFhirMolecularSequenceQualityList = fhir4_resources_clinical.TFhirMolecularSequenceQualityList;
  TFhirMolecularSequenceQualityRoc = fhir4_resources_clinical.TFhirMolecularSequenceQualityRoc;
  TFhirMolecularSequenceQualityRocList = fhir4_resources_clinical.TFhirMolecularSequenceQualityRocList;
  TFhirMolecularSequenceRepository = fhir4_resources_clinical.TFhirMolecularSequenceRepository;
  TFhirMolecularSequenceRepositoryList = fhir4_resources_clinical.TFhirMolecularSequenceRepositoryList;
  TFhirMolecularSequenceStructureVariant = fhir4_resources_clinical.TFhirMolecularSequenceStructureVariant;
  TFhirMolecularSequenceStructureVariantList = fhir4_resources_clinical.TFhirMolecularSequenceStructureVariantList;
  TFhirMolecularSequenceStructureVariantOuter = fhir4_resources_clinical.TFhirMolecularSequenceStructureVariantOuter;
  TFhirMolecularSequenceStructureVariantOuterList = fhir4_resources_clinical.TFhirMolecularSequenceStructureVariantOuterList;
  TFhirMolecularSequenceStructureVariantInner = fhir4_resources_clinical.TFhirMolecularSequenceStructureVariantInner;
  TFhirMolecularSequenceStructureVariantInnerList = fhir4_resources_clinical.TFhirMolecularSequenceStructureVariantInnerList;
  TFhirMolecularSequence = fhir4_resources_clinical.TFhirMolecularSequence;
  TFhirMolecularSequenceList = fhir4_resources_clinical.TFhirMolecularSequenceList;
{$ENDIF FHIR_MOLECULARSEQUENCE}
{$IFDEF FHIR_NAMINGSYSTEM}
  TFhirNamingSystemUniqueId = fhir4_resources_canonical.TFhirNamingSystemUniqueId;
  TFhirNamingSystemUniqueIdList = fhir4_resources_canonical.TFhirNamingSystemUniqueIdList;
  TFhirNamingSystem = fhir4_resources_canonical.TFhirNamingSystem;
  TFhirNamingSystemList = fhir4_resources_canonical.TFhirNamingSystemList;
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_NUTRITIONORDER}
  TFhirNutritionOrderOralDiet = fhir4_resources_clinical.TFhirNutritionOrderOralDiet;
  TFhirNutritionOrderOralDietList = fhir4_resources_clinical.TFhirNutritionOrderOralDietList;
  TFhirNutritionOrderOralDietNutrient = fhir4_resources_clinical.TFhirNutritionOrderOralDietNutrient;
  TFhirNutritionOrderOralDietNutrientList = fhir4_resources_clinical.TFhirNutritionOrderOralDietNutrientList;
  TFhirNutritionOrderOralDietTexture = fhir4_resources_clinical.TFhirNutritionOrderOralDietTexture;
  TFhirNutritionOrderOralDietTextureList = fhir4_resources_clinical.TFhirNutritionOrderOralDietTextureList;
  TFhirNutritionOrderSupplement = fhir4_resources_clinical.TFhirNutritionOrderSupplement;
  TFhirNutritionOrderSupplementList = fhir4_resources_clinical.TFhirNutritionOrderSupplementList;
  TFhirNutritionOrderEnteralFormula = fhir4_resources_clinical.TFhirNutritionOrderEnteralFormula;
  TFhirNutritionOrderEnteralFormulaList = fhir4_resources_clinical.TFhirNutritionOrderEnteralFormulaList;
  TFhirNutritionOrderEnteralFormulaAdministration = fhir4_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministration;
  TFhirNutritionOrderEnteralFormulaAdministrationList = fhir4_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministrationList;
  TFhirNutritionOrder = fhir4_resources_clinical.TFhirNutritionOrder;
  TFhirNutritionOrderList = fhir4_resources_clinical.TFhirNutritionOrderList;
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_OBSERVATION}
  TFhirObservationReferenceRange = fhir4_resources_clinical.TFhirObservationReferenceRange;
  TFhirObservationReferenceRangeList = fhir4_resources_clinical.TFhirObservationReferenceRangeList;
  TFhirObservationComponent = fhir4_resources_clinical.TFhirObservationComponent;
  TFhirObservationComponentList = fhir4_resources_clinical.TFhirObservationComponentList;
  TFhirObservation = fhir4_resources_clinical.TFhirObservation;
  TFhirObservationList = fhir4_resources_clinical.TFhirObservationList;
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OBSERVATIONDEFINITION}
  TFhirObservationDefinitionQuantitativeDetails = fhir4_resources_canonical.TFhirObservationDefinitionQuantitativeDetails;
  TFhirObservationDefinitionQuantitativeDetailsList = fhir4_resources_canonical.TFhirObservationDefinitionQuantitativeDetailsList;
  TFhirObservationDefinitionQualifiedInterval = fhir4_resources_canonical.TFhirObservationDefinitionQualifiedInterval;
  TFhirObservationDefinitionQualifiedIntervalList = fhir4_resources_canonical.TFhirObservationDefinitionQualifiedIntervalList;
  TFhirObservationDefinition = fhir4_resources_canonical.TFhirObservationDefinition;
  TFhirObservationDefinitionList = fhir4_resources_canonical.TFhirObservationDefinitionList;
{$ENDIF FHIR_OBSERVATIONDEFINITION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  TFhirOperationDefinitionParameter = fhir4_resources_canonical.TFhirOperationDefinitionParameter;
  TFhirOperationDefinitionParameterList = fhir4_resources_canonical.TFhirOperationDefinitionParameterList;
  TFhirOperationDefinitionParameterBinding = fhir4_resources_canonical.TFhirOperationDefinitionParameterBinding;
  TFhirOperationDefinitionParameterBindingList = fhir4_resources_canonical.TFhirOperationDefinitionParameterBindingList;
  TFhirOperationDefinitionParameterReferencedFrom = fhir4_resources_canonical.TFhirOperationDefinitionParameterReferencedFrom;
  TFhirOperationDefinitionParameterReferencedFromList = fhir4_resources_canonical.TFhirOperationDefinitionParameterReferencedFromList;
  TFhirOperationDefinitionOverload = fhir4_resources_canonical.TFhirOperationDefinitionOverload;
  TFhirOperationDefinitionOverloadList = fhir4_resources_canonical.TFhirOperationDefinitionOverloadList;
  TFhirOperationDefinition = fhir4_resources_canonical.TFhirOperationDefinition;
  TFhirOperationDefinitionList = fhir4_resources_canonical.TFhirOperationDefinitionList;
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  TFhirOperationOutcomeIssue = fhir4_resources_other.TFhirOperationOutcomeIssue;
  TFhirOperationOutcomeIssueList = fhir4_resources_other.TFhirOperationOutcomeIssueList;
  TFhirOperationOutcome = fhir4_resources_other.TFhirOperationOutcome;
  TFhirOperationOutcomeList = fhir4_resources_other.TFhirOperationOutcomeList;
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_ORGANIZATION}
  TFhirOrganizationContact = fhir4_resources_admin.TFhirOrganizationContact;
  TFhirOrganizationContactList = fhir4_resources_admin.TFhirOrganizationContactList;
  TFhirOrganization = fhir4_resources_admin.TFhirOrganization;
  TFhirOrganizationList = fhir4_resources_admin.TFhirOrganizationList;
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  TFhirOrganizationAffiliation = fhir4_resources_admin.TFhirOrganizationAffiliation;
  TFhirOrganizationAffiliationList = fhir4_resources_admin.TFhirOrganizationAffiliationList;
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}
{$IFDEF FHIR_PATIENT}
  TFhirPatientContact = fhir4_resources_admin.TFhirPatientContact;
  TFhirPatientContactList = fhir4_resources_admin.TFhirPatientContactList;
  TFhirPatientCommunication = fhir4_resources_admin.TFhirPatientCommunication;
  TFhirPatientCommunicationList = fhir4_resources_admin.TFhirPatientCommunicationList;
  TFhirPatientLink = fhir4_resources_admin.TFhirPatientLink;
  TFhirPatientLinkList = fhir4_resources_admin.TFhirPatientLinkList;
  TFhirPatient = fhir4_resources_admin.TFhirPatient;
  TFhirPatientList = fhir4_resources_admin.TFhirPatientList;
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PAYMENTNOTICE}
  TFhirPaymentNotice = fhir4_resources_financial.TFhirPaymentNotice;
  TFhirPaymentNoticeList = fhir4_resources_financial.TFhirPaymentNoticeList;
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  TFhirPaymentReconciliationDetail = fhir4_resources_financial.TFhirPaymentReconciliationDetail;
  TFhirPaymentReconciliationDetailList = fhir4_resources_financial.TFhirPaymentReconciliationDetailList;
  TFhirPaymentReconciliationProcessNote = fhir4_resources_financial.TFhirPaymentReconciliationProcessNote;
  TFhirPaymentReconciliationProcessNoteList = fhir4_resources_financial.TFhirPaymentReconciliationProcessNoteList;
  TFhirPaymentReconciliation = fhir4_resources_financial.TFhirPaymentReconciliation;
  TFhirPaymentReconciliationList = fhir4_resources_financial.TFhirPaymentReconciliationList;
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERSON}
  TFhirPersonLink = fhir4_resources_admin.TFhirPersonLink;
  TFhirPersonLinkList = fhir4_resources_admin.TFhirPersonLinkList;
  TFhirPerson = fhir4_resources_admin.TFhirPerson;
  TFhirPersonList = fhir4_resources_admin.TFhirPersonList;
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PLANDEFINITION}
  TFhirPlanDefinitionGoal = fhir4_resources_canonical.TFhirPlanDefinitionGoal;
  TFhirPlanDefinitionGoalList = fhir4_resources_canonical.TFhirPlanDefinitionGoalList;
  TFhirPlanDefinitionGoalTarget = fhir4_resources_canonical.TFhirPlanDefinitionGoalTarget;
  TFhirPlanDefinitionGoalTargetList = fhir4_resources_canonical.TFhirPlanDefinitionGoalTargetList;
  TFhirPlanDefinitionAction = fhir4_resources_canonical.TFhirPlanDefinitionAction;
  TFhirPlanDefinitionActionList = fhir4_resources_canonical.TFhirPlanDefinitionActionList;
  TFhirPlanDefinitionActionCondition = fhir4_resources_canonical.TFhirPlanDefinitionActionCondition;
  TFhirPlanDefinitionActionConditionList = fhir4_resources_canonical.TFhirPlanDefinitionActionConditionList;
  TFhirPlanDefinitionActionRelatedAction = fhir4_resources_canonical.TFhirPlanDefinitionActionRelatedAction;
  TFhirPlanDefinitionActionRelatedActionList = fhir4_resources_canonical.TFhirPlanDefinitionActionRelatedActionList;
  TFhirPlanDefinitionActionParticipant = fhir4_resources_canonical.TFhirPlanDefinitionActionParticipant;
  TFhirPlanDefinitionActionParticipantList = fhir4_resources_canonical.TFhirPlanDefinitionActionParticipantList;
  TFhirPlanDefinitionActionDynamicValue = fhir4_resources_canonical.TFhirPlanDefinitionActionDynamicValue;
  TFhirPlanDefinitionActionDynamicValueList = fhir4_resources_canonical.TFhirPlanDefinitionActionDynamicValueList;
  TFhirPlanDefinition = fhir4_resources_canonical.TFhirPlanDefinition;
  TFhirPlanDefinitionList = fhir4_resources_canonical.TFhirPlanDefinitionList;
{$ENDIF FHIR_PLANDEFINITION}
{$IFDEF FHIR_PRACTITIONER}
  TFhirPractitionerQualification = fhir4_resources_admin.TFhirPractitionerQualification;
  TFhirPractitionerQualificationList = fhir4_resources_admin.TFhirPractitionerQualificationList;
  TFhirPractitioner = fhir4_resources_admin.TFhirPractitioner;
  TFhirPractitionerList = fhir4_resources_admin.TFhirPractitionerList;
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PRACTITIONERROLE}
  TFhirPractitionerRoleAvailableTime = fhir4_resources_admin.TFhirPractitionerRoleAvailableTime;
  TFhirPractitionerRoleAvailableTimeList = fhir4_resources_admin.TFhirPractitionerRoleAvailableTimeList;
  TFhirPractitionerRoleNotAvailable = fhir4_resources_admin.TFhirPractitionerRoleNotAvailable;
  TFhirPractitionerRoleNotAvailableList = fhir4_resources_admin.TFhirPractitionerRoleNotAvailableList;
  TFhirPractitionerRole = fhir4_resources_admin.TFhirPractitionerRole;
  TFhirPractitionerRoleList = fhir4_resources_admin.TFhirPractitionerRoleList;
{$ENDIF FHIR_PRACTITIONERROLE}
{$IFDEF FHIR_PROCEDURE}
  TFhirProcedurePerformer = fhir4_resources_clinical.TFhirProcedurePerformer;
  TFhirProcedurePerformerList = fhir4_resources_clinical.TFhirProcedurePerformerList;
  TFhirProcedureFocalDevice = fhir4_resources_clinical.TFhirProcedureFocalDevice;
  TFhirProcedureFocalDeviceList = fhir4_resources_clinical.TFhirProcedureFocalDeviceList;
  TFhirProcedure = fhir4_resources_clinical.TFhirProcedure;
  TFhirProcedureList = fhir4_resources_clinical.TFhirProcedureList;
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROVENANCE}
  TFhirProvenanceAgent = fhir4_resources_other.TFhirProvenanceAgent;
  TFhirProvenanceAgentList = fhir4_resources_other.TFhirProvenanceAgentList;
  TFhirProvenanceEntity = fhir4_resources_other.TFhirProvenanceEntity;
  TFhirProvenanceEntityList = fhir4_resources_other.TFhirProvenanceEntityList;
  TFhirProvenance = fhir4_resources_other.TFhirProvenance;
  TFhirProvenanceList = fhir4_resources_other.TFhirProvenanceList;
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  TFhirQuestionnaireItem = fhir4_resources_canonical.TFhirQuestionnaireItem;
  TFhirQuestionnaireItemList = fhir4_resources_canonical.TFhirQuestionnaireItemList;
  TFhirQuestionnaireItemEnableWhen = fhir4_resources_canonical.TFhirQuestionnaireItemEnableWhen;
  TFhirQuestionnaireItemEnableWhenList = fhir4_resources_canonical.TFhirQuestionnaireItemEnableWhenList;
  TFhirQuestionnaireItemAnswerOption = fhir4_resources_canonical.TFhirQuestionnaireItemAnswerOption;
  TFhirQuestionnaireItemAnswerOptionList = fhir4_resources_canonical.TFhirQuestionnaireItemAnswerOptionList;
  TFhirQuestionnaireItemInitial = fhir4_resources_canonical.TFhirQuestionnaireItemInitial;
  TFhirQuestionnaireItemInitialList = fhir4_resources_canonical.TFhirQuestionnaireItemInitialList;
  TFhirQuestionnaire = fhir4_resources_canonical.TFhirQuestionnaire;
  TFhirQuestionnaireList = fhir4_resources_canonical.TFhirQuestionnaireList;
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  TFhirQuestionnaireResponseItem = fhir4_resources_clinical.TFhirQuestionnaireResponseItem;
  TFhirQuestionnaireResponseItemList = fhir4_resources_clinical.TFhirQuestionnaireResponseItemList;
  TFhirQuestionnaireResponseItemAnswer = fhir4_resources_clinical.TFhirQuestionnaireResponseItemAnswer;
  TFhirQuestionnaireResponseItemAnswerList = fhir4_resources_clinical.TFhirQuestionnaireResponseItemAnswerList;
  TFhirQuestionnaireResponse = fhir4_resources_clinical.TFhirQuestionnaireResponse;
  TFhirQuestionnaireResponseList = fhir4_resources_clinical.TFhirQuestionnaireResponseList;
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_RELATEDPERSON}
  TFhirRelatedPersonCommunication = fhir4_resources_admin.TFhirRelatedPersonCommunication;
  TFhirRelatedPersonCommunicationList = fhir4_resources_admin.TFhirRelatedPersonCommunicationList;
  TFhirRelatedPerson = fhir4_resources_admin.TFhirRelatedPerson;
  TFhirRelatedPersonList = fhir4_resources_admin.TFhirRelatedPersonList;
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_REQUESTGROUP}
  TFhirRequestGroupAction = fhir4_resources_other.TFhirRequestGroupAction;
  TFhirRequestGroupActionList = fhir4_resources_other.TFhirRequestGroupActionList;
  TFhirRequestGroupActionCondition = fhir4_resources_other.TFhirRequestGroupActionCondition;
  TFhirRequestGroupActionConditionList = fhir4_resources_other.TFhirRequestGroupActionConditionList;
  TFhirRequestGroupActionRelatedAction = fhir4_resources_other.TFhirRequestGroupActionRelatedAction;
  TFhirRequestGroupActionRelatedActionList = fhir4_resources_other.TFhirRequestGroupActionRelatedActionList;
  TFhirRequestGroup = fhir4_resources_other.TFhirRequestGroup;
  TFhirRequestGroupList = fhir4_resources_other.TFhirRequestGroupList;
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_RESEARCHDEFINITION}
  TFhirResearchDefinition = fhir4_resources_other.TFhirResearchDefinition;
  TFhirResearchDefinitionList = fhir4_resources_other.TFhirResearchDefinitionList;
{$ENDIF FHIR_RESEARCHDEFINITION}
{$IFDEF FHIR_RESEARCHELEMENTDEFINITION}
  TFhirResearchElementDefinitionCharacteristic = fhir4_resources_other.TFhirResearchElementDefinitionCharacteristic;
  TFhirResearchElementDefinitionCharacteristicList = fhir4_resources_other.TFhirResearchElementDefinitionCharacteristicList;
  TFhirResearchElementDefinition = fhir4_resources_other.TFhirResearchElementDefinition;
  TFhirResearchElementDefinitionList = fhir4_resources_other.TFhirResearchElementDefinitionList;
{$ENDIF FHIR_RESEARCHELEMENTDEFINITION}
{$IFDEF FHIR_RESEARCHSTUDY}
  TFhirResearchStudyArm = fhir4_resources_other.TFhirResearchStudyArm;
  TFhirResearchStudyArmList = fhir4_resources_other.TFhirResearchStudyArmList;
  TFhirResearchStudyObjective = fhir4_resources_other.TFhirResearchStudyObjective;
  TFhirResearchStudyObjectiveList = fhir4_resources_other.TFhirResearchStudyObjectiveList;
  TFhirResearchStudy = fhir4_resources_other.TFhirResearchStudy;
  TFhirResearchStudyList = fhir4_resources_other.TFhirResearchStudyList;
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  TFhirResearchSubject = fhir4_resources_other.TFhirResearchSubject;
  TFhirResearchSubjectList = fhir4_resources_other.TFhirResearchSubjectList;
{$ENDIF FHIR_RESEARCHSUBJECT}
{$IFDEF FHIR_RISKASSESSMENT}
  TFhirRiskAssessmentPrediction = fhir4_resources_clinical.TFhirRiskAssessmentPrediction;
  TFhirRiskAssessmentPredictionList = fhir4_resources_clinical.TFhirRiskAssessmentPredictionList;
  TFhirRiskAssessment = fhir4_resources_clinical.TFhirRiskAssessment;
  TFhirRiskAssessmentList = fhir4_resources_clinical.TFhirRiskAssessmentList;
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_RISKEVIDENCESYNTHESIS}
  TFhirRiskEvidenceSynthesisSampleSize = fhir4_resources_other.TFhirRiskEvidenceSynthesisSampleSize;
  TFhirRiskEvidenceSynthesisSampleSizeList = fhir4_resources_other.TFhirRiskEvidenceSynthesisSampleSizeList;
  TFhirRiskEvidenceSynthesisRiskEstimate = fhir4_resources_other.TFhirRiskEvidenceSynthesisRiskEstimate;
  TFhirRiskEvidenceSynthesisRiskEstimateList = fhir4_resources_other.TFhirRiskEvidenceSynthesisRiskEstimateList;
  TFhirRiskEvidenceSynthesisRiskEstimatePrecisionEstimate = fhir4_resources_other.TFhirRiskEvidenceSynthesisRiskEstimatePrecisionEstimate;
  TFhirRiskEvidenceSynthesisRiskEstimatePrecisionEstimateList = fhir4_resources_other.TFhirRiskEvidenceSynthesisRiskEstimatePrecisionEstimateList;
  TFhirRiskEvidenceSynthesisCertainty = fhir4_resources_other.TFhirRiskEvidenceSynthesisCertainty;
  TFhirRiskEvidenceSynthesisCertaintyList = fhir4_resources_other.TFhirRiskEvidenceSynthesisCertaintyList;
  TFhirRiskEvidenceSynthesisCertaintyCertaintySubcomponent = fhir4_resources_other.TFhirRiskEvidenceSynthesisCertaintyCertaintySubcomponent;
  TFhirRiskEvidenceSynthesisCertaintyCertaintySubcomponentList = fhir4_resources_other.TFhirRiskEvidenceSynthesisCertaintyCertaintySubcomponentList;
  TFhirRiskEvidenceSynthesis = fhir4_resources_other.TFhirRiskEvidenceSynthesis;
  TFhirRiskEvidenceSynthesisList = fhir4_resources_other.TFhirRiskEvidenceSynthesisList;
{$ENDIF FHIR_RISKEVIDENCESYNTHESIS}
{$IFDEF FHIR_SCHEDULE}
  TFhirSchedule = fhir4_resources_admin.TFhirSchedule;
  TFhirScheduleList = fhir4_resources_admin.TFhirScheduleList;
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SEARCHPARAMETER}
  TFhirSearchParameterComponent = fhir4_resources_canonical.TFhirSearchParameterComponent;
  TFhirSearchParameterComponentList = fhir4_resources_canonical.TFhirSearchParameterComponentList;
  TFhirSearchParameter = fhir4_resources_canonical.TFhirSearchParameter;
  TFhirSearchParameterList = fhir4_resources_canonical.TFhirSearchParameterList;
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SERVICEREQUEST}
  TFhirServiceRequest = fhir4_resources_canonical.TFhirServiceRequest;
  TFhirServiceRequestList = fhir4_resources_canonical.TFhirServiceRequestList;
{$ENDIF FHIR_SERVICEREQUEST}
{$IFDEF FHIR_SLOT}
  TFhirSlot = fhir4_resources_admin.TFhirSlot;
  TFhirSlotList = fhir4_resources_admin.TFhirSlotList;
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  TFhirSpecimenCollection = fhir4_resources_clinical.TFhirSpecimenCollection;
  TFhirSpecimenCollectionList = fhir4_resources_clinical.TFhirSpecimenCollectionList;
  TFhirSpecimenProcessing = fhir4_resources_clinical.TFhirSpecimenProcessing;
  TFhirSpecimenProcessingList = fhir4_resources_clinical.TFhirSpecimenProcessingList;
  TFhirSpecimenContainer = fhir4_resources_clinical.TFhirSpecimenContainer;
  TFhirSpecimenContainerList = fhir4_resources_clinical.TFhirSpecimenContainerList;
  TFhirSpecimen = fhir4_resources_clinical.TFhirSpecimen;
  TFhirSpecimenList = fhir4_resources_clinical.TFhirSpecimenList;
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_SPECIMENDEFINITION}
  TFhirSpecimenDefinitionTypeTested = fhir4_resources_canonical.TFhirSpecimenDefinitionTypeTested;
  TFhirSpecimenDefinitionTypeTestedList = fhir4_resources_canonical.TFhirSpecimenDefinitionTypeTestedList;
  TFhirSpecimenDefinitionTypeTestedContainer = fhir4_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainer;
  TFhirSpecimenDefinitionTypeTestedContainerList = fhir4_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainerList;
  TFhirSpecimenDefinitionTypeTestedContainerAdditive = fhir4_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainerAdditive;
  TFhirSpecimenDefinitionTypeTestedContainerAdditiveList = fhir4_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainerAdditiveList;
  TFhirSpecimenDefinitionTypeTestedHandling = fhir4_resources_canonical.TFhirSpecimenDefinitionTypeTestedHandling;
  TFhirSpecimenDefinitionTypeTestedHandlingList = fhir4_resources_canonical.TFhirSpecimenDefinitionTypeTestedHandlingList;
  TFhirSpecimenDefinition = fhir4_resources_canonical.TFhirSpecimenDefinition;
  TFhirSpecimenDefinitionList = fhir4_resources_canonical.TFhirSpecimenDefinitionList;
{$ENDIF FHIR_SPECIMENDEFINITION}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  TFhirStructureDefinitionMapping = fhir4_resources_canonical.TFhirStructureDefinitionMapping;
  TFhirStructureDefinitionMappingList = fhir4_resources_canonical.TFhirStructureDefinitionMappingList;
  TFhirStructureDefinitionContext = fhir4_resources_canonical.TFhirStructureDefinitionContext;
  TFhirStructureDefinitionContextList = fhir4_resources_canonical.TFhirStructureDefinitionContextList;
  TFhirStructureDefinitionSnapshot = fhir4_resources_canonical.TFhirStructureDefinitionSnapshot;
  TFhirStructureDefinitionSnapshotList = fhir4_resources_canonical.TFhirStructureDefinitionSnapshotList;
  TFhirStructureDefinitionDifferential = fhir4_resources_canonical.TFhirStructureDefinitionDifferential;
  TFhirStructureDefinitionDifferentialList = fhir4_resources_canonical.TFhirStructureDefinitionDifferentialList;
  TFhirStructureDefinition = fhir4_resources_canonical.TFhirStructureDefinition;
  TFhirStructureDefinitionList = fhir4_resources_canonical.TFhirStructureDefinitionList;
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_STRUCTUREMAP}
  TFhirStructureMapStructure = fhir4_resources_canonical.TFhirStructureMapStructure;
  TFhirStructureMapStructureList = fhir4_resources_canonical.TFhirStructureMapStructureList;
  TFhirStructureMapGroup = fhir4_resources_canonical.TFhirStructureMapGroup;
  TFhirStructureMapGroupList = fhir4_resources_canonical.TFhirStructureMapGroupList;
  TFhirStructureMapGroupInput = fhir4_resources_canonical.TFhirStructureMapGroupInput;
  TFhirStructureMapGroupInputList = fhir4_resources_canonical.TFhirStructureMapGroupInputList;
  TFhirStructureMapGroupRule = fhir4_resources_canonical.TFhirStructureMapGroupRule;
  TFhirStructureMapGroupRuleList = fhir4_resources_canonical.TFhirStructureMapGroupRuleList;
  TFhirStructureMapGroupRuleSource = fhir4_resources_canonical.TFhirStructureMapGroupRuleSource;
  TFhirStructureMapGroupRuleSourceList = fhir4_resources_canonical.TFhirStructureMapGroupRuleSourceList;
  TFhirStructureMapGroupRuleTarget = fhir4_resources_canonical.TFhirStructureMapGroupRuleTarget;
  TFhirStructureMapGroupRuleTargetList = fhir4_resources_canonical.TFhirStructureMapGroupRuleTargetList;
  TFhirStructureMapGroupRuleTargetParameter = fhir4_resources_canonical.TFhirStructureMapGroupRuleTargetParameter;
  TFhirStructureMapGroupRuleTargetParameterList = fhir4_resources_canonical.TFhirStructureMapGroupRuleTargetParameterList;
  TFhirStructureMapGroupRuleDependent = fhir4_resources_canonical.TFhirStructureMapGroupRuleDependent;
  TFhirStructureMapGroupRuleDependentList = fhir4_resources_canonical.TFhirStructureMapGroupRuleDependentList;
  TFhirStructureMap = fhir4_resources_canonical.TFhirStructureMap;
  TFhirStructureMapList = fhir4_resources_canonical.TFhirStructureMapList;
{$ENDIF FHIR_STRUCTUREMAP}
{$IFDEF FHIR_SUBSCRIPTION}
  TFhirSubscriptionChannel = fhir4_resources_other.TFhirSubscriptionChannel;
  TFhirSubscriptionChannelList = fhir4_resources_other.TFhirSubscriptionChannelList;
  TFhirSubscription = fhir4_resources_other.TFhirSubscription;
  TFhirSubscriptionList = fhir4_resources_other.TFhirSubscriptionList;
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSTANCE}
  TFhirSubstanceInstance = fhir4_resources_medications.TFhirSubstanceInstance;
  TFhirSubstanceInstanceList = fhir4_resources_medications.TFhirSubstanceInstanceList;
  TFhirSubstanceIngredient = fhir4_resources_medications.TFhirSubstanceIngredient;
  TFhirSubstanceIngredientList = fhir4_resources_medications.TFhirSubstanceIngredientList;
  TFhirSubstance = fhir4_resources_medications.TFhirSubstance;
  TFhirSubstanceList = fhir4_resources_medications.TFhirSubstanceList;
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUBSTANCENUCLEICACID}
  TFhirSubstanceNucleicAcidSubunit = fhir4_resources_medications.TFhirSubstanceNucleicAcidSubunit;
  TFhirSubstanceNucleicAcidSubunitList = fhir4_resources_medications.TFhirSubstanceNucleicAcidSubunitList;
  TFhirSubstanceNucleicAcidSubunitLinkage = fhir4_resources_medications.TFhirSubstanceNucleicAcidSubunitLinkage;
  TFhirSubstanceNucleicAcidSubunitLinkageList = fhir4_resources_medications.TFhirSubstanceNucleicAcidSubunitLinkageList;
  TFhirSubstanceNucleicAcidSubunitSugar = fhir4_resources_medications.TFhirSubstanceNucleicAcidSubunitSugar;
  TFhirSubstanceNucleicAcidSubunitSugarList = fhir4_resources_medications.TFhirSubstanceNucleicAcidSubunitSugarList;
  TFhirSubstanceNucleicAcid = fhir4_resources_medications.TFhirSubstanceNucleicAcid;
  TFhirSubstanceNucleicAcidList = fhir4_resources_medications.TFhirSubstanceNucleicAcidList;
{$ENDIF FHIR_SUBSTANCENUCLEICACID}
{$IFDEF FHIR_SUBSTANCEPOLYMER}
  TFhirSubstancePolymerMonomerSet = fhir4_resources_medications.TFhirSubstancePolymerMonomerSet;
  TFhirSubstancePolymerMonomerSetList = fhir4_resources_medications.TFhirSubstancePolymerMonomerSetList;
  TFhirSubstancePolymerMonomerSetStartingMaterial = fhir4_resources_medications.TFhirSubstancePolymerMonomerSetStartingMaterial;
  TFhirSubstancePolymerMonomerSetStartingMaterialList = fhir4_resources_medications.TFhirSubstancePolymerMonomerSetStartingMaterialList;
  TFhirSubstancePolymerRepeat = fhir4_resources_medications.TFhirSubstancePolymerRepeat;
  TFhirSubstancePolymerRepeatList = fhir4_resources_medications.TFhirSubstancePolymerRepeatList;
  TFhirSubstancePolymerRepeatRepeatUnit = fhir4_resources_medications.TFhirSubstancePolymerRepeatRepeatUnit;
  TFhirSubstancePolymerRepeatRepeatUnitList = fhir4_resources_medications.TFhirSubstancePolymerRepeatRepeatUnitList;
  TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation = fhir4_resources_medications.TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation;
  TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationList = fhir4_resources_medications.TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationList;
  TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentation = fhir4_resources_medications.TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentation;
  TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentationList = fhir4_resources_medications.TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentationList;
  TFhirSubstancePolymer = fhir4_resources_medications.TFhirSubstancePolymer;
  TFhirSubstancePolymerList = fhir4_resources_medications.TFhirSubstancePolymerList;
{$ENDIF FHIR_SUBSTANCEPOLYMER}
{$IFDEF FHIR_SUBSTANCEPROTEIN}
  TFhirSubstanceProteinSubunit = fhir4_resources_medications.TFhirSubstanceProteinSubunit;
  TFhirSubstanceProteinSubunitList = fhir4_resources_medications.TFhirSubstanceProteinSubunitList;
  TFhirSubstanceProtein = fhir4_resources_medications.TFhirSubstanceProtein;
  TFhirSubstanceProteinList = fhir4_resources_medications.TFhirSubstanceProteinList;
{$ENDIF FHIR_SUBSTANCEPROTEIN}
{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  TFhirSubstanceReferenceInformationGene = fhir4_resources_medications.TFhirSubstanceReferenceInformationGene;
  TFhirSubstanceReferenceInformationGeneList = fhir4_resources_medications.TFhirSubstanceReferenceInformationGeneList;
  TFhirSubstanceReferenceInformationGeneElement = fhir4_resources_medications.TFhirSubstanceReferenceInformationGeneElement;
  TFhirSubstanceReferenceInformationGeneElementList = fhir4_resources_medications.TFhirSubstanceReferenceInformationGeneElementList;
  TFhirSubstanceReferenceInformationClassification = fhir4_resources_medications.TFhirSubstanceReferenceInformationClassification;
  TFhirSubstanceReferenceInformationClassificationList = fhir4_resources_medications.TFhirSubstanceReferenceInformationClassificationList;
  TFhirSubstanceReferenceInformationTarget = fhir4_resources_medications.TFhirSubstanceReferenceInformationTarget;
  TFhirSubstanceReferenceInformationTargetList = fhir4_resources_medications.TFhirSubstanceReferenceInformationTargetList;
  TFhirSubstanceReferenceInformation = fhir4_resources_medications.TFhirSubstanceReferenceInformation;
  TFhirSubstanceReferenceInformationList = fhir4_resources_medications.TFhirSubstanceReferenceInformationList;
{$ENDIF FHIR_SUBSTANCEREFERENCEINFORMATION}
{$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
  TFhirSubstanceSourceMaterialFractionDescription = fhir4_resources_medications.TFhirSubstanceSourceMaterialFractionDescription;
  TFhirSubstanceSourceMaterialFractionDescriptionList = fhir4_resources_medications.TFhirSubstanceSourceMaterialFractionDescriptionList;
  TFhirSubstanceSourceMaterialOrganism = fhir4_resources_medications.TFhirSubstanceSourceMaterialOrganism;
  TFhirSubstanceSourceMaterialOrganismList = fhir4_resources_medications.TFhirSubstanceSourceMaterialOrganismList;
  TFhirSubstanceSourceMaterialOrganismAuthor = fhir4_resources_medications.TFhirSubstanceSourceMaterialOrganismAuthor;
  TFhirSubstanceSourceMaterialOrganismAuthorList = fhir4_resources_medications.TFhirSubstanceSourceMaterialOrganismAuthorList;
  TFhirSubstanceSourceMaterialOrganismHybrid = fhir4_resources_medications.TFhirSubstanceSourceMaterialOrganismHybrid;
  TFhirSubstanceSourceMaterialOrganismHybridList = fhir4_resources_medications.TFhirSubstanceSourceMaterialOrganismHybridList;
  TFhirSubstanceSourceMaterialOrganismOrganismGeneral = fhir4_resources_medications.TFhirSubstanceSourceMaterialOrganismOrganismGeneral;
  TFhirSubstanceSourceMaterialOrganismOrganismGeneralList = fhir4_resources_medications.TFhirSubstanceSourceMaterialOrganismOrganismGeneralList;
  TFhirSubstanceSourceMaterialPartDescription = fhir4_resources_medications.TFhirSubstanceSourceMaterialPartDescription;
  TFhirSubstanceSourceMaterialPartDescriptionList = fhir4_resources_medications.TFhirSubstanceSourceMaterialPartDescriptionList;
  TFhirSubstanceSourceMaterial = fhir4_resources_medications.TFhirSubstanceSourceMaterial;
  TFhirSubstanceSourceMaterialList = fhir4_resources_medications.TFhirSubstanceSourceMaterialList;
{$ENDIF FHIR_SUBSTANCESOURCEMATERIAL}
{$IFDEF FHIR_SUBSTANCESPECIFICATION}
  TFhirSubstanceSpecificationMoiety = fhir4_resources_medications.TFhirSubstanceSpecificationMoiety;
  TFhirSubstanceSpecificationMoietyList = fhir4_resources_medications.TFhirSubstanceSpecificationMoietyList;
  TFhirSubstanceSpecificationProperty = fhir4_resources_medications.TFhirSubstanceSpecificationProperty;
  TFhirSubstanceSpecificationPropertyList = fhir4_resources_medications.TFhirSubstanceSpecificationPropertyList;
  TFhirSubstanceSpecificationStructure = fhir4_resources_medications.TFhirSubstanceSpecificationStructure;
  TFhirSubstanceSpecificationStructureList = fhir4_resources_medications.TFhirSubstanceSpecificationStructureList;
  TFhirSubstanceSpecificationStructureIsotope = fhir4_resources_medications.TFhirSubstanceSpecificationStructureIsotope;
  TFhirSubstanceSpecificationStructureIsotopeList = fhir4_resources_medications.TFhirSubstanceSpecificationStructureIsotopeList;
  TFhirSubstanceSpecificationStructureIsotopeMolecularWeight = fhir4_resources_medications.TFhirSubstanceSpecificationStructureIsotopeMolecularWeight;
  TFhirSubstanceSpecificationStructureIsotopeMolecularWeightList = fhir4_resources_medications.TFhirSubstanceSpecificationStructureIsotopeMolecularWeightList;
  TFhirSubstanceSpecificationStructureRepresentation = fhir4_resources_medications.TFhirSubstanceSpecificationStructureRepresentation;
  TFhirSubstanceSpecificationStructureRepresentationList = fhir4_resources_medications.TFhirSubstanceSpecificationStructureRepresentationList;
  TFhirSubstanceSpecificationCode = fhir4_resources_medications.TFhirSubstanceSpecificationCode;
  TFhirSubstanceSpecificationCodeList = fhir4_resources_medications.TFhirSubstanceSpecificationCodeList;
  TFhirSubstanceSpecificationName = fhir4_resources_medications.TFhirSubstanceSpecificationName;
  TFhirSubstanceSpecificationNameList = fhir4_resources_medications.TFhirSubstanceSpecificationNameList;
  TFhirSubstanceSpecificationNameOfficial = fhir4_resources_medications.TFhirSubstanceSpecificationNameOfficial;
  TFhirSubstanceSpecificationNameOfficialList = fhir4_resources_medications.TFhirSubstanceSpecificationNameOfficialList;
  TFhirSubstanceSpecificationRelationship = fhir4_resources_medications.TFhirSubstanceSpecificationRelationship;
  TFhirSubstanceSpecificationRelationshipList = fhir4_resources_medications.TFhirSubstanceSpecificationRelationshipList;
  TFhirSubstanceSpecification = fhir4_resources_medications.TFhirSubstanceSpecification;
  TFhirSubstanceSpecificationList = fhir4_resources_medications.TFhirSubstanceSpecificationList;
{$ENDIF FHIR_SUBSTANCESPECIFICATION}
{$IFDEF FHIR_SUPPLYDELIVERY}
  TFhirSupplyDeliverySuppliedItem = fhir4_resources_clinical.TFhirSupplyDeliverySuppliedItem;
  TFhirSupplyDeliverySuppliedItemList = fhir4_resources_clinical.TFhirSupplyDeliverySuppliedItemList;
  TFhirSupplyDelivery = fhir4_resources_clinical.TFhirSupplyDelivery;
  TFhirSupplyDeliveryList = fhir4_resources_clinical.TFhirSupplyDeliveryList;
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  TFhirSupplyRequestParameter = fhir4_resources_clinical.TFhirSupplyRequestParameter;
  TFhirSupplyRequestParameterList = fhir4_resources_clinical.TFhirSupplyRequestParameterList;
  TFhirSupplyRequest = fhir4_resources_clinical.TFhirSupplyRequest;
  TFhirSupplyRequestList = fhir4_resources_clinical.TFhirSupplyRequestList;
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TASK}
  TFhirTaskRestriction = fhir4_resources_other.TFhirTaskRestriction;
  TFhirTaskRestrictionList = fhir4_resources_other.TFhirTaskRestrictionList;
  TFhirTaskInput = fhir4_resources_other.TFhirTaskInput;
  TFhirTaskInputList = fhir4_resources_other.TFhirTaskInputList;
  TFhirTaskOutput = fhir4_resources_other.TFhirTaskOutput;
  TFhirTaskOutputList = fhir4_resources_other.TFhirTaskOutputList;
  TFhirTask = fhir4_resources_other.TFhirTask;
  TFhirTaskList = fhir4_resources_other.TFhirTaskList;
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  TFhirTerminologyCapabilitiesSoftware = fhir4_resources_canonical.TFhirTerminologyCapabilitiesSoftware;
  TFhirTerminologyCapabilitiesSoftwareList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesSoftwareList;
  TFhirTerminologyCapabilitiesImplementation = fhir4_resources_canonical.TFhirTerminologyCapabilitiesImplementation;
  TFhirTerminologyCapabilitiesImplementationList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesImplementationList;
  TFhirTerminologyCapabilitiesCodeSystem = fhir4_resources_canonical.TFhirTerminologyCapabilitiesCodeSystem;
  TFhirTerminologyCapabilitiesCodeSystemList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemList;
  TFhirTerminologyCapabilitiesCodeSystemVersion = fhir4_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersion;
  TFhirTerminologyCapabilitiesCodeSystemVersionList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersionList;
  TFhirTerminologyCapabilitiesCodeSystemVersionFilter = fhir4_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersionFilter;
  TFhirTerminologyCapabilitiesCodeSystemVersionFilterList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersionFilterList;
  TFhirTerminologyCapabilitiesExpansion = fhir4_resources_canonical.TFhirTerminologyCapabilitiesExpansion;
  TFhirTerminologyCapabilitiesExpansionList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesExpansionList;
  TFhirTerminologyCapabilitiesExpansionParameter = fhir4_resources_canonical.TFhirTerminologyCapabilitiesExpansionParameter;
  TFhirTerminologyCapabilitiesExpansionParameterList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesExpansionParameterList;
  TFhirTerminologyCapabilitiesValidateCode = fhir4_resources_canonical.TFhirTerminologyCapabilitiesValidateCode;
  TFhirTerminologyCapabilitiesValidateCodeList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesValidateCodeList;
  TFhirTerminologyCapabilitiesTranslation = fhir4_resources_canonical.TFhirTerminologyCapabilitiesTranslation;
  TFhirTerminologyCapabilitiesTranslationList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesTranslationList;
  TFhirTerminologyCapabilitiesClosure = fhir4_resources_canonical.TFhirTerminologyCapabilitiesClosure;
  TFhirTerminologyCapabilitiesClosureList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesClosureList;
  TFhirTerminologyCapabilities = fhir4_resources_canonical.TFhirTerminologyCapabilities;
  TFhirTerminologyCapabilitiesList = fhir4_resources_canonical.TFhirTerminologyCapabilitiesList;
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}
{$IFDEF FHIR_TESTREPORT}
  TFhirTestReportParticipant = fhir4_resources_other.TFhirTestReportParticipant;
  TFhirTestReportParticipantList = fhir4_resources_other.TFhirTestReportParticipantList;
  TFhirTestReportSetup = fhir4_resources_other.TFhirTestReportSetup;
  TFhirTestReportSetupList = fhir4_resources_other.TFhirTestReportSetupList;
  TFhirTestReportSetupAction = fhir4_resources_other.TFhirTestReportSetupAction;
  TFhirTestReportSetupActionList = fhir4_resources_other.TFhirTestReportSetupActionList;
  TFhirTestReportSetupActionOperation = fhir4_resources_other.TFhirTestReportSetupActionOperation;
  TFhirTestReportSetupActionOperationList = fhir4_resources_other.TFhirTestReportSetupActionOperationList;
  TFhirTestReportSetupActionAssert = fhir4_resources_other.TFhirTestReportSetupActionAssert;
  TFhirTestReportSetupActionAssertList = fhir4_resources_other.TFhirTestReportSetupActionAssertList;
  TFhirTestReportTest = fhir4_resources_other.TFhirTestReportTest;
  TFhirTestReportTestList = fhir4_resources_other.TFhirTestReportTestList;
  TFhirTestReportTestAction = fhir4_resources_other.TFhirTestReportTestAction;
  TFhirTestReportTestActionList = fhir4_resources_other.TFhirTestReportTestActionList;
  TFhirTestReportTeardown = fhir4_resources_other.TFhirTestReportTeardown;
  TFhirTestReportTeardownList = fhir4_resources_other.TFhirTestReportTeardownList;
  TFhirTestReportTeardownAction = fhir4_resources_other.TFhirTestReportTeardownAction;
  TFhirTestReportTeardownActionList = fhir4_resources_other.TFhirTestReportTeardownActionList;
  TFhirTestReport = fhir4_resources_other.TFhirTestReport;
  TFhirTestReportList = fhir4_resources_other.TFhirTestReportList;
{$ENDIF FHIR_TESTREPORT}
{$IFDEF FHIR_TESTSCRIPT}
  TFhirTestScriptOrigin = fhir4_resources_canonical.TFhirTestScriptOrigin;
  TFhirTestScriptOriginList = fhir4_resources_canonical.TFhirTestScriptOriginList;
  TFhirTestScriptDestination = fhir4_resources_canonical.TFhirTestScriptDestination;
  TFhirTestScriptDestinationList = fhir4_resources_canonical.TFhirTestScriptDestinationList;
  TFhirTestScriptMetadata = fhir4_resources_canonical.TFhirTestScriptMetadata;
  TFhirTestScriptMetadataList = fhir4_resources_canonical.TFhirTestScriptMetadataList;
  TFhirTestScriptMetadataLink = fhir4_resources_canonical.TFhirTestScriptMetadataLink;
  TFhirTestScriptMetadataLinkList = fhir4_resources_canonical.TFhirTestScriptMetadataLinkList;
  TFhirTestScriptMetadataCapability = fhir4_resources_canonical.TFhirTestScriptMetadataCapability;
  TFhirTestScriptMetadataCapabilityList = fhir4_resources_canonical.TFhirTestScriptMetadataCapabilityList;
  TFhirTestScriptFixture = fhir4_resources_canonical.TFhirTestScriptFixture;
  TFhirTestScriptFixtureList = fhir4_resources_canonical.TFhirTestScriptFixtureList;
  TFhirTestScriptVariable = fhir4_resources_canonical.TFhirTestScriptVariable;
  TFhirTestScriptVariableList = fhir4_resources_canonical.TFhirTestScriptVariableList;
  TFhirTestScriptSetup = fhir4_resources_canonical.TFhirTestScriptSetup;
  TFhirTestScriptSetupList = fhir4_resources_canonical.TFhirTestScriptSetupList;
  TFhirTestScriptSetupAction = fhir4_resources_canonical.TFhirTestScriptSetupAction;
  TFhirTestScriptSetupActionList = fhir4_resources_canonical.TFhirTestScriptSetupActionList;
  TFhirTestScriptSetupActionOperation = fhir4_resources_canonical.TFhirTestScriptSetupActionOperation;
  TFhirTestScriptSetupActionOperationList = fhir4_resources_canonical.TFhirTestScriptSetupActionOperationList;
  TFhirTestScriptSetupActionOperationRequestHeader = fhir4_resources_canonical.TFhirTestScriptSetupActionOperationRequestHeader;
  TFhirTestScriptSetupActionOperationRequestHeaderList = fhir4_resources_canonical.TFhirTestScriptSetupActionOperationRequestHeaderList;
  TFhirTestScriptSetupActionAssert = fhir4_resources_canonical.TFhirTestScriptSetupActionAssert;
  TFhirTestScriptSetupActionAssertList = fhir4_resources_canonical.TFhirTestScriptSetupActionAssertList;
  TFhirTestScriptTest = fhir4_resources_canonical.TFhirTestScriptTest;
  TFhirTestScriptTestList = fhir4_resources_canonical.TFhirTestScriptTestList;
  TFhirTestScriptTestAction = fhir4_resources_canonical.TFhirTestScriptTestAction;
  TFhirTestScriptTestActionList = fhir4_resources_canonical.TFhirTestScriptTestActionList;
  TFhirTestScriptTeardown = fhir4_resources_canonical.TFhirTestScriptTeardown;
  TFhirTestScriptTeardownList = fhir4_resources_canonical.TFhirTestScriptTeardownList;
  TFhirTestScriptTeardownAction = fhir4_resources_canonical.TFhirTestScriptTeardownAction;
  TFhirTestScriptTeardownActionList = fhir4_resources_canonical.TFhirTestScriptTeardownActionList;
  TFhirTestScript = fhir4_resources_canonical.TFhirTestScript;
  TFhirTestScriptList = fhir4_resources_canonical.TFhirTestScriptList;
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  TFhirValueSetCompose = fhir4_resources_canonical.TFhirValueSetCompose;
  TFhirValueSetComposeList = fhir4_resources_canonical.TFhirValueSetComposeList;
  TFhirValueSetComposeInclude = fhir4_resources_canonical.TFhirValueSetComposeInclude;
  TFhirValueSetComposeIncludeList = fhir4_resources_canonical.TFhirValueSetComposeIncludeList;
  TFhirValueSetComposeIncludeConcept = fhir4_resources_canonical.TFhirValueSetComposeIncludeConcept;
  TFhirValueSetComposeIncludeConceptList = fhir4_resources_canonical.TFhirValueSetComposeIncludeConceptList;
  TFhirValueSetComposeIncludeConceptDesignation = fhir4_resources_canonical.TFhirValueSetComposeIncludeConceptDesignation;
  TFhirValueSetComposeIncludeConceptDesignationList = fhir4_resources_canonical.TFhirValueSetComposeIncludeConceptDesignationList;
  TFhirValueSetComposeIncludeFilter = fhir4_resources_canonical.TFhirValueSetComposeIncludeFilter;
  TFhirValueSetComposeIncludeFilterList = fhir4_resources_canonical.TFhirValueSetComposeIncludeFilterList;
  TFhirValueSetExpansion = fhir4_resources_canonical.TFhirValueSetExpansion;
  TFhirValueSetExpansionList = fhir4_resources_canonical.TFhirValueSetExpansionList;
  TFhirValueSetExpansionParameter = fhir4_resources_canonical.TFhirValueSetExpansionParameter;
  TFhirValueSetExpansionParameterList = fhir4_resources_canonical.TFhirValueSetExpansionParameterList;
  TFhirValueSetExpansionContains = fhir4_resources_canonical.TFhirValueSetExpansionContains;
  TFhirValueSetExpansionContainsList = fhir4_resources_canonical.TFhirValueSetExpansionContainsList;
  TFhirValueSet = fhir4_resources_canonical.TFhirValueSet;
  TFhirValueSetList = fhir4_resources_canonical.TFhirValueSetList;
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VERIFICATIONRESULT}
  TFhirVerificationResultPrimarySource = fhir4_resources_other.TFhirVerificationResultPrimarySource;
  TFhirVerificationResultPrimarySourceList = fhir4_resources_other.TFhirVerificationResultPrimarySourceList;
  TFhirVerificationResultAttestation = fhir4_resources_other.TFhirVerificationResultAttestation;
  TFhirVerificationResultAttestationList = fhir4_resources_other.TFhirVerificationResultAttestationList;
  TFhirVerificationResultValidator = fhir4_resources_other.TFhirVerificationResultValidator;
  TFhirVerificationResultValidatorList = fhir4_resources_other.TFhirVerificationResultValidatorList;
  TFhirVerificationResult = fhir4_resources_other.TFhirVerificationResult;
  TFhirVerificationResultList = fhir4_resources_other.TFhirVerificationResultList;
{$ENDIF FHIR_VERIFICATIONRESULT}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  TFhirVisionPrescriptionLensSpecification = fhir4_resources_clinical.TFhirVisionPrescriptionLensSpecification;
  TFhirVisionPrescriptionLensSpecificationList = fhir4_resources_clinical.TFhirVisionPrescriptionLensSpecificationList;
  TFhirVisionPrescriptionLensSpecificationPrism = fhir4_resources_clinical.TFhirVisionPrescriptionLensSpecificationPrism;
  TFhirVisionPrescriptionLensSpecificationPrismList = fhir4_resources_clinical.TFhirVisionPrescriptionLensSpecificationPrismList;
  TFhirVisionPrescription = fhir4_resources_clinical.TFhirVisionPrescription;
  TFhirVisionPrescriptionList = fhir4_resources_clinical.TFhirVisionPrescriptionList;
{$ENDIF FHIR_VISIONPRESCRIPTION}

implementation

end.

