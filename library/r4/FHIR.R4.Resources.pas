unit FHIR.R4.Resources;

{$I FHIR.R4.inc}

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
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Utilities, FHIR.Base.Lang,
  FHIR.R4.Base, FHIR.R4.Types, FHIR.R4.Resources.Base,
  FHIR.R4.Resources.Admin, FHIR.R4.Resources.Canonical, FHIR.R4.Resources.Clinical, FHIR.R4.Resources.Financial, FHIR.R4.Resources.Medications, FHIR.R4.Resources.Other;

Type
  TFhirResourceType = FHIR.R4.Resources.Base.TFhirResourceType;
  TFhirResourceTypeSet = FHIR.R4.Resources.Base.TFhirResourceTypeSet;
  TFhirResource = FHIR.R4.Resources.Base.TFhirResource;
  TFhirResourceList = FHIR.R4.Resources.Base.TFhirResourceList;
  TFhirDomainResource = FHIR.R4.Resources.Base.TFhirDomainResource;
  TFhirDomainResourceList = FHIR.R4.Resources.Base.TFhirDomainResourceList;
  TFhirMetadataResource = FHIR.R4.Resources.Canonical.TFhirMetadataResource;
  TFhirMetadataResourceList = FHIR.R4.Resources.Canonical.TFhirMetadataResourceList;

{$IFDEF FHIR_PARAMETERS}
  TFhirParametersParameter = FHIR.R4.Resources.Other.TFhirParametersParameter;
  TFhirParametersParameterList = FHIR.R4.Resources.Other.TFhirParametersParameterList;
  TFhirParameters = FHIR.R4.Resources.Other.TFhirParameters;
  TFhirParametersList = FHIR.R4.Resources.Other.TFhirParametersList;
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_ACCOUNT}
  TFhirAccountCoverage = FHIR.R4.Resources.Clinical.TFhirAccountCoverage;
  TFhirAccountCoverageList = FHIR.R4.Resources.Clinical.TFhirAccountCoverageList;
  TFhirAccountGuarantor = FHIR.R4.Resources.Clinical.TFhirAccountGuarantor;
  TFhirAccountGuarantorList = FHIR.R4.Resources.Clinical.TFhirAccountGuarantorList;
  TFhirAccount = FHIR.R4.Resources.Clinical.TFhirAccount;
  TFhirAccountList = FHIR.R4.Resources.Clinical.TFhirAccountList;
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  TFhirActivityDefinitionParticipant = FHIR.R4.Resources.Canonical.TFhirActivityDefinitionParticipant;
  TFhirActivityDefinitionParticipantList = FHIR.R4.Resources.Canonical.TFhirActivityDefinitionParticipantList;
  TFhirActivityDefinitionDynamicValue = FHIR.R4.Resources.Canonical.TFhirActivityDefinitionDynamicValue;
  TFhirActivityDefinitionDynamicValueList = FHIR.R4.Resources.Canonical.TFhirActivityDefinitionDynamicValueList;
  TFhirActivityDefinition = FHIR.R4.Resources.Canonical.TFhirActivityDefinition;
  TFhirActivityDefinitionList = FHIR.R4.Resources.Canonical.TFhirActivityDefinitionList;
{$ENDIF FHIR_ACTIVITYDEFINITION}
{$IFDEF FHIR_ADVERSEEVENT}
  TFhirAdverseEventSuspectEntity = FHIR.R4.Resources.Clinical.TFhirAdverseEventSuspectEntity;
  TFhirAdverseEventSuspectEntityList = FHIR.R4.Resources.Clinical.TFhirAdverseEventSuspectEntityList;
  TFhirAdverseEventSuspectEntityCausality = FHIR.R4.Resources.Clinical.TFhirAdverseEventSuspectEntityCausality;
  TFhirAdverseEventSuspectEntityCausalityList = FHIR.R4.Resources.Clinical.TFhirAdverseEventSuspectEntityCausalityList;
  TFhirAdverseEvent = FHIR.R4.Resources.Clinical.TFhirAdverseEvent;
  TFhirAdverseEventList = FHIR.R4.Resources.Clinical.TFhirAdverseEventList;
{$ENDIF FHIR_ADVERSEEVENT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  TFhirAllergyIntoleranceReaction = FHIR.R4.Resources.Clinical.TFhirAllergyIntoleranceReaction;
  TFhirAllergyIntoleranceReactionList = FHIR.R4.Resources.Clinical.TFhirAllergyIntoleranceReactionList;
  TFhirAllergyIntolerance = FHIR.R4.Resources.Clinical.TFhirAllergyIntolerance;
  TFhirAllergyIntoleranceList = FHIR.R4.Resources.Clinical.TFhirAllergyIntoleranceList;
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  TFhirAppointmentParticipant = FHIR.R4.Resources.Clinical.TFhirAppointmentParticipant;
  TFhirAppointmentParticipantList = FHIR.R4.Resources.Clinical.TFhirAppointmentParticipantList;
  TFhirAppointment = FHIR.R4.Resources.Clinical.TFhirAppointment;
  TFhirAppointmentList = FHIR.R4.Resources.Clinical.TFhirAppointmentList;
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  TFhirAppointmentResponse = FHIR.R4.Resources.Clinical.TFhirAppointmentResponse;
  TFhirAppointmentResponseList = FHIR.R4.Resources.Clinical.TFhirAppointmentResponseList;
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_AUDITEVENT}
  TFhirAuditEventAgent = FHIR.R4.Resources.Other.TFhirAuditEventAgent;
  TFhirAuditEventAgentList = FHIR.R4.Resources.Other.TFhirAuditEventAgentList;
  TFhirAuditEventAgentNetwork = FHIR.R4.Resources.Other.TFhirAuditEventAgentNetwork;
  TFhirAuditEventAgentNetworkList = FHIR.R4.Resources.Other.TFhirAuditEventAgentNetworkList;
  TFhirAuditEventSource = FHIR.R4.Resources.Other.TFhirAuditEventSource;
  TFhirAuditEventSourceList = FHIR.R4.Resources.Other.TFhirAuditEventSourceList;
  TFhirAuditEventEntity = FHIR.R4.Resources.Other.TFhirAuditEventEntity;
  TFhirAuditEventEntityList = FHIR.R4.Resources.Other.TFhirAuditEventEntityList;
  TFhirAuditEventEntityDetail = FHIR.R4.Resources.Other.TFhirAuditEventEntityDetail;
  TFhirAuditEventEntityDetailList = FHIR.R4.Resources.Other.TFhirAuditEventEntityDetailList;
  TFhirAuditEvent = FHIR.R4.Resources.Other.TFhirAuditEvent;
  TFhirAuditEventList = FHIR.R4.Resources.Other.TFhirAuditEventList;
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BASIC}
  TFhirBasic = FHIR.R4.Resources.Clinical.TFhirBasic;
  TFhirBasicList = FHIR.R4.Resources.Clinical.TFhirBasicList;
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BINARY}
  TFhirBinary = FHIR.R4.Resources.Other.TFhirBinary;
  TFhirBinaryList = FHIR.R4.Resources.Other.TFhirBinaryList;
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  TFhirBiologicallyDerivedProductCollection = FHIR.R4.Resources.Admin.TFhirBiologicallyDerivedProductCollection;
  TFhirBiologicallyDerivedProductCollectionList = FHIR.R4.Resources.Admin.TFhirBiologicallyDerivedProductCollectionList;
  TFhirBiologicallyDerivedProductProcessing = FHIR.R4.Resources.Admin.TFhirBiologicallyDerivedProductProcessing;
  TFhirBiologicallyDerivedProductProcessingList = FHIR.R4.Resources.Admin.TFhirBiologicallyDerivedProductProcessingList;
  TFhirBiologicallyDerivedProductManipulation = FHIR.R4.Resources.Admin.TFhirBiologicallyDerivedProductManipulation;
  TFhirBiologicallyDerivedProductManipulationList = FHIR.R4.Resources.Admin.TFhirBiologicallyDerivedProductManipulationList;
  TFhirBiologicallyDerivedProductStorage = FHIR.R4.Resources.Admin.TFhirBiologicallyDerivedProductStorage;
  TFhirBiologicallyDerivedProductStorageList = FHIR.R4.Resources.Admin.TFhirBiologicallyDerivedProductStorageList;
  TFhirBiologicallyDerivedProduct = FHIR.R4.Resources.Admin.TFhirBiologicallyDerivedProduct;
  TFhirBiologicallyDerivedProductList = FHIR.R4.Resources.Admin.TFhirBiologicallyDerivedProductList;
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
{$IFDEF FHIR_BODYSTRUCTURE}
  TFhirBodyStructure = FHIR.R4.Resources.Clinical.TFhirBodyStructure;
  TFhirBodyStructureList = FHIR.R4.Resources.Clinical.TFhirBodyStructureList;
{$ENDIF FHIR_BODYSTRUCTURE}
{$IFDEF FHIR_BUNDLE}
  TFhirBundleLink = FHIR.R4.Resources.Other.TFhirBundleLink;
  TFhirBundleLinkList = FHIR.R4.Resources.Other.TFhirBundleLinkList;
  TFhirBundleEntry = FHIR.R4.Resources.Other.TFhirBundleEntry;
  TFhirBundleEntryList = FHIR.R4.Resources.Other.TFhirBundleEntryList;
  TFhirBundleEntrySearch = FHIR.R4.Resources.Other.TFhirBundleEntrySearch;
  TFhirBundleEntrySearchList = FHIR.R4.Resources.Other.TFhirBundleEntrySearchList;
  TFhirBundleEntryRequest = FHIR.R4.Resources.Other.TFhirBundleEntryRequest;
  TFhirBundleEntryRequestList = FHIR.R4.Resources.Other.TFhirBundleEntryRequestList;
  TFhirBundleEntryResponse = FHIR.R4.Resources.Other.TFhirBundleEntryResponse;
  TFhirBundleEntryResponseList = FHIR.R4.Resources.Other.TFhirBundleEntryResponseList;
  TFhirBundle = FHIR.R4.Resources.Other.TFhirBundle;
  TFhirBundleList = FHIR.R4.Resources.Other.TFhirBundleList;
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  TFhirCapabilityStatementSoftware = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementSoftware;
  TFhirCapabilityStatementSoftwareList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementSoftwareList;
  TFhirCapabilityStatementImplementation = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementImplementation;
  TFhirCapabilityStatementImplementationList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementImplementationList;
  TFhirCapabilityStatementRest = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRest;
  TFhirCapabilityStatementRestList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestList;
  TFhirCapabilityStatementRestSecurity = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestSecurity;
  TFhirCapabilityStatementRestSecurityList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestSecurityList;
  TFhirCapabilityStatementRestResource = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestResource;
  TFhirCapabilityStatementRestResourceList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestResourceList;
  TFhirCapabilityStatementRestResourceInteraction = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestResourceInteraction;
  TFhirCapabilityStatementRestResourceInteractionList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestResourceInteractionList;
  TFhirCapabilityStatementRestResourceSearchParam = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestResourceSearchParam;
  TFhirCapabilityStatementRestResourceSearchParamList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestResourceSearchParamList;
  TFhirCapabilityStatementRestResourceOperation = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestResourceOperation;
  TFhirCapabilityStatementRestResourceOperationList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestResourceOperationList;
  TFhirCapabilityStatementRestInteraction = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestInteraction;
  TFhirCapabilityStatementRestInteractionList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementRestInteractionList;
  TFhirCapabilityStatementMessaging = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementMessaging;
  TFhirCapabilityStatementMessagingList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementMessagingList;
  TFhirCapabilityStatementMessagingEndpoint = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementMessagingEndpoint;
  TFhirCapabilityStatementMessagingEndpointList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementMessagingEndpointList;
  TFhirCapabilityStatementMessagingSupportedMessage = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementMessagingSupportedMessage;
  TFhirCapabilityStatementMessagingSupportedMessageList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementMessagingSupportedMessageList;
  TFhirCapabilityStatementDocument = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementDocument;
  TFhirCapabilityStatementDocumentList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementDocumentList;
  TFhirCapabilityStatement = FHIR.R4.Resources.Canonical.TFhirCapabilityStatement;
  TFhirCapabilityStatementList = FHIR.R4.Resources.Canonical.TFhirCapabilityStatementList;
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CAREPLAN}
  TFhirCarePlanActivity = FHIR.R4.Resources.Clinical.TFhirCarePlanActivity;
  TFhirCarePlanActivityList = FHIR.R4.Resources.Clinical.TFhirCarePlanActivityList;
  TFhirCarePlanActivityDetail = FHIR.R4.Resources.Clinical.TFhirCarePlanActivityDetail;
  TFhirCarePlanActivityDetailList = FHIR.R4.Resources.Clinical.TFhirCarePlanActivityDetailList;
  TFhirCarePlan = FHIR.R4.Resources.Clinical.TFhirCarePlan;
  TFhirCarePlanList = FHIR.R4.Resources.Clinical.TFhirCarePlanList;
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CARETEAM}
  TFhirCareTeamParticipant = FHIR.R4.Resources.Clinical.TFhirCareTeamParticipant;
  TFhirCareTeamParticipantList = FHIR.R4.Resources.Clinical.TFhirCareTeamParticipantList;
  TFhirCareTeam = FHIR.R4.Resources.Clinical.TFhirCareTeam;
  TFhirCareTeamList = FHIR.R4.Resources.Clinical.TFhirCareTeamList;
{$ENDIF FHIR_CARETEAM}
{$IFDEF FHIR_CATALOGENTRY}
  TFhirCatalogEntryRelatedEntry = FHIR.R4.Resources.Admin.TFhirCatalogEntryRelatedEntry;
  TFhirCatalogEntryRelatedEntryList = FHIR.R4.Resources.Admin.TFhirCatalogEntryRelatedEntryList;
  TFhirCatalogEntry = FHIR.R4.Resources.Admin.TFhirCatalogEntry;
  TFhirCatalogEntryList = FHIR.R4.Resources.Admin.TFhirCatalogEntryList;
{$ENDIF FHIR_CATALOGENTRY}
{$IFDEF FHIR_CHARGEITEM}
  TFhirChargeItemPerformer = FHIR.R4.Resources.Financial.TFhirChargeItemPerformer;
  TFhirChargeItemPerformerList = FHIR.R4.Resources.Financial.TFhirChargeItemPerformerList;
  TFhirChargeItem = FHIR.R4.Resources.Financial.TFhirChargeItem;
  TFhirChargeItemList = FHIR.R4.Resources.Financial.TFhirChargeItemList;
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CHARGEITEMDEFINITION}
  TFhirChargeItemDefinitionApplicability = FHIR.R4.Resources.Canonical.TFhirChargeItemDefinitionApplicability;
  TFhirChargeItemDefinitionApplicabilityList = FHIR.R4.Resources.Canonical.TFhirChargeItemDefinitionApplicabilityList;
  TFhirChargeItemDefinitionPropertyGroup = FHIR.R4.Resources.Canonical.TFhirChargeItemDefinitionPropertyGroup;
  TFhirChargeItemDefinitionPropertyGroupList = FHIR.R4.Resources.Canonical.TFhirChargeItemDefinitionPropertyGroupList;
  TFhirChargeItemDefinitionPropertyGroupPriceComponent = FHIR.R4.Resources.Canonical.TFhirChargeItemDefinitionPropertyGroupPriceComponent;
  TFhirChargeItemDefinitionPropertyGroupPriceComponentList = FHIR.R4.Resources.Canonical.TFhirChargeItemDefinitionPropertyGroupPriceComponentList;
  TFhirChargeItemDefinition = FHIR.R4.Resources.Canonical.TFhirChargeItemDefinition;
  TFhirChargeItemDefinitionList = FHIR.R4.Resources.Canonical.TFhirChargeItemDefinitionList;
{$ENDIF FHIR_CHARGEITEMDEFINITION}
{$IFDEF FHIR_CLAIM}
  TFhirClaimRelated = FHIR.R4.Resources.Financial.TFhirClaimRelated;
  TFhirClaimRelatedList = FHIR.R4.Resources.Financial.TFhirClaimRelatedList;
  TFhirClaimPayee = FHIR.R4.Resources.Financial.TFhirClaimPayee;
  TFhirClaimPayeeList = FHIR.R4.Resources.Financial.TFhirClaimPayeeList;
  TFhirClaimCareTeam = FHIR.R4.Resources.Financial.TFhirClaimCareTeam;
  TFhirClaimCareTeamList = FHIR.R4.Resources.Financial.TFhirClaimCareTeamList;
  TFhirClaimSupportingInfo = FHIR.R4.Resources.Financial.TFhirClaimSupportingInfo;
  TFhirClaimSupportingInfoList = FHIR.R4.Resources.Financial.TFhirClaimSupportingInfoList;
  TFhirClaimDiagnosis = FHIR.R4.Resources.Financial.TFhirClaimDiagnosis;
  TFhirClaimDiagnosisList = FHIR.R4.Resources.Financial.TFhirClaimDiagnosisList;
  TFhirClaimProcedure = FHIR.R4.Resources.Financial.TFhirClaimProcedure;
  TFhirClaimProcedureList = FHIR.R4.Resources.Financial.TFhirClaimProcedureList;
  TFhirClaimInsurance = FHIR.R4.Resources.Financial.TFhirClaimInsurance;
  TFhirClaimInsuranceList = FHIR.R4.Resources.Financial.TFhirClaimInsuranceList;
  TFhirClaimAccident = FHIR.R4.Resources.Financial.TFhirClaimAccident;
  TFhirClaimAccidentList = FHIR.R4.Resources.Financial.TFhirClaimAccidentList;
  TFhirClaimItem = FHIR.R4.Resources.Financial.TFhirClaimItem;
  TFhirClaimItemList = FHIR.R4.Resources.Financial.TFhirClaimItemList;
  TFhirClaimItemDetail = FHIR.R4.Resources.Financial.TFhirClaimItemDetail;
  TFhirClaimItemDetailList = FHIR.R4.Resources.Financial.TFhirClaimItemDetailList;
  TFhirClaimItemDetailSubDetail = FHIR.R4.Resources.Financial.TFhirClaimItemDetailSubDetail;
  TFhirClaimItemDetailSubDetailList = FHIR.R4.Resources.Financial.TFhirClaimItemDetailSubDetailList;
  TFhirClaim = FHIR.R4.Resources.Financial.TFhirClaim;
  TFhirClaimList = FHIR.R4.Resources.Financial.TFhirClaimList;
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  TFhirClaimResponseItem = FHIR.R4.Resources.Financial.TFhirClaimResponseItem;
  TFhirClaimResponseItemList = FHIR.R4.Resources.Financial.TFhirClaimResponseItemList;
  TFhirClaimResponseItemAdjudication = FHIR.R4.Resources.Financial.TFhirClaimResponseItemAdjudication;
  TFhirClaimResponseItemAdjudicationList = FHIR.R4.Resources.Financial.TFhirClaimResponseItemAdjudicationList;
  TFhirClaimResponseItemDetail = FHIR.R4.Resources.Financial.TFhirClaimResponseItemDetail;
  TFhirClaimResponseItemDetailList = FHIR.R4.Resources.Financial.TFhirClaimResponseItemDetailList;
  TFhirClaimResponseItemDetailSubDetail = FHIR.R4.Resources.Financial.TFhirClaimResponseItemDetailSubDetail;
  TFhirClaimResponseItemDetailSubDetailList = FHIR.R4.Resources.Financial.TFhirClaimResponseItemDetailSubDetailList;
  TFhirClaimResponseAddItem = FHIR.R4.Resources.Financial.TFhirClaimResponseAddItem;
  TFhirClaimResponseAddItemList = FHIR.R4.Resources.Financial.TFhirClaimResponseAddItemList;
  TFhirClaimResponseAddItemDetail = FHIR.R4.Resources.Financial.TFhirClaimResponseAddItemDetail;
  TFhirClaimResponseAddItemDetailList = FHIR.R4.Resources.Financial.TFhirClaimResponseAddItemDetailList;
  TFhirClaimResponseAddItemDetailSubDetail = FHIR.R4.Resources.Financial.TFhirClaimResponseAddItemDetailSubDetail;
  TFhirClaimResponseAddItemDetailSubDetailList = FHIR.R4.Resources.Financial.TFhirClaimResponseAddItemDetailSubDetailList;
  TFhirClaimResponseTotal = FHIR.R4.Resources.Financial.TFhirClaimResponseTotal;
  TFhirClaimResponseTotalList = FHIR.R4.Resources.Financial.TFhirClaimResponseTotalList;
  TFhirClaimResponsePayment = FHIR.R4.Resources.Financial.TFhirClaimResponsePayment;
  TFhirClaimResponsePaymentList = FHIR.R4.Resources.Financial.TFhirClaimResponsePaymentList;
  TFhirClaimResponseProcessNote = FHIR.R4.Resources.Financial.TFhirClaimResponseProcessNote;
  TFhirClaimResponseProcessNoteList = FHIR.R4.Resources.Financial.TFhirClaimResponseProcessNoteList;
  TFhirClaimResponseInsurance = FHIR.R4.Resources.Financial.TFhirClaimResponseInsurance;
  TFhirClaimResponseInsuranceList = FHIR.R4.Resources.Financial.TFhirClaimResponseInsuranceList;
  TFhirClaimResponseError = FHIR.R4.Resources.Financial.TFhirClaimResponseError;
  TFhirClaimResponseErrorList = FHIR.R4.Resources.Financial.TFhirClaimResponseErrorList;
  TFhirClaimResponse = FHIR.R4.Resources.Financial.TFhirClaimResponse;
  TFhirClaimResponseList = FHIR.R4.Resources.Financial.TFhirClaimResponseList;
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  TFhirClinicalImpressionInvestigation = FHIR.R4.Resources.Clinical.TFhirClinicalImpressionInvestigation;
  TFhirClinicalImpressionInvestigationList = FHIR.R4.Resources.Clinical.TFhirClinicalImpressionInvestigationList;
  TFhirClinicalImpressionFinding = FHIR.R4.Resources.Clinical.TFhirClinicalImpressionFinding;
  TFhirClinicalImpressionFindingList = FHIR.R4.Resources.Clinical.TFhirClinicalImpressionFindingList;
  TFhirClinicalImpression = FHIR.R4.Resources.Clinical.TFhirClinicalImpression;
  TFhirClinicalImpressionList = FHIR.R4.Resources.Clinical.TFhirClinicalImpressionList;
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_CODESYSTEM}
  TFhirCodeSystemFilter = FHIR.R4.Resources.Canonical.TFhirCodeSystemFilter;
  TFhirCodeSystemFilterList = FHIR.R4.Resources.Canonical.TFhirCodeSystemFilterList;
  TFhirCodeSystemProperty = FHIR.R4.Resources.Canonical.TFhirCodeSystemProperty;
  TFhirCodeSystemPropertyList = FHIR.R4.Resources.Canonical.TFhirCodeSystemPropertyList;
  TFhirCodeSystemConcept = FHIR.R4.Resources.Canonical.TFhirCodeSystemConcept;
  TFhirCodeSystemConceptList = FHIR.R4.Resources.Canonical.TFhirCodeSystemConceptList;
  TFhirCodeSystemConceptDesignation = FHIR.R4.Resources.Canonical.TFhirCodeSystemConceptDesignation;
  TFhirCodeSystemConceptDesignationList = FHIR.R4.Resources.Canonical.TFhirCodeSystemConceptDesignationList;
  TFhirCodeSystemConceptProperty = FHIR.R4.Resources.Canonical.TFhirCodeSystemConceptProperty;
  TFhirCodeSystemConceptPropertyList = FHIR.R4.Resources.Canonical.TFhirCodeSystemConceptPropertyList;
  TFhirCodeSystem = FHIR.R4.Resources.Canonical.TFhirCodeSystem;
  TFhirCodeSystemList = FHIR.R4.Resources.Canonical.TFhirCodeSystemList;
{$ENDIF FHIR_CODESYSTEM}
{$IFDEF FHIR_COMMUNICATION}
  TFhirCommunicationPayload = FHIR.R4.Resources.Clinical.TFhirCommunicationPayload;
  TFhirCommunicationPayloadList = FHIR.R4.Resources.Clinical.TFhirCommunicationPayloadList;
  TFhirCommunication = FHIR.R4.Resources.Clinical.TFhirCommunication;
  TFhirCommunicationList = FHIR.R4.Resources.Clinical.TFhirCommunicationList;
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  TFhirCommunicationRequestPayload = FHIR.R4.Resources.Clinical.TFhirCommunicationRequestPayload;
  TFhirCommunicationRequestPayloadList = FHIR.R4.Resources.Clinical.TFhirCommunicationRequestPayloadList;
  TFhirCommunicationRequest = FHIR.R4.Resources.Clinical.TFhirCommunicationRequest;
  TFhirCommunicationRequestList = FHIR.R4.Resources.Clinical.TFhirCommunicationRequestList;
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  TFhirCompartmentDefinitionResource = FHIR.R4.Resources.Canonical.TFhirCompartmentDefinitionResource;
  TFhirCompartmentDefinitionResourceList = FHIR.R4.Resources.Canonical.TFhirCompartmentDefinitionResourceList;
  TFhirCompartmentDefinition = FHIR.R4.Resources.Canonical.TFhirCompartmentDefinition;
  TFhirCompartmentDefinitionList = FHIR.R4.Resources.Canonical.TFhirCompartmentDefinitionList;
{$ENDIF FHIR_COMPARTMENTDEFINITION}
{$IFDEF FHIR_COMPOSITION}
  TFhirCompositionAttester = FHIR.R4.Resources.Clinical.TFhirCompositionAttester;
  TFhirCompositionAttesterList = FHIR.R4.Resources.Clinical.TFhirCompositionAttesterList;
  TFhirCompositionRelatesTo = FHIR.R4.Resources.Clinical.TFhirCompositionRelatesTo;
  TFhirCompositionRelatesToList = FHIR.R4.Resources.Clinical.TFhirCompositionRelatesToList;
  TFhirCompositionEvent = FHIR.R4.Resources.Clinical.TFhirCompositionEvent;
  TFhirCompositionEventList = FHIR.R4.Resources.Clinical.TFhirCompositionEventList;
  TFhirCompositionSection = FHIR.R4.Resources.Clinical.TFhirCompositionSection;
  TFhirCompositionSectionList = FHIR.R4.Resources.Clinical.TFhirCompositionSectionList;
  TFhirComposition = FHIR.R4.Resources.Clinical.TFhirComposition;
  TFhirCompositionList = FHIR.R4.Resources.Clinical.TFhirCompositionList;
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  TFhirConceptMapGroup = FHIR.R4.Resources.Canonical.TFhirConceptMapGroup;
  TFhirConceptMapGroupList = FHIR.R4.Resources.Canonical.TFhirConceptMapGroupList;
  TFhirConceptMapGroupElement = FHIR.R4.Resources.Canonical.TFhirConceptMapGroupElement;
  TFhirConceptMapGroupElementList = FHIR.R4.Resources.Canonical.TFhirConceptMapGroupElementList;
  TFhirConceptMapGroupElementTarget = FHIR.R4.Resources.Canonical.TFhirConceptMapGroupElementTarget;
  TFhirConceptMapGroupElementTargetList = FHIR.R4.Resources.Canonical.TFhirConceptMapGroupElementTargetList;
  TFhirConceptMapGroupElementTargetDependsOn = FHIR.R4.Resources.Canonical.TFhirConceptMapGroupElementTargetDependsOn;
  TFhirConceptMapGroupElementTargetDependsOnList = FHIR.R4.Resources.Canonical.TFhirConceptMapGroupElementTargetDependsOnList;
  TFhirConceptMapGroupUnmapped = FHIR.R4.Resources.Canonical.TFhirConceptMapGroupUnmapped;
  TFhirConceptMapGroupUnmappedList = FHIR.R4.Resources.Canonical.TFhirConceptMapGroupUnmappedList;
  TFhirConceptMap = FHIR.R4.Resources.Canonical.TFhirConceptMap;
  TFhirConceptMapList = FHIR.R4.Resources.Canonical.TFhirConceptMapList;
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_CONDITION}
  TFhirConditionStage = FHIR.R4.Resources.Clinical.TFhirConditionStage;
  TFhirConditionStageList = FHIR.R4.Resources.Clinical.TFhirConditionStageList;
  TFhirConditionEvidence = FHIR.R4.Resources.Clinical.TFhirConditionEvidence;
  TFhirConditionEvidenceList = FHIR.R4.Resources.Clinical.TFhirConditionEvidenceList;
  TFhirCondition = FHIR.R4.Resources.Clinical.TFhirCondition;
  TFhirConditionList = FHIR.R4.Resources.Clinical.TFhirConditionList;
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_CONSENT}
  TFhirConsentPolicy = FHIR.R4.Resources.Other.TFhirConsentPolicy;
  TFhirConsentPolicyList = FHIR.R4.Resources.Other.TFhirConsentPolicyList;
  TFhirConsentVerification = FHIR.R4.Resources.Other.TFhirConsentVerification;
  TFhirConsentVerificationList = FHIR.R4.Resources.Other.TFhirConsentVerificationList;
  TFhirConsentProvision = FHIR.R4.Resources.Other.TFhirConsentProvision;
  TFhirConsentProvisionList = FHIR.R4.Resources.Other.TFhirConsentProvisionList;
  TFhirConsentProvisionActor = FHIR.R4.Resources.Other.TFhirConsentProvisionActor;
  TFhirConsentProvisionActorList = FHIR.R4.Resources.Other.TFhirConsentProvisionActorList;
  TFhirConsentProvisionData = FHIR.R4.Resources.Other.TFhirConsentProvisionData;
  TFhirConsentProvisionDataList = FHIR.R4.Resources.Other.TFhirConsentProvisionDataList;
  TFhirConsent = FHIR.R4.Resources.Other.TFhirConsent;
  TFhirConsentList = FHIR.R4.Resources.Other.TFhirConsentList;
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  TFhirContractContentDefinition = FHIR.R4.Resources.Other.TFhirContractContentDefinition;
  TFhirContractContentDefinitionList = FHIR.R4.Resources.Other.TFhirContractContentDefinitionList;
  TFhirContractTerm = FHIR.R4.Resources.Other.TFhirContractTerm;
  TFhirContractTermList = FHIR.R4.Resources.Other.TFhirContractTermList;
  TFhirContractTermSecurityLabel = FHIR.R4.Resources.Other.TFhirContractTermSecurityLabel;
  TFhirContractTermSecurityLabelList = FHIR.R4.Resources.Other.TFhirContractTermSecurityLabelList;
  TFhirContractTermOffer = FHIR.R4.Resources.Other.TFhirContractTermOffer;
  TFhirContractTermOfferList = FHIR.R4.Resources.Other.TFhirContractTermOfferList;
  TFhirContractTermOfferParty = FHIR.R4.Resources.Other.TFhirContractTermOfferParty;
  TFhirContractTermOfferPartyList = FHIR.R4.Resources.Other.TFhirContractTermOfferPartyList;
  TFhirContractTermOfferAnswer = FHIR.R4.Resources.Other.TFhirContractTermOfferAnswer;
  TFhirContractTermOfferAnswerList = FHIR.R4.Resources.Other.TFhirContractTermOfferAnswerList;
  TFhirContractTermAsset = FHIR.R4.Resources.Other.TFhirContractTermAsset;
  TFhirContractTermAssetList = FHIR.R4.Resources.Other.TFhirContractTermAssetList;
  TFhirContractTermAssetContext = FHIR.R4.Resources.Other.TFhirContractTermAssetContext;
  TFhirContractTermAssetContextList = FHIR.R4.Resources.Other.TFhirContractTermAssetContextList;
  TFhirContractTermAssetValuedItem = FHIR.R4.Resources.Other.TFhirContractTermAssetValuedItem;
  TFhirContractTermAssetValuedItemList = FHIR.R4.Resources.Other.TFhirContractTermAssetValuedItemList;
  TFhirContractTermAction = FHIR.R4.Resources.Other.TFhirContractTermAction;
  TFhirContractTermActionList = FHIR.R4.Resources.Other.TFhirContractTermActionList;
  TFhirContractTermActionSubject = FHIR.R4.Resources.Other.TFhirContractTermActionSubject;
  TFhirContractTermActionSubjectList = FHIR.R4.Resources.Other.TFhirContractTermActionSubjectList;
  TFhirContractSigner = FHIR.R4.Resources.Other.TFhirContractSigner;
  TFhirContractSignerList = FHIR.R4.Resources.Other.TFhirContractSignerList;
  TFhirContractFriendly = FHIR.R4.Resources.Other.TFhirContractFriendly;
  TFhirContractFriendlyList = FHIR.R4.Resources.Other.TFhirContractFriendlyList;
  TFhirContractLegal = FHIR.R4.Resources.Other.TFhirContractLegal;
  TFhirContractLegalList = FHIR.R4.Resources.Other.TFhirContractLegalList;
  TFhirContractRule = FHIR.R4.Resources.Other.TFhirContractRule;
  TFhirContractRuleList = FHIR.R4.Resources.Other.TFhirContractRuleList;
  TFhirContract = FHIR.R4.Resources.Other.TFhirContract;
  TFhirContractList = FHIR.R4.Resources.Other.TFhirContractList;
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_COVERAGE}
  TFhirCoverageClass = FHIR.R4.Resources.Financial.TFhirCoverageClass;
  TFhirCoverageClassList = FHIR.R4.Resources.Financial.TFhirCoverageClassList;
  TFhirCoverageCostToBeneficiary = FHIR.R4.Resources.Financial.TFhirCoverageCostToBeneficiary;
  TFhirCoverageCostToBeneficiaryList = FHIR.R4.Resources.Financial.TFhirCoverageCostToBeneficiaryList;
  TFhirCoverageCostToBeneficiaryException = FHIR.R4.Resources.Financial.TFhirCoverageCostToBeneficiaryException;
  TFhirCoverageCostToBeneficiaryExceptionList = FHIR.R4.Resources.Financial.TFhirCoverageCostToBeneficiaryExceptionList;
  TFhirCoverage = FHIR.R4.Resources.Financial.TFhirCoverage;
  TFhirCoverageList = FHIR.R4.Resources.Financial.TFhirCoverageList;
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  TFhirCoverageEligibilityRequestSupportingInfo = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityRequestSupportingInfo;
  TFhirCoverageEligibilityRequestSupportingInfoList = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityRequestSupportingInfoList;
  TFhirCoverageEligibilityRequestInsurance = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityRequestInsurance;
  TFhirCoverageEligibilityRequestInsuranceList = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityRequestInsuranceList;
  TFhirCoverageEligibilityRequestItem = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityRequestItem;
  TFhirCoverageEligibilityRequestItemList = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityRequestItemList;
  TFhirCoverageEligibilityRequestItemDiagnosis = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityRequestItemDiagnosis;
  TFhirCoverageEligibilityRequestItemDiagnosisList = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityRequestItemDiagnosisList;
  TFhirCoverageEligibilityRequest = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityRequest;
  TFhirCoverageEligibilityRequestList = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityRequestList;
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}
{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  TFhirCoverageEligibilityResponseInsurance = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityResponseInsurance;
  TFhirCoverageEligibilityResponseInsuranceList = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityResponseInsuranceList;
  TFhirCoverageEligibilityResponseInsuranceItem = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityResponseInsuranceItem;
  TFhirCoverageEligibilityResponseInsuranceItemList = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityResponseInsuranceItemList;
  TFhirCoverageEligibilityResponseInsuranceItemBenefit = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityResponseInsuranceItemBenefit;
  TFhirCoverageEligibilityResponseInsuranceItemBenefitList = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityResponseInsuranceItemBenefitList;
  TFhirCoverageEligibilityResponseError = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityResponseError;
  TFhirCoverageEligibilityResponseErrorList = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityResponseErrorList;
  TFhirCoverageEligibilityResponse = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityResponse;
  TFhirCoverageEligibilityResponseList = FHIR.R4.Resources.Financial.TFhirCoverageEligibilityResponseList;
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}
{$IFDEF FHIR_DETECTEDISSUE}
  TFhirDetectedIssueEvidence = FHIR.R4.Resources.Clinical.TFhirDetectedIssueEvidence;
  TFhirDetectedIssueEvidenceList = FHIR.R4.Resources.Clinical.TFhirDetectedIssueEvidenceList;
  TFhirDetectedIssueMitigation = FHIR.R4.Resources.Clinical.TFhirDetectedIssueMitigation;
  TFhirDetectedIssueMitigationList = FHIR.R4.Resources.Clinical.TFhirDetectedIssueMitigationList;
  TFhirDetectedIssue = FHIR.R4.Resources.Clinical.TFhirDetectedIssue;
  TFhirDetectedIssueList = FHIR.R4.Resources.Clinical.TFhirDetectedIssueList;
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  TFhirDeviceUdiCarrier = FHIR.R4.Resources.Admin.TFhirDeviceUdiCarrier;
  TFhirDeviceUdiCarrierList = FHIR.R4.Resources.Admin.TFhirDeviceUdiCarrierList;
  TFhirDeviceDeviceName = FHIR.R4.Resources.Admin.TFhirDeviceDeviceName;
  TFhirDeviceDeviceNameList = FHIR.R4.Resources.Admin.TFhirDeviceDeviceNameList;
  TFhirDeviceSpecialization = FHIR.R4.Resources.Admin.TFhirDeviceSpecialization;
  TFhirDeviceSpecializationList = FHIR.R4.Resources.Admin.TFhirDeviceSpecializationList;
  TFhirDeviceVersion = FHIR.R4.Resources.Admin.TFhirDeviceVersion;
  TFhirDeviceVersionList = FHIR.R4.Resources.Admin.TFhirDeviceVersionList;
  TFhirDeviceProperty = FHIR.R4.Resources.Admin.TFhirDeviceProperty;
  TFhirDevicePropertyList = FHIR.R4.Resources.Admin.TFhirDevicePropertyList;
  TFhirDevice = FHIR.R4.Resources.Admin.TFhirDevice;
  TFhirDeviceList = FHIR.R4.Resources.Admin.TFhirDeviceList;
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICEDEFINITION}
  TFhirDeviceDefinitionUdiDeviceIdentifier = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionUdiDeviceIdentifier;
  TFhirDeviceDefinitionUdiDeviceIdentifierList = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionUdiDeviceIdentifierList;
  TFhirDeviceDefinitionDeviceName = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionDeviceName;
  TFhirDeviceDefinitionDeviceNameList = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionDeviceNameList;
  TFhirDeviceDefinitionSpecialization = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionSpecialization;
  TFhirDeviceDefinitionSpecializationList = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionSpecializationList;
  TFhirDeviceDefinitionCapability = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionCapability;
  TFhirDeviceDefinitionCapabilityList = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionCapabilityList;
  TFhirDeviceDefinitionProperty = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionProperty;
  TFhirDeviceDefinitionPropertyList = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionPropertyList;
  TFhirDeviceDefinitionMaterial = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionMaterial;
  TFhirDeviceDefinitionMaterialList = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionMaterialList;
  TFhirDeviceDefinition = FHIR.R4.Resources.Canonical.TFhirDeviceDefinition;
  TFhirDeviceDefinitionList = FHIR.R4.Resources.Canonical.TFhirDeviceDefinitionList;
{$ENDIF FHIR_DEVICEDEFINITION}
{$IFDEF FHIR_DEVICEMETRIC}
  TFhirDeviceMetricCalibration = FHIR.R4.Resources.Admin.TFhirDeviceMetricCalibration;
  TFhirDeviceMetricCalibrationList = FHIR.R4.Resources.Admin.TFhirDeviceMetricCalibrationList;
  TFhirDeviceMetric = FHIR.R4.Resources.Admin.TFhirDeviceMetric;
  TFhirDeviceMetricList = FHIR.R4.Resources.Admin.TFhirDeviceMetricList;
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEREQUEST}
  TFhirDeviceRequestParameter = FHIR.R4.Resources.Clinical.TFhirDeviceRequestParameter;
  TFhirDeviceRequestParameterList = FHIR.R4.Resources.Clinical.TFhirDeviceRequestParameterList;
  TFhirDeviceRequest = FHIR.R4.Resources.Clinical.TFhirDeviceRequest;
  TFhirDeviceRequestList = FHIR.R4.Resources.Clinical.TFhirDeviceRequestList;
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  TFhirDeviceUseStatement = FHIR.R4.Resources.Clinical.TFhirDeviceUseStatement;
  TFhirDeviceUseStatementList = FHIR.R4.Resources.Clinical.TFhirDeviceUseStatementList;
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  TFhirDiagnosticReportMedia = FHIR.R4.Resources.Clinical.TFhirDiagnosticReportMedia;
  TFhirDiagnosticReportMediaList = FHIR.R4.Resources.Clinical.TFhirDiagnosticReportMediaList;
  TFhirDiagnosticReport = FHIR.R4.Resources.Clinical.TFhirDiagnosticReport;
  TFhirDiagnosticReportList = FHIR.R4.Resources.Clinical.TFhirDiagnosticReportList;
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  TFhirDocumentManifestRelated = FHIR.R4.Resources.Clinical.TFhirDocumentManifestRelated;
  TFhirDocumentManifestRelatedList = FHIR.R4.Resources.Clinical.TFhirDocumentManifestRelatedList;
  TFhirDocumentManifest = FHIR.R4.Resources.Clinical.TFhirDocumentManifest;
  TFhirDocumentManifestList = FHIR.R4.Resources.Clinical.TFhirDocumentManifestList;
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  TFhirDocumentReferenceRelatesTo = FHIR.R4.Resources.Clinical.TFhirDocumentReferenceRelatesTo;
  TFhirDocumentReferenceRelatesToList = FHIR.R4.Resources.Clinical.TFhirDocumentReferenceRelatesToList;
  TFhirDocumentReferenceContent = FHIR.R4.Resources.Clinical.TFhirDocumentReferenceContent;
  TFhirDocumentReferenceContentList = FHIR.R4.Resources.Clinical.TFhirDocumentReferenceContentList;
  TFhirDocumentReferenceContext = FHIR.R4.Resources.Clinical.TFhirDocumentReferenceContext;
  TFhirDocumentReferenceContextList = FHIR.R4.Resources.Clinical.TFhirDocumentReferenceContextList;
  TFhirDocumentReference = FHIR.R4.Resources.Clinical.TFhirDocumentReference;
  TFhirDocumentReferenceList = FHIR.R4.Resources.Clinical.TFhirDocumentReferenceList;
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_EFFECTEVIDENCESYNTHESIS}
  TFhirEffectEvidenceSynthesisSampleSize = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisSampleSize;
  TFhirEffectEvidenceSynthesisSampleSizeList = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisSampleSizeList;
  TFhirEffectEvidenceSynthesisResultsByExposure = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisResultsByExposure;
  TFhirEffectEvidenceSynthesisResultsByExposureList = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisResultsByExposureList;
  TFhirEffectEvidenceSynthesisEffectEstimate = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisEffectEstimate;
  TFhirEffectEvidenceSynthesisEffectEstimateList = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisEffectEstimateList;
  TFhirEffectEvidenceSynthesisEffectEstimatePrecisionEstimate = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisEffectEstimatePrecisionEstimate;
  TFhirEffectEvidenceSynthesisEffectEstimatePrecisionEstimateList = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisEffectEstimatePrecisionEstimateList;
  TFhirEffectEvidenceSynthesisCertainty = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisCertainty;
  TFhirEffectEvidenceSynthesisCertaintyList = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisCertaintyList;
  TFhirEffectEvidenceSynthesisCertaintyCertaintySubcomponent = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisCertaintyCertaintySubcomponent;
  TFhirEffectEvidenceSynthesisCertaintyCertaintySubcomponentList = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisCertaintyCertaintySubcomponentList;
  TFhirEffectEvidenceSynthesis = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesis;
  TFhirEffectEvidenceSynthesisList = FHIR.R4.Resources.Other.TFhirEffectEvidenceSynthesisList;
{$ENDIF FHIR_EFFECTEVIDENCESYNTHESIS}
{$IFDEF FHIR_ENCOUNTER}
  TFhirEncounterStatusHistory = FHIR.R4.Resources.Admin.TFhirEncounterStatusHistory;
  TFhirEncounterStatusHistoryList = FHIR.R4.Resources.Admin.TFhirEncounterStatusHistoryList;
  TFhirEncounterClassHistory = FHIR.R4.Resources.Admin.TFhirEncounterClassHistory;
  TFhirEncounterClassHistoryList = FHIR.R4.Resources.Admin.TFhirEncounterClassHistoryList;
  TFhirEncounterParticipant = FHIR.R4.Resources.Admin.TFhirEncounterParticipant;
  TFhirEncounterParticipantList = FHIR.R4.Resources.Admin.TFhirEncounterParticipantList;
  TFhirEncounterDiagnosis = FHIR.R4.Resources.Admin.TFhirEncounterDiagnosis;
  TFhirEncounterDiagnosisList = FHIR.R4.Resources.Admin.TFhirEncounterDiagnosisList;
  TFhirEncounterHospitalization = FHIR.R4.Resources.Admin.TFhirEncounterHospitalization;
  TFhirEncounterHospitalizationList = FHIR.R4.Resources.Admin.TFhirEncounterHospitalizationList;
  TFhirEncounterLocation = FHIR.R4.Resources.Admin.TFhirEncounterLocation;
  TFhirEncounterLocationList = FHIR.R4.Resources.Admin.TFhirEncounterLocationList;
  TFhirEncounter = FHIR.R4.Resources.Admin.TFhirEncounter;
  TFhirEncounterList = FHIR.R4.Resources.Admin.TFhirEncounterList;
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENDPOINT}
  TFhirEndpoint = FHIR.R4.Resources.Admin.TFhirEndpoint;
  TFhirEndpointList = FHIR.R4.Resources.Admin.TFhirEndpointList;
{$ENDIF FHIR_ENDPOINT}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  TFhirEnrollmentRequest = FHIR.R4.Resources.Financial.TFhirEnrollmentRequest;
  TFhirEnrollmentRequestList = FHIR.R4.Resources.Financial.TFhirEnrollmentRequestList;
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  TFhirEnrollmentResponse = FHIR.R4.Resources.Financial.TFhirEnrollmentResponse;
  TFhirEnrollmentResponseList = FHIR.R4.Resources.Financial.TFhirEnrollmentResponseList;
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EPISODEOFCARE}
  TFhirEpisodeOfCareStatusHistory = FHIR.R4.Resources.Admin.TFhirEpisodeOfCareStatusHistory;
  TFhirEpisodeOfCareStatusHistoryList = FHIR.R4.Resources.Admin.TFhirEpisodeOfCareStatusHistoryList;
  TFhirEpisodeOfCareDiagnosis = FHIR.R4.Resources.Admin.TFhirEpisodeOfCareDiagnosis;
  TFhirEpisodeOfCareDiagnosisList = FHIR.R4.Resources.Admin.TFhirEpisodeOfCareDiagnosisList;
  TFhirEpisodeOfCare = FHIR.R4.Resources.Admin.TFhirEpisodeOfCare;
  TFhirEpisodeOfCareList = FHIR.R4.Resources.Admin.TFhirEpisodeOfCareList;
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EVENTDEFINITION}
  TFhirEventDefinition = FHIR.R4.Resources.Canonical.TFhirEventDefinition;
  TFhirEventDefinitionList = FHIR.R4.Resources.Canonical.TFhirEventDefinitionList;
{$ENDIF FHIR_EVENTDEFINITION}
{$IFDEF FHIR_EVIDENCE}
  TFhirEvidence = FHIR.R4.Resources.Other.TFhirEvidence;
  TFhirEvidenceList = FHIR.R4.Resources.Other.TFhirEvidenceList;
{$ENDIF FHIR_EVIDENCE}
{$IFDEF FHIR_EVIDENCEVARIABLE}
  TFhirEvidenceVariableCharacteristic = FHIR.R4.Resources.Other.TFhirEvidenceVariableCharacteristic;
  TFhirEvidenceVariableCharacteristicList = FHIR.R4.Resources.Other.TFhirEvidenceVariableCharacteristicList;
  TFhirEvidenceVariable = FHIR.R4.Resources.Other.TFhirEvidenceVariable;
  TFhirEvidenceVariableList = FHIR.R4.Resources.Other.TFhirEvidenceVariableList;
{$ENDIF FHIR_EVIDENCEVARIABLE}
{$IFDEF FHIR_EXAMPLESCENARIO}
  TFhirExampleScenarioActor = FHIR.R4.Resources.Canonical.TFhirExampleScenarioActor;
  TFhirExampleScenarioActorList = FHIR.R4.Resources.Canonical.TFhirExampleScenarioActorList;
  TFhirExampleScenarioInstance = FHIR.R4.Resources.Canonical.TFhirExampleScenarioInstance;
  TFhirExampleScenarioInstanceList = FHIR.R4.Resources.Canonical.TFhirExampleScenarioInstanceList;
  TFhirExampleScenarioInstanceVersion = FHIR.R4.Resources.Canonical.TFhirExampleScenarioInstanceVersion;
  TFhirExampleScenarioInstanceVersionList = FHIR.R4.Resources.Canonical.TFhirExampleScenarioInstanceVersionList;
  TFhirExampleScenarioInstanceContainedInstance = FHIR.R4.Resources.Canonical.TFhirExampleScenarioInstanceContainedInstance;
  TFhirExampleScenarioInstanceContainedInstanceList = FHIR.R4.Resources.Canonical.TFhirExampleScenarioInstanceContainedInstanceList;
  TFhirExampleScenarioProcess = FHIR.R4.Resources.Canonical.TFhirExampleScenarioProcess;
  TFhirExampleScenarioProcessList = FHIR.R4.Resources.Canonical.TFhirExampleScenarioProcessList;
  TFhirExampleScenarioProcessStep = FHIR.R4.Resources.Canonical.TFhirExampleScenarioProcessStep;
  TFhirExampleScenarioProcessStepList = FHIR.R4.Resources.Canonical.TFhirExampleScenarioProcessStepList;
  TFhirExampleScenarioProcessStepOperation = FHIR.R4.Resources.Canonical.TFhirExampleScenarioProcessStepOperation;
  TFhirExampleScenarioProcessStepOperationList = FHIR.R4.Resources.Canonical.TFhirExampleScenarioProcessStepOperationList;
  TFhirExampleScenarioProcessStepAlternative = FHIR.R4.Resources.Canonical.TFhirExampleScenarioProcessStepAlternative;
  TFhirExampleScenarioProcessStepAlternativeList = FHIR.R4.Resources.Canonical.TFhirExampleScenarioProcessStepAlternativeList;
  TFhirExampleScenario = FHIR.R4.Resources.Canonical.TFhirExampleScenario;
  TFhirExampleScenarioList = FHIR.R4.Resources.Canonical.TFhirExampleScenarioList;
{$ENDIF FHIR_EXAMPLESCENARIO}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  TFhirExplanationOfBenefitRelated = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitRelated;
  TFhirExplanationOfBenefitRelatedList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitRelatedList;
  TFhirExplanationOfBenefitPayee = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitPayee;
  TFhirExplanationOfBenefitPayeeList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitPayeeList;
  TFhirExplanationOfBenefitCareTeam = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitCareTeam;
  TFhirExplanationOfBenefitCareTeamList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitCareTeamList;
  TFhirExplanationOfBenefitSupportingInfo = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitSupportingInfo;
  TFhirExplanationOfBenefitSupportingInfoList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitSupportingInfoList;
  TFhirExplanationOfBenefitDiagnosis = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitDiagnosis;
  TFhirExplanationOfBenefitDiagnosisList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitDiagnosisList;
  TFhirExplanationOfBenefitProcedure = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitProcedure;
  TFhirExplanationOfBenefitProcedureList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitProcedureList;
  TFhirExplanationOfBenefitInsurance = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitInsurance;
  TFhirExplanationOfBenefitInsuranceList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitInsuranceList;
  TFhirExplanationOfBenefitAccident = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitAccident;
  TFhirExplanationOfBenefitAccidentList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitAccidentList;
  TFhirExplanationOfBenefitItem = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitItem;
  TFhirExplanationOfBenefitItemList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitItemList;
  TFhirExplanationOfBenefitItemAdjudication = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitItemAdjudication;
  TFhirExplanationOfBenefitItemAdjudicationList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitItemAdjudicationList;
  TFhirExplanationOfBenefitItemDetail = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitItemDetail;
  TFhirExplanationOfBenefitItemDetailList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitItemDetailList;
  TFhirExplanationOfBenefitItemDetailSubDetail = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitItemDetailSubDetail;
  TFhirExplanationOfBenefitItemDetailSubDetailList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitItemDetailSubDetailList;
  TFhirExplanationOfBenefitAddItem = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitAddItem;
  TFhirExplanationOfBenefitAddItemList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitAddItemList;
  TFhirExplanationOfBenefitAddItemDetail = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitAddItemDetail;
  TFhirExplanationOfBenefitAddItemDetailList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitAddItemDetailList;
  TFhirExplanationOfBenefitAddItemDetailSubDetail = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitAddItemDetailSubDetail;
  TFhirExplanationOfBenefitAddItemDetailSubDetailList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitAddItemDetailSubDetailList;
  TFhirExplanationOfBenefitTotal = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitTotal;
  TFhirExplanationOfBenefitTotalList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitTotalList;
  TFhirExplanationOfBenefitPayment = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitPayment;
  TFhirExplanationOfBenefitPaymentList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitPaymentList;
  TFhirExplanationOfBenefitProcessNote = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitProcessNote;
  TFhirExplanationOfBenefitProcessNoteList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitProcessNoteList;
  TFhirExplanationOfBenefitBenefitBalance = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitBenefitBalance;
  TFhirExplanationOfBenefitBenefitBalanceList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitBenefitBalanceList;
  TFhirExplanationOfBenefitBenefitBalanceFinancial = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitBenefitBalanceFinancial;
  TFhirExplanationOfBenefitBenefitBalanceFinancialList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitBenefitBalanceFinancialList;
  TFhirExplanationOfBenefit = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefit;
  TFhirExplanationOfBenefitList = FHIR.R4.Resources.Financial.TFhirExplanationOfBenefitList;
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  TFhirFamilyMemberHistoryCondition = FHIR.R4.Resources.Clinical.TFhirFamilyMemberHistoryCondition;
  TFhirFamilyMemberHistoryConditionList = FHIR.R4.Resources.Clinical.TFhirFamilyMemberHistoryConditionList;
  TFhirFamilyMemberHistory = FHIR.R4.Resources.Clinical.TFhirFamilyMemberHistory;
  TFhirFamilyMemberHistoryList = FHIR.R4.Resources.Clinical.TFhirFamilyMemberHistoryList;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  TFhirFlag = FHIR.R4.Resources.Clinical.TFhirFlag;
  TFhirFlagList = FHIR.R4.Resources.Clinical.TFhirFlagList;
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  TFhirGoalTarget = FHIR.R4.Resources.Clinical.TFhirGoalTarget;
  TFhirGoalTargetList = FHIR.R4.Resources.Clinical.TFhirGoalTargetList;
  TFhirGoal = FHIR.R4.Resources.Clinical.TFhirGoal;
  TFhirGoalList = FHIR.R4.Resources.Clinical.TFhirGoalList;
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GRAPHDEFINITION}
  TFhirGraphDefinitionLink = FHIR.R4.Resources.Canonical.TFhirGraphDefinitionLink;
  TFhirGraphDefinitionLinkList = FHIR.R4.Resources.Canonical.TFhirGraphDefinitionLinkList;
  TFhirGraphDefinitionLinkTarget = FHIR.R4.Resources.Canonical.TFhirGraphDefinitionLinkTarget;
  TFhirGraphDefinitionLinkTargetList = FHIR.R4.Resources.Canonical.TFhirGraphDefinitionLinkTargetList;
  TFhirGraphDefinitionLinkTargetCompartment = FHIR.R4.Resources.Canonical.TFhirGraphDefinitionLinkTargetCompartment;
  TFhirGraphDefinitionLinkTargetCompartmentList = FHIR.R4.Resources.Canonical.TFhirGraphDefinitionLinkTargetCompartmentList;
  TFhirGraphDefinition = FHIR.R4.Resources.Canonical.TFhirGraphDefinition;
  TFhirGraphDefinitionList = FHIR.R4.Resources.Canonical.TFhirGraphDefinitionList;
{$ENDIF FHIR_GRAPHDEFINITION}
{$IFDEF FHIR_GROUP}
  TFhirGroupCharacteristic = FHIR.R4.Resources.Admin.TFhirGroupCharacteristic;
  TFhirGroupCharacteristicList = FHIR.R4.Resources.Admin.TFhirGroupCharacteristicList;
  TFhirGroupMember = FHIR.R4.Resources.Admin.TFhirGroupMember;
  TFhirGroupMemberList = FHIR.R4.Resources.Admin.TFhirGroupMemberList;
  TFhirGroup = FHIR.R4.Resources.Admin.TFhirGroup;
  TFhirGroupList = FHIR.R4.Resources.Admin.TFhirGroupList;
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_GUIDANCERESPONSE}
  TFhirGuidanceResponse = FHIR.R4.Resources.Other.TFhirGuidanceResponse;
  TFhirGuidanceResponseList = FHIR.R4.Resources.Other.TFhirGuidanceResponseList;
{$ENDIF FHIR_GUIDANCERESPONSE}
{$IFDEF FHIR_HEALTHCARESERVICE}
  TFhirHealthcareServiceEligibility = FHIR.R4.Resources.Admin.TFhirHealthcareServiceEligibility;
  TFhirHealthcareServiceEligibilityList = FHIR.R4.Resources.Admin.TFhirHealthcareServiceEligibilityList;
  TFhirHealthcareServiceAvailableTime = FHIR.R4.Resources.Admin.TFhirHealthcareServiceAvailableTime;
  TFhirHealthcareServiceAvailableTimeList = FHIR.R4.Resources.Admin.TFhirHealthcareServiceAvailableTimeList;
  TFhirHealthcareServiceNotAvailable = FHIR.R4.Resources.Admin.TFhirHealthcareServiceNotAvailable;
  TFhirHealthcareServiceNotAvailableList = FHIR.R4.Resources.Admin.TFhirHealthcareServiceNotAvailableList;
  TFhirHealthcareService = FHIR.R4.Resources.Admin.TFhirHealthcareService;
  TFhirHealthcareServiceList = FHIR.R4.Resources.Admin.TFhirHealthcareServiceList;
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGSTUDY}
  TFhirImagingStudySeries = FHIR.R4.Resources.Clinical.TFhirImagingStudySeries;
  TFhirImagingStudySeriesList = FHIR.R4.Resources.Clinical.TFhirImagingStudySeriesList;
  TFhirImagingStudySeriesPerformer = FHIR.R4.Resources.Clinical.TFhirImagingStudySeriesPerformer;
  TFhirImagingStudySeriesPerformerList = FHIR.R4.Resources.Clinical.TFhirImagingStudySeriesPerformerList;
  TFhirImagingStudySeriesInstance = FHIR.R4.Resources.Clinical.TFhirImagingStudySeriesInstance;
  TFhirImagingStudySeriesInstanceList = FHIR.R4.Resources.Clinical.TFhirImagingStudySeriesInstanceList;
  TFhirImagingStudy = FHIR.R4.Resources.Clinical.TFhirImagingStudy;
  TFhirImagingStudyList = FHIR.R4.Resources.Clinical.TFhirImagingStudyList;
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  TFhirImmunizationPerformer = FHIR.R4.Resources.Clinical.TFhirImmunizationPerformer;
  TFhirImmunizationPerformerList = FHIR.R4.Resources.Clinical.TFhirImmunizationPerformerList;
  TFhirImmunizationEducation = FHIR.R4.Resources.Clinical.TFhirImmunizationEducation;
  TFhirImmunizationEducationList = FHIR.R4.Resources.Clinical.TFhirImmunizationEducationList;
  TFhirImmunizationReaction = FHIR.R4.Resources.Clinical.TFhirImmunizationReaction;
  TFhirImmunizationReactionList = FHIR.R4.Resources.Clinical.TFhirImmunizationReactionList;
  TFhirImmunizationProtocolApplied = FHIR.R4.Resources.Clinical.TFhirImmunizationProtocolApplied;
  TFhirImmunizationProtocolAppliedList = FHIR.R4.Resources.Clinical.TFhirImmunizationProtocolAppliedList;
  TFhirImmunization = FHIR.R4.Resources.Clinical.TFhirImmunization;
  TFhirImmunizationList = FHIR.R4.Resources.Clinical.TFhirImmunizationList;
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  TFhirImmunizationEvaluation = FHIR.R4.Resources.Clinical.TFhirImmunizationEvaluation;
  TFhirImmunizationEvaluationList = FHIR.R4.Resources.Clinical.TFhirImmunizationEvaluationList;
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  TFhirImmunizationRecommendationRecommendation = FHIR.R4.Resources.Clinical.TFhirImmunizationRecommendationRecommendation;
  TFhirImmunizationRecommendationRecommendationList = FHIR.R4.Resources.Clinical.TFhirImmunizationRecommendationRecommendationList;
  TFhirImmunizationRecommendationRecommendationDateCriterion = FHIR.R4.Resources.Clinical.TFhirImmunizationRecommendationRecommendationDateCriterion;
  TFhirImmunizationRecommendationRecommendationDateCriterionList = FHIR.R4.Resources.Clinical.TFhirImmunizationRecommendationRecommendationDateCriterionList;
  TFhirImmunizationRecommendation = FHIR.R4.Resources.Clinical.TFhirImmunizationRecommendation;
  TFhirImmunizationRecommendationList = FHIR.R4.Resources.Clinical.TFhirImmunizationRecommendationList;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  TFhirImplementationGuideDependsOn = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDependsOn;
  TFhirImplementationGuideDependsOnList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDependsOnList;
  TFhirImplementationGuideGlobal = FHIR.R4.Resources.Canonical.TFhirImplementationGuideGlobal;
  TFhirImplementationGuideGlobalList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideGlobalList;
  TFhirImplementationGuideDefinition = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinition;
  TFhirImplementationGuideDefinitionList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionList;
  TFhirImplementationGuideDefinitionGrouping = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionGrouping;
  TFhirImplementationGuideDefinitionGroupingList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionGroupingList;
  TFhirImplementationGuideDefinitionResource = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionResource;
  TFhirImplementationGuideDefinitionResourceList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionResourceList;
  TFhirImplementationGuideDefinitionPage = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionPage;
  TFhirImplementationGuideDefinitionPageList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionPageList;
  TFhirImplementationGuideDefinitionParameter = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionParameter;
  TFhirImplementationGuideDefinitionParameterList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionParameterList;
  TFhirImplementationGuideDefinitionTemplate = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionTemplate;
  TFhirImplementationGuideDefinitionTemplateList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideDefinitionTemplateList;
  TFhirImplementationGuideManifest = FHIR.R4.Resources.Canonical.TFhirImplementationGuideManifest;
  TFhirImplementationGuideManifestList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideManifestList;
  TFhirImplementationGuideManifestResource = FHIR.R4.Resources.Canonical.TFhirImplementationGuideManifestResource;
  TFhirImplementationGuideManifestResourceList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideManifestResourceList;
  TFhirImplementationGuideManifestPage = FHIR.R4.Resources.Canonical.TFhirImplementationGuideManifestPage;
  TFhirImplementationGuideManifestPageList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideManifestPageList;
  TFhirImplementationGuide = FHIR.R4.Resources.Canonical.TFhirImplementationGuide;
  TFhirImplementationGuideList = FHIR.R4.Resources.Canonical.TFhirImplementationGuideList;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_INSURANCEPLAN}
  TFhirInsurancePlanContact = FHIR.R4.Resources.Financial.TFhirInsurancePlanContact;
  TFhirInsurancePlanContactList = FHIR.R4.Resources.Financial.TFhirInsurancePlanContactList;
  TFhirInsurancePlanCoverage = FHIR.R4.Resources.Financial.TFhirInsurancePlanCoverage;
  TFhirInsurancePlanCoverageList = FHIR.R4.Resources.Financial.TFhirInsurancePlanCoverageList;
  TFhirInsurancePlanCoverageBenefit = FHIR.R4.Resources.Financial.TFhirInsurancePlanCoverageBenefit;
  TFhirInsurancePlanCoverageBenefitList = FHIR.R4.Resources.Financial.TFhirInsurancePlanCoverageBenefitList;
  TFhirInsurancePlanCoverageBenefitLimit = FHIR.R4.Resources.Financial.TFhirInsurancePlanCoverageBenefitLimit;
  TFhirInsurancePlanCoverageBenefitLimitList = FHIR.R4.Resources.Financial.TFhirInsurancePlanCoverageBenefitLimitList;
  TFhirInsurancePlanPlan = FHIR.R4.Resources.Financial.TFhirInsurancePlanPlan;
  TFhirInsurancePlanPlanList = FHIR.R4.Resources.Financial.TFhirInsurancePlanPlanList;
  TFhirInsurancePlanPlanGeneralCost = FHIR.R4.Resources.Financial.TFhirInsurancePlanPlanGeneralCost;
  TFhirInsurancePlanPlanGeneralCostList = FHIR.R4.Resources.Financial.TFhirInsurancePlanPlanGeneralCostList;
  TFhirInsurancePlanPlanSpecificCost = FHIR.R4.Resources.Financial.TFhirInsurancePlanPlanSpecificCost;
  TFhirInsurancePlanPlanSpecificCostList = FHIR.R4.Resources.Financial.TFhirInsurancePlanPlanSpecificCostList;
  TFhirInsurancePlanPlanSpecificCostBenefit = FHIR.R4.Resources.Financial.TFhirInsurancePlanPlanSpecificCostBenefit;
  TFhirInsurancePlanPlanSpecificCostBenefitList = FHIR.R4.Resources.Financial.TFhirInsurancePlanPlanSpecificCostBenefitList;
  TFhirInsurancePlanPlanSpecificCostBenefitCost = FHIR.R4.Resources.Financial.TFhirInsurancePlanPlanSpecificCostBenefitCost;
  TFhirInsurancePlanPlanSpecificCostBenefitCostList = FHIR.R4.Resources.Financial.TFhirInsurancePlanPlanSpecificCostBenefitCostList;
  TFhirInsurancePlan = FHIR.R4.Resources.Financial.TFhirInsurancePlan;
  TFhirInsurancePlanList = FHIR.R4.Resources.Financial.TFhirInsurancePlanList;
{$ENDIF FHIR_INSURANCEPLAN}
{$IFDEF FHIR_INVOICE}
  TFhirInvoiceParticipant = FHIR.R4.Resources.Financial.TFhirInvoiceParticipant;
  TFhirInvoiceParticipantList = FHIR.R4.Resources.Financial.TFhirInvoiceParticipantList;
  TFhirInvoiceLineItem = FHIR.R4.Resources.Financial.TFhirInvoiceLineItem;
  TFhirInvoiceLineItemList = FHIR.R4.Resources.Financial.TFhirInvoiceLineItemList;
  TFhirInvoiceLineItemPriceComponent = FHIR.R4.Resources.Financial.TFhirInvoiceLineItemPriceComponent;
  TFhirInvoiceLineItemPriceComponentList = FHIR.R4.Resources.Financial.TFhirInvoiceLineItemPriceComponentList;
  TFhirInvoice = FHIR.R4.Resources.Financial.TFhirInvoice;
  TFhirInvoiceList = FHIR.R4.Resources.Financial.TFhirInvoiceList;
{$ENDIF FHIR_INVOICE}
{$IFDEF FHIR_LIBRARY}
  TFhirLibrary = FHIR.R4.Resources.Canonical.TFhirLibrary;
  TFhirLibraryList = FHIR.R4.Resources.Canonical.TFhirLibraryList;
{$ENDIF FHIR_LIBRARY}
{$IFDEF FHIR_LINKAGE}
  TFhirLinkageItem = FHIR.R4.Resources.Other.TFhirLinkageItem;
  TFhirLinkageItemList = FHIR.R4.Resources.Other.TFhirLinkageItemList;
  TFhirLinkage = FHIR.R4.Resources.Other.TFhirLinkage;
  TFhirLinkageList = FHIR.R4.Resources.Other.TFhirLinkageList;
{$ENDIF FHIR_LINKAGE}
{$IFDEF FHIR_LIST}
  TFhirListEntry = FHIR.R4.Resources.Other.TFhirListEntry;
  TFhirListEntryList = FHIR.R4.Resources.Other.TFhirListEntryList;
  TFhirList = FHIR.R4.Resources.Other.TFhirList;
  TFhirListList = FHIR.R4.Resources.Other.TFhirListList;
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_LOCATION}
  TFhirLocationPosition = FHIR.R4.Resources.Admin.TFhirLocationPosition;
  TFhirLocationPositionList = FHIR.R4.Resources.Admin.TFhirLocationPositionList;
  TFhirLocationHoursOfOperation = FHIR.R4.Resources.Admin.TFhirLocationHoursOfOperation;
  TFhirLocationHoursOfOperationList = FHIR.R4.Resources.Admin.TFhirLocationHoursOfOperationList;
  TFhirLocation = FHIR.R4.Resources.Admin.TFhirLocation;
  TFhirLocationList = FHIR.R4.Resources.Admin.TFhirLocationList;
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_MEASURE}
  TFhirMeasureGroup = FHIR.R4.Resources.Canonical.TFhirMeasureGroup;
  TFhirMeasureGroupList = FHIR.R4.Resources.Canonical.TFhirMeasureGroupList;
  TFhirMeasureGroupPopulation = FHIR.R4.Resources.Canonical.TFhirMeasureGroupPopulation;
  TFhirMeasureGroupPopulationList = FHIR.R4.Resources.Canonical.TFhirMeasureGroupPopulationList;
  TFhirMeasureGroupStratifier = FHIR.R4.Resources.Canonical.TFhirMeasureGroupStratifier;
  TFhirMeasureGroupStratifierList = FHIR.R4.Resources.Canonical.TFhirMeasureGroupStratifierList;
  TFhirMeasureGroupStratifierComponent = FHIR.R4.Resources.Canonical.TFhirMeasureGroupStratifierComponent;
  TFhirMeasureGroupStratifierComponentList = FHIR.R4.Resources.Canonical.TFhirMeasureGroupStratifierComponentList;
  TFhirMeasureSupplementalData = FHIR.R4.Resources.Canonical.TFhirMeasureSupplementalData;
  TFhirMeasureSupplementalDataList = FHIR.R4.Resources.Canonical.TFhirMeasureSupplementalDataList;
  TFhirMeasure = FHIR.R4.Resources.Canonical.TFhirMeasure;
  TFhirMeasureList = FHIR.R4.Resources.Canonical.TFhirMeasureList;
{$ENDIF FHIR_MEASURE}
{$IFDEF FHIR_MEASUREREPORT}
  TFhirMeasureReportGroup = FHIR.R4.Resources.Other.TFhirMeasureReportGroup;
  TFhirMeasureReportGroupList = FHIR.R4.Resources.Other.TFhirMeasureReportGroupList;
  TFhirMeasureReportGroupPopulation = FHIR.R4.Resources.Other.TFhirMeasureReportGroupPopulation;
  TFhirMeasureReportGroupPopulationList = FHIR.R4.Resources.Other.TFhirMeasureReportGroupPopulationList;
  TFhirMeasureReportGroupStratifier = FHIR.R4.Resources.Other.TFhirMeasureReportGroupStratifier;
  TFhirMeasureReportGroupStratifierList = FHIR.R4.Resources.Other.TFhirMeasureReportGroupStratifierList;
  TFhirMeasureReportGroupStratifierStratum = FHIR.R4.Resources.Other.TFhirMeasureReportGroupStratifierStratum;
  TFhirMeasureReportGroupStratifierStratumList = FHIR.R4.Resources.Other.TFhirMeasureReportGroupStratifierStratumList;
  TFhirMeasureReportGroupStratifierStratumComponent = FHIR.R4.Resources.Other.TFhirMeasureReportGroupStratifierStratumComponent;
  TFhirMeasureReportGroupStratifierStratumComponentList = FHIR.R4.Resources.Other.TFhirMeasureReportGroupStratifierStratumComponentList;
  TFhirMeasureReportGroupStratifierStratumPopulation = FHIR.R4.Resources.Other.TFhirMeasureReportGroupStratifierStratumPopulation;
  TFhirMeasureReportGroupStratifierStratumPopulationList = FHIR.R4.Resources.Other.TFhirMeasureReportGroupStratifierStratumPopulationList;
  TFhirMeasureReport = FHIR.R4.Resources.Other.TFhirMeasureReport;
  TFhirMeasureReportList = FHIR.R4.Resources.Other.TFhirMeasureReportList;
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MEDIA}
  TFhirMedia = FHIR.R4.Resources.Clinical.TFhirMedia;
  TFhirMediaList = FHIR.R4.Resources.Clinical.TFhirMediaList;
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATION}
  TFhirMedicationIngredient = FHIR.R4.Resources.Medications.TFhirMedicationIngredient;
  TFhirMedicationIngredientList = FHIR.R4.Resources.Medications.TFhirMedicationIngredientList;
  TFhirMedicationBatch = FHIR.R4.Resources.Medications.TFhirMedicationBatch;
  TFhirMedicationBatchList = FHIR.R4.Resources.Medications.TFhirMedicationBatchList;
  TFhirMedication = FHIR.R4.Resources.Medications.TFhirMedication;
  TFhirMedicationList = FHIR.R4.Resources.Medications.TFhirMedicationList;
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  TFhirMedicationAdministrationPerformer = FHIR.R4.Resources.Clinical.TFhirMedicationAdministrationPerformer;
  TFhirMedicationAdministrationPerformerList = FHIR.R4.Resources.Clinical.TFhirMedicationAdministrationPerformerList;
  TFhirMedicationAdministrationDosage = FHIR.R4.Resources.Clinical.TFhirMedicationAdministrationDosage;
  TFhirMedicationAdministrationDosageList = FHIR.R4.Resources.Clinical.TFhirMedicationAdministrationDosageList;
  TFhirMedicationAdministration = FHIR.R4.Resources.Clinical.TFhirMedicationAdministration;
  TFhirMedicationAdministrationList = FHIR.R4.Resources.Clinical.TFhirMedicationAdministrationList;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  TFhirMedicationDispensePerformer = FHIR.R4.Resources.Clinical.TFhirMedicationDispensePerformer;
  TFhirMedicationDispensePerformerList = FHIR.R4.Resources.Clinical.TFhirMedicationDispensePerformerList;
  TFhirMedicationDispenseSubstitution = FHIR.R4.Resources.Clinical.TFhirMedicationDispenseSubstitution;
  TFhirMedicationDispenseSubstitutionList = FHIR.R4.Resources.Clinical.TFhirMedicationDispenseSubstitutionList;
  TFhirMedicationDispense = FHIR.R4.Resources.Clinical.TFhirMedicationDispense;
  TFhirMedicationDispenseList = FHIR.R4.Resources.Clinical.TFhirMedicationDispenseList;
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  TFhirMedicationKnowledgeRelatedMedicationKnowledge = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeRelatedMedicationKnowledge;
  TFhirMedicationKnowledgeRelatedMedicationKnowledgeList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeRelatedMedicationKnowledgeList;
  TFhirMedicationKnowledgeMonograph = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeMonograph;
  TFhirMedicationKnowledgeMonographList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeMonographList;
  TFhirMedicationKnowledgeIngredient = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeIngredient;
  TFhirMedicationKnowledgeIngredientList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeIngredientList;
  TFhirMedicationKnowledgeCost = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeCost;
  TFhirMedicationKnowledgeCostList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeCostList;
  TFhirMedicationKnowledgeMonitoringProgram = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeMonitoringProgram;
  TFhirMedicationKnowledgeMonitoringProgramList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeMonitoringProgramList;
  TFhirMedicationKnowledgeAdministrationGuidelines = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelines;
  TFhirMedicationKnowledgeAdministrationGuidelinesList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelinesList;
  TFhirMedicationKnowledgeAdministrationGuidelinesDosage = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelinesDosage;
  TFhirMedicationKnowledgeAdministrationGuidelinesDosageList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelinesDosageList;
  TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristics = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristics;
  TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsList;
  TFhirMedicationKnowledgeMedicineClassification = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeMedicineClassification;
  TFhirMedicationKnowledgeMedicineClassificationList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeMedicineClassificationList;
  TFhirMedicationKnowledgePackaging = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgePackaging;
  TFhirMedicationKnowledgePackagingList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgePackagingList;
  TFhirMedicationKnowledgeDrugCharacteristic = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeDrugCharacteristic;
  TFhirMedicationKnowledgeDrugCharacteristicList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeDrugCharacteristicList;
  TFhirMedicationKnowledgeRegulatory = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeRegulatory;
  TFhirMedicationKnowledgeRegulatoryList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeRegulatoryList;
  TFhirMedicationKnowledgeRegulatorySubstitution = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeRegulatorySubstitution;
  TFhirMedicationKnowledgeRegulatorySubstitutionList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeRegulatorySubstitutionList;
  TFhirMedicationKnowledgeRegulatorySchedule = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeRegulatorySchedule;
  TFhirMedicationKnowledgeRegulatoryScheduleList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeRegulatoryScheduleList;
  TFhirMedicationKnowledgeRegulatoryMaxDispense = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeRegulatoryMaxDispense;
  TFhirMedicationKnowledgeRegulatoryMaxDispenseList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeRegulatoryMaxDispenseList;
  TFhirMedicationKnowledgeKinetics = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeKinetics;
  TFhirMedicationKnowledgeKineticsList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeKineticsList;
  TFhirMedicationKnowledge = FHIR.R4.Resources.Medications.TFhirMedicationKnowledge;
  TFhirMedicationKnowledgeList = FHIR.R4.Resources.Medications.TFhirMedicationKnowledgeList;
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  TFhirMedicationRequestDispenseRequest = FHIR.R4.Resources.Clinical.TFhirMedicationRequestDispenseRequest;
  TFhirMedicationRequestDispenseRequestList = FHIR.R4.Resources.Clinical.TFhirMedicationRequestDispenseRequestList;
  TFhirMedicationRequestDispenseRequestInitialFill = FHIR.R4.Resources.Clinical.TFhirMedicationRequestDispenseRequestInitialFill;
  TFhirMedicationRequestDispenseRequestInitialFillList = FHIR.R4.Resources.Clinical.TFhirMedicationRequestDispenseRequestInitialFillList;
  TFhirMedicationRequestSubstitution = FHIR.R4.Resources.Clinical.TFhirMedicationRequestSubstitution;
  TFhirMedicationRequestSubstitutionList = FHIR.R4.Resources.Clinical.TFhirMedicationRequestSubstitutionList;
  TFhirMedicationRequest = FHIR.R4.Resources.Clinical.TFhirMedicationRequest;
  TFhirMedicationRequestList = FHIR.R4.Resources.Clinical.TFhirMedicationRequestList;
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  TFhirMedicationStatement = FHIR.R4.Resources.Clinical.TFhirMedicationStatement;
  TFhirMedicationStatementList = FHIR.R4.Resources.Clinical.TFhirMedicationStatementList;
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_MEDICINALPRODUCT}
  TFhirMedicinalProductName = FHIR.R4.Resources.Medications.TFhirMedicinalProductName;
  TFhirMedicinalProductNameList = FHIR.R4.Resources.Medications.TFhirMedicinalProductNameList;
  TFhirMedicinalProductNameNamePart = FHIR.R4.Resources.Medications.TFhirMedicinalProductNameNamePart;
  TFhirMedicinalProductNameNamePartList = FHIR.R4.Resources.Medications.TFhirMedicinalProductNameNamePartList;
  TFhirMedicinalProductNameCountryLanguage = FHIR.R4.Resources.Medications.TFhirMedicinalProductNameCountryLanguage;
  TFhirMedicinalProductNameCountryLanguageList = FHIR.R4.Resources.Medications.TFhirMedicinalProductNameCountryLanguageList;
  TFhirMedicinalProductManufacturingBusinessOperation = FHIR.R4.Resources.Medications.TFhirMedicinalProductManufacturingBusinessOperation;
  TFhirMedicinalProductManufacturingBusinessOperationList = FHIR.R4.Resources.Medications.TFhirMedicinalProductManufacturingBusinessOperationList;
  TFhirMedicinalProductSpecialDesignation = FHIR.R4.Resources.Medications.TFhirMedicinalProductSpecialDesignation;
  TFhirMedicinalProductSpecialDesignationList = FHIR.R4.Resources.Medications.TFhirMedicinalProductSpecialDesignationList;
  TFhirMedicinalProduct = FHIR.R4.Resources.Medications.TFhirMedicinalProduct;
  TFhirMedicinalProductList = FHIR.R4.Resources.Medications.TFhirMedicinalProductList;
{$ENDIF FHIR_MEDICINALPRODUCT}
{$IFDEF FHIR_MEDICINALPRODUCTAUTHORIZATION}
  TFhirMedicinalProductAuthorizationJurisdictionalAuthorization = FHIR.R4.Resources.Medications.TFhirMedicinalProductAuthorizationJurisdictionalAuthorization;
  TFhirMedicinalProductAuthorizationJurisdictionalAuthorizationList = FHIR.R4.Resources.Medications.TFhirMedicinalProductAuthorizationJurisdictionalAuthorizationList;
  TFhirMedicinalProductAuthorizationProcedure = FHIR.R4.Resources.Medications.TFhirMedicinalProductAuthorizationProcedure;
  TFhirMedicinalProductAuthorizationProcedureList = FHIR.R4.Resources.Medications.TFhirMedicinalProductAuthorizationProcedureList;
  TFhirMedicinalProductAuthorization = FHIR.R4.Resources.Medications.TFhirMedicinalProductAuthorization;
  TFhirMedicinalProductAuthorizationList = FHIR.R4.Resources.Medications.TFhirMedicinalProductAuthorizationList;
{$ENDIF FHIR_MEDICINALPRODUCTAUTHORIZATION}
{$IFDEF FHIR_MEDICINALPRODUCTCONTRAINDICATION}
  TFhirMedicinalProductContraindicationOtherTherapy = FHIR.R4.Resources.Medications.TFhirMedicinalProductContraindicationOtherTherapy;
  TFhirMedicinalProductContraindicationOtherTherapyList = FHIR.R4.Resources.Medications.TFhirMedicinalProductContraindicationOtherTherapyList;
  TFhirMedicinalProductContraindication = FHIR.R4.Resources.Medications.TFhirMedicinalProductContraindication;
  TFhirMedicinalProductContraindicationList = FHIR.R4.Resources.Medications.TFhirMedicinalProductContraindicationList;
{$ENDIF FHIR_MEDICINALPRODUCTCONTRAINDICATION}
{$IFDEF FHIR_MEDICINALPRODUCTINDICATION}
  TFhirMedicinalProductIndicationOtherTherapy = FHIR.R4.Resources.Medications.TFhirMedicinalProductIndicationOtherTherapy;
  TFhirMedicinalProductIndicationOtherTherapyList = FHIR.R4.Resources.Medications.TFhirMedicinalProductIndicationOtherTherapyList;
  TFhirMedicinalProductIndication = FHIR.R4.Resources.Medications.TFhirMedicinalProductIndication;
  TFhirMedicinalProductIndicationList = FHIR.R4.Resources.Medications.TFhirMedicinalProductIndicationList;
{$ENDIF FHIR_MEDICINALPRODUCTINDICATION}
{$IFDEF FHIR_MEDICINALPRODUCTINGREDIENT}
  TFhirMedicinalProductIngredientSpecifiedSubstance = FHIR.R4.Resources.Medications.TFhirMedicinalProductIngredientSpecifiedSubstance;
  TFhirMedicinalProductIngredientSpecifiedSubstanceList = FHIR.R4.Resources.Medications.TFhirMedicinalProductIngredientSpecifiedSubstanceList;
  TFhirMedicinalProductIngredientSpecifiedSubstanceStrength = FHIR.R4.Resources.Medications.TFhirMedicinalProductIngredientSpecifiedSubstanceStrength;
  TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthList = FHIR.R4.Resources.Medications.TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthList;
  TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength = FHIR.R4.Resources.Medications.TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength;
  TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthList = FHIR.R4.Resources.Medications.TFhirMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthList;
  TFhirMedicinalProductIngredientSubstance = FHIR.R4.Resources.Medications.TFhirMedicinalProductIngredientSubstance;
  TFhirMedicinalProductIngredientSubstanceList = FHIR.R4.Resources.Medications.TFhirMedicinalProductIngredientSubstanceList;
  TFhirMedicinalProductIngredient = FHIR.R4.Resources.Medications.TFhirMedicinalProductIngredient;
  TFhirMedicinalProductIngredientList = FHIR.R4.Resources.Medications.TFhirMedicinalProductIngredientList;
{$ENDIF FHIR_MEDICINALPRODUCTINGREDIENT}
{$IFDEF FHIR_MEDICINALPRODUCTINTERACTION}
  TFhirMedicinalProductInteractionInteractant = FHIR.R4.Resources.Medications.TFhirMedicinalProductInteractionInteractant;
  TFhirMedicinalProductInteractionInteractantList = FHIR.R4.Resources.Medications.TFhirMedicinalProductInteractionInteractantList;
  TFhirMedicinalProductInteraction = FHIR.R4.Resources.Medications.TFhirMedicinalProductInteraction;
  TFhirMedicinalProductInteractionList = FHIR.R4.Resources.Medications.TFhirMedicinalProductInteractionList;
{$ENDIF FHIR_MEDICINALPRODUCTINTERACTION}
{$IFDEF FHIR_MEDICINALPRODUCTMANUFACTURED}
  TFhirMedicinalProductManufactured = FHIR.R4.Resources.Medications.TFhirMedicinalProductManufactured;
  TFhirMedicinalProductManufacturedList = FHIR.R4.Resources.Medications.TFhirMedicinalProductManufacturedList;
{$ENDIF FHIR_MEDICINALPRODUCTMANUFACTURED}
{$IFDEF FHIR_MEDICINALPRODUCTPACKAGED}
  TFhirMedicinalProductPackagedBatchIdentifier = FHIR.R4.Resources.Medications.TFhirMedicinalProductPackagedBatchIdentifier;
  TFhirMedicinalProductPackagedBatchIdentifierList = FHIR.R4.Resources.Medications.TFhirMedicinalProductPackagedBatchIdentifierList;
  TFhirMedicinalProductPackagedPackageItem = FHIR.R4.Resources.Medications.TFhirMedicinalProductPackagedPackageItem;
  TFhirMedicinalProductPackagedPackageItemList = FHIR.R4.Resources.Medications.TFhirMedicinalProductPackagedPackageItemList;
  TFhirMedicinalProductPackaged = FHIR.R4.Resources.Medications.TFhirMedicinalProductPackaged;
  TFhirMedicinalProductPackagedList = FHIR.R4.Resources.Medications.TFhirMedicinalProductPackagedList;
{$ENDIF FHIR_MEDICINALPRODUCTPACKAGED}
{$IFDEF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}
  TFhirMedicinalProductPharmaceuticalCharacteristics = FHIR.R4.Resources.Medications.TFhirMedicinalProductPharmaceuticalCharacteristics;
  TFhirMedicinalProductPharmaceuticalCharacteristicsList = FHIR.R4.Resources.Medications.TFhirMedicinalProductPharmaceuticalCharacteristicsList;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministration = FHIR.R4.Resources.Medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministration;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministrationList = FHIR.R4.Resources.Medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministrationList;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies = FHIR.R4.Resources.Medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesList = FHIR.R4.Resources.Medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesList;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod = FHIR.R4.Resources.Medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod;
  TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodList = FHIR.R4.Resources.Medications.TFhirMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodList;
  TFhirMedicinalProductPharmaceutical = FHIR.R4.Resources.Medications.TFhirMedicinalProductPharmaceutical;
  TFhirMedicinalProductPharmaceuticalList = FHIR.R4.Resources.Medications.TFhirMedicinalProductPharmaceuticalList;
{$ENDIF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}
{$IFDEF FHIR_MEDICINALPRODUCTUNDESIRABLEEFFECT}
  TFhirMedicinalProductUndesirableEffect = FHIR.R4.Resources.Medications.TFhirMedicinalProductUndesirableEffect;
  TFhirMedicinalProductUndesirableEffectList = FHIR.R4.Resources.Medications.TFhirMedicinalProductUndesirableEffectList;
{$ENDIF FHIR_MEDICINALPRODUCTUNDESIRABLEEFFECT}
{$IFDEF FHIR_MESSAGEDEFINITION}
  TFhirMessageDefinitionFocus = FHIR.R4.Resources.Canonical.TFhirMessageDefinitionFocus;
  TFhirMessageDefinitionFocusList = FHIR.R4.Resources.Canonical.TFhirMessageDefinitionFocusList;
  TFhirMessageDefinitionAllowedResponse = FHIR.R4.Resources.Canonical.TFhirMessageDefinitionAllowedResponse;
  TFhirMessageDefinitionAllowedResponseList = FHIR.R4.Resources.Canonical.TFhirMessageDefinitionAllowedResponseList;
  TFhirMessageDefinition = FHIR.R4.Resources.Canonical.TFhirMessageDefinition;
  TFhirMessageDefinitionList = FHIR.R4.Resources.Canonical.TFhirMessageDefinitionList;
{$ENDIF FHIR_MESSAGEDEFINITION}
{$IFDEF FHIR_MESSAGEHEADER}
  TFhirMessageHeaderDestination = FHIR.R4.Resources.Other.TFhirMessageHeaderDestination;
  TFhirMessageHeaderDestinationList = FHIR.R4.Resources.Other.TFhirMessageHeaderDestinationList;
  TFhirMessageHeaderSource = FHIR.R4.Resources.Other.TFhirMessageHeaderSource;
  TFhirMessageHeaderSourceList = FHIR.R4.Resources.Other.TFhirMessageHeaderSourceList;
  TFhirMessageHeaderResponse = FHIR.R4.Resources.Other.TFhirMessageHeaderResponse;
  TFhirMessageHeaderResponseList = FHIR.R4.Resources.Other.TFhirMessageHeaderResponseList;
  TFhirMessageHeader = FHIR.R4.Resources.Other.TFhirMessageHeader;
  TFhirMessageHeaderList = FHIR.R4.Resources.Other.TFhirMessageHeaderList;
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_MOLECULARSEQUENCE}
  TFhirMolecularSequenceReferenceSeq = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceReferenceSeq;
  TFhirMolecularSequenceReferenceSeqList = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceReferenceSeqList;
  TFhirMolecularSequenceVariant = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceVariant;
  TFhirMolecularSequenceVariantList = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceVariantList;
  TFhirMolecularSequenceQuality = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceQuality;
  TFhirMolecularSequenceQualityList = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceQualityList;
  TFhirMolecularSequenceQualityRoc = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceQualityRoc;
  TFhirMolecularSequenceQualityRocList = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceQualityRocList;
  TFhirMolecularSequenceRepository = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceRepository;
  TFhirMolecularSequenceRepositoryList = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceRepositoryList;
  TFhirMolecularSequenceStructureVariant = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceStructureVariant;
  TFhirMolecularSequenceStructureVariantList = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceStructureVariantList;
  TFhirMolecularSequenceStructureVariantOuter = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceStructureVariantOuter;
  TFhirMolecularSequenceStructureVariantOuterList = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceStructureVariantOuterList;
  TFhirMolecularSequenceStructureVariantInner = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceStructureVariantInner;
  TFhirMolecularSequenceStructureVariantInnerList = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceStructureVariantInnerList;
  TFhirMolecularSequence = FHIR.R4.Resources.Clinical.TFhirMolecularSequence;
  TFhirMolecularSequenceList = FHIR.R4.Resources.Clinical.TFhirMolecularSequenceList;
{$ENDIF FHIR_MOLECULARSEQUENCE}
{$IFDEF FHIR_NAMINGSYSTEM}
  TFhirNamingSystemUniqueId = FHIR.R4.Resources.Canonical.TFhirNamingSystemUniqueId;
  TFhirNamingSystemUniqueIdList = FHIR.R4.Resources.Canonical.TFhirNamingSystemUniqueIdList;
  TFhirNamingSystem = FHIR.R4.Resources.Canonical.TFhirNamingSystem;
  TFhirNamingSystemList = FHIR.R4.Resources.Canonical.TFhirNamingSystemList;
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_NUTRITIONORDER}
  TFhirNutritionOrderOralDiet = FHIR.R4.Resources.Clinical.TFhirNutritionOrderOralDiet;
  TFhirNutritionOrderOralDietList = FHIR.R4.Resources.Clinical.TFhirNutritionOrderOralDietList;
  TFhirNutritionOrderOralDietNutrient = FHIR.R4.Resources.Clinical.TFhirNutritionOrderOralDietNutrient;
  TFhirNutritionOrderOralDietNutrientList = FHIR.R4.Resources.Clinical.TFhirNutritionOrderOralDietNutrientList;
  TFhirNutritionOrderOralDietTexture = FHIR.R4.Resources.Clinical.TFhirNutritionOrderOralDietTexture;
  TFhirNutritionOrderOralDietTextureList = FHIR.R4.Resources.Clinical.TFhirNutritionOrderOralDietTextureList;
  TFhirNutritionOrderSupplement = FHIR.R4.Resources.Clinical.TFhirNutritionOrderSupplement;
  TFhirNutritionOrderSupplementList = FHIR.R4.Resources.Clinical.TFhirNutritionOrderSupplementList;
  TFhirNutritionOrderEnteralFormula = FHIR.R4.Resources.Clinical.TFhirNutritionOrderEnteralFormula;
  TFhirNutritionOrderEnteralFormulaList = FHIR.R4.Resources.Clinical.TFhirNutritionOrderEnteralFormulaList;
  TFhirNutritionOrderEnteralFormulaAdministration = FHIR.R4.Resources.Clinical.TFhirNutritionOrderEnteralFormulaAdministration;
  TFhirNutritionOrderEnteralFormulaAdministrationList = FHIR.R4.Resources.Clinical.TFhirNutritionOrderEnteralFormulaAdministrationList;
  TFhirNutritionOrder = FHIR.R4.Resources.Clinical.TFhirNutritionOrder;
  TFhirNutritionOrderList = FHIR.R4.Resources.Clinical.TFhirNutritionOrderList;
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_OBSERVATION}
  TFhirObservationReferenceRange = FHIR.R4.Resources.Clinical.TFhirObservationReferenceRange;
  TFhirObservationReferenceRangeList = FHIR.R4.Resources.Clinical.TFhirObservationReferenceRangeList;
  TFhirObservationComponent = FHIR.R4.Resources.Clinical.TFhirObservationComponent;
  TFhirObservationComponentList = FHIR.R4.Resources.Clinical.TFhirObservationComponentList;
  TFhirObservation = FHIR.R4.Resources.Clinical.TFhirObservation;
  TFhirObservationList = FHIR.R4.Resources.Clinical.TFhirObservationList;
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OBSERVATIONDEFINITION}
  TFhirObservationDefinitionQuantitativeDetails = FHIR.R4.Resources.Canonical.TFhirObservationDefinitionQuantitativeDetails;
  TFhirObservationDefinitionQuantitativeDetailsList = FHIR.R4.Resources.Canonical.TFhirObservationDefinitionQuantitativeDetailsList;
  TFhirObservationDefinitionQualifiedInterval = FHIR.R4.Resources.Canonical.TFhirObservationDefinitionQualifiedInterval;
  TFhirObservationDefinitionQualifiedIntervalList = FHIR.R4.Resources.Canonical.TFhirObservationDefinitionQualifiedIntervalList;
  TFhirObservationDefinition = FHIR.R4.Resources.Canonical.TFhirObservationDefinition;
  TFhirObservationDefinitionList = FHIR.R4.Resources.Canonical.TFhirObservationDefinitionList;
{$ENDIF FHIR_OBSERVATIONDEFINITION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  TFhirOperationDefinitionParameter = FHIR.R4.Resources.Canonical.TFhirOperationDefinitionParameter;
  TFhirOperationDefinitionParameterList = FHIR.R4.Resources.Canonical.TFhirOperationDefinitionParameterList;
  TFhirOperationDefinitionParameterBinding = FHIR.R4.Resources.Canonical.TFhirOperationDefinitionParameterBinding;
  TFhirOperationDefinitionParameterBindingList = FHIR.R4.Resources.Canonical.TFhirOperationDefinitionParameterBindingList;
  TFhirOperationDefinitionParameterReferencedFrom = FHIR.R4.Resources.Canonical.TFhirOperationDefinitionParameterReferencedFrom;
  TFhirOperationDefinitionParameterReferencedFromList = FHIR.R4.Resources.Canonical.TFhirOperationDefinitionParameterReferencedFromList;
  TFhirOperationDefinitionOverload = FHIR.R4.Resources.Canonical.TFhirOperationDefinitionOverload;
  TFhirOperationDefinitionOverloadList = FHIR.R4.Resources.Canonical.TFhirOperationDefinitionOverloadList;
  TFhirOperationDefinition = FHIR.R4.Resources.Canonical.TFhirOperationDefinition;
  TFhirOperationDefinitionList = FHIR.R4.Resources.Canonical.TFhirOperationDefinitionList;
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  TFhirOperationOutcomeIssue = FHIR.R4.Resources.Other.TFhirOperationOutcomeIssue;
  TFhirOperationOutcomeIssueList = FHIR.R4.Resources.Other.TFhirOperationOutcomeIssueList;
  TFhirOperationOutcome = FHIR.R4.Resources.Other.TFhirOperationOutcome;
  TFhirOperationOutcomeList = FHIR.R4.Resources.Other.TFhirOperationOutcomeList;
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_ORGANIZATION}
  TFhirOrganizationContact = FHIR.R4.Resources.Admin.TFhirOrganizationContact;
  TFhirOrganizationContactList = FHIR.R4.Resources.Admin.TFhirOrganizationContactList;
  TFhirOrganization = FHIR.R4.Resources.Admin.TFhirOrganization;
  TFhirOrganizationList = FHIR.R4.Resources.Admin.TFhirOrganizationList;
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  TFhirOrganizationAffiliation = FHIR.R4.Resources.Admin.TFhirOrganizationAffiliation;
  TFhirOrganizationAffiliationList = FHIR.R4.Resources.Admin.TFhirOrganizationAffiliationList;
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}
{$IFDEF FHIR_PATIENT}
  TFhirPatientContact = FHIR.R4.Resources.Admin.TFhirPatientContact;
  TFhirPatientContactList = FHIR.R4.Resources.Admin.TFhirPatientContactList;
  TFhirPatientCommunication = FHIR.R4.Resources.Admin.TFhirPatientCommunication;
  TFhirPatientCommunicationList = FHIR.R4.Resources.Admin.TFhirPatientCommunicationList;
  TFhirPatientLink = FHIR.R4.Resources.Admin.TFhirPatientLink;
  TFhirPatientLinkList = FHIR.R4.Resources.Admin.TFhirPatientLinkList;
  TFhirPatient = FHIR.R4.Resources.Admin.TFhirPatient;
  TFhirPatientList = FHIR.R4.Resources.Admin.TFhirPatientList;
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PAYMENTNOTICE}
  TFhirPaymentNotice = FHIR.R4.Resources.Financial.TFhirPaymentNotice;
  TFhirPaymentNoticeList = FHIR.R4.Resources.Financial.TFhirPaymentNoticeList;
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  TFhirPaymentReconciliationDetail = FHIR.R4.Resources.Financial.TFhirPaymentReconciliationDetail;
  TFhirPaymentReconciliationDetailList = FHIR.R4.Resources.Financial.TFhirPaymentReconciliationDetailList;
  TFhirPaymentReconciliationProcessNote = FHIR.R4.Resources.Financial.TFhirPaymentReconciliationProcessNote;
  TFhirPaymentReconciliationProcessNoteList = FHIR.R4.Resources.Financial.TFhirPaymentReconciliationProcessNoteList;
  TFhirPaymentReconciliation = FHIR.R4.Resources.Financial.TFhirPaymentReconciliation;
  TFhirPaymentReconciliationList = FHIR.R4.Resources.Financial.TFhirPaymentReconciliationList;
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERSON}
  TFhirPersonLink = FHIR.R4.Resources.Admin.TFhirPersonLink;
  TFhirPersonLinkList = FHIR.R4.Resources.Admin.TFhirPersonLinkList;
  TFhirPerson = FHIR.R4.Resources.Admin.TFhirPerson;
  TFhirPersonList = FHIR.R4.Resources.Admin.TFhirPersonList;
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PLANDEFINITION}
  TFhirPlanDefinitionGoal = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionGoal;
  TFhirPlanDefinitionGoalList = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionGoalList;
  TFhirPlanDefinitionGoalTarget = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionGoalTarget;
  TFhirPlanDefinitionGoalTargetList = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionGoalTargetList;
  TFhirPlanDefinitionAction = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionAction;
  TFhirPlanDefinitionActionList = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionActionList;
  TFhirPlanDefinitionActionCondition = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionActionCondition;
  TFhirPlanDefinitionActionConditionList = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionActionConditionList;
  TFhirPlanDefinitionActionRelatedAction = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionActionRelatedAction;
  TFhirPlanDefinitionActionRelatedActionList = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionActionRelatedActionList;
  TFhirPlanDefinitionActionParticipant = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionActionParticipant;
  TFhirPlanDefinitionActionParticipantList = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionActionParticipantList;
  TFhirPlanDefinitionActionDynamicValue = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionActionDynamicValue;
  TFhirPlanDefinitionActionDynamicValueList = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionActionDynamicValueList;
  TFhirPlanDefinition = FHIR.R4.Resources.Canonical.TFhirPlanDefinition;
  TFhirPlanDefinitionList = FHIR.R4.Resources.Canonical.TFhirPlanDefinitionList;
{$ENDIF FHIR_PLANDEFINITION}
{$IFDEF FHIR_PRACTITIONER}
  TFhirPractitionerQualification = FHIR.R4.Resources.Admin.TFhirPractitionerQualification;
  TFhirPractitionerQualificationList = FHIR.R4.Resources.Admin.TFhirPractitionerQualificationList;
  TFhirPractitioner = FHIR.R4.Resources.Admin.TFhirPractitioner;
  TFhirPractitionerList = FHIR.R4.Resources.Admin.TFhirPractitionerList;
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PRACTITIONERROLE}
  TFhirPractitionerRoleAvailableTime = FHIR.R4.Resources.Admin.TFhirPractitionerRoleAvailableTime;
  TFhirPractitionerRoleAvailableTimeList = FHIR.R4.Resources.Admin.TFhirPractitionerRoleAvailableTimeList;
  TFhirPractitionerRoleNotAvailable = FHIR.R4.Resources.Admin.TFhirPractitionerRoleNotAvailable;
  TFhirPractitionerRoleNotAvailableList = FHIR.R4.Resources.Admin.TFhirPractitionerRoleNotAvailableList;
  TFhirPractitionerRole = FHIR.R4.Resources.Admin.TFhirPractitionerRole;
  TFhirPractitionerRoleList = FHIR.R4.Resources.Admin.TFhirPractitionerRoleList;
{$ENDIF FHIR_PRACTITIONERROLE}
{$IFDEF FHIR_PROCEDURE}
  TFhirProcedurePerformer = FHIR.R4.Resources.Clinical.TFhirProcedurePerformer;
  TFhirProcedurePerformerList = FHIR.R4.Resources.Clinical.TFhirProcedurePerformerList;
  TFhirProcedureFocalDevice = FHIR.R4.Resources.Clinical.TFhirProcedureFocalDevice;
  TFhirProcedureFocalDeviceList = FHIR.R4.Resources.Clinical.TFhirProcedureFocalDeviceList;
  TFhirProcedure = FHIR.R4.Resources.Clinical.TFhirProcedure;
  TFhirProcedureList = FHIR.R4.Resources.Clinical.TFhirProcedureList;
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROVENANCE}
  TFhirProvenanceAgent = FHIR.R4.Resources.Other.TFhirProvenanceAgent;
  TFhirProvenanceAgentList = FHIR.R4.Resources.Other.TFhirProvenanceAgentList;
  TFhirProvenanceEntity = FHIR.R4.Resources.Other.TFhirProvenanceEntity;
  TFhirProvenanceEntityList = FHIR.R4.Resources.Other.TFhirProvenanceEntityList;
  TFhirProvenance = FHIR.R4.Resources.Other.TFhirProvenance;
  TFhirProvenanceList = FHIR.R4.Resources.Other.TFhirProvenanceList;
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  TFhirQuestionnaireItem = FHIR.R4.Resources.Canonical.TFhirQuestionnaireItem;
  TFhirQuestionnaireItemList = FHIR.R4.Resources.Canonical.TFhirQuestionnaireItemList;
  TFhirQuestionnaireItemEnableWhen = FHIR.R4.Resources.Canonical.TFhirQuestionnaireItemEnableWhen;
  TFhirQuestionnaireItemEnableWhenList = FHIR.R4.Resources.Canonical.TFhirQuestionnaireItemEnableWhenList;
  TFhirQuestionnaireItemAnswerOption = FHIR.R4.Resources.Canonical.TFhirQuestionnaireItemAnswerOption;
  TFhirQuestionnaireItemAnswerOptionList = FHIR.R4.Resources.Canonical.TFhirQuestionnaireItemAnswerOptionList;
  TFhirQuestionnaireItemInitial = FHIR.R4.Resources.Canonical.TFhirQuestionnaireItemInitial;
  TFhirQuestionnaireItemInitialList = FHIR.R4.Resources.Canonical.TFhirQuestionnaireItemInitialList;
  TFhirQuestionnaire = FHIR.R4.Resources.Canonical.TFhirQuestionnaire;
  TFhirQuestionnaireList = FHIR.R4.Resources.Canonical.TFhirQuestionnaireList;
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  TFhirQuestionnaireResponseItem = FHIR.R4.Resources.Clinical.TFhirQuestionnaireResponseItem;
  TFhirQuestionnaireResponseItemList = FHIR.R4.Resources.Clinical.TFhirQuestionnaireResponseItemList;
  TFhirQuestionnaireResponseItemAnswer = FHIR.R4.Resources.Clinical.TFhirQuestionnaireResponseItemAnswer;
  TFhirQuestionnaireResponseItemAnswerList = FHIR.R4.Resources.Clinical.TFhirQuestionnaireResponseItemAnswerList;
  TFhirQuestionnaireResponse = FHIR.R4.Resources.Clinical.TFhirQuestionnaireResponse;
  TFhirQuestionnaireResponseList = FHIR.R4.Resources.Clinical.TFhirQuestionnaireResponseList;
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_RELATEDPERSON}
  TFhirRelatedPersonCommunication = FHIR.R4.Resources.Admin.TFhirRelatedPersonCommunication;
  TFhirRelatedPersonCommunicationList = FHIR.R4.Resources.Admin.TFhirRelatedPersonCommunicationList;
  TFhirRelatedPerson = FHIR.R4.Resources.Admin.TFhirRelatedPerson;
  TFhirRelatedPersonList = FHIR.R4.Resources.Admin.TFhirRelatedPersonList;
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_REQUESTGROUP}
  TFhirRequestGroupAction = FHIR.R4.Resources.Other.TFhirRequestGroupAction;
  TFhirRequestGroupActionList = FHIR.R4.Resources.Other.TFhirRequestGroupActionList;
  TFhirRequestGroupActionCondition = FHIR.R4.Resources.Other.TFhirRequestGroupActionCondition;
  TFhirRequestGroupActionConditionList = FHIR.R4.Resources.Other.TFhirRequestGroupActionConditionList;
  TFhirRequestGroupActionRelatedAction = FHIR.R4.Resources.Other.TFhirRequestGroupActionRelatedAction;
  TFhirRequestGroupActionRelatedActionList = FHIR.R4.Resources.Other.TFhirRequestGroupActionRelatedActionList;
  TFhirRequestGroup = FHIR.R4.Resources.Other.TFhirRequestGroup;
  TFhirRequestGroupList = FHIR.R4.Resources.Other.TFhirRequestGroupList;
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_RESEARCHDEFINITION}
  TFhirResearchDefinition = FHIR.R4.Resources.Other.TFhirResearchDefinition;
  TFhirResearchDefinitionList = FHIR.R4.Resources.Other.TFhirResearchDefinitionList;
{$ENDIF FHIR_RESEARCHDEFINITION}
{$IFDEF FHIR_RESEARCHELEMENTDEFINITION}
  TFhirResearchElementDefinitionCharacteristic = FHIR.R4.Resources.Other.TFhirResearchElementDefinitionCharacteristic;
  TFhirResearchElementDefinitionCharacteristicList = FHIR.R4.Resources.Other.TFhirResearchElementDefinitionCharacteristicList;
  TFhirResearchElementDefinition = FHIR.R4.Resources.Other.TFhirResearchElementDefinition;
  TFhirResearchElementDefinitionList = FHIR.R4.Resources.Other.TFhirResearchElementDefinitionList;
{$ENDIF FHIR_RESEARCHELEMENTDEFINITION}
{$IFDEF FHIR_RESEARCHSTUDY}
  TFhirResearchStudyArm = FHIR.R4.Resources.Other.TFhirResearchStudyArm;
  TFhirResearchStudyArmList = FHIR.R4.Resources.Other.TFhirResearchStudyArmList;
  TFhirResearchStudyObjective = FHIR.R4.Resources.Other.TFhirResearchStudyObjective;
  TFhirResearchStudyObjectiveList = FHIR.R4.Resources.Other.TFhirResearchStudyObjectiveList;
  TFhirResearchStudy = FHIR.R4.Resources.Other.TFhirResearchStudy;
  TFhirResearchStudyList = FHIR.R4.Resources.Other.TFhirResearchStudyList;
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  TFhirResearchSubject = FHIR.R4.Resources.Other.TFhirResearchSubject;
  TFhirResearchSubjectList = FHIR.R4.Resources.Other.TFhirResearchSubjectList;
{$ENDIF FHIR_RESEARCHSUBJECT}
{$IFDEF FHIR_RISKASSESSMENT}
  TFhirRiskAssessmentPrediction = FHIR.R4.Resources.Clinical.TFhirRiskAssessmentPrediction;
  TFhirRiskAssessmentPredictionList = FHIR.R4.Resources.Clinical.TFhirRiskAssessmentPredictionList;
  TFhirRiskAssessment = FHIR.R4.Resources.Clinical.TFhirRiskAssessment;
  TFhirRiskAssessmentList = FHIR.R4.Resources.Clinical.TFhirRiskAssessmentList;
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_RISKEVIDENCESYNTHESIS}
  TFhirRiskEvidenceSynthesisSampleSize = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisSampleSize;
  TFhirRiskEvidenceSynthesisSampleSizeList = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisSampleSizeList;
  TFhirRiskEvidenceSynthesisRiskEstimate = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisRiskEstimate;
  TFhirRiskEvidenceSynthesisRiskEstimateList = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisRiskEstimateList;
  TFhirRiskEvidenceSynthesisRiskEstimatePrecisionEstimate = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisRiskEstimatePrecisionEstimate;
  TFhirRiskEvidenceSynthesisRiskEstimatePrecisionEstimateList = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisRiskEstimatePrecisionEstimateList;
  TFhirRiskEvidenceSynthesisCertainty = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisCertainty;
  TFhirRiskEvidenceSynthesisCertaintyList = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisCertaintyList;
  TFhirRiskEvidenceSynthesisCertaintyCertaintySubcomponent = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisCertaintyCertaintySubcomponent;
  TFhirRiskEvidenceSynthesisCertaintyCertaintySubcomponentList = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisCertaintyCertaintySubcomponentList;
  TFhirRiskEvidenceSynthesis = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesis;
  TFhirRiskEvidenceSynthesisList = FHIR.R4.Resources.Other.TFhirRiskEvidenceSynthesisList;
{$ENDIF FHIR_RISKEVIDENCESYNTHESIS}
{$IFDEF FHIR_SCHEDULE}
  TFhirSchedule = FHIR.R4.Resources.Admin.TFhirSchedule;
  TFhirScheduleList = FHIR.R4.Resources.Admin.TFhirScheduleList;
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SEARCHPARAMETER}
  TFhirSearchParameterComponent = FHIR.R4.Resources.Canonical.TFhirSearchParameterComponent;
  TFhirSearchParameterComponentList = FHIR.R4.Resources.Canonical.TFhirSearchParameterComponentList;
  TFhirSearchParameter = FHIR.R4.Resources.Canonical.TFhirSearchParameter;
  TFhirSearchParameterList = FHIR.R4.Resources.Canonical.TFhirSearchParameterList;
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SERVICEREQUEST}
  TFhirServiceRequest = FHIR.R4.Resources.Canonical.TFhirServiceRequest;
  TFhirServiceRequestList = FHIR.R4.Resources.Canonical.TFhirServiceRequestList;
{$ENDIF FHIR_SERVICEREQUEST}
{$IFDEF FHIR_SLOT}
  TFhirSlot = FHIR.R4.Resources.Admin.TFhirSlot;
  TFhirSlotList = FHIR.R4.Resources.Admin.TFhirSlotList;
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  TFhirSpecimenCollection = FHIR.R4.Resources.Clinical.TFhirSpecimenCollection;
  TFhirSpecimenCollectionList = FHIR.R4.Resources.Clinical.TFhirSpecimenCollectionList;
  TFhirSpecimenProcessing = FHIR.R4.Resources.Clinical.TFhirSpecimenProcessing;
  TFhirSpecimenProcessingList = FHIR.R4.Resources.Clinical.TFhirSpecimenProcessingList;
  TFhirSpecimenContainer = FHIR.R4.Resources.Clinical.TFhirSpecimenContainer;
  TFhirSpecimenContainerList = FHIR.R4.Resources.Clinical.TFhirSpecimenContainerList;
  TFhirSpecimen = FHIR.R4.Resources.Clinical.TFhirSpecimen;
  TFhirSpecimenList = FHIR.R4.Resources.Clinical.TFhirSpecimenList;
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_SPECIMENDEFINITION}
  TFhirSpecimenDefinitionTypeTested = FHIR.R4.Resources.Canonical.TFhirSpecimenDefinitionTypeTested;
  TFhirSpecimenDefinitionTypeTestedList = FHIR.R4.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedList;
  TFhirSpecimenDefinitionTypeTestedContainer = FHIR.R4.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedContainer;
  TFhirSpecimenDefinitionTypeTestedContainerList = FHIR.R4.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedContainerList;
  TFhirSpecimenDefinitionTypeTestedContainerAdditive = FHIR.R4.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedContainerAdditive;
  TFhirSpecimenDefinitionTypeTestedContainerAdditiveList = FHIR.R4.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedContainerAdditiveList;
  TFhirSpecimenDefinitionTypeTestedHandling = FHIR.R4.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedHandling;
  TFhirSpecimenDefinitionTypeTestedHandlingList = FHIR.R4.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedHandlingList;
  TFhirSpecimenDefinition = FHIR.R4.Resources.Canonical.TFhirSpecimenDefinition;
  TFhirSpecimenDefinitionList = FHIR.R4.Resources.Canonical.TFhirSpecimenDefinitionList;
{$ENDIF FHIR_SPECIMENDEFINITION}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  TFhirStructureDefinitionMapping = FHIR.R4.Resources.Canonical.TFhirStructureDefinitionMapping;
  TFhirStructureDefinitionMappingList = FHIR.R4.Resources.Canonical.TFhirStructureDefinitionMappingList;
  TFhirStructureDefinitionContext = FHIR.R4.Resources.Canonical.TFhirStructureDefinitionContext;
  TFhirStructureDefinitionContextList = FHIR.R4.Resources.Canonical.TFhirStructureDefinitionContextList;
  TFhirStructureDefinitionSnapshot = FHIR.R4.Resources.Canonical.TFhirStructureDefinitionSnapshot;
  TFhirStructureDefinitionSnapshotList = FHIR.R4.Resources.Canonical.TFhirStructureDefinitionSnapshotList;
  TFhirStructureDefinitionDifferential = FHIR.R4.Resources.Canonical.TFhirStructureDefinitionDifferential;
  TFhirStructureDefinitionDifferentialList = FHIR.R4.Resources.Canonical.TFhirStructureDefinitionDifferentialList;
  TFhirStructureDefinition = FHIR.R4.Resources.Canonical.TFhirStructureDefinition;
  TFhirStructureDefinitionList = FHIR.R4.Resources.Canonical.TFhirStructureDefinitionList;
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_STRUCTUREMAP}
  TFhirStructureMapStructure = FHIR.R4.Resources.Canonical.TFhirStructureMapStructure;
  TFhirStructureMapStructureList = FHIR.R4.Resources.Canonical.TFhirStructureMapStructureList;
  TFhirStructureMapGroup = FHIR.R4.Resources.Canonical.TFhirStructureMapGroup;
  TFhirStructureMapGroupList = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupList;
  TFhirStructureMapGroupInput = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupInput;
  TFhirStructureMapGroupInputList = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupInputList;
  TFhirStructureMapGroupRule = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupRule;
  TFhirStructureMapGroupRuleList = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupRuleList;
  TFhirStructureMapGroupRuleSource = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupRuleSource;
  TFhirStructureMapGroupRuleSourceList = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupRuleSourceList;
  TFhirStructureMapGroupRuleTarget = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupRuleTarget;
  TFhirStructureMapGroupRuleTargetList = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupRuleTargetList;
  TFhirStructureMapGroupRuleTargetParameter = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupRuleTargetParameter;
  TFhirStructureMapGroupRuleTargetParameterList = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupRuleTargetParameterList;
  TFhirStructureMapGroupRuleDependent = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupRuleDependent;
  TFhirStructureMapGroupRuleDependentList = FHIR.R4.Resources.Canonical.TFhirStructureMapGroupRuleDependentList;
  TFhirStructureMap = FHIR.R4.Resources.Canonical.TFhirStructureMap;
  TFhirStructureMapList = FHIR.R4.Resources.Canonical.TFhirStructureMapList;
{$ENDIF FHIR_STRUCTUREMAP}
{$IFDEF FHIR_SUBSCRIPTION}
  TFhirSubscriptionChannel = FHIR.R4.Resources.Other.TFhirSubscriptionChannel;
  TFhirSubscriptionChannelList = FHIR.R4.Resources.Other.TFhirSubscriptionChannelList;
  TFhirSubscription = FHIR.R4.Resources.Other.TFhirSubscription;
  TFhirSubscriptionList = FHIR.R4.Resources.Other.TFhirSubscriptionList;
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSTANCE}
  TFhirSubstanceInstance = FHIR.R4.Resources.Medications.TFhirSubstanceInstance;
  TFhirSubstanceInstanceList = FHIR.R4.Resources.Medications.TFhirSubstanceInstanceList;
  TFhirSubstanceIngredient = FHIR.R4.Resources.Medications.TFhirSubstanceIngredient;
  TFhirSubstanceIngredientList = FHIR.R4.Resources.Medications.TFhirSubstanceIngredientList;
  TFhirSubstance = FHIR.R4.Resources.Medications.TFhirSubstance;
  TFhirSubstanceList = FHIR.R4.Resources.Medications.TFhirSubstanceList;
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUBSTANCENUCLEICACID}
  TFhirSubstanceNucleicAcidSubunit = FHIR.R4.Resources.Medications.TFhirSubstanceNucleicAcidSubunit;
  TFhirSubstanceNucleicAcidSubunitList = FHIR.R4.Resources.Medications.TFhirSubstanceNucleicAcidSubunitList;
  TFhirSubstanceNucleicAcidSubunitLinkage = FHIR.R4.Resources.Medications.TFhirSubstanceNucleicAcidSubunitLinkage;
  TFhirSubstanceNucleicAcidSubunitLinkageList = FHIR.R4.Resources.Medications.TFhirSubstanceNucleicAcidSubunitLinkageList;
  TFhirSubstanceNucleicAcidSubunitSugar = FHIR.R4.Resources.Medications.TFhirSubstanceNucleicAcidSubunitSugar;
  TFhirSubstanceNucleicAcidSubunitSugarList = FHIR.R4.Resources.Medications.TFhirSubstanceNucleicAcidSubunitSugarList;
  TFhirSubstanceNucleicAcid = FHIR.R4.Resources.Medications.TFhirSubstanceNucleicAcid;
  TFhirSubstanceNucleicAcidList = FHIR.R4.Resources.Medications.TFhirSubstanceNucleicAcidList;
{$ENDIF FHIR_SUBSTANCENUCLEICACID}
{$IFDEF FHIR_SUBSTANCEPOLYMER}
  TFhirSubstancePolymerMonomerSet = FHIR.R4.Resources.Medications.TFhirSubstancePolymerMonomerSet;
  TFhirSubstancePolymerMonomerSetList = FHIR.R4.Resources.Medications.TFhirSubstancePolymerMonomerSetList;
  TFhirSubstancePolymerMonomerSetStartingMaterial = FHIR.R4.Resources.Medications.TFhirSubstancePolymerMonomerSetStartingMaterial;
  TFhirSubstancePolymerMonomerSetStartingMaterialList = FHIR.R4.Resources.Medications.TFhirSubstancePolymerMonomerSetStartingMaterialList;
  TFhirSubstancePolymerRepeat = FHIR.R4.Resources.Medications.TFhirSubstancePolymerRepeat;
  TFhirSubstancePolymerRepeatList = FHIR.R4.Resources.Medications.TFhirSubstancePolymerRepeatList;
  TFhirSubstancePolymerRepeatRepeatUnit = FHIR.R4.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnit;
  TFhirSubstancePolymerRepeatRepeatUnitList = FHIR.R4.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnitList;
  TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation = FHIR.R4.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation;
  TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationList = FHIR.R4.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationList;
  TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentation = FHIR.R4.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentation;
  TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentationList = FHIR.R4.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentationList;
  TFhirSubstancePolymer = FHIR.R4.Resources.Medications.TFhirSubstancePolymer;
  TFhirSubstancePolymerList = FHIR.R4.Resources.Medications.TFhirSubstancePolymerList;
{$ENDIF FHIR_SUBSTANCEPOLYMER}
{$IFDEF FHIR_SUBSTANCEPROTEIN}
  TFhirSubstanceProteinSubunit = FHIR.R4.Resources.Medications.TFhirSubstanceProteinSubunit;
  TFhirSubstanceProteinSubunitList = FHIR.R4.Resources.Medications.TFhirSubstanceProteinSubunitList;
  TFhirSubstanceProtein = FHIR.R4.Resources.Medications.TFhirSubstanceProtein;
  TFhirSubstanceProteinList = FHIR.R4.Resources.Medications.TFhirSubstanceProteinList;
{$ENDIF FHIR_SUBSTANCEPROTEIN}
{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  TFhirSubstanceReferenceInformationGene = FHIR.R4.Resources.Medications.TFhirSubstanceReferenceInformationGene;
  TFhirSubstanceReferenceInformationGeneList = FHIR.R4.Resources.Medications.TFhirSubstanceReferenceInformationGeneList;
  TFhirSubstanceReferenceInformationGeneElement = FHIR.R4.Resources.Medications.TFhirSubstanceReferenceInformationGeneElement;
  TFhirSubstanceReferenceInformationGeneElementList = FHIR.R4.Resources.Medications.TFhirSubstanceReferenceInformationGeneElementList;
  TFhirSubstanceReferenceInformationClassification = FHIR.R4.Resources.Medications.TFhirSubstanceReferenceInformationClassification;
  TFhirSubstanceReferenceInformationClassificationList = FHIR.R4.Resources.Medications.TFhirSubstanceReferenceInformationClassificationList;
  TFhirSubstanceReferenceInformationTarget = FHIR.R4.Resources.Medications.TFhirSubstanceReferenceInformationTarget;
  TFhirSubstanceReferenceInformationTargetList = FHIR.R4.Resources.Medications.TFhirSubstanceReferenceInformationTargetList;
  TFhirSubstanceReferenceInformation = FHIR.R4.Resources.Medications.TFhirSubstanceReferenceInformation;
  TFhirSubstanceReferenceInformationList = FHIR.R4.Resources.Medications.TFhirSubstanceReferenceInformationList;
{$ENDIF FHIR_SUBSTANCEREFERENCEINFORMATION}
{$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
  TFhirSubstanceSourceMaterialFractionDescription = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialFractionDescription;
  TFhirSubstanceSourceMaterialFractionDescriptionList = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialFractionDescriptionList;
  TFhirSubstanceSourceMaterialOrganism = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialOrganism;
  TFhirSubstanceSourceMaterialOrganismList = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialOrganismList;
  TFhirSubstanceSourceMaterialOrganismAuthor = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialOrganismAuthor;
  TFhirSubstanceSourceMaterialOrganismAuthorList = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialOrganismAuthorList;
  TFhirSubstanceSourceMaterialOrganismHybrid = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialOrganismHybrid;
  TFhirSubstanceSourceMaterialOrganismHybridList = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialOrganismHybridList;
  TFhirSubstanceSourceMaterialOrganismOrganismGeneral = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialOrganismOrganismGeneral;
  TFhirSubstanceSourceMaterialOrganismOrganismGeneralList = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialOrganismOrganismGeneralList;
  TFhirSubstanceSourceMaterialPartDescription = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialPartDescription;
  TFhirSubstanceSourceMaterialPartDescriptionList = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialPartDescriptionList;
  TFhirSubstanceSourceMaterial = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterial;
  TFhirSubstanceSourceMaterialList = FHIR.R4.Resources.Medications.TFhirSubstanceSourceMaterialList;
{$ENDIF FHIR_SUBSTANCESOURCEMATERIAL}
{$IFDEF FHIR_SUBSTANCESPECIFICATION}
  TFhirSubstanceSpecificationMoiety = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationMoiety;
  TFhirSubstanceSpecificationMoietyList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationMoietyList;
  TFhirSubstanceSpecificationProperty = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationProperty;
  TFhirSubstanceSpecificationPropertyList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationPropertyList;
  TFhirSubstanceSpecificationStructure = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationStructure;
  TFhirSubstanceSpecificationStructureList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationStructureList;
  TFhirSubstanceSpecificationStructureIsotope = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationStructureIsotope;
  TFhirSubstanceSpecificationStructureIsotopeList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationStructureIsotopeList;
  TFhirSubstanceSpecificationStructureIsotopeMolecularWeight = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationStructureIsotopeMolecularWeight;
  TFhirSubstanceSpecificationStructureIsotopeMolecularWeightList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationStructureIsotopeMolecularWeightList;
  TFhirSubstanceSpecificationStructureRepresentation = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationStructureRepresentation;
  TFhirSubstanceSpecificationStructureRepresentationList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationStructureRepresentationList;
  TFhirSubstanceSpecificationCode = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationCode;
  TFhirSubstanceSpecificationCodeList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationCodeList;
  TFhirSubstanceSpecificationName = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationName;
  TFhirSubstanceSpecificationNameList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationNameList;
  TFhirSubstanceSpecificationNameOfficial = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationNameOfficial;
  TFhirSubstanceSpecificationNameOfficialList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationNameOfficialList;
  TFhirSubstanceSpecificationRelationship = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationRelationship;
  TFhirSubstanceSpecificationRelationshipList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationRelationshipList;
  TFhirSubstanceSpecification = FHIR.R4.Resources.Medications.TFhirSubstanceSpecification;
  TFhirSubstanceSpecificationList = FHIR.R4.Resources.Medications.TFhirSubstanceSpecificationList;
{$ENDIF FHIR_SUBSTANCESPECIFICATION}
{$IFDEF FHIR_SUPPLYDELIVERY}
  TFhirSupplyDeliverySuppliedItem = FHIR.R4.Resources.Clinical.TFhirSupplyDeliverySuppliedItem;
  TFhirSupplyDeliverySuppliedItemList = FHIR.R4.Resources.Clinical.TFhirSupplyDeliverySuppliedItemList;
  TFhirSupplyDelivery = FHIR.R4.Resources.Clinical.TFhirSupplyDelivery;
  TFhirSupplyDeliveryList = FHIR.R4.Resources.Clinical.TFhirSupplyDeliveryList;
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  TFhirSupplyRequestParameter = FHIR.R4.Resources.Clinical.TFhirSupplyRequestParameter;
  TFhirSupplyRequestParameterList = FHIR.R4.Resources.Clinical.TFhirSupplyRequestParameterList;
  TFhirSupplyRequest = FHIR.R4.Resources.Clinical.TFhirSupplyRequest;
  TFhirSupplyRequestList = FHIR.R4.Resources.Clinical.TFhirSupplyRequestList;
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TASK}
  TFhirTaskRestriction = FHIR.R4.Resources.Other.TFhirTaskRestriction;
  TFhirTaskRestrictionList = FHIR.R4.Resources.Other.TFhirTaskRestrictionList;
  TFhirTaskInput = FHIR.R4.Resources.Other.TFhirTaskInput;
  TFhirTaskInputList = FHIR.R4.Resources.Other.TFhirTaskInputList;
  TFhirTaskOutput = FHIR.R4.Resources.Other.TFhirTaskOutput;
  TFhirTaskOutputList = FHIR.R4.Resources.Other.TFhirTaskOutputList;
  TFhirTask = FHIR.R4.Resources.Other.TFhirTask;
  TFhirTaskList = FHIR.R4.Resources.Other.TFhirTaskList;
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  TFhirTerminologyCapabilitiesSoftware = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesSoftware;
  TFhirTerminologyCapabilitiesSoftwareList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesSoftwareList;
  TFhirTerminologyCapabilitiesImplementation = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesImplementation;
  TFhirTerminologyCapabilitiesImplementationList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesImplementationList;
  TFhirTerminologyCapabilitiesCodeSystem = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystem;
  TFhirTerminologyCapabilitiesCodeSystemList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystemList;
  TFhirTerminologyCapabilitiesCodeSystemVersion = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystemVersion;
  TFhirTerminologyCapabilitiesCodeSystemVersionList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystemVersionList;
  TFhirTerminologyCapabilitiesCodeSystemVersionFilter = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystemVersionFilter;
  TFhirTerminologyCapabilitiesCodeSystemVersionFilterList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystemVersionFilterList;
  TFhirTerminologyCapabilitiesExpansion = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesExpansion;
  TFhirTerminologyCapabilitiesExpansionList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesExpansionList;
  TFhirTerminologyCapabilitiesExpansionParameter = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesExpansionParameter;
  TFhirTerminologyCapabilitiesExpansionParameterList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesExpansionParameterList;
  TFhirTerminologyCapabilitiesValidateCode = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesValidateCode;
  TFhirTerminologyCapabilitiesValidateCodeList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesValidateCodeList;
  TFhirTerminologyCapabilitiesTranslation = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesTranslation;
  TFhirTerminologyCapabilitiesTranslationList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesTranslationList;
  TFhirTerminologyCapabilitiesClosure = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesClosure;
  TFhirTerminologyCapabilitiesClosureList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesClosureList;
  TFhirTerminologyCapabilities = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilities;
  TFhirTerminologyCapabilitiesList = FHIR.R4.Resources.Canonical.TFhirTerminologyCapabilitiesList;
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}
{$IFDEF FHIR_TESTREPORT}
  TFhirTestReportParticipant = FHIR.R4.Resources.Other.TFhirTestReportParticipant;
  TFhirTestReportParticipantList = FHIR.R4.Resources.Other.TFhirTestReportParticipantList;
  TFhirTestReportSetup = FHIR.R4.Resources.Other.TFhirTestReportSetup;
  TFhirTestReportSetupList = FHIR.R4.Resources.Other.TFhirTestReportSetupList;
  TFhirTestReportSetupAction = FHIR.R4.Resources.Other.TFhirTestReportSetupAction;
  TFhirTestReportSetupActionList = FHIR.R4.Resources.Other.TFhirTestReportSetupActionList;
  TFhirTestReportSetupActionOperation = FHIR.R4.Resources.Other.TFhirTestReportSetupActionOperation;
  TFhirTestReportSetupActionOperationList = FHIR.R4.Resources.Other.TFhirTestReportSetupActionOperationList;
  TFhirTestReportSetupActionAssert = FHIR.R4.Resources.Other.TFhirTestReportSetupActionAssert;
  TFhirTestReportSetupActionAssertList = FHIR.R4.Resources.Other.TFhirTestReportSetupActionAssertList;
  TFhirTestReportTest = FHIR.R4.Resources.Other.TFhirTestReportTest;
  TFhirTestReportTestList = FHIR.R4.Resources.Other.TFhirTestReportTestList;
  TFhirTestReportTestAction = FHIR.R4.Resources.Other.TFhirTestReportTestAction;
  TFhirTestReportTestActionList = FHIR.R4.Resources.Other.TFhirTestReportTestActionList;
  TFhirTestReportTeardown = FHIR.R4.Resources.Other.TFhirTestReportTeardown;
  TFhirTestReportTeardownList = FHIR.R4.Resources.Other.TFhirTestReportTeardownList;
  TFhirTestReportTeardownAction = FHIR.R4.Resources.Other.TFhirTestReportTeardownAction;
  TFhirTestReportTeardownActionList = FHIR.R4.Resources.Other.TFhirTestReportTeardownActionList;
  TFhirTestReport = FHIR.R4.Resources.Other.TFhirTestReport;
  TFhirTestReportList = FHIR.R4.Resources.Other.TFhirTestReportList;
{$ENDIF FHIR_TESTREPORT}
{$IFDEF FHIR_TESTSCRIPT}
  TFhirTestScriptOrigin = FHIR.R4.Resources.Canonical.TFhirTestScriptOrigin;
  TFhirTestScriptOriginList = FHIR.R4.Resources.Canonical.TFhirTestScriptOriginList;
  TFhirTestScriptDestination = FHIR.R4.Resources.Canonical.TFhirTestScriptDestination;
  TFhirTestScriptDestinationList = FHIR.R4.Resources.Canonical.TFhirTestScriptDestinationList;
  TFhirTestScriptMetadata = FHIR.R4.Resources.Canonical.TFhirTestScriptMetadata;
  TFhirTestScriptMetadataList = FHIR.R4.Resources.Canonical.TFhirTestScriptMetadataList;
  TFhirTestScriptMetadataLink = FHIR.R4.Resources.Canonical.TFhirTestScriptMetadataLink;
  TFhirTestScriptMetadataLinkList = FHIR.R4.Resources.Canonical.TFhirTestScriptMetadataLinkList;
  TFhirTestScriptMetadataCapability = FHIR.R4.Resources.Canonical.TFhirTestScriptMetadataCapability;
  TFhirTestScriptMetadataCapabilityList = FHIR.R4.Resources.Canonical.TFhirTestScriptMetadataCapabilityList;
  TFhirTestScriptFixture = FHIR.R4.Resources.Canonical.TFhirTestScriptFixture;
  TFhirTestScriptFixtureList = FHIR.R4.Resources.Canonical.TFhirTestScriptFixtureList;
  TFhirTestScriptVariable = FHIR.R4.Resources.Canonical.TFhirTestScriptVariable;
  TFhirTestScriptVariableList = FHIR.R4.Resources.Canonical.TFhirTestScriptVariableList;
  TFhirTestScriptSetup = FHIR.R4.Resources.Canonical.TFhirTestScriptSetup;
  TFhirTestScriptSetupList = FHIR.R4.Resources.Canonical.TFhirTestScriptSetupList;
  TFhirTestScriptSetupAction = FHIR.R4.Resources.Canonical.TFhirTestScriptSetupAction;
  TFhirTestScriptSetupActionList = FHIR.R4.Resources.Canonical.TFhirTestScriptSetupActionList;
  TFhirTestScriptSetupActionOperation = FHIR.R4.Resources.Canonical.TFhirTestScriptSetupActionOperation;
  TFhirTestScriptSetupActionOperationList = FHIR.R4.Resources.Canonical.TFhirTestScriptSetupActionOperationList;
  TFhirTestScriptSetupActionOperationRequestHeader = FHIR.R4.Resources.Canonical.TFhirTestScriptSetupActionOperationRequestHeader;
  TFhirTestScriptSetupActionOperationRequestHeaderList = FHIR.R4.Resources.Canonical.TFhirTestScriptSetupActionOperationRequestHeaderList;
  TFhirTestScriptSetupActionAssert = FHIR.R4.Resources.Canonical.TFhirTestScriptSetupActionAssert;
  TFhirTestScriptSetupActionAssertList = FHIR.R4.Resources.Canonical.TFhirTestScriptSetupActionAssertList;
  TFhirTestScriptTest = FHIR.R4.Resources.Canonical.TFhirTestScriptTest;
  TFhirTestScriptTestList = FHIR.R4.Resources.Canonical.TFhirTestScriptTestList;
  TFhirTestScriptTestAction = FHIR.R4.Resources.Canonical.TFhirTestScriptTestAction;
  TFhirTestScriptTestActionList = FHIR.R4.Resources.Canonical.TFhirTestScriptTestActionList;
  TFhirTestScriptTeardown = FHIR.R4.Resources.Canonical.TFhirTestScriptTeardown;
  TFhirTestScriptTeardownList = FHIR.R4.Resources.Canonical.TFhirTestScriptTeardownList;
  TFhirTestScriptTeardownAction = FHIR.R4.Resources.Canonical.TFhirTestScriptTeardownAction;
  TFhirTestScriptTeardownActionList = FHIR.R4.Resources.Canonical.TFhirTestScriptTeardownActionList;
  TFhirTestScript = FHIR.R4.Resources.Canonical.TFhirTestScript;
  TFhirTestScriptList = FHIR.R4.Resources.Canonical.TFhirTestScriptList;
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  TFhirValueSetCompose = FHIR.R4.Resources.Canonical.TFhirValueSetCompose;
  TFhirValueSetComposeList = FHIR.R4.Resources.Canonical.TFhirValueSetComposeList;
  TFhirValueSetComposeInclude = FHIR.R4.Resources.Canonical.TFhirValueSetComposeInclude;
  TFhirValueSetComposeIncludeList = FHIR.R4.Resources.Canonical.TFhirValueSetComposeIncludeList;
  TFhirValueSetComposeIncludeConcept = FHIR.R4.Resources.Canonical.TFhirValueSetComposeIncludeConcept;
  TFhirValueSetComposeIncludeConceptList = FHIR.R4.Resources.Canonical.TFhirValueSetComposeIncludeConceptList;
  TFhirValueSetComposeIncludeConceptDesignation = FHIR.R4.Resources.Canonical.TFhirValueSetComposeIncludeConceptDesignation;
  TFhirValueSetComposeIncludeConceptDesignationList = FHIR.R4.Resources.Canonical.TFhirValueSetComposeIncludeConceptDesignationList;
  TFhirValueSetComposeIncludeFilter = FHIR.R4.Resources.Canonical.TFhirValueSetComposeIncludeFilter;
  TFhirValueSetComposeIncludeFilterList = FHIR.R4.Resources.Canonical.TFhirValueSetComposeIncludeFilterList;
  TFhirValueSetExpansion = FHIR.R4.Resources.Canonical.TFhirValueSetExpansion;
  TFhirValueSetExpansionList = FHIR.R4.Resources.Canonical.TFhirValueSetExpansionList;
  TFhirValueSetExpansionParameter = FHIR.R4.Resources.Canonical.TFhirValueSetExpansionParameter;
  TFhirValueSetExpansionParameterList = FHIR.R4.Resources.Canonical.TFhirValueSetExpansionParameterList;
  TFhirValueSetExpansionContains = FHIR.R4.Resources.Canonical.TFhirValueSetExpansionContains;
  TFhirValueSetExpansionContainsList = FHIR.R4.Resources.Canonical.TFhirValueSetExpansionContainsList;
  TFhirValueSet = FHIR.R4.Resources.Canonical.TFhirValueSet;
  TFhirValueSetList = FHIR.R4.Resources.Canonical.TFhirValueSetList;
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VERIFICATIONRESULT}
  TFhirVerificationResultPrimarySource = FHIR.R4.Resources.Other.TFhirVerificationResultPrimarySource;
  TFhirVerificationResultPrimarySourceList = FHIR.R4.Resources.Other.TFhirVerificationResultPrimarySourceList;
  TFhirVerificationResultAttestation = FHIR.R4.Resources.Other.TFhirVerificationResultAttestation;
  TFhirVerificationResultAttestationList = FHIR.R4.Resources.Other.TFhirVerificationResultAttestationList;
  TFhirVerificationResultValidator = FHIR.R4.Resources.Other.TFhirVerificationResultValidator;
  TFhirVerificationResultValidatorList = FHIR.R4.Resources.Other.TFhirVerificationResultValidatorList;
  TFhirVerificationResult = FHIR.R4.Resources.Other.TFhirVerificationResult;
  TFhirVerificationResultList = FHIR.R4.Resources.Other.TFhirVerificationResultList;
{$ENDIF FHIR_VERIFICATIONRESULT}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  TFhirVisionPrescriptionLensSpecification = FHIR.R4.Resources.Clinical.TFhirVisionPrescriptionLensSpecification;
  TFhirVisionPrescriptionLensSpecificationList = FHIR.R4.Resources.Clinical.TFhirVisionPrescriptionLensSpecificationList;
  TFhirVisionPrescriptionLensSpecificationPrism = FHIR.R4.Resources.Clinical.TFhirVisionPrescriptionLensSpecificationPrism;
  TFhirVisionPrescriptionLensSpecificationPrismList = FHIR.R4.Resources.Clinical.TFhirVisionPrescriptionLensSpecificationPrismList;
  TFhirVisionPrescription = FHIR.R4.Resources.Clinical.TFhirVisionPrescription;
  TFhirVisionPrescriptionList = FHIR.R4.Resources.Clinical.TFhirVisionPrescriptionList;
{$ENDIF FHIR_VISIONPRESCRIPTION}

implementation

end.

