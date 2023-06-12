unit fhir5_resources;

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

{$i fhir.inc}
{$i fhir5.inc}

interface

// Generated on Thu, Nov 10, 2022 for FHIR v5.0.0



uses
  SysUtils, Classes, 
  fsl_base, fsl_utilities, fsl_stream, 
  fhir_objects, fhir_utilities,  
  fhir5_base, fhir5_enums, fhir5_types, fhir5_resources_base,
  fhir5_resources_admin, fhir5_resources_canonical, fhir5_resources_clinical, fhir5_resources_financial, fhir5_resources_medications, fhir5_resources_other;

type
 TFhirResourceType = fhir5_resources_base.TFhirResourceType;
  TFhirResourceTypeSet = fhir5_resources_base.TFhirResourceTypeSet;
  TFhirResource = fhir5_resources_base.TFhirResource;
  TFhirResourceClass = fhir5_resources_base.TFhirResourceClass;
  TFhirResourceList = fhir5_resources_base.TFhirResourceList;
  TFhirDomainResource = fhir5_resources_base.TFhirDomainResource;

  TFhirCanonicalResource = fhir5_resources_canonical.TFhirCanonicalResource;
  TFhirMetadataResource = fhir5_resources_canonical.TFhirMetadataResource;
  
{$IFDEF FHIR_ACCOUNT}
  TFhirAccountCoverage = fhir5_resources_financial.TFhirAccountCoverage;
  TFhirAccountCoverageList = fhir5_resources_financial.TFhirAccountCoverageList;
  TFhirAccountGuarantor = fhir5_resources_financial.TFhirAccountGuarantor;
  TFhirAccountGuarantorList = fhir5_resources_financial.TFhirAccountGuarantorList;
  TFhirAccountRelatedAccount = fhir5_resources_financial.TFhirAccountRelatedAccount;
  TFhirAccountRelatedAccountList = fhir5_resources_financial.TFhirAccountRelatedAccountList;
  TFhirAccountBalance = fhir5_resources_financial.TFhirAccountBalance;
  TFhirAccountBalanceList = fhir5_resources_financial.TFhirAccountBalanceList;
  TFhirAccount = fhir5_resources_financial.TFhirAccount;
  TFhirAccountList = fhir5_resources_financial.TFhirAccountList;
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  TFhirActivityDefinitionParticipant = fhir5_resources_other.TFhirActivityDefinitionParticipant;
  TFhirActivityDefinitionParticipantList = fhir5_resources_other.TFhirActivityDefinitionParticipantList;
  TFhirActivityDefinitionDynamicValue = fhir5_resources_other.TFhirActivityDefinitionDynamicValue;
  TFhirActivityDefinitionDynamicValueList = fhir5_resources_other.TFhirActivityDefinitionDynamicValueList;
  TFhirActivityDefinition = fhir5_resources_other.TFhirActivityDefinition;
  TFhirActivityDefinitionList = fhir5_resources_other.TFhirActivityDefinitionList;
{$ENDIF FHIR_ACTIVITYDEFINITION}
{$IFDEF FHIR_ACTORDEFINITION}
  TFhirActorDefinition = fhir5_resources_canonical.TFhirActorDefinition;
  TFhirActorDefinitionList = fhir5_resources_canonical.TFhirActorDefinitionList;
{$ENDIF FHIR_ACTORDEFINITION}
{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
  TFhirAdministrableProductDefinitionProperty = fhir5_resources_medications.TFhirAdministrableProductDefinitionProperty;
  TFhirAdministrableProductDefinitionPropertyList = fhir5_resources_medications.TFhirAdministrableProductDefinitionPropertyList;
  TFhirAdministrableProductDefinitionRouteOfAdministration = fhir5_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministration;
  TFhirAdministrableProductDefinitionRouteOfAdministrationList = fhir5_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministrationList;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpecies = fhir5_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpecies;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesList = fhir5_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesList;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriod = fhir5_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriod;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriodList = fhir5_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriodList;
  TFhirAdministrableProductDefinition = fhir5_resources_medications.TFhirAdministrableProductDefinition;
  TFhirAdministrableProductDefinitionList = fhir5_resources_medications.TFhirAdministrableProductDefinitionList;
{$ENDIF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
{$IFDEF FHIR_ADVERSEEVENT}
  TFhirAdverseEventParticipant = fhir5_resources_clinical.TFhirAdverseEventParticipant;
  TFhirAdverseEventParticipantList = fhir5_resources_clinical.TFhirAdverseEventParticipantList;
  TFhirAdverseEventSuspectEntity = fhir5_resources_clinical.TFhirAdverseEventSuspectEntity;
  TFhirAdverseEventSuspectEntityList = fhir5_resources_clinical.TFhirAdverseEventSuspectEntityList;
  TFhirAdverseEventSuspectEntityCausality = fhir5_resources_clinical.TFhirAdverseEventSuspectEntityCausality;
  TFhirAdverseEventSuspectEntityCausalityList = fhir5_resources_clinical.TFhirAdverseEventSuspectEntityCausalityList;
  TFhirAdverseEventContributingFactor = fhir5_resources_clinical.TFhirAdverseEventContributingFactor;
  TFhirAdverseEventContributingFactorList = fhir5_resources_clinical.TFhirAdverseEventContributingFactorList;
  TFhirAdverseEventPreventiveAction = fhir5_resources_clinical.TFhirAdverseEventPreventiveAction;
  TFhirAdverseEventPreventiveActionList = fhir5_resources_clinical.TFhirAdverseEventPreventiveActionList;
  TFhirAdverseEventMitigatingAction = fhir5_resources_clinical.TFhirAdverseEventMitigatingAction;
  TFhirAdverseEventMitigatingActionList = fhir5_resources_clinical.TFhirAdverseEventMitigatingActionList;
  TFhirAdverseEventSupportingInfo = fhir5_resources_clinical.TFhirAdverseEventSupportingInfo;
  TFhirAdverseEventSupportingInfoList = fhir5_resources_clinical.TFhirAdverseEventSupportingInfoList;
  TFhirAdverseEvent = fhir5_resources_clinical.TFhirAdverseEvent;
  TFhirAdverseEventList = fhir5_resources_clinical.TFhirAdverseEventList;
{$ENDIF FHIR_ADVERSEEVENT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  TFhirAllergyIntoleranceParticipant = fhir5_resources_clinical.TFhirAllergyIntoleranceParticipant;
  TFhirAllergyIntoleranceParticipantList = fhir5_resources_clinical.TFhirAllergyIntoleranceParticipantList;
  TFhirAllergyIntoleranceReaction = fhir5_resources_clinical.TFhirAllergyIntoleranceReaction;
  TFhirAllergyIntoleranceReactionList = fhir5_resources_clinical.TFhirAllergyIntoleranceReactionList;
  TFhirAllergyIntolerance = fhir5_resources_clinical.TFhirAllergyIntolerance;
  TFhirAllergyIntoleranceList = fhir5_resources_clinical.TFhirAllergyIntoleranceList;
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  TFhirAppointmentParticipant = fhir5_resources_clinical.TFhirAppointmentParticipant;
  TFhirAppointmentParticipantList = fhir5_resources_clinical.TFhirAppointmentParticipantList;
  TFhirAppointmentRecurrenceTemplate = fhir5_resources_clinical.TFhirAppointmentRecurrenceTemplate;
  TFhirAppointmentRecurrenceTemplateList = fhir5_resources_clinical.TFhirAppointmentRecurrenceTemplateList;
  TFhirAppointmentRecurrenceTemplateWeeklyTemplate = fhir5_resources_clinical.TFhirAppointmentRecurrenceTemplateWeeklyTemplate;
  TFhirAppointmentRecurrenceTemplateWeeklyTemplateList = fhir5_resources_clinical.TFhirAppointmentRecurrenceTemplateWeeklyTemplateList;
  TFhirAppointmentRecurrenceTemplateMonthlyTemplate = fhir5_resources_clinical.TFhirAppointmentRecurrenceTemplateMonthlyTemplate;
  TFhirAppointmentRecurrenceTemplateMonthlyTemplateList = fhir5_resources_clinical.TFhirAppointmentRecurrenceTemplateMonthlyTemplateList;
  TFhirAppointmentRecurrenceTemplateYearlyTemplate = fhir5_resources_clinical.TFhirAppointmentRecurrenceTemplateYearlyTemplate;
  TFhirAppointmentRecurrenceTemplateYearlyTemplateList = fhir5_resources_clinical.TFhirAppointmentRecurrenceTemplateYearlyTemplateList;
  TFhirAppointment = fhir5_resources_clinical.TFhirAppointment;
  TFhirAppointmentList = fhir5_resources_clinical.TFhirAppointmentList;
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  TFhirAppointmentResponse = fhir5_resources_clinical.TFhirAppointmentResponse;
  TFhirAppointmentResponseList = fhir5_resources_clinical.TFhirAppointmentResponseList;
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_ARTIFACTASSESSMENT}
  TFhirArtifactAssessmentContent = fhir5_resources_other.TFhirArtifactAssessmentContent;
  TFhirArtifactAssessmentContentList = fhir5_resources_other.TFhirArtifactAssessmentContentList;
  TFhirArtifactAssessment = fhir5_resources_other.TFhirArtifactAssessment;
  TFhirArtifactAssessmentList = fhir5_resources_other.TFhirArtifactAssessmentList;
{$ENDIF FHIR_ARTIFACTASSESSMENT}
{$IFDEF FHIR_AUDITEVENT}
  TFhirAuditEventOutcome = fhir5_resources_other.TFhirAuditEventOutcome;
  TFhirAuditEventOutcomeList = fhir5_resources_other.TFhirAuditEventOutcomeList;
  TFhirAuditEventAgent = fhir5_resources_other.TFhirAuditEventAgent;
  TFhirAuditEventAgentList = fhir5_resources_other.TFhirAuditEventAgentList;
  TFhirAuditEventSource = fhir5_resources_other.TFhirAuditEventSource;
  TFhirAuditEventSourceList = fhir5_resources_other.TFhirAuditEventSourceList;
  TFhirAuditEventEntity = fhir5_resources_other.TFhirAuditEventEntity;
  TFhirAuditEventEntityList = fhir5_resources_other.TFhirAuditEventEntityList;
  TFhirAuditEventEntityDetail = fhir5_resources_other.TFhirAuditEventEntityDetail;
  TFhirAuditEventEntityDetailList = fhir5_resources_other.TFhirAuditEventEntityDetailList;
  TFhirAuditEvent = fhir5_resources_other.TFhirAuditEvent;
  TFhirAuditEventList = fhir5_resources_other.TFhirAuditEventList;
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BASIC}
  TFhirBasic = fhir5_resources_clinical.TFhirBasic;
  TFhirBasicList = fhir5_resources_clinical.TFhirBasicList;
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BINARY}
  TFhirBinary = fhir5_resources_other.TFhirBinary;
  TFhirBinaryList = fhir5_resources_other.TFhirBinaryList;
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  TFhirBiologicallyDerivedProductCollection = fhir5_resources_clinical.TFhirBiologicallyDerivedProductCollection;
  TFhirBiologicallyDerivedProductCollectionList = fhir5_resources_clinical.TFhirBiologicallyDerivedProductCollectionList;
  TFhirBiologicallyDerivedProductProperty = fhir5_resources_clinical.TFhirBiologicallyDerivedProductProperty;
  TFhirBiologicallyDerivedProductPropertyList = fhir5_resources_clinical.TFhirBiologicallyDerivedProductPropertyList;
  TFhirBiologicallyDerivedProduct = fhir5_resources_clinical.TFhirBiologicallyDerivedProduct;
  TFhirBiologicallyDerivedProductList = fhir5_resources_clinical.TFhirBiologicallyDerivedProductList;
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
{$IFDEF FHIR_BODYSTRUCTURE}
  TFhirBodyStructureIncludedStructure = fhir5_resources_clinical.TFhirBodyStructureIncludedStructure;
  TFhirBodyStructureIncludedStructureList = fhir5_resources_clinical.TFhirBodyStructureIncludedStructureList;
  TFhirBodyStructureExcludedStructure = fhir5_resources_clinical.TFhirBodyStructureExcludedStructure;
  TFhirBodyStructureExcludedStructureList = fhir5_resources_clinical.TFhirBodyStructureExcludedStructureList;
  TFhirBodyStructure = fhir5_resources_clinical.TFhirBodyStructure;
  TFhirBodyStructureList = fhir5_resources_clinical.TFhirBodyStructureList;
{$ENDIF FHIR_BODYSTRUCTURE}
{$IFDEF FHIR_BUNDLE}
  TFhirBundleLink = fhir5_resources_other.TFhirBundleLink;
  TFhirBundleLinkList = fhir5_resources_other.TFhirBundleLinkList;
  TFhirBundleEntry = fhir5_resources_other.TFhirBundleEntry;
  TFhirBundleEntryList = fhir5_resources_other.TFhirBundleEntryList;
  TFhirBundleEntrySearch = fhir5_resources_other.TFhirBundleEntrySearch;
  TFhirBundleEntrySearchList = fhir5_resources_other.TFhirBundleEntrySearchList;
  TFhirBundleEntryRequest = fhir5_resources_other.TFhirBundleEntryRequest;
  TFhirBundleEntryRequestList = fhir5_resources_other.TFhirBundleEntryRequestList;
  TFhirBundleEntryResponse = fhir5_resources_other.TFhirBundleEntryResponse;
  TFhirBundleEntryResponseList = fhir5_resources_other.TFhirBundleEntryResponseList;
  TFhirBundle = fhir5_resources_other.TFhirBundle;
  TFhirBundleList = fhir5_resources_other.TFhirBundleList;
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  TFhirCapabilityStatementSoftware = fhir5_resources_canonical.TFhirCapabilityStatementSoftware;
  TFhirCapabilityStatementSoftwareList = fhir5_resources_canonical.TFhirCapabilityStatementSoftwareList;
  TFhirCapabilityStatementImplementation = fhir5_resources_canonical.TFhirCapabilityStatementImplementation;
  TFhirCapabilityStatementImplementationList = fhir5_resources_canonical.TFhirCapabilityStatementImplementationList;
  TFhirCapabilityStatementRest = fhir5_resources_canonical.TFhirCapabilityStatementRest;
  TFhirCapabilityStatementRestList = fhir5_resources_canonical.TFhirCapabilityStatementRestList;
  TFhirCapabilityStatementRestSecurity = fhir5_resources_canonical.TFhirCapabilityStatementRestSecurity;
  TFhirCapabilityStatementRestSecurityList = fhir5_resources_canonical.TFhirCapabilityStatementRestSecurityList;
  TFhirCapabilityStatementRestResource = fhir5_resources_canonical.TFhirCapabilityStatementRestResource;
  TFhirCapabilityStatementRestResourceList = fhir5_resources_canonical.TFhirCapabilityStatementRestResourceList;
  TFhirCapabilityStatementRestResourceInteraction = fhir5_resources_canonical.TFhirCapabilityStatementRestResourceInteraction;
  TFhirCapabilityStatementRestResourceInteractionList = fhir5_resources_canonical.TFhirCapabilityStatementRestResourceInteractionList;
  TFhirCapabilityStatementRestResourceSearchParam = fhir5_resources_canonical.TFhirCapabilityStatementRestResourceSearchParam;
  TFhirCapabilityStatementRestResourceSearchParamList = fhir5_resources_canonical.TFhirCapabilityStatementRestResourceSearchParamList;
  TFhirCapabilityStatementRestResourceOperation = fhir5_resources_canonical.TFhirCapabilityStatementRestResourceOperation;
  TFhirCapabilityStatementRestResourceOperationList = fhir5_resources_canonical.TFhirCapabilityStatementRestResourceOperationList;
  TFhirCapabilityStatementRestInteraction = fhir5_resources_canonical.TFhirCapabilityStatementRestInteraction;
  TFhirCapabilityStatementRestInteractionList = fhir5_resources_canonical.TFhirCapabilityStatementRestInteractionList;
  TFhirCapabilityStatementMessaging = fhir5_resources_canonical.TFhirCapabilityStatementMessaging;
  TFhirCapabilityStatementMessagingList = fhir5_resources_canonical.TFhirCapabilityStatementMessagingList;
  TFhirCapabilityStatementMessagingEndpoint = fhir5_resources_canonical.TFhirCapabilityStatementMessagingEndpoint;
  TFhirCapabilityStatementMessagingEndpointList = fhir5_resources_canonical.TFhirCapabilityStatementMessagingEndpointList;
  TFhirCapabilityStatementMessagingSupportedMessage = fhir5_resources_canonical.TFhirCapabilityStatementMessagingSupportedMessage;
  TFhirCapabilityStatementMessagingSupportedMessageList = fhir5_resources_canonical.TFhirCapabilityStatementMessagingSupportedMessageList;
  TFhirCapabilityStatementDocument = fhir5_resources_canonical.TFhirCapabilityStatementDocument;
  TFhirCapabilityStatementDocumentList = fhir5_resources_canonical.TFhirCapabilityStatementDocumentList;
  TFhirCapabilityStatement = fhir5_resources_canonical.TFhirCapabilityStatement;
  TFhirCapabilityStatementList = fhir5_resources_canonical.TFhirCapabilityStatementList;
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CAREPLAN}
  TFhirCarePlanActivity = fhir5_resources_clinical.TFhirCarePlanActivity;
  TFhirCarePlanActivityList = fhir5_resources_clinical.TFhirCarePlanActivityList;
  TFhirCarePlanActivityPlannedActivityDetail = fhir5_resources_clinical.TFhirCarePlanActivityPlannedActivityDetail;
  TFhirCarePlanActivityPlannedActivityDetailList = fhir5_resources_clinical.TFhirCarePlanActivityPlannedActivityDetailList;
  TFhirCarePlan = fhir5_resources_clinical.TFhirCarePlan;
  TFhirCarePlanList = fhir5_resources_clinical.TFhirCarePlanList;
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CARETEAM}
  TFhirCareTeamParticipant = fhir5_resources_clinical.TFhirCareTeamParticipant;
  TFhirCareTeamParticipantList = fhir5_resources_clinical.TFhirCareTeamParticipantList;
  TFhirCareTeam = fhir5_resources_clinical.TFhirCareTeam;
  TFhirCareTeamList = fhir5_resources_clinical.TFhirCareTeamList;
{$ENDIF FHIR_CARETEAM}
{$IFDEF FHIR_CHARGEITEM}
  TFhirChargeItemPerformer = fhir5_resources_financial.TFhirChargeItemPerformer;
  TFhirChargeItemPerformerList = fhir5_resources_financial.TFhirChargeItemPerformerList;
  TFhirChargeItem = fhir5_resources_financial.TFhirChargeItem;
  TFhirChargeItemList = fhir5_resources_financial.TFhirChargeItemList;
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CHARGEITEMDEFINITION}
  TFhirChargeItemDefinitionApplicability = fhir5_resources_financial.TFhirChargeItemDefinitionApplicability;
  TFhirChargeItemDefinitionApplicabilityList = fhir5_resources_financial.TFhirChargeItemDefinitionApplicabilityList;
  TFhirChargeItemDefinitionPropertyGroup = fhir5_resources_financial.TFhirChargeItemDefinitionPropertyGroup;
  TFhirChargeItemDefinitionPropertyGroupList = fhir5_resources_financial.TFhirChargeItemDefinitionPropertyGroupList;
  TFhirChargeItemDefinition = fhir5_resources_financial.TFhirChargeItemDefinition;
  TFhirChargeItemDefinitionList = fhir5_resources_financial.TFhirChargeItemDefinitionList;
{$ENDIF FHIR_CHARGEITEMDEFINITION}
{$IFDEF FHIR_CITATION}
  TFhirCitationSummary = fhir5_resources_financial.TFhirCitationSummary;
  TFhirCitationSummaryList = fhir5_resources_financial.TFhirCitationSummaryList;
  TFhirCitationClassification = fhir5_resources_financial.TFhirCitationClassification;
  TFhirCitationClassificationList = fhir5_resources_financial.TFhirCitationClassificationList;
  TFhirCitationStatusDate = fhir5_resources_financial.TFhirCitationStatusDate;
  TFhirCitationStatusDateList = fhir5_resources_financial.TFhirCitationStatusDateList;
  TFhirCitationCitedArtifact = fhir5_resources_financial.TFhirCitationCitedArtifact;
  TFhirCitationCitedArtifactList = fhir5_resources_financial.TFhirCitationCitedArtifactList;
  TFhirCitationCitedArtifactVersion = fhir5_resources_financial.TFhirCitationCitedArtifactVersion;
  TFhirCitationCitedArtifactVersionList = fhir5_resources_financial.TFhirCitationCitedArtifactVersionList;
  TFhirCitationCitedArtifactStatusDate = fhir5_resources_financial.TFhirCitationCitedArtifactStatusDate;
  TFhirCitationCitedArtifactStatusDateList = fhir5_resources_financial.TFhirCitationCitedArtifactStatusDateList;
  TFhirCitationCitedArtifactTitle = fhir5_resources_financial.TFhirCitationCitedArtifactTitle;
  TFhirCitationCitedArtifactTitleList = fhir5_resources_financial.TFhirCitationCitedArtifactTitleList;
  TFhirCitationCitedArtifactAbstract = fhir5_resources_financial.TFhirCitationCitedArtifactAbstract;
  TFhirCitationCitedArtifactAbstractList = fhir5_resources_financial.TFhirCitationCitedArtifactAbstractList;
  TFhirCitationCitedArtifactPart = fhir5_resources_financial.TFhirCitationCitedArtifactPart;
  TFhirCitationCitedArtifactPartList = fhir5_resources_financial.TFhirCitationCitedArtifactPartList;
  TFhirCitationCitedArtifactRelatesTo = fhir5_resources_financial.TFhirCitationCitedArtifactRelatesTo;
  TFhirCitationCitedArtifactRelatesToList = fhir5_resources_financial.TFhirCitationCitedArtifactRelatesToList;
  TFhirCitationCitedArtifactPublicationForm = fhir5_resources_financial.TFhirCitationCitedArtifactPublicationForm;
  TFhirCitationCitedArtifactPublicationFormList = fhir5_resources_financial.TFhirCitationCitedArtifactPublicationFormList;
  TFhirCitationCitedArtifactPublicationFormPublishedIn = fhir5_resources_financial.TFhirCitationCitedArtifactPublicationFormPublishedIn;
  TFhirCitationCitedArtifactPublicationFormPublishedInList = fhir5_resources_financial.TFhirCitationCitedArtifactPublicationFormPublishedInList;
  TFhirCitationCitedArtifactWebLocation = fhir5_resources_financial.TFhirCitationCitedArtifactWebLocation;
  TFhirCitationCitedArtifactWebLocationList = fhir5_resources_financial.TFhirCitationCitedArtifactWebLocationList;
  TFhirCitationCitedArtifactClassification = fhir5_resources_financial.TFhirCitationCitedArtifactClassification;
  TFhirCitationCitedArtifactClassificationList = fhir5_resources_financial.TFhirCitationCitedArtifactClassificationList;
  TFhirCitationCitedArtifactContributorship = fhir5_resources_financial.TFhirCitationCitedArtifactContributorship;
  TFhirCitationCitedArtifactContributorshipList = fhir5_resources_financial.TFhirCitationCitedArtifactContributorshipList;
  TFhirCitationCitedArtifactContributorshipEntry = fhir5_resources_financial.TFhirCitationCitedArtifactContributorshipEntry;
  TFhirCitationCitedArtifactContributorshipEntryList = fhir5_resources_financial.TFhirCitationCitedArtifactContributorshipEntryList;
  TFhirCitationCitedArtifactContributorshipEntryContributionInstance = fhir5_resources_financial.TFhirCitationCitedArtifactContributorshipEntryContributionInstance;
  TFhirCitationCitedArtifactContributorshipEntryContributionInstanceList = fhir5_resources_financial.TFhirCitationCitedArtifactContributorshipEntryContributionInstanceList;
  TFhirCitationCitedArtifactContributorshipSummary = fhir5_resources_financial.TFhirCitationCitedArtifactContributorshipSummary;
  TFhirCitationCitedArtifactContributorshipSummaryList = fhir5_resources_financial.TFhirCitationCitedArtifactContributorshipSummaryList;
  TFhirCitation = fhir5_resources_financial.TFhirCitation;
  TFhirCitationList = fhir5_resources_financial.TFhirCitationList;
{$ENDIF FHIR_CITATION}
{$IFDEF FHIR_CLAIM}
  TFhirClaimRelated = fhir5_resources_financial.TFhirClaimRelated;
  TFhirClaimRelatedList = fhir5_resources_financial.TFhirClaimRelatedList;
  TFhirClaimPayee = fhir5_resources_financial.TFhirClaimPayee;
  TFhirClaimPayeeList = fhir5_resources_financial.TFhirClaimPayeeList;
  TFhirClaimCareTeam = fhir5_resources_financial.TFhirClaimCareTeam;
  TFhirClaimCareTeamList = fhir5_resources_financial.TFhirClaimCareTeamList;
  TFhirClaimSupportingInfo = fhir5_resources_financial.TFhirClaimSupportingInfo;
  TFhirClaimSupportingInfoList = fhir5_resources_financial.TFhirClaimSupportingInfoList;
  TFhirClaimDiagnosis = fhir5_resources_financial.TFhirClaimDiagnosis;
  TFhirClaimDiagnosisList = fhir5_resources_financial.TFhirClaimDiagnosisList;
  TFhirClaimProcedure = fhir5_resources_financial.TFhirClaimProcedure;
  TFhirClaimProcedureList = fhir5_resources_financial.TFhirClaimProcedureList;
  TFhirClaimInsurance = fhir5_resources_financial.TFhirClaimInsurance;
  TFhirClaimInsuranceList = fhir5_resources_financial.TFhirClaimInsuranceList;
  TFhirClaimAccident = fhir5_resources_financial.TFhirClaimAccident;
  TFhirClaimAccidentList = fhir5_resources_financial.TFhirClaimAccidentList;
  TFhirClaimItem = fhir5_resources_financial.TFhirClaimItem;
  TFhirClaimItemList = fhir5_resources_financial.TFhirClaimItemList;
  TFhirClaimItemBodySite = fhir5_resources_financial.TFhirClaimItemBodySite;
  TFhirClaimItemBodySiteList = fhir5_resources_financial.TFhirClaimItemBodySiteList;
  TFhirClaimItemDetail = fhir5_resources_financial.TFhirClaimItemDetail;
  TFhirClaimItemDetailList = fhir5_resources_financial.TFhirClaimItemDetailList;
  TFhirClaimItemDetailSubDetail = fhir5_resources_financial.TFhirClaimItemDetailSubDetail;
  TFhirClaimItemDetailSubDetailList = fhir5_resources_financial.TFhirClaimItemDetailSubDetailList;
  TFhirClaim = fhir5_resources_financial.TFhirClaim;
  TFhirClaimList = fhir5_resources_financial.TFhirClaimList;
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  TFhirClaimResponseItem = fhir5_resources_financial.TFhirClaimResponseItem;
  TFhirClaimResponseItemList = fhir5_resources_financial.TFhirClaimResponseItemList;
  TFhirClaimResponseItemAdjudication = fhir5_resources_financial.TFhirClaimResponseItemAdjudication;
  TFhirClaimResponseItemAdjudicationList = fhir5_resources_financial.TFhirClaimResponseItemAdjudicationList;
  TFhirClaimResponseItemDetail = fhir5_resources_financial.TFhirClaimResponseItemDetail;
  TFhirClaimResponseItemDetailList = fhir5_resources_financial.TFhirClaimResponseItemDetailList;
  TFhirClaimResponseItemDetailSubDetail = fhir5_resources_financial.TFhirClaimResponseItemDetailSubDetail;
  TFhirClaimResponseItemDetailSubDetailList = fhir5_resources_financial.TFhirClaimResponseItemDetailSubDetailList;
  TFhirClaimResponseAddItem = fhir5_resources_financial.TFhirClaimResponseAddItem;
  TFhirClaimResponseAddItemList = fhir5_resources_financial.TFhirClaimResponseAddItemList;
  TFhirClaimResponseAddItemBodySite = fhir5_resources_financial.TFhirClaimResponseAddItemBodySite;
  TFhirClaimResponseAddItemBodySiteList = fhir5_resources_financial.TFhirClaimResponseAddItemBodySiteList;
  TFhirClaimResponseAddItemDetail = fhir5_resources_financial.TFhirClaimResponseAddItemDetail;
  TFhirClaimResponseAddItemDetailList = fhir5_resources_financial.TFhirClaimResponseAddItemDetailList;
  TFhirClaimResponseAddItemDetailSubDetail = fhir5_resources_financial.TFhirClaimResponseAddItemDetailSubDetail;
  TFhirClaimResponseAddItemDetailSubDetailList = fhir5_resources_financial.TFhirClaimResponseAddItemDetailSubDetailList;
  TFhirClaimResponseTotal = fhir5_resources_financial.TFhirClaimResponseTotal;
  TFhirClaimResponseTotalList = fhir5_resources_financial.TFhirClaimResponseTotalList;
  TFhirClaimResponsePayment = fhir5_resources_financial.TFhirClaimResponsePayment;
  TFhirClaimResponsePaymentList = fhir5_resources_financial.TFhirClaimResponsePaymentList;
  TFhirClaimResponseProcessNote = fhir5_resources_financial.TFhirClaimResponseProcessNote;
  TFhirClaimResponseProcessNoteList = fhir5_resources_financial.TFhirClaimResponseProcessNoteList;
  TFhirClaimResponseInsurance = fhir5_resources_financial.TFhirClaimResponseInsurance;
  TFhirClaimResponseInsuranceList = fhir5_resources_financial.TFhirClaimResponseInsuranceList;
  TFhirClaimResponseError = fhir5_resources_financial.TFhirClaimResponseError;
  TFhirClaimResponseErrorList = fhir5_resources_financial.TFhirClaimResponseErrorList;
  TFhirClaimResponse = fhir5_resources_financial.TFhirClaimResponse;
  TFhirClaimResponseList = fhir5_resources_financial.TFhirClaimResponseList;
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  TFhirClinicalImpressionFinding = fhir5_resources_clinical.TFhirClinicalImpressionFinding;
  TFhirClinicalImpressionFindingList = fhir5_resources_clinical.TFhirClinicalImpressionFindingList;
  TFhirClinicalImpression = fhir5_resources_clinical.TFhirClinicalImpression;
  TFhirClinicalImpressionList = fhir5_resources_clinical.TFhirClinicalImpressionList;
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_CLINICALUSEDEFINITION}
  TFhirClinicalUseDefinitionContraindication = fhir5_resources_clinical.TFhirClinicalUseDefinitionContraindication;
  TFhirClinicalUseDefinitionContraindicationList = fhir5_resources_clinical.TFhirClinicalUseDefinitionContraindicationList;
  TFhirClinicalUseDefinitionContraindicationOtherTherapy = fhir5_resources_clinical.TFhirClinicalUseDefinitionContraindicationOtherTherapy;
  TFhirClinicalUseDefinitionContraindicationOtherTherapyList = fhir5_resources_clinical.TFhirClinicalUseDefinitionContraindicationOtherTherapyList;
  TFhirClinicalUseDefinitionIndication = fhir5_resources_clinical.TFhirClinicalUseDefinitionIndication;
  TFhirClinicalUseDefinitionIndicationList = fhir5_resources_clinical.TFhirClinicalUseDefinitionIndicationList;
  TFhirClinicalUseDefinitionInteraction = fhir5_resources_clinical.TFhirClinicalUseDefinitionInteraction;
  TFhirClinicalUseDefinitionInteractionList = fhir5_resources_clinical.TFhirClinicalUseDefinitionInteractionList;
  TFhirClinicalUseDefinitionInteractionInteractant = fhir5_resources_clinical.TFhirClinicalUseDefinitionInteractionInteractant;
  TFhirClinicalUseDefinitionInteractionInteractantList = fhir5_resources_clinical.TFhirClinicalUseDefinitionInteractionInteractantList;
  TFhirClinicalUseDefinitionUndesirableEffect = fhir5_resources_clinical.TFhirClinicalUseDefinitionUndesirableEffect;
  TFhirClinicalUseDefinitionUndesirableEffectList = fhir5_resources_clinical.TFhirClinicalUseDefinitionUndesirableEffectList;
  TFhirClinicalUseDefinitionWarning = fhir5_resources_clinical.TFhirClinicalUseDefinitionWarning;
  TFhirClinicalUseDefinitionWarningList = fhir5_resources_clinical.TFhirClinicalUseDefinitionWarningList;
  TFhirClinicalUseDefinition = fhir5_resources_clinical.TFhirClinicalUseDefinition;
  TFhirClinicalUseDefinitionList = fhir5_resources_clinical.TFhirClinicalUseDefinitionList;
{$ENDIF FHIR_CLINICALUSEDEFINITION}
{$IFDEF FHIR_CODESYSTEM}
  TFhirCodeSystemFilter = fhir5_resources_canonical.TFhirCodeSystemFilter;
  TFhirCodeSystemFilterList = fhir5_resources_canonical.TFhirCodeSystemFilterList;
  TFhirCodeSystemProperty = fhir5_resources_canonical.TFhirCodeSystemProperty;
  TFhirCodeSystemPropertyList = fhir5_resources_canonical.TFhirCodeSystemPropertyList;
  TFhirCodeSystemConcept = fhir5_resources_canonical.TFhirCodeSystemConcept;
  TFhirCodeSystemConceptList = fhir5_resources_canonical.TFhirCodeSystemConceptList;
  TFhirCodeSystemConceptDesignation = fhir5_resources_canonical.TFhirCodeSystemConceptDesignation;
  TFhirCodeSystemConceptDesignationList = fhir5_resources_canonical.TFhirCodeSystemConceptDesignationList;
  TFhirCodeSystemConceptProperty = fhir5_resources_canonical.TFhirCodeSystemConceptProperty;
  TFhirCodeSystemConceptPropertyList = fhir5_resources_canonical.TFhirCodeSystemConceptPropertyList;
  TFhirCodeSystem = fhir5_resources_canonical.TFhirCodeSystem;
  TFhirCodeSystemList = fhir5_resources_canonical.TFhirCodeSystemList;
{$ENDIF FHIR_CODESYSTEM}
{$IFDEF FHIR_COMMUNICATION}
  TFhirCommunicationPayload = fhir5_resources_clinical.TFhirCommunicationPayload;
  TFhirCommunicationPayloadList = fhir5_resources_clinical.TFhirCommunicationPayloadList;
  TFhirCommunication = fhir5_resources_clinical.TFhirCommunication;
  TFhirCommunicationList = fhir5_resources_clinical.TFhirCommunicationList;
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  TFhirCommunicationRequestPayload = fhir5_resources_clinical.TFhirCommunicationRequestPayload;
  TFhirCommunicationRequestPayloadList = fhir5_resources_clinical.TFhirCommunicationRequestPayloadList;
  TFhirCommunicationRequest = fhir5_resources_clinical.TFhirCommunicationRequest;
  TFhirCommunicationRequestList = fhir5_resources_clinical.TFhirCommunicationRequestList;
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  TFhirCompartmentDefinitionResource = fhir5_resources_canonical.TFhirCompartmentDefinitionResource;
  TFhirCompartmentDefinitionResourceList = fhir5_resources_canonical.TFhirCompartmentDefinitionResourceList;
  TFhirCompartmentDefinition = fhir5_resources_canonical.TFhirCompartmentDefinition;
  TFhirCompartmentDefinitionList = fhir5_resources_canonical.TFhirCompartmentDefinitionList;
{$ENDIF FHIR_COMPARTMENTDEFINITION}
{$IFDEF FHIR_COMPOSITION}
  TFhirCompositionAttester = fhir5_resources_clinical.TFhirCompositionAttester;
  TFhirCompositionAttesterList = fhir5_resources_clinical.TFhirCompositionAttesterList;
  TFhirCompositionEvent = fhir5_resources_clinical.TFhirCompositionEvent;
  TFhirCompositionEventList = fhir5_resources_clinical.TFhirCompositionEventList;
  TFhirCompositionSection = fhir5_resources_clinical.TFhirCompositionSection;
  TFhirCompositionSectionList = fhir5_resources_clinical.TFhirCompositionSectionList;
  TFhirComposition = fhir5_resources_clinical.TFhirComposition;
  TFhirCompositionList = fhir5_resources_clinical.TFhirCompositionList;
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  TFhirConceptMapGroup = fhir5_resources_canonical.TFhirConceptMapGroup;
  TFhirConceptMapGroupList = fhir5_resources_canonical.TFhirConceptMapGroupList;
  TFhirConceptMapGroupElement = fhir5_resources_canonical.TFhirConceptMapGroupElement;
  TFhirConceptMapGroupElementList = fhir5_resources_canonical.TFhirConceptMapGroupElementList;
  TFhirConceptMapGroupElementTarget = fhir5_resources_canonical.TFhirConceptMapGroupElementTarget;
  TFhirConceptMapGroupElementTargetList = fhir5_resources_canonical.TFhirConceptMapGroupElementTargetList;
  TFhirConceptMapGroupElementTargetDependsOn = fhir5_resources_canonical.TFhirConceptMapGroupElementTargetDependsOn;
  TFhirConceptMapGroupElementTargetDependsOnList = fhir5_resources_canonical.TFhirConceptMapGroupElementTargetDependsOnList;
  TFhirConceptMapGroupUnmapped = fhir5_resources_canonical.TFhirConceptMapGroupUnmapped;
  TFhirConceptMapGroupUnmappedList = fhir5_resources_canonical.TFhirConceptMapGroupUnmappedList;
  TFhirConceptMap = fhir5_resources_canonical.TFhirConceptMap;
  TFhirConceptMapList = fhir5_resources_canonical.TFhirConceptMapList;
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_CONDITION}
  TFhirConditionParticipant = fhir5_resources_clinical.TFhirConditionParticipant;
  TFhirConditionParticipantList = fhir5_resources_clinical.TFhirConditionParticipantList;
  TFhirConditionStage = fhir5_resources_clinical.TFhirConditionStage;
  TFhirConditionStageList = fhir5_resources_clinical.TFhirConditionStageList;
  TFhirCondition = fhir5_resources_clinical.TFhirCondition;
  TFhirConditionList = fhir5_resources_clinical.TFhirConditionList;
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_CONDITIONDEFINITION}
  TFhirConditionDefinitionObservation = fhir5_resources_canonical.TFhirConditionDefinitionObservation;
  TFhirConditionDefinitionObservationList = fhir5_resources_canonical.TFhirConditionDefinitionObservationList;
  TFhirConditionDefinitionMedication = fhir5_resources_canonical.TFhirConditionDefinitionMedication;
  TFhirConditionDefinitionMedicationList = fhir5_resources_canonical.TFhirConditionDefinitionMedicationList;
  TFhirConditionDefinitionPrecondition = fhir5_resources_canonical.TFhirConditionDefinitionPrecondition;
  TFhirConditionDefinitionPreconditionList = fhir5_resources_canonical.TFhirConditionDefinitionPreconditionList;
  TFhirConditionDefinitionQuestionnaire = fhir5_resources_canonical.TFhirConditionDefinitionQuestionnaire;
  TFhirConditionDefinitionQuestionnaireList = fhir5_resources_canonical.TFhirConditionDefinitionQuestionnaireList;
  TFhirConditionDefinitionPlan = fhir5_resources_canonical.TFhirConditionDefinitionPlan;
  TFhirConditionDefinitionPlanList = fhir5_resources_canonical.TFhirConditionDefinitionPlanList;
  TFhirConditionDefinition = fhir5_resources_canonical.TFhirConditionDefinition;
  TFhirConditionDefinitionList = fhir5_resources_canonical.TFhirConditionDefinitionList;
{$ENDIF FHIR_CONDITIONDEFINITION}
{$IFDEF FHIR_CONSENT}
  TFhirConsentPolicyBasis = fhir5_resources_other.TFhirConsentPolicyBasis;
  TFhirConsentPolicyBasisList = fhir5_resources_other.TFhirConsentPolicyBasisList;
  TFhirConsentVerification = fhir5_resources_other.TFhirConsentVerification;
  TFhirConsentVerificationList = fhir5_resources_other.TFhirConsentVerificationList;
  TFhirConsentProvision = fhir5_resources_other.TFhirConsentProvision;
  TFhirConsentProvisionList = fhir5_resources_other.TFhirConsentProvisionList;
  TFhirConsentProvisionActor = fhir5_resources_other.TFhirConsentProvisionActor;
  TFhirConsentProvisionActorList = fhir5_resources_other.TFhirConsentProvisionActorList;
  TFhirConsentProvisionData = fhir5_resources_other.TFhirConsentProvisionData;
  TFhirConsentProvisionDataList = fhir5_resources_other.TFhirConsentProvisionDataList;
  TFhirConsent = fhir5_resources_other.TFhirConsent;
  TFhirConsentList = fhir5_resources_other.TFhirConsentList;
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  TFhirContractContentDefinition = fhir5_resources_other.TFhirContractContentDefinition;
  TFhirContractContentDefinitionList = fhir5_resources_other.TFhirContractContentDefinitionList;
  TFhirContractTerm = fhir5_resources_other.TFhirContractTerm;
  TFhirContractTermList = fhir5_resources_other.TFhirContractTermList;
  TFhirContractTermSecurityLabel = fhir5_resources_other.TFhirContractTermSecurityLabel;
  TFhirContractTermSecurityLabelList = fhir5_resources_other.TFhirContractTermSecurityLabelList;
  TFhirContractTermOffer = fhir5_resources_other.TFhirContractTermOffer;
  TFhirContractTermOfferList = fhir5_resources_other.TFhirContractTermOfferList;
  TFhirContractTermOfferParty = fhir5_resources_other.TFhirContractTermOfferParty;
  TFhirContractTermOfferPartyList = fhir5_resources_other.TFhirContractTermOfferPartyList;
  TFhirContractTermOfferAnswer = fhir5_resources_other.TFhirContractTermOfferAnswer;
  TFhirContractTermOfferAnswerList = fhir5_resources_other.TFhirContractTermOfferAnswerList;
  TFhirContractTermAsset = fhir5_resources_other.TFhirContractTermAsset;
  TFhirContractTermAssetList = fhir5_resources_other.TFhirContractTermAssetList;
  TFhirContractTermAssetContext = fhir5_resources_other.TFhirContractTermAssetContext;
  TFhirContractTermAssetContextList = fhir5_resources_other.TFhirContractTermAssetContextList;
  TFhirContractTermAssetValuedItem = fhir5_resources_other.TFhirContractTermAssetValuedItem;
  TFhirContractTermAssetValuedItemList = fhir5_resources_other.TFhirContractTermAssetValuedItemList;
  TFhirContractTermAction = fhir5_resources_other.TFhirContractTermAction;
  TFhirContractTermActionList = fhir5_resources_other.TFhirContractTermActionList;
  TFhirContractTermActionSubject = fhir5_resources_other.TFhirContractTermActionSubject;
  TFhirContractTermActionSubjectList = fhir5_resources_other.TFhirContractTermActionSubjectList;
  TFhirContractSigner = fhir5_resources_other.TFhirContractSigner;
  TFhirContractSignerList = fhir5_resources_other.TFhirContractSignerList;
  TFhirContractFriendly = fhir5_resources_other.TFhirContractFriendly;
  TFhirContractFriendlyList = fhir5_resources_other.TFhirContractFriendlyList;
  TFhirContractLegal = fhir5_resources_other.TFhirContractLegal;
  TFhirContractLegalList = fhir5_resources_other.TFhirContractLegalList;
  TFhirContractRule = fhir5_resources_other.TFhirContractRule;
  TFhirContractRuleList = fhir5_resources_other.TFhirContractRuleList;
  TFhirContract = fhir5_resources_other.TFhirContract;
  TFhirContractList = fhir5_resources_other.TFhirContractList;
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_COVERAGE}
  TFhirCoveragePaymentBy = fhir5_resources_financial.TFhirCoveragePaymentBy;
  TFhirCoveragePaymentByList = fhir5_resources_financial.TFhirCoveragePaymentByList;
  TFhirCoverageClass = fhir5_resources_financial.TFhirCoverageClass;
  TFhirCoverageClassList = fhir5_resources_financial.TFhirCoverageClassList;
  TFhirCoverageCostToBeneficiary = fhir5_resources_financial.TFhirCoverageCostToBeneficiary;
  TFhirCoverageCostToBeneficiaryList = fhir5_resources_financial.TFhirCoverageCostToBeneficiaryList;
  TFhirCoverageCostToBeneficiaryException = fhir5_resources_financial.TFhirCoverageCostToBeneficiaryException;
  TFhirCoverageCostToBeneficiaryExceptionList = fhir5_resources_financial.TFhirCoverageCostToBeneficiaryExceptionList;
  TFhirCoverage = fhir5_resources_financial.TFhirCoverage;
  TFhirCoverageList = fhir5_resources_financial.TFhirCoverageList;
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  TFhirCoverageEligibilityRequestSupportingInfo = fhir5_resources_financial.TFhirCoverageEligibilityRequestSupportingInfo;
  TFhirCoverageEligibilityRequestSupportingInfoList = fhir5_resources_financial.TFhirCoverageEligibilityRequestSupportingInfoList;
  TFhirCoverageEligibilityRequestInsurance = fhir5_resources_financial.TFhirCoverageEligibilityRequestInsurance;
  TFhirCoverageEligibilityRequestInsuranceList = fhir5_resources_financial.TFhirCoverageEligibilityRequestInsuranceList;
  TFhirCoverageEligibilityRequestItem = fhir5_resources_financial.TFhirCoverageEligibilityRequestItem;
  TFhirCoverageEligibilityRequestItemList = fhir5_resources_financial.TFhirCoverageEligibilityRequestItemList;
  TFhirCoverageEligibilityRequestItemDiagnosis = fhir5_resources_financial.TFhirCoverageEligibilityRequestItemDiagnosis;
  TFhirCoverageEligibilityRequestItemDiagnosisList = fhir5_resources_financial.TFhirCoverageEligibilityRequestItemDiagnosisList;
  TFhirCoverageEligibilityRequest = fhir5_resources_financial.TFhirCoverageEligibilityRequest;
  TFhirCoverageEligibilityRequestList = fhir5_resources_financial.TFhirCoverageEligibilityRequestList;
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}
{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  TFhirCoverageEligibilityResponseInsurance = fhir5_resources_financial.TFhirCoverageEligibilityResponseInsurance;
  TFhirCoverageEligibilityResponseInsuranceList = fhir5_resources_financial.TFhirCoverageEligibilityResponseInsuranceList;
  TFhirCoverageEligibilityResponseInsuranceItem = fhir5_resources_financial.TFhirCoverageEligibilityResponseInsuranceItem;
  TFhirCoverageEligibilityResponseInsuranceItemList = fhir5_resources_financial.TFhirCoverageEligibilityResponseInsuranceItemList;
  TFhirCoverageEligibilityResponseInsuranceItemBenefit = fhir5_resources_financial.TFhirCoverageEligibilityResponseInsuranceItemBenefit;
  TFhirCoverageEligibilityResponseInsuranceItemBenefitList = fhir5_resources_financial.TFhirCoverageEligibilityResponseInsuranceItemBenefitList;
  TFhirCoverageEligibilityResponseError = fhir5_resources_financial.TFhirCoverageEligibilityResponseError;
  TFhirCoverageEligibilityResponseErrorList = fhir5_resources_financial.TFhirCoverageEligibilityResponseErrorList;
  TFhirCoverageEligibilityResponse = fhir5_resources_financial.TFhirCoverageEligibilityResponse;
  TFhirCoverageEligibilityResponseList = fhir5_resources_financial.TFhirCoverageEligibilityResponseList;
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}
{$IFDEF FHIR_DETECTEDISSUE}
  TFhirDetectedIssueEvidence = fhir5_resources_clinical.TFhirDetectedIssueEvidence;
  TFhirDetectedIssueEvidenceList = fhir5_resources_clinical.TFhirDetectedIssueEvidenceList;
  TFhirDetectedIssueMitigation = fhir5_resources_clinical.TFhirDetectedIssueMitigation;
  TFhirDetectedIssueMitigationList = fhir5_resources_clinical.TFhirDetectedIssueMitigationList;
  TFhirDetectedIssue = fhir5_resources_clinical.TFhirDetectedIssue;
  TFhirDetectedIssueList = fhir5_resources_clinical.TFhirDetectedIssueList;
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  TFhirDeviceUdiCarrier = fhir5_resources_admin.TFhirDeviceUdiCarrier;
  TFhirDeviceUdiCarrierList = fhir5_resources_admin.TFhirDeviceUdiCarrierList;
  TFhirDeviceDeviceName = fhir5_resources_admin.TFhirDeviceDeviceName;
  TFhirDeviceDeviceNameList = fhir5_resources_admin.TFhirDeviceDeviceNameList;
  TFhirDeviceVersion = fhir5_resources_admin.TFhirDeviceVersion;
  TFhirDeviceVersionList = fhir5_resources_admin.TFhirDeviceVersionList;
  TFhirDeviceSpecialization = fhir5_resources_admin.TFhirDeviceSpecialization;
  TFhirDeviceSpecializationList = fhir5_resources_admin.TFhirDeviceSpecializationList;
  TFhirDeviceProperty = fhir5_resources_admin.TFhirDeviceProperty;
  TFhirDevicePropertyList = fhir5_resources_admin.TFhirDevicePropertyList;
  TFhirDeviceOperation = fhir5_resources_admin.TFhirDeviceOperation;
  TFhirDeviceOperationList = fhir5_resources_admin.TFhirDeviceOperationList;
  TFhirDeviceAssociation = fhir5_resources_admin.TFhirDeviceAssociation;
  TFhirDeviceAssociationList = fhir5_resources_admin.TFhirDeviceAssociationList;
  TFhirDevice = fhir5_resources_admin.TFhirDevice;
  TFhirDeviceList = fhir5_resources_admin.TFhirDeviceList;
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICEDEFINITION}
  TFhirDeviceDefinitionUdiDeviceIdentifier = fhir5_resources_admin.TFhirDeviceDefinitionUdiDeviceIdentifier;
  TFhirDeviceDefinitionUdiDeviceIdentifierList = fhir5_resources_admin.TFhirDeviceDefinitionUdiDeviceIdentifierList;
  TFhirDeviceDefinitionUdiDeviceIdentifierMarketDistribution = fhir5_resources_admin.TFhirDeviceDefinitionUdiDeviceIdentifierMarketDistribution;
  TFhirDeviceDefinitionUdiDeviceIdentifierMarketDistributionList = fhir5_resources_admin.TFhirDeviceDefinitionUdiDeviceIdentifierMarketDistributionList;
  TFhirDeviceDefinitionRegulatoryIdentifier = fhir5_resources_admin.TFhirDeviceDefinitionRegulatoryIdentifier;
  TFhirDeviceDefinitionRegulatoryIdentifierList = fhir5_resources_admin.TFhirDeviceDefinitionRegulatoryIdentifierList;
  TFhirDeviceDefinitionDeviceName = fhir5_resources_admin.TFhirDeviceDefinitionDeviceName;
  TFhirDeviceDefinitionDeviceNameList = fhir5_resources_admin.TFhirDeviceDefinitionDeviceNameList;
  TFhirDeviceDefinitionClassification = fhir5_resources_admin.TFhirDeviceDefinitionClassification;
  TFhirDeviceDefinitionClassificationList = fhir5_resources_admin.TFhirDeviceDefinitionClassificationList;
  TFhirDeviceDefinitionHasPart = fhir5_resources_admin.TFhirDeviceDefinitionHasPart;
  TFhirDeviceDefinitionHasPartList = fhir5_resources_admin.TFhirDeviceDefinitionHasPartList;
  TFhirDeviceDefinitionPackaging = fhir5_resources_admin.TFhirDeviceDefinitionPackaging;
  TFhirDeviceDefinitionPackagingList = fhir5_resources_admin.TFhirDeviceDefinitionPackagingList;
  TFhirDeviceDefinitionPackagingDistributor = fhir5_resources_admin.TFhirDeviceDefinitionPackagingDistributor;
  TFhirDeviceDefinitionPackagingDistributorList = fhir5_resources_admin.TFhirDeviceDefinitionPackagingDistributorList;
  TFhirDeviceDefinitionVersion = fhir5_resources_admin.TFhirDeviceDefinitionVersion;
  TFhirDeviceDefinitionVersionList = fhir5_resources_admin.TFhirDeviceDefinitionVersionList;
  TFhirDeviceDefinitionProperty = fhir5_resources_admin.TFhirDeviceDefinitionProperty;
  TFhirDeviceDefinitionPropertyList = fhir5_resources_admin.TFhirDeviceDefinitionPropertyList;
  TFhirDeviceDefinitionLink = fhir5_resources_admin.TFhirDeviceDefinitionLink;
  TFhirDeviceDefinitionLinkList = fhir5_resources_admin.TFhirDeviceDefinitionLinkList;
  TFhirDeviceDefinitionMaterial = fhir5_resources_admin.TFhirDeviceDefinitionMaterial;
  TFhirDeviceDefinitionMaterialList = fhir5_resources_admin.TFhirDeviceDefinitionMaterialList;
  TFhirDeviceDefinitionGuideline = fhir5_resources_admin.TFhirDeviceDefinitionGuideline;
  TFhirDeviceDefinitionGuidelineList = fhir5_resources_admin.TFhirDeviceDefinitionGuidelineList;
  TFhirDeviceDefinitionCorrectiveAction = fhir5_resources_admin.TFhirDeviceDefinitionCorrectiveAction;
  TFhirDeviceDefinitionCorrectiveActionList = fhir5_resources_admin.TFhirDeviceDefinitionCorrectiveActionList;
  TFhirDeviceDefinitionChargeItem = fhir5_resources_admin.TFhirDeviceDefinitionChargeItem;
  TFhirDeviceDefinitionChargeItemList = fhir5_resources_admin.TFhirDeviceDefinitionChargeItemList;
  TFhirDeviceDefinition = fhir5_resources_admin.TFhirDeviceDefinition;
  TFhirDeviceDefinitionList = fhir5_resources_admin.TFhirDeviceDefinitionList;
{$ENDIF FHIR_DEVICEDEFINITION}
{$IFDEF FHIR_DEVICEDISPENSE}
  TFhirDeviceDispensePerformer = fhir5_resources_clinical.TFhirDeviceDispensePerformer;
  TFhirDeviceDispensePerformerList = fhir5_resources_clinical.TFhirDeviceDispensePerformerList;
  TFhirDeviceDispense = fhir5_resources_clinical.TFhirDeviceDispense;
  TFhirDeviceDispenseList = fhir5_resources_clinical.TFhirDeviceDispenseList;
{$ENDIF FHIR_DEVICEDISPENSE}
{$IFDEF FHIR_DEVICEMETRIC}
  TFhirDeviceMetricCalibration = fhir5_resources_admin.TFhirDeviceMetricCalibration;
  TFhirDeviceMetricCalibrationList = fhir5_resources_admin.TFhirDeviceMetricCalibrationList;
  TFhirDeviceMetric = fhir5_resources_admin.TFhirDeviceMetric;
  TFhirDeviceMetricList = fhir5_resources_admin.TFhirDeviceMetricList;
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEREQUEST}
  TFhirDeviceRequestParameter = fhir5_resources_clinical.TFhirDeviceRequestParameter;
  TFhirDeviceRequestParameterList = fhir5_resources_clinical.TFhirDeviceRequestParameterList;
  TFhirDeviceRequest = fhir5_resources_clinical.TFhirDeviceRequest;
  TFhirDeviceRequestList = fhir5_resources_clinical.TFhirDeviceRequestList;
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSAGE}
  TFhirDeviceUsageAdherence = fhir5_resources_clinical.TFhirDeviceUsageAdherence;
  TFhirDeviceUsageAdherenceList = fhir5_resources_clinical.TFhirDeviceUsageAdherenceList;
  TFhirDeviceUsage = fhir5_resources_clinical.TFhirDeviceUsage;
  TFhirDeviceUsageList = fhir5_resources_clinical.TFhirDeviceUsageList;
{$ENDIF FHIR_DEVICEUSAGE}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  TFhirDiagnosticReportSupportingInfo = fhir5_resources_clinical.TFhirDiagnosticReportSupportingInfo;
  TFhirDiagnosticReportSupportingInfoList = fhir5_resources_clinical.TFhirDiagnosticReportSupportingInfoList;
  TFhirDiagnosticReportMedia = fhir5_resources_clinical.TFhirDiagnosticReportMedia;
  TFhirDiagnosticReportMediaList = fhir5_resources_clinical.TFhirDiagnosticReportMediaList;
  TFhirDiagnosticReport = fhir5_resources_clinical.TFhirDiagnosticReport;
  TFhirDiagnosticReportList = fhir5_resources_clinical.TFhirDiagnosticReportList;
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  TFhirDocumentManifestRelated = fhir5_resources_clinical.TFhirDocumentManifestRelated;
  TFhirDocumentManifestRelatedList = fhir5_resources_clinical.TFhirDocumentManifestRelatedList;
  TFhirDocumentManifest = fhir5_resources_clinical.TFhirDocumentManifest;
  TFhirDocumentManifestList = fhir5_resources_clinical.TFhirDocumentManifestList;
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  TFhirDocumentReferenceAttester = fhir5_resources_clinical.TFhirDocumentReferenceAttester;
  TFhirDocumentReferenceAttesterList = fhir5_resources_clinical.TFhirDocumentReferenceAttesterList;
  TFhirDocumentReferenceRelatesTo = fhir5_resources_clinical.TFhirDocumentReferenceRelatesTo;
  TFhirDocumentReferenceRelatesToList = fhir5_resources_clinical.TFhirDocumentReferenceRelatesToList;
  TFhirDocumentReferenceContent = fhir5_resources_clinical.TFhirDocumentReferenceContent;
  TFhirDocumentReferenceContentList = fhir5_resources_clinical.TFhirDocumentReferenceContentList;
  TFhirDocumentReferenceContentProfile = fhir5_resources_clinical.TFhirDocumentReferenceContentProfile;
  TFhirDocumentReferenceContentProfileList = fhir5_resources_clinical.TFhirDocumentReferenceContentProfileList;
  TFhirDocumentReference = fhir5_resources_clinical.TFhirDocumentReference;
  TFhirDocumentReferenceList = fhir5_resources_clinical.TFhirDocumentReferenceList;
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_ENCOUNTER}
  TFhirEncounterStatusHistory = fhir5_resources_admin.TFhirEncounterStatusHistory;
  TFhirEncounterStatusHistoryList = fhir5_resources_admin.TFhirEncounterStatusHistoryList;
  TFhirEncounterClassHistory = fhir5_resources_admin.TFhirEncounterClassHistory;
  TFhirEncounterClassHistoryList = fhir5_resources_admin.TFhirEncounterClassHistoryList;
  TFhirEncounterParticipant = fhir5_resources_admin.TFhirEncounterParticipant;
  TFhirEncounterParticipantList = fhir5_resources_admin.TFhirEncounterParticipantList;
  TFhirEncounterDiagnosis = fhir5_resources_admin.TFhirEncounterDiagnosis;
  TFhirEncounterDiagnosisList = fhir5_resources_admin.TFhirEncounterDiagnosisList;
  TFhirEncounterAdmission = fhir5_resources_admin.TFhirEncounterAdmission;
  TFhirEncounterAdmissionList = fhir5_resources_admin.TFhirEncounterAdmissionList;
  TFhirEncounterLocation = fhir5_resources_admin.TFhirEncounterLocation;
  TFhirEncounterLocationList = fhir5_resources_admin.TFhirEncounterLocationList;
  TFhirEncounter = fhir5_resources_admin.TFhirEncounter;
  TFhirEncounterList = fhir5_resources_admin.TFhirEncounterList;
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENDPOINT}
  TFhirEndpoint = fhir5_resources_admin.TFhirEndpoint;
  TFhirEndpointList = fhir5_resources_admin.TFhirEndpointList;
{$ENDIF FHIR_ENDPOINT}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  TFhirEnrollmentRequest = fhir5_resources_financial.TFhirEnrollmentRequest;
  TFhirEnrollmentRequestList = fhir5_resources_financial.TFhirEnrollmentRequestList;
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  TFhirEnrollmentResponse = fhir5_resources_financial.TFhirEnrollmentResponse;
  TFhirEnrollmentResponseList = fhir5_resources_financial.TFhirEnrollmentResponseList;
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EPISODEOFCARE}
  TFhirEpisodeOfCareStatusHistory = fhir5_resources_clinical.TFhirEpisodeOfCareStatusHistory;
  TFhirEpisodeOfCareStatusHistoryList = fhir5_resources_clinical.TFhirEpisodeOfCareStatusHistoryList;
  TFhirEpisodeOfCareDiagnosis = fhir5_resources_clinical.TFhirEpisodeOfCareDiagnosis;
  TFhirEpisodeOfCareDiagnosisList = fhir5_resources_clinical.TFhirEpisodeOfCareDiagnosisList;
  TFhirEpisodeOfCare = fhir5_resources_clinical.TFhirEpisodeOfCare;
  TFhirEpisodeOfCareList = fhir5_resources_clinical.TFhirEpisodeOfCareList;
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EVENTDEFINITION}
  TFhirEventDefinition = fhir5_resources_canonical.TFhirEventDefinition;
  TFhirEventDefinitionList = fhir5_resources_canonical.TFhirEventDefinitionList;
{$ENDIF FHIR_EVENTDEFINITION}
{$IFDEF FHIR_EVIDENCE}
  TFhirEvidenceVariableDefinition = fhir5_resources_other.TFhirEvidenceVariableDefinition;
  TFhirEvidenceVariableDefinitionList = fhir5_resources_other.TFhirEvidenceVariableDefinitionList;
  TFhirEvidenceStatistic = fhir5_resources_other.TFhirEvidenceStatistic;
  TFhirEvidenceStatisticList = fhir5_resources_other.TFhirEvidenceStatisticList;
  TFhirEvidenceStatisticSampleSize = fhir5_resources_other.TFhirEvidenceStatisticSampleSize;
  TFhirEvidenceStatisticSampleSizeList = fhir5_resources_other.TFhirEvidenceStatisticSampleSizeList;
  TFhirEvidenceStatisticAttributeEstimate = fhir5_resources_other.TFhirEvidenceStatisticAttributeEstimate;
  TFhirEvidenceStatisticAttributeEstimateList = fhir5_resources_other.TFhirEvidenceStatisticAttributeEstimateList;
  TFhirEvidenceStatisticModelCharacteristic = fhir5_resources_other.TFhirEvidenceStatisticModelCharacteristic;
  TFhirEvidenceStatisticModelCharacteristicList = fhir5_resources_other.TFhirEvidenceStatisticModelCharacteristicList;
  TFhirEvidenceStatisticModelCharacteristicVariable = fhir5_resources_other.TFhirEvidenceStatisticModelCharacteristicVariable;
  TFhirEvidenceStatisticModelCharacteristicVariableList = fhir5_resources_other.TFhirEvidenceStatisticModelCharacteristicVariableList;
  TFhirEvidenceCertainty = fhir5_resources_other.TFhirEvidenceCertainty;
  TFhirEvidenceCertaintyList = fhir5_resources_other.TFhirEvidenceCertaintyList;
  TFhirEvidence = fhir5_resources_other.TFhirEvidence;
  TFhirEvidenceList = fhir5_resources_other.TFhirEvidenceList;
{$ENDIF FHIR_EVIDENCE}
{$IFDEF FHIR_EVIDENCEREPORT}
  TFhirEvidenceReportSubject = fhir5_resources_other.TFhirEvidenceReportSubject;
  TFhirEvidenceReportSubjectList = fhir5_resources_other.TFhirEvidenceReportSubjectList;
  TFhirEvidenceReportSubjectCharacteristic = fhir5_resources_other.TFhirEvidenceReportSubjectCharacteristic;
  TFhirEvidenceReportSubjectCharacteristicList = fhir5_resources_other.TFhirEvidenceReportSubjectCharacteristicList;
  TFhirEvidenceReportRelatesTo = fhir5_resources_other.TFhirEvidenceReportRelatesTo;
  TFhirEvidenceReportRelatesToList = fhir5_resources_other.TFhirEvidenceReportRelatesToList;
  TFhirEvidenceReportRelatesToTarget = fhir5_resources_other.TFhirEvidenceReportRelatesToTarget;
  TFhirEvidenceReportRelatesToTargetList = fhir5_resources_other.TFhirEvidenceReportRelatesToTargetList;
  TFhirEvidenceReportSection = fhir5_resources_other.TFhirEvidenceReportSection;
  TFhirEvidenceReportSectionList = fhir5_resources_other.TFhirEvidenceReportSectionList;
  TFhirEvidenceReport = fhir5_resources_other.TFhirEvidenceReport;
  TFhirEvidenceReportList = fhir5_resources_other.TFhirEvidenceReportList;
{$ENDIF FHIR_EVIDENCEREPORT}
{$IFDEF FHIR_EVIDENCEVARIABLE}
  TFhirEvidenceVariableCharacteristic = fhir5_resources_other.TFhirEvidenceVariableCharacteristic;
  TFhirEvidenceVariableCharacteristicList = fhir5_resources_other.TFhirEvidenceVariableCharacteristicList;
  TFhirEvidenceVariableCharacteristicDefinitionByTypeAndValue = fhir5_resources_other.TFhirEvidenceVariableCharacteristicDefinitionByTypeAndValue;
  TFhirEvidenceVariableCharacteristicDefinitionByTypeAndValueList = fhir5_resources_other.TFhirEvidenceVariableCharacteristicDefinitionByTypeAndValueList;
  TFhirEvidenceVariableCharacteristicDefinitionByCombination = fhir5_resources_other.TFhirEvidenceVariableCharacteristicDefinitionByCombination;
  TFhirEvidenceVariableCharacteristicDefinitionByCombinationList = fhir5_resources_other.TFhirEvidenceVariableCharacteristicDefinitionByCombinationList;
  TFhirEvidenceVariableCharacteristicTimeFromEvent = fhir5_resources_other.TFhirEvidenceVariableCharacteristicTimeFromEvent;
  TFhirEvidenceVariableCharacteristicTimeFromEventList = fhir5_resources_other.TFhirEvidenceVariableCharacteristicTimeFromEventList;
  TFhirEvidenceVariableCategory = fhir5_resources_other.TFhirEvidenceVariableCategory;
  TFhirEvidenceVariableCategoryList = fhir5_resources_other.TFhirEvidenceVariableCategoryList;
  TFhirEvidenceVariable = fhir5_resources_other.TFhirEvidenceVariable;
  TFhirEvidenceVariableList = fhir5_resources_other.TFhirEvidenceVariableList;
{$ENDIF FHIR_EVIDENCEVARIABLE}
{$IFDEF FHIR_EXAMPLESCENARIO}
  TFhirExampleScenarioActor = fhir5_resources_canonical.TFhirExampleScenarioActor;
  TFhirExampleScenarioActorList = fhir5_resources_canonical.TFhirExampleScenarioActorList;
  TFhirExampleScenarioInstance = fhir5_resources_canonical.TFhirExampleScenarioInstance;
  TFhirExampleScenarioInstanceList = fhir5_resources_canonical.TFhirExampleScenarioInstanceList;
  TFhirExampleScenarioInstanceVersion = fhir5_resources_canonical.TFhirExampleScenarioInstanceVersion;
  TFhirExampleScenarioInstanceVersionList = fhir5_resources_canonical.TFhirExampleScenarioInstanceVersionList;
  TFhirExampleScenarioInstanceContainedInstance = fhir5_resources_canonical.TFhirExampleScenarioInstanceContainedInstance;
  TFhirExampleScenarioInstanceContainedInstanceList = fhir5_resources_canonical.TFhirExampleScenarioInstanceContainedInstanceList;
  TFhirExampleScenarioProcess = fhir5_resources_canonical.TFhirExampleScenarioProcess;
  TFhirExampleScenarioProcessList = fhir5_resources_canonical.TFhirExampleScenarioProcessList;
  TFhirExampleScenarioProcessStep = fhir5_resources_canonical.TFhirExampleScenarioProcessStep;
  TFhirExampleScenarioProcessStepList = fhir5_resources_canonical.TFhirExampleScenarioProcessStepList;
  TFhirExampleScenarioProcessStepOperation = fhir5_resources_canonical.TFhirExampleScenarioProcessStepOperation;
  TFhirExampleScenarioProcessStepOperationList = fhir5_resources_canonical.TFhirExampleScenarioProcessStepOperationList;
  TFhirExampleScenarioProcessStepAlternative = fhir5_resources_canonical.TFhirExampleScenarioProcessStepAlternative;
  TFhirExampleScenarioProcessStepAlternativeList = fhir5_resources_canonical.TFhirExampleScenarioProcessStepAlternativeList;
  TFhirExampleScenario = fhir5_resources_canonical.TFhirExampleScenario;
  TFhirExampleScenarioList = fhir5_resources_canonical.TFhirExampleScenarioList;
{$ENDIF FHIR_EXAMPLESCENARIO}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  TFhirExplanationOfBenefitRelated = fhir5_resources_financial.TFhirExplanationOfBenefitRelated;
  TFhirExplanationOfBenefitRelatedList = fhir5_resources_financial.TFhirExplanationOfBenefitRelatedList;
  TFhirExplanationOfBenefitPayee = fhir5_resources_financial.TFhirExplanationOfBenefitPayee;
  TFhirExplanationOfBenefitPayeeList = fhir5_resources_financial.TFhirExplanationOfBenefitPayeeList;
  TFhirExplanationOfBenefitCareTeam = fhir5_resources_financial.TFhirExplanationOfBenefitCareTeam;
  TFhirExplanationOfBenefitCareTeamList = fhir5_resources_financial.TFhirExplanationOfBenefitCareTeamList;
  TFhirExplanationOfBenefitSupportingInfo = fhir5_resources_financial.TFhirExplanationOfBenefitSupportingInfo;
  TFhirExplanationOfBenefitSupportingInfoList = fhir5_resources_financial.TFhirExplanationOfBenefitSupportingInfoList;
  TFhirExplanationOfBenefitDiagnosis = fhir5_resources_financial.TFhirExplanationOfBenefitDiagnosis;
  TFhirExplanationOfBenefitDiagnosisList = fhir5_resources_financial.TFhirExplanationOfBenefitDiagnosisList;
  TFhirExplanationOfBenefitProcedure = fhir5_resources_financial.TFhirExplanationOfBenefitProcedure;
  TFhirExplanationOfBenefitProcedureList = fhir5_resources_financial.TFhirExplanationOfBenefitProcedureList;
  TFhirExplanationOfBenefitInsurance = fhir5_resources_financial.TFhirExplanationOfBenefitInsurance;
  TFhirExplanationOfBenefitInsuranceList = fhir5_resources_financial.TFhirExplanationOfBenefitInsuranceList;
  TFhirExplanationOfBenefitAccident = fhir5_resources_financial.TFhirExplanationOfBenefitAccident;
  TFhirExplanationOfBenefitAccidentList = fhir5_resources_financial.TFhirExplanationOfBenefitAccidentList;
  TFhirExplanationOfBenefitItem = fhir5_resources_financial.TFhirExplanationOfBenefitItem;
  TFhirExplanationOfBenefitItemList = fhir5_resources_financial.TFhirExplanationOfBenefitItemList;
  TFhirExplanationOfBenefitItemBodySite = fhir5_resources_financial.TFhirExplanationOfBenefitItemBodySite;
  TFhirExplanationOfBenefitItemBodySiteList = fhir5_resources_financial.TFhirExplanationOfBenefitItemBodySiteList;
  TFhirExplanationOfBenefitItemAdjudication = fhir5_resources_financial.TFhirExplanationOfBenefitItemAdjudication;
  TFhirExplanationOfBenefitItemAdjudicationList = fhir5_resources_financial.TFhirExplanationOfBenefitItemAdjudicationList;
  TFhirExplanationOfBenefitItemDetail = fhir5_resources_financial.TFhirExplanationOfBenefitItemDetail;
  TFhirExplanationOfBenefitItemDetailList = fhir5_resources_financial.TFhirExplanationOfBenefitItemDetailList;
  TFhirExplanationOfBenefitItemDetailSubDetail = fhir5_resources_financial.TFhirExplanationOfBenefitItemDetailSubDetail;
  TFhirExplanationOfBenefitItemDetailSubDetailList = fhir5_resources_financial.TFhirExplanationOfBenefitItemDetailSubDetailList;
  TFhirExplanationOfBenefitAddItem = fhir5_resources_financial.TFhirExplanationOfBenefitAddItem;
  TFhirExplanationOfBenefitAddItemList = fhir5_resources_financial.TFhirExplanationOfBenefitAddItemList;
  TFhirExplanationOfBenefitAddItemBodySite = fhir5_resources_financial.TFhirExplanationOfBenefitAddItemBodySite;
  TFhirExplanationOfBenefitAddItemBodySiteList = fhir5_resources_financial.TFhirExplanationOfBenefitAddItemBodySiteList;
  TFhirExplanationOfBenefitAddItemDetail = fhir5_resources_financial.TFhirExplanationOfBenefitAddItemDetail;
  TFhirExplanationOfBenefitAddItemDetailList = fhir5_resources_financial.TFhirExplanationOfBenefitAddItemDetailList;
  TFhirExplanationOfBenefitAddItemDetailSubDetail = fhir5_resources_financial.TFhirExplanationOfBenefitAddItemDetailSubDetail;
  TFhirExplanationOfBenefitAddItemDetailSubDetailList = fhir5_resources_financial.TFhirExplanationOfBenefitAddItemDetailSubDetailList;
  TFhirExplanationOfBenefitTotal = fhir5_resources_financial.TFhirExplanationOfBenefitTotal;
  TFhirExplanationOfBenefitTotalList = fhir5_resources_financial.TFhirExplanationOfBenefitTotalList;
  TFhirExplanationOfBenefitPayment = fhir5_resources_financial.TFhirExplanationOfBenefitPayment;
  TFhirExplanationOfBenefitPaymentList = fhir5_resources_financial.TFhirExplanationOfBenefitPaymentList;
  TFhirExplanationOfBenefitProcessNote = fhir5_resources_financial.TFhirExplanationOfBenefitProcessNote;
  TFhirExplanationOfBenefitProcessNoteList = fhir5_resources_financial.TFhirExplanationOfBenefitProcessNoteList;
  TFhirExplanationOfBenefitBenefitBalance = fhir5_resources_financial.TFhirExplanationOfBenefitBenefitBalance;
  TFhirExplanationOfBenefitBenefitBalanceList = fhir5_resources_financial.TFhirExplanationOfBenefitBenefitBalanceList;
  TFhirExplanationOfBenefitBenefitBalanceFinancial = fhir5_resources_financial.TFhirExplanationOfBenefitBenefitBalanceFinancial;
  TFhirExplanationOfBenefitBenefitBalanceFinancialList = fhir5_resources_financial.TFhirExplanationOfBenefitBenefitBalanceFinancialList;
  TFhirExplanationOfBenefit = fhir5_resources_financial.TFhirExplanationOfBenefit;
  TFhirExplanationOfBenefitList = fhir5_resources_financial.TFhirExplanationOfBenefitList;
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  TFhirFamilyMemberHistoryCondition = fhir5_resources_clinical.TFhirFamilyMemberHistoryCondition;
  TFhirFamilyMemberHistoryConditionList = fhir5_resources_clinical.TFhirFamilyMemberHistoryConditionList;
  TFhirFamilyMemberHistoryProcedure = fhir5_resources_clinical.TFhirFamilyMemberHistoryProcedure;
  TFhirFamilyMemberHistoryProcedureList = fhir5_resources_clinical.TFhirFamilyMemberHistoryProcedureList;
  TFhirFamilyMemberHistory = fhir5_resources_clinical.TFhirFamilyMemberHistory;
  TFhirFamilyMemberHistoryList = fhir5_resources_clinical.TFhirFamilyMemberHistoryList;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  TFhirFlag = fhir5_resources_clinical.TFhirFlag;
  TFhirFlagList = fhir5_resources_clinical.TFhirFlagList;
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_FORMULARYITEM}
  TFhirFormularyItem = fhir5_resources_medications.TFhirFormularyItem;
  TFhirFormularyItemList = fhir5_resources_medications.TFhirFormularyItemList;
{$ENDIF FHIR_FORMULARYITEM}
{$IFDEF FHIR_GENOMICSTUDY}
  TFhirGenomicStudyAnalysis = fhir5_resources_clinical.TFhirGenomicStudyAnalysis;
  TFhirGenomicStudyAnalysisList = fhir5_resources_clinical.TFhirGenomicStudyAnalysisList;
  TFhirGenomicStudyAnalysisInput = fhir5_resources_clinical.TFhirGenomicStudyAnalysisInput;
  TFhirGenomicStudyAnalysisInputList = fhir5_resources_clinical.TFhirGenomicStudyAnalysisInputList;
  TFhirGenomicStudyAnalysisOutput = fhir5_resources_clinical.TFhirGenomicStudyAnalysisOutput;
  TFhirGenomicStudyAnalysisOutputList = fhir5_resources_clinical.TFhirGenomicStudyAnalysisOutputList;
  TFhirGenomicStudyAnalysisPerformer = fhir5_resources_clinical.TFhirGenomicStudyAnalysisPerformer;
  TFhirGenomicStudyAnalysisPerformerList = fhir5_resources_clinical.TFhirGenomicStudyAnalysisPerformerList;
  TFhirGenomicStudyAnalysisDevice = fhir5_resources_clinical.TFhirGenomicStudyAnalysisDevice;
  TFhirGenomicStudyAnalysisDeviceList = fhir5_resources_clinical.TFhirGenomicStudyAnalysisDeviceList;
  TFhirGenomicStudy = fhir5_resources_clinical.TFhirGenomicStudy;
  TFhirGenomicStudyList = fhir5_resources_clinical.TFhirGenomicStudyList;
{$ENDIF FHIR_GENOMICSTUDY}
{$IFDEF FHIR_GOAL}
  TFhirGoalTarget = fhir5_resources_clinical.TFhirGoalTarget;
  TFhirGoalTargetList = fhir5_resources_clinical.TFhirGoalTargetList;
  TFhirGoal = fhir5_resources_clinical.TFhirGoal;
  TFhirGoalList = fhir5_resources_clinical.TFhirGoalList;
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GRAPHDEFINITION}
  TFhirGraphDefinitionLink = fhir5_resources_canonical.TFhirGraphDefinitionLink;
  TFhirGraphDefinitionLinkList = fhir5_resources_canonical.TFhirGraphDefinitionLinkList;
  TFhirGraphDefinitionLinkTarget = fhir5_resources_canonical.TFhirGraphDefinitionLinkTarget;
  TFhirGraphDefinitionLinkTargetList = fhir5_resources_canonical.TFhirGraphDefinitionLinkTargetList;
  TFhirGraphDefinitionLinkTargetCompartment = fhir5_resources_canonical.TFhirGraphDefinitionLinkTargetCompartment;
  TFhirGraphDefinitionLinkTargetCompartmentList = fhir5_resources_canonical.TFhirGraphDefinitionLinkTargetCompartmentList;
  TFhirGraphDefinition = fhir5_resources_canonical.TFhirGraphDefinition;
  TFhirGraphDefinitionList = fhir5_resources_canonical.TFhirGraphDefinitionList;
{$ENDIF FHIR_GRAPHDEFINITION}
{$IFDEF FHIR_GROUP}
  TFhirGroupCharacteristic = fhir5_resources_admin.TFhirGroupCharacteristic;
  TFhirGroupCharacteristicList = fhir5_resources_admin.TFhirGroupCharacteristicList;
  TFhirGroupMember = fhir5_resources_admin.TFhirGroupMember;
  TFhirGroupMemberList = fhir5_resources_admin.TFhirGroupMemberList;
  TFhirGroup = fhir5_resources_admin.TFhirGroup;
  TFhirGroupList = fhir5_resources_admin.TFhirGroupList;
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_GUIDANCERESPONSE}
  TFhirGuidanceResponse = fhir5_resources_other.TFhirGuidanceResponse;
  TFhirGuidanceResponseList = fhir5_resources_other.TFhirGuidanceResponseList;
{$ENDIF FHIR_GUIDANCERESPONSE}
{$IFDEF FHIR_HEALTHCARESERVICE}
  TFhirHealthcareServiceEligibility = fhir5_resources_admin.TFhirHealthcareServiceEligibility;
  TFhirHealthcareServiceEligibilityList = fhir5_resources_admin.TFhirHealthcareServiceEligibilityList;
  TFhirHealthcareService = fhir5_resources_admin.TFhirHealthcareService;
  TFhirHealthcareServiceList = fhir5_resources_admin.TFhirHealthcareServiceList;
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGSELECTION}
  TFhirImagingSelectionPerformer = fhir5_resources_clinical.TFhirImagingSelectionPerformer;
  TFhirImagingSelectionPerformerList = fhir5_resources_clinical.TFhirImagingSelectionPerformerList;
  TFhirImagingSelectionInstance = fhir5_resources_clinical.TFhirImagingSelectionInstance;
  TFhirImagingSelectionInstanceList = fhir5_resources_clinical.TFhirImagingSelectionInstanceList;
  TFhirImagingSelectionInstanceImageRegion = fhir5_resources_clinical.TFhirImagingSelectionInstanceImageRegion;
  TFhirImagingSelectionInstanceImageRegionList = fhir5_resources_clinical.TFhirImagingSelectionInstanceImageRegionList;
  TFhirImagingSelectionImageRegion = fhir5_resources_clinical.TFhirImagingSelectionImageRegion;
  TFhirImagingSelectionImageRegionList = fhir5_resources_clinical.TFhirImagingSelectionImageRegionList;
  TFhirImagingSelection = fhir5_resources_clinical.TFhirImagingSelection;
  TFhirImagingSelectionList = fhir5_resources_clinical.TFhirImagingSelectionList;
{$ENDIF FHIR_IMAGINGSELECTION}
{$IFDEF FHIR_IMAGINGSTUDY}
  TFhirImagingStudySeries = fhir5_resources_clinical.TFhirImagingStudySeries;
  TFhirImagingStudySeriesList = fhir5_resources_clinical.TFhirImagingStudySeriesList;
  TFhirImagingStudySeriesPerformer = fhir5_resources_clinical.TFhirImagingStudySeriesPerformer;
  TFhirImagingStudySeriesPerformerList = fhir5_resources_clinical.TFhirImagingStudySeriesPerformerList;
  TFhirImagingStudySeriesInstance = fhir5_resources_clinical.TFhirImagingStudySeriesInstance;
  TFhirImagingStudySeriesInstanceList = fhir5_resources_clinical.TFhirImagingStudySeriesInstanceList;
  TFhirImagingStudy = fhir5_resources_clinical.TFhirImagingStudy;
  TFhirImagingStudyList = fhir5_resources_clinical.TFhirImagingStudyList;
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  TFhirImmunizationPerformer = fhir5_resources_clinical.TFhirImmunizationPerformer;
  TFhirImmunizationPerformerList = fhir5_resources_clinical.TFhirImmunizationPerformerList;
  TFhirImmunizationProgramEligibility = fhir5_resources_clinical.TFhirImmunizationProgramEligibility;
  TFhirImmunizationProgramEligibilityList = fhir5_resources_clinical.TFhirImmunizationProgramEligibilityList;
  TFhirImmunizationReaction = fhir5_resources_clinical.TFhirImmunizationReaction;
  TFhirImmunizationReactionList = fhir5_resources_clinical.TFhirImmunizationReactionList;
  TFhirImmunizationProtocolApplied = fhir5_resources_clinical.TFhirImmunizationProtocolApplied;
  TFhirImmunizationProtocolAppliedList = fhir5_resources_clinical.TFhirImmunizationProtocolAppliedList;
  TFhirImmunization = fhir5_resources_clinical.TFhirImmunization;
  TFhirImmunizationList = fhir5_resources_clinical.TFhirImmunizationList;
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  TFhirImmunizationEvaluation = fhir5_resources_clinical.TFhirImmunizationEvaluation;
  TFhirImmunizationEvaluationList = fhir5_resources_clinical.TFhirImmunizationEvaluationList;
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  TFhirImmunizationRecommendationRecommendation = fhir5_resources_clinical.TFhirImmunizationRecommendationRecommendation;
  TFhirImmunizationRecommendationRecommendationList = fhir5_resources_clinical.TFhirImmunizationRecommendationRecommendationList;
  TFhirImmunizationRecommendationRecommendationDateCriterion = fhir5_resources_clinical.TFhirImmunizationRecommendationRecommendationDateCriterion;
  TFhirImmunizationRecommendationRecommendationDateCriterionList = fhir5_resources_clinical.TFhirImmunizationRecommendationRecommendationDateCriterionList;
  TFhirImmunizationRecommendation = fhir5_resources_clinical.TFhirImmunizationRecommendation;
  TFhirImmunizationRecommendationList = fhir5_resources_clinical.TFhirImmunizationRecommendationList;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  TFhirImplementationGuideDependsOn = fhir5_resources_canonical.TFhirImplementationGuideDependsOn;
  TFhirImplementationGuideDependsOnList = fhir5_resources_canonical.TFhirImplementationGuideDependsOnList;
  TFhirImplementationGuideGlobal = fhir5_resources_canonical.TFhirImplementationGuideGlobal;
  TFhirImplementationGuideGlobalList = fhir5_resources_canonical.TFhirImplementationGuideGlobalList;
  TFhirImplementationGuideDefinition = fhir5_resources_canonical.TFhirImplementationGuideDefinition;
  TFhirImplementationGuideDefinitionList = fhir5_resources_canonical.TFhirImplementationGuideDefinitionList;
  TFhirImplementationGuideDefinitionGrouping = fhir5_resources_canonical.TFhirImplementationGuideDefinitionGrouping;
  TFhirImplementationGuideDefinitionGroupingList = fhir5_resources_canonical.TFhirImplementationGuideDefinitionGroupingList;
  TFhirImplementationGuideDefinitionResource = fhir5_resources_canonical.TFhirImplementationGuideDefinitionResource;
  TFhirImplementationGuideDefinitionResourceList = fhir5_resources_canonical.TFhirImplementationGuideDefinitionResourceList;
  TFhirImplementationGuideDefinitionPage = fhir5_resources_canonical.TFhirImplementationGuideDefinitionPage;
  TFhirImplementationGuideDefinitionPageList = fhir5_resources_canonical.TFhirImplementationGuideDefinitionPageList;
  TFhirImplementationGuideDefinitionParameter = fhir5_resources_canonical.TFhirImplementationGuideDefinitionParameter;
  TFhirImplementationGuideDefinitionParameterList = fhir5_resources_canonical.TFhirImplementationGuideDefinitionParameterList;
  TFhirImplementationGuideDefinitionTemplate = fhir5_resources_canonical.TFhirImplementationGuideDefinitionTemplate;
  TFhirImplementationGuideDefinitionTemplateList = fhir5_resources_canonical.TFhirImplementationGuideDefinitionTemplateList;
  TFhirImplementationGuideManifest = fhir5_resources_canonical.TFhirImplementationGuideManifest;
  TFhirImplementationGuideManifestList = fhir5_resources_canonical.TFhirImplementationGuideManifestList;
  TFhirImplementationGuideManifestResource = fhir5_resources_canonical.TFhirImplementationGuideManifestResource;
  TFhirImplementationGuideManifestResourceList = fhir5_resources_canonical.TFhirImplementationGuideManifestResourceList;
  TFhirImplementationGuideManifestPage = fhir5_resources_canonical.TFhirImplementationGuideManifestPage;
  TFhirImplementationGuideManifestPageList = fhir5_resources_canonical.TFhirImplementationGuideManifestPageList;
  TFhirImplementationGuide = fhir5_resources_canonical.TFhirImplementationGuide;
  TFhirImplementationGuideList = fhir5_resources_canonical.TFhirImplementationGuideList;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_INGREDIENT}
  TFhirIngredientManufacturer = fhir5_resources_medications.TFhirIngredientManufacturer;
  TFhirIngredientManufacturerList = fhir5_resources_medications.TFhirIngredientManufacturerList;
  TFhirIngredientSubstance = fhir5_resources_medications.TFhirIngredientSubstance;
  TFhirIngredientSubstanceList = fhir5_resources_medications.TFhirIngredientSubstanceList;
  TFhirIngredientSubstanceStrength = fhir5_resources_medications.TFhirIngredientSubstanceStrength;
  TFhirIngredientSubstanceStrengthList = fhir5_resources_medications.TFhirIngredientSubstanceStrengthList;
  TFhirIngredientSubstanceStrengthReferenceStrength = fhir5_resources_medications.TFhirIngredientSubstanceStrengthReferenceStrength;
  TFhirIngredientSubstanceStrengthReferenceStrengthList = fhir5_resources_medications.TFhirIngredientSubstanceStrengthReferenceStrengthList;
  TFhirIngredient = fhir5_resources_medications.TFhirIngredient;
  TFhirIngredientList = fhir5_resources_medications.TFhirIngredientList;
{$ENDIF FHIR_INGREDIENT}
{$IFDEF FHIR_INSURANCEPLAN}
  TFhirInsurancePlanCoverage = fhir5_resources_financial.TFhirInsurancePlanCoverage;
  TFhirInsurancePlanCoverageList = fhir5_resources_financial.TFhirInsurancePlanCoverageList;
  TFhirInsurancePlanCoverageBenefit = fhir5_resources_financial.TFhirInsurancePlanCoverageBenefit;
  TFhirInsurancePlanCoverageBenefitList = fhir5_resources_financial.TFhirInsurancePlanCoverageBenefitList;
  TFhirInsurancePlanCoverageBenefitLimit = fhir5_resources_financial.TFhirInsurancePlanCoverageBenefitLimit;
  TFhirInsurancePlanCoverageBenefitLimitList = fhir5_resources_financial.TFhirInsurancePlanCoverageBenefitLimitList;
  TFhirInsurancePlanPlan = fhir5_resources_financial.TFhirInsurancePlanPlan;
  TFhirInsurancePlanPlanList = fhir5_resources_financial.TFhirInsurancePlanPlanList;
  TFhirInsurancePlanPlanGeneralCost = fhir5_resources_financial.TFhirInsurancePlanPlanGeneralCost;
  TFhirInsurancePlanPlanGeneralCostList = fhir5_resources_financial.TFhirInsurancePlanPlanGeneralCostList;
  TFhirInsurancePlanPlanSpecificCost = fhir5_resources_financial.TFhirInsurancePlanPlanSpecificCost;
  TFhirInsurancePlanPlanSpecificCostList = fhir5_resources_financial.TFhirInsurancePlanPlanSpecificCostList;
  TFhirInsurancePlanPlanSpecificCostBenefit = fhir5_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefit;
  TFhirInsurancePlanPlanSpecificCostBenefitList = fhir5_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefitList;
  TFhirInsurancePlanPlanSpecificCostBenefitCost = fhir5_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefitCost;
  TFhirInsurancePlanPlanSpecificCostBenefitCostList = fhir5_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefitCostList;
  TFhirInsurancePlan = fhir5_resources_financial.TFhirInsurancePlan;
  TFhirInsurancePlanList = fhir5_resources_financial.TFhirInsurancePlanList;
{$ENDIF FHIR_INSURANCEPLAN}
{$IFDEF FHIR_INVENTORYREPORT}
  TFhirInventoryReportInventoryListing = fhir5_resources_other.TFhirInventoryReportInventoryListing;
  TFhirInventoryReportInventoryListingList = fhir5_resources_other.TFhirInventoryReportInventoryListingList;
  TFhirInventoryReportInventoryListingItems = fhir5_resources_other.TFhirInventoryReportInventoryListingItems;
  TFhirInventoryReportInventoryListingItemsList = fhir5_resources_other.TFhirInventoryReportInventoryListingItemsList;
  TFhirInventoryReport = fhir5_resources_other.TFhirInventoryReport;
  TFhirInventoryReportList = fhir5_resources_other.TFhirInventoryReportList;
{$ENDIF FHIR_INVENTORYREPORT}
{$IFDEF FHIR_INVOICE}
  TFhirInvoiceParticipant = fhir5_resources_financial.TFhirInvoiceParticipant;
  TFhirInvoiceParticipantList = fhir5_resources_financial.TFhirInvoiceParticipantList;
  TFhirInvoiceLineItem = fhir5_resources_financial.TFhirInvoiceLineItem;
  TFhirInvoiceLineItemList = fhir5_resources_financial.TFhirInvoiceLineItemList;
  TFhirInvoice = fhir5_resources_financial.TFhirInvoice;
  TFhirInvoiceList = fhir5_resources_financial.TFhirInvoiceList;
{$ENDIF FHIR_INVOICE}
{$IFDEF FHIR_LIBRARY}
  TFhirLibrary = fhir5_resources_canonical.TFhirLibrary;
  TFhirLibraryList = fhir5_resources_canonical.TFhirLibraryList;
{$ENDIF FHIR_LIBRARY}
{$IFDEF FHIR_LINKAGE}
  TFhirLinkageItem = fhir5_resources_other.TFhirLinkageItem;
  TFhirLinkageItemList = fhir5_resources_other.TFhirLinkageItemList;
  TFhirLinkage = fhir5_resources_other.TFhirLinkage;
  TFhirLinkageList = fhir5_resources_other.TFhirLinkageList;
{$ENDIF FHIR_LINKAGE}
{$IFDEF FHIR_LIST}
  TFhirListEntry = fhir5_resources_other.TFhirListEntry;
  TFhirListEntryList = fhir5_resources_other.TFhirListEntryList;
  TFhirList = fhir5_resources_other.TFhirList;
  TFhirListList = fhir5_resources_other.TFhirListList;
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_LOCATION}
  TFhirLocationPosition = fhir5_resources_admin.TFhirLocationPosition;
  TFhirLocationPositionList = fhir5_resources_admin.TFhirLocationPositionList;
  TFhirLocation = fhir5_resources_admin.TFhirLocation;
  TFhirLocationList = fhir5_resources_admin.TFhirLocationList;
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
  TFhirManufacturedItemDefinitionProperty = fhir5_resources_medications.TFhirManufacturedItemDefinitionProperty;
  TFhirManufacturedItemDefinitionPropertyList = fhir5_resources_medications.TFhirManufacturedItemDefinitionPropertyList;
  TFhirManufacturedItemDefinitionComponent = fhir5_resources_medications.TFhirManufacturedItemDefinitionComponent;
  TFhirManufacturedItemDefinitionComponentList = fhir5_resources_medications.TFhirManufacturedItemDefinitionComponentList;
  TFhirManufacturedItemDefinitionComponentConstituent = fhir5_resources_medications.TFhirManufacturedItemDefinitionComponentConstituent;
  TFhirManufacturedItemDefinitionComponentConstituentList = fhir5_resources_medications.TFhirManufacturedItemDefinitionComponentConstituentList;
  TFhirManufacturedItemDefinition = fhir5_resources_medications.TFhirManufacturedItemDefinition;
  TFhirManufacturedItemDefinitionList = fhir5_resources_medications.TFhirManufacturedItemDefinitionList;
{$ENDIF FHIR_MANUFACTUREDITEMDEFINITION}
{$IFDEF FHIR_MEASURE}
  TFhirMeasureTerm = fhir5_resources_canonical.TFhirMeasureTerm;
  TFhirMeasureTermList = fhir5_resources_canonical.TFhirMeasureTermList;
  TFhirMeasureGroup = fhir5_resources_canonical.TFhirMeasureGroup;
  TFhirMeasureGroupList = fhir5_resources_canonical.TFhirMeasureGroupList;
  TFhirMeasureGroupPopulation = fhir5_resources_canonical.TFhirMeasureGroupPopulation;
  TFhirMeasureGroupPopulationList = fhir5_resources_canonical.TFhirMeasureGroupPopulationList;
  TFhirMeasureGroupStratifier = fhir5_resources_canonical.TFhirMeasureGroupStratifier;
  TFhirMeasureGroupStratifierList = fhir5_resources_canonical.TFhirMeasureGroupStratifierList;
  TFhirMeasureGroupStratifierComponent = fhir5_resources_canonical.TFhirMeasureGroupStratifierComponent;
  TFhirMeasureGroupStratifierComponentList = fhir5_resources_canonical.TFhirMeasureGroupStratifierComponentList;
  TFhirMeasureSupplementalData = fhir5_resources_canonical.TFhirMeasureSupplementalData;
  TFhirMeasureSupplementalDataList = fhir5_resources_canonical.TFhirMeasureSupplementalDataList;
  TFhirMeasure = fhir5_resources_canonical.TFhirMeasure;
  TFhirMeasureList = fhir5_resources_canonical.TFhirMeasureList;
{$ENDIF FHIR_MEASURE}
{$IFDEF FHIR_MEASUREREPORT}
  TFhirMeasureReportGroup = fhir5_resources_other.TFhirMeasureReportGroup;
  TFhirMeasureReportGroupList = fhir5_resources_other.TFhirMeasureReportGroupList;
  TFhirMeasureReportGroupPopulation = fhir5_resources_other.TFhirMeasureReportGroupPopulation;
  TFhirMeasureReportGroupPopulationList = fhir5_resources_other.TFhirMeasureReportGroupPopulationList;
  TFhirMeasureReportGroupStratifier = fhir5_resources_other.TFhirMeasureReportGroupStratifier;
  TFhirMeasureReportGroupStratifierList = fhir5_resources_other.TFhirMeasureReportGroupStratifierList;
  TFhirMeasureReportGroupStratifierStratum = fhir5_resources_other.TFhirMeasureReportGroupStratifierStratum;
  TFhirMeasureReportGroupStratifierStratumList = fhir5_resources_other.TFhirMeasureReportGroupStratifierStratumList;
  TFhirMeasureReportGroupStratifierStratumComponent = fhir5_resources_other.TFhirMeasureReportGroupStratifierStratumComponent;
  TFhirMeasureReportGroupStratifierStratumComponentList = fhir5_resources_other.TFhirMeasureReportGroupStratifierStratumComponentList;
  TFhirMeasureReportGroupStratifierStratumPopulation = fhir5_resources_other.TFhirMeasureReportGroupStratifierStratumPopulation;
  TFhirMeasureReportGroupStratifierStratumPopulationList = fhir5_resources_other.TFhirMeasureReportGroupStratifierStratumPopulationList;
  TFhirMeasureReport = fhir5_resources_other.TFhirMeasureReport;
  TFhirMeasureReportList = fhir5_resources_other.TFhirMeasureReportList;
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MEDICATION}
  TFhirMedicationIngredient = fhir5_resources_medications.TFhirMedicationIngredient;
  TFhirMedicationIngredientList = fhir5_resources_medications.TFhirMedicationIngredientList;
  TFhirMedicationBatch = fhir5_resources_medications.TFhirMedicationBatch;
  TFhirMedicationBatchList = fhir5_resources_medications.TFhirMedicationBatchList;
  TFhirMedication = fhir5_resources_medications.TFhirMedication;
  TFhirMedicationList = fhir5_resources_medications.TFhirMedicationList;
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  TFhirMedicationAdministrationPerformer = fhir5_resources_clinical.TFhirMedicationAdministrationPerformer;
  TFhirMedicationAdministrationPerformerList = fhir5_resources_clinical.TFhirMedicationAdministrationPerformerList;
  TFhirMedicationAdministrationDosage = fhir5_resources_clinical.TFhirMedicationAdministrationDosage;
  TFhirMedicationAdministrationDosageList = fhir5_resources_clinical.TFhirMedicationAdministrationDosageList;
  TFhirMedicationAdministration = fhir5_resources_clinical.TFhirMedicationAdministration;
  TFhirMedicationAdministrationList = fhir5_resources_clinical.TFhirMedicationAdministrationList;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  TFhirMedicationDispensePerformer = fhir5_resources_clinical.TFhirMedicationDispensePerformer;
  TFhirMedicationDispensePerformerList = fhir5_resources_clinical.TFhirMedicationDispensePerformerList;
  TFhirMedicationDispenseSubstitution = fhir5_resources_clinical.TFhirMedicationDispenseSubstitution;
  TFhirMedicationDispenseSubstitutionList = fhir5_resources_clinical.TFhirMedicationDispenseSubstitutionList;
  TFhirMedicationDispense = fhir5_resources_clinical.TFhirMedicationDispense;
  TFhirMedicationDispenseList = fhir5_resources_clinical.TFhirMedicationDispenseList;
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  TFhirMedicationKnowledgeRelatedMedicationKnowledge = fhir5_resources_medications.TFhirMedicationKnowledgeRelatedMedicationKnowledge;
  TFhirMedicationKnowledgeRelatedMedicationKnowledgeList = fhir5_resources_medications.TFhirMedicationKnowledgeRelatedMedicationKnowledgeList;
  TFhirMedicationKnowledgeMonograph = fhir5_resources_medications.TFhirMedicationKnowledgeMonograph;
  TFhirMedicationKnowledgeMonographList = fhir5_resources_medications.TFhirMedicationKnowledgeMonographList;
  TFhirMedicationKnowledgeCost = fhir5_resources_medications.TFhirMedicationKnowledgeCost;
  TFhirMedicationKnowledgeCostList = fhir5_resources_medications.TFhirMedicationKnowledgeCostList;
  TFhirMedicationKnowledgeMonitoringProgram = fhir5_resources_medications.TFhirMedicationKnowledgeMonitoringProgram;
  TFhirMedicationKnowledgeMonitoringProgramList = fhir5_resources_medications.TFhirMedicationKnowledgeMonitoringProgramList;
  TFhirMedicationKnowledgeIndicationGuideline = fhir5_resources_medications.TFhirMedicationKnowledgeIndicationGuideline;
  TFhirMedicationKnowledgeIndicationGuidelineList = fhir5_resources_medications.TFhirMedicationKnowledgeIndicationGuidelineList;
  TFhirMedicationKnowledgeIndicationGuidelineDosingGuideline = fhir5_resources_medications.TFhirMedicationKnowledgeIndicationGuidelineDosingGuideline;
  TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelineList = fhir5_resources_medications.TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelineList;
  TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelineDosage = fhir5_resources_medications.TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelineDosage;
  TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelineDosageList = fhir5_resources_medications.TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelineDosageList;
  TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelinePatientCharacteristic = fhir5_resources_medications.TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelinePatientCharacteristic;
  TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelinePatientCharacteristicList = fhir5_resources_medications.TFhirMedicationKnowledgeIndicationGuidelineDosingGuidelinePatientCharacteristicList;
  TFhirMedicationKnowledgeMedicineClassification = fhir5_resources_medications.TFhirMedicationKnowledgeMedicineClassification;
  TFhirMedicationKnowledgeMedicineClassificationList = fhir5_resources_medications.TFhirMedicationKnowledgeMedicineClassificationList;
  TFhirMedicationKnowledgePackaging = fhir5_resources_medications.TFhirMedicationKnowledgePackaging;
  TFhirMedicationKnowledgePackagingList = fhir5_resources_medications.TFhirMedicationKnowledgePackagingList;
  TFhirMedicationKnowledgeStorageGuideline = fhir5_resources_medications.TFhirMedicationKnowledgeStorageGuideline;
  TFhirMedicationKnowledgeStorageGuidelineList = fhir5_resources_medications.TFhirMedicationKnowledgeStorageGuidelineList;
  TFhirMedicationKnowledgeStorageGuidelineEnvironmentalSetting = fhir5_resources_medications.TFhirMedicationKnowledgeStorageGuidelineEnvironmentalSetting;
  TFhirMedicationKnowledgeStorageGuidelineEnvironmentalSettingList = fhir5_resources_medications.TFhirMedicationKnowledgeStorageGuidelineEnvironmentalSettingList;
  TFhirMedicationKnowledgeRegulatory = fhir5_resources_medications.TFhirMedicationKnowledgeRegulatory;
  TFhirMedicationKnowledgeRegulatoryList = fhir5_resources_medications.TFhirMedicationKnowledgeRegulatoryList;
  TFhirMedicationKnowledgeRegulatorySubstitution = fhir5_resources_medications.TFhirMedicationKnowledgeRegulatorySubstitution;
  TFhirMedicationKnowledgeRegulatorySubstitutionList = fhir5_resources_medications.TFhirMedicationKnowledgeRegulatorySubstitutionList;
  TFhirMedicationKnowledgeRegulatoryMaxDispense = fhir5_resources_medications.TFhirMedicationKnowledgeRegulatoryMaxDispense;
  TFhirMedicationKnowledgeRegulatoryMaxDispenseList = fhir5_resources_medications.TFhirMedicationKnowledgeRegulatoryMaxDispenseList;
  TFhirMedicationKnowledgeDefinitional = fhir5_resources_medications.TFhirMedicationKnowledgeDefinitional;
  TFhirMedicationKnowledgeDefinitionalList = fhir5_resources_medications.TFhirMedicationKnowledgeDefinitionalList;
  TFhirMedicationKnowledgeDefinitionalIngredient = fhir5_resources_medications.TFhirMedicationKnowledgeDefinitionalIngredient;
  TFhirMedicationKnowledgeDefinitionalIngredientList = fhir5_resources_medications.TFhirMedicationKnowledgeDefinitionalIngredientList;
  TFhirMedicationKnowledgeDefinitionalDrugCharacteristic = fhir5_resources_medications.TFhirMedicationKnowledgeDefinitionalDrugCharacteristic;
  TFhirMedicationKnowledgeDefinitionalDrugCharacteristicList = fhir5_resources_medications.TFhirMedicationKnowledgeDefinitionalDrugCharacteristicList;
  TFhirMedicationKnowledge = fhir5_resources_medications.TFhirMedicationKnowledge;
  TFhirMedicationKnowledgeList = fhir5_resources_medications.TFhirMedicationKnowledgeList;
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  TFhirMedicationRequestDose = fhir5_resources_clinical.TFhirMedicationRequestDose;
  TFhirMedicationRequestDoseList = fhir5_resources_clinical.TFhirMedicationRequestDoseList;
  TFhirMedicationRequestDispenseRequest = fhir5_resources_clinical.TFhirMedicationRequestDispenseRequest;
  TFhirMedicationRequestDispenseRequestList = fhir5_resources_clinical.TFhirMedicationRequestDispenseRequestList;
  TFhirMedicationRequestDispenseRequestInitialFill = fhir5_resources_clinical.TFhirMedicationRequestDispenseRequestInitialFill;
  TFhirMedicationRequestDispenseRequestInitialFillList = fhir5_resources_clinical.TFhirMedicationRequestDispenseRequestInitialFillList;
  TFhirMedicationRequestSubstitution = fhir5_resources_clinical.TFhirMedicationRequestSubstitution;
  TFhirMedicationRequestSubstitutionList = fhir5_resources_clinical.TFhirMedicationRequestSubstitutionList;
  TFhirMedicationRequest = fhir5_resources_clinical.TFhirMedicationRequest;
  TFhirMedicationRequestList = fhir5_resources_clinical.TFhirMedicationRequestList;
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONUSAGE}
  TFhirMedicationUsageAdherence = fhir5_resources_clinical.TFhirMedicationUsageAdherence;
  TFhirMedicationUsageAdherenceList = fhir5_resources_clinical.TFhirMedicationUsageAdherenceList;
  TFhirMedicationUsage = fhir5_resources_clinical.TFhirMedicationUsage;
  TFhirMedicationUsageList = fhir5_resources_clinical.TFhirMedicationUsageList;
{$ENDIF FHIR_MEDICATIONUSAGE}
{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
  TFhirMedicinalProductDefinitionContact = fhir5_resources_medications.TFhirMedicinalProductDefinitionContact;
  TFhirMedicinalProductDefinitionContactList = fhir5_resources_medications.TFhirMedicinalProductDefinitionContactList;
  TFhirMedicinalProductDefinitionName = fhir5_resources_medications.TFhirMedicinalProductDefinitionName;
  TFhirMedicinalProductDefinitionNameList = fhir5_resources_medications.TFhirMedicinalProductDefinitionNameList;
  TFhirMedicinalProductDefinitionNamePart = fhir5_resources_medications.TFhirMedicinalProductDefinitionNamePart;
  TFhirMedicinalProductDefinitionNamePartList = fhir5_resources_medications.TFhirMedicinalProductDefinitionNamePartList;
  TFhirMedicinalProductDefinitionNameUsage = fhir5_resources_medications.TFhirMedicinalProductDefinitionNameUsage;
  TFhirMedicinalProductDefinitionNameUsageList = fhir5_resources_medications.TFhirMedicinalProductDefinitionNameUsageList;
  TFhirMedicinalProductDefinitionCrossReference = fhir5_resources_medications.TFhirMedicinalProductDefinitionCrossReference;
  TFhirMedicinalProductDefinitionCrossReferenceList = fhir5_resources_medications.TFhirMedicinalProductDefinitionCrossReferenceList;
  TFhirMedicinalProductDefinitionOperation = fhir5_resources_medications.TFhirMedicinalProductDefinitionOperation;
  TFhirMedicinalProductDefinitionOperationList = fhir5_resources_medications.TFhirMedicinalProductDefinitionOperationList;
  TFhirMedicinalProductDefinitionCharacteristic = fhir5_resources_medications.TFhirMedicinalProductDefinitionCharacteristic;
  TFhirMedicinalProductDefinitionCharacteristicList = fhir5_resources_medications.TFhirMedicinalProductDefinitionCharacteristicList;
  TFhirMedicinalProductDefinition = fhir5_resources_medications.TFhirMedicinalProductDefinition;
  TFhirMedicinalProductDefinitionList = fhir5_resources_medications.TFhirMedicinalProductDefinitionList;
{$ENDIF FHIR_MEDICINALPRODUCTDEFINITION}
{$IFDEF FHIR_MESSAGEDEFINITION}
  TFhirMessageDefinitionFocus = fhir5_resources_canonical.TFhirMessageDefinitionFocus;
  TFhirMessageDefinitionFocusList = fhir5_resources_canonical.TFhirMessageDefinitionFocusList;
  TFhirMessageDefinitionAllowedResponse = fhir5_resources_canonical.TFhirMessageDefinitionAllowedResponse;
  TFhirMessageDefinitionAllowedResponseList = fhir5_resources_canonical.TFhirMessageDefinitionAllowedResponseList;
  TFhirMessageDefinition = fhir5_resources_canonical.TFhirMessageDefinition;
  TFhirMessageDefinitionList = fhir5_resources_canonical.TFhirMessageDefinitionList;
{$ENDIF FHIR_MESSAGEDEFINITION}
{$IFDEF FHIR_MESSAGEHEADER}
  TFhirMessageHeaderDestination = fhir5_resources_other.TFhirMessageHeaderDestination;
  TFhirMessageHeaderDestinationList = fhir5_resources_other.TFhirMessageHeaderDestinationList;
  TFhirMessageHeaderSource = fhir5_resources_other.TFhirMessageHeaderSource;
  TFhirMessageHeaderSourceList = fhir5_resources_other.TFhirMessageHeaderSourceList;
  TFhirMessageHeaderResponse = fhir5_resources_other.TFhirMessageHeaderResponse;
  TFhirMessageHeaderResponseList = fhir5_resources_other.TFhirMessageHeaderResponseList;
  TFhirMessageHeader = fhir5_resources_other.TFhirMessageHeader;
  TFhirMessageHeaderList = fhir5_resources_other.TFhirMessageHeaderList;
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_MOLECULARSEQUENCE}
  TFhirMolecularSequenceRelative = fhir5_resources_clinical.TFhirMolecularSequenceRelative;
  TFhirMolecularSequenceRelativeList = fhir5_resources_clinical.TFhirMolecularSequenceRelativeList;
  TFhirMolecularSequenceRelativeStartingSequence = fhir5_resources_clinical.TFhirMolecularSequenceRelativeStartingSequence;
  TFhirMolecularSequenceRelativeStartingSequenceList = fhir5_resources_clinical.TFhirMolecularSequenceRelativeStartingSequenceList;
  TFhirMolecularSequenceRelativeEdit = fhir5_resources_clinical.TFhirMolecularSequenceRelativeEdit;
  TFhirMolecularSequenceRelativeEditList = fhir5_resources_clinical.TFhirMolecularSequenceRelativeEditList;
  TFhirMolecularSequence = fhir5_resources_clinical.TFhirMolecularSequence;
  TFhirMolecularSequenceList = fhir5_resources_clinical.TFhirMolecularSequenceList;
{$ENDIF FHIR_MOLECULARSEQUENCE}
{$IFDEF FHIR_NAMINGSYSTEM}
  TFhirNamingSystemUniqueId = fhir5_resources_canonical.TFhirNamingSystemUniqueId;
  TFhirNamingSystemUniqueIdList = fhir5_resources_canonical.TFhirNamingSystemUniqueIdList;
  TFhirNamingSystem = fhir5_resources_canonical.TFhirNamingSystem;
  TFhirNamingSystemList = fhir5_resources_canonical.TFhirNamingSystemList;
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_NUTRITIONINTAKE}
  TFhirNutritionIntakeConsumedItem = fhir5_resources_clinical.TFhirNutritionIntakeConsumedItem;
  TFhirNutritionIntakeConsumedItemList = fhir5_resources_clinical.TFhirNutritionIntakeConsumedItemList;
  TFhirNutritionIntakeIngredientLabel = fhir5_resources_clinical.TFhirNutritionIntakeIngredientLabel;
  TFhirNutritionIntakeIngredientLabelList = fhir5_resources_clinical.TFhirNutritionIntakeIngredientLabelList;
  TFhirNutritionIntakePerformer = fhir5_resources_clinical.TFhirNutritionIntakePerformer;
  TFhirNutritionIntakePerformerList = fhir5_resources_clinical.TFhirNutritionIntakePerformerList;
  TFhirNutritionIntake = fhir5_resources_clinical.TFhirNutritionIntake;
  TFhirNutritionIntakeList = fhir5_resources_clinical.TFhirNutritionIntakeList;
{$ENDIF FHIR_NUTRITIONINTAKE}
{$IFDEF FHIR_NUTRITIONORDER}
  TFhirNutritionOrderOralDiet = fhir5_resources_clinical.TFhirNutritionOrderOralDiet;
  TFhirNutritionOrderOralDietList = fhir5_resources_clinical.TFhirNutritionOrderOralDietList;
  TFhirNutritionOrderOralDietSchedule = fhir5_resources_clinical.TFhirNutritionOrderOralDietSchedule;
  TFhirNutritionOrderOralDietScheduleList = fhir5_resources_clinical.TFhirNutritionOrderOralDietScheduleList;
  TFhirNutritionOrderOralDietNutrient = fhir5_resources_clinical.TFhirNutritionOrderOralDietNutrient;
  TFhirNutritionOrderOralDietNutrientList = fhir5_resources_clinical.TFhirNutritionOrderOralDietNutrientList;
  TFhirNutritionOrderOralDietTexture = fhir5_resources_clinical.TFhirNutritionOrderOralDietTexture;
  TFhirNutritionOrderOralDietTextureList = fhir5_resources_clinical.TFhirNutritionOrderOralDietTextureList;
  TFhirNutritionOrderSupplement = fhir5_resources_clinical.TFhirNutritionOrderSupplement;
  TFhirNutritionOrderSupplementList = fhir5_resources_clinical.TFhirNutritionOrderSupplementList;
  TFhirNutritionOrderSupplementSchedule = fhir5_resources_clinical.TFhirNutritionOrderSupplementSchedule;
  TFhirNutritionOrderSupplementScheduleList = fhir5_resources_clinical.TFhirNutritionOrderSupplementScheduleList;
  TFhirNutritionOrderEnteralFormula = fhir5_resources_clinical.TFhirNutritionOrderEnteralFormula;
  TFhirNutritionOrderEnteralFormulaList = fhir5_resources_clinical.TFhirNutritionOrderEnteralFormulaList;
  TFhirNutritionOrderEnteralFormulaAdditive = fhir5_resources_clinical.TFhirNutritionOrderEnteralFormulaAdditive;
  TFhirNutritionOrderEnteralFormulaAdditiveList = fhir5_resources_clinical.TFhirNutritionOrderEnteralFormulaAdditiveList;
  TFhirNutritionOrderEnteralFormulaAdministration = fhir5_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministration;
  TFhirNutritionOrderEnteralFormulaAdministrationList = fhir5_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministrationList;
  TFhirNutritionOrderEnteralFormulaAdministrationSchedule = fhir5_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministrationSchedule;
  TFhirNutritionOrderEnteralFormulaAdministrationScheduleList = fhir5_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministrationScheduleList;
  TFhirNutritionOrder = fhir5_resources_clinical.TFhirNutritionOrder;
  TFhirNutritionOrderList = fhir5_resources_clinical.TFhirNutritionOrderList;
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_NUTRITIONPRODUCT}
  TFhirNutritionProductNutrient = fhir5_resources_medications.TFhirNutritionProductNutrient;
  TFhirNutritionProductNutrientList = fhir5_resources_medications.TFhirNutritionProductNutrientList;
  TFhirNutritionProductIngredient = fhir5_resources_medications.TFhirNutritionProductIngredient;
  TFhirNutritionProductIngredientList = fhir5_resources_medications.TFhirNutritionProductIngredientList;
  TFhirNutritionProductCharacteristic = fhir5_resources_medications.TFhirNutritionProductCharacteristic;
  TFhirNutritionProductCharacteristicList = fhir5_resources_medications.TFhirNutritionProductCharacteristicList;
  TFhirNutritionProductInstance = fhir5_resources_medications.TFhirNutritionProductInstance;
  TFhirNutritionProductInstanceList = fhir5_resources_medications.TFhirNutritionProductInstanceList;
  TFhirNutritionProduct = fhir5_resources_medications.TFhirNutritionProduct;
  TFhirNutritionProductList = fhir5_resources_medications.TFhirNutritionProductList;
{$ENDIF FHIR_NUTRITIONPRODUCT}
{$IFDEF FHIR_OBSERVATION}
  TFhirObservationTriggeredBy = fhir5_resources_clinical.TFhirObservationTriggeredBy;
  TFhirObservationTriggeredByList = fhir5_resources_clinical.TFhirObservationTriggeredByList;
  TFhirObservationReferenceRange = fhir5_resources_clinical.TFhirObservationReferenceRange;
  TFhirObservationReferenceRangeList = fhir5_resources_clinical.TFhirObservationReferenceRangeList;
  TFhirObservationComponent = fhir5_resources_clinical.TFhirObservationComponent;
  TFhirObservationComponentList = fhir5_resources_clinical.TFhirObservationComponentList;
  TFhirObservation = fhir5_resources_clinical.TFhirObservation;
  TFhirObservationList = fhir5_resources_clinical.TFhirObservationList;
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OBSERVATIONDEFINITION}
  TFhirObservationDefinitionQuantitativeDetails = fhir5_resources_canonical.TFhirObservationDefinitionQuantitativeDetails;
  TFhirObservationDefinitionQuantitativeDetailsList = fhir5_resources_canonical.TFhirObservationDefinitionQuantitativeDetailsList;
  TFhirObservationDefinitionQualifiedValue = fhir5_resources_canonical.TFhirObservationDefinitionQualifiedValue;
  TFhirObservationDefinitionQualifiedValueList = fhir5_resources_canonical.TFhirObservationDefinitionQualifiedValueList;
  TFhirObservationDefinitionComponent = fhir5_resources_canonical.TFhirObservationDefinitionComponent;
  TFhirObservationDefinitionComponentList = fhir5_resources_canonical.TFhirObservationDefinitionComponentList;
  TFhirObservationDefinition = fhir5_resources_canonical.TFhirObservationDefinition;
  TFhirObservationDefinitionList = fhir5_resources_canonical.TFhirObservationDefinitionList;
{$ENDIF FHIR_OBSERVATIONDEFINITION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  TFhirOperationDefinitionParameter = fhir5_resources_canonical.TFhirOperationDefinitionParameter;
  TFhirOperationDefinitionParameterList = fhir5_resources_canonical.TFhirOperationDefinitionParameterList;
  TFhirOperationDefinitionParameterBinding = fhir5_resources_canonical.TFhirOperationDefinitionParameterBinding;
  TFhirOperationDefinitionParameterBindingList = fhir5_resources_canonical.TFhirOperationDefinitionParameterBindingList;
  TFhirOperationDefinitionParameterReferencedFrom = fhir5_resources_canonical.TFhirOperationDefinitionParameterReferencedFrom;
  TFhirOperationDefinitionParameterReferencedFromList = fhir5_resources_canonical.TFhirOperationDefinitionParameterReferencedFromList;
  TFhirOperationDefinitionOverload = fhir5_resources_canonical.TFhirOperationDefinitionOverload;
  TFhirOperationDefinitionOverloadList = fhir5_resources_canonical.TFhirOperationDefinitionOverloadList;
  TFhirOperationDefinition = fhir5_resources_canonical.TFhirOperationDefinition;
  TFhirOperationDefinitionList = fhir5_resources_canonical.TFhirOperationDefinitionList;
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  TFhirOperationOutcomeIssue = fhir5_resources_other.TFhirOperationOutcomeIssue;
  TFhirOperationOutcomeIssueList = fhir5_resources_other.TFhirOperationOutcomeIssueList;
  TFhirOperationOutcome = fhir5_resources_other.TFhirOperationOutcome;
  TFhirOperationOutcomeList = fhir5_resources_other.TFhirOperationOutcomeList;
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_ORGANIZATION}
  TFhirOrganizationQualification = fhir5_resources_admin.TFhirOrganizationQualification;
  TFhirOrganizationQualificationList = fhir5_resources_admin.TFhirOrganizationQualificationList;
  TFhirOrganization = fhir5_resources_admin.TFhirOrganization;
  TFhirOrganizationList = fhir5_resources_admin.TFhirOrganizationList;
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  TFhirOrganizationAffiliation = fhir5_resources_admin.TFhirOrganizationAffiliation;
  TFhirOrganizationAffiliationList = fhir5_resources_admin.TFhirOrganizationAffiliationList;
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}
{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
  TFhirPackagedProductDefinitionLegalStatusOfSupply = fhir5_resources_medications.TFhirPackagedProductDefinitionLegalStatusOfSupply;
  TFhirPackagedProductDefinitionLegalStatusOfSupplyList = fhir5_resources_medications.TFhirPackagedProductDefinitionLegalStatusOfSupplyList;
  TFhirPackagedProductDefinitionPackaging = fhir5_resources_medications.TFhirPackagedProductDefinitionPackaging;
  TFhirPackagedProductDefinitionPackagingList = fhir5_resources_medications.TFhirPackagedProductDefinitionPackagingList;
  TFhirPackagedProductDefinitionPackagingProperty = fhir5_resources_medications.TFhirPackagedProductDefinitionPackagingProperty;
  TFhirPackagedProductDefinitionPackagingPropertyList = fhir5_resources_medications.TFhirPackagedProductDefinitionPackagingPropertyList;
  TFhirPackagedProductDefinitionPackagingContainedItem = fhir5_resources_medications.TFhirPackagedProductDefinitionPackagingContainedItem;
  TFhirPackagedProductDefinitionPackagingContainedItemList = fhir5_resources_medications.TFhirPackagedProductDefinitionPackagingContainedItemList;
  TFhirPackagedProductDefinition = fhir5_resources_medications.TFhirPackagedProductDefinition;
  TFhirPackagedProductDefinitionList = fhir5_resources_medications.TFhirPackagedProductDefinitionList;
{$ENDIF FHIR_PACKAGEDPRODUCTDEFINITION}
{$IFDEF FHIR_PARAMETERS}
  TFhirParametersParameter = fhir5_resources_other.TFhirParametersParameter;
  TFhirParametersParameterList = fhir5_resources_other.TFhirParametersParameterList;
  TFhirParameters = fhir5_resources_other.TFhirParameters;
  TFhirParametersList = fhir5_resources_other.TFhirParametersList;
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_PATIENT}
  TFhirPatientContact = fhir5_resources_admin.TFhirPatientContact;
  TFhirPatientContactList = fhir5_resources_admin.TFhirPatientContactList;
  TFhirPatientCommunication = fhir5_resources_admin.TFhirPatientCommunication;
  TFhirPatientCommunicationList = fhir5_resources_admin.TFhirPatientCommunicationList;
  TFhirPatientLink = fhir5_resources_admin.TFhirPatientLink;
  TFhirPatientLinkList = fhir5_resources_admin.TFhirPatientLinkList;
  TFhirPatient = fhir5_resources_admin.TFhirPatient;
  TFhirPatientList = fhir5_resources_admin.TFhirPatientList;
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PAYMENTNOTICE}
  TFhirPaymentNotice = fhir5_resources_financial.TFhirPaymentNotice;
  TFhirPaymentNoticeList = fhir5_resources_financial.TFhirPaymentNoticeList;
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  TFhirPaymentReconciliationAllocation = fhir5_resources_financial.TFhirPaymentReconciliationAllocation;
  TFhirPaymentReconciliationAllocationList = fhir5_resources_financial.TFhirPaymentReconciliationAllocationList;
  TFhirPaymentReconciliationProcessNote = fhir5_resources_financial.TFhirPaymentReconciliationProcessNote;
  TFhirPaymentReconciliationProcessNoteList = fhir5_resources_financial.TFhirPaymentReconciliationProcessNoteList;
  TFhirPaymentReconciliation = fhir5_resources_financial.TFhirPaymentReconciliation;
  TFhirPaymentReconciliationList = fhir5_resources_financial.TFhirPaymentReconciliationList;
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERMISSION}
  TFhirPermissionJustification = fhir5_resources_other.TFhirPermissionJustification;
  TFhirPermissionJustificationList = fhir5_resources_other.TFhirPermissionJustificationList;
  TFhirPermissionRule = fhir5_resources_other.TFhirPermissionRule;
  TFhirPermissionRuleList = fhir5_resources_other.TFhirPermissionRuleList;
  TFhirPermissionRuleData = fhir5_resources_other.TFhirPermissionRuleData;
  TFhirPermissionRuleDataList = fhir5_resources_other.TFhirPermissionRuleDataList;
  TFhirPermissionRuleDataResource = fhir5_resources_other.TFhirPermissionRuleDataResource;
  TFhirPermissionRuleDataResourceList = fhir5_resources_other.TFhirPermissionRuleDataResourceList;
  TFhirPermissionRuleActivity = fhir5_resources_other.TFhirPermissionRuleActivity;
  TFhirPermissionRuleActivityList = fhir5_resources_other.TFhirPermissionRuleActivityList;
  TFhirPermission = fhir5_resources_other.TFhirPermission;
  TFhirPermissionList = fhir5_resources_other.TFhirPermissionList;
{$ENDIF FHIR_PERMISSION}
{$IFDEF FHIR_PERSON}
  TFhirPersonCommunication = fhir5_resources_admin.TFhirPersonCommunication;
  TFhirPersonCommunicationList = fhir5_resources_admin.TFhirPersonCommunicationList;
  TFhirPersonLink = fhir5_resources_admin.TFhirPersonLink;
  TFhirPersonLinkList = fhir5_resources_admin.TFhirPersonLinkList;
  TFhirPerson = fhir5_resources_admin.TFhirPerson;
  TFhirPersonList = fhir5_resources_admin.TFhirPersonList;
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PLANDEFINITION}
  TFhirPlanDefinitionGoal = fhir5_resources_other.TFhirPlanDefinitionGoal;
  TFhirPlanDefinitionGoalList = fhir5_resources_other.TFhirPlanDefinitionGoalList;
  TFhirPlanDefinitionGoalTarget = fhir5_resources_other.TFhirPlanDefinitionGoalTarget;
  TFhirPlanDefinitionGoalTargetList = fhir5_resources_other.TFhirPlanDefinitionGoalTargetList;
  TFhirPlanDefinitionActor = fhir5_resources_other.TFhirPlanDefinitionActor;
  TFhirPlanDefinitionActorList = fhir5_resources_other.TFhirPlanDefinitionActorList;
  TFhirPlanDefinitionActorOption = fhir5_resources_other.TFhirPlanDefinitionActorOption;
  TFhirPlanDefinitionActorOptionList = fhir5_resources_other.TFhirPlanDefinitionActorOptionList;
  TFhirPlanDefinitionAction = fhir5_resources_other.TFhirPlanDefinitionAction;
  TFhirPlanDefinitionActionList = fhir5_resources_other.TFhirPlanDefinitionActionList;
  TFhirPlanDefinitionActionCondition = fhir5_resources_other.TFhirPlanDefinitionActionCondition;
  TFhirPlanDefinitionActionConditionList = fhir5_resources_other.TFhirPlanDefinitionActionConditionList;
  TFhirPlanDefinitionActionInput = fhir5_resources_other.TFhirPlanDefinitionActionInput;
  TFhirPlanDefinitionActionInputList = fhir5_resources_other.TFhirPlanDefinitionActionInputList;
  TFhirPlanDefinitionActionOutput = fhir5_resources_other.TFhirPlanDefinitionActionOutput;
  TFhirPlanDefinitionActionOutputList = fhir5_resources_other.TFhirPlanDefinitionActionOutputList;
  TFhirPlanDefinitionActionRelatedAction = fhir5_resources_other.TFhirPlanDefinitionActionRelatedAction;
  TFhirPlanDefinitionActionRelatedActionList = fhir5_resources_other.TFhirPlanDefinitionActionRelatedActionList;
  TFhirPlanDefinitionActionParticipant = fhir5_resources_other.TFhirPlanDefinitionActionParticipant;
  TFhirPlanDefinitionActionParticipantList = fhir5_resources_other.TFhirPlanDefinitionActionParticipantList;
  TFhirPlanDefinitionActionDynamicValue = fhir5_resources_other.TFhirPlanDefinitionActionDynamicValue;
  TFhirPlanDefinitionActionDynamicValueList = fhir5_resources_other.TFhirPlanDefinitionActionDynamicValueList;
  TFhirPlanDefinition = fhir5_resources_other.TFhirPlanDefinition;
  TFhirPlanDefinitionList = fhir5_resources_other.TFhirPlanDefinitionList;
{$ENDIF FHIR_PLANDEFINITION}
{$IFDEF FHIR_PRACTITIONER}
  TFhirPractitionerQualification = fhir5_resources_admin.TFhirPractitionerQualification;
  TFhirPractitionerQualificationList = fhir5_resources_admin.TFhirPractitionerQualificationList;
  TFhirPractitioner = fhir5_resources_admin.TFhirPractitioner;
  TFhirPractitionerList = fhir5_resources_admin.TFhirPractitionerList;
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PRACTITIONERROLE}
  TFhirPractitionerRole = fhir5_resources_admin.TFhirPractitionerRole;
  TFhirPractitionerRoleList = fhir5_resources_admin.TFhirPractitionerRoleList;
{$ENDIF FHIR_PRACTITIONERROLE}
{$IFDEF FHIR_PROCEDURE}
  TFhirProcedurePerformer = fhir5_resources_clinical.TFhirProcedurePerformer;
  TFhirProcedurePerformerList = fhir5_resources_clinical.TFhirProcedurePerformerList;
  TFhirProcedureFocalDevice = fhir5_resources_clinical.TFhirProcedureFocalDevice;
  TFhirProcedureFocalDeviceList = fhir5_resources_clinical.TFhirProcedureFocalDeviceList;
  TFhirProcedure = fhir5_resources_clinical.TFhirProcedure;
  TFhirProcedureList = fhir5_resources_clinical.TFhirProcedureList;
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROVENANCE}
  TFhirProvenanceAgent = fhir5_resources_other.TFhirProvenanceAgent;
  TFhirProvenanceAgentList = fhir5_resources_other.TFhirProvenanceAgentList;
  TFhirProvenanceEntity = fhir5_resources_other.TFhirProvenanceEntity;
  TFhirProvenanceEntityList = fhir5_resources_other.TFhirProvenanceEntityList;
  TFhirProvenance = fhir5_resources_other.TFhirProvenance;
  TFhirProvenanceList = fhir5_resources_other.TFhirProvenanceList;
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  TFhirQuestionnaireItem = fhir5_resources_canonical.TFhirQuestionnaireItem;
  TFhirQuestionnaireItemList = fhir5_resources_canonical.TFhirQuestionnaireItemList;
  TFhirQuestionnaireItemEnableWhen = fhir5_resources_canonical.TFhirQuestionnaireItemEnableWhen;
  TFhirQuestionnaireItemEnableWhenList = fhir5_resources_canonical.TFhirQuestionnaireItemEnableWhenList;
  TFhirQuestionnaireItemAnswerOption = fhir5_resources_canonical.TFhirQuestionnaireItemAnswerOption;
  TFhirQuestionnaireItemAnswerOptionList = fhir5_resources_canonical.TFhirQuestionnaireItemAnswerOptionList;
  TFhirQuestionnaireItemInitial = fhir5_resources_canonical.TFhirQuestionnaireItemInitial;
  TFhirQuestionnaireItemInitialList = fhir5_resources_canonical.TFhirQuestionnaireItemInitialList;
  TFhirQuestionnaire = fhir5_resources_canonical.TFhirQuestionnaire;
  TFhirQuestionnaireList = fhir5_resources_canonical.TFhirQuestionnaireList;
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  TFhirQuestionnaireResponseItem = fhir5_resources_other.TFhirQuestionnaireResponseItem;
  TFhirQuestionnaireResponseItemList = fhir5_resources_other.TFhirQuestionnaireResponseItemList;
  TFhirQuestionnaireResponseItemAnswer = fhir5_resources_other.TFhirQuestionnaireResponseItemAnswer;
  TFhirQuestionnaireResponseItemAnswerList = fhir5_resources_other.TFhirQuestionnaireResponseItemAnswerList;
  TFhirQuestionnaireResponse = fhir5_resources_other.TFhirQuestionnaireResponse;
  TFhirQuestionnaireResponseList = fhir5_resources_other.TFhirQuestionnaireResponseList;
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REGULATEDAUTHORIZATION}
  TFhirRegulatedAuthorizationCase = fhir5_resources_medications.TFhirRegulatedAuthorizationCase;
  TFhirRegulatedAuthorizationCaseList = fhir5_resources_medications.TFhirRegulatedAuthorizationCaseList;
  TFhirRegulatedAuthorization = fhir5_resources_medications.TFhirRegulatedAuthorization;
  TFhirRegulatedAuthorizationList = fhir5_resources_medications.TFhirRegulatedAuthorizationList;
{$ENDIF FHIR_REGULATEDAUTHORIZATION}
{$IFDEF FHIR_RELATEDPERSON}
  TFhirRelatedPersonCommunication = fhir5_resources_admin.TFhirRelatedPersonCommunication;
  TFhirRelatedPersonCommunicationList = fhir5_resources_admin.TFhirRelatedPersonCommunicationList;
  TFhirRelatedPerson = fhir5_resources_admin.TFhirRelatedPerson;
  TFhirRelatedPersonList = fhir5_resources_admin.TFhirRelatedPersonList;
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_REQUESTGROUP}
  TFhirRequestGroupAction = fhir5_resources_other.TFhirRequestGroupAction;
  TFhirRequestGroupActionList = fhir5_resources_other.TFhirRequestGroupActionList;
  TFhirRequestGroupActionCondition = fhir5_resources_other.TFhirRequestGroupActionCondition;
  TFhirRequestGroupActionConditionList = fhir5_resources_other.TFhirRequestGroupActionConditionList;
  TFhirRequestGroupActionRelatedAction = fhir5_resources_other.TFhirRequestGroupActionRelatedAction;
  TFhirRequestGroupActionRelatedActionList = fhir5_resources_other.TFhirRequestGroupActionRelatedActionList;
  TFhirRequestGroupActionParticipant = fhir5_resources_other.TFhirRequestGroupActionParticipant;
  TFhirRequestGroupActionParticipantList = fhir5_resources_other.TFhirRequestGroupActionParticipantList;
  TFhirRequestGroup = fhir5_resources_other.TFhirRequestGroup;
  TFhirRequestGroupList = fhir5_resources_other.TFhirRequestGroupList;
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_REQUESTORCHESTRATION}
  TFhirRequestOrchestrationAction = fhir5_resources_other.TFhirRequestOrchestrationAction;
  TFhirRequestOrchestrationActionList = fhir5_resources_other.TFhirRequestOrchestrationActionList;
  TFhirRequestOrchestrationActionCondition = fhir5_resources_other.TFhirRequestOrchestrationActionCondition;
  TFhirRequestOrchestrationActionConditionList = fhir5_resources_other.TFhirRequestOrchestrationActionConditionList;
  TFhirRequestOrchestrationActionInput = fhir5_resources_other.TFhirRequestOrchestrationActionInput;
  TFhirRequestOrchestrationActionInputList = fhir5_resources_other.TFhirRequestOrchestrationActionInputList;
  TFhirRequestOrchestrationActionOutput = fhir5_resources_other.TFhirRequestOrchestrationActionOutput;
  TFhirRequestOrchestrationActionOutputList = fhir5_resources_other.TFhirRequestOrchestrationActionOutputList;
  TFhirRequestOrchestrationActionRelatedAction = fhir5_resources_other.TFhirRequestOrchestrationActionRelatedAction;
  TFhirRequestOrchestrationActionRelatedActionList = fhir5_resources_other.TFhirRequestOrchestrationActionRelatedActionList;
  TFhirRequestOrchestrationActionParticipant = fhir5_resources_other.TFhirRequestOrchestrationActionParticipant;
  TFhirRequestOrchestrationActionParticipantList = fhir5_resources_other.TFhirRequestOrchestrationActionParticipantList;
  TFhirRequestOrchestrationActionDynamicValue = fhir5_resources_other.TFhirRequestOrchestrationActionDynamicValue;
  TFhirRequestOrchestrationActionDynamicValueList = fhir5_resources_other.TFhirRequestOrchestrationActionDynamicValueList;
  TFhirRequestOrchestration = fhir5_resources_other.TFhirRequestOrchestration;
  TFhirRequestOrchestrationList = fhir5_resources_other.TFhirRequestOrchestrationList;
{$ENDIF FHIR_REQUESTORCHESTRATION}
{$IFDEF FHIR_REQUIREMENTS}
  TFhirRequirementsStatement = fhir5_resources_canonical.TFhirRequirementsStatement;
  TFhirRequirementsStatementList = fhir5_resources_canonical.TFhirRequirementsStatementList;
  TFhirRequirements = fhir5_resources_canonical.TFhirRequirements;
  TFhirRequirementsList = fhir5_resources_canonical.TFhirRequirementsList;
{$ENDIF FHIR_REQUIREMENTS}
{$IFDEF FHIR_RESEARCHSTUDY}
  TFhirResearchStudyLabel = fhir5_resources_other.TFhirResearchStudyLabel;
  TFhirResearchStudyLabelList = fhir5_resources_other.TFhirResearchStudyLabelList;
  TFhirResearchStudyFocus = fhir5_resources_other.TFhirResearchStudyFocus;
  TFhirResearchStudyFocusList = fhir5_resources_other.TFhirResearchStudyFocusList;
  TFhirResearchStudyAssociatedParty = fhir5_resources_other.TFhirResearchStudyAssociatedParty;
  TFhirResearchStudyAssociatedPartyList = fhir5_resources_other.TFhirResearchStudyAssociatedPartyList;
  TFhirResearchStudyProgressStatus = fhir5_resources_other.TFhirResearchStudyProgressStatus;
  TFhirResearchStudyProgressStatusList = fhir5_resources_other.TFhirResearchStudyProgressStatusList;
  TFhirResearchStudyRecruitment = fhir5_resources_other.TFhirResearchStudyRecruitment;
  TFhirResearchStudyRecruitmentList = fhir5_resources_other.TFhirResearchStudyRecruitmentList;
  TFhirResearchStudyComparisonGroup = fhir5_resources_other.TFhirResearchStudyComparisonGroup;
  TFhirResearchStudyComparisonGroupList = fhir5_resources_other.TFhirResearchStudyComparisonGroupList;
  TFhirResearchStudyObjective = fhir5_resources_other.TFhirResearchStudyObjective;
  TFhirResearchStudyObjectiveList = fhir5_resources_other.TFhirResearchStudyObjectiveList;
  TFhirResearchStudyOutcomeMeasure = fhir5_resources_other.TFhirResearchStudyOutcomeMeasure;
  TFhirResearchStudyOutcomeMeasureList = fhir5_resources_other.TFhirResearchStudyOutcomeMeasureList;
  TFhirResearchStudyWebLocation = fhir5_resources_other.TFhirResearchStudyWebLocation;
  TFhirResearchStudyWebLocationList = fhir5_resources_other.TFhirResearchStudyWebLocationList;
  TFhirResearchStudy = fhir5_resources_other.TFhirResearchStudy;
  TFhirResearchStudyList = fhir5_resources_other.TFhirResearchStudyList;
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  TFhirResearchSubjectProgress = fhir5_resources_other.TFhirResearchSubjectProgress;
  TFhirResearchSubjectProgressList = fhir5_resources_other.TFhirResearchSubjectProgressList;
  TFhirResearchSubject = fhir5_resources_other.TFhirResearchSubject;
  TFhirResearchSubjectList = fhir5_resources_other.TFhirResearchSubjectList;
{$ENDIF FHIR_RESEARCHSUBJECT}
{$IFDEF FHIR_RISKASSESSMENT}
  TFhirRiskAssessmentPrediction = fhir5_resources_clinical.TFhirRiskAssessmentPrediction;
  TFhirRiskAssessmentPredictionList = fhir5_resources_clinical.TFhirRiskAssessmentPredictionList;
  TFhirRiskAssessment = fhir5_resources_clinical.TFhirRiskAssessment;
  TFhirRiskAssessmentList = fhir5_resources_clinical.TFhirRiskAssessmentList;
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_SCHEDULE}
  TFhirSchedule = fhir5_resources_admin.TFhirSchedule;
  TFhirScheduleList = fhir5_resources_admin.TFhirScheduleList;
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SEARCHPARAMETER}
  TFhirSearchParameterComponent = fhir5_resources_canonical.TFhirSearchParameterComponent;
  TFhirSearchParameterComponentList = fhir5_resources_canonical.TFhirSearchParameterComponentList;
  TFhirSearchParameter = fhir5_resources_canonical.TFhirSearchParameter;
  TFhirSearchParameterList = fhir5_resources_canonical.TFhirSearchParameterList;
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SERVICEREQUEST}
  TFhirServiceRequest = fhir5_resources_clinical.TFhirServiceRequest;
  TFhirServiceRequestList = fhir5_resources_clinical.TFhirServiceRequestList;
{$ENDIF FHIR_SERVICEREQUEST}
{$IFDEF FHIR_SLOT}
  TFhirSlot = fhir5_resources_admin.TFhirSlot;
  TFhirSlotList = fhir5_resources_admin.TFhirSlotList;
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  TFhirSpecimenFeature = fhir5_resources_clinical.TFhirSpecimenFeature;
  TFhirSpecimenFeatureList = fhir5_resources_clinical.TFhirSpecimenFeatureList;
  TFhirSpecimenCollection = fhir5_resources_clinical.TFhirSpecimenCollection;
  TFhirSpecimenCollectionList = fhir5_resources_clinical.TFhirSpecimenCollectionList;
  TFhirSpecimenProcessing = fhir5_resources_clinical.TFhirSpecimenProcessing;
  TFhirSpecimenProcessingList = fhir5_resources_clinical.TFhirSpecimenProcessingList;
  TFhirSpecimenContainer = fhir5_resources_clinical.TFhirSpecimenContainer;
  TFhirSpecimenContainerList = fhir5_resources_clinical.TFhirSpecimenContainerList;
  TFhirSpecimen = fhir5_resources_clinical.TFhirSpecimen;
  TFhirSpecimenList = fhir5_resources_clinical.TFhirSpecimenList;
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_SPECIMENDEFINITION}
  TFhirSpecimenDefinitionTypeTested = fhir5_resources_canonical.TFhirSpecimenDefinitionTypeTested;
  TFhirSpecimenDefinitionTypeTestedList = fhir5_resources_canonical.TFhirSpecimenDefinitionTypeTestedList;
  TFhirSpecimenDefinitionTypeTestedContainer = fhir5_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainer;
  TFhirSpecimenDefinitionTypeTestedContainerList = fhir5_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainerList;
  TFhirSpecimenDefinitionTypeTestedContainerAdditive = fhir5_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainerAdditive;
  TFhirSpecimenDefinitionTypeTestedContainerAdditiveList = fhir5_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainerAdditiveList;
  TFhirSpecimenDefinitionTypeTestedHandling = fhir5_resources_canonical.TFhirSpecimenDefinitionTypeTestedHandling;
  TFhirSpecimenDefinitionTypeTestedHandlingList = fhir5_resources_canonical.TFhirSpecimenDefinitionTypeTestedHandlingList;
  TFhirSpecimenDefinition = fhir5_resources_canonical.TFhirSpecimenDefinition;
  TFhirSpecimenDefinitionList = fhir5_resources_canonical.TFhirSpecimenDefinitionList;
{$ENDIF FHIR_SPECIMENDEFINITION}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  TFhirStructureDefinitionMapping = fhir5_resources_canonical.TFhirStructureDefinitionMapping;
  TFhirStructureDefinitionMappingList = fhir5_resources_canonical.TFhirStructureDefinitionMappingList;
  TFhirStructureDefinitionContext = fhir5_resources_canonical.TFhirStructureDefinitionContext;
  TFhirStructureDefinitionContextList = fhir5_resources_canonical.TFhirStructureDefinitionContextList;
  TFhirStructureDefinitionSnapshot = fhir5_resources_canonical.TFhirStructureDefinitionSnapshot;
  TFhirStructureDefinitionSnapshotList = fhir5_resources_canonical.TFhirStructureDefinitionSnapshotList;
  TFhirStructureDefinitionDifferential = fhir5_resources_canonical.TFhirStructureDefinitionDifferential;
  TFhirStructureDefinitionDifferentialList = fhir5_resources_canonical.TFhirStructureDefinitionDifferentialList;
  TFhirStructureDefinition = fhir5_resources_canonical.TFhirStructureDefinition;
  TFhirStructureDefinitionList = fhir5_resources_canonical.TFhirStructureDefinitionList;
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_STRUCTUREMAP}
  TFhirStructureMapStructure = fhir5_resources_canonical.TFhirStructureMapStructure;
  TFhirStructureMapStructureList = fhir5_resources_canonical.TFhirStructureMapStructureList;
  TFhirStructureMapGroup = fhir5_resources_canonical.TFhirStructureMapGroup;
  TFhirStructureMapGroupList = fhir5_resources_canonical.TFhirStructureMapGroupList;
  TFhirStructureMapGroupInput = fhir5_resources_canonical.TFhirStructureMapGroupInput;
  TFhirStructureMapGroupInputList = fhir5_resources_canonical.TFhirStructureMapGroupInputList;
  TFhirStructureMapGroupRule = fhir5_resources_canonical.TFhirStructureMapGroupRule;
  TFhirStructureMapGroupRuleList = fhir5_resources_canonical.TFhirStructureMapGroupRuleList;
  TFhirStructureMapGroupRuleSource = fhir5_resources_canonical.TFhirStructureMapGroupRuleSource;
  TFhirStructureMapGroupRuleSourceList = fhir5_resources_canonical.TFhirStructureMapGroupRuleSourceList;
  TFhirStructureMapGroupRuleTarget = fhir5_resources_canonical.TFhirStructureMapGroupRuleTarget;
  TFhirStructureMapGroupRuleTargetList = fhir5_resources_canonical.TFhirStructureMapGroupRuleTargetList;
  TFhirStructureMapGroupRuleTargetParameter = fhir5_resources_canonical.TFhirStructureMapGroupRuleTargetParameter;
  TFhirStructureMapGroupRuleTargetParameterList = fhir5_resources_canonical.TFhirStructureMapGroupRuleTargetParameterList;
  TFhirStructureMapGroupRuleDependent = fhir5_resources_canonical.TFhirStructureMapGroupRuleDependent;
  TFhirStructureMapGroupRuleDependentList = fhir5_resources_canonical.TFhirStructureMapGroupRuleDependentList;
  TFhirStructureMap = fhir5_resources_canonical.TFhirStructureMap;
  TFhirStructureMapList = fhir5_resources_canonical.TFhirStructureMapList;
{$ENDIF FHIR_STRUCTUREMAP}
{$IFDEF FHIR_SUBSCRIPTION}
  TFhirSubscriptionFilterBy = fhir5_resources_other.TFhirSubscriptionFilterBy;
  TFhirSubscriptionFilterByList = fhir5_resources_other.TFhirSubscriptionFilterByList;
  TFhirSubscription = fhir5_resources_other.TFhirSubscription;
  TFhirSubscriptionList = fhir5_resources_other.TFhirSubscriptionList;
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
  TFhirSubscriptionStatusNotificationEvent = fhir5_resources_other.TFhirSubscriptionStatusNotificationEvent;
  TFhirSubscriptionStatusNotificationEventList = fhir5_resources_other.TFhirSubscriptionStatusNotificationEventList;
  TFhirSubscriptionStatus = fhir5_resources_other.TFhirSubscriptionStatus;
  TFhirSubscriptionStatusList = fhir5_resources_other.TFhirSubscriptionStatusList;
{$ENDIF FHIR_SUBSCRIPTIONSTATUS}
{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
  TFhirSubscriptionTopicResourceTrigger = fhir5_resources_other.TFhirSubscriptionTopicResourceTrigger;
  TFhirSubscriptionTopicResourceTriggerList = fhir5_resources_other.TFhirSubscriptionTopicResourceTriggerList;
  TFhirSubscriptionTopicResourceTriggerQueryCriteria = fhir5_resources_other.TFhirSubscriptionTopicResourceTriggerQueryCriteria;
  TFhirSubscriptionTopicResourceTriggerQueryCriteriaList = fhir5_resources_other.TFhirSubscriptionTopicResourceTriggerQueryCriteriaList;
  TFhirSubscriptionTopicEventTrigger = fhir5_resources_other.TFhirSubscriptionTopicEventTrigger;
  TFhirSubscriptionTopicEventTriggerList = fhir5_resources_other.TFhirSubscriptionTopicEventTriggerList;
  TFhirSubscriptionTopicCanFilterBy = fhir5_resources_other.TFhirSubscriptionTopicCanFilterBy;
  TFhirSubscriptionTopicCanFilterByList = fhir5_resources_other.TFhirSubscriptionTopicCanFilterByList;
  TFhirSubscriptionTopicNotificationShape = fhir5_resources_other.TFhirSubscriptionTopicNotificationShape;
  TFhirSubscriptionTopicNotificationShapeList = fhir5_resources_other.TFhirSubscriptionTopicNotificationShapeList;
  TFhirSubscriptionTopic = fhir5_resources_other.TFhirSubscriptionTopic;
  TFhirSubscriptionTopicList = fhir5_resources_other.TFhirSubscriptionTopicList;
{$ENDIF FHIR_SUBSCRIPTIONTOPIC}
{$IFDEF FHIR_SUBSTANCE}
  TFhirSubstanceIngredient = fhir5_resources_medications.TFhirSubstanceIngredient;
  TFhirSubstanceIngredientList = fhir5_resources_medications.TFhirSubstanceIngredientList;
  TFhirSubstance = fhir5_resources_medications.TFhirSubstance;
  TFhirSubstanceList = fhir5_resources_medications.TFhirSubstanceList;
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUBSTANCEDEFINITION}
  TFhirSubstanceDefinitionMoiety = fhir5_resources_medications.TFhirSubstanceDefinitionMoiety;
  TFhirSubstanceDefinitionMoietyList = fhir5_resources_medications.TFhirSubstanceDefinitionMoietyList;
  TFhirSubstanceDefinitionProperty = fhir5_resources_medications.TFhirSubstanceDefinitionProperty;
  TFhirSubstanceDefinitionPropertyList = fhir5_resources_medications.TFhirSubstanceDefinitionPropertyList;
  TFhirSubstanceDefinitionMolecularWeight = fhir5_resources_medications.TFhirSubstanceDefinitionMolecularWeight;
  TFhirSubstanceDefinitionMolecularWeightList = fhir5_resources_medications.TFhirSubstanceDefinitionMolecularWeightList;
  TFhirSubstanceDefinitionStructure = fhir5_resources_medications.TFhirSubstanceDefinitionStructure;
  TFhirSubstanceDefinitionStructureList = fhir5_resources_medications.TFhirSubstanceDefinitionStructureList;
  TFhirSubstanceDefinitionStructureRepresentation = fhir5_resources_medications.TFhirSubstanceDefinitionStructureRepresentation;
  TFhirSubstanceDefinitionStructureRepresentationList = fhir5_resources_medications.TFhirSubstanceDefinitionStructureRepresentationList;
  TFhirSubstanceDefinitionCode = fhir5_resources_medications.TFhirSubstanceDefinitionCode;
  TFhirSubstanceDefinitionCodeList = fhir5_resources_medications.TFhirSubstanceDefinitionCodeList;
  TFhirSubstanceDefinitionName = fhir5_resources_medications.TFhirSubstanceDefinitionName;
  TFhirSubstanceDefinitionNameList = fhir5_resources_medications.TFhirSubstanceDefinitionNameList;
  TFhirSubstanceDefinitionNameOfficial = fhir5_resources_medications.TFhirSubstanceDefinitionNameOfficial;
  TFhirSubstanceDefinitionNameOfficialList = fhir5_resources_medications.TFhirSubstanceDefinitionNameOfficialList;
  TFhirSubstanceDefinitionRelationship = fhir5_resources_medications.TFhirSubstanceDefinitionRelationship;
  TFhirSubstanceDefinitionRelationshipList = fhir5_resources_medications.TFhirSubstanceDefinitionRelationshipList;
  TFhirSubstanceDefinitionSourceMaterial = fhir5_resources_medications.TFhirSubstanceDefinitionSourceMaterial;
  TFhirSubstanceDefinitionSourceMaterialList = fhir5_resources_medications.TFhirSubstanceDefinitionSourceMaterialList;
  TFhirSubstanceDefinition = fhir5_resources_medications.TFhirSubstanceDefinition;
  TFhirSubstanceDefinitionList = fhir5_resources_medications.TFhirSubstanceDefinitionList;
{$ENDIF FHIR_SUBSTANCEDEFINITION}
{$IFDEF FHIR_SUBSTANCENUCLEICACID}
  TFhirSubstanceNucleicAcidSubunit = fhir5_resources_medications.TFhirSubstanceNucleicAcidSubunit;
  TFhirSubstanceNucleicAcidSubunitList = fhir5_resources_medications.TFhirSubstanceNucleicAcidSubunitList;
  TFhirSubstanceNucleicAcidSubunitLinkage = fhir5_resources_medications.TFhirSubstanceNucleicAcidSubunitLinkage;
  TFhirSubstanceNucleicAcidSubunitLinkageList = fhir5_resources_medications.TFhirSubstanceNucleicAcidSubunitLinkageList;
  TFhirSubstanceNucleicAcidSubunitSugar = fhir5_resources_medications.TFhirSubstanceNucleicAcidSubunitSugar;
  TFhirSubstanceNucleicAcidSubunitSugarList = fhir5_resources_medications.TFhirSubstanceNucleicAcidSubunitSugarList;
  TFhirSubstanceNucleicAcid = fhir5_resources_medications.TFhirSubstanceNucleicAcid;
  TFhirSubstanceNucleicAcidList = fhir5_resources_medications.TFhirSubstanceNucleicAcidList;
{$ENDIF FHIR_SUBSTANCENUCLEICACID}
{$IFDEF FHIR_SUBSTANCEPOLYMER}
  TFhirSubstancePolymerMonomerSet = fhir5_resources_medications.TFhirSubstancePolymerMonomerSet;
  TFhirSubstancePolymerMonomerSetList = fhir5_resources_medications.TFhirSubstancePolymerMonomerSetList;
  TFhirSubstancePolymerMonomerSetStartingMaterial = fhir5_resources_medications.TFhirSubstancePolymerMonomerSetStartingMaterial;
  TFhirSubstancePolymerMonomerSetStartingMaterialList = fhir5_resources_medications.TFhirSubstancePolymerMonomerSetStartingMaterialList;
  TFhirSubstancePolymerRepeat = fhir5_resources_medications.TFhirSubstancePolymerRepeat;
  TFhirSubstancePolymerRepeatList = fhir5_resources_medications.TFhirSubstancePolymerRepeatList;
  TFhirSubstancePolymerRepeatRepeatUnit = fhir5_resources_medications.TFhirSubstancePolymerRepeatRepeatUnit;
  TFhirSubstancePolymerRepeatRepeatUnitList = fhir5_resources_medications.TFhirSubstancePolymerRepeatRepeatUnitList;
  TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation = fhir5_resources_medications.TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation;
  TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationList = fhir5_resources_medications.TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationList;
  TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentation = fhir5_resources_medications.TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentation;
  TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentationList = fhir5_resources_medications.TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentationList;
  TFhirSubstancePolymer = fhir5_resources_medications.TFhirSubstancePolymer;
  TFhirSubstancePolymerList = fhir5_resources_medications.TFhirSubstancePolymerList;
{$ENDIF FHIR_SUBSTANCEPOLYMER}
{$IFDEF FHIR_SUBSTANCEPROTEIN}
  TFhirSubstanceProteinSubunit = fhir5_resources_medications.TFhirSubstanceProteinSubunit;
  TFhirSubstanceProteinSubunitList = fhir5_resources_medications.TFhirSubstanceProteinSubunitList;
  TFhirSubstanceProtein = fhir5_resources_medications.TFhirSubstanceProtein;
  TFhirSubstanceProteinList = fhir5_resources_medications.TFhirSubstanceProteinList;
{$ENDIF FHIR_SUBSTANCEPROTEIN}
{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  TFhirSubstanceReferenceInformationGene = fhir5_resources_medications.TFhirSubstanceReferenceInformationGene;
  TFhirSubstanceReferenceInformationGeneList = fhir5_resources_medications.TFhirSubstanceReferenceInformationGeneList;
  TFhirSubstanceReferenceInformationGeneElement = fhir5_resources_medications.TFhirSubstanceReferenceInformationGeneElement;
  TFhirSubstanceReferenceInformationGeneElementList = fhir5_resources_medications.TFhirSubstanceReferenceInformationGeneElementList;
  TFhirSubstanceReferenceInformationTarget = fhir5_resources_medications.TFhirSubstanceReferenceInformationTarget;
  TFhirSubstanceReferenceInformationTargetList = fhir5_resources_medications.TFhirSubstanceReferenceInformationTargetList;
  TFhirSubstanceReferenceInformation = fhir5_resources_medications.TFhirSubstanceReferenceInformation;
  TFhirSubstanceReferenceInformationList = fhir5_resources_medications.TFhirSubstanceReferenceInformationList;
{$ENDIF FHIR_SUBSTANCEREFERENCEINFORMATION}
{$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
  TFhirSubstanceSourceMaterialFractionDescription = fhir5_resources_medications.TFhirSubstanceSourceMaterialFractionDescription;
  TFhirSubstanceSourceMaterialFractionDescriptionList = fhir5_resources_medications.TFhirSubstanceSourceMaterialFractionDescriptionList;
  TFhirSubstanceSourceMaterialOrganism = fhir5_resources_medications.TFhirSubstanceSourceMaterialOrganism;
  TFhirSubstanceSourceMaterialOrganismList = fhir5_resources_medications.TFhirSubstanceSourceMaterialOrganismList;
  TFhirSubstanceSourceMaterialOrganismAuthor = fhir5_resources_medications.TFhirSubstanceSourceMaterialOrganismAuthor;
  TFhirSubstanceSourceMaterialOrganismAuthorList = fhir5_resources_medications.TFhirSubstanceSourceMaterialOrganismAuthorList;
  TFhirSubstanceSourceMaterialOrganismHybrid = fhir5_resources_medications.TFhirSubstanceSourceMaterialOrganismHybrid;
  TFhirSubstanceSourceMaterialOrganismHybridList = fhir5_resources_medications.TFhirSubstanceSourceMaterialOrganismHybridList;
  TFhirSubstanceSourceMaterialOrganismOrganismGeneral = fhir5_resources_medications.TFhirSubstanceSourceMaterialOrganismOrganismGeneral;
  TFhirSubstanceSourceMaterialOrganismOrganismGeneralList = fhir5_resources_medications.TFhirSubstanceSourceMaterialOrganismOrganismGeneralList;
  TFhirSubstanceSourceMaterialPartDescription = fhir5_resources_medications.TFhirSubstanceSourceMaterialPartDescription;
  TFhirSubstanceSourceMaterialPartDescriptionList = fhir5_resources_medications.TFhirSubstanceSourceMaterialPartDescriptionList;
  TFhirSubstanceSourceMaterial = fhir5_resources_medications.TFhirSubstanceSourceMaterial;
  TFhirSubstanceSourceMaterialList = fhir5_resources_medications.TFhirSubstanceSourceMaterialList;
{$ENDIF FHIR_SUBSTANCESOURCEMATERIAL}
{$IFDEF FHIR_SUPPLYDELIVERY}
  TFhirSupplyDeliverySuppliedItem = fhir5_resources_clinical.TFhirSupplyDeliverySuppliedItem;
  TFhirSupplyDeliverySuppliedItemList = fhir5_resources_clinical.TFhirSupplyDeliverySuppliedItemList;
  TFhirSupplyDelivery = fhir5_resources_clinical.TFhirSupplyDelivery;
  TFhirSupplyDeliveryList = fhir5_resources_clinical.TFhirSupplyDeliveryList;
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  TFhirSupplyRequestParameter = fhir5_resources_clinical.TFhirSupplyRequestParameter;
  TFhirSupplyRequestParameterList = fhir5_resources_clinical.TFhirSupplyRequestParameterList;
  TFhirSupplyRequest = fhir5_resources_clinical.TFhirSupplyRequest;
  TFhirSupplyRequestList = fhir5_resources_clinical.TFhirSupplyRequestList;
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TASK}
  TFhirTaskRestriction = fhir5_resources_other.TFhirTaskRestriction;
  TFhirTaskRestrictionList = fhir5_resources_other.TFhirTaskRestrictionList;
  TFhirTaskInput = fhir5_resources_other.TFhirTaskInput;
  TFhirTaskInputList = fhir5_resources_other.TFhirTaskInputList;
  TFhirTaskOutput = fhir5_resources_other.TFhirTaskOutput;
  TFhirTaskOutputList = fhir5_resources_other.TFhirTaskOutputList;
  TFhirTask = fhir5_resources_other.TFhirTask;
  TFhirTaskList = fhir5_resources_other.TFhirTaskList;
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  TFhirTerminologyCapabilitiesSoftware = fhir5_resources_canonical.TFhirTerminologyCapabilitiesSoftware;
  TFhirTerminologyCapabilitiesSoftwareList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesSoftwareList;
  TFhirTerminologyCapabilitiesImplementation = fhir5_resources_canonical.TFhirTerminologyCapabilitiesImplementation;
  TFhirTerminologyCapabilitiesImplementationList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesImplementationList;
  TFhirTerminologyCapabilitiesCodeSystem = fhir5_resources_canonical.TFhirTerminologyCapabilitiesCodeSystem;
  TFhirTerminologyCapabilitiesCodeSystemList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemList;
  TFhirTerminologyCapabilitiesCodeSystemVersion = fhir5_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersion;
  TFhirTerminologyCapabilitiesCodeSystemVersionList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersionList;
  TFhirTerminologyCapabilitiesCodeSystemVersionFilter = fhir5_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersionFilter;
  TFhirTerminologyCapabilitiesCodeSystemVersionFilterList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersionFilterList;
  TFhirTerminologyCapabilitiesExpansion = fhir5_resources_canonical.TFhirTerminologyCapabilitiesExpansion;
  TFhirTerminologyCapabilitiesExpansionList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesExpansionList;
  TFhirTerminologyCapabilitiesExpansionParameter = fhir5_resources_canonical.TFhirTerminologyCapabilitiesExpansionParameter;
  TFhirTerminologyCapabilitiesExpansionParameterList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesExpansionParameterList;
  TFhirTerminologyCapabilitiesValidateCode = fhir5_resources_canonical.TFhirTerminologyCapabilitiesValidateCode;
  TFhirTerminologyCapabilitiesValidateCodeList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesValidateCodeList;
  TFhirTerminologyCapabilitiesTranslation = fhir5_resources_canonical.TFhirTerminologyCapabilitiesTranslation;
  TFhirTerminologyCapabilitiesTranslationList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesTranslationList;
  TFhirTerminologyCapabilitiesClosure = fhir5_resources_canonical.TFhirTerminologyCapabilitiesClosure;
  TFhirTerminologyCapabilitiesClosureList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesClosureList;
  TFhirTerminologyCapabilities = fhir5_resources_canonical.TFhirTerminologyCapabilities;
  TFhirTerminologyCapabilitiesList = fhir5_resources_canonical.TFhirTerminologyCapabilitiesList;
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}
{$IFDEF FHIR_TESTREPORT}
  TFhirTestReportParticipant = fhir5_resources_other.TFhirTestReportParticipant;
  TFhirTestReportParticipantList = fhir5_resources_other.TFhirTestReportParticipantList;
  TFhirTestReportSetup = fhir5_resources_other.TFhirTestReportSetup;
  TFhirTestReportSetupList = fhir5_resources_other.TFhirTestReportSetupList;
  TFhirTestReportSetupAction = fhir5_resources_other.TFhirTestReportSetupAction;
  TFhirTestReportSetupActionList = fhir5_resources_other.TFhirTestReportSetupActionList;
  TFhirTestReportSetupActionOperation = fhir5_resources_other.TFhirTestReportSetupActionOperation;
  TFhirTestReportSetupActionOperationList = fhir5_resources_other.TFhirTestReportSetupActionOperationList;
  TFhirTestReportSetupActionAssert = fhir5_resources_other.TFhirTestReportSetupActionAssert;
  TFhirTestReportSetupActionAssertList = fhir5_resources_other.TFhirTestReportSetupActionAssertList;
  TFhirTestReportTest = fhir5_resources_other.TFhirTestReportTest;
  TFhirTestReportTestList = fhir5_resources_other.TFhirTestReportTestList;
  TFhirTestReportTestAction = fhir5_resources_other.TFhirTestReportTestAction;
  TFhirTestReportTestActionList = fhir5_resources_other.TFhirTestReportTestActionList;
  TFhirTestReportTeardown = fhir5_resources_other.TFhirTestReportTeardown;
  TFhirTestReportTeardownList = fhir5_resources_other.TFhirTestReportTeardownList;
  TFhirTestReportTeardownAction = fhir5_resources_other.TFhirTestReportTeardownAction;
  TFhirTestReportTeardownActionList = fhir5_resources_other.TFhirTestReportTeardownActionList;
  TFhirTestReport = fhir5_resources_other.TFhirTestReport;
  TFhirTestReportList = fhir5_resources_other.TFhirTestReportList;
{$ENDIF FHIR_TESTREPORT}
{$IFDEF FHIR_TESTSCRIPT}
  TFhirTestScriptOrigin = fhir5_resources_canonical.TFhirTestScriptOrigin;
  TFhirTestScriptOriginList = fhir5_resources_canonical.TFhirTestScriptOriginList;
  TFhirTestScriptDestination = fhir5_resources_canonical.TFhirTestScriptDestination;
  TFhirTestScriptDestinationList = fhir5_resources_canonical.TFhirTestScriptDestinationList;
  TFhirTestScriptMetadata = fhir5_resources_canonical.TFhirTestScriptMetadata;
  TFhirTestScriptMetadataList = fhir5_resources_canonical.TFhirTestScriptMetadataList;
  TFhirTestScriptMetadataLink = fhir5_resources_canonical.TFhirTestScriptMetadataLink;
  TFhirTestScriptMetadataLinkList = fhir5_resources_canonical.TFhirTestScriptMetadataLinkList;
  TFhirTestScriptMetadataCapability = fhir5_resources_canonical.TFhirTestScriptMetadataCapability;
  TFhirTestScriptMetadataCapabilityList = fhir5_resources_canonical.TFhirTestScriptMetadataCapabilityList;
  TFhirTestScriptScope = fhir5_resources_canonical.TFhirTestScriptScope;
  TFhirTestScriptScopeList = fhir5_resources_canonical.TFhirTestScriptScopeList;
  TFhirTestScriptFixture = fhir5_resources_canonical.TFhirTestScriptFixture;
  TFhirTestScriptFixtureList = fhir5_resources_canonical.TFhirTestScriptFixtureList;
  TFhirTestScriptVariable = fhir5_resources_canonical.TFhirTestScriptVariable;
  TFhirTestScriptVariableList = fhir5_resources_canonical.TFhirTestScriptVariableList;
  TFhirTestScriptSetup = fhir5_resources_canonical.TFhirTestScriptSetup;
  TFhirTestScriptSetupList = fhir5_resources_canonical.TFhirTestScriptSetupList;
  TFhirTestScriptSetupAction = fhir5_resources_canonical.TFhirTestScriptSetupAction;
  TFhirTestScriptSetupActionList = fhir5_resources_canonical.TFhirTestScriptSetupActionList;
  TFhirTestScriptSetupActionOperation = fhir5_resources_canonical.TFhirTestScriptSetupActionOperation;
  TFhirTestScriptSetupActionOperationList = fhir5_resources_canonical.TFhirTestScriptSetupActionOperationList;
  TFhirTestScriptSetupActionOperationRequestHeader = fhir5_resources_canonical.TFhirTestScriptSetupActionOperationRequestHeader;
  TFhirTestScriptSetupActionOperationRequestHeaderList = fhir5_resources_canonical.TFhirTestScriptSetupActionOperationRequestHeaderList;
  TFhirTestScriptSetupActionAssert = fhir5_resources_canonical.TFhirTestScriptSetupActionAssert;
  TFhirTestScriptSetupActionAssertList = fhir5_resources_canonical.TFhirTestScriptSetupActionAssertList;
  TFhirTestScriptTest = fhir5_resources_canonical.TFhirTestScriptTest;
  TFhirTestScriptTestList = fhir5_resources_canonical.TFhirTestScriptTestList;
  TFhirTestScriptTestAction = fhir5_resources_canonical.TFhirTestScriptTestAction;
  TFhirTestScriptTestActionList = fhir5_resources_canonical.TFhirTestScriptTestActionList;
  TFhirTestScriptTeardown = fhir5_resources_canonical.TFhirTestScriptTeardown;
  TFhirTestScriptTeardownList = fhir5_resources_canonical.TFhirTestScriptTeardownList;
  TFhirTestScriptTeardownAction = fhir5_resources_canonical.TFhirTestScriptTeardownAction;
  TFhirTestScriptTeardownActionList = fhir5_resources_canonical.TFhirTestScriptTeardownActionList;
  TFhirTestScript = fhir5_resources_canonical.TFhirTestScript;
  TFhirTestScriptList = fhir5_resources_canonical.TFhirTestScriptList;
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_TRANSPORT}
  TFhirTransportRestriction = fhir5_resources_clinical.TFhirTransportRestriction;
  TFhirTransportRestrictionList = fhir5_resources_clinical.TFhirTransportRestrictionList;
  TFhirTransportInput = fhir5_resources_clinical.TFhirTransportInput;
  TFhirTransportInputList = fhir5_resources_clinical.TFhirTransportInputList;
  TFhirTransportOutput = fhir5_resources_clinical.TFhirTransportOutput;
  TFhirTransportOutputList = fhir5_resources_clinical.TFhirTransportOutputList;
  TFhirTransport = fhir5_resources_clinical.TFhirTransport;
  TFhirTransportList = fhir5_resources_clinical.TFhirTransportList;
{$ENDIF FHIR_TRANSPORT}
{$IFDEF FHIR_VALUESET}
  TFhirValueSetCompose = fhir5_resources_canonical.TFhirValueSetCompose;
  TFhirValueSetComposeList = fhir5_resources_canonical.TFhirValueSetComposeList;
  TFhirValueSetComposeInclude = fhir5_resources_canonical.TFhirValueSetComposeInclude;
  TFhirValueSetComposeIncludeList = fhir5_resources_canonical.TFhirValueSetComposeIncludeList;
  TFhirValueSetComposeIncludeConcept = fhir5_resources_canonical.TFhirValueSetComposeIncludeConcept;
  TFhirValueSetComposeIncludeConceptList = fhir5_resources_canonical.TFhirValueSetComposeIncludeConceptList;
  TFhirValueSetComposeIncludeConceptDesignation = fhir5_resources_canonical.TFhirValueSetComposeIncludeConceptDesignation;
  TFhirValueSetComposeIncludeConceptDesignationList = fhir5_resources_canonical.TFhirValueSetComposeIncludeConceptDesignationList;
  TFhirValueSetComposeIncludeFilter = fhir5_resources_canonical.TFhirValueSetComposeIncludeFilter;
  TFhirValueSetComposeIncludeFilterList = fhir5_resources_canonical.TFhirValueSetComposeIncludeFilterList;
  TFhirValueSetExpansion = fhir5_resources_canonical.TFhirValueSetExpansion;
  TFhirValueSetExpansionList = fhir5_resources_canonical.TFhirValueSetExpansionList;
  TFhirValueSetExpansionParameter = fhir5_resources_canonical.TFhirValueSetExpansionParameter;
  TFhirValueSetExpansionParameterList = fhir5_resources_canonical.TFhirValueSetExpansionParameterList;
  TFhirValueSetExpansionProperty = fhir5_resources_canonical.TFhirValueSetExpansionProperty;
  TFhirValueSetExpansionPropertyList = fhir5_resources_canonical.TFhirValueSetExpansionPropertyList;
  TFhirValueSetExpansionContains = fhir5_resources_canonical.TFhirValueSetExpansionContains;
  TFhirValueSetExpansionContainsList = fhir5_resources_canonical.TFhirValueSetExpansionContainsList;
  TFhirValueSetExpansionContainsProperty = fhir5_resources_canonical.TFhirValueSetExpansionContainsProperty;
  TFhirValueSetExpansionContainsPropertyList = fhir5_resources_canonical.TFhirValueSetExpansionContainsPropertyList;
  TFhirValueSetExpansionContainsPropertySubProperty = fhir5_resources_canonical.TFhirValueSetExpansionContainsPropertySubProperty;
  TFhirValueSetExpansionContainsPropertySubPropertyList = fhir5_resources_canonical.TFhirValueSetExpansionContainsPropertySubPropertyList;
  TFhirValueSetScope = fhir5_resources_canonical.TFhirValueSetScope;
  TFhirValueSetScopeList = fhir5_resources_canonical.TFhirValueSetScopeList;
  TFhirValueSet = fhir5_resources_canonical.TFhirValueSet;
  TFhirValueSetList = fhir5_resources_canonical.TFhirValueSetList;
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VERIFICATIONRESULT}
  TFhirVerificationResultPrimarySource = fhir5_resources_other.TFhirVerificationResultPrimarySource;
  TFhirVerificationResultPrimarySourceList = fhir5_resources_other.TFhirVerificationResultPrimarySourceList;
  TFhirVerificationResultAttestation = fhir5_resources_other.TFhirVerificationResultAttestation;
  TFhirVerificationResultAttestationList = fhir5_resources_other.TFhirVerificationResultAttestationList;
  TFhirVerificationResultValidator = fhir5_resources_other.TFhirVerificationResultValidator;
  TFhirVerificationResultValidatorList = fhir5_resources_other.TFhirVerificationResultValidatorList;
  TFhirVerificationResult = fhir5_resources_other.TFhirVerificationResult;
  TFhirVerificationResultList = fhir5_resources_other.TFhirVerificationResultList;
{$ENDIF FHIR_VERIFICATIONRESULT}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  TFhirVisionPrescriptionLensSpecification = fhir5_resources_clinical.TFhirVisionPrescriptionLensSpecification;
  TFhirVisionPrescriptionLensSpecificationList = fhir5_resources_clinical.TFhirVisionPrescriptionLensSpecificationList;
  TFhirVisionPrescriptionLensSpecificationPrism = fhir5_resources_clinical.TFhirVisionPrescriptionLensSpecificationPrism;
  TFhirVisionPrescriptionLensSpecificationPrismList = fhir5_resources_clinical.TFhirVisionPrescriptionLensSpecificationPrismList;
  TFhirVisionPrescription = fhir5_resources_clinical.TFhirVisionPrescription;
  TFhirVisionPrescriptionList = fhir5_resources_clinical.TFhirVisionPrescriptionList;
{$ENDIF FHIR_VISIONPRESCRIPTION}



implementation

end.

