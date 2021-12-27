unit fhir4b_resources;

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
{$I fhir4b.inc}

interface

// Generated on Mon, Dec 27, 2021 21:46+1100 for FHIR v4.3.0

uses
  SysUtils, Classes, 
  fsl_base, fsl_utilities, fsl_stream, 
  fhir_objects, fhir_utilities,  
  fhir4b_base, fhir4b_enums, fhir4b_types, fhir4b_resources_base,
  fhir4b_resources_admin, fhir4b_resources_canonical, fhir4b_resources_clinical, fhir4b_resources_financial, fhir4b_resources_medications, fhir4b_resources_other;

type
 TFhirResourceType = fhir4b_resources_base.TFhirResourceType;
  TFhirResourceTypeSet = fhir4b_resources_base.TFhirResourceTypeSet;
  TFhirResource = fhir4b_resources_base.TFhirResource;
  TFhirResourceClass = fhir4b_resources_base.TFhirResourceClass;
  TFhirResourceList = fhir4b_resources_base.TFhirResourceList;
  TFhirDomainResource = fhir4b_resources_base.TFhirDomainResource;

  TFhirCanonicalResource = fhir4b_resources_canonical.TFhirCanonicalResource;
  TFhirMetadataResource = fhir4b_resources_canonical.TFhirMetadataResource;
  
{$IFDEF FHIR_ACCOUNT}
  TFhirAccountCoverage = fhir4b_resources_financial.TFhirAccountCoverage;
  TFhirAccountCoverageList = fhir4b_resources_financial.TFhirAccountCoverageList;
  TFhirAccountGuarantor = fhir4b_resources_financial.TFhirAccountGuarantor;
  TFhirAccountGuarantorList = fhir4b_resources_financial.TFhirAccountGuarantorList;
  TFhirAccount = fhir4b_resources_financial.TFhirAccount;
  TFhirAccountList = fhir4b_resources_financial.TFhirAccountList;
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  TFhirActivityDefinitionParticipant = fhir4b_resources_other.TFhirActivityDefinitionParticipant;
  TFhirActivityDefinitionParticipantList = fhir4b_resources_other.TFhirActivityDefinitionParticipantList;
  TFhirActivityDefinitionDynamicValue = fhir4b_resources_other.TFhirActivityDefinitionDynamicValue;
  TFhirActivityDefinitionDynamicValueList = fhir4b_resources_other.TFhirActivityDefinitionDynamicValueList;
  TFhirActivityDefinition = fhir4b_resources_other.TFhirActivityDefinition;
  TFhirActivityDefinitionList = fhir4b_resources_other.TFhirActivityDefinitionList;
{$ENDIF FHIR_ACTIVITYDEFINITION}
{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
  TFhirAdministrableProductDefinitionProperty = fhir4b_resources_medications.TFhirAdministrableProductDefinitionProperty;
  TFhirAdministrableProductDefinitionPropertyList = fhir4b_resources_medications.TFhirAdministrableProductDefinitionPropertyList;
  TFhirAdministrableProductDefinitionRouteOfAdministration = fhir4b_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministration;
  TFhirAdministrableProductDefinitionRouteOfAdministrationList = fhir4b_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministrationList;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpecies = fhir4b_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpecies;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesList = fhir4b_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesList;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriod = fhir4b_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriod;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriodList = fhir4b_resources_medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriodList;
  TFhirAdministrableProductDefinition = fhir4b_resources_medications.TFhirAdministrableProductDefinition;
  TFhirAdministrableProductDefinitionList = fhir4b_resources_medications.TFhirAdministrableProductDefinitionList;
{$ENDIF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
{$IFDEF FHIR_ADVERSEEVENT}
  TFhirAdverseEventSuspectEntity = fhir4b_resources_clinical.TFhirAdverseEventSuspectEntity;
  TFhirAdverseEventSuspectEntityList = fhir4b_resources_clinical.TFhirAdverseEventSuspectEntityList;
  TFhirAdverseEventSuspectEntityCausality = fhir4b_resources_clinical.TFhirAdverseEventSuspectEntityCausality;
  TFhirAdverseEventSuspectEntityCausalityList = fhir4b_resources_clinical.TFhirAdverseEventSuspectEntityCausalityList;
  TFhirAdverseEvent = fhir4b_resources_clinical.TFhirAdverseEvent;
  TFhirAdverseEventList = fhir4b_resources_clinical.TFhirAdverseEventList;
{$ENDIF FHIR_ADVERSEEVENT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  TFhirAllergyIntoleranceReaction = fhir4b_resources_clinical.TFhirAllergyIntoleranceReaction;
  TFhirAllergyIntoleranceReactionList = fhir4b_resources_clinical.TFhirAllergyIntoleranceReactionList;
  TFhirAllergyIntolerance = fhir4b_resources_clinical.TFhirAllergyIntolerance;
  TFhirAllergyIntoleranceList = fhir4b_resources_clinical.TFhirAllergyIntoleranceList;
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  TFhirAppointmentParticipant = fhir4b_resources_clinical.TFhirAppointmentParticipant;
  TFhirAppointmentParticipantList = fhir4b_resources_clinical.TFhirAppointmentParticipantList;
  TFhirAppointment = fhir4b_resources_clinical.TFhirAppointment;
  TFhirAppointmentList = fhir4b_resources_clinical.TFhirAppointmentList;
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  TFhirAppointmentResponse = fhir4b_resources_clinical.TFhirAppointmentResponse;
  TFhirAppointmentResponseList = fhir4b_resources_clinical.TFhirAppointmentResponseList;
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_AUDITEVENT}
  TFhirAuditEventAgent = fhir4b_resources_other.TFhirAuditEventAgent;
  TFhirAuditEventAgentList = fhir4b_resources_other.TFhirAuditEventAgentList;
  TFhirAuditEventAgentNetwork = fhir4b_resources_other.TFhirAuditEventAgentNetwork;
  TFhirAuditEventAgentNetworkList = fhir4b_resources_other.TFhirAuditEventAgentNetworkList;
  TFhirAuditEventSource = fhir4b_resources_other.TFhirAuditEventSource;
  TFhirAuditEventSourceList = fhir4b_resources_other.TFhirAuditEventSourceList;
  TFhirAuditEventEntity = fhir4b_resources_other.TFhirAuditEventEntity;
  TFhirAuditEventEntityList = fhir4b_resources_other.TFhirAuditEventEntityList;
  TFhirAuditEventEntityDetail = fhir4b_resources_other.TFhirAuditEventEntityDetail;
  TFhirAuditEventEntityDetailList = fhir4b_resources_other.TFhirAuditEventEntityDetailList;
  TFhirAuditEvent = fhir4b_resources_other.TFhirAuditEvent;
  TFhirAuditEventList = fhir4b_resources_other.TFhirAuditEventList;
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BASIC}
  TFhirBasic = fhir4b_resources_clinical.TFhirBasic;
  TFhirBasicList = fhir4b_resources_clinical.TFhirBasicList;
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BINARY}
  TFhirBinary = fhir4b_resources_other.TFhirBinary;
  TFhirBinaryList = fhir4b_resources_other.TFhirBinaryList;
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  TFhirBiologicallyDerivedProductCollection = fhir4b_resources_clinical.TFhirBiologicallyDerivedProductCollection;
  TFhirBiologicallyDerivedProductCollectionList = fhir4b_resources_clinical.TFhirBiologicallyDerivedProductCollectionList;
  TFhirBiologicallyDerivedProductProcessing = fhir4b_resources_clinical.TFhirBiologicallyDerivedProductProcessing;
  TFhirBiologicallyDerivedProductProcessingList = fhir4b_resources_clinical.TFhirBiologicallyDerivedProductProcessingList;
  TFhirBiologicallyDerivedProductManipulation = fhir4b_resources_clinical.TFhirBiologicallyDerivedProductManipulation;
  TFhirBiologicallyDerivedProductManipulationList = fhir4b_resources_clinical.TFhirBiologicallyDerivedProductManipulationList;
  TFhirBiologicallyDerivedProductStorage = fhir4b_resources_clinical.TFhirBiologicallyDerivedProductStorage;
  TFhirBiologicallyDerivedProductStorageList = fhir4b_resources_clinical.TFhirBiologicallyDerivedProductStorageList;
  TFhirBiologicallyDerivedProduct = fhir4b_resources_clinical.TFhirBiologicallyDerivedProduct;
  TFhirBiologicallyDerivedProductList = fhir4b_resources_clinical.TFhirBiologicallyDerivedProductList;
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
{$IFDEF FHIR_BODYSTRUCTURE}
  TFhirBodyStructure = fhir4b_resources_clinical.TFhirBodyStructure;
  TFhirBodyStructureList = fhir4b_resources_clinical.TFhirBodyStructureList;
{$ENDIF FHIR_BODYSTRUCTURE}
{$IFDEF FHIR_BUNDLE}
  TFhirBundleLink = fhir4b_resources_other.TFhirBundleLink;
  TFhirBundleLinkList = fhir4b_resources_other.TFhirBundleLinkList;
  TFhirBundleEntry = fhir4b_resources_other.TFhirBundleEntry;
  TFhirBundleEntryList = fhir4b_resources_other.TFhirBundleEntryList;
  TFhirBundleEntrySearch = fhir4b_resources_other.TFhirBundleEntrySearch;
  TFhirBundleEntrySearchList = fhir4b_resources_other.TFhirBundleEntrySearchList;
  TFhirBundleEntryRequest = fhir4b_resources_other.TFhirBundleEntryRequest;
  TFhirBundleEntryRequestList = fhir4b_resources_other.TFhirBundleEntryRequestList;
  TFhirBundleEntryResponse = fhir4b_resources_other.TFhirBundleEntryResponse;
  TFhirBundleEntryResponseList = fhir4b_resources_other.TFhirBundleEntryResponseList;
  TFhirBundle = fhir4b_resources_other.TFhirBundle;
  TFhirBundleList = fhir4b_resources_other.TFhirBundleList;
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  TFhirCapabilityStatementSoftware = fhir4b_resources_canonical.TFhirCapabilityStatementSoftware;
  TFhirCapabilityStatementSoftwareList = fhir4b_resources_canonical.TFhirCapabilityStatementSoftwareList;
  TFhirCapabilityStatementImplementation = fhir4b_resources_canonical.TFhirCapabilityStatementImplementation;
  TFhirCapabilityStatementImplementationList = fhir4b_resources_canonical.TFhirCapabilityStatementImplementationList;
  TFhirCapabilityStatementRest = fhir4b_resources_canonical.TFhirCapabilityStatementRest;
  TFhirCapabilityStatementRestList = fhir4b_resources_canonical.TFhirCapabilityStatementRestList;
  TFhirCapabilityStatementRestSecurity = fhir4b_resources_canonical.TFhirCapabilityStatementRestSecurity;
  TFhirCapabilityStatementRestSecurityList = fhir4b_resources_canonical.TFhirCapabilityStatementRestSecurityList;
  TFhirCapabilityStatementRestResource = fhir4b_resources_canonical.TFhirCapabilityStatementRestResource;
  TFhirCapabilityStatementRestResourceList = fhir4b_resources_canonical.TFhirCapabilityStatementRestResourceList;
  TFhirCapabilityStatementRestResourceInteraction = fhir4b_resources_canonical.TFhirCapabilityStatementRestResourceInteraction;
  TFhirCapabilityStatementRestResourceInteractionList = fhir4b_resources_canonical.TFhirCapabilityStatementRestResourceInteractionList;
  TFhirCapabilityStatementRestResourceSearchParam = fhir4b_resources_canonical.TFhirCapabilityStatementRestResourceSearchParam;
  TFhirCapabilityStatementRestResourceSearchParamList = fhir4b_resources_canonical.TFhirCapabilityStatementRestResourceSearchParamList;
  TFhirCapabilityStatementRestResourceOperation = fhir4b_resources_canonical.TFhirCapabilityStatementRestResourceOperation;
  TFhirCapabilityStatementRestResourceOperationList = fhir4b_resources_canonical.TFhirCapabilityStatementRestResourceOperationList;
  TFhirCapabilityStatementRestInteraction = fhir4b_resources_canonical.TFhirCapabilityStatementRestInteraction;
  TFhirCapabilityStatementRestInteractionList = fhir4b_resources_canonical.TFhirCapabilityStatementRestInteractionList;
  TFhirCapabilityStatementMessaging = fhir4b_resources_canonical.TFhirCapabilityStatementMessaging;
  TFhirCapabilityStatementMessagingList = fhir4b_resources_canonical.TFhirCapabilityStatementMessagingList;
  TFhirCapabilityStatementMessagingEndpoint = fhir4b_resources_canonical.TFhirCapabilityStatementMessagingEndpoint;
  TFhirCapabilityStatementMessagingEndpointList = fhir4b_resources_canonical.TFhirCapabilityStatementMessagingEndpointList;
  TFhirCapabilityStatementMessagingSupportedMessage = fhir4b_resources_canonical.TFhirCapabilityStatementMessagingSupportedMessage;
  TFhirCapabilityStatementMessagingSupportedMessageList = fhir4b_resources_canonical.TFhirCapabilityStatementMessagingSupportedMessageList;
  TFhirCapabilityStatementDocument = fhir4b_resources_canonical.TFhirCapabilityStatementDocument;
  TFhirCapabilityStatementDocumentList = fhir4b_resources_canonical.TFhirCapabilityStatementDocumentList;
  TFhirCapabilityStatement = fhir4b_resources_canonical.TFhirCapabilityStatement;
  TFhirCapabilityStatementList = fhir4b_resources_canonical.TFhirCapabilityStatementList;
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CAREPLAN}
  TFhirCarePlanActivity = fhir4b_resources_clinical.TFhirCarePlanActivity;
  TFhirCarePlanActivityList = fhir4b_resources_clinical.TFhirCarePlanActivityList;
  TFhirCarePlanActivityDetail = fhir4b_resources_clinical.TFhirCarePlanActivityDetail;
  TFhirCarePlanActivityDetailList = fhir4b_resources_clinical.TFhirCarePlanActivityDetailList;
  TFhirCarePlan = fhir4b_resources_clinical.TFhirCarePlan;
  TFhirCarePlanList = fhir4b_resources_clinical.TFhirCarePlanList;
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CARETEAM}
  TFhirCareTeamParticipant = fhir4b_resources_clinical.TFhirCareTeamParticipant;
  TFhirCareTeamParticipantList = fhir4b_resources_clinical.TFhirCareTeamParticipantList;
  TFhirCareTeam = fhir4b_resources_clinical.TFhirCareTeam;
  TFhirCareTeamList = fhir4b_resources_clinical.TFhirCareTeamList;
{$ENDIF FHIR_CARETEAM}
{$IFDEF FHIR_CATALOGENTRY}
  TFhirCatalogEntryRelatedEntry = fhir4b_resources_admin.TFhirCatalogEntryRelatedEntry;
  TFhirCatalogEntryRelatedEntryList = fhir4b_resources_admin.TFhirCatalogEntryRelatedEntryList;
  TFhirCatalogEntry = fhir4b_resources_admin.TFhirCatalogEntry;
  TFhirCatalogEntryList = fhir4b_resources_admin.TFhirCatalogEntryList;
{$ENDIF FHIR_CATALOGENTRY}
{$IFDEF FHIR_CHARGEITEM}
  TFhirChargeItemPerformer = fhir4b_resources_financial.TFhirChargeItemPerformer;
  TFhirChargeItemPerformerList = fhir4b_resources_financial.TFhirChargeItemPerformerList;
  TFhirChargeItem = fhir4b_resources_financial.TFhirChargeItem;
  TFhirChargeItemList = fhir4b_resources_financial.TFhirChargeItemList;
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CHARGEITEMDEFINITION}
  TFhirChargeItemDefinitionApplicability = fhir4b_resources_financial.TFhirChargeItemDefinitionApplicability;
  TFhirChargeItemDefinitionApplicabilityList = fhir4b_resources_financial.TFhirChargeItemDefinitionApplicabilityList;
  TFhirChargeItemDefinitionPropertyGroup = fhir4b_resources_financial.TFhirChargeItemDefinitionPropertyGroup;
  TFhirChargeItemDefinitionPropertyGroupList = fhir4b_resources_financial.TFhirChargeItemDefinitionPropertyGroupList;
  TFhirChargeItemDefinitionPropertyGroupPriceComponent = fhir4b_resources_financial.TFhirChargeItemDefinitionPropertyGroupPriceComponent;
  TFhirChargeItemDefinitionPropertyGroupPriceComponentList = fhir4b_resources_financial.TFhirChargeItemDefinitionPropertyGroupPriceComponentList;
  TFhirChargeItemDefinition = fhir4b_resources_financial.TFhirChargeItemDefinition;
  TFhirChargeItemDefinitionList = fhir4b_resources_financial.TFhirChargeItemDefinitionList;
{$ENDIF FHIR_CHARGEITEMDEFINITION}
{$IFDEF FHIR_CITATION}
  TFhirCitationClassification = fhir4b_resources_financial.TFhirCitationClassification;
  TFhirCitationClassificationList = fhir4b_resources_financial.TFhirCitationClassificationList;
  TFhirCitationStatusDate = fhir4b_resources_financial.TFhirCitationStatusDate;
  TFhirCitationStatusDateList = fhir4b_resources_financial.TFhirCitationStatusDateList;
  TFhirCitationRelatesTo = fhir4b_resources_financial.TFhirCitationRelatesTo;
  TFhirCitationRelatesToList = fhir4b_resources_financial.TFhirCitationRelatesToList;
  TFhirCitationCitedArtifact = fhir4b_resources_financial.TFhirCitationCitedArtifact;
  TFhirCitationCitedArtifactList = fhir4b_resources_financial.TFhirCitationCitedArtifactList;
  TFhirCitationCitedArtifactVersion = fhir4b_resources_financial.TFhirCitationCitedArtifactVersion;
  TFhirCitationCitedArtifactVersionList = fhir4b_resources_financial.TFhirCitationCitedArtifactVersionList;
  TFhirCitationCitedArtifactStatusDate = fhir4b_resources_financial.TFhirCitationCitedArtifactStatusDate;
  TFhirCitationCitedArtifactStatusDateList = fhir4b_resources_financial.TFhirCitationCitedArtifactStatusDateList;
  TFhirCitationCitedArtifactTitle = fhir4b_resources_financial.TFhirCitationCitedArtifactTitle;
  TFhirCitationCitedArtifactTitleList = fhir4b_resources_financial.TFhirCitationCitedArtifactTitleList;
  TFhirCitationCitedArtifactAbstract = fhir4b_resources_financial.TFhirCitationCitedArtifactAbstract;
  TFhirCitationCitedArtifactAbstractList = fhir4b_resources_financial.TFhirCitationCitedArtifactAbstractList;
  TFhirCitationCitedArtifactPart = fhir4b_resources_financial.TFhirCitationCitedArtifactPart;
  TFhirCitationCitedArtifactPartList = fhir4b_resources_financial.TFhirCitationCitedArtifactPartList;
  TFhirCitationCitedArtifactRelatesTo = fhir4b_resources_financial.TFhirCitationCitedArtifactRelatesTo;
  TFhirCitationCitedArtifactRelatesToList = fhir4b_resources_financial.TFhirCitationCitedArtifactRelatesToList;
  TFhirCitationCitedArtifactPublicationForm = fhir4b_resources_financial.TFhirCitationCitedArtifactPublicationForm;
  TFhirCitationCitedArtifactPublicationFormList = fhir4b_resources_financial.TFhirCitationCitedArtifactPublicationFormList;
  TFhirCitationCitedArtifactPublicationFormPublishedIn = fhir4b_resources_financial.TFhirCitationCitedArtifactPublicationFormPublishedIn;
  TFhirCitationCitedArtifactPublicationFormPublishedInList = fhir4b_resources_financial.TFhirCitationCitedArtifactPublicationFormPublishedInList;
  TFhirCitationCitedArtifactPublicationFormPeriodicRelease = fhir4b_resources_financial.TFhirCitationCitedArtifactPublicationFormPeriodicRelease;
  TFhirCitationCitedArtifactPublicationFormPeriodicReleaseList = fhir4b_resources_financial.TFhirCitationCitedArtifactPublicationFormPeriodicReleaseList;
  TFhirCitationCitedArtifactPublicationFormPeriodicReleaseDateOfPublication = fhir4b_resources_financial.TFhirCitationCitedArtifactPublicationFormPeriodicReleaseDateOfPublication;
  TFhirCitationCitedArtifactPublicationFormPeriodicReleaseDateOfPublicationList = fhir4b_resources_financial.TFhirCitationCitedArtifactPublicationFormPeriodicReleaseDateOfPublicationList;
  TFhirCitationCitedArtifactWebLocation = fhir4b_resources_financial.TFhirCitationCitedArtifactWebLocation;
  TFhirCitationCitedArtifactWebLocationList = fhir4b_resources_financial.TFhirCitationCitedArtifactWebLocationList;
  TFhirCitationCitedArtifactClassification = fhir4b_resources_financial.TFhirCitationCitedArtifactClassification;
  TFhirCitationCitedArtifactClassificationList = fhir4b_resources_financial.TFhirCitationCitedArtifactClassificationList;
  TFhirCitationCitedArtifactClassificationWhoClassified = fhir4b_resources_financial.TFhirCitationCitedArtifactClassificationWhoClassified;
  TFhirCitationCitedArtifactClassificationWhoClassifiedList = fhir4b_resources_financial.TFhirCitationCitedArtifactClassificationWhoClassifiedList;
  TFhirCitationCitedArtifactContributorship = fhir4b_resources_financial.TFhirCitationCitedArtifactContributorship;
  TFhirCitationCitedArtifactContributorshipList = fhir4b_resources_financial.TFhirCitationCitedArtifactContributorshipList;
  TFhirCitationCitedArtifactContributorshipEntry = fhir4b_resources_financial.TFhirCitationCitedArtifactContributorshipEntry;
  TFhirCitationCitedArtifactContributorshipEntryList = fhir4b_resources_financial.TFhirCitationCitedArtifactContributorshipEntryList;
  TFhirCitationCitedArtifactContributorshipEntryAffiliationInfo = fhir4b_resources_financial.TFhirCitationCitedArtifactContributorshipEntryAffiliationInfo;
  TFhirCitationCitedArtifactContributorshipEntryAffiliationInfoList = fhir4b_resources_financial.TFhirCitationCitedArtifactContributorshipEntryAffiliationInfoList;
  TFhirCitationCitedArtifactContributorshipEntryContributionInstance = fhir4b_resources_financial.TFhirCitationCitedArtifactContributorshipEntryContributionInstance;
  TFhirCitationCitedArtifactContributorshipEntryContributionInstanceList = fhir4b_resources_financial.TFhirCitationCitedArtifactContributorshipEntryContributionInstanceList;
  TFhirCitationCitedArtifactContributorshipSummary = fhir4b_resources_financial.TFhirCitationCitedArtifactContributorshipSummary;
  TFhirCitationCitedArtifactContributorshipSummaryList = fhir4b_resources_financial.TFhirCitationCitedArtifactContributorshipSummaryList;
  TFhirCitation = fhir4b_resources_financial.TFhirCitation;
  TFhirCitationList = fhir4b_resources_financial.TFhirCitationList;
{$ENDIF FHIR_CITATION}
{$IFDEF FHIR_CLAIM}
  TFhirClaimRelated = fhir4b_resources_financial.TFhirClaimRelated;
  TFhirClaimRelatedList = fhir4b_resources_financial.TFhirClaimRelatedList;
  TFhirClaimPayee = fhir4b_resources_financial.TFhirClaimPayee;
  TFhirClaimPayeeList = fhir4b_resources_financial.TFhirClaimPayeeList;
  TFhirClaimCareTeam = fhir4b_resources_financial.TFhirClaimCareTeam;
  TFhirClaimCareTeamList = fhir4b_resources_financial.TFhirClaimCareTeamList;
  TFhirClaimSupportingInfo = fhir4b_resources_financial.TFhirClaimSupportingInfo;
  TFhirClaimSupportingInfoList = fhir4b_resources_financial.TFhirClaimSupportingInfoList;
  TFhirClaimDiagnosis = fhir4b_resources_financial.TFhirClaimDiagnosis;
  TFhirClaimDiagnosisList = fhir4b_resources_financial.TFhirClaimDiagnosisList;
  TFhirClaimProcedure = fhir4b_resources_financial.TFhirClaimProcedure;
  TFhirClaimProcedureList = fhir4b_resources_financial.TFhirClaimProcedureList;
  TFhirClaimInsurance = fhir4b_resources_financial.TFhirClaimInsurance;
  TFhirClaimInsuranceList = fhir4b_resources_financial.TFhirClaimInsuranceList;
  TFhirClaimAccident = fhir4b_resources_financial.TFhirClaimAccident;
  TFhirClaimAccidentList = fhir4b_resources_financial.TFhirClaimAccidentList;
  TFhirClaimItem = fhir4b_resources_financial.TFhirClaimItem;
  TFhirClaimItemList = fhir4b_resources_financial.TFhirClaimItemList;
  TFhirClaimItemDetail = fhir4b_resources_financial.TFhirClaimItemDetail;
  TFhirClaimItemDetailList = fhir4b_resources_financial.TFhirClaimItemDetailList;
  TFhirClaimItemDetailSubDetail = fhir4b_resources_financial.TFhirClaimItemDetailSubDetail;
  TFhirClaimItemDetailSubDetailList = fhir4b_resources_financial.TFhirClaimItemDetailSubDetailList;
  TFhirClaim = fhir4b_resources_financial.TFhirClaim;
  TFhirClaimList = fhir4b_resources_financial.TFhirClaimList;
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  TFhirClaimResponseItem = fhir4b_resources_financial.TFhirClaimResponseItem;
  TFhirClaimResponseItemList = fhir4b_resources_financial.TFhirClaimResponseItemList;
  TFhirClaimResponseItemAdjudication = fhir4b_resources_financial.TFhirClaimResponseItemAdjudication;
  TFhirClaimResponseItemAdjudicationList = fhir4b_resources_financial.TFhirClaimResponseItemAdjudicationList;
  TFhirClaimResponseItemDetail = fhir4b_resources_financial.TFhirClaimResponseItemDetail;
  TFhirClaimResponseItemDetailList = fhir4b_resources_financial.TFhirClaimResponseItemDetailList;
  TFhirClaimResponseItemDetailSubDetail = fhir4b_resources_financial.TFhirClaimResponseItemDetailSubDetail;
  TFhirClaimResponseItemDetailSubDetailList = fhir4b_resources_financial.TFhirClaimResponseItemDetailSubDetailList;
  TFhirClaimResponseAddItem = fhir4b_resources_financial.TFhirClaimResponseAddItem;
  TFhirClaimResponseAddItemList = fhir4b_resources_financial.TFhirClaimResponseAddItemList;
  TFhirClaimResponseAddItemDetail = fhir4b_resources_financial.TFhirClaimResponseAddItemDetail;
  TFhirClaimResponseAddItemDetailList = fhir4b_resources_financial.TFhirClaimResponseAddItemDetailList;
  TFhirClaimResponseAddItemDetailSubDetail = fhir4b_resources_financial.TFhirClaimResponseAddItemDetailSubDetail;
  TFhirClaimResponseAddItemDetailSubDetailList = fhir4b_resources_financial.TFhirClaimResponseAddItemDetailSubDetailList;
  TFhirClaimResponseTotal = fhir4b_resources_financial.TFhirClaimResponseTotal;
  TFhirClaimResponseTotalList = fhir4b_resources_financial.TFhirClaimResponseTotalList;
  TFhirClaimResponsePayment = fhir4b_resources_financial.TFhirClaimResponsePayment;
  TFhirClaimResponsePaymentList = fhir4b_resources_financial.TFhirClaimResponsePaymentList;
  TFhirClaimResponseProcessNote = fhir4b_resources_financial.TFhirClaimResponseProcessNote;
  TFhirClaimResponseProcessNoteList = fhir4b_resources_financial.TFhirClaimResponseProcessNoteList;
  TFhirClaimResponseInsurance = fhir4b_resources_financial.TFhirClaimResponseInsurance;
  TFhirClaimResponseInsuranceList = fhir4b_resources_financial.TFhirClaimResponseInsuranceList;
  TFhirClaimResponseError = fhir4b_resources_financial.TFhirClaimResponseError;
  TFhirClaimResponseErrorList = fhir4b_resources_financial.TFhirClaimResponseErrorList;
  TFhirClaimResponse = fhir4b_resources_financial.TFhirClaimResponse;
  TFhirClaimResponseList = fhir4b_resources_financial.TFhirClaimResponseList;
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CLINICALIMPRESSION}
  TFhirClinicalImpressionInvestigation = fhir4b_resources_clinical.TFhirClinicalImpressionInvestigation;
  TFhirClinicalImpressionInvestigationList = fhir4b_resources_clinical.TFhirClinicalImpressionInvestigationList;
  TFhirClinicalImpressionFinding = fhir4b_resources_clinical.TFhirClinicalImpressionFinding;
  TFhirClinicalImpressionFindingList = fhir4b_resources_clinical.TFhirClinicalImpressionFindingList;
  TFhirClinicalImpression = fhir4b_resources_clinical.TFhirClinicalImpression;
  TFhirClinicalImpressionList = fhir4b_resources_clinical.TFhirClinicalImpressionList;
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_CLINICALUSEDEFINITION}
  TFhirClinicalUseDefinitionContraindication = fhir4b_resources_clinical.TFhirClinicalUseDefinitionContraindication;
  TFhirClinicalUseDefinitionContraindicationList = fhir4b_resources_clinical.TFhirClinicalUseDefinitionContraindicationList;
  TFhirClinicalUseDefinitionContraindicationOtherTherapy = fhir4b_resources_clinical.TFhirClinicalUseDefinitionContraindicationOtherTherapy;
  TFhirClinicalUseDefinitionContraindicationOtherTherapyList = fhir4b_resources_clinical.TFhirClinicalUseDefinitionContraindicationOtherTherapyList;
  TFhirClinicalUseDefinitionIndication = fhir4b_resources_clinical.TFhirClinicalUseDefinitionIndication;
  TFhirClinicalUseDefinitionIndicationList = fhir4b_resources_clinical.TFhirClinicalUseDefinitionIndicationList;
  TFhirClinicalUseDefinitionInteraction = fhir4b_resources_clinical.TFhirClinicalUseDefinitionInteraction;
  TFhirClinicalUseDefinitionInteractionList = fhir4b_resources_clinical.TFhirClinicalUseDefinitionInteractionList;
  TFhirClinicalUseDefinitionInteractionInteractant = fhir4b_resources_clinical.TFhirClinicalUseDefinitionInteractionInteractant;
  TFhirClinicalUseDefinitionInteractionInteractantList = fhir4b_resources_clinical.TFhirClinicalUseDefinitionInteractionInteractantList;
  TFhirClinicalUseDefinitionUndesirableEffect = fhir4b_resources_clinical.TFhirClinicalUseDefinitionUndesirableEffect;
  TFhirClinicalUseDefinitionUndesirableEffectList = fhir4b_resources_clinical.TFhirClinicalUseDefinitionUndesirableEffectList;
  TFhirClinicalUseDefinitionWarning = fhir4b_resources_clinical.TFhirClinicalUseDefinitionWarning;
  TFhirClinicalUseDefinitionWarningList = fhir4b_resources_clinical.TFhirClinicalUseDefinitionWarningList;
  TFhirClinicalUseDefinition = fhir4b_resources_clinical.TFhirClinicalUseDefinition;
  TFhirClinicalUseDefinitionList = fhir4b_resources_clinical.TFhirClinicalUseDefinitionList;
{$ENDIF FHIR_CLINICALUSEDEFINITION}
{$IFDEF FHIR_CODESYSTEM}
  TFhirCodeSystemFilter = fhir4b_resources_canonical.TFhirCodeSystemFilter;
  TFhirCodeSystemFilterList = fhir4b_resources_canonical.TFhirCodeSystemFilterList;
  TFhirCodeSystemProperty = fhir4b_resources_canonical.TFhirCodeSystemProperty;
  TFhirCodeSystemPropertyList = fhir4b_resources_canonical.TFhirCodeSystemPropertyList;
  TFhirCodeSystemConcept = fhir4b_resources_canonical.TFhirCodeSystemConcept;
  TFhirCodeSystemConceptList = fhir4b_resources_canonical.TFhirCodeSystemConceptList;
  TFhirCodeSystemConceptDesignation = fhir4b_resources_canonical.TFhirCodeSystemConceptDesignation;
  TFhirCodeSystemConceptDesignationList = fhir4b_resources_canonical.TFhirCodeSystemConceptDesignationList;
  TFhirCodeSystemConceptProperty = fhir4b_resources_canonical.TFhirCodeSystemConceptProperty;
  TFhirCodeSystemConceptPropertyList = fhir4b_resources_canonical.TFhirCodeSystemConceptPropertyList;
  TFhirCodeSystem = fhir4b_resources_canonical.TFhirCodeSystem;
  TFhirCodeSystemList = fhir4b_resources_canonical.TFhirCodeSystemList;
{$ENDIF FHIR_CODESYSTEM}
{$IFDEF FHIR_COMMUNICATION}
  TFhirCommunicationPayload = fhir4b_resources_clinical.TFhirCommunicationPayload;
  TFhirCommunicationPayloadList = fhir4b_resources_clinical.TFhirCommunicationPayloadList;
  TFhirCommunication = fhir4b_resources_clinical.TFhirCommunication;
  TFhirCommunicationList = fhir4b_resources_clinical.TFhirCommunicationList;
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  TFhirCommunicationRequestPayload = fhir4b_resources_clinical.TFhirCommunicationRequestPayload;
  TFhirCommunicationRequestPayloadList = fhir4b_resources_clinical.TFhirCommunicationRequestPayloadList;
  TFhirCommunicationRequest = fhir4b_resources_clinical.TFhirCommunicationRequest;
  TFhirCommunicationRequestList = fhir4b_resources_clinical.TFhirCommunicationRequestList;
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  TFhirCompartmentDefinitionResource = fhir4b_resources_canonical.TFhirCompartmentDefinitionResource;
  TFhirCompartmentDefinitionResourceList = fhir4b_resources_canonical.TFhirCompartmentDefinitionResourceList;
  TFhirCompartmentDefinition = fhir4b_resources_canonical.TFhirCompartmentDefinition;
  TFhirCompartmentDefinitionList = fhir4b_resources_canonical.TFhirCompartmentDefinitionList;
{$ENDIF FHIR_COMPARTMENTDEFINITION}
{$IFDEF FHIR_COMPOSITION}
  TFhirCompositionAttester = fhir4b_resources_clinical.TFhirCompositionAttester;
  TFhirCompositionAttesterList = fhir4b_resources_clinical.TFhirCompositionAttesterList;
  TFhirCompositionRelatesTo = fhir4b_resources_clinical.TFhirCompositionRelatesTo;
  TFhirCompositionRelatesToList = fhir4b_resources_clinical.TFhirCompositionRelatesToList;
  TFhirCompositionEvent = fhir4b_resources_clinical.TFhirCompositionEvent;
  TFhirCompositionEventList = fhir4b_resources_clinical.TFhirCompositionEventList;
  TFhirCompositionSection = fhir4b_resources_clinical.TFhirCompositionSection;
  TFhirCompositionSectionList = fhir4b_resources_clinical.TFhirCompositionSectionList;
  TFhirComposition = fhir4b_resources_clinical.TFhirComposition;
  TFhirCompositionList = fhir4b_resources_clinical.TFhirCompositionList;
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONCEPTMAP}
  TFhirConceptMapGroup = fhir4b_resources_canonical.TFhirConceptMapGroup;
  TFhirConceptMapGroupList = fhir4b_resources_canonical.TFhirConceptMapGroupList;
  TFhirConceptMapGroupElement = fhir4b_resources_canonical.TFhirConceptMapGroupElement;
  TFhirConceptMapGroupElementList = fhir4b_resources_canonical.TFhirConceptMapGroupElementList;
  TFhirConceptMapGroupElementTarget = fhir4b_resources_canonical.TFhirConceptMapGroupElementTarget;
  TFhirConceptMapGroupElementTargetList = fhir4b_resources_canonical.TFhirConceptMapGroupElementTargetList;
  TFhirConceptMapGroupElementTargetDependsOn = fhir4b_resources_canonical.TFhirConceptMapGroupElementTargetDependsOn;
  TFhirConceptMapGroupElementTargetDependsOnList = fhir4b_resources_canonical.TFhirConceptMapGroupElementTargetDependsOnList;
  TFhirConceptMapGroupUnmapped = fhir4b_resources_canonical.TFhirConceptMapGroupUnmapped;
  TFhirConceptMapGroupUnmappedList = fhir4b_resources_canonical.TFhirConceptMapGroupUnmappedList;
  TFhirConceptMap = fhir4b_resources_canonical.TFhirConceptMap;
  TFhirConceptMapList = fhir4b_resources_canonical.TFhirConceptMapList;
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_CONDITION}
  TFhirConditionStage = fhir4b_resources_clinical.TFhirConditionStage;
  TFhirConditionStageList = fhir4b_resources_clinical.TFhirConditionStageList;
  TFhirConditionEvidence = fhir4b_resources_clinical.TFhirConditionEvidence;
  TFhirConditionEvidenceList = fhir4b_resources_clinical.TFhirConditionEvidenceList;
  TFhirCondition = fhir4b_resources_clinical.TFhirCondition;
  TFhirConditionList = fhir4b_resources_clinical.TFhirConditionList;
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_CONSENT}
  TFhirConsentPolicy = fhir4b_resources_other.TFhirConsentPolicy;
  TFhirConsentPolicyList = fhir4b_resources_other.TFhirConsentPolicyList;
  TFhirConsentVerification = fhir4b_resources_other.TFhirConsentVerification;
  TFhirConsentVerificationList = fhir4b_resources_other.TFhirConsentVerificationList;
  TFhirConsentProvision = fhir4b_resources_other.TFhirConsentProvision;
  TFhirConsentProvisionList = fhir4b_resources_other.TFhirConsentProvisionList;
  TFhirConsentProvisionActor = fhir4b_resources_other.TFhirConsentProvisionActor;
  TFhirConsentProvisionActorList = fhir4b_resources_other.TFhirConsentProvisionActorList;
  TFhirConsentProvisionData = fhir4b_resources_other.TFhirConsentProvisionData;
  TFhirConsentProvisionDataList = fhir4b_resources_other.TFhirConsentProvisionDataList;
  TFhirConsent = fhir4b_resources_other.TFhirConsent;
  TFhirConsentList = fhir4b_resources_other.TFhirConsentList;
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  TFhirContractContentDefinition = fhir4b_resources_other.TFhirContractContentDefinition;
  TFhirContractContentDefinitionList = fhir4b_resources_other.TFhirContractContentDefinitionList;
  TFhirContractTerm = fhir4b_resources_other.TFhirContractTerm;
  TFhirContractTermList = fhir4b_resources_other.TFhirContractTermList;
  TFhirContractTermSecurityLabel = fhir4b_resources_other.TFhirContractTermSecurityLabel;
  TFhirContractTermSecurityLabelList = fhir4b_resources_other.TFhirContractTermSecurityLabelList;
  TFhirContractTermOffer = fhir4b_resources_other.TFhirContractTermOffer;
  TFhirContractTermOfferList = fhir4b_resources_other.TFhirContractTermOfferList;
  TFhirContractTermOfferParty = fhir4b_resources_other.TFhirContractTermOfferParty;
  TFhirContractTermOfferPartyList = fhir4b_resources_other.TFhirContractTermOfferPartyList;
  TFhirContractTermOfferAnswer = fhir4b_resources_other.TFhirContractTermOfferAnswer;
  TFhirContractTermOfferAnswerList = fhir4b_resources_other.TFhirContractTermOfferAnswerList;
  TFhirContractTermAsset = fhir4b_resources_other.TFhirContractTermAsset;
  TFhirContractTermAssetList = fhir4b_resources_other.TFhirContractTermAssetList;
  TFhirContractTermAssetContext = fhir4b_resources_other.TFhirContractTermAssetContext;
  TFhirContractTermAssetContextList = fhir4b_resources_other.TFhirContractTermAssetContextList;
  TFhirContractTermAssetValuedItem = fhir4b_resources_other.TFhirContractTermAssetValuedItem;
  TFhirContractTermAssetValuedItemList = fhir4b_resources_other.TFhirContractTermAssetValuedItemList;
  TFhirContractTermAction = fhir4b_resources_other.TFhirContractTermAction;
  TFhirContractTermActionList = fhir4b_resources_other.TFhirContractTermActionList;
  TFhirContractTermActionSubject = fhir4b_resources_other.TFhirContractTermActionSubject;
  TFhirContractTermActionSubjectList = fhir4b_resources_other.TFhirContractTermActionSubjectList;
  TFhirContractSigner = fhir4b_resources_other.TFhirContractSigner;
  TFhirContractSignerList = fhir4b_resources_other.TFhirContractSignerList;
  TFhirContractFriendly = fhir4b_resources_other.TFhirContractFriendly;
  TFhirContractFriendlyList = fhir4b_resources_other.TFhirContractFriendlyList;
  TFhirContractLegal = fhir4b_resources_other.TFhirContractLegal;
  TFhirContractLegalList = fhir4b_resources_other.TFhirContractLegalList;
  TFhirContractRule = fhir4b_resources_other.TFhirContractRule;
  TFhirContractRuleList = fhir4b_resources_other.TFhirContractRuleList;
  TFhirContract = fhir4b_resources_other.TFhirContract;
  TFhirContractList = fhir4b_resources_other.TFhirContractList;
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_COVERAGE}
  TFhirCoverageClass = fhir4b_resources_financial.TFhirCoverageClass;
  TFhirCoverageClassList = fhir4b_resources_financial.TFhirCoverageClassList;
  TFhirCoverageCostToBeneficiary = fhir4b_resources_financial.TFhirCoverageCostToBeneficiary;
  TFhirCoverageCostToBeneficiaryList = fhir4b_resources_financial.TFhirCoverageCostToBeneficiaryList;
  TFhirCoverageCostToBeneficiaryException = fhir4b_resources_financial.TFhirCoverageCostToBeneficiaryException;
  TFhirCoverageCostToBeneficiaryExceptionList = fhir4b_resources_financial.TFhirCoverageCostToBeneficiaryExceptionList;
  TFhirCoverage = fhir4b_resources_financial.TFhirCoverage;
  TFhirCoverageList = fhir4b_resources_financial.TFhirCoverageList;
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  TFhirCoverageEligibilityRequestSupportingInfo = fhir4b_resources_financial.TFhirCoverageEligibilityRequestSupportingInfo;
  TFhirCoverageEligibilityRequestSupportingInfoList = fhir4b_resources_financial.TFhirCoverageEligibilityRequestSupportingInfoList;
  TFhirCoverageEligibilityRequestInsurance = fhir4b_resources_financial.TFhirCoverageEligibilityRequestInsurance;
  TFhirCoverageEligibilityRequestInsuranceList = fhir4b_resources_financial.TFhirCoverageEligibilityRequestInsuranceList;
  TFhirCoverageEligibilityRequestItem = fhir4b_resources_financial.TFhirCoverageEligibilityRequestItem;
  TFhirCoverageEligibilityRequestItemList = fhir4b_resources_financial.TFhirCoverageEligibilityRequestItemList;
  TFhirCoverageEligibilityRequestItemDiagnosis = fhir4b_resources_financial.TFhirCoverageEligibilityRequestItemDiagnosis;
  TFhirCoverageEligibilityRequestItemDiagnosisList = fhir4b_resources_financial.TFhirCoverageEligibilityRequestItemDiagnosisList;
  TFhirCoverageEligibilityRequest = fhir4b_resources_financial.TFhirCoverageEligibilityRequest;
  TFhirCoverageEligibilityRequestList = fhir4b_resources_financial.TFhirCoverageEligibilityRequestList;
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}
{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  TFhirCoverageEligibilityResponseInsurance = fhir4b_resources_financial.TFhirCoverageEligibilityResponseInsurance;
  TFhirCoverageEligibilityResponseInsuranceList = fhir4b_resources_financial.TFhirCoverageEligibilityResponseInsuranceList;
  TFhirCoverageEligibilityResponseInsuranceItem = fhir4b_resources_financial.TFhirCoverageEligibilityResponseInsuranceItem;
  TFhirCoverageEligibilityResponseInsuranceItemList = fhir4b_resources_financial.TFhirCoverageEligibilityResponseInsuranceItemList;
  TFhirCoverageEligibilityResponseInsuranceItemBenefit = fhir4b_resources_financial.TFhirCoverageEligibilityResponseInsuranceItemBenefit;
  TFhirCoverageEligibilityResponseInsuranceItemBenefitList = fhir4b_resources_financial.TFhirCoverageEligibilityResponseInsuranceItemBenefitList;
  TFhirCoverageEligibilityResponseError = fhir4b_resources_financial.TFhirCoverageEligibilityResponseError;
  TFhirCoverageEligibilityResponseErrorList = fhir4b_resources_financial.TFhirCoverageEligibilityResponseErrorList;
  TFhirCoverageEligibilityResponse = fhir4b_resources_financial.TFhirCoverageEligibilityResponse;
  TFhirCoverageEligibilityResponseList = fhir4b_resources_financial.TFhirCoverageEligibilityResponseList;
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}
{$IFDEF FHIR_DETECTEDISSUE}
  TFhirDetectedIssueEvidence = fhir4b_resources_clinical.TFhirDetectedIssueEvidence;
  TFhirDetectedIssueEvidenceList = fhir4b_resources_clinical.TFhirDetectedIssueEvidenceList;
  TFhirDetectedIssueMitigation = fhir4b_resources_clinical.TFhirDetectedIssueMitigation;
  TFhirDetectedIssueMitigationList = fhir4b_resources_clinical.TFhirDetectedIssueMitigationList;
  TFhirDetectedIssue = fhir4b_resources_clinical.TFhirDetectedIssue;
  TFhirDetectedIssueList = fhir4b_resources_clinical.TFhirDetectedIssueList;
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICE}
  TFhirDeviceUdiCarrier = fhir4b_resources_admin.TFhirDeviceUdiCarrier;
  TFhirDeviceUdiCarrierList = fhir4b_resources_admin.TFhirDeviceUdiCarrierList;
  TFhirDeviceDeviceName = fhir4b_resources_admin.TFhirDeviceDeviceName;
  TFhirDeviceDeviceNameList = fhir4b_resources_admin.TFhirDeviceDeviceNameList;
  TFhirDeviceSpecialization = fhir4b_resources_admin.TFhirDeviceSpecialization;
  TFhirDeviceSpecializationList = fhir4b_resources_admin.TFhirDeviceSpecializationList;
  TFhirDeviceVersion = fhir4b_resources_admin.TFhirDeviceVersion;
  TFhirDeviceVersionList = fhir4b_resources_admin.TFhirDeviceVersionList;
  TFhirDeviceProperty = fhir4b_resources_admin.TFhirDeviceProperty;
  TFhirDevicePropertyList = fhir4b_resources_admin.TFhirDevicePropertyList;
  TFhirDevice = fhir4b_resources_admin.TFhirDevice;
  TFhirDeviceList = fhir4b_resources_admin.TFhirDeviceList;
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICEDEFINITION}
  TFhirDeviceDefinitionUdiDeviceIdentifier = fhir4b_resources_admin.TFhirDeviceDefinitionUdiDeviceIdentifier;
  TFhirDeviceDefinitionUdiDeviceIdentifierList = fhir4b_resources_admin.TFhirDeviceDefinitionUdiDeviceIdentifierList;
  TFhirDeviceDefinitionDeviceName = fhir4b_resources_admin.TFhirDeviceDefinitionDeviceName;
  TFhirDeviceDefinitionDeviceNameList = fhir4b_resources_admin.TFhirDeviceDefinitionDeviceNameList;
  TFhirDeviceDefinitionSpecialization = fhir4b_resources_admin.TFhirDeviceDefinitionSpecialization;
  TFhirDeviceDefinitionSpecializationList = fhir4b_resources_admin.TFhirDeviceDefinitionSpecializationList;
  TFhirDeviceDefinitionCapability = fhir4b_resources_admin.TFhirDeviceDefinitionCapability;
  TFhirDeviceDefinitionCapabilityList = fhir4b_resources_admin.TFhirDeviceDefinitionCapabilityList;
  TFhirDeviceDefinitionProperty = fhir4b_resources_admin.TFhirDeviceDefinitionProperty;
  TFhirDeviceDefinitionPropertyList = fhir4b_resources_admin.TFhirDeviceDefinitionPropertyList;
  TFhirDeviceDefinitionMaterial = fhir4b_resources_admin.TFhirDeviceDefinitionMaterial;
  TFhirDeviceDefinitionMaterialList = fhir4b_resources_admin.TFhirDeviceDefinitionMaterialList;
  TFhirDeviceDefinition = fhir4b_resources_admin.TFhirDeviceDefinition;
  TFhirDeviceDefinitionList = fhir4b_resources_admin.TFhirDeviceDefinitionList;
{$ENDIF FHIR_DEVICEDEFINITION}
{$IFDEF FHIR_DEVICEMETRIC}
  TFhirDeviceMetricCalibration = fhir4b_resources_admin.TFhirDeviceMetricCalibration;
  TFhirDeviceMetricCalibrationList = fhir4b_resources_admin.TFhirDeviceMetricCalibrationList;
  TFhirDeviceMetric = fhir4b_resources_admin.TFhirDeviceMetric;
  TFhirDeviceMetricList = fhir4b_resources_admin.TFhirDeviceMetricList;
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_DEVICEREQUEST}
  TFhirDeviceRequestParameter = fhir4b_resources_clinical.TFhirDeviceRequestParameter;
  TFhirDeviceRequestParameterList = fhir4b_resources_clinical.TFhirDeviceRequestParameterList;
  TFhirDeviceRequest = fhir4b_resources_clinical.TFhirDeviceRequest;
  TFhirDeviceRequestList = fhir4b_resources_clinical.TFhirDeviceRequestList;
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  TFhirDeviceUseStatement = fhir4b_resources_clinical.TFhirDeviceUseStatement;
  TFhirDeviceUseStatementList = fhir4b_resources_clinical.TFhirDeviceUseStatementList;
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  TFhirDiagnosticReportMedia = fhir4b_resources_clinical.TFhirDiagnosticReportMedia;
  TFhirDiagnosticReportMediaList = fhir4b_resources_clinical.TFhirDiagnosticReportMediaList;
  TFhirDiagnosticReport = fhir4b_resources_clinical.TFhirDiagnosticReport;
  TFhirDiagnosticReportList = fhir4b_resources_clinical.TFhirDiagnosticReportList;
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  TFhirDocumentManifestRelated = fhir4b_resources_clinical.TFhirDocumentManifestRelated;
  TFhirDocumentManifestRelatedList = fhir4b_resources_clinical.TFhirDocumentManifestRelatedList;
  TFhirDocumentManifest = fhir4b_resources_clinical.TFhirDocumentManifest;
  TFhirDocumentManifestList = fhir4b_resources_clinical.TFhirDocumentManifestList;
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  TFhirDocumentReferenceRelatesTo = fhir4b_resources_clinical.TFhirDocumentReferenceRelatesTo;
  TFhirDocumentReferenceRelatesToList = fhir4b_resources_clinical.TFhirDocumentReferenceRelatesToList;
  TFhirDocumentReferenceContent = fhir4b_resources_clinical.TFhirDocumentReferenceContent;
  TFhirDocumentReferenceContentList = fhir4b_resources_clinical.TFhirDocumentReferenceContentList;
  TFhirDocumentReferenceContext = fhir4b_resources_clinical.TFhirDocumentReferenceContext;
  TFhirDocumentReferenceContextList = fhir4b_resources_clinical.TFhirDocumentReferenceContextList;
  TFhirDocumentReference = fhir4b_resources_clinical.TFhirDocumentReference;
  TFhirDocumentReferenceList = fhir4b_resources_clinical.TFhirDocumentReferenceList;
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_ENCOUNTER}
  TFhirEncounterStatusHistory = fhir4b_resources_admin.TFhirEncounterStatusHistory;
  TFhirEncounterStatusHistoryList = fhir4b_resources_admin.TFhirEncounterStatusHistoryList;
  TFhirEncounterClassHistory = fhir4b_resources_admin.TFhirEncounterClassHistory;
  TFhirEncounterClassHistoryList = fhir4b_resources_admin.TFhirEncounterClassHistoryList;
  TFhirEncounterParticipant = fhir4b_resources_admin.TFhirEncounterParticipant;
  TFhirEncounterParticipantList = fhir4b_resources_admin.TFhirEncounterParticipantList;
  TFhirEncounterDiagnosis = fhir4b_resources_admin.TFhirEncounterDiagnosis;
  TFhirEncounterDiagnosisList = fhir4b_resources_admin.TFhirEncounterDiagnosisList;
  TFhirEncounterHospitalization = fhir4b_resources_admin.TFhirEncounterHospitalization;
  TFhirEncounterHospitalizationList = fhir4b_resources_admin.TFhirEncounterHospitalizationList;
  TFhirEncounterLocation = fhir4b_resources_admin.TFhirEncounterLocation;
  TFhirEncounterLocationList = fhir4b_resources_admin.TFhirEncounterLocationList;
  TFhirEncounter = fhir4b_resources_admin.TFhirEncounter;
  TFhirEncounterList = fhir4b_resources_admin.TFhirEncounterList;
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENDPOINT}
  TFhirEndpoint = fhir4b_resources_admin.TFhirEndpoint;
  TFhirEndpointList = fhir4b_resources_admin.TFhirEndpointList;
{$ENDIF FHIR_ENDPOINT}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  TFhirEnrollmentRequest = fhir4b_resources_financial.TFhirEnrollmentRequest;
  TFhirEnrollmentRequestList = fhir4b_resources_financial.TFhirEnrollmentRequestList;
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  TFhirEnrollmentResponse = fhir4b_resources_financial.TFhirEnrollmentResponse;
  TFhirEnrollmentResponseList = fhir4b_resources_financial.TFhirEnrollmentResponseList;
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EPISODEOFCARE}
  TFhirEpisodeOfCareStatusHistory = fhir4b_resources_clinical.TFhirEpisodeOfCareStatusHistory;
  TFhirEpisodeOfCareStatusHistoryList = fhir4b_resources_clinical.TFhirEpisodeOfCareStatusHistoryList;
  TFhirEpisodeOfCareDiagnosis = fhir4b_resources_clinical.TFhirEpisodeOfCareDiagnosis;
  TFhirEpisodeOfCareDiagnosisList = fhir4b_resources_clinical.TFhirEpisodeOfCareDiagnosisList;
  TFhirEpisodeOfCare = fhir4b_resources_clinical.TFhirEpisodeOfCare;
  TFhirEpisodeOfCareList = fhir4b_resources_clinical.TFhirEpisodeOfCareList;
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_EVENTDEFINITION}
  TFhirEventDefinition = fhir4b_resources_canonical.TFhirEventDefinition;
  TFhirEventDefinitionList = fhir4b_resources_canonical.TFhirEventDefinitionList;
{$ENDIF FHIR_EVENTDEFINITION}
{$IFDEF FHIR_EVIDENCE}
  TFhirEvidenceVariableDefinition = fhir4b_resources_other.TFhirEvidenceVariableDefinition;
  TFhirEvidenceVariableDefinitionList = fhir4b_resources_other.TFhirEvidenceVariableDefinitionList;
  TFhirEvidenceStatistic = fhir4b_resources_other.TFhirEvidenceStatistic;
  TFhirEvidenceStatisticList = fhir4b_resources_other.TFhirEvidenceStatisticList;
  TFhirEvidenceStatisticSampleSize = fhir4b_resources_other.TFhirEvidenceStatisticSampleSize;
  TFhirEvidenceStatisticSampleSizeList = fhir4b_resources_other.TFhirEvidenceStatisticSampleSizeList;
  TFhirEvidenceStatisticAttributeEstimate = fhir4b_resources_other.TFhirEvidenceStatisticAttributeEstimate;
  TFhirEvidenceStatisticAttributeEstimateList = fhir4b_resources_other.TFhirEvidenceStatisticAttributeEstimateList;
  TFhirEvidenceStatisticModelCharacteristic = fhir4b_resources_other.TFhirEvidenceStatisticModelCharacteristic;
  TFhirEvidenceStatisticModelCharacteristicList = fhir4b_resources_other.TFhirEvidenceStatisticModelCharacteristicList;
  TFhirEvidenceStatisticModelCharacteristicVariable = fhir4b_resources_other.TFhirEvidenceStatisticModelCharacteristicVariable;
  TFhirEvidenceStatisticModelCharacteristicVariableList = fhir4b_resources_other.TFhirEvidenceStatisticModelCharacteristicVariableList;
  TFhirEvidenceCertainty = fhir4b_resources_other.TFhirEvidenceCertainty;
  TFhirEvidenceCertaintyList = fhir4b_resources_other.TFhirEvidenceCertaintyList;
  TFhirEvidence = fhir4b_resources_other.TFhirEvidence;
  TFhirEvidenceList = fhir4b_resources_other.TFhirEvidenceList;
{$ENDIF FHIR_EVIDENCE}
{$IFDEF FHIR_EVIDENCEREPORT}
  TFhirEvidenceReportSubject = fhir4b_resources_other.TFhirEvidenceReportSubject;
  TFhirEvidenceReportSubjectList = fhir4b_resources_other.TFhirEvidenceReportSubjectList;
  TFhirEvidenceReportSubjectCharacteristic = fhir4b_resources_other.TFhirEvidenceReportSubjectCharacteristic;
  TFhirEvidenceReportSubjectCharacteristicList = fhir4b_resources_other.TFhirEvidenceReportSubjectCharacteristicList;
  TFhirEvidenceReportRelatesTo = fhir4b_resources_other.TFhirEvidenceReportRelatesTo;
  TFhirEvidenceReportRelatesToList = fhir4b_resources_other.TFhirEvidenceReportRelatesToList;
  TFhirEvidenceReportSection = fhir4b_resources_other.TFhirEvidenceReportSection;
  TFhirEvidenceReportSectionList = fhir4b_resources_other.TFhirEvidenceReportSectionList;
  TFhirEvidenceReport = fhir4b_resources_other.TFhirEvidenceReport;
  TFhirEvidenceReportList = fhir4b_resources_other.TFhirEvidenceReportList;
{$ENDIF FHIR_EVIDENCEREPORT}
{$IFDEF FHIR_EVIDENCEVARIABLE}
  TFhirEvidenceVariableCharacteristic = fhir4b_resources_other.TFhirEvidenceVariableCharacteristic;
  TFhirEvidenceVariableCharacteristicList = fhir4b_resources_other.TFhirEvidenceVariableCharacteristicList;
  TFhirEvidenceVariableCharacteristicTimeFromStart = fhir4b_resources_other.TFhirEvidenceVariableCharacteristicTimeFromStart;
  TFhirEvidenceVariableCharacteristicTimeFromStartList = fhir4b_resources_other.TFhirEvidenceVariableCharacteristicTimeFromStartList;
  TFhirEvidenceVariableCategory = fhir4b_resources_other.TFhirEvidenceVariableCategory;
  TFhirEvidenceVariableCategoryList = fhir4b_resources_other.TFhirEvidenceVariableCategoryList;
  TFhirEvidenceVariable = fhir4b_resources_other.TFhirEvidenceVariable;
  TFhirEvidenceVariableList = fhir4b_resources_other.TFhirEvidenceVariableList;
{$ENDIF FHIR_EVIDENCEVARIABLE}
{$IFDEF FHIR_EXAMPLESCENARIO}
  TFhirExampleScenarioActor = fhir4b_resources_canonical.TFhirExampleScenarioActor;
  TFhirExampleScenarioActorList = fhir4b_resources_canonical.TFhirExampleScenarioActorList;
  TFhirExampleScenarioInstance = fhir4b_resources_canonical.TFhirExampleScenarioInstance;
  TFhirExampleScenarioInstanceList = fhir4b_resources_canonical.TFhirExampleScenarioInstanceList;
  TFhirExampleScenarioInstanceVersion = fhir4b_resources_canonical.TFhirExampleScenarioInstanceVersion;
  TFhirExampleScenarioInstanceVersionList = fhir4b_resources_canonical.TFhirExampleScenarioInstanceVersionList;
  TFhirExampleScenarioInstanceContainedInstance = fhir4b_resources_canonical.TFhirExampleScenarioInstanceContainedInstance;
  TFhirExampleScenarioInstanceContainedInstanceList = fhir4b_resources_canonical.TFhirExampleScenarioInstanceContainedInstanceList;
  TFhirExampleScenarioProcess = fhir4b_resources_canonical.TFhirExampleScenarioProcess;
  TFhirExampleScenarioProcessList = fhir4b_resources_canonical.TFhirExampleScenarioProcessList;
  TFhirExampleScenarioProcessStep = fhir4b_resources_canonical.TFhirExampleScenarioProcessStep;
  TFhirExampleScenarioProcessStepList = fhir4b_resources_canonical.TFhirExampleScenarioProcessStepList;
  TFhirExampleScenarioProcessStepOperation = fhir4b_resources_canonical.TFhirExampleScenarioProcessStepOperation;
  TFhirExampleScenarioProcessStepOperationList = fhir4b_resources_canonical.TFhirExampleScenarioProcessStepOperationList;
  TFhirExampleScenarioProcessStepAlternative = fhir4b_resources_canonical.TFhirExampleScenarioProcessStepAlternative;
  TFhirExampleScenarioProcessStepAlternativeList = fhir4b_resources_canonical.TFhirExampleScenarioProcessStepAlternativeList;
  TFhirExampleScenario = fhir4b_resources_canonical.TFhirExampleScenario;
  TFhirExampleScenarioList = fhir4b_resources_canonical.TFhirExampleScenarioList;
{$ENDIF FHIR_EXAMPLESCENARIO}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  TFhirExplanationOfBenefitRelated = fhir4b_resources_financial.TFhirExplanationOfBenefitRelated;
  TFhirExplanationOfBenefitRelatedList = fhir4b_resources_financial.TFhirExplanationOfBenefitRelatedList;
  TFhirExplanationOfBenefitPayee = fhir4b_resources_financial.TFhirExplanationOfBenefitPayee;
  TFhirExplanationOfBenefitPayeeList = fhir4b_resources_financial.TFhirExplanationOfBenefitPayeeList;
  TFhirExplanationOfBenefitCareTeam = fhir4b_resources_financial.TFhirExplanationOfBenefitCareTeam;
  TFhirExplanationOfBenefitCareTeamList = fhir4b_resources_financial.TFhirExplanationOfBenefitCareTeamList;
  TFhirExplanationOfBenefitSupportingInfo = fhir4b_resources_financial.TFhirExplanationOfBenefitSupportingInfo;
  TFhirExplanationOfBenefitSupportingInfoList = fhir4b_resources_financial.TFhirExplanationOfBenefitSupportingInfoList;
  TFhirExplanationOfBenefitDiagnosis = fhir4b_resources_financial.TFhirExplanationOfBenefitDiagnosis;
  TFhirExplanationOfBenefitDiagnosisList = fhir4b_resources_financial.TFhirExplanationOfBenefitDiagnosisList;
  TFhirExplanationOfBenefitProcedure = fhir4b_resources_financial.TFhirExplanationOfBenefitProcedure;
  TFhirExplanationOfBenefitProcedureList = fhir4b_resources_financial.TFhirExplanationOfBenefitProcedureList;
  TFhirExplanationOfBenefitInsurance = fhir4b_resources_financial.TFhirExplanationOfBenefitInsurance;
  TFhirExplanationOfBenefitInsuranceList = fhir4b_resources_financial.TFhirExplanationOfBenefitInsuranceList;
  TFhirExplanationOfBenefitAccident = fhir4b_resources_financial.TFhirExplanationOfBenefitAccident;
  TFhirExplanationOfBenefitAccidentList = fhir4b_resources_financial.TFhirExplanationOfBenefitAccidentList;
  TFhirExplanationOfBenefitItem = fhir4b_resources_financial.TFhirExplanationOfBenefitItem;
  TFhirExplanationOfBenefitItemList = fhir4b_resources_financial.TFhirExplanationOfBenefitItemList;
  TFhirExplanationOfBenefitItemAdjudication = fhir4b_resources_financial.TFhirExplanationOfBenefitItemAdjudication;
  TFhirExplanationOfBenefitItemAdjudicationList = fhir4b_resources_financial.TFhirExplanationOfBenefitItemAdjudicationList;
  TFhirExplanationOfBenefitItemDetail = fhir4b_resources_financial.TFhirExplanationOfBenefitItemDetail;
  TFhirExplanationOfBenefitItemDetailList = fhir4b_resources_financial.TFhirExplanationOfBenefitItemDetailList;
  TFhirExplanationOfBenefitItemDetailSubDetail = fhir4b_resources_financial.TFhirExplanationOfBenefitItemDetailSubDetail;
  TFhirExplanationOfBenefitItemDetailSubDetailList = fhir4b_resources_financial.TFhirExplanationOfBenefitItemDetailSubDetailList;
  TFhirExplanationOfBenefitAddItem = fhir4b_resources_financial.TFhirExplanationOfBenefitAddItem;
  TFhirExplanationOfBenefitAddItemList = fhir4b_resources_financial.TFhirExplanationOfBenefitAddItemList;
  TFhirExplanationOfBenefitAddItemDetail = fhir4b_resources_financial.TFhirExplanationOfBenefitAddItemDetail;
  TFhirExplanationOfBenefitAddItemDetailList = fhir4b_resources_financial.TFhirExplanationOfBenefitAddItemDetailList;
  TFhirExplanationOfBenefitAddItemDetailSubDetail = fhir4b_resources_financial.TFhirExplanationOfBenefitAddItemDetailSubDetail;
  TFhirExplanationOfBenefitAddItemDetailSubDetailList = fhir4b_resources_financial.TFhirExplanationOfBenefitAddItemDetailSubDetailList;
  TFhirExplanationOfBenefitTotal = fhir4b_resources_financial.TFhirExplanationOfBenefitTotal;
  TFhirExplanationOfBenefitTotalList = fhir4b_resources_financial.TFhirExplanationOfBenefitTotalList;
  TFhirExplanationOfBenefitPayment = fhir4b_resources_financial.TFhirExplanationOfBenefitPayment;
  TFhirExplanationOfBenefitPaymentList = fhir4b_resources_financial.TFhirExplanationOfBenefitPaymentList;
  TFhirExplanationOfBenefitProcessNote = fhir4b_resources_financial.TFhirExplanationOfBenefitProcessNote;
  TFhirExplanationOfBenefitProcessNoteList = fhir4b_resources_financial.TFhirExplanationOfBenefitProcessNoteList;
  TFhirExplanationOfBenefitBenefitBalance = fhir4b_resources_financial.TFhirExplanationOfBenefitBenefitBalance;
  TFhirExplanationOfBenefitBenefitBalanceList = fhir4b_resources_financial.TFhirExplanationOfBenefitBenefitBalanceList;
  TFhirExplanationOfBenefitBenefitBalanceFinancial = fhir4b_resources_financial.TFhirExplanationOfBenefitBenefitBalanceFinancial;
  TFhirExplanationOfBenefitBenefitBalanceFinancialList = fhir4b_resources_financial.TFhirExplanationOfBenefitBenefitBalanceFinancialList;
  TFhirExplanationOfBenefit = fhir4b_resources_financial.TFhirExplanationOfBenefit;
  TFhirExplanationOfBenefitList = fhir4b_resources_financial.TFhirExplanationOfBenefitList;
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  TFhirFamilyMemberHistoryCondition = fhir4b_resources_clinical.TFhirFamilyMemberHistoryCondition;
  TFhirFamilyMemberHistoryConditionList = fhir4b_resources_clinical.TFhirFamilyMemberHistoryConditionList;
  TFhirFamilyMemberHistory = fhir4b_resources_clinical.TFhirFamilyMemberHistory;
  TFhirFamilyMemberHistoryList = fhir4b_resources_clinical.TFhirFamilyMemberHistoryList;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  TFhirFlag = fhir4b_resources_clinical.TFhirFlag;
  TFhirFlagList = fhir4b_resources_clinical.TFhirFlagList;
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  TFhirGoalTarget = fhir4b_resources_clinical.TFhirGoalTarget;
  TFhirGoalTargetList = fhir4b_resources_clinical.TFhirGoalTargetList;
  TFhirGoal = fhir4b_resources_clinical.TFhirGoal;
  TFhirGoalList = fhir4b_resources_clinical.TFhirGoalList;
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_GRAPHDEFINITION}
  TFhirGraphDefinitionLink = fhir4b_resources_canonical.TFhirGraphDefinitionLink;
  TFhirGraphDefinitionLinkList = fhir4b_resources_canonical.TFhirGraphDefinitionLinkList;
  TFhirGraphDefinitionLinkTarget = fhir4b_resources_canonical.TFhirGraphDefinitionLinkTarget;
  TFhirGraphDefinitionLinkTargetList = fhir4b_resources_canonical.TFhirGraphDefinitionLinkTargetList;
  TFhirGraphDefinitionLinkTargetCompartment = fhir4b_resources_canonical.TFhirGraphDefinitionLinkTargetCompartment;
  TFhirGraphDefinitionLinkTargetCompartmentList = fhir4b_resources_canonical.TFhirGraphDefinitionLinkTargetCompartmentList;
  TFhirGraphDefinition = fhir4b_resources_canonical.TFhirGraphDefinition;
  TFhirGraphDefinitionList = fhir4b_resources_canonical.TFhirGraphDefinitionList;
{$ENDIF FHIR_GRAPHDEFINITION}
{$IFDEF FHIR_GROUP}
  TFhirGroupCharacteristic = fhir4b_resources_admin.TFhirGroupCharacteristic;
  TFhirGroupCharacteristicList = fhir4b_resources_admin.TFhirGroupCharacteristicList;
  TFhirGroupMember = fhir4b_resources_admin.TFhirGroupMember;
  TFhirGroupMemberList = fhir4b_resources_admin.TFhirGroupMemberList;
  TFhirGroup = fhir4b_resources_admin.TFhirGroup;
  TFhirGroupList = fhir4b_resources_admin.TFhirGroupList;
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_GUIDANCERESPONSE}
  TFhirGuidanceResponse = fhir4b_resources_other.TFhirGuidanceResponse;
  TFhirGuidanceResponseList = fhir4b_resources_other.TFhirGuidanceResponseList;
{$ENDIF FHIR_GUIDANCERESPONSE}
{$IFDEF FHIR_HEALTHCARESERVICE}
  TFhirHealthcareServiceEligibility = fhir4b_resources_admin.TFhirHealthcareServiceEligibility;
  TFhirHealthcareServiceEligibilityList = fhir4b_resources_admin.TFhirHealthcareServiceEligibilityList;
  TFhirHealthcareServiceAvailableTime = fhir4b_resources_admin.TFhirHealthcareServiceAvailableTime;
  TFhirHealthcareServiceAvailableTimeList = fhir4b_resources_admin.TFhirHealthcareServiceAvailableTimeList;
  TFhirHealthcareServiceNotAvailable = fhir4b_resources_admin.TFhirHealthcareServiceNotAvailable;
  TFhirHealthcareServiceNotAvailableList = fhir4b_resources_admin.TFhirHealthcareServiceNotAvailableList;
  TFhirHealthcareService = fhir4b_resources_admin.TFhirHealthcareService;
  TFhirHealthcareServiceList = fhir4b_resources_admin.TFhirHealthcareServiceList;
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_IMAGINGSTUDY}
  TFhirImagingStudySeries = fhir4b_resources_clinical.TFhirImagingStudySeries;
  TFhirImagingStudySeriesList = fhir4b_resources_clinical.TFhirImagingStudySeriesList;
  TFhirImagingStudySeriesPerformer = fhir4b_resources_clinical.TFhirImagingStudySeriesPerformer;
  TFhirImagingStudySeriesPerformerList = fhir4b_resources_clinical.TFhirImagingStudySeriesPerformerList;
  TFhirImagingStudySeriesInstance = fhir4b_resources_clinical.TFhirImagingStudySeriesInstance;
  TFhirImagingStudySeriesInstanceList = fhir4b_resources_clinical.TFhirImagingStudySeriesInstanceList;
  TFhirImagingStudy = fhir4b_resources_clinical.TFhirImagingStudy;
  TFhirImagingStudyList = fhir4b_resources_clinical.TFhirImagingStudyList;
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  TFhirImmunizationPerformer = fhir4b_resources_clinical.TFhirImmunizationPerformer;
  TFhirImmunizationPerformerList = fhir4b_resources_clinical.TFhirImmunizationPerformerList;
  TFhirImmunizationEducation = fhir4b_resources_clinical.TFhirImmunizationEducation;
  TFhirImmunizationEducationList = fhir4b_resources_clinical.TFhirImmunizationEducationList;
  TFhirImmunizationReaction = fhir4b_resources_clinical.TFhirImmunizationReaction;
  TFhirImmunizationReactionList = fhir4b_resources_clinical.TFhirImmunizationReactionList;
  TFhirImmunizationProtocolApplied = fhir4b_resources_clinical.TFhirImmunizationProtocolApplied;
  TFhirImmunizationProtocolAppliedList = fhir4b_resources_clinical.TFhirImmunizationProtocolAppliedList;
  TFhirImmunization = fhir4b_resources_clinical.TFhirImmunization;
  TFhirImmunizationList = fhir4b_resources_clinical.TFhirImmunizationList;
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  TFhirImmunizationEvaluation = fhir4b_resources_clinical.TFhirImmunizationEvaluation;
  TFhirImmunizationEvaluationList = fhir4b_resources_clinical.TFhirImmunizationEvaluationList;
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  TFhirImmunizationRecommendationRecommendation = fhir4b_resources_clinical.TFhirImmunizationRecommendationRecommendation;
  TFhirImmunizationRecommendationRecommendationList = fhir4b_resources_clinical.TFhirImmunizationRecommendationRecommendationList;
  TFhirImmunizationRecommendationRecommendationDateCriterion = fhir4b_resources_clinical.TFhirImmunizationRecommendationRecommendationDateCriterion;
  TFhirImmunizationRecommendationRecommendationDateCriterionList = fhir4b_resources_clinical.TFhirImmunizationRecommendationRecommendationDateCriterionList;
  TFhirImmunizationRecommendation = fhir4b_resources_clinical.TFhirImmunizationRecommendation;
  TFhirImmunizationRecommendationList = fhir4b_resources_clinical.TFhirImmunizationRecommendationList;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  TFhirImplementationGuideDependsOn = fhir4b_resources_canonical.TFhirImplementationGuideDependsOn;
  TFhirImplementationGuideDependsOnList = fhir4b_resources_canonical.TFhirImplementationGuideDependsOnList;
  TFhirImplementationGuideGlobal = fhir4b_resources_canonical.TFhirImplementationGuideGlobal;
  TFhirImplementationGuideGlobalList = fhir4b_resources_canonical.TFhirImplementationGuideGlobalList;
  TFhirImplementationGuideDefinition = fhir4b_resources_canonical.TFhirImplementationGuideDefinition;
  TFhirImplementationGuideDefinitionList = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionList;
  TFhirImplementationGuideDefinitionGrouping = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionGrouping;
  TFhirImplementationGuideDefinitionGroupingList = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionGroupingList;
  TFhirImplementationGuideDefinitionResource = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionResource;
  TFhirImplementationGuideDefinitionResourceList = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionResourceList;
  TFhirImplementationGuideDefinitionPage = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionPage;
  TFhirImplementationGuideDefinitionPageList = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionPageList;
  TFhirImplementationGuideDefinitionParameter = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionParameter;
  TFhirImplementationGuideDefinitionParameterList = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionParameterList;
  TFhirImplementationGuideDefinitionTemplate = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionTemplate;
  TFhirImplementationGuideDefinitionTemplateList = fhir4b_resources_canonical.TFhirImplementationGuideDefinitionTemplateList;
  TFhirImplementationGuideManifest = fhir4b_resources_canonical.TFhirImplementationGuideManifest;
  TFhirImplementationGuideManifestList = fhir4b_resources_canonical.TFhirImplementationGuideManifestList;
  TFhirImplementationGuideManifestResource = fhir4b_resources_canonical.TFhirImplementationGuideManifestResource;
  TFhirImplementationGuideManifestResourceList = fhir4b_resources_canonical.TFhirImplementationGuideManifestResourceList;
  TFhirImplementationGuideManifestPage = fhir4b_resources_canonical.TFhirImplementationGuideManifestPage;
  TFhirImplementationGuideManifestPageList = fhir4b_resources_canonical.TFhirImplementationGuideManifestPageList;
  TFhirImplementationGuide = fhir4b_resources_canonical.TFhirImplementationGuide;
  TFhirImplementationGuideList = fhir4b_resources_canonical.TFhirImplementationGuideList;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_INGREDIENT}
  TFhirIngredientManufacturer = fhir4b_resources_medications.TFhirIngredientManufacturer;
  TFhirIngredientManufacturerList = fhir4b_resources_medications.TFhirIngredientManufacturerList;
  TFhirIngredientSubstance = fhir4b_resources_medications.TFhirIngredientSubstance;
  TFhirIngredientSubstanceList = fhir4b_resources_medications.TFhirIngredientSubstanceList;
  TFhirIngredientSubstanceStrength = fhir4b_resources_medications.TFhirIngredientSubstanceStrength;
  TFhirIngredientSubstanceStrengthList = fhir4b_resources_medications.TFhirIngredientSubstanceStrengthList;
  TFhirIngredientSubstanceStrengthReferenceStrength = fhir4b_resources_medications.TFhirIngredientSubstanceStrengthReferenceStrength;
  TFhirIngredientSubstanceStrengthReferenceStrengthList = fhir4b_resources_medications.TFhirIngredientSubstanceStrengthReferenceStrengthList;
  TFhirIngredient = fhir4b_resources_medications.TFhirIngredient;
  TFhirIngredientList = fhir4b_resources_medications.TFhirIngredientList;
{$ENDIF FHIR_INGREDIENT}
{$IFDEF FHIR_INSURANCEPLAN}
  TFhirInsurancePlanContact = fhir4b_resources_financial.TFhirInsurancePlanContact;
  TFhirInsurancePlanContactList = fhir4b_resources_financial.TFhirInsurancePlanContactList;
  TFhirInsurancePlanCoverage = fhir4b_resources_financial.TFhirInsurancePlanCoverage;
  TFhirInsurancePlanCoverageList = fhir4b_resources_financial.TFhirInsurancePlanCoverageList;
  TFhirInsurancePlanCoverageBenefit = fhir4b_resources_financial.TFhirInsurancePlanCoverageBenefit;
  TFhirInsurancePlanCoverageBenefitList = fhir4b_resources_financial.TFhirInsurancePlanCoverageBenefitList;
  TFhirInsurancePlanCoverageBenefitLimit = fhir4b_resources_financial.TFhirInsurancePlanCoverageBenefitLimit;
  TFhirInsurancePlanCoverageBenefitLimitList = fhir4b_resources_financial.TFhirInsurancePlanCoverageBenefitLimitList;
  TFhirInsurancePlanPlan = fhir4b_resources_financial.TFhirInsurancePlanPlan;
  TFhirInsurancePlanPlanList = fhir4b_resources_financial.TFhirInsurancePlanPlanList;
  TFhirInsurancePlanPlanGeneralCost = fhir4b_resources_financial.TFhirInsurancePlanPlanGeneralCost;
  TFhirInsurancePlanPlanGeneralCostList = fhir4b_resources_financial.TFhirInsurancePlanPlanGeneralCostList;
  TFhirInsurancePlanPlanSpecificCost = fhir4b_resources_financial.TFhirInsurancePlanPlanSpecificCost;
  TFhirInsurancePlanPlanSpecificCostList = fhir4b_resources_financial.TFhirInsurancePlanPlanSpecificCostList;
  TFhirInsurancePlanPlanSpecificCostBenefit = fhir4b_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefit;
  TFhirInsurancePlanPlanSpecificCostBenefitList = fhir4b_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefitList;
  TFhirInsurancePlanPlanSpecificCostBenefitCost = fhir4b_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefitCost;
  TFhirInsurancePlanPlanSpecificCostBenefitCostList = fhir4b_resources_financial.TFhirInsurancePlanPlanSpecificCostBenefitCostList;
  TFhirInsurancePlan = fhir4b_resources_financial.TFhirInsurancePlan;
  TFhirInsurancePlanList = fhir4b_resources_financial.TFhirInsurancePlanList;
{$ENDIF FHIR_INSURANCEPLAN}
{$IFDEF FHIR_INVOICE}
  TFhirInvoiceParticipant = fhir4b_resources_financial.TFhirInvoiceParticipant;
  TFhirInvoiceParticipantList = fhir4b_resources_financial.TFhirInvoiceParticipantList;
  TFhirInvoiceLineItem = fhir4b_resources_financial.TFhirInvoiceLineItem;
  TFhirInvoiceLineItemList = fhir4b_resources_financial.TFhirInvoiceLineItemList;
  TFhirInvoiceLineItemPriceComponent = fhir4b_resources_financial.TFhirInvoiceLineItemPriceComponent;
  TFhirInvoiceLineItemPriceComponentList = fhir4b_resources_financial.TFhirInvoiceLineItemPriceComponentList;
  TFhirInvoice = fhir4b_resources_financial.TFhirInvoice;
  TFhirInvoiceList = fhir4b_resources_financial.TFhirInvoiceList;
{$ENDIF FHIR_INVOICE}
{$IFDEF FHIR_LIBRARY}
  TFhirLibrary = fhir4b_resources_canonical.TFhirLibrary;
  TFhirLibraryList = fhir4b_resources_canonical.TFhirLibraryList;
{$ENDIF FHIR_LIBRARY}
{$IFDEF FHIR_LINKAGE}
  TFhirLinkageItem = fhir4b_resources_other.TFhirLinkageItem;
  TFhirLinkageItemList = fhir4b_resources_other.TFhirLinkageItemList;
  TFhirLinkage = fhir4b_resources_other.TFhirLinkage;
  TFhirLinkageList = fhir4b_resources_other.TFhirLinkageList;
{$ENDIF FHIR_LINKAGE}
{$IFDEF FHIR_LIST}
  TFhirListEntry = fhir4b_resources_other.TFhirListEntry;
  TFhirListEntryList = fhir4b_resources_other.TFhirListEntryList;
  TFhirList = fhir4b_resources_other.TFhirList;
  TFhirListList = fhir4b_resources_other.TFhirListList;
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_LOCATION}
  TFhirLocationPosition = fhir4b_resources_admin.TFhirLocationPosition;
  TFhirLocationPositionList = fhir4b_resources_admin.TFhirLocationPositionList;
  TFhirLocationHoursOfOperation = fhir4b_resources_admin.TFhirLocationHoursOfOperation;
  TFhirLocationHoursOfOperationList = fhir4b_resources_admin.TFhirLocationHoursOfOperationList;
  TFhirLocation = fhir4b_resources_admin.TFhirLocation;
  TFhirLocationList = fhir4b_resources_admin.TFhirLocationList;
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
  TFhirManufacturedItemDefinitionProperty = fhir4b_resources_medications.TFhirManufacturedItemDefinitionProperty;
  TFhirManufacturedItemDefinitionPropertyList = fhir4b_resources_medications.TFhirManufacturedItemDefinitionPropertyList;
  TFhirManufacturedItemDefinition = fhir4b_resources_medications.TFhirManufacturedItemDefinition;
  TFhirManufacturedItemDefinitionList = fhir4b_resources_medications.TFhirManufacturedItemDefinitionList;
{$ENDIF FHIR_MANUFACTUREDITEMDEFINITION}
{$IFDEF FHIR_MEASURE}
  TFhirMeasureGroup = fhir4b_resources_canonical.TFhirMeasureGroup;
  TFhirMeasureGroupList = fhir4b_resources_canonical.TFhirMeasureGroupList;
  TFhirMeasureGroupPopulation = fhir4b_resources_canonical.TFhirMeasureGroupPopulation;
  TFhirMeasureGroupPopulationList = fhir4b_resources_canonical.TFhirMeasureGroupPopulationList;
  TFhirMeasureGroupStratifier = fhir4b_resources_canonical.TFhirMeasureGroupStratifier;
  TFhirMeasureGroupStratifierList = fhir4b_resources_canonical.TFhirMeasureGroupStratifierList;
  TFhirMeasureGroupStratifierComponent = fhir4b_resources_canonical.TFhirMeasureGroupStratifierComponent;
  TFhirMeasureGroupStratifierComponentList = fhir4b_resources_canonical.TFhirMeasureGroupStratifierComponentList;
  TFhirMeasureSupplementalData = fhir4b_resources_canonical.TFhirMeasureSupplementalData;
  TFhirMeasureSupplementalDataList = fhir4b_resources_canonical.TFhirMeasureSupplementalDataList;
  TFhirMeasure = fhir4b_resources_canonical.TFhirMeasure;
  TFhirMeasureList = fhir4b_resources_canonical.TFhirMeasureList;
{$ENDIF FHIR_MEASURE}
{$IFDEF FHIR_MEASUREREPORT}
  TFhirMeasureReportGroup = fhir4b_resources_other.TFhirMeasureReportGroup;
  TFhirMeasureReportGroupList = fhir4b_resources_other.TFhirMeasureReportGroupList;
  TFhirMeasureReportGroupPopulation = fhir4b_resources_other.TFhirMeasureReportGroupPopulation;
  TFhirMeasureReportGroupPopulationList = fhir4b_resources_other.TFhirMeasureReportGroupPopulationList;
  TFhirMeasureReportGroupStratifier = fhir4b_resources_other.TFhirMeasureReportGroupStratifier;
  TFhirMeasureReportGroupStratifierList = fhir4b_resources_other.TFhirMeasureReportGroupStratifierList;
  TFhirMeasureReportGroupStratifierStratum = fhir4b_resources_other.TFhirMeasureReportGroupStratifierStratum;
  TFhirMeasureReportGroupStratifierStratumList = fhir4b_resources_other.TFhirMeasureReportGroupStratifierStratumList;
  TFhirMeasureReportGroupStratifierStratumComponent = fhir4b_resources_other.TFhirMeasureReportGroupStratifierStratumComponent;
  TFhirMeasureReportGroupStratifierStratumComponentList = fhir4b_resources_other.TFhirMeasureReportGroupStratifierStratumComponentList;
  TFhirMeasureReportGroupStratifierStratumPopulation = fhir4b_resources_other.TFhirMeasureReportGroupStratifierStratumPopulation;
  TFhirMeasureReportGroupStratifierStratumPopulationList = fhir4b_resources_other.TFhirMeasureReportGroupStratifierStratumPopulationList;
  TFhirMeasureReport = fhir4b_resources_other.TFhirMeasureReport;
  TFhirMeasureReportList = fhir4b_resources_other.TFhirMeasureReportList;
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MEDIA}
  TFhirMedia = fhir4b_resources_admin.TFhirMedia;
  TFhirMediaList = fhir4b_resources_admin.TFhirMediaList;
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATION}
  TFhirMedicationIngredient = fhir4b_resources_medications.TFhirMedicationIngredient;
  TFhirMedicationIngredientList = fhir4b_resources_medications.TFhirMedicationIngredientList;
  TFhirMedicationBatch = fhir4b_resources_medications.TFhirMedicationBatch;
  TFhirMedicationBatchList = fhir4b_resources_medications.TFhirMedicationBatchList;
  TFhirMedication = fhir4b_resources_medications.TFhirMedication;
  TFhirMedicationList = fhir4b_resources_medications.TFhirMedicationList;
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  TFhirMedicationAdministrationPerformer = fhir4b_resources_clinical.TFhirMedicationAdministrationPerformer;
  TFhirMedicationAdministrationPerformerList = fhir4b_resources_clinical.TFhirMedicationAdministrationPerformerList;
  TFhirMedicationAdministrationDosage = fhir4b_resources_clinical.TFhirMedicationAdministrationDosage;
  TFhirMedicationAdministrationDosageList = fhir4b_resources_clinical.TFhirMedicationAdministrationDosageList;
  TFhirMedicationAdministration = fhir4b_resources_clinical.TFhirMedicationAdministration;
  TFhirMedicationAdministrationList = fhir4b_resources_clinical.TFhirMedicationAdministrationList;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  TFhirMedicationDispensePerformer = fhir4b_resources_clinical.TFhirMedicationDispensePerformer;
  TFhirMedicationDispensePerformerList = fhir4b_resources_clinical.TFhirMedicationDispensePerformerList;
  TFhirMedicationDispenseSubstitution = fhir4b_resources_clinical.TFhirMedicationDispenseSubstitution;
  TFhirMedicationDispenseSubstitutionList = fhir4b_resources_clinical.TFhirMedicationDispenseSubstitutionList;
  TFhirMedicationDispense = fhir4b_resources_clinical.TFhirMedicationDispense;
  TFhirMedicationDispenseList = fhir4b_resources_clinical.TFhirMedicationDispenseList;
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  TFhirMedicationKnowledgeRelatedMedicationKnowledge = fhir4b_resources_medications.TFhirMedicationKnowledgeRelatedMedicationKnowledge;
  TFhirMedicationKnowledgeRelatedMedicationKnowledgeList = fhir4b_resources_medications.TFhirMedicationKnowledgeRelatedMedicationKnowledgeList;
  TFhirMedicationKnowledgeMonograph = fhir4b_resources_medications.TFhirMedicationKnowledgeMonograph;
  TFhirMedicationKnowledgeMonographList = fhir4b_resources_medications.TFhirMedicationKnowledgeMonographList;
  TFhirMedicationKnowledgeIngredient = fhir4b_resources_medications.TFhirMedicationKnowledgeIngredient;
  TFhirMedicationKnowledgeIngredientList = fhir4b_resources_medications.TFhirMedicationKnowledgeIngredientList;
  TFhirMedicationKnowledgeCost = fhir4b_resources_medications.TFhirMedicationKnowledgeCost;
  TFhirMedicationKnowledgeCostList = fhir4b_resources_medications.TFhirMedicationKnowledgeCostList;
  TFhirMedicationKnowledgeMonitoringProgram = fhir4b_resources_medications.TFhirMedicationKnowledgeMonitoringProgram;
  TFhirMedicationKnowledgeMonitoringProgramList = fhir4b_resources_medications.TFhirMedicationKnowledgeMonitoringProgramList;
  TFhirMedicationKnowledgeAdministrationGuidelines = fhir4b_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelines;
  TFhirMedicationKnowledgeAdministrationGuidelinesList = fhir4b_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelinesList;
  TFhirMedicationKnowledgeAdministrationGuidelinesDosage = fhir4b_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelinesDosage;
  TFhirMedicationKnowledgeAdministrationGuidelinesDosageList = fhir4b_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelinesDosageList;
  TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristics = fhir4b_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristics;
  TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsList = fhir4b_resources_medications.TFhirMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsList;
  TFhirMedicationKnowledgeMedicineClassification = fhir4b_resources_medications.TFhirMedicationKnowledgeMedicineClassification;
  TFhirMedicationKnowledgeMedicineClassificationList = fhir4b_resources_medications.TFhirMedicationKnowledgeMedicineClassificationList;
  TFhirMedicationKnowledgePackaging = fhir4b_resources_medications.TFhirMedicationKnowledgePackaging;
  TFhirMedicationKnowledgePackagingList = fhir4b_resources_medications.TFhirMedicationKnowledgePackagingList;
  TFhirMedicationKnowledgeDrugCharacteristic = fhir4b_resources_medications.TFhirMedicationKnowledgeDrugCharacteristic;
  TFhirMedicationKnowledgeDrugCharacteristicList = fhir4b_resources_medications.TFhirMedicationKnowledgeDrugCharacteristicList;
  TFhirMedicationKnowledgeRegulatory = fhir4b_resources_medications.TFhirMedicationKnowledgeRegulatory;
  TFhirMedicationKnowledgeRegulatoryList = fhir4b_resources_medications.TFhirMedicationKnowledgeRegulatoryList;
  TFhirMedicationKnowledgeRegulatorySubstitution = fhir4b_resources_medications.TFhirMedicationKnowledgeRegulatorySubstitution;
  TFhirMedicationKnowledgeRegulatorySubstitutionList = fhir4b_resources_medications.TFhirMedicationKnowledgeRegulatorySubstitutionList;
  TFhirMedicationKnowledgeRegulatorySchedule = fhir4b_resources_medications.TFhirMedicationKnowledgeRegulatorySchedule;
  TFhirMedicationKnowledgeRegulatoryScheduleList = fhir4b_resources_medications.TFhirMedicationKnowledgeRegulatoryScheduleList;
  TFhirMedicationKnowledgeRegulatoryMaxDispense = fhir4b_resources_medications.TFhirMedicationKnowledgeRegulatoryMaxDispense;
  TFhirMedicationKnowledgeRegulatoryMaxDispenseList = fhir4b_resources_medications.TFhirMedicationKnowledgeRegulatoryMaxDispenseList;
  TFhirMedicationKnowledgeKinetics = fhir4b_resources_medications.TFhirMedicationKnowledgeKinetics;
  TFhirMedicationKnowledgeKineticsList = fhir4b_resources_medications.TFhirMedicationKnowledgeKineticsList;
  TFhirMedicationKnowledge = fhir4b_resources_medications.TFhirMedicationKnowledge;
  TFhirMedicationKnowledgeList = fhir4b_resources_medications.TFhirMedicationKnowledgeList;
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  TFhirMedicationRequestDispenseRequest = fhir4b_resources_clinical.TFhirMedicationRequestDispenseRequest;
  TFhirMedicationRequestDispenseRequestList = fhir4b_resources_clinical.TFhirMedicationRequestDispenseRequestList;
  TFhirMedicationRequestDispenseRequestInitialFill = fhir4b_resources_clinical.TFhirMedicationRequestDispenseRequestInitialFill;
  TFhirMedicationRequestDispenseRequestInitialFillList = fhir4b_resources_clinical.TFhirMedicationRequestDispenseRequestInitialFillList;
  TFhirMedicationRequestSubstitution = fhir4b_resources_clinical.TFhirMedicationRequestSubstitution;
  TFhirMedicationRequestSubstitutionList = fhir4b_resources_clinical.TFhirMedicationRequestSubstitutionList;
  TFhirMedicationRequest = fhir4b_resources_clinical.TFhirMedicationRequest;
  TFhirMedicationRequestList = fhir4b_resources_clinical.TFhirMedicationRequestList;
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  TFhirMedicationStatement = fhir4b_resources_clinical.TFhirMedicationStatement;
  TFhirMedicationStatementList = fhir4b_resources_clinical.TFhirMedicationStatementList;
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
  TFhirMedicinalProductDefinitionContact = fhir4b_resources_medications.TFhirMedicinalProductDefinitionContact;
  TFhirMedicinalProductDefinitionContactList = fhir4b_resources_medications.TFhirMedicinalProductDefinitionContactList;
  TFhirMedicinalProductDefinitionName = fhir4b_resources_medications.TFhirMedicinalProductDefinitionName;
  TFhirMedicinalProductDefinitionNameList = fhir4b_resources_medications.TFhirMedicinalProductDefinitionNameList;
  TFhirMedicinalProductDefinitionNameNamePart = fhir4b_resources_medications.TFhirMedicinalProductDefinitionNameNamePart;
  TFhirMedicinalProductDefinitionNameNamePartList = fhir4b_resources_medications.TFhirMedicinalProductDefinitionNameNamePartList;
  TFhirMedicinalProductDefinitionNameCountryLanguage = fhir4b_resources_medications.TFhirMedicinalProductDefinitionNameCountryLanguage;
  TFhirMedicinalProductDefinitionNameCountryLanguageList = fhir4b_resources_medications.TFhirMedicinalProductDefinitionNameCountryLanguageList;
  TFhirMedicinalProductDefinitionCrossReference = fhir4b_resources_medications.TFhirMedicinalProductDefinitionCrossReference;
  TFhirMedicinalProductDefinitionCrossReferenceList = fhir4b_resources_medications.TFhirMedicinalProductDefinitionCrossReferenceList;
  TFhirMedicinalProductDefinitionOperation = fhir4b_resources_medications.TFhirMedicinalProductDefinitionOperation;
  TFhirMedicinalProductDefinitionOperationList = fhir4b_resources_medications.TFhirMedicinalProductDefinitionOperationList;
  TFhirMedicinalProductDefinitionCharacteristic = fhir4b_resources_medications.TFhirMedicinalProductDefinitionCharacteristic;
  TFhirMedicinalProductDefinitionCharacteristicList = fhir4b_resources_medications.TFhirMedicinalProductDefinitionCharacteristicList;
  TFhirMedicinalProductDefinition = fhir4b_resources_medications.TFhirMedicinalProductDefinition;
  TFhirMedicinalProductDefinitionList = fhir4b_resources_medications.TFhirMedicinalProductDefinitionList;
{$ENDIF FHIR_MEDICINALPRODUCTDEFINITION}
{$IFDEF FHIR_MESSAGEDEFINITION}
  TFhirMessageDefinitionFocus = fhir4b_resources_canonical.TFhirMessageDefinitionFocus;
  TFhirMessageDefinitionFocusList = fhir4b_resources_canonical.TFhirMessageDefinitionFocusList;
  TFhirMessageDefinitionAllowedResponse = fhir4b_resources_canonical.TFhirMessageDefinitionAllowedResponse;
  TFhirMessageDefinitionAllowedResponseList = fhir4b_resources_canonical.TFhirMessageDefinitionAllowedResponseList;
  TFhirMessageDefinition = fhir4b_resources_canonical.TFhirMessageDefinition;
  TFhirMessageDefinitionList = fhir4b_resources_canonical.TFhirMessageDefinitionList;
{$ENDIF FHIR_MESSAGEDEFINITION}
{$IFDEF FHIR_MESSAGEHEADER}
  TFhirMessageHeaderDestination = fhir4b_resources_other.TFhirMessageHeaderDestination;
  TFhirMessageHeaderDestinationList = fhir4b_resources_other.TFhirMessageHeaderDestinationList;
  TFhirMessageHeaderSource = fhir4b_resources_other.TFhirMessageHeaderSource;
  TFhirMessageHeaderSourceList = fhir4b_resources_other.TFhirMessageHeaderSourceList;
  TFhirMessageHeaderResponse = fhir4b_resources_other.TFhirMessageHeaderResponse;
  TFhirMessageHeaderResponseList = fhir4b_resources_other.TFhirMessageHeaderResponseList;
  TFhirMessageHeader = fhir4b_resources_other.TFhirMessageHeader;
  TFhirMessageHeaderList = fhir4b_resources_other.TFhirMessageHeaderList;
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_MOLECULARSEQUENCE}
  TFhirMolecularSequenceReferenceSeq = fhir4b_resources_clinical.TFhirMolecularSequenceReferenceSeq;
  TFhirMolecularSequenceReferenceSeqList = fhir4b_resources_clinical.TFhirMolecularSequenceReferenceSeqList;
  TFhirMolecularSequenceVariant = fhir4b_resources_clinical.TFhirMolecularSequenceVariant;
  TFhirMolecularSequenceVariantList = fhir4b_resources_clinical.TFhirMolecularSequenceVariantList;
  TFhirMolecularSequenceQuality = fhir4b_resources_clinical.TFhirMolecularSequenceQuality;
  TFhirMolecularSequenceQualityList = fhir4b_resources_clinical.TFhirMolecularSequenceQualityList;
  TFhirMolecularSequenceQualityRoc = fhir4b_resources_clinical.TFhirMolecularSequenceQualityRoc;
  TFhirMolecularSequenceQualityRocList = fhir4b_resources_clinical.TFhirMolecularSequenceQualityRocList;
  TFhirMolecularSequenceRepository = fhir4b_resources_clinical.TFhirMolecularSequenceRepository;
  TFhirMolecularSequenceRepositoryList = fhir4b_resources_clinical.TFhirMolecularSequenceRepositoryList;
  TFhirMolecularSequenceStructureVariant = fhir4b_resources_clinical.TFhirMolecularSequenceStructureVariant;
  TFhirMolecularSequenceStructureVariantList = fhir4b_resources_clinical.TFhirMolecularSequenceStructureVariantList;
  TFhirMolecularSequenceStructureVariantOuter = fhir4b_resources_clinical.TFhirMolecularSequenceStructureVariantOuter;
  TFhirMolecularSequenceStructureVariantOuterList = fhir4b_resources_clinical.TFhirMolecularSequenceStructureVariantOuterList;
  TFhirMolecularSequenceStructureVariantInner = fhir4b_resources_clinical.TFhirMolecularSequenceStructureVariantInner;
  TFhirMolecularSequenceStructureVariantInnerList = fhir4b_resources_clinical.TFhirMolecularSequenceStructureVariantInnerList;
  TFhirMolecularSequence = fhir4b_resources_clinical.TFhirMolecularSequence;
  TFhirMolecularSequenceList = fhir4b_resources_clinical.TFhirMolecularSequenceList;
{$ENDIF FHIR_MOLECULARSEQUENCE}
{$IFDEF FHIR_NAMINGSYSTEM}
  TFhirNamingSystemUniqueId = fhir4b_resources_canonical.TFhirNamingSystemUniqueId;
  TFhirNamingSystemUniqueIdList = fhir4b_resources_canonical.TFhirNamingSystemUniqueIdList;
  TFhirNamingSystem = fhir4b_resources_canonical.TFhirNamingSystem;
  TFhirNamingSystemList = fhir4b_resources_canonical.TFhirNamingSystemList;
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_NUTRITIONORDER}
  TFhirNutritionOrderOralDiet = fhir4b_resources_clinical.TFhirNutritionOrderOralDiet;
  TFhirNutritionOrderOralDietList = fhir4b_resources_clinical.TFhirNutritionOrderOralDietList;
  TFhirNutritionOrderOralDietNutrient = fhir4b_resources_clinical.TFhirNutritionOrderOralDietNutrient;
  TFhirNutritionOrderOralDietNutrientList = fhir4b_resources_clinical.TFhirNutritionOrderOralDietNutrientList;
  TFhirNutritionOrderOralDietTexture = fhir4b_resources_clinical.TFhirNutritionOrderOralDietTexture;
  TFhirNutritionOrderOralDietTextureList = fhir4b_resources_clinical.TFhirNutritionOrderOralDietTextureList;
  TFhirNutritionOrderSupplement = fhir4b_resources_clinical.TFhirNutritionOrderSupplement;
  TFhirNutritionOrderSupplementList = fhir4b_resources_clinical.TFhirNutritionOrderSupplementList;
  TFhirNutritionOrderEnteralFormula = fhir4b_resources_clinical.TFhirNutritionOrderEnteralFormula;
  TFhirNutritionOrderEnteralFormulaList = fhir4b_resources_clinical.TFhirNutritionOrderEnteralFormulaList;
  TFhirNutritionOrderEnteralFormulaAdministration = fhir4b_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministration;
  TFhirNutritionOrderEnteralFormulaAdministrationList = fhir4b_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministrationList;
  TFhirNutritionOrder = fhir4b_resources_clinical.TFhirNutritionOrder;
  TFhirNutritionOrderList = fhir4b_resources_clinical.TFhirNutritionOrderList;
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_NUTRITIONPRODUCT}
  TFhirNutritionProductNutrient = fhir4b_resources_medications.TFhirNutritionProductNutrient;
  TFhirNutritionProductNutrientList = fhir4b_resources_medications.TFhirNutritionProductNutrientList;
  TFhirNutritionProductIngredient = fhir4b_resources_medications.TFhirNutritionProductIngredient;
  TFhirNutritionProductIngredientList = fhir4b_resources_medications.TFhirNutritionProductIngredientList;
  TFhirNutritionProductProductCharacteristic = fhir4b_resources_medications.TFhirNutritionProductProductCharacteristic;
  TFhirNutritionProductProductCharacteristicList = fhir4b_resources_medications.TFhirNutritionProductProductCharacteristicList;
  TFhirNutritionProductInstance = fhir4b_resources_medications.TFhirNutritionProductInstance;
  TFhirNutritionProductInstanceList = fhir4b_resources_medications.TFhirNutritionProductInstanceList;
  TFhirNutritionProduct = fhir4b_resources_medications.TFhirNutritionProduct;
  TFhirNutritionProductList = fhir4b_resources_medications.TFhirNutritionProductList;
{$ENDIF FHIR_NUTRITIONPRODUCT}
{$IFDEF FHIR_OBSERVATION}
  TFhirObservationReferenceRange = fhir4b_resources_clinical.TFhirObservationReferenceRange;
  TFhirObservationReferenceRangeList = fhir4b_resources_clinical.TFhirObservationReferenceRangeList;
  TFhirObservationComponent = fhir4b_resources_clinical.TFhirObservationComponent;
  TFhirObservationComponentList = fhir4b_resources_clinical.TFhirObservationComponentList;
  TFhirObservation = fhir4b_resources_clinical.TFhirObservation;
  TFhirObservationList = fhir4b_resources_clinical.TFhirObservationList;
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_OBSERVATIONDEFINITION}
  TFhirObservationDefinitionQuantitativeDetails = fhir4b_resources_canonical.TFhirObservationDefinitionQuantitativeDetails;
  TFhirObservationDefinitionQuantitativeDetailsList = fhir4b_resources_canonical.TFhirObservationDefinitionQuantitativeDetailsList;
  TFhirObservationDefinitionQualifiedInterval = fhir4b_resources_canonical.TFhirObservationDefinitionQualifiedInterval;
  TFhirObservationDefinitionQualifiedIntervalList = fhir4b_resources_canonical.TFhirObservationDefinitionQualifiedIntervalList;
  TFhirObservationDefinition = fhir4b_resources_canonical.TFhirObservationDefinition;
  TFhirObservationDefinitionList = fhir4b_resources_canonical.TFhirObservationDefinitionList;
{$ENDIF FHIR_OBSERVATIONDEFINITION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  TFhirOperationDefinitionParameter = fhir4b_resources_canonical.TFhirOperationDefinitionParameter;
  TFhirOperationDefinitionParameterList = fhir4b_resources_canonical.TFhirOperationDefinitionParameterList;
  TFhirOperationDefinitionParameterBinding = fhir4b_resources_canonical.TFhirOperationDefinitionParameterBinding;
  TFhirOperationDefinitionParameterBindingList = fhir4b_resources_canonical.TFhirOperationDefinitionParameterBindingList;
  TFhirOperationDefinitionParameterReferencedFrom = fhir4b_resources_canonical.TFhirOperationDefinitionParameterReferencedFrom;
  TFhirOperationDefinitionParameterReferencedFromList = fhir4b_resources_canonical.TFhirOperationDefinitionParameterReferencedFromList;
  TFhirOperationDefinitionOverload = fhir4b_resources_canonical.TFhirOperationDefinitionOverload;
  TFhirOperationDefinitionOverloadList = fhir4b_resources_canonical.TFhirOperationDefinitionOverloadList;
  TFhirOperationDefinition = fhir4b_resources_canonical.TFhirOperationDefinition;
  TFhirOperationDefinitionList = fhir4b_resources_canonical.TFhirOperationDefinitionList;
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  TFhirOperationOutcomeIssue = fhir4b_resources_other.TFhirOperationOutcomeIssue;
  TFhirOperationOutcomeIssueList = fhir4b_resources_other.TFhirOperationOutcomeIssueList;
  TFhirOperationOutcome = fhir4b_resources_other.TFhirOperationOutcome;
  TFhirOperationOutcomeList = fhir4b_resources_other.TFhirOperationOutcomeList;
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_ORGANIZATION}
  TFhirOrganizationContact = fhir4b_resources_admin.TFhirOrganizationContact;
  TFhirOrganizationContactList = fhir4b_resources_admin.TFhirOrganizationContactList;
  TFhirOrganization = fhir4b_resources_admin.TFhirOrganization;
  TFhirOrganizationList = fhir4b_resources_admin.TFhirOrganizationList;
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  TFhirOrganizationAffiliation = fhir4b_resources_admin.TFhirOrganizationAffiliation;
  TFhirOrganizationAffiliationList = fhir4b_resources_admin.TFhirOrganizationAffiliationList;
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}
{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
  TFhirPackagedProductDefinitionLegalStatusOfSupply = fhir4b_resources_medications.TFhirPackagedProductDefinitionLegalStatusOfSupply;
  TFhirPackagedProductDefinitionLegalStatusOfSupplyList = fhir4b_resources_medications.TFhirPackagedProductDefinitionLegalStatusOfSupplyList;
  TFhirPackagedProductDefinitionPackage = fhir4b_resources_medications.TFhirPackagedProductDefinitionPackage;
  TFhirPackagedProductDefinitionPackageList = fhir4b_resources_medications.TFhirPackagedProductDefinitionPackageList;
  TFhirPackagedProductDefinitionPackageProperty = fhir4b_resources_medications.TFhirPackagedProductDefinitionPackageProperty;
  TFhirPackagedProductDefinitionPackagePropertyList = fhir4b_resources_medications.TFhirPackagedProductDefinitionPackagePropertyList;
  TFhirPackagedProductDefinitionPackageContainedItem = fhir4b_resources_medications.TFhirPackagedProductDefinitionPackageContainedItem;
  TFhirPackagedProductDefinitionPackageContainedItemList = fhir4b_resources_medications.TFhirPackagedProductDefinitionPackageContainedItemList;
  TFhirPackagedProductDefinition = fhir4b_resources_medications.TFhirPackagedProductDefinition;
  TFhirPackagedProductDefinitionList = fhir4b_resources_medications.TFhirPackagedProductDefinitionList;
{$ENDIF FHIR_PACKAGEDPRODUCTDEFINITION}
{$IFDEF FHIR_PARAMETERS}
  TFhirParametersParameter = fhir4b_resources_other.TFhirParametersParameter;
  TFhirParametersParameterList = fhir4b_resources_other.TFhirParametersParameterList;
  TFhirParameters = fhir4b_resources_other.TFhirParameters;
  TFhirParametersList = fhir4b_resources_other.TFhirParametersList;
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_PATIENT}
  TFhirPatientContact = fhir4b_resources_admin.TFhirPatientContact;
  TFhirPatientContactList = fhir4b_resources_admin.TFhirPatientContactList;
  TFhirPatientCommunication = fhir4b_resources_admin.TFhirPatientCommunication;
  TFhirPatientCommunicationList = fhir4b_resources_admin.TFhirPatientCommunicationList;
  TFhirPatientLink = fhir4b_resources_admin.TFhirPatientLink;
  TFhirPatientLinkList = fhir4b_resources_admin.TFhirPatientLinkList;
  TFhirPatient = fhir4b_resources_admin.TFhirPatient;
  TFhirPatientList = fhir4b_resources_admin.TFhirPatientList;
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PAYMENTNOTICE}
  TFhirPaymentNotice = fhir4b_resources_financial.TFhirPaymentNotice;
  TFhirPaymentNoticeList = fhir4b_resources_financial.TFhirPaymentNoticeList;
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  TFhirPaymentReconciliationDetail = fhir4b_resources_financial.TFhirPaymentReconciliationDetail;
  TFhirPaymentReconciliationDetailList = fhir4b_resources_financial.TFhirPaymentReconciliationDetailList;
  TFhirPaymentReconciliationProcessNote = fhir4b_resources_financial.TFhirPaymentReconciliationProcessNote;
  TFhirPaymentReconciliationProcessNoteList = fhir4b_resources_financial.TFhirPaymentReconciliationProcessNoteList;
  TFhirPaymentReconciliation = fhir4b_resources_financial.TFhirPaymentReconciliation;
  TFhirPaymentReconciliationList = fhir4b_resources_financial.TFhirPaymentReconciliationList;
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PERSON}
  TFhirPersonLink = fhir4b_resources_admin.TFhirPersonLink;
  TFhirPersonLinkList = fhir4b_resources_admin.TFhirPersonLinkList;
  TFhirPerson = fhir4b_resources_admin.TFhirPerson;
  TFhirPersonList = fhir4b_resources_admin.TFhirPersonList;
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PLANDEFINITION}
  TFhirPlanDefinitionGoal = fhir4b_resources_other.TFhirPlanDefinitionGoal;
  TFhirPlanDefinitionGoalList = fhir4b_resources_other.TFhirPlanDefinitionGoalList;
  TFhirPlanDefinitionGoalTarget = fhir4b_resources_other.TFhirPlanDefinitionGoalTarget;
  TFhirPlanDefinitionGoalTargetList = fhir4b_resources_other.TFhirPlanDefinitionGoalTargetList;
  TFhirPlanDefinitionAction = fhir4b_resources_other.TFhirPlanDefinitionAction;
  TFhirPlanDefinitionActionList = fhir4b_resources_other.TFhirPlanDefinitionActionList;
  TFhirPlanDefinitionActionCondition = fhir4b_resources_other.TFhirPlanDefinitionActionCondition;
  TFhirPlanDefinitionActionConditionList = fhir4b_resources_other.TFhirPlanDefinitionActionConditionList;
  TFhirPlanDefinitionActionRelatedAction = fhir4b_resources_other.TFhirPlanDefinitionActionRelatedAction;
  TFhirPlanDefinitionActionRelatedActionList = fhir4b_resources_other.TFhirPlanDefinitionActionRelatedActionList;
  TFhirPlanDefinitionActionParticipant = fhir4b_resources_other.TFhirPlanDefinitionActionParticipant;
  TFhirPlanDefinitionActionParticipantList = fhir4b_resources_other.TFhirPlanDefinitionActionParticipantList;
  TFhirPlanDefinitionActionDynamicValue = fhir4b_resources_other.TFhirPlanDefinitionActionDynamicValue;
  TFhirPlanDefinitionActionDynamicValueList = fhir4b_resources_other.TFhirPlanDefinitionActionDynamicValueList;
  TFhirPlanDefinition = fhir4b_resources_other.TFhirPlanDefinition;
  TFhirPlanDefinitionList = fhir4b_resources_other.TFhirPlanDefinitionList;
{$ENDIF FHIR_PLANDEFINITION}
{$IFDEF FHIR_PRACTITIONER}
  TFhirPractitionerQualification = fhir4b_resources_admin.TFhirPractitionerQualification;
  TFhirPractitionerQualificationList = fhir4b_resources_admin.TFhirPractitionerQualificationList;
  TFhirPractitioner = fhir4b_resources_admin.TFhirPractitioner;
  TFhirPractitionerList = fhir4b_resources_admin.TFhirPractitionerList;
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PRACTITIONERROLE}
  TFhirPractitionerRoleAvailableTime = fhir4b_resources_admin.TFhirPractitionerRoleAvailableTime;
  TFhirPractitionerRoleAvailableTimeList = fhir4b_resources_admin.TFhirPractitionerRoleAvailableTimeList;
  TFhirPractitionerRoleNotAvailable = fhir4b_resources_admin.TFhirPractitionerRoleNotAvailable;
  TFhirPractitionerRoleNotAvailableList = fhir4b_resources_admin.TFhirPractitionerRoleNotAvailableList;
  TFhirPractitionerRole = fhir4b_resources_admin.TFhirPractitionerRole;
  TFhirPractitionerRoleList = fhir4b_resources_admin.TFhirPractitionerRoleList;
{$ENDIF FHIR_PRACTITIONERROLE}
{$IFDEF FHIR_PROCEDURE}
  TFhirProcedurePerformer = fhir4b_resources_clinical.TFhirProcedurePerformer;
  TFhirProcedurePerformerList = fhir4b_resources_clinical.TFhirProcedurePerformerList;
  TFhirProcedureFocalDevice = fhir4b_resources_clinical.TFhirProcedureFocalDevice;
  TFhirProcedureFocalDeviceList = fhir4b_resources_clinical.TFhirProcedureFocalDeviceList;
  TFhirProcedure = fhir4b_resources_clinical.TFhirProcedure;
  TFhirProcedureList = fhir4b_resources_clinical.TFhirProcedureList;
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROVENANCE}
  TFhirProvenanceAgent = fhir4b_resources_other.TFhirProvenanceAgent;
  TFhirProvenanceAgentList = fhir4b_resources_other.TFhirProvenanceAgentList;
  TFhirProvenanceEntity = fhir4b_resources_other.TFhirProvenanceEntity;
  TFhirProvenanceEntityList = fhir4b_resources_other.TFhirProvenanceEntityList;
  TFhirProvenance = fhir4b_resources_other.TFhirProvenance;
  TFhirProvenanceList = fhir4b_resources_other.TFhirProvenanceList;
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRE}
  TFhirQuestionnaireItem = fhir4b_resources_canonical.TFhirQuestionnaireItem;
  TFhirQuestionnaireItemList = fhir4b_resources_canonical.TFhirQuestionnaireItemList;
  TFhirQuestionnaireItemEnableWhen = fhir4b_resources_canonical.TFhirQuestionnaireItemEnableWhen;
  TFhirQuestionnaireItemEnableWhenList = fhir4b_resources_canonical.TFhirQuestionnaireItemEnableWhenList;
  TFhirQuestionnaireItemAnswerOption = fhir4b_resources_canonical.TFhirQuestionnaireItemAnswerOption;
  TFhirQuestionnaireItemAnswerOptionList = fhir4b_resources_canonical.TFhirQuestionnaireItemAnswerOptionList;
  TFhirQuestionnaireItemInitial = fhir4b_resources_canonical.TFhirQuestionnaireItemInitial;
  TFhirQuestionnaireItemInitialList = fhir4b_resources_canonical.TFhirQuestionnaireItemInitialList;
  TFhirQuestionnaire = fhir4b_resources_canonical.TFhirQuestionnaire;
  TFhirQuestionnaireList = fhir4b_resources_canonical.TFhirQuestionnaireList;
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  TFhirQuestionnaireResponseItem = fhir4b_resources_other.TFhirQuestionnaireResponseItem;
  TFhirQuestionnaireResponseItemList = fhir4b_resources_other.TFhirQuestionnaireResponseItemList;
  TFhirQuestionnaireResponseItemAnswer = fhir4b_resources_other.TFhirQuestionnaireResponseItemAnswer;
  TFhirQuestionnaireResponseItemAnswerList = fhir4b_resources_other.TFhirQuestionnaireResponseItemAnswerList;
  TFhirQuestionnaireResponse = fhir4b_resources_other.TFhirQuestionnaireResponse;
  TFhirQuestionnaireResponseList = fhir4b_resources_other.TFhirQuestionnaireResponseList;
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REGULATEDAUTHORIZATION}
  TFhirRegulatedAuthorizationCase = fhir4b_resources_medications.TFhirRegulatedAuthorizationCase;
  TFhirRegulatedAuthorizationCaseList = fhir4b_resources_medications.TFhirRegulatedAuthorizationCaseList;
  TFhirRegulatedAuthorization = fhir4b_resources_medications.TFhirRegulatedAuthorization;
  TFhirRegulatedAuthorizationList = fhir4b_resources_medications.TFhirRegulatedAuthorizationList;
{$ENDIF FHIR_REGULATEDAUTHORIZATION}
{$IFDEF FHIR_RELATEDPERSON}
  TFhirRelatedPersonCommunication = fhir4b_resources_admin.TFhirRelatedPersonCommunication;
  TFhirRelatedPersonCommunicationList = fhir4b_resources_admin.TFhirRelatedPersonCommunicationList;
  TFhirRelatedPerson = fhir4b_resources_admin.TFhirRelatedPerson;
  TFhirRelatedPersonList = fhir4b_resources_admin.TFhirRelatedPersonList;
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_REQUESTGROUP}
  TFhirRequestGroupAction = fhir4b_resources_other.TFhirRequestGroupAction;
  TFhirRequestGroupActionList = fhir4b_resources_other.TFhirRequestGroupActionList;
  TFhirRequestGroupActionCondition = fhir4b_resources_other.TFhirRequestGroupActionCondition;
  TFhirRequestGroupActionConditionList = fhir4b_resources_other.TFhirRequestGroupActionConditionList;
  TFhirRequestGroupActionRelatedAction = fhir4b_resources_other.TFhirRequestGroupActionRelatedAction;
  TFhirRequestGroupActionRelatedActionList = fhir4b_resources_other.TFhirRequestGroupActionRelatedActionList;
  TFhirRequestGroup = fhir4b_resources_other.TFhirRequestGroup;
  TFhirRequestGroupList = fhir4b_resources_other.TFhirRequestGroupList;
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_RESEARCHDEFINITION}
  TFhirResearchDefinition = fhir4b_resources_other.TFhirResearchDefinition;
  TFhirResearchDefinitionList = fhir4b_resources_other.TFhirResearchDefinitionList;
{$ENDIF FHIR_RESEARCHDEFINITION}
{$IFDEF FHIR_RESEARCHELEMENTDEFINITION}
  TFhirResearchElementDefinitionCharacteristic = fhir4b_resources_other.TFhirResearchElementDefinitionCharacteristic;
  TFhirResearchElementDefinitionCharacteristicList = fhir4b_resources_other.TFhirResearchElementDefinitionCharacteristicList;
  TFhirResearchElementDefinition = fhir4b_resources_other.TFhirResearchElementDefinition;
  TFhirResearchElementDefinitionList = fhir4b_resources_other.TFhirResearchElementDefinitionList;
{$ENDIF FHIR_RESEARCHELEMENTDEFINITION}
{$IFDEF FHIR_RESEARCHSTUDY}
  TFhirResearchStudyArm = fhir4b_resources_other.TFhirResearchStudyArm;
  TFhirResearchStudyArmList = fhir4b_resources_other.TFhirResearchStudyArmList;
  TFhirResearchStudyObjective = fhir4b_resources_other.TFhirResearchStudyObjective;
  TFhirResearchStudyObjectiveList = fhir4b_resources_other.TFhirResearchStudyObjectiveList;
  TFhirResearchStudy = fhir4b_resources_other.TFhirResearchStudy;
  TFhirResearchStudyList = fhir4b_resources_other.TFhirResearchStudyList;
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  TFhirResearchSubject = fhir4b_resources_other.TFhirResearchSubject;
  TFhirResearchSubjectList = fhir4b_resources_other.TFhirResearchSubjectList;
{$ENDIF FHIR_RESEARCHSUBJECT}
{$IFDEF FHIR_RISKASSESSMENT}
  TFhirRiskAssessmentPrediction = fhir4b_resources_clinical.TFhirRiskAssessmentPrediction;
  TFhirRiskAssessmentPredictionList = fhir4b_resources_clinical.TFhirRiskAssessmentPredictionList;
  TFhirRiskAssessment = fhir4b_resources_clinical.TFhirRiskAssessment;
  TFhirRiskAssessmentList = fhir4b_resources_clinical.TFhirRiskAssessmentList;
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_SCHEDULE}
  TFhirSchedule = fhir4b_resources_admin.TFhirSchedule;
  TFhirScheduleList = fhir4b_resources_admin.TFhirScheduleList;
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SEARCHPARAMETER}
  TFhirSearchParameterComponent = fhir4b_resources_canonical.TFhirSearchParameterComponent;
  TFhirSearchParameterComponentList = fhir4b_resources_canonical.TFhirSearchParameterComponentList;
  TFhirSearchParameter = fhir4b_resources_canonical.TFhirSearchParameter;
  TFhirSearchParameterList = fhir4b_resources_canonical.TFhirSearchParameterList;
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SERVICEREQUEST}
  TFhirServiceRequest = fhir4b_resources_clinical.TFhirServiceRequest;
  TFhirServiceRequestList = fhir4b_resources_clinical.TFhirServiceRequestList;
{$ENDIF FHIR_SERVICEREQUEST}
{$IFDEF FHIR_SLOT}
  TFhirSlot = fhir4b_resources_admin.TFhirSlot;
  TFhirSlotList = fhir4b_resources_admin.TFhirSlotList;
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SPECIMEN}
  TFhirSpecimenCollection = fhir4b_resources_clinical.TFhirSpecimenCollection;
  TFhirSpecimenCollectionList = fhir4b_resources_clinical.TFhirSpecimenCollectionList;
  TFhirSpecimenProcessing = fhir4b_resources_clinical.TFhirSpecimenProcessing;
  TFhirSpecimenProcessingList = fhir4b_resources_clinical.TFhirSpecimenProcessingList;
  TFhirSpecimenContainer = fhir4b_resources_clinical.TFhirSpecimenContainer;
  TFhirSpecimenContainerList = fhir4b_resources_clinical.TFhirSpecimenContainerList;
  TFhirSpecimen = fhir4b_resources_clinical.TFhirSpecimen;
  TFhirSpecimenList = fhir4b_resources_clinical.TFhirSpecimenList;
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_SPECIMENDEFINITION}
  TFhirSpecimenDefinitionTypeTested = fhir4b_resources_canonical.TFhirSpecimenDefinitionTypeTested;
  TFhirSpecimenDefinitionTypeTestedList = fhir4b_resources_canonical.TFhirSpecimenDefinitionTypeTestedList;
  TFhirSpecimenDefinitionTypeTestedContainer = fhir4b_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainer;
  TFhirSpecimenDefinitionTypeTestedContainerList = fhir4b_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainerList;
  TFhirSpecimenDefinitionTypeTestedContainerAdditive = fhir4b_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainerAdditive;
  TFhirSpecimenDefinitionTypeTestedContainerAdditiveList = fhir4b_resources_canonical.TFhirSpecimenDefinitionTypeTestedContainerAdditiveList;
  TFhirSpecimenDefinitionTypeTestedHandling = fhir4b_resources_canonical.TFhirSpecimenDefinitionTypeTestedHandling;
  TFhirSpecimenDefinitionTypeTestedHandlingList = fhir4b_resources_canonical.TFhirSpecimenDefinitionTypeTestedHandlingList;
  TFhirSpecimenDefinition = fhir4b_resources_canonical.TFhirSpecimenDefinition;
  TFhirSpecimenDefinitionList = fhir4b_resources_canonical.TFhirSpecimenDefinitionList;
{$ENDIF FHIR_SPECIMENDEFINITION}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  TFhirStructureDefinitionMapping = fhir4b_resources_canonical.TFhirStructureDefinitionMapping;
  TFhirStructureDefinitionMappingList = fhir4b_resources_canonical.TFhirStructureDefinitionMappingList;
  TFhirStructureDefinitionContext = fhir4b_resources_canonical.TFhirStructureDefinitionContext;
  TFhirStructureDefinitionContextList = fhir4b_resources_canonical.TFhirStructureDefinitionContextList;
  TFhirStructureDefinitionSnapshot = fhir4b_resources_canonical.TFhirStructureDefinitionSnapshot;
  TFhirStructureDefinitionSnapshotList = fhir4b_resources_canonical.TFhirStructureDefinitionSnapshotList;
  TFhirStructureDefinitionDifferential = fhir4b_resources_canonical.TFhirStructureDefinitionDifferential;
  TFhirStructureDefinitionDifferentialList = fhir4b_resources_canonical.TFhirStructureDefinitionDifferentialList;
  TFhirStructureDefinition = fhir4b_resources_canonical.TFhirStructureDefinition;
  TFhirStructureDefinitionList = fhir4b_resources_canonical.TFhirStructureDefinitionList;
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_STRUCTUREMAP}
  TFhirStructureMapStructure = fhir4b_resources_canonical.TFhirStructureMapStructure;
  TFhirStructureMapStructureList = fhir4b_resources_canonical.TFhirStructureMapStructureList;
  TFhirStructureMapGroup = fhir4b_resources_canonical.TFhirStructureMapGroup;
  TFhirStructureMapGroupList = fhir4b_resources_canonical.TFhirStructureMapGroupList;
  TFhirStructureMapGroupInput = fhir4b_resources_canonical.TFhirStructureMapGroupInput;
  TFhirStructureMapGroupInputList = fhir4b_resources_canonical.TFhirStructureMapGroupInputList;
  TFhirStructureMapGroupRule = fhir4b_resources_canonical.TFhirStructureMapGroupRule;
  TFhirStructureMapGroupRuleList = fhir4b_resources_canonical.TFhirStructureMapGroupRuleList;
  TFhirStructureMapGroupRuleSource = fhir4b_resources_canonical.TFhirStructureMapGroupRuleSource;
  TFhirStructureMapGroupRuleSourceList = fhir4b_resources_canonical.TFhirStructureMapGroupRuleSourceList;
  TFhirStructureMapGroupRuleTarget = fhir4b_resources_canonical.TFhirStructureMapGroupRuleTarget;
  TFhirStructureMapGroupRuleTargetList = fhir4b_resources_canonical.TFhirStructureMapGroupRuleTargetList;
  TFhirStructureMapGroupRuleTargetParameter = fhir4b_resources_canonical.TFhirStructureMapGroupRuleTargetParameter;
  TFhirStructureMapGroupRuleTargetParameterList = fhir4b_resources_canonical.TFhirStructureMapGroupRuleTargetParameterList;
  TFhirStructureMapGroupRuleDependent = fhir4b_resources_canonical.TFhirStructureMapGroupRuleDependent;
  TFhirStructureMapGroupRuleDependentList = fhir4b_resources_canonical.TFhirStructureMapGroupRuleDependentList;
  TFhirStructureMap = fhir4b_resources_canonical.TFhirStructureMap;
  TFhirStructureMapList = fhir4b_resources_canonical.TFhirStructureMapList;
{$ENDIF FHIR_STRUCTUREMAP}
{$IFDEF FHIR_SUBSCRIPTION}
  TFhirSubscriptionChannel = fhir4b_resources_other.TFhirSubscriptionChannel;
  TFhirSubscriptionChannelList = fhir4b_resources_other.TFhirSubscriptionChannelList;
  TFhirSubscription = fhir4b_resources_other.TFhirSubscription;
  TFhirSubscriptionList = fhir4b_resources_other.TFhirSubscriptionList;
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
  TFhirSubscriptionStatusNotificationEvent = fhir4b_resources_other.TFhirSubscriptionStatusNotificationEvent;
  TFhirSubscriptionStatusNotificationEventList = fhir4b_resources_other.TFhirSubscriptionStatusNotificationEventList;
  TFhirSubscriptionStatus = fhir4b_resources_other.TFhirSubscriptionStatus;
  TFhirSubscriptionStatusList = fhir4b_resources_other.TFhirSubscriptionStatusList;
{$ENDIF FHIR_SUBSCRIPTIONSTATUS}
{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
  TFhirSubscriptionTopicResourceTrigger = fhir4b_resources_other.TFhirSubscriptionTopicResourceTrigger;
  TFhirSubscriptionTopicResourceTriggerList = fhir4b_resources_other.TFhirSubscriptionTopicResourceTriggerList;
  TFhirSubscriptionTopicResourceTriggerQueryCriteria = fhir4b_resources_other.TFhirSubscriptionTopicResourceTriggerQueryCriteria;
  TFhirSubscriptionTopicResourceTriggerQueryCriteriaList = fhir4b_resources_other.TFhirSubscriptionTopicResourceTriggerQueryCriteriaList;
  TFhirSubscriptionTopicEventTrigger = fhir4b_resources_other.TFhirSubscriptionTopicEventTrigger;
  TFhirSubscriptionTopicEventTriggerList = fhir4b_resources_other.TFhirSubscriptionTopicEventTriggerList;
  TFhirSubscriptionTopicCanFilterBy = fhir4b_resources_other.TFhirSubscriptionTopicCanFilterBy;
  TFhirSubscriptionTopicCanFilterByList = fhir4b_resources_other.TFhirSubscriptionTopicCanFilterByList;
  TFhirSubscriptionTopicNotificationShape = fhir4b_resources_other.TFhirSubscriptionTopicNotificationShape;
  TFhirSubscriptionTopicNotificationShapeList = fhir4b_resources_other.TFhirSubscriptionTopicNotificationShapeList;
  TFhirSubscriptionTopic = fhir4b_resources_other.TFhirSubscriptionTopic;
  TFhirSubscriptionTopicList = fhir4b_resources_other.TFhirSubscriptionTopicList;
{$ENDIF FHIR_SUBSCRIPTIONTOPIC}
{$IFDEF FHIR_SUBSTANCE}
  TFhirSubstanceInstance = fhir4b_resources_medications.TFhirSubstanceInstance;
  TFhirSubstanceInstanceList = fhir4b_resources_medications.TFhirSubstanceInstanceList;
  TFhirSubstanceIngredient = fhir4b_resources_medications.TFhirSubstanceIngredient;
  TFhirSubstanceIngredientList = fhir4b_resources_medications.TFhirSubstanceIngredientList;
  TFhirSubstance = fhir4b_resources_medications.TFhirSubstance;
  TFhirSubstanceList = fhir4b_resources_medications.TFhirSubstanceList;
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUBSTANCEDEFINITION}
  TFhirSubstanceDefinitionMoiety = fhir4b_resources_medications.TFhirSubstanceDefinitionMoiety;
  TFhirSubstanceDefinitionMoietyList = fhir4b_resources_medications.TFhirSubstanceDefinitionMoietyList;
  TFhirSubstanceDefinitionProperty = fhir4b_resources_medications.TFhirSubstanceDefinitionProperty;
  TFhirSubstanceDefinitionPropertyList = fhir4b_resources_medications.TFhirSubstanceDefinitionPropertyList;
  TFhirSubstanceDefinitionMolecularWeight = fhir4b_resources_medications.TFhirSubstanceDefinitionMolecularWeight;
  TFhirSubstanceDefinitionMolecularWeightList = fhir4b_resources_medications.TFhirSubstanceDefinitionMolecularWeightList;
  TFhirSubstanceDefinitionStructure = fhir4b_resources_medications.TFhirSubstanceDefinitionStructure;
  TFhirSubstanceDefinitionStructureList = fhir4b_resources_medications.TFhirSubstanceDefinitionStructureList;
  TFhirSubstanceDefinitionStructureRepresentation = fhir4b_resources_medications.TFhirSubstanceDefinitionStructureRepresentation;
  TFhirSubstanceDefinitionStructureRepresentationList = fhir4b_resources_medications.TFhirSubstanceDefinitionStructureRepresentationList;
  TFhirSubstanceDefinitionCode = fhir4b_resources_medications.TFhirSubstanceDefinitionCode;
  TFhirSubstanceDefinitionCodeList = fhir4b_resources_medications.TFhirSubstanceDefinitionCodeList;
  TFhirSubstanceDefinitionName = fhir4b_resources_medications.TFhirSubstanceDefinitionName;
  TFhirSubstanceDefinitionNameList = fhir4b_resources_medications.TFhirSubstanceDefinitionNameList;
  TFhirSubstanceDefinitionNameOfficial = fhir4b_resources_medications.TFhirSubstanceDefinitionNameOfficial;
  TFhirSubstanceDefinitionNameOfficialList = fhir4b_resources_medications.TFhirSubstanceDefinitionNameOfficialList;
  TFhirSubstanceDefinitionRelationship = fhir4b_resources_medications.TFhirSubstanceDefinitionRelationship;
  TFhirSubstanceDefinitionRelationshipList = fhir4b_resources_medications.TFhirSubstanceDefinitionRelationshipList;
  TFhirSubstanceDefinitionSourceMaterial = fhir4b_resources_medications.TFhirSubstanceDefinitionSourceMaterial;
  TFhirSubstanceDefinitionSourceMaterialList = fhir4b_resources_medications.TFhirSubstanceDefinitionSourceMaterialList;
  TFhirSubstanceDefinition = fhir4b_resources_medications.TFhirSubstanceDefinition;
  TFhirSubstanceDefinitionList = fhir4b_resources_medications.TFhirSubstanceDefinitionList;
{$ENDIF FHIR_SUBSTANCEDEFINITION}
{$IFDEF FHIR_SUPPLYDELIVERY}
  TFhirSupplyDeliverySuppliedItem = fhir4b_resources_clinical.TFhirSupplyDeliverySuppliedItem;
  TFhirSupplyDeliverySuppliedItemList = fhir4b_resources_clinical.TFhirSupplyDeliverySuppliedItemList;
  TFhirSupplyDelivery = fhir4b_resources_clinical.TFhirSupplyDelivery;
  TFhirSupplyDeliveryList = fhir4b_resources_clinical.TFhirSupplyDeliveryList;
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  TFhirSupplyRequestParameter = fhir4b_resources_clinical.TFhirSupplyRequestParameter;
  TFhirSupplyRequestParameterList = fhir4b_resources_clinical.TFhirSupplyRequestParameterList;
  TFhirSupplyRequest = fhir4b_resources_clinical.TFhirSupplyRequest;
  TFhirSupplyRequestList = fhir4b_resources_clinical.TFhirSupplyRequestList;
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_TASK}
  TFhirTaskRestriction = fhir4b_resources_other.TFhirTaskRestriction;
  TFhirTaskRestrictionList = fhir4b_resources_other.TFhirTaskRestrictionList;
  TFhirTaskInput = fhir4b_resources_other.TFhirTaskInput;
  TFhirTaskInputList = fhir4b_resources_other.TFhirTaskInputList;
  TFhirTaskOutput = fhir4b_resources_other.TFhirTaskOutput;
  TFhirTaskOutputList = fhir4b_resources_other.TFhirTaskOutputList;
  TFhirTask = fhir4b_resources_other.TFhirTask;
  TFhirTaskList = fhir4b_resources_other.TFhirTaskList;
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  TFhirTerminologyCapabilitiesSoftware = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesSoftware;
  TFhirTerminologyCapabilitiesSoftwareList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesSoftwareList;
  TFhirTerminologyCapabilitiesImplementation = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesImplementation;
  TFhirTerminologyCapabilitiesImplementationList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesImplementationList;
  TFhirTerminologyCapabilitiesCodeSystem = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesCodeSystem;
  TFhirTerminologyCapabilitiesCodeSystemList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemList;
  TFhirTerminologyCapabilitiesCodeSystemVersion = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersion;
  TFhirTerminologyCapabilitiesCodeSystemVersionList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersionList;
  TFhirTerminologyCapabilitiesCodeSystemVersionFilter = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersionFilter;
  TFhirTerminologyCapabilitiesCodeSystemVersionFilterList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersionFilterList;
  TFhirTerminologyCapabilitiesExpansion = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesExpansion;
  TFhirTerminologyCapabilitiesExpansionList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesExpansionList;
  TFhirTerminologyCapabilitiesExpansionParameter = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesExpansionParameter;
  TFhirTerminologyCapabilitiesExpansionParameterList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesExpansionParameterList;
  TFhirTerminologyCapabilitiesValidateCode = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesValidateCode;
  TFhirTerminologyCapabilitiesValidateCodeList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesValidateCodeList;
  TFhirTerminologyCapabilitiesTranslation = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesTranslation;
  TFhirTerminologyCapabilitiesTranslationList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesTranslationList;
  TFhirTerminologyCapabilitiesClosure = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesClosure;
  TFhirTerminologyCapabilitiesClosureList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesClosureList;
  TFhirTerminologyCapabilities = fhir4b_resources_canonical.TFhirTerminologyCapabilities;
  TFhirTerminologyCapabilitiesList = fhir4b_resources_canonical.TFhirTerminologyCapabilitiesList;
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}
{$IFDEF FHIR_TESTREPORT}
  TFhirTestReportParticipant = fhir4b_resources_other.TFhirTestReportParticipant;
  TFhirTestReportParticipantList = fhir4b_resources_other.TFhirTestReportParticipantList;
  TFhirTestReportSetup = fhir4b_resources_other.TFhirTestReportSetup;
  TFhirTestReportSetupList = fhir4b_resources_other.TFhirTestReportSetupList;
  TFhirTestReportSetupAction = fhir4b_resources_other.TFhirTestReportSetupAction;
  TFhirTestReportSetupActionList = fhir4b_resources_other.TFhirTestReportSetupActionList;
  TFhirTestReportSetupActionOperation = fhir4b_resources_other.TFhirTestReportSetupActionOperation;
  TFhirTestReportSetupActionOperationList = fhir4b_resources_other.TFhirTestReportSetupActionOperationList;
  TFhirTestReportSetupActionAssert = fhir4b_resources_other.TFhirTestReportSetupActionAssert;
  TFhirTestReportSetupActionAssertList = fhir4b_resources_other.TFhirTestReportSetupActionAssertList;
  TFhirTestReportTest = fhir4b_resources_other.TFhirTestReportTest;
  TFhirTestReportTestList = fhir4b_resources_other.TFhirTestReportTestList;
  TFhirTestReportTestAction = fhir4b_resources_other.TFhirTestReportTestAction;
  TFhirTestReportTestActionList = fhir4b_resources_other.TFhirTestReportTestActionList;
  TFhirTestReportTeardown = fhir4b_resources_other.TFhirTestReportTeardown;
  TFhirTestReportTeardownList = fhir4b_resources_other.TFhirTestReportTeardownList;
  TFhirTestReportTeardownAction = fhir4b_resources_other.TFhirTestReportTeardownAction;
  TFhirTestReportTeardownActionList = fhir4b_resources_other.TFhirTestReportTeardownActionList;
  TFhirTestReport = fhir4b_resources_other.TFhirTestReport;
  TFhirTestReportList = fhir4b_resources_other.TFhirTestReportList;
{$ENDIF FHIR_TESTREPORT}
{$IFDEF FHIR_TESTSCRIPT}
  TFhirTestScriptOrigin = fhir4b_resources_canonical.TFhirTestScriptOrigin;
  TFhirTestScriptOriginList = fhir4b_resources_canonical.TFhirTestScriptOriginList;
  TFhirTestScriptDestination = fhir4b_resources_canonical.TFhirTestScriptDestination;
  TFhirTestScriptDestinationList = fhir4b_resources_canonical.TFhirTestScriptDestinationList;
  TFhirTestScriptMetadata = fhir4b_resources_canonical.TFhirTestScriptMetadata;
  TFhirTestScriptMetadataList = fhir4b_resources_canonical.TFhirTestScriptMetadataList;
  TFhirTestScriptMetadataLink = fhir4b_resources_canonical.TFhirTestScriptMetadataLink;
  TFhirTestScriptMetadataLinkList = fhir4b_resources_canonical.TFhirTestScriptMetadataLinkList;
  TFhirTestScriptMetadataCapability = fhir4b_resources_canonical.TFhirTestScriptMetadataCapability;
  TFhirTestScriptMetadataCapabilityList = fhir4b_resources_canonical.TFhirTestScriptMetadataCapabilityList;
  TFhirTestScriptFixture = fhir4b_resources_canonical.TFhirTestScriptFixture;
  TFhirTestScriptFixtureList = fhir4b_resources_canonical.TFhirTestScriptFixtureList;
  TFhirTestScriptVariable = fhir4b_resources_canonical.TFhirTestScriptVariable;
  TFhirTestScriptVariableList = fhir4b_resources_canonical.TFhirTestScriptVariableList;
  TFhirTestScriptSetup = fhir4b_resources_canonical.TFhirTestScriptSetup;
  TFhirTestScriptSetupList = fhir4b_resources_canonical.TFhirTestScriptSetupList;
  TFhirTestScriptSetupAction = fhir4b_resources_canonical.TFhirTestScriptSetupAction;
  TFhirTestScriptSetupActionList = fhir4b_resources_canonical.TFhirTestScriptSetupActionList;
  TFhirTestScriptSetupActionOperation = fhir4b_resources_canonical.TFhirTestScriptSetupActionOperation;
  TFhirTestScriptSetupActionOperationList = fhir4b_resources_canonical.TFhirTestScriptSetupActionOperationList;
  TFhirTestScriptSetupActionOperationRequestHeader = fhir4b_resources_canonical.TFhirTestScriptSetupActionOperationRequestHeader;
  TFhirTestScriptSetupActionOperationRequestHeaderList = fhir4b_resources_canonical.TFhirTestScriptSetupActionOperationRequestHeaderList;
  TFhirTestScriptSetupActionAssert = fhir4b_resources_canonical.TFhirTestScriptSetupActionAssert;
  TFhirTestScriptSetupActionAssertList = fhir4b_resources_canonical.TFhirTestScriptSetupActionAssertList;
  TFhirTestScriptTest = fhir4b_resources_canonical.TFhirTestScriptTest;
  TFhirTestScriptTestList = fhir4b_resources_canonical.TFhirTestScriptTestList;
  TFhirTestScriptTestAction = fhir4b_resources_canonical.TFhirTestScriptTestAction;
  TFhirTestScriptTestActionList = fhir4b_resources_canonical.TFhirTestScriptTestActionList;
  TFhirTestScriptTeardown = fhir4b_resources_canonical.TFhirTestScriptTeardown;
  TFhirTestScriptTeardownList = fhir4b_resources_canonical.TFhirTestScriptTeardownList;
  TFhirTestScriptTeardownAction = fhir4b_resources_canonical.TFhirTestScriptTeardownAction;
  TFhirTestScriptTeardownActionList = fhir4b_resources_canonical.TFhirTestScriptTeardownActionList;
  TFhirTestScript = fhir4b_resources_canonical.TFhirTestScript;
  TFhirTestScriptList = fhir4b_resources_canonical.TFhirTestScriptList;
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  TFhirValueSetCompose = fhir4b_resources_canonical.TFhirValueSetCompose;
  TFhirValueSetComposeList = fhir4b_resources_canonical.TFhirValueSetComposeList;
  TFhirValueSetComposeInclude = fhir4b_resources_canonical.TFhirValueSetComposeInclude;
  TFhirValueSetComposeIncludeList = fhir4b_resources_canonical.TFhirValueSetComposeIncludeList;
  TFhirValueSetComposeIncludeConcept = fhir4b_resources_canonical.TFhirValueSetComposeIncludeConcept;
  TFhirValueSetComposeIncludeConceptList = fhir4b_resources_canonical.TFhirValueSetComposeIncludeConceptList;
  TFhirValueSetComposeIncludeConceptDesignation = fhir4b_resources_canonical.TFhirValueSetComposeIncludeConceptDesignation;
  TFhirValueSetComposeIncludeConceptDesignationList = fhir4b_resources_canonical.TFhirValueSetComposeIncludeConceptDesignationList;
  TFhirValueSetComposeIncludeFilter = fhir4b_resources_canonical.TFhirValueSetComposeIncludeFilter;
  TFhirValueSetComposeIncludeFilterList = fhir4b_resources_canonical.TFhirValueSetComposeIncludeFilterList;
  TFhirValueSetExpansion = fhir4b_resources_canonical.TFhirValueSetExpansion;
  TFhirValueSetExpansionList = fhir4b_resources_canonical.TFhirValueSetExpansionList;
  TFhirValueSetExpansionParameter = fhir4b_resources_canonical.TFhirValueSetExpansionParameter;
  TFhirValueSetExpansionParameterList = fhir4b_resources_canonical.TFhirValueSetExpansionParameterList;
  TFhirValueSetExpansionContains = fhir4b_resources_canonical.TFhirValueSetExpansionContains;
  TFhirValueSetExpansionContainsList = fhir4b_resources_canonical.TFhirValueSetExpansionContainsList;
  TFhirValueSet = fhir4b_resources_canonical.TFhirValueSet;
  TFhirValueSetList = fhir4b_resources_canonical.TFhirValueSetList;
{$ENDIF FHIR_VALUESET}
{$IFDEF FHIR_VERIFICATIONRESULT}
  TFhirVerificationResultPrimarySource = fhir4b_resources_other.TFhirVerificationResultPrimarySource;
  TFhirVerificationResultPrimarySourceList = fhir4b_resources_other.TFhirVerificationResultPrimarySourceList;
  TFhirVerificationResultAttestation = fhir4b_resources_other.TFhirVerificationResultAttestation;
  TFhirVerificationResultAttestationList = fhir4b_resources_other.TFhirVerificationResultAttestationList;
  TFhirVerificationResultValidator = fhir4b_resources_other.TFhirVerificationResultValidator;
  TFhirVerificationResultValidatorList = fhir4b_resources_other.TFhirVerificationResultValidatorList;
  TFhirVerificationResult = fhir4b_resources_other.TFhirVerificationResult;
  TFhirVerificationResultList = fhir4b_resources_other.TFhirVerificationResultList;
{$ENDIF FHIR_VERIFICATIONRESULT}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  TFhirVisionPrescriptionLensSpecification = fhir4b_resources_clinical.TFhirVisionPrescriptionLensSpecification;
  TFhirVisionPrescriptionLensSpecificationList = fhir4b_resources_clinical.TFhirVisionPrescriptionLensSpecificationList;
  TFhirVisionPrescriptionLensSpecificationPrism = fhir4b_resources_clinical.TFhirVisionPrescriptionLensSpecificationPrism;
  TFhirVisionPrescriptionLensSpecificationPrismList = fhir4b_resources_clinical.TFhirVisionPrescriptionLensSpecificationPrismList;
  TFhirVisionPrescription = fhir4b_resources_clinical.TFhirVisionPrescription;
  TFhirVisionPrescriptionList = fhir4b_resources_clinical.TFhirVisionPrescriptionList;
{$ENDIF FHIR_VISIONPRESCRIPTION}



implementation

end.

