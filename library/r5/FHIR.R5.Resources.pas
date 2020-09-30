unit FHIR.R5.Resources;

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
{$I fhir.r5.inc}

interface

// Generated on Fri, Aug 21, 2020 11:27+1000 for FHIR v4.5.0

uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Utilities, FHIR.Base.Lang,
  FHIR.R5.Base, FHIR.R5.Enums, FHIR.R5.Types, FHIR.R5.Resources.Base,
  FHIR.R5.Resources.Admin, FHIR.R5.Resources.Canonical, FHIR.R5.Resources.Clinical, FHIR.R5.Resources.Financial, FHIR.R5.Resources.Medications, FHIR.R5.Resources.Other;

type
  TFhirResourceType = FHIR.R5.Resources.Base.TFhirResourceType;
  TFhirResourceTypeSet = FHIR.R5.Resources.Base.TFhirResourceTypeSet;
  TFhirResource = FHIR.R5.Resources.Base.TFhirResource;
  TFhirResourceClass = FHIR.R5.Resources.Base.TFhirResourceClass;
  TFhirResourceList = FHIR.R5.Resources.Base.TFhirResourceList;
  TFhirDomainResource = FHIR.R5.Resources.Base.TFhirDomainResource;

  TFhirCanonicalResource = FHIR.R5.Resources.Canonical.TFhirCanonicalResource;
  TFhirMetadataResource = FHIR.R5.Resources.Canonical.TFhirMetadataResource;

{$IFDEF FHIR_CATALOGENTRY}
  TFhirCatalogEntryRelatedEntry = FHIR.R5.Resources.Admin.TFhirCatalogEntryRelatedEntry;
  TFhirCatalogEntryRelatedEntryList = FHIR.R5.Resources.Admin.TFhirCatalogEntryRelatedEntryList;
  TFhirCatalogEntry = FHIR.R5.Resources.Admin.TFhirCatalogEntry;
  TFhirCatalogEntryList = FHIR.R5.Resources.Admin.TFhirCatalogEntryList;
{$ENDIF FHIR_CATALOGENTRY}
{$IFDEF FHIR_DEVICE}
  TFhirDeviceUdiCarrier = FHIR.R5.Resources.Admin.TFhirDeviceUdiCarrier;
  TFhirDeviceUdiCarrierList = FHIR.R5.Resources.Admin.TFhirDeviceUdiCarrierList;
  TFhirDeviceDeviceName = FHIR.R5.Resources.Admin.TFhirDeviceDeviceName;
  TFhirDeviceDeviceNameList = FHIR.R5.Resources.Admin.TFhirDeviceDeviceNameList;
  TFhirDeviceSpecialization = FHIR.R5.Resources.Admin.TFhirDeviceSpecialization;
  TFhirDeviceSpecializationList = FHIR.R5.Resources.Admin.TFhirDeviceSpecializationList;
  TFhirDeviceVersion = FHIR.R5.Resources.Admin.TFhirDeviceVersion;
  TFhirDeviceVersionList = FHIR.R5.Resources.Admin.TFhirDeviceVersionList;
  TFhirDeviceProperty = FHIR.R5.Resources.Admin.TFhirDeviceProperty;
  TFhirDevicePropertyList = FHIR.R5.Resources.Admin.TFhirDevicePropertyList;
  TFhirDeviceOperationalStatus = FHIR.R5.Resources.Admin.TFhirDeviceOperationalStatus;
  TFhirDeviceOperationalStatusList = FHIR.R5.Resources.Admin.TFhirDeviceOperationalStatusList;
  TFhirDeviceAssociationStatus = FHIR.R5.Resources.Admin.TFhirDeviceAssociationStatus;
  TFhirDeviceAssociationStatusList = FHIR.R5.Resources.Admin.TFhirDeviceAssociationStatusList;
  TFhirDevice = FHIR.R5.Resources.Admin.TFhirDevice;
  TFhirDeviceList = FHIR.R5.Resources.Admin.TFhirDeviceList;
{$ENDIF FHIR_DEVICE}

{$IFDEF FHIR_DEVICEDEFINITION}
  TFhirDeviceDefinitionUdiDeviceIdentifier = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionUdiDeviceIdentifier;
  TFhirDeviceDefinitionUdiDeviceIdentifierList = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionUdiDeviceIdentifierList;
  TFhirDeviceDefinitionDeviceName = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionDeviceName;
  TFhirDeviceDefinitionDeviceNameList = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionDeviceNameList;
  TFhirDeviceDefinitionSpecialization = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionSpecialization;
  TFhirDeviceDefinitionSpecializationList = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionSpecializationList;
  TFhirDeviceDefinitionCapability = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionCapability;
  TFhirDeviceDefinitionCapabilityList = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionCapabilityList;
  TFhirDeviceDefinitionProperty = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionProperty;
  TFhirDeviceDefinitionPropertyList = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionPropertyList;
  TFhirDeviceDefinitionMaterial = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionMaterial;
  TFhirDeviceDefinitionMaterialList = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionMaterialList;
  TFhirDeviceDefinition = FHIR.R5.Resources.Admin.TFhirDeviceDefinition;
  TFhirDeviceDefinitionList = FHIR.R5.Resources.Admin.TFhirDeviceDefinitionList;
{$ENDIF FHIR_DEVICEDEFINITION}
{$IFDEF FHIR_DEVICEMETRIC}
  TFhirDeviceMetricCalibration = FHIR.R5.Resources.Admin.TFhirDeviceMetricCalibration;
  TFhirDeviceMetricCalibrationList = FHIR.R5.Resources.Admin.TFhirDeviceMetricCalibrationList;
  TFhirDeviceMetric = FHIR.R5.Resources.Admin.TFhirDeviceMetric;
  TFhirDeviceMetricList = FHIR.R5.Resources.Admin.TFhirDeviceMetricList;
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_ENCOUNTER}
  TFhirEncounterStatusHistory = FHIR.R5.Resources.Admin.TFhirEncounterStatusHistory;
  TFhirEncounterStatusHistoryList = FHIR.R5.Resources.Admin.TFhirEncounterStatusHistoryList;
  TFhirEncounterClassHistory = FHIR.R5.Resources.Admin.TFhirEncounterClassHistory;
  TFhirEncounterClassHistoryList = FHIR.R5.Resources.Admin.TFhirEncounterClassHistoryList;
  TFhirEncounterParticipant = FHIR.R5.Resources.Admin.TFhirEncounterParticipant;
  TFhirEncounterParticipantList = FHIR.R5.Resources.Admin.TFhirEncounterParticipantList;
  TFhirEncounterDiagnosis = FHIR.R5.Resources.Admin.TFhirEncounterDiagnosis;
  TFhirEncounterDiagnosisList = FHIR.R5.Resources.Admin.TFhirEncounterDiagnosisList;
  TFhirEncounterHospitalization = FHIR.R5.Resources.Admin.TFhirEncounterHospitalization;
  TFhirEncounterHospitalizationList = FHIR.R5.Resources.Admin.TFhirEncounterHospitalizationList;
  TFhirEncounterLocation = FHIR.R5.Resources.Admin.TFhirEncounterLocation;
  TFhirEncounterLocationList = FHIR.R5.Resources.Admin.TFhirEncounterLocationList;
  TFhirEncounter = FHIR.R5.Resources.Admin.TFhirEncounter;
  TFhirEncounterList = FHIR.R5.Resources.Admin.TFhirEncounterList;
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENDPOINT}
  TFhirEndpoint = FHIR.R5.Resources.Admin.TFhirEndpoint;
  TFhirEndpointList = FHIR.R5.Resources.Admin.TFhirEndpointList;
{$ENDIF FHIR_ENDPOINT}
{$IFDEF FHIR_GROUP}
  TFhirGroupCharacteristic = FHIR.R5.Resources.Admin.TFhirGroupCharacteristic;
  TFhirGroupCharacteristicList = FHIR.R5.Resources.Admin.TFhirGroupCharacteristicList;
  TFhirGroupMember = FHIR.R5.Resources.Admin.TFhirGroupMember;
  TFhirGroupMemberList = FHIR.R5.Resources.Admin.TFhirGroupMemberList;
  TFhirGroup = FHIR.R5.Resources.Admin.TFhirGroup;
  TFhirGroupList = FHIR.R5.Resources.Admin.TFhirGroupList;
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_HEALTHCARESERVICE}
  TFhirHealthcareServiceEligibility = FHIR.R5.Resources.Admin.TFhirHealthcareServiceEligibility;
  TFhirHealthcareServiceEligibilityList = FHIR.R5.Resources.Admin.TFhirHealthcareServiceEligibilityList;
  TFhirHealthcareServiceAvailableTime = FHIR.R5.Resources.Admin.TFhirHealthcareServiceAvailableTime;
  TFhirHealthcareServiceAvailableTimeList = FHIR.R5.Resources.Admin.TFhirHealthcareServiceAvailableTimeList;
  TFhirHealthcareServiceNotAvailable = FHIR.R5.Resources.Admin.TFhirHealthcareServiceNotAvailable;
  TFhirHealthcareServiceNotAvailableList = FHIR.R5.Resources.Admin.TFhirHealthcareServiceNotAvailableList;
  TFhirHealthcareService = FHIR.R5.Resources.Admin.TFhirHealthcareService;
  TFhirHealthcareServiceList = FHIR.R5.Resources.Admin.TFhirHealthcareServiceList;
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_LOCATION}
  TFhirLocationPosition = FHIR.R5.Resources.Admin.TFhirLocationPosition;
  TFhirLocationPositionList = FHIR.R5.Resources.Admin.TFhirLocationPositionList;
  TFhirLocationHoursOfOperation = FHIR.R5.Resources.Admin.TFhirLocationHoursOfOperation;
  TFhirLocationHoursOfOperationList = FHIR.R5.Resources.Admin.TFhirLocationHoursOfOperationList;
  TFhirLocation = FHIR.R5.Resources.Admin.TFhirLocation;
  TFhirLocationList = FHIR.R5.Resources.Admin.TFhirLocationList;
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_ORGANIZATION}
  TFhirOrganizationContact = FHIR.R5.Resources.Admin.TFhirOrganizationContact;
  TFhirOrganizationContactList = FHIR.R5.Resources.Admin.TFhirOrganizationContactList;
  TFhirOrganization = FHIR.R5.Resources.Admin.TFhirOrganization;
  TFhirOrganizationList = FHIR.R5.Resources.Admin.TFhirOrganizationList;
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  TFhirOrganizationAffiliation = FHIR.R5.Resources.Admin.TFhirOrganizationAffiliation;
  TFhirOrganizationAffiliationList = FHIR.R5.Resources.Admin.TFhirOrganizationAffiliationList;
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}
{$IFDEF FHIR_PATIENT}
  TFhirPatientContact = FHIR.R5.Resources.Admin.TFhirPatientContact;
  TFhirPatientContactList = FHIR.R5.Resources.Admin.TFhirPatientContactList;
  TFhirPatientCommunication = FHIR.R5.Resources.Admin.TFhirPatientCommunication;
  TFhirPatientCommunicationList = FHIR.R5.Resources.Admin.TFhirPatientCommunicationList;
  TFhirPatientLink = FHIR.R5.Resources.Admin.TFhirPatientLink;
  TFhirPatientLinkList = FHIR.R5.Resources.Admin.TFhirPatientLinkList;
  TFhirPatient = FHIR.R5.Resources.Admin.TFhirPatient;
  TFhirPatientList = FHIR.R5.Resources.Admin.TFhirPatientList;
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PERSON}
  TFhirPersonLink = FHIR.R5.Resources.Admin.TFhirPersonLink;
  TFhirPersonLinkList = FHIR.R5.Resources.Admin.TFhirPersonLinkList;
  TFhirPerson = FHIR.R5.Resources.Admin.TFhirPerson;
  TFhirPersonList = FHIR.R5.Resources.Admin.TFhirPersonList;
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PRACTITIONER}
  TFhirPractitionerQualification = FHIR.R5.Resources.Admin.TFhirPractitionerQualification;
  TFhirPractitionerQualificationList = FHIR.R5.Resources.Admin.TFhirPractitionerQualificationList;
  TFhirPractitioner = FHIR.R5.Resources.Admin.TFhirPractitioner;
  TFhirPractitionerList = FHIR.R5.Resources.Admin.TFhirPractitionerList;
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PRACTITIONERROLE}
  TFhirPractitionerRoleAvailableTime = FHIR.R5.Resources.Admin.TFhirPractitionerRoleAvailableTime;
  TFhirPractitionerRoleAvailableTimeList = FHIR.R5.Resources.Admin.TFhirPractitionerRoleAvailableTimeList;
  TFhirPractitionerRoleNotAvailable = FHIR.R5.Resources.Admin.TFhirPractitionerRoleNotAvailable;
  TFhirPractitionerRoleNotAvailableList = FHIR.R5.Resources.Admin.TFhirPractitionerRoleNotAvailableList;
  TFhirPractitionerRole = FHIR.R5.Resources.Admin.TFhirPractitionerRole;
  TFhirPractitionerRoleList = FHIR.R5.Resources.Admin.TFhirPractitionerRoleList;
{$ENDIF FHIR_PRACTITIONERROLE}
{$IFDEF FHIR_RELATEDPERSON}
  TFhirRelatedPersonCommunication = FHIR.R5.Resources.Admin.TFhirRelatedPersonCommunication;
  TFhirRelatedPersonCommunicationList = FHIR.R5.Resources.Admin.TFhirRelatedPersonCommunicationList;
  TFhirRelatedPerson = FHIR.R5.Resources.Admin.TFhirRelatedPerson;
  TFhirRelatedPersonList = FHIR.R5.Resources.Admin.TFhirRelatedPersonList;
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_SCHEDULE}
  TFhirSchedule = FHIR.R5.Resources.Admin.TFhirSchedule;
  TFhirScheduleList = FHIR.R5.Resources.Admin.TFhirScheduleList;
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SLOT}
  TFhirSlot = FHIR.R5.Resources.Admin.TFhirSlot;
  TFhirSlotList = FHIR.R5.Resources.Admin.TFhirSlotList;
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_ADVERSEEVENT}
  TFhirAdverseEventParticipant = FHIR.R5.Resources.Clinical.TFhirAdverseEventParticipant;
  TFhirAdverseEventParticipantList = FHIR.R5.Resources.Clinical.TFhirAdverseEventParticipantList;
  TFhirAdverseEventSuspectEntity = FHIR.R5.Resources.Clinical.TFhirAdverseEventSuspectEntity;
  TFhirAdverseEventSuspectEntityList = FHIR.R5.Resources.Clinical.TFhirAdverseEventSuspectEntityList;
  TFhirAdverseEventSuspectEntityCausality = FHIR.R5.Resources.Clinical.TFhirAdverseEventSuspectEntityCausality;
  TFhirAdverseEventSuspectEntityCausalityList = FHIR.R5.Resources.Clinical.TFhirAdverseEventSuspectEntityCausalityList;
  TFhirAdverseEventContributingFactor = FHIR.R5.Resources.Clinical.TFhirAdverseEventContributingFactor;
  TFhirAdverseEventContributingFactorList = FHIR.R5.Resources.Clinical.TFhirAdverseEventContributingFactorList;
  TFhirAdverseEventPreventiveAction = FHIR.R5.Resources.Clinical.TFhirAdverseEventPreventiveAction;
  TFhirAdverseEventPreventiveActionList = FHIR.R5.Resources.Clinical.TFhirAdverseEventPreventiveActionList;
  TFhirAdverseEventMitigatingAction = FHIR.R5.Resources.Clinical.TFhirAdverseEventMitigatingAction;
  TFhirAdverseEventMitigatingActionList = FHIR.R5.Resources.Clinical.TFhirAdverseEventMitigatingActionList;
  TFhirAdverseEventSupportingInfo = FHIR.R5.Resources.Clinical.TFhirAdverseEventSupportingInfo;
  TFhirAdverseEventSupportingInfoList = FHIR.R5.Resources.Clinical.TFhirAdverseEventSupportingInfoList;
  TFhirAdverseEvent = FHIR.R5.Resources.Clinical.TFhirAdverseEvent;
  TFhirAdverseEventList = FHIR.R5.Resources.Clinical.TFhirAdverseEventList;
{$ENDIF FHIR_ADVERSEEVENT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  TFhirAllergyIntoleranceReaction = FHIR.R5.Resources.Clinical.TFhirAllergyIntoleranceReaction;
  TFhirAllergyIntoleranceReactionList = FHIR.R5.Resources.Clinical.TFhirAllergyIntoleranceReactionList;
  TFhirAllergyIntolerance = FHIR.R5.Resources.Clinical.TFhirAllergyIntolerance;
  TFhirAllergyIntoleranceList = FHIR.R5.Resources.Clinical.TFhirAllergyIntoleranceList;
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  TFhirAppointmentParticipant = FHIR.R5.Resources.Clinical.TFhirAppointmentParticipant;
  TFhirAppointmentParticipantList = FHIR.R5.Resources.Clinical.TFhirAppointmentParticipantList;
  TFhirAppointment = FHIR.R5.Resources.Clinical.TFhirAppointment;
  TFhirAppointmentList = FHIR.R5.Resources.Clinical.TFhirAppointmentList;
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  TFhirAppointmentResponse = FHIR.R5.Resources.Clinical.TFhirAppointmentResponse;
  TFhirAppointmentResponseList = FHIR.R5.Resources.Clinical.TFhirAppointmentResponseList;
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_BASIC}
  TFhirBasic = FHIR.R5.Resources.Clinical.TFhirBasic;
  TFhirBasicList = FHIR.R5.Resources.Clinical.TFhirBasicList;
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  TFhirBiologicallyDerivedProductCollection = FHIR.R5.Resources.Clinical.TFhirBiologicallyDerivedProductCollection;
  TFhirBiologicallyDerivedProductCollectionList = FHIR.R5.Resources.Clinical.TFhirBiologicallyDerivedProductCollectionList;
  TFhirBiologicallyDerivedProductProcessing = FHIR.R5.Resources.Clinical.TFhirBiologicallyDerivedProductProcessing;
  TFhirBiologicallyDerivedProductProcessingList = FHIR.R5.Resources.Clinical.TFhirBiologicallyDerivedProductProcessingList;
  TFhirBiologicallyDerivedProductManipulation = FHIR.R5.Resources.Clinical.TFhirBiologicallyDerivedProductManipulation;
  TFhirBiologicallyDerivedProductManipulationList = FHIR.R5.Resources.Clinical.TFhirBiologicallyDerivedProductManipulationList;
  TFhirBiologicallyDerivedProductStorage = FHIR.R5.Resources.Clinical.TFhirBiologicallyDerivedProductStorage;
  TFhirBiologicallyDerivedProductStorageList = FHIR.R5.Resources.Clinical.TFhirBiologicallyDerivedProductStorageList;
  TFhirBiologicallyDerivedProduct = FHIR.R5.Resources.Clinical.TFhirBiologicallyDerivedProduct;
  TFhirBiologicallyDerivedProductList = FHIR.R5.Resources.Clinical.TFhirBiologicallyDerivedProductList;
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
{$IFDEF FHIR_BODYSTRUCTURE}
  TFhirBodyStructure = FHIR.R5.Resources.Clinical.TFhirBodyStructure;
  TFhirBodyStructureList = FHIR.R5.Resources.Clinical.TFhirBodyStructureList;
{$ENDIF FHIR_BODYSTRUCTURE}
{$IFDEF FHIR_CAREPLAN}
  TFhirCarePlanActivity = FHIR.R5.Resources.Clinical.TFhirCarePlanActivity;
  TFhirCarePlanActivityList = FHIR.R5.Resources.Clinical.TFhirCarePlanActivityList;
  TFhirCarePlanActivityDetail = FHIR.R5.Resources.Clinical.TFhirCarePlanActivityDetail;
  TFhirCarePlanActivityDetailList = FHIR.R5.Resources.Clinical.TFhirCarePlanActivityDetailList;
  TFhirCarePlan = FHIR.R5.Resources.Clinical.TFhirCarePlan;
  TFhirCarePlanList = FHIR.R5.Resources.Clinical.TFhirCarePlanList;
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CARETEAM}
  TFhirCareTeamParticipant = FHIR.R5.Resources.Clinical.TFhirCareTeamParticipant;
  TFhirCareTeamParticipantList = FHIR.R5.Resources.Clinical.TFhirCareTeamParticipantList;
  TFhirCareTeam = FHIR.R5.Resources.Clinical.TFhirCareTeam;
  TFhirCareTeamList = FHIR.R5.Resources.Clinical.TFhirCareTeamList;
{$ENDIF FHIR_CARETEAM}
{$IFDEF FHIR_CLINICALIMPRESSION}
  TFhirClinicalImpressionFinding = FHIR.R5.Resources.Clinical.TFhirClinicalImpressionFinding;
  TFhirClinicalImpressionFindingList = FHIR.R5.Resources.Clinical.TFhirClinicalImpressionFindingList;
  TFhirClinicalImpression = FHIR.R5.Resources.Clinical.TFhirClinicalImpression;
  TFhirClinicalImpressionList = FHIR.R5.Resources.Clinical.TFhirClinicalImpressionList;
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_CLINICALUSEISSUE}
  TFhirClinicalUseIssueContraindication = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueContraindication;
  TFhirClinicalUseIssueContraindicationList = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueContraindicationList;
  TFhirClinicalUseIssueContraindicationOtherTherapy = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueContraindicationOtherTherapy;
  TFhirClinicalUseIssueContraindicationOtherTherapyList = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueContraindicationOtherTherapyList;
  TFhirClinicalUseIssueIndication = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueIndication;
  TFhirClinicalUseIssueIndicationList = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueIndicationList;
  TFhirClinicalUseIssueInteraction = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueInteraction;
  TFhirClinicalUseIssueInteractionList = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueInteractionList;
  TFhirClinicalUseIssueInteractionInteractant = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueInteractionInteractant;
  TFhirClinicalUseIssueInteractionInteractantList = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueInteractionInteractantList;
  TFhirClinicalUseIssueUndesirableEffect = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueUndesirableEffect;
  TFhirClinicalUseIssueUndesirableEffectList = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueUndesirableEffectList;
  TFhirClinicalUseIssue = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssue;
  TFhirClinicalUseIssueList = FHIR.R5.Resources.Clinical.TFhirClinicalUseIssueList;
{$ENDIF FHIR_CLINICALUSEISSUE}
{$IFDEF FHIR_COMMUNICATION}
  TFhirCommunicationPayload = FHIR.R5.Resources.Clinical.TFhirCommunicationPayload;
  TFhirCommunicationPayloadList = FHIR.R5.Resources.Clinical.TFhirCommunicationPayloadList;
  TFhirCommunication = FHIR.R5.Resources.Clinical.TFhirCommunication;
  TFhirCommunicationList = FHIR.R5.Resources.Clinical.TFhirCommunicationList;
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  TFhirCommunicationRequestPayload = FHIR.R5.Resources.Clinical.TFhirCommunicationRequestPayload;
  TFhirCommunicationRequestPayloadList = FHIR.R5.Resources.Clinical.TFhirCommunicationRequestPayloadList;
  TFhirCommunicationRequest = FHIR.R5.Resources.Clinical.TFhirCommunicationRequest;
  TFhirCommunicationRequestList = FHIR.R5.Resources.Clinical.TFhirCommunicationRequestList;
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPOSITION}
  TFhirCompositionAttester = FHIR.R5.Resources.Clinical.TFhirCompositionAttester;
  TFhirCompositionAttesterList = FHIR.R5.Resources.Clinical.TFhirCompositionAttesterList;
  TFhirCompositionRelatesTo = FHIR.R5.Resources.Clinical.TFhirCompositionRelatesTo;
  TFhirCompositionRelatesToList = FHIR.R5.Resources.Clinical.TFhirCompositionRelatesToList;
  TFhirCompositionEvent = FHIR.R5.Resources.Clinical.TFhirCompositionEvent;
  TFhirCompositionEventList = FHIR.R5.Resources.Clinical.TFhirCompositionEventList;
  TFhirCompositionSection = FHIR.R5.Resources.Clinical.TFhirCompositionSection;
  TFhirCompositionSectionList = FHIR.R5.Resources.Clinical.TFhirCompositionSectionList;
  TFhirComposition = FHIR.R5.Resources.Clinical.TFhirComposition;
  TFhirCompositionList = FHIR.R5.Resources.Clinical.TFhirCompositionList;
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONDITION}
  TFhirConditionStage = FHIR.R5.Resources.Clinical.TFhirConditionStage;
  TFhirConditionStageList = FHIR.R5.Resources.Clinical.TFhirConditionStageList;
  TFhirConditionEvidence = FHIR.R5.Resources.Clinical.TFhirConditionEvidence;
  TFhirConditionEvidenceList = FHIR.R5.Resources.Clinical.TFhirConditionEvidenceList;
  TFhirCondition = FHIR.R5.Resources.Clinical.TFhirCondition;
  TFhirConditionList = FHIR.R5.Resources.Clinical.TFhirConditionList;
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_DETECTEDISSUE}
  TFhirDetectedIssueEvidence = FHIR.R5.Resources.Clinical.TFhirDetectedIssueEvidence;
  TFhirDetectedIssueEvidenceList = FHIR.R5.Resources.Clinical.TFhirDetectedIssueEvidenceList;
  TFhirDetectedIssueMitigation = FHIR.R5.Resources.Clinical.TFhirDetectedIssueMitigation;
  TFhirDetectedIssueMitigationList = FHIR.R5.Resources.Clinical.TFhirDetectedIssueMitigationList;
  TFhirDetectedIssue = FHIR.R5.Resources.Clinical.TFhirDetectedIssue;
  TFhirDetectedIssueList = FHIR.R5.Resources.Clinical.TFhirDetectedIssueList;
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICEREQUEST}
  TFhirDeviceRequestParameter = FHIR.R5.Resources.Clinical.TFhirDeviceRequestParameter;
  TFhirDeviceRequestParameterList = FHIR.R5.Resources.Clinical.TFhirDeviceRequestParameterList;
  TFhirDeviceRequest = FHIR.R5.Resources.Clinical.TFhirDeviceRequest;
  TFhirDeviceRequestList = FHIR.R5.Resources.Clinical.TFhirDeviceRequestList;
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  TFhirDeviceUseStatement = FHIR.R5.Resources.Clinical.TFhirDeviceUseStatement;
  TFhirDeviceUseStatementList = FHIR.R5.Resources.Clinical.TFhirDeviceUseStatementList;
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  TFhirDiagnosticReportMedia = FHIR.R5.Resources.Clinical.TFhirDiagnosticReportMedia;
  TFhirDiagnosticReportMediaList = FHIR.R5.Resources.Clinical.TFhirDiagnosticReportMediaList;
  TFhirDiagnosticReport = FHIR.R5.Resources.Clinical.TFhirDiagnosticReport;
  TFhirDiagnosticReportList = FHIR.R5.Resources.Clinical.TFhirDiagnosticReportList;
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  TFhirDocumentManifestRelated = FHIR.R5.Resources.Clinical.TFhirDocumentManifestRelated;
  TFhirDocumentManifestRelatedList = FHIR.R5.Resources.Clinical.TFhirDocumentManifestRelatedList;
  TFhirDocumentManifest = FHIR.R5.Resources.Clinical.TFhirDocumentManifest;
  TFhirDocumentManifestList = FHIR.R5.Resources.Clinical.TFhirDocumentManifestList;
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  TFhirDocumentReferenceAttester = FHIR.R5.Resources.Clinical.TFhirDocumentReferenceAttester;
  TFhirDocumentReferenceAttesterList = FHIR.R5.Resources.Clinical.TFhirDocumentReferenceAttesterList;
  TFhirDocumentReferenceRelatesTo = FHIR.R5.Resources.Clinical.TFhirDocumentReferenceRelatesTo;
  TFhirDocumentReferenceRelatesToList = FHIR.R5.Resources.Clinical.TFhirDocumentReferenceRelatesToList;
  TFhirDocumentReferenceContent = FHIR.R5.Resources.Clinical.TFhirDocumentReferenceContent;
  TFhirDocumentReferenceContentList = FHIR.R5.Resources.Clinical.TFhirDocumentReferenceContentList;
  TFhirDocumentReference = FHIR.R5.Resources.Clinical.TFhirDocumentReference;
  TFhirDocumentReferenceList = FHIR.R5.Resources.Clinical.TFhirDocumentReferenceList;
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_EPISODEOFCARE}
  TFhirEpisodeOfCareStatusHistory = FHIR.R5.Resources.Clinical.TFhirEpisodeOfCareStatusHistory;
  TFhirEpisodeOfCareStatusHistoryList = FHIR.R5.Resources.Clinical.TFhirEpisodeOfCareStatusHistoryList;
  TFhirEpisodeOfCareDiagnosis = FHIR.R5.Resources.Clinical.TFhirEpisodeOfCareDiagnosis;
  TFhirEpisodeOfCareDiagnosisList = FHIR.R5.Resources.Clinical.TFhirEpisodeOfCareDiagnosisList;
  TFhirEpisodeOfCare = FHIR.R5.Resources.Clinical.TFhirEpisodeOfCare;
  TFhirEpisodeOfCareList = FHIR.R5.Resources.Clinical.TFhirEpisodeOfCareList;
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  TFhirFamilyMemberHistoryCondition = FHIR.R5.Resources.Clinical.TFhirFamilyMemberHistoryCondition;
  TFhirFamilyMemberHistoryConditionList = FHIR.R5.Resources.Clinical.TFhirFamilyMemberHistoryConditionList;
  TFhirFamilyMemberHistoryProcedure = FHIR.R5.Resources.Clinical.TFhirFamilyMemberHistoryProcedure;
  TFhirFamilyMemberHistoryProcedureList = FHIR.R5.Resources.Clinical.TFhirFamilyMemberHistoryProcedureList;
  TFhirFamilyMemberHistory = FHIR.R5.Resources.Clinical.TFhirFamilyMemberHistory;
  TFhirFamilyMemberHistoryList = FHIR.R5.Resources.Clinical.TFhirFamilyMemberHistoryList;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  TFhirFlag = FHIR.R5.Resources.Clinical.TFhirFlag;
  TFhirFlagList = FHIR.R5.Resources.Clinical.TFhirFlagList;
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  TFhirGoalTarget = FHIR.R5.Resources.Clinical.TFhirGoalTarget;
  TFhirGoalTargetList = FHIR.R5.Resources.Clinical.TFhirGoalTargetList;
  TFhirGoal = FHIR.R5.Resources.Clinical.TFhirGoal;
  TFhirGoalList = FHIR.R5.Resources.Clinical.TFhirGoalList;
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_IMAGINGSTUDY}
  TFhirImagingStudyProcedure = FHIR.R5.Resources.Clinical.TFhirImagingStudyProcedure;
  TFhirImagingStudyProcedureList = FHIR.R5.Resources.Clinical.TFhirImagingStudyProcedureList;
  TFhirImagingStudySeries = FHIR.R5.Resources.Clinical.TFhirImagingStudySeries;
  TFhirImagingStudySeriesList = FHIR.R5.Resources.Clinical.TFhirImagingStudySeriesList;
  TFhirImagingStudySeriesPerformer = FHIR.R5.Resources.Clinical.TFhirImagingStudySeriesPerformer;
  TFhirImagingStudySeriesPerformerList = FHIR.R5.Resources.Clinical.TFhirImagingStudySeriesPerformerList;
  TFhirImagingStudySeriesInstance = FHIR.R5.Resources.Clinical.TFhirImagingStudySeriesInstance;
  TFhirImagingStudySeriesInstanceList = FHIR.R5.Resources.Clinical.TFhirImagingStudySeriesInstanceList;
  TFhirImagingStudy = FHIR.R5.Resources.Clinical.TFhirImagingStudy;
  TFhirImagingStudyList = FHIR.R5.Resources.Clinical.TFhirImagingStudyList;
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  TFhirImmunizationPerformer = FHIR.R5.Resources.Clinical.TFhirImmunizationPerformer;
  TFhirImmunizationPerformerList = FHIR.R5.Resources.Clinical.TFhirImmunizationPerformerList;
  TFhirImmunizationEducation = FHIR.R5.Resources.Clinical.TFhirImmunizationEducation;
  TFhirImmunizationEducationList = FHIR.R5.Resources.Clinical.TFhirImmunizationEducationList;
  TFhirImmunizationReaction = FHIR.R5.Resources.Clinical.TFhirImmunizationReaction;
  TFhirImmunizationReactionList = FHIR.R5.Resources.Clinical.TFhirImmunizationReactionList;
  TFhirImmunizationProtocolApplied = FHIR.R5.Resources.Clinical.TFhirImmunizationProtocolApplied;
  TFhirImmunizationProtocolAppliedList = FHIR.R5.Resources.Clinical.TFhirImmunizationProtocolAppliedList;
  TFhirImmunization = FHIR.R5.Resources.Clinical.TFhirImmunization;
  TFhirImmunizationList = FHIR.R5.Resources.Clinical.TFhirImmunizationList;
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  TFhirImmunizationEvaluation = FHIR.R5.Resources.Clinical.TFhirImmunizationEvaluation;
  TFhirImmunizationEvaluationList = FHIR.R5.Resources.Clinical.TFhirImmunizationEvaluationList;
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  TFhirImmunizationRecommendationRecommendation = FHIR.R5.Resources.Clinical.TFhirImmunizationRecommendationRecommendation;
  TFhirImmunizationRecommendationRecommendationList = FHIR.R5.Resources.Clinical.TFhirImmunizationRecommendationRecommendationList;
  TFhirImmunizationRecommendationRecommendationDateCriterion = FHIR.R5.Resources.Clinical.TFhirImmunizationRecommendationRecommendationDateCriterion;
  TFhirImmunizationRecommendationRecommendationDateCriterionList = FHIR.R5.Resources.Clinical.TFhirImmunizationRecommendationRecommendationDateCriterionList;
  TFhirImmunizationRecommendation = FHIR.R5.Resources.Clinical.TFhirImmunizationRecommendation;
  TFhirImmunizationRecommendationList = FHIR.R5.Resources.Clinical.TFhirImmunizationRecommendationList;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  TFhirMedicationAdministrationPerformer = FHIR.R5.Resources.Clinical.TFhirMedicationAdministrationPerformer;
  TFhirMedicationAdministrationPerformerList = FHIR.R5.Resources.Clinical.TFhirMedicationAdministrationPerformerList;
  TFhirMedicationAdministrationDosage = FHIR.R5.Resources.Clinical.TFhirMedicationAdministrationDosage;
  TFhirMedicationAdministrationDosageList = FHIR.R5.Resources.Clinical.TFhirMedicationAdministrationDosageList;
  TFhirMedicationAdministration = FHIR.R5.Resources.Clinical.TFhirMedicationAdministration;
  TFhirMedicationAdministrationList = FHIR.R5.Resources.Clinical.TFhirMedicationAdministrationList;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  TFhirMedicationDispensePerformer = FHIR.R5.Resources.Clinical.TFhirMedicationDispensePerformer;
  TFhirMedicationDispensePerformerList = FHIR.R5.Resources.Clinical.TFhirMedicationDispensePerformerList;
  TFhirMedicationDispenseSubstitution = FHIR.R5.Resources.Clinical.TFhirMedicationDispenseSubstitution;
  TFhirMedicationDispenseSubstitutionList = FHIR.R5.Resources.Clinical.TFhirMedicationDispenseSubstitutionList;
  TFhirMedicationDispense = FHIR.R5.Resources.Clinical.TFhirMedicationDispense;
  TFhirMedicationDispenseList = FHIR.R5.Resources.Clinical.TFhirMedicationDispenseList;
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  TFhirMedicationRequestDispenseRequest = FHIR.R5.Resources.Clinical.TFhirMedicationRequestDispenseRequest;
  TFhirMedicationRequestDispenseRequestList = FHIR.R5.Resources.Clinical.TFhirMedicationRequestDispenseRequestList;
  TFhirMedicationRequestDispenseRequestInitialFill = FHIR.R5.Resources.Clinical.TFhirMedicationRequestDispenseRequestInitialFill;
  TFhirMedicationRequestDispenseRequestInitialFillList = FHIR.R5.Resources.Clinical.TFhirMedicationRequestDispenseRequestInitialFillList;
  TFhirMedicationRequestSubstitution = FHIR.R5.Resources.Clinical.TFhirMedicationRequestSubstitution;
  TFhirMedicationRequestSubstitutionList = FHIR.R5.Resources.Clinical.TFhirMedicationRequestSubstitutionList;
  TFhirMedicationRequest = FHIR.R5.Resources.Clinical.TFhirMedicationRequest;
  TFhirMedicationRequestList = FHIR.R5.Resources.Clinical.TFhirMedicationRequestList;
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONUSAGE}
  TFhirMedicationUsage = FHIR.R5.Resources.Clinical.TFhirMedicationUsage;
  TFhirMedicationUsageList = FHIR.R5.Resources.Clinical.TFhirMedicationUsageList;
{$ENDIF FHIR_MEDICATIONUSAGE}
{$IFDEF FHIR_MOLECULARSEQUENCE}
  TFhirMolecularSequenceReferenceSeq = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceReferenceSeq;
  TFhirMolecularSequenceReferenceSeqList = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceReferenceSeqList;
  TFhirMolecularSequenceVariant = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceVariant;
  TFhirMolecularSequenceVariantList = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceVariantList;
  TFhirMolecularSequenceQuality = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceQuality;
  TFhirMolecularSequenceQualityList = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceQualityList;
  TFhirMolecularSequenceQualityRoc = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceQualityRoc;
  TFhirMolecularSequenceQualityRocList = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceQualityRocList;
  TFhirMolecularSequenceRepository = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceRepository;
  TFhirMolecularSequenceRepositoryList = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceRepositoryList;
  TFhirMolecularSequenceStructureVariant = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceStructureVariant;
  TFhirMolecularSequenceStructureVariantList = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceStructureVariantList;
  TFhirMolecularSequenceStructureVariantOuter = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceStructureVariantOuter;
  TFhirMolecularSequenceStructureVariantOuterList = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceStructureVariantOuterList;
  TFhirMolecularSequenceStructureVariantInner = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceStructureVariantInner;
  TFhirMolecularSequenceStructureVariantInnerList = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceStructureVariantInnerList;
  TFhirMolecularSequence = FHIR.R5.Resources.Clinical.TFhirMolecularSequence;
  TFhirMolecularSequenceList = FHIR.R5.Resources.Clinical.TFhirMolecularSequenceList;
{$ENDIF FHIR_MOLECULARSEQUENCE}
{$IFDEF FHIR_NUTRITIONINTAKE}
  TFhirNutritionIntakeConsumedItem = FHIR.R5.Resources.Clinical.TFhirNutritionIntakeConsumedItem;
  TFhirNutritionIntakeConsumedItemList = FHIR.R5.Resources.Clinical.TFhirNutritionIntakeConsumedItemList;
  TFhirNutritionIntakeIngredientLabel = FHIR.R5.Resources.Clinical.TFhirNutritionIntakeIngredientLabel;
  TFhirNutritionIntakeIngredientLabelList = FHIR.R5.Resources.Clinical.TFhirNutritionIntakeIngredientLabelList;
  TFhirNutritionIntakePerformer = FHIR.R5.Resources.Clinical.TFhirNutritionIntakePerformer;
  TFhirNutritionIntakePerformerList = FHIR.R5.Resources.Clinical.TFhirNutritionIntakePerformerList;
  TFhirNutritionIntake = FHIR.R5.Resources.Clinical.TFhirNutritionIntake;
  TFhirNutritionIntakeList = FHIR.R5.Resources.Clinical.TFhirNutritionIntakeList;
{$ENDIF FHIR_NUTRITIONINTAKE}
{$IFDEF FHIR_NUTRITIONORDER}
  TFhirNutritionOrderOralDiet = FHIR.R5.Resources.Clinical.TFhirNutritionOrderOralDiet;
  TFhirNutritionOrderOralDietList = FHIR.R5.Resources.Clinical.TFhirNutritionOrderOralDietList;
  TFhirNutritionOrderOralDietNutrient = FHIR.R5.Resources.Clinical.TFhirNutritionOrderOralDietNutrient;
  TFhirNutritionOrderOralDietNutrientList = FHIR.R5.Resources.Clinical.TFhirNutritionOrderOralDietNutrientList;
  TFhirNutritionOrderOralDietTexture = FHIR.R5.Resources.Clinical.TFhirNutritionOrderOralDietTexture;
  TFhirNutritionOrderOralDietTextureList = FHIR.R5.Resources.Clinical.TFhirNutritionOrderOralDietTextureList;
  TFhirNutritionOrderSupplement = FHIR.R5.Resources.Clinical.TFhirNutritionOrderSupplement;
  TFhirNutritionOrderSupplementList = FHIR.R5.Resources.Clinical.TFhirNutritionOrderSupplementList;
  TFhirNutritionOrderEnteralFormula = FHIR.R5.Resources.Clinical.TFhirNutritionOrderEnteralFormula;
  TFhirNutritionOrderEnteralFormulaList = FHIR.R5.Resources.Clinical.TFhirNutritionOrderEnteralFormulaList;
  TFhirNutritionOrderEnteralFormulaAdministration = FHIR.R5.Resources.Clinical.TFhirNutritionOrderEnteralFormulaAdministration;
  TFhirNutritionOrderEnteralFormulaAdministrationList = FHIR.R5.Resources.Clinical.TFhirNutritionOrderEnteralFormulaAdministrationList;
  TFhirNutritionOrder = FHIR.R5.Resources.Clinical.TFhirNutritionOrder;
  TFhirNutritionOrderList = FHIR.R5.Resources.Clinical.TFhirNutritionOrderList;
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_OBSERVATION}
  TFhirObservationReferenceRange = FHIR.R5.Resources.Clinical.TFhirObservationReferenceRange;
  TFhirObservationReferenceRangeList = FHIR.R5.Resources.Clinical.TFhirObservationReferenceRangeList;
  TFhirObservationComponent = FHIR.R5.Resources.Clinical.TFhirObservationComponent;
  TFhirObservationComponentList = FHIR.R5.Resources.Clinical.TFhirObservationComponentList;
  TFhirObservation = FHIR.R5.Resources.Clinical.TFhirObservation;
  TFhirObservationList = FHIR.R5.Resources.Clinical.TFhirObservationList;
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_PROCEDURE}
  TFhirProcedurePerformer = FHIR.R5.Resources.Clinical.TFhirProcedurePerformer;
  TFhirProcedurePerformerList = FHIR.R5.Resources.Clinical.TFhirProcedurePerformerList;
  TFhirProcedureFocalDevice = FHIR.R5.Resources.Clinical.TFhirProcedureFocalDevice;
  TFhirProcedureFocalDeviceList = FHIR.R5.Resources.Clinical.TFhirProcedureFocalDeviceList;
  TFhirProcedure = FHIR.R5.Resources.Clinical.TFhirProcedure;
  TFhirProcedureList = FHIR.R5.Resources.Clinical.TFhirProcedureList;
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_RISKASSESSMENT}
  TFhirRiskAssessmentPrediction = FHIR.R5.Resources.Clinical.TFhirRiskAssessmentPrediction;
  TFhirRiskAssessmentPredictionList = FHIR.R5.Resources.Clinical.TFhirRiskAssessmentPredictionList;
  TFhirRiskAssessment = FHIR.R5.Resources.Clinical.TFhirRiskAssessment;
  TFhirRiskAssessmentList = FHIR.R5.Resources.Clinical.TFhirRiskAssessmentList;
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_SERVICEREQUEST}
  TFhirServiceRequest = FHIR.R5.Resources.Clinical.TFhirServiceRequest;
  TFhirServiceRequestList = FHIR.R5.Resources.Clinical.TFhirServiceRequestList;
{$ENDIF FHIR_SERVICEREQUEST}
{$IFDEF FHIR_SPECIMEN}
  TFhirSpecimenCollection = FHIR.R5.Resources.Clinical.TFhirSpecimenCollection;
  TFhirSpecimenCollectionList = FHIR.R5.Resources.Clinical.TFhirSpecimenCollectionList;
  TFhirSpecimenProcessing = FHIR.R5.Resources.Clinical.TFhirSpecimenProcessing;
  TFhirSpecimenProcessingList = FHIR.R5.Resources.Clinical.TFhirSpecimenProcessingList;
  TFhirSpecimenContainer = FHIR.R5.Resources.Clinical.TFhirSpecimenContainer;
  TFhirSpecimenContainerList = FHIR.R5.Resources.Clinical.TFhirSpecimenContainerList;
  TFhirSpecimen = FHIR.R5.Resources.Clinical.TFhirSpecimen;
  TFhirSpecimenList = FHIR.R5.Resources.Clinical.TFhirSpecimenList;
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_SUPPLYDELIVERY}
  TFhirSupplyDeliverySuppliedItem = FHIR.R5.Resources.Clinical.TFhirSupplyDeliverySuppliedItem;
  TFhirSupplyDeliverySuppliedItemList = FHIR.R5.Resources.Clinical.TFhirSupplyDeliverySuppliedItemList;
  TFhirSupplyDelivery = FHIR.R5.Resources.Clinical.TFhirSupplyDelivery;
  TFhirSupplyDeliveryList = FHIR.R5.Resources.Clinical.TFhirSupplyDeliveryList;
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  TFhirSupplyRequestParameter = FHIR.R5.Resources.Clinical.TFhirSupplyRequestParameter;
  TFhirSupplyRequestParameterList = FHIR.R5.Resources.Clinical.TFhirSupplyRequestParameterList;
  TFhirSupplyRequest = FHIR.R5.Resources.Clinical.TFhirSupplyRequest;
  TFhirSupplyRequestList = FHIR.R5.Resources.Clinical.TFhirSupplyRequestList;
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  TFhirVisionPrescriptionLensSpecification = FHIR.R5.Resources.Clinical.TFhirVisionPrescriptionLensSpecification;
  TFhirVisionPrescriptionLensSpecificationList = FHIR.R5.Resources.Clinical.TFhirVisionPrescriptionLensSpecificationList;
  TFhirVisionPrescriptionLensSpecificationPrism = FHIR.R5.Resources.Clinical.TFhirVisionPrescriptionLensSpecificationPrism;
  TFhirVisionPrescriptionLensSpecificationPrismList = FHIR.R5.Resources.Clinical.TFhirVisionPrescriptionLensSpecificationPrismList;
  TFhirVisionPrescription = FHIR.R5.Resources.Clinical.TFhirVisionPrescription;
  TFhirVisionPrescriptionList = FHIR.R5.Resources.Clinical.TFhirVisionPrescriptionList;
{$ENDIF FHIR_VISIONPRESCRIPTION}
{$IFDEF FHIR_ACCOUNT}
  TFhirAccountCoverage = FHIR.R5.Resources.Financial.TFhirAccountCoverage;
  TFhirAccountCoverageList = FHIR.R5.Resources.Financial.TFhirAccountCoverageList;
  TFhirAccountGuarantor = FHIR.R5.Resources.Financial.TFhirAccountGuarantor;
  TFhirAccountGuarantorList = FHIR.R5.Resources.Financial.TFhirAccountGuarantorList;
  TFhirAccount = FHIR.R5.Resources.Financial.TFhirAccount;
  TFhirAccountList = FHIR.R5.Resources.Financial.TFhirAccountList;
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_CHARGEITEM}
  TFhirChargeItemPerformer = FHIR.R5.Resources.Financial.TFhirChargeItemPerformer;
  TFhirChargeItemPerformerList = FHIR.R5.Resources.Financial.TFhirChargeItemPerformerList;
  TFhirChargeItem = FHIR.R5.Resources.Financial.TFhirChargeItem;
  TFhirChargeItemList = FHIR.R5.Resources.Financial.TFhirChargeItemList;
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CHARGEITEMDEFINITION}
  TFhirChargeItemDefinitionApplicability = FHIR.R5.Resources.Financial.TFhirChargeItemDefinitionApplicability;
  TFhirChargeItemDefinitionApplicabilityList = FHIR.R5.Resources.Financial.TFhirChargeItemDefinitionApplicabilityList;
  TFhirChargeItemDefinitionPropertyGroup = FHIR.R5.Resources.Financial.TFhirChargeItemDefinitionPropertyGroup;
  TFhirChargeItemDefinitionPropertyGroupList = FHIR.R5.Resources.Financial.TFhirChargeItemDefinitionPropertyGroupList;
  TFhirChargeItemDefinitionPropertyGroupPriceComponent = FHIR.R5.Resources.Financial.TFhirChargeItemDefinitionPropertyGroupPriceComponent;
  TFhirChargeItemDefinitionPropertyGroupPriceComponentList = FHIR.R5.Resources.Financial.TFhirChargeItemDefinitionPropertyGroupPriceComponentList;
  TFhirChargeItemDefinition = FHIR.R5.Resources.Financial.TFhirChargeItemDefinition;
  TFhirChargeItemDefinitionList = FHIR.R5.Resources.Financial.TFhirChargeItemDefinitionList;
{$ENDIF FHIR_CHARGEITEMDEFINITION}
{$IFDEF FHIR_CITATION}
  TFhirCitationSummary = FHIR.R5.Resources.Financial.TFhirCitationSummary;
  TFhirCitationSummaryList = FHIR.R5.Resources.Financial.TFhirCitationSummaryList;
  TFhirCitationVariantCitation = FHIR.R5.Resources.Financial.TFhirCitationVariantCitation;
  TFhirCitationVariantCitationList = FHIR.R5.Resources.Financial.TFhirCitationVariantCitationList;
  TFhirCitationJournal = FHIR.R5.Resources.Financial.TFhirCitationJournal;
  TFhirCitationJournalList = FHIR.R5.Resources.Financial.TFhirCitationJournalList;
  TFhirCitationJournalJournalIssue = FHIR.R5.Resources.Financial.TFhirCitationJournalJournalIssue;
  TFhirCitationJournalJournalIssueList = FHIR.R5.Resources.Financial.TFhirCitationJournalJournalIssueList;
  TFhirCitationJournalJournalIssuePublicationDate = FHIR.R5.Resources.Financial.TFhirCitationJournalJournalIssuePublicationDate;
  TFhirCitationJournalJournalIssuePublicationDateList = FHIR.R5.Resources.Financial.TFhirCitationJournalJournalIssuePublicationDateList;
  TFhirCitationPublicationInfo = FHIR.R5.Resources.Financial.TFhirCitationPublicationInfo;
  TFhirCitationPublicationInfoList = FHIR.R5.Resources.Financial.TFhirCitationPublicationInfoList;
  TFhirCitationPublicationInfoPublishedIn = FHIR.R5.Resources.Financial.TFhirCitationPublicationInfoPublishedIn;
  TFhirCitationPublicationInfoPublishedInList = FHIR.R5.Resources.Financial.TFhirCitationPublicationInfoPublishedInList;
  TFhirCitationAlternativeTitle = FHIR.R5.Resources.Financial.TFhirCitationAlternativeTitle;
  TFhirCitationAlternativeTitleList = FHIR.R5.Resources.Financial.TFhirCitationAlternativeTitleList;
  TFhirCitationPagination = FHIR.R5.Resources.Financial.TFhirCitationPagination;
  TFhirCitationPaginationList = FHIR.R5.Resources.Financial.TFhirCitationPaginationList;
  TFhirCitationArticleUrl = FHIR.R5.Resources.Financial.TFhirCitationArticleUrl;
  TFhirCitationArticleUrlList = FHIR.R5.Resources.Financial.TFhirCitationArticleUrlList;
  TFhirCitationAlternativeAbstract = FHIR.R5.Resources.Financial.TFhirCitationAlternativeAbstract;
  TFhirCitationAlternativeAbstractList = FHIR.R5.Resources.Financial.TFhirCitationAlternativeAbstractList;
  TFhirCitationContributorship = FHIR.R5.Resources.Financial.TFhirCitationContributorship;
  TFhirCitationContributorshipList = FHIR.R5.Resources.Financial.TFhirCitationContributorshipList;
  TFhirCitationContributorshipEntry = FHIR.R5.Resources.Financial.TFhirCitationContributorshipEntry;
  TFhirCitationContributorshipEntryList = FHIR.R5.Resources.Financial.TFhirCitationContributorshipEntryList;
  TFhirCitationContributorshipEntryAffiliationInfo = FHIR.R5.Resources.Financial.TFhirCitationContributorshipEntryAffiliationInfo;
  TFhirCitationContributorshipEntryAffiliationInfoList = FHIR.R5.Resources.Financial.TFhirCitationContributorshipEntryAffiliationInfoList;
  TFhirCitationContributorshipSummary = FHIR.R5.Resources.Financial.TFhirCitationContributorshipSummary;
  TFhirCitationContributorshipSummaryList = FHIR.R5.Resources.Financial.TFhirCitationContributorshipSummaryList;
  TFhirCitationAlternativeForm = FHIR.R5.Resources.Financial.TFhirCitationAlternativeForm;
  TFhirCitationAlternativeFormList = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormList;
  TFhirCitationAlternativeFormJournalIssue = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormJournalIssue;
  TFhirCitationAlternativeFormJournalIssueList = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormJournalIssueList;
  TFhirCitationAlternativeFormJournalIssuePublicationDate = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormJournalIssuePublicationDate;
  TFhirCitationAlternativeFormJournalIssuePublicationDateList = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormJournalIssuePublicationDateList;
  TFhirCitationAlternativeFormPagination = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormPagination;
  TFhirCitationAlternativeFormPaginationList = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormPaginationList;
  TFhirCitationAlternativeFormPublicationInfo = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormPublicationInfo;
  TFhirCitationAlternativeFormPublicationInfoList = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormPublicationInfoList;
  TFhirCitationAlternativeFormPublicationInfoPublishedIn = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormPublicationInfoPublishedIn;
  TFhirCitationAlternativeFormPublicationInfoPublishedInList = FHIR.R5.Resources.Financial.TFhirCitationAlternativeFormPublicationInfoPublishedInList;
  TFhirCitationKeywordList = FHIR.R5.Resources.Financial.TFhirCitationKeywordList;
  TFhirCitationKeywordListList = FHIR.R5.Resources.Financial.TFhirCitationKeywordListList;
  TFhirCitationKeywordListKeyword = FHIR.R5.Resources.Financial.TFhirCitationKeywordListKeyword;
  TFhirCitationKeywordListKeywordList = FHIR.R5.Resources.Financial.TFhirCitationKeywordListKeywordList;
  TFhirCitationMedlinePubMed = FHIR.R5.Resources.Financial.TFhirCitationMedlinePubMed;
  TFhirCitationMedlinePubMedList = FHIR.R5.Resources.Financial.TFhirCitationMedlinePubMedList;
  TFhirCitationMedlinePubMedPubMedPubDate = FHIR.R5.Resources.Financial.TFhirCitationMedlinePubMedPubMedPubDate;
  TFhirCitationMedlinePubMedPubMedPubDateList = FHIR.R5.Resources.Financial.TFhirCitationMedlinePubMedPubMedPubDateList;
  TFhirCitationMedlinePubMedRelatedArticle = FHIR.R5.Resources.Financial.TFhirCitationMedlinePubMedRelatedArticle;
  TFhirCitationMedlinePubMedRelatedArticleList = FHIR.R5.Resources.Financial.TFhirCitationMedlinePubMedRelatedArticleList;
  TFhirCitation = FHIR.R5.Resources.Financial.TFhirCitation;
  TFhirCitationList = FHIR.R5.Resources.Financial.TFhirCitationList;
{$ENDIF FHIR_CITATION}
{$IFDEF FHIR_CLAIM}
  TFhirClaimRelated = FHIR.R5.Resources.Financial.TFhirClaimRelated;
  TFhirClaimRelatedList = FHIR.R5.Resources.Financial.TFhirClaimRelatedList;
  TFhirClaimPayee = FHIR.R5.Resources.Financial.TFhirClaimPayee;
  TFhirClaimPayeeList = FHIR.R5.Resources.Financial.TFhirClaimPayeeList;
  TFhirClaimCareTeam = FHIR.R5.Resources.Financial.TFhirClaimCareTeam;
  TFhirClaimCareTeamList = FHIR.R5.Resources.Financial.TFhirClaimCareTeamList;
  TFhirClaimSupportingInfo = FHIR.R5.Resources.Financial.TFhirClaimSupportingInfo;
  TFhirClaimSupportingInfoList = FHIR.R5.Resources.Financial.TFhirClaimSupportingInfoList;
  TFhirClaimDiagnosis = FHIR.R5.Resources.Financial.TFhirClaimDiagnosis;
  TFhirClaimDiagnosisList = FHIR.R5.Resources.Financial.TFhirClaimDiagnosisList;
  TFhirClaimProcedure = FHIR.R5.Resources.Financial.TFhirClaimProcedure;
  TFhirClaimProcedureList = FHIR.R5.Resources.Financial.TFhirClaimProcedureList;
  TFhirClaimInsurance = FHIR.R5.Resources.Financial.TFhirClaimInsurance;
  TFhirClaimInsuranceList = FHIR.R5.Resources.Financial.TFhirClaimInsuranceList;
  TFhirClaimAccident = FHIR.R5.Resources.Financial.TFhirClaimAccident;
  TFhirClaimAccidentList = FHIR.R5.Resources.Financial.TFhirClaimAccidentList;
  TFhirClaimItem = FHIR.R5.Resources.Financial.TFhirClaimItem;
  TFhirClaimItemList = FHIR.R5.Resources.Financial.TFhirClaimItemList;
  TFhirClaimItemDetail = FHIR.R5.Resources.Financial.TFhirClaimItemDetail;
  TFhirClaimItemDetailList = FHIR.R5.Resources.Financial.TFhirClaimItemDetailList;
  TFhirClaimItemDetailSubDetail = FHIR.R5.Resources.Financial.TFhirClaimItemDetailSubDetail;
  TFhirClaimItemDetailSubDetailList = FHIR.R5.Resources.Financial.TFhirClaimItemDetailSubDetailList;
  TFhirClaim = FHIR.R5.Resources.Financial.TFhirClaim;
  TFhirClaimList = FHIR.R5.Resources.Financial.TFhirClaimList;
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  TFhirClaimResponseItem = FHIR.R5.Resources.Financial.TFhirClaimResponseItem;
  TFhirClaimResponseItemList = FHIR.R5.Resources.Financial.TFhirClaimResponseItemList;
  TFhirClaimResponseItemAdjudication = FHIR.R5.Resources.Financial.TFhirClaimResponseItemAdjudication;
  TFhirClaimResponseItemAdjudicationList = FHIR.R5.Resources.Financial.TFhirClaimResponseItemAdjudicationList;
  TFhirClaimResponseItemDetail = FHIR.R5.Resources.Financial.TFhirClaimResponseItemDetail;
  TFhirClaimResponseItemDetailList = FHIR.R5.Resources.Financial.TFhirClaimResponseItemDetailList;
  TFhirClaimResponseItemDetailSubDetail = FHIR.R5.Resources.Financial.TFhirClaimResponseItemDetailSubDetail;
  TFhirClaimResponseItemDetailSubDetailList = FHIR.R5.Resources.Financial.TFhirClaimResponseItemDetailSubDetailList;
  TFhirClaimResponseAddItem = FHIR.R5.Resources.Financial.TFhirClaimResponseAddItem;
  TFhirClaimResponseAddItemList = FHIR.R5.Resources.Financial.TFhirClaimResponseAddItemList;
  TFhirClaimResponseAddItemDetail = FHIR.R5.Resources.Financial.TFhirClaimResponseAddItemDetail;
  TFhirClaimResponseAddItemDetailList = FHIR.R5.Resources.Financial.TFhirClaimResponseAddItemDetailList;
  TFhirClaimResponseAddItemDetailSubDetail = FHIR.R5.Resources.Financial.TFhirClaimResponseAddItemDetailSubDetail;
  TFhirClaimResponseAddItemDetailSubDetailList = FHIR.R5.Resources.Financial.TFhirClaimResponseAddItemDetailSubDetailList;
  TFhirClaimResponseTotal = FHIR.R5.Resources.Financial.TFhirClaimResponseTotal;
  TFhirClaimResponseTotalList = FHIR.R5.Resources.Financial.TFhirClaimResponseTotalList;
  TFhirClaimResponsePayment = FHIR.R5.Resources.Financial.TFhirClaimResponsePayment;
  TFhirClaimResponsePaymentList = FHIR.R5.Resources.Financial.TFhirClaimResponsePaymentList;
  TFhirClaimResponseProcessNote = FHIR.R5.Resources.Financial.TFhirClaimResponseProcessNote;
  TFhirClaimResponseProcessNoteList = FHIR.R5.Resources.Financial.TFhirClaimResponseProcessNoteList;
  TFhirClaimResponseInsurance = FHIR.R5.Resources.Financial.TFhirClaimResponseInsurance;
  TFhirClaimResponseInsuranceList = FHIR.R5.Resources.Financial.TFhirClaimResponseInsuranceList;
  TFhirClaimResponseError = FHIR.R5.Resources.Financial.TFhirClaimResponseError;
  TFhirClaimResponseErrorList = FHIR.R5.Resources.Financial.TFhirClaimResponseErrorList;
  TFhirClaimResponse = FHIR.R5.Resources.Financial.TFhirClaimResponse;
  TFhirClaimResponseList = FHIR.R5.Resources.Financial.TFhirClaimResponseList;
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_COVERAGE}
  TFhirCoverageClass = FHIR.R5.Resources.Financial.TFhirCoverageClass;
  TFhirCoverageClassList = FHIR.R5.Resources.Financial.TFhirCoverageClassList;
  TFhirCoverageCostToBeneficiary = FHIR.R5.Resources.Financial.TFhirCoverageCostToBeneficiary;
  TFhirCoverageCostToBeneficiaryList = FHIR.R5.Resources.Financial.TFhirCoverageCostToBeneficiaryList;
  TFhirCoverageCostToBeneficiaryException = FHIR.R5.Resources.Financial.TFhirCoverageCostToBeneficiaryException;
  TFhirCoverageCostToBeneficiaryExceptionList = FHIR.R5.Resources.Financial.TFhirCoverageCostToBeneficiaryExceptionList;
  TFhirCoverage = FHIR.R5.Resources.Financial.TFhirCoverage;
  TFhirCoverageList = FHIR.R5.Resources.Financial.TFhirCoverageList;
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  TFhirCoverageEligibilityRequestSupportingInfo = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityRequestSupportingInfo;
  TFhirCoverageEligibilityRequestSupportingInfoList = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityRequestSupportingInfoList;
  TFhirCoverageEligibilityRequestInsurance = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityRequestInsurance;
  TFhirCoverageEligibilityRequestInsuranceList = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityRequestInsuranceList;
  TFhirCoverageEligibilityRequestItem = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityRequestItem;
  TFhirCoverageEligibilityRequestItemList = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityRequestItemList;
  TFhirCoverageEligibilityRequestItemDiagnosis = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityRequestItemDiagnosis;
  TFhirCoverageEligibilityRequestItemDiagnosisList = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityRequestItemDiagnosisList;
  TFhirCoverageEligibilityRequest = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityRequest;
  TFhirCoverageEligibilityRequestList = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityRequestList;
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}
{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  TFhirCoverageEligibilityResponseInsurance = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityResponseInsurance;
  TFhirCoverageEligibilityResponseInsuranceList = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityResponseInsuranceList;
  TFhirCoverageEligibilityResponseInsuranceItem = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityResponseInsuranceItem;
  TFhirCoverageEligibilityResponseInsuranceItemList = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityResponseInsuranceItemList;
  TFhirCoverageEligibilityResponseInsuranceItemBenefit = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityResponseInsuranceItemBenefit;
  TFhirCoverageEligibilityResponseInsuranceItemBenefitList = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityResponseInsuranceItemBenefitList;
  TFhirCoverageEligibilityResponseError = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityResponseError;
  TFhirCoverageEligibilityResponseErrorList = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityResponseErrorList;
  TFhirCoverageEligibilityResponse = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityResponse;
  TFhirCoverageEligibilityResponseList = FHIR.R5.Resources.Financial.TFhirCoverageEligibilityResponseList;
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  TFhirEnrollmentRequest = FHIR.R5.Resources.Financial.TFhirEnrollmentRequest;
  TFhirEnrollmentRequestList = FHIR.R5.Resources.Financial.TFhirEnrollmentRequestList;
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  TFhirEnrollmentResponse = FHIR.R5.Resources.Financial.TFhirEnrollmentResponse;
  TFhirEnrollmentResponseList = FHIR.R5.Resources.Financial.TFhirEnrollmentResponseList;
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  TFhirExplanationOfBenefitRelated = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitRelated;
  TFhirExplanationOfBenefitRelatedList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitRelatedList;
  TFhirExplanationOfBenefitPayee = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitPayee;
  TFhirExplanationOfBenefitPayeeList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitPayeeList;
  TFhirExplanationOfBenefitCareTeam = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitCareTeam;
  TFhirExplanationOfBenefitCareTeamList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitCareTeamList;
  TFhirExplanationOfBenefitSupportingInfo = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitSupportingInfo;
  TFhirExplanationOfBenefitSupportingInfoList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitSupportingInfoList;
  TFhirExplanationOfBenefitDiagnosis = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitDiagnosis;
  TFhirExplanationOfBenefitDiagnosisList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitDiagnosisList;
  TFhirExplanationOfBenefitProcedure = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitProcedure;
  TFhirExplanationOfBenefitProcedureList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitProcedureList;
  TFhirExplanationOfBenefitInsurance = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitInsurance;
  TFhirExplanationOfBenefitInsuranceList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitInsuranceList;
  TFhirExplanationOfBenefitAccident = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitAccident;
  TFhirExplanationOfBenefitAccidentList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitAccidentList;
  TFhirExplanationOfBenefitItem = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitItem;
  TFhirExplanationOfBenefitItemList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitItemList;
  TFhirExplanationOfBenefitItemAdjudication = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitItemAdjudication;
  TFhirExplanationOfBenefitItemAdjudicationList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitItemAdjudicationList;
  TFhirExplanationOfBenefitItemDetail = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitItemDetail;
  TFhirExplanationOfBenefitItemDetailList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitItemDetailList;
  TFhirExplanationOfBenefitItemDetailSubDetail = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitItemDetailSubDetail;
  TFhirExplanationOfBenefitItemDetailSubDetailList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitItemDetailSubDetailList;
  TFhirExplanationOfBenefitAddItem = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitAddItem;
  TFhirExplanationOfBenefitAddItemList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitAddItemList;
  TFhirExplanationOfBenefitAddItemDetail = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitAddItemDetail;
  TFhirExplanationOfBenefitAddItemDetailList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitAddItemDetailList;
  TFhirExplanationOfBenefitAddItemDetailSubDetail = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitAddItemDetailSubDetail;
  TFhirExplanationOfBenefitAddItemDetailSubDetailList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitAddItemDetailSubDetailList;
  TFhirExplanationOfBenefitTotal = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitTotal;
  TFhirExplanationOfBenefitTotalList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitTotalList;
  TFhirExplanationOfBenefitPayment = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitPayment;
  TFhirExplanationOfBenefitPaymentList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitPaymentList;
  TFhirExplanationOfBenefitProcessNote = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitProcessNote;
  TFhirExplanationOfBenefitProcessNoteList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitProcessNoteList;
  TFhirExplanationOfBenefitBenefitBalance = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitBenefitBalance;
  TFhirExplanationOfBenefitBenefitBalanceList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitBenefitBalanceList;
  TFhirExplanationOfBenefitBenefitBalanceFinancial = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitBenefitBalanceFinancial;
  TFhirExplanationOfBenefitBenefitBalanceFinancialList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitBenefitBalanceFinancialList;
  TFhirExplanationOfBenefit = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefit;
  TFhirExplanationOfBenefitList = FHIR.R5.Resources.Financial.TFhirExplanationOfBenefitList;
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_INSURANCEPLAN}
  TFhirInsurancePlanContact = FHIR.R5.Resources.Financial.TFhirInsurancePlanContact;
  TFhirInsurancePlanContactList = FHIR.R5.Resources.Financial.TFhirInsurancePlanContactList;
  TFhirInsurancePlanCoverage = FHIR.R5.Resources.Financial.TFhirInsurancePlanCoverage;
  TFhirInsurancePlanCoverageList = FHIR.R5.Resources.Financial.TFhirInsurancePlanCoverageList;
  TFhirInsurancePlanCoverageBenefit = FHIR.R5.Resources.Financial.TFhirInsurancePlanCoverageBenefit;
  TFhirInsurancePlanCoverageBenefitList = FHIR.R5.Resources.Financial.TFhirInsurancePlanCoverageBenefitList;
  TFhirInsurancePlanCoverageBenefitLimit = FHIR.R5.Resources.Financial.TFhirInsurancePlanCoverageBenefitLimit;
  TFhirInsurancePlanCoverageBenefitLimitList = FHIR.R5.Resources.Financial.TFhirInsurancePlanCoverageBenefitLimitList;
  TFhirInsurancePlanPlan = FHIR.R5.Resources.Financial.TFhirInsurancePlanPlan;
  TFhirInsurancePlanPlanList = FHIR.R5.Resources.Financial.TFhirInsurancePlanPlanList;
  TFhirInsurancePlanPlanGeneralCost = FHIR.R5.Resources.Financial.TFhirInsurancePlanPlanGeneralCost;
  TFhirInsurancePlanPlanGeneralCostList = FHIR.R5.Resources.Financial.TFhirInsurancePlanPlanGeneralCostList;
  TFhirInsurancePlanPlanSpecificCost = FHIR.R5.Resources.Financial.TFhirInsurancePlanPlanSpecificCost;
  TFhirInsurancePlanPlanSpecificCostList = FHIR.R5.Resources.Financial.TFhirInsurancePlanPlanSpecificCostList;
  TFhirInsurancePlanPlanSpecificCostBenefit = FHIR.R5.Resources.Financial.TFhirInsurancePlanPlanSpecificCostBenefit;
  TFhirInsurancePlanPlanSpecificCostBenefitList = FHIR.R5.Resources.Financial.TFhirInsurancePlanPlanSpecificCostBenefitList;
  TFhirInsurancePlanPlanSpecificCostBenefitCost = FHIR.R5.Resources.Financial.TFhirInsurancePlanPlanSpecificCostBenefitCost;
  TFhirInsurancePlanPlanSpecificCostBenefitCostList = FHIR.R5.Resources.Financial.TFhirInsurancePlanPlanSpecificCostBenefitCostList;
  TFhirInsurancePlan = FHIR.R5.Resources.Financial.TFhirInsurancePlan;
  TFhirInsurancePlanList = FHIR.R5.Resources.Financial.TFhirInsurancePlanList;
{$ENDIF FHIR_INSURANCEPLAN}
{$IFDEF FHIR_INVOICE}
  TFhirInvoiceParticipant = FHIR.R5.Resources.Financial.TFhirInvoiceParticipant;
  TFhirInvoiceParticipantList = FHIR.R5.Resources.Financial.TFhirInvoiceParticipantList;
  TFhirInvoiceLineItem = FHIR.R5.Resources.Financial.TFhirInvoiceLineItem;
  TFhirInvoiceLineItemList = FHIR.R5.Resources.Financial.TFhirInvoiceLineItemList;
  TFhirInvoiceLineItemPriceComponent = FHIR.R5.Resources.Financial.TFhirInvoiceLineItemPriceComponent;
  TFhirInvoiceLineItemPriceComponentList = FHIR.R5.Resources.Financial.TFhirInvoiceLineItemPriceComponentList;
  TFhirInvoice = FHIR.R5.Resources.Financial.TFhirInvoice;
  TFhirInvoiceList = FHIR.R5.Resources.Financial.TFhirInvoiceList;
{$ENDIF FHIR_INVOICE}
{$IFDEF FHIR_PAYMENTNOTICE}
  TFhirPaymentNotice = FHIR.R5.Resources.Financial.TFhirPaymentNotice;
  TFhirPaymentNoticeList = FHIR.R5.Resources.Financial.TFhirPaymentNoticeList;
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  TFhirPaymentReconciliationDetail = FHIR.R5.Resources.Financial.TFhirPaymentReconciliationDetail;
  TFhirPaymentReconciliationDetailList = FHIR.R5.Resources.Financial.TFhirPaymentReconciliationDetailList;
  TFhirPaymentReconciliationProcessNote = FHIR.R5.Resources.Financial.TFhirPaymentReconciliationProcessNote;
  TFhirPaymentReconciliationProcessNoteList = FHIR.R5.Resources.Financial.TFhirPaymentReconciliationProcessNoteList;
  TFhirPaymentReconciliation = FHIR.R5.Resources.Financial.TFhirPaymentReconciliation;
  TFhirPaymentReconciliationList = FHIR.R5.Resources.Financial.TFhirPaymentReconciliationList;
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
  TFhirAdministrableProductDefinitionProperty = FHIR.R5.Resources.Medications.TFhirAdministrableProductDefinitionProperty;
  TFhirAdministrableProductDefinitionPropertyList = FHIR.R5.Resources.Medications.TFhirAdministrableProductDefinitionPropertyList;
  TFhirAdministrableProductDefinitionRouteOfAdministration = FHIR.R5.Resources.Medications.TFhirAdministrableProductDefinitionRouteOfAdministration;
  TFhirAdministrableProductDefinitionRouteOfAdministrationList = FHIR.R5.Resources.Medications.TFhirAdministrableProductDefinitionRouteOfAdministrationList;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpecies = FHIR.R5.Resources.Medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpecies;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesList = FHIR.R5.Resources.Medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesList;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriod = FHIR.R5.Resources.Medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriod;
  TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriodList = FHIR.R5.Resources.Medications.TFhirAdministrableProductDefinitionRouteOfAdministrationTargetSpeciesWithdrawalPeriodList;
  TFhirAdministrableProductDefinition = FHIR.R5.Resources.Medications.TFhirAdministrableProductDefinition;
  TFhirAdministrableProductDefinitionList = FHIR.R5.Resources.Medications.TFhirAdministrableProductDefinitionList;
{$ENDIF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
{$IFDEF FHIR_INGREDIENT}
  TFhirIngredientSubstance = FHIR.R5.Resources.Medications.TFhirIngredientSubstance;
  TFhirIngredientSubstanceList = FHIR.R5.Resources.Medications.TFhirIngredientSubstanceList;
  TFhirIngredientSubstanceStrength = FHIR.R5.Resources.Medications.TFhirIngredientSubstanceStrength;
  TFhirIngredientSubstanceStrengthList = FHIR.R5.Resources.Medications.TFhirIngredientSubstanceStrengthList;
  TFhirIngredientSubstanceStrengthReferenceStrength = FHIR.R5.Resources.Medications.TFhirIngredientSubstanceStrengthReferenceStrength;
  TFhirIngredientSubstanceStrengthReferenceStrengthList = FHIR.R5.Resources.Medications.TFhirIngredientSubstanceStrengthReferenceStrengthList;
  TFhirIngredientSpecifiedSubstance = FHIR.R5.Resources.Medications.TFhirIngredientSpecifiedSubstance;
  TFhirIngredientSpecifiedSubstanceList = FHIR.R5.Resources.Medications.TFhirIngredientSpecifiedSubstanceList;
  TFhirIngredient = FHIR.R5.Resources.Medications.TFhirIngredient;
  TFhirIngredientList = FHIR.R5.Resources.Medications.TFhirIngredientList;
{$ENDIF FHIR_INGREDIENT}
{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
  TFhirManufacturedItemDefinitionProperty = FHIR.R5.Resources.Medications.TFhirManufacturedItemDefinitionProperty;
  TFhirManufacturedItemDefinitionPropertyList = FHIR.R5.Resources.Medications.TFhirManufacturedItemDefinitionPropertyList;
  TFhirManufacturedItemDefinition = FHIR.R5.Resources.Medications.TFhirManufacturedItemDefinition;
  TFhirManufacturedItemDefinitionList = FHIR.R5.Resources.Medications.TFhirManufacturedItemDefinitionList;
{$ENDIF FHIR_MANUFACTUREDITEMDEFINITION}
{$IFDEF FHIR_MEDICATION}
  TFhirMedicationIngredient = FHIR.R5.Resources.Medications.TFhirMedicationIngredient;
  TFhirMedicationIngredientList = FHIR.R5.Resources.Medications.TFhirMedicationIngredientList;
  TFhirMedicationBatch = FHIR.R5.Resources.Medications.TFhirMedicationBatch;
  TFhirMedicationBatchList = FHIR.R5.Resources.Medications.TFhirMedicationBatchList;
  TFhirMedication = FHIR.R5.Resources.Medications.TFhirMedication;
  TFhirMedicationList = FHIR.R5.Resources.Medications.TFhirMedicationList;
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  TFhirMedicationKnowledgeRelatedMedicationKnowledge = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeRelatedMedicationKnowledge;
  TFhirMedicationKnowledgeRelatedMedicationKnowledgeList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeRelatedMedicationKnowledgeList;
  TFhirMedicationKnowledgeMonograph = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeMonograph;
  TFhirMedicationKnowledgeMonographList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeMonographList;
  TFhirMedicationKnowledgeIngredient = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeIngredient;
  TFhirMedicationKnowledgeIngredientList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeIngredientList;
  TFhirMedicationKnowledgeCost = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeCost;
  TFhirMedicationKnowledgeCostList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeCostList;
  TFhirMedicationKnowledgeMonitoringProgram = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeMonitoringProgram;
  TFhirMedicationKnowledgeMonitoringProgramList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeMonitoringProgramList;
  TFhirMedicationKnowledgeAdministrationGuideline = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuideline;
  TFhirMedicationKnowledgeAdministrationGuidelineList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelineList;
  TFhirMedicationKnowledgeAdministrationGuidelineDosage = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelineDosage;
  TFhirMedicationKnowledgeAdministrationGuidelineDosageList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelineDosageList;
  TFhirMedicationKnowledgeAdministrationGuidelinePatientCharacteristic = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelinePatientCharacteristic;
  TFhirMedicationKnowledgeAdministrationGuidelinePatientCharacteristicList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeAdministrationGuidelinePatientCharacteristicList;
  TFhirMedicationKnowledgeMedicineClassification = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeMedicineClassification;
  TFhirMedicationKnowledgeMedicineClassificationList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeMedicineClassificationList;
  TFhirMedicationKnowledgePackaging = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgePackaging;
  TFhirMedicationKnowledgePackagingList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgePackagingList;
  TFhirMedicationKnowledgeDrugCharacteristic = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeDrugCharacteristic;
  TFhirMedicationKnowledgeDrugCharacteristicList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeDrugCharacteristicList;
  TFhirMedicationKnowledgeRegulatory = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeRegulatory;
  TFhirMedicationKnowledgeRegulatoryList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeRegulatoryList;
  TFhirMedicationKnowledgeRegulatorySubstitution = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeRegulatorySubstitution;
  TFhirMedicationKnowledgeRegulatorySubstitutionList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeRegulatorySubstitutionList;
  TFhirMedicationKnowledgeRegulatoryMaxDispense = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeRegulatoryMaxDispense;
  TFhirMedicationKnowledgeRegulatoryMaxDispenseList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeRegulatoryMaxDispenseList;
  TFhirMedicationKnowledgeKineticCharacteristic = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeKineticCharacteristic;
  TFhirMedicationKnowledgeKineticCharacteristicList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeKineticCharacteristicList;
  TFhirMedicationKnowledge = FHIR.R5.Resources.Medications.TFhirMedicationKnowledge;
  TFhirMedicationKnowledgeList = FHIR.R5.Resources.Medications.TFhirMedicationKnowledgeList;
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}
{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
  TFhirMedicinalProductDefinitionContact = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionContact;
  TFhirMedicinalProductDefinitionContactList = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionContactList;
  TFhirMedicinalProductDefinitionName = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionName;
  TFhirMedicinalProductDefinitionNameList = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionNameList;
  TFhirMedicinalProductDefinitionNameNamePart = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionNameNamePart;
  TFhirMedicinalProductDefinitionNameNamePartList = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionNameNamePartList;
  TFhirMedicinalProductDefinitionNameCountryLanguage = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionNameCountryLanguage;
  TFhirMedicinalProductDefinitionNameCountryLanguageList = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionNameCountryLanguageList;
  TFhirMedicinalProductDefinitionCrossReference = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionCrossReference;
  TFhirMedicinalProductDefinitionCrossReferenceList = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionCrossReferenceList;
  TFhirMedicinalProductDefinitionManufacturingBusinessOperation = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionManufacturingBusinessOperation;
  TFhirMedicinalProductDefinitionManufacturingBusinessOperationList = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionManufacturingBusinessOperationList;
  TFhirMedicinalProductDefinition = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinition;
  TFhirMedicinalProductDefinitionList = FHIR.R5.Resources.Medications.TFhirMedicinalProductDefinitionList;
{$ENDIF FHIR_MEDICINALPRODUCTDEFINITION}
{$IFDEF FHIR_NUTRITIONPRODUCT}
  TFhirNutritionProductNutrient = FHIR.R5.Resources.Medications.TFhirNutritionProductNutrient;
  TFhirNutritionProductNutrientList = FHIR.R5.Resources.Medications.TFhirNutritionProductNutrientList;
  TFhirNutritionProductIngredient = FHIR.R5.Resources.Medications.TFhirNutritionProductIngredient;
  TFhirNutritionProductIngredientList = FHIR.R5.Resources.Medications.TFhirNutritionProductIngredientList;
  TFhirNutritionProductProductCharacteristic = FHIR.R5.Resources.Medications.TFhirNutritionProductProductCharacteristic;
  TFhirNutritionProductProductCharacteristicList = FHIR.R5.Resources.Medications.TFhirNutritionProductProductCharacteristicList;
  TFhirNutritionProductInstance = FHIR.R5.Resources.Medications.TFhirNutritionProductInstance;
  TFhirNutritionProductInstanceList = FHIR.R5.Resources.Medications.TFhirNutritionProductInstanceList;
  TFhirNutritionProduct = FHIR.R5.Resources.Medications.TFhirNutritionProduct;
  TFhirNutritionProductList = FHIR.R5.Resources.Medications.TFhirNutritionProductList;
{$ENDIF FHIR_NUTRITIONPRODUCT}
{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
  TFhirPackagedProductDefinitionBatchIdentifier = FHIR.R5.Resources.Medications.TFhirPackagedProductDefinitionBatchIdentifier;
  TFhirPackagedProductDefinitionBatchIdentifierList = FHIR.R5.Resources.Medications.TFhirPackagedProductDefinitionBatchIdentifierList;
  TFhirPackagedProductDefinitionPackage = FHIR.R5.Resources.Medications.TFhirPackagedProductDefinitionPackage;
  TFhirPackagedProductDefinitionPackageList = FHIR.R5.Resources.Medications.TFhirPackagedProductDefinitionPackageList;
  TFhirPackagedProductDefinitionPackageProperty = FHIR.R5.Resources.Medications.TFhirPackagedProductDefinitionPackageProperty;
  TFhirPackagedProductDefinitionPackagePropertyList = FHIR.R5.Resources.Medications.TFhirPackagedProductDefinitionPackagePropertyList;
  TFhirPackagedProductDefinitionPackageContainedItem = FHIR.R5.Resources.Medications.TFhirPackagedProductDefinitionPackageContainedItem;
  TFhirPackagedProductDefinitionPackageContainedItemList = FHIR.R5.Resources.Medications.TFhirPackagedProductDefinitionPackageContainedItemList;
  TFhirPackagedProductDefinition = FHIR.R5.Resources.Medications.TFhirPackagedProductDefinition;
  TFhirPackagedProductDefinitionList = FHIR.R5.Resources.Medications.TFhirPackagedProductDefinitionList;
{$ENDIF FHIR_PACKAGEDPRODUCTDEFINITION}
{$IFDEF FHIR_REGULATEDAUTHORIZATION}
  TFhirRegulatedAuthorizationRelatedDate = FHIR.R5.Resources.Medications.TFhirRegulatedAuthorizationRelatedDate;
  TFhirRegulatedAuthorizationRelatedDateList = FHIR.R5.Resources.Medications.TFhirRegulatedAuthorizationRelatedDateList;
  TFhirRegulatedAuthorizationCase = FHIR.R5.Resources.Medications.TFhirRegulatedAuthorizationCase;
  TFhirRegulatedAuthorizationCaseList = FHIR.R5.Resources.Medications.TFhirRegulatedAuthorizationCaseList;
  TFhirRegulatedAuthorization = FHIR.R5.Resources.Medications.TFhirRegulatedAuthorization;
  TFhirRegulatedAuthorizationList = FHIR.R5.Resources.Medications.TFhirRegulatedAuthorizationList;
{$ENDIF FHIR_REGULATEDAUTHORIZATION}
{$IFDEF FHIR_SUBSTANCE}
  TFhirSubstanceInstance = FHIR.R5.Resources.Medications.TFhirSubstanceInstance;
  TFhirSubstanceInstanceList = FHIR.R5.Resources.Medications.TFhirSubstanceInstanceList;
  TFhirSubstanceIngredient = FHIR.R5.Resources.Medications.TFhirSubstanceIngredient;
  TFhirSubstanceIngredientList = FHIR.R5.Resources.Medications.TFhirSubstanceIngredientList;
  TFhirSubstance = FHIR.R5.Resources.Medications.TFhirSubstance;
  TFhirSubstanceList = FHIR.R5.Resources.Medications.TFhirSubstanceList;
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_SUBSTANCEDEFINITION}
  TFhirSubstanceDefinitionMoiety = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionMoiety;
  TFhirSubstanceDefinitionMoietyList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionMoietyList;
  TFhirSubstanceDefinitionProperty = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionProperty;
  TFhirSubstanceDefinitionPropertyList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionPropertyList;
  TFhirSubstanceDefinitionStructure = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionStructure;
  TFhirSubstanceDefinitionStructureList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionStructureList;
  TFhirSubstanceDefinitionStructureIsotope = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionStructureIsotope;
  TFhirSubstanceDefinitionStructureIsotopeList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionStructureIsotopeList;
  TFhirSubstanceDefinitionStructureIsotopeMolecularWeight = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionStructureIsotopeMolecularWeight;
  TFhirSubstanceDefinitionStructureIsotopeMolecularWeightList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionStructureIsotopeMolecularWeightList;
  TFhirSubstanceDefinitionStructureRepresentation = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionStructureRepresentation;
  TFhirSubstanceDefinitionStructureRepresentationList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionStructureRepresentationList;
  TFhirSubstanceDefinitionCode = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionCode;
  TFhirSubstanceDefinitionCodeList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionCodeList;
  TFhirSubstanceDefinitionName = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionName;
  TFhirSubstanceDefinitionNameList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionNameList;
  TFhirSubstanceDefinitionNameOfficial = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionNameOfficial;
  TFhirSubstanceDefinitionNameOfficialList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionNameOfficialList;
  TFhirSubstanceDefinitionRelationship = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionRelationship;
  TFhirSubstanceDefinitionRelationshipList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionRelationshipList;
  TFhirSubstanceDefinition = FHIR.R5.Resources.Medications.TFhirSubstanceDefinition;
  TFhirSubstanceDefinitionList = FHIR.R5.Resources.Medications.TFhirSubstanceDefinitionList;
{$ENDIF FHIR_SUBSTANCEDEFINITION}
{$IFDEF FHIR_SUBSTANCENUCLEICACID}
  TFhirSubstanceNucleicAcidSubunit = FHIR.R5.Resources.Medications.TFhirSubstanceNucleicAcidSubunit;
  TFhirSubstanceNucleicAcidSubunitList = FHIR.R5.Resources.Medications.TFhirSubstanceNucleicAcidSubunitList;
  TFhirSubstanceNucleicAcidSubunitLinkage = FHIR.R5.Resources.Medications.TFhirSubstanceNucleicAcidSubunitLinkage;
  TFhirSubstanceNucleicAcidSubunitLinkageList = FHIR.R5.Resources.Medications.TFhirSubstanceNucleicAcidSubunitLinkageList;
  TFhirSubstanceNucleicAcidSubunitSugar = FHIR.R5.Resources.Medications.TFhirSubstanceNucleicAcidSubunitSugar;
  TFhirSubstanceNucleicAcidSubunitSugarList = FHIR.R5.Resources.Medications.TFhirSubstanceNucleicAcidSubunitSugarList;
  TFhirSubstanceNucleicAcid = FHIR.R5.Resources.Medications.TFhirSubstanceNucleicAcid;
  TFhirSubstanceNucleicAcidList = FHIR.R5.Resources.Medications.TFhirSubstanceNucleicAcidList;
{$ENDIF FHIR_SUBSTANCENUCLEICACID}
{$IFDEF FHIR_SUBSTANCEPOLYMER}
  TFhirSubstancePolymerMonomerSet = FHIR.R5.Resources.Medications.TFhirSubstancePolymerMonomerSet;
  TFhirSubstancePolymerMonomerSetList = FHIR.R5.Resources.Medications.TFhirSubstancePolymerMonomerSetList;
  TFhirSubstancePolymerMonomerSetStartingMaterial = FHIR.R5.Resources.Medications.TFhirSubstancePolymerMonomerSetStartingMaterial;
  TFhirSubstancePolymerMonomerSetStartingMaterialList = FHIR.R5.Resources.Medications.TFhirSubstancePolymerMonomerSetStartingMaterialList;
  TFhirSubstancePolymerRepeat = FHIR.R5.Resources.Medications.TFhirSubstancePolymerRepeat;
  TFhirSubstancePolymerRepeatList = FHIR.R5.Resources.Medications.TFhirSubstancePolymerRepeatList;
  TFhirSubstancePolymerRepeatRepeatUnit = FHIR.R5.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnit;
  TFhirSubstancePolymerRepeatRepeatUnitList = FHIR.R5.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnitList;
  TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation = FHIR.R5.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation;
  TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationList = FHIR.R5.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationList;
  TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentation = FHIR.R5.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentation;
  TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentationList = FHIR.R5.Resources.Medications.TFhirSubstancePolymerRepeatRepeatUnitStructuralRepresentationList;
  TFhirSubstancePolymer = FHIR.R5.Resources.Medications.TFhirSubstancePolymer;
  TFhirSubstancePolymerList = FHIR.R5.Resources.Medications.TFhirSubstancePolymerList;
{$ENDIF FHIR_SUBSTANCEPOLYMER}
{$IFDEF FHIR_SUBSTANCEPROTEIN}
  TFhirSubstanceProteinSubunit = FHIR.R5.Resources.Medications.TFhirSubstanceProteinSubunit;
  TFhirSubstanceProteinSubunitList = FHIR.R5.Resources.Medications.TFhirSubstanceProteinSubunitList;
  TFhirSubstanceProtein = FHIR.R5.Resources.Medications.TFhirSubstanceProtein;
  TFhirSubstanceProteinList = FHIR.R5.Resources.Medications.TFhirSubstanceProteinList;
{$ENDIF FHIR_SUBSTANCEPROTEIN}
{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  TFhirSubstanceReferenceInformationGene = FHIR.R5.Resources.Medications.TFhirSubstanceReferenceInformationGene;
  TFhirSubstanceReferenceInformationGeneList = FHIR.R5.Resources.Medications.TFhirSubstanceReferenceInformationGeneList;
  TFhirSubstanceReferenceInformationGeneElement = FHIR.R5.Resources.Medications.TFhirSubstanceReferenceInformationGeneElement;
  TFhirSubstanceReferenceInformationGeneElementList = FHIR.R5.Resources.Medications.TFhirSubstanceReferenceInformationGeneElementList;
  TFhirSubstanceReferenceInformationTarget = FHIR.R5.Resources.Medications.TFhirSubstanceReferenceInformationTarget;
  TFhirSubstanceReferenceInformationTargetList = FHIR.R5.Resources.Medications.TFhirSubstanceReferenceInformationTargetList;
  TFhirSubstanceReferenceInformation = FHIR.R5.Resources.Medications.TFhirSubstanceReferenceInformation;
  TFhirSubstanceReferenceInformationList = FHIR.R5.Resources.Medications.TFhirSubstanceReferenceInformationList;
{$ENDIF FHIR_SUBSTANCEREFERENCEINFORMATION}
{$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
  TFhirSubstanceSourceMaterialFractionDescription = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialFractionDescription;
  TFhirSubstanceSourceMaterialFractionDescriptionList = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialFractionDescriptionList;
  TFhirSubstanceSourceMaterialOrganism = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialOrganism;
  TFhirSubstanceSourceMaterialOrganismList = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialOrganismList;
  TFhirSubstanceSourceMaterialOrganismAuthor = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialOrganismAuthor;
  TFhirSubstanceSourceMaterialOrganismAuthorList = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialOrganismAuthorList;
  TFhirSubstanceSourceMaterialOrganismHybrid = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialOrganismHybrid;
  TFhirSubstanceSourceMaterialOrganismHybridList = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialOrganismHybridList;
  TFhirSubstanceSourceMaterialOrganismOrganismGeneral = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialOrganismOrganismGeneral;
  TFhirSubstanceSourceMaterialOrganismOrganismGeneralList = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialOrganismOrganismGeneralList;
  TFhirSubstanceSourceMaterialPartDescription = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialPartDescription;
  TFhirSubstanceSourceMaterialPartDescriptionList = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialPartDescriptionList;
  TFhirSubstanceSourceMaterial = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterial;
  TFhirSubstanceSourceMaterialList = FHIR.R5.Resources.Medications.TFhirSubstanceSourceMaterialList;
{$ENDIF FHIR_SUBSTANCESOURCEMATERIAL}
{$IFDEF FHIR_AUDITEVENT}
  TFhirAuditEventAgent = FHIR.R5.Resources.Other.TFhirAuditEventAgent;
  TFhirAuditEventAgentList = FHIR.R5.Resources.Other.TFhirAuditEventAgentList;
  TFhirAuditEventAgentNetwork = FHIR.R5.Resources.Other.TFhirAuditEventAgentNetwork;
  TFhirAuditEventAgentNetworkList = FHIR.R5.Resources.Other.TFhirAuditEventAgentNetworkList;
  TFhirAuditEventSource = FHIR.R5.Resources.Other.TFhirAuditEventSource;
  TFhirAuditEventSourceList = FHIR.R5.Resources.Other.TFhirAuditEventSourceList;
  TFhirAuditEventEntity = FHIR.R5.Resources.Other.TFhirAuditEventEntity;
  TFhirAuditEventEntityList = FHIR.R5.Resources.Other.TFhirAuditEventEntityList;
  TFhirAuditEventEntityDetail = FHIR.R5.Resources.Other.TFhirAuditEventEntityDetail;
  TFhirAuditEventEntityDetailList = FHIR.R5.Resources.Other.TFhirAuditEventEntityDetailList;
  TFhirAuditEvent = FHIR.R5.Resources.Other.TFhirAuditEvent;
  TFhirAuditEventList = FHIR.R5.Resources.Other.TFhirAuditEventList;
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BINARY}
  TFhirBinary = FHIR.R5.Resources.Other.TFhirBinary;
  TFhirBinaryList = FHIR.R5.Resources.Other.TFhirBinaryList;
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BUNDLE}
  TFhirBundleLink = FHIR.R5.Resources.Other.TFhirBundleLink;
  TFhirBundleLinkList = FHIR.R5.Resources.Other.TFhirBundleLinkList;
  TFhirBundleEntry = FHIR.R5.Resources.Other.TFhirBundleEntry;
  TFhirBundleEntryList = FHIR.R5.Resources.Other.TFhirBundleEntryList;
  TFhirBundleEntrySearch = FHIR.R5.Resources.Other.TFhirBundleEntrySearch;
  TFhirBundleEntrySearchList = FHIR.R5.Resources.Other.TFhirBundleEntrySearchList;
  TFhirBundleEntryRequest = FHIR.R5.Resources.Other.TFhirBundleEntryRequest;
  TFhirBundleEntryRequestList = FHIR.R5.Resources.Other.TFhirBundleEntryRequestList;
  TFhirBundleEntryResponse = FHIR.R5.Resources.Other.TFhirBundleEntryResponse;
  TFhirBundleEntryResponseList = FHIR.R5.Resources.Other.TFhirBundleEntryResponseList;
  TFhirBundle = FHIR.R5.Resources.Other.TFhirBundle;
  TFhirBundleList = FHIR.R5.Resources.Other.TFhirBundleList;
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CONSENT}
  TFhirConsentPolicy = FHIR.R5.Resources.Other.TFhirConsentPolicy;
  TFhirConsentPolicyList = FHIR.R5.Resources.Other.TFhirConsentPolicyList;
  TFhirConsentVerification = FHIR.R5.Resources.Other.TFhirConsentVerification;
  TFhirConsentVerificationList = FHIR.R5.Resources.Other.TFhirConsentVerificationList;
  TFhirConsentProvision = FHIR.R5.Resources.Other.TFhirConsentProvision;
  TFhirConsentProvisionList = FHIR.R5.Resources.Other.TFhirConsentProvisionList;
  TFhirConsentProvisionActor = FHIR.R5.Resources.Other.TFhirConsentProvisionActor;
  TFhirConsentProvisionActorList = FHIR.R5.Resources.Other.TFhirConsentProvisionActorList;
  TFhirConsentProvisionData = FHIR.R5.Resources.Other.TFhirConsentProvisionData;
  TFhirConsentProvisionDataList = FHIR.R5.Resources.Other.TFhirConsentProvisionDataList;
  TFhirConsent = FHIR.R5.Resources.Other.TFhirConsent;
  TFhirConsentList = FHIR.R5.Resources.Other.TFhirConsentList;
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  TFhirContractContentDefinition = FHIR.R5.Resources.Other.TFhirContractContentDefinition;
  TFhirContractContentDefinitionList = FHIR.R5.Resources.Other.TFhirContractContentDefinitionList;
  TFhirContractTerm = FHIR.R5.Resources.Other.TFhirContractTerm;
  TFhirContractTermList = FHIR.R5.Resources.Other.TFhirContractTermList;
  TFhirContractTermSecurityLabel = FHIR.R5.Resources.Other.TFhirContractTermSecurityLabel;
  TFhirContractTermSecurityLabelList = FHIR.R5.Resources.Other.TFhirContractTermSecurityLabelList;
  TFhirContractTermOffer = FHIR.R5.Resources.Other.TFhirContractTermOffer;
  TFhirContractTermOfferList = FHIR.R5.Resources.Other.TFhirContractTermOfferList;
  TFhirContractTermOfferParty = FHIR.R5.Resources.Other.TFhirContractTermOfferParty;
  TFhirContractTermOfferPartyList = FHIR.R5.Resources.Other.TFhirContractTermOfferPartyList;
  TFhirContractTermOfferAnswer = FHIR.R5.Resources.Other.TFhirContractTermOfferAnswer;
  TFhirContractTermOfferAnswerList = FHIR.R5.Resources.Other.TFhirContractTermOfferAnswerList;
  TFhirContractTermAsset = FHIR.R5.Resources.Other.TFhirContractTermAsset;
  TFhirContractTermAssetList = FHIR.R5.Resources.Other.TFhirContractTermAssetList;
  TFhirContractTermAssetContext = FHIR.R5.Resources.Other.TFhirContractTermAssetContext;
  TFhirContractTermAssetContextList = FHIR.R5.Resources.Other.TFhirContractTermAssetContextList;
  TFhirContractTermAssetValuedItem = FHIR.R5.Resources.Other.TFhirContractTermAssetValuedItem;
  TFhirContractTermAssetValuedItemList = FHIR.R5.Resources.Other.TFhirContractTermAssetValuedItemList;
  TFhirContractTermAction = FHIR.R5.Resources.Other.TFhirContractTermAction;
  TFhirContractTermActionList = FHIR.R5.Resources.Other.TFhirContractTermActionList;
  TFhirContractTermActionSubject = FHIR.R5.Resources.Other.TFhirContractTermActionSubject;
  TFhirContractTermActionSubjectList = FHIR.R5.Resources.Other.TFhirContractTermActionSubjectList;
  TFhirContractSigner = FHIR.R5.Resources.Other.TFhirContractSigner;
  TFhirContractSignerList = FHIR.R5.Resources.Other.TFhirContractSignerList;
  TFhirContractFriendly = FHIR.R5.Resources.Other.TFhirContractFriendly;
  TFhirContractFriendlyList = FHIR.R5.Resources.Other.TFhirContractFriendlyList;
  TFhirContractLegal = FHIR.R5.Resources.Other.TFhirContractLegal;
  TFhirContractLegalList = FHIR.R5.Resources.Other.TFhirContractLegalList;
  TFhirContractRule = FHIR.R5.Resources.Other.TFhirContractRule;
  TFhirContractRuleList = FHIR.R5.Resources.Other.TFhirContractRuleList;
  TFhirContract = FHIR.R5.Resources.Other.TFhirContract;
  TFhirContractList = FHIR.R5.Resources.Other.TFhirContractList;
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_EVIDENCE}
  TFhirEvidenceVariableDefinition = FHIR.R5.Resources.Other.TFhirEvidenceVariableDefinition;
  TFhirEvidenceVariableDefinitionList = FHIR.R5.Resources.Other.TFhirEvidenceVariableDefinitionList;
  TFhirEvidenceCertainty = FHIR.R5.Resources.Other.TFhirEvidenceCertainty;
  TFhirEvidenceCertaintyList = FHIR.R5.Resources.Other.TFhirEvidenceCertaintyList;
  TFhirEvidenceCertaintyCertaintySubcomponent = FHIR.R5.Resources.Other.TFhirEvidenceCertaintyCertaintySubcomponent;
  TFhirEvidenceCertaintyCertaintySubcomponentList = FHIR.R5.Resources.Other.TFhirEvidenceCertaintyCertaintySubcomponentList;
  TFhirEvidence = FHIR.R5.Resources.Other.TFhirEvidence;
  TFhirEvidenceList = FHIR.R5.Resources.Other.TFhirEvidenceList;
{$ENDIF FHIR_EVIDENCE}
{$IFDEF FHIR_EVIDENCEREPORT}
  TFhirEvidenceReportSubject = FHIR.R5.Resources.Other.TFhirEvidenceReportSubject;
  TFhirEvidenceReportSubjectList = FHIR.R5.Resources.Other.TFhirEvidenceReportSubjectList;
  TFhirEvidenceReportSubjectCharacteristic = FHIR.R5.Resources.Other.TFhirEvidenceReportSubjectCharacteristic;
  TFhirEvidenceReportSubjectCharacteristicList = FHIR.R5.Resources.Other.TFhirEvidenceReportSubjectCharacteristicList;
  TFhirEvidenceReportRelatesTo = FHIR.R5.Resources.Other.TFhirEvidenceReportRelatesTo;
  TFhirEvidenceReportRelatesToList = FHIR.R5.Resources.Other.TFhirEvidenceReportRelatesToList;
  TFhirEvidenceReportSection = FHIR.R5.Resources.Other.TFhirEvidenceReportSection;
  TFhirEvidenceReportSectionList = FHIR.R5.Resources.Other.TFhirEvidenceReportSectionList;
  TFhirEvidenceReport = FHIR.R5.Resources.Other.TFhirEvidenceReport;
  TFhirEvidenceReportList = FHIR.R5.Resources.Other.TFhirEvidenceReportList;
{$ENDIF FHIR_EVIDENCEREPORT}
{$IFDEF FHIR_EVIDENCEVARIABLE}
  TFhirEvidenceVariableCharacteristic = FHIR.R5.Resources.Other.TFhirEvidenceVariableCharacteristic;
  TFhirEvidenceVariableCharacteristicList = FHIR.R5.Resources.Other.TFhirEvidenceVariableCharacteristicList;
  TFhirEvidenceVariableCharacteristicTimeFromStart = FHIR.R5.Resources.Other.TFhirEvidenceVariableCharacteristicTimeFromStart;
  TFhirEvidenceVariableCharacteristicTimeFromStartList = FHIR.R5.Resources.Other.TFhirEvidenceVariableCharacteristicTimeFromStartList;
  TFhirEvidenceVariableCategory = FHIR.R5.Resources.Other.TFhirEvidenceVariableCategory;
  TFhirEvidenceVariableCategoryList = FHIR.R5.Resources.Other.TFhirEvidenceVariableCategoryList;
  TFhirEvidenceVariable = FHIR.R5.Resources.Other.TFhirEvidenceVariable;
  TFhirEvidenceVariableList = FHIR.R5.Resources.Other.TFhirEvidenceVariableList;
{$ENDIF FHIR_EVIDENCEVARIABLE}
{$IFDEF FHIR_GUIDANCERESPONSE}
  TFhirGuidanceResponse = FHIR.R5.Resources.Other.TFhirGuidanceResponse;
  TFhirGuidanceResponseList = FHIR.R5.Resources.Other.TFhirGuidanceResponseList;
{$ENDIF FHIR_GUIDANCERESPONSE}
{$IFDEF FHIR_LINKAGE}
  TFhirLinkageItem = FHIR.R5.Resources.Other.TFhirLinkageItem;
  TFhirLinkageItemList = FHIR.R5.Resources.Other.TFhirLinkageItemList;
  TFhirLinkage = FHIR.R5.Resources.Other.TFhirLinkage;
  TFhirLinkageList = FHIR.R5.Resources.Other.TFhirLinkageList;
{$ENDIF FHIR_LINKAGE}
{$IFDEF FHIR_LIST}
  TFhirListEntry = FHIR.R5.Resources.Other.TFhirListEntry;
  TFhirListEntryList = FHIR.R5.Resources.Other.TFhirListEntryList;
  TFhirList = FHIR.R5.Resources.Other.TFhirList;
  TFhirListList = FHIR.R5.Resources.Other.TFhirListList;
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_MEASUREREPORT}
  TFhirMeasureReportGroup = FHIR.R5.Resources.Other.TFhirMeasureReportGroup;
  TFhirMeasureReportGroupList = FHIR.R5.Resources.Other.TFhirMeasureReportGroupList;
  TFhirMeasureReportGroupPopulation = FHIR.R5.Resources.Other.TFhirMeasureReportGroupPopulation;
  TFhirMeasureReportGroupPopulationList = FHIR.R5.Resources.Other.TFhirMeasureReportGroupPopulationList;
  TFhirMeasureReportGroupStratifier = FHIR.R5.Resources.Other.TFhirMeasureReportGroupStratifier;
  TFhirMeasureReportGroupStratifierList = FHIR.R5.Resources.Other.TFhirMeasureReportGroupStratifierList;
  TFhirMeasureReportGroupStratifierStratum = FHIR.R5.Resources.Other.TFhirMeasureReportGroupStratifierStratum;
  TFhirMeasureReportGroupStratifierStratumList = FHIR.R5.Resources.Other.TFhirMeasureReportGroupStratifierStratumList;
  TFhirMeasureReportGroupStratifierStratumComponent = FHIR.R5.Resources.Other.TFhirMeasureReportGroupStratifierStratumComponent;
  TFhirMeasureReportGroupStratifierStratumComponentList = FHIR.R5.Resources.Other.TFhirMeasureReportGroupStratifierStratumComponentList;
  TFhirMeasureReportGroupStratifierStratumPopulation = FHIR.R5.Resources.Other.TFhirMeasureReportGroupStratifierStratumPopulation;
  TFhirMeasureReportGroupStratifierStratumPopulationList = FHIR.R5.Resources.Other.TFhirMeasureReportGroupStratifierStratumPopulationList;
  TFhirMeasureReport = FHIR.R5.Resources.Other.TFhirMeasureReport;
  TFhirMeasureReportList = FHIR.R5.Resources.Other.TFhirMeasureReportList;
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MESSAGEHEADER}
  TFhirMessageHeaderDestination = FHIR.R5.Resources.Other.TFhirMessageHeaderDestination;
  TFhirMessageHeaderDestinationList = FHIR.R5.Resources.Other.TFhirMessageHeaderDestinationList;
  TFhirMessageHeaderSource = FHIR.R5.Resources.Other.TFhirMessageHeaderSource;
  TFhirMessageHeaderSourceList = FHIR.R5.Resources.Other.TFhirMessageHeaderSourceList;
  TFhirMessageHeaderResponse = FHIR.R5.Resources.Other.TFhirMessageHeaderResponse;
  TFhirMessageHeaderResponseList = FHIR.R5.Resources.Other.TFhirMessageHeaderResponseList;
  TFhirMessageHeader = FHIR.R5.Resources.Other.TFhirMessageHeader;
  TFhirMessageHeaderList = FHIR.R5.Resources.Other.TFhirMessageHeaderList;
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_OPERATIONOUTCOME}
  TFhirOperationOutcomeIssue = FHIR.R5.Resources.Other.TFhirOperationOutcomeIssue;
  TFhirOperationOutcomeIssueList = FHIR.R5.Resources.Other.TFhirOperationOutcomeIssueList;
  TFhirOperationOutcome = FHIR.R5.Resources.Other.TFhirOperationOutcome;
  TFhirOperationOutcomeList = FHIR.R5.Resources.Other.TFhirOperationOutcomeList;
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_PARAMETERS}
  TFhirParametersParameter = FHIR.R5.Resources.Other.TFhirParametersParameter;
  TFhirParametersParameterList = FHIR.R5.Resources.Other.TFhirParametersParameterList;
  TFhirParameters = FHIR.R5.Resources.Other.TFhirParameters;
  TFhirParametersList = FHIR.R5.Resources.Other.TFhirParametersList;
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_PERMISSION}
  TFhirPermissionProcessingActivity = FHIR.R5.Resources.Other.TFhirPermissionProcessingActivity;
  TFhirPermissionProcessingActivityList = FHIR.R5.Resources.Other.TFhirPermissionProcessingActivityList;
  TFhirPermissionJustification = FHIR.R5.Resources.Other.TFhirPermissionJustification;
  TFhirPermissionJustificationList = FHIR.R5.Resources.Other.TFhirPermissionJustificationList;
  TFhirPermission = FHIR.R5.Resources.Other.TFhirPermission;
  TFhirPermissionList = FHIR.R5.Resources.Other.TFhirPermissionList;
{$ENDIF FHIR_PERMISSION}
{$IFDEF FHIR_PROVENANCE}
  TFhirProvenanceAgent = FHIR.R5.Resources.Other.TFhirProvenanceAgent;
  TFhirProvenanceAgentList = FHIR.R5.Resources.Other.TFhirProvenanceAgentList;
  TFhirProvenanceEntity = FHIR.R5.Resources.Other.TFhirProvenanceEntity;
  TFhirProvenanceEntityList = FHIR.R5.Resources.Other.TFhirProvenanceEntityList;
  TFhirProvenance = FHIR.R5.Resources.Other.TFhirProvenance;
  TFhirProvenanceList = FHIR.R5.Resources.Other.TFhirProvenanceList;
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  TFhirQuestionnaireResponseItem = FHIR.R5.Resources.Other.TFhirQuestionnaireResponseItem;
  TFhirQuestionnaireResponseItemList = FHIR.R5.Resources.Other.TFhirQuestionnaireResponseItemList;
  TFhirQuestionnaireResponseItemAnswer = FHIR.R5.Resources.Other.TFhirQuestionnaireResponseItemAnswer;
  TFhirQuestionnaireResponseItemAnswerList = FHIR.R5.Resources.Other.TFhirQuestionnaireResponseItemAnswerList;
  TFhirQuestionnaireResponse = FHIR.R5.Resources.Other.TFhirQuestionnaireResponse;
  TFhirQuestionnaireResponseList = FHIR.R5.Resources.Other.TFhirQuestionnaireResponseList;
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_RESEARCHSTUDY}
  TFhirResearchStudyArm = FHIR.R5.Resources.Other.TFhirResearchStudyArm;
  TFhirResearchStudyArmList = FHIR.R5.Resources.Other.TFhirResearchStudyArmList;
  TFhirResearchStudyObjective = FHIR.R5.Resources.Other.TFhirResearchStudyObjective;
  TFhirResearchStudyObjectiveList = FHIR.R5.Resources.Other.TFhirResearchStudyObjectiveList;
  TFhirResearchStudy = FHIR.R5.Resources.Other.TFhirResearchStudy;
  TFhirResearchStudyList = FHIR.R5.Resources.Other.TFhirResearchStudyList;
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  TFhirResearchSubjectProgress = FHIR.R5.Resources.Other.TFhirResearchSubjectProgress;
  TFhirResearchSubjectProgressList = FHIR.R5.Resources.Other.TFhirResearchSubjectProgressList;
  TFhirResearchSubject = FHIR.R5.Resources.Other.TFhirResearchSubject;
  TFhirResearchSubjectList = FHIR.R5.Resources.Other.TFhirResearchSubjectList;
{$ENDIF FHIR_RESEARCHSUBJECT}
{$IFDEF FHIR_SUBSCRIPTION}
  TFhirSubscriptionFilterBy = FHIR.R5.Resources.Other.TFhirSubscriptionFilterBy;
  TFhirSubscriptionFilterByList = FHIR.R5.Resources.Other.TFhirSubscriptionFilterByList;
  TFhirSubscription = FHIR.R5.Resources.Other.TFhirSubscription;
  TFhirSubscriptionList = FHIR.R5.Resources.Other.TFhirSubscriptionList;
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
  TFhirSubscriptionStatus = FHIR.R5.Resources.Other.TFhirSubscriptionStatus;
  TFhirSubscriptionStatusList = FHIR.R5.Resources.Other.TFhirSubscriptionStatusList;
{$ENDIF FHIR_SUBSCRIPTIONSTATUS}
{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
  TFhirSubscriptionTopicResourceTrigger = FHIR.R5.Resources.Other.TFhirSubscriptionTopicResourceTrigger;
  TFhirSubscriptionTopicResourceTriggerList = FHIR.R5.Resources.Other.TFhirSubscriptionTopicResourceTriggerList;
  TFhirSubscriptionTopicResourceTriggerQueryCriteria = FHIR.R5.Resources.Other.TFhirSubscriptionTopicResourceTriggerQueryCriteria;
  TFhirSubscriptionTopicResourceTriggerQueryCriteriaList = FHIR.R5.Resources.Other.TFhirSubscriptionTopicResourceTriggerQueryCriteriaList;
  TFhirSubscriptionTopicCanFilterBy = FHIR.R5.Resources.Other.TFhirSubscriptionTopicCanFilterBy;
  TFhirSubscriptionTopicCanFilterByList = FHIR.R5.Resources.Other.TFhirSubscriptionTopicCanFilterByList;
  TFhirSubscriptionTopic = FHIR.R5.Resources.Other.TFhirSubscriptionTopic;
  TFhirSubscriptionTopicList = FHIR.R5.Resources.Other.TFhirSubscriptionTopicList;
{$ENDIF FHIR_SUBSCRIPTIONTOPIC}
{$IFDEF FHIR_TASK}
  TFhirTaskRestriction = FHIR.R5.Resources.Other.TFhirTaskRestriction;
  TFhirTaskRestrictionList = FHIR.R5.Resources.Other.TFhirTaskRestrictionList;
  TFhirTaskInput = FHIR.R5.Resources.Other.TFhirTaskInput;
  TFhirTaskInputList = FHIR.R5.Resources.Other.TFhirTaskInputList;
  TFhirTaskOutput = FHIR.R5.Resources.Other.TFhirTaskOutput;
  TFhirTaskOutputList = FHIR.R5.Resources.Other.TFhirTaskOutputList;
  TFhirTask = FHIR.R5.Resources.Other.TFhirTask;
  TFhirTaskList = FHIR.R5.Resources.Other.TFhirTaskList;
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TESTREPORT}
  TFhirTestReportParticipant = FHIR.R5.Resources.Other.TFhirTestReportParticipant;
  TFhirTestReportParticipantList = FHIR.R5.Resources.Other.TFhirTestReportParticipantList;
  TFhirTestReportSetup = FHIR.R5.Resources.Other.TFhirTestReportSetup;
  TFhirTestReportSetupList = FHIR.R5.Resources.Other.TFhirTestReportSetupList;
  TFhirTestReportSetupAction = FHIR.R5.Resources.Other.TFhirTestReportSetupAction;
  TFhirTestReportSetupActionList = FHIR.R5.Resources.Other.TFhirTestReportSetupActionList;
  TFhirTestReportSetupActionOperation = FHIR.R5.Resources.Other.TFhirTestReportSetupActionOperation;
  TFhirTestReportSetupActionOperationList = FHIR.R5.Resources.Other.TFhirTestReportSetupActionOperationList;
  TFhirTestReportSetupActionAssert = FHIR.R5.Resources.Other.TFhirTestReportSetupActionAssert;
  TFhirTestReportSetupActionAssertList = FHIR.R5.Resources.Other.TFhirTestReportSetupActionAssertList;
  TFhirTestReportTest = FHIR.R5.Resources.Other.TFhirTestReportTest;
  TFhirTestReportTestList = FHIR.R5.Resources.Other.TFhirTestReportTestList;
  TFhirTestReportTestAction = FHIR.R5.Resources.Other.TFhirTestReportTestAction;
  TFhirTestReportTestActionList = FHIR.R5.Resources.Other.TFhirTestReportTestActionList;
  TFhirTestReportTeardown = FHIR.R5.Resources.Other.TFhirTestReportTeardown;
  TFhirTestReportTeardownList = FHIR.R5.Resources.Other.TFhirTestReportTeardownList;
  TFhirTestReportTeardownAction = FHIR.R5.Resources.Other.TFhirTestReportTeardownAction;
  TFhirTestReportTeardownActionList = FHIR.R5.Resources.Other.TFhirTestReportTeardownActionList;
  TFhirTestReport = FHIR.R5.Resources.Other.TFhirTestReport;
  TFhirTestReportList = FHIR.R5.Resources.Other.TFhirTestReportList;
{$ENDIF FHIR_TESTREPORT}
{$IFDEF FHIR_VERIFICATIONRESULT}
  TFhirVerificationResultPrimarySource = FHIR.R5.Resources.Other.TFhirVerificationResultPrimarySource;
  TFhirVerificationResultPrimarySourceList = FHIR.R5.Resources.Other.TFhirVerificationResultPrimarySourceList;
  TFhirVerificationResultAttestation = FHIR.R5.Resources.Other.TFhirVerificationResultAttestation;
  TFhirVerificationResultAttestationList = FHIR.R5.Resources.Other.TFhirVerificationResultAttestationList;
  TFhirVerificationResultValidator = FHIR.R5.Resources.Other.TFhirVerificationResultValidator;
  TFhirVerificationResultValidatorList = FHIR.R5.Resources.Other.TFhirVerificationResultValidatorList;
  TFhirVerificationResult = FHIR.R5.Resources.Other.TFhirVerificationResult;
  TFhirVerificationResultList = FHIR.R5.Resources.Other.TFhirVerificationResultList;
{$ENDIF FHIR_VERIFICATIONRESULT}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  TFhirActivityDefinitionParticipant = FHIR.R5.Resources.Other.TFhirActivityDefinitionParticipant;
  TFhirActivityDefinitionParticipantList = FHIR.R5.Resources.Other.TFhirActivityDefinitionParticipantList;
  TFhirActivityDefinitionDynamicValue = FHIR.R5.Resources.Other.TFhirActivityDefinitionDynamicValue;
  TFhirActivityDefinitionDynamicValueList = FHIR.R5.Resources.Other.TFhirActivityDefinitionDynamicValueList;
  TFhirActivityDefinition = FHIR.R5.Resources.Other.TFhirActivityDefinition;
  TFhirActivityDefinitionList = FHIR.R5.Resources.Other.TFhirActivityDefinitionList;
{$ENDIF FHIR_ACTIVITYDEFINITION}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  TFhirCapabilityStatementSoftware = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementSoftware;
  TFhirCapabilityStatementSoftwareList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementSoftwareList;
  TFhirCapabilityStatementImplementation = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementImplementation;
  TFhirCapabilityStatementImplementationList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementImplementationList;
  TFhirCapabilityStatementRest = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRest;
  TFhirCapabilityStatementRestList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestList;
  TFhirCapabilityStatementRestSecurity = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestSecurity;
  TFhirCapabilityStatementRestSecurityList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestSecurityList;
  TFhirCapabilityStatementRestResource = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestResource;
  TFhirCapabilityStatementRestResourceList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestResourceList;
  TFhirCapabilityStatementRestResourceInteraction = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestResourceInteraction;
  TFhirCapabilityStatementRestResourceInteractionList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestResourceInteractionList;
  TFhirCapabilityStatementRestResourceSearchParam = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestResourceSearchParam;
  TFhirCapabilityStatementRestResourceSearchParamList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestResourceSearchParamList;
  TFhirCapabilityStatementRestResourceOperation = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestResourceOperation;
  TFhirCapabilityStatementRestResourceOperationList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestResourceOperationList;
  TFhirCapabilityStatementRestInteraction = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestInteraction;
  TFhirCapabilityStatementRestInteractionList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementRestInteractionList;
  TFhirCapabilityStatementMessaging = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementMessaging;
  TFhirCapabilityStatementMessagingList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementMessagingList;
  TFhirCapabilityStatementMessagingEndpoint = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementMessagingEndpoint;
  TFhirCapabilityStatementMessagingEndpointList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementMessagingEndpointList;
  TFhirCapabilityStatementMessagingSupportedMessage = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementMessagingSupportedMessage;
  TFhirCapabilityStatementMessagingSupportedMessageList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementMessagingSupportedMessageList;
  TFhirCapabilityStatementDocument = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementDocument;
  TFhirCapabilityStatementDocumentList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementDocumentList;
  TFhirCapabilityStatement = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement;
  TFhirCapabilityStatementList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatementList;
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CAPABILITYSTATEMENT2}
  TFhirCapabilityStatement2Software = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2Software;
  TFhirCapabilityStatement2SoftwareList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2SoftwareList;
  TFhirCapabilityStatement2Implementation = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2Implementation;
  TFhirCapabilityStatement2ImplementationList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2ImplementationList;
  TFhirCapabilityStatement2Rest = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2Rest;
  TFhirCapabilityStatement2RestList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestList;
  TFhirCapabilityStatement2RestResource = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestResource;
  TFhirCapabilityStatement2RestResourceList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestResourceList;
  TFhirCapabilityStatement2RestResourceInteraction = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestResourceInteraction;
  TFhirCapabilityStatement2RestResourceInteractionList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestResourceInteractionList;
  TFhirCapabilityStatement2RestResourceSearchParam = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestResourceSearchParam;
  TFhirCapabilityStatement2RestResourceSearchParamList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestResourceSearchParamList;
  TFhirCapabilityStatement2RestResourceOperation = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestResourceOperation;
  TFhirCapabilityStatement2RestResourceOperationList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestResourceOperationList;
  TFhirCapabilityStatement2RestInteraction = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestInteraction;
  TFhirCapabilityStatement2RestInteractionList = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2RestInteractionList;
  TFhirCapabilityStatement2 = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2;
  TFhirCapabilityStatement2List = FHIR.R5.Resources.Canonical.TFhirCapabilityStatement2List;
{$ENDIF FHIR_CAPABILITYSTATEMENT2}
{$IFDEF FHIR_CODESYSTEM}
  TFhirCodeSystemFilter = FHIR.R5.Resources.Canonical.TFhirCodeSystemFilter;
  TFhirCodeSystemFilterList = FHIR.R5.Resources.Canonical.TFhirCodeSystemFilterList;
  TFhirCodeSystemProperty = FHIR.R5.Resources.Canonical.TFhirCodeSystemProperty;
  TFhirCodeSystemPropertyList = FHIR.R5.Resources.Canonical.TFhirCodeSystemPropertyList;
  TFhirCodeSystemConcept = FHIR.R5.Resources.Canonical.TFhirCodeSystemConcept;
  TFhirCodeSystemConceptList = FHIR.R5.Resources.Canonical.TFhirCodeSystemConceptList;
  TFhirCodeSystemConceptDesignation = FHIR.R5.Resources.Canonical.TFhirCodeSystemConceptDesignation;
  TFhirCodeSystemConceptDesignationList = FHIR.R5.Resources.Canonical.TFhirCodeSystemConceptDesignationList;
  TFhirCodeSystemConceptProperty = FHIR.R5.Resources.Canonical.TFhirCodeSystemConceptProperty;
  TFhirCodeSystemConceptPropertyList = FHIR.R5.Resources.Canonical.TFhirCodeSystemConceptPropertyList;
  TFhirCodeSystem = FHIR.R5.Resources.Canonical.TFhirCodeSystem;
  TFhirCodeSystemList = FHIR.R5.Resources.Canonical.TFhirCodeSystemList;
{$ENDIF FHIR_CODESYSTEM}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  TFhirCompartmentDefinitionResource = FHIR.R5.Resources.Canonical.TFhirCompartmentDefinitionResource;
  TFhirCompartmentDefinitionResourceList = FHIR.R5.Resources.Canonical.TFhirCompartmentDefinitionResourceList;
  TFhirCompartmentDefinition = FHIR.R5.Resources.Canonical.TFhirCompartmentDefinition;
  TFhirCompartmentDefinitionList = FHIR.R5.Resources.Canonical.TFhirCompartmentDefinitionList;
{$ENDIF FHIR_COMPARTMENTDEFINITION}
{$IFDEF FHIR_CONCEPTMAP}
  TFhirConceptMapGroup = FHIR.R5.Resources.Canonical.TFhirConceptMapGroup;
  TFhirConceptMapGroupList = FHIR.R5.Resources.Canonical.TFhirConceptMapGroupList;
  TFhirConceptMapGroupElement = FHIR.R5.Resources.Canonical.TFhirConceptMapGroupElement;
  TFhirConceptMapGroupElementList = FHIR.R5.Resources.Canonical.TFhirConceptMapGroupElementList;
  TFhirConceptMapGroupElementTarget = FHIR.R5.Resources.Canonical.TFhirConceptMapGroupElementTarget;
  TFhirConceptMapGroupElementTargetList = FHIR.R5.Resources.Canonical.TFhirConceptMapGroupElementTargetList;
  TFhirConceptMapGroupElementTargetDependsOn = FHIR.R5.Resources.Canonical.TFhirConceptMapGroupElementTargetDependsOn;
  TFhirConceptMapGroupElementTargetDependsOnList = FHIR.R5.Resources.Canonical.TFhirConceptMapGroupElementTargetDependsOnList;
  TFhirConceptMapGroupUnmapped = FHIR.R5.Resources.Canonical.TFhirConceptMapGroupUnmapped;
  TFhirConceptMapGroupUnmappedList = FHIR.R5.Resources.Canonical.TFhirConceptMapGroupUnmappedList;
  TFhirConceptMap = FHIR.R5.Resources.Canonical.TFhirConceptMap;
  TFhirConceptMapList = FHIR.R5.Resources.Canonical.TFhirConceptMapList;
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_CONDITIONDEFINITION}
  TFhirConditionDefinitionObservation = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionObservation;
  TFhirConditionDefinitionObservationList = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionObservationList;
  TFhirConditionDefinitionMedication = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionMedication;
  TFhirConditionDefinitionMedicationList = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionMedicationList;
  TFhirConditionDefinitionPrecondition = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionPrecondition;
  TFhirConditionDefinitionPreconditionList = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionPreconditionList;
  TFhirConditionDefinitionQuestionnaire = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionQuestionnaire;
  TFhirConditionDefinitionQuestionnaireList = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionQuestionnaireList;
  TFhirConditionDefinitionPlan = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionPlan;
  TFhirConditionDefinitionPlanList = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionPlanList;
  TFhirConditionDefinition = FHIR.R5.Resources.Canonical.TFhirConditionDefinition;
  TFhirConditionDefinitionList = FHIR.R5.Resources.Canonical.TFhirConditionDefinitionList;
{$ENDIF FHIR_CONDITIONDEFINITION}
{$IFDEF FHIR_EVENTDEFINITION}
  TFhirEventDefinition = FHIR.R5.Resources.Canonical.TFhirEventDefinition;
  TFhirEventDefinitionList = FHIR.R5.Resources.Canonical.TFhirEventDefinitionList;
{$ENDIF FHIR_EVENTDEFINITION}
{$IFDEF FHIR_EXAMPLESCENARIO}
  TFhirExampleScenarioActor = FHIR.R5.Resources.Canonical.TFhirExampleScenarioActor;
  TFhirExampleScenarioActorList = FHIR.R5.Resources.Canonical.TFhirExampleScenarioActorList;
  TFhirExampleScenarioInstance = FHIR.R5.Resources.Canonical.TFhirExampleScenarioInstance;
  TFhirExampleScenarioInstanceList = FHIR.R5.Resources.Canonical.TFhirExampleScenarioInstanceList;
  TFhirExampleScenarioInstanceVersion = FHIR.R5.Resources.Canonical.TFhirExampleScenarioInstanceVersion;
  TFhirExampleScenarioInstanceVersionList = FHIR.R5.Resources.Canonical.TFhirExampleScenarioInstanceVersionList;
  TFhirExampleScenarioInstanceContainedInstance = FHIR.R5.Resources.Canonical.TFhirExampleScenarioInstanceContainedInstance;
  TFhirExampleScenarioInstanceContainedInstanceList = FHIR.R5.Resources.Canonical.TFhirExampleScenarioInstanceContainedInstanceList;
  TFhirExampleScenarioProcess = FHIR.R5.Resources.Canonical.TFhirExampleScenarioProcess;
  TFhirExampleScenarioProcessList = FHIR.R5.Resources.Canonical.TFhirExampleScenarioProcessList;
  TFhirExampleScenarioProcessStep = FHIR.R5.Resources.Canonical.TFhirExampleScenarioProcessStep;
  TFhirExampleScenarioProcessStepList = FHIR.R5.Resources.Canonical.TFhirExampleScenarioProcessStepList;
  TFhirExampleScenarioProcessStepOperation = FHIR.R5.Resources.Canonical.TFhirExampleScenarioProcessStepOperation;
  TFhirExampleScenarioProcessStepOperationList = FHIR.R5.Resources.Canonical.TFhirExampleScenarioProcessStepOperationList;
  TFhirExampleScenarioProcessStepAlternative = FHIR.R5.Resources.Canonical.TFhirExampleScenarioProcessStepAlternative;
  TFhirExampleScenarioProcessStepAlternativeList = FHIR.R5.Resources.Canonical.TFhirExampleScenarioProcessStepAlternativeList;
  TFhirExampleScenario = FHIR.R5.Resources.Canonical.TFhirExampleScenario;
  TFhirExampleScenarioList = FHIR.R5.Resources.Canonical.TFhirExampleScenarioList;
{$ENDIF FHIR_EXAMPLESCENARIO}
{$IFDEF FHIR_GRAPHDEFINITION}
  TFhirGraphDefinitionLink = FHIR.R5.Resources.Canonical.TFhirGraphDefinitionLink;
  TFhirGraphDefinitionLinkList = FHIR.R5.Resources.Canonical.TFhirGraphDefinitionLinkList;
  TFhirGraphDefinitionLinkTarget = FHIR.R5.Resources.Canonical.TFhirGraphDefinitionLinkTarget;
  TFhirGraphDefinitionLinkTargetList = FHIR.R5.Resources.Canonical.TFhirGraphDefinitionLinkTargetList;
  TFhirGraphDefinitionLinkTargetCompartment = FHIR.R5.Resources.Canonical.TFhirGraphDefinitionLinkTargetCompartment;
  TFhirGraphDefinitionLinkTargetCompartmentList = FHIR.R5.Resources.Canonical.TFhirGraphDefinitionLinkTargetCompartmentList;
  TFhirGraphDefinition = FHIR.R5.Resources.Canonical.TFhirGraphDefinition;
  TFhirGraphDefinitionList = FHIR.R5.Resources.Canonical.TFhirGraphDefinitionList;
{$ENDIF FHIR_GRAPHDEFINITION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  TFhirImplementationGuideDependsOn = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDependsOn;
  TFhirImplementationGuideDependsOnList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDependsOnList;
  TFhirImplementationGuideGlobal = FHIR.R5.Resources.Canonical.TFhirImplementationGuideGlobal;
  TFhirImplementationGuideGlobalList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideGlobalList;
  TFhirImplementationGuideDefinition = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinition;
  TFhirImplementationGuideDefinitionList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionList;
  TFhirImplementationGuideDefinitionGrouping = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionGrouping;
  TFhirImplementationGuideDefinitionGroupingList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionGroupingList;
  TFhirImplementationGuideDefinitionResource = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionResource;
  TFhirImplementationGuideDefinitionResourceList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionResourceList;
  TFhirImplementationGuideDefinitionPage = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionPage;
  TFhirImplementationGuideDefinitionPageList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionPageList;
  TFhirImplementationGuideDefinitionParameter = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionParameter;
  TFhirImplementationGuideDefinitionParameterList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionParameterList;
  TFhirImplementationGuideDefinitionTemplate = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionTemplate;
  TFhirImplementationGuideDefinitionTemplateList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideDefinitionTemplateList;
  TFhirImplementationGuideManifest = FHIR.R5.Resources.Canonical.TFhirImplementationGuideManifest;
  TFhirImplementationGuideManifestList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideManifestList;
  TFhirImplementationGuideManifestResource = FHIR.R5.Resources.Canonical.TFhirImplementationGuideManifestResource;
  TFhirImplementationGuideManifestResourceList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideManifestResourceList;
  TFhirImplementationGuideManifestPage = FHIR.R5.Resources.Canonical.TFhirImplementationGuideManifestPage;
  TFhirImplementationGuideManifestPageList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideManifestPageList;
  TFhirImplementationGuide = FHIR.R5.Resources.Canonical.TFhirImplementationGuide;
  TFhirImplementationGuideList = FHIR.R5.Resources.Canonical.TFhirImplementationGuideList;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_LIBRARY}
  TFhirLibrary = FHIR.R5.Resources.Canonical.TFhirLibrary;
  TFhirLibraryList = FHIR.R5.Resources.Canonical.TFhirLibraryList;
{$ENDIF FHIR_LIBRARY}
{$IFDEF FHIR_MEASURE}
  TFhirMeasureGroup = FHIR.R5.Resources.Canonical.TFhirMeasureGroup;
  TFhirMeasureGroupList = FHIR.R5.Resources.Canonical.TFhirMeasureGroupList;
  TFhirMeasureGroupPopulation = FHIR.R5.Resources.Canonical.TFhirMeasureGroupPopulation;
  TFhirMeasureGroupPopulationList = FHIR.R5.Resources.Canonical.TFhirMeasureGroupPopulationList;
  TFhirMeasureGroupStratifier = FHIR.R5.Resources.Canonical.TFhirMeasureGroupStratifier;
  TFhirMeasureGroupStratifierList = FHIR.R5.Resources.Canonical.TFhirMeasureGroupStratifierList;
  TFhirMeasureGroupStratifierComponent = FHIR.R5.Resources.Canonical.TFhirMeasureGroupStratifierComponent;
  TFhirMeasureGroupStratifierComponentList = FHIR.R5.Resources.Canonical.TFhirMeasureGroupStratifierComponentList;
  TFhirMeasureSupplementalData = FHIR.R5.Resources.Canonical.TFhirMeasureSupplementalData;
  TFhirMeasureSupplementalDataList = FHIR.R5.Resources.Canonical.TFhirMeasureSupplementalDataList;
  TFhirMeasure = FHIR.R5.Resources.Canonical.TFhirMeasure;
  TFhirMeasureList = FHIR.R5.Resources.Canonical.TFhirMeasureList;
{$ENDIF FHIR_MEASURE}
{$IFDEF FHIR_MESSAGEDEFINITION}
  TFhirMessageDefinitionFocus = FHIR.R5.Resources.Canonical.TFhirMessageDefinitionFocus;
  TFhirMessageDefinitionFocusList = FHIR.R5.Resources.Canonical.TFhirMessageDefinitionFocusList;
  TFhirMessageDefinitionAllowedResponse = FHIR.R5.Resources.Canonical.TFhirMessageDefinitionAllowedResponse;
  TFhirMessageDefinitionAllowedResponseList = FHIR.R5.Resources.Canonical.TFhirMessageDefinitionAllowedResponseList;
  TFhirMessageDefinition = FHIR.R5.Resources.Canonical.TFhirMessageDefinition;
  TFhirMessageDefinitionList = FHIR.R5.Resources.Canonical.TFhirMessageDefinitionList;
{$ENDIF FHIR_MESSAGEDEFINITION}
{$IFDEF FHIR_NAMINGSYSTEM}
  TFhirNamingSystemUniqueId = FHIR.R5.Resources.Canonical.TFhirNamingSystemUniqueId;
  TFhirNamingSystemUniqueIdList = FHIR.R5.Resources.Canonical.TFhirNamingSystemUniqueIdList;
  TFhirNamingSystem = FHIR.R5.Resources.Canonical.TFhirNamingSystem;
  TFhirNamingSystemList = FHIR.R5.Resources.Canonical.TFhirNamingSystemList;
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_OBSERVATIONDEFINITION}
  TFhirObservationDefinitionQuantitativeDetails = FHIR.R5.Resources.Canonical.TFhirObservationDefinitionQuantitativeDetails;
  TFhirObservationDefinitionQuantitativeDetailsList = FHIR.R5.Resources.Canonical.TFhirObservationDefinitionQuantitativeDetailsList;
  TFhirObservationDefinitionQualifiedInterval = FHIR.R5.Resources.Canonical.TFhirObservationDefinitionQualifiedInterval;
  TFhirObservationDefinitionQualifiedIntervalList = FHIR.R5.Resources.Canonical.TFhirObservationDefinitionQualifiedIntervalList;
  TFhirObservationDefinitionComponent = FHIR.R5.Resources.Canonical.TFhirObservationDefinitionComponent;
  TFhirObservationDefinitionComponentList = FHIR.R5.Resources.Canonical.TFhirObservationDefinitionComponentList;
  TFhirObservationDefinition = FHIR.R5.Resources.Canonical.TFhirObservationDefinition;
  TFhirObservationDefinitionList = FHIR.R5.Resources.Canonical.TFhirObservationDefinitionList;
{$ENDIF FHIR_OBSERVATIONDEFINITION}
{$IFDEF FHIR_OPERATIONDEFINITION}
  TFhirOperationDefinitionParameter = FHIR.R5.Resources.Canonical.TFhirOperationDefinitionParameter;
  TFhirOperationDefinitionParameterList = FHIR.R5.Resources.Canonical.TFhirOperationDefinitionParameterList;
  TFhirOperationDefinitionParameterBinding = FHIR.R5.Resources.Canonical.TFhirOperationDefinitionParameterBinding;
  TFhirOperationDefinitionParameterBindingList = FHIR.R5.Resources.Canonical.TFhirOperationDefinitionParameterBindingList;
  TFhirOperationDefinitionParameterReferencedFrom = FHIR.R5.Resources.Canonical.TFhirOperationDefinitionParameterReferencedFrom;
  TFhirOperationDefinitionParameterReferencedFromList = FHIR.R5.Resources.Canonical.TFhirOperationDefinitionParameterReferencedFromList;
  TFhirOperationDefinitionOverload = FHIR.R5.Resources.Canonical.TFhirOperationDefinitionOverload;
  TFhirOperationDefinitionOverloadList = FHIR.R5.Resources.Canonical.TFhirOperationDefinitionOverloadList;
  TFhirOperationDefinition = FHIR.R5.Resources.Canonical.TFhirOperationDefinition;
  TFhirOperationDefinitionList = FHIR.R5.Resources.Canonical.TFhirOperationDefinitionList;
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_PLANDEFINITION}
  TFhirPlanDefinitionGoal = FHIR.R5.Resources.Other.TFhirPlanDefinitionGoal;
  TFhirPlanDefinitionGoalList = FHIR.R5.Resources.Other.TFhirPlanDefinitionGoalList;
  TFhirPlanDefinitionGoalTarget = FHIR.R5.Resources.Other.TFhirPlanDefinitionGoalTarget;
  TFhirPlanDefinitionGoalTargetList = FHIR.R5.Resources.Other.TFhirPlanDefinitionGoalTargetList;
  TFhirPlanDefinitionAction = FHIR.R5.Resources.Other.TFhirPlanDefinitionAction;
  TFhirPlanDefinitionActionList = FHIR.R5.Resources.Other.TFhirPlanDefinitionActionList;
  TFhirPlanDefinitionActionCondition = FHIR.R5.Resources.Other.TFhirPlanDefinitionActionCondition;
  TFhirPlanDefinitionActionConditionList = FHIR.R5.Resources.Other.TFhirPlanDefinitionActionConditionList;
  TFhirPlanDefinitionActionRelatedAction = FHIR.R5.Resources.Other.TFhirPlanDefinitionActionRelatedAction;
  TFhirPlanDefinitionActionRelatedActionList = FHIR.R5.Resources.Other.TFhirPlanDefinitionActionRelatedActionList;
  TFhirPlanDefinitionActionParticipant = FHIR.R5.Resources.Other.TFhirPlanDefinitionActionParticipant;
  TFhirPlanDefinitionActionParticipantList = FHIR.R5.Resources.Other.TFhirPlanDefinitionActionParticipantList;
  TFhirPlanDefinitionActionDynamicValue = FHIR.R5.Resources.Other.TFhirPlanDefinitionActionDynamicValue;
  TFhirPlanDefinitionActionDynamicValueList = FHIR.R5.Resources.Other.TFhirPlanDefinitionActionDynamicValueList;
  TFhirPlanDefinition = FHIR.R5.Resources.Other.TFhirPlanDefinition;
  TFhirPlanDefinitionList = FHIR.R5.Resources.Other.TFhirPlanDefinitionList;
{$ENDIF FHIR_PLANDEFINITION}
{$IFDEF FHIR_QUESTIONNAIRE}
  TFhirQuestionnaireItem = FHIR.R5.Resources.Canonical.TFhirQuestionnaireItem;
  TFhirQuestionnaireItemList = FHIR.R5.Resources.Canonical.TFhirQuestionnaireItemList;
  TFhirQuestionnaireItemEnableWhen = FHIR.R5.Resources.Canonical.TFhirQuestionnaireItemEnableWhen;
  TFhirQuestionnaireItemEnableWhenList = FHIR.R5.Resources.Canonical.TFhirQuestionnaireItemEnableWhenList;
  TFhirQuestionnaireItemAnswerOption = FHIR.R5.Resources.Canonical.TFhirQuestionnaireItemAnswerOption;
  TFhirQuestionnaireItemAnswerOptionList = FHIR.R5.Resources.Canonical.TFhirQuestionnaireItemAnswerOptionList;
  TFhirQuestionnaireItemInitial = FHIR.R5.Resources.Canonical.TFhirQuestionnaireItemInitial;
  TFhirQuestionnaireItemInitialList = FHIR.R5.Resources.Canonical.TFhirQuestionnaireItemInitialList;
  TFhirQuestionnaire = FHIR.R5.Resources.Canonical.TFhirQuestionnaire;
  TFhirQuestionnaireList = FHIR.R5.Resources.Canonical.TFhirQuestionnaireList;
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_REQUESTGROUP}
  TFhirRequestGroupAction = FHIR.R5.Resources.Other.TFhirRequestGroupAction;
  TFhirRequestGroupActionList = FHIR.R5.Resources.Other.TFhirRequestGroupActionList;
  TFhirRequestGroupActionCondition = FHIR.R5.Resources.Other.TFhirRequestGroupActionCondition;
  TFhirRequestGroupActionConditionList = FHIR.R5.Resources.Other.TFhirRequestGroupActionConditionList;
  TFhirRequestGroupActionRelatedAction = FHIR.R5.Resources.Other.TFhirRequestGroupActionRelatedAction;
  TFhirRequestGroupActionRelatedActionList = FHIR.R5.Resources.Other.TFhirRequestGroupActionRelatedActionList;
  TFhirRequestGroup = FHIR.R5.Resources.Other.TFhirRequestGroup;
  TFhirRequestGroupList = FHIR.R5.Resources.Other.TFhirRequestGroupList;
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_SEARCHPARAMETER}
  TFhirSearchParameterComponent = FHIR.R5.Resources.Canonical.TFhirSearchParameterComponent;
  TFhirSearchParameterComponentList = FHIR.R5.Resources.Canonical.TFhirSearchParameterComponentList;
  TFhirSearchParameter = FHIR.R5.Resources.Canonical.TFhirSearchParameter;
  TFhirSearchParameterList = FHIR.R5.Resources.Canonical.TFhirSearchParameterList;
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SPECIMENDEFINITION}
  TFhirSpecimenDefinitionTypeTested = FHIR.R5.Resources.Canonical.TFhirSpecimenDefinitionTypeTested;
  TFhirSpecimenDefinitionTypeTestedList = FHIR.R5.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedList;
  TFhirSpecimenDefinitionTypeTestedContainer = FHIR.R5.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedContainer;
  TFhirSpecimenDefinitionTypeTestedContainerList = FHIR.R5.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedContainerList;
  TFhirSpecimenDefinitionTypeTestedContainerAdditive = FHIR.R5.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedContainerAdditive;
  TFhirSpecimenDefinitionTypeTestedContainerAdditiveList = FHIR.R5.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedContainerAdditiveList;
  TFhirSpecimenDefinitionTypeTestedHandling = FHIR.R5.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedHandling;
  TFhirSpecimenDefinitionTypeTestedHandlingList = FHIR.R5.Resources.Canonical.TFhirSpecimenDefinitionTypeTestedHandlingList;
  TFhirSpecimenDefinition = FHIR.R5.Resources.Canonical.TFhirSpecimenDefinition;
  TFhirSpecimenDefinitionList = FHIR.R5.Resources.Canonical.TFhirSpecimenDefinitionList;
{$ENDIF FHIR_SPECIMENDEFINITION}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  TFhirStructureDefinitionMapping = FHIR.R5.Resources.Canonical.TFhirStructureDefinitionMapping;
  TFhirStructureDefinitionMappingList = FHIR.R5.Resources.Canonical.TFhirStructureDefinitionMappingList;
  TFhirStructureDefinitionContext = FHIR.R5.Resources.Canonical.TFhirStructureDefinitionContext;
  TFhirStructureDefinitionContextList = FHIR.R5.Resources.Canonical.TFhirStructureDefinitionContextList;
  TFhirStructureDefinitionSnapshot = FHIR.R5.Resources.Canonical.TFhirStructureDefinitionSnapshot;
  TFhirStructureDefinitionSnapshotList = FHIR.R5.Resources.Canonical.TFhirStructureDefinitionSnapshotList;
  TFhirStructureDefinitionDifferential = FHIR.R5.Resources.Canonical.TFhirStructureDefinitionDifferential;
  TFhirStructureDefinitionDifferentialList = FHIR.R5.Resources.Canonical.TFhirStructureDefinitionDifferentialList;
  TFhirStructureDefinition = FHIR.R5.Resources.Canonical.TFhirStructureDefinition;
  TFhirStructureDefinitionList = FHIR.R5.Resources.Canonical.TFhirStructureDefinitionList;
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_STRUCTUREMAP}
  TFhirStructureMapStructure = FHIR.R5.Resources.Canonical.TFhirStructureMapStructure;
  TFhirStructureMapStructureList = FHIR.R5.Resources.Canonical.TFhirStructureMapStructureList;
  TFhirStructureMapGroup = FHIR.R5.Resources.Canonical.TFhirStructureMapGroup;
  TFhirStructureMapGroupList = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupList;
  TFhirStructureMapGroupInput = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupInput;
  TFhirStructureMapGroupInputList = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupInputList;
  TFhirStructureMapGroupRule = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupRule;
  TFhirStructureMapGroupRuleList = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupRuleList;
  TFhirStructureMapGroupRuleSource = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupRuleSource;
  TFhirStructureMapGroupRuleSourceList = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupRuleSourceList;
  TFhirStructureMapGroupRuleTarget = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupRuleTarget;
  TFhirStructureMapGroupRuleTargetList = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupRuleTargetList;
  TFhirStructureMapGroupRuleTargetParameter = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupRuleTargetParameter;
  TFhirStructureMapGroupRuleTargetParameterList = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupRuleTargetParameterList;
  TFhirStructureMapGroupRuleDependent = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupRuleDependent;
  TFhirStructureMapGroupRuleDependentList = FHIR.R5.Resources.Canonical.TFhirStructureMapGroupRuleDependentList;
  TFhirStructureMap = FHIR.R5.Resources.Canonical.TFhirStructureMap;
  TFhirStructureMapList = FHIR.R5.Resources.Canonical.TFhirStructureMapList;
{$ENDIF FHIR_STRUCTUREMAP}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  TFhirTerminologyCapabilitiesSoftware = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesSoftware;
  TFhirTerminologyCapabilitiesSoftwareList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesSoftwareList;
  TFhirTerminologyCapabilitiesImplementation = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesImplementation;
  TFhirTerminologyCapabilitiesImplementationList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesImplementationList;
  TFhirTerminologyCapabilitiesCodeSystem = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystem;
  TFhirTerminologyCapabilitiesCodeSystemList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystemList;
  TFhirTerminologyCapabilitiesCodeSystemVersion = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystemVersion;
  TFhirTerminologyCapabilitiesCodeSystemVersionList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystemVersionList;
  TFhirTerminologyCapabilitiesCodeSystemVersionFilter = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystemVersionFilter;
  TFhirTerminologyCapabilitiesCodeSystemVersionFilterList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesCodeSystemVersionFilterList;
  TFhirTerminologyCapabilitiesExpansion = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesExpansion;
  TFhirTerminologyCapabilitiesExpansionList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesExpansionList;
  TFhirTerminologyCapabilitiesExpansionParameter = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesExpansionParameter;
  TFhirTerminologyCapabilitiesExpansionParameterList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesExpansionParameterList;
  TFhirTerminologyCapabilitiesValidateCode = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesValidateCode;
  TFhirTerminologyCapabilitiesValidateCodeList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesValidateCodeList;
  TFhirTerminologyCapabilitiesTranslation = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesTranslation;
  TFhirTerminologyCapabilitiesTranslationList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesTranslationList;
  TFhirTerminologyCapabilitiesClosure = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesClosure;
  TFhirTerminologyCapabilitiesClosureList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesClosureList;
  TFhirTerminologyCapabilities = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilities;
  TFhirTerminologyCapabilitiesList = FHIR.R5.Resources.Canonical.TFhirTerminologyCapabilitiesList;
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}
{$IFDEF FHIR_TESTSCRIPT}
  TFhirTestScriptOrigin = FHIR.R5.Resources.Canonical.TFhirTestScriptOrigin;
  TFhirTestScriptOriginList = FHIR.R5.Resources.Canonical.TFhirTestScriptOriginList;
  TFhirTestScriptDestination = FHIR.R5.Resources.Canonical.TFhirTestScriptDestination;
  TFhirTestScriptDestinationList = FHIR.R5.Resources.Canonical.TFhirTestScriptDestinationList;
  TFhirTestScriptMetadata = FHIR.R5.Resources.Canonical.TFhirTestScriptMetadata;
  TFhirTestScriptMetadataList = FHIR.R5.Resources.Canonical.TFhirTestScriptMetadataList;
  TFhirTestScriptMetadataLink = FHIR.R5.Resources.Canonical.TFhirTestScriptMetadataLink;
  TFhirTestScriptMetadataLinkList = FHIR.R5.Resources.Canonical.TFhirTestScriptMetadataLinkList;
  TFhirTestScriptMetadataCapability = FHIR.R5.Resources.Canonical.TFhirTestScriptMetadataCapability;
  TFhirTestScriptMetadataCapabilityList = FHIR.R5.Resources.Canonical.TFhirTestScriptMetadataCapabilityList;
  TFhirTestScriptFixture = FHIR.R5.Resources.Canonical.TFhirTestScriptFixture;
  TFhirTestScriptFixtureList = FHIR.R5.Resources.Canonical.TFhirTestScriptFixtureList;
  TFhirTestScriptVariable = FHIR.R5.Resources.Canonical.TFhirTestScriptVariable;
  TFhirTestScriptVariableList = FHIR.R5.Resources.Canonical.TFhirTestScriptVariableList;
  TFhirTestScriptSetup = FHIR.R5.Resources.Canonical.TFhirTestScriptSetup;
  TFhirTestScriptSetupList = FHIR.R5.Resources.Canonical.TFhirTestScriptSetupList;
  TFhirTestScriptSetupAction = FHIR.R5.Resources.Canonical.TFhirTestScriptSetupAction;
  TFhirTestScriptSetupActionList = FHIR.R5.Resources.Canonical.TFhirTestScriptSetupActionList;
  TFhirTestScriptSetupActionOperation = FHIR.R5.Resources.Canonical.TFhirTestScriptSetupActionOperation;
  TFhirTestScriptSetupActionOperationList = FHIR.R5.Resources.Canonical.TFhirTestScriptSetupActionOperationList;
  TFhirTestScriptSetupActionOperationRequestHeader = FHIR.R5.Resources.Canonical.TFhirTestScriptSetupActionOperationRequestHeader;
  TFhirTestScriptSetupActionOperationRequestHeaderList = FHIR.R5.Resources.Canonical.TFhirTestScriptSetupActionOperationRequestHeaderList;
  TFhirTestScriptSetupActionAssert = FHIR.R5.Resources.Canonical.TFhirTestScriptSetupActionAssert;
  TFhirTestScriptSetupActionAssertList = FHIR.R5.Resources.Canonical.TFhirTestScriptSetupActionAssertList;
  TFhirTestScriptTest = FHIR.R5.Resources.Canonical.TFhirTestScriptTest;
  TFhirTestScriptTestList = FHIR.R5.Resources.Canonical.TFhirTestScriptTestList;
  TFhirTestScriptTestAction = FHIR.R5.Resources.Canonical.TFhirTestScriptTestAction;
  TFhirTestScriptTestActionList = FHIR.R5.Resources.Canonical.TFhirTestScriptTestActionList;
  TFhirTestScriptTeardown = FHIR.R5.Resources.Canonical.TFhirTestScriptTeardown;
  TFhirTestScriptTeardownList = FHIR.R5.Resources.Canonical.TFhirTestScriptTeardownList;
  TFhirTestScriptTeardownAction = FHIR.R5.Resources.Canonical.TFhirTestScriptTeardownAction;
  TFhirTestScriptTeardownActionList = FHIR.R5.Resources.Canonical.TFhirTestScriptTeardownActionList;
  TFhirTestScript = FHIR.R5.Resources.Canonical.TFhirTestScript;
  TFhirTestScriptList = FHIR.R5.Resources.Canonical.TFhirTestScriptList;
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  TFhirValueSetCompose = FHIR.R5.Resources.Canonical.TFhirValueSetCompose;
  TFhirValueSetComposeList = FHIR.R5.Resources.Canonical.TFhirValueSetComposeList;
  TFhirValueSetComposeInclude = FHIR.R5.Resources.Canonical.TFhirValueSetComposeInclude;
  TFhirValueSetComposeIncludeList = FHIR.R5.Resources.Canonical.TFhirValueSetComposeIncludeList;
  TFhirValueSetComposeIncludeConcept = FHIR.R5.Resources.Canonical.TFhirValueSetComposeIncludeConcept;
  TFhirValueSetComposeIncludeConceptList = FHIR.R5.Resources.Canonical.TFhirValueSetComposeIncludeConceptList;
  TFhirValueSetComposeIncludeConceptDesignation = FHIR.R5.Resources.Canonical.TFhirValueSetComposeIncludeConceptDesignation;
  TFhirValueSetComposeIncludeConceptDesignationList = FHIR.R5.Resources.Canonical.TFhirValueSetComposeIncludeConceptDesignationList;
  TFhirValueSetComposeIncludeFilter = FHIR.R5.Resources.Canonical.TFhirValueSetComposeIncludeFilter;
  TFhirValueSetComposeIncludeFilterList = FHIR.R5.Resources.Canonical.TFhirValueSetComposeIncludeFilterList;
  TFhirValueSetExpansion = FHIR.R5.Resources.Canonical.TFhirValueSetExpansion;
  TFhirValueSetExpansionList = FHIR.R5.Resources.Canonical.TFhirValueSetExpansionList;
  TFhirValueSetExpansionParameter = FHIR.R5.Resources.Canonical.TFhirValueSetExpansionParameter;
  TFhirValueSetExpansionParameterList = FHIR.R5.Resources.Canonical.TFhirValueSetExpansionParameterList;
  TFhirValueSetExpansionProperty = FHIR.R5.Resources.Canonical.TFhirValueSetExpansionProperty;
  TFhirValueSetExpansionPropertyList = FHIR.R5.Resources.Canonical.TFhirValueSetExpansionPropertyList;
  TFhirValueSetExpansionContains = FHIR.R5.Resources.Canonical.TFhirValueSetExpansionContains;
  TFhirValueSetExpansionContainsList = FHIR.R5.Resources.Canonical.TFhirValueSetExpansionContainsList;
  TFhirValueSetExpansionContainsProperty = FHIR.R5.Resources.Canonical.TFhirValueSetExpansionContainsProperty;
  TFhirValueSetExpansionContainsPropertyList = FHIR.R5.Resources.Canonical.TFhirValueSetExpansionContainsPropertyList;
  TFhirValueSet = FHIR.R5.Resources.Canonical.TFhirValueSet;
  TFhirValueSetList = FHIR.R5.Resources.Canonical.TFhirValueSetList;
{$ENDIF FHIR_VALUESET}

implementation

end.

