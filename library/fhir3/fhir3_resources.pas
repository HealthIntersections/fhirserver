unit fhir3_resources;

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
{$I fhir3.inc}

interface

// FHIR v3.0.1 generated 2018-06-12T19:15:59+10:00

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_stream,
  fhir_objects, fhir_utilities, 
  fhir3_base, fhir3_types, fhir3_resources_base,
  fhir3_resources_admin, fhir3_resources_canonical, fhir3_resources_clinical, fhir3_resources_other;

Type
  TFhirResourceType = fhir3_resources_base.TFhirResourceType;
  TFhirResourceTypeSet = fhir3_resources_base.TFhirResourceTypeSet;
Type
  TFhirResource = fhir3_resources_base.TFhirResource;
  TFhirResourceClass = fhir3_resources_base.TFhirResourceClass;
  TFhirResourceList = fhir3_resources_base.TFhirResourceList;
  TFhirDomainResource = fhir3_resources_base.TFhirDomainResource;
  TFhirDomainResourceList = fhir3_resources_base.TFhirDomainResourceList;
  TFhirMetadataResourceList = fhir3_resources_canonical.TFhirMetadataResourceList;
  TFhirMetadataResource = fhir3_resources_canonical.TFhirMetadataResource;

{$IFDEF FHIR_DEVICE}
  TFhirDeviceUdi = fhir3_resources_admin.TFhirDeviceUdi;
  TFhirDeviceUdiList = fhir3_resources_admin.TFhirDeviceUdiList;
  TFhirDevice = fhir3_resources_admin.TFhirDevice;
  TFhirDeviceList = fhir3_resources_admin.TFhirDeviceList;
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICECOMPONENT}
  TFhirDeviceComponentProductionSpecification = fhir3_resources_admin.TFhirDeviceComponentProductionSpecification;
  TFhirDeviceComponentProductionSpecificationList = fhir3_resources_admin.TFhirDeviceComponentProductionSpecificationList;
  TFhirDeviceComponent = fhir3_resources_admin.TFhirDeviceComponent;
  TFhirDeviceComponentList = fhir3_resources_admin.TFhirDeviceComponentList;
{$ENDIF FHIR_DEVICECOMPONENT}
{$IFDEF FHIR_DEVICEMETRIC}
  TFhirDeviceMetricCalibration = fhir3_resources_admin.TFhirDeviceMetricCalibration;
  TFhirDeviceMetricCalibrationList = fhir3_resources_admin.TFhirDeviceMetricCalibrationList;
  TFhirDeviceMetric = fhir3_resources_admin.TFhirDeviceMetric;
  TFhirDeviceMetricList = fhir3_resources_admin.TFhirDeviceMetricList;
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_ENCOUNTER}
  TFhirEncounterStatusHistory = fhir3_resources_admin.TFhirEncounterStatusHistory;
  TFhirEncounterStatusHistoryList = fhir3_resources_admin.TFhirEncounterStatusHistoryList;
  TFhirEncounterClassHistory = fhir3_resources_admin.TFhirEncounterClassHistory;
  TFhirEncounterClassHistoryList = fhir3_resources_admin.TFhirEncounterClassHistoryList;
  TFhirEncounterParticipant = fhir3_resources_admin.TFhirEncounterParticipant;
  TFhirEncounterParticipantList = fhir3_resources_admin.TFhirEncounterParticipantList;
  TFhirEncounterDiagnosis = fhir3_resources_admin.TFhirEncounterDiagnosis;
  TFhirEncounterDiagnosisList = fhir3_resources_admin.TFhirEncounterDiagnosisList;
  TFhirEncounterHospitalization = fhir3_resources_admin.TFhirEncounterHospitalization;
  TFhirEncounterHospitalizationList = fhir3_resources_admin.TFhirEncounterHospitalizationList;
  TFhirEncounterLocation = fhir3_resources_admin.TFhirEncounterLocation;
  TFhirEncounterLocationList = fhir3_resources_admin.TFhirEncounterLocationList;
  TFhirEncounter = fhir3_resources_admin.TFhirEncounter;
  TFhirEncounterList = fhir3_resources_admin.TFhirEncounterList;
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENDPOINT}
  TFhirEndpoint = fhir3_resources_admin.TFhirEndpoint;
  TFhirEndpointList = fhir3_resources_admin.TFhirEndpointList;
{$ENDIF FHIR_ENDPOINT}
{$IFDEF FHIR_EPISODEOFCARE}
  TFhirEpisodeOfCareStatusHistory = fhir3_resources_admin.TFhirEpisodeOfCareStatusHistory;
  TFhirEpisodeOfCareStatusHistoryList = fhir3_resources_admin.TFhirEpisodeOfCareStatusHistoryList;
  TFhirEpisodeOfCareDiagnosis = fhir3_resources_admin.TFhirEpisodeOfCareDiagnosis;
  TFhirEpisodeOfCareDiagnosisList = fhir3_resources_admin.TFhirEpisodeOfCareDiagnosisList;
  TFhirEpisodeOfCare = fhir3_resources_admin.TFhirEpisodeOfCare;
  TFhirEpisodeOfCareList = fhir3_resources_admin.TFhirEpisodeOfCareList;
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_GROUP}
  TFhirGroupCharacteristic = fhir3_resources_admin.TFhirGroupCharacteristic;
  TFhirGroupCharacteristicList = fhir3_resources_admin.TFhirGroupCharacteristicList;
  TFhirGroupMember = fhir3_resources_admin.TFhirGroupMember;
  TFhirGroupMemberList = fhir3_resources_admin.TFhirGroupMemberList;
  TFhirGroup = fhir3_resources_admin.TFhirGroup;
  TFhirGroupList = fhir3_resources_admin.TFhirGroupList;
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_HEALTHCARESERVICE}
  TFhirHealthcareServiceAvailableTime = fhir3_resources_admin.TFhirHealthcareServiceAvailableTime;
  TFhirHealthcareServiceAvailableTimeList = fhir3_resources_admin.TFhirHealthcareServiceAvailableTimeList;
  TFhirHealthcareServiceNotAvailable = fhir3_resources_admin.TFhirHealthcareServiceNotAvailable;
  TFhirHealthcareServiceNotAvailableList = fhir3_resources_admin.TFhirHealthcareServiceNotAvailableList;
  TFhirHealthcareService = fhir3_resources_admin.TFhirHealthcareService;
  TFhirHealthcareServiceList = fhir3_resources_admin.TFhirHealthcareServiceList;
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_LOCATION}
  TFhirLocationPosition = fhir3_resources_admin.TFhirLocationPosition;
  TFhirLocationPositionList = fhir3_resources_admin.TFhirLocationPositionList;
  TFhirLocation = fhir3_resources_admin.TFhirLocation;
  TFhirLocationList = fhir3_resources_admin.TFhirLocationList;
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_ORGANIZATION}
  TFhirOrganizationContact = fhir3_resources_admin.TFhirOrganizationContact;
  TFhirOrganizationContactList = fhir3_resources_admin.TFhirOrganizationContactList;
  TFhirOrganization = fhir3_resources_admin.TFhirOrganization;
  TFhirOrganizationList = fhir3_resources_admin.TFhirOrganizationList;
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_PATIENT}
  TFhirPatientContact = fhir3_resources_admin.TFhirPatientContact;
  TFhirPatientContactList = fhir3_resources_admin.TFhirPatientContactList;
  TFhirPatientAnimal = fhir3_resources_admin.TFhirPatientAnimal;
  TFhirPatientAnimalList = fhir3_resources_admin.TFhirPatientAnimalList;
  TFhirPatientCommunication = fhir3_resources_admin.TFhirPatientCommunication;
  TFhirPatientCommunicationList = fhir3_resources_admin.TFhirPatientCommunicationList;
  TFhirPatientLink = fhir3_resources_admin.TFhirPatientLink;
  TFhirPatientLinkList = fhir3_resources_admin.TFhirPatientLinkList;
  TFhirPatient = fhir3_resources_admin.TFhirPatient;
  TFhirPatientList = fhir3_resources_admin.TFhirPatientList;
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PERSON}
  TFhirPersonLink = fhir3_resources_admin.TFhirPersonLink;
  TFhirPersonLinkList = fhir3_resources_admin.TFhirPersonLinkList;
  TFhirPerson = fhir3_resources_admin.TFhirPerson;
  TFhirPersonList = fhir3_resources_admin.TFhirPersonList;
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PRACTITIONER}
  TFhirPractitionerQualification = fhir3_resources_admin.TFhirPractitionerQualification;
  TFhirPractitionerQualificationList = fhir3_resources_admin.TFhirPractitionerQualificationList;
  TFhirPractitioner = fhir3_resources_admin.TFhirPractitioner;
  TFhirPractitionerList = fhir3_resources_admin.TFhirPractitionerList;
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PRACTITIONERROLE}
  TFhirPractitionerRoleAvailableTime = fhir3_resources_admin.TFhirPractitionerRoleAvailableTime;
  TFhirPractitionerRoleAvailableTimeList = fhir3_resources_admin.TFhirPractitionerRoleAvailableTimeList;
  TFhirPractitionerRoleNotAvailable = fhir3_resources_admin.TFhirPractitionerRoleNotAvailable;
  TFhirPractitionerRoleNotAvailableList = fhir3_resources_admin.TFhirPractitionerRoleNotAvailableList;
  TFhirPractitionerRole = fhir3_resources_admin.TFhirPractitionerRole;
  TFhirPractitionerRoleList = fhir3_resources_admin.TFhirPractitionerRoleList;
{$ENDIF FHIR_PRACTITIONERROLE}
{$IFDEF FHIR_RELATEDPERSON}
  TFhirRelatedPerson = fhir3_resources_admin.TFhirRelatedPerson;
  TFhirRelatedPersonList = fhir3_resources_admin.TFhirRelatedPersonList;
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_SCHEDULE}
  TFhirSchedule = fhir3_resources_admin.TFhirSchedule;
  TFhirScheduleList = fhir3_resources_admin.TFhirScheduleList;
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SLOT}
  TFhirSlot = fhir3_resources_admin.TFhirSlot;
  TFhirSlotList = fhir3_resources_admin.TFhirSlotList;
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SUBSTANCE}
  TFhirSubstanceInstance = fhir3_resources_admin.TFhirSubstanceInstance;
  TFhirSubstanceInstanceList = fhir3_resources_admin.TFhirSubstanceInstanceList;
  TFhirSubstanceIngredient = fhir3_resources_admin.TFhirSubstanceIngredient;
  TFhirSubstanceIngredientList = fhir3_resources_admin.TFhirSubstanceIngredientList;
  TFhirSubstance = fhir3_resources_admin.TFhirSubstance;
  TFhirSubstanceList = fhir3_resources_admin.TFhirSubstanceList;
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_ACCOUNT}
  TFhirAccountCoverage = fhir3_resources_clinical.TFhirAccountCoverage;
  TFhirAccountCoverageList = fhir3_resources_clinical.TFhirAccountCoverageList;
  TFhirAccountGuarantor = fhir3_resources_clinical.TFhirAccountGuarantor;
  TFhirAccountGuarantorList = fhir3_resources_clinical.TFhirAccountGuarantorList;
  TFhirAccount = fhir3_resources_clinical.TFhirAccount;
  TFhirAccountList = fhir3_resources_clinical.TFhirAccountList;
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ADVERSEEVENT}
  TFhirAdverseEventSuspectEntity = fhir3_resources_clinical.TFhirAdverseEventSuspectEntity;
  TFhirAdverseEventSuspectEntityList = fhir3_resources_clinical.TFhirAdverseEventSuspectEntityList;
  TFhirAdverseEvent = fhir3_resources_clinical.TFhirAdverseEvent;
  TFhirAdverseEventList = fhir3_resources_clinical.TFhirAdverseEventList;
{$ENDIF FHIR_ADVERSEEVENT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  TFhirAllergyIntoleranceReaction = fhir3_resources_clinical.TFhirAllergyIntoleranceReaction;
  TFhirAllergyIntoleranceReactionList = fhir3_resources_clinical.TFhirAllergyIntoleranceReactionList;
  TFhirAllergyIntolerance = fhir3_resources_clinical.TFhirAllergyIntolerance;
  TFhirAllergyIntoleranceList = fhir3_resources_clinical.TFhirAllergyIntoleranceList;
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  TFhirAppointmentParticipant = fhir3_resources_clinical.TFhirAppointmentParticipant;
  TFhirAppointmentParticipantList = fhir3_resources_clinical.TFhirAppointmentParticipantList;
  TFhirAppointment = fhir3_resources_clinical.TFhirAppointment;
  TFhirAppointmentList = fhir3_resources_clinical.TFhirAppointmentList;
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  TFhirAppointmentResponse = fhir3_resources_clinical.TFhirAppointmentResponse;
  TFhirAppointmentResponseList = fhir3_resources_clinical.TFhirAppointmentResponseList;
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_BASIC}
  TFhirBasic = fhir3_resources_clinical.TFhirBasic;
  TFhirBasicList = fhir3_resources_clinical.TFhirBasicList;
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BODYSITE}
  TFhirBodySite = fhir3_resources_clinical.TFhirBodySite;
  TFhirBodySiteList = fhir3_resources_clinical.TFhirBodySiteList;
{$ENDIF FHIR_BODYSITE}
{$IFDEF FHIR_CAREPLAN}
  TFhirCarePlanActivity = fhir3_resources_clinical.TFhirCarePlanActivity;
  TFhirCarePlanActivityList = fhir3_resources_clinical.TFhirCarePlanActivityList;
  TFhirCarePlanActivityDetail = fhir3_resources_clinical.TFhirCarePlanActivityDetail;
  TFhirCarePlanActivityDetailList = fhir3_resources_clinical.TFhirCarePlanActivityDetailList;
  TFhirCarePlan = fhir3_resources_clinical.TFhirCarePlan;
  TFhirCarePlanList = fhir3_resources_clinical.TFhirCarePlanList;
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CARETEAM}
  TFhirCareTeamParticipant = fhir3_resources_clinical.TFhirCareTeamParticipant;
  TFhirCareTeamParticipantList = fhir3_resources_clinical.TFhirCareTeamParticipantList;
  TFhirCareTeam = fhir3_resources_clinical.TFhirCareTeam;
  TFhirCareTeamList = fhir3_resources_clinical.TFhirCareTeamList;
{$ENDIF FHIR_CARETEAM}
{$IFDEF FHIR_CLINICALIMPRESSION}
  TFhirClinicalImpressionInvestigation = fhir3_resources_clinical.TFhirClinicalImpressionInvestigation;
  TFhirClinicalImpressionInvestigationList = fhir3_resources_clinical.TFhirClinicalImpressionInvestigationList;
  TFhirClinicalImpressionFinding = fhir3_resources_clinical.TFhirClinicalImpressionFinding;
  TFhirClinicalImpressionFindingList = fhir3_resources_clinical.TFhirClinicalImpressionFindingList;
  TFhirClinicalImpression = fhir3_resources_clinical.TFhirClinicalImpression;
  TFhirClinicalImpressionList = fhir3_resources_clinical.TFhirClinicalImpressionList;
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_COMMUNICATION}
  TFhirCommunicationPayload = fhir3_resources_clinical.TFhirCommunicationPayload;
  TFhirCommunicationPayloadList = fhir3_resources_clinical.TFhirCommunicationPayloadList;
  TFhirCommunication = fhir3_resources_clinical.TFhirCommunication;
  TFhirCommunicationList = fhir3_resources_clinical.TFhirCommunicationList;
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  TFhirCommunicationRequestPayload = fhir3_resources_clinical.TFhirCommunicationRequestPayload;
  TFhirCommunicationRequestPayloadList = fhir3_resources_clinical.TFhirCommunicationRequestPayloadList;
  TFhirCommunicationRequestRequester = fhir3_resources_clinical.TFhirCommunicationRequestRequester;
  TFhirCommunicationRequestRequesterList = fhir3_resources_clinical.TFhirCommunicationRequestRequesterList;
  TFhirCommunicationRequest = fhir3_resources_clinical.TFhirCommunicationRequest;
  TFhirCommunicationRequestList = fhir3_resources_clinical.TFhirCommunicationRequestList;
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPOSITION}
  TFhirCompositionAttester = fhir3_resources_clinical.TFhirCompositionAttester;
  TFhirCompositionAttesterList = fhir3_resources_clinical.TFhirCompositionAttesterList;
  TFhirCompositionRelatesTo = fhir3_resources_clinical.TFhirCompositionRelatesTo;
  TFhirCompositionRelatesToList = fhir3_resources_clinical.TFhirCompositionRelatesToList;
  TFhirCompositionEvent = fhir3_resources_clinical.TFhirCompositionEvent;
  TFhirCompositionEventList = fhir3_resources_clinical.TFhirCompositionEventList;
  TFhirCompositionSection = fhir3_resources_clinical.TFhirCompositionSection;
  TFhirCompositionSectionList = fhir3_resources_clinical.TFhirCompositionSectionList;
  TFhirComposition = fhir3_resources_clinical.TFhirComposition;
  TFhirCompositionList = fhir3_resources_clinical.TFhirCompositionList;
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONDITION}
  TFhirConditionStage = fhir3_resources_clinical.TFhirConditionStage;
  TFhirConditionStageList = fhir3_resources_clinical.TFhirConditionStageList;
  TFhirConditionEvidence = fhir3_resources_clinical.TFhirConditionEvidence;
  TFhirConditionEvidenceList = fhir3_resources_clinical.TFhirConditionEvidenceList;
  TFhirCondition = fhir3_resources_clinical.TFhirCondition;
  TFhirConditionList = fhir3_resources_clinical.TFhirConditionList;
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_COVERAGE}
  TFhirCoverageGrouping = fhir3_resources_clinical.TFhirCoverageGrouping;
  TFhirCoverageGroupingList = fhir3_resources_clinical.TFhirCoverageGroupingList;
  TFhirCoverage = fhir3_resources_clinical.TFhirCoverage;
  TFhirCoverageList = fhir3_resources_clinical.TFhirCoverageList;
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_DETECTEDISSUE}
  TFhirDetectedIssueMitigation = fhir3_resources_clinical.TFhirDetectedIssueMitigation;
  TFhirDetectedIssueMitigationList = fhir3_resources_clinical.TFhirDetectedIssueMitigationList;
  TFhirDetectedIssue = fhir3_resources_clinical.TFhirDetectedIssue;
  TFhirDetectedIssueList = fhir3_resources_clinical.TFhirDetectedIssueList;
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICEREQUEST}
  TFhirDeviceRequestRequester = fhir3_resources_clinical.TFhirDeviceRequestRequester;
  TFhirDeviceRequestRequesterList = fhir3_resources_clinical.TFhirDeviceRequestRequesterList;
  TFhirDeviceRequest = fhir3_resources_clinical.TFhirDeviceRequest;
  TFhirDeviceRequestList = fhir3_resources_clinical.TFhirDeviceRequestList;
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  TFhirDeviceUseStatement = fhir3_resources_clinical.TFhirDeviceUseStatement;
  TFhirDeviceUseStatementList = fhir3_resources_clinical.TFhirDeviceUseStatementList;
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  TFhirDiagnosticReportPerformer = fhir3_resources_clinical.TFhirDiagnosticReportPerformer;
  TFhirDiagnosticReportPerformerList = fhir3_resources_clinical.TFhirDiagnosticReportPerformerList;
  TFhirDiagnosticReportImage = fhir3_resources_clinical.TFhirDiagnosticReportImage;
  TFhirDiagnosticReportImageList = fhir3_resources_clinical.TFhirDiagnosticReportImageList;
  TFhirDiagnosticReport = fhir3_resources_clinical.TFhirDiagnosticReport;
  TFhirDiagnosticReportList = fhir3_resources_clinical.TFhirDiagnosticReportList;
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  TFhirDocumentManifestContent = fhir3_resources_clinical.TFhirDocumentManifestContent;
  TFhirDocumentManifestContentList = fhir3_resources_clinical.TFhirDocumentManifestContentList;
  TFhirDocumentManifestRelated = fhir3_resources_clinical.TFhirDocumentManifestRelated;
  TFhirDocumentManifestRelatedList = fhir3_resources_clinical.TFhirDocumentManifestRelatedList;
  TFhirDocumentManifest = fhir3_resources_clinical.TFhirDocumentManifest;
  TFhirDocumentManifestList = fhir3_resources_clinical.TFhirDocumentManifestList;
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  TFhirDocumentReferenceRelatesTo = fhir3_resources_clinical.TFhirDocumentReferenceRelatesTo;
  TFhirDocumentReferenceRelatesToList = fhir3_resources_clinical.TFhirDocumentReferenceRelatesToList;
  TFhirDocumentReferenceContent = fhir3_resources_clinical.TFhirDocumentReferenceContent;
  TFhirDocumentReferenceContentList = fhir3_resources_clinical.TFhirDocumentReferenceContentList;
  TFhirDocumentReferenceContext = fhir3_resources_clinical.TFhirDocumentReferenceContext;
  TFhirDocumentReferenceContextList = fhir3_resources_clinical.TFhirDocumentReferenceContextList;
  TFhirDocumentReferenceContextRelated = fhir3_resources_clinical.TFhirDocumentReferenceContextRelated;
  TFhirDocumentReferenceContextRelatedList = fhir3_resources_clinical.TFhirDocumentReferenceContextRelatedList;
  TFhirDocumentReference = fhir3_resources_clinical.TFhirDocumentReference;
  TFhirDocumentReferenceList = fhir3_resources_clinical.TFhirDocumentReferenceList;
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  TFhirFamilyMemberHistoryCondition = fhir3_resources_clinical.TFhirFamilyMemberHistoryCondition;
  TFhirFamilyMemberHistoryConditionList = fhir3_resources_clinical.TFhirFamilyMemberHistoryConditionList;
  TFhirFamilyMemberHistory = fhir3_resources_clinical.TFhirFamilyMemberHistory;
  TFhirFamilyMemberHistoryList = fhir3_resources_clinical.TFhirFamilyMemberHistoryList;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  TFhirFlag = fhir3_resources_clinical.TFhirFlag;
  TFhirFlagList = fhir3_resources_clinical.TFhirFlagList;
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  TFhirGoalTarget = fhir3_resources_clinical.TFhirGoalTarget;
  TFhirGoalTargetList = fhir3_resources_clinical.TFhirGoalTargetList;
  TFhirGoal = fhir3_resources_clinical.TFhirGoal;
  TFhirGoalList = fhir3_resources_clinical.TFhirGoalList;
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_IMAGINGMANIFEST}
  TFhirImagingManifestStudy = fhir3_resources_clinical.TFhirImagingManifestStudy;
  TFhirImagingManifestStudyList = fhir3_resources_clinical.TFhirImagingManifestStudyList;
  TFhirImagingManifestStudySeries = fhir3_resources_clinical.TFhirImagingManifestStudySeries;
  TFhirImagingManifestStudySeriesList = fhir3_resources_clinical.TFhirImagingManifestStudySeriesList;
  TFhirImagingManifestStudySeriesInstance = fhir3_resources_clinical.TFhirImagingManifestStudySeriesInstance;
  TFhirImagingManifestStudySeriesInstanceList = fhir3_resources_clinical.TFhirImagingManifestStudySeriesInstanceList;
  TFhirImagingManifest = fhir3_resources_clinical.TFhirImagingManifest;
  TFhirImagingManifestList = fhir3_resources_clinical.TFhirImagingManifestList;
{$ENDIF FHIR_IMAGINGMANIFEST}
{$IFDEF FHIR_IMAGINGSTUDY}
  TFhirImagingStudySeries = fhir3_resources_clinical.TFhirImagingStudySeries;
  TFhirImagingStudySeriesList = fhir3_resources_clinical.TFhirImagingStudySeriesList;
  TFhirImagingStudySeriesInstance = fhir3_resources_clinical.TFhirImagingStudySeriesInstance;
  TFhirImagingStudySeriesInstanceList = fhir3_resources_clinical.TFhirImagingStudySeriesInstanceList;
  TFhirImagingStudy = fhir3_resources_clinical.TFhirImagingStudy;
  TFhirImagingStudyList = fhir3_resources_clinical.TFhirImagingStudyList;
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  TFhirImmunizationPractitioner = fhir3_resources_clinical.TFhirImmunizationPractitioner;
  TFhirImmunizationPractitionerList = fhir3_resources_clinical.TFhirImmunizationPractitionerList;
  TFhirImmunizationExplanation = fhir3_resources_clinical.TFhirImmunizationExplanation;
  TFhirImmunizationExplanationList = fhir3_resources_clinical.TFhirImmunizationExplanationList;
  TFhirImmunizationReaction = fhir3_resources_clinical.TFhirImmunizationReaction;
  TFhirImmunizationReactionList = fhir3_resources_clinical.TFhirImmunizationReactionList;
  TFhirImmunizationVaccinationProtocol = fhir3_resources_clinical.TFhirImmunizationVaccinationProtocol;
  TFhirImmunizationVaccinationProtocolList = fhir3_resources_clinical.TFhirImmunizationVaccinationProtocolList;
  TFhirImmunization = fhir3_resources_clinical.TFhirImmunization;
  TFhirImmunizationList = fhir3_resources_clinical.TFhirImmunizationList;
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  TFhirImmunizationRecommendationRecommendation = fhir3_resources_clinical.TFhirImmunizationRecommendationRecommendation;
  TFhirImmunizationRecommendationRecommendationList = fhir3_resources_clinical.TFhirImmunizationRecommendationRecommendationList;
  TFhirImmunizationRecommendationRecommendationDateCriterion = fhir3_resources_clinical.TFhirImmunizationRecommendationRecommendationDateCriterion;
  TFhirImmunizationRecommendationRecommendationDateCriterionList = fhir3_resources_clinical.TFhirImmunizationRecommendationRecommendationDateCriterionList;
  TFhirImmunizationRecommendationRecommendationProtocol = fhir3_resources_clinical.TFhirImmunizationRecommendationRecommendationProtocol;
  TFhirImmunizationRecommendationRecommendationProtocolList = fhir3_resources_clinical.TFhirImmunizationRecommendationRecommendationProtocolList;
  TFhirImmunizationRecommendation = fhir3_resources_clinical.TFhirImmunizationRecommendation;
  TFhirImmunizationRecommendationList = fhir3_resources_clinical.TFhirImmunizationRecommendationList;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_MEDIA}
  TFhirMedia = fhir3_resources_clinical.TFhirMedia;
  TFhirMediaList = fhir3_resources_clinical.TFhirMediaList;
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  TFhirMedicationAdministrationPerformer = fhir3_resources_clinical.TFhirMedicationAdministrationPerformer;
  TFhirMedicationAdministrationPerformerList = fhir3_resources_clinical.TFhirMedicationAdministrationPerformerList;
  TFhirMedicationAdministrationDosage = fhir3_resources_clinical.TFhirMedicationAdministrationDosage;
  TFhirMedicationAdministrationDosageList = fhir3_resources_clinical.TFhirMedicationAdministrationDosageList;
  TFhirMedicationAdministration = fhir3_resources_clinical.TFhirMedicationAdministration;
  TFhirMedicationAdministrationList = fhir3_resources_clinical.TFhirMedicationAdministrationList;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  TFhirMedicationDispensePerformer = fhir3_resources_clinical.TFhirMedicationDispensePerformer;
  TFhirMedicationDispensePerformerList = fhir3_resources_clinical.TFhirMedicationDispensePerformerList;
  TFhirMedicationDispenseSubstitution = fhir3_resources_clinical.TFhirMedicationDispenseSubstitution;
  TFhirMedicationDispenseSubstitutionList = fhir3_resources_clinical.TFhirMedicationDispenseSubstitutionList;
  TFhirMedicationDispense = fhir3_resources_clinical.TFhirMedicationDispense;
  TFhirMedicationDispenseList = fhir3_resources_clinical.TFhirMedicationDispenseList;
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  TFhirMedicationRequestRequester = fhir3_resources_clinical.TFhirMedicationRequestRequester;
  TFhirMedicationRequestRequesterList = fhir3_resources_clinical.TFhirMedicationRequestRequesterList;
  TFhirMedicationRequestDispenseRequest = fhir3_resources_clinical.TFhirMedicationRequestDispenseRequest;
  TFhirMedicationRequestDispenseRequestList = fhir3_resources_clinical.TFhirMedicationRequestDispenseRequestList;
  TFhirMedicationRequestSubstitution = fhir3_resources_clinical.TFhirMedicationRequestSubstitution;
  TFhirMedicationRequestSubstitutionList = fhir3_resources_clinical.TFhirMedicationRequestSubstitutionList;
  TFhirMedicationRequest = fhir3_resources_clinical.TFhirMedicationRequest;
  TFhirMedicationRequestList = fhir3_resources_clinical.TFhirMedicationRequestList;
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  TFhirMedicationStatement = fhir3_resources_clinical.TFhirMedicationStatement;
  TFhirMedicationStatementList = fhir3_resources_clinical.TFhirMedicationStatementList;
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_NUTRITIONORDER}
  TFhirNutritionOrderOralDiet = fhir3_resources_clinical.TFhirNutritionOrderOralDiet;
  TFhirNutritionOrderOralDietList = fhir3_resources_clinical.TFhirNutritionOrderOralDietList;
  TFhirNutritionOrderOralDietNutrient = fhir3_resources_clinical.TFhirNutritionOrderOralDietNutrient;
  TFhirNutritionOrderOralDietNutrientList = fhir3_resources_clinical.TFhirNutritionOrderOralDietNutrientList;
  TFhirNutritionOrderOralDietTexture = fhir3_resources_clinical.TFhirNutritionOrderOralDietTexture;
  TFhirNutritionOrderOralDietTextureList = fhir3_resources_clinical.TFhirNutritionOrderOralDietTextureList;
  TFhirNutritionOrderSupplement = fhir3_resources_clinical.TFhirNutritionOrderSupplement;
  TFhirNutritionOrderSupplementList = fhir3_resources_clinical.TFhirNutritionOrderSupplementList;
  TFhirNutritionOrderEnteralFormula = fhir3_resources_clinical.TFhirNutritionOrderEnteralFormula;
  TFhirNutritionOrderEnteralFormulaList = fhir3_resources_clinical.TFhirNutritionOrderEnteralFormulaList;
  TFhirNutritionOrderEnteralFormulaAdministration = fhir3_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministration;
  TFhirNutritionOrderEnteralFormulaAdministrationList = fhir3_resources_clinical.TFhirNutritionOrderEnteralFormulaAdministrationList;
  TFhirNutritionOrder = fhir3_resources_clinical.TFhirNutritionOrder;
  TFhirNutritionOrderList = fhir3_resources_clinical.TFhirNutritionOrderList;
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_OBSERVATION}
  TFhirObservationReferenceRange = fhir3_resources_clinical.TFhirObservationReferenceRange;
  TFhirObservationReferenceRangeList = fhir3_resources_clinical.TFhirObservationReferenceRangeList;
  TFhirObservationRelated = fhir3_resources_clinical.TFhirObservationRelated;
  TFhirObservationRelatedList = fhir3_resources_clinical.TFhirObservationRelatedList;
  TFhirObservationComponent = fhir3_resources_clinical.TFhirObservationComponent;
  TFhirObservationComponentList = fhir3_resources_clinical.TFhirObservationComponentList;
  TFhirObservation = fhir3_resources_clinical.TFhirObservation;
  TFhirObservationList = fhir3_resources_clinical.TFhirObservationList;
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_PROCEDURE}
  TFhirProcedurePerformer = fhir3_resources_clinical.TFhirProcedurePerformer;
  TFhirProcedurePerformerList = fhir3_resources_clinical.TFhirProcedurePerformerList;
  TFhirProcedureFocalDevice = fhir3_resources_clinical.TFhirProcedureFocalDevice;
  TFhirProcedureFocalDeviceList = fhir3_resources_clinical.TFhirProcedureFocalDeviceList;
  TFhirProcedure = fhir3_resources_clinical.TFhirProcedure;
  TFhirProcedureList = fhir3_resources_clinical.TFhirProcedureList;
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROCEDUREREQUEST}
  TFhirProcedureRequestRequester = fhir3_resources_clinical.TFhirProcedureRequestRequester;
  TFhirProcedureRequestRequesterList = fhir3_resources_clinical.TFhirProcedureRequestRequesterList;
  TFhirProcedureRequest = fhir3_resources_clinical.TFhirProcedureRequest;
  TFhirProcedureRequestList = fhir3_resources_clinical.TFhirProcedureRequestList;
{$ENDIF FHIR_PROCEDUREREQUEST}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  TFhirQuestionnaireResponseItem = fhir3_resources_clinical.TFhirQuestionnaireResponseItem;
  TFhirQuestionnaireResponseItemList = fhir3_resources_clinical.TFhirQuestionnaireResponseItemList;
  TFhirQuestionnaireResponseItemAnswer = fhir3_resources_clinical.TFhirQuestionnaireResponseItemAnswer;
  TFhirQuestionnaireResponseItemAnswerList = fhir3_resources_clinical.TFhirQuestionnaireResponseItemAnswerList;
  TFhirQuestionnaireResponse = fhir3_resources_clinical.TFhirQuestionnaireResponse;
  TFhirQuestionnaireResponseList = fhir3_resources_clinical.TFhirQuestionnaireResponseList;
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REFERRALREQUEST}
  TFhirReferralRequestRequester = fhir3_resources_clinical.TFhirReferralRequestRequester;
  TFhirReferralRequestRequesterList = fhir3_resources_clinical.TFhirReferralRequestRequesterList;
  TFhirReferralRequest = fhir3_resources_clinical.TFhirReferralRequest;
  TFhirReferralRequestList = fhir3_resources_clinical.TFhirReferralRequestList;
{$ENDIF FHIR_REFERRALREQUEST}
{$IFDEF FHIR_RISKASSESSMENT}
  TFhirRiskAssessmentPrediction = fhir3_resources_clinical.TFhirRiskAssessmentPrediction;
  TFhirRiskAssessmentPredictionList = fhir3_resources_clinical.TFhirRiskAssessmentPredictionList;
  TFhirRiskAssessment = fhir3_resources_clinical.TFhirRiskAssessment;
  TFhirRiskAssessmentList = fhir3_resources_clinical.TFhirRiskAssessmentList;
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_SEQUENCE}
  TFhirSequenceReferenceSeq = fhir3_resources_clinical.TFhirSequenceReferenceSeq;
  TFhirSequenceReferenceSeqList = fhir3_resources_clinical.TFhirSequenceReferenceSeqList;
  TFhirSequenceVariant = fhir3_resources_clinical.TFhirSequenceVariant;
  TFhirSequenceVariantList = fhir3_resources_clinical.TFhirSequenceVariantList;
  TFhirSequenceQuality = fhir3_resources_clinical.TFhirSequenceQuality;
  TFhirSequenceQualityList = fhir3_resources_clinical.TFhirSequenceQualityList;
  TFhirSequenceRepository = fhir3_resources_clinical.TFhirSequenceRepository;
  TFhirSequenceRepositoryList = fhir3_resources_clinical.TFhirSequenceRepositoryList;
  TFhirSequence = fhir3_resources_clinical.TFhirSequence;
  TFhirSequenceList = fhir3_resources_clinical.TFhirSequenceList;
{$ENDIF FHIR_SEQUENCE}
{$IFDEF FHIR_SPECIMEN}
  TFhirSpecimenCollection = fhir3_resources_clinical.TFhirSpecimenCollection;
  TFhirSpecimenCollectionList = fhir3_resources_clinical.TFhirSpecimenCollectionList;
  TFhirSpecimenProcessing = fhir3_resources_clinical.TFhirSpecimenProcessing;
  TFhirSpecimenProcessingList = fhir3_resources_clinical.TFhirSpecimenProcessingList;
  TFhirSpecimenContainer = fhir3_resources_clinical.TFhirSpecimenContainer;
  TFhirSpecimenContainerList = fhir3_resources_clinical.TFhirSpecimenContainerList;
  TFhirSpecimen = fhir3_resources_clinical.TFhirSpecimen;
  TFhirSpecimenList = fhir3_resources_clinical.TFhirSpecimenList;
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_SUPPLYDELIVERY}
  TFhirSupplyDeliverySuppliedItem = fhir3_resources_clinical.TFhirSupplyDeliverySuppliedItem;
  TFhirSupplyDeliverySuppliedItemList = fhir3_resources_clinical.TFhirSupplyDeliverySuppliedItemList;
  TFhirSupplyDelivery = fhir3_resources_clinical.TFhirSupplyDelivery;
  TFhirSupplyDeliveryList = fhir3_resources_clinical.TFhirSupplyDeliveryList;
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  TFhirSupplyRequestOrderedItem = fhir3_resources_clinical.TFhirSupplyRequestOrderedItem;
  TFhirSupplyRequestOrderedItemList = fhir3_resources_clinical.TFhirSupplyRequestOrderedItemList;
  TFhirSupplyRequestRequester = fhir3_resources_clinical.TFhirSupplyRequestRequester;
  TFhirSupplyRequestRequesterList = fhir3_resources_clinical.TFhirSupplyRequestRequesterList;
  TFhirSupplyRequest = fhir3_resources_clinical.TFhirSupplyRequest;
  TFhirSupplyRequestList = fhir3_resources_clinical.TFhirSupplyRequestList;
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  TFhirVisionPrescriptionDispense = fhir3_resources_clinical.TFhirVisionPrescriptionDispense;
  TFhirVisionPrescriptionDispenseList = fhir3_resources_clinical.TFhirVisionPrescriptionDispenseList;
  TFhirVisionPrescription = fhir3_resources_clinical.TFhirVisionPrescription;
  TFhirVisionPrescriptionList = fhir3_resources_clinical.TFhirVisionPrescriptionList;
{$ENDIF FHIR_VISIONPRESCRIPTION}
{$IFDEF FHIR_PARAMETERS}
  TFhirParametersParameter = fhir3_resources_other.TFhirParametersParameter;
  TFhirParametersParameterList = fhir3_resources_other.TFhirParametersParameterList;
  TFhirParameters = fhir3_resources_other.TFhirParameters;
  TFhirParametersList = fhir3_resources_other.TFhirParametersList;
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_AUDITEVENT}
  TFhirAuditEventAgent = fhir3_resources_other.TFhirAuditEventAgent;
  TFhirAuditEventAgentList = fhir3_resources_other.TFhirAuditEventAgentList;
  TFhirAuditEventAgentNetwork = fhir3_resources_other.TFhirAuditEventAgentNetwork;
  TFhirAuditEventAgentNetworkList = fhir3_resources_other.TFhirAuditEventAgentNetworkList;
  TFhirAuditEventSource = fhir3_resources_other.TFhirAuditEventSource;
  TFhirAuditEventSourceList = fhir3_resources_other.TFhirAuditEventSourceList;
  TFhirAuditEventEntity = fhir3_resources_other.TFhirAuditEventEntity;
  TFhirAuditEventEntityList = fhir3_resources_other.TFhirAuditEventEntityList;
  TFhirAuditEventEntityDetail = fhir3_resources_other.TFhirAuditEventEntityDetail;
  TFhirAuditEventEntityDetailList = fhir3_resources_other.TFhirAuditEventEntityDetailList;
  TFhirAuditEvent = fhir3_resources_other.TFhirAuditEvent;
  TFhirAuditEventList = fhir3_resources_other.TFhirAuditEventList;
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BINARY}
  TFhirBinary = fhir3_resources_other.TFhirBinary;
  TFhirBinaryList = fhir3_resources_other.TFhirBinaryList;
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BUNDLE}
  TFhirBundleLink = fhir3_resources_other.TFhirBundleLink;
  TFhirBundleLinkList = fhir3_resources_other.TFhirBundleLinkList;
  TFhirBundleEntry = fhir3_resources_other.TFhirBundleEntry;
  TFhirBundleEntryList = fhir3_resources_other.TFhirBundleEntryList;
  TFhirBundleEntrySearch = fhir3_resources_other.TFhirBundleEntrySearch;
  TFhirBundleEntrySearchList = fhir3_resources_other.TFhirBundleEntrySearchList;
  TFhirBundleEntryRequest = fhir3_resources_other.TFhirBundleEntryRequest;
  TFhirBundleEntryRequestList = fhir3_resources_other.TFhirBundleEntryRequestList;
  TFhirBundleEntryResponse = fhir3_resources_other.TFhirBundleEntryResponse;
  TFhirBundleEntryResponseList = fhir3_resources_other.TFhirBundleEntryResponseList;
  TFhirBundle = fhir3_resources_other.TFhirBundle;
  TFhirBundleList = fhir3_resources_other.TFhirBundleList;
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CHARGEITEM}
  TFhirChargeItemParticipant = fhir3_resources_other.TFhirChargeItemParticipant;
  TFhirChargeItemParticipantList = fhir3_resources_other.TFhirChargeItemParticipantList;
  TFhirChargeItem = fhir3_resources_other.TFhirChargeItem;
  TFhirChargeItemList = fhir3_resources_other.TFhirChargeItemList;
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CLAIM}
  TFhirClaimRelated = fhir3_resources_other.TFhirClaimRelated;
  TFhirClaimRelatedList = fhir3_resources_other.TFhirClaimRelatedList;
  TFhirClaimPayee = fhir3_resources_other.TFhirClaimPayee;
  TFhirClaimPayeeList = fhir3_resources_other.TFhirClaimPayeeList;
  TFhirClaimCareTeam = fhir3_resources_other.TFhirClaimCareTeam;
  TFhirClaimCareTeamList = fhir3_resources_other.TFhirClaimCareTeamList;
  TFhirClaimInformation = fhir3_resources_other.TFhirClaimInformation;
  TFhirClaimInformationList = fhir3_resources_other.TFhirClaimInformationList;
  TFhirClaimDiagnosis = fhir3_resources_other.TFhirClaimDiagnosis;
  TFhirClaimDiagnosisList = fhir3_resources_other.TFhirClaimDiagnosisList;
  TFhirClaimProcedure = fhir3_resources_other.TFhirClaimProcedure;
  TFhirClaimProcedureList = fhir3_resources_other.TFhirClaimProcedureList;
  TFhirClaimInsurance = fhir3_resources_other.TFhirClaimInsurance;
  TFhirClaimInsuranceList = fhir3_resources_other.TFhirClaimInsuranceList;
  TFhirClaimAccident = fhir3_resources_other.TFhirClaimAccident;
  TFhirClaimAccidentList = fhir3_resources_other.TFhirClaimAccidentList;
  TFhirClaimItem = fhir3_resources_other.TFhirClaimItem;
  TFhirClaimItemList = fhir3_resources_other.TFhirClaimItemList;
  TFhirClaimItemDetail = fhir3_resources_other.TFhirClaimItemDetail;
  TFhirClaimItemDetailList = fhir3_resources_other.TFhirClaimItemDetailList;
  TFhirClaimItemDetailSubDetail = fhir3_resources_other.TFhirClaimItemDetailSubDetail;
  TFhirClaimItemDetailSubDetailList = fhir3_resources_other.TFhirClaimItemDetailSubDetailList;
  TFhirClaim = fhir3_resources_other.TFhirClaim;
  TFhirClaimList = fhir3_resources_other.TFhirClaimList;
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  TFhirClaimResponseItem = fhir3_resources_other.TFhirClaimResponseItem;
  TFhirClaimResponseItemList = fhir3_resources_other.TFhirClaimResponseItemList;
  TFhirClaimResponseItemAdjudication = fhir3_resources_other.TFhirClaimResponseItemAdjudication;
  TFhirClaimResponseItemAdjudicationList = fhir3_resources_other.TFhirClaimResponseItemAdjudicationList;
  TFhirClaimResponseItemDetail = fhir3_resources_other.TFhirClaimResponseItemDetail;
  TFhirClaimResponseItemDetailList = fhir3_resources_other.TFhirClaimResponseItemDetailList;
  TFhirClaimResponseItemDetailSubDetail = fhir3_resources_other.TFhirClaimResponseItemDetailSubDetail;
  TFhirClaimResponseItemDetailSubDetailList = fhir3_resources_other.TFhirClaimResponseItemDetailSubDetailList;
  TFhirClaimResponseAddItem = fhir3_resources_other.TFhirClaimResponseAddItem;
  TFhirClaimResponseAddItemList = fhir3_resources_other.TFhirClaimResponseAddItemList;
  TFhirClaimResponseAddItemDetail = fhir3_resources_other.TFhirClaimResponseAddItemDetail;
  TFhirClaimResponseAddItemDetailList = fhir3_resources_other.TFhirClaimResponseAddItemDetailList;
  TFhirClaimResponseError = fhir3_resources_other.TFhirClaimResponseError;
  TFhirClaimResponseErrorList = fhir3_resources_other.TFhirClaimResponseErrorList;
  TFhirClaimResponsePayment = fhir3_resources_other.TFhirClaimResponsePayment;
  TFhirClaimResponsePaymentList = fhir3_resources_other.TFhirClaimResponsePaymentList;
  TFhirClaimResponseProcessNote = fhir3_resources_other.TFhirClaimResponseProcessNote;
  TFhirClaimResponseProcessNoteList = fhir3_resources_other.TFhirClaimResponseProcessNoteList;
  TFhirClaimResponseInsurance = fhir3_resources_other.TFhirClaimResponseInsurance;
  TFhirClaimResponseInsuranceList = fhir3_resources_other.TFhirClaimResponseInsuranceList;
  TFhirClaimResponse = fhir3_resources_other.TFhirClaimResponse;
  TFhirClaimResponseList = fhir3_resources_other.TFhirClaimResponseList;
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CONSENT}
  TFhirConsentActor = fhir3_resources_other.TFhirConsentActor;
  TFhirConsentActorList = fhir3_resources_other.TFhirConsentActorList;
  TFhirConsentPolicy = fhir3_resources_other.TFhirConsentPolicy;
  TFhirConsentPolicyList = fhir3_resources_other.TFhirConsentPolicyList;
  TFhirConsentData = fhir3_resources_other.TFhirConsentData;
  TFhirConsentDataList = fhir3_resources_other.TFhirConsentDataList;
  TFhirConsentExcept = fhir3_resources_other.TFhirConsentExcept;
  TFhirConsentExceptList = fhir3_resources_other.TFhirConsentExceptList;
  TFhirConsentExceptActor = fhir3_resources_other.TFhirConsentExceptActor;
  TFhirConsentExceptActorList = fhir3_resources_other.TFhirConsentExceptActorList;
  TFhirConsentExceptData = fhir3_resources_other.TFhirConsentExceptData;
  TFhirConsentExceptDataList = fhir3_resources_other.TFhirConsentExceptDataList;
  TFhirConsent = fhir3_resources_other.TFhirConsent;
  TFhirConsentList = fhir3_resources_other.TFhirConsentList;
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  TFhirContractAgent = fhir3_resources_other.TFhirContractAgent;
  TFhirContractAgentList = fhir3_resources_other.TFhirContractAgentList;
  TFhirContractSigner = fhir3_resources_other.TFhirContractSigner;
  TFhirContractSignerList = fhir3_resources_other.TFhirContractSignerList;
  TFhirContractValuedItem = fhir3_resources_other.TFhirContractValuedItem;
  TFhirContractValuedItemList = fhir3_resources_other.TFhirContractValuedItemList;
  TFhirContractTerm = fhir3_resources_other.TFhirContractTerm;
  TFhirContractTermList = fhir3_resources_other.TFhirContractTermList;
  TFhirContractTermAgent = fhir3_resources_other.TFhirContractTermAgent;
  TFhirContractTermAgentList = fhir3_resources_other.TFhirContractTermAgentList;
  TFhirContractTermValuedItem = fhir3_resources_other.TFhirContractTermValuedItem;
  TFhirContractTermValuedItemList = fhir3_resources_other.TFhirContractTermValuedItemList;
  TFhirContractFriendly = fhir3_resources_other.TFhirContractFriendly;
  TFhirContractFriendlyList = fhir3_resources_other.TFhirContractFriendlyList;
  TFhirContractLegal = fhir3_resources_other.TFhirContractLegal;
  TFhirContractLegalList = fhir3_resources_other.TFhirContractLegalList;
  TFhirContractRule = fhir3_resources_other.TFhirContractRule;
  TFhirContractRuleList = fhir3_resources_other.TFhirContractRuleList;
  TFhirContract = fhir3_resources_other.TFhirContract;
  TFhirContractList = fhir3_resources_other.TFhirContractList;
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_ELIGIBILITYREQUEST}
  TFhirEligibilityRequest = fhir3_resources_other.TFhirEligibilityRequest;
  TFhirEligibilityRequestList = fhir3_resources_other.TFhirEligibilityRequestList;
{$ENDIF FHIR_ELIGIBILITYREQUEST}
{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  TFhirEligibilityResponseInsurance = fhir3_resources_other.TFhirEligibilityResponseInsurance;
  TFhirEligibilityResponseInsuranceList = fhir3_resources_other.TFhirEligibilityResponseInsuranceList;
  TFhirEligibilityResponseInsuranceBenefitBalance = fhir3_resources_other.TFhirEligibilityResponseInsuranceBenefitBalance;
  TFhirEligibilityResponseInsuranceBenefitBalanceList = fhir3_resources_other.TFhirEligibilityResponseInsuranceBenefitBalanceList;
  TFhirEligibilityResponseInsuranceBenefitBalanceFinancial = fhir3_resources_other.TFhirEligibilityResponseInsuranceBenefitBalanceFinancial;
  TFhirEligibilityResponseInsuranceBenefitBalanceFinancialList = fhir3_resources_other.TFhirEligibilityResponseInsuranceBenefitBalanceFinancialList;
  TFhirEligibilityResponseError = fhir3_resources_other.TFhirEligibilityResponseError;
  TFhirEligibilityResponseErrorList = fhir3_resources_other.TFhirEligibilityResponseErrorList;
  TFhirEligibilityResponse = fhir3_resources_other.TFhirEligibilityResponse;
  TFhirEligibilityResponseList = fhir3_resources_other.TFhirEligibilityResponseList;
{$ENDIF FHIR_ELIGIBILITYRESPONSE}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  TFhirEnrollmentRequest = fhir3_resources_other.TFhirEnrollmentRequest;
  TFhirEnrollmentRequestList = fhir3_resources_other.TFhirEnrollmentRequestList;
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  TFhirEnrollmentResponse = fhir3_resources_other.TFhirEnrollmentResponse;
  TFhirEnrollmentResponseList = fhir3_resources_other.TFhirEnrollmentResponseList;
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  TFhirExplanationOfBenefitRelated = fhir3_resources_other.TFhirExplanationOfBenefitRelated;
  TFhirExplanationOfBenefitRelatedList = fhir3_resources_other.TFhirExplanationOfBenefitRelatedList;
  TFhirExplanationOfBenefitPayee = fhir3_resources_other.TFhirExplanationOfBenefitPayee;
  TFhirExplanationOfBenefitPayeeList = fhir3_resources_other.TFhirExplanationOfBenefitPayeeList;
  TFhirExplanationOfBenefitInformation = fhir3_resources_other.TFhirExplanationOfBenefitInformation;
  TFhirExplanationOfBenefitInformationList = fhir3_resources_other.TFhirExplanationOfBenefitInformationList;
  TFhirExplanationOfBenefitCareTeam = fhir3_resources_other.TFhirExplanationOfBenefitCareTeam;
  TFhirExplanationOfBenefitCareTeamList = fhir3_resources_other.TFhirExplanationOfBenefitCareTeamList;
  TFhirExplanationOfBenefitDiagnosis = fhir3_resources_other.TFhirExplanationOfBenefitDiagnosis;
  TFhirExplanationOfBenefitDiagnosisList = fhir3_resources_other.TFhirExplanationOfBenefitDiagnosisList;
  TFhirExplanationOfBenefitProcedure = fhir3_resources_other.TFhirExplanationOfBenefitProcedure;
  TFhirExplanationOfBenefitProcedureList = fhir3_resources_other.TFhirExplanationOfBenefitProcedureList;
  TFhirExplanationOfBenefitInsurance = fhir3_resources_other.TFhirExplanationOfBenefitInsurance;
  TFhirExplanationOfBenefitInsuranceList = fhir3_resources_other.TFhirExplanationOfBenefitInsuranceList;
  TFhirExplanationOfBenefitAccident = fhir3_resources_other.TFhirExplanationOfBenefitAccident;
  TFhirExplanationOfBenefitAccidentList = fhir3_resources_other.TFhirExplanationOfBenefitAccidentList;
  TFhirExplanationOfBenefitItem = fhir3_resources_other.TFhirExplanationOfBenefitItem;
  TFhirExplanationOfBenefitItemList = fhir3_resources_other.TFhirExplanationOfBenefitItemList;
  TFhirExplanationOfBenefitItemAdjudication = fhir3_resources_other.TFhirExplanationOfBenefitItemAdjudication;
  TFhirExplanationOfBenefitItemAdjudicationList = fhir3_resources_other.TFhirExplanationOfBenefitItemAdjudicationList;
  TFhirExplanationOfBenefitItemDetail = fhir3_resources_other.TFhirExplanationOfBenefitItemDetail;
  TFhirExplanationOfBenefitItemDetailList = fhir3_resources_other.TFhirExplanationOfBenefitItemDetailList;
  TFhirExplanationOfBenefitItemDetailSubDetail = fhir3_resources_other.TFhirExplanationOfBenefitItemDetailSubDetail;
  TFhirExplanationOfBenefitItemDetailSubDetailList = fhir3_resources_other.TFhirExplanationOfBenefitItemDetailSubDetailList;
  TFhirExplanationOfBenefitAddItem = fhir3_resources_other.TFhirExplanationOfBenefitAddItem;
  TFhirExplanationOfBenefitAddItemList = fhir3_resources_other.TFhirExplanationOfBenefitAddItemList;
  TFhirExplanationOfBenefitAddItemDetail = fhir3_resources_other.TFhirExplanationOfBenefitAddItemDetail;
  TFhirExplanationOfBenefitAddItemDetailList = fhir3_resources_other.TFhirExplanationOfBenefitAddItemDetailList;
  TFhirExplanationOfBenefitPayment = fhir3_resources_other.TFhirExplanationOfBenefitPayment;
  TFhirExplanationOfBenefitPaymentList = fhir3_resources_other.TFhirExplanationOfBenefitPaymentList;
  TFhirExplanationOfBenefitProcessNote = fhir3_resources_other.TFhirExplanationOfBenefitProcessNote;
  TFhirExplanationOfBenefitProcessNoteList = fhir3_resources_other.TFhirExplanationOfBenefitProcessNoteList;
  TFhirExplanationOfBenefitBenefitBalance = fhir3_resources_other.TFhirExplanationOfBenefitBenefitBalance;
  TFhirExplanationOfBenefitBenefitBalanceList = fhir3_resources_other.TFhirExplanationOfBenefitBenefitBalanceList;
  TFhirExplanationOfBenefitBenefitBalanceFinancial = fhir3_resources_other.TFhirExplanationOfBenefitBenefitBalanceFinancial;
  TFhirExplanationOfBenefitBenefitBalanceFinancialList = fhir3_resources_other.TFhirExplanationOfBenefitBenefitBalanceFinancialList;
  TFhirExplanationOfBenefit = fhir3_resources_other.TFhirExplanationOfBenefit;
  TFhirExplanationOfBenefitList = fhir3_resources_other.TFhirExplanationOfBenefitList;
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_GUIDANCERESPONSE}
  TFhirGuidanceResponse = fhir3_resources_other.TFhirGuidanceResponse;
  TFhirGuidanceResponseList = fhir3_resources_other.TFhirGuidanceResponseList;
{$ENDIF FHIR_GUIDANCERESPONSE}
{$IFDEF FHIR_LINKAGE}
  TFhirLinkageItem = fhir3_resources_other.TFhirLinkageItem;
  TFhirLinkageItemList = fhir3_resources_other.TFhirLinkageItemList;
  TFhirLinkage = fhir3_resources_other.TFhirLinkage;
  TFhirLinkageList = fhir3_resources_other.TFhirLinkageList;
{$ENDIF FHIR_LINKAGE}
{$IFDEF FHIR_LIST}
  TFhirListEntry = fhir3_resources_other.TFhirListEntry;
  TFhirListEntryList = fhir3_resources_other.TFhirListEntryList;
  TFhirList = fhir3_resources_other.TFhirList;
  TFhirListList = fhir3_resources_other.TFhirListList;
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_MEASUREREPORT}
  TFhirMeasureReportGroup = fhir3_resources_other.TFhirMeasureReportGroup;
  TFhirMeasureReportGroupList = fhir3_resources_other.TFhirMeasureReportGroupList;
  TFhirMeasureReportGroupPopulation = fhir3_resources_other.TFhirMeasureReportGroupPopulation;
  TFhirMeasureReportGroupPopulationList = fhir3_resources_other.TFhirMeasureReportGroupPopulationList;
  TFhirMeasureReportGroupStratifier = fhir3_resources_other.TFhirMeasureReportGroupStratifier;
  TFhirMeasureReportGroupStratifierList = fhir3_resources_other.TFhirMeasureReportGroupStratifierList;
  TFhirMeasureReportGroupStratifierStratum = fhir3_resources_other.TFhirMeasureReportGroupStratifierStratum;
  TFhirMeasureReportGroupStratifierStratumList = fhir3_resources_other.TFhirMeasureReportGroupStratifierStratumList;
  TFhirMeasureReportGroupStratifierStratumPopulation = fhir3_resources_other.TFhirMeasureReportGroupStratifierStratumPopulation;
  TFhirMeasureReportGroupStratifierStratumPopulationList = fhir3_resources_other.TFhirMeasureReportGroupStratifierStratumPopulationList;
  TFhirMeasureReport = fhir3_resources_other.TFhirMeasureReport;
  TFhirMeasureReportList = fhir3_resources_other.TFhirMeasureReportList;
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MEDICATION}
  TFhirMedicationIngredient = fhir3_resources_other.TFhirMedicationIngredient;
  TFhirMedicationIngredientList = fhir3_resources_other.TFhirMedicationIngredientList;
  TFhirMedicationPackage = fhir3_resources_other.TFhirMedicationPackage;
  TFhirMedicationPackageList = fhir3_resources_other.TFhirMedicationPackageList;
  TFhirMedicationPackageContent = fhir3_resources_other.TFhirMedicationPackageContent;
  TFhirMedicationPackageContentList = fhir3_resources_other.TFhirMedicationPackageContentList;
  TFhirMedicationPackageBatch = fhir3_resources_other.TFhirMedicationPackageBatch;
  TFhirMedicationPackageBatchList = fhir3_resources_other.TFhirMedicationPackageBatchList;
  TFhirMedication = fhir3_resources_other.TFhirMedication;
  TFhirMedicationList = fhir3_resources_other.TFhirMedicationList;
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  TFhirOperationOutcomeIssue = fhir3_resources_other.TFhirOperationOutcomeIssue;
  TFhirOperationOutcomeIssueList = fhir3_resources_other.TFhirOperationOutcomeIssueList;
  TFhirOperationOutcome = fhir3_resources_other.TFhirOperationOutcome;
  TFhirOperationOutcomeList = fhir3_resources_other.TFhirOperationOutcomeList;
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_MESSAGEHEADER}
  TFhirMessageHeaderDestination = fhir3_resources_other.TFhirMessageHeaderDestination;
  TFhirMessageHeaderDestinationList = fhir3_resources_other.TFhirMessageHeaderDestinationList;
  TFhirMessageHeaderSource = fhir3_resources_other.TFhirMessageHeaderSource;
  TFhirMessageHeaderSourceList = fhir3_resources_other.TFhirMessageHeaderSourceList;
  TFhirMessageHeaderResponse = fhir3_resources_other.TFhirMessageHeaderResponse;
  TFhirMessageHeaderResponseList = fhir3_resources_other.TFhirMessageHeaderResponseList;
  TFhirMessageHeader = fhir3_resources_other.TFhirMessageHeader;
  TFhirMessageHeaderList = fhir3_resources_other.TFhirMessageHeaderList;
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_PAYMENTNOTICE}
  TFhirPaymentNotice = fhir3_resources_other.TFhirPaymentNotice;
  TFhirPaymentNoticeList = fhir3_resources_other.TFhirPaymentNoticeList;
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  TFhirPaymentReconciliationDetail = fhir3_resources_other.TFhirPaymentReconciliationDetail;
  TFhirPaymentReconciliationDetailList = fhir3_resources_other.TFhirPaymentReconciliationDetailList;
  TFhirPaymentReconciliationProcessNote = fhir3_resources_other.TFhirPaymentReconciliationProcessNote;
  TFhirPaymentReconciliationProcessNoteList = fhir3_resources_other.TFhirPaymentReconciliationProcessNoteList;
  TFhirPaymentReconciliation = fhir3_resources_other.TFhirPaymentReconciliation;
  TFhirPaymentReconciliationList = fhir3_resources_other.TFhirPaymentReconciliationList;
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PROCESSREQUEST}
  TFhirProcessRequestItem = fhir3_resources_other.TFhirProcessRequestItem;
  TFhirProcessRequestItemList = fhir3_resources_other.TFhirProcessRequestItemList;
  TFhirProcessRequest = fhir3_resources_other.TFhirProcessRequest;
  TFhirProcessRequestList = fhir3_resources_other.TFhirProcessRequestList;
{$ENDIF FHIR_PROCESSREQUEST}
{$IFDEF FHIR_PROCESSRESPONSE}
  TFhirProcessResponseProcessNote = fhir3_resources_other.TFhirProcessResponseProcessNote;
  TFhirProcessResponseProcessNoteList = fhir3_resources_other.TFhirProcessResponseProcessNoteList;
  TFhirProcessResponse = fhir3_resources_other.TFhirProcessResponse;
  TFhirProcessResponseList = fhir3_resources_other.TFhirProcessResponseList;
{$ENDIF FHIR_PROCESSRESPONSE}
{$IFDEF FHIR_PROVENANCE}
  TFhirProvenanceAgent = fhir3_resources_other.TFhirProvenanceAgent;
  TFhirProvenanceAgentList = fhir3_resources_other.TFhirProvenanceAgentList;
  TFhirProvenanceEntity = fhir3_resources_other.TFhirProvenanceEntity;
  TFhirProvenanceEntityList = fhir3_resources_other.TFhirProvenanceEntityList;
  TFhirProvenance = fhir3_resources_other.TFhirProvenance;
  TFhirProvenanceList = fhir3_resources_other.TFhirProvenanceList;
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_RESEARCHSTUDY}
  TFhirResearchStudyArm = fhir3_resources_other.TFhirResearchStudyArm;
  TFhirResearchStudyArmList = fhir3_resources_other.TFhirResearchStudyArmList;
  TFhirResearchStudy = fhir3_resources_other.TFhirResearchStudy;
  TFhirResearchStudyList = fhir3_resources_other.TFhirResearchStudyList;
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  TFhirResearchSubject = fhir3_resources_other.TFhirResearchSubject;
  TFhirResearchSubjectList = fhir3_resources_other.TFhirResearchSubjectList;
{$ENDIF FHIR_RESEARCHSUBJECT}
{$IFDEF FHIR_SUBSCRIPTION}
  TFhirSubscriptionChannel = fhir3_resources_other.TFhirSubscriptionChannel;
  TFhirSubscriptionChannelList = fhir3_resources_other.TFhirSubscriptionChannelList;
  TFhirSubscription = fhir3_resources_other.TFhirSubscription;
  TFhirSubscriptionList = fhir3_resources_other.TFhirSubscriptionList;
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_TASK}
  TFhirTaskRequester = fhir3_resources_other.TFhirTaskRequester;
  TFhirTaskRequesterList = fhir3_resources_other.TFhirTaskRequesterList;
  TFhirTaskRestriction = fhir3_resources_other.TFhirTaskRestriction;
  TFhirTaskRestrictionList = fhir3_resources_other.TFhirTaskRestrictionList;
  TFhirTaskInput = fhir3_resources_other.TFhirTaskInput;
  TFhirTaskInputList = fhir3_resources_other.TFhirTaskInputList;
  TFhirTaskOutput = fhir3_resources_other.TFhirTaskOutput;
  TFhirTaskOutputList = fhir3_resources_other.TFhirTaskOutputList;
  TFhirTask = fhir3_resources_other.TFhirTask;
  TFhirTaskList = fhir3_resources_other.TFhirTaskList;
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TESTREPORT}
  TFhirTestReportParticipant = fhir3_resources_other.TFhirTestReportParticipant;
  TFhirTestReportParticipantList = fhir3_resources_other.TFhirTestReportParticipantList;
  TFhirTestReportSetup = fhir3_resources_other.TFhirTestReportSetup;
  TFhirTestReportSetupList = fhir3_resources_other.TFhirTestReportSetupList;
  TFhirTestReportSetupAction = fhir3_resources_other.TFhirTestReportSetupAction;
  TFhirTestReportSetupActionList = fhir3_resources_other.TFhirTestReportSetupActionList;
  TFhirTestReportSetupActionOperation = fhir3_resources_other.TFhirTestReportSetupActionOperation;
  TFhirTestReportSetupActionOperationList = fhir3_resources_other.TFhirTestReportSetupActionOperationList;
  TFhirTestReportSetupActionAssert = fhir3_resources_other.TFhirTestReportSetupActionAssert;
  TFhirTestReportSetupActionAssertList = fhir3_resources_other.TFhirTestReportSetupActionAssertList;
  TFhirTestReportTest = fhir3_resources_other.TFhirTestReportTest;
  TFhirTestReportTestList = fhir3_resources_other.TFhirTestReportTestList;
  TFhirTestReportTestAction = fhir3_resources_other.TFhirTestReportTestAction;
  TFhirTestReportTestActionList = fhir3_resources_other.TFhirTestReportTestActionList;
  TFhirTestReportTeardown = fhir3_resources_other.TFhirTestReportTeardown;
  TFhirTestReportTeardownList = fhir3_resources_other.TFhirTestReportTeardownList;
  TFhirTestReportTeardownAction = fhir3_resources_other.TFhirTestReportTeardownAction;
  TFhirTestReportTeardownActionList = fhir3_resources_other.TFhirTestReportTeardownActionList;
  TFhirTestReport = fhir3_resources_other.TFhirTestReport;
  TFhirTestReportList = fhir3_resources_other.TFhirTestReportList;
{$ENDIF FHIR_TESTREPORT}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  TFhirActivityDefinitionParticipant = fhir3_resources_canonical.TFhirActivityDefinitionParticipant;
  TFhirActivityDefinitionParticipantList = fhir3_resources_canonical.TFhirActivityDefinitionParticipantList;
  TFhirActivityDefinitionDynamicValue = fhir3_resources_canonical.TFhirActivityDefinitionDynamicValue;
  TFhirActivityDefinitionDynamicValueList = fhir3_resources_canonical.TFhirActivityDefinitionDynamicValueList;
  TFhirActivityDefinition = fhir3_resources_canonical.TFhirActivityDefinition;
  TFhirActivityDefinitionList = fhir3_resources_canonical.TFhirActivityDefinitionList;
{$ENDIF FHIR_ACTIVITYDEFINITION}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  TFhirCapabilityStatementSoftware = fhir3_resources_canonical.TFhirCapabilityStatementSoftware;
  TFhirCapabilityStatementSoftwareList = fhir3_resources_canonical.TFhirCapabilityStatementSoftwareList;
  TFhirCapabilityStatementImplementation = fhir3_resources_canonical.TFhirCapabilityStatementImplementation;
  TFhirCapabilityStatementImplementationList = fhir3_resources_canonical.TFhirCapabilityStatementImplementationList;
  TFhirCapabilityStatementRest = fhir3_resources_canonical.TFhirCapabilityStatementRest;
  TFhirCapabilityStatementRestList = fhir3_resources_canonical.TFhirCapabilityStatementRestList;
  TFhirCapabilityStatementRestSecurity = fhir3_resources_canonical.TFhirCapabilityStatementRestSecurity;
  TFhirCapabilityStatementRestSecurityList = fhir3_resources_canonical.TFhirCapabilityStatementRestSecurityList;
  TFhirCapabilityStatementRestSecurityCertificate = fhir3_resources_canonical.TFhirCapabilityStatementRestSecurityCertificate;
  TFhirCapabilityStatementRestSecurityCertificateList = fhir3_resources_canonical.TFhirCapabilityStatementRestSecurityCertificateList;
  TFhirCapabilityStatementRestResource = fhir3_resources_canonical.TFhirCapabilityStatementRestResource;
  TFhirCapabilityStatementRestResourceList = fhir3_resources_canonical.TFhirCapabilityStatementRestResourceList;
  TFhirCapabilityStatementRestResourceInteraction = fhir3_resources_canonical.TFhirCapabilityStatementRestResourceInteraction;
  TFhirCapabilityStatementRestResourceInteractionList = fhir3_resources_canonical.TFhirCapabilityStatementRestResourceInteractionList;
  TFhirCapabilityStatementRestResourceSearchParam = fhir3_resources_canonical.TFhirCapabilityStatementRestResourceSearchParam;
  TFhirCapabilityStatementRestResourceSearchParamList = fhir3_resources_canonical.TFhirCapabilityStatementRestResourceSearchParamList;
  TFhirCapabilityStatementRestInteraction = fhir3_resources_canonical.TFhirCapabilityStatementRestInteraction;
  TFhirCapabilityStatementRestInteractionList = fhir3_resources_canonical.TFhirCapabilityStatementRestInteractionList;
  TFhirCapabilityStatementRestOperation = fhir3_resources_canonical.TFhirCapabilityStatementRestOperation;
  TFhirCapabilityStatementRestOperationList = fhir3_resources_canonical.TFhirCapabilityStatementRestOperationList;
  TFhirCapabilityStatementMessaging = fhir3_resources_canonical.TFhirCapabilityStatementMessaging;
  TFhirCapabilityStatementMessagingList = fhir3_resources_canonical.TFhirCapabilityStatementMessagingList;
  TFhirCapabilityStatementMessagingEndpoint = fhir3_resources_canonical.TFhirCapabilityStatementMessagingEndpoint;
  TFhirCapabilityStatementMessagingEndpointList = fhir3_resources_canonical.TFhirCapabilityStatementMessagingEndpointList;
  TFhirCapabilityStatementMessagingSupportedMessage = fhir3_resources_canonical.TFhirCapabilityStatementMessagingSupportedMessage;
  TFhirCapabilityStatementMessagingSupportedMessageList = fhir3_resources_canonical.TFhirCapabilityStatementMessagingSupportedMessageList;
  TFhirCapabilityStatementMessagingEvent = fhir3_resources_canonical.TFhirCapabilityStatementMessagingEvent;
  TFhirCapabilityStatementMessagingEventList = fhir3_resources_canonical.TFhirCapabilityStatementMessagingEventList;
  TFhirCapabilityStatementDocument = fhir3_resources_canonical.TFhirCapabilityStatementDocument;
  TFhirCapabilityStatementDocumentList = fhir3_resources_canonical.TFhirCapabilityStatementDocumentList;
  TFhirCapabilityStatement = fhir3_resources_canonical.TFhirCapabilityStatement;
  TFhirCapabilityStatementList = fhir3_resources_canonical.TFhirCapabilityStatementList;
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CODESYSTEM}
  TFhirCodeSystemFilter = fhir3_resources_canonical.TFhirCodeSystemFilter;
  TFhirCodeSystemFilterList = fhir3_resources_canonical.TFhirCodeSystemFilterList;
  TFhirCodeSystemProperty = fhir3_resources_canonical.TFhirCodeSystemProperty;
  TFhirCodeSystemPropertyList = fhir3_resources_canonical.TFhirCodeSystemPropertyList;
  TFhirCodeSystemConcept = fhir3_resources_canonical.TFhirCodeSystemConcept;
  TFhirCodeSystemConceptList = fhir3_resources_canonical.TFhirCodeSystemConceptList;
  TFhirCodeSystemConceptDesignation = fhir3_resources_canonical.TFhirCodeSystemConceptDesignation;
  TFhirCodeSystemConceptDesignationList = fhir3_resources_canonical.TFhirCodeSystemConceptDesignationList;
  TFhirCodeSystemConceptProperty = fhir3_resources_canonical.TFhirCodeSystemConceptProperty;
  TFhirCodeSystemConceptPropertyList = fhir3_resources_canonical.TFhirCodeSystemConceptPropertyList;
  TFhirCodeSystem = fhir3_resources_canonical.TFhirCodeSystem;
  TFhirCodeSystemList = fhir3_resources_canonical.TFhirCodeSystemList;
{$ENDIF FHIR_CODESYSTEM}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  TFhirCompartmentDefinitionResource = fhir3_resources_canonical.TFhirCompartmentDefinitionResource;
  TFhirCompartmentDefinitionResourceList = fhir3_resources_canonical.TFhirCompartmentDefinitionResourceList;
  TFhirCompartmentDefinition = fhir3_resources_canonical.TFhirCompartmentDefinition;
  TFhirCompartmentDefinitionList = fhir3_resources_canonical.TFhirCompartmentDefinitionList;
{$ENDIF FHIR_COMPARTMENTDEFINITION}
{$IFDEF FHIR_CONCEPTMAP}
  TFhirConceptMapGroup = fhir3_resources_canonical.TFhirConceptMapGroup;
  TFhirConceptMapGroupList = fhir3_resources_canonical.TFhirConceptMapGroupList;
  TFhirConceptMapGroupElement = fhir3_resources_canonical.TFhirConceptMapGroupElement;
  TFhirConceptMapGroupElementList = fhir3_resources_canonical.TFhirConceptMapGroupElementList;
  TFhirConceptMapGroupElementTarget = fhir3_resources_canonical.TFhirConceptMapGroupElementTarget;
  TFhirConceptMapGroupElementTargetList = fhir3_resources_canonical.TFhirConceptMapGroupElementTargetList;
  TFhirConceptMapGroupElementTargetDependsOn = fhir3_resources_canonical.TFhirConceptMapGroupElementTargetDependsOn;
  TFhirConceptMapGroupElementTargetDependsOnList = fhir3_resources_canonical.TFhirConceptMapGroupElementTargetDependsOnList;
  TFhirConceptMapGroupUnmapped = fhir3_resources_canonical.TFhirConceptMapGroupUnmapped;
  TFhirConceptMapGroupUnmappedList = fhir3_resources_canonical.TFhirConceptMapGroupUnmappedList;
  TFhirConceptMap = fhir3_resources_canonical.TFhirConceptMap;
  TFhirConceptMapList = fhir3_resources_canonical.TFhirConceptMapList;
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_DATAELEMENT}
  TFhirDataElementMapping = fhir3_resources_canonical.TFhirDataElementMapping;
  TFhirDataElementMappingList = fhir3_resources_canonical.TFhirDataElementMappingList;
  TFhirDataElement = fhir3_resources_canonical.TFhirDataElement;
  TFhirDataElementList = fhir3_resources_canonical.TFhirDataElementList;
{$ENDIF FHIR_DATAELEMENT}
{$IFDEF FHIR_EXPANSIONPROFILE}
  TFhirExpansionProfileFixedVersion = fhir3_resources_canonical.TFhirExpansionProfileFixedVersion;
  TFhirExpansionProfileFixedVersionList = fhir3_resources_canonical.TFhirExpansionProfileFixedVersionList;
  TFhirExpansionProfileExcludedSystem = fhir3_resources_canonical.TFhirExpansionProfileExcludedSystem;
  TFhirExpansionProfileExcludedSystemList = fhir3_resources_canonical.TFhirExpansionProfileExcludedSystemList;
  TFhirExpansionProfileDesignation = fhir3_resources_canonical.TFhirExpansionProfileDesignation;
  TFhirExpansionProfileDesignationList = fhir3_resources_canonical.TFhirExpansionProfileDesignationList;
  TFhirExpansionProfileDesignationInclude = fhir3_resources_canonical.TFhirExpansionProfileDesignationInclude;
  TFhirExpansionProfileDesignationIncludeList = fhir3_resources_canonical.TFhirExpansionProfileDesignationIncludeList;
  TFhirExpansionProfileDesignationIncludeDesignation = fhir3_resources_canonical.TFhirExpansionProfileDesignationIncludeDesignation;
  TFhirExpansionProfileDesignationIncludeDesignationList = fhir3_resources_canonical.TFhirExpansionProfileDesignationIncludeDesignationList;
  TFhirExpansionProfileDesignationExclude = fhir3_resources_canonical.TFhirExpansionProfileDesignationExclude;
  TFhirExpansionProfileDesignationExcludeList = fhir3_resources_canonical.TFhirExpansionProfileDesignationExcludeList;
  TFhirExpansionProfileDesignationExcludeDesignation = fhir3_resources_canonical.TFhirExpansionProfileDesignationExcludeDesignation;
  TFhirExpansionProfileDesignationExcludeDesignationList = fhir3_resources_canonical.TFhirExpansionProfileDesignationExcludeDesignationList;
  TFhirExpansionProfile = fhir3_resources_canonical.TFhirExpansionProfile;
  TFhirExpansionProfileList = fhir3_resources_canonical.TFhirExpansionProfileList;
{$ENDIF FHIR_EXPANSIONPROFILE}
{$IFDEF FHIR_GRAPHDEFINITION}
  TFhirGraphDefinitionLink = fhir3_resources_canonical.TFhirGraphDefinitionLink;
  TFhirGraphDefinitionLinkList = fhir3_resources_canonical.TFhirGraphDefinitionLinkList;
  TFhirGraphDefinitionLinkTarget = fhir3_resources_canonical.TFhirGraphDefinitionLinkTarget;
  TFhirGraphDefinitionLinkTargetList = fhir3_resources_canonical.TFhirGraphDefinitionLinkTargetList;
  TFhirGraphDefinitionLinkTargetCompartment = fhir3_resources_canonical.TFhirGraphDefinitionLinkTargetCompartment;
  TFhirGraphDefinitionLinkTargetCompartmentList = fhir3_resources_canonical.TFhirGraphDefinitionLinkTargetCompartmentList;
  TFhirGraphDefinition = fhir3_resources_canonical.TFhirGraphDefinition;
  TFhirGraphDefinitionList = fhir3_resources_canonical.TFhirGraphDefinitionList;
{$ENDIF FHIR_GRAPHDEFINITION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  TFhirImplementationGuideDependency = fhir3_resources_canonical.TFhirImplementationGuideDependency;
  TFhirImplementationGuideDependencyList = fhir3_resources_canonical.TFhirImplementationGuideDependencyList;
  TFhirImplementationGuidePackage = fhir3_resources_canonical.TFhirImplementationGuidePackage;
  TFhirImplementationGuidePackageList = fhir3_resources_canonical.TFhirImplementationGuidePackageList;
  TFhirImplementationGuidePackageResource = fhir3_resources_canonical.TFhirImplementationGuidePackageResource;
  TFhirImplementationGuidePackageResourceList = fhir3_resources_canonical.TFhirImplementationGuidePackageResourceList;
  TFhirImplementationGuideGlobal = fhir3_resources_canonical.TFhirImplementationGuideGlobal;
  TFhirImplementationGuideGlobalList = fhir3_resources_canonical.TFhirImplementationGuideGlobalList;
  TFhirImplementationGuidePage = fhir3_resources_canonical.TFhirImplementationGuidePage;
  TFhirImplementationGuidePageList = fhir3_resources_canonical.TFhirImplementationGuidePageList;
  TFhirImplementationGuide = fhir3_resources_canonical.TFhirImplementationGuide;
  TFhirImplementationGuideList = fhir3_resources_canonical.TFhirImplementationGuideList;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_LIBRARY}
  TFhirLibrary = fhir3_resources_canonical.TFhirLibrary;
  TFhirLibraryList = fhir3_resources_canonical.TFhirLibraryList;
{$ENDIF FHIR_LIBRARY}
{$IFDEF FHIR_MEASURE}
  TFhirMeasureGroup = fhir3_resources_canonical.TFhirMeasureGroup;
  TFhirMeasureGroupList = fhir3_resources_canonical.TFhirMeasureGroupList;
  TFhirMeasureGroupPopulation = fhir3_resources_canonical.TFhirMeasureGroupPopulation;
  TFhirMeasureGroupPopulationList = fhir3_resources_canonical.TFhirMeasureGroupPopulationList;
  TFhirMeasureGroupStratifier = fhir3_resources_canonical.TFhirMeasureGroupStratifier;
  TFhirMeasureGroupStratifierList = fhir3_resources_canonical.TFhirMeasureGroupStratifierList;
  TFhirMeasureSupplementalData = fhir3_resources_canonical.TFhirMeasureSupplementalData;
  TFhirMeasureSupplementalDataList = fhir3_resources_canonical.TFhirMeasureSupplementalDataList;
  TFhirMeasure = fhir3_resources_canonical.TFhirMeasure;
  TFhirMeasureList = fhir3_resources_canonical.TFhirMeasureList;
{$ENDIF FHIR_MEASURE}
{$IFDEF FHIR_MESSAGEDEFINITION}
  TFhirMessageDefinitionFocus = fhir3_resources_canonical.TFhirMessageDefinitionFocus;
  TFhirMessageDefinitionFocusList = fhir3_resources_canonical.TFhirMessageDefinitionFocusList;
  TFhirMessageDefinitionAllowedResponse = fhir3_resources_canonical.TFhirMessageDefinitionAllowedResponse;
  TFhirMessageDefinitionAllowedResponseList = fhir3_resources_canonical.TFhirMessageDefinitionAllowedResponseList;
  TFhirMessageDefinition = fhir3_resources_canonical.TFhirMessageDefinition;
  TFhirMessageDefinitionList = fhir3_resources_canonical.TFhirMessageDefinitionList;
{$ENDIF FHIR_MESSAGEDEFINITION}
{$IFDEF FHIR_NAMINGSYSTEM}
  TFhirNamingSystemUniqueId = fhir3_resources_canonical.TFhirNamingSystemUniqueId;
  TFhirNamingSystemUniqueIdList = fhir3_resources_canonical.TFhirNamingSystemUniqueIdList;
  TFhirNamingSystem = fhir3_resources_canonical.TFhirNamingSystem;
  TFhirNamingSystemList = fhir3_resources_canonical.TFhirNamingSystemList;
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_OPERATIONDEFINITION}
  TFhirOperationDefinitionParameter = fhir3_resources_canonical.TFhirOperationDefinitionParameter;
  TFhirOperationDefinitionParameterList = fhir3_resources_canonical.TFhirOperationDefinitionParameterList;
  TFhirOperationDefinitionParameterBinding = fhir3_resources_canonical.TFhirOperationDefinitionParameterBinding;
  TFhirOperationDefinitionParameterBindingList = fhir3_resources_canonical.TFhirOperationDefinitionParameterBindingList;
  TFhirOperationDefinitionOverload = fhir3_resources_canonical.TFhirOperationDefinitionOverload;
  TFhirOperationDefinitionOverloadList = fhir3_resources_canonical.TFhirOperationDefinitionOverloadList;
  TFhirOperationDefinition = fhir3_resources_canonical.TFhirOperationDefinition;
  TFhirOperationDefinitionList = fhir3_resources_canonical.TFhirOperationDefinitionList;
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_PLANDEFINITION}
  TFhirPlanDefinitionGoal = fhir3_resources_canonical.TFhirPlanDefinitionGoal;
  TFhirPlanDefinitionGoalList = fhir3_resources_canonical.TFhirPlanDefinitionGoalList;
  TFhirPlanDefinitionGoalTarget = fhir3_resources_canonical.TFhirPlanDefinitionGoalTarget;
  TFhirPlanDefinitionGoalTargetList = fhir3_resources_canonical.TFhirPlanDefinitionGoalTargetList;
  TFhirPlanDefinitionAction = fhir3_resources_canonical.TFhirPlanDefinitionAction;
  TFhirPlanDefinitionActionList = fhir3_resources_canonical.TFhirPlanDefinitionActionList;
  TFhirPlanDefinitionActionCondition = fhir3_resources_canonical.TFhirPlanDefinitionActionCondition;
  TFhirPlanDefinitionActionConditionList = fhir3_resources_canonical.TFhirPlanDefinitionActionConditionList;
  TFhirPlanDefinitionActionRelatedAction = fhir3_resources_canonical.TFhirPlanDefinitionActionRelatedAction;
  TFhirPlanDefinitionActionRelatedActionList = fhir3_resources_canonical.TFhirPlanDefinitionActionRelatedActionList;
  TFhirPlanDefinitionActionParticipant = fhir3_resources_canonical.TFhirPlanDefinitionActionParticipant;
  TFhirPlanDefinitionActionParticipantList = fhir3_resources_canonical.TFhirPlanDefinitionActionParticipantList;
  TFhirPlanDefinitionActionDynamicValue = fhir3_resources_canonical.TFhirPlanDefinitionActionDynamicValue;
  TFhirPlanDefinitionActionDynamicValueList = fhir3_resources_canonical.TFhirPlanDefinitionActionDynamicValueList;
  TFhirPlanDefinition = fhir3_resources_canonical.TFhirPlanDefinition;
  TFhirPlanDefinitionList = fhir3_resources_canonical.TFhirPlanDefinitionList;
{$ENDIF FHIR_PLANDEFINITION}
{$IFDEF FHIR_QUESTIONNAIRE}
  TFhirQuestionnaireItem = fhir3_resources_canonical.TFhirQuestionnaireItem;
  TFhirQuestionnaireItemList = fhir3_resources_canonical.TFhirQuestionnaireItemList;
  TFhirQuestionnaireItemEnableWhen = fhir3_resources_canonical.TFhirQuestionnaireItemEnableWhen;
  TFhirQuestionnaireItemEnableWhenList = fhir3_resources_canonical.TFhirQuestionnaireItemEnableWhenList;
  TFhirQuestionnaireItemOption = fhir3_resources_canonical.TFhirQuestionnaireItemOption;
  TFhirQuestionnaireItemOptionList = fhir3_resources_canonical.TFhirQuestionnaireItemOptionList;
  TFhirQuestionnaire = fhir3_resources_canonical.TFhirQuestionnaire;
  TFhirQuestionnaireList = fhir3_resources_canonical.TFhirQuestionnaireList;
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_REQUESTGROUP}
  TFhirRequestGroupAction = fhir3_resources_canonical.TFhirRequestGroupAction;
  TFhirRequestGroupActionList = fhir3_resources_canonical.TFhirRequestGroupActionList;
  TFhirRequestGroupActionCondition = fhir3_resources_canonical.TFhirRequestGroupActionCondition;
  TFhirRequestGroupActionConditionList = fhir3_resources_canonical.TFhirRequestGroupActionConditionList;
  TFhirRequestGroupActionRelatedAction = fhir3_resources_canonical.TFhirRequestGroupActionRelatedAction;
  TFhirRequestGroupActionRelatedActionList = fhir3_resources_canonical.TFhirRequestGroupActionRelatedActionList;
  TFhirRequestGroup = fhir3_resources_canonical.TFhirRequestGroup;
  TFhirRequestGroupList = fhir3_resources_canonical.TFhirRequestGroupList;
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_SEARCHPARAMETER}
  TFhirSearchParameterComponent = fhir3_resources_canonical.TFhirSearchParameterComponent;
  TFhirSearchParameterComponentList = fhir3_resources_canonical.TFhirSearchParameterComponentList;
  TFhirSearchParameter = fhir3_resources_canonical.TFhirSearchParameter;
  TFhirSearchParameterList = fhir3_resources_canonical.TFhirSearchParameterList;
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SERVICEDEFINITION}
  TFhirServiceDefinition = fhir3_resources_canonical.TFhirServiceDefinition;
  TFhirServiceDefinitionList = fhir3_resources_canonical.TFhirServiceDefinitionList;
{$ENDIF FHIR_SERVICEDEFINITION}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  TFhirStructureDefinitionMapping = fhir3_resources_canonical.TFhirStructureDefinitionMapping;
  TFhirStructureDefinitionMappingList = fhir3_resources_canonical.TFhirStructureDefinitionMappingList;
  TFhirStructureDefinitionSnapshot = fhir3_resources_canonical.TFhirStructureDefinitionSnapshot;
  TFhirStructureDefinitionSnapshotList = fhir3_resources_canonical.TFhirStructureDefinitionSnapshotList;
  TFhirStructureDefinitionDifferential = fhir3_resources_canonical.TFhirStructureDefinitionDifferential;
  TFhirStructureDefinitionDifferentialList = fhir3_resources_canonical.TFhirStructureDefinitionDifferentialList;
  TFhirStructureDefinition = fhir3_resources_canonical.TFhirStructureDefinition;
  TFhirStructureDefinitionList = fhir3_resources_canonical.TFhirStructureDefinitionList;
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_STRUCTUREMAP}
  TFhirStructureMapStructure = fhir3_resources_canonical.TFhirStructureMapStructure;
  TFhirStructureMapStructureList = fhir3_resources_canonical.TFhirStructureMapStructureList;
  TFhirStructureMapGroup = fhir3_resources_canonical.TFhirStructureMapGroup;
  TFhirStructureMapGroupList = fhir3_resources_canonical.TFhirStructureMapGroupList;
  TFhirStructureMapGroupInput = fhir3_resources_canonical.TFhirStructureMapGroupInput;
  TFhirStructureMapGroupInputList = fhir3_resources_canonical.TFhirStructureMapGroupInputList;
  TFhirStructureMapGroupRule = fhir3_resources_canonical.TFhirStructureMapGroupRule;
  TFhirStructureMapGroupRuleList = fhir3_resources_canonical.TFhirStructureMapGroupRuleList;
  TFhirStructureMapGroupRuleSource = fhir3_resources_canonical.TFhirStructureMapGroupRuleSource;
  TFhirStructureMapGroupRuleSourceList = fhir3_resources_canonical.TFhirStructureMapGroupRuleSourceList;
  TFhirStructureMapGroupRuleTarget = fhir3_resources_canonical.TFhirStructureMapGroupRuleTarget;
  TFhirStructureMapGroupRuleTargetList = fhir3_resources_canonical.TFhirStructureMapGroupRuleTargetList;
  TFhirStructureMapGroupRuleTargetParameter = fhir3_resources_canonical.TFhirStructureMapGroupRuleTargetParameter;
  TFhirStructureMapGroupRuleTargetParameterList = fhir3_resources_canonical.TFhirStructureMapGroupRuleTargetParameterList;
  TFhirStructureMapGroupRuleDependent = fhir3_resources_canonical.TFhirStructureMapGroupRuleDependent;
  TFhirStructureMapGroupRuleDependentList = fhir3_resources_canonical.TFhirStructureMapGroupRuleDependentList;
  TFhirStructureMap = fhir3_resources_canonical.TFhirStructureMap;
  TFhirStructureMapList = fhir3_resources_canonical.TFhirStructureMapList;
{$ENDIF FHIR_STRUCTUREMAP}
{$IFDEF FHIR_TESTSCRIPT}
  TFhirTestScriptOrigin = fhir3_resources_canonical.TFhirTestScriptOrigin;
  TFhirTestScriptOriginList = fhir3_resources_canonical.TFhirTestScriptOriginList;
  TFhirTestScriptDestination = fhir3_resources_canonical.TFhirTestScriptDestination;
  TFhirTestScriptDestinationList = fhir3_resources_canonical.TFhirTestScriptDestinationList;
  TFhirTestScriptMetadata = fhir3_resources_canonical.TFhirTestScriptMetadata;
  TFhirTestScriptMetadataList = fhir3_resources_canonical.TFhirTestScriptMetadataList;
  TFhirTestScriptMetadataLink = fhir3_resources_canonical.TFhirTestScriptMetadataLink;
  TFhirTestScriptMetadataLinkList = fhir3_resources_canonical.TFhirTestScriptMetadataLinkList;
  TFhirTestScriptMetadataCapability = fhir3_resources_canonical.TFhirTestScriptMetadataCapability;
  TFhirTestScriptMetadataCapabilityList = fhir3_resources_canonical.TFhirTestScriptMetadataCapabilityList;
  TFhirTestScriptFixture = fhir3_resources_canonical.TFhirTestScriptFixture;
  TFhirTestScriptFixtureList = fhir3_resources_canonical.TFhirTestScriptFixtureList;
  TFhirTestScriptVariable = fhir3_resources_canonical.TFhirTestScriptVariable;
  TFhirTestScriptVariableList = fhir3_resources_canonical.TFhirTestScriptVariableList;
  TFhirTestScriptRule = fhir3_resources_canonical.TFhirTestScriptRule;
  TFhirTestScriptRuleList = fhir3_resources_canonical.TFhirTestScriptRuleList;
  TFhirTestScriptRuleParam = fhir3_resources_canonical.TFhirTestScriptRuleParam;
  TFhirTestScriptRuleParamList = fhir3_resources_canonical.TFhirTestScriptRuleParamList;
  TFhirTestScriptRuleset = fhir3_resources_canonical.TFhirTestScriptRuleset;
  TFhirTestScriptRulesetList = fhir3_resources_canonical.TFhirTestScriptRulesetList;
  TFhirTestScriptRulesetRule = fhir3_resources_canonical.TFhirTestScriptRulesetRule;
  TFhirTestScriptRulesetRuleList = fhir3_resources_canonical.TFhirTestScriptRulesetRuleList;
  TFhirTestScriptRulesetRuleParam = fhir3_resources_canonical.TFhirTestScriptRulesetRuleParam;
  TFhirTestScriptRulesetRuleParamList = fhir3_resources_canonical.TFhirTestScriptRulesetRuleParamList;
  TFhirTestScriptSetup = fhir3_resources_canonical.TFhirTestScriptSetup;
  TFhirTestScriptSetupList = fhir3_resources_canonical.TFhirTestScriptSetupList;
  TFhirTestScriptSetupAction = fhir3_resources_canonical.TFhirTestScriptSetupAction;
  TFhirTestScriptSetupActionList = fhir3_resources_canonical.TFhirTestScriptSetupActionList;
  TFhirTestScriptSetupActionOperation = fhir3_resources_canonical.TFhirTestScriptSetupActionOperation;
  TFhirTestScriptSetupActionOperationList = fhir3_resources_canonical.TFhirTestScriptSetupActionOperationList;
  TFhirTestScriptSetupActionOperationRequestHeader = fhir3_resources_canonical.TFhirTestScriptSetupActionOperationRequestHeader;
  TFhirTestScriptSetupActionOperationRequestHeaderList = fhir3_resources_canonical.TFhirTestScriptSetupActionOperationRequestHeaderList;
  TFhirTestScriptSetupActionAssert = fhir3_resources_canonical.TFhirTestScriptSetupActionAssert;
  TFhirTestScriptSetupActionAssertList = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertList;
  TFhirTestScriptSetupActionAssertRule = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertRule;
  TFhirTestScriptSetupActionAssertRuleList = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertRuleList;
  TFhirTestScriptSetupActionAssertRuleParam = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertRuleParam;
  TFhirTestScriptSetupActionAssertRuleParamList = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertRuleParamList;
  TFhirTestScriptSetupActionAssertRuleset = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertRuleset;
  TFhirTestScriptSetupActionAssertRulesetList = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertRulesetList;
  TFhirTestScriptSetupActionAssertRulesetRule = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertRulesetRule;
  TFhirTestScriptSetupActionAssertRulesetRuleList = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertRulesetRuleList;
  TFhirTestScriptSetupActionAssertRulesetRuleParam = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertRulesetRuleParam;
  TFhirTestScriptSetupActionAssertRulesetRuleParamList = fhir3_resources_canonical.TFhirTestScriptSetupActionAssertRulesetRuleParamList;
  TFhirTestScriptTest = fhir3_resources_canonical.TFhirTestScriptTest;
  TFhirTestScriptTestList = fhir3_resources_canonical.TFhirTestScriptTestList;
  TFhirTestScriptTestAction = fhir3_resources_canonical.TFhirTestScriptTestAction;
  TFhirTestScriptTestActionList = fhir3_resources_canonical.TFhirTestScriptTestActionList;
  TFhirTestScriptTeardown = fhir3_resources_canonical.TFhirTestScriptTeardown;
  TFhirTestScriptTeardownList = fhir3_resources_canonical.TFhirTestScriptTeardownList;
  TFhirTestScriptTeardownAction = fhir3_resources_canonical.TFhirTestScriptTeardownAction;
  TFhirTestScriptTeardownActionList = fhir3_resources_canonical.TFhirTestScriptTeardownActionList;
  TFhirTestScript = fhir3_resources_canonical.TFhirTestScript;
  TFhirTestScriptList = fhir3_resources_canonical.TFhirTestScriptList;
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  TFhirValueSetCompose = fhir3_resources_canonical.TFhirValueSetCompose;
  TFhirValueSetComposeList = fhir3_resources_canonical.TFhirValueSetComposeList;
  TFhirValueSetComposeInclude = fhir3_resources_canonical.TFhirValueSetComposeInclude;
  TFhirValueSetComposeIncludeList = fhir3_resources_canonical.TFhirValueSetComposeIncludeList;
  TFhirValueSetComposeIncludeConcept = fhir3_resources_canonical.TFhirValueSetComposeIncludeConcept;
  TFhirValueSetComposeIncludeConceptList = fhir3_resources_canonical.TFhirValueSetComposeIncludeConceptList;
  TFhirValueSetComposeIncludeConceptDesignation = fhir3_resources_canonical.TFhirValueSetComposeIncludeConceptDesignation;
  TFhirValueSetComposeIncludeConceptDesignationList = fhir3_resources_canonical.TFhirValueSetComposeIncludeConceptDesignationList;
  TFhirValueSetComposeIncludeFilter = fhir3_resources_canonical.TFhirValueSetComposeIncludeFilter;
  TFhirValueSetComposeIncludeFilterList = fhir3_resources_canonical.TFhirValueSetComposeIncludeFilterList;
  TFhirValueSetExpansion = fhir3_resources_canonical.TFhirValueSetExpansion;
  TFhirValueSetExpansionList = fhir3_resources_canonical.TFhirValueSetExpansionList;
  TFhirValueSetExpansionParameter = fhir3_resources_canonical.TFhirValueSetExpansionParameter;
  TFhirValueSetExpansionParameterList = fhir3_resources_canonical.TFhirValueSetExpansionParameterList;
  TFhirValueSetExpansionContains = fhir3_resources_canonical.TFhirValueSetExpansionContains;
  TFhirValueSetExpansionContainsList = fhir3_resources_canonical.TFhirValueSetExpansionContainsList;
  TFhirValueSet = fhir3_resources_canonical.TFhirValueSet;
  TFhirValueSetList = fhir3_resources_canonical.TFhirValueSetList;
{$ENDIF FHIR_VALUESET}

implementation

end.

