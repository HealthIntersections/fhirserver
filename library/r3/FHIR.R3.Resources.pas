unit FHIR.R3.Resources;

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
{$I FHIR.R3.inc}

interface

// FHIR v3.0.1 generated 2018-06-12T19:15:59+10:00

uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Utilities, FHIR.Base.Lang,
  FHIR.R3.Base, FHIR.R3.Types, FHIR.R3.Resources.Base,
  FHIR.R3.Resources.Admin, FHIR.R3.Resources.Canonical, FHIR.R3.Resources.Clinical, FHIR.R3.Resources.Other;

Type
  TFhirResourceType = FHIR.R3.Resources.Base.TFhirResourceType;
  TFhirResourceTypeSet = FHIR.R3.Resources.Base.TFhirResourceTypeSet;
Type
  TFhirResource = FHIR.R3.Resources.Base.TFhirResource;
  TFhirResourceClass = FHIR.R3.Resources.Base.TFhirResourceClass;
  TFhirResourceList = FHIR.R3.Resources.Base.TFhirResourceList;
  TFhirDomainResource = FHIR.R3.Resources.Base.TFhirDomainResource;
  TFhirDomainResourceList = FHIR.R3.Resources.Base.TFhirDomainResourceList;
  TFhirMetadataResourceList = FHIR.R3.Resources.Canonical.TFhirMetadataResourceList;
  TFhirMetadataResource = FHIR.R3.Resources.Canonical.TFhirMetadataResource;

{$IFDEF FHIR_DEVICE}
  TFhirDeviceUdi = FHIR.R3.Resources.Admin.TFhirDeviceUdi;
  TFhirDeviceUdiList = FHIR.R3.Resources.Admin.TFhirDeviceUdiList;
  TFhirDevice = FHIR.R3.Resources.Admin.TFhirDevice;
  TFhirDeviceList = FHIR.R3.Resources.Admin.TFhirDeviceList;
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICECOMPONENT}
  TFhirDeviceComponentProductionSpecification = FHIR.R3.Resources.Admin.TFhirDeviceComponentProductionSpecification;
  TFhirDeviceComponentProductionSpecificationList = FHIR.R3.Resources.Admin.TFhirDeviceComponentProductionSpecificationList;
  TFhirDeviceComponent = FHIR.R3.Resources.Admin.TFhirDeviceComponent;
  TFhirDeviceComponentList = FHIR.R3.Resources.Admin.TFhirDeviceComponentList;
{$ENDIF FHIR_DEVICECOMPONENT}
{$IFDEF FHIR_DEVICEMETRIC}
  TFhirDeviceMetricCalibration = FHIR.R3.Resources.Admin.TFhirDeviceMetricCalibration;
  TFhirDeviceMetricCalibrationList = FHIR.R3.Resources.Admin.TFhirDeviceMetricCalibrationList;
  TFhirDeviceMetric = FHIR.R3.Resources.Admin.TFhirDeviceMetric;
  TFhirDeviceMetricList = FHIR.R3.Resources.Admin.TFhirDeviceMetricList;
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_ENCOUNTER}
  TFhirEncounterStatusHistory = FHIR.R3.Resources.Admin.TFhirEncounterStatusHistory;
  TFhirEncounterStatusHistoryList = FHIR.R3.Resources.Admin.TFhirEncounterStatusHistoryList;
  TFhirEncounterClassHistory = FHIR.R3.Resources.Admin.TFhirEncounterClassHistory;
  TFhirEncounterClassHistoryList = FHIR.R3.Resources.Admin.TFhirEncounterClassHistoryList;
  TFhirEncounterParticipant = FHIR.R3.Resources.Admin.TFhirEncounterParticipant;
  TFhirEncounterParticipantList = FHIR.R3.Resources.Admin.TFhirEncounterParticipantList;
  TFhirEncounterDiagnosis = FHIR.R3.Resources.Admin.TFhirEncounterDiagnosis;
  TFhirEncounterDiagnosisList = FHIR.R3.Resources.Admin.TFhirEncounterDiagnosisList;
  TFhirEncounterHospitalization = FHIR.R3.Resources.Admin.TFhirEncounterHospitalization;
  TFhirEncounterHospitalizationList = FHIR.R3.Resources.Admin.TFhirEncounterHospitalizationList;
  TFhirEncounterLocation = FHIR.R3.Resources.Admin.TFhirEncounterLocation;
  TFhirEncounterLocationList = FHIR.R3.Resources.Admin.TFhirEncounterLocationList;
  TFhirEncounter = FHIR.R3.Resources.Admin.TFhirEncounter;
  TFhirEncounterList = FHIR.R3.Resources.Admin.TFhirEncounterList;
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_ENDPOINT}
  TFhirEndpoint = FHIR.R3.Resources.Admin.TFhirEndpoint;
  TFhirEndpointList = FHIR.R3.Resources.Admin.TFhirEndpointList;
{$ENDIF FHIR_ENDPOINT}
{$IFDEF FHIR_EPISODEOFCARE}
  TFhirEpisodeOfCareStatusHistory = FHIR.R3.Resources.Admin.TFhirEpisodeOfCareStatusHistory;
  TFhirEpisodeOfCareStatusHistoryList = FHIR.R3.Resources.Admin.TFhirEpisodeOfCareStatusHistoryList;
  TFhirEpisodeOfCareDiagnosis = FHIR.R3.Resources.Admin.TFhirEpisodeOfCareDiagnosis;
  TFhirEpisodeOfCareDiagnosisList = FHIR.R3.Resources.Admin.TFhirEpisodeOfCareDiagnosisList;
  TFhirEpisodeOfCare = FHIR.R3.Resources.Admin.TFhirEpisodeOfCare;
  TFhirEpisodeOfCareList = FHIR.R3.Resources.Admin.TFhirEpisodeOfCareList;
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_GROUP}
  TFhirGroupCharacteristic = FHIR.R3.Resources.Admin.TFhirGroupCharacteristic;
  TFhirGroupCharacteristicList = FHIR.R3.Resources.Admin.TFhirGroupCharacteristicList;
  TFhirGroupMember = FHIR.R3.Resources.Admin.TFhirGroupMember;
  TFhirGroupMemberList = FHIR.R3.Resources.Admin.TFhirGroupMemberList;
  TFhirGroup = FHIR.R3.Resources.Admin.TFhirGroup;
  TFhirGroupList = FHIR.R3.Resources.Admin.TFhirGroupList;
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_HEALTHCARESERVICE}
  TFhirHealthcareServiceAvailableTime = FHIR.R3.Resources.Admin.TFhirHealthcareServiceAvailableTime;
  TFhirHealthcareServiceAvailableTimeList = FHIR.R3.Resources.Admin.TFhirHealthcareServiceAvailableTimeList;
  TFhirHealthcareServiceNotAvailable = FHIR.R3.Resources.Admin.TFhirHealthcareServiceNotAvailable;
  TFhirHealthcareServiceNotAvailableList = FHIR.R3.Resources.Admin.TFhirHealthcareServiceNotAvailableList;
  TFhirHealthcareService = FHIR.R3.Resources.Admin.TFhirHealthcareService;
  TFhirHealthcareServiceList = FHIR.R3.Resources.Admin.TFhirHealthcareServiceList;
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_LOCATION}
  TFhirLocationPosition = FHIR.R3.Resources.Admin.TFhirLocationPosition;
  TFhirLocationPositionList = FHIR.R3.Resources.Admin.TFhirLocationPositionList;
  TFhirLocation = FHIR.R3.Resources.Admin.TFhirLocation;
  TFhirLocationList = FHIR.R3.Resources.Admin.TFhirLocationList;
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_ORGANIZATION}
  TFhirOrganizationContact = FHIR.R3.Resources.Admin.TFhirOrganizationContact;
  TFhirOrganizationContactList = FHIR.R3.Resources.Admin.TFhirOrganizationContactList;
  TFhirOrganization = FHIR.R3.Resources.Admin.TFhirOrganization;
  TFhirOrganizationList = FHIR.R3.Resources.Admin.TFhirOrganizationList;
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_PATIENT}
  TFhirPatientContact = FHIR.R3.Resources.Admin.TFhirPatientContact;
  TFhirPatientContactList = FHIR.R3.Resources.Admin.TFhirPatientContactList;
  TFhirPatientAnimal = FHIR.R3.Resources.Admin.TFhirPatientAnimal;
  TFhirPatientAnimalList = FHIR.R3.Resources.Admin.TFhirPatientAnimalList;
  TFhirPatientCommunication = FHIR.R3.Resources.Admin.TFhirPatientCommunication;
  TFhirPatientCommunicationList = FHIR.R3.Resources.Admin.TFhirPatientCommunicationList;
  TFhirPatientLink = FHIR.R3.Resources.Admin.TFhirPatientLink;
  TFhirPatientLinkList = FHIR.R3.Resources.Admin.TFhirPatientLinkList;
  TFhirPatient = FHIR.R3.Resources.Admin.TFhirPatient;
  TFhirPatientList = FHIR.R3.Resources.Admin.TFhirPatientList;
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PERSON}
  TFhirPersonLink = FHIR.R3.Resources.Admin.TFhirPersonLink;
  TFhirPersonLinkList = FHIR.R3.Resources.Admin.TFhirPersonLinkList;
  TFhirPerson = FHIR.R3.Resources.Admin.TFhirPerson;
  TFhirPersonList = FHIR.R3.Resources.Admin.TFhirPersonList;
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PRACTITIONER}
  TFhirPractitionerQualification = FHIR.R3.Resources.Admin.TFhirPractitionerQualification;
  TFhirPractitionerQualificationList = FHIR.R3.Resources.Admin.TFhirPractitionerQualificationList;
  TFhirPractitioner = FHIR.R3.Resources.Admin.TFhirPractitioner;
  TFhirPractitionerList = FHIR.R3.Resources.Admin.TFhirPractitionerList;
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_PRACTITIONERROLE}
  TFhirPractitionerRoleAvailableTime = FHIR.R3.Resources.Admin.TFhirPractitionerRoleAvailableTime;
  TFhirPractitionerRoleAvailableTimeList = FHIR.R3.Resources.Admin.TFhirPractitionerRoleAvailableTimeList;
  TFhirPractitionerRoleNotAvailable = FHIR.R3.Resources.Admin.TFhirPractitionerRoleNotAvailable;
  TFhirPractitionerRoleNotAvailableList = FHIR.R3.Resources.Admin.TFhirPractitionerRoleNotAvailableList;
  TFhirPractitionerRole = FHIR.R3.Resources.Admin.TFhirPractitionerRole;
  TFhirPractitionerRoleList = FHIR.R3.Resources.Admin.TFhirPractitionerRoleList;
{$ENDIF FHIR_PRACTITIONERROLE}
{$IFDEF FHIR_RELATEDPERSON}
  TFhirRelatedPerson = FHIR.R3.Resources.Admin.TFhirRelatedPerson;
  TFhirRelatedPersonList = FHIR.R3.Resources.Admin.TFhirRelatedPersonList;
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_SCHEDULE}
  TFhirSchedule = FHIR.R3.Resources.Admin.TFhirSchedule;
  TFhirScheduleList = FHIR.R3.Resources.Admin.TFhirScheduleList;
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SLOT}
  TFhirSlot = FHIR.R3.Resources.Admin.TFhirSlot;
  TFhirSlotList = FHIR.R3.Resources.Admin.TFhirSlotList;
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SUBSTANCE}
  TFhirSubstanceInstance = FHIR.R3.Resources.Admin.TFhirSubstanceInstance;
  TFhirSubstanceInstanceList = FHIR.R3.Resources.Admin.TFhirSubstanceInstanceList;
  TFhirSubstanceIngredient = FHIR.R3.Resources.Admin.TFhirSubstanceIngredient;
  TFhirSubstanceIngredientList = FHIR.R3.Resources.Admin.TFhirSubstanceIngredientList;
  TFhirSubstance = FHIR.R3.Resources.Admin.TFhirSubstance;
  TFhirSubstanceList = FHIR.R3.Resources.Admin.TFhirSubstanceList;
{$ENDIF FHIR_SUBSTANCE}
{$IFDEF FHIR_ACCOUNT}
  TFhirAccountCoverage = FHIR.R3.Resources.Clinical.TFhirAccountCoverage;
  TFhirAccountCoverageList = FHIR.R3.Resources.Clinical.TFhirAccountCoverageList;
  TFhirAccountGuarantor = FHIR.R3.Resources.Clinical.TFhirAccountGuarantor;
  TFhirAccountGuarantorList = FHIR.R3.Resources.Clinical.TFhirAccountGuarantorList;
  TFhirAccount = FHIR.R3.Resources.Clinical.TFhirAccount;
  TFhirAccountList = FHIR.R3.Resources.Clinical.TFhirAccountList;
{$ENDIF FHIR_ACCOUNT}
{$IFDEF FHIR_ADVERSEEVENT}
  TFhirAdverseEventSuspectEntity = FHIR.R3.Resources.Clinical.TFhirAdverseEventSuspectEntity;
  TFhirAdverseEventSuspectEntityList = FHIR.R3.Resources.Clinical.TFhirAdverseEventSuspectEntityList;
  TFhirAdverseEvent = FHIR.R3.Resources.Clinical.TFhirAdverseEvent;
  TFhirAdverseEventList = FHIR.R3.Resources.Clinical.TFhirAdverseEventList;
{$ENDIF FHIR_ADVERSEEVENT}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  TFhirAllergyIntoleranceReaction = FHIR.R3.Resources.Clinical.TFhirAllergyIntoleranceReaction;
  TFhirAllergyIntoleranceReactionList = FHIR.R3.Resources.Clinical.TFhirAllergyIntoleranceReactionList;
  TFhirAllergyIntolerance = FHIR.R3.Resources.Clinical.TFhirAllergyIntolerance;
  TFhirAllergyIntoleranceList = FHIR.R3.Resources.Clinical.TFhirAllergyIntoleranceList;
{$ENDIF FHIR_ALLERGYINTOLERANCE}
{$IFDEF FHIR_APPOINTMENT}
  TFhirAppointmentParticipant = FHIR.R3.Resources.Clinical.TFhirAppointmentParticipant;
  TFhirAppointmentParticipantList = FHIR.R3.Resources.Clinical.TFhirAppointmentParticipantList;
  TFhirAppointment = FHIR.R3.Resources.Clinical.TFhirAppointment;
  TFhirAppointmentList = FHIR.R3.Resources.Clinical.TFhirAppointmentList;
{$ENDIF FHIR_APPOINTMENT}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  TFhirAppointmentResponse = FHIR.R3.Resources.Clinical.TFhirAppointmentResponse;
  TFhirAppointmentResponseList = FHIR.R3.Resources.Clinical.TFhirAppointmentResponseList;
{$ENDIF FHIR_APPOINTMENTRESPONSE}
{$IFDEF FHIR_BASIC}
  TFhirBasic = FHIR.R3.Resources.Clinical.TFhirBasic;
  TFhirBasicList = FHIR.R3.Resources.Clinical.TFhirBasicList;
{$ENDIF FHIR_BASIC}
{$IFDEF FHIR_BODYSITE}
  TFhirBodySite = FHIR.R3.Resources.Clinical.TFhirBodySite;
  TFhirBodySiteList = FHIR.R3.Resources.Clinical.TFhirBodySiteList;
{$ENDIF FHIR_BODYSITE}
{$IFDEF FHIR_CAREPLAN}
  TFhirCarePlanActivity = FHIR.R3.Resources.Clinical.TFhirCarePlanActivity;
  TFhirCarePlanActivityList = FHIR.R3.Resources.Clinical.TFhirCarePlanActivityList;
  TFhirCarePlanActivityDetail = FHIR.R3.Resources.Clinical.TFhirCarePlanActivityDetail;
  TFhirCarePlanActivityDetailList = FHIR.R3.Resources.Clinical.TFhirCarePlanActivityDetailList;
  TFhirCarePlan = FHIR.R3.Resources.Clinical.TFhirCarePlan;
  TFhirCarePlanList = FHIR.R3.Resources.Clinical.TFhirCarePlanList;
{$ENDIF FHIR_CAREPLAN}
{$IFDEF FHIR_CARETEAM}
  TFhirCareTeamParticipant = FHIR.R3.Resources.Clinical.TFhirCareTeamParticipant;
  TFhirCareTeamParticipantList = FHIR.R3.Resources.Clinical.TFhirCareTeamParticipantList;
  TFhirCareTeam = FHIR.R3.Resources.Clinical.TFhirCareTeam;
  TFhirCareTeamList = FHIR.R3.Resources.Clinical.TFhirCareTeamList;
{$ENDIF FHIR_CARETEAM}
{$IFDEF FHIR_CLINICALIMPRESSION}
  TFhirClinicalImpressionInvestigation = FHIR.R3.Resources.Clinical.TFhirClinicalImpressionInvestigation;
  TFhirClinicalImpressionInvestigationList = FHIR.R3.Resources.Clinical.TFhirClinicalImpressionInvestigationList;
  TFhirClinicalImpressionFinding = FHIR.R3.Resources.Clinical.TFhirClinicalImpressionFinding;
  TFhirClinicalImpressionFindingList = FHIR.R3.Resources.Clinical.TFhirClinicalImpressionFindingList;
  TFhirClinicalImpression = FHIR.R3.Resources.Clinical.TFhirClinicalImpression;
  TFhirClinicalImpressionList = FHIR.R3.Resources.Clinical.TFhirClinicalImpressionList;
{$ENDIF FHIR_CLINICALIMPRESSION}
{$IFDEF FHIR_COMMUNICATION}
  TFhirCommunicationPayload = FHIR.R3.Resources.Clinical.TFhirCommunicationPayload;
  TFhirCommunicationPayloadList = FHIR.R3.Resources.Clinical.TFhirCommunicationPayloadList;
  TFhirCommunication = FHIR.R3.Resources.Clinical.TFhirCommunication;
  TFhirCommunicationList = FHIR.R3.Resources.Clinical.TFhirCommunicationList;
{$ENDIF FHIR_COMMUNICATION}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  TFhirCommunicationRequestPayload = FHIR.R3.Resources.Clinical.TFhirCommunicationRequestPayload;
  TFhirCommunicationRequestPayloadList = FHIR.R3.Resources.Clinical.TFhirCommunicationRequestPayloadList;
  TFhirCommunicationRequestRequester = FHIR.R3.Resources.Clinical.TFhirCommunicationRequestRequester;
  TFhirCommunicationRequestRequesterList = FHIR.R3.Resources.Clinical.TFhirCommunicationRequestRequesterList;
  TFhirCommunicationRequest = FHIR.R3.Resources.Clinical.TFhirCommunicationRequest;
  TFhirCommunicationRequestList = FHIR.R3.Resources.Clinical.TFhirCommunicationRequestList;
{$ENDIF FHIR_COMMUNICATIONREQUEST}
{$IFDEF FHIR_COMPOSITION}
  TFhirCompositionAttester = FHIR.R3.Resources.Clinical.TFhirCompositionAttester;
  TFhirCompositionAttesterList = FHIR.R3.Resources.Clinical.TFhirCompositionAttesterList;
  TFhirCompositionRelatesTo = FHIR.R3.Resources.Clinical.TFhirCompositionRelatesTo;
  TFhirCompositionRelatesToList = FHIR.R3.Resources.Clinical.TFhirCompositionRelatesToList;
  TFhirCompositionEvent = FHIR.R3.Resources.Clinical.TFhirCompositionEvent;
  TFhirCompositionEventList = FHIR.R3.Resources.Clinical.TFhirCompositionEventList;
  TFhirCompositionSection = FHIR.R3.Resources.Clinical.TFhirCompositionSection;
  TFhirCompositionSectionList = FHIR.R3.Resources.Clinical.TFhirCompositionSectionList;
  TFhirComposition = FHIR.R3.Resources.Clinical.TFhirComposition;
  TFhirCompositionList = FHIR.R3.Resources.Clinical.TFhirCompositionList;
{$ENDIF FHIR_COMPOSITION}
{$IFDEF FHIR_CONDITION}
  TFhirConditionStage = FHIR.R3.Resources.Clinical.TFhirConditionStage;
  TFhirConditionStageList = FHIR.R3.Resources.Clinical.TFhirConditionStageList;
  TFhirConditionEvidence = FHIR.R3.Resources.Clinical.TFhirConditionEvidence;
  TFhirConditionEvidenceList = FHIR.R3.Resources.Clinical.TFhirConditionEvidenceList;
  TFhirCondition = FHIR.R3.Resources.Clinical.TFhirCondition;
  TFhirConditionList = FHIR.R3.Resources.Clinical.TFhirConditionList;
{$ENDIF FHIR_CONDITION}
{$IFDEF FHIR_COVERAGE}
  TFhirCoverageGrouping = FHIR.R3.Resources.Clinical.TFhirCoverageGrouping;
  TFhirCoverageGroupingList = FHIR.R3.Resources.Clinical.TFhirCoverageGroupingList;
  TFhirCoverage = FHIR.R3.Resources.Clinical.TFhirCoverage;
  TFhirCoverageList = FHIR.R3.Resources.Clinical.TFhirCoverageList;
{$ENDIF FHIR_COVERAGE}
{$IFDEF FHIR_DETECTEDISSUE}
  TFhirDetectedIssueMitigation = FHIR.R3.Resources.Clinical.TFhirDetectedIssueMitigation;
  TFhirDetectedIssueMitigationList = FHIR.R3.Resources.Clinical.TFhirDetectedIssueMitigationList;
  TFhirDetectedIssue = FHIR.R3.Resources.Clinical.TFhirDetectedIssue;
  TFhirDetectedIssueList = FHIR.R3.Resources.Clinical.TFhirDetectedIssueList;
{$ENDIF FHIR_DETECTEDISSUE}
{$IFDEF FHIR_DEVICEREQUEST}
  TFhirDeviceRequestRequester = FHIR.R3.Resources.Clinical.TFhirDeviceRequestRequester;
  TFhirDeviceRequestRequesterList = FHIR.R3.Resources.Clinical.TFhirDeviceRequestRequesterList;
  TFhirDeviceRequest = FHIR.R3.Resources.Clinical.TFhirDeviceRequest;
  TFhirDeviceRequestList = FHIR.R3.Resources.Clinical.TFhirDeviceRequestList;
{$ENDIF FHIR_DEVICEREQUEST}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  TFhirDeviceUseStatement = FHIR.R3.Resources.Clinical.TFhirDeviceUseStatement;
  TFhirDeviceUseStatementList = FHIR.R3.Resources.Clinical.TFhirDeviceUseStatementList;
{$ENDIF FHIR_DEVICEUSESTATEMENT}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  TFhirDiagnosticReportPerformer = FHIR.R3.Resources.Clinical.TFhirDiagnosticReportPerformer;
  TFhirDiagnosticReportPerformerList = FHIR.R3.Resources.Clinical.TFhirDiagnosticReportPerformerList;
  TFhirDiagnosticReportImage = FHIR.R3.Resources.Clinical.TFhirDiagnosticReportImage;
  TFhirDiagnosticReportImageList = FHIR.R3.Resources.Clinical.TFhirDiagnosticReportImageList;
  TFhirDiagnosticReport = FHIR.R3.Resources.Clinical.TFhirDiagnosticReport;
  TFhirDiagnosticReportList = FHIR.R3.Resources.Clinical.TFhirDiagnosticReportList;
{$ENDIF FHIR_DIAGNOSTICREPORT}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  TFhirDocumentManifestContent = FHIR.R3.Resources.Clinical.TFhirDocumentManifestContent;
  TFhirDocumentManifestContentList = FHIR.R3.Resources.Clinical.TFhirDocumentManifestContentList;
  TFhirDocumentManifestRelated = FHIR.R3.Resources.Clinical.TFhirDocumentManifestRelated;
  TFhirDocumentManifestRelatedList = FHIR.R3.Resources.Clinical.TFhirDocumentManifestRelatedList;
  TFhirDocumentManifest = FHIR.R3.Resources.Clinical.TFhirDocumentManifest;
  TFhirDocumentManifestList = FHIR.R3.Resources.Clinical.TFhirDocumentManifestList;
{$ENDIF FHIR_DOCUMENTMANIFEST}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  TFhirDocumentReferenceRelatesTo = FHIR.R3.Resources.Clinical.TFhirDocumentReferenceRelatesTo;
  TFhirDocumentReferenceRelatesToList = FHIR.R3.Resources.Clinical.TFhirDocumentReferenceRelatesToList;
  TFhirDocumentReferenceContent = FHIR.R3.Resources.Clinical.TFhirDocumentReferenceContent;
  TFhirDocumentReferenceContentList = FHIR.R3.Resources.Clinical.TFhirDocumentReferenceContentList;
  TFhirDocumentReferenceContext = FHIR.R3.Resources.Clinical.TFhirDocumentReferenceContext;
  TFhirDocumentReferenceContextList = FHIR.R3.Resources.Clinical.TFhirDocumentReferenceContextList;
  TFhirDocumentReferenceContextRelated = FHIR.R3.Resources.Clinical.TFhirDocumentReferenceContextRelated;
  TFhirDocumentReferenceContextRelatedList = FHIR.R3.Resources.Clinical.TFhirDocumentReferenceContextRelatedList;
  TFhirDocumentReference = FHIR.R3.Resources.Clinical.TFhirDocumentReference;
  TFhirDocumentReferenceList = FHIR.R3.Resources.Clinical.TFhirDocumentReferenceList;
{$ENDIF FHIR_DOCUMENTREFERENCE}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  TFhirFamilyMemberHistoryCondition = FHIR.R3.Resources.Clinical.TFhirFamilyMemberHistoryCondition;
  TFhirFamilyMemberHistoryConditionList = FHIR.R3.Resources.Clinical.TFhirFamilyMemberHistoryConditionList;
  TFhirFamilyMemberHistory = FHIR.R3.Resources.Clinical.TFhirFamilyMemberHistory;
  TFhirFamilyMemberHistoryList = FHIR.R3.Resources.Clinical.TFhirFamilyMemberHistoryList;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}
{$IFDEF FHIR_FLAG}
  TFhirFlag = FHIR.R3.Resources.Clinical.TFhirFlag;
  TFhirFlagList = FHIR.R3.Resources.Clinical.TFhirFlagList;
{$ENDIF FHIR_FLAG}
{$IFDEF FHIR_GOAL}
  TFhirGoalTarget = FHIR.R3.Resources.Clinical.TFhirGoalTarget;
  TFhirGoalTargetList = FHIR.R3.Resources.Clinical.TFhirGoalTargetList;
  TFhirGoal = FHIR.R3.Resources.Clinical.TFhirGoal;
  TFhirGoalList = FHIR.R3.Resources.Clinical.TFhirGoalList;
{$ENDIF FHIR_GOAL}
{$IFDEF FHIR_IMAGINGMANIFEST}
  TFhirImagingManifestStudy = FHIR.R3.Resources.Clinical.TFhirImagingManifestStudy;
  TFhirImagingManifestStudyList = FHIR.R3.Resources.Clinical.TFhirImagingManifestStudyList;
  TFhirImagingManifestStudySeries = FHIR.R3.Resources.Clinical.TFhirImagingManifestStudySeries;
  TFhirImagingManifestStudySeriesList = FHIR.R3.Resources.Clinical.TFhirImagingManifestStudySeriesList;
  TFhirImagingManifestStudySeriesInstance = FHIR.R3.Resources.Clinical.TFhirImagingManifestStudySeriesInstance;
  TFhirImagingManifestStudySeriesInstanceList = FHIR.R3.Resources.Clinical.TFhirImagingManifestStudySeriesInstanceList;
  TFhirImagingManifest = FHIR.R3.Resources.Clinical.TFhirImagingManifest;
  TFhirImagingManifestList = FHIR.R3.Resources.Clinical.TFhirImagingManifestList;
{$ENDIF FHIR_IMAGINGMANIFEST}
{$IFDEF FHIR_IMAGINGSTUDY}
  TFhirImagingStudySeries = FHIR.R3.Resources.Clinical.TFhirImagingStudySeries;
  TFhirImagingStudySeriesList = FHIR.R3.Resources.Clinical.TFhirImagingStudySeriesList;
  TFhirImagingStudySeriesInstance = FHIR.R3.Resources.Clinical.TFhirImagingStudySeriesInstance;
  TFhirImagingStudySeriesInstanceList = FHIR.R3.Resources.Clinical.TFhirImagingStudySeriesInstanceList;
  TFhirImagingStudy = FHIR.R3.Resources.Clinical.TFhirImagingStudy;
  TFhirImagingStudyList = FHIR.R3.Resources.Clinical.TFhirImagingStudyList;
{$ENDIF FHIR_IMAGINGSTUDY}
{$IFDEF FHIR_IMMUNIZATION}
  TFhirImmunizationPractitioner = FHIR.R3.Resources.Clinical.TFhirImmunizationPractitioner;
  TFhirImmunizationPractitionerList = FHIR.R3.Resources.Clinical.TFhirImmunizationPractitionerList;
  TFhirImmunizationExplanation = FHIR.R3.Resources.Clinical.TFhirImmunizationExplanation;
  TFhirImmunizationExplanationList = FHIR.R3.Resources.Clinical.TFhirImmunizationExplanationList;
  TFhirImmunizationReaction = FHIR.R3.Resources.Clinical.TFhirImmunizationReaction;
  TFhirImmunizationReactionList = FHIR.R3.Resources.Clinical.TFhirImmunizationReactionList;
  TFhirImmunizationVaccinationProtocol = FHIR.R3.Resources.Clinical.TFhirImmunizationVaccinationProtocol;
  TFhirImmunizationVaccinationProtocolList = FHIR.R3.Resources.Clinical.TFhirImmunizationVaccinationProtocolList;
  TFhirImmunization = FHIR.R3.Resources.Clinical.TFhirImmunization;
  TFhirImmunizationList = FHIR.R3.Resources.Clinical.TFhirImmunizationList;
{$ENDIF FHIR_IMMUNIZATION}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  TFhirImmunizationRecommendationRecommendation = FHIR.R3.Resources.Clinical.TFhirImmunizationRecommendationRecommendation;
  TFhirImmunizationRecommendationRecommendationList = FHIR.R3.Resources.Clinical.TFhirImmunizationRecommendationRecommendationList;
  TFhirImmunizationRecommendationRecommendationDateCriterion = FHIR.R3.Resources.Clinical.TFhirImmunizationRecommendationRecommendationDateCriterion;
  TFhirImmunizationRecommendationRecommendationDateCriterionList = FHIR.R3.Resources.Clinical.TFhirImmunizationRecommendationRecommendationDateCriterionList;
  TFhirImmunizationRecommendationRecommendationProtocol = FHIR.R3.Resources.Clinical.TFhirImmunizationRecommendationRecommendationProtocol;
  TFhirImmunizationRecommendationRecommendationProtocolList = FHIR.R3.Resources.Clinical.TFhirImmunizationRecommendationRecommendationProtocolList;
  TFhirImmunizationRecommendation = FHIR.R3.Resources.Clinical.TFhirImmunizationRecommendation;
  TFhirImmunizationRecommendationList = FHIR.R3.Resources.Clinical.TFhirImmunizationRecommendationList;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}
{$IFDEF FHIR_MEDIA}
  TFhirMedia = FHIR.R3.Resources.Clinical.TFhirMedia;
  TFhirMediaList = FHIR.R3.Resources.Clinical.TFhirMediaList;
{$ENDIF FHIR_MEDIA}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  TFhirMedicationAdministrationPerformer = FHIR.R3.Resources.Clinical.TFhirMedicationAdministrationPerformer;
  TFhirMedicationAdministrationPerformerList = FHIR.R3.Resources.Clinical.TFhirMedicationAdministrationPerformerList;
  TFhirMedicationAdministrationDosage = FHIR.R3.Resources.Clinical.TFhirMedicationAdministrationDosage;
  TFhirMedicationAdministrationDosageList = FHIR.R3.Resources.Clinical.TFhirMedicationAdministrationDosageList;
  TFhirMedicationAdministration = FHIR.R3.Resources.Clinical.TFhirMedicationAdministration;
  TFhirMedicationAdministrationList = FHIR.R3.Resources.Clinical.TFhirMedicationAdministrationList;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  TFhirMedicationDispensePerformer = FHIR.R3.Resources.Clinical.TFhirMedicationDispensePerformer;
  TFhirMedicationDispensePerformerList = FHIR.R3.Resources.Clinical.TFhirMedicationDispensePerformerList;
  TFhirMedicationDispenseSubstitution = FHIR.R3.Resources.Clinical.TFhirMedicationDispenseSubstitution;
  TFhirMedicationDispenseSubstitutionList = FHIR.R3.Resources.Clinical.TFhirMedicationDispenseSubstitutionList;
  TFhirMedicationDispense = FHIR.R3.Resources.Clinical.TFhirMedicationDispense;
  TFhirMedicationDispenseList = FHIR.R3.Resources.Clinical.TFhirMedicationDispenseList;
{$ENDIF FHIR_MEDICATIONDISPENSE}
{$IFDEF FHIR_MEDICATIONREQUEST}
  TFhirMedicationRequestRequester = FHIR.R3.Resources.Clinical.TFhirMedicationRequestRequester;
  TFhirMedicationRequestRequesterList = FHIR.R3.Resources.Clinical.TFhirMedicationRequestRequesterList;
  TFhirMedicationRequestDispenseRequest = FHIR.R3.Resources.Clinical.TFhirMedicationRequestDispenseRequest;
  TFhirMedicationRequestDispenseRequestList = FHIR.R3.Resources.Clinical.TFhirMedicationRequestDispenseRequestList;
  TFhirMedicationRequestSubstitution = FHIR.R3.Resources.Clinical.TFhirMedicationRequestSubstitution;
  TFhirMedicationRequestSubstitutionList = FHIR.R3.Resources.Clinical.TFhirMedicationRequestSubstitutionList;
  TFhirMedicationRequest = FHIR.R3.Resources.Clinical.TFhirMedicationRequest;
  TFhirMedicationRequestList = FHIR.R3.Resources.Clinical.TFhirMedicationRequestList;
{$ENDIF FHIR_MEDICATIONREQUEST}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  TFhirMedicationStatement = FHIR.R3.Resources.Clinical.TFhirMedicationStatement;
  TFhirMedicationStatementList = FHIR.R3.Resources.Clinical.TFhirMedicationStatementList;
{$ENDIF FHIR_MEDICATIONSTATEMENT}
{$IFDEF FHIR_NUTRITIONORDER}
  TFhirNutritionOrderOralDiet = FHIR.R3.Resources.Clinical.TFhirNutritionOrderOralDiet;
  TFhirNutritionOrderOralDietList = FHIR.R3.Resources.Clinical.TFhirNutritionOrderOralDietList;
  TFhirNutritionOrderOralDietNutrient = FHIR.R3.Resources.Clinical.TFhirNutritionOrderOralDietNutrient;
  TFhirNutritionOrderOralDietNutrientList = FHIR.R3.Resources.Clinical.TFhirNutritionOrderOralDietNutrientList;
  TFhirNutritionOrderOralDietTexture = FHIR.R3.Resources.Clinical.TFhirNutritionOrderOralDietTexture;
  TFhirNutritionOrderOralDietTextureList = FHIR.R3.Resources.Clinical.TFhirNutritionOrderOralDietTextureList;
  TFhirNutritionOrderSupplement = FHIR.R3.Resources.Clinical.TFhirNutritionOrderSupplement;
  TFhirNutritionOrderSupplementList = FHIR.R3.Resources.Clinical.TFhirNutritionOrderSupplementList;
  TFhirNutritionOrderEnteralFormula = FHIR.R3.Resources.Clinical.TFhirNutritionOrderEnteralFormula;
  TFhirNutritionOrderEnteralFormulaList = FHIR.R3.Resources.Clinical.TFhirNutritionOrderEnteralFormulaList;
  TFhirNutritionOrderEnteralFormulaAdministration = FHIR.R3.Resources.Clinical.TFhirNutritionOrderEnteralFormulaAdministration;
  TFhirNutritionOrderEnteralFormulaAdministrationList = FHIR.R3.Resources.Clinical.TFhirNutritionOrderEnteralFormulaAdministrationList;
  TFhirNutritionOrder = FHIR.R3.Resources.Clinical.TFhirNutritionOrder;
  TFhirNutritionOrderList = FHIR.R3.Resources.Clinical.TFhirNutritionOrderList;
{$ENDIF FHIR_NUTRITIONORDER}
{$IFDEF FHIR_OBSERVATION}
  TFhirObservationReferenceRange = FHIR.R3.Resources.Clinical.TFhirObservationReferenceRange;
  TFhirObservationReferenceRangeList = FHIR.R3.Resources.Clinical.TFhirObservationReferenceRangeList;
  TFhirObservationRelated = FHIR.R3.Resources.Clinical.TFhirObservationRelated;
  TFhirObservationRelatedList = FHIR.R3.Resources.Clinical.TFhirObservationRelatedList;
  TFhirObservationComponent = FHIR.R3.Resources.Clinical.TFhirObservationComponent;
  TFhirObservationComponentList = FHIR.R3.Resources.Clinical.TFhirObservationComponentList;
  TFhirObservation = FHIR.R3.Resources.Clinical.TFhirObservation;
  TFhirObservationList = FHIR.R3.Resources.Clinical.TFhirObservationList;
{$ENDIF FHIR_OBSERVATION}
{$IFDEF FHIR_PROCEDURE}
  TFhirProcedurePerformer = FHIR.R3.Resources.Clinical.TFhirProcedurePerformer;
  TFhirProcedurePerformerList = FHIR.R3.Resources.Clinical.TFhirProcedurePerformerList;
  TFhirProcedureFocalDevice = FHIR.R3.Resources.Clinical.TFhirProcedureFocalDevice;
  TFhirProcedureFocalDeviceList = FHIR.R3.Resources.Clinical.TFhirProcedureFocalDeviceList;
  TFhirProcedure = FHIR.R3.Resources.Clinical.TFhirProcedure;
  TFhirProcedureList = FHIR.R3.Resources.Clinical.TFhirProcedureList;
{$ENDIF FHIR_PROCEDURE}
{$IFDEF FHIR_PROCEDUREREQUEST}
  TFhirProcedureRequestRequester = FHIR.R3.Resources.Clinical.TFhirProcedureRequestRequester;
  TFhirProcedureRequestRequesterList = FHIR.R3.Resources.Clinical.TFhirProcedureRequestRequesterList;
  TFhirProcedureRequest = FHIR.R3.Resources.Clinical.TFhirProcedureRequest;
  TFhirProcedureRequestList = FHIR.R3.Resources.Clinical.TFhirProcedureRequestList;
{$ENDIF FHIR_PROCEDUREREQUEST}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  TFhirQuestionnaireResponseItem = FHIR.R3.Resources.Clinical.TFhirQuestionnaireResponseItem;
  TFhirQuestionnaireResponseItemList = FHIR.R3.Resources.Clinical.TFhirQuestionnaireResponseItemList;
  TFhirQuestionnaireResponseItemAnswer = FHIR.R3.Resources.Clinical.TFhirQuestionnaireResponseItemAnswer;
  TFhirQuestionnaireResponseItemAnswerList = FHIR.R3.Resources.Clinical.TFhirQuestionnaireResponseItemAnswerList;
  TFhirQuestionnaireResponse = FHIR.R3.Resources.Clinical.TFhirQuestionnaireResponse;
  TFhirQuestionnaireResponseList = FHIR.R3.Resources.Clinical.TFhirQuestionnaireResponseList;
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}
{$IFDEF FHIR_REFERRALREQUEST}
  TFhirReferralRequestRequester = FHIR.R3.Resources.Clinical.TFhirReferralRequestRequester;
  TFhirReferralRequestRequesterList = FHIR.R3.Resources.Clinical.TFhirReferralRequestRequesterList;
  TFhirReferralRequest = FHIR.R3.Resources.Clinical.TFhirReferralRequest;
  TFhirReferralRequestList = FHIR.R3.Resources.Clinical.TFhirReferralRequestList;
{$ENDIF FHIR_REFERRALREQUEST}
{$IFDEF FHIR_RISKASSESSMENT}
  TFhirRiskAssessmentPrediction = FHIR.R3.Resources.Clinical.TFhirRiskAssessmentPrediction;
  TFhirRiskAssessmentPredictionList = FHIR.R3.Resources.Clinical.TFhirRiskAssessmentPredictionList;
  TFhirRiskAssessment = FHIR.R3.Resources.Clinical.TFhirRiskAssessment;
  TFhirRiskAssessmentList = FHIR.R3.Resources.Clinical.TFhirRiskAssessmentList;
{$ENDIF FHIR_RISKASSESSMENT}
{$IFDEF FHIR_SEQUENCE}
  TFhirSequenceReferenceSeq = FHIR.R3.Resources.Clinical.TFhirSequenceReferenceSeq;
  TFhirSequenceReferenceSeqList = FHIR.R3.Resources.Clinical.TFhirSequenceReferenceSeqList;
  TFhirSequenceVariant = FHIR.R3.Resources.Clinical.TFhirSequenceVariant;
  TFhirSequenceVariantList = FHIR.R3.Resources.Clinical.TFhirSequenceVariantList;
  TFhirSequenceQuality = FHIR.R3.Resources.Clinical.TFhirSequenceQuality;
  TFhirSequenceQualityList = FHIR.R3.Resources.Clinical.TFhirSequenceQualityList;
  TFhirSequenceRepository = FHIR.R3.Resources.Clinical.TFhirSequenceRepository;
  TFhirSequenceRepositoryList = FHIR.R3.Resources.Clinical.TFhirSequenceRepositoryList;
  TFhirSequence = FHIR.R3.Resources.Clinical.TFhirSequence;
  TFhirSequenceList = FHIR.R3.Resources.Clinical.TFhirSequenceList;
{$ENDIF FHIR_SEQUENCE}
{$IFDEF FHIR_SPECIMEN}
  TFhirSpecimenCollection = FHIR.R3.Resources.Clinical.TFhirSpecimenCollection;
  TFhirSpecimenCollectionList = FHIR.R3.Resources.Clinical.TFhirSpecimenCollectionList;
  TFhirSpecimenProcessing = FHIR.R3.Resources.Clinical.TFhirSpecimenProcessing;
  TFhirSpecimenProcessingList = FHIR.R3.Resources.Clinical.TFhirSpecimenProcessingList;
  TFhirSpecimenContainer = FHIR.R3.Resources.Clinical.TFhirSpecimenContainer;
  TFhirSpecimenContainerList = FHIR.R3.Resources.Clinical.TFhirSpecimenContainerList;
  TFhirSpecimen = FHIR.R3.Resources.Clinical.TFhirSpecimen;
  TFhirSpecimenList = FHIR.R3.Resources.Clinical.TFhirSpecimenList;
{$ENDIF FHIR_SPECIMEN}
{$IFDEF FHIR_SUPPLYDELIVERY}
  TFhirSupplyDeliverySuppliedItem = FHIR.R3.Resources.Clinical.TFhirSupplyDeliverySuppliedItem;
  TFhirSupplyDeliverySuppliedItemList = FHIR.R3.Resources.Clinical.TFhirSupplyDeliverySuppliedItemList;
  TFhirSupplyDelivery = FHIR.R3.Resources.Clinical.TFhirSupplyDelivery;
  TFhirSupplyDeliveryList = FHIR.R3.Resources.Clinical.TFhirSupplyDeliveryList;
{$ENDIF FHIR_SUPPLYDELIVERY}
{$IFDEF FHIR_SUPPLYREQUEST}
  TFhirSupplyRequestOrderedItem = FHIR.R3.Resources.Clinical.TFhirSupplyRequestOrderedItem;
  TFhirSupplyRequestOrderedItemList = FHIR.R3.Resources.Clinical.TFhirSupplyRequestOrderedItemList;
  TFhirSupplyRequestRequester = FHIR.R3.Resources.Clinical.TFhirSupplyRequestRequester;
  TFhirSupplyRequestRequesterList = FHIR.R3.Resources.Clinical.TFhirSupplyRequestRequesterList;
  TFhirSupplyRequest = FHIR.R3.Resources.Clinical.TFhirSupplyRequest;
  TFhirSupplyRequestList = FHIR.R3.Resources.Clinical.TFhirSupplyRequestList;
{$ENDIF FHIR_SUPPLYREQUEST}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  TFhirVisionPrescriptionDispense = FHIR.R3.Resources.Clinical.TFhirVisionPrescriptionDispense;
  TFhirVisionPrescriptionDispenseList = FHIR.R3.Resources.Clinical.TFhirVisionPrescriptionDispenseList;
  TFhirVisionPrescription = FHIR.R3.Resources.Clinical.TFhirVisionPrescription;
  TFhirVisionPrescriptionList = FHIR.R3.Resources.Clinical.TFhirVisionPrescriptionList;
{$ENDIF FHIR_VISIONPRESCRIPTION}
{$IFDEF FHIR_PARAMETERS}
  TFhirParametersParameter = FHIR.R3.Resources.Other.TFhirParametersParameter;
  TFhirParametersParameterList = FHIR.R3.Resources.Other.TFhirParametersParameterList;
  TFhirParameters = FHIR.R3.Resources.Other.TFhirParameters;
  TFhirParametersList = FHIR.R3.Resources.Other.TFhirParametersList;
{$ENDIF FHIR_PARAMETERS}
{$IFDEF FHIR_AUDITEVENT}
  TFhirAuditEventAgent = FHIR.R3.Resources.Other.TFhirAuditEventAgent;
  TFhirAuditEventAgentList = FHIR.R3.Resources.Other.TFhirAuditEventAgentList;
  TFhirAuditEventAgentNetwork = FHIR.R3.Resources.Other.TFhirAuditEventAgentNetwork;
  TFhirAuditEventAgentNetworkList = FHIR.R3.Resources.Other.TFhirAuditEventAgentNetworkList;
  TFhirAuditEventSource = FHIR.R3.Resources.Other.TFhirAuditEventSource;
  TFhirAuditEventSourceList = FHIR.R3.Resources.Other.TFhirAuditEventSourceList;
  TFhirAuditEventEntity = FHIR.R3.Resources.Other.TFhirAuditEventEntity;
  TFhirAuditEventEntityList = FHIR.R3.Resources.Other.TFhirAuditEventEntityList;
  TFhirAuditEventEntityDetail = FHIR.R3.Resources.Other.TFhirAuditEventEntityDetail;
  TFhirAuditEventEntityDetailList = FHIR.R3.Resources.Other.TFhirAuditEventEntityDetailList;
  TFhirAuditEvent = FHIR.R3.Resources.Other.TFhirAuditEvent;
  TFhirAuditEventList = FHIR.R3.Resources.Other.TFhirAuditEventList;
{$ENDIF FHIR_AUDITEVENT}
{$IFDEF FHIR_BINARY}
  TFhirBinary = FHIR.R3.Resources.Other.TFhirBinary;
  TFhirBinaryList = FHIR.R3.Resources.Other.TFhirBinaryList;
{$ENDIF FHIR_BINARY}
{$IFDEF FHIR_BUNDLE}
  TFhirBundleLink = FHIR.R3.Resources.Other.TFhirBundleLink;
  TFhirBundleLinkList = FHIR.R3.Resources.Other.TFhirBundleLinkList;
  TFhirBundleEntry = FHIR.R3.Resources.Other.TFhirBundleEntry;
  TFhirBundleEntryList = FHIR.R3.Resources.Other.TFhirBundleEntryList;
  TFhirBundleEntrySearch = FHIR.R3.Resources.Other.TFhirBundleEntrySearch;
  TFhirBundleEntrySearchList = FHIR.R3.Resources.Other.TFhirBundleEntrySearchList;
  TFhirBundleEntryRequest = FHIR.R3.Resources.Other.TFhirBundleEntryRequest;
  TFhirBundleEntryRequestList = FHIR.R3.Resources.Other.TFhirBundleEntryRequestList;
  TFhirBundleEntryResponse = FHIR.R3.Resources.Other.TFhirBundleEntryResponse;
  TFhirBundleEntryResponseList = FHIR.R3.Resources.Other.TFhirBundleEntryResponseList;
  TFhirBundle = FHIR.R3.Resources.Other.TFhirBundle;
  TFhirBundleList = FHIR.R3.Resources.Other.TFhirBundleList;
{$ENDIF FHIR_BUNDLE}
{$IFDEF FHIR_CHARGEITEM}
  TFhirChargeItemParticipant = FHIR.R3.Resources.Other.TFhirChargeItemParticipant;
  TFhirChargeItemParticipantList = FHIR.R3.Resources.Other.TFhirChargeItemParticipantList;
  TFhirChargeItem = FHIR.R3.Resources.Other.TFhirChargeItem;
  TFhirChargeItemList = FHIR.R3.Resources.Other.TFhirChargeItemList;
{$ENDIF FHIR_CHARGEITEM}
{$IFDEF FHIR_CLAIM}
  TFhirClaimRelated = FHIR.R3.Resources.Other.TFhirClaimRelated;
  TFhirClaimRelatedList = FHIR.R3.Resources.Other.TFhirClaimRelatedList;
  TFhirClaimPayee = FHIR.R3.Resources.Other.TFhirClaimPayee;
  TFhirClaimPayeeList = FHIR.R3.Resources.Other.TFhirClaimPayeeList;
  TFhirClaimCareTeam = FHIR.R3.Resources.Other.TFhirClaimCareTeam;
  TFhirClaimCareTeamList = FHIR.R3.Resources.Other.TFhirClaimCareTeamList;
  TFhirClaimInformation = FHIR.R3.Resources.Other.TFhirClaimInformation;
  TFhirClaimInformationList = FHIR.R3.Resources.Other.TFhirClaimInformationList;
  TFhirClaimDiagnosis = FHIR.R3.Resources.Other.TFhirClaimDiagnosis;
  TFhirClaimDiagnosisList = FHIR.R3.Resources.Other.TFhirClaimDiagnosisList;
  TFhirClaimProcedure = FHIR.R3.Resources.Other.TFhirClaimProcedure;
  TFhirClaimProcedureList = FHIR.R3.Resources.Other.TFhirClaimProcedureList;
  TFhirClaimInsurance = FHIR.R3.Resources.Other.TFhirClaimInsurance;
  TFhirClaimInsuranceList = FHIR.R3.Resources.Other.TFhirClaimInsuranceList;
  TFhirClaimAccident = FHIR.R3.Resources.Other.TFhirClaimAccident;
  TFhirClaimAccidentList = FHIR.R3.Resources.Other.TFhirClaimAccidentList;
  TFhirClaimItem = FHIR.R3.Resources.Other.TFhirClaimItem;
  TFhirClaimItemList = FHIR.R3.Resources.Other.TFhirClaimItemList;
  TFhirClaimItemDetail = FHIR.R3.Resources.Other.TFhirClaimItemDetail;
  TFhirClaimItemDetailList = FHIR.R3.Resources.Other.TFhirClaimItemDetailList;
  TFhirClaimItemDetailSubDetail = FHIR.R3.Resources.Other.TFhirClaimItemDetailSubDetail;
  TFhirClaimItemDetailSubDetailList = FHIR.R3.Resources.Other.TFhirClaimItemDetailSubDetailList;
  TFhirClaim = FHIR.R3.Resources.Other.TFhirClaim;
  TFhirClaimList = FHIR.R3.Resources.Other.TFhirClaimList;
{$ENDIF FHIR_CLAIM}
{$IFDEF FHIR_CLAIMRESPONSE}
  TFhirClaimResponseItem = FHIR.R3.Resources.Other.TFhirClaimResponseItem;
  TFhirClaimResponseItemList = FHIR.R3.Resources.Other.TFhirClaimResponseItemList;
  TFhirClaimResponseItemAdjudication = FHIR.R3.Resources.Other.TFhirClaimResponseItemAdjudication;
  TFhirClaimResponseItemAdjudicationList = FHIR.R3.Resources.Other.TFhirClaimResponseItemAdjudicationList;
  TFhirClaimResponseItemDetail = FHIR.R3.Resources.Other.TFhirClaimResponseItemDetail;
  TFhirClaimResponseItemDetailList = FHIR.R3.Resources.Other.TFhirClaimResponseItemDetailList;
  TFhirClaimResponseItemDetailSubDetail = FHIR.R3.Resources.Other.TFhirClaimResponseItemDetailSubDetail;
  TFhirClaimResponseItemDetailSubDetailList = FHIR.R3.Resources.Other.TFhirClaimResponseItemDetailSubDetailList;
  TFhirClaimResponseAddItem = FHIR.R3.Resources.Other.TFhirClaimResponseAddItem;
  TFhirClaimResponseAddItemList = FHIR.R3.Resources.Other.TFhirClaimResponseAddItemList;
  TFhirClaimResponseAddItemDetail = FHIR.R3.Resources.Other.TFhirClaimResponseAddItemDetail;
  TFhirClaimResponseAddItemDetailList = FHIR.R3.Resources.Other.TFhirClaimResponseAddItemDetailList;
  TFhirClaimResponseError = FHIR.R3.Resources.Other.TFhirClaimResponseError;
  TFhirClaimResponseErrorList = FHIR.R3.Resources.Other.TFhirClaimResponseErrorList;
  TFhirClaimResponsePayment = FHIR.R3.Resources.Other.TFhirClaimResponsePayment;
  TFhirClaimResponsePaymentList = FHIR.R3.Resources.Other.TFhirClaimResponsePaymentList;
  TFhirClaimResponseProcessNote = FHIR.R3.Resources.Other.TFhirClaimResponseProcessNote;
  TFhirClaimResponseProcessNoteList = FHIR.R3.Resources.Other.TFhirClaimResponseProcessNoteList;
  TFhirClaimResponseInsurance = FHIR.R3.Resources.Other.TFhirClaimResponseInsurance;
  TFhirClaimResponseInsuranceList = FHIR.R3.Resources.Other.TFhirClaimResponseInsuranceList;
  TFhirClaimResponse = FHIR.R3.Resources.Other.TFhirClaimResponse;
  TFhirClaimResponseList = FHIR.R3.Resources.Other.TFhirClaimResponseList;
{$ENDIF FHIR_CLAIMRESPONSE}
{$IFDEF FHIR_CONSENT}
  TFhirConsentActor = FHIR.R3.Resources.Other.TFhirConsentActor;
  TFhirConsentActorList = FHIR.R3.Resources.Other.TFhirConsentActorList;
  TFhirConsentPolicy = FHIR.R3.Resources.Other.TFhirConsentPolicy;
  TFhirConsentPolicyList = FHIR.R3.Resources.Other.TFhirConsentPolicyList;
  TFhirConsentData = FHIR.R3.Resources.Other.TFhirConsentData;
  TFhirConsentDataList = FHIR.R3.Resources.Other.TFhirConsentDataList;
  TFhirConsentExcept = FHIR.R3.Resources.Other.TFhirConsentExcept;
  TFhirConsentExceptList = FHIR.R3.Resources.Other.TFhirConsentExceptList;
  TFhirConsentExceptActor = FHIR.R3.Resources.Other.TFhirConsentExceptActor;
  TFhirConsentExceptActorList = FHIR.R3.Resources.Other.TFhirConsentExceptActorList;
  TFhirConsentExceptData = FHIR.R3.Resources.Other.TFhirConsentExceptData;
  TFhirConsentExceptDataList = FHIR.R3.Resources.Other.TFhirConsentExceptDataList;
  TFhirConsent = FHIR.R3.Resources.Other.TFhirConsent;
  TFhirConsentList = FHIR.R3.Resources.Other.TFhirConsentList;
{$ENDIF FHIR_CONSENT}
{$IFDEF FHIR_CONTRACT}
  TFhirContractAgent = FHIR.R3.Resources.Other.TFhirContractAgent;
  TFhirContractAgentList = FHIR.R3.Resources.Other.TFhirContractAgentList;
  TFhirContractSigner = FHIR.R3.Resources.Other.TFhirContractSigner;
  TFhirContractSignerList = FHIR.R3.Resources.Other.TFhirContractSignerList;
  TFhirContractValuedItem = FHIR.R3.Resources.Other.TFhirContractValuedItem;
  TFhirContractValuedItemList = FHIR.R3.Resources.Other.TFhirContractValuedItemList;
  TFhirContractTerm = FHIR.R3.Resources.Other.TFhirContractTerm;
  TFhirContractTermList = FHIR.R3.Resources.Other.TFhirContractTermList;
  TFhirContractTermAgent = FHIR.R3.Resources.Other.TFhirContractTermAgent;
  TFhirContractTermAgentList = FHIR.R3.Resources.Other.TFhirContractTermAgentList;
  TFhirContractTermValuedItem = FHIR.R3.Resources.Other.TFhirContractTermValuedItem;
  TFhirContractTermValuedItemList = FHIR.R3.Resources.Other.TFhirContractTermValuedItemList;
  TFhirContractFriendly = FHIR.R3.Resources.Other.TFhirContractFriendly;
  TFhirContractFriendlyList = FHIR.R3.Resources.Other.TFhirContractFriendlyList;
  TFhirContractLegal = FHIR.R3.Resources.Other.TFhirContractLegal;
  TFhirContractLegalList = FHIR.R3.Resources.Other.TFhirContractLegalList;
  TFhirContractRule = FHIR.R3.Resources.Other.TFhirContractRule;
  TFhirContractRuleList = FHIR.R3.Resources.Other.TFhirContractRuleList;
  TFhirContract = FHIR.R3.Resources.Other.TFhirContract;
  TFhirContractList = FHIR.R3.Resources.Other.TFhirContractList;
{$ENDIF FHIR_CONTRACT}
{$IFDEF FHIR_ELIGIBILITYREQUEST}
  TFhirEligibilityRequest = FHIR.R3.Resources.Other.TFhirEligibilityRequest;
  TFhirEligibilityRequestList = FHIR.R3.Resources.Other.TFhirEligibilityRequestList;
{$ENDIF FHIR_ELIGIBILITYREQUEST}
{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  TFhirEligibilityResponseInsurance = FHIR.R3.Resources.Other.TFhirEligibilityResponseInsurance;
  TFhirEligibilityResponseInsuranceList = FHIR.R3.Resources.Other.TFhirEligibilityResponseInsuranceList;
  TFhirEligibilityResponseInsuranceBenefitBalance = FHIR.R3.Resources.Other.TFhirEligibilityResponseInsuranceBenefitBalance;
  TFhirEligibilityResponseInsuranceBenefitBalanceList = FHIR.R3.Resources.Other.TFhirEligibilityResponseInsuranceBenefitBalanceList;
  TFhirEligibilityResponseInsuranceBenefitBalanceFinancial = FHIR.R3.Resources.Other.TFhirEligibilityResponseInsuranceBenefitBalanceFinancial;
  TFhirEligibilityResponseInsuranceBenefitBalanceFinancialList = FHIR.R3.Resources.Other.TFhirEligibilityResponseInsuranceBenefitBalanceFinancialList;
  TFhirEligibilityResponseError = FHIR.R3.Resources.Other.TFhirEligibilityResponseError;
  TFhirEligibilityResponseErrorList = FHIR.R3.Resources.Other.TFhirEligibilityResponseErrorList;
  TFhirEligibilityResponse = FHIR.R3.Resources.Other.TFhirEligibilityResponse;
  TFhirEligibilityResponseList = FHIR.R3.Resources.Other.TFhirEligibilityResponseList;
{$ENDIF FHIR_ELIGIBILITYRESPONSE}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  TFhirEnrollmentRequest = FHIR.R3.Resources.Other.TFhirEnrollmentRequest;
  TFhirEnrollmentRequestList = FHIR.R3.Resources.Other.TFhirEnrollmentRequestList;
{$ENDIF FHIR_ENROLLMENTREQUEST}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  TFhirEnrollmentResponse = FHIR.R3.Resources.Other.TFhirEnrollmentResponse;
  TFhirEnrollmentResponseList = FHIR.R3.Resources.Other.TFhirEnrollmentResponseList;
{$ENDIF FHIR_ENROLLMENTRESPONSE}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  TFhirExplanationOfBenefitRelated = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitRelated;
  TFhirExplanationOfBenefitRelatedList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitRelatedList;
  TFhirExplanationOfBenefitPayee = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitPayee;
  TFhirExplanationOfBenefitPayeeList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitPayeeList;
  TFhirExplanationOfBenefitInformation = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitInformation;
  TFhirExplanationOfBenefitInformationList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitInformationList;
  TFhirExplanationOfBenefitCareTeam = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitCareTeam;
  TFhirExplanationOfBenefitCareTeamList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitCareTeamList;
  TFhirExplanationOfBenefitDiagnosis = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitDiagnosis;
  TFhirExplanationOfBenefitDiagnosisList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitDiagnosisList;
  TFhirExplanationOfBenefitProcedure = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitProcedure;
  TFhirExplanationOfBenefitProcedureList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitProcedureList;
  TFhirExplanationOfBenefitInsurance = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitInsurance;
  TFhirExplanationOfBenefitInsuranceList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitInsuranceList;
  TFhirExplanationOfBenefitAccident = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitAccident;
  TFhirExplanationOfBenefitAccidentList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitAccidentList;
  TFhirExplanationOfBenefitItem = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitItem;
  TFhirExplanationOfBenefitItemList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitItemList;
  TFhirExplanationOfBenefitItemAdjudication = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitItemAdjudication;
  TFhirExplanationOfBenefitItemAdjudicationList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitItemAdjudicationList;
  TFhirExplanationOfBenefitItemDetail = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitItemDetail;
  TFhirExplanationOfBenefitItemDetailList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitItemDetailList;
  TFhirExplanationOfBenefitItemDetailSubDetail = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitItemDetailSubDetail;
  TFhirExplanationOfBenefitItemDetailSubDetailList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitItemDetailSubDetailList;
  TFhirExplanationOfBenefitAddItem = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitAddItem;
  TFhirExplanationOfBenefitAddItemList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitAddItemList;
  TFhirExplanationOfBenefitAddItemDetail = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitAddItemDetail;
  TFhirExplanationOfBenefitAddItemDetailList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitAddItemDetailList;
  TFhirExplanationOfBenefitPayment = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitPayment;
  TFhirExplanationOfBenefitPaymentList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitPaymentList;
  TFhirExplanationOfBenefitProcessNote = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitProcessNote;
  TFhirExplanationOfBenefitProcessNoteList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitProcessNoteList;
  TFhirExplanationOfBenefitBenefitBalance = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitBenefitBalance;
  TFhirExplanationOfBenefitBenefitBalanceList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitBenefitBalanceList;
  TFhirExplanationOfBenefitBenefitBalanceFinancial = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitBenefitBalanceFinancial;
  TFhirExplanationOfBenefitBenefitBalanceFinancialList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitBenefitBalanceFinancialList;
  TFhirExplanationOfBenefit = FHIR.R3.Resources.Other.TFhirExplanationOfBenefit;
  TFhirExplanationOfBenefitList = FHIR.R3.Resources.Other.TFhirExplanationOfBenefitList;
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}
{$IFDEF FHIR_GUIDANCERESPONSE}
  TFhirGuidanceResponse = FHIR.R3.Resources.Other.TFhirGuidanceResponse;
  TFhirGuidanceResponseList = FHIR.R3.Resources.Other.TFhirGuidanceResponseList;
{$ENDIF FHIR_GUIDANCERESPONSE}
{$IFDEF FHIR_LINKAGE}
  TFhirLinkageItem = FHIR.R3.Resources.Other.TFhirLinkageItem;
  TFhirLinkageItemList = FHIR.R3.Resources.Other.TFhirLinkageItemList;
  TFhirLinkage = FHIR.R3.Resources.Other.TFhirLinkage;
  TFhirLinkageList = FHIR.R3.Resources.Other.TFhirLinkageList;
{$ENDIF FHIR_LINKAGE}
{$IFDEF FHIR_LIST}
  TFhirListEntry = FHIR.R3.Resources.Other.TFhirListEntry;
  TFhirListEntryList = FHIR.R3.Resources.Other.TFhirListEntryList;
  TFhirList = FHIR.R3.Resources.Other.TFhirList;
  TFhirListList = FHIR.R3.Resources.Other.TFhirListList;
{$ENDIF FHIR_LIST}
{$IFDEF FHIR_MEASUREREPORT}
  TFhirMeasureReportGroup = FHIR.R3.Resources.Other.TFhirMeasureReportGroup;
  TFhirMeasureReportGroupList = FHIR.R3.Resources.Other.TFhirMeasureReportGroupList;
  TFhirMeasureReportGroupPopulation = FHIR.R3.Resources.Other.TFhirMeasureReportGroupPopulation;
  TFhirMeasureReportGroupPopulationList = FHIR.R3.Resources.Other.TFhirMeasureReportGroupPopulationList;
  TFhirMeasureReportGroupStratifier = FHIR.R3.Resources.Other.TFhirMeasureReportGroupStratifier;
  TFhirMeasureReportGroupStratifierList = FHIR.R3.Resources.Other.TFhirMeasureReportGroupStratifierList;
  TFhirMeasureReportGroupStratifierStratum = FHIR.R3.Resources.Other.TFhirMeasureReportGroupStratifierStratum;
  TFhirMeasureReportGroupStratifierStratumList = FHIR.R3.Resources.Other.TFhirMeasureReportGroupStratifierStratumList;
  TFhirMeasureReportGroupStratifierStratumPopulation = FHIR.R3.Resources.Other.TFhirMeasureReportGroupStratifierStratumPopulation;
  TFhirMeasureReportGroupStratifierStratumPopulationList = FHIR.R3.Resources.Other.TFhirMeasureReportGroupStratifierStratumPopulationList;
  TFhirMeasureReport = FHIR.R3.Resources.Other.TFhirMeasureReport;
  TFhirMeasureReportList = FHIR.R3.Resources.Other.TFhirMeasureReportList;
{$ENDIF FHIR_MEASUREREPORT}
{$IFDEF FHIR_MEDICATION}
  TFhirMedicationIngredient = FHIR.R3.Resources.Other.TFhirMedicationIngredient;
  TFhirMedicationIngredientList = FHIR.R3.Resources.Other.TFhirMedicationIngredientList;
  TFhirMedicationPackage = FHIR.R3.Resources.Other.TFhirMedicationPackage;
  TFhirMedicationPackageList = FHIR.R3.Resources.Other.TFhirMedicationPackageList;
  TFhirMedicationPackageContent = FHIR.R3.Resources.Other.TFhirMedicationPackageContent;
  TFhirMedicationPackageContentList = FHIR.R3.Resources.Other.TFhirMedicationPackageContentList;
  TFhirMedicationPackageBatch = FHIR.R3.Resources.Other.TFhirMedicationPackageBatch;
  TFhirMedicationPackageBatchList = FHIR.R3.Resources.Other.TFhirMedicationPackageBatchList;
  TFhirMedication = FHIR.R3.Resources.Other.TFhirMedication;
  TFhirMedicationList = FHIR.R3.Resources.Other.TFhirMedicationList;
{$ENDIF FHIR_MEDICATION}
{$IFDEF FHIR_OPERATIONOUTCOME}
  TFhirOperationOutcomeIssue = FHIR.R3.Resources.Other.TFhirOperationOutcomeIssue;
  TFhirOperationOutcomeIssueList = FHIR.R3.Resources.Other.TFhirOperationOutcomeIssueList;
  TFhirOperationOutcome = FHIR.R3.Resources.Other.TFhirOperationOutcome;
  TFhirOperationOutcomeList = FHIR.R3.Resources.Other.TFhirOperationOutcomeList;
{$ENDIF FHIR_OPERATIONOUTCOME}
{$IFDEF FHIR_MESSAGEHEADER}
  TFhirMessageHeaderDestination = FHIR.R3.Resources.Other.TFhirMessageHeaderDestination;
  TFhirMessageHeaderDestinationList = FHIR.R3.Resources.Other.TFhirMessageHeaderDestinationList;
  TFhirMessageHeaderSource = FHIR.R3.Resources.Other.TFhirMessageHeaderSource;
  TFhirMessageHeaderSourceList = FHIR.R3.Resources.Other.TFhirMessageHeaderSourceList;
  TFhirMessageHeaderResponse = FHIR.R3.Resources.Other.TFhirMessageHeaderResponse;
  TFhirMessageHeaderResponseList = FHIR.R3.Resources.Other.TFhirMessageHeaderResponseList;
  TFhirMessageHeader = FHIR.R3.Resources.Other.TFhirMessageHeader;
  TFhirMessageHeaderList = FHIR.R3.Resources.Other.TFhirMessageHeaderList;
{$ENDIF FHIR_MESSAGEHEADER}
{$IFDEF FHIR_PAYMENTNOTICE}
  TFhirPaymentNotice = FHIR.R3.Resources.Other.TFhirPaymentNotice;
  TFhirPaymentNoticeList = FHIR.R3.Resources.Other.TFhirPaymentNoticeList;
{$ENDIF FHIR_PAYMENTNOTICE}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  TFhirPaymentReconciliationDetail = FHIR.R3.Resources.Other.TFhirPaymentReconciliationDetail;
  TFhirPaymentReconciliationDetailList = FHIR.R3.Resources.Other.TFhirPaymentReconciliationDetailList;
  TFhirPaymentReconciliationProcessNote = FHIR.R3.Resources.Other.TFhirPaymentReconciliationProcessNote;
  TFhirPaymentReconciliationProcessNoteList = FHIR.R3.Resources.Other.TFhirPaymentReconciliationProcessNoteList;
  TFhirPaymentReconciliation = FHIR.R3.Resources.Other.TFhirPaymentReconciliation;
  TFhirPaymentReconciliationList = FHIR.R3.Resources.Other.TFhirPaymentReconciliationList;
{$ENDIF FHIR_PAYMENTRECONCILIATION}
{$IFDEF FHIR_PROCESSREQUEST}
  TFhirProcessRequestItem = FHIR.R3.Resources.Other.TFhirProcessRequestItem;
  TFhirProcessRequestItemList = FHIR.R3.Resources.Other.TFhirProcessRequestItemList;
  TFhirProcessRequest = FHIR.R3.Resources.Other.TFhirProcessRequest;
  TFhirProcessRequestList = FHIR.R3.Resources.Other.TFhirProcessRequestList;
{$ENDIF FHIR_PROCESSREQUEST}
{$IFDEF FHIR_PROCESSRESPONSE}
  TFhirProcessResponseProcessNote = FHIR.R3.Resources.Other.TFhirProcessResponseProcessNote;
  TFhirProcessResponseProcessNoteList = FHIR.R3.Resources.Other.TFhirProcessResponseProcessNoteList;
  TFhirProcessResponse = FHIR.R3.Resources.Other.TFhirProcessResponse;
  TFhirProcessResponseList = FHIR.R3.Resources.Other.TFhirProcessResponseList;
{$ENDIF FHIR_PROCESSRESPONSE}
{$IFDEF FHIR_PROVENANCE}
  TFhirProvenanceAgent = FHIR.R3.Resources.Other.TFhirProvenanceAgent;
  TFhirProvenanceAgentList = FHIR.R3.Resources.Other.TFhirProvenanceAgentList;
  TFhirProvenanceEntity = FHIR.R3.Resources.Other.TFhirProvenanceEntity;
  TFhirProvenanceEntityList = FHIR.R3.Resources.Other.TFhirProvenanceEntityList;
  TFhirProvenance = FHIR.R3.Resources.Other.TFhirProvenance;
  TFhirProvenanceList = FHIR.R3.Resources.Other.TFhirProvenanceList;
{$ENDIF FHIR_PROVENANCE}
{$IFDEF FHIR_RESEARCHSTUDY}
  TFhirResearchStudyArm = FHIR.R3.Resources.Other.TFhirResearchStudyArm;
  TFhirResearchStudyArmList = FHIR.R3.Resources.Other.TFhirResearchStudyArmList;
  TFhirResearchStudy = FHIR.R3.Resources.Other.TFhirResearchStudy;
  TFhirResearchStudyList = FHIR.R3.Resources.Other.TFhirResearchStudyList;
{$ENDIF FHIR_RESEARCHSTUDY}
{$IFDEF FHIR_RESEARCHSUBJECT}
  TFhirResearchSubject = FHIR.R3.Resources.Other.TFhirResearchSubject;
  TFhirResearchSubjectList = FHIR.R3.Resources.Other.TFhirResearchSubjectList;
{$ENDIF FHIR_RESEARCHSUBJECT}
{$IFDEF FHIR_SUBSCRIPTION}
  TFhirSubscriptionChannel = FHIR.R3.Resources.Other.TFhirSubscriptionChannel;
  TFhirSubscriptionChannelList = FHIR.R3.Resources.Other.TFhirSubscriptionChannelList;
  TFhirSubscription = FHIR.R3.Resources.Other.TFhirSubscription;
  TFhirSubscriptionList = FHIR.R3.Resources.Other.TFhirSubscriptionList;
{$ENDIF FHIR_SUBSCRIPTION}
{$IFDEF FHIR_TASK}
  TFhirTaskRequester = FHIR.R3.Resources.Other.TFhirTaskRequester;
  TFhirTaskRequesterList = FHIR.R3.Resources.Other.TFhirTaskRequesterList;
  TFhirTaskRestriction = FHIR.R3.Resources.Other.TFhirTaskRestriction;
  TFhirTaskRestrictionList = FHIR.R3.Resources.Other.TFhirTaskRestrictionList;
  TFhirTaskInput = FHIR.R3.Resources.Other.TFhirTaskInput;
  TFhirTaskInputList = FHIR.R3.Resources.Other.TFhirTaskInputList;
  TFhirTaskOutput = FHIR.R3.Resources.Other.TFhirTaskOutput;
  TFhirTaskOutputList = FHIR.R3.Resources.Other.TFhirTaskOutputList;
  TFhirTask = FHIR.R3.Resources.Other.TFhirTask;
  TFhirTaskList = FHIR.R3.Resources.Other.TFhirTaskList;
{$ENDIF FHIR_TASK}
{$IFDEF FHIR_TESTREPORT}
  TFhirTestReportParticipant = FHIR.R3.Resources.Other.TFhirTestReportParticipant;
  TFhirTestReportParticipantList = FHIR.R3.Resources.Other.TFhirTestReportParticipantList;
  TFhirTestReportSetup = FHIR.R3.Resources.Other.TFhirTestReportSetup;
  TFhirTestReportSetupList = FHIR.R3.Resources.Other.TFhirTestReportSetupList;
  TFhirTestReportSetupAction = FHIR.R3.Resources.Other.TFhirTestReportSetupAction;
  TFhirTestReportSetupActionList = FHIR.R3.Resources.Other.TFhirTestReportSetupActionList;
  TFhirTestReportSetupActionOperation = FHIR.R3.Resources.Other.TFhirTestReportSetupActionOperation;
  TFhirTestReportSetupActionOperationList = FHIR.R3.Resources.Other.TFhirTestReportSetupActionOperationList;
  TFhirTestReportSetupActionAssert = FHIR.R3.Resources.Other.TFhirTestReportSetupActionAssert;
  TFhirTestReportSetupActionAssertList = FHIR.R3.Resources.Other.TFhirTestReportSetupActionAssertList;
  TFhirTestReportTest = FHIR.R3.Resources.Other.TFhirTestReportTest;
  TFhirTestReportTestList = FHIR.R3.Resources.Other.TFhirTestReportTestList;
  TFhirTestReportTestAction = FHIR.R3.Resources.Other.TFhirTestReportTestAction;
  TFhirTestReportTestActionList = FHIR.R3.Resources.Other.TFhirTestReportTestActionList;
  TFhirTestReportTeardown = FHIR.R3.Resources.Other.TFhirTestReportTeardown;
  TFhirTestReportTeardownList = FHIR.R3.Resources.Other.TFhirTestReportTeardownList;
  TFhirTestReportTeardownAction = FHIR.R3.Resources.Other.TFhirTestReportTeardownAction;
  TFhirTestReportTeardownActionList = FHIR.R3.Resources.Other.TFhirTestReportTeardownActionList;
  TFhirTestReport = FHIR.R3.Resources.Other.TFhirTestReport;
  TFhirTestReportList = FHIR.R3.Resources.Other.TFhirTestReportList;
{$ENDIF FHIR_TESTREPORT}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  TFhirActivityDefinitionParticipant = FHIR.R3.Resources.Canonical.TFhirActivityDefinitionParticipant;
  TFhirActivityDefinitionParticipantList = FHIR.R3.Resources.Canonical.TFhirActivityDefinitionParticipantList;
  TFhirActivityDefinitionDynamicValue = FHIR.R3.Resources.Canonical.TFhirActivityDefinitionDynamicValue;
  TFhirActivityDefinitionDynamicValueList = FHIR.R3.Resources.Canonical.TFhirActivityDefinitionDynamicValueList;
  TFhirActivityDefinition = FHIR.R3.Resources.Canonical.TFhirActivityDefinition;
  TFhirActivityDefinitionList = FHIR.R3.Resources.Canonical.TFhirActivityDefinitionList;
{$ENDIF FHIR_ACTIVITYDEFINITION}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  TFhirCapabilityStatementSoftware = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementSoftware;
  TFhirCapabilityStatementSoftwareList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementSoftwareList;
  TFhirCapabilityStatementImplementation = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementImplementation;
  TFhirCapabilityStatementImplementationList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementImplementationList;
  TFhirCapabilityStatementRest = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRest;
  TFhirCapabilityStatementRestList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestList;
  TFhirCapabilityStatementRestSecurity = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestSecurity;
  TFhirCapabilityStatementRestSecurityList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestSecurityList;
  TFhirCapabilityStatementRestSecurityCertificate = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestSecurityCertificate;
  TFhirCapabilityStatementRestSecurityCertificateList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestSecurityCertificateList;
  TFhirCapabilityStatementRestResource = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestResource;
  TFhirCapabilityStatementRestResourceList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestResourceList;
  TFhirCapabilityStatementRestResourceInteraction = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestResourceInteraction;
  TFhirCapabilityStatementRestResourceInteractionList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestResourceInteractionList;
  TFhirCapabilityStatementRestResourceSearchParam = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestResourceSearchParam;
  TFhirCapabilityStatementRestResourceSearchParamList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestResourceSearchParamList;
  TFhirCapabilityStatementRestInteraction = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestInteraction;
  TFhirCapabilityStatementRestInteractionList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestInteractionList;
  TFhirCapabilityStatementRestOperation = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestOperation;
  TFhirCapabilityStatementRestOperationList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementRestOperationList;
  TFhirCapabilityStatementMessaging = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementMessaging;
  TFhirCapabilityStatementMessagingList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementMessagingList;
  TFhirCapabilityStatementMessagingEndpoint = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementMessagingEndpoint;
  TFhirCapabilityStatementMessagingEndpointList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementMessagingEndpointList;
  TFhirCapabilityStatementMessagingSupportedMessage = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementMessagingSupportedMessage;
  TFhirCapabilityStatementMessagingSupportedMessageList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementMessagingSupportedMessageList;
  TFhirCapabilityStatementMessagingEvent = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementMessagingEvent;
  TFhirCapabilityStatementMessagingEventList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementMessagingEventList;
  TFhirCapabilityStatementDocument = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementDocument;
  TFhirCapabilityStatementDocumentList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementDocumentList;
  TFhirCapabilityStatement = FHIR.R3.Resources.Canonical.TFhirCapabilityStatement;
  TFhirCapabilityStatementList = FHIR.R3.Resources.Canonical.TFhirCapabilityStatementList;
{$ENDIF FHIR_CAPABILITYSTATEMENT}
{$IFDEF FHIR_CODESYSTEM}
  TFhirCodeSystemFilter = FHIR.R3.Resources.Canonical.TFhirCodeSystemFilter;
  TFhirCodeSystemFilterList = FHIR.R3.Resources.Canonical.TFhirCodeSystemFilterList;
  TFhirCodeSystemProperty = FHIR.R3.Resources.Canonical.TFhirCodeSystemProperty;
  TFhirCodeSystemPropertyList = FHIR.R3.Resources.Canonical.TFhirCodeSystemPropertyList;
  TFhirCodeSystemConcept = FHIR.R3.Resources.Canonical.TFhirCodeSystemConcept;
  TFhirCodeSystemConceptList = FHIR.R3.Resources.Canonical.TFhirCodeSystemConceptList;
  TFhirCodeSystemConceptDesignation = FHIR.R3.Resources.Canonical.TFhirCodeSystemConceptDesignation;
  TFhirCodeSystemConceptDesignationList = FHIR.R3.Resources.Canonical.TFhirCodeSystemConceptDesignationList;
  TFhirCodeSystemConceptProperty = FHIR.R3.Resources.Canonical.TFhirCodeSystemConceptProperty;
  TFhirCodeSystemConceptPropertyList = FHIR.R3.Resources.Canonical.TFhirCodeSystemConceptPropertyList;
  TFhirCodeSystem = FHIR.R3.Resources.Canonical.TFhirCodeSystem;
  TFhirCodeSystemList = FHIR.R3.Resources.Canonical.TFhirCodeSystemList;
{$ENDIF FHIR_CODESYSTEM}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  TFhirCompartmentDefinitionResource = FHIR.R3.Resources.Canonical.TFhirCompartmentDefinitionResource;
  TFhirCompartmentDefinitionResourceList = FHIR.R3.Resources.Canonical.TFhirCompartmentDefinitionResourceList;
  TFhirCompartmentDefinition = FHIR.R3.Resources.Canonical.TFhirCompartmentDefinition;
  TFhirCompartmentDefinitionList = FHIR.R3.Resources.Canonical.TFhirCompartmentDefinitionList;
{$ENDIF FHIR_COMPARTMENTDEFINITION}
{$IFDEF FHIR_CONCEPTMAP}
  TFhirConceptMapGroup = FHIR.R3.Resources.Canonical.TFhirConceptMapGroup;
  TFhirConceptMapGroupList = FHIR.R3.Resources.Canonical.TFhirConceptMapGroupList;
  TFhirConceptMapGroupElement = FHIR.R3.Resources.Canonical.TFhirConceptMapGroupElement;
  TFhirConceptMapGroupElementList = FHIR.R3.Resources.Canonical.TFhirConceptMapGroupElementList;
  TFhirConceptMapGroupElementTarget = FHIR.R3.Resources.Canonical.TFhirConceptMapGroupElementTarget;
  TFhirConceptMapGroupElementTargetList = FHIR.R3.Resources.Canonical.TFhirConceptMapGroupElementTargetList;
  TFhirConceptMapGroupElementTargetDependsOn = FHIR.R3.Resources.Canonical.TFhirConceptMapGroupElementTargetDependsOn;
  TFhirConceptMapGroupElementTargetDependsOnList = FHIR.R3.Resources.Canonical.TFhirConceptMapGroupElementTargetDependsOnList;
  TFhirConceptMapGroupUnmapped = FHIR.R3.Resources.Canonical.TFhirConceptMapGroupUnmapped;
  TFhirConceptMapGroupUnmappedList = FHIR.R3.Resources.Canonical.TFhirConceptMapGroupUnmappedList;
  TFhirConceptMap = FHIR.R3.Resources.Canonical.TFhirConceptMap;
  TFhirConceptMapList = FHIR.R3.Resources.Canonical.TFhirConceptMapList;
{$ENDIF FHIR_CONCEPTMAP}
{$IFDEF FHIR_DATAELEMENT}
  TFhirDataElementMapping = FHIR.R3.Resources.Canonical.TFhirDataElementMapping;
  TFhirDataElementMappingList = FHIR.R3.Resources.Canonical.TFhirDataElementMappingList;
  TFhirDataElement = FHIR.R3.Resources.Canonical.TFhirDataElement;
  TFhirDataElementList = FHIR.R3.Resources.Canonical.TFhirDataElementList;
{$ENDIF FHIR_DATAELEMENT}
{$IFDEF FHIR_EXPANSIONPROFILE}
  TFhirExpansionProfileFixedVersion = FHIR.R3.Resources.Canonical.TFhirExpansionProfileFixedVersion;
  TFhirExpansionProfileFixedVersionList = FHIR.R3.Resources.Canonical.TFhirExpansionProfileFixedVersionList;
  TFhirExpansionProfileExcludedSystem = FHIR.R3.Resources.Canonical.TFhirExpansionProfileExcludedSystem;
  TFhirExpansionProfileExcludedSystemList = FHIR.R3.Resources.Canonical.TFhirExpansionProfileExcludedSystemList;
  TFhirExpansionProfileDesignation = FHIR.R3.Resources.Canonical.TFhirExpansionProfileDesignation;
  TFhirExpansionProfileDesignationList = FHIR.R3.Resources.Canonical.TFhirExpansionProfileDesignationList;
  TFhirExpansionProfileDesignationInclude = FHIR.R3.Resources.Canonical.TFhirExpansionProfileDesignationInclude;
  TFhirExpansionProfileDesignationIncludeList = FHIR.R3.Resources.Canonical.TFhirExpansionProfileDesignationIncludeList;
  TFhirExpansionProfileDesignationIncludeDesignation = FHIR.R3.Resources.Canonical.TFhirExpansionProfileDesignationIncludeDesignation;
  TFhirExpansionProfileDesignationIncludeDesignationList = FHIR.R3.Resources.Canonical.TFhirExpansionProfileDesignationIncludeDesignationList;
  TFhirExpansionProfileDesignationExclude = FHIR.R3.Resources.Canonical.TFhirExpansionProfileDesignationExclude;
  TFhirExpansionProfileDesignationExcludeList = FHIR.R3.Resources.Canonical.TFhirExpansionProfileDesignationExcludeList;
  TFhirExpansionProfileDesignationExcludeDesignation = FHIR.R3.Resources.Canonical.TFhirExpansionProfileDesignationExcludeDesignation;
  TFhirExpansionProfileDesignationExcludeDesignationList = FHIR.R3.Resources.Canonical.TFhirExpansionProfileDesignationExcludeDesignationList;
  TFhirExpansionProfile = FHIR.R3.Resources.Canonical.TFhirExpansionProfile;
  TFhirExpansionProfileList = FHIR.R3.Resources.Canonical.TFhirExpansionProfileList;
{$ENDIF FHIR_EXPANSIONPROFILE}
{$IFDEF FHIR_GRAPHDEFINITION}
  TFhirGraphDefinitionLink = FHIR.R3.Resources.Canonical.TFhirGraphDefinitionLink;
  TFhirGraphDefinitionLinkList = FHIR.R3.Resources.Canonical.TFhirGraphDefinitionLinkList;
  TFhirGraphDefinitionLinkTarget = FHIR.R3.Resources.Canonical.TFhirGraphDefinitionLinkTarget;
  TFhirGraphDefinitionLinkTargetList = FHIR.R3.Resources.Canonical.TFhirGraphDefinitionLinkTargetList;
  TFhirGraphDefinitionLinkTargetCompartment = FHIR.R3.Resources.Canonical.TFhirGraphDefinitionLinkTargetCompartment;
  TFhirGraphDefinitionLinkTargetCompartmentList = FHIR.R3.Resources.Canonical.TFhirGraphDefinitionLinkTargetCompartmentList;
  TFhirGraphDefinition = FHIR.R3.Resources.Canonical.TFhirGraphDefinition;
  TFhirGraphDefinitionList = FHIR.R3.Resources.Canonical.TFhirGraphDefinitionList;
{$ENDIF FHIR_GRAPHDEFINITION}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  TFhirImplementationGuideDependency = FHIR.R3.Resources.Canonical.TFhirImplementationGuideDependency;
  TFhirImplementationGuideDependencyList = FHIR.R3.Resources.Canonical.TFhirImplementationGuideDependencyList;
  TFhirImplementationGuidePackage = FHIR.R3.Resources.Canonical.TFhirImplementationGuidePackage;
  TFhirImplementationGuidePackageList = FHIR.R3.Resources.Canonical.TFhirImplementationGuidePackageList;
  TFhirImplementationGuidePackageResource = FHIR.R3.Resources.Canonical.TFhirImplementationGuidePackageResource;
  TFhirImplementationGuidePackageResourceList = FHIR.R3.Resources.Canonical.TFhirImplementationGuidePackageResourceList;
  TFhirImplementationGuideGlobal = FHIR.R3.Resources.Canonical.TFhirImplementationGuideGlobal;
  TFhirImplementationGuideGlobalList = FHIR.R3.Resources.Canonical.TFhirImplementationGuideGlobalList;
  TFhirImplementationGuidePage = FHIR.R3.Resources.Canonical.TFhirImplementationGuidePage;
  TFhirImplementationGuidePageList = FHIR.R3.Resources.Canonical.TFhirImplementationGuidePageList;
  TFhirImplementationGuide = FHIR.R3.Resources.Canonical.TFhirImplementationGuide;
  TFhirImplementationGuideList = FHIR.R3.Resources.Canonical.TFhirImplementationGuideList;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}
{$IFDEF FHIR_LIBRARY}
  TFhirLibrary = FHIR.R3.Resources.Canonical.TFhirLibrary;
  TFhirLibraryList = FHIR.R3.Resources.Canonical.TFhirLibraryList;
{$ENDIF FHIR_LIBRARY}
{$IFDEF FHIR_MEASURE}
  TFhirMeasureGroup = FHIR.R3.Resources.Canonical.TFhirMeasureGroup;
  TFhirMeasureGroupList = FHIR.R3.Resources.Canonical.TFhirMeasureGroupList;
  TFhirMeasureGroupPopulation = FHIR.R3.Resources.Canonical.TFhirMeasureGroupPopulation;
  TFhirMeasureGroupPopulationList = FHIR.R3.Resources.Canonical.TFhirMeasureGroupPopulationList;
  TFhirMeasureGroupStratifier = FHIR.R3.Resources.Canonical.TFhirMeasureGroupStratifier;
  TFhirMeasureGroupStratifierList = FHIR.R3.Resources.Canonical.TFhirMeasureGroupStratifierList;
  TFhirMeasureSupplementalData = FHIR.R3.Resources.Canonical.TFhirMeasureSupplementalData;
  TFhirMeasureSupplementalDataList = FHIR.R3.Resources.Canonical.TFhirMeasureSupplementalDataList;
  TFhirMeasure = FHIR.R3.Resources.Canonical.TFhirMeasure;
  TFhirMeasureList = FHIR.R3.Resources.Canonical.TFhirMeasureList;
{$ENDIF FHIR_MEASURE}
{$IFDEF FHIR_MESSAGEDEFINITION}
  TFhirMessageDefinitionFocus = FHIR.R3.Resources.Canonical.TFhirMessageDefinitionFocus;
  TFhirMessageDefinitionFocusList = FHIR.R3.Resources.Canonical.TFhirMessageDefinitionFocusList;
  TFhirMessageDefinitionAllowedResponse = FHIR.R3.Resources.Canonical.TFhirMessageDefinitionAllowedResponse;
  TFhirMessageDefinitionAllowedResponseList = FHIR.R3.Resources.Canonical.TFhirMessageDefinitionAllowedResponseList;
  TFhirMessageDefinition = FHIR.R3.Resources.Canonical.TFhirMessageDefinition;
  TFhirMessageDefinitionList = FHIR.R3.Resources.Canonical.TFhirMessageDefinitionList;
{$ENDIF FHIR_MESSAGEDEFINITION}
{$IFDEF FHIR_NAMINGSYSTEM}
  TFhirNamingSystemUniqueId = FHIR.R3.Resources.Canonical.TFhirNamingSystemUniqueId;
  TFhirNamingSystemUniqueIdList = FHIR.R3.Resources.Canonical.TFhirNamingSystemUniqueIdList;
  TFhirNamingSystem = FHIR.R3.Resources.Canonical.TFhirNamingSystem;
  TFhirNamingSystemList = FHIR.R3.Resources.Canonical.TFhirNamingSystemList;
{$ENDIF FHIR_NAMINGSYSTEM}
{$IFDEF FHIR_OPERATIONDEFINITION}
  TFhirOperationDefinitionParameter = FHIR.R3.Resources.Canonical.TFhirOperationDefinitionParameter;
  TFhirOperationDefinitionParameterList = FHIR.R3.Resources.Canonical.TFhirOperationDefinitionParameterList;
  TFhirOperationDefinitionParameterBinding = FHIR.R3.Resources.Canonical.TFhirOperationDefinitionParameterBinding;
  TFhirOperationDefinitionParameterBindingList = FHIR.R3.Resources.Canonical.TFhirOperationDefinitionParameterBindingList;
  TFhirOperationDefinitionOverload = FHIR.R3.Resources.Canonical.TFhirOperationDefinitionOverload;
  TFhirOperationDefinitionOverloadList = FHIR.R3.Resources.Canonical.TFhirOperationDefinitionOverloadList;
  TFhirOperationDefinition = FHIR.R3.Resources.Canonical.TFhirOperationDefinition;
  TFhirOperationDefinitionList = FHIR.R3.Resources.Canonical.TFhirOperationDefinitionList;
{$ENDIF FHIR_OPERATIONDEFINITION}
{$IFDEF FHIR_PLANDEFINITION}
  TFhirPlanDefinitionGoal = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionGoal;
  TFhirPlanDefinitionGoalList = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionGoalList;
  TFhirPlanDefinitionGoalTarget = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionGoalTarget;
  TFhirPlanDefinitionGoalTargetList = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionGoalTargetList;
  TFhirPlanDefinitionAction = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionAction;
  TFhirPlanDefinitionActionList = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionActionList;
  TFhirPlanDefinitionActionCondition = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionActionCondition;
  TFhirPlanDefinitionActionConditionList = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionActionConditionList;
  TFhirPlanDefinitionActionRelatedAction = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionActionRelatedAction;
  TFhirPlanDefinitionActionRelatedActionList = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionActionRelatedActionList;
  TFhirPlanDefinitionActionParticipant = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionActionParticipant;
  TFhirPlanDefinitionActionParticipantList = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionActionParticipantList;
  TFhirPlanDefinitionActionDynamicValue = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionActionDynamicValue;
  TFhirPlanDefinitionActionDynamicValueList = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionActionDynamicValueList;
  TFhirPlanDefinition = FHIR.R3.Resources.Canonical.TFhirPlanDefinition;
  TFhirPlanDefinitionList = FHIR.R3.Resources.Canonical.TFhirPlanDefinitionList;
{$ENDIF FHIR_PLANDEFINITION}
{$IFDEF FHIR_QUESTIONNAIRE}
  TFhirQuestionnaireItem = FHIR.R3.Resources.Canonical.TFhirQuestionnaireItem;
  TFhirQuestionnaireItemList = FHIR.R3.Resources.Canonical.TFhirQuestionnaireItemList;
  TFhirQuestionnaireItemEnableWhen = FHIR.R3.Resources.Canonical.TFhirQuestionnaireItemEnableWhen;
  TFhirQuestionnaireItemEnableWhenList = FHIR.R3.Resources.Canonical.TFhirQuestionnaireItemEnableWhenList;
  TFhirQuestionnaireItemOption = FHIR.R3.Resources.Canonical.TFhirQuestionnaireItemOption;
  TFhirQuestionnaireItemOptionList = FHIR.R3.Resources.Canonical.TFhirQuestionnaireItemOptionList;
  TFhirQuestionnaire = FHIR.R3.Resources.Canonical.TFhirQuestionnaire;
  TFhirQuestionnaireList = FHIR.R3.Resources.Canonical.TFhirQuestionnaireList;
{$ENDIF FHIR_QUESTIONNAIRE}
{$IFDEF FHIR_REQUESTGROUP}
  TFhirRequestGroupAction = FHIR.R3.Resources.Canonical.TFhirRequestGroupAction;
  TFhirRequestGroupActionList = FHIR.R3.Resources.Canonical.TFhirRequestGroupActionList;
  TFhirRequestGroupActionCondition = FHIR.R3.Resources.Canonical.TFhirRequestGroupActionCondition;
  TFhirRequestGroupActionConditionList = FHIR.R3.Resources.Canonical.TFhirRequestGroupActionConditionList;
  TFhirRequestGroupActionRelatedAction = FHIR.R3.Resources.Canonical.TFhirRequestGroupActionRelatedAction;
  TFhirRequestGroupActionRelatedActionList = FHIR.R3.Resources.Canonical.TFhirRequestGroupActionRelatedActionList;
  TFhirRequestGroup = FHIR.R3.Resources.Canonical.TFhirRequestGroup;
  TFhirRequestGroupList = FHIR.R3.Resources.Canonical.TFhirRequestGroupList;
{$ENDIF FHIR_REQUESTGROUP}
{$IFDEF FHIR_SEARCHPARAMETER}
  TFhirSearchParameterComponent = FHIR.R3.Resources.Canonical.TFhirSearchParameterComponent;
  TFhirSearchParameterComponentList = FHIR.R3.Resources.Canonical.TFhirSearchParameterComponentList;
  TFhirSearchParameter = FHIR.R3.Resources.Canonical.TFhirSearchParameter;
  TFhirSearchParameterList = FHIR.R3.Resources.Canonical.TFhirSearchParameterList;
{$ENDIF FHIR_SEARCHPARAMETER}
{$IFDEF FHIR_SERVICEDEFINITION}
  TFhirServiceDefinition = FHIR.R3.Resources.Canonical.TFhirServiceDefinition;
  TFhirServiceDefinitionList = FHIR.R3.Resources.Canonical.TFhirServiceDefinitionList;
{$ENDIF FHIR_SERVICEDEFINITION}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  TFhirStructureDefinitionMapping = FHIR.R3.Resources.Canonical.TFhirStructureDefinitionMapping;
  TFhirStructureDefinitionMappingList = FHIR.R3.Resources.Canonical.TFhirStructureDefinitionMappingList;
  TFhirStructureDefinitionSnapshot = FHIR.R3.Resources.Canonical.TFhirStructureDefinitionSnapshot;
  TFhirStructureDefinitionSnapshotList = FHIR.R3.Resources.Canonical.TFhirStructureDefinitionSnapshotList;
  TFhirStructureDefinitionDifferential = FHIR.R3.Resources.Canonical.TFhirStructureDefinitionDifferential;
  TFhirStructureDefinitionDifferentialList = FHIR.R3.Resources.Canonical.TFhirStructureDefinitionDifferentialList;
  TFhirStructureDefinition = FHIR.R3.Resources.Canonical.TFhirStructureDefinition;
  TFhirStructureDefinitionList = FHIR.R3.Resources.Canonical.TFhirStructureDefinitionList;
{$ENDIF FHIR_STRUCTUREDEFINITION}
{$IFDEF FHIR_STRUCTUREMAP}
  TFhirStructureMapStructure = FHIR.R3.Resources.Canonical.TFhirStructureMapStructure;
  TFhirStructureMapStructureList = FHIR.R3.Resources.Canonical.TFhirStructureMapStructureList;
  TFhirStructureMapGroup = FHIR.R3.Resources.Canonical.TFhirStructureMapGroup;
  TFhirStructureMapGroupList = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupList;
  TFhirStructureMapGroupInput = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupInput;
  TFhirStructureMapGroupInputList = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupInputList;
  TFhirStructureMapGroupRule = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupRule;
  TFhirStructureMapGroupRuleList = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupRuleList;
  TFhirStructureMapGroupRuleSource = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupRuleSource;
  TFhirStructureMapGroupRuleSourceList = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupRuleSourceList;
  TFhirStructureMapGroupRuleTarget = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupRuleTarget;
  TFhirStructureMapGroupRuleTargetList = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupRuleTargetList;
  TFhirStructureMapGroupRuleTargetParameter = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupRuleTargetParameter;
  TFhirStructureMapGroupRuleTargetParameterList = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupRuleTargetParameterList;
  TFhirStructureMapGroupRuleDependent = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupRuleDependent;
  TFhirStructureMapGroupRuleDependentList = FHIR.R3.Resources.Canonical.TFhirStructureMapGroupRuleDependentList;
  TFhirStructureMap = FHIR.R3.Resources.Canonical.TFhirStructureMap;
  TFhirStructureMapList = FHIR.R3.Resources.Canonical.TFhirStructureMapList;
{$ENDIF FHIR_STRUCTUREMAP}
{$IFDEF FHIR_TESTSCRIPT}
  TFhirTestScriptOrigin = FHIR.R3.Resources.Canonical.TFhirTestScriptOrigin;
  TFhirTestScriptOriginList = FHIR.R3.Resources.Canonical.TFhirTestScriptOriginList;
  TFhirTestScriptDestination = FHIR.R3.Resources.Canonical.TFhirTestScriptDestination;
  TFhirTestScriptDestinationList = FHIR.R3.Resources.Canonical.TFhirTestScriptDestinationList;
  TFhirTestScriptMetadata = FHIR.R3.Resources.Canonical.TFhirTestScriptMetadata;
  TFhirTestScriptMetadataList = FHIR.R3.Resources.Canonical.TFhirTestScriptMetadataList;
  TFhirTestScriptMetadataLink = FHIR.R3.Resources.Canonical.TFhirTestScriptMetadataLink;
  TFhirTestScriptMetadataLinkList = FHIR.R3.Resources.Canonical.TFhirTestScriptMetadataLinkList;
  TFhirTestScriptMetadataCapability = FHIR.R3.Resources.Canonical.TFhirTestScriptMetadataCapability;
  TFhirTestScriptMetadataCapabilityList = FHIR.R3.Resources.Canonical.TFhirTestScriptMetadataCapabilityList;
  TFhirTestScriptFixture = FHIR.R3.Resources.Canonical.TFhirTestScriptFixture;
  TFhirTestScriptFixtureList = FHIR.R3.Resources.Canonical.TFhirTestScriptFixtureList;
  TFhirTestScriptVariable = FHIR.R3.Resources.Canonical.TFhirTestScriptVariable;
  TFhirTestScriptVariableList = FHIR.R3.Resources.Canonical.TFhirTestScriptVariableList;
  TFhirTestScriptRule = FHIR.R3.Resources.Canonical.TFhirTestScriptRule;
  TFhirTestScriptRuleList = FHIR.R3.Resources.Canonical.TFhirTestScriptRuleList;
  TFhirTestScriptRuleParam = FHIR.R3.Resources.Canonical.TFhirTestScriptRuleParam;
  TFhirTestScriptRuleParamList = FHIR.R3.Resources.Canonical.TFhirTestScriptRuleParamList;
  TFhirTestScriptRuleset = FHIR.R3.Resources.Canonical.TFhirTestScriptRuleset;
  TFhirTestScriptRulesetList = FHIR.R3.Resources.Canonical.TFhirTestScriptRulesetList;
  TFhirTestScriptRulesetRule = FHIR.R3.Resources.Canonical.TFhirTestScriptRulesetRule;
  TFhirTestScriptRulesetRuleList = FHIR.R3.Resources.Canonical.TFhirTestScriptRulesetRuleList;
  TFhirTestScriptRulesetRuleParam = FHIR.R3.Resources.Canonical.TFhirTestScriptRulesetRuleParam;
  TFhirTestScriptRulesetRuleParamList = FHIR.R3.Resources.Canonical.TFhirTestScriptRulesetRuleParamList;
  TFhirTestScriptSetup = FHIR.R3.Resources.Canonical.TFhirTestScriptSetup;
  TFhirTestScriptSetupList = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupList;
  TFhirTestScriptSetupAction = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupAction;
  TFhirTestScriptSetupActionList = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionList;
  TFhirTestScriptSetupActionOperation = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionOperation;
  TFhirTestScriptSetupActionOperationList = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionOperationList;
  TFhirTestScriptSetupActionOperationRequestHeader = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionOperationRequestHeader;
  TFhirTestScriptSetupActionOperationRequestHeaderList = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionOperationRequestHeaderList;
  TFhirTestScriptSetupActionAssert = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssert;
  TFhirTestScriptSetupActionAssertList = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertList;
  TFhirTestScriptSetupActionAssertRule = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertRule;
  TFhirTestScriptSetupActionAssertRuleList = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertRuleList;
  TFhirTestScriptSetupActionAssertRuleParam = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertRuleParam;
  TFhirTestScriptSetupActionAssertRuleParamList = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertRuleParamList;
  TFhirTestScriptSetupActionAssertRuleset = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertRuleset;
  TFhirTestScriptSetupActionAssertRulesetList = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertRulesetList;
  TFhirTestScriptSetupActionAssertRulesetRule = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertRulesetRule;
  TFhirTestScriptSetupActionAssertRulesetRuleList = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertRulesetRuleList;
  TFhirTestScriptSetupActionAssertRulesetRuleParam = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertRulesetRuleParam;
  TFhirTestScriptSetupActionAssertRulesetRuleParamList = FHIR.R3.Resources.Canonical.TFhirTestScriptSetupActionAssertRulesetRuleParamList;
  TFhirTestScriptTest = FHIR.R3.Resources.Canonical.TFhirTestScriptTest;
  TFhirTestScriptTestList = FHIR.R3.Resources.Canonical.TFhirTestScriptTestList;
  TFhirTestScriptTestAction = FHIR.R3.Resources.Canonical.TFhirTestScriptTestAction;
  TFhirTestScriptTestActionList = FHIR.R3.Resources.Canonical.TFhirTestScriptTestActionList;
  TFhirTestScriptTeardown = FHIR.R3.Resources.Canonical.TFhirTestScriptTeardown;
  TFhirTestScriptTeardownList = FHIR.R3.Resources.Canonical.TFhirTestScriptTeardownList;
  TFhirTestScriptTeardownAction = FHIR.R3.Resources.Canonical.TFhirTestScriptTeardownAction;
  TFhirTestScriptTeardownActionList = FHIR.R3.Resources.Canonical.TFhirTestScriptTeardownActionList;
  TFhirTestScript = FHIR.R3.Resources.Canonical.TFhirTestScript;
  TFhirTestScriptList = FHIR.R3.Resources.Canonical.TFhirTestScriptList;
{$ENDIF FHIR_TESTSCRIPT}
{$IFDEF FHIR_VALUESET}
  TFhirValueSetCompose = FHIR.R3.Resources.Canonical.TFhirValueSetCompose;
  TFhirValueSetComposeList = FHIR.R3.Resources.Canonical.TFhirValueSetComposeList;
  TFhirValueSetComposeInclude = FHIR.R3.Resources.Canonical.TFhirValueSetComposeInclude;
  TFhirValueSetComposeIncludeList = FHIR.R3.Resources.Canonical.TFhirValueSetComposeIncludeList;
  TFhirValueSetComposeIncludeConcept = FHIR.R3.Resources.Canonical.TFhirValueSetComposeIncludeConcept;
  TFhirValueSetComposeIncludeConceptList = FHIR.R3.Resources.Canonical.TFhirValueSetComposeIncludeConceptList;
  TFhirValueSetComposeIncludeConceptDesignation = FHIR.R3.Resources.Canonical.TFhirValueSetComposeIncludeConceptDesignation;
  TFhirValueSetComposeIncludeConceptDesignationList = FHIR.R3.Resources.Canonical.TFhirValueSetComposeIncludeConceptDesignationList;
  TFhirValueSetComposeIncludeFilter = FHIR.R3.Resources.Canonical.TFhirValueSetComposeIncludeFilter;
  TFhirValueSetComposeIncludeFilterList = FHIR.R3.Resources.Canonical.TFhirValueSetComposeIncludeFilterList;
  TFhirValueSetExpansion = FHIR.R3.Resources.Canonical.TFhirValueSetExpansion;
  TFhirValueSetExpansionList = FHIR.R3.Resources.Canonical.TFhirValueSetExpansionList;
  TFhirValueSetExpansionParameter = FHIR.R3.Resources.Canonical.TFhirValueSetExpansionParameter;
  TFhirValueSetExpansionParameterList = FHIR.R3.Resources.Canonical.TFhirValueSetExpansionParameterList;
  TFhirValueSetExpansionContains = FHIR.R3.Resources.Canonical.TFhirValueSetExpansionContains;
  TFhirValueSetExpansionContainsList = FHIR.R3.Resources.Canonical.TFhirValueSetExpansionContainsList;
  TFhirValueSet = FHIR.R3.Resources.Canonical.TFhirValueSet;
  TFhirValueSetList = FHIR.R3.Resources.Canonical.TFhirValueSetList;
{$ENDIF FHIR_VALUESET}

implementation

end.

