{!Wrapper uses FHIRBase, FHIRBase_Wrapper, FHIRTypes, FHIRTypes_Wrapper, FHIRResources, FHIRResources_Wrapper}
{!ignore ALL_RESOURCE_TYPES}

unit FHIRIndexConstants;

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

{$IFNDEF FHIR_DSTU3}
This is the dstu3 version of the FHIR code
{$ENDIF}


interface

// FHIR v1.3.0 generated 2016-03-29T23:40:20+11:00

uses
  SysUtils, Classes, StringSupport, DecimalSupport, AdvBuffers, DateAndTime, FHIRBase, FHIRTypes, FHIRResources, FHIRConstants;

Const
  TYPES_TSearchParamsAccount : Array[TSearchParamsAccount] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeQUANTITY,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsAccount : Array[TSearchParamsAccount] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsAccount : Array[TSearchParamsAccount] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsAccount : Array[TSearchParamsAccount] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [frtOrganization], [frtPatient], [], [], [frtDevice, frtPatient, frtLocation, frtHealthcareService, frtOrganization, frtPractitioner], 
      []);
  TYPES_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtPatient], [frtPatient, frtPractitioner], [frtPatient, frtPractitioner, frtRelatedPerson], 
      [], [], [], [], []);
  TYPES_TSearchParamsAppointment : Array[TSearchParamsAppointment] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsAppointment : Array[TSearchParamsAppointment] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsAppointment : Array[TSearchParamsAppointment] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsAppointment : Array[TSearchParamsAppointment] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtPatient, frtHealthcareService, frtLocation, frtPractitioner, frtRelatedPerson], [], [], [], [frtLocation], [], 
      [frtPatient], [frtPractitioner], [], []);
  TYPES_TSearchParamsAppointmentResponse : Array[TSearchParamsAppointmentResponse] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsAppointmentResponse : Array[TSearchParamsAppointmentResponse] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsAppointmentResponse : Array[TSearchParamsAppointmentResponse] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsAppointmentResponse : Array[TSearchParamsAppointmentResponse] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtPatient, frtHealthcareService, frtLocation, frtPractitioner, frtRelatedPerson], [frtAppointment], 
      [], [frtLocation], [], [frtPatient], [frtPractitioner]);
  TYPES_TSearchParamsAuditEvent : Array[TSearchParamsAuditEvent] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN, 
       SearchParamTypeREFERENCE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsAuditEvent : Array[TSearchParamsAuditEvent] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsAuditEvent : Array[TSearchParamsAuditEvent] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsAuditEvent : Array[TSearchParamsAuditEvent] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], [], [], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance,
    frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication,
         frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder],
      
    [], 
      
    [], 
      
    [], 
      
    [frtPatient], 
      
    [], 
      
    [], 
      
    [], 
      
    [], 
      
    [], 
      
    []);
  TYPES_TSearchParamsBasic : Array[TSearchParamsBasic] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsBasic : Array[TSearchParamsBasic] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsBasic : Array[TSearchParamsBasic] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsBasic : Array[TSearchParamsBasic] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtPatient, frtPractitioner, frtRelatedPerson], [], [], [], [frtPatient], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder]);
  TYPES_TSearchParamsBinary : Array[TSearchParamsBinary] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeTOKEN);
  PATHS_TSearchParamsBinary : Array[TSearchParamsBinary] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsBinary : Array[TSearchParamsBinary] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsBinary : Array[TSearchParamsBinary] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsBodySite : Array[TSearchParamsBodySite] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsBodySite : Array[TSearchParamsBodySite] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsBodySite : Array[TSearchParamsBodySite] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsBodySite : Array[TSearchParamsBodySite] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtPatient]);
  TYPES_TSearchParamsBundle : Array[TSearchParamsBundle] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsBundle : Array[TSearchParamsBundle] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsBundle : Array[TSearchParamsBundle] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsBundle : Array[TSearchParamsBundle] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtComposition], [frtMessageHeader], []);
  TYPES_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE, 
       SearchParamTypeCOMPOSITE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtReferralRequest, frtAppointment, frtProcedureRequest, frtCommunicationRequest, frtOrder, frtSupplyRequest, frtVisionPrescription, frtMedicationOrder, frtDeviceUseRequest, frtProcessRequest, frtDiagnosticOrder, frtNutritionOrder], 
      [frtCondition], [], [frtGoal], [frtPatient, frtOrganization, frtPractitioner, frtRelatedPerson], [frtPatient], [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], [], [], [frtCarePlan], [frtPatient, frtGroup]);
  TYPES_TSearchParamsCareTeam : Array[TSearchParamsCareTeam] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsCareTeam : Array[TSearchParamsCareTeam] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsCareTeam : Array[TSearchParamsCareTeam] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsCareTeam : Array[TSearchParamsCareTeam] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtPatient, frtOrganization, frtPractitioner, frtRelatedPerson], [frtPatient], [], [frtPatient, frtGroup], []);
  TYPES_TSearchParamsClaim : Array[TSearchParamsClaim] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsClaim : Array[TSearchParamsClaim] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsClaim : Array[TSearchParamsClaim] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsClaim : Array[TSearchParamsClaim] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtLocation], [], [], [frtOrganization], [], [frtPatient], [], [], [frtPractitioner], [], [frtOrganization], []);
  TYPES_TSearchParamsClaimResponse : Array[TSearchParamsClaimResponse] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsClaimResponse : Array[TSearchParamsClaimResponse] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsClaimResponse : Array[TSearchParamsClaimResponse] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsClaimResponse : Array[TSearchParamsClaimResponse] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [frtOrganization], [], [], [], [frtClaim]);
  TYPES_TSearchParamsClinicalImpression : Array[TSearchParamsClinicalImpression] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsClinicalImpression : Array[TSearchParamsClinicalImpression] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsClinicalImpression : Array[TSearchParamsClinicalImpression] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsClinicalImpression : Array[TSearchParamsClinicalImpression] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtReferralRequest, frtProcedureRequest, frtAppointment, frtSupplyRequest, frtProcedure, frtMedicationOrder, frtDiagnosticOrder, frtNutritionOrder], 
      [frtPractitioner], [], [], [frtFamilyMemberHistory, frtObservation, frtQuestionnaireResponse, frtDiagnosticReport], [frtPatient], [frtCarePlan, frtReferralRequest, frtProcedureRequest, frtCommunicationRequest, frtOrder, frtVisionPrescription, frtProcessRequest, frtDeviceUseRequest, frtAppointment, frtSupplyRequest, frtMedicationOrder, frtDiagnosticOrder, frtNutritionOrder], 
      [frtClinicalImpression], [frtCondition, frtAllergyIntolerance], [], [], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    []);
  TYPES_TSearchParamsCodeSystem : Array[TSearchParamsCodeSystem] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeURI, 
       SearchParamTypeURI,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsCodeSystem : Array[TSearchParamsCodeSystem] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsCodeSystem : Array[TSearchParamsCodeSystem] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsCodeSystem : Array[TSearchParamsCodeSystem] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsCommunication : Array[TSearchParamsCommunication] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeDATE, 
       SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsCommunication : Array[TSearchParamsCommunication] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsCommunication : Array[TSearchParamsCommunication] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsCommunication : Array[TSearchParamsCommunication] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtEncounter], [], [], [frtPatient], [], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtGroup, frtRelatedPerson], 
      [frtCommunicationRequest], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], [], [], [frtPatient]);
  TYPES_TSearchParamsCommunicationRequest : Array[TSearchParamsCommunicationRequest] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeREFERENCE, 
       SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeDATE);
  PATHS_TSearchParamsCommunicationRequest : Array[TSearchParamsCommunicationRequest] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsCommunicationRequest : Array[TSearchParamsCommunicationRequest] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsCommunicationRequest : Array[TSearchParamsCommunicationRequest] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtEncounter], [], [], [frtPatient], [], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], 
      [], [frtPatient, frtPractitioner, frtRelatedPerson], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], [], [frtPatient], []);
  TYPES_TSearchParamsCompartmentDefinition : Array[TSearchParamsCompartmentDefinition] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeURI);
  PATHS_TSearchParamsCompartmentDefinition : Array[TSearchParamsCompartmentDefinition] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsCompartmentDefinition : Array[TSearchParamsCompartmentDefinition] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsCompartmentDefinition : Array[TSearchParamsCompartmentDefinition] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsComposition : Array[TSearchParamsComposition] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE, 
       SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsComposition : Array[TSearchParamsComposition] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsComposition : Array[TSearchParamsComposition] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsComposition : Array[TSearchParamsComposition] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtPatient, frtOrganization, frtPractitioner], [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson], [], [], [], [], 
      [frtEncounter], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse,
         frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder],
      
    [], 
      
    [frtPatient], 
      
    [], 
      
    [], 
      
    [], 
      
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    [], 
      
    []);
  TYPES_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeURI,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN, 
       SearchParamTypeURI,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeURI,  SearchParamTypeURI,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtValueSet, frtStructureDefinition], [], [], [frtValueSet, frtStructureDefinition], [], [frtValueSet, frtStructureDefinition], 
      [], [], [], []);
  TYPES_TSearchParamsCondition : Array[TSearchParamsCondition] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeNUMBER,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsCondition : Array[TSearchParamsCondition] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsCondition : Array[TSearchParamsCondition] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsCondition : Array[TSearchParamsCondition] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtPatient, frtPractitioner], [], [], [], [], [], [frtEncounter], [], [], [], [], [frtPatient], [], []);
  TYPES_TSearchParamsConformance : Array[TSearchParamsConformance] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN, 
       SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeURI,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsConformance : Array[TSearchParamsConformance] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsConformance : Array[TSearchParamsConformance] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsConformance : Array[TSearchParamsConformance] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtStructureDefinition], [], [], [], [frtStructureDefinition], [], []);
  TYPES_TSearchParamsContract : Array[TSearchParamsContract] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE, 
       SearchParamTypeREFERENCE);
  PATHS_TSearchParamsContract : Array[TSearchParamsContract] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsContract : Array[TSearchParamsContract] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsContract : Array[TSearchParamsContract] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtLocation, frtOrganization, frtPatient, frtContract, frtPractitioner, frtSubstance, frtGroup, frtRelatedPerson], [frtOrganization], 
      [frtLocation], [], [], [frtPatient], [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder]);
  TYPES_TSearchParamsCoverage : Array[TSearchParamsCoverage] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsCoverage : Array[TSearchParamsCoverage] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsCoverage : Array[TSearchParamsCoverage] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsCoverage : Array[TSearchParamsCoverage] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtPatient], [], [], [], [], [frtOrganization], [], [], [frtPatient, frtOrganization], [], [], []);
  TYPES_TSearchParamsDataElement : Array[TSearchParamsDataElement] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeURI,  SearchParamTypeSTRING);
  PATHS_TSearchParamsDataElement : Array[TSearchParamsDataElement] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDataElement : Array[TSearchParamsDataElement] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDataElement : Array[TSearchParamsDataElement] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsDecisionSupportRule : Array[TSearchParamsDecisionSupportRule] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING);
  PATHS_TSearchParamsDecisionSupportRule : Array[TSearchParamsDecisionSupportRule] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDecisionSupportRule : Array[TSearchParamsDecisionSupportRule] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDecisionSupportRule : Array[TSearchParamsDecisionSupportRule] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsDecisionSupportServiceModule : Array[TSearchParamsDecisionSupportServiceModule] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING);
  PATHS_TSearchParamsDecisionSupportServiceModule : Array[TSearchParamsDecisionSupportServiceModule] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDecisionSupportServiceModule : Array[TSearchParamsDecisionSupportServiceModule] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDecisionSupportServiceModule : Array[TSearchParamsDecisionSupportServiceModule] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsDetectedIssue : Array[TSearchParamsDetectedIssue] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsDetectedIssue : Array[TSearchParamsDetectedIssue] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDetectedIssue : Array[TSearchParamsDetectedIssue] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDetectedIssue : Array[TSearchParamsDetectedIssue] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtPractitioner], [], [], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal,
         frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder],
      
    [frtPatient]);
  TYPES_TSearchParamsDevice : Array[TSearchParamsDevice] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeURI);
  PATHS_TSearchParamsDevice : Array[TSearchParamsDevice] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDevice : Array[TSearchParamsDevice] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDevice : Array[TSearchParamsDevice] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtLocation], [], [], [frtOrganization], [frtPatient], [], [], []);
  TYPES_TSearchParamsDeviceComponent : Array[TSearchParamsDeviceComponent] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsDeviceComponent : Array[TSearchParamsDeviceComponent] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDeviceComponent : Array[TSearchParamsDeviceComponent] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDeviceComponent : Array[TSearchParamsDeviceComponent] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDeviceComponent], [frtDevice], []);
  TYPES_TSearchParamsDeviceMetric : Array[TSearchParamsDeviceMetric] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsDeviceMetric : Array[TSearchParamsDeviceMetric] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDeviceMetric : Array[TSearchParamsDeviceMetric] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDeviceMetric : Array[TSearchParamsDeviceMetric] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtDeviceComponent], [frtDevice], []);
  TYPES_TSearchParamsDeviceUseRequest : Array[TSearchParamsDeviceUseRequest] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsDeviceUseRequest : Array[TSearchParamsDeviceUseRequest] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDeviceUseRequest : Array[TSearchParamsDeviceUseRequest] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDeviceUseRequest : Array[TSearchParamsDeviceUseRequest] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice], [frtPatient], [frtPatient]);
  TYPES_TSearchParamsDeviceUseStatement : Array[TSearchParamsDeviceUseStatement] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsDeviceUseStatement : Array[TSearchParamsDeviceUseStatement] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDeviceUseStatement : Array[TSearchParamsDeviceUseStatement] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDeviceUseStatement : Array[TSearchParamsDeviceUseStatement] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice], [frtPatient], [frtPatient]);
  TYPES_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeCOMPOSITE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeCOMPOSITE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtPractitioner], [], [], [frtEncounter], [], [], [], [], [], [], [], [], [frtPractitioner], [frtPatient], 
      [frtSpecimen], [], [frtDevice, frtPatient, frtLocation, frtGroup]);
  TYPES_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE, 
       SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [frtEncounter], [], [frtMedia], [], [frtPatient], [frtOrganization, frtPractitioner], [frtReferralRequest, frtProcedureRequest, frtDiagnosticOrder], 
      [frtObservation], [frtSpecimen], [], [frtDevice, frtPatient, frtLocation, frtGroup]);
  TYPES_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE, 
       SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    [], 
      
    [], 
      
    [], 
      
    [frtPatient], 
      
    [frtPatient, frtOrganization, frtPractitioner, frtRelatedPerson], 
      
    [], 
      
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal,
         frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder],
      
    [], 
      
    [], 
      
    [frtDevice, frtPatient, frtPractitioner, frtGroup], 
      
    []);
  TYPES_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeURI,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeCOMPOSITE, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtOrganization, frtPractitioner], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], 
      [], [], [frtOrganization], [], [frtEncounter], [], [], [], [], [], [], [], [frtPatient], [], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal,
         frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder],
      
    [frtDocumentReference], 
      
    [], 
      
    [], 
      
    [], 
      
    [], 
      
    [], 
      
    [frtDevice, frtPatient, frtPractitioner, frtGroup], 
      
    []);
  TYPES_TSearchParamsEligibilityRequest : Array[TSearchParamsEligibilityRequest] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsEligibilityRequest : Array[TSearchParamsEligibilityRequest] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsEligibilityRequest : Array[TSearchParamsEligibilityRequest] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsEligibilityRequest : Array[TSearchParamsEligibilityRequest] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtLocation], [], [], [frtOrganization], [], [frtPatient], [], [frtPractitioner]);
  TYPES_TSearchParamsEligibilityResponse : Array[TSearchParamsEligibilityResponse] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN, 
       SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsEligibilityResponse : Array[TSearchParamsEligibilityResponse] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsEligibilityResponse : Array[TSearchParamsEligibilityResponse] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsEligibilityResponse : Array[TSearchParamsEligibilityResponse] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [frtOrganization], [], [], [], [frtOrganization], [], [frtPractitioner], [frtEligibilityRequest]);
  TYPES_TSearchParamsEncounter : Array[TSearchParamsEncounter] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeNUMBER,  SearchParamTypeREFERENCE, 
       SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsEncounter : Array[TSearchParamsEncounter] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsEncounter : Array[TSearchParamsEncounter] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsEncounter : Array[TSearchParamsEncounter] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtAppointment], [frtCondition], [], [frtEpisodeOfCare], [], [frtReferralRequest], [frtCondition, frtProcedure], [], [frtLocation], 
      [], [frtEncounter], [frtPractitioner, frtRelatedPerson], [], [frtPatient], [frtPractitioner], [frtProcedure], [], [], [], []);
  TYPES_TSearchParamsEnrollmentRequest : Array[TSearchParamsEnrollmentRequest] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsEnrollmentRequest : Array[TSearchParamsEnrollmentRequest] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsEnrollmentRequest : Array[TSearchParamsEnrollmentRequest] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsEnrollmentRequest : Array[TSearchParamsEnrollmentRequest] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtPatient], [frtPatient]);
  TYPES_TSearchParamsEnrollmentResponse : Array[TSearchParamsEnrollmentResponse] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsEnrollmentResponse : Array[TSearchParamsEnrollmentResponse] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsEnrollmentResponse : Array[TSearchParamsEnrollmentResponse] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsEnrollmentResponse : Array[TSearchParamsEnrollmentResponse] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsEpisodeOfCare : Array[TSearchParamsEpisodeOfCare] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsEpisodeOfCare : Array[TSearchParamsEpisodeOfCare] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsEpisodeOfCare : Array[TSearchParamsEpisodeOfCare] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsEpisodeOfCare : Array[TSearchParamsEpisodeOfCare] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtPractitioner], [frtCondition], [], [], [frtReferralRequest], [frtOrganization], [frtPatient], [], []);
  TYPES_TSearchParamsExpansionProfile : Array[TSearchParamsExpansionProfile] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeURI,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsExpansionProfile : Array[TSearchParamsExpansionProfile] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsExpansionProfile : Array[TSearchParamsExpansionProfile] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsExpansionProfile : Array[TSearchParamsExpansionProfile] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsExplanationOfBenefit : Array[TSearchParamsExplanationOfBenefit] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsExplanationOfBenefit : Array[TSearchParamsExplanationOfBenefit] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsExplanationOfBenefit : Array[TSearchParamsExplanationOfBenefit] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsExplanationOfBenefit : Array[TSearchParamsExplanationOfBenefit] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtClaim], [], [], [], [frtLocation], [], [], [frtOrganization], [], [frtPatient], [], [frtPractitioner]);
  TYPES_TSearchParamsFamilyMemberHistory : Array[TSearchParamsFamilyMemberHistory] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsFamilyMemberHistory : Array[TSearchParamsFamilyMemberHistory] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsFamilyMemberHistory : Array[TSearchParamsFamilyMemberHistory] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsFamilyMemberHistory : Array[TSearchParamsFamilyMemberHistory] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [frtPatient], []);
  TYPES_TSearchParamsFlag : Array[TSearchParamsFlag] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsFlag : Array[TSearchParamsFlag] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsFlag : Array[TSearchParamsFlag] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsFlag : Array[TSearchParamsFlag] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtOrganization, frtPatient, frtPractitioner], [], [frtEncounter], [frtPatient], [frtPatient, frtLocation, frtOrganization, frtPractitioner, frtGroup]);
  TYPES_TSearchParamsGoal : Array[TSearchParamsGoal] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeDATE);
  PATHS_TSearchParamsGoal : Array[TSearchParamsGoal] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsGoal : Array[TSearchParamsGoal] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsGoal : Array[TSearchParamsGoal] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtPatient], [], [frtPatient, frtOrganization, frtGroup], []);
  TYPES_TSearchParamsGroup : Array[TSearchParamsGroup] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeCOMPOSITE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsGroup : Array[TSearchParamsGroup] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsGroup : Array[TSearchParamsGroup] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsGroup : Array[TSearchParamsGroup] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtMedication, frtDevice, frtPatient, frtPractitioner, frtSubstance], [], []);
  TYPES_TSearchParamsGuidanceResponse : Array[TSearchParamsGuidanceResponse] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING);
  PATHS_TSearchParamsGuidanceResponse : Array[TSearchParamsGuidanceResponse] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsGuidanceResponse : Array[TSearchParamsGuidanceResponse] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsGuidanceResponse : Array[TSearchParamsGuidanceResponse] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], []);
  TYPES_TSearchParamsHealthcareService : Array[TSearchParamsHealthcareService] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsHealthcareService : Array[TSearchParamsHealthcareService] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsHealthcareService : Array[TSearchParamsHealthcareService] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsHealthcareService : Array[TSearchParamsHealthcareService] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtLocation], [], [frtOrganization], [], [], []);
  TYPES_TSearchParamsImagingExcerpt : Array[TSearchParamsImagingExcerpt] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeREFERENCE,  SearchParamTypeURI,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsImagingExcerpt : Array[TSearchParamsImagingExcerpt] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsImagingExcerpt : Array[TSearchParamsImagingExcerpt] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsImagingExcerpt : Array[TSearchParamsImagingExcerpt] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], [], [], [frtPatient], [], []);
  TYPES_TSearchParamsImagingObjectSelection : Array[TSearchParamsImagingObjectSelection] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeREFERENCE,  SearchParamTypeURI,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsImagingObjectSelection : Array[TSearchParamsImagingObjectSelection] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsImagingObjectSelection : Array[TSearchParamsImagingObjectSelection] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsImagingObjectSelection : Array[TSearchParamsImagingObjectSelection] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], [], [], [frtPatient], [], 
      []);
  TYPES_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeURI,  SearchParamTypeDATE,  SearchParamTypeURI, 
       SearchParamTypeURI);
  PATHS_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [frtDiagnosticOrder], [frtPatient], [], [], [], []);
  TYPES_TSearchParamsImmunization : Array[TSearchParamsImmunization] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeNUMBER,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE, 
       SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsImmunization : Array[TSearchParamsImmunization] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsImmunization : Array[TSearchParamsImmunization] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsImmunization : Array[TSearchParamsImmunization] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [frtLocation], [], [frtOrganization], [], [frtPatient], [frtPractitioner], [frtObservation], [], [], [], [frtPractitioner], 
      [], []);
  TYPES_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeNUMBER,  SearchParamTypeNUMBER,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [frtObservation, frtAllergyIntolerance], [frtPatient], [], [frtImmunization], 
      []);
  TYPES_TSearchParamsImplementationGuide : Array[TSearchParamsImplementationGuide] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeURI, 
       SearchParamTypeTOKEN);
  PATHS_TSearchParamsImplementationGuide : Array[TSearchParamsImplementationGuide] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsImplementationGuide : Array[TSearchParamsImplementationGuide] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsImplementationGuide : Array[TSearchParamsImplementationGuide] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal,
         frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder],
      
    [], 
      
    [], 
      
    []);
  TYPES_TSearchParamsLibrary : Array[TSearchParamsLibrary] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING);
  PATHS_TSearchParamsLibrary : Array[TSearchParamsLibrary] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsLibrary : Array[TSearchParamsLibrary] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsLibrary : Array[TSearchParamsLibrary] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsLinkage : Array[TSearchParamsLinkage] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsLinkage : Array[TSearchParamsLinkage] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsLinkage : Array[TSearchParamsLinkage] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsLinkage : Array[TSearchParamsLinkage] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtOrganization, frtPractitioner], [], []);
  TYPES_TSearchParamsList : Array[TSearchParamsList] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE, 
       SearchParamTypeSTRING);
  PATHS_TSearchParamsList : Array[TSearchParamsList] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsList : Array[TSearchParamsList] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsList : Array[TSearchParamsList] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [frtEncounter], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    [], 
      
    [frtPatient], 
      
    [frtDevice, frtPatient, frtPractitioner], 
      
    [], 
      
    [frtDevice, frtPatient, frtLocation, frtGroup], 
      
    []);
  TYPES_TSearchParamsLocation : Array[TSearchParamsLocation] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsLocation : Array[TSearchParamsLocation] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsLocation : Array[TSearchParamsLocation] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNearby, SearchXpathUsageDistance, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsLocation : Array[TSearchParamsLocation] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtOrganization], [frtLocation], [], []);
  TYPES_TSearchParamsMeasure : Array[TSearchParamsMeasure] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING);
  PATHS_TSearchParamsMeasure : Array[TSearchParamsMeasure] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsMeasure : Array[TSearchParamsMeasure] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsMeasure : Array[TSearchParamsMeasure] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsMeasureReport : Array[TSearchParamsMeasureReport] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsMeasureReport : Array[TSearchParamsMeasureReport] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsMeasureReport : Array[TSearchParamsMeasureReport] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsMeasureReport : Array[TSearchParamsMeasureReport] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtPatient]);
  TYPES_TSearchParamsMedia : Array[TSearchParamsMedia] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsMedia : Array[TSearchParamsMedia] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsMedia : Array[TSearchParamsMedia] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsMedia : Array[TSearchParamsMedia] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtPractitioner], [frtPatient], [frtSpecimen, frtDevice, frtPatient, frtPractitioner, frtGroup], [], [], []);
  TYPES_TSearchParamsMedication : Array[TSearchParamsMedication] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsMedication : Array[TSearchParamsMedication] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsMedication : Array[TSearchParamsMedication] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsMedication : Array[TSearchParamsMedication] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [frtMedication, frtSubstance], [], [frtOrganization], [frtMedication], []);
  TYPES_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtDevice], [], [frtEncounter], [], [frtMedication], [frtPatient], [frtPatient, frtPractitioner, frtRelatedPerson], 
      [frtMedicationOrder], [], []);
  TYPES_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeDATE);
  PATHS_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtLocation], [frtPractitioner], [], [frtMedication], [frtPatient], [frtMedicationOrder], [frtPatient, frtPractitioner], 
      [frtPractitioner], [], [], [], []);
  TYPES_TSearchParamsMedicationOrder : Array[TSearchParamsMedicationOrder] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsMedicationOrder : Array[TSearchParamsMedicationOrder] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsMedicationOrder : Array[TSearchParamsMedicationOrder] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsMedicationOrder : Array[TSearchParamsMedicationOrder] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtEncounter], [], [frtMedication], [frtPatient], [frtPractitioner], []);
  TYPES_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [frtMedication], [frtPatient], [frtPatient, frtPractitioner, frtRelatedPerson], []);
  TYPES_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeURI,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE, 
       SearchParamTypeSTRING,  SearchParamTypeURI,  SearchParamTypeREFERENCE,  SearchParamTypeDATE);
  PATHS_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtPractitioner], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    [], 
      
    [], 
      
    [frtPractitioner], 
      
    [], 
      
    [frtOrganization, frtPractitioner], 
      
    [], 
      
    [frtOrganization, frtPractitioner], 
      
    [], 
      
    [], 
      
    [frtDevice], 
      
    []);
  TYPES_TSearchParamsModuleDefinition : Array[TSearchParamsModuleDefinition] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING);
  PATHS_TSearchParamsModuleDefinition : Array[TSearchParamsModuleDefinition] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsModuleDefinition : Array[TSearchParamsModuleDefinition] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsModuleDefinition : Array[TSearchParamsModuleDefinition] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], []);
  TYPES_TSearchParamsNamingSystem : Array[TSearchParamsNamingSystem] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING);
  PATHS_TSearchParamsNamingSystem : Array[TSearchParamsNamingSystem] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsNamingSystem : Array[TSearchParamsNamingSystem] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsNamingSystem : Array[TSearchParamsNamingSystem] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtNamingSystem], [], [], [], [], []);
  TYPES_TSearchParamsNutritionOrder : Array[TSearchParamsNutritionOrder] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsNutritionOrder : Array[TSearchParamsNutritionOrder] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsNutritionOrder : Array[TSearchParamsNutritionOrder] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsNutritionOrder : Array[TSearchParamsNutritionOrder] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtEncounter], [], [], [], [frtPatient], [frtPractitioner], [], []);
  TYPES_TSearchParamsObservation : Array[TSearchParamsObservation] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeCOMPOSITE,  SearchParamTypeTOKEN,  SearchParamTypeCOMPOSITE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeQUANTITY,  SearchParamTypeSTRING,  SearchParamTypeTOKEN, 
       SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeCOMPOSITE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeQUANTITY,  SearchParamTypeSTRING);
  PATHS_TSearchParamsObservation : Array[TSearchParamsObservation] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsObservation : Array[TSearchParamsObservation] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsObservation : Array[TSearchParamsObservation] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtDevice, frtDeviceMetric], [frtEncounter], [], [frtPatient], [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], 
      [], [frtObservation, frtQuestionnaireResponse], [], [frtSpecimen], [], [frtDevice, frtPatient, frtLocation, frtGroup], [], [], [], []);
  TYPES_TSearchParamsOperationDefinition : Array[TSearchParamsOperationDefinition] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeURI,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsOperationDefinition : Array[TSearchParamsOperationDefinition] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsOperationDefinition : Array[TSearchParamsOperationDefinition] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsOperationDefinition : Array[TSearchParamsOperationDefinition] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtOperationDefinition], [], [], [], [], [], [], [frtStructureDefinition], [], [], [], [], [], []);
  TYPES_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING);
  PATHS_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], []);
  TYPES_TSearchParamsOrder : Array[TSearchParamsOrder] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsOrder : Array[TSearchParamsOrder] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsOrder : Array[TSearchParamsOrder] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsOrder : Array[TSearchParamsOrder] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    [], 
      
    [frtPatient], 
      
    [frtOrganization, frtPractitioner], 
      
    [frtDevice, frtPatient, frtSubstance, frtGroup], 
      
    [frtDevice, frtOrganization, frtPractitioner], 
      
    [], 
      
    []);
  TYPES_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    [], 
      
    [frtOrder], 
      
    [frtDevice, frtOrganization, frtPractitioner]);
  TYPES_TSearchParamsOrderSet : Array[TSearchParamsOrderSet] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING);
  PATHS_TSearchParamsOrderSet : Array[TSearchParamsOrderSet] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsOrderSet : Array[TSearchParamsOrderSet] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsOrderSet : Array[TSearchParamsOrderSet] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsOrganization : Array[TSearchParamsOrganization] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsOrganization : Array[TSearchParamsOrganization] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsOrganization : Array[TSearchParamsOrganization] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsagePhonetic, SearchXpathUsageNormal);
  TARGETS_TSearchParamsOrganization : Array[TSearchParamsOrganization] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtOrganization], [], []);
  TYPES_TSearchParamsPatient : Array[TSearchParamsPatient] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE, 
       SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING, 
       SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsPatient : Array[TSearchParamsPatient] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsPatient : Array[TSearchParamsPatient] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsagePhonetic, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsPatient : Array[TSearchParamsPatient] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtOrganization, frtPractitioner], [], [], [], [], [], [], [], [], [], [frtPatient], [], [frtOrganization], 
      [], [], [], []);
  TYPES_TSearchParamsPaymentNotice : Array[TSearchParamsPaymentNotice] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN, 
       SearchParamTypeREFERENCE,  SearchParamTypeDATE);
  PATHS_TSearchParamsPaymentNotice : Array[TSearchParamsPaymentNotice] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsPaymentNotice : Array[TSearchParamsPaymentNotice] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsPaymentNotice : Array[TSearchParamsPaymentNotice] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [frtOrganization], [], [], [frtPractitioner], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal,
         frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder],
      
    [], 
      
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition,
    frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal,
         frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder],
      
    []);
  TYPES_TSearchParamsPaymentReconciliation : Array[TSearchParamsPaymentReconciliation] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsPaymentReconciliation : Array[TSearchParamsPaymentReconciliation] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsPaymentReconciliation : Array[TSearchParamsPaymentReconciliation] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsPaymentReconciliation : Array[TSearchParamsPaymentReconciliation] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [frtOrganization], [], [], [], [frtOrganization], [], [frtPractitioner], [frtProcessRequest]);
  TYPES_TSearchParamsPerson : Array[TSearchParamsPerson] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsPerson : Array[TSearchParamsPerson] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsPerson : Array[TSearchParamsPerson] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsagePhonetic, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsPerson : Array[TSearchParamsPerson] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtPatient, frtPractitioner, frtPerson, frtRelatedPerson], [], [frtOrganization], [frtPatient], 
      [], [], [frtPractitioner], [frtRelatedPerson], []);
  TYPES_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsagePhonetic, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtLocation], [], [frtOrganization], [], [], [], [], []);
  TYPES_TSearchParamsPractitionerRole : Array[TSearchParamsPractitionerRole] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsPractitionerRole : Array[TSearchParamsPractitionerRole] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsPractitionerRole : Array[TSearchParamsPractitionerRole] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsPractitionerRole : Array[TSearchParamsPractitionerRole] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtLocation], [frtOrganization], [], [frtPractitioner], [], [], []);
  TYPES_TSearchParamsProcedure : Array[TSearchParamsProcedure] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsProcedure : Array[TSearchParamsProcedure] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsProcedure : Array[TSearchParamsProcedure] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsProcedure : Array[TSearchParamsProcedure] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtEncounter], [], [frtLocation], [frtPatient], [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], [frtPatient, frtGroup]);
  TYPES_TSearchParamsProcedureRequest : Array[TSearchParamsProcedureRequest] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsProcedureRequest : Array[TSearchParamsProcedureRequest] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsProcedureRequest : Array[TSearchParamsProcedureRequest] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsProcedureRequest : Array[TSearchParamsProcedureRequest] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtEncounter], [], [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson], [frtPatient], [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], 
      [frtPatient, frtGroup]);
  TYPES_TSearchParamsProcessRequest : Array[TSearchParamsProcessRequest] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsProcessRequest : Array[TSearchParamsProcessRequest] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsProcessRequest : Array[TSearchParamsProcessRequest] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsProcessRequest : Array[TSearchParamsProcessRequest] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtOrganization], [], [frtPractitioner], []);
  TYPES_TSearchParamsProcessResponse : Array[TSearchParamsProcessResponse] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsProcessResponse : Array[TSearchParamsProcessResponse] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsProcessResponse : Array[TSearchParamsProcessResponse] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsProcessResponse : Array[TSearchParamsProcessResponse] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtOrganization], [], [], [frtOrganization], [], [frtPractitioner], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder]);
  TYPES_TSearchParamsProtocol : Array[TSearchParamsProtocol] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsProtocol : Array[TSearchParamsProtocol] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsProtocol : Array[TSearchParamsProtocol] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsProtocol : Array[TSearchParamsProtocol] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtCondition, frtMedication, frtDevice]);
  TYPES_TSearchParamsProvenance : Array[TSearchParamsProvenance] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsProvenance : Array[TSearchParamsProvenance] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsProvenance : Array[TSearchParamsProvenance] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsProvenance : Array[TSearchParamsProvenance] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtPatient, frtOrganization, frtPractitioner, frtRelatedPerson], [], [], [], [frtLocation], [frtPatient], [], [], 
      
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    []);
  TYPES_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING);
  PATHS_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsQuestionnaireResponse : Array[TSearchParamsQuestionnaireResponse] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsQuestionnaireResponse : Array[TSearchParamsQuestionnaireResponse] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsQuestionnaireResponse : Array[TSearchParamsQuestionnaireResponse] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsQuestionnaireResponse : Array[TSearchParamsQuestionnaireResponse] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson], [], [frtEncounter], [frtPatient], [frtQuestionnaire], 
      [frtPatient, frtPractitioner, frtRelatedPerson], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder]);
  TYPES_TSearchParamsReferralRequest : Array[TSearchParamsReferralRequest] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN, 
       SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsReferralRequest : Array[TSearchParamsReferralRequest] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsReferralRequest : Array[TSearchParamsReferralRequest] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsReferralRequest : Array[TSearchParamsReferralRequest] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtCarePlan, frtReferralRequest, frtProcedureRequest, frtDiagnosticOrder], [], [frtEncounter, frtEpisodeOfCare], [], 
      [], [frtPatient], [], [frtOrganization, frtPractitioner], [frtOrganization, frtPatient, frtPractitioner], [], [], []);
  TYPES_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsagePhonetic, SearchXpathUsageNormal);
  TARGETS_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtPatient], [], [], []);
  TYPES_TSearchParamsRiskAssessment : Array[TSearchParamsRiskAssessment] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsRiskAssessment : Array[TSearchParamsRiskAssessment] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsRiskAssessment : Array[TSearchParamsRiskAssessment] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsRiskAssessment : Array[TSearchParamsRiskAssessment] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtCondition], [], [frtEncounter], [], [], [frtPatient], [frtDevice, frtPractitioner], [frtPatient, frtGroup]);
  TYPES_TSearchParamsSchedule : Array[TSearchParamsSchedule] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeREFERENCE,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsSchedule : Array[TSearchParamsSchedule] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsSchedule : Array[TSearchParamsSchedule] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsSchedule : Array[TSearchParamsSchedule] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtDevice, frtPatient, frtHealthcareService, frtLocation, frtPractitioner, frtRelatedPerson], [], [], []);
  TYPES_TSearchParamsSearchParameter : Array[TSearchParamsSearchParameter] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeURI);
  PATHS_TSearchParamsSearchParameter : Array[TSearchParamsSearchParameter] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsSearchParameter : Array[TSearchParamsSearchParameter] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsSearchParameter : Array[TSearchParamsSearchParameter] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsSequence : Array[TSearchParamsSequence] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeCOMPOSITE,  SearchParamTypeNUMBER,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeNUMBER,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsSequence : Array[TSearchParamsSequence] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsSequence : Array[TSearchParamsSequence] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsSequence : Array[TSearchParamsSequence] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [frtPatient], [], [], []);
  TYPES_TSearchParamsSlot : Array[TSearchParamsSlot] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsSlot : Array[TSearchParamsSlot] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsSlot : Array[TSearchParamsSlot] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsSlot : Array[TSearchParamsSlot] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtSchedule], [], [], []);
  TYPES_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN);
  PATHS_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [frtPractitioner], [], [], [], [frtSpecimen], [frtPatient], [frtDevice, frtPatient, frtSubstance, frtGroup], []);
  TYPES_TSearchParamsStructureDefinition : Array[TSearchParamsStructureDefinition] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING, 
       SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeURI,  SearchParamTypeREFERENCE, 
       SearchParamTypeTOKEN);
  PATHS_TSearchParamsStructureDefinition : Array[TSearchParamsStructureDefinition] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsStructureDefinition : Array[TSearchParamsStructureDefinition] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsStructureDefinition : Array[TSearchParamsStructureDefinition] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtValueSet], []);
  TYPES_TSearchParamsStructureMap : Array[TSearchParamsStructureMap] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeURI,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsStructureMap : Array[TSearchParamsStructureMap] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsStructureMap : Array[TSearchParamsStructureMap] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsStructureMap : Array[TSearchParamsStructureMap] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsSubscription : Array[TSearchParamsSubscription] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeURI);
  PATHS_TSearchParamsSubscription : Array[TSearchParamsSubscription] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsSubscription : Array[TSearchParamsSubscription] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsSubscription : Array[TSearchParamsSubscription] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsSubstance : Array[TSearchParamsSubstance] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeQUANTITY,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsSubstance : Array[TSearchParamsSubstance] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsSubstance : Array[TSearchParamsSubstance] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsSubstance : Array[TSearchParamsSubstance] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [frtSubstance]);
  TYPES_TSearchParamsSupplyDelivery : Array[TSearchParamsSupplyDelivery] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsSupplyDelivery : Array[TSearchParamsSupplyDelivery] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsSupplyDelivery : Array[TSearchParamsSupplyDelivery] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsSupplyDelivery : Array[TSearchParamsSupplyDelivery] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtPatient], [frtPractitioner], [], [frtPractitioner]);
  TYPES_TSearchParamsSupplyRequest : Array[TSearchParamsSupplyRequest] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsSupplyRequest : Array[TSearchParamsSupplyRequest] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsSupplyRequest : Array[TSearchParamsSupplyRequest] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsSupplyRequest : Array[TSearchParamsSupplyRequest] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [frtPatient], [frtOrganization, frtPatient, frtPractitioner], [], [frtOrganization]);
  TYPES_TSearchParamsTask : Array[TSearchParamsTask] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeSTRING, 
       SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeREFERENCE,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsTask : Array[TSearchParamsTask] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsTask : Array[TSearchParamsTask] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsTask : Array[TSearchParamsTask] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], [], [], [], [], [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], 
      [frtTask], [], [], [], 
    [frtCondition, frtDeviceComponent, frtCommunication, frtPractitionerRole, frtGroup, frtValueSet, frtCoverage, frtAppointment, frtLibrary, frtSlot, frtDecisionSupportRule, frtEpisodeOfCare, frtComposition, frtConformance, frtNamingSystem, frtHealthcareService, frtLinkage, frtOrderResponse, frtTask, frtConceptMap, frtPractitioner, frtCarePlan, frtSubstance, frtDeviceUseRequest, frtQuestionnaireResponse, frtSupplyDelivery, frtSchedule, frtEligibilityRequest, frtPaymentReconciliation, frtTestScript, frtImagingObjectSelection, frtOperationDefinition, frtClaimResponse, frtFlag, frtBodySite, frtCommunicationRequest, frtRiskAssessment, frtClaim, frtExpansionProfile, frtExplanationOfBenefit, frtAllergyIntolerance, frtObservation, frtRelatedPerson, frtProcessResponse, frtAuditEvent, frtEligibilityResponse, frtMedicationOrder, frtPerson, frtModuleDefinition, frtProcedureRequest,
         frtDeviceMetric, frtOrganization, frtMeasure, frtProcessRequest, frtImmunizationRecommendation, frtMedicationDispense, frtDetectedIssue, frtPaymentNotice, frtAppointmentResponse, frtMedicationStatement, frtSequence, frtImplementationGuide, frtProtocol, frtQuestionnaire, frtOperationOutcome, frtFamilyMemberHistory, frtDecisionSupportServiceModule, frtImagingExcerpt, frtMedia, frtBinary, frtVisionPrescription, frtDocumentReference, frtCareTeam, frtImmunization, frtBundle, frtSubscription, frtMeasureReport, frtImagingStudy, frtProvenance, frtDevice, frtStructureDefinition, frtAccount, frtOrder, frtProcedure, frtOrderSet, frtDiagnosticReport, frtMedication, frtMessageHeader, frtDocumentManifest, frtDataElement, frtStructureMap, frtMedicationAdministration, frtEncounter, frtCompartmentDefinition, frtCodeSystem, frtList, frtDeviceUseStatement, frtGoal, frtGuidanceResponse, frtSearchParameter, frtNutritionOrder, frtClinicalImpression, frtReferralRequest, frtEnrollmentRequest, frtLocation, frtContract, frtBasic, frtSpecimen, frtEnrollmentResponse, frtSupplyRequest, frtPatient, frtDiagnosticOrder], 
      
    []);
  TYPES_TSearchParamsTestScript : Array[TSearchParamsTestScript] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeURI);
  PATHS_TSearchParamsTestScript : Array[TSearchParamsTestScript] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsTestScript : Array[TSearchParamsTestScript] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsTestScript : Array[TSearchParamsTestScript] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsValueSet : Array[TSearchParamsValueSet] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeSTRING,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeSTRING,  SearchParamTypeSTRING,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeURI,  SearchParamTypeTOKEN);
  PATHS_TSearchParamsValueSet : Array[TSearchParamsValueSet] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsValueSet : Array[TSearchParamsValueSet] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsValueSet : Array[TSearchParamsValueSet] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []);
  TYPES_TSearchParamsVisionPrescription : Array[TSearchParamsVisionPrescription] of TFhirSearchParamTypeEnum = ( SearchParamTypeSTRING,  SearchParamTypeTOKEN,  SearchParamTypeDATE,  SearchParamTypeURI,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN,  SearchParamTypeTOKEN, 
       SearchParamTypeSTRING,  SearchParamTypeDATE,  SearchParamTypeREFERENCE,  SearchParamTypeTOKEN,  SearchParamTypeREFERENCE,  SearchParamTypeREFERENCE);
  PATHS_TSearchParamsVisionPrescription : Array[TSearchParamsVisionPrescription] of String = ('',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '',
     '');
  USES_TSearchParamsVisionPrescription : Array[TSearchParamsVisionPrescription] of TFhirSearchXpathUsageEnum = (SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal, SearchXpathUsageNormal);
  TARGETS_TSearchParamsVisionPrescription : Array[TSearchParamsVisionPrescription] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtEncounter], [], [frtPatient], [frtPractitioner]);


implementation

end.

