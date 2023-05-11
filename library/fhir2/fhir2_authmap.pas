unit fhir2_authmap;

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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

uses
  fhir_common,
  fhir2_resources_base, fhir2_resources_canonical, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_other;

// categories for web login
// tcClinical, tcData, tcMeds, tcSchedule, tcAudit, tcDocuments, tcFinancial, tcOther

const
  RESOURCE_CATEGORY : array [TFHIRResourceType] of TTokenCategory =
    (
    tcOther,
    tcFinancial , // frtAccount,
    tcClinical , // frtAllergyIntolerance,
    tcSchedule , // frtAppointment,
    tcSchedule , // frtAppointmentResponse,
    tcAudit , // frtAuditEvent,
    tcOther , // frtBasic,
    tcDocuments , // frtBinary,
    tcOther , // frtBodySite,
    tcOther , // frtBundle,
    tcClinical , // frtCarePlan,
    tcFinancial , // frtClaim,
    tcFinancial , // frtClaimResponse,
    tcClinical , // frtClinicalImpression,
    tcOther , // frtCommunication,
    tcOther , // frtCommunicationRequest,
    tcDocuments , // frtComposition,
    tcOther , // frtConceptMap,
    tcClinical , // frtCondition,
    tcOther , // frtConformance,
    tcFinancial , // frtContract,
    tcFinancial , // frtCoverage,
    tcOther , // frtDataElement,
    tcClinical,  // frtDetectedIssue,
    tcOther , // frtDevice,
    tcOther , // frtDeviceComponent,
    tcOther , // frtDeviceMetric,
    tcOther , // frtDeviceUseRequest,
    tcClinical, // frtDeviceUseStatement,
    tcOther , // frtDiagnosticOrder,
    tcClinical, // frtDiagnosticReport
    tcDocuments , // frtDocumentManifest,
    tcDocuments, // frtDocumentReference
    tcFinancial , // frtEligibilityRequest,
    tcFinancial, // frtEligibilityResponse
    tcSchedule , // frtEncounter,
    tcFinancial, // frtEnrollmentRequest
    tcFinancial , // frtEnrollmentResponse,
    tcSchedule, // frtEpisodeOfCare
    tcFinancial , // frtExplanationOfBenefit,
    tcClinical, // frtFamilyMemberHistory
    tcClinical , // frtFlag,
    tcClinical , // frtGoal,
    tcOther , // frtGroup,
    tcSchedule , // frtHealthcareService,
    tcData, // frtImagingObjectSelection
    tcData , // frtImagingStudy,
    tcMeds , // frtImmunization,
    tcMeds , // frtImmunizationRecommendation,
    tcOther , // frtImplemnetationGuide,
    tcOther , // frtList,
    tcOther , // frtLocation,
    tcOther , // frtMedia,
    tcMeds , // frtMedication,
    tcMeds , // frtMedicationAdministration,
    tcMeds , // frtMedicationDispense,
    tcMeds , // frtMedicationPrescription,
    tcMeds , // frtMedicationStatement,
    tcOther , // frtMessageHeader,
    tcOther , // frtNamingSystem,
    tcMeds , // frtNutritionOrder,
    tcData , // frtObservation,
    tcOther , // frtOperationDefinition,
    tcOther , // frtOperationOutcome,
    tcOther , // frtOrder,
    tcOther , // frtOrderResponse,
    tcOther , // frtOrganization,
    tcOther , // frtParameters,
    tcSchedule , // frtPatient,
    tcFinancial , // frtPaymentNotice,
    tcFinancial , // frtPaymentReconciliation,
    tcOther , // frtPerson,
    tcOther , // frtPractitioner,
    tcClinical , // frtProcedure,
    tcOther , // frtProcedureRequest,
    tcOther , // frtProcessRequest,
    tcOther , // frtProcessResponse,
    tcAudit , // frtProvenance,
    tcOther , // frtQuestionnaire,
    tcOther, // frtQuestionnaireResponse
    tcClinical , // frtReferralRequest,
    tcOther , // frtRelatedPerson,
    tcOther , // frtRiskAssessment,
    tcSchedule , // frtSchedule,
    tcOther , // frtSearchParameter,
    tcSchedule , // frtSlot,
    tcData , // frtSpecimen,
    tcOther , // frtStructureDefinition,
    tcOther , // frtSubscription,
    tcOther , // frtSubstance,
    tcOther , // frtSupplyDelivery,
    tcOther , // frtSupplyRequest,
    tcOther , // frtTestScript,
    tcOther , // frtValueSet,
    tcClinical, tcOther); // frtVisionPrescription);
implementation

end.
