unit fhir3_authmap;

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

uses
  fhir_common,
  fhir3_resources;

// categories for web login
// tcClinical, tcData, tcMeds, tcSchedule, tcAudit, tcDocuments, tcFinancial, tcOther

const
  RESOURCE_CATEGORY : array [TFHIRResourceType] of TTokenCategory =
    (
    tcOther, // frtNull
    tcFinancial , // frtAccount
    tcOther, // frtActivityDefinition
    tcClinical , // frtAllergyIntolerance
    tcSchedule , // frtAppointment
    tcSchedule , // frtAppointmentResponse
    tcAudit , // frtAuditEvent
    tcOther , // frtBasic
    tcDocuments , // frtBinary
    tcOther , // frtBodySite
    tcOther , // frtBundle
    tcOther , // frtCapabilityStatement
    tcClinical , // frtCarePlan
    tcClinical , // frtCareTeam
    tcOther , // frtCareTeam
    tcFinancial , // frtClaim
    tcFinancial , // frtClaimResponse
    tcClinical , // frtClinicalImpression
    tcOther , // frtCodeSystem
    tcOther , // frtCommunication
    tcOther , // frtCommunicationRequest
    tcOther , // frtCompartmentDefinition
    tcDocuments , // frtComposition
    tcOther , // frtConceptMap
    tcClinical , // frtCondition
    tcOther, // frtConsent
    tcFinancial , // frtContract
    tcFinancial , // frtCoverage
    tcOther , // frtDataElement
    tcClinical,  // frtDetectedIssue
    tcOther , // frtDevice
    tcOther , // frtDeviceComponent
    tcOther , // frtDeviceMetric
    tcOther , // frtDeviceUseRequest
    tcClinical, // frtDeviceUseStatement
    tcClinical, // frtDiagnosticReport
    tcClinical , // frtDiagnosticRequest
    tcDocuments , // frtDocumentManifest
    tcDocuments, // frtDocumentReference
    tcFinancial , // frtEligibilityRequest
    tcFinancial, // frtEligibilityResponse
    tcSchedule , // frtEncounter
    tcOther, // frtEndpoint
    tcFinancial, // frtEnrollmentRequest
    tcFinancial , // frtEnrollmentResponse
    tcSchedule, // frtEpisodeOfCare
    tcOther, // frtExpansionProfile
    tcFinancial , // frtExplanationOfBenefit
    tcClinical, // frtFamilyMemberHistory
    tcClinical , // frtFlag
    tcClinical , // frtGoal
    tcOther, // frtGraphDefinition
    tcOther , // frtGroup
    tcClinical , // frtGuidanceResponse
    tcSchedule , // frtHealthcareService
    tcData , // frtImagingManifest
    tcData , // frtImagingStudy
    tcMeds , // frtImmunization
    tcMeds , // frtImmunizationRecommendation
    tcOther , // frtImplementationGuide
    tcOther, // frtLibrary
    tcClinical , // frtLinkage
    tcOther , // frtList
    tcOther , // frtLocation
    tcOther, // frtMeasure
    tcOther, // frtMeasureReport
    tcOther , // frtMedia
    tcMeds , // frtMedication
    tcMeds , // frtMedicationAdministration
    tcMeds , // frtMedicationDispense
    tcMeds , // frtMedicationRequest
    tcMeds , // frtMedicationStatement
    tcOther, // frtMessageDefinition
    tcOther , // frtMessageHeader
    tcOther , // frtNamingSystem
    tcMeds , // frtNutritionRequest
    tcData , // frtObservation
    tcOther , // frtOperationDefinition
    tcOther , // frtOperationOutcome
    tcOther , // frtOrganization
    tcOther , // frtParameters
    tcSchedule , // frtPatient
    tcFinancial , // frtPaymentNotice
    tcFinancial , // frtPaymentReconciliation
    tcOther , // frtPerson
    tcOther , // frtPlanDefinition
    tcOther , // frtPractitioner
    tcOther , // frtPractitionerRole
    tcClinical , // frtProcedure
    tcOther , // frtProcedureRequest
    tcOther , // frtProcessRequest
    tcOther , // frtProcessResponse
    tcAudit , // frtProvenance
    tcOther , // frtQuestionnaire
    tcOther, // frtQuestionnaireResponse
    tcClinical , // frtReferralRequest
    tcOther , // frtRelatedPerson
    tcOther , // frtRequestGroup
    tcOther , // frtResearchStudy
    tcOther , // frtResearchSubject
    tcOther , // frtRiskAssessment
    tcSchedule , // frtSchedule
    tcOther , // frtSearchParameter
    tcData, // frtSequence
    tcOther , // frtServiceDefinition
    tcSchedule , // frtSlot
    tcData , // frtSpecimen
    tcOther , // frtStructureDefinition
    tcOther , // frtStructureMap
    tcOther , // frtSubscription
    tcOther , // frtSubstance
    tcOther , // frtSupplyDelivery
    tcOther , // frtSupplyRequest
    tcOther , // frtTask
    tcOther , // frtTestReport
    tcOther , // frtTestScript
    tcOther , // frtValueSet
    tcClinical,// frtVisionPrescription
    tcOther); // frtCustom


implementation

end.
