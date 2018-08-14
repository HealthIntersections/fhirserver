unit FHIR.R4.AuthMap;

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


interface

uses
  FHIR.Base.Common,
  FHIR.R4.Resources;

// categories for web login
// tcClinical, tcData, tcMeds, tcSchedule, tcAudit, tcDocuments, tcFinancial, tcOther

const
  RESOURCE_CATEGORY : array [TFHIRResourceType] of TTokenCategory =
    (
    tcOther , // frtNull,
    tcFinancial, // frtAccount | frtAccount
    tcOther, // frtActivityDefinition | frtActivityDefinition
    tcClinical, // frtAdverseEvent | frtAdverseEvent
    tcClinical, // frtAllergyIntolerance | frtAllergyIntolerance
    tcSchedule, // frtAppointment | frtAppointment
    tcSchedule, // frtAppointmentResponse | frtAppointmentResponse
    tcAudit, // frtAuditEvent | frtAuditEvent
    tcClinical, // frtBasic | frtBasic
    tcDocuments, // frtBinary | frtBinary
    tcMedicationDefinition, // frtBiologicallyDerivedProduct | frtBiologicallyDerivedProduct
    tcClinical, // frtBodyStructure | frtBodyStructure
    tcDocuments, // frtBundle | frtBundle
    tcOther, // frtCapabilityStatement | frtCapabilityStatement
    tcClinical, // frtCarePlan | frtCarePlan
    tcClinical, // frtCareTeam | frtCareTeam
    tcFinancial, // frtChargeItem | frtChargeItem
    tcFinancial, // frtChargeItemDefinition | frtChargeItemDefinition
    tcFinancial, // frtClaim | frtClaim
    tcFinancial, // frtClaimResponse | frtClaimResponse
    tcClinical, // frtClinicalImpression | frtClinicalImpression
    tcOther, // frtCodeSystem | frtCodeSystem
    tcDocuments, // frtCommunication | frtCommunication
    tcDocuments, // frtCommunicationRequest | frtCommunicationRequest
    tcOther, // frtCompartmentDefinition | frtCompartmentDefinition
    tcDocuments, // frtComposition | frtComposition
    tcOther, // frtConceptMap | frtConceptMap
    tcClinical, // frtCondition | frtCondition
    tcData, // frtConsent | frtConsent
    tcDocuments, // frtContract | frtContract
    tcFinancial, // frtCoverage | frtCoverage
    tcClinical, // frtDetectedIssue | frtDetectedIssue
    tcData, // frtDevice | frtDevice
    tcData, // frtDeviceComponent | frtDeviceComponent
    tcData, // frtDeviceMetric | frtDeviceMetric
    tcClinical, // frtDeviceRequest | frtDeviceRequest
    tcClinical, // frtDeviceUseStatement | frtDeviceUseStatement
    tcClinical, // frtDiagnosticReport | frtDiagnosticReport
    tcDocuments, // frtDocumentManifest | frtDocumentManifest
    tcDocuments, // frtDocumentReference | frtDocumentReference
    tcFinancial, // frtEligibilityRequest | frtEligibilityRequest
    tcFinancial, // frtEligibilityResponse | frtEligibilityResponse
    tcSchedule, // frtEncounter | frtEncounter
    tcData, // frtEndpoint | frtEndpoint
    tcFinancial, // frtEnrollmentRequest | frtEnrollmentRequest
    tcFinancial, // frtEnrollmentResponse | frtEnrollmentResponse
    tcOther, // frtEntryDefinition | frtEntryDefinition
    tcSchedule, // frtEpisodeOfCare | frtEpisodeOfCare
    tcOther, // frtEventDefinition | frtEventDefinition
    tcOther, // frtExampleScenario | frtExampleScenario
    tcFinancial, // frtExplanationOfBenefit | frtExplanationOfBenefit
    tcClinical, // frtFamilyMemberHistory | frtFamilyMemberHistory
    tcClinical, // frtFlag | frtFlag
    tcClinical, // frtGoal | frtGoal
    tcOther, // frtGraphDefinition | frtGraphDefinition
    tcData, // frtGroup | frtGroup
    tcClinical, // frtGuidanceResponse | frtGuidanceResponse
    tcData, // frtHealthcareService | frtHealthcareService
    tcClinical, // frtImagingStudy | frtImagingStudy
    tcClinical, // frtImmunization | frtImmunization
    tcClinical, // frtImmunizationEvaluation | frtImmunizationEvaluation
    tcClinical, // frtImmunizationRecommendation | frtImmunizationRecommendation
    tcOther, // frtImplementationGuide | frtImplementationGuide
    tcFinancial, // frtInvoice | frtInvoice
    tcMedicationDefinition, // frtItemInstance | frtItemInstance
    tcOther, // frtLibrary | frtLibrary
    tcData, // frtLinkage | frtLinkage
    tcDocuments, // frtList | frtList
    tcData, // frtLocation | frtLocation
    tcOther, // frtMeasure | frtMeasure
    tcData, // frtMeasureReport | frtMeasureReport
    tcDocuments, // frtMedia | frtMedia
    tcMeds, // frtMedication | frtMedication
    tcMeds, // frtMedicationAdministration | frtMedicationAdministration
    tcMeds, // frtMedicationDispense | frtMedicationDispense
    tcMedicationDefinition, // frtMedicinalKnowledge9 | frtMedicationKnowledge
    tcMeds, // frtMedicationRequest | frtMedicationRequest
    tcMeds, // frtMedicationStatement | frtMedicationStatement
    tcMedicationDefinition, // frtMedicinalProduct | frtMedicinalProduct
    tcMedicationDefinition, // frtMedicinalProductAuthorization
    tcMedicationDefinition, // frtMedicinalProductClinicals
    tcMedicationDefinition, // frtMedicinalProductDeviceSpec
    tcMedicationDefinition, // frtMedicinalProductIngredient
    tcMedicationDefinition, // frtMedicinalProductManufactured
    tcMedicationDefinition, // frtMedicinalProductPackaged
    tcMedicationDefinition, //
    tcOther, // frtMessageDefinition
    tcData, // frtMessageHeader
    tcOther, // frtNamingSystem
    tcClinical, // frtNutritionOrder
    tcClinical, // frtObservation
    tcOther, // frtObservationDefinition
    tcOther, // frtOperationDefinition
    tcData, // frtOperationOutcome
    tcData, // frtOrganization
    tcData, // frtOrganizationRole
    tcData, // frtParameters
    tcData, // frtPatient
    tcFinancial, // frtPaymentNotice
    tcFinancial, // frtPaymentReconciliation
    tcData, // frtPerson
    tcOther, // frtPlanDefinition
    tcData, // frtPractitioner
    tcData, // frtPractitionerRole
    tcClinical, // frtProcedure
    tcFinancial, // frtProcessRequest
    tcFinancial, // frtProcessResponse
    tcMedicationDefinition, // frtProductPlan
    tcAudit, // frtProvenance
    tcOther, // frtQuestionnaire
    tcClinical, // frtQuestionnaireResponse
    tcData, // frtRelatedPerson
    tcOther, // frtRequestGroup
    tcData, // frtResearchStudy
    tcData, // frtResearchSubject
    tcClinical, // frtRiskAssessment
    tcSchedule, // frtSchedule
    tcOther, // frtSearchParameter
    tcClinical, // frtSequence
    tcOther, // frtServiceDefinition
//    tcClinical, // frtServiceRequest
    tcSchedule, // frtSlot
    tcClinical, // frtSpecimen
    tcOther, // frtSpecimenDefinition
    tcOther, // frtStructureDefinition
    tcOther, // frtStructureMap
    tcData, // frtSubscription
    tcData, // frtSubstance
    tcMedicationDefinition, // frtSubstancePolymer
    tcMedicationDefinition, // frtSubstanceReferenceInformation
    tcMedicationDefinition, // frtSubstanceSpecification
    tcData, // frtSupplyDelivery
    tcData, // frtSupplyRequest
    tcData, // frtTask
    tcOther, // frtTerminologyCapabilities
    tcOther, // frtTestReport
    tcOther, // frtTestScript
    tcData, // frtUserSession
    tcOther, // frtValueSet
    tcMedicationDefinition, // frtVerificationResult
    tcClinical, // frtVisionPrescription
    tcOther); // frtCustom);


    {$IFDEF FHIR_ACCOUNT}frtAccount, {$ENDIF}
    {$IFDEF FHIR_ACTIVITYDEFINITION}frtActivityDefinition, {$ENDIF}
    {$IFDEF FHIR_ADVERSEEVENT}frtAdverseEvent, {$ENDIF}
    {$IFDEF FHIR_ALLERGYINTOLERANCE}frtAllergyIntolerance, {$ENDIF}
    {$IFDEF FHIR_APPOINTMENT}frtAppointment, {$ENDIF}
    {$IFDEF FHIR_APPOINTMENTRESPONSE}frtAppointmentResponse, {$ENDIF}
    {$IFDEF FHIR_AUDITEVENT}frtAuditEvent, {$ENDIF}
    {$IFDEF FHIR_BASIC}frtBasic, {$ENDIF}
    {$IFDEF FHIR_BINARY}frtBinary, {$ENDIF}
    {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}frtBiologicallyDerivedProduct, {$ENDIF}
    {$IFDEF FHIR_BODYSTRUCTURE}frtBodyStructure, {$ENDIF}
    {$IFDEF FHIR_BUNDLE}frtBundle, {$ENDIF}
    {$IFDEF FHIR_CAPABILITYSTATEMENT}frtCapabilityStatement, {$ENDIF}
    {$IFDEF FHIR_CAREPLAN}frtCarePlan, {$ENDIF}
    {$IFDEF FHIR_CARETEAM}frtCareTeam, {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM}frtChargeItem, {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION}frtChargeItemDefinition, {$ENDIF}
    {$IFDEF FHIR_CLAIM}frtClaim, {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE}frtClaimResponse, {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION}frtClinicalImpression, {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM}frtCodeSystem, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION}frtCommunication, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST}frtCommunicationRequest, {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION}frtCompartmentDefinition, {$ENDIF}
    {$IFDEF FHIR_COMPOSITION}frtComposition, {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP}frtConceptMap, {$ENDIF}
    {$IFDEF FHIR_CONDITION}frtCondition, {$ENDIF}
    {$IFDEF FHIR_CONSENT}frtConsent, {$ENDIF}
    {$IFDEF FHIR_CONTRACT}frtContract, {$ENDIF}
    {$IFDEF FHIR_COVERAGE}frtCoverage, {$ENDIF}
    {$IFDEF FHIR_DETECTEDISSUE}frtDetectedIssue, {$ENDIF}
    {$IFDEF FHIR_DEVICE}frtDevice, {$ENDIF}
    {$IFDEF FHIR_DEVICECOMPONENT}frtDeviceComponent, {$ENDIF}
    {$IFDEF FHIR_DEVICEMETRIC}frtDeviceMetric, {$ENDIF}
    {$IFDEF FHIR_DEVICEREQUEST}frtDeviceRequest, {$ENDIF}
    {$IFDEF FHIR_DEVICEUSESTATEMENT}frtDeviceUseStatement, {$ENDIF}
    {$IFDEF FHIR_DIAGNOSTICREPORT}frtDiagnosticReport, {$ENDIF}
    {$IFDEF FHIR_DOCUMENTMANIFEST}frtDocumentManifest, {$ENDIF}
    {$IFDEF FHIR_DOCUMENTREFERENCE}frtDocumentReference, {$ENDIF}
    {$IFDEF FHIR_ELIGIBILITYREQUEST}frtEligibilityRequest, {$ENDIF}
    {$IFDEF FHIR_ELIGIBILITYRESPONSE}frtEligibilityResponse, {$ENDIF}
    {$IFDEF FHIR_ENCOUNTER}frtEncounter, {$ENDIF}
    {$IFDEF FHIR_ENDPOINT}frtEndpoint, {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTREQUEST}frtEnrollmentRequest, {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTRESPONSE}frtEnrollmentResponse, {$ENDIF}
    {$IFDEF FHIR_ENTRYDEFINITION}frtEntryDefinition, {$ENDIF}
    {$IFDEF FHIR_EPISODEOFCARE}frtEpisodeOfCare, {$ENDIF}
    {$IFDEF FHIR_EVENTDEFINITION}frtEventDefinition, {$ENDIF}
    {$IFDEF FHIR_EXAMPLESCENARIO}frtExampleScenario, {$ENDIF}
    {$IFDEF FHIR_EXPLANATIONOFBENEFIT}frtExplanationOfBenefit, {$ENDIF}
    {$IFDEF FHIR_FAMILYMEMBERHISTORY}frtFamilyMemberHistory, {$ENDIF}
    {$IFDEF FHIR_FLAG}frtFlag, {$ENDIF}
    {$IFDEF FHIR_GOAL}frtGoal, {$ENDIF}
    {$IFDEF FHIR_GRAPHDEFINITION}frtGraphDefinition, {$ENDIF}
    {$IFDEF FHIR_GROUP}frtGroup, {$ENDIF}
    {$IFDEF FHIR_GUIDANCERESPONSE}frtGuidanceResponse, {$ENDIF}
    {$IFDEF FHIR_HEALTHCARESERVICE}frtHealthcareService, {$ENDIF}
    {$IFDEF FHIR_IMAGINGSTUDY}frtImagingStudy, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATION}frtImmunization, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONEVALUATION}frtImmunizationEvaluation, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}frtImmunizationRecommendation, {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE}frtImplementationGuide, {$ENDIF}
    {$IFDEF FHIR_INVOICE}frtInvoice, {$ENDIF}
    {$IFDEF FHIR_ITEMINSTANCE}frtItemInstance, {$ENDIF}
    {$IFDEF FHIR_LIBRARY}frtLibrary, {$ENDIF}
    {$IFDEF FHIR_LINKAGE}frtLinkage, {$ENDIF}
    {$IFDEF FHIR_LIST}frtList, {$ENDIF}
    {$IFDEF FHIR_LOCATION}frtLocation, {$ENDIF}
    {$IFDEF FHIR_MEASURE}frtMeasure, {$ENDIF}
    {$IFDEF FHIR_MEASUREREPORT}frtMeasureReport, {$ENDIF}
    {$IFDEF FHIR_MEDIA}frtMedia, {$ENDIF}
    {$IFDEF FHIR_MEDICATION}frtMedication, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION}frtMedicationAdministration, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE}frtMedicationDispense, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE}frtMedicationKnowledge, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST}frtMedicationRequest, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONSTATEMENT}frtMedicationStatement, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCT}frtMedicinalProduct, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTAUTHORIZATION}frtMedicinalProductAuthorization, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTCLINICALS}frtMedicinalProductClinicals, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEVICESPEC}frtMedicinalProductDeviceSpec, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTINGREDIENT}frtMedicinalProductIngredient, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTMANUFACTURED}frtMedicinalProductManufactured, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTPACKAGED}frtMedicinalProductPackaged, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}frtMedicinalProductPharmaceutical, {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION}frtMessageDefinition, {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER}frtMessageHeader, {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM}frtNamingSystem, {$ENDIF}
    {$IFDEF FHIR_NUTRITIONORDER}frtNutritionOrder, {$ENDIF}
    {$IFDEF FHIR_OBSERVATION}frtObservation, {$ENDIF}
    {$IFDEF FHIR_OBSERVATIONDEFINITION}frtObservationDefinition, {$ENDIF}
    {$IFDEF FHIR_OPERATIONDEFINITION}frtOperationDefinition, {$ENDIF}
    {$IFDEF FHIR_OPERATIONOUTCOME}frtOperationOutcome, {$ENDIF}
    {$IFDEF FHIR_ORGANIZATION}frtOrganization, {$ENDIF}
    {$IFDEF FHIR_ORGANIZATIONROLE}frtOrganizationRole, {$ENDIF}
    {$IFDEF FHIR_PARAMETERS}frtParameters, {$ENDIF}
    {$IFDEF FHIR_PATIENT}frtPatient, {$ENDIF}
    {$IFDEF FHIR_PAYMENTNOTICE}frtPaymentNotice, {$ENDIF}
    {$IFDEF FHIR_PAYMENTRECONCILIATION}frtPaymentReconciliation, {$ENDIF}
    {$IFDEF FHIR_PERSON}frtPerson, {$ENDIF}
    {$IFDEF FHIR_PLANDEFINITION}frtPlanDefinition, {$ENDIF}
    {$IFDEF FHIR_PRACTITIONER}frtPractitioner, {$ENDIF}
    {$IFDEF FHIR_PRACTITIONERROLE}frtPractitionerRole, {$ENDIF}
    {$IFDEF FHIR_PROCEDURE}frtProcedure, {$ENDIF}
    {$IFDEF FHIR_PROCESSREQUEST}frtProcessRequest, {$ENDIF}
    {$IFDEF FHIR_PROCESSRESPONSE}frtProcessResponse, {$ENDIF}
    {$IFDEF FHIR_PRODUCTPLAN}frtProductPlan, {$ENDIF}
    {$IFDEF FHIR_PROVENANCE}frtProvenance, {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE}frtQuestionnaire, {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRERESPONSE}frtQuestionnaireResponse, {$ENDIF}
    {$IFDEF FHIR_RELATEDPERSON}frtRelatedPerson, {$ENDIF}
    {$IFDEF FHIR_REQUESTGROUP}frtRequestGroup, {$ENDIF}
    {$IFDEF FHIR_RESEARCHSTUDY}frtResearchStudy, {$ENDIF}
    {$IFDEF FHIR_RESEARCHSUBJECT}frtResearchSubject, {$ENDIF}
    {$IFDEF FHIR_RISKASSESSMENT}frtRiskAssessment, {$ENDIF}
    {$IFDEF FHIR_SCHEDULE}frtSchedule, {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER}frtSearchParameter, {$ENDIF}
    {$IFDEF FHIR_SEQUENCE}frtSequence, {$ENDIF}
    {$IFDEF FHIR_SERVICEREQUEST}frtServiceRequest, {$ENDIF}
    {$IFDEF FHIR_SLOT}frtSlot, {$ENDIF}
    {$IFDEF FHIR_SPECIMEN}frtSpecimen, {$ENDIF}
    {$IFDEF FHIR_SPECIMENDEFINITION}frtSpecimenDefinition, {$ENDIF}
    {$IFDEF FHIR_STRUCTUREDEFINITION}frtStructureDefinition, {$ENDIF}
    {$IFDEF FHIR_STRUCTUREMAP}frtStructureMap, {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTION}frtSubscription, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCE}frtSubstance, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPOLYMER}frtSubstancePolymer, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}frtSubstanceReferenceInformation, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCESPECIFICATION}frtSubstanceSpecification, {$ENDIF}
    {$IFDEF FHIR_SUPPLYDELIVERY}frtSupplyDelivery, {$ENDIF}
    {$IFDEF FHIR_SUPPLYREQUEST}frtSupplyRequest, {$ENDIF}
    {$IFDEF FHIR_TASK}frtTask, {$ENDIF}
    {$IFDEF FHIR_TERMINOLOGYCAPABILITIES}frtTerminologyCapabilities, {$ENDIF}
    {$IFDEF FHIR_TESTREPORT}frtTestReport, {$ENDIF}
    {$IFDEF FHIR_TESTSCRIPT}frtTestScript, {$ENDIF}
    {$IFDEF FHIR_USERSESSION}frtUserSession, {$ENDIF}
    {$IFDEF FHIR_VALUESET}frtValueSet, {$ENDIF}
    {$IFDEF FHIR_VERIFICATIONRESULT}frtVerificationResult, {$ENDIF}
    {$IFDEF FHIR_VISIONPRESCRIPTION}frtVisionPrescription, {$ENDIF}

implementation

end.