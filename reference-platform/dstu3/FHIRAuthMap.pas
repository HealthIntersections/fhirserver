unit FHIRAuthMap;

interface

uses
  FHIRResources;

type
    TTokenCategory = (tcClinical, tcData, tcMeds, tcSchedule, tcAudit, tcDocuments, tcFinancial, tcOther);

const
  CODES_TTokenCategory : array [TTokenCategory] of String = ('Clinical', 'Data', 'Meds', 'Schedule', 'Audit', 'Documents', 'Financial', 'Other');

// categories for web login
// tcClinical, tcData, tcMeds, tcSchedule, tcAudit, tcDocuments, tcFinancial, tcOther

  RESOURCE_CATEGORY : array [TFHIRResourceType] of TTokenCategory =
    (
    tcOther, // frtNull
    tcFinancial , // frtAccount Account
    tcOther, // ActivityDefinition ActivityDefinition
    tcOther, // ActivityGroup ActivityGroup
    tcClinical , // frtAllergyIntolerance AllergyIntolerance
    tcSchedule , // frtAppointment Appointment
    tcSchedule , // frtAppointmentResponse AppointmentResponse
    tcAudit , // frtAuditEvent AuditEvent
    tcOther , // frtBasic Basic
    tcDocuments , // frtBinary Binary
    tcOther , // frtBodySite BodySite
    tcOther , // frtBundle Bundle
    tcClinical , // frtCarePlan CarePlan
    tcClinical , // frtCareTeam CareTeam
    tcFinancial , // frtClaim Claim
    tcFinancial , // frtClaimResponse ClaimResponse
    tcClinical , // frtClinicalImpression ClinicalImpression
    tcOther , // frtCodeSystem CodeSystem
    tcOther , // frtCommunication Communication
    tcOther , // frtCommunicationRequest CommunicationRequest
    tcOther , // frtCompartmentDefinition CompartmentDefinition
    tcDocuments , // frtComposition Composition
    tcOther , // frtConceptMap ConceptMap
    tcClinical , // frtCondition Condition (aka Problem)
    tcOther , // frtConformance Conformance
    tcOther, // frtConsent
    tcFinancial , // frtContract Contract
    tcFinancial , // frtCoverage Coverage
    tcOther , // frtDataElement DataElement
    tcOther , // frtDecisionSupportRule DecisionSupportServiceModule
    tcClinical,  // frtDetectedIssue DetectedIssue
    tcOther , // frtDevice Device
    tcOther , // frtDeviceComponent DeviceComponent
    tcOther , // frtDeviceMetric DeviceMetric
    tcOther , // frtDeviceUseRequest DeviceUseRequest
    tcClinical, // frtDeviceUseStatement DeviceUseStatement
    tcClinical, // frtDiagnosticReport DiagnosticRequest
    tcClinical , // frtDiagnosticOrder DiagnosticReport
    tcDocuments , // frtDocumentManifest DocumentManifest
    tcDocuments, // frtDocumentReference DocumentReference
    tcFinancial , // frtEligibilityRequest EligibilityRequest
    tcFinancial, // frtEligibilityResponse
    tcSchedule , // frtEncounter Encounter
    tcOther, // frtEndpoint Endpoint
    tcFinancial, // frtEnrollmentRequest EnrollmentRequest
    tcFinancial , // frtEnrollmentResponse EnrollmentResponse
    tcSchedule, // frtEpisodeOfCare EpisodeOfCare
    tcOther, // frtExpansionProfile ExpansionProfile
    tcFinancial , // frtExplanationOfBenefit ExplanationOfBenefit
    tcClinical, // frtFamilyMemberHistory FamilyMemberHistory
    tcClinical , // frtFlag Flag
    tcClinical , // frtGoal Goal
    tcOther , // frtGroup Group
    tcClinical , // frtGuidanceResponse GuidanceResponse
    tcSchedule , // frtHealthcareService HealthcareService
    tcData , // frtImagingStudy ImagingManifest
    tcData , // frtImagingStudy ImagingStudy
    tcMeds , // frtImmunization Immunization
    tcMeds , // frtImmunizationRecommendation ImmunizationRecommendation
    tcOther , // frtImplemnetationGuide ImplementationGuide
    tcOther, // frtLibrary Library
    tcClinical , // frtLinkage Linkage
    tcOther , // frtList List
    tcOther , // frtLocation Location
    tcOther, // frtMeasure Measure
    tcOther , // frtMedia MeasureReport
    tcOther , // frtMedia Media
    tcMeds , // frtMedication Medication
    tcMeds , // frtMedicationAdministration MedicationAdministration
    tcMeds , // frtMedicationDispense MedicationDispense
    tcMeds , // frtMedicationPrescription MedicationOrder
    tcMeds , // frtMedicationStatement MedicationStatement
    tcOther , // frtMessageHeader MessageHeader
    tcOther , // frtNamingSystem NamingSystem
    tcMeds , // frtNutritionOrder NutritionRequest
    tcData , // frtObservation Observation
    tcOther , // frtOperationDefinition OperationDefinition
    tcOther , // frtOperationOutcome OperationOutcome
    tcOther , // frtOrganization Organization
    tcOther , // frtParameters
    tcSchedule , // frtPatient Patient
    tcFinancial , // frtPaymentNotice PaymentNotice
    tcFinancial , // frtPaymentReconciliation PaymentReconciliation
    tcOther , // frtPerson Person
    tcOther , // frtPlanDefinition PlanDefinition
    tcOther , // frtPractitioner Practitioner
    tcOther , // frtPractitionerRole PractitionerRole
    tcClinical , // frtProcedure Procedure
    tcOther , // frtProcedureRequest ProcedureRequest
    tcOther , // frtProcessRequest ProcessRequest
    tcOther , // frtProcessResponse ProcessResponse
    tcAudit , // frtProvenance Provenance
    tcOther , // frtQuestionnaire Questionnaire
    tcOther, // frtQuestionnaireResponse QuestionnaireResponse
    tcClinical , // frtReferralRequest, ReferralRequest
    tcOther , // frtRelatedPerson, RelatedPerson
    tcOther , // frtRiskAssessment, RiskAssessment
    tcSchedule , // frtSchedule, Schedule
    tcOther , // frtSearchParameter, SearchParameter
    tcData, // frtSequence Sequence
    tcOther , // frtSearchParameter,
    tcSchedule , // frtSlot,
    tcData , // frtSpecimen,
    tcOther , // frtStructureDefinition,
    tcOther , // frtStructureMap[,
    tcOther , // frtSubscription,
    tcOther , // frtSubstance,
    tcOther , // frtSupplyDelivery,
    tcOther , // frtSupplyRequest,
    tcOther , // frtTask,
    tcOther , // frtTestScript,
    tcOther , // frtValueSet,
    tcClinical,// frtVisionPrescription,
    tcOther); // frtCustom



implementation

end.