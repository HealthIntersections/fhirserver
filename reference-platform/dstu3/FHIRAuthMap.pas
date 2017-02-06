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
    tcOther , // frtCatalog
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