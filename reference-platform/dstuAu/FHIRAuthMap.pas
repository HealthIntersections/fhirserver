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
    tcFinancial , // frtAccount - frtAccount
    tcOther, // ActivityDefinition
    tcClinical , // frtAllergyIntolerance - frtAllergyIntolerance
    tcSchedule , // frtAppointment - frtAppointment
    tcSchedule , // frtAppointmentResponse - frtAppointmentResponse
    tcAudit , // frtAuditEvent - frtAuditEvent
    tcOther , // frtBasic - frtBasic
    tcDocuments , // frtBinary - frtBinary
    tcOther , // frtBodySite - frtBodySite
    tcOther , // frtBundle - frtBundle
    tcClinical , // frtCarePlan - frtCarePlan
    tcClinical , // frtCareTeam - frtCareTeam
    tcFinancial , // frtClaim - frtClaim
    tcFinancial , // frtClaimResponse - frtClaimResponse
    tcClinical , // frtClinicalImpression - frtClinicalImpression
    tcOther , // frtCodeSystem - frtCodeSystem
    tcOther , // frtCommunication - frtCommunication
    tcOther , // frtCommunicationRequest - frtCommunicationRequest
    tcOther , // frtCompartmentDefinition - frtCompartmentDefinition
    tcDocuments , // frtComposition - frtComposition
    tcOther , // frtConceptMap - frtConceptMap
    tcClinical , // frtCondition - frtCondition
    tcOther , // frtConformance - frtConformance
    tcOther, // frtConsent
    tcFinancial , // frtContract - frtContract
    tcFinancial , // frtCoverage - frtCoverage
    tcOther , // frtDataElement - frtDataElement
    tcOther , // frtDecisionSupportRule - frtDecisionSupportRule
    tcOther , // frtDecisionSupportServiceModule - frtDecisionSupportServiceModule
    tcClinical,  // frtDetectedIssue - frtDetectedIssue
    tcOther , // frtDevice - frtDevice
    tcOther , // frtDeviceComponent - frtDeviceComponent
    tcOther , // frtDeviceMetric - frtDeviceMetric
    tcOther , // frtDeviceUseRequest - frtDeviceUseRequest
    tcClinical, // frtDeviceUseStatement - frtDeviceUseStatement
    tcOther , // frtDiagnosticOrder - frtDiagnosticOrder
    tcClinical, // frtDiagnosticReport - frtDiagnosticReport
    tcDocuments , // frtDocumentManifest - frtDocumentManifest
    tcDocuments, // frtDocumentReference - frtDocumentReference
    tcFinancial , // frtEligibilityRequest - frtEligibilityRequest
    tcFinancial, // frtEligibilityResponse - frtEligibilityResponse
    tcSchedule , // frtEncounter - frtEncounter
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
    tcData, // frtImagingManifest
    tcData , // frtImagingStudy
    tcMeds , // frtImmunization
    tcMeds , // frtImmunizationRecommendation
    tcOther , // frtImplemnetationGuide
    tcOther, // frtLibrary
    tcClinical , // frtLinkage
    tcOther , // frtList
    tcOther , // frtLocation
    tcOther, // frtMeasure
    tcOther , // frtMedia
    tcOther , // frtMedia
    tcMeds , // frtMedication
    tcMeds , // frtMedicationAdministration
    tcMeds , // frtMedicationDispense
    tcMeds , // frtMedicationPrescription
    tcMeds , // frtMedicationStatement
    tcOther , // frtMessageHeader
    tcData, // frtModuleDefinition
    tcOther , // frtNamingSystem
    tcMeds , // frtNutritionOrder
    tcData , // frtObservation
    tcOther , // frtOperationDefinition
    tcOther , // frtOperationOutcome
    tcOther , // frtOrder
    tcOther , // frtOrderResponse
    tcData, // frtOrderSet
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
    tcOther , // frtProtocol,
    tcAudit , // frtProvenance
    tcOther , // frtQuestionnaire
    tcOther, // frtQuestionnaireResponse
    tcClinical , // frtReferralRequest,
    tcOther , // frtRelatedPerson,
    tcOther , // frtRiskAssessment,
    tcSchedule , // frtSchedule,
    tcOther , // frtSearchParameter,
    tcData, // frtSequence
    tcSchedule , // frtSlot,
    tcData , // frtSpecimen,
    tcOther , // frtStructureDefinition,
    tcOther , // frtStructureMap[,
    tcOther , // frtSubscription,
    tcOther , // frtSubstance,
    tcOther , // frtSupplyDelivery,
    tcOther , // frtSupplyRequest,
    tcOther , // frtTask,
    tcOther{$IFNDEF FHIR2CM} , // frtTestScript,
    tcOther , // frtValueSet,
    tcClinical,// frtVisionPrescription,
    tcOther {$ENDIF}); // frtCustom

implementation

end.