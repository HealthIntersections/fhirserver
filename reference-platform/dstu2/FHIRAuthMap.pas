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