unit fhir5_enums;

{$I fhir5.inc}

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

// Generated on Fri, Aug 21, 2020 11:27+1000 for FHIR v4.5.0

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_crypto, fsl_stream,
  fhir_objects, fhir_xhtml,  
  fhir5_base;

type

  // Indicates whether the account is available to be used. (from http://hl7.org/fhir/ValueSet/account-status)
  TFhirAccountStatusEnum = (
    AccountStatusNull, // Value is missing from Instance
    AccountStatusActive,
    AccountStatusInactive,
    AccountStatusEnteredInError,
    AccountStatusOnHold,
    AccountStatusUnknown);
  TFhirAccountStatusEnumList = set of TFhirAccountStatusEnum;

  // Defines behavior for an action or a group for how many times that item may be repeated. (from http://hl7.org/fhir/ValueSet/action-cardinality-behavior)
  TFhirActionCardinalityBehaviorEnum = (
    ActionCardinalityBehaviorNull, // Value is missing from Instance
    ActionCardinalityBehaviorSingle,
    ActionCardinalityBehaviorMultiple);
  TFhirActionCardinalityBehaviorEnumList = set of TFhirActionCardinalityBehaviorEnum;

  // Defines the kinds of conditions that can appear on actions. (from http://hl7.org/fhir/ValueSet/action-condition-kind)
  TFhirActionConditionKindEnum = (
    ActionConditionKindNull, // Value is missing from Instance
    ActionConditionKindApplicability,
    ActionConditionKindStart,
    ActionConditionKindStop);
  TFhirActionConditionKindEnumList = set of TFhirActionConditionKindEnum;

  // Defines organization behavior of a group. (from http://hl7.org/fhir/ValueSet/action-grouping-behavior)
  TFhirActionGroupingBehaviorEnum = (
    ActionGroupingBehaviorNull, // Value is missing from Instance
    ActionGroupingBehaviorVisualGroup,
    ActionGroupingBehaviorLogicalGroup,
    ActionGroupingBehaviorSentenceGroup);
  TFhirActionGroupingBehaviorEnumList = set of TFhirActionGroupingBehaviorEnum;

  // The type of participant for the action. (from http://hl7.org/fhir/ValueSet/action-participant-type)
  TFhirActionParticipantTypeEnum = (
    ActionParticipantTypeNull, // Value is missing from Instance
    ActionParticipantTypePatient,
    ActionParticipantTypePractitioner,
    ActionParticipantTypeRelatedPerson,
    ActionParticipantTypeDevice);
  TFhirActionParticipantTypeEnumList = set of TFhirActionParticipantTypeEnum;

  // Defines selection frequency behavior for an action or group. (from http://hl7.org/fhir/ValueSet/action-precheck-behavior)
  TFhirActionPrecheckBehaviorEnum = (
    ActionPrecheckBehaviorNull, // Value is missing from Instance
    ActionPrecheckBehaviorYes,
    ActionPrecheckBehaviorNo);
  TFhirActionPrecheckBehaviorEnumList = set of TFhirActionPrecheckBehaviorEnum;

  // Defines the types of relationships between actions. (from http://hl7.org/fhir/ValueSet/action-relationship-type)
  TFhirActionRelationshipTypeEnum = (
    ActionRelationshipTypeNull, // Value is missing from Instance
    ActionRelationshipTypeBeforeStart,
    ActionRelationshipTypeBefore,
    ActionRelationshipTypeBeforeEnd,
    ActionRelationshipTypeConcurrentWithStart,
    ActionRelationshipTypeConcurrent,
    ActionRelationshipTypeConcurrentWithEnd,
    ActionRelationshipTypeAfterStart,
    ActionRelationshipTypeAfter,
    ActionRelationshipTypeAfterEnd);
  TFhirActionRelationshipTypeEnumList = set of TFhirActionRelationshipTypeEnum;

  // Defines expectations around whether an action or action group is required. (from http://hl7.org/fhir/ValueSet/action-required-behavior)
  TFhirActionRequiredBehaviorEnum = (
    ActionRequiredBehaviorNull, // Value is missing from Instance
    ActionRequiredBehaviorMust,
    ActionRequiredBehaviorCould,
    ActionRequiredBehaviorMustUnlessDocumented);
  TFhirActionRequiredBehaviorEnumList = set of TFhirActionRequiredBehaviorEnum;

  // Defines selection behavior of a group. (from http://hl7.org/fhir/ValueSet/action-selection-behavior)
  TFhirActionSelectionBehaviorEnum = (
    ActionSelectionBehaviorNull, // Value is missing from Instance
    ActionSelectionBehaviorAny,
    ActionSelectionBehaviorAll,
    ActionSelectionBehaviorAllOrNone,
    ActionSelectionBehaviorExactlyOne,
    ActionSelectionBehaviorAtMostOne,
    ActionSelectionBehaviorOneOrMore);
  TFhirActionSelectionBehaviorEnumList = set of TFhirActionSelectionBehaviorEnum;

  // The type of an address (physical / postal). (from http://hl7.org/fhir/ValueSet/address-type)
  TFhirAddressTypeEnum = (
    AddressTypeNull, // Value is missing from Instance
    AddressTypePostal,
    AddressTypePhysical,
    AddressTypeBoth);
  TFhirAddressTypeEnumList = set of TFhirAddressTypeEnum;

  // The use of an address. (from http://hl7.org/fhir/ValueSet/address-use)
  TFhirAddressUseEnum = (
    AddressUseNull, // Value is missing from Instance
    AddressUseHome,
    AddressUseWork,
    AddressUseTemp,
    AddressUseOld,
    AddressUseBilling);
  TFhirAddressUseEnumList = set of TFhirAddressUseEnum;

  // The gender of a person used for administrative purposes. (from http://hl7.org/fhir/ValueSet/administrative-gender)
  TFhirAdministrativeGenderEnum = (
    AdministrativeGenderNull, // Value is missing from Instance
    AdministrativeGenderMale,
    AdministrativeGenderFemale,
    AdministrativeGenderOther,
    AdministrativeGenderUnknown);
  TFhirAdministrativeGenderEnumList = set of TFhirAdministrativeGenderEnum;

  // Overall nature of the adverse event, e.g. real or potential. (from http://hl7.org/fhir/ValueSet/adverse-event-actuality)
  TFhirAdverseEventActualityEnum = (
    AdverseEventActualityNull, // Value is missing from Instance
    AdverseEventActualityActual,
    AdverseEventActualityPotential);
  TFhirAdverseEventActualityEnumList = set of TFhirAdverseEventActualityEnum;

  // Codes identifying the lifecycle stage of an adverse event. (from http://hl7.org/fhir/ValueSet/adverse-event-status)
  TFhirAdverseEventStatusEnum = (
    AdverseEventStatusNull, // Value is missing from Instance
    AdverseEventStatusInProgress,
    AdverseEventStatusCompleted,
    AdverseEventStatusEnteredInError,
    AdverseEventStatusUnknown);
  TFhirAdverseEventStatusEnumList = set of TFhirAdverseEventStatusEnum;

  // How resource references can be aggregated. (from http://hl7.org/fhir/ValueSet/resource-aggregation-mode)
  TFhirAggregationModeEnum = (
    AggregationModeNull, // Value is missing from Instance
    AggregationModeContained,
    AggregationModeReferenced,
    AggregationModeBundled);
  TFhirAggregationModeEnumList = set of TFhirAggregationModeEnum;

  // A list of all the concrete types defined in this version of the FHIR specification - Abstract Types, Data Types and Resource Types. (from http://hl7.org/fhir/ValueSet/all-types)
  TFhirAllTypesEnum = (
    AllTypesNull, // Value is missing from Instance
    AllTypesAddress,
    AllTypesAge,
    AllTypesAnnotation,
    AllTypesAttachment,
    AllTypesBackboneElement,
    AllTypesBackboneType,
    AllTypesBase,
    AllTypesCodeableConcept,
    AllTypesCodeableReference,
    AllTypesCoding,
    AllTypesContactDetail,
    AllTypesContactPoint,
    AllTypesContributor,
    AllTypesCount,
    AllTypesDataRequirement,
    AllTypesDataType,
    AllTypesDistance,
    AllTypesDosage,
    AllTypesDuration,
    AllTypesElement,
    AllTypesElementDefinition,
    AllTypesExpression,
    AllTypesExtension,
    AllTypesHumanName,
    AllTypesIdentifier,
    AllTypesMarketingStatus,
    AllTypesMeta,
    AllTypesMoney,
    AllTypesMoneyQuantity,
    AllTypesNarrative,
    AllTypesOrderedDistribution,
    AllTypesParameterDefinition,
    AllTypesPeriod,
    AllTypesPopulation,
    AllTypesPrimitiveType,
    AllTypesProdCharacteristic,
    AllTypesProductShelfLife,
    AllTypesQuantity,
    AllTypesRange,
    AllTypesRatio,
    AllTypesReference,
    AllTypesRelatedArtifact,
    AllTypesSampledData,
    AllTypesSignature,
    AllTypesSimpleQuantity,
    AllTypesStatistic,
    AllTypesTiming,
    AllTypesTriggerDefinition,
    AllTypesUsageContext,
    AllTypesBase64Binary,
    AllTypesBoolean,
    AllTypesCanonical,
    AllTypesCode,
    AllTypesDate,
    AllTypesDateTime,
    AllTypesDecimal,
    AllTypesId,
    AllTypesInstant,
    AllTypesInteger,
    AllTypesInteger64,
    AllTypesMarkdown,
    AllTypesOid,
    AllTypesPositiveInt,
    AllTypesString,
    AllTypesTime,
    AllTypesUnsignedInt,
    AllTypesUri,
    AllTypesUrl,
    AllTypesUuid,
    AllTypesXhtml,
    AllTypesAccount,
    AllTypesActivityDefinition,
    AllTypesAdministrableProductDefinition,
    AllTypesAdverseEvent,
    AllTypesAllergyIntolerance,
    AllTypesAppointment,
    AllTypesAppointmentResponse,
    AllTypesAuditEvent,
    AllTypesBasic,
    AllTypesBinary,
    AllTypesBiologicallyDerivedProduct,
    AllTypesBodyStructure,
    AllTypesBundle,
    AllTypesCapabilityStatement,
    AllTypesCapabilityStatement2,
    AllTypesCarePlan,
    AllTypesCareTeam,
    AllTypesCatalogEntry,
    AllTypesChargeItem,
    AllTypesChargeItemDefinition,
    AllTypesCitation,
    AllTypesClaim,
    AllTypesClaimResponse,
    AllTypesClinicalImpression,
    AllTypesClinicalUseIssue,
    AllTypesCodeSystem,
    AllTypesCommunication,
    AllTypesCommunicationRequest,
    AllTypesCompartmentDefinition,
    AllTypesComposition,
    AllTypesConceptMap,
    AllTypesCondition,
    AllTypesConditionDefinition,
    AllTypesConsent,
    AllTypesContract,
    AllTypesCoverage,
    AllTypesCoverageEligibilityRequest,
    AllTypesCoverageEligibilityResponse,
    AllTypesDetectedIssue,
    AllTypesDevice,
    AllTypesDeviceDefinition,
    AllTypesDeviceMetric,
    AllTypesDeviceRequest,
    AllTypesDeviceUseStatement,
    AllTypesDiagnosticReport,
    AllTypesDocumentManifest,
    AllTypesDocumentReference,
    AllTypesDomainResource,
    AllTypesEncounter,
    AllTypesEndpoint,
    AllTypesEnrollmentRequest,
    AllTypesEnrollmentResponse,
    AllTypesEpisodeOfCare,
    AllTypesEventDefinition,
    AllTypesEvidence,
    AllTypesEvidenceReport,
    AllTypesEvidenceVariable,
    AllTypesExampleScenario,
    AllTypesExplanationOfBenefit,
    AllTypesFamilyMemberHistory,
    AllTypesFlag,
    AllTypesGoal,
    AllTypesGraphDefinition,
    AllTypesGroup,
    AllTypesGuidanceResponse,
    AllTypesHealthcareService,
    AllTypesImagingStudy,
    AllTypesImmunization,
    AllTypesImmunizationEvaluation,
    AllTypesImmunizationRecommendation,
    AllTypesImplementationGuide,
    AllTypesIngredient,
    AllTypesInsurancePlan,
    AllTypesInvoice,
    AllTypesLibrary,
    AllTypesLinkage,
    AllTypesList,
    AllTypesLocation,
    AllTypesManufacturedItemDefinition,
    AllTypesMeasure,
    AllTypesMeasureReport,
    AllTypesMedication,
    AllTypesMedicationAdministration,
    AllTypesMedicationDispense,
    AllTypesMedicationKnowledge,
    AllTypesMedicationRequest,
    AllTypesMedicationUsage,
    AllTypesMedicinalProductDefinition,
    AllTypesMessageDefinition,
    AllTypesMessageHeader,
    AllTypesMolecularSequence,
    AllTypesNamingSystem,
    AllTypesNutritionIntake,
    AllTypesNutritionOrder,
    AllTypesNutritionProduct,
    AllTypesObservation,
    AllTypesObservationDefinition,
    AllTypesOperationDefinition,
    AllTypesOperationOutcome,
    AllTypesOrganization,
    AllTypesOrganizationAffiliation,
    AllTypesPackagedProductDefinition,
    AllTypesParameters,
    AllTypesPatient,
    AllTypesPaymentNotice,
    AllTypesPaymentReconciliation,
    AllTypesPermission,
    AllTypesPerson,
    AllTypesPlanDefinition,
    AllTypesPractitioner,
    AllTypesPractitionerRole,
    AllTypesProcedure,
    AllTypesProvenance,
    AllTypesQuestionnaire,
    AllTypesQuestionnaireResponse,
    AllTypesRegulatedAuthorization,
    AllTypesRelatedPerson,
    AllTypesRequestGroup,
    AllTypesResearchStudy,
    AllTypesResearchSubject,
    AllTypesResource,
    AllTypesRiskAssessment,
    AllTypesSchedule,
    AllTypesSearchParameter,
    AllTypesServiceRequest,
    AllTypesSlot,
    AllTypesSpecimen,
    AllTypesSpecimenDefinition,
    AllTypesStructureDefinition,
    AllTypesStructureMap,
    AllTypesSubscription,
    AllTypesSubscriptionStatus,
    AllTypesSubscriptionTopic,
    AllTypesSubstance,
    AllTypesSubstanceDefinition,
    AllTypesSubstanceNucleicAcid,
    AllTypesSubstancePolymer,
    AllTypesSubstanceProtein,
    AllTypesSubstanceReferenceInformation,
    AllTypesSubstanceSourceMaterial,
    AllTypesSupplyDelivery,
    AllTypesSupplyRequest,
    AllTypesTask,
    AllTypesTerminologyCapabilities,
    AllTypesTestReport,
    AllTypesTestScript,
    AllTypesValueSet,
    AllTypesVerificationResult,
    AllTypesVisionPrescription,
    AllTypesType,
    AllTypesAny);
  TFhirAllTypesEnumList = set of TFhirAllTypesEnum;

  // Category of an identified substance associated with allergies or intolerances. (from http://hl7.org/fhir/ValueSet/allergy-intolerance-category)
  TFhirAllergyIntoleranceCategoryEnum = (
    AllergyIntoleranceCategoryNull, // Value is missing from Instance
    AllergyIntoleranceCategoryFood,
    AllergyIntoleranceCategoryMedication,
    AllergyIntoleranceCategoryEnvironment,
    AllergyIntoleranceCategoryBiologic);
  TFhirAllergyIntoleranceCategoryEnumList = set of TFhirAllergyIntoleranceCategoryEnum;

  // Estimate of the potential clinical harm, or seriousness, of a reaction to an identified substance. (from http://hl7.org/fhir/ValueSet/allergy-intolerance-criticality)
  TFhirAllergyIntoleranceCriticalityEnum = (
    AllergyIntoleranceCriticalityNull, // Value is missing from Instance
    AllergyIntoleranceCriticalityLow,
    AllergyIntoleranceCriticalityHigh,
    AllergyIntoleranceCriticalityUnableToAssess);
  TFhirAllergyIntoleranceCriticalityEnumList = set of TFhirAllergyIntoleranceCriticalityEnum;

  // Clinical assessment of the severity of a reaction event as a whole, potentially considering multiple different manifestations. (from http://hl7.org/fhir/ValueSet/reaction-event-severity)
  TFhirAllergyIntoleranceSeverityEnum = (
    AllergyIntoleranceSeverityNull, // Value is missing from Instance
    AllergyIntoleranceSeverityMild,
    AllergyIntoleranceSeverityModerate,
    AllergyIntoleranceSeveritySevere);
  TFhirAllergyIntoleranceSeverityEnumList = set of TFhirAllergyIntoleranceSeverityEnum;

  // Identification of the underlying physiological mechanism for a Reaction Risk. (from http://hl7.org/fhir/ValueSet/allergy-intolerance-type)
  TFhirAllergyIntoleranceTypeEnum = (
    AllergyIntoleranceTypeNull, // Value is missing from Instance
    AllergyIntoleranceTypeAllergy,
    AllergyIntoleranceTypeIntolerance);
  TFhirAllergyIntoleranceTypeEnumList = set of TFhirAllergyIntoleranceTypeEnum;

  // The free/busy status of an appointment. (from http://hl7.org/fhir/ValueSet/appointmentstatus)
  TFhirAppointmentStatusEnum = (
    AppointmentStatusNull, // Value is missing from Instance
    AppointmentStatusProposed,
    AppointmentStatusPending,
    AppointmentStatusBooked,
    AppointmentStatusArrived,
    AppointmentStatusFulfilled,
    AppointmentStatusCancelled,
    AppointmentStatusNoshow,
    AppointmentStatusEnteredInError,
    AppointmentStatusCheckedIn,
    AppointmentStatusWaitlist);
  TFhirAppointmentStatusEnumList = set of TFhirAppointmentStatusEnum;

  // The type of direction to use for assertion. (from http://hl7.org/fhir/ValueSet/assert-direction-codes)
  TFhirAssertionDirectionTypeEnum = (
    AssertionDirectionTypeNull, // Value is missing from Instance
    AssertionDirectionTypeResponse,
    AssertionDirectionTypeRequest);
  TFhirAssertionDirectionTypeEnumList = set of TFhirAssertionDirectionTypeEnum;

  // The type of operator to use for assertion. (from http://hl7.org/fhir/ValueSet/assert-operator-codes)
  TFhirAssertionOperatorTypeEnum = (
    AssertionOperatorTypeNull, // Value is missing from Instance
    AssertionOperatorTypeEquals,
    AssertionOperatorTypeNotEquals,
    AssertionOperatorTypeIn,
    AssertionOperatorTypeNotIn,
    AssertionOperatorTypeGreaterThan,
    AssertionOperatorTypeLessThan,
    AssertionOperatorTypeEmpty,
    AssertionOperatorTypeNotEmpty,
    AssertionOperatorTypeContains,
    AssertionOperatorTypeNotContains,
    AssertionOperatorTypeEval);
  TFhirAssertionOperatorTypeEnumList = set of TFhirAssertionOperatorTypeEnum;

  // The type of response code to use for assertion. (from http://hl7.org/fhir/ValueSet/assert-response-code-types)
  TFhirAssertionResponseTypesEnum = (
    AssertionResponseTypesNull, // Value is missing from Instance
    AssertionResponseTypesOkay,
    AssertionResponseTypesCreated,
    AssertionResponseTypesNoContent,
    AssertionResponseTypesNotModified,
    AssertionResponseTypesBad,
    AssertionResponseTypesForbidden,
    AssertionResponseTypesNotFound,
    AssertionResponseTypesMethodNotAllowed,
    AssertionResponseTypesConflict,
    AssertionResponseTypesGone,
    AssertionResponseTypesPreconditionFailed,
    AssertionResponseTypesUnprocessable);
  TFhirAssertionResponseTypesEnumList = set of TFhirAssertionResponseTypesEnum;

  // Indicator for type of action performed during the event that generated the event. (from http://hl7.org/fhir/ValueSet/audit-event-action)
  TFhirAuditEventActionEnum = (
    AuditEventActionNull, // Value is missing from Instance
    AuditEventActionC,
    AuditEventActionR,
    AuditEventActionU,
    AuditEventActionD,
    AuditEventActionE);
  TFhirAuditEventActionEnumList = set of TFhirAuditEventActionEnum;

  // The type of network access point of this agent in the audit event. (from http://hl7.org/fhir/ValueSet/network-type)
  TFhirAuditEventAgentNetworkTypeEnum = (
    AuditEventAgentNetworkTypeNull, // Value is missing from Instance
    AuditEventAgentNetworkType1,
    AuditEventAgentNetworkType2,
    AuditEventAgentNetworkType3,
    AuditEventAgentNetworkType4,
    AuditEventAgentNetworkType5);
  TFhirAuditEventAgentNetworkTypeEnumList = set of TFhirAuditEventAgentNetworkTypeEnum;

  // The severity of the audit entry. (from http://hl7.org/fhir/ValueSet/audit-event-severity)
  TFhirAuditEventSeverityEnum = (
    AuditEventSeverityNull, // Value is missing from Instance
    AuditEventSeverityEmergency,
    AuditEventSeverityAlert,
    AuditEventSeverityCritical,
    AuditEventSeverityError,
    AuditEventSeverityWarning,
    AuditEventSeverityNotice,
    AuditEventSeverityInformational,
    AuditEventSeverityDebug);
  TFhirAuditEventSeverityEnumList = set of TFhirAuditEventSeverityEnum;

  // Indication of the degree of conformance expectations associated with a binding. (from http://hl7.org/fhir/ValueSet/binding-strength)
  TFhirBindingStrengthEnum = (
    BindingStrengthNull, // Value is missing from Instance
    BindingStrengthRequired,
    BindingStrengthExtensible,
    BindingStrengthPreferred,
    BindingStrengthExample);
  TFhirBindingStrengthEnumList = set of TFhirBindingStrengthEnum;

  // Biologically Derived Product Category. (from http://hl7.org/fhir/ValueSet/product-category)
  TFhirBiologicallyDerivedProductCategoryEnum = (
    BiologicallyDerivedProductCategoryNull, // Value is missing from Instance
    BiologicallyDerivedProductCategoryOrgan,
    BiologicallyDerivedProductCategoryTissue,
    BiologicallyDerivedProductCategoryFluid,
    BiologicallyDerivedProductCategoryCells,
    BiologicallyDerivedProductCategoryBiologicalAgent);
  TFhirBiologicallyDerivedProductCategoryEnumList = set of TFhirBiologicallyDerivedProductCategoryEnum;

  // Biologically Derived Product Status. (from http://hl7.org/fhir/ValueSet/product-status)
  TFhirBiologicallyDerivedProductStatusEnum = (
    BiologicallyDerivedProductStatusNull, // Value is missing from Instance
    BiologicallyDerivedProductStatusAvailable,
    BiologicallyDerivedProductStatusUnavailable);
  TFhirBiologicallyDerivedProductStatusEnumList = set of TFhirBiologicallyDerivedProductStatusEnum;

  // BiologicallyDerived Product Storage Scale. (from http://hl7.org/fhir/ValueSet/product-storage-scale)
  TFhirBiologicallyDerivedProductStorageScaleEnum = (
    BiologicallyDerivedProductStorageScaleNull, // Value is missing from Instance
    BiologicallyDerivedProductStorageScaleFarenheit,
    BiologicallyDerivedProductStorageScaleCelsius,
    BiologicallyDerivedProductStorageScaleKelvin);
  TFhirBiologicallyDerivedProductStorageScaleEnumList = set of TFhirBiologicallyDerivedProductStorageScaleEnum;

  // Indicates the purpose of a bundle - how it is intended to be used. (from http://hl7.org/fhir/ValueSet/bundle-type)
  TFhirBundleTypeEnum = (
    BundleTypeNull, // Value is missing from Instance
    BundleTypeDocument,
    BundleTypeMessage,
    BundleTypeTransaction,
    BundleTypeTransactionResponse,
    BundleTypeBatch,
    BundleTypeBatchResponse,
    BundleTypeHistory,
    BundleTypeSearchset,
    BundleTypeCollection,
    BundleTypeSubscriptionNotification);
  TFhirBundleTypeEnumList = set of TFhirBundleTypeEnum;

  // How a capability statement is intended to be used. (from http://hl7.org/fhir/ValueSet/capability-statement-kind)
  TFhirCapabilityStatementKindEnum = (
    CapabilityStatementKindNull, // Value is missing from Instance
    CapabilityStatementKindInstance,
    CapabilityStatementKindCapability,
    CapabilityStatementKindRequirements);
  TFhirCapabilityStatementKindEnumList = set of TFhirCapabilityStatementKindEnum;

  // Resource types defined as part of FHIR that can be represented as in-line definitions of a care plan activity. (from http://hl7.org/fhir/ValueSet/care-plan-activity-kind)
  TFhirCarePlanActivityKindEnum = (
    CarePlanActivityKindNull, // Value is missing from Instance
    CarePlanActivityKindAppointment,
    CarePlanActivityKindCommunicationRequest,
    CarePlanActivityKindDeviceRequest,
    CarePlanActivityKindMedicationRequest,
    CarePlanActivityKindNutritionOrder,
    CarePlanActivityKindTask,
    CarePlanActivityKindServiceRequest,
    CarePlanActivityKindVisionPrescription);
  TFhirCarePlanActivityKindEnumList = set of TFhirCarePlanActivityKindEnum;

  // Codes that reflect the current state of a care plan activity within its overall life cycle. (from http://hl7.org/fhir/ValueSet/care-plan-activity-status)
  TFhirCarePlanActivityStatusEnum = (
    CarePlanActivityStatusNull, // Value is missing from Instance
    CarePlanActivityStatusNotStarted,
    CarePlanActivityStatusScheduled,
    CarePlanActivityStatusInProgress,
    CarePlanActivityStatusOnHold,
    CarePlanActivityStatusCompleted,
    CarePlanActivityStatusCancelled,
    CarePlanActivityStatusStopped,
    CarePlanActivityStatusUnknown,
    CarePlanActivityStatusEnteredInError);
  TFhirCarePlanActivityStatusEnumList = set of TFhirCarePlanActivityStatusEnum;

  // Codes indicating the degree of authority/intentionality associated with a care plan. (from http://hl7.org/fhir/ValueSet/care-plan-intent)
  TFhirCarePlanIntentEnum = (
    CarePlanIntentNull, // Value is missing from Instance
    CarePlanIntentProposal,
    CarePlanIntentPlan,
    CarePlanIntentOrder,
    CarePlanIntentOption,
    CarePlanIntentDirective);
  TFhirCarePlanIntentEnumList = set of TFhirCarePlanIntentEnum;

  // Indicates the status of the care team. (from http://hl7.org/fhir/ValueSet/care-team-status)
  TFhirCareTeamStatusEnum = (
    CareTeamStatusNull, // Value is missing from Instance
    CareTeamStatusProposed,
    CareTeamStatusActive,
    CareTeamStatusSuspended,
    CareTeamStatusInactive,
    CareTeamStatusEnteredInError);
  TFhirCareTeamStatusEnumList = set of TFhirCareTeamStatusEnum;

  // Types of relationships between entries. (from http://hl7.org/fhir/ValueSet/catalogentry-relation-type)
  TFhirCatalogEntryRelationTypeEnum = (
    CatalogEntryRelationTypeNull, // Value is missing from Instance
    CatalogEntryRelationTypeTriggers,
    CatalogEntryRelationTypeIsReplacedBy,
    CatalogEntryRelationTypeExcludes,
    CatalogEntryRelationTypeIncludes);
  TFhirCatalogEntryRelationTypeEnumList = set of TFhirCatalogEntryRelationTypeEnum;

  // Public usability statuses for catalog entries. (from http://hl7.org/fhir/ValueSet/catalogentry-status)
  TFhirCatalogEntryStatusEnum = (
    CatalogEntryStatusNull, // Value is missing from Instance
    CatalogEntryStatusDraft,
    CatalogEntryStatusActive,
    CatalogEntryStatusRetired);
  TFhirCatalogEntryStatusEnumList = set of TFhirCatalogEntryStatusEnum;

  // Types of resources that can be attached to catalog entries. (from http://hl7.org/fhir/ValueSet/catalogentry-type)
  TFhirCatalogEntryTypeEnum = (
    CatalogEntryTypeNull, // Value is missing from Instance
    CatalogEntryTypeActivityDefinition,
    CatalogEntryTypePlanDefinition,
    CatalogEntryTypeSpecimenDefinition,
    CatalogEntryTypeObservationDefinition,
    CatalogEntryTypeDeviceDefinition,
    CatalogEntryTypeOrganization,
    CatalogEntryTypePractitioner,
    CatalogEntryTypePractitionerRole,
    CatalogEntryTypeHealthcareService,
    CatalogEntryTypeMedicationKnowledge,
    CatalogEntryTypeMedication,
    CatalogEntryTypeSubstance,
    CatalogEntryTypeLocation);
  TFhirCatalogEntryTypeEnumList = set of TFhirCatalogEntryTypeEnum;

  // Logical grouping of characteristics. (from http://hl7.org/fhir/ValueSet/characteristic-combination)
  TFhirCharacteristicCombinationEnum = (
    CharacteristicCombinationNull, // Value is missing from Instance
    CharacteristicCombinationIntersection,
    CharacteristicCombinationUnion);
  TFhirCharacteristicCombinationEnumList = set of TFhirCharacteristicCombinationEnum;

  // Codes identifying the lifecycle stage of a ChargeItem. (from http://hl7.org/fhir/ValueSet/chargeitem-status)
  TFhirChargeItemStatusEnum = (
    ChargeItemStatusNull, // Value is missing from Instance
    ChargeItemStatusPlanned,
    ChargeItemStatusBillable,
    ChargeItemStatusNotBillable,
    ChargeItemStatusAborted,
    ChargeItemStatusBilled,
    ChargeItemStatusEnteredInError,
    ChargeItemStatusUnknown);
  TFhirChargeItemStatusEnumList = set of TFhirChargeItemStatusEnum;

  // This value set includes Claim Processing Outcome codes. (from http://hl7.org/fhir/ValueSet/remittance-outcome)
  TFhirClaimProcessingCodesEnum = (
    ClaimProcessingCodesNull, // Value is missing from Instance
    ClaimProcessingCodesQueued,
    ClaimProcessingCodesComplete,
    ClaimProcessingCodesError,
    ClaimProcessingCodesPartial);
  TFhirClaimProcessingCodesEnumList = set of TFhirClaimProcessingCodesEnum;

  // Overall defining type of this clinical use issue. (from http://hl7.org/fhir/ValueSet/clinical-use-issue-type)
  TFhirClinicalUseIssueTypeEnum = (
    ClinicalUseIssueTypeNull, // Value is missing from Instance
    ClinicalUseIssueTypeIndication,
    ClinicalUseIssueTypeContraindication,
    ClinicalUseIssueTypeInteraction,
    ClinicalUseIssueTypeUndesirableEffect,
    ClinicalUseIssueTypeWarning);
  TFhirClinicalUseIssueTypeEnumList = set of TFhirClinicalUseIssueTypeEnum;

  // The degree to which the server supports the code search parameter on ValueSet, if it is supported. (from http://hl7.org/fhir/ValueSet/code-search-support)
  TFhirCodeSearchSupportEnum = (
    CodeSearchSupportNull, // Value is missing from Instance
    CodeSearchSupportExplicit,
    CodeSearchSupportAll);
  TFhirCodeSearchSupportEnumList = set of TFhirCodeSearchSupportEnum;

  // The extent of the content of the code system (the concepts and codes it defines) are represented in a code system resource. (from http://hl7.org/fhir/ValueSet/codesystem-content-mode)
  TFhirCodeSystemContentModeEnum = (
    CodeSystemContentModeNull, // Value is missing from Instance
    CodeSystemContentModeNotPresent,
    CodeSystemContentModeExample,
    CodeSystemContentModeFragment,
    CodeSystemContentModeComplete,
    CodeSystemContentModeSupplement);
  TFhirCodeSystemContentModeEnumList = set of TFhirCodeSystemContentModeEnum;

  // The meaning of the hierarchy of concepts in a code system. (from http://hl7.org/fhir/ValueSet/codesystem-hierarchy-meaning)
  TFhirCodeSystemHierarchyMeaningEnum = (
    CodeSystemHierarchyMeaningNull, // Value is missing from Instance
    CodeSystemHierarchyMeaningGroupedBy,
    CodeSystemHierarchyMeaningIsA,
    CodeSystemHierarchyMeaningPartOf,
    CodeSystemHierarchyMeaningClassifiedWith);
  TFhirCodeSystemHierarchyMeaningEnumList = set of TFhirCodeSystemHierarchyMeaningEnum;

  // Which type a compartment definition describes. (from http://hl7.org/fhir/ValueSet/compartment-type)
  TFhirCompartmentTypeEnum = (
    CompartmentTypeNull, // Value is missing from Instance
    CompartmentTypePatient,
    CompartmentTypeEncounter,
    CompartmentTypeRelatedPerson,
    CompartmentTypePractitioner,
    CompartmentTypeDevice);
  TFhirCompartmentTypeEnumList = set of TFhirCompartmentTypeEnum;

  // The way in which a person authenticated a composition. (from http://hl7.org/fhir/ValueSet/composition-attestation-mode)
  TFhirCompositionAttestationModeEnum = (
    CompositionAttestationModeNull, // Value is missing from Instance
    CompositionAttestationModePersonal,
    CompositionAttestationModeProfessional,
    CompositionAttestationModeLegal,
    CompositionAttestationModeOfficial);
  TFhirCompositionAttestationModeEnumList = set of TFhirCompositionAttestationModeEnum;

  // The workflow/clinical status of the composition. (from http://hl7.org/fhir/ValueSet/composition-status)
  TFhirCompositionStatusEnum = (
    CompositionStatusNull, // Value is missing from Instance
    CompositionStatusPreliminary,
    CompositionStatusFinal,
    CompositionStatusAmended,
    CompositionStatusEnteredInError);
  TFhirCompositionStatusEnumList = set of TFhirCompositionStatusEnum;

  // Defines which action to take if there is no match in the group. (from http://hl7.org/fhir/ValueSet/conceptmap-unmapped-mode)
  TFhirConceptMapGroupUnmappedModeEnum = (
    ConceptMapGroupUnmappedModeNull, // Value is missing from Instance
    ConceptMapGroupUnmappedModeProvided,
    ConceptMapGroupUnmappedModeFixed,
    ConceptMapGroupUnmappedModeOtherMap);
  TFhirConceptMapGroupUnmappedModeEnumList = set of TFhirConceptMapGroupUnmappedModeEnum;

  // The relationship between concepts. (from http://hl7.org/fhir/ValueSet/concept-map-relationship)
  TFhirConceptMapRelationshipEnum = (
    ConceptMapRelationshipNull, // Value is missing from Instance
    ConceptMapRelationshipRelatedTo,
    ConceptMapRelationshipEquivalent,
    ConceptMapRelationshipSourceIsNarrowerThanTarget,
    ConceptMapRelationshipSourceIsBroaderThanTarget,
    ConceptMapRelationshipNotRelatedTo);
  TFhirConceptMapRelationshipEnumList = set of TFhirConceptMapRelationshipEnum;

  // The type of a property value. (from http://hl7.org/fhir/ValueSet/concept-property-type)
  TFhirConceptPropertyTypeEnum = (
    ConceptPropertyTypeNull, // Value is missing from Instance
    ConceptPropertyTypeCode,
    ConceptPropertyTypeCoding,
    ConceptPropertyTypeString,
    ConceptPropertyTypeInteger,
    ConceptPropertyTypeBoolean,
    ConceptPropertyTypeDateTime,
    ConceptPropertyTypeDecimal);
  TFhirConceptPropertyTypeEnumList = set of TFhirConceptPropertyTypeEnum;

  // Kind of precondition for the condition. (from http://hl7.org/fhir/ValueSet/condition-precondition-type)
  TFhirConditionPreconditionTypeEnum = (
    ConditionPreconditionTypeNull, // Value is missing from Instance
    ConditionPreconditionTypeSensitive,
    ConditionPreconditionTypeSpecific);
  TFhirConditionPreconditionTypeEnumList = set of TFhirConditionPreconditionTypeEnum;

  // The use of a questionnaire. (from http://hl7.org/fhir/ValueSet/condition-questionnaire-purpose)
  TFhirConditionQuestionnairePurposeEnum = (
    ConditionQuestionnairePurposeNull, // Value is missing from Instance
    ConditionQuestionnairePurposePreadmit,
    ConditionQuestionnairePurposeDiffDiagnosis,
    ConditionQuestionnairePurposeOutcome);
  TFhirConditionQuestionnairePurposeEnumList = set of TFhirConditionQuestionnairePurposeEnum;

  // A code that indicates how the server supports conditional delete. (from http://hl7.org/fhir/ValueSet/conditional-delete-status)
  TFhirConditionalDeleteStatusEnum = (
    ConditionalDeleteStatusNull, // Value is missing from Instance
    ConditionalDeleteStatusNotSupported,
    ConditionalDeleteStatusSingle,
    ConditionalDeleteStatusMultiple);
  TFhirConditionalDeleteStatusEnumList = set of TFhirConditionalDeleteStatusEnum;

  // A code that indicates how the server supports conditional read. (from http://hl7.org/fhir/ValueSet/conditional-read-status)
  TFhirConditionalReadStatusEnum = (
    ConditionalReadStatusNull, // Value is missing from Instance
    ConditionalReadStatusNotSupported,
    ConditionalReadStatusModifiedSince,
    ConditionalReadStatusNotMatch,
    ConditionalReadStatusFullSupport);
  TFhirConditionalReadStatusEnumList = set of TFhirConditionalReadStatusEnum;

  // How a resource reference is interpreted when testing consent restrictions. (from http://hl7.org/fhir/ValueSet/consent-data-meaning)
  TFhirConsentDataMeaningEnum = (
    ConsentDataMeaningNull, // Value is missing from Instance
    ConsentDataMeaningInstance,
    ConsentDataMeaningRelated,
    ConsentDataMeaningDependents,
    ConsentDataMeaningAuthoredby);
  TFhirConsentDataMeaningEnumList = set of TFhirConsentDataMeaningEnum;

  // How a rule statement is applied, such as adding additional consent or removing consent. (from http://hl7.org/fhir/ValueSet/consent-provision-type)
  TFhirConsentProvisionTypeEnum = (
    ConsentProvisionTypeNull, // Value is missing from Instance
    ConsentProvisionTypeDeny,
    ConsentProvisionTypePermit);
  TFhirConsentProvisionTypeEnumList = set of TFhirConsentProvisionTypeEnum;

  // Indicates the state of the consent. (from http://hl7.org/fhir/ValueSet/consent-state-codes)
  TFhirConsentStateEnum = (
    ConsentStateNull, // Value is missing from Instance
    ConsentStateDraft,
    ConsentStateActive,
    ConsentStateInactive,
    ConsentStateEnteredInError,
    ConsentStateUnknown);
  TFhirConsentStateEnumList = set of TFhirConsentStateEnum;

  // SHALL applications comply with this constraint? (from http://hl7.org/fhir/ValueSet/constraint-severity)
  TFhirConstraintSeverityEnum = (
    ConstraintSeverityNull, // Value is missing from Instance
    ConstraintSeverityError,
    ConstraintSeverityWarning);
  TFhirConstraintSeverityEnumList = set of TFhirConstraintSeverityEnum;

  // Telecommunications form for contact point. (from http://hl7.org/fhir/ValueSet/contact-point-system)
  TFhirContactPointSystemEnum = (
    ContactPointSystemNull, // Value is missing from Instance
    ContactPointSystemPhone,
    ContactPointSystemFax,
    ContactPointSystemEmail,
    ContactPointSystemPager,
    ContactPointSystemUrl,
    ContactPointSystemSms,
    ContactPointSystemOther);
  TFhirContactPointSystemEnumList = set of TFhirContactPointSystemEnum;

  // Use of contact point. (from http://hl7.org/fhir/ValueSet/contact-point-use)
  TFhirContactPointUseEnum = (
    ContactPointUseNull, // Value is missing from Instance
    ContactPointUseHome,
    ContactPointUseWork,
    ContactPointUseTemp,
    ContactPointUseOld,
    ContactPointUseMobile);
  TFhirContactPointUseEnumList = set of TFhirContactPointUseEnum;

  // This value set contract specific codes for status. (from http://hl7.org/fhir/ValueSet/contract-publicationstatus)
  TFhirContractResourcePublicationStatusCodesEnum = (
    ContractResourcePublicationStatusCodesNull, // Value is missing from Instance
    ContractResourcePublicationStatusCodesAmended,
    ContractResourcePublicationStatusCodesAppended,
    ContractResourcePublicationStatusCodesCancelled,
    ContractResourcePublicationStatusCodesDisputed,
    ContractResourcePublicationStatusCodesEnteredInError,
    ContractResourcePublicationStatusCodesExecutable,
    ContractResourcePublicationStatusCodesExecuted,
    ContractResourcePublicationStatusCodesNegotiable,
    ContractResourcePublicationStatusCodesOffered,
    ContractResourcePublicationStatusCodesPolicy,
    ContractResourcePublicationStatusCodesRejected,
    ContractResourcePublicationStatusCodesRenewed,
    ContractResourcePublicationStatusCodesRevoked,
    ContractResourcePublicationStatusCodesResolved,
    ContractResourcePublicationStatusCodesTerminated);
  TFhirContractResourcePublicationStatusCodesEnumList = set of TFhirContractResourcePublicationStatusCodesEnum;

  // This value set contract specific codes for status. (from http://hl7.org/fhir/ValueSet/contract-status)
  TFhirContractResourceStatusCodesEnum = (
    ContractResourceStatusCodesNull, // Value is missing from Instance
    ContractResourceStatusCodesAmended,
    ContractResourceStatusCodesAppended,
    ContractResourceStatusCodesCancelled,
    ContractResourceStatusCodesDisputed,
    ContractResourceStatusCodesEnteredInError,
    ContractResourceStatusCodesExecutable,
    ContractResourceStatusCodesExecuted,
    ContractResourceStatusCodesNegotiable,
    ContractResourceStatusCodesOffered,
    ContractResourceStatusCodesPolicy,
    ContractResourceStatusCodesRejected,
    ContractResourceStatusCodesRenewed,
    ContractResourceStatusCodesRevoked,
    ContractResourceStatusCodesResolved,
    ContractResourceStatusCodesTerminated);
  TFhirContractResourceStatusCodesEnumList = set of TFhirContractResourceStatusCodesEnum;

  // The type of contributor. (from http://hl7.org/fhir/ValueSet/contributor-type)
  TFhirContributorTypeEnum = (
    ContributorTypeNull, // Value is missing from Instance
    ContributorTypeAuthor,
    ContributorTypeEditor,
    ContributorTypeReviewer,
    ContributorTypeEndorser);
  TFhirContributorTypeEnumList = set of TFhirContributorTypeEnum;

  // The days of the week. (from http://hl7.org/fhir/ValueSet/days-of-week)
  TFhirDaysOfWeekEnum = (
    DaysOfWeekNull, // Value is missing from Instance
    DaysOfWeekMon,
    DaysOfWeekTue,
    DaysOfWeekWed,
    DaysOfWeekThu,
    DaysOfWeekFri,
    DaysOfWeekSat,
    DaysOfWeekSun);
  TFhirDaysOfWeekEnumList = set of TFhirDaysOfWeekEnum;

  // Indicates the potential degree of impact of the identified issue on the patient. (from http://hl7.org/fhir/ValueSet/detectedissue-severity)
  TFhirDetectedIssueSeverityEnum = (
    DetectedIssueSeverityNull, // Value is missing from Instance
    DetectedIssueSeverityHigh,
    DetectedIssueSeverityModerate,
    DetectedIssueSeverityLow);
  TFhirDetectedIssueSeverityEnumList = set of TFhirDetectedIssueSeverityEnum;

  // Describes the state of a metric calibration. (from http://hl7.org/fhir/ValueSet/metric-calibration-state)
  TFhirDeviceMetricCalibrationStateEnum = (
    DeviceMetricCalibrationStateNull, // Value is missing from Instance
    DeviceMetricCalibrationStateNotCalibrated,
    DeviceMetricCalibrationStateCalibrationRequired,
    DeviceMetricCalibrationStateCalibrated,
    DeviceMetricCalibrationStateUnspecified);
  TFhirDeviceMetricCalibrationStateEnumList = set of TFhirDeviceMetricCalibrationStateEnum;

  // Describes the type of a metric calibration. (from http://hl7.org/fhir/ValueSet/metric-calibration-type)
  TFhirDeviceMetricCalibrationTypeEnum = (
    DeviceMetricCalibrationTypeNull, // Value is missing from Instance
    DeviceMetricCalibrationTypeUnspecified,
    DeviceMetricCalibrationTypeOffset,
    DeviceMetricCalibrationTypeGain,
    DeviceMetricCalibrationTypeTwoPoint);
  TFhirDeviceMetricCalibrationTypeEnumList = set of TFhirDeviceMetricCalibrationTypeEnum;

  // Describes the category of the metric. (from http://hl7.org/fhir/ValueSet/metric-category)
  TFhirDeviceMetricCategoryEnum = (
    DeviceMetricCategoryNull, // Value is missing from Instance
    DeviceMetricCategoryMeasurement,
    DeviceMetricCategorySetting,
    DeviceMetricCategoryCalculation,
    DeviceMetricCategoryUnspecified);
  TFhirDeviceMetricCategoryEnumList = set of TFhirDeviceMetricCategoryEnum;

  // Describes the typical color of representation. (from http://hl7.org/fhir/ValueSet/metric-color)
  TFhirDeviceMetricColorEnum = (
    DeviceMetricColorNull, // Value is missing from Instance
    DeviceMetricColorBlack,
    DeviceMetricColorRed,
    DeviceMetricColorGreen,
    DeviceMetricColorYellow,
    DeviceMetricColorBlue,
    DeviceMetricColorMagenta,
    DeviceMetricColorCyan,
    DeviceMetricColorWhite);
  TFhirDeviceMetricColorEnumList = set of TFhirDeviceMetricColorEnum;

  // Describes the operational status of the DeviceMetric. (from http://hl7.org/fhir/ValueSet/metric-operational-status)
  TFhirDeviceMetricOperationalStatusEnum = (
    DeviceMetricOperationalStatusNull, // Value is missing from Instance
    DeviceMetricOperationalStatusOn,
    DeviceMetricOperationalStatusOff,
    DeviceMetricOperationalStatusStandby,
    DeviceMetricOperationalStatusEnteredInError);
  TFhirDeviceMetricOperationalStatusEnumList = set of TFhirDeviceMetricOperationalStatusEnum;

  // The type of name the device is referred by. (from http://hl7.org/fhir/ValueSet/device-nametype)
  TFhirDeviceNameTypeEnum = (
    DeviceNameTypeNull, // Value is missing from Instance
    DeviceNameTypeUdiLabelName,
    DeviceNameTypeUserFriendlyName,
    DeviceNameTypePatientReportedName,
    DeviceNameTypeManufacturerName,
    DeviceNameTypeModelName,
    DeviceNameTypeOther);
  TFhirDeviceNameTypeEnumList = set of TFhirDeviceNameTypeEnum;

  // A coded concept indicating the current status of the Device Usage. (from http://hl7.org/fhir/ValueSet/device-statement-status)
  TFhirDeviceUseStatementStatusEnum = (
    DeviceUseStatementStatusNull, // Value is missing from Instance
    DeviceUseStatementStatusActive,
    DeviceUseStatementStatusCompleted,
    DeviceUseStatementStatusEnteredInError,
    DeviceUseStatementStatusIntended,
    DeviceUseStatementStatusStopped,
    DeviceUseStatementStatusOnHold);
  TFhirDeviceUseStatementStatusEnumList = set of TFhirDeviceUseStatementStatusEnum;

  // The status of the diagnostic report. (from http://hl7.org/fhir/ValueSet/diagnostic-report-status)
  TFhirDiagnosticReportStatusEnum = (
    DiagnosticReportStatusNull, // Value is missing from Instance
    DiagnosticReportStatusRegistered,
    DiagnosticReportStatusPartial,
    DiagnosticReportStatusPreliminary,
    DiagnosticReportStatusFinal,
    DiagnosticReportStatusAmended,
    DiagnosticReportStatusCorrected,
    DiagnosticReportStatusAppended,
    DiagnosticReportStatusCancelled,
    DiagnosticReportStatusEnteredInError,
    DiagnosticReportStatusUnknown);
  TFhirDiagnosticReportStatusEnumList = set of TFhirDiagnosticReportStatusEnum;

  // How an element value is interpreted when discrimination is evaluated. (from http://hl7.org/fhir/ValueSet/discriminator-type)
  TFhirDiscriminatorTypeEnum = (
    DiscriminatorTypeNull, // Value is missing from Instance
    DiscriminatorTypeValue,
    DiscriminatorTypeExists,
    DiscriminatorTypePattern,
    DiscriminatorTypeType,
    DiscriminatorTypeProfile);
  TFhirDiscriminatorTypeEnumList = set of TFhirDiscriminatorTypeEnum;

  // The way in which a person authenticated a document. (from http://hl7.org/fhir/ValueSet/document-attestation-mode)
  TFhirDocumentAttestationModeEnum = (
    DocumentAttestationModeNull, // Value is missing from Instance
    DocumentAttestationModePersonal,
    DocumentAttestationModeProfessional,
    DocumentAttestationModeLegal,
    DocumentAttestationModeOfficial);
  TFhirDocumentAttestationModeEnumList = set of TFhirDocumentAttestationModeEnum;

  // Whether the application produces or consumes documents. (from http://hl7.org/fhir/ValueSet/document-mode)
  TFhirDocumentModeEnum = (
    DocumentModeNull, // Value is missing from Instance
    DocumentModeProducer,
    DocumentModeConsumer);
  TFhirDocumentModeEnumList = set of TFhirDocumentModeEnum;

  // The status of the document reference. (from http://hl7.org/fhir/ValueSet/document-reference-status)
  TFhirDocumentReferenceStatusEnum = (
    DocumentReferenceStatusNull, // Value is missing from Instance
    DocumentReferenceStatusCurrent,
    DocumentReferenceStatusSuperseded,
    DocumentReferenceStatusEnteredInError);
  TFhirDocumentReferenceStatusEnumList = set of TFhirDocumentReferenceStatusEnum;

  // The type of relationship between documents. (from http://hl7.org/fhir/ValueSet/document-relationship-type)
  TFhirDocumentRelationshipTypeEnum = (
    DocumentRelationshipTypeNull, // Value is missing from Instance
    DocumentRelationshipTypeReplaces,
    DocumentRelationshipTypeTransforms,
    DocumentRelationshipTypeSigns,
    DocumentRelationshipTypeAppends);
  TFhirDocumentRelationshipTypeEnumList = set of TFhirDocumentRelationshipTypeEnum;

  // A code specifying the types of information being requested. (from http://hl7.org/fhir/ValueSet/eligibilityrequest-purpose)
  TFhirEligibilityRequestPurposeEnum = (
    EligibilityRequestPurposeNull, // Value is missing from Instance
    EligibilityRequestPurposeAuthRequirements,
    EligibilityRequestPurposeBenefits,
    EligibilityRequestPurposeDiscovery,
    EligibilityRequestPurposeValidation);
  TFhirEligibilityRequestPurposeEnumList = set of TFhirEligibilityRequestPurposeEnum;

  // A code specifying the types of information being requested. (from http://hl7.org/fhir/ValueSet/eligibilityresponse-purpose)
  TFhirEligibilityResponsePurposeEnum = (
    EligibilityResponsePurposeNull, // Value is missing from Instance
    EligibilityResponsePurposeAuthRequirements,
    EligibilityResponsePurposeBenefits,
    EligibilityResponsePurposeDiscovery,
    EligibilityResponsePurposeValidation);
  TFhirEligibilityResponsePurposeEnumList = set of TFhirEligibilityResponsePurposeEnum;

  // Controls how multiple enableWhen values are interpreted -  whether all or any must be true. (from http://hl7.org/fhir/ValueSet/questionnaire-enable-behavior)
  TFhirEnableWhenBehaviorEnum = (
    EnableWhenBehaviorNull, // Value is missing from Instance
    EnableWhenBehaviorAll,
    EnableWhenBehaviorAny);
  TFhirEnableWhenBehaviorEnumList = set of TFhirEnableWhenBehaviorEnum;

  // The status of the location. (from http://hl7.org/fhir/ValueSet/encounter-location-status)
  TFhirEncounterLocationStatusEnum = (
    EncounterLocationStatusNull, // Value is missing from Instance
    EncounterLocationStatusPlanned,
    EncounterLocationStatusActive,
    EncounterLocationStatusReserved,
    EncounterLocationStatusCompleted);
  TFhirEncounterLocationStatusEnumList = set of TFhirEncounterLocationStatusEnum;

  // Current state of the encounter. (from http://hl7.org/fhir/ValueSet/encounter-status)
  TFhirEncounterStatusEnum = (
    EncounterStatusNull, // Value is missing from Instance
    EncounterStatusPlanned,
    EncounterStatusInProgress,
    EncounterStatusOnhold,
    EncounterStatusCompleted,
    EncounterStatusCancelled,
    EncounterStatusEnteredInError,
    EncounterStatusUnknown);
  TFhirEncounterStatusEnumList = set of TFhirEncounterStatusEnum;

  // The status of the endpoint. (from http://hl7.org/fhir/ValueSet/endpoint-status)
  TFhirEndpointStatusEnum = (
    EndpointStatusNull, // Value is missing from Instance
    EndpointStatusActive,
    EndpointStatusSuspended,
    EndpointStatusError,
    EndpointStatusOff,
    EndpointStatusEnteredInError,
    EndpointStatusTest);
  TFhirEndpointStatusEnumList = set of TFhirEndpointStatusEnum;

  // The status of the episode of care. (from http://hl7.org/fhir/ValueSet/episode-of-care-status)
  TFhirEpisodeOfCareStatusEnum = (
    EpisodeOfCareStatusNull, // Value is missing from Instance
    EpisodeOfCareStatusPlanned,
    EpisodeOfCareStatusWaitlist,
    EpisodeOfCareStatusActive,
    EpisodeOfCareStatusOnhold,
    EpisodeOfCareStatusFinished,
    EpisodeOfCareStatusCancelled,
    EpisodeOfCareStatusEnteredInError);
  TFhirEpisodeOfCareStatusEnumList = set of TFhirEpisodeOfCareStatusEnum;

  // The mode of a message capability statement. (from http://hl7.org/fhir/ValueSet/event-capability-mode)
  TFhirEventCapabilityModeEnum = (
    EventCapabilityModeNull, // Value is missing from Instance
    EventCapabilityModeSender,
    EventCapabilityModeReceiver);
  TFhirEventCapabilityModeEnumList = set of TFhirEventCapabilityModeEnum;

  // Codes identifying the lifecycle stage of an event. (from http://hl7.org/fhir/ValueSet/event-status)
  TFhirEventStatusEnum = (
    EventStatusNull, // Value is missing from Instance
    EventStatusPreparation,
    EventStatusInProgress,
    EventStatusNotDone,
    EventStatusOnHold,
    EventStatusStopped,
    EventStatusCompleted,
    EventStatusEnteredInError,
    EventStatusUnknown);
  TFhirEventStatusEnumList = set of TFhirEventStatusEnum;

  // Real world event relating to the schedule. (from http://hl7.org/fhir/ValueSet/event-timing)
  TFhirEventTimingEnum = (
    EventTimingNull, // Value is missing from Instance
    EventTimingMORN,
    EventTimingMORNEarly,
    EventTimingMORNLate,
    EventTimingNOON,
    EventTimingAFT,
    EventTimingAFTEarly,
    EventTimingAFTLate,
    EventTimingEVE,
    EventTimingEVEEarly,
    EventTimingEVELate,
    EventTimingNIGHT,
    EventTimingPHS,
    EventTimingHS,
    EventTimingWAKE,
    EventTimingC,
    EventTimingCM,
    EventTimingCD,
    EventTimingCV,
    EventTimingAC,
    EventTimingACM,
    EventTimingACD,
    EventTimingACV,
    EventTimingPC,
    EventTimingPCM,
    EventTimingPCD,
    EventTimingPCV);
  TFhirEventTimingEnumList = set of TFhirEventTimingEnum;

  // The handling of the variable in statistical analysis for exposures or outcomes (E.g. Dichotomous, Continuous, Descriptive). (from http://hl7.org/fhir/ValueSet/variable-handling)
  TFhirEvidenceVariableHandlingEnum = (
    EvidenceVariableHandlingNull, // Value is missing from Instance
    EvidenceVariableHandlingContinuous,
    EvidenceVariableHandlingDichotomous,
    EvidenceVariableHandlingOrdinal,
    EvidenceVariableHandlingPolychotomous);
  TFhirEvidenceVariableHandlingEnumList = set of TFhirEvidenceVariableHandlingEnum;

  // The type of actor - system or human. (from http://hl7.org/fhir/ValueSet/examplescenario-actor-type)
  TFhirExampleScenarioActorTypeEnum = (
    ExampleScenarioActorTypeNull, // Value is missing from Instance
    ExampleScenarioActorTypePerson,
    ExampleScenarioActorTypeEntity);
  TFhirExampleScenarioActorTypeEnumList = set of TFhirExampleScenarioActorTypeEnum;

  // A code specifying the state of the resource instance. (from http://hl7.org/fhir/ValueSet/explanationofbenefit-status)
  TFhirExplanationOfBenefitStatusEnum = (
    ExplanationOfBenefitStatusNull, // Value is missing from Instance
    ExplanationOfBenefitStatusActive,
    ExplanationOfBenefitStatusCancelled,
    ExplanationOfBenefitStatusDraft,
    ExplanationOfBenefitStatusEnteredInError);
  TFhirExplanationOfBenefitStatusEnumList = set of TFhirExplanationOfBenefitStatusEnum;

  // How an extension context is interpreted. (from http://hl7.org/fhir/ValueSet/extension-context-type)
  TFhirExtensionContextTypeEnum = (
    ExtensionContextTypeNull, // Value is missing from Instance
    ExtensionContextTypeFhirpath,
    ExtensionContextTypeElement,
    ExtensionContextTypeExtension);
  TFhirExtensionContextTypeEnumList = set of TFhirExtensionContextTypeEnum;

  // A list of all the concrete types defined in this version of the FHIR specification - Data Types and Resource Types. (from http://hl7.org/fhir/ValueSet/defined-types)
  TFhirFHIRDefinedTypeEnum = (
    FHIRDefinedTypeNull, // Value is missing from Instance
    FHIRDefinedTypeAddress,
    FHIRDefinedTypeAge,
    FHIRDefinedTypeAnnotation,
    FHIRDefinedTypeAttachment,
    FHIRDefinedTypeBackboneElement,
    FHIRDefinedTypeBackboneType,
    FHIRDefinedTypeBase,
    FHIRDefinedTypeCodeableConcept,
    FHIRDefinedTypeCodeableReference,
    FHIRDefinedTypeCoding,
    FHIRDefinedTypeContactDetail,
    FHIRDefinedTypeContactPoint,
    FHIRDefinedTypeContributor,
    FHIRDefinedTypeCount,
    FHIRDefinedTypeDataRequirement,
    FHIRDefinedTypeDataType,
    FHIRDefinedTypeDistance,
    FHIRDefinedTypeDosage,
    FHIRDefinedTypeDuration,
    FHIRDefinedTypeElement,
    FHIRDefinedTypeElementDefinition,
    FHIRDefinedTypeExpression,
    FHIRDefinedTypeExtension,
    FHIRDefinedTypeHumanName,
    FHIRDefinedTypeIdentifier,
    FHIRDefinedTypeMarketingStatus,
    FHIRDefinedTypeMeta,
    FHIRDefinedTypeMoney,
    FHIRDefinedTypeMoneyQuantity,
    FHIRDefinedTypeNarrative,
    FHIRDefinedTypeOrderedDistribution,
    FHIRDefinedTypeParameterDefinition,
    FHIRDefinedTypePeriod,
    FHIRDefinedTypePopulation,
    FHIRDefinedTypePrimitiveType,
    FHIRDefinedTypeProdCharacteristic,
    FHIRDefinedTypeProductShelfLife,
    FHIRDefinedTypeQuantity,
    FHIRDefinedTypeRange,
    FHIRDefinedTypeRatio,
    FHIRDefinedTypeReference,
    FHIRDefinedTypeRelatedArtifact,
    FHIRDefinedTypeSampledData,
    FHIRDefinedTypeSignature,
    FHIRDefinedTypeSimpleQuantity,
    FHIRDefinedTypeStatistic,
    FHIRDefinedTypeTiming,
    FHIRDefinedTypeTriggerDefinition,
    FHIRDefinedTypeUsageContext,
    FHIRDefinedTypeBase64Binary,
    FHIRDefinedTypeBoolean,
    FHIRDefinedTypeCanonical,
    FHIRDefinedTypeCode,
    FHIRDefinedTypeDate,
    FHIRDefinedTypeDateTime,
    FHIRDefinedTypeDecimal,
    FHIRDefinedTypeId,
    FHIRDefinedTypeInstant,
    FHIRDefinedTypeInteger,
    FHIRDefinedTypeInteger64,
    FHIRDefinedTypeMarkdown,
    FHIRDefinedTypeOid,
    FHIRDefinedTypePositiveInt,
    FHIRDefinedTypeString,
    FHIRDefinedTypeTime,
    FHIRDefinedTypeUnsignedInt,
    FHIRDefinedTypeUri,
    FHIRDefinedTypeUrl,
    FHIRDefinedTypeUuid,
    FHIRDefinedTypeXhtml,
    FHIRDefinedTypeAccount,
    FHIRDefinedTypeActivityDefinition,
    FHIRDefinedTypeAdministrableProductDefinition,
    FHIRDefinedTypeAdverseEvent,
    FHIRDefinedTypeAllergyIntolerance,
    FHIRDefinedTypeAppointment,
    FHIRDefinedTypeAppointmentResponse,
    FHIRDefinedTypeAuditEvent,
    FHIRDefinedTypeBasic,
    FHIRDefinedTypeBinary,
    FHIRDefinedTypeBiologicallyDerivedProduct,
    FHIRDefinedTypeBodyStructure,
    FHIRDefinedTypeBundle,
    FHIRDefinedTypeCapabilityStatement,
    FHIRDefinedTypeCapabilityStatement2,
    FHIRDefinedTypeCarePlan,
    FHIRDefinedTypeCareTeam,
    FHIRDefinedTypeCatalogEntry,
    FHIRDefinedTypeChargeItem,
    FHIRDefinedTypeChargeItemDefinition,
    FHIRDefinedTypeCitation,
    FHIRDefinedTypeClaim,
    FHIRDefinedTypeClaimResponse,
    FHIRDefinedTypeClinicalImpression,
    FHIRDefinedTypeClinicalUseIssue,
    FHIRDefinedTypeCodeSystem,
    FHIRDefinedTypeCommunication,
    FHIRDefinedTypeCommunicationRequest,
    FHIRDefinedTypeCompartmentDefinition,
    FHIRDefinedTypeComposition,
    FHIRDefinedTypeConceptMap,
    FHIRDefinedTypeCondition,
    FHIRDefinedTypeConditionDefinition,
    FHIRDefinedTypeConsent,
    FHIRDefinedTypeContract,
    FHIRDefinedTypeCoverage,
    FHIRDefinedTypeCoverageEligibilityRequest,
    FHIRDefinedTypeCoverageEligibilityResponse,
    FHIRDefinedTypeDetectedIssue,
    FHIRDefinedTypeDevice,
    FHIRDefinedTypeDeviceDefinition,
    FHIRDefinedTypeDeviceMetric,
    FHIRDefinedTypeDeviceRequest,
    FHIRDefinedTypeDeviceUseStatement,
    FHIRDefinedTypeDiagnosticReport,
    FHIRDefinedTypeDocumentManifest,
    FHIRDefinedTypeDocumentReference,
    FHIRDefinedTypeDomainResource,
    FHIRDefinedTypeEncounter,
    FHIRDefinedTypeEndpoint,
    FHIRDefinedTypeEnrollmentRequest,
    FHIRDefinedTypeEnrollmentResponse,
    FHIRDefinedTypeEpisodeOfCare,
    FHIRDefinedTypeEventDefinition,
    FHIRDefinedTypeEvidence,
    FHIRDefinedTypeEvidenceReport,
    FHIRDefinedTypeEvidenceVariable,
    FHIRDefinedTypeExampleScenario,
    FHIRDefinedTypeExplanationOfBenefit,
    FHIRDefinedTypeFamilyMemberHistory,
    FHIRDefinedTypeFlag,
    FHIRDefinedTypeGoal,
    FHIRDefinedTypeGraphDefinition,
    FHIRDefinedTypeGroup,
    FHIRDefinedTypeGuidanceResponse,
    FHIRDefinedTypeHealthcareService,
    FHIRDefinedTypeImagingStudy,
    FHIRDefinedTypeImmunization,
    FHIRDefinedTypeImmunizationEvaluation,
    FHIRDefinedTypeImmunizationRecommendation,
    FHIRDefinedTypeImplementationGuide,
    FHIRDefinedTypeIngredient,
    FHIRDefinedTypeInsurancePlan,
    FHIRDefinedTypeInvoice,
    FHIRDefinedTypeLibrary,
    FHIRDefinedTypeLinkage,
    FHIRDefinedTypeList,
    FHIRDefinedTypeLocation,
    FHIRDefinedTypeManufacturedItemDefinition,
    FHIRDefinedTypeMeasure,
    FHIRDefinedTypeMeasureReport,
    FHIRDefinedTypeMedication,
    FHIRDefinedTypeMedicationAdministration,
    FHIRDefinedTypeMedicationDispense,
    FHIRDefinedTypeMedicationKnowledge,
    FHIRDefinedTypeMedicationRequest,
    FHIRDefinedTypeMedicationUsage,
    FHIRDefinedTypeMedicinalProductDefinition,
    FHIRDefinedTypeMessageDefinition,
    FHIRDefinedTypeMessageHeader,
    FHIRDefinedTypeMolecularSequence,
    FHIRDefinedTypeNamingSystem,
    FHIRDefinedTypeNutritionIntake,
    FHIRDefinedTypeNutritionOrder,
    FHIRDefinedTypeNutritionProduct,
    FHIRDefinedTypeObservation,
    FHIRDefinedTypeObservationDefinition,
    FHIRDefinedTypeOperationDefinition,
    FHIRDefinedTypeOperationOutcome,
    FHIRDefinedTypeOrganization,
    FHIRDefinedTypeOrganizationAffiliation,
    FHIRDefinedTypePackagedProductDefinition,
    FHIRDefinedTypeParameters,
    FHIRDefinedTypePatient,
    FHIRDefinedTypePaymentNotice,
    FHIRDefinedTypePaymentReconciliation,
    FHIRDefinedTypePermission,
    FHIRDefinedTypePerson,
    FHIRDefinedTypePlanDefinition,
    FHIRDefinedTypePractitioner,
    FHIRDefinedTypePractitionerRole,
    FHIRDefinedTypeProcedure,
    FHIRDefinedTypeProvenance,
    FHIRDefinedTypeQuestionnaire,
    FHIRDefinedTypeQuestionnaireResponse,
    FHIRDefinedTypeRegulatedAuthorization,
    FHIRDefinedTypeRelatedPerson,
    FHIRDefinedTypeRequestGroup,
    FHIRDefinedTypeResearchStudy,
    FHIRDefinedTypeResearchSubject,
    FHIRDefinedTypeResource,
    FHIRDefinedTypeRiskAssessment,
    FHIRDefinedTypeSchedule,
    FHIRDefinedTypeSearchParameter,
    FHIRDefinedTypeServiceRequest,
    FHIRDefinedTypeSlot,
    FHIRDefinedTypeSpecimen,
    FHIRDefinedTypeSpecimenDefinition,
    FHIRDefinedTypeStructureDefinition,
    FHIRDefinedTypeStructureMap,
    FHIRDefinedTypeSubscription,
    FHIRDefinedTypeSubscriptionStatus,
    FHIRDefinedTypeSubscriptionTopic,
    FHIRDefinedTypeSubstance,
    FHIRDefinedTypeSubstanceDefinition,
    FHIRDefinedTypeSubstanceNucleicAcid,
    FHIRDefinedTypeSubstancePolymer,
    FHIRDefinedTypeSubstanceProtein,
    FHIRDefinedTypeSubstanceReferenceInformation,
    FHIRDefinedTypeSubstanceSourceMaterial,
    FHIRDefinedTypeSupplyDelivery,
    FHIRDefinedTypeSupplyRequest,
    FHIRDefinedTypeTask,
    FHIRDefinedTypeTerminologyCapabilities,
    FHIRDefinedTypeTestReport,
    FHIRDefinedTypeTestScript,
    FHIRDefinedTypeValueSet,
    FHIRDefinedTypeVerificationResult,
    FHIRDefinedTypeVisionPrescription);
  TFhirFHIRDefinedTypeEnumList = set of TFhirFHIRDefinedTypeEnum;

  // The availability status of the device. (from http://hl7.org/fhir/ValueSet/device-status)
  TFhirFHIRDeviceStatusEnum = (
    FHIRDeviceStatusNull, // Value is missing from Instance
    FHIRDeviceStatusActive,
    FHIRDeviceStatusInactive,
    FHIRDeviceStatusEnteredInError,
    FHIRDeviceStatusUnknown);
  TFhirFHIRDeviceStatusEnumList = set of TFhirFHIRDeviceStatusEnum;

  // A code to indicate if the substance is actively used. (from http://hl7.org/fhir/ValueSet/substance-status)
  TFhirFHIRSubstanceStatusEnum = (
    FHIRSubstanceStatusNull, // Value is missing from Instance
    FHIRSubstanceStatusActive,
    FHIRSubstanceStatusInactive,
    FHIRSubstanceStatusEnteredInError);
  TFhirFHIRSubstanceStatusEnumList = set of TFhirFHIRSubstanceStatusEnum;

  // All published FHIR Versions. (from http://hl7.org/fhir/ValueSet/FHIR-version)
  TFhirFHIRVersionEnum = (
    FHIRVersionNull, // Value is missing from Instance
    FHIRVersion001,
    FHIRVersion005,
    FHIRVersion006,
    FHIRVersion011,
    FHIRVersion0080,
    FHIRVersion0081,
    FHIRVersion0082,
    FHIRVersion040,
    FHIRVersion050,
    FHIRVersion100,
    FHIRVersion101,
    FHIRVersion102,
    FHIRVersion110,
    FHIRVersion140,
    FHIRVersion160,
    FHIRVersion180,
    FHIRVersion300,
    FHIRVersion301,
    FHIRVersion302,
    FHIRVersion330,
    FHIRVersion350,
    FHIRVersion400,
    FHIRVersion401,
    FHIRVersion410,
    FHIRVersion420,
    FHIRVersion440,
    FHIRVersion450);
  TFhirFHIRVersionEnumList = set of TFhirFHIRVersionEnum;

  // A code that identifies the status of the family history record. (from http://hl7.org/fhir/ValueSet/history-status)
  TFhirFamilyHistoryStatusEnum = (
    FamilyHistoryStatusNull, // Value is missing from Instance
    FamilyHistoryStatusPartial,
    FamilyHistoryStatusCompleted,
    FamilyHistoryStatusEnteredInError,
    FamilyHistoryStatusHealthUnknown);
  TFhirFamilyHistoryStatusEnumList = set of TFhirFamilyHistoryStatusEnum;

  // The kind of operation to perform as a part of a property based filter. (from http://hl7.org/fhir/ValueSet/filter-operator)
  TFhirFilterOperatorEnum = (
    FilterOperatorNull, // Value is missing from Instance
    FilterOperatorEqual,
    FilterOperatorIsA,
    FilterOperatorDescendentOf,
    FilterOperatorIsNotA,
    FilterOperatorRegex,
    FilterOperatorIn,
    FilterOperatorNotIn,
    FilterOperatorGeneralizes,
    FilterOperatorExists);
  TFhirFilterOperatorEnumList = set of TFhirFilterOperatorEnum;

  // This value set includes Status codes. (from http://hl7.org/fhir/ValueSet/fm-status)
  TFhirFinancialResourceStatusCodesEnum = (
    FinancialResourceStatusCodesNull, // Value is missing from Instance
    FinancialResourceStatusCodesActive,
    FinancialResourceStatusCodesCancelled,
    FinancialResourceStatusCodesDraft,
    FinancialResourceStatusCodesEnteredInError);
  TFhirFinancialResourceStatusCodesEnumList = set of TFhirFinancialResourceStatusCodesEnum;

  // Indicates whether this flag is active and needs to be displayed to a user, or whether it is no longer needed or was entered in error. (from http://hl7.org/fhir/ValueSet/flag-status)
  TFhirFlagStatusEnum = (
    FlagStatusNull, // Value is missing from Instance
    FlagStatusActive,
    FlagStatusInactive,
    FlagStatusEnteredInError);
  TFhirFlagStatusEnumList = set of TFhirFlagStatusEnum;

  // Codes that reflect the current state of a goal and whether the goal is still being targeted. (from http://hl7.org/fhir/ValueSet/goal-status)
  TFhirGoalLifecycleStatusEnum = (
    GoalLifecycleStatusNull, // Value is missing from Instance
    GoalLifecycleStatusProposed,
    GoalLifecycleStatusPlanned,
    GoalLifecycleStatusAccepted,
    GoalLifecycleStatusActive,
    GoalLifecycleStatusOnHold,
    GoalLifecycleStatusCompleted,
    GoalLifecycleStatusCancelled,
    GoalLifecycleStatusEnteredInError,
    GoalLifecycleStatusRejected);
  TFhirGoalLifecycleStatusEnumList = set of TFhirGoalLifecycleStatusEnum;

  // How a compartment must be linked. (from http://hl7.org/fhir/ValueSet/graph-compartment-rule)
  TFhirGraphCompartmentRuleEnum = (
    GraphCompartmentRuleNull, // Value is missing from Instance
    GraphCompartmentRuleIdentical,
    GraphCompartmentRuleMatching,
    GraphCompartmentRuleDifferent,
    GraphCompartmentRuleCustom);
  TFhirGraphCompartmentRuleEnumList = set of TFhirGraphCompartmentRuleEnum;

  // Defines how a compartment rule is used. (from http://hl7.org/fhir/ValueSet/graph-compartment-use)
  TFhirGraphCompartmentUseEnum = (
    GraphCompartmentUseNull, // Value is missing from Instance
    GraphCompartmentUseCondition,
    GraphCompartmentUseRequirement);
  TFhirGraphCompartmentUseEnumList = set of TFhirGraphCompartmentUseEnum;

  // Possible group measure aggregates (E.g. Mean, Median). (from http://hl7.org/fhir/ValueSet/group-measure)
  TFhirGroupMeasureEnum = (
    GroupMeasureNull, // Value is missing from Instance
    GroupMeasureMean,
    GroupMeasureMedian,
    GroupMeasureMeanOfMean,
    GroupMeasureMeanOfMedian,
    GroupMeasureMedianOfMean,
    GroupMeasureMedianOfMedian);
  TFhirGroupMeasureEnumList = set of TFhirGroupMeasureEnum;

  // Types of resources that are part of group. (from http://hl7.org/fhir/ValueSet/group-type)
  TFhirGroupTypeEnum = (
    GroupTypeNull, // Value is missing from Instance
    GroupTypePerson,
    GroupTypeAnimal,
    GroupTypePractitioner,
    GroupTypeDevice,
    GroupTypeMedication,
    GroupTypeSubstance);
  TFhirGroupTypeEnumList = set of TFhirGroupTypeEnum;

  // The status of a guidance response. (from http://hl7.org/fhir/ValueSet/guidance-response-status)
  TFhirGuidanceResponseStatusEnum = (
    GuidanceResponseStatusNull, // Value is missing from Instance
    GuidanceResponseStatusSuccess,
    GuidanceResponseStatusDataRequested,
    GuidanceResponseStatusDataRequired,
    GuidanceResponseStatusInProgress,
    GuidanceResponseStatusFailure,
    GuidanceResponseStatusEnteredInError);
  TFhirGuidanceResponseStatusEnumList = set of TFhirGuidanceResponseStatusEnum;

  // A code that indicates how the page is generated. (from http://hl7.org/fhir/ValueSet/guide-page-generation)
  TFhirGuidePageGenerationEnum = (
    GuidePageGenerationNull, // Value is missing from Instance
    GuidePageGenerationHtml,
    GuidePageGenerationMarkdown,
    GuidePageGenerationXml,
    GuidePageGenerationGenerated);
  TFhirGuidePageGenerationEnumList = set of TFhirGuidePageGenerationEnum;

  // HTTP verbs (in the HTTP command line). See [HTTP rfc](https://tools.ietf.org/html/rfc7231) for details. (from http://hl7.org/fhir/ValueSet/http-verb)
  TFhirHTTPVerbEnum = (
    HTTPVerbNull, // Value is missing from Instance
    HTTPVerbGET,
    HTTPVerbHEAD,
    HTTPVerbPOST,
    HTTPVerbPUT,
    HTTPVerbDELETE,
    HTTPVerbPATCH);
  TFhirHTTPVerbEnumList = set of TFhirHTTPVerbEnum;

  // Identifies the purpose for this identifier, if known . (from http://hl7.org/fhir/ValueSet/identifier-use)
  TFhirIdentifierUseEnum = (
    IdentifierUseNull, // Value is missing from Instance
    IdentifierUseUsual,
    IdentifierUseOfficial,
    IdentifierUseTemp,
    IdentifierUseSecondary,
    IdentifierUseOld);
  TFhirIdentifierUseEnumList = set of TFhirIdentifierUseEnum;

  // The level of confidence that this link represents the same actual person, based on NIST Authentication Levels. (from http://hl7.org/fhir/ValueSet/identity-assuranceLevel)
  TFhirIdentityAssuranceLevelEnum = (
    IdentityAssuranceLevelNull, // Value is missing from Instance
    IdentityAssuranceLevelLevel1,
    IdentityAssuranceLevelLevel2,
    IdentityAssuranceLevelLevel3,
    IdentityAssuranceLevelLevel4);
  TFhirIdentityAssuranceLevelEnumList = set of TFhirIdentityAssuranceLevelEnum;

  // The status of the ImagingStudy. (from http://hl7.org/fhir/ValueSet/imagingstudy-status)
  TFhirImagingStudyStatusEnum = (
    ImagingStudyStatusNull, // Value is missing from Instance
    ImagingStudyStatusRegistered,
    ImagingStudyStatusAvailable,
    ImagingStudyStatusCancelled,
    ImagingStudyStatusEnteredInError,
    ImagingStudyStatusUnknown);
  TFhirImagingStudyStatusEnumList = set of TFhirImagingStudyStatusEnum;

  // The value set to instantiate this attribute should be drawn from a terminologically robust code system that consists of or contains concepts to support describing the current status of the evaluation for vaccine administration event. (from http://hl7.org/fhir/ValueSet/immunization-evaluation-status)
  TFhirImmunizationEvaluationStatusCodesEnum = (
    ImmunizationEvaluationStatusCodesNull, // Value is missing from Instance
    ImmunizationEvaluationStatusCodesCompleted,
    ImmunizationEvaluationStatusCodesEnteredInError);
  TFhirImmunizationEvaluationStatusCodesEnumList = set of TFhirImmunizationEvaluationStatusCodesEnum;

  // The value set to instantiate this attribute should be drawn from a terminologically robust code system that consists of or contains concepts to support describing the current status of the administered dose of vaccine. (from http://hl7.org/fhir/ValueSet/immunization-status)
  TFhirImmunizationStatusCodesEnum = (
    ImmunizationStatusCodesNull, // Value is missing from Instance
    ImmunizationStatusCodesCompleted,
    ImmunizationStatusCodesEnteredInError,
    ImmunizationStatusCodesNotDone);
  TFhirImmunizationStatusCodesEnumList = set of TFhirImmunizationStatusCodesEnum;

  // FHIR RESTful interaction codes used for SubscriptionTopic trigger. (from http://hl7.org/fhir/ValueSet/interaction-trigger)
  TFhirInteractionTriggerEnum = (
    InteractionTriggerNull, // Value is missing from Instance
    InteractionTriggerCreate,
    InteractionTriggerUpdate,
    InteractionTriggerDelete);
  TFhirInteractionTriggerEnumList = set of TFhirInteractionTriggerEnum;

  // Codes indicating the kind of the price component. (from http://hl7.org/fhir/ValueSet/invoice-priceComponentType)
  TFhirInvoicePriceComponentTypeEnum = (
    InvoicePriceComponentTypeNull, // Value is missing from Instance
    InvoicePriceComponentTypeBase,
    InvoicePriceComponentTypeSurcharge,
    InvoicePriceComponentTypeDeduction,
    InvoicePriceComponentTypeDiscount,
    InvoicePriceComponentTypeTax,
    InvoicePriceComponentTypeInformational);
  TFhirInvoicePriceComponentTypeEnumList = set of TFhirInvoicePriceComponentTypeEnum;

  // Codes identifying the lifecycle stage of an Invoice. (from http://hl7.org/fhir/ValueSet/invoice-status)
  TFhirInvoiceStatusEnum = (
    InvoiceStatusNull, // Value is missing from Instance
    InvoiceStatusDraft,
    InvoiceStatusIssued,
    InvoiceStatusBalanced,
    InvoiceStatusCancelled,
    InvoiceStatusEnteredInError);
  TFhirInvoiceStatusEnumList = set of TFhirInvoiceStatusEnum;

  // How the issue affects the success of the action. (from http://hl7.org/fhir/ValueSet/issue-severity)
  TFhirIssueSeverityEnum = (
    IssueSeverityNull, // Value is missing from Instance
    IssueSeverityFatal,
    IssueSeverityError,
    IssueSeverityWarning,
    IssueSeverityInformation);
  TFhirIssueSeverityEnumList = set of TFhirIssueSeverityEnum;

  // A code that describes the type of issue. (from http://hl7.org/fhir/ValueSet/issue-type)
  TFhirIssueTypeEnum = (
    IssueTypeNull, // Value is missing from Instance
    IssueTypeInvalid,
    IssueTypeStructure,
    IssueTypeRequired,
    IssueTypeValue,
    IssueTypeInvariant,
    IssueTypeSecurity,
    IssueTypeLogin,
    IssueTypeUnknown,
    IssueTypeExpired,
    IssueTypeForbidden,
    IssueTypeSuppressed,
    IssueTypeProcessing,
    IssueTypeNotSupported,
    IssueTypeDuplicate,
    IssueTypeMultipleMatches,
    IssueTypeNotFound,
    IssueTypeDeleted,
    IssueTypeTooLong,
    IssueTypeCodeInvalid,
    IssueTypeExtension,
    IssueTypeTooCostly,
    IssueTypeBusinessRule,
    IssueTypeConflict,
    IssueTypeTransient,
    IssueTypeLockError,
    IssueTypeNoStore,
    IssueTypeException,
    IssueTypeTimeout,
    IssueTypeIncomplete,
    IssueTypeThrottled,
    IssueTypeInformational);
  TFhirIssueTypeEnumList = set of TFhirIssueTypeEnum;

  // The type of link between this patient resource and another patient resource. (from http://hl7.org/fhir/ValueSet/link-type)
  TFhirLinkTypeEnum = (
    LinkTypeNull, // Value is missing from Instance
    LinkTypeReplacedBy,
    LinkTypeReplaces,
    LinkTypeRefer,
    LinkTypeSeealso);
  TFhirLinkTypeEnumList = set of TFhirLinkTypeEnum;

  // Used to distinguish different roles a resource can play within a set of linked resources. (from http://hl7.org/fhir/ValueSet/linkage-type)
  TFhirLinkageTypeEnum = (
    LinkageTypeNull, // Value is missing from Instance
    LinkageTypeSource,
    LinkageTypeAlternate,
    LinkageTypeHistorical);
  TFhirLinkageTypeEnumList = set of TFhirLinkageTypeEnum;

  // The processing mode that applies to this list. (from http://hl7.org/fhir/ValueSet/list-mode)
  TFhirListModeEnum = (
    ListModeNull, // Value is missing from Instance
    ListModeWorking,
    ListModeSnapshot,
    ListModeChanges);
  TFhirListModeEnumList = set of TFhirListModeEnum;

  // The current state of the list. (from http://hl7.org/fhir/ValueSet/list-status)
  TFhirListStatusEnum = (
    ListStatusNull, // Value is missing from Instance
    ListStatusCurrent,
    ListStatusRetired,
    ListStatusEnteredInError);
  TFhirListStatusEnumList = set of TFhirListStatusEnum;

  // Indicates whether a resource instance represents a specific location or a class of locations. (from http://hl7.org/fhir/ValueSet/location-mode)
  TFhirLocationModeEnum = (
    LocationModeNull, // Value is missing from Instance
    LocationModeInstance,
    LocationModeKind);
  TFhirLocationModeEnumList = set of TFhirLocationModeEnum;

  // Indicates whether the location is still in use. (from http://hl7.org/fhir/ValueSet/location-status)
  TFhirLocationStatusEnum = (
    LocationStatusNull, // Value is missing from Instance
    LocationStatusActive,
    LocationStatusSuspended,
    LocationStatusInactive);
  TFhirLocationStatusEnumList = set of TFhirLocationStatusEnum;

  // The status of the measure report. (from http://hl7.org/fhir/ValueSet/measure-report-status)
  TFhirMeasureReportStatusEnum = (
    MeasureReportStatusNull, // Value is missing from Instance
    MeasureReportStatusComplete,
    MeasureReportStatusPending,
    MeasureReportStatusError);
  TFhirMeasureReportStatusEnumList = set of TFhirMeasureReportStatusEnum;

  // The type of the measure report. (from http://hl7.org/fhir/ValueSet/measure-report-type)
  TFhirMeasureReportTypeEnum = (
    MeasureReportTypeNull, // Value is missing from Instance
    MeasureReportTypeIndividual,
    MeasureReportTypeSubjectList,
    MeasureReportTypeSummary,
    MeasureReportTypeDataCollection);
  TFhirMeasureReportTypeEnumList = set of TFhirMeasureReportTypeEnum;

  // MedicationAdministration Status Codes (from http://hl7.org/fhir/ValueSet/medication-admin-status)
  TFhirMedicationAdministrationStatusCodesEnum = (
    MedicationAdministrationStatusCodesNull, // Value is missing from Instance
    MedicationAdministrationStatusCodesInProgress,
    MedicationAdministrationStatusCodesNotDone,
    MedicationAdministrationStatusCodesOnHold,
    MedicationAdministrationStatusCodesCompleted,
    MedicationAdministrationStatusCodesEnteredInError,
    MedicationAdministrationStatusCodesStopped,
    MedicationAdministrationStatusCodesUnknown);
  TFhirMedicationAdministrationStatusCodesEnumList = set of TFhirMedicationAdministrationStatusCodesEnum;

  // MedicationDispense Status Codes (from http://hl7.org/fhir/ValueSet/medicationdispense-status)
  TFhirMedicationDispenseStatusCodesEnum = (
    MedicationDispenseStatusCodesNull, // Value is missing from Instance
    MedicationDispenseStatusCodesPreparation,
    MedicationDispenseStatusCodesInProgress,
    MedicationDispenseStatusCodesCancelled,
    MedicationDispenseStatusCodesOnHold,
    MedicationDispenseStatusCodesCompleted,
    MedicationDispenseStatusCodesEnteredInError,
    MedicationDispenseStatusCodesStopped,
    MedicationDispenseStatusCodesDeclined,
    MedicationDispenseStatusCodesUnknown);
  TFhirMedicationDispenseStatusCodesEnumList = set of TFhirMedicationDispenseStatusCodesEnum;

  // MedicationKnowledge Status Codes (from http://hl7.org/fhir/ValueSet/medicationknowledge-status)
  TFhirMedicationKnowledgeStatusCodesEnum = (
    MedicationKnowledgeStatusCodesNull, // Value is missing from Instance
    MedicationKnowledgeStatusCodesActive,
    MedicationKnowledgeStatusCodesInactive,
    MedicationKnowledgeStatusCodesEnteredInError);
  TFhirMedicationKnowledgeStatusCodesEnumList = set of TFhirMedicationKnowledgeStatusCodesEnum;

  // MedicationRequest Intent Codes (from http://hl7.org/fhir/ValueSet/medicationrequest-intent)
  TFhirMedicationRequestIntentEnum = (
    MedicationRequestIntentNull, // Value is missing from Instance
    MedicationRequestIntentProposal,
    MedicationRequestIntentPlan,
    MedicationRequestIntentOrder,
    MedicationRequestIntentOriginalOrder,
    MedicationRequestIntentReflexOrder,
    MedicationRequestIntentFillerOrder,
    MedicationRequestIntentInstanceOrder,
    MedicationRequestIntentOption);
  TFhirMedicationRequestIntentEnumList = set of TFhirMedicationRequestIntentEnum;

  // Medication Status Codes (from http://hl7.org/fhir/ValueSet/medication-status)
  TFhirMedicationStatusCodesEnum = (
    MedicationStatusCodesNull, // Value is missing from Instance
    MedicationStatusCodesActive,
    MedicationStatusCodesInactive,
    MedicationStatusCodesEnteredInError);
  TFhirMedicationStatusCodesEnumList = set of TFhirMedicationStatusCodesEnum;

  // MedicationUsage Status Codes (from http://hl7.org/fhir/ValueSet/medication-usage-status)
  TFhirMedicationUsageStatusCodesEnum = (
    MedicationUsageStatusCodesNull, // Value is missing from Instance
    MedicationUsageStatusCodesActive,
    MedicationUsageStatusCodesCompleted,
    MedicationUsageStatusCodesEnteredInError,
    MedicationUsageStatusCodesIntended,
    MedicationUsageStatusCodesStopped,
    MedicationUsageStatusCodesOnHold,
    MedicationUsageStatusCodesUnknown,
    MedicationUsageStatusCodesNotTaken);
  TFhirMedicationUsageStatusCodesEnumList = set of TFhirMedicationUsageStatusCodesEnum;

  // MedicationRequest Status Codes (from http://hl7.org/fhir/ValueSet/medicationrequest-status)
  TFhirMedicationrequestStatusEnum = (
    MedicationrequestStatusNull, // Value is missing from Instance
    MedicationrequestStatusActive,
    MedicationrequestStatusOnHold,
    MedicationrequestStatusCancelled,
    MedicationrequestStatusCompleted,
    MedicationrequestStatusEnteredInError,
    MedicationrequestStatusStopped,
    MedicationrequestStatusDraft,
    MedicationrequestStatusUnknown);
  TFhirMedicationrequestStatusEnumList = set of TFhirMedicationrequestStatusEnum;

  // The impact of the content of a message. (from http://hl7.org/fhir/ValueSet/message-significance-category)
  TFhirMessageSignificanceCategoryEnum = (
    MessageSignificanceCategoryNull, // Value is missing from Instance
    MessageSignificanceCategoryConsequence,
    MessageSignificanceCategoryCurrency,
    MessageSignificanceCategoryNotification);
  TFhirMessageSignificanceCategoryEnumList = set of TFhirMessageSignificanceCategoryEnum;

  // HL7-defined table of codes which identify conditions under which acknowledgments are required to be returned in response to a message. (from http://hl7.org/fhir/ValueSet/messageheader-response-request)
  TFhirMessageheaderResponseRequestEnum = (
    MessageheaderResponseRequestNull, // Value is missing from Instance
    MessageheaderResponseRequestAlways,
    MessageheaderResponseRequestOnError,
    MessageheaderResponseRequestNever,
    MessageheaderResponseRequestOnSuccess);
  TFhirMessageheaderResponseRequestEnumList = set of TFhirMessageheaderResponseRequestEnum;

  // The use of a human name. (from http://hl7.org/fhir/ValueSet/name-use)
  TFhirNameUseEnum = (
    NameUseNull, // Value is missing from Instance
    NameUseUsual,
    NameUseOfficial,
    NameUseTemp,
    NameUseNickname,
    NameUseAnonymous,
    NameUseOld,
    NameUseMaiden);
  TFhirNameUseEnumList = set of TFhirNameUseEnum;

  // Identifies the style of unique identifier used to identify a namespace. (from http://hl7.org/fhir/ValueSet/namingsystem-identifier-type)
  TFhirNamingSystemIdentifierTypeEnum = (
    NamingSystemIdentifierTypeNull, // Value is missing from Instance
    NamingSystemIdentifierTypeOid,
    NamingSystemIdentifierTypeUuid,
    NamingSystemIdentifierTypeUri,
    NamingSystemIdentifierTypeOther);
  TFhirNamingSystemIdentifierTypeEnumList = set of TFhirNamingSystemIdentifierTypeEnum;

  // Identifies the purpose of the naming system. (from http://hl7.org/fhir/ValueSet/namingsystem-type)
  TFhirNamingSystemTypeEnum = (
    NamingSystemTypeNull, // Value is missing from Instance
    NamingSystemTypeCodesystem,
    NamingSystemTypeIdentifier,
    NamingSystemTypeRoot);
  TFhirNamingSystemTypeEnumList = set of TFhirNamingSystemTypeEnum;

  // The status of a resource narrative. (from http://hl7.org/fhir/ValueSet/narrative-status)
  TFhirNarrativeStatusEnum = (
    NarrativeStatusNull, // Value is missing from Instance
    NarrativeStatusGenerated,
    NarrativeStatusExtensions,
    NarrativeStatusAdditional,
    NarrativeStatusEmpty);
  TFhirNarrativeStatusEnumList = set of TFhirNarrativeStatusEnum;

  // The presentation types of notes. (from http://hl7.org/fhir/ValueSet/note-type)
  TFhirNoteTypeEnum = (
    NoteTypeNull, // Value is missing from Instance
    NoteTypeDisplay,
    NoteTypePrint,
    NoteTypePrintoper);
  TFhirNoteTypeEnumList = set of TFhirNoteTypeEnum;

  // Codes identifying the lifecycle stage of a product. (from http://hl7.org/fhir/ValueSet/nutritionproduct-status)
  TFhirNutritionProductStatusEnum = (
    NutritionProductStatusNull, // Value is missing from Instance
    NutritionProductStatusActive,
    NutritionProductStatusInactive,
    NutritionProductStatusEnteredInError);
  TFhirNutritionProductStatusEnumList = set of TFhirNutritionProductStatusEnum;

  // Permitted data type for observation value. (from http://hl7.org/fhir/ValueSet/permitted-data-type)
  TFhirObservationDataTypeEnum = (
    ObservationDataTypeNull, // Value is missing from Instance
    ObservationDataTypeQuantity,
    ObservationDataTypeCodeableConcept,
    ObservationDataTypeString,
    ObservationDataTypeBoolean,
    ObservationDataTypeInteger,
    ObservationDataTypeRange,
    ObservationDataTypeRatio,
    ObservationDataTypeSampledData,
    ObservationDataTypeTime,
    ObservationDataTypeDateTime,
    ObservationDataTypePeriod);
  TFhirObservationDataTypeEnumList = set of TFhirObservationDataTypeEnum;

  // Codes identifying the category of observation range. (from http://hl7.org/fhir/ValueSet/observation-range-category)
  TFhirObservationRangeCategoryEnum = (
    ObservationRangeCategoryNull, // Value is missing from Instance
    ObservationRangeCategoryReference,
    ObservationRangeCategoryCritical,
    ObservationRangeCategoryAbsolute);
  TFhirObservationRangeCategoryEnumList = set of TFhirObservationRangeCategoryEnum;

  // Codes providing the status of an observation. (from http://hl7.org/fhir/ValueSet/observation-status)
  TFhirObservationStatusEnum = (
    ObservationStatusNull, // Value is missing from Instance
    ObservationStatusRegistered,
    ObservationStatusPreliminary,
    ObservationStatusFinal,
    ObservationStatusAmended,
    ObservationStatusCorrected,
    ObservationStatusCancelled,
    ObservationStatusEnteredInError,
    ObservationStatusUnknown);
  TFhirObservationStatusEnumList = set of TFhirObservationStatusEnum;

  // Whether an operation is a normal operation or a query. (from http://hl7.org/fhir/ValueSet/operation-kind)
  TFhirOperationKindEnum = (
    OperationKindNull, // Value is missing from Instance
    OperationKindOperation,
    OperationKindQuery);
  TFhirOperationKindEnumList = set of TFhirOperationKindEnum;

  // Whether an operation parameter is an input or an output parameter. (from http://hl7.org/fhir/ValueSet/operation-parameter-use)
  TFhirOperationParameterUseEnum = (
    OperationParameterUseNull, // Value is missing from Instance
    OperationParameterUseIn,
    OperationParameterUseOut);
  TFhirOperationParameterUseEnumList = set of TFhirOperationParameterUseEnum;

  // Type for orientation. (from http://hl7.org/fhir/ValueSet/orientation-type)
  TFhirOrientationTypeEnum = (
    OrientationTypeNull, // Value is missing from Instance
    OrientationTypeSense,
    OrientationTypeAntisense);
  TFhirOrientationTypeEnumList = set of TFhirOrientationTypeEnum;

  // Is the Participant required to attend the appointment. (from http://hl7.org/fhir/ValueSet/participantrequired)
  TFhirParticipantRequiredEnum = (
    ParticipantRequiredNull, // Value is missing from Instance
    ParticipantRequiredRequired,
    ParticipantRequiredOptional,
    ParticipantRequiredInformationOnly);
  TFhirParticipantRequiredEnumList = set of TFhirParticipantRequiredEnum;

  // The Participation status of an appointment. (from http://hl7.org/fhir/ValueSet/participationstatus)
  TFhirParticipationStatusEnum = (
    ParticipationStatusNull, // Value is missing from Instance
    ParticipationStatusAccepted,
    ParticipationStatusDeclined,
    ParticipationStatusTentative,
    ParticipationStatusNeedsAction);
  TFhirParticipationStatusEnumList = set of TFhirParticipationStatusEnum;

  // Codes identifying the lifecycle stage of a product. (from http://hl7.org/fhir/ValueSet/permission-status)
  TFhirPermissionStatusEnum = (
    PermissionStatusNull, // Value is missing from Instance
    PermissionStatusActive,
    PermissionStatusEnteredInError,
    PermissionStatusDraft,
    PermissionStatusRejected);
  TFhirPermissionStatusEnumList = set of TFhirPermissionStatusEnum;

  // How a property is represented when serialized. (from http://hl7.org/fhir/ValueSet/property-representation)
  TFhirPropertyRepresentationEnum = (
    PropertyRepresentationNull, // Value is missing from Instance
    PropertyRepresentationXmlAttr,
    PropertyRepresentationXmlText,
    PropertyRepresentationTypeAttr,
    PropertyRepresentationCdaText,
    PropertyRepresentationXhtml);
  TFhirPropertyRepresentationEnumList = set of TFhirPropertyRepresentationEnum;

  // How an entity was used in an activity. (from http://hl7.org/fhir/ValueSet/provenance-entity-role)
  TFhirProvenanceEntityRoleEnum = (
    ProvenanceEntityRoleNull, // Value is missing from Instance
    ProvenanceEntityRoleDerivation,
    ProvenanceEntityRoleRevision,
    ProvenanceEntityRoleQuotation,
    ProvenanceEntityRoleSource,
    ProvenanceEntityRoleRemoval);
  TFhirProvenanceEntityRoleEnumList = set of TFhirProvenanceEntityRoleEnum;

  // The lifecycle status of an artifact. (from http://hl7.org/fhir/ValueSet/publication-status)
  TFhirPublicationStatusEnum = (
    PublicationStatusNull, // Value is missing from Instance
    PublicationStatusDraft,
    PublicationStatusActive,
    PublicationStatusRetired,
    PublicationStatusUnknown);
  TFhirPublicationStatusEnumList = set of TFhirPublicationStatusEnum;

  // Type for quality report. (from http://hl7.org/fhir/ValueSet/quality-type)
  TFhirQualityTypeEnum = (
    QualityTypeNull, // Value is missing from Instance
    QualityTypeIndel,
    QualityTypeSnp,
    QualityTypeUnknown);
  TFhirQualityTypeEnumList = set of TFhirQualityTypeEnum;

  // How the Quantity should be understood and represented. (from http://hl7.org/fhir/ValueSet/quantity-comparator)
  TFhirQuantityComparatorEnum = (
    QuantityComparatorNull, // Value is missing from Instance
    QuantityComparatorLessThan,
    QuantityComparatorLessOrEquals,
    QuantityComparatorGreaterOrEquals,
    QuantityComparatorGreaterThan);
  TFhirQuantityComparatorEnumList = set of TFhirQuantityComparatorEnum;

  // The criteria by which a question is enabled. (from http://hl7.org/fhir/ValueSet/questionnaire-enable-operator)
  TFhirQuestionnaireItemOperatorEnum = (
    QuestionnaireItemOperatorNull, // Value is missing from Instance
    QuestionnaireItemOperatorExists,
    QuestionnaireItemOperatorEqual,
    QuestionnaireItemOperatorNotEqual,
    QuestionnaireItemOperatorGreaterThan,
    QuestionnaireItemOperatorLessThan,
    QuestionnaireItemOperatorGreaterOrEquals,
    QuestionnaireItemOperatorLessOrEquals);
  TFhirQuestionnaireItemOperatorEnumList = set of TFhirQuestionnaireItemOperatorEnum;

  // Distinguishes groups from questions and display text and indicates data type for questions. (from http://hl7.org/fhir/ValueSet/item-type)
  TFhirQuestionnaireItemTypeEnum = (
    QuestionnaireItemTypeNull, // Value is missing from Instance
    QuestionnaireItemTypeGroup,
    QuestionnaireItemTypeDisplay,
    QuestionnaireItemTypeQuestion,
    QuestionnaireItemTypeBoolean,
    QuestionnaireItemTypeDecimal,
    QuestionnaireItemTypeInteger,
    QuestionnaireItemTypeDate,
    QuestionnaireItemTypeDateTime,
    QuestionnaireItemTypeTime,
    QuestionnaireItemTypeString,
    QuestionnaireItemTypeText,
    QuestionnaireItemTypeUrl,
    QuestionnaireItemTypeChoice,
    QuestionnaireItemTypeOpenChoice,
    QuestionnaireItemTypeAttachment,
    QuestionnaireItemTypeReference,
    QuestionnaireItemTypeQuantity);
  TFhirQuestionnaireItemTypeEnumList = set of TFhirQuestionnaireItemTypeEnum;

  // Lifecycle status of the questionnaire response. (from http://hl7.org/fhir/ValueSet/questionnaire-answers-status)
  TFhirQuestionnaireResponseStatusEnum = (
    QuestionnaireResponseStatusNull, // Value is missing from Instance
    QuestionnaireResponseStatusInProgress,
    QuestionnaireResponseStatusCompleted,
    QuestionnaireResponseStatusAmended,
    QuestionnaireResponseStatusEnteredInError,
    QuestionnaireResponseStatusStopped);
  TFhirQuestionnaireResponseStatusEnumList = set of TFhirQuestionnaireResponseStatusEnum;

  // A set of flags that defines how references are supported. (from http://hl7.org/fhir/ValueSet/reference-handling-policy)
  TFhirReferenceHandlingPolicyEnum = (
    ReferenceHandlingPolicyNull, // Value is missing from Instance
    ReferenceHandlingPolicyLiteral,
    ReferenceHandlingPolicyLogical,
    ReferenceHandlingPolicyResolves,
    ReferenceHandlingPolicyEnforced,
    ReferenceHandlingPolicyLocal);
  TFhirReferenceHandlingPolicyEnumList = set of TFhirReferenceHandlingPolicyEnum;

  // Whether a reference needs to be version specific or version independent, or whether either can be used. (from http://hl7.org/fhir/ValueSet/reference-version-rules)
  TFhirReferenceVersionRulesEnum = (
    ReferenceVersionRulesNull, // Value is missing from Instance
    ReferenceVersionRulesEither,
    ReferenceVersionRulesIndependent,
    ReferenceVersionRulesSpecific);
  TFhirReferenceVersionRulesEnumList = set of TFhirReferenceVersionRulesEnum;

  // The type of relationship to the related artifact. (from http://hl7.org/fhir/ValueSet/related-artifact-type)
  TFhirRelatedArtifactTypeEnum = (
    RelatedArtifactTypeNull, // Value is missing from Instance
    RelatedArtifactTypeDocumentation,
    RelatedArtifactTypeJustification,
    RelatedArtifactTypeCitation,
    RelatedArtifactTypePredecessor,
    RelatedArtifactTypeSuccessor,
    RelatedArtifactTypeDerivedFrom,
    RelatedArtifactTypeDependsOn,
    RelatedArtifactTypeComposedOf);
  TFhirRelatedArtifactTypeEnumList = set of TFhirRelatedArtifactTypeEnum;

  // The type of relationship between reports. (from http://hl7.org/fhir/ValueSet/report-relation-type)
  TFhirReportRelationshipTypeEnum = (
    ReportRelationshipTypeNull, // Value is missing from Instance
    ReportRelationshipTypeReplaces,
    ReportRelationshipTypeAmends,
    ReportRelationshipTypeAppends,
    ReportRelationshipTypeTransforms);
  TFhirReportRelationshipTypeEnumList = set of TFhirReportRelationshipTypeEnum;

  // Type for access of external URI. (from http://hl7.org/fhir/ValueSet/repository-type)
  TFhirRepositoryTypeEnum = (
    RepositoryTypeNull, // Value is missing from Instance
    RepositoryTypeDirectlink,
    RepositoryTypeOpenapi,
    RepositoryTypeLogin,
    RepositoryTypeOauth,
    RepositoryTypeOther);
  TFhirRepositoryTypeEnumList = set of TFhirRepositoryTypeEnum;

  // Codes indicating the degree of authority/intentionality associated with a request. (from http://hl7.org/fhir/ValueSet/request-intent)
  TFhirRequestIntentEnum = (
    RequestIntentNull, // Value is missing from Instance
    RequestIntentProposal,
    RequestIntentPlan,
    RequestIntentDirective,
    RequestIntentOrder,
    RequestIntentOriginalOrder,
    RequestIntentReflexOrder,
    RequestIntentFillerOrder,
    RequestIntentInstanceOrder,
    RequestIntentOption);
  TFhirRequestIntentEnumList = set of TFhirRequestIntentEnum;

  // The clinical priority of a diagnostic order. (from http://hl7.org/fhir/ValueSet/request-priority)
  TFhirRequestPriorityEnum = (
    RequestPriorityNull, // Value is missing from Instance
    RequestPriorityRoutine,
    RequestPriorityUrgent,
    RequestPriorityAsap,
    RequestPriorityStat);
  TFhirRequestPriorityEnumList = set of TFhirRequestPriorityEnum;

  // A list of all the request resource types defined in this version of the FHIR specification. (from http://hl7.org/fhir/ValueSet/request-resource-types)
  TFhirRequestResourceTypeEnum = (
    RequestResourceTypeNull, // Value is missing from Instance
    RequestResourceTypeAppointment,
    RequestResourceTypeAppointmentResponse,
    RequestResourceTypeCarePlan,
    RequestResourceTypeClaim,
    RequestResourceTypeCommunicationRequest,
    RequestResourceTypeContract,
    RequestResourceTypeDeviceRequest,
    RequestResourceTypeEnrollmentRequest,
    RequestResourceTypeImmunizationRecommendation,
    RequestResourceTypeMedicationRequest,
    RequestResourceTypeNutritionOrder,
    RequestResourceTypeServiceRequest,
    RequestResourceTypeSupplyRequest,
    RequestResourceTypeTask,
    RequestResourceTypeVisionPrescription);
  TFhirRequestResourceTypeEnumList = set of TFhirRequestResourceTypeEnum;

  // Codes identifying the lifecycle stage of a request. (from http://hl7.org/fhir/ValueSet/request-status)
  TFhirRequestStatusEnum = (
    RequestStatusNull, // Value is missing from Instance
    RequestStatusDraft,
    RequestStatusActive,
    RequestStatusOnHold,
    RequestStatusRevoked,
    RequestStatusCompleted,
    RequestStatusEnteredInError,
    RequestStatusUnknown);
  TFhirRequestStatusEnumList = set of TFhirRequestStatusEnum;

  // Codes that convey the current status of the research study. (from http://hl7.org/fhir/ValueSet/research-study-status)
  TFhirResearchStudyStatusEnum = (
    ResearchStudyStatusNull, // Value is missing from Instance
    ResearchStudyStatusActive,
    ResearchStudyStatusAdministrativelyCompleted,
    ResearchStudyStatusApproved,
    ResearchStudyStatusClosedToAccrual,
    ResearchStudyStatusClosedToAccrualAndIntervention,
    ResearchStudyStatusCompleted,
    ResearchStudyStatusDisapproved,
    ResearchStudyStatusInReview,
    ResearchStudyStatusTemporarilyClosedToAccrual,
    ResearchStudyStatusTemporarilyClosedToAccrualAndIntervention,
    ResearchStudyStatusWithdrawn);
  TFhirResearchStudyStatusEnumList = set of TFhirResearchStudyStatusEnum;

  // Indicates the progression of a study subject through a study. (from http://hl7.org/fhir/ValueSet/research-subject-status)
  TFhirResearchSubjectStatusEnum = (
    ResearchSubjectStatusNull, // Value is missing from Instance
    ResearchSubjectStatusCandidate,
    ResearchSubjectStatusEligible,
    ResearchSubjectStatusFollowUp,
    ResearchSubjectStatusIneligible,
    ResearchSubjectStatusNotRegistered,
    ResearchSubjectStatusOffStudy,
    ResearchSubjectStatusOnStudy,
    ResearchSubjectStatusOnStudyIntervention,
    ResearchSubjectStatusOnStudyObservation,
    ResearchSubjectStatusPendingOnStudy,
    ResearchSubjectStatusPotentialCandidate,
    ResearchSubjectStatusScreening,
    ResearchSubjectStatusWithdrawn);
  TFhirResearchSubjectStatusEnumList = set of TFhirResearchSubjectStatusEnum;

  // One of the resource types defined as part of this version of FHIR. (from http://hl7.org/fhir/ValueSet/resource-types)
  TFhirResourceTypesEnum = (
    ResourceTypesNull, // Value is missing from Instance
    ResourceTypesAccount,
    ResourceTypesActivityDefinition,
    ResourceTypesAdministrableProductDefinition,
    ResourceTypesAdverseEvent,
    ResourceTypesAllergyIntolerance,
    ResourceTypesAppointment,
    ResourceTypesAppointmentResponse,
    ResourceTypesAuditEvent,
    ResourceTypesBasic,
    ResourceTypesBinary,
    ResourceTypesBiologicallyDerivedProduct,
    ResourceTypesBodyStructure,
    ResourceTypesBundle,
    ResourceTypesCapabilityStatement,
    ResourceTypesCapabilityStatement2,
    ResourceTypesCarePlan,
    ResourceTypesCareTeam,
    ResourceTypesCatalogEntry,
    ResourceTypesChargeItem,
    ResourceTypesChargeItemDefinition,
    ResourceTypesCitation,
    ResourceTypesClaim,
    ResourceTypesClaimResponse,
    ResourceTypesClinicalImpression,
    ResourceTypesClinicalUseIssue,
    ResourceTypesCodeSystem,
    ResourceTypesCommunication,
    ResourceTypesCommunicationRequest,
    ResourceTypesCompartmentDefinition,
    ResourceTypesComposition,
    ResourceTypesConceptMap,
    ResourceTypesCondition,
    ResourceTypesConditionDefinition,
    ResourceTypesConsent,
    ResourceTypesContract,
    ResourceTypesCoverage,
    ResourceTypesCoverageEligibilityRequest,
    ResourceTypesCoverageEligibilityResponse,
    ResourceTypesDetectedIssue,
    ResourceTypesDevice,
    ResourceTypesDeviceDefinition,
    ResourceTypesDeviceMetric,
    ResourceTypesDeviceRequest,
    ResourceTypesDeviceUseStatement,
    ResourceTypesDiagnosticReport,
    ResourceTypesDocumentManifest,
    ResourceTypesDocumentReference,
    ResourceTypesDomainResource,
    ResourceTypesEncounter,
    ResourceTypesEndpoint,
    ResourceTypesEnrollmentRequest,
    ResourceTypesEnrollmentResponse,
    ResourceTypesEpisodeOfCare,
    ResourceTypesEventDefinition,
    ResourceTypesEvidence,
    ResourceTypesEvidenceReport,
    ResourceTypesEvidenceVariable,
    ResourceTypesExampleScenario,
    ResourceTypesExplanationOfBenefit,
    ResourceTypesFamilyMemberHistory,
    ResourceTypesFlag,
    ResourceTypesGoal,
    ResourceTypesGraphDefinition,
    ResourceTypesGroup,
    ResourceTypesGuidanceResponse,
    ResourceTypesHealthcareService,
    ResourceTypesImagingStudy,
    ResourceTypesImmunization,
    ResourceTypesImmunizationEvaluation,
    ResourceTypesImmunizationRecommendation,
    ResourceTypesImplementationGuide,
    ResourceTypesIngredient,
    ResourceTypesInsurancePlan,
    ResourceTypesInvoice,
    ResourceTypesLibrary,
    ResourceTypesLinkage,
    ResourceTypesList,
    ResourceTypesLocation,
    ResourceTypesManufacturedItemDefinition,
    ResourceTypesMeasure,
    ResourceTypesMeasureReport,
    ResourceTypesMedication,
    ResourceTypesMedicationAdministration,
    ResourceTypesMedicationDispense,
    ResourceTypesMedicationKnowledge,
    ResourceTypesMedicationRequest,
    ResourceTypesMedicationUsage,
    ResourceTypesMedicinalProductDefinition,
    ResourceTypesMessageDefinition,
    ResourceTypesMessageHeader,
    ResourceTypesMolecularSequence,
    ResourceTypesNamingSystem,
    ResourceTypesNutritionIntake,
    ResourceTypesNutritionOrder,
    ResourceTypesNutritionProduct,
    ResourceTypesObservation,
    ResourceTypesObservationDefinition,
    ResourceTypesOperationDefinition,
    ResourceTypesOperationOutcome,
    ResourceTypesOrganization,
    ResourceTypesOrganizationAffiliation,
    ResourceTypesPackagedProductDefinition,
    ResourceTypesParameters,
    ResourceTypesPatient,
    ResourceTypesPaymentNotice,
    ResourceTypesPaymentReconciliation,
    ResourceTypesPermission,
    ResourceTypesPerson,
    ResourceTypesPlanDefinition,
    ResourceTypesPractitioner,
    ResourceTypesPractitionerRole,
    ResourceTypesProcedure,
    ResourceTypesProvenance,
    ResourceTypesQuestionnaire,
    ResourceTypesQuestionnaireResponse,
    ResourceTypesRegulatedAuthorization,
    ResourceTypesRelatedPerson,
    ResourceTypesRequestGroup,
    ResourceTypesResearchStudy,
    ResourceTypesResearchSubject,
    ResourceTypesResource,
    ResourceTypesRiskAssessment,
    ResourceTypesSchedule,
    ResourceTypesSearchParameter,
    ResourceTypesServiceRequest,
    ResourceTypesSlot,
    ResourceTypesSpecimen,
    ResourceTypesSpecimenDefinition,
    ResourceTypesStructureDefinition,
    ResourceTypesStructureMap,
    ResourceTypesSubscription,
    ResourceTypesSubscriptionStatus,
    ResourceTypesSubscriptionTopic,
    ResourceTypesSubstance,
    ResourceTypesSubstanceDefinition,
    ResourceTypesSubstanceNucleicAcid,
    ResourceTypesSubstancePolymer,
    ResourceTypesSubstanceProtein,
    ResourceTypesSubstanceReferenceInformation,
    ResourceTypesSubstanceSourceMaterial,
    ResourceTypesSupplyDelivery,
    ResourceTypesSupplyRequest,
    ResourceTypesTask,
    ResourceTypesTerminologyCapabilities,
    ResourceTypesTestReport,
    ResourceTypesTestScript,
    ResourceTypesValueSet,
    ResourceTypesVerificationResult,
    ResourceTypesVisionPrescription);
  TFhirResourceTypesEnumList = set of TFhirResourceTypesEnum;

  // How the system supports versioning for a resource. (from http://hl7.org/fhir/ValueSet/versioning-policy)
  TFhirResourceVersionPolicyEnum = (
    ResourceVersionPolicyNull, // Value is missing from Instance
    ResourceVersionPolicyNoVersion,
    ResourceVersionPolicyVersioned,
    ResourceVersionPolicyVersionedUpdate);
  TFhirResourceVersionPolicyEnumList = set of TFhirResourceVersionPolicyEnum;

  // The kind of response to a message. (from http://hl7.org/fhir/ValueSet/response-code)
  TFhirResponseTypeEnum = (
    ResponseTypeNull, // Value is missing from Instance
    ResponseTypeOk,
    ResponseTypeTransientError,
    ResponseTypeFatalError);
  TFhirResponseTypeEnumList = set of TFhirResponseTypeEnum;

  // The mode of a RESTful capability statement. (from http://hl7.org/fhir/ValueSet/restful-capability-mode)
  TFhirRestfulCapabilityModeEnum = (
    RestfulCapabilityModeNull, // Value is missing from Instance
    RestfulCapabilityModeClient,
    RestfulCapabilityModeServer);
  TFhirRestfulCapabilityModeEnumList = set of TFhirRestfulCapabilityModeEnum;

  // The license that applies to an Implementation Guide (using an SPDX license Identifiers, or 'not-open-source'). The binding is required but new SPDX license Identifiers are allowed to be used (https://spdx.org/licenses/). (from http://hl7.org/fhir/ValueSet/spdx-license)
  TFhirSPDXLicenseEnum = (
    SPDXLicenseNull, // Value is missing from Instance
    SPDXLicenseNotOpenSource,
    SPDXLicense0BSD,
    SPDXLicenseAAL,
    SPDXLicenseAbstyles,
    SPDXLicenseAdobe2006,
    SPDXLicenseAdobeGlyph,
    SPDXLicenseADSL,
    SPDXLicenseAFL11,
    SPDXLicenseAFL12,
    SPDXLicenseAFL20,
    SPDXLicenseAFL21,
    SPDXLicenseAFL30,
    SPDXLicenseAfmparse,
    SPDXLicenseAGPL10Only,
    SPDXLicenseAGPL10OrLater,
    SPDXLicenseAGPL30Only,
    SPDXLicenseAGPL30OrLater,
    SPDXLicenseAladdin,
    SPDXLicenseAMDPLPA,
    SPDXLicenseAML,
    SPDXLicenseAMPAS,
    SPDXLicenseANTLRPD,
    SPDXLicenseApache10,
    SPDXLicenseApache11,
    SPDXLicenseApache20,
    SPDXLicenseAPAFML,
    SPDXLicenseAPL10,
    SPDXLicenseAPSL10,
    SPDXLicenseAPSL11,
    SPDXLicenseAPSL12,
    SPDXLicenseAPSL20,
    SPDXLicenseArtistic10Cl8,
    SPDXLicenseArtistic10Perl,
    SPDXLicenseArtistic10,
    SPDXLicenseArtistic20,
    SPDXLicenseBahyph,
    SPDXLicenseBarr,
    SPDXLicenseBeerware,
    SPDXLicenseBitTorrent10,
    SPDXLicenseBitTorrent11,
    SPDXLicenseBorceux,
    SPDXLicenseBSD1Clause,
    SPDXLicenseBSD2ClauseFreeBSD,
    SPDXLicenseBSD2ClauseNetBSD,
    SPDXLicenseBSD2ClausePatent,
    SPDXLicenseBSD2Clause,
    SPDXLicenseBSD3ClauseAttribution,
    SPDXLicenseBSD3ClauseClear,
    SPDXLicenseBSD3ClauseLBNL,
    SPDXLicenseBSD3ClauseNoNuclearLicense2014,
    SPDXLicenseBSD3ClauseNoNuclearLicense,
    SPDXLicenseBSD3ClauseNoNuclearWarranty,
    SPDXLicenseBSD3Clause,
    SPDXLicenseBSD4ClauseUC,
    SPDXLicenseBSD4Clause,
    SPDXLicenseBSDProtection,
    SPDXLicenseBSDSourceCode,
    SPDXLicenseBSL10,
    SPDXLicenseBzip2105,
    SPDXLicenseBzip2106,
    SPDXLicenseCaldera,
    SPDXLicenseCATOSL11,
    SPDXLicenseCCBY10,
    SPDXLicenseCCBY20,
    SPDXLicenseCCBY25,
    SPDXLicenseCCBY30,
    SPDXLicenseCCBY40,
    SPDXLicenseCCBYNC10,
    SPDXLicenseCCBYNC20,
    SPDXLicenseCCBYNC25,
    SPDXLicenseCCBYNC30,
    SPDXLicenseCCBYNC40,
    SPDXLicenseCCBYNCND10,
    SPDXLicenseCCBYNCND20,
    SPDXLicenseCCBYNCND25,
    SPDXLicenseCCBYNCND30,
    SPDXLicenseCCBYNCND40,
    SPDXLicenseCCBYNCSA10,
    SPDXLicenseCCBYNCSA20,
    SPDXLicenseCCBYNCSA25,
    SPDXLicenseCCBYNCSA30,
    SPDXLicenseCCBYNCSA40,
    SPDXLicenseCCBYND10,
    SPDXLicenseCCBYND20,
    SPDXLicenseCCBYND25,
    SPDXLicenseCCBYND30,
    SPDXLicenseCCBYND40,
    SPDXLicenseCCBYSA10,
    SPDXLicenseCCBYSA20,
    SPDXLicenseCCBYSA25,
    SPDXLicenseCCBYSA30,
    SPDXLicenseCCBYSA40,
    SPDXLicenseCC010,
    SPDXLicenseCDDL10,
    SPDXLicenseCDDL11,
    SPDXLicenseCDLAPermissive10,
    SPDXLicenseCDLASharing10,
    SPDXLicenseCECILL10,
    SPDXLicenseCECILL11,
    SPDXLicenseCECILL20,
    SPDXLicenseCECILL21,
    SPDXLicenseCECILLB,
    SPDXLicenseCECILLC,
    SPDXLicenseClArtistic,
    SPDXLicenseCNRIJython,
    SPDXLicenseCNRIPythonGPLCompatible,
    SPDXLicenseCNRIPython,
    SPDXLicenseCondor11,
    SPDXLicenseCPAL10,
    SPDXLicenseCPL10,
    SPDXLicenseCPOL102,
    SPDXLicenseCrossword,
    SPDXLicenseCrystalStacker,
    SPDXLicenseCUAOPL10,
    SPDXLicenseCube,
    SPDXLicenseCurl,
    SPDXLicenseDFSL10,
    SPDXLicenseDiffmark,
    SPDXLicenseDOC,
    SPDXLicenseDotseqn,
    SPDXLicenseDSDP,
    SPDXLicenseDvipdfm,
    SPDXLicenseECL10,
    SPDXLicenseECL20,
    SPDXLicenseEFL10,
    SPDXLicenseEFL20,
    SPDXLicenseEGenix,
    SPDXLicenseEntessa,
    SPDXLicenseEPL10,
    SPDXLicenseEPL20,
    SPDXLicenseErlPL11,
    SPDXLicenseEUDatagrid,
    SPDXLicenseEUPL10,
    SPDXLicenseEUPL11,
    SPDXLicenseEUPL12,
    SPDXLicenseEurosym,
    SPDXLicenseFair,
    SPDXLicenseFrameworx10,
    SPDXLicenseFreeImage,
    SPDXLicenseFSFAP,
    SPDXLicenseFSFUL,
    SPDXLicenseFSFULLR,
    SPDXLicenseFTL,
    SPDXLicenseGFDL11Only,
    SPDXLicenseGFDL11OrLater,
    SPDXLicenseGFDL12Only,
    SPDXLicenseGFDL12OrLater,
    SPDXLicenseGFDL13Only,
    SPDXLicenseGFDL13OrLater,
    SPDXLicenseGiftware,
    SPDXLicenseGL2PS,
    SPDXLicenseGlide,
    SPDXLicenseGlulxe,
    SPDXLicenseGnuplot,
    SPDXLicenseGPL10Only,
    SPDXLicenseGPL10OrLater,
    SPDXLicenseGPL20Only,
    SPDXLicenseGPL20OrLater,
    SPDXLicenseGPL30Only,
    SPDXLicenseGPL30OrLater,
    SPDXLicenseGSOAP13b,
    SPDXLicenseHaskellReport,
    SPDXLicenseHPND,
    SPDXLicenseIBMPibs,
    SPDXLicenseICU,
    SPDXLicenseIJG,
    SPDXLicenseImageMagick,
    SPDXLicenseIMatix,
    SPDXLicenseImlib2,
    SPDXLicenseInfoZIP,
    SPDXLicenseIntelACPI,
    SPDXLicenseIntel,
    SPDXLicenseInterbase10,
    SPDXLicenseIPA,
    SPDXLicenseIPL10,
    SPDXLicenseISC,
    SPDXLicenseJasPer20,
    SPDXLicenseJSON,
    SPDXLicenseLAL12,
    SPDXLicenseLAL13,
    SPDXLicenseLatex2e,
    SPDXLicenseLeptonica,
    SPDXLicenseLGPL20Only,
    SPDXLicenseLGPL20OrLater,
    SPDXLicenseLGPL21Only,
    SPDXLicenseLGPL21OrLater,
    SPDXLicenseLGPL30Only,
    SPDXLicenseLGPL30OrLater,
    SPDXLicenseLGPLLR,
    SPDXLicenseLibpng,
    SPDXLicenseLibtiff,
    SPDXLicenseLiLiQP11,
    SPDXLicenseLiLiQR11,
    SPDXLicenseLiLiQRplus11,
    SPDXLicenseLinuxOpenIB,
    SPDXLicenseLPL10,
    SPDXLicenseLPL102,
    SPDXLicenseLPPL10,
    SPDXLicenseLPPL11,
    SPDXLicenseLPPL12,
    SPDXLicenseLPPL13a,
    SPDXLicenseLPPL13c,
    SPDXLicenseMakeIndex,
    SPDXLicenseMirOS,
    SPDXLicenseMIT0,
    SPDXLicenseMITAdvertising,
    SPDXLicenseMITCMU,
    SPDXLicenseMITEnna,
    SPDXLicenseMITFeh,
    SPDXLicenseMIT,
    SPDXLicenseMITNFA,
    SPDXLicenseMotosoto,
    SPDXLicenseMpich2,
    SPDXLicenseMPL10,
    SPDXLicenseMPL11,
    SPDXLicenseMPL20NoCopyleftException,
    SPDXLicenseMPL20,
    SPDXLicenseMSPL,
    SPDXLicenseMSRL,
    SPDXLicenseMTLL,
    SPDXLicenseMultics,
    SPDXLicenseMup,
    SPDXLicenseNASA13,
    SPDXLicenseNaumen,
    SPDXLicenseNBPL10,
    SPDXLicenseNCSA,
    SPDXLicenseNetSNMP,
    SPDXLicenseNetCDF,
    SPDXLicenseNewsletr,
    SPDXLicenseNGPL,
    SPDXLicenseNLOD10,
    SPDXLicenseNLPL,
    SPDXLicenseNokia,
    SPDXLicenseNOSL,
    SPDXLicenseNoweb,
    SPDXLicenseNPL10,
    SPDXLicenseNPL11,
    SPDXLicenseNPOSL30,
    SPDXLicenseNRL,
    SPDXLicenseNTP,
    SPDXLicenseOCCTPL,
    SPDXLicenseOCLC20,
    SPDXLicenseODbL10,
    SPDXLicenseOFL10,
    SPDXLicenseOFL11,
    SPDXLicenseOGTSL,
    SPDXLicenseOLDAP11,
    SPDXLicenseOLDAP12,
    SPDXLicenseOLDAP13,
    SPDXLicenseOLDAP14,
    SPDXLicenseOLDAP201,
    SPDXLicenseOLDAP20,
    SPDXLicenseOLDAP21,
    SPDXLicenseOLDAP221,
    SPDXLicenseOLDAP222,
    SPDXLicenseOLDAP22,
    SPDXLicenseOLDAP23,
    SPDXLicenseOLDAP24,
    SPDXLicenseOLDAP25,
    SPDXLicenseOLDAP26,
    SPDXLicenseOLDAP27,
    SPDXLicenseOLDAP28,
    SPDXLicenseOML,
    SPDXLicenseOpenSSL,
    SPDXLicenseOPL10,
    SPDXLicenseOSETPL21,
    SPDXLicenseOSL10,
    SPDXLicenseOSL11,
    SPDXLicenseOSL20,
    SPDXLicenseOSL21,
    SPDXLicenseOSL30,
    SPDXLicensePDDL10,
    SPDXLicensePHP30,
    SPDXLicensePHP301,
    SPDXLicensePlexus,
    SPDXLicensePostgreSQL,
    SPDXLicensePsfrag,
    SPDXLicensePsutils,
    SPDXLicensePython20,
    SPDXLicenseQhull,
    SPDXLicenseQPL10,
    SPDXLicenseRdisc,
    SPDXLicenseRHeCos11,
    SPDXLicenseRPL11,
    SPDXLicenseRPL15,
    SPDXLicenseRPSL10,
    SPDXLicenseRSAMD,
    SPDXLicenseRSCPL,
    SPDXLicenseRuby,
    SPDXLicenseSAXPD,
    SPDXLicenseSaxpath,
    SPDXLicenseSCEA,
    SPDXLicenseSendmail,
    SPDXLicenseSGIB10,
    SPDXLicenseSGIB11,
    SPDXLicenseSGIB20,
    SPDXLicenseSimPL20,
    SPDXLicenseSISSL12,
    SPDXLicenseSISSL,
    SPDXLicenseSleepycat,
    SPDXLicenseSMLNJ,
    SPDXLicenseSMPPL,
    SPDXLicenseSNIA,
    SPDXLicenseSpencer86,
    SPDXLicenseSpencer94,
    SPDXLicenseSpencer99,
    SPDXLicenseSPL10,
    SPDXLicenseSugarCRM113,
    SPDXLicenseSWL,
    SPDXLicenseTCL,
    SPDXLicenseTCPWrappers,
    SPDXLicenseTMate,
    SPDXLicenseTORQUE11,
    SPDXLicenseTOSL,
    SPDXLicenseUnicodeDFS2015,
    SPDXLicenseUnicodeDFS2016,
    SPDXLicenseUnicodeTOU,
    SPDXLicenseUnlicense,
    SPDXLicenseUPL10,
    SPDXLicenseVim,
    SPDXLicenseVOSTROM,
    SPDXLicenseVSL10,
    SPDXLicenseW3C19980720,
    SPDXLicenseW3C20150513,
    SPDXLicenseW3C,
    SPDXLicenseWatcom10,
    SPDXLicenseWsuipa,
    SPDXLicenseWTFPL,
    SPDXLicenseX11,
    SPDXLicenseXerox,
    SPDXLicenseXFree8611,
    SPDXLicenseXinetd,
    SPDXLicenseXnet,
    SPDXLicenseXpp,
    SPDXLicenseXSkat,
    SPDXLicenseYPL10,
    SPDXLicenseYPL11,
    SPDXLicenseZed,
    SPDXLicenseZend20,
    SPDXLicenseZimbra13,
    SPDXLicenseZimbra14,
    SPDXLicenseZlibAcknowledgement,
    SPDXLicenseZlib,
    SPDXLicenseZPL11,
    SPDXLicenseZPL20,
    SPDXLicenseZPL21);
  // What Search Comparator Codes are supported in search. (from http://hl7.org/fhir/ValueSet/search-comparator)
  TFhirSearchComparatorEnum = (
    SearchComparatorNull, // Value is missing from Instance
    SearchComparatorEq,
    SearchComparatorNe,
    SearchComparatorGt,
    SearchComparatorLt,
    SearchComparatorGe,
    SearchComparatorLe,
    SearchComparatorSa,
    SearchComparatorEb,
    SearchComparatorAp);
  TFhirSearchComparatorEnumList = set of TFhirSearchComparatorEnum;

  // Why an entry is in the result set - whether it's included as a match or because of an _include requirement, or to convey information or warning information about the search process. (from http://hl7.org/fhir/ValueSet/search-entry-mode)
  TFhirSearchEntryModeEnum = (
    SearchEntryModeNull, // Value is missing from Instance
    SearchEntryModeMatch,
    SearchEntryModeInclude,
    SearchEntryModeOutcome);
  TFhirSearchEntryModeEnumList = set of TFhirSearchEntryModeEnum;

  // A supported modifier for a search parameter. (from http://hl7.org/fhir/ValueSet/search-modifier-code)
  TFhirSearchModifierCodeEnum = (
    SearchModifierCodeNull, // Value is missing from Instance
    SearchModifierCodeMissing,
    SearchModifierCodeExact,
    SearchModifierCodeContains,
    SearchModifierCodeNot,
    SearchModifierCodeText,
    SearchModifierCodeIn,
    SearchModifierCodeNotIn,
    SearchModifierCodeBelow,
    SearchModifierCodeAbove,
    SearchModifierCodeType,
    SearchModifierCodeIdentifier,
    SearchModifierCodeOfType);
  TFhirSearchModifierCodeEnumList = set of TFhirSearchModifierCodeEnum;

  // Data types allowed to be used for search parameters. (from http://hl7.org/fhir/ValueSet/search-param-type)
  TFhirSearchParamTypeEnum = (
    SearchParamTypeNull, // Value is missing from Instance
    SearchParamTypeNumber,
    SearchParamTypeDate,
    SearchParamTypeString,
    SearchParamTypeToken,
    SearchParamTypeReference,
    SearchParamTypeComposite,
    SearchParamTypeQuantity,
    SearchParamTypeUri,
    SearchParamTypeSpecial);
  TFhirSearchParamTypeEnumList = set of TFhirSearchParamTypeEnum;

  // Type if a sequence -- DNA, RNA, or amino acid sequence. (from http://hl7.org/fhir/ValueSet/sequence-type)
  TFhirSequenceTypeEnum = (
    SequenceTypeNull, // Value is missing from Instance
    SequenceTypeAa,
    SequenceTypeDna,
    SequenceTypeRna);
  TFhirSequenceTypeEnumList = set of TFhirSequenceTypeEnum;

  // How slices are interpreted when evaluating an instance. (from http://hl7.org/fhir/ValueSet/resource-slicing-rules)
  TFhirSlicingRulesEnum = (
    SlicingRulesNull, // Value is missing from Instance
    SlicingRulesClosed,
    SlicingRulesOpen,
    SlicingRulesOpenAtEnd);
  TFhirSlicingRulesEnumList = set of TFhirSlicingRulesEnum;

  // The free/busy status of the slot. (from http://hl7.org/fhir/ValueSet/slotstatus)
  TFhirSlotStatusEnum = (
    SlotStatusNull, // Value is missing from Instance
    SlotStatusBusy,
    SlotStatusFree,
    SlotStatusBusyUnavailable,
    SlotStatusBusyTentative,
    SlotStatusEnteredInError);
  TFhirSlotStatusEnumList = set of TFhirSlotStatusEnum;

  // The possible sort directions, ascending or descending. (from http://hl7.org/fhir/ValueSet/sort-direction)
  TFhirSortDirectionEnum = (
    SortDirectionNull, // Value is missing from Instance
    SortDirectionAscending,
    SortDirectionDescending);
  TFhirSortDirectionEnumList = set of TFhirSortDirectionEnum;

  // Degree of preference of a type of conditioned specimen. (from http://hl7.org/fhir/ValueSet/specimen-contained-preference)
  TFhirSpecimenContainedPreferenceEnum = (
    SpecimenContainedPreferenceNull, // Value is missing from Instance
    SpecimenContainedPreferencePreferred,
    SpecimenContainedPreferenceAlternate);
  TFhirSpecimenContainedPreferenceEnumList = set of TFhirSpecimenContainedPreferenceEnum;

  // Codes providing the status/availability of a specimen. (from http://hl7.org/fhir/ValueSet/specimen-status)
  TFhirSpecimenStatusEnum = (
    SpecimenStatusNull, // Value is missing from Instance
    SpecimenStatusAvailable,
    SpecimenStatusUnavailable,
    SpecimenStatusUnsatisfactory,
    SpecimenStatusEnteredInError);
  TFhirSpecimenStatusEnumList = set of TFhirSpecimenStatusEnum;

  // The validation status of the target (from http://hl7.org/fhir/ValueSet/verificationresult-status)
  TFhirStatusEnum = (
    StatusNull, // Value is missing from Instance
    StatusAttested,
    StatusValidated,
    StatusInProcess,
    StatusReqRevalid,
    StatusValFail,
    StatusRevalFail);
  TFhirStatusEnumList = set of TFhirStatusEnum;

  // Type for strand. (from http://hl7.org/fhir/ValueSet/strand-type)
  TFhirStrandTypeEnum = (
    StrandTypeNull, // Value is missing from Instance
    StrandTypeWatson,
    StrandTypeCrick);
  TFhirStrandTypeEnumList = set of TFhirStrandTypeEnum;

  // Defines the type of structure that a definition is describing. (from http://hl7.org/fhir/ValueSet/structure-definition-kind)
  TFhirStructureDefinitionKindEnum = (
    StructureDefinitionKindNull, // Value is missing from Instance
    StructureDefinitionKindPrimitiveType,
    StructureDefinitionKindComplexType,
    StructureDefinitionKindResource,
    StructureDefinitionKindLogical);
  TFhirStructureDefinitionKindEnumList = set of TFhirStructureDefinitionKindEnum;

  // How to interpret the context. (from http://hl7.org/fhir/ValueSet/map-context-type)
  TFhirStructureMapContextTypeEnum = (
    StructureMapContextTypeNull, // Value is missing from Instance
    StructureMapContextTypeType,
    StructureMapContextTypeVariable);
  TFhirStructureMapContextTypeEnumList = set of TFhirStructureMapContextTypeEnum;

  // If this is the default rule set to apply for the source type, or this combination of types. (from http://hl7.org/fhir/ValueSet/map-group-type-mode)
  TFhirStructureMapGroupTypeModeEnum = (
    StructureMapGroupTypeModeNull, // Value is missing from Instance
    StructureMapGroupTypeModeTypes,
    StructureMapGroupTypeModeTypeAndTypes);
  TFhirStructureMapGroupTypeModeEnumList = set of TFhirStructureMapGroupTypeModeEnum;

  // Mode for this instance of data. (from http://hl7.org/fhir/ValueSet/map-input-mode)
  TFhirStructureMapInputModeEnum = (
    StructureMapInputModeNull, // Value is missing from Instance
    StructureMapInputModeSource,
    StructureMapInputModeTarget);
  TFhirStructureMapInputModeEnumList = set of TFhirStructureMapInputModeEnum;

  // How the referenced structure is used in this mapping. (from http://hl7.org/fhir/ValueSet/map-model-mode)
  TFhirStructureMapModelModeEnum = (
    StructureMapModelModeNull, // Value is missing from Instance
    StructureMapModelModeSource,
    StructureMapModelModeQueried,
    StructureMapModelModeTarget,
    StructureMapModelModeProduced);
  TFhirStructureMapModelModeEnumList = set of TFhirStructureMapModelModeEnum;

  // If field is a list, how to manage the source. (from http://hl7.org/fhir/ValueSet/map-source-list-mode)
  TFhirStructureMapSourceListModeEnum = (
    StructureMapSourceListModeNull, // Value is missing from Instance
    StructureMapSourceListModeFirst,
    StructureMapSourceListModeNotFirst,
    StructureMapSourceListModeLast,
    StructureMapSourceListModeNotLast,
    StructureMapSourceListModeOnlyOne);
  TFhirStructureMapSourceListModeEnumList = set of TFhirStructureMapSourceListModeEnum;

  // If field is a list, how to manage the production. (from http://hl7.org/fhir/ValueSet/map-target-list-mode)
  TFhirStructureMapTargetListModeEnum = (
    StructureMapTargetListModeNull, // Value is missing from Instance
    StructureMapTargetListModeFirst,
    StructureMapTargetListModeShare,
    StructureMapTargetListModeLast,
    StructureMapTargetListModeCollate);
  TFhirStructureMapTargetListModeEnumList = set of TFhirStructureMapTargetListModeEnum;

  // How data is copied/created. (from http://hl7.org/fhir/ValueSet/map-transform)
  TFhirStructureMapTransformEnum = (
    StructureMapTransformNull, // Value is missing from Instance
    StructureMapTransformCreate,
    StructureMapTransformCopy,
    StructureMapTransformTruncate,
    StructureMapTransformEscape,
    StructureMapTransformCast,
    StructureMapTransformAppend,
    StructureMapTransformTranslate,
    StructureMapTransformReference,
    StructureMapTransformDateOp,
    StructureMapTransformUuid,
    StructureMapTransformPointer,
    StructureMapTransformEvaluate,
    StructureMapTransformCc,
    StructureMapTransformC,
    StructureMapTransformQty,
    StructureMapTransformId,
    StructureMapTransformCp);
  TFhirStructureMapTransformEnumList = set of TFhirStructureMapTransformEnum;

  // The type of notification represented by the status message. (from http://hl7.org/fhir/ValueSet/subscription-notification-type)
  TFhirSubscriptionNotificationTypeEnum = (
    SubscriptionNotificationTypeNull, // Value is missing from Instance
    SubscriptionNotificationTypeHandshake,
    SubscriptionNotificationTypeHeartbeat,
    SubscriptionNotificationTypeEventNotification,
    SubscriptionNotificationTypeQueryStatus);
  TFhirSubscriptionNotificationTypeEnumList = set of TFhirSubscriptionNotificationTypeEnum;

  // Codes to represent how much resource content to send in the notification payload. (from http://hl7.org/fhir/ValueSet/subscription-payload-content)
  TFhirSubscriptionPayloadContentEnum = (
    SubscriptionPayloadContentNull, // Value is missing from Instance
    SubscriptionPayloadContentEmpty,
    SubscriptionPayloadContentIdOnly,
    SubscriptionPayloadContentFullResource);
  TFhirSubscriptionPayloadContentEnumList = set of TFhirSubscriptionPayloadContentEnum;

  // FHIR search modifiers allowed for use in Subscriptions and SubscriptionTopics. (from http://hl7.org/fhir/ValueSet/subscription-search-modifier)
  TFhirSubscriptionSearchModifierEnum = (
    SubscriptionSearchModifierNull, // Value is missing from Instance
    SubscriptionSearchModifierEqual,
    SubscriptionSearchModifierEq,
    SubscriptionSearchModifierNe,
    SubscriptionSearchModifierGt,
    SubscriptionSearchModifierLt,
    SubscriptionSearchModifierGe,
    SubscriptionSearchModifierLe,
    SubscriptionSearchModifierSa,
    SubscriptionSearchModifierEb,
    SubscriptionSearchModifierAp,
    SubscriptionSearchModifierAbove,
    SubscriptionSearchModifierBelow,
    SubscriptionSearchModifierIn,
    SubscriptionSearchModifierNotIn,
    SubscriptionSearchModifierOfType);
  TFhirSubscriptionSearchModifierEnumList = set of TFhirSubscriptionSearchModifierEnum;

  // State values for FHIR Subscriptions. (from http://hl7.org/fhir/ValueSet/subscription-state)
  TFhirSubscriptionStateEnum = (
    SubscriptionStateNull, // Value is missing from Instance
    SubscriptionStateRequested,
    SubscriptionStateActive,
    SubscriptionStateError,
    SubscriptionStateOff);
  TFhirSubscriptionStateEnumList = set of TFhirSubscriptionStateEnum;

  // Status of the supply delivery. (from http://hl7.org/fhir/ValueSet/supplydelivery-status)
  TFhirSupplyDeliveryStatusEnum = (
    SupplyDeliveryStatusNull, // Value is missing from Instance
    SupplyDeliveryStatusInProgress,
    SupplyDeliveryStatusCompleted,
    SupplyDeliveryStatusAbandoned,
    SupplyDeliveryStatusEnteredInError);
  TFhirSupplyDeliveryStatusEnumList = set of TFhirSupplyDeliveryStatusEnum;

  // Status of the supply request. (from http://hl7.org/fhir/ValueSet/supplyrequest-status)
  TFhirSupplyRequestStatusEnum = (
    SupplyRequestStatusNull, // Value is missing from Instance
    SupplyRequestStatusDraft,
    SupplyRequestStatusActive,
    SupplyRequestStatusSuspended,
    SupplyRequestStatusCancelled,
    SupplyRequestStatusCompleted,
    SupplyRequestStatusEnteredInError,
    SupplyRequestStatusUnknown);
  TFhirSupplyRequestStatusEnumList = set of TFhirSupplyRequestStatusEnum;

  // Operations supported by REST at the system level. (from http://hl7.org/fhir/ValueSet/system-restful-interaction)
  TFhirSystemRestfulInteractionEnum = (
    SystemRestfulInteractionNull, // Value is missing from Instance
    SystemRestfulInteractionTransaction,
    SystemRestfulInteractionBatch,
    SystemRestfulInteractionSearchSystem,
    SystemRestfulInteractionHistorySystem);
  TFhirSystemRestfulInteractionEnumList = set of TFhirSystemRestfulInteractionEnum;

  // Distinguishes whether the task is a proposal, plan or full order. (from http://hl7.org/fhir/ValueSet/task-intent)
  TFhirTaskIntentEnum = (
    TaskIntentNull, // Value is missing from Instance
    TaskIntentUnknown,
    TaskIntentProposal,
    TaskIntentPlan,
    TaskIntentOrder,
    TaskIntentOriginalOrder,
    TaskIntentReflexOrder,
    TaskIntentFillerOrder,
    TaskIntentInstanceOrder,
    TaskIntentOption);
  TFhirTaskIntentEnumList = set of TFhirTaskIntentEnum;

  // The current status of the task. (from http://hl7.org/fhir/ValueSet/task-status)
  TFhirTaskStatusEnum = (
    TaskStatusNull, // Value is missing from Instance
    TaskStatusDraft,
    TaskStatusRequested,
    TaskStatusReceived,
    TaskStatusAccepted,
    TaskStatusRejected,
    TaskStatusReady,
    TaskStatusCancelled,
    TaskStatusInProgress,
    TaskStatusOnHold,
    TaskStatusFailed,
    TaskStatusCompleted,
    TaskStatusEnteredInError);
  TFhirTaskStatusEnumList = set of TFhirTaskStatusEnum;

  // The results of executing an action. (from http://hl7.org/fhir/ValueSet/report-action-result-codes)
  TFhirTestReportActionResultEnum = (
    TestReportActionResultNull, // Value is missing from Instance
    TestReportActionResultPass,
    TestReportActionResultSkip,
    TestReportActionResultFail,
    TestReportActionResultWarning,
    TestReportActionResultError);
  TFhirTestReportActionResultEnumList = set of TFhirTestReportActionResultEnum;

  // The type of participant. (from http://hl7.org/fhir/ValueSet/report-participant-type)
  TFhirTestReportParticipantTypeEnum = (
    TestReportParticipantTypeNull, // Value is missing from Instance
    TestReportParticipantTypeTestEngine,
    TestReportParticipantTypeClient,
    TestReportParticipantTypeServer);
  TFhirTestReportParticipantTypeEnumList = set of TFhirTestReportParticipantTypeEnum;

  // The reported execution result. (from http://hl7.org/fhir/ValueSet/report-result-codes)
  TFhirTestReportResultEnum = (
    TestReportResultNull, // Value is missing from Instance
    TestReportResultPass,
    TestReportResultFail,
    TestReportResultPending);
  TFhirTestReportResultEnumList = set of TFhirTestReportResultEnum;

  // The current status of the test report. (from http://hl7.org/fhir/ValueSet/report-status-codes)
  TFhirTestReportStatusEnum = (
    TestReportStatusNull, // Value is missing from Instance
    TestReportStatusCompleted,
    TestReportStatusInProgress,
    TestReportStatusWaiting,
    TestReportStatusStopped,
    TestReportStatusEnteredInError);
  TFhirTestReportStatusEnumList = set of TFhirTestReportStatusEnum;

  // The allowable request method or HTTP operation codes. (from http://hl7.org/fhir/ValueSet/http-operations)
  TFhirTestScriptRequestMethodCodeEnum = (
    TestScriptRequestMethodCodeNull, // Value is missing from Instance
    TestScriptRequestMethodCodeDelete,
    TestScriptRequestMethodCodeGet,
    TestScriptRequestMethodCodeOptions,
    TestScriptRequestMethodCodePatch,
    TestScriptRequestMethodCodePost,
    TestScriptRequestMethodCodePut,
    TestScriptRequestMethodCodeHead);
  TFhirTestScriptRequestMethodCodeEnumList = set of TFhirTestScriptRequestMethodCodeEnum;

  // The type of trigger. (from http://hl7.org/fhir/ValueSet/trigger-type)
  TFhirTriggerTypeEnum = (
    TriggerTypeNull, // Value is missing from Instance
    TriggerTypeNamedEvent,
    TriggerTypePeriodic,
    TriggerTypeDataChanged,
    TriggerTypeDataAdded,
    TriggerTypeDataModified,
    TriggerTypeDataRemoved,
    TriggerTypeDataAccessed,
    TriggerTypeDataAccessEnded);
  TFhirTriggerTypeEnumList = set of TFhirTriggerTypeEnum;

  // How a type relates to its baseDefinition. (from http://hl7.org/fhir/ValueSet/type-derivation-rule)
  TFhirTypeDerivationRuleEnum = (
    TypeDerivationRuleNull, // Value is missing from Instance
    TypeDerivationRuleSpecialization,
    TypeDerivationRuleConstraint);
  TFhirTypeDerivationRuleEnumList = set of TFhirTypeDerivationRuleEnum;

  // Operations supported by REST at the type or instance level. (from http://hl7.org/fhir/ValueSet/type-restful-interaction)
  TFhirTypeRestfulInteractionEnum = (
    TypeRestfulInteractionNull, // Value is missing from Instance
    TypeRestfulInteractionRead,
    TypeRestfulInteractionVread,
    TypeRestfulInteractionUpdate,
    TypeRestfulInteractionPatch,
    TypeRestfulInteractionDelete,
    TypeRestfulInteractionHistoryInstance,
    TypeRestfulInteractionHistoryType,
    TypeRestfulInteractionCreate,
    TypeRestfulInteractionSearchType);
  TFhirTypeRestfulInteractionEnumList = set of TFhirTypeRestfulInteractionEnum;

  // Codes to identify how UDI data was entered. (from http://hl7.org/fhir/ValueSet/udi-entry-type)
  TFhirUDIEntryTypeEnum = (
    UDIEntryTypeNull, // Value is missing from Instance
    UDIEntryTypeBarcode,
    UDIEntryTypeRfid,
    UDIEntryTypeManual,
    UDIEntryTypeCard,
    UDIEntryTypeSelfReported,
    UDIEntryTypeElectronicTransmission,
    UDIEntryTypeUnknown);
  TFhirUDIEntryTypeEnumList = set of TFhirUDIEntryTypeEnum;

  // A unit of time (units from UCUM). (from http://hl7.org/fhir/ValueSet/units-of-time)
  TFhirUnitsOfTimeEnum = (
    UnitsOfTimeNull, // Value is missing from Instance
    UnitsOfTimeS,
    UnitsOfTimeMin,
    UnitsOfTimeH,
    UnitsOfTimeD,
    UnitsOfTimeWk,
    UnitsOfTimeMo,
    UnitsOfTimeA);
  TFhirUnitsOfTimeEnumList = set of TFhirUnitsOfTimeEnum;

  // The purpose of the Claim: predetermination, preauthorization, claim. (from http://hl7.org/fhir/ValueSet/claim-use)
  TFhirUseEnum = (
    UseNull, // Value is missing from Instance
    UseClaim,
    UsePreauthorization,
    UsePredetermination);
  TFhirUseEnumList = set of TFhirUseEnum;

  // A coded concept listing the base codes. (from http://hl7.org/fhir/ValueSet/vision-base-codes)
  TFhirVisionBaseEnum = (
    VisionBaseNull, // Value is missing from Instance
    VisionBaseUp,
    VisionBaseDown,
    VisionBaseIn,
    VisionBaseOut);
  TFhirVisionBaseEnumList = set of TFhirVisionBaseEnum;

  // A coded concept listing the eye codes. (from http://hl7.org/fhir/ValueSet/vision-eye-codes)
  TFhirVisionEyesEnum = (
    VisionEyesNull, // Value is missing from Instance
    VisionEyesRight,
    VisionEyesLeft);
  TFhirVisionEyesEnumList = set of TFhirVisionEyesEnum;

  // How a search parameter relates to the set of elements returned by evaluating its xpath query. (from http://hl7.org/fhir/ValueSet/search-xpath-usage)
  TFhirXPathUsageTypeEnum = (
    XPathUsageTypeNull, // Value is missing from Instance
    XPathUsageTypeNormal,
    XPathUsageTypePhonetic,
    XPathUsageTypeNearby,
    XPathUsageTypeDistance,
    XPathUsageTypeOther);
  TFhirXPathUsageTypeEnumList = set of TFhirXPathUsageTypeEnum;

const
  CODES_TFhirAccountStatusEnum : Array[TFhirAccountStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error', 'on-hold', 'unknown');
  SYSTEMS_TFhirAccountStatusEnum : Array[TFhirAccountStatusEnum] of String = ('', 'http://hl7.org/fhir/account-status', 'http://hl7.org/fhir/account-status', 'http://hl7.org/fhir/account-status', 'http://hl7.org/fhir/account-status', 'http://hl7.org/fhir/account-status');
  CODES_TFhirActionCardinalityBehaviorEnum : Array[TFhirActionCardinalityBehaviorEnum] of String = ('', 'single', 'multiple');
  SYSTEMS_TFhirActionCardinalityBehaviorEnum : Array[TFhirActionCardinalityBehaviorEnum] of String = ('', 'http://hl7.org/fhir/action-cardinality-behavior', 'http://hl7.org/fhir/action-cardinality-behavior');
  CODES_TFhirActionConditionKindEnum : Array[TFhirActionConditionKindEnum] of String = ('', 'applicability', 'start', 'stop');
  SYSTEMS_TFhirActionConditionKindEnum : Array[TFhirActionConditionKindEnum] of String = ('', 'http://hl7.org/fhir/action-condition-kind', 'http://hl7.org/fhir/action-condition-kind', 'http://hl7.org/fhir/action-condition-kind');
  CODES_TFhirActionGroupingBehaviorEnum : Array[TFhirActionGroupingBehaviorEnum] of String = ('', 'visual-group', 'logical-group', 'sentence-group');
  SYSTEMS_TFhirActionGroupingBehaviorEnum : Array[TFhirActionGroupingBehaviorEnum] of String = ('', 'http://hl7.org/fhir/action-grouping-behavior', 'http://hl7.org/fhir/action-grouping-behavior', 'http://hl7.org/fhir/action-grouping-behavior');
  CODES_TFhirActionParticipantTypeEnum : Array[TFhirActionParticipantTypeEnum] of String = ('', 'patient', 'practitioner', 'related-person', 'device');
  SYSTEMS_TFhirActionParticipantTypeEnum : Array[TFhirActionParticipantTypeEnum] of String = ('', 'http://hl7.org/fhir/action-participant-type', 'http://hl7.org/fhir/action-participant-type', 'http://hl7.org/fhir/action-participant-type', 'http://hl7.org/fhir/action-participant-type');
  CODES_TFhirActionPrecheckBehaviorEnum : Array[TFhirActionPrecheckBehaviorEnum] of String = ('', 'yes', 'no');
  SYSTEMS_TFhirActionPrecheckBehaviorEnum : Array[TFhirActionPrecheckBehaviorEnum] of String = ('', 'http://hl7.org/fhir/action-precheck-behavior', 'http://hl7.org/fhir/action-precheck-behavior');
  CODES_TFhirActionRelationshipTypeEnum : Array[TFhirActionRelationshipTypeEnum] of String = ('', 'before-start', 'before', 'before-end', 'concurrent-with-start', 'concurrent', 'concurrent-with-end', 'after-start', 'after', 'after-end');
  SYSTEMS_TFhirActionRelationshipTypeEnum : Array[TFhirActionRelationshipTypeEnum] of String = ('', 'http://hl7.org/fhir/action-relationship-type', 'http://hl7.org/fhir/action-relationship-type', 'http://hl7.org/fhir/action-relationship-type', 'http://hl7.org/fhir/action-relationship-type', 'http://hl7.org/fhir/action-relationship-type', 'http://hl7.org/fhir/action-relationship-type', 'http://hl7.org/fhir/action-relationship-type', 'http://hl7.org/fhir/action-relationship-type', 'http://hl7.org/fhir/action-relationship-type');
  CODES_TFhirActionRequiredBehaviorEnum : Array[TFhirActionRequiredBehaviorEnum] of String = ('', 'must', 'could', 'must-unless-documented');
  SYSTEMS_TFhirActionRequiredBehaviorEnum : Array[TFhirActionRequiredBehaviorEnum] of String = ('', 'http://hl7.org/fhir/action-required-behavior', 'http://hl7.org/fhir/action-required-behavior', 'http://hl7.org/fhir/action-required-behavior');
  CODES_TFhirActionSelectionBehaviorEnum : Array[TFhirActionSelectionBehaviorEnum] of String = ('', 'any', 'all', 'all-or-none', 'exactly-one', 'at-most-one', 'one-or-more');
  SYSTEMS_TFhirActionSelectionBehaviorEnum : Array[TFhirActionSelectionBehaviorEnum] of String = ('', 'http://hl7.org/fhir/action-selection-behavior', 'http://hl7.org/fhir/action-selection-behavior', 'http://hl7.org/fhir/action-selection-behavior', 'http://hl7.org/fhir/action-selection-behavior', 'http://hl7.org/fhir/action-selection-behavior', 'http://hl7.org/fhir/action-selection-behavior');
  CODES_TFhirAddressTypeEnum : Array[TFhirAddressTypeEnum] of String = ('', 'postal', 'physical', 'both');
  SYSTEMS_TFhirAddressTypeEnum : Array[TFhirAddressTypeEnum] of String = ('', 'http://hl7.org/fhir/address-type', 'http://hl7.org/fhir/address-type', 'http://hl7.org/fhir/address-type');
  CODES_TFhirAddressUseEnum : Array[TFhirAddressUseEnum] of String = ('', 'home', 'work', 'temp', 'old', 'billing');
  SYSTEMS_TFhirAddressUseEnum : Array[TFhirAddressUseEnum] of String = ('', 'http://hl7.org/fhir/address-use', 'http://hl7.org/fhir/address-use', 'http://hl7.org/fhir/address-use', 'http://hl7.org/fhir/address-use', 'http://hl7.org/fhir/address-use');
  CODES_TFhirAdministrativeGenderEnum : Array[TFhirAdministrativeGenderEnum] of String = ('', 'male', 'female', 'other', 'unknown');
  SYSTEMS_TFhirAdministrativeGenderEnum : Array[TFhirAdministrativeGenderEnum] of String = ('', 'http://hl7.org/fhir/administrative-gender', 'http://hl7.org/fhir/administrative-gender', 'http://hl7.org/fhir/administrative-gender', 'http://hl7.org/fhir/administrative-gender');
  CODES_TFhirAdverseEventActualityEnum : Array[TFhirAdverseEventActualityEnum] of String = ('', 'actual', 'potential');
  SYSTEMS_TFhirAdverseEventActualityEnum : Array[TFhirAdverseEventActualityEnum] of String = ('', 'http://hl7.org/fhir/adverse-event-actuality', 'http://hl7.org/fhir/adverse-event-actuality');
  CODES_TFhirAdverseEventStatusEnum : Array[TFhirAdverseEventStatusEnum] of String = ('', 'in-progress', 'completed', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirAdverseEventStatusEnum : Array[TFhirAdverseEventStatusEnum] of String = ('', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status');
  CODES_TFhirAggregationModeEnum : Array[TFhirAggregationModeEnum] of String = ('', 'contained', 'referenced', 'bundled');
  SYSTEMS_TFhirAggregationModeEnum : Array[TFhirAggregationModeEnum] of String = ('', 'http://hl7.org/fhir/resource-aggregation-mode', 'http://hl7.org/fhir/resource-aggregation-mode', 'http://hl7.org/fhir/resource-aggregation-mode');
  CODES_TFhirAllTypesEnum : Array[TFhirAllTypesEnum] of String = ('', 'Address', 'Age', 'Annotation', 'Attachment', 'BackboneElement', 'BackboneType', 'Base', 'CodeableConcept', 'CodeableReference', 'Coding', 'ContactDetail', 'ContactPoint', 'Contributor', 'Count', 'DataRequirement', 'DataType', 'Distance', 'Dosage', 'Duration', 'Element', 'ElementDefinition', 'Expression', 'Extension', 'HumanName', 'Identifier', 'MarketingStatus', 'Meta', 'Money', 'MoneyQuantity', 'Narrative', 'OrderedDistribution', 'ParameterDefinition', 'Period', 'Population', 'PrimitiveType', 'ProdCharacteristic', 'ProductShelfLife', 'Quantity', 'Range', 'Ratio', 'Reference', 'RelatedArtifact', 'SampledData', 'Signature', 'SimpleQuantity', 'Statistic', 'Timing', 'TriggerDefinition', 'UsageContext', 'base64Binary', 'boolean', 'canonical', 'code', 'date', 'dateTime', 'decimal', 'id', 'instant', 'integer', 'integer64', 'markdown', 'oid', 'positiveInt', 'string', 'time', 'unsignedInt', 'uri', 'url', 'uuid', 'xhtml', 'Account',
       'ActivityDefinition', 'AdministrableProductDefinition', 'AdverseEvent', 'AllergyIntolerance', 'Appointment', 'AppointmentResponse', 'AuditEvent', 'Basic', 'Binary', 'BiologicallyDerivedProduct', 'BodyStructure', 'Bundle', 'CapabilityStatement', 'CapabilityStatement2', 'CarePlan', 'CareTeam', 'CatalogEntry', 'ChargeItem', 'ChargeItemDefinition', 'Citation', 'Claim', 'ClaimResponse', 'ClinicalImpression', 'ClinicalUseIssue', 'CodeSystem', 'Communication', 'CommunicationRequest', 'CompartmentDefinition', 'Composition', 'ConceptMap', 'Condition', 'ConditionDefinition', 'Consent', 'Contract', 'Coverage', 'CoverageEligibilityRequest', 'CoverageEligibilityResponse', 'DetectedIssue', 'Device', 'DeviceDefinition', 'DeviceMetric', 'DeviceRequest', 'DeviceUseStatement', 'DiagnosticReport', 'DocumentManifest', 'DocumentReference', 'DomainResource', 'Encounter', 'Endpoint', 'EnrollmentRequest', 'EnrollmentResponse', 'EpisodeOfCare', 'EventDefinition', 'Evidence', 'EvidenceReport', 'EvidenceVariable',
       'ExampleScenario', 'ExplanationOfBenefit', 'FamilyMemberHistory', 'Flag', 'Goal', 'GraphDefinition', 'Group', 'GuidanceResponse', 'HealthcareService', 'ImagingStudy', 'Immunization', 'ImmunizationEvaluation', 'ImmunizationRecommendation', 'ImplementationGuide', 'Ingredient', 'InsurancePlan', 'Invoice', 'Library', 'Linkage', 'List', 'Location', 'ManufacturedItemDefinition', 'Measure', 'MeasureReport', 'Medication', 'MedicationAdministration', 'MedicationDispense', 'MedicationKnowledge', 'MedicationRequest', 'MedicationUsage', 'MedicinalProductDefinition', 'MessageDefinition', 'MessageHeader', 'MolecularSequence', 'NamingSystem', 'NutritionIntake', 'NutritionOrder', 'NutritionProduct', 'Observation', 'ObservationDefinition', 'OperationDefinition', 'OperationOutcome', 'Organization', 'OrganizationAffiliation', 'PackagedProductDefinition', 'Parameters', 'Patient', 'PaymentNotice', 'PaymentReconciliation', 'Permission', 'Person', 'PlanDefinition', 'Practitioner', 'PractitionerRole', 'Procedure',
       'Provenance', 'Questionnaire', 'QuestionnaireResponse', 'RegulatedAuthorization', 'RelatedPerson', 'RequestGroup', 'ResearchStudy', 'ResearchSubject', 'Resource', 'RiskAssessment', 'Schedule', 'SearchParameter', 'ServiceRequest', 'Slot', 'Specimen', 'SpecimenDefinition', 'StructureDefinition', 'StructureMap', 'Subscription', 'SubscriptionStatus', 'SubscriptionTopic', 'Substance', 'SubstanceDefinition', 'SubstanceNucleicAcid', 'SubstancePolymer', 'SubstanceProtein', 'SubstanceReferenceInformation', 'SubstanceSourceMaterial', 'SupplyDelivery', 'SupplyRequest', 'Task', 'TerminologyCapabilities', 'TestReport', 'TestScript', 'ValueSet', 'VerificationResult', 'VisionPrescription', 'Type', 'Any');
  SYSTEMS_TFhirAllTypesEnum : Array[TFhirAllTypesEnum] of String = ('', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types',
       'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types',
       'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/abstract-types', 'http://hl7.org/fhir/abstract-types');
  CODES_TFhirAllergyIntoleranceCategoryEnum : Array[TFhirAllergyIntoleranceCategoryEnum] of String = ('', 'food', 'medication', 'environment', 'biologic');
  SYSTEMS_TFhirAllergyIntoleranceCategoryEnum : Array[TFhirAllergyIntoleranceCategoryEnum] of String = ('', 'http://hl7.org/fhir/allergy-intolerance-category', 'http://hl7.org/fhir/allergy-intolerance-category', 'http://hl7.org/fhir/allergy-intolerance-category', 'http://hl7.org/fhir/allergy-intolerance-category');
  CODES_TFhirAllergyIntoleranceCriticalityEnum : Array[TFhirAllergyIntoleranceCriticalityEnum] of String = ('', 'low', 'high', 'unable-to-assess');
  SYSTEMS_TFhirAllergyIntoleranceCriticalityEnum : Array[TFhirAllergyIntoleranceCriticalityEnum] of String = ('', 'http://hl7.org/fhir/allergy-intolerance-criticality', 'http://hl7.org/fhir/allergy-intolerance-criticality', 'http://hl7.org/fhir/allergy-intolerance-criticality');
  CODES_TFhirAllergyIntoleranceSeverityEnum : Array[TFhirAllergyIntoleranceSeverityEnum] of String = ('', 'mild', 'moderate', 'severe');
  SYSTEMS_TFhirAllergyIntoleranceSeverityEnum : Array[TFhirAllergyIntoleranceSeverityEnum] of String = ('', 'http://hl7.org/fhir/reaction-event-severity', 'http://hl7.org/fhir/reaction-event-severity', 'http://hl7.org/fhir/reaction-event-severity');
  CODES_TFhirAllergyIntoleranceTypeEnum : Array[TFhirAllergyIntoleranceTypeEnum] of String = ('', 'allergy', 'intolerance');
  SYSTEMS_TFhirAllergyIntoleranceTypeEnum : Array[TFhirAllergyIntoleranceTypeEnum] of String = ('', 'http://hl7.org/fhir/allergy-intolerance-type', 'http://hl7.org/fhir/allergy-intolerance-type');
  CODES_TFhirAppointmentStatusEnum : Array[TFhirAppointmentStatusEnum] of String = ('', 'proposed', 'pending', 'booked', 'arrived', 'fulfilled', 'cancelled', 'noshow', 'entered-in-error', 'checked-in', 'waitlist');
  SYSTEMS_TFhirAppointmentStatusEnum : Array[TFhirAppointmentStatusEnum] of String = ('', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus');
  CODES_TFhirAssertionDirectionTypeEnum : Array[TFhirAssertionDirectionTypeEnum] of String = ('', 'response', 'request');
  SYSTEMS_TFhirAssertionDirectionTypeEnum : Array[TFhirAssertionDirectionTypeEnum] of String = ('', 'http://hl7.org/fhir/assert-direction-codes', 'http://hl7.org/fhir/assert-direction-codes');
  CODES_TFhirAssertionOperatorTypeEnum : Array[TFhirAssertionOperatorTypeEnum] of String = ('', 'equals', 'notEquals', 'in', 'notIn', 'greaterThan', 'lessThan', 'empty', 'notEmpty', 'contains', 'notContains', 'eval');
  SYSTEMS_TFhirAssertionOperatorTypeEnum : Array[TFhirAssertionOperatorTypeEnum] of String = ('', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes');
  CODES_TFhirAssertionResponseTypesEnum : Array[TFhirAssertionResponseTypesEnum] of String = ('', 'okay', 'created', 'noContent', 'notModified', 'bad', 'forbidden', 'notFound', 'methodNotAllowed', 'conflict', 'gone', 'preconditionFailed', 'unprocessable');
  SYSTEMS_TFhirAssertionResponseTypesEnum : Array[TFhirAssertionResponseTypesEnum] of String = ('', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types');
  CODES_TFhirAuditEventActionEnum : Array[TFhirAuditEventActionEnum] of String = ('', 'C', 'R', 'U', 'D', 'E');
  SYSTEMS_TFhirAuditEventActionEnum : Array[TFhirAuditEventActionEnum] of String = ('', 'http://hl7.org/fhir/audit-event-action', 'http://hl7.org/fhir/audit-event-action', 'http://hl7.org/fhir/audit-event-action', 'http://hl7.org/fhir/audit-event-action', 'http://hl7.org/fhir/audit-event-action');
  CODES_TFhirAuditEventAgentNetworkTypeEnum : Array[TFhirAuditEventAgentNetworkTypeEnum] of String = ('', '1', '2', '3', '4', '5');
  SYSTEMS_TFhirAuditEventAgentNetworkTypeEnum : Array[TFhirAuditEventAgentNetworkTypeEnum] of String = ('', 'http://hl7.org/fhir/network-type', 'http://hl7.org/fhir/network-type', 'http://hl7.org/fhir/network-type', 'http://hl7.org/fhir/network-type', 'http://hl7.org/fhir/network-type');
  CODES_TFhirAuditEventSeverityEnum : Array[TFhirAuditEventSeverityEnum] of String = ('', 'emergency', 'alert', 'critical', 'error', 'warning', 'notice', 'informational', 'debug');
  SYSTEMS_TFhirAuditEventSeverityEnum : Array[TFhirAuditEventSeverityEnum] of String = ('', 'http://hl7.org/fhir/audit-event-severity', 'http://hl7.org/fhir/audit-event-severity', 'http://hl7.org/fhir/audit-event-severity', 'http://hl7.org/fhir/audit-event-severity', 'http://hl7.org/fhir/audit-event-severity', 'http://hl7.org/fhir/audit-event-severity', 'http://hl7.org/fhir/audit-event-severity', 'http://hl7.org/fhir/audit-event-severity');
  CODES_TFhirBindingStrengthEnum : Array[TFhirBindingStrengthEnum] of String = ('', 'required', 'extensible', 'preferred', 'example');
  SYSTEMS_TFhirBindingStrengthEnum : Array[TFhirBindingStrengthEnum] of String = ('', 'http://hl7.org/fhir/binding-strength', 'http://hl7.org/fhir/binding-strength', 'http://hl7.org/fhir/binding-strength', 'http://hl7.org/fhir/binding-strength');
  CODES_TFhirBiologicallyDerivedProductCategoryEnum : Array[TFhirBiologicallyDerivedProductCategoryEnum] of String = ('', 'organ', 'tissue', 'fluid', 'cells', 'biologicalAgent');
  SYSTEMS_TFhirBiologicallyDerivedProductCategoryEnum : Array[TFhirBiologicallyDerivedProductCategoryEnum] of String = ('', 'http://hl7.org/fhir/product-category', 'http://hl7.org/fhir/product-category', 'http://hl7.org/fhir/product-category', 'http://hl7.org/fhir/product-category', 'http://hl7.org/fhir/product-category');
  CODES_TFhirBiologicallyDerivedProductStatusEnum : Array[TFhirBiologicallyDerivedProductStatusEnum] of String = ('', 'available', 'unavailable');
  SYSTEMS_TFhirBiologicallyDerivedProductStatusEnum : Array[TFhirBiologicallyDerivedProductStatusEnum] of String = ('', 'http://hl7.org/fhir/product-status', 'http://hl7.org/fhir/product-status');
  CODES_TFhirBiologicallyDerivedProductStorageScaleEnum : Array[TFhirBiologicallyDerivedProductStorageScaleEnum] of String = ('', 'farenheit', 'celsius', 'kelvin');
  SYSTEMS_TFhirBiologicallyDerivedProductStorageScaleEnum : Array[TFhirBiologicallyDerivedProductStorageScaleEnum] of String = ('', 'http://hl7.org/fhir/product-storage-scale', 'http://hl7.org/fhir/product-storage-scale', 'http://hl7.org/fhir/product-storage-scale');
  CODES_TFhirBundleTypeEnum : Array[TFhirBundleTypeEnum] of String = ('', 'document', 'message', 'transaction', 'transaction-response', 'batch', 'batch-response', 'history', 'searchset', 'collection', 'subscription-notification');
  SYSTEMS_TFhirBundleTypeEnum : Array[TFhirBundleTypeEnum] of String = ('', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type');
  CODES_TFhirCapabilityStatementKindEnum : Array[TFhirCapabilityStatementKindEnum] of String = ('', 'instance', 'capability', 'requirements');
  SYSTEMS_TFhirCapabilityStatementKindEnum : Array[TFhirCapabilityStatementKindEnum] of String = ('', 'http://hl7.org/fhir/capability-statement-kind', 'http://hl7.org/fhir/capability-statement-kind', 'http://hl7.org/fhir/capability-statement-kind');
  CODES_TFhirCarePlanActivityKindEnum : Array[TFhirCarePlanActivityKindEnum] of String = ('', 'Appointment', 'CommunicationRequest', 'DeviceRequest', 'MedicationRequest', 'NutritionOrder', 'Task', 'ServiceRequest', 'VisionPrescription');
  SYSTEMS_TFhirCarePlanActivityKindEnum : Array[TFhirCarePlanActivityKindEnum] of String = ('', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types');
  CODES_TFhirCarePlanActivityStatusEnum : Array[TFhirCarePlanActivityStatusEnum] of String = ('', 'not-started', 'scheduled', 'in-progress', 'on-hold', 'completed', 'cancelled', 'stopped', 'unknown', 'entered-in-error');
  SYSTEMS_TFhirCarePlanActivityStatusEnum : Array[TFhirCarePlanActivityStatusEnum] of String = ('', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status');
  CODES_TFhirCarePlanIntentEnum : Array[TFhirCarePlanIntentEnum] of String = ('', 'proposal', 'plan', 'order', 'option', 'directive');
  SYSTEMS_TFhirCarePlanIntentEnum : Array[TFhirCarePlanIntentEnum] of String = ('', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent');
  CODES_TFhirCareTeamStatusEnum : Array[TFhirCareTeamStatusEnum] of String = ('', 'proposed', 'active', 'suspended', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirCareTeamStatusEnum : Array[TFhirCareTeamStatusEnum] of String = ('', 'http://hl7.org/fhir/care-team-status', 'http://hl7.org/fhir/care-team-status', 'http://hl7.org/fhir/care-team-status', 'http://hl7.org/fhir/care-team-status', 'http://hl7.org/fhir/care-team-status');
  CODES_TFhirCatalogEntryRelationTypeEnum : Array[TFhirCatalogEntryRelationTypeEnum] of String = ('', 'triggers', 'is-replaced-by', 'excludes', 'includes');
  SYSTEMS_TFhirCatalogEntryRelationTypeEnum : Array[TFhirCatalogEntryRelationTypeEnum] of String = ('', 'http://hl7.org/fhir/catalogentry-relation-type', 'http://hl7.org/fhir/catalogentry-relation-type', 'http://hl7.org/fhir/catalogentry-relation-type', 'http://hl7.org/fhir/catalogentry-relation-type');
  CODES_TFhirCatalogEntryStatusEnum : Array[TFhirCatalogEntryStatusEnum] of String = ('', 'draft', 'active', 'retired');
  SYSTEMS_TFhirCatalogEntryStatusEnum : Array[TFhirCatalogEntryStatusEnum] of String = ('', 'http://hl7.org/fhir/catalogentry-status', 'http://hl7.org/fhir/catalogentry-status', 'http://hl7.org/fhir/catalogentry-status');
  CODES_TFhirCatalogEntryTypeEnum : Array[TFhirCatalogEntryTypeEnum] of String = ('', 'ActivityDefinition', 'PlanDefinition', 'SpecimenDefinition', 'ObservationDefinition', 'DeviceDefinition', 'Organization', 'Practitioner', 'PractitionerRole', 'HealthcareService', 'MedicationKnowledge', 'Medication', 'Substance', 'Location');
  SYSTEMS_TFhirCatalogEntryTypeEnum : Array[TFhirCatalogEntryTypeEnum] of String = ('', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type', 'http://hl7.org/fhir/catalogentry-type');
  CODES_TFhirCharacteristicCombinationEnum : Array[TFhirCharacteristicCombinationEnum] of String = ('', 'intersection', 'union');
  SYSTEMS_TFhirCharacteristicCombinationEnum : Array[TFhirCharacteristicCombinationEnum] of String = ('', 'http://hl7.org/fhir/characteristic-combination', 'http://hl7.org/fhir/characteristic-combination');
  CODES_TFhirChargeItemStatusEnum : Array[TFhirChargeItemStatusEnum] of String = ('', 'planned', 'billable', 'not-billable', 'aborted', 'billed', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirChargeItemStatusEnum : Array[TFhirChargeItemStatusEnum] of String = ('', 'http://hl7.org/fhir/chargeitem-status', 'http://hl7.org/fhir/chargeitem-status', 'http://hl7.org/fhir/chargeitem-status', 'http://hl7.org/fhir/chargeitem-status', 'http://hl7.org/fhir/chargeitem-status', 'http://hl7.org/fhir/chargeitem-status', 'http://hl7.org/fhir/chargeitem-status');
  CODES_TFhirClaimProcessingCodesEnum : Array[TFhirClaimProcessingCodesEnum] of String = ('', 'queued', 'complete', 'error', 'partial');
  SYSTEMS_TFhirClaimProcessingCodesEnum : Array[TFhirClaimProcessingCodesEnum] of String = ('', 'http://hl7.org/fhir/remittance-outcome', 'http://hl7.org/fhir/remittance-outcome', 'http://hl7.org/fhir/remittance-outcome', 'http://hl7.org/fhir/remittance-outcome');
  CODES_TFhirClinicalUseIssueTypeEnum : Array[TFhirClinicalUseIssueTypeEnum] of String = ('', 'indication', 'contraindication', 'interaction', 'undesirable-effect', 'warning');
  SYSTEMS_TFhirClinicalUseIssueTypeEnum : Array[TFhirClinicalUseIssueTypeEnum] of String = ('', 'http://hl7.org/fhir/clinical-use-issue-type', 'http://hl7.org/fhir/clinical-use-issue-type', 'http://hl7.org/fhir/clinical-use-issue-type', 'http://hl7.org/fhir/clinical-use-issue-type', 'http://hl7.org/fhir/clinical-use-issue-type');
  CODES_TFhirCodeSearchSupportEnum : Array[TFhirCodeSearchSupportEnum] of String = ('', 'explicit', 'all');
  SYSTEMS_TFhirCodeSearchSupportEnum : Array[TFhirCodeSearchSupportEnum] of String = ('', 'http://hl7.org/fhir/code-search-support', 'http://hl7.org/fhir/code-search-support');
  CODES_TFhirCodeSystemContentModeEnum : Array[TFhirCodeSystemContentModeEnum] of String = ('', 'not-present', 'example', 'fragment', 'complete', 'supplement');
  SYSTEMS_TFhirCodeSystemContentModeEnum : Array[TFhirCodeSystemContentModeEnum] of String = ('', 'http://hl7.org/fhir/codesystem-content-mode', 'http://hl7.org/fhir/codesystem-content-mode', 'http://hl7.org/fhir/codesystem-content-mode', 'http://hl7.org/fhir/codesystem-content-mode', 'http://hl7.org/fhir/codesystem-content-mode');
  CODES_TFhirCodeSystemHierarchyMeaningEnum : Array[TFhirCodeSystemHierarchyMeaningEnum] of String = ('', 'grouped-by', 'is-a', 'part-of', 'classified-with');
  SYSTEMS_TFhirCodeSystemHierarchyMeaningEnum : Array[TFhirCodeSystemHierarchyMeaningEnum] of String = ('', 'http://hl7.org/fhir/codesystem-hierarchy-meaning', 'http://hl7.org/fhir/codesystem-hierarchy-meaning', 'http://hl7.org/fhir/codesystem-hierarchy-meaning', 'http://hl7.org/fhir/codesystem-hierarchy-meaning');
  CODES_TFhirCompartmentTypeEnum : Array[TFhirCompartmentTypeEnum] of String = ('', 'Patient', 'Encounter', 'RelatedPerson', 'Practitioner', 'Device');
  SYSTEMS_TFhirCompartmentTypeEnum : Array[TFhirCompartmentTypeEnum] of String = ('', 'http://hl7.org/fhir/compartment-type', 'http://hl7.org/fhir/compartment-type', 'http://hl7.org/fhir/compartment-type', 'http://hl7.org/fhir/compartment-type', 'http://hl7.org/fhir/compartment-type');
  CODES_TFhirCompositionAttestationModeEnum : Array[TFhirCompositionAttestationModeEnum] of String = ('', 'personal', 'professional', 'legal', 'official');
  SYSTEMS_TFhirCompositionAttestationModeEnum : Array[TFhirCompositionAttestationModeEnum] of String = ('', 'http://hl7.org/fhir/composition-attestation-mode', 'http://hl7.org/fhir/composition-attestation-mode', 'http://hl7.org/fhir/composition-attestation-mode', 'http://hl7.org/fhir/composition-attestation-mode');
  CODES_TFhirCompositionStatusEnum : Array[TFhirCompositionStatusEnum] of String = ('', 'preliminary', 'final', 'amended', 'entered-in-error');
  SYSTEMS_TFhirCompositionStatusEnum : Array[TFhirCompositionStatusEnum] of String = ('', 'http://hl7.org/fhir/composition-status', 'http://hl7.org/fhir/composition-status', 'http://hl7.org/fhir/composition-status', 'http://hl7.org/fhir/composition-status');
  CODES_TFhirConceptMapGroupUnmappedModeEnum : Array[TFhirConceptMapGroupUnmappedModeEnum] of String = ('', 'provided', 'fixed', 'other-map');
  SYSTEMS_TFhirConceptMapGroupUnmappedModeEnum : Array[TFhirConceptMapGroupUnmappedModeEnum] of String = ('', 'http://hl7.org/fhir/conceptmap-unmapped-mode', 'http://hl7.org/fhir/conceptmap-unmapped-mode', 'http://hl7.org/fhir/conceptmap-unmapped-mode');
  CODES_TFhirConceptMapRelationshipEnum : Array[TFhirConceptMapRelationshipEnum] of String = ('', 'related-to', 'equivalent', 'source-is-narrower-than-target', 'source-is-broader-than-target', 'not-related-to');
  SYSTEMS_TFhirConceptMapRelationshipEnum : Array[TFhirConceptMapRelationshipEnum] of String = ('', 'http://hl7.org/fhir/concept-map-relationship', 'http://hl7.org/fhir/concept-map-relationship', 'http://hl7.org/fhir/concept-map-relationship', 'http://hl7.org/fhir/concept-map-relationship', 'http://hl7.org/fhir/concept-map-relationship');
  CODES_TFhirConceptPropertyTypeEnum : Array[TFhirConceptPropertyTypeEnum] of String = ('', 'code', 'Coding', 'string', 'integer', 'boolean', 'dateTime', 'decimal');
  SYSTEMS_TFhirConceptPropertyTypeEnum : Array[TFhirConceptPropertyTypeEnum] of String = ('', 'http://hl7.org/fhir/concept-property-type', 'http://hl7.org/fhir/concept-property-type', 'http://hl7.org/fhir/concept-property-type', 'http://hl7.org/fhir/concept-property-type', 'http://hl7.org/fhir/concept-property-type', 'http://hl7.org/fhir/concept-property-type', 'http://hl7.org/fhir/concept-property-type');
  CODES_TFhirConditionPreconditionTypeEnum : Array[TFhirConditionPreconditionTypeEnum] of String = ('', 'sensitive', 'specific');
  SYSTEMS_TFhirConditionPreconditionTypeEnum : Array[TFhirConditionPreconditionTypeEnum] of String = ('', 'http://hl7.org/fhir/condition-precondition-type', 'http://hl7.org/fhir/condition-precondition-type');
  CODES_TFhirConditionQuestionnairePurposeEnum : Array[TFhirConditionQuestionnairePurposeEnum] of String = ('', 'preadmit', 'diff-diagnosis', 'outcome');
  SYSTEMS_TFhirConditionQuestionnairePurposeEnum : Array[TFhirConditionQuestionnairePurposeEnum] of String = ('', 'http://hl7.org/fhir/condition-questionnaire-purpose', 'http://hl7.org/fhir/condition-questionnaire-purpose', 'http://hl7.org/fhir/condition-questionnaire-purpose');
  CODES_TFhirConditionalDeleteStatusEnum : Array[TFhirConditionalDeleteStatusEnum] of String = ('', 'not-supported', 'single', 'multiple');
  SYSTEMS_TFhirConditionalDeleteStatusEnum : Array[TFhirConditionalDeleteStatusEnum] of String = ('', 'http://hl7.org/fhir/conditional-delete-status', 'http://hl7.org/fhir/conditional-delete-status', 'http://hl7.org/fhir/conditional-delete-status');
  CODES_TFhirConditionalReadStatusEnum : Array[TFhirConditionalReadStatusEnum] of String = ('', 'not-supported', 'modified-since', 'not-match', 'full-support');
  SYSTEMS_TFhirConditionalReadStatusEnum : Array[TFhirConditionalReadStatusEnum] of String = ('', 'http://hl7.org/fhir/conditional-read-status', 'http://hl7.org/fhir/conditional-read-status', 'http://hl7.org/fhir/conditional-read-status', 'http://hl7.org/fhir/conditional-read-status');
  CODES_TFhirConsentDataMeaningEnum : Array[TFhirConsentDataMeaningEnum] of String = ('', 'instance', 'related', 'dependents', 'authoredby');
  SYSTEMS_TFhirConsentDataMeaningEnum : Array[TFhirConsentDataMeaningEnum] of String = ('', 'http://hl7.org/fhir/consent-data-meaning', 'http://hl7.org/fhir/consent-data-meaning', 'http://hl7.org/fhir/consent-data-meaning', 'http://hl7.org/fhir/consent-data-meaning');
  CODES_TFhirConsentProvisionTypeEnum : Array[TFhirConsentProvisionTypeEnum] of String = ('', 'deny', 'permit');
  SYSTEMS_TFhirConsentProvisionTypeEnum : Array[TFhirConsentProvisionTypeEnum] of String = ('', 'http://hl7.org/fhir/consent-provision-type', 'http://hl7.org/fhir/consent-provision-type');
  CODES_TFhirConsentStateEnum : Array[TFhirConsentStateEnum] of String = ('', 'draft', 'active', 'inactive', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirConsentStateEnum : Array[TFhirConsentStateEnum] of String = ('', 'http://hl7.org/fhir/consent-state-codes', 'http://hl7.org/fhir/consent-state-codes', 'http://hl7.org/fhir/consent-state-codes', 'http://hl7.org/fhir/consent-state-codes', 'http://hl7.org/fhir/consent-state-codes');
  CODES_TFhirConstraintSeverityEnum : Array[TFhirConstraintSeverityEnum] of String = ('', 'error', 'warning');
  SYSTEMS_TFhirConstraintSeverityEnum : Array[TFhirConstraintSeverityEnum] of String = ('', 'http://hl7.org/fhir/constraint-severity', 'http://hl7.org/fhir/constraint-severity');
  CODES_TFhirContactPointSystemEnum : Array[TFhirContactPointSystemEnum] of String = ('', 'phone', 'fax', 'email', 'pager', 'url', 'sms', 'other');
  SYSTEMS_TFhirContactPointSystemEnum : Array[TFhirContactPointSystemEnum] of String = ('', 'http://hl7.org/fhir/contact-point-system', 'http://hl7.org/fhir/contact-point-system', 'http://hl7.org/fhir/contact-point-system', 'http://hl7.org/fhir/contact-point-system', 'http://hl7.org/fhir/contact-point-system', 'http://hl7.org/fhir/contact-point-system', 'http://hl7.org/fhir/contact-point-system');
  CODES_TFhirContactPointUseEnum : Array[TFhirContactPointUseEnum] of String = ('', 'home', 'work', 'temp', 'old', 'mobile');
  SYSTEMS_TFhirContactPointUseEnum : Array[TFhirContactPointUseEnum] of String = ('', 'http://hl7.org/fhir/contact-point-use', 'http://hl7.org/fhir/contact-point-use', 'http://hl7.org/fhir/contact-point-use', 'http://hl7.org/fhir/contact-point-use', 'http://hl7.org/fhir/contact-point-use');
  CODES_TFhirContractResourcePublicationStatusCodesEnum : Array[TFhirContractResourcePublicationStatusCodesEnum] of String = ('', 'amended', 'appended', 'cancelled', 'disputed', 'entered-in-error', 'executable', 'executed', 'negotiable', 'offered', 'policy', 'rejected', 'renewed', 'revoked', 'resolved', 'terminated');
  SYSTEMS_TFhirContractResourcePublicationStatusCodesEnum : Array[TFhirContractResourcePublicationStatusCodesEnum] of String = ('', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus', 'http://hl7.org/fhir/contract-publicationstatus');
  CODES_TFhirContractResourceStatusCodesEnum : Array[TFhirContractResourceStatusCodesEnum] of String = ('', 'amended', 'appended', 'cancelled', 'disputed', 'entered-in-error', 'executable', 'executed', 'negotiable', 'offered', 'policy', 'rejected', 'renewed', 'revoked', 'resolved', 'terminated');
  SYSTEMS_TFhirContractResourceStatusCodesEnum : Array[TFhirContractResourceStatusCodesEnum] of String = ('', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status', 'http://hl7.org/fhir/contract-status');
  CODES_TFhirContributorTypeEnum : Array[TFhirContributorTypeEnum] of String = ('', 'author', 'editor', 'reviewer', 'endorser');
  SYSTEMS_TFhirContributorTypeEnum : Array[TFhirContributorTypeEnum] of String = ('', 'http://hl7.org/fhir/contributor-type', 'http://hl7.org/fhir/contributor-type', 'http://hl7.org/fhir/contributor-type', 'http://hl7.org/fhir/contributor-type');
  CODES_TFhirDaysOfWeekEnum : Array[TFhirDaysOfWeekEnum] of String = ('', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun');
  SYSTEMS_TFhirDaysOfWeekEnum : Array[TFhirDaysOfWeekEnum] of String = ('', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week');
  CODES_TFhirDetectedIssueSeverityEnum : Array[TFhirDetectedIssueSeverityEnum] of String = ('', 'high', 'moderate', 'low');
  SYSTEMS_TFhirDetectedIssueSeverityEnum : Array[TFhirDetectedIssueSeverityEnum] of String = ('', 'http://hl7.org/fhir/detectedissue-severity', 'http://hl7.org/fhir/detectedissue-severity', 'http://hl7.org/fhir/detectedissue-severity');
  CODES_TFhirDeviceMetricCalibrationStateEnum : Array[TFhirDeviceMetricCalibrationStateEnum] of String = ('', 'not-calibrated', 'calibration-required', 'calibrated', 'unspecified');
  SYSTEMS_TFhirDeviceMetricCalibrationStateEnum : Array[TFhirDeviceMetricCalibrationStateEnum] of String = ('', 'http://hl7.org/fhir/metric-calibration-state', 'http://hl7.org/fhir/metric-calibration-state', 'http://hl7.org/fhir/metric-calibration-state', 'http://hl7.org/fhir/metric-calibration-state');
  CODES_TFhirDeviceMetricCalibrationTypeEnum : Array[TFhirDeviceMetricCalibrationTypeEnum] of String = ('', 'unspecified', 'offset', 'gain', 'two-point');
  SYSTEMS_TFhirDeviceMetricCalibrationTypeEnum : Array[TFhirDeviceMetricCalibrationTypeEnum] of String = ('', 'http://hl7.org/fhir/metric-calibration-type', 'http://hl7.org/fhir/metric-calibration-type', 'http://hl7.org/fhir/metric-calibration-type', 'http://hl7.org/fhir/metric-calibration-type');
  CODES_TFhirDeviceMetricCategoryEnum : Array[TFhirDeviceMetricCategoryEnum] of String = ('', 'measurement', 'setting', 'calculation', 'unspecified');
  SYSTEMS_TFhirDeviceMetricCategoryEnum : Array[TFhirDeviceMetricCategoryEnum] of String = ('', 'http://hl7.org/fhir/metric-category', 'http://hl7.org/fhir/metric-category', 'http://hl7.org/fhir/metric-category', 'http://hl7.org/fhir/metric-category');
  CODES_TFhirDeviceMetricColorEnum : Array[TFhirDeviceMetricColorEnum] of String = ('', 'black', 'red', 'green', 'yellow', 'blue', 'magenta', 'cyan', 'white');
  SYSTEMS_TFhirDeviceMetricColorEnum : Array[TFhirDeviceMetricColorEnum] of String = ('', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color');
  CODES_TFhirDeviceMetricOperationalStatusEnum : Array[TFhirDeviceMetricOperationalStatusEnum] of String = ('', 'on', 'off', 'standby', 'entered-in-error');
  SYSTEMS_TFhirDeviceMetricOperationalStatusEnum : Array[TFhirDeviceMetricOperationalStatusEnum] of String = ('', 'http://hl7.org/fhir/metric-operational-status', 'http://hl7.org/fhir/metric-operational-status', 'http://hl7.org/fhir/metric-operational-status', 'http://hl7.org/fhir/metric-operational-status');
  CODES_TFhirDeviceNameTypeEnum : Array[TFhirDeviceNameTypeEnum] of String = ('', 'udi-label-name', 'user-friendly-name', 'patient-reported-name', 'manufacturer-name', 'model-name', 'other');
  SYSTEMS_TFhirDeviceNameTypeEnum : Array[TFhirDeviceNameTypeEnum] of String = ('', 'http://hl7.org/fhir/device-nametype', 'http://hl7.org/fhir/device-nametype', 'http://hl7.org/fhir/device-nametype', 'http://hl7.org/fhir/device-nametype', 'http://hl7.org/fhir/device-nametype', 'http://hl7.org/fhir/device-nametype');
  CODES_TFhirDeviceUseStatementStatusEnum : Array[TFhirDeviceUseStatementStatusEnum] of String = ('', 'active', 'completed', 'entered-in-error', 'intended', 'stopped', 'on-hold');
  SYSTEMS_TFhirDeviceUseStatementStatusEnum : Array[TFhirDeviceUseStatementStatusEnum] of String = ('', 'http://hl7.org/fhir/device-statement-status', 'http://hl7.org/fhir/device-statement-status', 'http://hl7.org/fhir/device-statement-status', 'http://hl7.org/fhir/device-statement-status', 'http://hl7.org/fhir/device-statement-status', 'http://hl7.org/fhir/device-statement-status');
  CODES_TFhirDiagnosticReportStatusEnum : Array[TFhirDiagnosticReportStatusEnum] of String = ('', 'registered', 'partial', 'preliminary', 'final', 'amended', 'corrected', 'appended', 'cancelled', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirDiagnosticReportStatusEnum : Array[TFhirDiagnosticReportStatusEnum] of String = ('', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status');
  CODES_TFhirDiscriminatorTypeEnum : Array[TFhirDiscriminatorTypeEnum] of String = ('', 'value', 'exists', 'pattern', 'type', 'profile');
  SYSTEMS_TFhirDiscriminatorTypeEnum : Array[TFhirDiscriminatorTypeEnum] of String = ('', 'http://hl7.org/fhir/discriminator-type', 'http://hl7.org/fhir/discriminator-type', 'http://hl7.org/fhir/discriminator-type', 'http://hl7.org/fhir/discriminator-type', 'http://hl7.org/fhir/discriminator-type');
  CODES_TFhirDocumentAttestationModeEnum : Array[TFhirDocumentAttestationModeEnum] of String = ('', 'personal', 'professional', 'legal', 'official');
  SYSTEMS_TFhirDocumentAttestationModeEnum : Array[TFhirDocumentAttestationModeEnum] of String = ('', 'http://hl7.org/fhir/document-attestation-mode', 'http://hl7.org/fhir/document-attestation-mode', 'http://hl7.org/fhir/document-attestation-mode', 'http://hl7.org/fhir/document-attestation-mode');
  CODES_TFhirDocumentModeEnum : Array[TFhirDocumentModeEnum] of String = ('', 'producer', 'consumer');
  SYSTEMS_TFhirDocumentModeEnum : Array[TFhirDocumentModeEnum] of String = ('', 'http://hl7.org/fhir/document-mode', 'http://hl7.org/fhir/document-mode');
  CODES_TFhirDocumentReferenceStatusEnum : Array[TFhirDocumentReferenceStatusEnum] of String = ('', 'current', 'superseded', 'entered-in-error');
  SYSTEMS_TFhirDocumentReferenceStatusEnum : Array[TFhirDocumentReferenceStatusEnum] of String = ('', 'http://hl7.org/fhir/document-reference-status', 'http://hl7.org/fhir/document-reference-status', 'http://hl7.org/fhir/document-reference-status');
  CODES_TFhirDocumentRelationshipTypeEnum : Array[TFhirDocumentRelationshipTypeEnum] of String = ('', 'replaces', 'transforms', 'signs', 'appends');
  SYSTEMS_TFhirDocumentRelationshipTypeEnum : Array[TFhirDocumentRelationshipTypeEnum] of String = ('', 'http://hl7.org/fhir/document-relationship-type', 'http://hl7.org/fhir/document-relationship-type', 'http://hl7.org/fhir/document-relationship-type', 'http://hl7.org/fhir/document-relationship-type');
  CODES_TFhirEligibilityRequestPurposeEnum : Array[TFhirEligibilityRequestPurposeEnum] of String = ('', 'auth-requirements', 'benefits', 'discovery', 'validation');
  SYSTEMS_TFhirEligibilityRequestPurposeEnum : Array[TFhirEligibilityRequestPurposeEnum] of String = ('', 'http://hl7.org/fhir/eligibilityrequest-purpose', 'http://hl7.org/fhir/eligibilityrequest-purpose', 'http://hl7.org/fhir/eligibilityrequest-purpose', 'http://hl7.org/fhir/eligibilityrequest-purpose');
  CODES_TFhirEligibilityResponsePurposeEnum : Array[TFhirEligibilityResponsePurposeEnum] of String = ('', 'auth-requirements', 'benefits', 'discovery', 'validation');
  SYSTEMS_TFhirEligibilityResponsePurposeEnum : Array[TFhirEligibilityResponsePurposeEnum] of String = ('', 'http://hl7.org/fhir/eligibilityresponse-purpose', 'http://hl7.org/fhir/eligibilityresponse-purpose', 'http://hl7.org/fhir/eligibilityresponse-purpose', 'http://hl7.org/fhir/eligibilityresponse-purpose');
  CODES_TFhirEnableWhenBehaviorEnum : Array[TFhirEnableWhenBehaviorEnum] of String = ('', 'all', 'any');
  SYSTEMS_TFhirEnableWhenBehaviorEnum : Array[TFhirEnableWhenBehaviorEnum] of String = ('', 'http://hl7.org/fhir/questionnaire-enable-behavior', 'http://hl7.org/fhir/questionnaire-enable-behavior');
  CODES_TFhirEncounterLocationStatusEnum : Array[TFhirEncounterLocationStatusEnum] of String = ('', 'planned', 'active', 'reserved', 'completed');
  SYSTEMS_TFhirEncounterLocationStatusEnum : Array[TFhirEncounterLocationStatusEnum] of String = ('', 'http://hl7.org/fhir/encounter-location-status', 'http://hl7.org/fhir/encounter-location-status', 'http://hl7.org/fhir/encounter-location-status', 'http://hl7.org/fhir/encounter-location-status');
  CODES_TFhirEncounterStatusEnum : Array[TFhirEncounterStatusEnum] of String = ('', 'planned', 'in-progress', 'onhold', 'completed', 'cancelled', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirEncounterStatusEnum : Array[TFhirEncounterStatusEnum] of String = ('', 'http://hl7.org/fhir/encounter-status', 'http://hl7.org/fhir/encounter-status', 'http://hl7.org/fhir/encounter-status', 'http://hl7.org/fhir/encounter-status', 'http://hl7.org/fhir/encounter-status', 'http://hl7.org/fhir/encounter-status', 'http://hl7.org/fhir/encounter-status');
  CODES_TFhirEndpointStatusEnum : Array[TFhirEndpointStatusEnum] of String = ('', 'active', 'suspended', 'error', 'off', 'entered-in-error', 'test');
  SYSTEMS_TFhirEndpointStatusEnum : Array[TFhirEndpointStatusEnum] of String = ('', 'http://hl7.org/fhir/endpoint-status', 'http://hl7.org/fhir/endpoint-status', 'http://hl7.org/fhir/endpoint-status', 'http://hl7.org/fhir/endpoint-status', 'http://hl7.org/fhir/endpoint-status', 'http://hl7.org/fhir/endpoint-status');
  CODES_TFhirEpisodeOfCareStatusEnum : Array[TFhirEpisodeOfCareStatusEnum] of String = ('', 'planned', 'waitlist', 'active', 'onhold', 'finished', 'cancelled', 'entered-in-error');
  SYSTEMS_TFhirEpisodeOfCareStatusEnum : Array[TFhirEpisodeOfCareStatusEnum] of String = ('', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status');
  CODES_TFhirEventCapabilityModeEnum : Array[TFhirEventCapabilityModeEnum] of String = ('', 'sender', 'receiver');
  SYSTEMS_TFhirEventCapabilityModeEnum : Array[TFhirEventCapabilityModeEnum] of String = ('', 'http://hl7.org/fhir/event-capability-mode', 'http://hl7.org/fhir/event-capability-mode');
  CODES_TFhirEventStatusEnum : Array[TFhirEventStatusEnum] of String = ('', 'preparation', 'in-progress', 'not-done', 'on-hold', 'stopped', 'completed', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirEventStatusEnum : Array[TFhirEventStatusEnum] of String = ('', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status');
  CODES_TFhirEventTimingEnum : Array[TFhirEventTimingEnum] of String = ('', 'MORN', 'MORN.early', 'MORN.late', 'NOON', 'AFT', 'AFT.early', 'AFT.late', 'EVE', 'EVE.early', 'EVE.late', 'NIGHT', 'PHS', 'HS', 'WAKE', 'C', 'CM', 'CD', 'CV', 'AC', 'ACM', 'ACD', 'ACV', 'PC', 'PCM', 'PCD', 'PCV');
  SYSTEMS_TFhirEventTimingEnum : Array[TFhirEventTimingEnum] of String = ('', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://hl7.org/fhir/event-timing', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent',
       'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent', 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent');
  CODES_TFhirEvidenceVariableHandlingEnum : Array[TFhirEvidenceVariableHandlingEnum] of String = ('', 'continuous', 'dichotomous', 'ordinal', 'polychotomous');
  SYSTEMS_TFhirEvidenceVariableHandlingEnum : Array[TFhirEvidenceVariableHandlingEnum] of String = ('', 'http://hl7.org/fhir/variable-handling', 'http://hl7.org/fhir/variable-handling', 'http://hl7.org/fhir/variable-handling', 'http://hl7.org/fhir/variable-handling');
  CODES_TFhirExampleScenarioActorTypeEnum : Array[TFhirExampleScenarioActorTypeEnum] of String = ('', 'person', 'entity');
  SYSTEMS_TFhirExampleScenarioActorTypeEnum : Array[TFhirExampleScenarioActorTypeEnum] of String = ('', 'http://hl7.org/fhir/examplescenario-actor-type', 'http://hl7.org/fhir/examplescenario-actor-type');
  CODES_TFhirExplanationOfBenefitStatusEnum : Array[TFhirExplanationOfBenefitStatusEnum] of String = ('', 'active', 'cancelled', 'draft', 'entered-in-error');
  SYSTEMS_TFhirExplanationOfBenefitStatusEnum : Array[TFhirExplanationOfBenefitStatusEnum] of String = ('', 'http://hl7.org/fhir/explanationofbenefit-status', 'http://hl7.org/fhir/explanationofbenefit-status', 'http://hl7.org/fhir/explanationofbenefit-status', 'http://hl7.org/fhir/explanationofbenefit-status');
  CODES_TFhirExtensionContextTypeEnum : Array[TFhirExtensionContextTypeEnum] of String = ('', 'fhirpath', 'element', 'extension');
  SYSTEMS_TFhirExtensionContextTypeEnum : Array[TFhirExtensionContextTypeEnum] of String = ('', 'http://hl7.org/fhir/extension-context-type', 'http://hl7.org/fhir/extension-context-type', 'http://hl7.org/fhir/extension-context-type');
  CODES_TFhirFHIRDefinedTypeEnum : Array[TFhirFHIRDefinedTypeEnum] of String = ('', 'Address', 'Age', 'Annotation', 'Attachment', 'BackboneElement', 'BackboneType', 'Base', 'CodeableConcept', 'CodeableReference', 'Coding', 'ContactDetail', 'ContactPoint', 'Contributor', 'Count', 'DataRequirement', 'DataType', 'Distance', 'Dosage', 'Duration', 'Element', 'ElementDefinition', 'Expression', 'Extension', 'HumanName', 'Identifier', 'MarketingStatus', 'Meta', 'Money', 'MoneyQuantity', 'Narrative', 'OrderedDistribution', 'ParameterDefinition', 'Period', 'Population', 'PrimitiveType', 'ProdCharacteristic', 'ProductShelfLife', 'Quantity', 'Range', 'Ratio', 'Reference', 'RelatedArtifact', 'SampledData', 'Signature', 'SimpleQuantity', 'Statistic', 'Timing', 'TriggerDefinition', 'UsageContext', 'base64Binary', 'boolean', 'canonical', 'code', 'date', 'dateTime', 'decimal', 'id', 'instant', 'integer', 'integer64', 'markdown', 'oid', 'positiveInt', 'string', 'time', 'unsignedInt', 'uri', 'url', 'uuid', 'xhtml',
       'Account', 'ActivityDefinition', 'AdministrableProductDefinition', 'AdverseEvent', 'AllergyIntolerance', 'Appointment', 'AppointmentResponse', 'AuditEvent', 'Basic', 'Binary', 'BiologicallyDerivedProduct', 'BodyStructure', 'Bundle', 'CapabilityStatement', 'CapabilityStatement2', 'CarePlan', 'CareTeam', 'CatalogEntry', 'ChargeItem', 'ChargeItemDefinition', 'Citation', 'Claim', 'ClaimResponse', 'ClinicalImpression', 'ClinicalUseIssue', 'CodeSystem', 'Communication', 'CommunicationRequest', 'CompartmentDefinition', 'Composition', 'ConceptMap', 'Condition', 'ConditionDefinition', 'Consent', 'Contract', 'Coverage', 'CoverageEligibilityRequest', 'CoverageEligibilityResponse', 'DetectedIssue', 'Device', 'DeviceDefinition', 'DeviceMetric', 'DeviceRequest', 'DeviceUseStatement', 'DiagnosticReport', 'DocumentManifest', 'DocumentReference', 'DomainResource', 'Encounter', 'Endpoint', 'EnrollmentRequest', 'EnrollmentResponse', 'EpisodeOfCare', 'EventDefinition', 'Evidence', 'EvidenceReport',
       'EvidenceVariable', 'ExampleScenario', 'ExplanationOfBenefit', 'FamilyMemberHistory', 'Flag', 'Goal', 'GraphDefinition', 'Group', 'GuidanceResponse', 'HealthcareService', 'ImagingStudy', 'Immunization', 'ImmunizationEvaluation', 'ImmunizationRecommendation', 'ImplementationGuide', 'Ingredient', 'InsurancePlan', 'Invoice', 'Library', 'Linkage', 'List', 'Location', 'ManufacturedItemDefinition', 'Measure', 'MeasureReport', 'Medication', 'MedicationAdministration', 'MedicationDispense', 'MedicationKnowledge', 'MedicationRequest', 'MedicationUsage', 'MedicinalProductDefinition', 'MessageDefinition', 'MessageHeader', 'MolecularSequence', 'NamingSystem', 'NutritionIntake', 'NutritionOrder', 'NutritionProduct', 'Observation', 'ObservationDefinition', 'OperationDefinition', 'OperationOutcome', 'Organization', 'OrganizationAffiliation', 'PackagedProductDefinition', 'Parameters', 'Patient', 'PaymentNotice', 'PaymentReconciliation', 'Permission', 'Person', 'PlanDefinition', 'Practitioner',
       'PractitionerRole', 'Procedure', 'Provenance', 'Questionnaire', 'QuestionnaireResponse', 'RegulatedAuthorization', 'RelatedPerson', 'RequestGroup', 'ResearchStudy', 'ResearchSubject', 'Resource', 'RiskAssessment', 'Schedule', 'SearchParameter', 'ServiceRequest', 'Slot', 'Specimen', 'SpecimenDefinition', 'StructureDefinition', 'StructureMap', 'Subscription', 'SubscriptionStatus', 'SubscriptionTopic', 'Substance', 'SubstanceDefinition', 'SubstanceNucleicAcid', 'SubstancePolymer', 'SubstanceProtein', 'SubstanceReferenceInformation', 'SubstanceSourceMaterial', 'SupplyDelivery', 'SupplyRequest', 'Task', 'TerminologyCapabilities', 'TestReport', 'TestScript', 'ValueSet', 'VerificationResult', 'VisionPrescription');
  SYSTEMS_TFhirFHIRDefinedTypeEnum : Array[TFhirFHIRDefinedTypeEnum] of String = ('', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types',
       'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types',
       'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types');
  CODES_TFhirFHIRDeviceStatusEnum : Array[TFhirFHIRDeviceStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirFHIRDeviceStatusEnum : Array[TFhirFHIRDeviceStatusEnum] of String = ('', 'http://hl7.org/fhir/device-status', 'http://hl7.org/fhir/device-status', 'http://hl7.org/fhir/device-status', 'http://hl7.org/fhir/device-status');
  CODES_TFhirFHIRSubstanceStatusEnum : Array[TFhirFHIRSubstanceStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirFHIRSubstanceStatusEnum : Array[TFhirFHIRSubstanceStatusEnum] of String = ('', 'http://hl7.org/fhir/substance-status', 'http://hl7.org/fhir/substance-status', 'http://hl7.org/fhir/substance-status');
  CODES_TFhirFHIRVersionEnum : Array[TFhirFHIRVersionEnum] of String = ('', '0.01', '0.05', '0.06', '0.11', '0.0.80', '0.0.81', '0.0.82', '0.4.0', '0.5.0', '1.0.0', '1.0.1', '1.0.2', '1.1.0', '1.4.0', '1.6.0', '1.8.0', '3.0.0', '3.0.1', '3.0.2', '3.3.0', '3.5.0', '4.0.0', '4.0.1', '4.1.0', '4.2.0', '4.4.0', '4.5.0');
  SYSTEMS_TFhirFHIRVersionEnum : Array[TFhirFHIRVersionEnum] of String = ('', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version', 'http://hl7.org/fhir/FHIR-version',
       'http://hl7.org/fhir/FHIR-version');
  CODES_TFhirFamilyHistoryStatusEnum : Array[TFhirFamilyHistoryStatusEnum] of String = ('', 'partial', 'completed', 'entered-in-error', 'health-unknown');
  SYSTEMS_TFhirFamilyHistoryStatusEnum : Array[TFhirFamilyHistoryStatusEnum] of String = ('', 'http://hl7.org/fhir/history-status', 'http://hl7.org/fhir/history-status', 'http://hl7.org/fhir/history-status', 'http://hl7.org/fhir/history-status');
  CODES_TFhirFilterOperatorEnum : Array[TFhirFilterOperatorEnum] of String = ('', '=', 'is-a', 'descendent-of', 'is-not-a', 'regex', 'in', 'not-in', 'generalizes', 'exists');
  SYSTEMS_TFhirFilterOperatorEnum : Array[TFhirFilterOperatorEnum] of String = ('', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator');
  CODES_TFhirFinancialResourceStatusCodesEnum : Array[TFhirFinancialResourceStatusCodesEnum] of String = ('', 'active', 'cancelled', 'draft', 'entered-in-error');
  SYSTEMS_TFhirFinancialResourceStatusCodesEnum : Array[TFhirFinancialResourceStatusCodesEnum] of String = ('', 'http://hl7.org/fhir/fm-status', 'http://hl7.org/fhir/fm-status', 'http://hl7.org/fhir/fm-status', 'http://hl7.org/fhir/fm-status');
  CODES_TFhirFlagStatusEnum : Array[TFhirFlagStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirFlagStatusEnum : Array[TFhirFlagStatusEnum] of String = ('', 'http://hl7.org/fhir/flag-status', 'http://hl7.org/fhir/flag-status', 'http://hl7.org/fhir/flag-status');
  CODES_TFhirGoalLifecycleStatusEnum : Array[TFhirGoalLifecycleStatusEnum] of String = ('', 'proposed', 'planned', 'accepted', 'active', 'on-hold', 'completed', 'cancelled', 'entered-in-error', 'rejected');
  SYSTEMS_TFhirGoalLifecycleStatusEnum : Array[TFhirGoalLifecycleStatusEnum] of String = ('', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status');
  CODES_TFhirGraphCompartmentRuleEnum : Array[TFhirGraphCompartmentRuleEnum] of String = ('', 'identical', 'matching', 'different', 'custom');
  SYSTEMS_TFhirGraphCompartmentRuleEnum : Array[TFhirGraphCompartmentRuleEnum] of String = ('', 'http://hl7.org/fhir/graph-compartment-rule', 'http://hl7.org/fhir/graph-compartment-rule', 'http://hl7.org/fhir/graph-compartment-rule', 'http://hl7.org/fhir/graph-compartment-rule');
  CODES_TFhirGraphCompartmentUseEnum : Array[TFhirGraphCompartmentUseEnum] of String = ('', 'condition', 'requirement');
  SYSTEMS_TFhirGraphCompartmentUseEnum : Array[TFhirGraphCompartmentUseEnum] of String = ('', 'http://hl7.org/fhir/graph-compartment-use', 'http://hl7.org/fhir/graph-compartment-use');
  CODES_TFhirGroupMeasureEnum : Array[TFhirGroupMeasureEnum] of String = ('', 'mean', 'median', 'mean-of-mean', 'mean-of-median', 'median-of-mean', 'median-of-median');
  SYSTEMS_TFhirGroupMeasureEnum : Array[TFhirGroupMeasureEnum] of String = ('', 'http://hl7.org/fhir/group-measure', 'http://hl7.org/fhir/group-measure', 'http://hl7.org/fhir/group-measure', 'http://hl7.org/fhir/group-measure', 'http://hl7.org/fhir/group-measure', 'http://hl7.org/fhir/group-measure');
  CODES_TFhirGroupTypeEnum : Array[TFhirGroupTypeEnum] of String = ('', 'person', 'animal', 'practitioner', 'device', 'medication', 'substance');
  SYSTEMS_TFhirGroupTypeEnum : Array[TFhirGroupTypeEnum] of String = ('', 'http://hl7.org/fhir/group-type', 'http://hl7.org/fhir/group-type', 'http://hl7.org/fhir/group-type', 'http://hl7.org/fhir/group-type', 'http://hl7.org/fhir/group-type', 'http://hl7.org/fhir/group-type');
  CODES_TFhirGuidanceResponseStatusEnum : Array[TFhirGuidanceResponseStatusEnum] of String = ('', 'success', 'data-requested', 'data-required', 'in-progress', 'failure', 'entered-in-error');
  SYSTEMS_TFhirGuidanceResponseStatusEnum : Array[TFhirGuidanceResponseStatusEnum] of String = ('', 'http://hl7.org/fhir/guidance-response-status', 'http://hl7.org/fhir/guidance-response-status', 'http://hl7.org/fhir/guidance-response-status', 'http://hl7.org/fhir/guidance-response-status', 'http://hl7.org/fhir/guidance-response-status', 'http://hl7.org/fhir/guidance-response-status');
  CODES_TFhirGuidePageGenerationEnum : Array[TFhirGuidePageGenerationEnum] of String = ('', 'html', 'markdown', 'xml', 'generated');
  SYSTEMS_TFhirGuidePageGenerationEnum : Array[TFhirGuidePageGenerationEnum] of String = ('', 'http://hl7.org/fhir/guide-page-generation', 'http://hl7.org/fhir/guide-page-generation', 'http://hl7.org/fhir/guide-page-generation', 'http://hl7.org/fhir/guide-page-generation');
  CODES_TFhirHTTPVerbEnum : Array[TFhirHTTPVerbEnum] of String = ('', 'GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'PATCH');
  SYSTEMS_TFhirHTTPVerbEnum : Array[TFhirHTTPVerbEnum] of String = ('', 'http://hl7.org/fhir/http-verb', 'http://hl7.org/fhir/http-verb', 'http://hl7.org/fhir/http-verb', 'http://hl7.org/fhir/http-verb', 'http://hl7.org/fhir/http-verb', 'http://hl7.org/fhir/http-verb');
  CODES_TFhirIdentifierUseEnum : Array[TFhirIdentifierUseEnum] of String = ('', 'usual', 'official', 'temp', 'secondary', 'old');
  SYSTEMS_TFhirIdentifierUseEnum : Array[TFhirIdentifierUseEnum] of String = ('', 'http://hl7.org/fhir/identifier-use', 'http://hl7.org/fhir/identifier-use', 'http://hl7.org/fhir/identifier-use', 'http://hl7.org/fhir/identifier-use', 'http://hl7.org/fhir/identifier-use');
  CODES_TFhirIdentityAssuranceLevelEnum : Array[TFhirIdentityAssuranceLevelEnum] of String = ('', 'level1', 'level2', 'level3', 'level4');
  SYSTEMS_TFhirIdentityAssuranceLevelEnum : Array[TFhirIdentityAssuranceLevelEnum] of String = ('', 'http://hl7.org/fhir/identity-assuranceLevel', 'http://hl7.org/fhir/identity-assuranceLevel', 'http://hl7.org/fhir/identity-assuranceLevel', 'http://hl7.org/fhir/identity-assuranceLevel');
  CODES_TFhirImagingStudyStatusEnum : Array[TFhirImagingStudyStatusEnum] of String = ('', 'registered', 'available', 'cancelled', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirImagingStudyStatusEnum : Array[TFhirImagingStudyStatusEnum] of String = ('', 'http://hl7.org/fhir/imagingstudy-status', 'http://hl7.org/fhir/imagingstudy-status', 'http://hl7.org/fhir/imagingstudy-status', 'http://hl7.org/fhir/imagingstudy-status', 'http://hl7.org/fhir/imagingstudy-status');
  CODES_TFhirImmunizationEvaluationStatusCodesEnum : Array[TFhirImmunizationEvaluationStatusCodesEnum] of String = ('', 'completed', 'entered-in-error');
  SYSTEMS_TFhirImmunizationEvaluationStatusCodesEnum : Array[TFhirImmunizationEvaluationStatusCodesEnum] of String = ('', 'http://hl7.org/fhir/CodeSystem/medication-admin-status', 'http://hl7.org/fhir/CodeSystem/medication-admin-status');
  CODES_TFhirImmunizationStatusCodesEnum : Array[TFhirImmunizationStatusCodesEnum] of String = ('', 'completed', 'entered-in-error', 'not-done');
  SYSTEMS_TFhirImmunizationStatusCodesEnum : Array[TFhirImmunizationStatusCodesEnum] of String = ('', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status', 'http://hl7.org/fhir/event-status');
  CODES_TFhirInteractionTriggerEnum : Array[TFhirInteractionTriggerEnum] of String = ('', 'create', 'update', 'delete');
  SYSTEMS_TFhirInteractionTriggerEnum : Array[TFhirInteractionTriggerEnum] of String = ('', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction');
  CODES_TFhirInvoicePriceComponentTypeEnum : Array[TFhirInvoicePriceComponentTypeEnum] of String = ('', 'base', 'surcharge', 'deduction', 'discount', 'tax', 'informational');
  SYSTEMS_TFhirInvoicePriceComponentTypeEnum : Array[TFhirInvoicePriceComponentTypeEnum] of String = ('', 'http://hl7.org/fhir/invoice-priceComponentType', 'http://hl7.org/fhir/invoice-priceComponentType', 'http://hl7.org/fhir/invoice-priceComponentType', 'http://hl7.org/fhir/invoice-priceComponentType', 'http://hl7.org/fhir/invoice-priceComponentType', 'http://hl7.org/fhir/invoice-priceComponentType');
  CODES_TFhirInvoiceStatusEnum : Array[TFhirInvoiceStatusEnum] of String = ('', 'draft', 'issued', 'balanced', 'cancelled', 'entered-in-error');
  SYSTEMS_TFhirInvoiceStatusEnum : Array[TFhirInvoiceStatusEnum] of String = ('', 'http://hl7.org/fhir/invoice-status', 'http://hl7.org/fhir/invoice-status', 'http://hl7.org/fhir/invoice-status', 'http://hl7.org/fhir/invoice-status', 'http://hl7.org/fhir/invoice-status');
  CODES_TFhirIssueSeverityEnum : Array[TFhirIssueSeverityEnum] of String = ('', 'fatal', 'error', 'warning', 'information');
  SYSTEMS_TFhirIssueSeverityEnum : Array[TFhirIssueSeverityEnum] of String = ('', 'http://hl7.org/fhir/issue-severity', 'http://hl7.org/fhir/issue-severity', 'http://hl7.org/fhir/issue-severity', 'http://hl7.org/fhir/issue-severity');
  CODES_TFhirIssueTypeEnum : Array[TFhirIssueTypeEnum] of String = ('', 'invalid', 'structure', 'required', 'value', 'invariant', 'security', 'login', 'unknown', 'expired', 'forbidden', 'suppressed', 'processing', 'not-supported', 'duplicate', 'multiple-matches', 'not-found', 'deleted', 'too-long', 'code-invalid', 'extension', 'too-costly', 'business-rule', 'conflict', 'transient', 'lock-error', 'no-store', 'exception', 'timeout', 'incomplete', 'throttled', 'informational');
  SYSTEMS_TFhirIssueTypeEnum : Array[TFhirIssueTypeEnum] of String = ('', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type',
       'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type');
  CODES_TFhirLinkTypeEnum : Array[TFhirLinkTypeEnum] of String = ('', 'replaced-by', 'replaces', 'refer', 'seealso');
  SYSTEMS_TFhirLinkTypeEnum : Array[TFhirLinkTypeEnum] of String = ('', 'http://hl7.org/fhir/link-type', 'http://hl7.org/fhir/link-type', 'http://hl7.org/fhir/link-type', 'http://hl7.org/fhir/link-type');
  CODES_TFhirLinkageTypeEnum : Array[TFhirLinkageTypeEnum] of String = ('', 'source', 'alternate', 'historical');
  SYSTEMS_TFhirLinkageTypeEnum : Array[TFhirLinkageTypeEnum] of String = ('', 'http://hl7.org/fhir/linkage-type', 'http://hl7.org/fhir/linkage-type', 'http://hl7.org/fhir/linkage-type');
  CODES_TFhirListModeEnum : Array[TFhirListModeEnum] of String = ('', 'working', 'snapshot', 'changes');
  SYSTEMS_TFhirListModeEnum : Array[TFhirListModeEnum] of String = ('', 'http://hl7.org/fhir/list-mode', 'http://hl7.org/fhir/list-mode', 'http://hl7.org/fhir/list-mode');
  CODES_TFhirListStatusEnum : Array[TFhirListStatusEnum] of String = ('', 'current', 'retired', 'entered-in-error');
  SYSTEMS_TFhirListStatusEnum : Array[TFhirListStatusEnum] of String = ('', 'http://hl7.org/fhir/list-status', 'http://hl7.org/fhir/list-status', 'http://hl7.org/fhir/list-status');
  CODES_TFhirLocationModeEnum : Array[TFhirLocationModeEnum] of String = ('', 'instance', 'kind');
  SYSTEMS_TFhirLocationModeEnum : Array[TFhirLocationModeEnum] of String = ('', 'http://hl7.org/fhir/location-mode', 'http://hl7.org/fhir/location-mode');
  CODES_TFhirLocationStatusEnum : Array[TFhirLocationStatusEnum] of String = ('', 'active', 'suspended', 'inactive');
  SYSTEMS_TFhirLocationStatusEnum : Array[TFhirLocationStatusEnum] of String = ('', 'http://hl7.org/fhir/location-status', 'http://hl7.org/fhir/location-status', 'http://hl7.org/fhir/location-status');
  CODES_TFhirMeasureReportStatusEnum : Array[TFhirMeasureReportStatusEnum] of String = ('', 'complete', 'pending', 'error');
  SYSTEMS_TFhirMeasureReportStatusEnum : Array[TFhirMeasureReportStatusEnum] of String = ('', 'http://hl7.org/fhir/measure-report-status', 'http://hl7.org/fhir/measure-report-status', 'http://hl7.org/fhir/measure-report-status');
  CODES_TFhirMeasureReportTypeEnum : Array[TFhirMeasureReportTypeEnum] of String = ('', 'individual', 'subject-list', 'summary', 'data-collection');
  SYSTEMS_TFhirMeasureReportTypeEnum : Array[TFhirMeasureReportTypeEnum] of String = ('', 'http://hl7.org/fhir/measure-report-type', 'http://hl7.org/fhir/measure-report-type', 'http://hl7.org/fhir/measure-report-type', 'http://hl7.org/fhir/measure-report-type');
  CODES_TFhirMedicationAdministrationStatusCodesEnum : Array[TFhirMedicationAdministrationStatusCodesEnum] of String = ('', 'in-progress', 'not-done', 'on-hold', 'completed', 'entered-in-error', 'stopped', 'unknown');
  SYSTEMS_TFhirMedicationAdministrationStatusCodesEnum : Array[TFhirMedicationAdministrationStatusCodesEnum] of String = ('', 'http://hl7.org/fhir/CodeSystem/medication-admin-status', 'http://hl7.org/fhir/CodeSystem/medication-admin-status', 'http://hl7.org/fhir/CodeSystem/medication-admin-status', 'http://hl7.org/fhir/CodeSystem/medication-admin-status', 'http://hl7.org/fhir/CodeSystem/medication-admin-status', 'http://hl7.org/fhir/CodeSystem/medication-admin-status', 'http://hl7.org/fhir/CodeSystem/medication-admin-status');
  CODES_TFhirMedicationDispenseStatusCodesEnum : Array[TFhirMedicationDispenseStatusCodesEnum] of String = ('', 'preparation', 'in-progress', 'cancelled', 'on-hold', 'completed', 'entered-in-error', 'stopped', 'declined', 'unknown');
  SYSTEMS_TFhirMedicationDispenseStatusCodesEnum : Array[TFhirMedicationDispenseStatusCodesEnum] of String = ('', 'http://hl7.org/fhir/CodeSystem/medicationdispense-status', 'http://hl7.org/fhir/CodeSystem/medicationdispense-status', 'http://hl7.org/fhir/CodeSystem/medicationdispense-status', 'http://hl7.org/fhir/CodeSystem/medicationdispense-status', 'http://hl7.org/fhir/CodeSystem/medicationdispense-status', 'http://hl7.org/fhir/CodeSystem/medicationdispense-status', 'http://hl7.org/fhir/CodeSystem/medicationdispense-status', 'http://hl7.org/fhir/CodeSystem/medicationdispense-status', 'http://hl7.org/fhir/CodeSystem/medicationdispense-status');
  CODES_TFhirMedicationKnowledgeStatusCodesEnum : Array[TFhirMedicationKnowledgeStatusCodesEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirMedicationKnowledgeStatusCodesEnum : Array[TFhirMedicationKnowledgeStatusCodesEnum] of String = ('', 'http://terminology.hl7.org/CodeSystem/medicationknowledge-status', 'http://terminology.hl7.org/CodeSystem/medicationknowledge-status', 'http://terminology.hl7.org/CodeSystem/medicationknowledge-status');
  CODES_TFhirMedicationRequestIntentEnum : Array[TFhirMedicationRequestIntentEnum] of String = ('', 'proposal', 'plan', 'order', 'original-order', 'reflex-order', 'filler-order', 'instance-order', 'option');
  SYSTEMS_TFhirMedicationRequestIntentEnum : Array[TFhirMedicationRequestIntentEnum] of String = ('', 'http://hl7.org/fhir/CodeSystem/medicationrequest-intent', 'http://hl7.org/fhir/CodeSystem/medicationrequest-intent', 'http://hl7.org/fhir/CodeSystem/medicationrequest-intent', 'http://hl7.org/fhir/CodeSystem/medicationrequest-intent', 'http://hl7.org/fhir/CodeSystem/medicationrequest-intent', 'http://hl7.org/fhir/CodeSystem/medicationrequest-intent', 'http://hl7.org/fhir/CodeSystem/medicationrequest-intent', 'http://hl7.org/fhir/CodeSystem/medicationrequest-intent');
  CODES_TFhirMedicationStatusCodesEnum : Array[TFhirMedicationStatusCodesEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirMedicationStatusCodesEnum : Array[TFhirMedicationStatusCodesEnum] of String = ('', 'http://hl7.org/fhir/CodeSystem/medication-status', 'http://hl7.org/fhir/CodeSystem/medication-status', 'http://hl7.org/fhir/CodeSystem/medication-status');
  CODES_TFhirMedicationUsageStatusCodesEnum : Array[TFhirMedicationUsageStatusCodesEnum] of String = ('', 'active', 'completed', 'entered-in-error', 'intended', 'stopped', 'on-hold', 'unknown', 'not-taken');
  SYSTEMS_TFhirMedicationUsageStatusCodesEnum : Array[TFhirMedicationUsageStatusCodesEnum] of String = ('', 'http://hl7.org/fhir/CodeSystem/medication-usage-status', 'http://hl7.org/fhir/CodeSystem/medication-usage-status', 'http://hl7.org/fhir/CodeSystem/medication-usage-status', 'http://hl7.org/fhir/CodeSystem/medication-usage-status', 'http://hl7.org/fhir/CodeSystem/medication-usage-status', 'http://hl7.org/fhir/CodeSystem/medication-usage-status', 'http://hl7.org/fhir/CodeSystem/medication-usage-status', 'http://hl7.org/fhir/CodeSystem/medication-usage-status');
  CODES_TFhirMedicationrequestStatusEnum : Array[TFhirMedicationrequestStatusEnum] of String = ('', 'active', 'on-hold', 'cancelled', 'completed', 'entered-in-error', 'stopped', 'draft', 'unknown');
  SYSTEMS_TFhirMedicationrequestStatusEnum : Array[TFhirMedicationrequestStatusEnum] of String = ('', 'http://hl7.org/fhir/CodeSystem/medicationrequest-status', 'http://hl7.org/fhir/CodeSystem/medicationrequest-status', 'http://hl7.org/fhir/CodeSystem/medicationrequest-status', 'http://hl7.org/fhir/CodeSystem/medicationrequest-status', 'http://hl7.org/fhir/CodeSystem/medicationrequest-status', 'http://hl7.org/fhir/CodeSystem/medicationrequest-status', 'http://hl7.org/fhir/CodeSystem/medicationrequest-status', 'http://hl7.org/fhir/CodeSystem/medicationrequest-status');
  CODES_TFhirMessageSignificanceCategoryEnum : Array[TFhirMessageSignificanceCategoryEnum] of String = ('', 'consequence', 'currency', 'notification');
  SYSTEMS_TFhirMessageSignificanceCategoryEnum : Array[TFhirMessageSignificanceCategoryEnum] of String = ('', 'http://hl7.org/fhir/message-significance-category', 'http://hl7.org/fhir/message-significance-category', 'http://hl7.org/fhir/message-significance-category');
  CODES_TFhirMessageheaderResponseRequestEnum : Array[TFhirMessageheaderResponseRequestEnum] of String = ('', 'always', 'on-error', 'never', 'on-success');
  SYSTEMS_TFhirMessageheaderResponseRequestEnum : Array[TFhirMessageheaderResponseRequestEnum] of String = ('', 'http://hl7.org/fhir/messageheader-response-request', 'http://hl7.org/fhir/messageheader-response-request', 'http://hl7.org/fhir/messageheader-response-request', 'http://hl7.org/fhir/messageheader-response-request');
  CODES_TFhirNameUseEnum : Array[TFhirNameUseEnum] of String = ('', 'usual', 'official', 'temp', 'nickname', 'anonymous', 'old', 'maiden');
  SYSTEMS_TFhirNameUseEnum : Array[TFhirNameUseEnum] of String = ('', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use');
  CODES_TFhirNamingSystemIdentifierTypeEnum : Array[TFhirNamingSystemIdentifierTypeEnum] of String = ('', 'oid', 'uuid', 'uri', 'other');
  SYSTEMS_TFhirNamingSystemIdentifierTypeEnum : Array[TFhirNamingSystemIdentifierTypeEnum] of String = ('', 'http://hl7.org/fhir/namingsystem-identifier-type', 'http://hl7.org/fhir/namingsystem-identifier-type', 'http://hl7.org/fhir/namingsystem-identifier-type', 'http://hl7.org/fhir/namingsystem-identifier-type');
  CODES_TFhirNamingSystemTypeEnum : Array[TFhirNamingSystemTypeEnum] of String = ('', 'codesystem', 'identifier', 'root');
  SYSTEMS_TFhirNamingSystemTypeEnum : Array[TFhirNamingSystemTypeEnum] of String = ('', 'http://hl7.org/fhir/namingsystem-type', 'http://hl7.org/fhir/namingsystem-type', 'http://hl7.org/fhir/namingsystem-type');
  CODES_TFhirNarrativeStatusEnum : Array[TFhirNarrativeStatusEnum] of String = ('', 'generated', 'extensions', 'additional', 'empty');
  SYSTEMS_TFhirNarrativeStatusEnum : Array[TFhirNarrativeStatusEnum] of String = ('', 'http://hl7.org/fhir/narrative-status', 'http://hl7.org/fhir/narrative-status', 'http://hl7.org/fhir/narrative-status', 'http://hl7.org/fhir/narrative-status');
  CODES_TFhirNoteTypeEnum : Array[TFhirNoteTypeEnum] of String = ('', 'display', 'print', 'printoper');
  SYSTEMS_TFhirNoteTypeEnum : Array[TFhirNoteTypeEnum] of String = ('', 'http://hl7.org/fhir/note-type', 'http://hl7.org/fhir/note-type', 'http://hl7.org/fhir/note-type');
  CODES_TFhirNutritionProductStatusEnum : Array[TFhirNutritionProductStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirNutritionProductStatusEnum : Array[TFhirNutritionProductStatusEnum] of String = ('', 'http://hl7.org/fhir/nutritionproduct-status', 'http://hl7.org/fhir/nutritionproduct-status', 'http://hl7.org/fhir/nutritionproduct-status');
  CODES_TFhirObservationDataTypeEnum : Array[TFhirObservationDataTypeEnum] of String = ('', 'Quantity', 'CodeableConcept', 'string', 'boolean', 'integer', 'Range', 'Ratio', 'SampledData', 'time', 'dateTime', 'Period');
  SYSTEMS_TFhirObservationDataTypeEnum : Array[TFhirObservationDataTypeEnum] of String = ('', 'http://hl7.org/fhir/permitted-data-type', 'http://hl7.org/fhir/permitted-data-type', 'http://hl7.org/fhir/permitted-data-type', 'http://hl7.org/fhir/permitted-data-type', 'http://hl7.org/fhir/permitted-data-type', 'http://hl7.org/fhir/permitted-data-type', 'http://hl7.org/fhir/permitted-data-type', 'http://hl7.org/fhir/permitted-data-type', 'http://hl7.org/fhir/permitted-data-type', 'http://hl7.org/fhir/permitted-data-type', 'http://hl7.org/fhir/permitted-data-type');
  CODES_TFhirObservationRangeCategoryEnum : Array[TFhirObservationRangeCategoryEnum] of String = ('', 'reference', 'critical', 'absolute');
  SYSTEMS_TFhirObservationRangeCategoryEnum : Array[TFhirObservationRangeCategoryEnum] of String = ('', 'http://hl7.org/fhir/observation-range-category', 'http://hl7.org/fhir/observation-range-category', 'http://hl7.org/fhir/observation-range-category');
  CODES_TFhirObservationStatusEnum : Array[TFhirObservationStatusEnum] of String = ('', 'registered', 'preliminary', 'final', 'amended', 'corrected', 'cancelled', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirObservationStatusEnum : Array[TFhirObservationStatusEnum] of String = ('', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status');
  CODES_TFhirOperationKindEnum : Array[TFhirOperationKindEnum] of String = ('', 'operation', 'query');
  SYSTEMS_TFhirOperationKindEnum : Array[TFhirOperationKindEnum] of String = ('', 'http://hl7.org/fhir/operation-kind', 'http://hl7.org/fhir/operation-kind');
  CODES_TFhirOperationParameterUseEnum : Array[TFhirOperationParameterUseEnum] of String = ('', 'in', 'out');
  SYSTEMS_TFhirOperationParameterUseEnum : Array[TFhirOperationParameterUseEnum] of String = ('', 'http://hl7.org/fhir/operation-parameter-use', 'http://hl7.org/fhir/operation-parameter-use');
  CODES_TFhirOrientationTypeEnum : Array[TFhirOrientationTypeEnum] of String = ('', 'sense', 'antisense');
  SYSTEMS_TFhirOrientationTypeEnum : Array[TFhirOrientationTypeEnum] of String = ('', 'http://hl7.org/fhir/orientation-type', 'http://hl7.org/fhir/orientation-type');
  CODES_TFhirParticipantRequiredEnum : Array[TFhirParticipantRequiredEnum] of String = ('', 'required', 'optional', 'information-only');
  SYSTEMS_TFhirParticipantRequiredEnum : Array[TFhirParticipantRequiredEnum] of String = ('', 'http://hl7.org/fhir/participantrequired', 'http://hl7.org/fhir/participantrequired', 'http://hl7.org/fhir/participantrequired');
  CODES_TFhirParticipationStatusEnum : Array[TFhirParticipationStatusEnum] of String = ('', 'accepted', 'declined', 'tentative', 'needs-action');
  SYSTEMS_TFhirParticipationStatusEnum : Array[TFhirParticipationStatusEnum] of String = ('', 'http://hl7.org/fhir/participationstatus', 'http://hl7.org/fhir/participationstatus', 'http://hl7.org/fhir/participationstatus', 'http://hl7.org/fhir/participationstatus');
  CODES_TFhirPermissionStatusEnum : Array[TFhirPermissionStatusEnum] of String = ('', 'active', 'entered-in-error', 'draft', 'rejected');
  SYSTEMS_TFhirPermissionStatusEnum : Array[TFhirPermissionStatusEnum] of String = ('', 'http://hl7.org/fhir/permission-status', 'http://hl7.org/fhir/permission-status', 'http://hl7.org/fhir/permission-status', 'http://hl7.org/fhir/permission-status');
  CODES_TFhirPropertyRepresentationEnum : Array[TFhirPropertyRepresentationEnum] of String = ('', 'xmlAttr', 'xmlText', 'typeAttr', 'cdaText', 'xhtml');
  SYSTEMS_TFhirPropertyRepresentationEnum : Array[TFhirPropertyRepresentationEnum] of String = ('', 'http://hl7.org/fhir/property-representation', 'http://hl7.org/fhir/property-representation', 'http://hl7.org/fhir/property-representation', 'http://hl7.org/fhir/property-representation', 'http://hl7.org/fhir/property-representation');
  CODES_TFhirProvenanceEntityRoleEnum : Array[TFhirProvenanceEntityRoleEnum] of String = ('', 'derivation', 'revision', 'quotation', 'source', 'removal');
  SYSTEMS_TFhirProvenanceEntityRoleEnum : Array[TFhirProvenanceEntityRoleEnum] of String = ('', 'http://hl7.org/fhir/provenance-entity-role', 'http://hl7.org/fhir/provenance-entity-role', 'http://hl7.org/fhir/provenance-entity-role', 'http://hl7.org/fhir/provenance-entity-role', 'http://hl7.org/fhir/provenance-entity-role');
  CODES_TFhirPublicationStatusEnum : Array[TFhirPublicationStatusEnum] of String = ('', 'draft', 'active', 'retired', 'unknown');
  SYSTEMS_TFhirPublicationStatusEnum : Array[TFhirPublicationStatusEnum] of String = ('', 'http://hl7.org/fhir/publication-status', 'http://hl7.org/fhir/publication-status', 'http://hl7.org/fhir/publication-status', 'http://hl7.org/fhir/publication-status');
  CODES_TFhirQualityTypeEnum : Array[TFhirQualityTypeEnum] of String = ('', 'indel', 'snp', 'unknown');
  SYSTEMS_TFhirQualityTypeEnum : Array[TFhirQualityTypeEnum] of String = ('', 'http://hl7.org/fhir/quality-type', 'http://hl7.org/fhir/quality-type', 'http://hl7.org/fhir/quality-type');
  CODES_TFhirQuantityComparatorEnum : Array[TFhirQuantityComparatorEnum] of String = ('', '<', '<=', '>=', '>');
  SYSTEMS_TFhirQuantityComparatorEnum : Array[TFhirQuantityComparatorEnum] of String = ('', 'http://hl7.org/fhir/quantity-comparator', 'http://hl7.org/fhir/quantity-comparator', 'http://hl7.org/fhir/quantity-comparator', 'http://hl7.org/fhir/quantity-comparator');
  CODES_TFhirQuestionnaireItemOperatorEnum : Array[TFhirQuestionnaireItemOperatorEnum] of String = ('', 'exists', '=', '!=', '>', '<', '>=', '<=');
  SYSTEMS_TFhirQuestionnaireItemOperatorEnum : Array[TFhirQuestionnaireItemOperatorEnum] of String = ('', 'http://hl7.org/fhir/questionnaire-enable-operator', 'http://hl7.org/fhir/questionnaire-enable-operator', 'http://hl7.org/fhir/questionnaire-enable-operator', 'http://hl7.org/fhir/questionnaire-enable-operator', 'http://hl7.org/fhir/questionnaire-enable-operator', 'http://hl7.org/fhir/questionnaire-enable-operator', 'http://hl7.org/fhir/questionnaire-enable-operator');
  CODES_TFhirQuestionnaireItemTypeEnum : Array[TFhirQuestionnaireItemTypeEnum] of String = ('', 'group', 'display', 'question', 'boolean', 'decimal', 'integer', 'date', 'dateTime', 'time', 'string', 'text', 'url', 'choice', 'open-choice', 'attachment', 'reference', 'quantity');
  SYSTEMS_TFhirQuestionnaireItemTypeEnum : Array[TFhirQuestionnaireItemTypeEnum] of String = ('', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type', 'http://hl7.org/fhir/item-type');
  CODES_TFhirQuestionnaireResponseStatusEnum : Array[TFhirQuestionnaireResponseStatusEnum] of String = ('', 'in-progress', 'completed', 'amended', 'entered-in-error', 'stopped');
  SYSTEMS_TFhirQuestionnaireResponseStatusEnum : Array[TFhirQuestionnaireResponseStatusEnum] of String = ('', 'http://hl7.org/fhir/questionnaire-answers-status', 'http://hl7.org/fhir/questionnaire-answers-status', 'http://hl7.org/fhir/questionnaire-answers-status', 'http://hl7.org/fhir/questionnaire-answers-status', 'http://hl7.org/fhir/questionnaire-answers-status');
  CODES_TFhirReferenceHandlingPolicyEnum : Array[TFhirReferenceHandlingPolicyEnum] of String = ('', 'literal', 'logical', 'resolves', 'enforced', 'local');
  SYSTEMS_TFhirReferenceHandlingPolicyEnum : Array[TFhirReferenceHandlingPolicyEnum] of String = ('', 'http://hl7.org/fhir/reference-handling-policy', 'http://hl7.org/fhir/reference-handling-policy', 'http://hl7.org/fhir/reference-handling-policy', 'http://hl7.org/fhir/reference-handling-policy', 'http://hl7.org/fhir/reference-handling-policy');
  CODES_TFhirReferenceVersionRulesEnum : Array[TFhirReferenceVersionRulesEnum] of String = ('', 'either', 'independent', 'specific');
  SYSTEMS_TFhirReferenceVersionRulesEnum : Array[TFhirReferenceVersionRulesEnum] of String = ('', 'http://hl7.org/fhir/reference-version-rules', 'http://hl7.org/fhir/reference-version-rules', 'http://hl7.org/fhir/reference-version-rules');
  CODES_TFhirRelatedArtifactTypeEnum : Array[TFhirRelatedArtifactTypeEnum] of String = ('', 'documentation', 'justification', 'citation', 'predecessor', 'successor', 'derived-from', 'depends-on', 'composed-of');
  SYSTEMS_TFhirRelatedArtifactTypeEnum : Array[TFhirRelatedArtifactTypeEnum] of String = ('', 'http://hl7.org/fhir/related-artifact-type', 'http://hl7.org/fhir/related-artifact-type', 'http://hl7.org/fhir/related-artifact-type', 'http://hl7.org/fhir/related-artifact-type', 'http://hl7.org/fhir/related-artifact-type', 'http://hl7.org/fhir/related-artifact-type', 'http://hl7.org/fhir/related-artifact-type', 'http://hl7.org/fhir/related-artifact-type');
  CODES_TFhirReportRelationshipTypeEnum : Array[TFhirReportRelationshipTypeEnum] of String = ('', 'replaces', 'amends', 'appends', 'transforms');
  SYSTEMS_TFhirReportRelationshipTypeEnum : Array[TFhirReportRelationshipTypeEnum] of String = ('', 'http://hl7.org/fhir/report-relation-type', 'http://hl7.org/fhir/report-relation-type', 'http://hl7.org/fhir/report-relation-type', 'http://hl7.org/fhir/report-relation-type');
  CODES_TFhirRepositoryTypeEnum : Array[TFhirRepositoryTypeEnum] of String = ('', 'directlink', 'openapi', 'login', 'oauth', 'other');
  SYSTEMS_TFhirRepositoryTypeEnum : Array[TFhirRepositoryTypeEnum] of String = ('', 'http://hl7.org/fhir/repository-type', 'http://hl7.org/fhir/repository-type', 'http://hl7.org/fhir/repository-type', 'http://hl7.org/fhir/repository-type', 'http://hl7.org/fhir/repository-type');
  CODES_TFhirRequestIntentEnum : Array[TFhirRequestIntentEnum] of String = ('', 'proposal', 'plan', 'directive', 'order', 'original-order', 'reflex-order', 'filler-order', 'instance-order', 'option');
  SYSTEMS_TFhirRequestIntentEnum : Array[TFhirRequestIntentEnum] of String = ('', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent');
  CODES_TFhirRequestPriorityEnum : Array[TFhirRequestPriorityEnum] of String = ('', 'routine', 'urgent', 'asap', 'stat');
  SYSTEMS_TFhirRequestPriorityEnum : Array[TFhirRequestPriorityEnum] of String = ('', 'http://hl7.org/fhir/request-priority', 'http://hl7.org/fhir/request-priority', 'http://hl7.org/fhir/request-priority', 'http://hl7.org/fhir/request-priority');
  CODES_TFhirRequestResourceTypeEnum : Array[TFhirRequestResourceTypeEnum] of String = ('', 'Appointment', 'AppointmentResponse', 'CarePlan', 'Claim', 'CommunicationRequest', 'Contract', 'DeviceRequest', 'EnrollmentRequest', 'ImmunizationRecommendation', 'MedicationRequest', 'NutritionOrder', 'ServiceRequest', 'SupplyRequest', 'Task', 'VisionPrescription');
  SYSTEMS_TFhirRequestResourceTypeEnum : Array[TFhirRequestResourceTypeEnum] of String = ('', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types', 'http://hl7.org/fhir/request-resource-types');
  CODES_TFhirRequestStatusEnum : Array[TFhirRequestStatusEnum] of String = ('', 'draft', 'active', 'on-hold', 'revoked', 'completed', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirRequestStatusEnum : Array[TFhirRequestStatusEnum] of String = ('', 'http://hl7.org/fhir/request-status', 'http://hl7.org/fhir/request-status', 'http://hl7.org/fhir/request-status', 'http://hl7.org/fhir/request-status', 'http://hl7.org/fhir/request-status', 'http://hl7.org/fhir/request-status', 'http://hl7.org/fhir/request-status');
  CODES_TFhirResearchStudyStatusEnum : Array[TFhirResearchStudyStatusEnum] of String = ('', 'active', 'administratively-completed', 'approved', 'closed-to-accrual', 'closed-to-accrual-and-intervention', 'completed', 'disapproved', 'in-review', 'temporarily-closed-to-accrual', 'temporarily-closed-to-accrual-and-intervention', 'withdrawn');
  SYSTEMS_TFhirResearchStudyStatusEnum : Array[TFhirResearchStudyStatusEnum] of String = ('', 'http://hl7.org/fhir/research-study-status', 'http://hl7.org/fhir/research-study-status', 'http://hl7.org/fhir/research-study-status', 'http://hl7.org/fhir/research-study-status', 'http://hl7.org/fhir/research-study-status', 'http://hl7.org/fhir/research-study-status', 'http://hl7.org/fhir/research-study-status', 'http://hl7.org/fhir/research-study-status', 'http://hl7.org/fhir/research-study-status', 'http://hl7.org/fhir/research-study-status', 'http://hl7.org/fhir/research-study-status');
  CODES_TFhirResearchSubjectStatusEnum : Array[TFhirResearchSubjectStatusEnum] of String = ('', 'candidate', 'eligible', 'follow-up', 'ineligible', 'not-registered', 'off-study', 'on-study', 'on-study-intervention', 'on-study-observation', 'pending-on-study', 'potential-candidate', 'screening', 'withdrawn');
  SYSTEMS_TFhirResearchSubjectStatusEnum : Array[TFhirResearchSubjectStatusEnum] of String = ('', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status', 'http://hl7.org/fhir/research-subject-status');
  CODES_TFhirResourceTypesEnum : Array[TFhirResourceTypesEnum] of String = ('', 'Account', 'ActivityDefinition', 'AdministrableProductDefinition', 'AdverseEvent', 'AllergyIntolerance', 'Appointment', 'AppointmentResponse', 'AuditEvent', 'Basic', 'Binary', 'BiologicallyDerivedProduct', 'BodyStructure', 'Bundle', 'CapabilityStatement', 'CapabilityStatement2', 'CarePlan', 'CareTeam', 'CatalogEntry', 'ChargeItem', 'ChargeItemDefinition', 'Citation', 'Claim', 'ClaimResponse', 'ClinicalImpression', 'ClinicalUseIssue', 'CodeSystem', 'Communication', 'CommunicationRequest', 'CompartmentDefinition', 'Composition', 'ConceptMap', 'Condition', 'ConditionDefinition', 'Consent', 'Contract', 'Coverage', 'CoverageEligibilityRequest', 'CoverageEligibilityResponse', 'DetectedIssue', 'Device', 'DeviceDefinition', 'DeviceMetric', 'DeviceRequest', 'DeviceUseStatement', 'DiagnosticReport', 'DocumentManifest', 'DocumentReference', 'DomainResource', 'Encounter', 'Endpoint', 'EnrollmentRequest', 'EnrollmentResponse',
       'EpisodeOfCare', 'EventDefinition', 'Evidence', 'EvidenceReport', 'EvidenceVariable', 'ExampleScenario', 'ExplanationOfBenefit', 'FamilyMemberHistory', 'Flag', 'Goal', 'GraphDefinition', 'Group', 'GuidanceResponse', 'HealthcareService', 'ImagingStudy', 'Immunization', 'ImmunizationEvaluation', 'ImmunizationRecommendation', 'ImplementationGuide', 'Ingredient', 'InsurancePlan', 'Invoice', 'Library', 'Linkage', 'List', 'Location', 'ManufacturedItemDefinition', 'Measure', 'MeasureReport', 'Medication', 'MedicationAdministration', 'MedicationDispense', 'MedicationKnowledge', 'MedicationRequest', 'MedicationUsage', 'MedicinalProductDefinition', 'MessageDefinition', 'MessageHeader', 'MolecularSequence', 'NamingSystem', 'NutritionIntake', 'NutritionOrder', 'NutritionProduct', 'Observation', 'ObservationDefinition', 'OperationDefinition', 'OperationOutcome', 'Organization', 'OrganizationAffiliation', 'PackagedProductDefinition', 'Parameters', 'Patient', 'PaymentNotice', 'PaymentReconciliation',
       'Permission', 'Person', 'PlanDefinition', 'Practitioner', 'PractitionerRole', 'Procedure', 'Provenance', 'Questionnaire', 'QuestionnaireResponse', 'RegulatedAuthorization', 'RelatedPerson', 'RequestGroup', 'ResearchStudy', 'ResearchSubject', 'Resource', 'RiskAssessment', 'Schedule', 'SearchParameter', 'ServiceRequest', 'Slot', 'Specimen', 'SpecimenDefinition', 'StructureDefinition', 'StructureMap', 'Subscription', 'SubscriptionStatus', 'SubscriptionTopic', 'Substance', 'SubstanceDefinition', 'SubstanceNucleicAcid', 'SubstancePolymer', 'SubstanceProtein', 'SubstanceReferenceInformation', 'SubstanceSourceMaterial', 'SupplyDelivery', 'SupplyRequest', 'Task', 'TerminologyCapabilities', 'TestReport', 'TestScript', 'ValueSet', 'VerificationResult', 'VisionPrescription');
  SYSTEMS_TFhirResourceTypesEnum : Array[TFhirResourceTypesEnum] of String = ('', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types',
       'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types');
  CODES_TFhirResourceVersionPolicyEnum : Array[TFhirResourceVersionPolicyEnum] of String = ('', 'no-version', 'versioned', 'versioned-update');
  SYSTEMS_TFhirResourceVersionPolicyEnum : Array[TFhirResourceVersionPolicyEnum] of String = ('', 'http://hl7.org/fhir/versioning-policy', 'http://hl7.org/fhir/versioning-policy', 'http://hl7.org/fhir/versioning-policy');
  CODES_TFhirResponseTypeEnum : Array[TFhirResponseTypeEnum] of String = ('', 'ok', 'transient-error', 'fatal-error');
  SYSTEMS_TFhirResponseTypeEnum : Array[TFhirResponseTypeEnum] of String = ('', 'http://hl7.org/fhir/response-code', 'http://hl7.org/fhir/response-code', 'http://hl7.org/fhir/response-code');
  CODES_TFhirRestfulCapabilityModeEnum : Array[TFhirRestfulCapabilityModeEnum] of String = ('', 'client', 'server');
  SYSTEMS_TFhirRestfulCapabilityModeEnum : Array[TFhirRestfulCapabilityModeEnum] of String = ('', 'http://hl7.org/fhir/restful-capability-mode', 'http://hl7.org/fhir/restful-capability-mode');
  CODES_TFhirSPDXLicenseEnum : Array[TFhirSPDXLicenseEnum] of String = ('', 'not-open-source', '0BSD', 'AAL', 'Abstyles', 'Adobe-2006', 'Adobe-Glyph', 'ADSL', 'AFL-1.1', 'AFL-1.2', 'AFL-2.0', 'AFL-2.1', 'AFL-3.0', 'Afmparse', 'AGPL-1.0-only', 'AGPL-1.0-or-later', 'AGPL-3.0-only', 'AGPL-3.0-or-later', 'Aladdin', 'AMDPLPA', 'AML', 'AMPAS', 'ANTLR-PD', 'Apache-1.0', 'Apache-1.1', 'Apache-2.0', 'APAFML', 'APL-1.0', 'APSL-1.0', 'APSL-1.1', 'APSL-1.2', 'APSL-2.0', 'Artistic-1.0-cl8', 'Artistic-1.0-Perl', 'Artistic-1.0', 'Artistic-2.0', 'Bahyph', 'Barr', 'Beerware', 'BitTorrent-1.0', 'BitTorrent-1.1', 'Borceux', 'BSD-1-Clause', 'BSD-2-Clause-FreeBSD', 'BSD-2-Clause-NetBSD', 'BSD-2-Clause-Patent', 'BSD-2-Clause', 'BSD-3-Clause-Attribution', 'BSD-3-Clause-Clear', 'BSD-3-Clause-LBNL', 'BSD-3-Clause-No-Nuclear-License-2014', 'BSD-3-Clause-No-Nuclear-License', 'BSD-3-Clause-No-Nuclear-Warranty', 'BSD-3-Clause', 'BSD-4-Clause-UC', 'BSD-4-Clause', 'BSD-Protection', 'BSD-Source-Code', 'BSL-1.0', 'bzip2-1.0.5',
       'bzip2-1.0.6', 'Caldera', 'CATOSL-1.1', 'CC-BY-1.0', 'CC-BY-2.0', 'CC-BY-2.5', 'CC-BY-3.0', 'CC-BY-4.0', 'CC-BY-NC-1.0', 'CC-BY-NC-2.0', 'CC-BY-NC-2.5', 'CC-BY-NC-3.0', 'CC-BY-NC-4.0', 'CC-BY-NC-ND-1.0', 'CC-BY-NC-ND-2.0', 'CC-BY-NC-ND-2.5', 'CC-BY-NC-ND-3.0', 'CC-BY-NC-ND-4.0', 'CC-BY-NC-SA-1.0', 'CC-BY-NC-SA-2.0', 'CC-BY-NC-SA-2.5', 'CC-BY-NC-SA-3.0', 'CC-BY-NC-SA-4.0', 'CC-BY-ND-1.0', 'CC-BY-ND-2.0', 'CC-BY-ND-2.5', 'CC-BY-ND-3.0', 'CC-BY-ND-4.0', 'CC-BY-SA-1.0', 'CC-BY-SA-2.0', 'CC-BY-SA-2.5', 'CC-BY-SA-3.0', 'CC-BY-SA-4.0', 'CC0-1.0', 'CDDL-1.0', 'CDDL-1.1', 'CDLA-Permissive-1.0', 'CDLA-Sharing-1.0', 'CECILL-1.0', 'CECILL-1.1', 'CECILL-2.0', 'CECILL-2.1', 'CECILL-B', 'CECILL-C', 'ClArtistic', 'CNRI-Jython', 'CNRI-Python-GPL-Compatible', 'CNRI-Python', 'Condor-1.1', 'CPAL-1.0', 'CPL-1.0', 'CPOL-1.02', 'Crossword', 'CrystalStacker', 'CUA-OPL-1.0', 'Cube', 'curl', 'D-FSL-1.0', 'diffmark', 'DOC', 'Dotseqn', 'DSDP', 'dvipdfm', 'ECL-1.0', 'ECL-2.0', 'EFL-1.0', 'EFL-2.0', 'eGenix', 'Entessa',
       'EPL-1.0', 'EPL-2.0', 'ErlPL-1.1', 'EUDatagrid', 'EUPL-1.0', 'EUPL-1.1', 'EUPL-1.2', 'Eurosym', 'Fair', 'Frameworx-1.0', 'FreeImage', 'FSFAP', 'FSFUL', 'FSFULLR', 'FTL', 'GFDL-1.1-only', 'GFDL-1.1-or-later', 'GFDL-1.2-only', 'GFDL-1.2-or-later', 'GFDL-1.3-only', 'GFDL-1.3-or-later', 'Giftware', 'GL2PS', 'Glide', 'Glulxe', 'gnuplot', 'GPL-1.0-only', 'GPL-1.0-or-later', 'GPL-2.0-only', 'GPL-2.0-or-later', 'GPL-3.0-only', 'GPL-3.0-or-later', 'gSOAP-1.3b', 'HaskellReport', 'HPND', 'IBM-pibs', 'ICU', 'IJG', 'ImageMagick', 'iMatix', 'Imlib2', 'Info-ZIP', 'Intel-ACPI', 'Intel', 'Interbase-1.0', 'IPA', 'IPL-1.0', 'ISC', 'JasPer-2.0', 'JSON', 'LAL-1.2', 'LAL-1.3', 'Latex2e', 'Leptonica', 'LGPL-2.0-only', 'LGPL-2.0-or-later', 'LGPL-2.1-only', 'LGPL-2.1-or-later', 'LGPL-3.0-only', 'LGPL-3.0-or-later', 'LGPLLR', 'Libpng', 'libtiff', 'LiLiQ-P-1.1', 'LiLiQ-R-1.1', 'LiLiQ-Rplus-1.1', 'Linux-OpenIB', 'LPL-1.0', 'LPL-1.02', 'LPPL-1.0', 'LPPL-1.1', 'LPPL-1.2', 'LPPL-1.3a', 'LPPL-1.3c', 'MakeIndex', 'MirOS',
       'MIT-0', 'MIT-advertising', 'MIT-CMU', 'MIT-enna', 'MIT-feh', 'MIT', 'MITNFA', 'Motosoto', 'mpich2', 'MPL-1.0', 'MPL-1.1', 'MPL-2.0-no-copyleft-exception', 'MPL-2.0', 'MS-PL', 'MS-RL', 'MTLL', 'Multics', 'Mup', 'NASA-1.3', 'Naumen', 'NBPL-1.0', 'NCSA', 'Net-SNMP', 'NetCDF', 'Newsletr', 'NGPL', 'NLOD-1.0', 'NLPL', 'Nokia', 'NOSL', 'Noweb', 'NPL-1.0', 'NPL-1.1', 'NPOSL-3.0', 'NRL', 'NTP', 'OCCT-PL', 'OCLC-2.0', 'ODbL-1.0', 'OFL-1.0', 'OFL-1.1', 'OGTSL', 'OLDAP-1.1', 'OLDAP-1.2', 'OLDAP-1.3', 'OLDAP-1.4', 'OLDAP-2.0.1', 'OLDAP-2.0', 'OLDAP-2.1', 'OLDAP-2.2.1', 'OLDAP-2.2.2', 'OLDAP-2.2', 'OLDAP-2.3', 'OLDAP-2.4', 'OLDAP-2.5', 'OLDAP-2.6', 'OLDAP-2.7', 'OLDAP-2.8', 'OML', 'OpenSSL', 'OPL-1.0', 'OSET-PL-2.1', 'OSL-1.0', 'OSL-1.1', 'OSL-2.0', 'OSL-2.1', 'OSL-3.0', 'PDDL-1.0', 'PHP-3.0', 'PHP-3.01', 'Plexus', 'PostgreSQL', 'psfrag', 'psutils', 'Python-2.0', 'Qhull', 'QPL-1.0', 'Rdisc', 'RHeCos-1.1', 'RPL-1.1', 'RPL-1.5', 'RPSL-1.0', 'RSA-MD', 'RSCPL', 'Ruby', 'SAX-PD', 'Saxpath', 'SCEA', 'Sendmail',
       'SGI-B-1.0', 'SGI-B-1.1', 'SGI-B-2.0', 'SimPL-2.0', 'SISSL-1.2', 'SISSL', 'Sleepycat', 'SMLNJ', 'SMPPL', 'SNIA', 'Spencer-86', 'Spencer-94', 'Spencer-99', 'SPL-1.0', 'SugarCRM-1.1.3', 'SWL', 'TCL', 'TCP-wrappers', 'TMate', 'TORQUE-1.1', 'TOSL', 'Unicode-DFS-2015', 'Unicode-DFS-2016', 'Unicode-TOU', 'Unlicense', 'UPL-1.0', 'Vim', 'VOSTROM', 'VSL-1.0', 'W3C-19980720', 'W3C-20150513', 'W3C', 'Watcom-1.0', 'Wsuipa', 'WTFPL', 'X11', 'Xerox', 'XFree86-1.1', 'xinetd', 'Xnet', 'xpp', 'XSkat', 'YPL-1.0', 'YPL-1.1', 'Zed', 'Zend-2.0', 'Zimbra-1.3', 'Zimbra-1.4', 'zlib-acknowledgement', 'Zlib', 'ZPL-1.1', 'ZPL-2.0', 'ZPL-2.1');
  SYSTEMS_TFhirSPDXLicenseEnum : Array[TFhirSPDXLicenseEnum] of String = ('', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license',
       'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license', 'http://hl7.org/fhir/spdx-license');
  CODES_TFhirSearchComparatorEnum : Array[TFhirSearchComparatorEnum] of String = ('', 'eq', 'ne', 'gt', 'lt', 'ge', 'le', 'sa', 'eb', 'ap');
  SYSTEMS_TFhirSearchComparatorEnum : Array[TFhirSearchComparatorEnum] of String = ('', 'http://hl7.org/fhir/search-comparator', 'http://hl7.org/fhir/search-comparator', 'http://hl7.org/fhir/search-comparator', 'http://hl7.org/fhir/search-comparator', 'http://hl7.org/fhir/search-comparator', 'http://hl7.org/fhir/search-comparator', 'http://hl7.org/fhir/search-comparator', 'http://hl7.org/fhir/search-comparator', 'http://hl7.org/fhir/search-comparator');
  CODES_TFhirSearchEntryModeEnum : Array[TFhirSearchEntryModeEnum] of String = ('', 'match', 'include', 'outcome');
  SYSTEMS_TFhirSearchEntryModeEnum : Array[TFhirSearchEntryModeEnum] of String = ('', 'http://hl7.org/fhir/search-entry-mode', 'http://hl7.org/fhir/search-entry-mode', 'http://hl7.org/fhir/search-entry-mode');
  CODES_TFhirSearchModifierCodeEnum : Array[TFhirSearchModifierCodeEnum] of String = ('', 'missing', 'exact', 'contains', 'not', 'text', 'in', 'not-in', 'below', 'above', 'type', 'identifier', 'ofType');
  SYSTEMS_TFhirSearchModifierCodeEnum : Array[TFhirSearchModifierCodeEnum] of String = ('', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code');
  CODES_TFhirSearchParamTypeEnum : Array[TFhirSearchParamTypeEnum] of String = ('', 'number', 'date', 'string', 'token', 'reference', 'composite', 'quantity', 'uri', 'special');
  SYSTEMS_TFhirSearchParamTypeEnum : Array[TFhirSearchParamTypeEnum] of String = ('', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type');
  CODES_TFhirSequenceTypeEnum : Array[TFhirSequenceTypeEnum] of String = ('', 'aa', 'dna', 'rna');
  SYSTEMS_TFhirSequenceTypeEnum : Array[TFhirSequenceTypeEnum] of String = ('', 'http://hl7.org/fhir/sequence-type', 'http://hl7.org/fhir/sequence-type', 'http://hl7.org/fhir/sequence-type');
  CODES_TFhirSlicingRulesEnum : Array[TFhirSlicingRulesEnum] of String = ('', 'closed', 'open', 'openAtEnd');
  SYSTEMS_TFhirSlicingRulesEnum : Array[TFhirSlicingRulesEnum] of String = ('', 'http://hl7.org/fhir/resource-slicing-rules', 'http://hl7.org/fhir/resource-slicing-rules', 'http://hl7.org/fhir/resource-slicing-rules');
  CODES_TFhirSlotStatusEnum : Array[TFhirSlotStatusEnum] of String = ('', 'busy', 'free', 'busy-unavailable', 'busy-tentative', 'entered-in-error');
  SYSTEMS_TFhirSlotStatusEnum : Array[TFhirSlotStatusEnum] of String = ('', 'http://hl7.org/fhir/slotstatus', 'http://hl7.org/fhir/slotstatus', 'http://hl7.org/fhir/slotstatus', 'http://hl7.org/fhir/slotstatus', 'http://hl7.org/fhir/slotstatus');
  CODES_TFhirSortDirectionEnum : Array[TFhirSortDirectionEnum] of String = ('', 'ascending', 'descending');
  SYSTEMS_TFhirSortDirectionEnum : Array[TFhirSortDirectionEnum] of String = ('', 'http://hl7.org/fhir/sort-direction', 'http://hl7.org/fhir/sort-direction');
  CODES_TFhirSpecimenContainedPreferenceEnum : Array[TFhirSpecimenContainedPreferenceEnum] of String = ('', 'preferred', 'alternate');
  SYSTEMS_TFhirSpecimenContainedPreferenceEnum : Array[TFhirSpecimenContainedPreferenceEnum] of String = ('', 'http://hl7.org/fhir/specimen-contained-preference', 'http://hl7.org/fhir/specimen-contained-preference');
  CODES_TFhirSpecimenStatusEnum : Array[TFhirSpecimenStatusEnum] of String = ('', 'available', 'unavailable', 'unsatisfactory', 'entered-in-error');
  SYSTEMS_TFhirSpecimenStatusEnum : Array[TFhirSpecimenStatusEnum] of String = ('', 'http://hl7.org/fhir/specimen-status', 'http://hl7.org/fhir/specimen-status', 'http://hl7.org/fhir/specimen-status', 'http://hl7.org/fhir/specimen-status');
  CODES_TFhirStatusEnum : Array[TFhirStatusEnum] of String = ('', 'attested', 'validated', 'in-process', 'req-revalid', 'val-fail', 'reval-fail');
  SYSTEMS_TFhirStatusEnum : Array[TFhirStatusEnum] of String = ('', 'http://hl7.org/fhir/CodeSystem/status', 'http://hl7.org/fhir/CodeSystem/status', 'http://hl7.org/fhir/CodeSystem/status', 'http://hl7.org/fhir/CodeSystem/status', 'http://hl7.org/fhir/CodeSystem/status', 'http://hl7.org/fhir/CodeSystem/status');
  CODES_TFhirStrandTypeEnum : Array[TFhirStrandTypeEnum] of String = ('', 'watson', 'crick');
  SYSTEMS_TFhirStrandTypeEnum : Array[TFhirStrandTypeEnum] of String = ('', 'http://hl7.org/fhir/strand-type', 'http://hl7.org/fhir/strand-type');
  CODES_TFhirStructureDefinitionKindEnum : Array[TFhirStructureDefinitionKindEnum] of String = ('', 'primitive-type', 'complex-type', 'resource', 'logical');
  SYSTEMS_TFhirStructureDefinitionKindEnum : Array[TFhirStructureDefinitionKindEnum] of String = ('', 'http://hl7.org/fhir/structure-definition-kind', 'http://hl7.org/fhir/structure-definition-kind', 'http://hl7.org/fhir/structure-definition-kind', 'http://hl7.org/fhir/structure-definition-kind');
  CODES_TFhirStructureMapContextTypeEnum : Array[TFhirStructureMapContextTypeEnum] of String = ('', 'type', 'variable');
  SYSTEMS_TFhirStructureMapContextTypeEnum : Array[TFhirStructureMapContextTypeEnum] of String = ('', 'http://hl7.org/fhir/map-context-type', 'http://hl7.org/fhir/map-context-type');
  CODES_TFhirStructureMapGroupTypeModeEnum : Array[TFhirStructureMapGroupTypeModeEnum] of String = ('', 'types', 'type-and-types');
  SYSTEMS_TFhirStructureMapGroupTypeModeEnum : Array[TFhirStructureMapGroupTypeModeEnum] of String = ('', 'http://hl7.org/fhir/map-group-type-mode', 'http://hl7.org/fhir/map-group-type-mode');
  CODES_TFhirStructureMapInputModeEnum : Array[TFhirStructureMapInputModeEnum] of String = ('', 'source', 'target');
  SYSTEMS_TFhirStructureMapInputModeEnum : Array[TFhirStructureMapInputModeEnum] of String = ('', 'http://hl7.org/fhir/map-input-mode', 'http://hl7.org/fhir/map-input-mode');
  CODES_TFhirStructureMapModelModeEnum : Array[TFhirStructureMapModelModeEnum] of String = ('', 'source', 'queried', 'target', 'produced');
  SYSTEMS_TFhirStructureMapModelModeEnum : Array[TFhirStructureMapModelModeEnum] of String = ('', 'http://hl7.org/fhir/map-model-mode', 'http://hl7.org/fhir/map-model-mode', 'http://hl7.org/fhir/map-model-mode', 'http://hl7.org/fhir/map-model-mode');
  CODES_TFhirStructureMapSourceListModeEnum : Array[TFhirStructureMapSourceListModeEnum] of String = ('', 'first', 'not_first', 'last', 'not_last', 'only_one');
  SYSTEMS_TFhirStructureMapSourceListModeEnum : Array[TFhirStructureMapSourceListModeEnum] of String = ('', 'http://hl7.org/fhir/map-source-list-mode', 'http://hl7.org/fhir/map-source-list-mode', 'http://hl7.org/fhir/map-source-list-mode', 'http://hl7.org/fhir/map-source-list-mode', 'http://hl7.org/fhir/map-source-list-mode');
  CODES_TFhirStructureMapTargetListModeEnum : Array[TFhirStructureMapTargetListModeEnum] of String = ('', 'first', 'share', 'last', 'collate');
  SYSTEMS_TFhirStructureMapTargetListModeEnum : Array[TFhirStructureMapTargetListModeEnum] of String = ('', 'http://hl7.org/fhir/map-target-list-mode', 'http://hl7.org/fhir/map-target-list-mode', 'http://hl7.org/fhir/map-target-list-mode', 'http://hl7.org/fhir/map-target-list-mode');
  CODES_TFhirStructureMapTransformEnum : Array[TFhirStructureMapTransformEnum] of String = ('', 'create', 'copy', 'truncate', 'escape', 'cast', 'append', 'translate', 'reference', 'dateOp', 'uuid', 'pointer', 'evaluate', 'cc', 'c', 'qty', 'id', 'cp');
  SYSTEMS_TFhirStructureMapTransformEnum : Array[TFhirStructureMapTransformEnum] of String = ('', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform', 'http://hl7.org/fhir/map-transform');
  CODES_TFhirSubscriptionNotificationTypeEnum : Array[TFhirSubscriptionNotificationTypeEnum] of String = ('', 'handshake', 'heartbeat', 'event-notification', 'query-status');
  SYSTEMS_TFhirSubscriptionNotificationTypeEnum : Array[TFhirSubscriptionNotificationTypeEnum] of String = ('', 'http://hl7.org/fhir/subscription-notification-type', 'http://hl7.org/fhir/subscription-notification-type', 'http://hl7.org/fhir/subscription-notification-type', 'http://hl7.org/fhir/subscription-notification-type');
  CODES_TFhirSubscriptionPayloadContentEnum : Array[TFhirSubscriptionPayloadContentEnum] of String = ('', 'empty', 'id-only', 'full-resource');
  SYSTEMS_TFhirSubscriptionPayloadContentEnum : Array[TFhirSubscriptionPayloadContentEnum] of String = ('', 'http://hl7.org/fhir/subscription-payload-content', 'http://hl7.org/fhir/subscription-payload-content', 'http://hl7.org/fhir/subscription-payload-content');
  CODES_TFhirSubscriptionSearchModifierEnum : Array[TFhirSubscriptionSearchModifierEnum] of String = ('', '=', 'eq', 'ne', 'gt', 'lt', 'ge', 'le', 'sa', 'eb', 'ap', 'above', 'below', 'in', 'not-in', 'of-type');
  SYSTEMS_TFhirSubscriptionSearchModifierEnum : Array[TFhirSubscriptionSearchModifierEnum] of String = ('', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier',
       'http://terminology.hl7.org/CodeSystem/subscription-search-modifier', 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier');
  CODES_TFhirSubscriptionStateEnum : Array[TFhirSubscriptionStateEnum] of String = ('', 'requested', 'active', 'error', 'off');
  SYSTEMS_TFhirSubscriptionStateEnum : Array[TFhirSubscriptionStateEnum] of String = ('', 'http://terminology.hl7.org/CodeSystem/subscription-state', 'http://terminology.hl7.org/CodeSystem/subscription-state', 'http://terminology.hl7.org/CodeSystem/subscription-state', 'http://terminology.hl7.org/CodeSystem/subscription-state');
  CODES_TFhirSupplyDeliveryStatusEnum : Array[TFhirSupplyDeliveryStatusEnum] of String = ('', 'in-progress', 'completed', 'abandoned', 'entered-in-error');
  SYSTEMS_TFhirSupplyDeliveryStatusEnum : Array[TFhirSupplyDeliveryStatusEnum] of String = ('', 'http://hl7.org/fhir/supplydelivery-status', 'http://hl7.org/fhir/supplydelivery-status', 'http://hl7.org/fhir/supplydelivery-status', 'http://hl7.org/fhir/supplydelivery-status');
  CODES_TFhirSupplyRequestStatusEnum : Array[TFhirSupplyRequestStatusEnum] of String = ('', 'draft', 'active', 'suspended', 'cancelled', 'completed', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirSupplyRequestStatusEnum : Array[TFhirSupplyRequestStatusEnum] of String = ('', 'http://hl7.org/fhir/supplyrequest-status', 'http://hl7.org/fhir/supplyrequest-status', 'http://hl7.org/fhir/supplyrequest-status', 'http://hl7.org/fhir/supplyrequest-status', 'http://hl7.org/fhir/supplyrequest-status', 'http://hl7.org/fhir/supplyrequest-status', 'http://hl7.org/fhir/supplyrequest-status');
  CODES_TFhirSystemRestfulInteractionEnum : Array[TFhirSystemRestfulInteractionEnum] of String = ('', 'transaction', 'batch', 'search-system', 'history-system');
  SYSTEMS_TFhirSystemRestfulInteractionEnum : Array[TFhirSystemRestfulInteractionEnum] of String = ('', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction');
  CODES_TFhirTaskIntentEnum : Array[TFhirTaskIntentEnum] of String = ('', 'unknown', 'proposal', 'plan', 'order', 'original-order', 'reflex-order', 'filler-order', 'instance-order', 'option');
  SYSTEMS_TFhirTaskIntentEnum : Array[TFhirTaskIntentEnum] of String = ('', 'http://hl7.org/fhir/task-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent', 'http://hl7.org/fhir/request-intent');
  CODES_TFhirTaskStatusEnum : Array[TFhirTaskStatusEnum] of String = ('', 'draft', 'requested', 'received', 'accepted', 'rejected', 'ready', 'cancelled', 'in-progress', 'on-hold', 'failed', 'completed', 'entered-in-error');
  SYSTEMS_TFhirTaskStatusEnum : Array[TFhirTaskStatusEnum] of String = ('', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status', 'http://hl7.org/fhir/task-status');
  CODES_TFhirTestReportActionResultEnum : Array[TFhirTestReportActionResultEnum] of String = ('', 'pass', 'skip', 'fail', 'warning', 'error');
  SYSTEMS_TFhirTestReportActionResultEnum : Array[TFhirTestReportActionResultEnum] of String = ('', 'http://hl7.org/fhir/report-action-result-codes', 'http://hl7.org/fhir/report-action-result-codes', 'http://hl7.org/fhir/report-action-result-codes', 'http://hl7.org/fhir/report-action-result-codes', 'http://hl7.org/fhir/report-action-result-codes');
  CODES_TFhirTestReportParticipantTypeEnum : Array[TFhirTestReportParticipantTypeEnum] of String = ('', 'test-engine', 'client', 'server');
  SYSTEMS_TFhirTestReportParticipantTypeEnum : Array[TFhirTestReportParticipantTypeEnum] of String = ('', 'http://hl7.org/fhir/report-participant-type', 'http://hl7.org/fhir/report-participant-type', 'http://hl7.org/fhir/report-participant-type');
  CODES_TFhirTestReportResultEnum : Array[TFhirTestReportResultEnum] of String = ('', 'pass', 'fail', 'pending');
  SYSTEMS_TFhirTestReportResultEnum : Array[TFhirTestReportResultEnum] of String = ('', 'http://hl7.org/fhir/report-result-codes', 'http://hl7.org/fhir/report-result-codes', 'http://hl7.org/fhir/report-result-codes');
  CODES_TFhirTestReportStatusEnum : Array[TFhirTestReportStatusEnum] of String = ('', 'completed', 'in-progress', 'waiting', 'stopped', 'entered-in-error');
  SYSTEMS_TFhirTestReportStatusEnum : Array[TFhirTestReportStatusEnum] of String = ('', 'http://hl7.org/fhir/report-status-codes', 'http://hl7.org/fhir/report-status-codes', 'http://hl7.org/fhir/report-status-codes', 'http://hl7.org/fhir/report-status-codes', 'http://hl7.org/fhir/report-status-codes');
  CODES_TFhirTestScriptRequestMethodCodeEnum : Array[TFhirTestScriptRequestMethodCodeEnum] of String = ('', 'delete', 'get', 'options', 'patch', 'post', 'put', 'head');
  SYSTEMS_TFhirTestScriptRequestMethodCodeEnum : Array[TFhirTestScriptRequestMethodCodeEnum] of String = ('', 'http://hl7.org/fhir/http-operations', 'http://hl7.org/fhir/http-operations', 'http://hl7.org/fhir/http-operations', 'http://hl7.org/fhir/http-operations', 'http://hl7.org/fhir/http-operations', 'http://hl7.org/fhir/http-operations', 'http://hl7.org/fhir/http-operations');
  CODES_TFhirTriggerTypeEnum : Array[TFhirTriggerTypeEnum] of String = ('', 'named-event', 'periodic', 'data-changed', 'data-added', 'data-modified', 'data-removed', 'data-accessed', 'data-access-ended');
  SYSTEMS_TFhirTriggerTypeEnum : Array[TFhirTriggerTypeEnum] of String = ('', 'http://hl7.org/fhir/trigger-type', 'http://hl7.org/fhir/trigger-type', 'http://hl7.org/fhir/trigger-type', 'http://hl7.org/fhir/trigger-type', 'http://hl7.org/fhir/trigger-type', 'http://hl7.org/fhir/trigger-type', 'http://hl7.org/fhir/trigger-type', 'http://hl7.org/fhir/trigger-type');
  CODES_TFhirTypeDerivationRuleEnum : Array[TFhirTypeDerivationRuleEnum] of String = ('', 'specialization', 'constraint');
  SYSTEMS_TFhirTypeDerivationRuleEnum : Array[TFhirTypeDerivationRuleEnum] of String = ('', 'http://hl7.org/fhir/type-derivation-rule', 'http://hl7.org/fhir/type-derivation-rule');
  CODES_TFhirTypeRestfulInteractionEnum : Array[TFhirTypeRestfulInteractionEnum] of String = ('', 'read', 'vread', 'update', 'patch', 'delete', 'history-instance', 'history-type', 'create', 'search-type');
  SYSTEMS_TFhirTypeRestfulInteractionEnum : Array[TFhirTypeRestfulInteractionEnum] of String = ('', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction');
  CODES_TFhirUDIEntryTypeEnum : Array[TFhirUDIEntryTypeEnum] of String = ('', 'barcode', 'rfid', 'manual', 'card', 'self-reported', 'electronic-transmission', 'unknown');
  SYSTEMS_TFhirUDIEntryTypeEnum : Array[TFhirUDIEntryTypeEnum] of String = ('', 'http://hl7.org/fhir/udi-entry-type', 'http://hl7.org/fhir/udi-entry-type', 'http://hl7.org/fhir/udi-entry-type', 'http://hl7.org/fhir/udi-entry-type', 'http://hl7.org/fhir/udi-entry-type', 'http://hl7.org/fhir/udi-entry-type', 'http://hl7.org/fhir/udi-entry-type');
  CODES_TFhirUnitsOfTimeEnum : Array[TFhirUnitsOfTimeEnum] of String = ('', 's', 'min', 'h', 'd', 'wk', 'mo', 'a');
  SYSTEMS_TFhirUnitsOfTimeEnum : Array[TFhirUnitsOfTimeEnum] of String = ('', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org');
  CODES_TFhirUseEnum : Array[TFhirUseEnum] of String = ('', 'claim', 'preauthorization', 'predetermination');
  SYSTEMS_TFhirUseEnum : Array[TFhirUseEnum] of String = ('', 'http://hl7.org/fhir/claim-use', 'http://hl7.org/fhir/claim-use', 'http://hl7.org/fhir/claim-use');
  CODES_TFhirVisionBaseEnum : Array[TFhirVisionBaseEnum] of String = ('', 'up', 'down', 'in', 'out');
  SYSTEMS_TFhirVisionBaseEnum : Array[TFhirVisionBaseEnum] of String = ('', 'http://hl7.org/fhir/vision-base-codes', 'http://hl7.org/fhir/vision-base-codes', 'http://hl7.org/fhir/vision-base-codes', 'http://hl7.org/fhir/vision-base-codes');
  CODES_TFhirVisionEyesEnum : Array[TFhirVisionEyesEnum] of String = ('', 'right', 'left');
  SYSTEMS_TFhirVisionEyesEnum : Array[TFhirVisionEyesEnum] of String = ('', 'http://hl7.org/fhir/vision-eye-codes', 'http://hl7.org/fhir/vision-eye-codes');
  CODES_TFhirXPathUsageTypeEnum : Array[TFhirXPathUsageTypeEnum] of String = ('', 'normal', 'phonetic', 'nearby', 'distance', 'other');
  SYSTEMS_TFhirXPathUsageTypeEnum : Array[TFhirXPathUsageTypeEnum] of String = ('', 'http://hl7.org/fhir/search-xpath-usage', 'http://hl7.org/fhir/search-xpath-usage', 'http://hl7.org/fhir/search-xpath-usage', 'http://hl7.org/fhir/search-xpath-usage', 'http://hl7.org/fhir/search-xpath-usage');

function TFhirAccountStatusEnumListAsInteger(aSet : TFhirAccountStatusEnumList) : Integer; overload;
function IntegerAsTFhirAccountStatusEnumList(i : integer) : TFhirAccountStatusEnumList; overload;
function TFhirActionCardinalityBehaviorEnumListAsInteger(aSet : TFhirActionCardinalityBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirActionCardinalityBehaviorEnumList(i : integer) : TFhirActionCardinalityBehaviorEnumList; overload;
function TFhirActionConditionKindEnumListAsInteger(aSet : TFhirActionConditionKindEnumList) : Integer; overload;
function IntegerAsTFhirActionConditionKindEnumList(i : integer) : TFhirActionConditionKindEnumList; overload;
function TFhirActionGroupingBehaviorEnumListAsInteger(aSet : TFhirActionGroupingBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirActionGroupingBehaviorEnumList(i : integer) : TFhirActionGroupingBehaviorEnumList; overload;
function TFhirActionParticipantTypeEnumListAsInteger(aSet : TFhirActionParticipantTypeEnumList) : Integer; overload;
function IntegerAsTFhirActionParticipantTypeEnumList(i : integer) : TFhirActionParticipantTypeEnumList; overload;
function TFhirActionPrecheckBehaviorEnumListAsInteger(aSet : TFhirActionPrecheckBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirActionPrecheckBehaviorEnumList(i : integer) : TFhirActionPrecheckBehaviorEnumList; overload;
function TFhirActionRelationshipTypeEnumListAsInteger(aSet : TFhirActionRelationshipTypeEnumList) : Integer; overload;
function IntegerAsTFhirActionRelationshipTypeEnumList(i : integer) : TFhirActionRelationshipTypeEnumList; overload;
function TFhirActionRequiredBehaviorEnumListAsInteger(aSet : TFhirActionRequiredBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirActionRequiredBehaviorEnumList(i : integer) : TFhirActionRequiredBehaviorEnumList; overload;
function TFhirActionSelectionBehaviorEnumListAsInteger(aSet : TFhirActionSelectionBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirActionSelectionBehaviorEnumList(i : integer) : TFhirActionSelectionBehaviorEnumList; overload;
function TFhirAddressTypeEnumListAsInteger(aSet : TFhirAddressTypeEnumList) : Integer; overload;
function IntegerAsTFhirAddressTypeEnumList(i : integer) : TFhirAddressTypeEnumList; overload;
function TFhirAddressUseEnumListAsInteger(aSet : TFhirAddressUseEnumList) : Integer; overload;
function IntegerAsTFhirAddressUseEnumList(i : integer) : TFhirAddressUseEnumList; overload;
function TFhirAdministrativeGenderEnumListAsInteger(aSet : TFhirAdministrativeGenderEnumList) : Integer; overload;
function IntegerAsTFhirAdministrativeGenderEnumList(i : integer) : TFhirAdministrativeGenderEnumList; overload;
function TFhirAdverseEventActualityEnumListAsInteger(aSet : TFhirAdverseEventActualityEnumList) : Integer; overload;
function IntegerAsTFhirAdverseEventActualityEnumList(i : integer) : TFhirAdverseEventActualityEnumList; overload;
function TFhirAdverseEventStatusEnumListAsInteger(aSet : TFhirAdverseEventStatusEnumList) : Integer; overload;
function IntegerAsTFhirAdverseEventStatusEnumList(i : integer) : TFhirAdverseEventStatusEnumList; overload;
function TFhirAggregationModeEnumListAsInteger(aSet : TFhirAggregationModeEnumList) : Integer; overload;
function IntegerAsTFhirAggregationModeEnumList(i : integer) : TFhirAggregationModeEnumList; overload;
function TFhirAllTypesEnumListAsInteger(aSet : TFhirAllTypesEnumList) : Integer; overload;
function IntegerAsTFhirAllTypesEnumList(i : integer) : TFhirAllTypesEnumList; overload;
function TFhirAllergyIntoleranceCategoryEnumListAsInteger(aSet : TFhirAllergyIntoleranceCategoryEnumList) : Integer; overload;
function IntegerAsTFhirAllergyIntoleranceCategoryEnumList(i : integer) : TFhirAllergyIntoleranceCategoryEnumList; overload;
function TFhirAllergyIntoleranceCriticalityEnumListAsInteger(aSet : TFhirAllergyIntoleranceCriticalityEnumList) : Integer; overload;
function IntegerAsTFhirAllergyIntoleranceCriticalityEnumList(i : integer) : TFhirAllergyIntoleranceCriticalityEnumList; overload;
function TFhirAllergyIntoleranceSeverityEnumListAsInteger(aSet : TFhirAllergyIntoleranceSeverityEnumList) : Integer; overload;
function IntegerAsTFhirAllergyIntoleranceSeverityEnumList(i : integer) : TFhirAllergyIntoleranceSeverityEnumList; overload;
function TFhirAllergyIntoleranceTypeEnumListAsInteger(aSet : TFhirAllergyIntoleranceTypeEnumList) : Integer; overload;
function IntegerAsTFhirAllergyIntoleranceTypeEnumList(i : integer) : TFhirAllergyIntoleranceTypeEnumList; overload;
function TFhirAppointmentStatusEnumListAsInteger(aSet : TFhirAppointmentStatusEnumList) : Integer; overload;
function IntegerAsTFhirAppointmentStatusEnumList(i : integer) : TFhirAppointmentStatusEnumList; overload;
function TFhirAssertionDirectionTypeEnumListAsInteger(aSet : TFhirAssertionDirectionTypeEnumList) : Integer; overload;
function IntegerAsTFhirAssertionDirectionTypeEnumList(i : integer) : TFhirAssertionDirectionTypeEnumList; overload;
function TFhirAssertionOperatorTypeEnumListAsInteger(aSet : TFhirAssertionOperatorTypeEnumList) : Integer; overload;
function IntegerAsTFhirAssertionOperatorTypeEnumList(i : integer) : TFhirAssertionOperatorTypeEnumList; overload;
function TFhirAssertionResponseTypesEnumListAsInteger(aSet : TFhirAssertionResponseTypesEnumList) : Integer; overload;
function IntegerAsTFhirAssertionResponseTypesEnumList(i : integer) : TFhirAssertionResponseTypesEnumList; overload;
function TFhirAuditEventActionEnumListAsInteger(aSet : TFhirAuditEventActionEnumList) : Integer; overload;
function IntegerAsTFhirAuditEventActionEnumList(i : integer) : TFhirAuditEventActionEnumList; overload;
function TFhirAuditEventAgentNetworkTypeEnumListAsInteger(aSet : TFhirAuditEventAgentNetworkTypeEnumList) : Integer; overload;
function IntegerAsTFhirAuditEventAgentNetworkTypeEnumList(i : integer) : TFhirAuditEventAgentNetworkTypeEnumList; overload;
function TFhirAuditEventSeverityEnumListAsInteger(aSet : TFhirAuditEventSeverityEnumList) : Integer; overload;
function IntegerAsTFhirAuditEventSeverityEnumList(i : integer) : TFhirAuditEventSeverityEnumList; overload;
function TFhirBindingStrengthEnumListAsInteger(aSet : TFhirBindingStrengthEnumList) : Integer; overload;
function IntegerAsTFhirBindingStrengthEnumList(i : integer) : TFhirBindingStrengthEnumList; overload;
function TFhirBiologicallyDerivedProductCategoryEnumListAsInteger(aSet : TFhirBiologicallyDerivedProductCategoryEnumList) : Integer; overload;
function IntegerAsTFhirBiologicallyDerivedProductCategoryEnumList(i : integer) : TFhirBiologicallyDerivedProductCategoryEnumList; overload;
function TFhirBiologicallyDerivedProductStatusEnumListAsInteger(aSet : TFhirBiologicallyDerivedProductStatusEnumList) : Integer; overload;
function IntegerAsTFhirBiologicallyDerivedProductStatusEnumList(i : integer) : TFhirBiologicallyDerivedProductStatusEnumList; overload;
function TFhirBiologicallyDerivedProductStorageScaleEnumListAsInteger(aSet : TFhirBiologicallyDerivedProductStorageScaleEnumList) : Integer; overload;
function IntegerAsTFhirBiologicallyDerivedProductStorageScaleEnumList(i : integer) : TFhirBiologicallyDerivedProductStorageScaleEnumList; overload;
function TFhirBundleTypeEnumListAsInteger(aSet : TFhirBundleTypeEnumList) : Integer; overload;
function IntegerAsTFhirBundleTypeEnumList(i : integer) : TFhirBundleTypeEnumList; overload;
function TFhirCapabilityStatementKindEnumListAsInteger(aSet : TFhirCapabilityStatementKindEnumList) : Integer; overload;
function IntegerAsTFhirCapabilityStatementKindEnumList(i : integer) : TFhirCapabilityStatementKindEnumList; overload;
function TFhirCarePlanActivityKindEnumListAsInteger(aSet : TFhirCarePlanActivityKindEnumList) : Integer; overload;
function IntegerAsTFhirCarePlanActivityKindEnumList(i : integer) : TFhirCarePlanActivityKindEnumList; overload;
function TFhirCarePlanActivityStatusEnumListAsInteger(aSet : TFhirCarePlanActivityStatusEnumList) : Integer; overload;
function IntegerAsTFhirCarePlanActivityStatusEnumList(i : integer) : TFhirCarePlanActivityStatusEnumList; overload;
function TFhirCarePlanIntentEnumListAsInteger(aSet : TFhirCarePlanIntentEnumList) : Integer; overload;
function IntegerAsTFhirCarePlanIntentEnumList(i : integer) : TFhirCarePlanIntentEnumList; overload;
function TFhirCareTeamStatusEnumListAsInteger(aSet : TFhirCareTeamStatusEnumList) : Integer; overload;
function IntegerAsTFhirCareTeamStatusEnumList(i : integer) : TFhirCareTeamStatusEnumList; overload;
function TFhirCatalogEntryRelationTypeEnumListAsInteger(aSet : TFhirCatalogEntryRelationTypeEnumList) : Integer; overload;
function IntegerAsTFhirCatalogEntryRelationTypeEnumList(i : integer) : TFhirCatalogEntryRelationTypeEnumList; overload;
function TFhirCatalogEntryStatusEnumListAsInteger(aSet : TFhirCatalogEntryStatusEnumList) : Integer; overload;
function IntegerAsTFhirCatalogEntryStatusEnumList(i : integer) : TFhirCatalogEntryStatusEnumList; overload;
function TFhirCatalogEntryTypeEnumListAsInteger(aSet : TFhirCatalogEntryTypeEnumList) : Integer; overload;
function IntegerAsTFhirCatalogEntryTypeEnumList(i : integer) : TFhirCatalogEntryTypeEnumList; overload;
function TFhirCharacteristicCombinationEnumListAsInteger(aSet : TFhirCharacteristicCombinationEnumList) : Integer; overload;
function IntegerAsTFhirCharacteristicCombinationEnumList(i : integer) : TFhirCharacteristicCombinationEnumList; overload;
function TFhirChargeItemStatusEnumListAsInteger(aSet : TFhirChargeItemStatusEnumList) : Integer; overload;
function IntegerAsTFhirChargeItemStatusEnumList(i : integer) : TFhirChargeItemStatusEnumList; overload;
function TFhirClaimProcessingCodesEnumListAsInteger(aSet : TFhirClaimProcessingCodesEnumList) : Integer; overload;
function IntegerAsTFhirClaimProcessingCodesEnumList(i : integer) : TFhirClaimProcessingCodesEnumList; overload;
function TFhirClinicalUseIssueTypeEnumListAsInteger(aSet : TFhirClinicalUseIssueTypeEnumList) : Integer; overload;
function IntegerAsTFhirClinicalUseIssueTypeEnumList(i : integer) : TFhirClinicalUseIssueTypeEnumList; overload;
function TFhirCodeSearchSupportEnumListAsInteger(aSet : TFhirCodeSearchSupportEnumList) : Integer; overload;
function IntegerAsTFhirCodeSearchSupportEnumList(i : integer) : TFhirCodeSearchSupportEnumList; overload;
function TFhirCodeSystemContentModeEnumListAsInteger(aSet : TFhirCodeSystemContentModeEnumList) : Integer; overload;
function IntegerAsTFhirCodeSystemContentModeEnumList(i : integer) : TFhirCodeSystemContentModeEnumList; overload;
function TFhirCodeSystemHierarchyMeaningEnumListAsInteger(aSet : TFhirCodeSystemHierarchyMeaningEnumList) : Integer; overload;
function IntegerAsTFhirCodeSystemHierarchyMeaningEnumList(i : integer) : TFhirCodeSystemHierarchyMeaningEnumList; overload;
function TFhirCompartmentTypeEnumListAsInteger(aSet : TFhirCompartmentTypeEnumList) : Integer; overload;
function IntegerAsTFhirCompartmentTypeEnumList(i : integer) : TFhirCompartmentTypeEnumList; overload;
function TFhirCompositionAttestationModeEnumListAsInteger(aSet : TFhirCompositionAttestationModeEnumList) : Integer; overload;
function IntegerAsTFhirCompositionAttestationModeEnumList(i : integer) : TFhirCompositionAttestationModeEnumList; overload;
function TFhirCompositionStatusEnumListAsInteger(aSet : TFhirCompositionStatusEnumList) : Integer; overload;
function IntegerAsTFhirCompositionStatusEnumList(i : integer) : TFhirCompositionStatusEnumList; overload;
function TFhirConceptMapGroupUnmappedModeEnumListAsInteger(aSet : TFhirConceptMapGroupUnmappedModeEnumList) : Integer; overload;
function IntegerAsTFhirConceptMapGroupUnmappedModeEnumList(i : integer) : TFhirConceptMapGroupUnmappedModeEnumList; overload;
function TFhirConceptMapRelationshipEnumListAsInteger(aSet : TFhirConceptMapRelationshipEnumList) : Integer; overload;
function IntegerAsTFhirConceptMapRelationshipEnumList(i : integer) : TFhirConceptMapRelationshipEnumList; overload;
function TFhirConceptPropertyTypeEnumListAsInteger(aSet : TFhirConceptPropertyTypeEnumList) : Integer; overload;
function IntegerAsTFhirConceptPropertyTypeEnumList(i : integer) : TFhirConceptPropertyTypeEnumList; overload;
function TFhirConditionPreconditionTypeEnumListAsInteger(aSet : TFhirConditionPreconditionTypeEnumList) : Integer; overload;
function IntegerAsTFhirConditionPreconditionTypeEnumList(i : integer) : TFhirConditionPreconditionTypeEnumList; overload;
function TFhirConditionQuestionnairePurposeEnumListAsInteger(aSet : TFhirConditionQuestionnairePurposeEnumList) : Integer; overload;
function IntegerAsTFhirConditionQuestionnairePurposeEnumList(i : integer) : TFhirConditionQuestionnairePurposeEnumList; overload;
function TFhirConditionalDeleteStatusEnumListAsInteger(aSet : TFhirConditionalDeleteStatusEnumList) : Integer; overload;
function IntegerAsTFhirConditionalDeleteStatusEnumList(i : integer) : TFhirConditionalDeleteStatusEnumList; overload;
function TFhirConditionalReadStatusEnumListAsInteger(aSet : TFhirConditionalReadStatusEnumList) : Integer; overload;
function IntegerAsTFhirConditionalReadStatusEnumList(i : integer) : TFhirConditionalReadStatusEnumList; overload;
function TFhirConsentDataMeaningEnumListAsInteger(aSet : TFhirConsentDataMeaningEnumList) : Integer; overload;
function IntegerAsTFhirConsentDataMeaningEnumList(i : integer) : TFhirConsentDataMeaningEnumList; overload;
function TFhirConsentProvisionTypeEnumListAsInteger(aSet : TFhirConsentProvisionTypeEnumList) : Integer; overload;
function IntegerAsTFhirConsentProvisionTypeEnumList(i : integer) : TFhirConsentProvisionTypeEnumList; overload;
function TFhirConsentStateEnumListAsInteger(aSet : TFhirConsentStateEnumList) : Integer; overload;
function IntegerAsTFhirConsentStateEnumList(i : integer) : TFhirConsentStateEnumList; overload;
function TFhirConstraintSeverityEnumListAsInteger(aSet : TFhirConstraintSeverityEnumList) : Integer; overload;
function IntegerAsTFhirConstraintSeverityEnumList(i : integer) : TFhirConstraintSeverityEnumList; overload;
function TFhirContactPointSystemEnumListAsInteger(aSet : TFhirContactPointSystemEnumList) : Integer; overload;
function IntegerAsTFhirContactPointSystemEnumList(i : integer) : TFhirContactPointSystemEnumList; overload;
function TFhirContactPointUseEnumListAsInteger(aSet : TFhirContactPointUseEnumList) : Integer; overload;
function IntegerAsTFhirContactPointUseEnumList(i : integer) : TFhirContactPointUseEnumList; overload;
function TFhirContractResourcePublicationStatusCodesEnumListAsInteger(aSet : TFhirContractResourcePublicationStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirContractResourcePublicationStatusCodesEnumList(i : integer) : TFhirContractResourcePublicationStatusCodesEnumList; overload;
function TFhirContractResourceStatusCodesEnumListAsInteger(aSet : TFhirContractResourceStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirContractResourceStatusCodesEnumList(i : integer) : TFhirContractResourceStatusCodesEnumList; overload;
function TFhirContributorTypeEnumListAsInteger(aSet : TFhirContributorTypeEnumList) : Integer; overload;
function IntegerAsTFhirContributorTypeEnumList(i : integer) : TFhirContributorTypeEnumList; overload;
function TFhirDaysOfWeekEnumListAsInteger(aSet : TFhirDaysOfWeekEnumList) : Integer; overload;
function IntegerAsTFhirDaysOfWeekEnumList(i : integer) : TFhirDaysOfWeekEnumList; overload;
function TFhirDetectedIssueSeverityEnumListAsInteger(aSet : TFhirDetectedIssueSeverityEnumList) : Integer; overload;
function IntegerAsTFhirDetectedIssueSeverityEnumList(i : integer) : TFhirDetectedIssueSeverityEnumList; overload;
function TFhirDeviceMetricCalibrationStateEnumListAsInteger(aSet : TFhirDeviceMetricCalibrationStateEnumList) : Integer; overload;
function IntegerAsTFhirDeviceMetricCalibrationStateEnumList(i : integer) : TFhirDeviceMetricCalibrationStateEnumList; overload;
function TFhirDeviceMetricCalibrationTypeEnumListAsInteger(aSet : TFhirDeviceMetricCalibrationTypeEnumList) : Integer; overload;
function IntegerAsTFhirDeviceMetricCalibrationTypeEnumList(i : integer) : TFhirDeviceMetricCalibrationTypeEnumList; overload;
function TFhirDeviceMetricCategoryEnumListAsInteger(aSet : TFhirDeviceMetricCategoryEnumList) : Integer; overload;
function IntegerAsTFhirDeviceMetricCategoryEnumList(i : integer) : TFhirDeviceMetricCategoryEnumList; overload;
function TFhirDeviceMetricColorEnumListAsInteger(aSet : TFhirDeviceMetricColorEnumList) : Integer; overload;
function IntegerAsTFhirDeviceMetricColorEnumList(i : integer) : TFhirDeviceMetricColorEnumList; overload;
function TFhirDeviceMetricOperationalStatusEnumListAsInteger(aSet : TFhirDeviceMetricOperationalStatusEnumList) : Integer; overload;
function IntegerAsTFhirDeviceMetricOperationalStatusEnumList(i : integer) : TFhirDeviceMetricOperationalStatusEnumList; overload;
function TFhirDeviceNameTypeEnumListAsInteger(aSet : TFhirDeviceNameTypeEnumList) : Integer; overload;
function IntegerAsTFhirDeviceNameTypeEnumList(i : integer) : TFhirDeviceNameTypeEnumList; overload;
function TFhirDeviceUseStatementStatusEnumListAsInteger(aSet : TFhirDeviceUseStatementStatusEnumList) : Integer; overload;
function IntegerAsTFhirDeviceUseStatementStatusEnumList(i : integer) : TFhirDeviceUseStatementStatusEnumList; overload;
function TFhirDiagnosticReportStatusEnumListAsInteger(aSet : TFhirDiagnosticReportStatusEnumList) : Integer; overload;
function IntegerAsTFhirDiagnosticReportStatusEnumList(i : integer) : TFhirDiagnosticReportStatusEnumList; overload;
function TFhirDiscriminatorTypeEnumListAsInteger(aSet : TFhirDiscriminatorTypeEnumList) : Integer; overload;
function IntegerAsTFhirDiscriminatorTypeEnumList(i : integer) : TFhirDiscriminatorTypeEnumList; overload;
function TFhirDocumentAttestationModeEnumListAsInteger(aSet : TFhirDocumentAttestationModeEnumList) : Integer; overload;
function IntegerAsTFhirDocumentAttestationModeEnumList(i : integer) : TFhirDocumentAttestationModeEnumList; overload;
function TFhirDocumentModeEnumListAsInteger(aSet : TFhirDocumentModeEnumList) : Integer; overload;
function IntegerAsTFhirDocumentModeEnumList(i : integer) : TFhirDocumentModeEnumList; overload;
function TFhirDocumentReferenceStatusEnumListAsInteger(aSet : TFhirDocumentReferenceStatusEnumList) : Integer; overload;
function IntegerAsTFhirDocumentReferenceStatusEnumList(i : integer) : TFhirDocumentReferenceStatusEnumList; overload;
function TFhirDocumentRelationshipTypeEnumListAsInteger(aSet : TFhirDocumentRelationshipTypeEnumList) : Integer; overload;
function IntegerAsTFhirDocumentRelationshipTypeEnumList(i : integer) : TFhirDocumentRelationshipTypeEnumList; overload;
function TFhirEligibilityRequestPurposeEnumListAsInteger(aSet : TFhirEligibilityRequestPurposeEnumList) : Integer; overload;
function IntegerAsTFhirEligibilityRequestPurposeEnumList(i : integer) : TFhirEligibilityRequestPurposeEnumList; overload;
function TFhirEligibilityResponsePurposeEnumListAsInteger(aSet : TFhirEligibilityResponsePurposeEnumList) : Integer; overload;
function IntegerAsTFhirEligibilityResponsePurposeEnumList(i : integer) : TFhirEligibilityResponsePurposeEnumList; overload;
function TFhirEnableWhenBehaviorEnumListAsInteger(aSet : TFhirEnableWhenBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirEnableWhenBehaviorEnumList(i : integer) : TFhirEnableWhenBehaviorEnumList; overload;
function TFhirEncounterLocationStatusEnumListAsInteger(aSet : TFhirEncounterLocationStatusEnumList) : Integer; overload;
function IntegerAsTFhirEncounterLocationStatusEnumList(i : integer) : TFhirEncounterLocationStatusEnumList; overload;
function TFhirEncounterStatusEnumListAsInteger(aSet : TFhirEncounterStatusEnumList) : Integer; overload;
function IntegerAsTFhirEncounterStatusEnumList(i : integer) : TFhirEncounterStatusEnumList; overload;
function TFhirEndpointStatusEnumListAsInteger(aSet : TFhirEndpointStatusEnumList) : Integer; overload;
function IntegerAsTFhirEndpointStatusEnumList(i : integer) : TFhirEndpointStatusEnumList; overload;
function TFhirEpisodeOfCareStatusEnumListAsInteger(aSet : TFhirEpisodeOfCareStatusEnumList) : Integer; overload;
function IntegerAsTFhirEpisodeOfCareStatusEnumList(i : integer) : TFhirEpisodeOfCareStatusEnumList; overload;
function TFhirEventCapabilityModeEnumListAsInteger(aSet : TFhirEventCapabilityModeEnumList) : Integer; overload;
function IntegerAsTFhirEventCapabilityModeEnumList(i : integer) : TFhirEventCapabilityModeEnumList; overload;
function TFhirEventStatusEnumListAsInteger(aSet : TFhirEventStatusEnumList) : Integer; overload;
function IntegerAsTFhirEventStatusEnumList(i : integer) : TFhirEventStatusEnumList; overload;
function TFhirEventTimingEnumListAsInteger(aSet : TFhirEventTimingEnumList) : Integer; overload;
function IntegerAsTFhirEventTimingEnumList(i : integer) : TFhirEventTimingEnumList; overload;
function TFhirEvidenceVariableHandlingEnumListAsInteger(aSet : TFhirEvidenceVariableHandlingEnumList) : Integer; overload;
function IntegerAsTFhirEvidenceVariableHandlingEnumList(i : integer) : TFhirEvidenceVariableHandlingEnumList; overload;
function TFhirExampleScenarioActorTypeEnumListAsInteger(aSet : TFhirExampleScenarioActorTypeEnumList) : Integer; overload;
function IntegerAsTFhirExampleScenarioActorTypeEnumList(i : integer) : TFhirExampleScenarioActorTypeEnumList; overload;
function TFhirExplanationOfBenefitStatusEnumListAsInteger(aSet : TFhirExplanationOfBenefitStatusEnumList) : Integer; overload;
function IntegerAsTFhirExplanationOfBenefitStatusEnumList(i : integer) : TFhirExplanationOfBenefitStatusEnumList; overload;
function TFhirExtensionContextTypeEnumListAsInteger(aSet : TFhirExtensionContextTypeEnumList) : Integer; overload;
function IntegerAsTFhirExtensionContextTypeEnumList(i : integer) : TFhirExtensionContextTypeEnumList; overload;
function TFhirFHIRDefinedTypeEnumListAsInteger(aSet : TFhirFHIRDefinedTypeEnumList) : Integer; overload;
function IntegerAsTFhirFHIRDefinedTypeEnumList(i : integer) : TFhirFHIRDefinedTypeEnumList; overload;
function TFhirFHIRDeviceStatusEnumListAsInteger(aSet : TFhirFHIRDeviceStatusEnumList) : Integer; overload;
function IntegerAsTFhirFHIRDeviceStatusEnumList(i : integer) : TFhirFHIRDeviceStatusEnumList; overload;
function TFhirFHIRSubstanceStatusEnumListAsInteger(aSet : TFhirFHIRSubstanceStatusEnumList) : Integer; overload;
function IntegerAsTFhirFHIRSubstanceStatusEnumList(i : integer) : TFhirFHIRSubstanceStatusEnumList; overload;
function TFhirFHIRVersionEnumListAsInteger(aSet : TFhirFHIRVersionEnumList) : Integer; overload;
function IntegerAsTFhirFHIRVersionEnumList(i : integer) : TFhirFHIRVersionEnumList; overload;
function TFhirFamilyHistoryStatusEnumListAsInteger(aSet : TFhirFamilyHistoryStatusEnumList) : Integer; overload;
function IntegerAsTFhirFamilyHistoryStatusEnumList(i : integer) : TFhirFamilyHistoryStatusEnumList; overload;
function TFhirFilterOperatorEnumListAsInteger(aSet : TFhirFilterOperatorEnumList) : Integer; overload;
function IntegerAsTFhirFilterOperatorEnumList(i : integer) : TFhirFilterOperatorEnumList; overload;
function TFhirFinancialResourceStatusCodesEnumListAsInteger(aSet : TFhirFinancialResourceStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirFinancialResourceStatusCodesEnumList(i : integer) : TFhirFinancialResourceStatusCodesEnumList; overload;
function TFhirFlagStatusEnumListAsInteger(aSet : TFhirFlagStatusEnumList) : Integer; overload;
function IntegerAsTFhirFlagStatusEnumList(i : integer) : TFhirFlagStatusEnumList; overload;
function TFhirGoalLifecycleStatusEnumListAsInteger(aSet : TFhirGoalLifecycleStatusEnumList) : Integer; overload;
function IntegerAsTFhirGoalLifecycleStatusEnumList(i : integer) : TFhirGoalLifecycleStatusEnumList; overload;
function TFhirGraphCompartmentRuleEnumListAsInteger(aSet : TFhirGraphCompartmentRuleEnumList) : Integer; overload;
function IntegerAsTFhirGraphCompartmentRuleEnumList(i : integer) : TFhirGraphCompartmentRuleEnumList; overload;
function TFhirGraphCompartmentUseEnumListAsInteger(aSet : TFhirGraphCompartmentUseEnumList) : Integer; overload;
function IntegerAsTFhirGraphCompartmentUseEnumList(i : integer) : TFhirGraphCompartmentUseEnumList; overload;
function TFhirGroupMeasureEnumListAsInteger(aSet : TFhirGroupMeasureEnumList) : Integer; overload;
function IntegerAsTFhirGroupMeasureEnumList(i : integer) : TFhirGroupMeasureEnumList; overload;
function TFhirGroupTypeEnumListAsInteger(aSet : TFhirGroupTypeEnumList) : Integer; overload;
function IntegerAsTFhirGroupTypeEnumList(i : integer) : TFhirGroupTypeEnumList; overload;
function TFhirGuidanceResponseStatusEnumListAsInteger(aSet : TFhirGuidanceResponseStatusEnumList) : Integer; overload;
function IntegerAsTFhirGuidanceResponseStatusEnumList(i : integer) : TFhirGuidanceResponseStatusEnumList; overload;
function TFhirGuidePageGenerationEnumListAsInteger(aSet : TFhirGuidePageGenerationEnumList) : Integer; overload;
function IntegerAsTFhirGuidePageGenerationEnumList(i : integer) : TFhirGuidePageGenerationEnumList; overload;
function TFhirHTTPVerbEnumListAsInteger(aSet : TFhirHTTPVerbEnumList) : Integer; overload;
function IntegerAsTFhirHTTPVerbEnumList(i : integer) : TFhirHTTPVerbEnumList; overload;
function TFhirIdentifierUseEnumListAsInteger(aSet : TFhirIdentifierUseEnumList) : Integer; overload;
function IntegerAsTFhirIdentifierUseEnumList(i : integer) : TFhirIdentifierUseEnumList; overload;
function TFhirIdentityAssuranceLevelEnumListAsInteger(aSet : TFhirIdentityAssuranceLevelEnumList) : Integer; overload;
function IntegerAsTFhirIdentityAssuranceLevelEnumList(i : integer) : TFhirIdentityAssuranceLevelEnumList; overload;
function TFhirImagingStudyStatusEnumListAsInteger(aSet : TFhirImagingStudyStatusEnumList) : Integer; overload;
function IntegerAsTFhirImagingStudyStatusEnumList(i : integer) : TFhirImagingStudyStatusEnumList; overload;
function TFhirImmunizationEvaluationStatusCodesEnumListAsInteger(aSet : TFhirImmunizationEvaluationStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirImmunizationEvaluationStatusCodesEnumList(i : integer) : TFhirImmunizationEvaluationStatusCodesEnumList; overload;
function TFhirImmunizationStatusCodesEnumListAsInteger(aSet : TFhirImmunizationStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirImmunizationStatusCodesEnumList(i : integer) : TFhirImmunizationStatusCodesEnumList; overload;
function TFhirInteractionTriggerEnumListAsInteger(aSet : TFhirInteractionTriggerEnumList) : Integer; overload;
function IntegerAsTFhirInteractionTriggerEnumList(i : integer) : TFhirInteractionTriggerEnumList; overload;
function TFhirInvoicePriceComponentTypeEnumListAsInteger(aSet : TFhirInvoicePriceComponentTypeEnumList) : Integer; overload;
function IntegerAsTFhirInvoicePriceComponentTypeEnumList(i : integer) : TFhirInvoicePriceComponentTypeEnumList; overload;
function TFhirInvoiceStatusEnumListAsInteger(aSet : TFhirInvoiceStatusEnumList) : Integer; overload;
function IntegerAsTFhirInvoiceStatusEnumList(i : integer) : TFhirInvoiceStatusEnumList; overload;
function TFhirIssueSeverityEnumListAsInteger(aSet : TFhirIssueSeverityEnumList) : Integer; overload;
function IntegerAsTFhirIssueSeverityEnumList(i : integer) : TFhirIssueSeverityEnumList; overload;
function TFhirIssueTypeEnumListAsInteger(aSet : TFhirIssueTypeEnumList) : Integer; overload;
function IntegerAsTFhirIssueTypeEnumList(i : integer) : TFhirIssueTypeEnumList; overload;
function TFhirLinkTypeEnumListAsInteger(aSet : TFhirLinkTypeEnumList) : Integer; overload;
function IntegerAsTFhirLinkTypeEnumList(i : integer) : TFhirLinkTypeEnumList; overload;
function TFhirLinkageTypeEnumListAsInteger(aSet : TFhirLinkageTypeEnumList) : Integer; overload;
function IntegerAsTFhirLinkageTypeEnumList(i : integer) : TFhirLinkageTypeEnumList; overload;
function TFhirListModeEnumListAsInteger(aSet : TFhirListModeEnumList) : Integer; overload;
function IntegerAsTFhirListModeEnumList(i : integer) : TFhirListModeEnumList; overload;
function TFhirListStatusEnumListAsInteger(aSet : TFhirListStatusEnumList) : Integer; overload;
function IntegerAsTFhirListStatusEnumList(i : integer) : TFhirListStatusEnumList; overload;
function TFhirLocationModeEnumListAsInteger(aSet : TFhirLocationModeEnumList) : Integer; overload;
function IntegerAsTFhirLocationModeEnumList(i : integer) : TFhirLocationModeEnumList; overload;
function TFhirLocationStatusEnumListAsInteger(aSet : TFhirLocationStatusEnumList) : Integer; overload;
function IntegerAsTFhirLocationStatusEnumList(i : integer) : TFhirLocationStatusEnumList; overload;
function TFhirMeasureReportStatusEnumListAsInteger(aSet : TFhirMeasureReportStatusEnumList) : Integer; overload;
function IntegerAsTFhirMeasureReportStatusEnumList(i : integer) : TFhirMeasureReportStatusEnumList; overload;
function TFhirMeasureReportTypeEnumListAsInteger(aSet : TFhirMeasureReportTypeEnumList) : Integer; overload;
function IntegerAsTFhirMeasureReportTypeEnumList(i : integer) : TFhirMeasureReportTypeEnumList; overload;
function TFhirMedicationAdministrationStatusCodesEnumListAsInteger(aSet : TFhirMedicationAdministrationStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirMedicationAdministrationStatusCodesEnumList(i : integer) : TFhirMedicationAdministrationStatusCodesEnumList; overload;
function TFhirMedicationDispenseStatusCodesEnumListAsInteger(aSet : TFhirMedicationDispenseStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirMedicationDispenseStatusCodesEnumList(i : integer) : TFhirMedicationDispenseStatusCodesEnumList; overload;
function TFhirMedicationKnowledgeStatusCodesEnumListAsInteger(aSet : TFhirMedicationKnowledgeStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirMedicationKnowledgeStatusCodesEnumList(i : integer) : TFhirMedicationKnowledgeStatusCodesEnumList; overload;
function TFhirMedicationRequestIntentEnumListAsInteger(aSet : TFhirMedicationRequestIntentEnumList) : Integer; overload;
function IntegerAsTFhirMedicationRequestIntentEnumList(i : integer) : TFhirMedicationRequestIntentEnumList; overload;
function TFhirMedicationStatusCodesEnumListAsInteger(aSet : TFhirMedicationStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirMedicationStatusCodesEnumList(i : integer) : TFhirMedicationStatusCodesEnumList; overload;
function TFhirMedicationUsageStatusCodesEnumListAsInteger(aSet : TFhirMedicationUsageStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirMedicationUsageStatusCodesEnumList(i : integer) : TFhirMedicationUsageStatusCodesEnumList; overload;
function TFhirMedicationrequestStatusEnumListAsInteger(aSet : TFhirMedicationrequestStatusEnumList) : Integer; overload;
function IntegerAsTFhirMedicationrequestStatusEnumList(i : integer) : TFhirMedicationrequestStatusEnumList; overload;
function TFhirMessageSignificanceCategoryEnumListAsInteger(aSet : TFhirMessageSignificanceCategoryEnumList) : Integer; overload;
function IntegerAsTFhirMessageSignificanceCategoryEnumList(i : integer) : TFhirMessageSignificanceCategoryEnumList; overload;
function TFhirMessageheaderResponseRequestEnumListAsInteger(aSet : TFhirMessageheaderResponseRequestEnumList) : Integer; overload;
function IntegerAsTFhirMessageheaderResponseRequestEnumList(i : integer) : TFhirMessageheaderResponseRequestEnumList; overload;
function TFhirNameUseEnumListAsInteger(aSet : TFhirNameUseEnumList) : Integer; overload;
function IntegerAsTFhirNameUseEnumList(i : integer) : TFhirNameUseEnumList; overload;
function TFhirNamingSystemIdentifierTypeEnumListAsInteger(aSet : TFhirNamingSystemIdentifierTypeEnumList) : Integer; overload;
function IntegerAsTFhirNamingSystemIdentifierTypeEnumList(i : integer) : TFhirNamingSystemIdentifierTypeEnumList; overload;
function TFhirNamingSystemTypeEnumListAsInteger(aSet : TFhirNamingSystemTypeEnumList) : Integer; overload;
function IntegerAsTFhirNamingSystemTypeEnumList(i : integer) : TFhirNamingSystemTypeEnumList; overload;
function TFhirNarrativeStatusEnumListAsInteger(aSet : TFhirNarrativeStatusEnumList) : Integer; overload;
function IntegerAsTFhirNarrativeStatusEnumList(i : integer) : TFhirNarrativeStatusEnumList; overload;
function TFhirNoteTypeEnumListAsInteger(aSet : TFhirNoteTypeEnumList) : Integer; overload;
function IntegerAsTFhirNoteTypeEnumList(i : integer) : TFhirNoteTypeEnumList; overload;
function TFhirNutritionProductStatusEnumListAsInteger(aSet : TFhirNutritionProductStatusEnumList) : Integer; overload;
function IntegerAsTFhirNutritionProductStatusEnumList(i : integer) : TFhirNutritionProductStatusEnumList; overload;
function TFhirObservationDataTypeEnumListAsInteger(aSet : TFhirObservationDataTypeEnumList) : Integer; overload;
function IntegerAsTFhirObservationDataTypeEnumList(i : integer) : TFhirObservationDataTypeEnumList; overload;
function TFhirObservationRangeCategoryEnumListAsInteger(aSet : TFhirObservationRangeCategoryEnumList) : Integer; overload;
function IntegerAsTFhirObservationRangeCategoryEnumList(i : integer) : TFhirObservationRangeCategoryEnumList; overload;
function TFhirObservationStatusEnumListAsInteger(aSet : TFhirObservationStatusEnumList) : Integer; overload;
function IntegerAsTFhirObservationStatusEnumList(i : integer) : TFhirObservationStatusEnumList; overload;
function TFhirOperationKindEnumListAsInteger(aSet : TFhirOperationKindEnumList) : Integer; overload;
function IntegerAsTFhirOperationKindEnumList(i : integer) : TFhirOperationKindEnumList; overload;
function TFhirOperationParameterUseEnumListAsInteger(aSet : TFhirOperationParameterUseEnumList) : Integer; overload;
function IntegerAsTFhirOperationParameterUseEnumList(i : integer) : TFhirOperationParameterUseEnumList; overload;
function TFhirOrientationTypeEnumListAsInteger(aSet : TFhirOrientationTypeEnumList) : Integer; overload;
function IntegerAsTFhirOrientationTypeEnumList(i : integer) : TFhirOrientationTypeEnumList; overload;
function TFhirParticipantRequiredEnumListAsInteger(aSet : TFhirParticipantRequiredEnumList) : Integer; overload;
function IntegerAsTFhirParticipantRequiredEnumList(i : integer) : TFhirParticipantRequiredEnumList; overload;
function TFhirParticipationStatusEnumListAsInteger(aSet : TFhirParticipationStatusEnumList) : Integer; overload;
function IntegerAsTFhirParticipationStatusEnumList(i : integer) : TFhirParticipationStatusEnumList; overload;
function TFhirPermissionStatusEnumListAsInteger(aSet : TFhirPermissionStatusEnumList) : Integer; overload;
function IntegerAsTFhirPermissionStatusEnumList(i : integer) : TFhirPermissionStatusEnumList; overload;
function TFhirPropertyRepresentationEnumListAsInteger(aSet : TFhirPropertyRepresentationEnumList) : Integer; overload;
function IntegerAsTFhirPropertyRepresentationEnumList(i : integer) : TFhirPropertyRepresentationEnumList; overload;
function TFhirProvenanceEntityRoleEnumListAsInteger(aSet : TFhirProvenanceEntityRoleEnumList) : Integer; overload;
function IntegerAsTFhirProvenanceEntityRoleEnumList(i : integer) : TFhirProvenanceEntityRoleEnumList; overload;
function TFhirPublicationStatusEnumListAsInteger(aSet : TFhirPublicationStatusEnumList) : Integer; overload;
function IntegerAsTFhirPublicationStatusEnumList(i : integer) : TFhirPublicationStatusEnumList; overload;
function TFhirQualityTypeEnumListAsInteger(aSet : TFhirQualityTypeEnumList) : Integer; overload;
function IntegerAsTFhirQualityTypeEnumList(i : integer) : TFhirQualityTypeEnumList; overload;
function TFhirQuantityComparatorEnumListAsInteger(aSet : TFhirQuantityComparatorEnumList) : Integer; overload;
function IntegerAsTFhirQuantityComparatorEnumList(i : integer) : TFhirQuantityComparatorEnumList; overload;
function TFhirQuestionnaireItemOperatorEnumListAsInteger(aSet : TFhirQuestionnaireItemOperatorEnumList) : Integer; overload;
function IntegerAsTFhirQuestionnaireItemOperatorEnumList(i : integer) : TFhirQuestionnaireItemOperatorEnumList; overload;
function TFhirQuestionnaireItemTypeEnumListAsInteger(aSet : TFhirQuestionnaireItemTypeEnumList) : Integer; overload;
function IntegerAsTFhirQuestionnaireItemTypeEnumList(i : integer) : TFhirQuestionnaireItemTypeEnumList; overload;
function TFhirQuestionnaireResponseStatusEnumListAsInteger(aSet : TFhirQuestionnaireResponseStatusEnumList) : Integer; overload;
function IntegerAsTFhirQuestionnaireResponseStatusEnumList(i : integer) : TFhirQuestionnaireResponseStatusEnumList; overload;
function TFhirReferenceHandlingPolicyEnumListAsInteger(aSet : TFhirReferenceHandlingPolicyEnumList) : Integer; overload;
function IntegerAsTFhirReferenceHandlingPolicyEnumList(i : integer) : TFhirReferenceHandlingPolicyEnumList; overload;
function TFhirReferenceVersionRulesEnumListAsInteger(aSet : TFhirReferenceVersionRulesEnumList) : Integer; overload;
function IntegerAsTFhirReferenceVersionRulesEnumList(i : integer) : TFhirReferenceVersionRulesEnumList; overload;
function TFhirRelatedArtifactTypeEnumListAsInteger(aSet : TFhirRelatedArtifactTypeEnumList) : Integer; overload;
function IntegerAsTFhirRelatedArtifactTypeEnumList(i : integer) : TFhirRelatedArtifactTypeEnumList; overload;
function TFhirReportRelationshipTypeEnumListAsInteger(aSet : TFhirReportRelationshipTypeEnumList) : Integer; overload;
function IntegerAsTFhirReportRelationshipTypeEnumList(i : integer) : TFhirReportRelationshipTypeEnumList; overload;
function TFhirRepositoryTypeEnumListAsInteger(aSet : TFhirRepositoryTypeEnumList) : Integer; overload;
function IntegerAsTFhirRepositoryTypeEnumList(i : integer) : TFhirRepositoryTypeEnumList; overload;
function TFhirRequestIntentEnumListAsInteger(aSet : TFhirRequestIntentEnumList) : Integer; overload;
function IntegerAsTFhirRequestIntentEnumList(i : integer) : TFhirRequestIntentEnumList; overload;
function TFhirRequestPriorityEnumListAsInteger(aSet : TFhirRequestPriorityEnumList) : Integer; overload;
function IntegerAsTFhirRequestPriorityEnumList(i : integer) : TFhirRequestPriorityEnumList; overload;
function TFhirRequestResourceTypeEnumListAsInteger(aSet : TFhirRequestResourceTypeEnumList) : Integer; overload;
function IntegerAsTFhirRequestResourceTypeEnumList(i : integer) : TFhirRequestResourceTypeEnumList; overload;
function TFhirRequestStatusEnumListAsInteger(aSet : TFhirRequestStatusEnumList) : Integer; overload;
function IntegerAsTFhirRequestStatusEnumList(i : integer) : TFhirRequestStatusEnumList; overload;
function TFhirResearchStudyStatusEnumListAsInteger(aSet : TFhirResearchStudyStatusEnumList) : Integer; overload;
function IntegerAsTFhirResearchStudyStatusEnumList(i : integer) : TFhirResearchStudyStatusEnumList; overload;
function TFhirResearchSubjectStatusEnumListAsInteger(aSet : TFhirResearchSubjectStatusEnumList) : Integer; overload;
function IntegerAsTFhirResearchSubjectStatusEnumList(i : integer) : TFhirResearchSubjectStatusEnumList; overload;
function TFhirResourceTypesEnumListAsInteger(aSet : TFhirResourceTypesEnumList) : Integer; overload;
function IntegerAsTFhirResourceTypesEnumList(i : integer) : TFhirResourceTypesEnumList; overload;
function TFhirResourceVersionPolicyEnumListAsInteger(aSet : TFhirResourceVersionPolicyEnumList) : Integer; overload;
function IntegerAsTFhirResourceVersionPolicyEnumList(i : integer) : TFhirResourceVersionPolicyEnumList; overload;
function TFhirResponseTypeEnumListAsInteger(aSet : TFhirResponseTypeEnumList) : Integer; overload;
function IntegerAsTFhirResponseTypeEnumList(i : integer) : TFhirResponseTypeEnumList; overload;
function TFhirRestfulCapabilityModeEnumListAsInteger(aSet : TFhirRestfulCapabilityModeEnumList) : Integer; overload;
function IntegerAsTFhirRestfulCapabilityModeEnumList(i : integer) : TFhirRestfulCapabilityModeEnumList; overload;
function TFhirSearchComparatorEnumListAsInteger(aSet : TFhirSearchComparatorEnumList) : Integer; overload;
function IntegerAsTFhirSearchComparatorEnumList(i : integer) : TFhirSearchComparatorEnumList; overload;
function TFhirSearchEntryModeEnumListAsInteger(aSet : TFhirSearchEntryModeEnumList) : Integer; overload;
function IntegerAsTFhirSearchEntryModeEnumList(i : integer) : TFhirSearchEntryModeEnumList; overload;
function TFhirSearchModifierCodeEnumListAsInteger(aSet : TFhirSearchModifierCodeEnumList) : Integer; overload;
function IntegerAsTFhirSearchModifierCodeEnumList(i : integer) : TFhirSearchModifierCodeEnumList; overload;
function TFhirSearchParamTypeEnumListAsInteger(aSet : TFhirSearchParamTypeEnumList) : Integer; overload;
function IntegerAsTFhirSearchParamTypeEnumList(i : integer) : TFhirSearchParamTypeEnumList; overload;
function TFhirSequenceTypeEnumListAsInteger(aSet : TFhirSequenceTypeEnumList) : Integer; overload;
function IntegerAsTFhirSequenceTypeEnumList(i : integer) : TFhirSequenceTypeEnumList; overload;
function TFhirSlicingRulesEnumListAsInteger(aSet : TFhirSlicingRulesEnumList) : Integer; overload;
function IntegerAsTFhirSlicingRulesEnumList(i : integer) : TFhirSlicingRulesEnumList; overload;
function TFhirSlotStatusEnumListAsInteger(aSet : TFhirSlotStatusEnumList) : Integer; overload;
function IntegerAsTFhirSlotStatusEnumList(i : integer) : TFhirSlotStatusEnumList; overload;
function TFhirSortDirectionEnumListAsInteger(aSet : TFhirSortDirectionEnumList) : Integer; overload;
function IntegerAsTFhirSortDirectionEnumList(i : integer) : TFhirSortDirectionEnumList; overload;
function TFhirSpecimenContainedPreferenceEnumListAsInteger(aSet : TFhirSpecimenContainedPreferenceEnumList) : Integer; overload;
function IntegerAsTFhirSpecimenContainedPreferenceEnumList(i : integer) : TFhirSpecimenContainedPreferenceEnumList; overload;
function TFhirSpecimenStatusEnumListAsInteger(aSet : TFhirSpecimenStatusEnumList) : Integer; overload;
function IntegerAsTFhirSpecimenStatusEnumList(i : integer) : TFhirSpecimenStatusEnumList; overload;
function TFhirStatusEnumListAsInteger(aSet : TFhirStatusEnumList) : Integer; overload;
function IntegerAsTFhirStatusEnumList(i : integer) : TFhirStatusEnumList; overload;
function TFhirStrandTypeEnumListAsInteger(aSet : TFhirStrandTypeEnumList) : Integer; overload;
function IntegerAsTFhirStrandTypeEnumList(i : integer) : TFhirStrandTypeEnumList; overload;
function TFhirStructureDefinitionKindEnumListAsInteger(aSet : TFhirStructureDefinitionKindEnumList) : Integer; overload;
function IntegerAsTFhirStructureDefinitionKindEnumList(i : integer) : TFhirStructureDefinitionKindEnumList; overload;
function TFhirStructureMapContextTypeEnumListAsInteger(aSet : TFhirStructureMapContextTypeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapContextTypeEnumList(i : integer) : TFhirStructureMapContextTypeEnumList; overload;
function TFhirStructureMapGroupTypeModeEnumListAsInteger(aSet : TFhirStructureMapGroupTypeModeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapGroupTypeModeEnumList(i : integer) : TFhirStructureMapGroupTypeModeEnumList; overload;
function TFhirStructureMapInputModeEnumListAsInteger(aSet : TFhirStructureMapInputModeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapInputModeEnumList(i : integer) : TFhirStructureMapInputModeEnumList; overload;
function TFhirStructureMapModelModeEnumListAsInteger(aSet : TFhirStructureMapModelModeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapModelModeEnumList(i : integer) : TFhirStructureMapModelModeEnumList; overload;
function TFhirStructureMapSourceListModeEnumListAsInteger(aSet : TFhirStructureMapSourceListModeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapSourceListModeEnumList(i : integer) : TFhirStructureMapSourceListModeEnumList; overload;
function TFhirStructureMapTargetListModeEnumListAsInteger(aSet : TFhirStructureMapTargetListModeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapTargetListModeEnumList(i : integer) : TFhirStructureMapTargetListModeEnumList; overload;
function TFhirStructureMapTransformEnumListAsInteger(aSet : TFhirStructureMapTransformEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapTransformEnumList(i : integer) : TFhirStructureMapTransformEnumList; overload;
function TFhirSubscriptionNotificationTypeEnumListAsInteger(aSet : TFhirSubscriptionNotificationTypeEnumList) : Integer; overload;
function IntegerAsTFhirSubscriptionNotificationTypeEnumList(i : integer) : TFhirSubscriptionNotificationTypeEnumList; overload;
function TFhirSubscriptionPayloadContentEnumListAsInteger(aSet : TFhirSubscriptionPayloadContentEnumList) : Integer; overload;
function IntegerAsTFhirSubscriptionPayloadContentEnumList(i : integer) : TFhirSubscriptionPayloadContentEnumList; overload;
function TFhirSubscriptionSearchModifierEnumListAsInteger(aSet : TFhirSubscriptionSearchModifierEnumList) : Integer; overload;
function IntegerAsTFhirSubscriptionSearchModifierEnumList(i : integer) : TFhirSubscriptionSearchModifierEnumList; overload;
function TFhirSubscriptionStateEnumListAsInteger(aSet : TFhirSubscriptionStateEnumList) : Integer; overload;
function IntegerAsTFhirSubscriptionStateEnumList(i : integer) : TFhirSubscriptionStateEnumList; overload;
function TFhirSupplyDeliveryStatusEnumListAsInteger(aSet : TFhirSupplyDeliveryStatusEnumList) : Integer; overload;
function IntegerAsTFhirSupplyDeliveryStatusEnumList(i : integer) : TFhirSupplyDeliveryStatusEnumList; overload;
function TFhirSupplyRequestStatusEnumListAsInteger(aSet : TFhirSupplyRequestStatusEnumList) : Integer; overload;
function IntegerAsTFhirSupplyRequestStatusEnumList(i : integer) : TFhirSupplyRequestStatusEnumList; overload;
function TFhirSystemRestfulInteractionEnumListAsInteger(aSet : TFhirSystemRestfulInteractionEnumList) : Integer; overload;
function IntegerAsTFhirSystemRestfulInteractionEnumList(i : integer) : TFhirSystemRestfulInteractionEnumList; overload;
function TFhirTaskIntentEnumListAsInteger(aSet : TFhirTaskIntentEnumList) : Integer; overload;
function IntegerAsTFhirTaskIntentEnumList(i : integer) : TFhirTaskIntentEnumList; overload;
function TFhirTaskStatusEnumListAsInteger(aSet : TFhirTaskStatusEnumList) : Integer; overload;
function IntegerAsTFhirTaskStatusEnumList(i : integer) : TFhirTaskStatusEnumList; overload;
function TFhirTestReportActionResultEnumListAsInteger(aSet : TFhirTestReportActionResultEnumList) : Integer; overload;
function IntegerAsTFhirTestReportActionResultEnumList(i : integer) : TFhirTestReportActionResultEnumList; overload;
function TFhirTestReportParticipantTypeEnumListAsInteger(aSet : TFhirTestReportParticipantTypeEnumList) : Integer; overload;
function IntegerAsTFhirTestReportParticipantTypeEnumList(i : integer) : TFhirTestReportParticipantTypeEnumList; overload;
function TFhirTestReportResultEnumListAsInteger(aSet : TFhirTestReportResultEnumList) : Integer; overload;
function IntegerAsTFhirTestReportResultEnumList(i : integer) : TFhirTestReportResultEnumList; overload;
function TFhirTestReportStatusEnumListAsInteger(aSet : TFhirTestReportStatusEnumList) : Integer; overload;
function IntegerAsTFhirTestReportStatusEnumList(i : integer) : TFhirTestReportStatusEnumList; overload;
function TFhirTestScriptRequestMethodCodeEnumListAsInteger(aSet : TFhirTestScriptRequestMethodCodeEnumList) : Integer; overload;
function IntegerAsTFhirTestScriptRequestMethodCodeEnumList(i : integer) : TFhirTestScriptRequestMethodCodeEnumList; overload;
function TFhirTriggerTypeEnumListAsInteger(aSet : TFhirTriggerTypeEnumList) : Integer; overload;
function IntegerAsTFhirTriggerTypeEnumList(i : integer) : TFhirTriggerTypeEnumList; overload;
function TFhirTypeDerivationRuleEnumListAsInteger(aSet : TFhirTypeDerivationRuleEnumList) : Integer; overload;
function IntegerAsTFhirTypeDerivationRuleEnumList(i : integer) : TFhirTypeDerivationRuleEnumList; overload;
function TFhirTypeRestfulInteractionEnumListAsInteger(aSet : TFhirTypeRestfulInteractionEnumList) : Integer; overload;
function IntegerAsTFhirTypeRestfulInteractionEnumList(i : integer) : TFhirTypeRestfulInteractionEnumList; overload;
function TFhirUDIEntryTypeEnumListAsInteger(aSet : TFhirUDIEntryTypeEnumList) : Integer; overload;
function IntegerAsTFhirUDIEntryTypeEnumList(i : integer) : TFhirUDIEntryTypeEnumList; overload;
function TFhirUnitsOfTimeEnumListAsInteger(aSet : TFhirUnitsOfTimeEnumList) : Integer; overload;
function IntegerAsTFhirUnitsOfTimeEnumList(i : integer) : TFhirUnitsOfTimeEnumList; overload;
function TFhirUseEnumListAsInteger(aSet : TFhirUseEnumList) : Integer; overload;
function IntegerAsTFhirUseEnumList(i : integer) : TFhirUseEnumList; overload;
function TFhirVisionBaseEnumListAsInteger(aSet : TFhirVisionBaseEnumList) : Integer; overload;
function IntegerAsTFhirVisionBaseEnumList(i : integer) : TFhirVisionBaseEnumList; overload;
function TFhirVisionEyesEnumListAsInteger(aSet : TFhirVisionEyesEnumList) : Integer; overload;
function IntegerAsTFhirVisionEyesEnumList(i : integer) : TFhirVisionEyesEnumList; overload;
function TFhirXPathUsageTypeEnumListAsInteger(aSet : TFhirXPathUsageTypeEnumList) : Integer; overload;
function IntegerAsTFhirXPathUsageTypeEnumList(i : integer) : TFhirXPathUsageTypeEnumList; overload;

implementation

function TFhirAccountStatusEnumListAsInteger(aSet : TFhirAccountStatusEnumList) : Integer;
var
  a : TFhirAccountStatusEnum;
begin
  result := 0;
  for a := low(TFhirAccountStatusEnum) to high(TFhirAccountStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAccountStatusEnumList(i : Integer) : TFhirAccountStatusEnumList;
var
  aLoop : TFhirAccountStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirAccountStatusEnum) to high(TFhirAccountStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionCardinalityBehaviorEnumListAsInteger(aSet : TFhirActionCardinalityBehaviorEnumList) : Integer;
var
  a : TFhirActionCardinalityBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirActionCardinalityBehaviorEnum) to high(TFhirActionCardinalityBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionCardinalityBehaviorEnumList(i : Integer) : TFhirActionCardinalityBehaviorEnumList;
var
  aLoop : TFhirActionCardinalityBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirActionCardinalityBehaviorEnum) to high(TFhirActionCardinalityBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionConditionKindEnumListAsInteger(aSet : TFhirActionConditionKindEnumList) : Integer;
var
  a : TFhirActionConditionKindEnum;
begin
  result := 0;
  for a := low(TFhirActionConditionKindEnum) to high(TFhirActionConditionKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionConditionKindEnumList(i : Integer) : TFhirActionConditionKindEnumList;
var
  aLoop : TFhirActionConditionKindEnum;
begin
  result := [];
  for aLoop := low(TFhirActionConditionKindEnum) to high(TFhirActionConditionKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionGroupingBehaviorEnumListAsInteger(aSet : TFhirActionGroupingBehaviorEnumList) : Integer;
var
  a : TFhirActionGroupingBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirActionGroupingBehaviorEnum) to high(TFhirActionGroupingBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionGroupingBehaviorEnumList(i : Integer) : TFhirActionGroupingBehaviorEnumList;
var
  aLoop : TFhirActionGroupingBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirActionGroupingBehaviorEnum) to high(TFhirActionGroupingBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionParticipantTypeEnumListAsInteger(aSet : TFhirActionParticipantTypeEnumList) : Integer;
var
  a : TFhirActionParticipantTypeEnum;
begin
  result := 0;
  for a := low(TFhirActionParticipantTypeEnum) to high(TFhirActionParticipantTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionParticipantTypeEnumList(i : Integer) : TFhirActionParticipantTypeEnumList;
var
  aLoop : TFhirActionParticipantTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirActionParticipantTypeEnum) to high(TFhirActionParticipantTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionPrecheckBehaviorEnumListAsInteger(aSet : TFhirActionPrecheckBehaviorEnumList) : Integer;
var
  a : TFhirActionPrecheckBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirActionPrecheckBehaviorEnum) to high(TFhirActionPrecheckBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionPrecheckBehaviorEnumList(i : Integer) : TFhirActionPrecheckBehaviorEnumList;
var
  aLoop : TFhirActionPrecheckBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirActionPrecheckBehaviorEnum) to high(TFhirActionPrecheckBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionRelationshipTypeEnumListAsInteger(aSet : TFhirActionRelationshipTypeEnumList) : Integer;
var
  a : TFhirActionRelationshipTypeEnum;
begin
  result := 0;
  for a := low(TFhirActionRelationshipTypeEnum) to high(TFhirActionRelationshipTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionRelationshipTypeEnumList(i : Integer) : TFhirActionRelationshipTypeEnumList;
var
  aLoop : TFhirActionRelationshipTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirActionRelationshipTypeEnum) to high(TFhirActionRelationshipTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionRequiredBehaviorEnumListAsInteger(aSet : TFhirActionRequiredBehaviorEnumList) : Integer;
var
  a : TFhirActionRequiredBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirActionRequiredBehaviorEnum) to high(TFhirActionRequiredBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionRequiredBehaviorEnumList(i : Integer) : TFhirActionRequiredBehaviorEnumList;
var
  aLoop : TFhirActionRequiredBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirActionRequiredBehaviorEnum) to high(TFhirActionRequiredBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionSelectionBehaviorEnumListAsInteger(aSet : TFhirActionSelectionBehaviorEnumList) : Integer;
var
  a : TFhirActionSelectionBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirActionSelectionBehaviorEnum) to high(TFhirActionSelectionBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionSelectionBehaviorEnumList(i : Integer) : TFhirActionSelectionBehaviorEnumList;
var
  aLoop : TFhirActionSelectionBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirActionSelectionBehaviorEnum) to high(TFhirActionSelectionBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAddressTypeEnumListAsInteger(aSet : TFhirAddressTypeEnumList) : Integer;
var
  a : TFhirAddressTypeEnum;
begin
  result := 0;
  for a := low(TFhirAddressTypeEnum) to high(TFhirAddressTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAddressTypeEnumList(i : Integer) : TFhirAddressTypeEnumList;
var
  aLoop : TFhirAddressTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAddressTypeEnum) to high(TFhirAddressTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAddressUseEnumListAsInteger(aSet : TFhirAddressUseEnumList) : Integer;
var
  a : TFhirAddressUseEnum;
begin
  result := 0;
  for a := low(TFhirAddressUseEnum) to high(TFhirAddressUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAddressUseEnumList(i : Integer) : TFhirAddressUseEnumList;
var
  aLoop : TFhirAddressUseEnum;
begin
  result := [];
  for aLoop := low(TFhirAddressUseEnum) to high(TFhirAddressUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAdministrativeGenderEnumListAsInteger(aSet : TFhirAdministrativeGenderEnumList) : Integer;
var
  a : TFhirAdministrativeGenderEnum;
begin
  result := 0;
  for a := low(TFhirAdministrativeGenderEnum) to high(TFhirAdministrativeGenderEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAdministrativeGenderEnumList(i : Integer) : TFhirAdministrativeGenderEnumList;
var
  aLoop : TFhirAdministrativeGenderEnum;
begin
  result := [];
  for aLoop := low(TFhirAdministrativeGenderEnum) to high(TFhirAdministrativeGenderEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAdverseEventActualityEnumListAsInteger(aSet : TFhirAdverseEventActualityEnumList) : Integer;
var
  a : TFhirAdverseEventActualityEnum;
begin
  result := 0;
  for a := low(TFhirAdverseEventActualityEnum) to high(TFhirAdverseEventActualityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAdverseEventActualityEnumList(i : Integer) : TFhirAdverseEventActualityEnumList;
var
  aLoop : TFhirAdverseEventActualityEnum;
begin
  result := [];
  for aLoop := low(TFhirAdverseEventActualityEnum) to high(TFhirAdverseEventActualityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAdverseEventStatusEnumListAsInteger(aSet : TFhirAdverseEventStatusEnumList) : Integer;
var
  a : TFhirAdverseEventStatusEnum;
begin
  result := 0;
  for a := low(TFhirAdverseEventStatusEnum) to high(TFhirAdverseEventStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAdverseEventStatusEnumList(i : Integer) : TFhirAdverseEventStatusEnumList;
var
  aLoop : TFhirAdverseEventStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirAdverseEventStatusEnum) to high(TFhirAdverseEventStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAggregationModeEnumListAsInteger(aSet : TFhirAggregationModeEnumList) : Integer;
var
  a : TFhirAggregationModeEnum;
begin
  result := 0;
  for a := low(TFhirAggregationModeEnum) to high(TFhirAggregationModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAggregationModeEnumList(i : Integer) : TFhirAggregationModeEnumList;
var
  aLoop : TFhirAggregationModeEnum;
begin
  result := [];
  for aLoop := low(TFhirAggregationModeEnum) to high(TFhirAggregationModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAllTypesEnumListAsInteger(aSet : TFhirAllTypesEnumList) : Integer;
var
  a : TFhirAllTypesEnum;
begin
  result := 0;
  for a := low(TFhirAllTypesEnum) to high(TFhirAllTypesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllTypesEnumList(i : Integer) : TFhirAllTypesEnumList;
var
  aLoop : TFhirAllTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirAllTypesEnum) to high(TFhirAllTypesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAllergyIntoleranceCategoryEnumListAsInteger(aSet : TFhirAllergyIntoleranceCategoryEnumList) : Integer;
var
  a : TFhirAllergyIntoleranceCategoryEnum;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceCategoryEnum) to high(TFhirAllergyIntoleranceCategoryEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceCategoryEnumList(i : Integer) : TFhirAllergyIntoleranceCategoryEnumList;
var
  aLoop : TFhirAllergyIntoleranceCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceCategoryEnum) to high(TFhirAllergyIntoleranceCategoryEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAllergyIntoleranceCriticalityEnumListAsInteger(aSet : TFhirAllergyIntoleranceCriticalityEnumList) : Integer;
var
  a : TFhirAllergyIntoleranceCriticalityEnum;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceCriticalityEnum) to high(TFhirAllergyIntoleranceCriticalityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceCriticalityEnumList(i : Integer) : TFhirAllergyIntoleranceCriticalityEnumList;
var
  aLoop : TFhirAllergyIntoleranceCriticalityEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceCriticalityEnum) to high(TFhirAllergyIntoleranceCriticalityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAllergyIntoleranceSeverityEnumListAsInteger(aSet : TFhirAllergyIntoleranceSeverityEnumList) : Integer;
var
  a : TFhirAllergyIntoleranceSeverityEnum;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceSeverityEnum) to high(TFhirAllergyIntoleranceSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceSeverityEnumList(i : Integer) : TFhirAllergyIntoleranceSeverityEnumList;
var
  aLoop : TFhirAllergyIntoleranceSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceSeverityEnum) to high(TFhirAllergyIntoleranceSeverityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAllergyIntoleranceTypeEnumListAsInteger(aSet : TFhirAllergyIntoleranceTypeEnumList) : Integer;
var
  a : TFhirAllergyIntoleranceTypeEnum;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceTypeEnum) to high(TFhirAllergyIntoleranceTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceTypeEnumList(i : Integer) : TFhirAllergyIntoleranceTypeEnumList;
var
  aLoop : TFhirAllergyIntoleranceTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceTypeEnum) to high(TFhirAllergyIntoleranceTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAppointmentStatusEnumListAsInteger(aSet : TFhirAppointmentStatusEnumList) : Integer;
var
  a : TFhirAppointmentStatusEnum;
begin
  result := 0;
  for a := low(TFhirAppointmentStatusEnum) to high(TFhirAppointmentStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAppointmentStatusEnumList(i : Integer) : TFhirAppointmentStatusEnumList;
var
  aLoop : TFhirAppointmentStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirAppointmentStatusEnum) to high(TFhirAppointmentStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAssertionDirectionTypeEnumListAsInteger(aSet : TFhirAssertionDirectionTypeEnumList) : Integer;
var
  a : TFhirAssertionDirectionTypeEnum;
begin
  result := 0;
  for a := low(TFhirAssertionDirectionTypeEnum) to high(TFhirAssertionDirectionTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertionDirectionTypeEnumList(i : Integer) : TFhirAssertionDirectionTypeEnumList;
var
  aLoop : TFhirAssertionDirectionTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAssertionDirectionTypeEnum) to high(TFhirAssertionDirectionTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAssertionOperatorTypeEnumListAsInteger(aSet : TFhirAssertionOperatorTypeEnumList) : Integer;
var
  a : TFhirAssertionOperatorTypeEnum;
begin
  result := 0;
  for a := low(TFhirAssertionOperatorTypeEnum) to high(TFhirAssertionOperatorTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertionOperatorTypeEnumList(i : Integer) : TFhirAssertionOperatorTypeEnumList;
var
  aLoop : TFhirAssertionOperatorTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAssertionOperatorTypeEnum) to high(TFhirAssertionOperatorTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAssertionResponseTypesEnumListAsInteger(aSet : TFhirAssertionResponseTypesEnumList) : Integer;
var
  a : TFhirAssertionResponseTypesEnum;
begin
  result := 0;
  for a := low(TFhirAssertionResponseTypesEnum) to high(TFhirAssertionResponseTypesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertionResponseTypesEnumList(i : Integer) : TFhirAssertionResponseTypesEnumList;
var
  aLoop : TFhirAssertionResponseTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirAssertionResponseTypesEnum) to high(TFhirAssertionResponseTypesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAuditEventActionEnumListAsInteger(aSet : TFhirAuditEventActionEnumList) : Integer;
var
  a : TFhirAuditEventActionEnum;
begin
  result := 0;
  for a := low(TFhirAuditEventActionEnum) to high(TFhirAuditEventActionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAuditEventActionEnumList(i : Integer) : TFhirAuditEventActionEnumList;
var
  aLoop : TFhirAuditEventActionEnum;
begin
  result := [];
  for aLoop := low(TFhirAuditEventActionEnum) to high(TFhirAuditEventActionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAuditEventAgentNetworkTypeEnumListAsInteger(aSet : TFhirAuditEventAgentNetworkTypeEnumList) : Integer;
var
  a : TFhirAuditEventAgentNetworkTypeEnum;
begin
  result := 0;
  for a := low(TFhirAuditEventAgentNetworkTypeEnum) to high(TFhirAuditEventAgentNetworkTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAuditEventAgentNetworkTypeEnumList(i : Integer) : TFhirAuditEventAgentNetworkTypeEnumList;
var
  aLoop : TFhirAuditEventAgentNetworkTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAuditEventAgentNetworkTypeEnum) to high(TFhirAuditEventAgentNetworkTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAuditEventSeverityEnumListAsInteger(aSet : TFhirAuditEventSeverityEnumList) : Integer;
var
  a : TFhirAuditEventSeverityEnum;
begin
  result := 0;
  for a := low(TFhirAuditEventSeverityEnum) to high(TFhirAuditEventSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAuditEventSeverityEnumList(i : Integer) : TFhirAuditEventSeverityEnumList;
var
  aLoop : TFhirAuditEventSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirAuditEventSeverityEnum) to high(TFhirAuditEventSeverityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirBindingStrengthEnumListAsInteger(aSet : TFhirBindingStrengthEnumList) : Integer;
var
  a : TFhirBindingStrengthEnum;
begin
  result := 0;
  for a := low(TFhirBindingStrengthEnum) to high(TFhirBindingStrengthEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBindingStrengthEnumList(i : Integer) : TFhirBindingStrengthEnumList;
var
  aLoop : TFhirBindingStrengthEnum;
begin
  result := [];
  for aLoop := low(TFhirBindingStrengthEnum) to high(TFhirBindingStrengthEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirBiologicallyDerivedProductCategoryEnumListAsInteger(aSet : TFhirBiologicallyDerivedProductCategoryEnumList) : Integer;
var
  a : TFhirBiologicallyDerivedProductCategoryEnum;
begin
  result := 0;
  for a := low(TFhirBiologicallyDerivedProductCategoryEnum) to high(TFhirBiologicallyDerivedProductCategoryEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBiologicallyDerivedProductCategoryEnumList(i : Integer) : TFhirBiologicallyDerivedProductCategoryEnumList;
var
  aLoop : TFhirBiologicallyDerivedProductCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirBiologicallyDerivedProductCategoryEnum) to high(TFhirBiologicallyDerivedProductCategoryEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirBiologicallyDerivedProductStatusEnumListAsInteger(aSet : TFhirBiologicallyDerivedProductStatusEnumList) : Integer;
var
  a : TFhirBiologicallyDerivedProductStatusEnum;
begin
  result := 0;
  for a := low(TFhirBiologicallyDerivedProductStatusEnum) to high(TFhirBiologicallyDerivedProductStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBiologicallyDerivedProductStatusEnumList(i : Integer) : TFhirBiologicallyDerivedProductStatusEnumList;
var
  aLoop : TFhirBiologicallyDerivedProductStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirBiologicallyDerivedProductStatusEnum) to high(TFhirBiologicallyDerivedProductStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirBiologicallyDerivedProductStorageScaleEnumListAsInteger(aSet : TFhirBiologicallyDerivedProductStorageScaleEnumList) : Integer;
var
  a : TFhirBiologicallyDerivedProductStorageScaleEnum;
begin
  result := 0;
  for a := low(TFhirBiologicallyDerivedProductStorageScaleEnum) to high(TFhirBiologicallyDerivedProductStorageScaleEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBiologicallyDerivedProductStorageScaleEnumList(i : Integer) : TFhirBiologicallyDerivedProductStorageScaleEnumList;
var
  aLoop : TFhirBiologicallyDerivedProductStorageScaleEnum;
begin
  result := [];
  for aLoop := low(TFhirBiologicallyDerivedProductStorageScaleEnum) to high(TFhirBiologicallyDerivedProductStorageScaleEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirBundleTypeEnumListAsInteger(aSet : TFhirBundleTypeEnumList) : Integer;
var
  a : TFhirBundleTypeEnum;
begin
  result := 0;
  for a := low(TFhirBundleTypeEnum) to high(TFhirBundleTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBundleTypeEnumList(i : Integer) : TFhirBundleTypeEnumList;
var
  aLoop : TFhirBundleTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirBundleTypeEnum) to high(TFhirBundleTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCapabilityStatementKindEnumListAsInteger(aSet : TFhirCapabilityStatementKindEnumList) : Integer;
var
  a : TFhirCapabilityStatementKindEnum;
begin
  result := 0;
  for a := low(TFhirCapabilityStatementKindEnum) to high(TFhirCapabilityStatementKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCapabilityStatementKindEnumList(i : Integer) : TFhirCapabilityStatementKindEnumList;
var
  aLoop : TFhirCapabilityStatementKindEnum;
begin
  result := [];
  for aLoop := low(TFhirCapabilityStatementKindEnum) to high(TFhirCapabilityStatementKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCarePlanActivityKindEnumListAsInteger(aSet : TFhirCarePlanActivityKindEnumList) : Integer;
var
  a : TFhirCarePlanActivityKindEnum;
begin
  result := 0;
  for a := low(TFhirCarePlanActivityKindEnum) to high(TFhirCarePlanActivityKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanActivityKindEnumList(i : Integer) : TFhirCarePlanActivityKindEnumList;
var
  aLoop : TFhirCarePlanActivityKindEnum;
begin
  result := [];
  for aLoop := low(TFhirCarePlanActivityKindEnum) to high(TFhirCarePlanActivityKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCarePlanActivityStatusEnumListAsInteger(aSet : TFhirCarePlanActivityStatusEnumList) : Integer;
var
  a : TFhirCarePlanActivityStatusEnum;
begin
  result := 0;
  for a := low(TFhirCarePlanActivityStatusEnum) to high(TFhirCarePlanActivityStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanActivityStatusEnumList(i : Integer) : TFhirCarePlanActivityStatusEnumList;
var
  aLoop : TFhirCarePlanActivityStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCarePlanActivityStatusEnum) to high(TFhirCarePlanActivityStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCarePlanIntentEnumListAsInteger(aSet : TFhirCarePlanIntentEnumList) : Integer;
var
  a : TFhirCarePlanIntentEnum;
begin
  result := 0;
  for a := low(TFhirCarePlanIntentEnum) to high(TFhirCarePlanIntentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanIntentEnumList(i : Integer) : TFhirCarePlanIntentEnumList;
var
  aLoop : TFhirCarePlanIntentEnum;
begin
  result := [];
  for aLoop := low(TFhirCarePlanIntentEnum) to high(TFhirCarePlanIntentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCareTeamStatusEnumListAsInteger(aSet : TFhirCareTeamStatusEnumList) : Integer;
var
  a : TFhirCareTeamStatusEnum;
begin
  result := 0;
  for a := low(TFhirCareTeamStatusEnum) to high(TFhirCareTeamStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCareTeamStatusEnumList(i : Integer) : TFhirCareTeamStatusEnumList;
var
  aLoop : TFhirCareTeamStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCareTeamStatusEnum) to high(TFhirCareTeamStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCatalogEntryRelationTypeEnumListAsInteger(aSet : TFhirCatalogEntryRelationTypeEnumList) : Integer;
var
  a : TFhirCatalogEntryRelationTypeEnum;
begin
  result := 0;
  for a := low(TFhirCatalogEntryRelationTypeEnum) to high(TFhirCatalogEntryRelationTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCatalogEntryRelationTypeEnumList(i : Integer) : TFhirCatalogEntryRelationTypeEnumList;
var
  aLoop : TFhirCatalogEntryRelationTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirCatalogEntryRelationTypeEnum) to high(TFhirCatalogEntryRelationTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCatalogEntryStatusEnumListAsInteger(aSet : TFhirCatalogEntryStatusEnumList) : Integer;
var
  a : TFhirCatalogEntryStatusEnum;
begin
  result := 0;
  for a := low(TFhirCatalogEntryStatusEnum) to high(TFhirCatalogEntryStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCatalogEntryStatusEnumList(i : Integer) : TFhirCatalogEntryStatusEnumList;
var
  aLoop : TFhirCatalogEntryStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCatalogEntryStatusEnum) to high(TFhirCatalogEntryStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCatalogEntryTypeEnumListAsInteger(aSet : TFhirCatalogEntryTypeEnumList) : Integer;
var
  a : TFhirCatalogEntryTypeEnum;
begin
  result := 0;
  for a := low(TFhirCatalogEntryTypeEnum) to high(TFhirCatalogEntryTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCatalogEntryTypeEnumList(i : Integer) : TFhirCatalogEntryTypeEnumList;
var
  aLoop : TFhirCatalogEntryTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirCatalogEntryTypeEnum) to high(TFhirCatalogEntryTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCharacteristicCombinationEnumListAsInteger(aSet : TFhirCharacteristicCombinationEnumList) : Integer;
var
  a : TFhirCharacteristicCombinationEnum;
begin
  result := 0;
  for a := low(TFhirCharacteristicCombinationEnum) to high(TFhirCharacteristicCombinationEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCharacteristicCombinationEnumList(i : Integer) : TFhirCharacteristicCombinationEnumList;
var
  aLoop : TFhirCharacteristicCombinationEnum;
begin
  result := [];
  for aLoop := low(TFhirCharacteristicCombinationEnum) to high(TFhirCharacteristicCombinationEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirChargeItemStatusEnumListAsInteger(aSet : TFhirChargeItemStatusEnumList) : Integer;
var
  a : TFhirChargeItemStatusEnum;
begin
  result := 0;
  for a := low(TFhirChargeItemStatusEnum) to high(TFhirChargeItemStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirChargeItemStatusEnumList(i : Integer) : TFhirChargeItemStatusEnumList;
var
  aLoop : TFhirChargeItemStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirChargeItemStatusEnum) to high(TFhirChargeItemStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirClaimProcessingCodesEnumListAsInteger(aSet : TFhirClaimProcessingCodesEnumList) : Integer;
var
  a : TFhirClaimProcessingCodesEnum;
begin
  result := 0;
  for a := low(TFhirClaimProcessingCodesEnum) to high(TFhirClaimProcessingCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirClaimProcessingCodesEnumList(i : Integer) : TFhirClaimProcessingCodesEnumList;
var
  aLoop : TFhirClaimProcessingCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirClaimProcessingCodesEnum) to high(TFhirClaimProcessingCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirClinicalUseIssueTypeEnumListAsInteger(aSet : TFhirClinicalUseIssueTypeEnumList) : Integer;
var
  a : TFhirClinicalUseIssueTypeEnum;
begin
  result := 0;
  for a := low(TFhirClinicalUseIssueTypeEnum) to high(TFhirClinicalUseIssueTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirClinicalUseIssueTypeEnumList(i : Integer) : TFhirClinicalUseIssueTypeEnumList;
var
  aLoop : TFhirClinicalUseIssueTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirClinicalUseIssueTypeEnum) to high(TFhirClinicalUseIssueTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCodeSearchSupportEnumListAsInteger(aSet : TFhirCodeSearchSupportEnumList) : Integer;
var
  a : TFhirCodeSearchSupportEnum;
begin
  result := 0;
  for a := low(TFhirCodeSearchSupportEnum) to high(TFhirCodeSearchSupportEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCodeSearchSupportEnumList(i : Integer) : TFhirCodeSearchSupportEnumList;
var
  aLoop : TFhirCodeSearchSupportEnum;
begin
  result := [];
  for aLoop := low(TFhirCodeSearchSupportEnum) to high(TFhirCodeSearchSupportEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCodeSystemContentModeEnumListAsInteger(aSet : TFhirCodeSystemContentModeEnumList) : Integer;
var
  a : TFhirCodeSystemContentModeEnum;
begin
  result := 0;
  for a := low(TFhirCodeSystemContentModeEnum) to high(TFhirCodeSystemContentModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCodeSystemContentModeEnumList(i : Integer) : TFhirCodeSystemContentModeEnumList;
var
  aLoop : TFhirCodeSystemContentModeEnum;
begin
  result := [];
  for aLoop := low(TFhirCodeSystemContentModeEnum) to high(TFhirCodeSystemContentModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCodeSystemHierarchyMeaningEnumListAsInteger(aSet : TFhirCodeSystemHierarchyMeaningEnumList) : Integer;
var
  a : TFhirCodeSystemHierarchyMeaningEnum;
begin
  result := 0;
  for a := low(TFhirCodeSystemHierarchyMeaningEnum) to high(TFhirCodeSystemHierarchyMeaningEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCodeSystemHierarchyMeaningEnumList(i : Integer) : TFhirCodeSystemHierarchyMeaningEnumList;
var
  aLoop : TFhirCodeSystemHierarchyMeaningEnum;
begin
  result := [];
  for aLoop := low(TFhirCodeSystemHierarchyMeaningEnum) to high(TFhirCodeSystemHierarchyMeaningEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCompartmentTypeEnumListAsInteger(aSet : TFhirCompartmentTypeEnumList) : Integer;
var
  a : TFhirCompartmentTypeEnum;
begin
  result := 0;
  for a := low(TFhirCompartmentTypeEnum) to high(TFhirCompartmentTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCompartmentTypeEnumList(i : Integer) : TFhirCompartmentTypeEnumList;
var
  aLoop : TFhirCompartmentTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirCompartmentTypeEnum) to high(TFhirCompartmentTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCompositionAttestationModeEnumListAsInteger(aSet : TFhirCompositionAttestationModeEnumList) : Integer;
var
  a : TFhirCompositionAttestationModeEnum;
begin
  result := 0;
  for a := low(TFhirCompositionAttestationModeEnum) to high(TFhirCompositionAttestationModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCompositionAttestationModeEnumList(i : Integer) : TFhirCompositionAttestationModeEnumList;
var
  aLoop : TFhirCompositionAttestationModeEnum;
begin
  result := [];
  for aLoop := low(TFhirCompositionAttestationModeEnum) to high(TFhirCompositionAttestationModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCompositionStatusEnumListAsInteger(aSet : TFhirCompositionStatusEnumList) : Integer;
var
  a : TFhirCompositionStatusEnum;
begin
  result := 0;
  for a := low(TFhirCompositionStatusEnum) to high(TFhirCompositionStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCompositionStatusEnumList(i : Integer) : TFhirCompositionStatusEnumList;
var
  aLoop : TFhirCompositionStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCompositionStatusEnum) to high(TFhirCompositionStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConceptMapGroupUnmappedModeEnumListAsInteger(aSet : TFhirConceptMapGroupUnmappedModeEnumList) : Integer;
var
  a : TFhirConceptMapGroupUnmappedModeEnum;
begin
  result := 0;
  for a := low(TFhirConceptMapGroupUnmappedModeEnum) to high(TFhirConceptMapGroupUnmappedModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConceptMapGroupUnmappedModeEnumList(i : Integer) : TFhirConceptMapGroupUnmappedModeEnumList;
var
  aLoop : TFhirConceptMapGroupUnmappedModeEnum;
begin
  result := [];
  for aLoop := low(TFhirConceptMapGroupUnmappedModeEnum) to high(TFhirConceptMapGroupUnmappedModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConceptMapRelationshipEnumListAsInteger(aSet : TFhirConceptMapRelationshipEnumList) : Integer;
var
  a : TFhirConceptMapRelationshipEnum;
begin
  result := 0;
  for a := low(TFhirConceptMapRelationshipEnum) to high(TFhirConceptMapRelationshipEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConceptMapRelationshipEnumList(i : Integer) : TFhirConceptMapRelationshipEnumList;
var
  aLoop : TFhirConceptMapRelationshipEnum;
begin
  result := [];
  for aLoop := low(TFhirConceptMapRelationshipEnum) to high(TFhirConceptMapRelationshipEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConceptPropertyTypeEnumListAsInteger(aSet : TFhirConceptPropertyTypeEnumList) : Integer;
var
  a : TFhirConceptPropertyTypeEnum;
begin
  result := 0;
  for a := low(TFhirConceptPropertyTypeEnum) to high(TFhirConceptPropertyTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConceptPropertyTypeEnumList(i : Integer) : TFhirConceptPropertyTypeEnumList;
var
  aLoop : TFhirConceptPropertyTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirConceptPropertyTypeEnum) to high(TFhirConceptPropertyTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConditionPreconditionTypeEnumListAsInteger(aSet : TFhirConditionPreconditionTypeEnumList) : Integer;
var
  a : TFhirConditionPreconditionTypeEnum;
begin
  result := 0;
  for a := low(TFhirConditionPreconditionTypeEnum) to high(TFhirConditionPreconditionTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionPreconditionTypeEnumList(i : Integer) : TFhirConditionPreconditionTypeEnumList;
var
  aLoop : TFhirConditionPreconditionTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirConditionPreconditionTypeEnum) to high(TFhirConditionPreconditionTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConditionQuestionnairePurposeEnumListAsInteger(aSet : TFhirConditionQuestionnairePurposeEnumList) : Integer;
var
  a : TFhirConditionQuestionnairePurposeEnum;
begin
  result := 0;
  for a := low(TFhirConditionQuestionnairePurposeEnum) to high(TFhirConditionQuestionnairePurposeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionQuestionnairePurposeEnumList(i : Integer) : TFhirConditionQuestionnairePurposeEnumList;
var
  aLoop : TFhirConditionQuestionnairePurposeEnum;
begin
  result := [];
  for aLoop := low(TFhirConditionQuestionnairePurposeEnum) to high(TFhirConditionQuestionnairePurposeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConditionalDeleteStatusEnumListAsInteger(aSet : TFhirConditionalDeleteStatusEnumList) : Integer;
var
  a : TFhirConditionalDeleteStatusEnum;
begin
  result := 0;
  for a := low(TFhirConditionalDeleteStatusEnum) to high(TFhirConditionalDeleteStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionalDeleteStatusEnumList(i : Integer) : TFhirConditionalDeleteStatusEnumList;
var
  aLoop : TFhirConditionalDeleteStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirConditionalDeleteStatusEnum) to high(TFhirConditionalDeleteStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConditionalReadStatusEnumListAsInteger(aSet : TFhirConditionalReadStatusEnumList) : Integer;
var
  a : TFhirConditionalReadStatusEnum;
begin
  result := 0;
  for a := low(TFhirConditionalReadStatusEnum) to high(TFhirConditionalReadStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionalReadStatusEnumList(i : Integer) : TFhirConditionalReadStatusEnumList;
var
  aLoop : TFhirConditionalReadStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirConditionalReadStatusEnum) to high(TFhirConditionalReadStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConsentDataMeaningEnumListAsInteger(aSet : TFhirConsentDataMeaningEnumList) : Integer;
var
  a : TFhirConsentDataMeaningEnum;
begin
  result := 0;
  for a := low(TFhirConsentDataMeaningEnum) to high(TFhirConsentDataMeaningEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConsentDataMeaningEnumList(i : Integer) : TFhirConsentDataMeaningEnumList;
var
  aLoop : TFhirConsentDataMeaningEnum;
begin
  result := [];
  for aLoop := low(TFhirConsentDataMeaningEnum) to high(TFhirConsentDataMeaningEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConsentProvisionTypeEnumListAsInteger(aSet : TFhirConsentProvisionTypeEnumList) : Integer;
var
  a : TFhirConsentProvisionTypeEnum;
begin
  result := 0;
  for a := low(TFhirConsentProvisionTypeEnum) to high(TFhirConsentProvisionTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConsentProvisionTypeEnumList(i : Integer) : TFhirConsentProvisionTypeEnumList;
var
  aLoop : TFhirConsentProvisionTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirConsentProvisionTypeEnum) to high(TFhirConsentProvisionTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConsentStateEnumListAsInteger(aSet : TFhirConsentStateEnumList) : Integer;
var
  a : TFhirConsentStateEnum;
begin
  result := 0;
  for a := low(TFhirConsentStateEnum) to high(TFhirConsentStateEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConsentStateEnumList(i : Integer) : TFhirConsentStateEnumList;
var
  aLoop : TFhirConsentStateEnum;
begin
  result := [];
  for aLoop := low(TFhirConsentStateEnum) to high(TFhirConsentStateEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConstraintSeverityEnumListAsInteger(aSet : TFhirConstraintSeverityEnumList) : Integer;
var
  a : TFhirConstraintSeverityEnum;
begin
  result := 0;
  for a := low(TFhirConstraintSeverityEnum) to high(TFhirConstraintSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConstraintSeverityEnumList(i : Integer) : TFhirConstraintSeverityEnumList;
var
  aLoop : TFhirConstraintSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirConstraintSeverityEnum) to high(TFhirConstraintSeverityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirContactPointSystemEnumListAsInteger(aSet : TFhirContactPointSystemEnumList) : Integer;
var
  a : TFhirContactPointSystemEnum;
begin
  result := 0;
  for a := low(TFhirContactPointSystemEnum) to high(TFhirContactPointSystemEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContactPointSystemEnumList(i : Integer) : TFhirContactPointSystemEnumList;
var
  aLoop : TFhirContactPointSystemEnum;
begin
  result := [];
  for aLoop := low(TFhirContactPointSystemEnum) to high(TFhirContactPointSystemEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirContactPointUseEnumListAsInteger(aSet : TFhirContactPointUseEnumList) : Integer;
var
  a : TFhirContactPointUseEnum;
begin
  result := 0;
  for a := low(TFhirContactPointUseEnum) to high(TFhirContactPointUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContactPointUseEnumList(i : Integer) : TFhirContactPointUseEnumList;
var
  aLoop : TFhirContactPointUseEnum;
begin
  result := [];
  for aLoop := low(TFhirContactPointUseEnum) to high(TFhirContactPointUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirContractResourcePublicationStatusCodesEnumListAsInteger(aSet : TFhirContractResourcePublicationStatusCodesEnumList) : Integer;
var
  a : TFhirContractResourcePublicationStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirContractResourcePublicationStatusCodesEnum) to high(TFhirContractResourcePublicationStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContractResourcePublicationStatusCodesEnumList(i : Integer) : TFhirContractResourcePublicationStatusCodesEnumList;
var
  aLoop : TFhirContractResourcePublicationStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirContractResourcePublicationStatusCodesEnum) to high(TFhirContractResourcePublicationStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirContractResourceStatusCodesEnumListAsInteger(aSet : TFhirContractResourceStatusCodesEnumList) : Integer;
var
  a : TFhirContractResourceStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirContractResourceStatusCodesEnum) to high(TFhirContractResourceStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContractResourceStatusCodesEnumList(i : Integer) : TFhirContractResourceStatusCodesEnumList;
var
  aLoop : TFhirContractResourceStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirContractResourceStatusCodesEnum) to high(TFhirContractResourceStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirContributorTypeEnumListAsInteger(aSet : TFhirContributorTypeEnumList) : Integer;
var
  a : TFhirContributorTypeEnum;
begin
  result := 0;
  for a := low(TFhirContributorTypeEnum) to high(TFhirContributorTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContributorTypeEnumList(i : Integer) : TFhirContributorTypeEnumList;
var
  aLoop : TFhirContributorTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirContributorTypeEnum) to high(TFhirContributorTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDaysOfWeekEnumListAsInteger(aSet : TFhirDaysOfWeekEnumList) : Integer;
var
  a : TFhirDaysOfWeekEnum;
begin
  result := 0;
  for a := low(TFhirDaysOfWeekEnum) to high(TFhirDaysOfWeekEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDaysOfWeekEnumList(i : Integer) : TFhirDaysOfWeekEnumList;
var
  aLoop : TFhirDaysOfWeekEnum;
begin
  result := [];
  for aLoop := low(TFhirDaysOfWeekEnum) to high(TFhirDaysOfWeekEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDetectedIssueSeverityEnumListAsInteger(aSet : TFhirDetectedIssueSeverityEnumList) : Integer;
var
  a : TFhirDetectedIssueSeverityEnum;
begin
  result := 0;
  for a := low(TFhirDetectedIssueSeverityEnum) to high(TFhirDetectedIssueSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDetectedIssueSeverityEnumList(i : Integer) : TFhirDetectedIssueSeverityEnumList;
var
  aLoop : TFhirDetectedIssueSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirDetectedIssueSeverityEnum) to high(TFhirDetectedIssueSeverityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceMetricCalibrationStateEnumListAsInteger(aSet : TFhirDeviceMetricCalibrationStateEnumList) : Integer;
var
  a : TFhirDeviceMetricCalibrationStateEnum;
begin
  result := 0;
  for a := low(TFhirDeviceMetricCalibrationStateEnum) to high(TFhirDeviceMetricCalibrationStateEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceMetricCalibrationStateEnumList(i : Integer) : TFhirDeviceMetricCalibrationStateEnumList;
var
  aLoop : TFhirDeviceMetricCalibrationStateEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceMetricCalibrationStateEnum) to high(TFhirDeviceMetricCalibrationStateEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceMetricCalibrationTypeEnumListAsInteger(aSet : TFhirDeviceMetricCalibrationTypeEnumList) : Integer;
var
  a : TFhirDeviceMetricCalibrationTypeEnum;
begin
  result := 0;
  for a := low(TFhirDeviceMetricCalibrationTypeEnum) to high(TFhirDeviceMetricCalibrationTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceMetricCalibrationTypeEnumList(i : Integer) : TFhirDeviceMetricCalibrationTypeEnumList;
var
  aLoop : TFhirDeviceMetricCalibrationTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceMetricCalibrationTypeEnum) to high(TFhirDeviceMetricCalibrationTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceMetricCategoryEnumListAsInteger(aSet : TFhirDeviceMetricCategoryEnumList) : Integer;
var
  a : TFhirDeviceMetricCategoryEnum;
begin
  result := 0;
  for a := low(TFhirDeviceMetricCategoryEnum) to high(TFhirDeviceMetricCategoryEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceMetricCategoryEnumList(i : Integer) : TFhirDeviceMetricCategoryEnumList;
var
  aLoop : TFhirDeviceMetricCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceMetricCategoryEnum) to high(TFhirDeviceMetricCategoryEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceMetricColorEnumListAsInteger(aSet : TFhirDeviceMetricColorEnumList) : Integer;
var
  a : TFhirDeviceMetricColorEnum;
begin
  result := 0;
  for a := low(TFhirDeviceMetricColorEnum) to high(TFhirDeviceMetricColorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceMetricColorEnumList(i : Integer) : TFhirDeviceMetricColorEnumList;
var
  aLoop : TFhirDeviceMetricColorEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceMetricColorEnum) to high(TFhirDeviceMetricColorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceMetricOperationalStatusEnumListAsInteger(aSet : TFhirDeviceMetricOperationalStatusEnumList) : Integer;
var
  a : TFhirDeviceMetricOperationalStatusEnum;
begin
  result := 0;
  for a := low(TFhirDeviceMetricOperationalStatusEnum) to high(TFhirDeviceMetricOperationalStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceMetricOperationalStatusEnumList(i : Integer) : TFhirDeviceMetricOperationalStatusEnumList;
var
  aLoop : TFhirDeviceMetricOperationalStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceMetricOperationalStatusEnum) to high(TFhirDeviceMetricOperationalStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceNameTypeEnumListAsInteger(aSet : TFhirDeviceNameTypeEnumList) : Integer;
var
  a : TFhirDeviceNameTypeEnum;
begin
  result := 0;
  for a := low(TFhirDeviceNameTypeEnum) to high(TFhirDeviceNameTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceNameTypeEnumList(i : Integer) : TFhirDeviceNameTypeEnumList;
var
  aLoop : TFhirDeviceNameTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceNameTypeEnum) to high(TFhirDeviceNameTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceUseStatementStatusEnumListAsInteger(aSet : TFhirDeviceUseStatementStatusEnumList) : Integer;
var
  a : TFhirDeviceUseStatementStatusEnum;
begin
  result := 0;
  for a := low(TFhirDeviceUseStatementStatusEnum) to high(TFhirDeviceUseStatementStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceUseStatementStatusEnumList(i : Integer) : TFhirDeviceUseStatementStatusEnumList;
var
  aLoop : TFhirDeviceUseStatementStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceUseStatementStatusEnum) to high(TFhirDeviceUseStatementStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDiagnosticReportStatusEnumListAsInteger(aSet : TFhirDiagnosticReportStatusEnumList) : Integer;
var
  a : TFhirDiagnosticReportStatusEnum;
begin
  result := 0;
  for a := low(TFhirDiagnosticReportStatusEnum) to high(TFhirDiagnosticReportStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDiagnosticReportStatusEnumList(i : Integer) : TFhirDiagnosticReportStatusEnumList;
var
  aLoop : TFhirDiagnosticReportStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDiagnosticReportStatusEnum) to high(TFhirDiagnosticReportStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDiscriminatorTypeEnumListAsInteger(aSet : TFhirDiscriminatorTypeEnumList) : Integer;
var
  a : TFhirDiscriminatorTypeEnum;
begin
  result := 0;
  for a := low(TFhirDiscriminatorTypeEnum) to high(TFhirDiscriminatorTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDiscriminatorTypeEnumList(i : Integer) : TFhirDiscriminatorTypeEnumList;
var
  aLoop : TFhirDiscriminatorTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirDiscriminatorTypeEnum) to high(TFhirDiscriminatorTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDocumentAttestationModeEnumListAsInteger(aSet : TFhirDocumentAttestationModeEnumList) : Integer;
var
  a : TFhirDocumentAttestationModeEnum;
begin
  result := 0;
  for a := low(TFhirDocumentAttestationModeEnum) to high(TFhirDocumentAttestationModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentAttestationModeEnumList(i : Integer) : TFhirDocumentAttestationModeEnumList;
var
  aLoop : TFhirDocumentAttestationModeEnum;
begin
  result := [];
  for aLoop := low(TFhirDocumentAttestationModeEnum) to high(TFhirDocumentAttestationModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDocumentModeEnumListAsInteger(aSet : TFhirDocumentModeEnumList) : Integer;
var
  a : TFhirDocumentModeEnum;
begin
  result := 0;
  for a := low(TFhirDocumentModeEnum) to high(TFhirDocumentModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentModeEnumList(i : Integer) : TFhirDocumentModeEnumList;
var
  aLoop : TFhirDocumentModeEnum;
begin
  result := [];
  for aLoop := low(TFhirDocumentModeEnum) to high(TFhirDocumentModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDocumentReferenceStatusEnumListAsInteger(aSet : TFhirDocumentReferenceStatusEnumList) : Integer;
var
  a : TFhirDocumentReferenceStatusEnum;
begin
  result := 0;
  for a := low(TFhirDocumentReferenceStatusEnum) to high(TFhirDocumentReferenceStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentReferenceStatusEnumList(i : Integer) : TFhirDocumentReferenceStatusEnumList;
var
  aLoop : TFhirDocumentReferenceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDocumentReferenceStatusEnum) to high(TFhirDocumentReferenceStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDocumentRelationshipTypeEnumListAsInteger(aSet : TFhirDocumentRelationshipTypeEnumList) : Integer;
var
  a : TFhirDocumentRelationshipTypeEnum;
begin
  result := 0;
  for a := low(TFhirDocumentRelationshipTypeEnum) to high(TFhirDocumentRelationshipTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentRelationshipTypeEnumList(i : Integer) : TFhirDocumentRelationshipTypeEnumList;
var
  aLoop : TFhirDocumentRelationshipTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirDocumentRelationshipTypeEnum) to high(TFhirDocumentRelationshipTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEligibilityRequestPurposeEnumListAsInteger(aSet : TFhirEligibilityRequestPurposeEnumList) : Integer;
var
  a : TFhirEligibilityRequestPurposeEnum;
begin
  result := 0;
  for a := low(TFhirEligibilityRequestPurposeEnum) to high(TFhirEligibilityRequestPurposeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEligibilityRequestPurposeEnumList(i : Integer) : TFhirEligibilityRequestPurposeEnumList;
var
  aLoop : TFhirEligibilityRequestPurposeEnum;
begin
  result := [];
  for aLoop := low(TFhirEligibilityRequestPurposeEnum) to high(TFhirEligibilityRequestPurposeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEligibilityResponsePurposeEnumListAsInteger(aSet : TFhirEligibilityResponsePurposeEnumList) : Integer;
var
  a : TFhirEligibilityResponsePurposeEnum;
begin
  result := 0;
  for a := low(TFhirEligibilityResponsePurposeEnum) to high(TFhirEligibilityResponsePurposeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEligibilityResponsePurposeEnumList(i : Integer) : TFhirEligibilityResponsePurposeEnumList;
var
  aLoop : TFhirEligibilityResponsePurposeEnum;
begin
  result := [];
  for aLoop := low(TFhirEligibilityResponsePurposeEnum) to high(TFhirEligibilityResponsePurposeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEnableWhenBehaviorEnumListAsInteger(aSet : TFhirEnableWhenBehaviorEnumList) : Integer;
var
  a : TFhirEnableWhenBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirEnableWhenBehaviorEnum) to high(TFhirEnableWhenBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEnableWhenBehaviorEnumList(i : Integer) : TFhirEnableWhenBehaviorEnumList;
var
  aLoop : TFhirEnableWhenBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirEnableWhenBehaviorEnum) to high(TFhirEnableWhenBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEncounterLocationStatusEnumListAsInteger(aSet : TFhirEncounterLocationStatusEnumList) : Integer;
var
  a : TFhirEncounterLocationStatusEnum;
begin
  result := 0;
  for a := low(TFhirEncounterLocationStatusEnum) to high(TFhirEncounterLocationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEncounterLocationStatusEnumList(i : Integer) : TFhirEncounterLocationStatusEnumList;
var
  aLoop : TFhirEncounterLocationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEncounterLocationStatusEnum) to high(TFhirEncounterLocationStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEncounterStatusEnumListAsInteger(aSet : TFhirEncounterStatusEnumList) : Integer;
var
  a : TFhirEncounterStatusEnum;
begin
  result := 0;
  for a := low(TFhirEncounterStatusEnum) to high(TFhirEncounterStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEncounterStatusEnumList(i : Integer) : TFhirEncounterStatusEnumList;
var
  aLoop : TFhirEncounterStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEncounterStatusEnum) to high(TFhirEncounterStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEndpointStatusEnumListAsInteger(aSet : TFhirEndpointStatusEnumList) : Integer;
var
  a : TFhirEndpointStatusEnum;
begin
  result := 0;
  for a := low(TFhirEndpointStatusEnum) to high(TFhirEndpointStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEndpointStatusEnumList(i : Integer) : TFhirEndpointStatusEnumList;
var
  aLoop : TFhirEndpointStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEndpointStatusEnum) to high(TFhirEndpointStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEpisodeOfCareStatusEnumListAsInteger(aSet : TFhirEpisodeOfCareStatusEnumList) : Integer;
var
  a : TFhirEpisodeOfCareStatusEnum;
begin
  result := 0;
  for a := low(TFhirEpisodeOfCareStatusEnum) to high(TFhirEpisodeOfCareStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEpisodeOfCareStatusEnumList(i : Integer) : TFhirEpisodeOfCareStatusEnumList;
var
  aLoop : TFhirEpisodeOfCareStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEpisodeOfCareStatusEnum) to high(TFhirEpisodeOfCareStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEventCapabilityModeEnumListAsInteger(aSet : TFhirEventCapabilityModeEnumList) : Integer;
var
  a : TFhirEventCapabilityModeEnum;
begin
  result := 0;
  for a := low(TFhirEventCapabilityModeEnum) to high(TFhirEventCapabilityModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEventCapabilityModeEnumList(i : Integer) : TFhirEventCapabilityModeEnumList;
var
  aLoop : TFhirEventCapabilityModeEnum;
begin
  result := [];
  for aLoop := low(TFhirEventCapabilityModeEnum) to high(TFhirEventCapabilityModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEventStatusEnumListAsInteger(aSet : TFhirEventStatusEnumList) : Integer;
var
  a : TFhirEventStatusEnum;
begin
  result := 0;
  for a := low(TFhirEventStatusEnum) to high(TFhirEventStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEventStatusEnumList(i : Integer) : TFhirEventStatusEnumList;
var
  aLoop : TFhirEventStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEventStatusEnum) to high(TFhirEventStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEventTimingEnumListAsInteger(aSet : TFhirEventTimingEnumList) : Integer;
var
  a : TFhirEventTimingEnum;
begin
  result := 0;
  for a := low(TFhirEventTimingEnum) to high(TFhirEventTimingEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEventTimingEnumList(i : Integer) : TFhirEventTimingEnumList;
var
  aLoop : TFhirEventTimingEnum;
begin
  result := [];
  for aLoop := low(TFhirEventTimingEnum) to high(TFhirEventTimingEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEvidenceVariableHandlingEnumListAsInteger(aSet : TFhirEvidenceVariableHandlingEnumList) : Integer;
var
  a : TFhirEvidenceVariableHandlingEnum;
begin
  result := 0;
  for a := low(TFhirEvidenceVariableHandlingEnum) to high(TFhirEvidenceVariableHandlingEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEvidenceVariableHandlingEnumList(i : Integer) : TFhirEvidenceVariableHandlingEnumList;
var
  aLoop : TFhirEvidenceVariableHandlingEnum;
begin
  result := [];
  for aLoop := low(TFhirEvidenceVariableHandlingEnum) to high(TFhirEvidenceVariableHandlingEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirExampleScenarioActorTypeEnumListAsInteger(aSet : TFhirExampleScenarioActorTypeEnumList) : Integer;
var
  a : TFhirExampleScenarioActorTypeEnum;
begin
  result := 0;
  for a := low(TFhirExampleScenarioActorTypeEnum) to high(TFhirExampleScenarioActorTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirExampleScenarioActorTypeEnumList(i : Integer) : TFhirExampleScenarioActorTypeEnumList;
var
  aLoop : TFhirExampleScenarioActorTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirExampleScenarioActorTypeEnum) to high(TFhirExampleScenarioActorTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirExplanationOfBenefitStatusEnumListAsInteger(aSet : TFhirExplanationOfBenefitStatusEnumList) : Integer;
var
  a : TFhirExplanationOfBenefitStatusEnum;
begin
  result := 0;
  for a := low(TFhirExplanationOfBenefitStatusEnum) to high(TFhirExplanationOfBenefitStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirExplanationOfBenefitStatusEnumList(i : Integer) : TFhirExplanationOfBenefitStatusEnumList;
var
  aLoop : TFhirExplanationOfBenefitStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirExplanationOfBenefitStatusEnum) to high(TFhirExplanationOfBenefitStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirExtensionContextTypeEnumListAsInteger(aSet : TFhirExtensionContextTypeEnumList) : Integer;
var
  a : TFhirExtensionContextTypeEnum;
begin
  result := 0;
  for a := low(TFhirExtensionContextTypeEnum) to high(TFhirExtensionContextTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirExtensionContextTypeEnumList(i : Integer) : TFhirExtensionContextTypeEnumList;
var
  aLoop : TFhirExtensionContextTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirExtensionContextTypeEnum) to high(TFhirExtensionContextTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFHIRDefinedTypeEnumListAsInteger(aSet : TFhirFHIRDefinedTypeEnumList) : Integer;
var
  a : TFhirFHIRDefinedTypeEnum;
begin
  result := 0;
  for a := low(TFhirFHIRDefinedTypeEnum) to high(TFhirFHIRDefinedTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFHIRDefinedTypeEnumList(i : Integer) : TFhirFHIRDefinedTypeEnumList;
var
  aLoop : TFhirFHIRDefinedTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirFHIRDefinedTypeEnum) to high(TFhirFHIRDefinedTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFHIRDeviceStatusEnumListAsInteger(aSet : TFhirFHIRDeviceStatusEnumList) : Integer;
var
  a : TFhirFHIRDeviceStatusEnum;
begin
  result := 0;
  for a := low(TFhirFHIRDeviceStatusEnum) to high(TFhirFHIRDeviceStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFHIRDeviceStatusEnumList(i : Integer) : TFhirFHIRDeviceStatusEnumList;
var
  aLoop : TFhirFHIRDeviceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirFHIRDeviceStatusEnum) to high(TFhirFHIRDeviceStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFHIRSubstanceStatusEnumListAsInteger(aSet : TFhirFHIRSubstanceStatusEnumList) : Integer;
var
  a : TFhirFHIRSubstanceStatusEnum;
begin
  result := 0;
  for a := low(TFhirFHIRSubstanceStatusEnum) to high(TFhirFHIRSubstanceStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFHIRSubstanceStatusEnumList(i : Integer) : TFhirFHIRSubstanceStatusEnumList;
var
  aLoop : TFhirFHIRSubstanceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirFHIRSubstanceStatusEnum) to high(TFhirFHIRSubstanceStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFHIRVersionEnumListAsInteger(aSet : TFhirFHIRVersionEnumList) : Integer;
var
  a : TFhirFHIRVersionEnum;
begin
  result := 0;
  for a := low(TFhirFHIRVersionEnum) to high(TFhirFHIRVersionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFHIRVersionEnumList(i : Integer) : TFhirFHIRVersionEnumList;
var
  aLoop : TFhirFHIRVersionEnum;
begin
  result := [];
  for aLoop := low(TFhirFHIRVersionEnum) to high(TFhirFHIRVersionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFamilyHistoryStatusEnumListAsInteger(aSet : TFhirFamilyHistoryStatusEnumList) : Integer;
var
  a : TFhirFamilyHistoryStatusEnum;
begin
  result := 0;
  for a := low(TFhirFamilyHistoryStatusEnum) to high(TFhirFamilyHistoryStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFamilyHistoryStatusEnumList(i : Integer) : TFhirFamilyHistoryStatusEnumList;
var
  aLoop : TFhirFamilyHistoryStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirFamilyHistoryStatusEnum) to high(TFhirFamilyHistoryStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFilterOperatorEnumListAsInteger(aSet : TFhirFilterOperatorEnumList) : Integer;
var
  a : TFhirFilterOperatorEnum;
begin
  result := 0;
  for a := low(TFhirFilterOperatorEnum) to high(TFhirFilterOperatorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFilterOperatorEnumList(i : Integer) : TFhirFilterOperatorEnumList;
var
  aLoop : TFhirFilterOperatorEnum;
begin
  result := [];
  for aLoop := low(TFhirFilterOperatorEnum) to high(TFhirFilterOperatorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFinancialResourceStatusCodesEnumListAsInteger(aSet : TFhirFinancialResourceStatusCodesEnumList) : Integer;
var
  a : TFhirFinancialResourceStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirFinancialResourceStatusCodesEnum) to high(TFhirFinancialResourceStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFinancialResourceStatusCodesEnumList(i : Integer) : TFhirFinancialResourceStatusCodesEnumList;
var
  aLoop : TFhirFinancialResourceStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirFinancialResourceStatusCodesEnum) to high(TFhirFinancialResourceStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFlagStatusEnumListAsInteger(aSet : TFhirFlagStatusEnumList) : Integer;
var
  a : TFhirFlagStatusEnum;
begin
  result := 0;
  for a := low(TFhirFlagStatusEnum) to high(TFhirFlagStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFlagStatusEnumList(i : Integer) : TFhirFlagStatusEnumList;
var
  aLoop : TFhirFlagStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirFlagStatusEnum) to high(TFhirFlagStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGoalLifecycleStatusEnumListAsInteger(aSet : TFhirGoalLifecycleStatusEnumList) : Integer;
var
  a : TFhirGoalLifecycleStatusEnum;
begin
  result := 0;
  for a := low(TFhirGoalLifecycleStatusEnum) to high(TFhirGoalLifecycleStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGoalLifecycleStatusEnumList(i : Integer) : TFhirGoalLifecycleStatusEnumList;
var
  aLoop : TFhirGoalLifecycleStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirGoalLifecycleStatusEnum) to high(TFhirGoalLifecycleStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGraphCompartmentRuleEnumListAsInteger(aSet : TFhirGraphCompartmentRuleEnumList) : Integer;
var
  a : TFhirGraphCompartmentRuleEnum;
begin
  result := 0;
  for a := low(TFhirGraphCompartmentRuleEnum) to high(TFhirGraphCompartmentRuleEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGraphCompartmentRuleEnumList(i : Integer) : TFhirGraphCompartmentRuleEnumList;
var
  aLoop : TFhirGraphCompartmentRuleEnum;
begin
  result := [];
  for aLoop := low(TFhirGraphCompartmentRuleEnum) to high(TFhirGraphCompartmentRuleEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGraphCompartmentUseEnumListAsInteger(aSet : TFhirGraphCompartmentUseEnumList) : Integer;
var
  a : TFhirGraphCompartmentUseEnum;
begin
  result := 0;
  for a := low(TFhirGraphCompartmentUseEnum) to high(TFhirGraphCompartmentUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGraphCompartmentUseEnumList(i : Integer) : TFhirGraphCompartmentUseEnumList;
var
  aLoop : TFhirGraphCompartmentUseEnum;
begin
  result := [];
  for aLoop := low(TFhirGraphCompartmentUseEnum) to high(TFhirGraphCompartmentUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGroupMeasureEnumListAsInteger(aSet : TFhirGroupMeasureEnumList) : Integer;
var
  a : TFhirGroupMeasureEnum;
begin
  result := 0;
  for a := low(TFhirGroupMeasureEnum) to high(TFhirGroupMeasureEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGroupMeasureEnumList(i : Integer) : TFhirGroupMeasureEnumList;
var
  aLoop : TFhirGroupMeasureEnum;
begin
  result := [];
  for aLoop := low(TFhirGroupMeasureEnum) to high(TFhirGroupMeasureEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGroupTypeEnumListAsInteger(aSet : TFhirGroupTypeEnumList) : Integer;
var
  a : TFhirGroupTypeEnum;
begin
  result := 0;
  for a := low(TFhirGroupTypeEnum) to high(TFhirGroupTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGroupTypeEnumList(i : Integer) : TFhirGroupTypeEnumList;
var
  aLoop : TFhirGroupTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirGroupTypeEnum) to high(TFhirGroupTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGuidanceResponseStatusEnumListAsInteger(aSet : TFhirGuidanceResponseStatusEnumList) : Integer;
var
  a : TFhirGuidanceResponseStatusEnum;
begin
  result := 0;
  for a := low(TFhirGuidanceResponseStatusEnum) to high(TFhirGuidanceResponseStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGuidanceResponseStatusEnumList(i : Integer) : TFhirGuidanceResponseStatusEnumList;
var
  aLoop : TFhirGuidanceResponseStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirGuidanceResponseStatusEnum) to high(TFhirGuidanceResponseStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGuidePageGenerationEnumListAsInteger(aSet : TFhirGuidePageGenerationEnumList) : Integer;
var
  a : TFhirGuidePageGenerationEnum;
begin
  result := 0;
  for a := low(TFhirGuidePageGenerationEnum) to high(TFhirGuidePageGenerationEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGuidePageGenerationEnumList(i : Integer) : TFhirGuidePageGenerationEnumList;
var
  aLoop : TFhirGuidePageGenerationEnum;
begin
  result := [];
  for aLoop := low(TFhirGuidePageGenerationEnum) to high(TFhirGuidePageGenerationEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirHTTPVerbEnumListAsInteger(aSet : TFhirHTTPVerbEnumList) : Integer;
var
  a : TFhirHTTPVerbEnum;
begin
  result := 0;
  for a := low(TFhirHTTPVerbEnum) to high(TFhirHTTPVerbEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirHTTPVerbEnumList(i : Integer) : TFhirHTTPVerbEnumList;
var
  aLoop : TFhirHTTPVerbEnum;
begin
  result := [];
  for aLoop := low(TFhirHTTPVerbEnum) to high(TFhirHTTPVerbEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirIdentifierUseEnumListAsInteger(aSet : TFhirIdentifierUseEnumList) : Integer;
var
  a : TFhirIdentifierUseEnum;
begin
  result := 0;
  for a := low(TFhirIdentifierUseEnum) to high(TFhirIdentifierUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIdentifierUseEnumList(i : Integer) : TFhirIdentifierUseEnumList;
var
  aLoop : TFhirIdentifierUseEnum;
begin
  result := [];
  for aLoop := low(TFhirIdentifierUseEnum) to high(TFhirIdentifierUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirIdentityAssuranceLevelEnumListAsInteger(aSet : TFhirIdentityAssuranceLevelEnumList) : Integer;
var
  a : TFhirIdentityAssuranceLevelEnum;
begin
  result := 0;
  for a := low(TFhirIdentityAssuranceLevelEnum) to high(TFhirIdentityAssuranceLevelEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIdentityAssuranceLevelEnumList(i : Integer) : TFhirIdentityAssuranceLevelEnumList;
var
  aLoop : TFhirIdentityAssuranceLevelEnum;
begin
  result := [];
  for aLoop := low(TFhirIdentityAssuranceLevelEnum) to high(TFhirIdentityAssuranceLevelEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirImagingStudyStatusEnumListAsInteger(aSet : TFhirImagingStudyStatusEnumList) : Integer;
var
  a : TFhirImagingStudyStatusEnum;
begin
  result := 0;
  for a := low(TFhirImagingStudyStatusEnum) to high(TFhirImagingStudyStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirImagingStudyStatusEnumList(i : Integer) : TFhirImagingStudyStatusEnumList;
var
  aLoop : TFhirImagingStudyStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirImagingStudyStatusEnum) to high(TFhirImagingStudyStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirImmunizationEvaluationStatusCodesEnumListAsInteger(aSet : TFhirImmunizationEvaluationStatusCodesEnumList) : Integer;
var
  a : TFhirImmunizationEvaluationStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirImmunizationEvaluationStatusCodesEnum) to high(TFhirImmunizationEvaluationStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirImmunizationEvaluationStatusCodesEnumList(i : Integer) : TFhirImmunizationEvaluationStatusCodesEnumList;
var
  aLoop : TFhirImmunizationEvaluationStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirImmunizationEvaluationStatusCodesEnum) to high(TFhirImmunizationEvaluationStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirImmunizationStatusCodesEnumListAsInteger(aSet : TFhirImmunizationStatusCodesEnumList) : Integer;
var
  a : TFhirImmunizationStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirImmunizationStatusCodesEnum) to high(TFhirImmunizationStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirImmunizationStatusCodesEnumList(i : Integer) : TFhirImmunizationStatusCodesEnumList;
var
  aLoop : TFhirImmunizationStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirImmunizationStatusCodesEnum) to high(TFhirImmunizationStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirInteractionTriggerEnumListAsInteger(aSet : TFhirInteractionTriggerEnumList) : Integer;
var
  a : TFhirInteractionTriggerEnum;
begin
  result := 0;
  for a := low(TFhirInteractionTriggerEnum) to high(TFhirInteractionTriggerEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirInteractionTriggerEnumList(i : Integer) : TFhirInteractionTriggerEnumList;
var
  aLoop : TFhirInteractionTriggerEnum;
begin
  result := [];
  for aLoop := low(TFhirInteractionTriggerEnum) to high(TFhirInteractionTriggerEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirInvoicePriceComponentTypeEnumListAsInteger(aSet : TFhirInvoicePriceComponentTypeEnumList) : Integer;
var
  a : TFhirInvoicePriceComponentTypeEnum;
begin
  result := 0;
  for a := low(TFhirInvoicePriceComponentTypeEnum) to high(TFhirInvoicePriceComponentTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirInvoicePriceComponentTypeEnumList(i : Integer) : TFhirInvoicePriceComponentTypeEnumList;
var
  aLoop : TFhirInvoicePriceComponentTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirInvoicePriceComponentTypeEnum) to high(TFhirInvoicePriceComponentTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirInvoiceStatusEnumListAsInteger(aSet : TFhirInvoiceStatusEnumList) : Integer;
var
  a : TFhirInvoiceStatusEnum;
begin
  result := 0;
  for a := low(TFhirInvoiceStatusEnum) to high(TFhirInvoiceStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirInvoiceStatusEnumList(i : Integer) : TFhirInvoiceStatusEnumList;
var
  aLoop : TFhirInvoiceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirInvoiceStatusEnum) to high(TFhirInvoiceStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirIssueSeverityEnumListAsInteger(aSet : TFhirIssueSeverityEnumList) : Integer;
var
  a : TFhirIssueSeverityEnum;
begin
  result := 0;
  for a := low(TFhirIssueSeverityEnum) to high(TFhirIssueSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIssueSeverityEnumList(i : Integer) : TFhirIssueSeverityEnumList;
var
  aLoop : TFhirIssueSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirIssueSeverityEnum) to high(TFhirIssueSeverityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirIssueTypeEnumListAsInteger(aSet : TFhirIssueTypeEnumList) : Integer;
var
  a : TFhirIssueTypeEnum;
begin
  result := 0;
  for a := low(TFhirIssueTypeEnum) to high(TFhirIssueTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIssueTypeEnumList(i : Integer) : TFhirIssueTypeEnumList;
var
  aLoop : TFhirIssueTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirIssueTypeEnum) to high(TFhirIssueTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirLinkTypeEnumListAsInteger(aSet : TFhirLinkTypeEnumList) : Integer;
var
  a : TFhirLinkTypeEnum;
begin
  result := 0;
  for a := low(TFhirLinkTypeEnum) to high(TFhirLinkTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLinkTypeEnumList(i : Integer) : TFhirLinkTypeEnumList;
var
  aLoop : TFhirLinkTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirLinkTypeEnum) to high(TFhirLinkTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirLinkageTypeEnumListAsInteger(aSet : TFhirLinkageTypeEnumList) : Integer;
var
  a : TFhirLinkageTypeEnum;
begin
  result := 0;
  for a := low(TFhirLinkageTypeEnum) to high(TFhirLinkageTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLinkageTypeEnumList(i : Integer) : TFhirLinkageTypeEnumList;
var
  aLoop : TFhirLinkageTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirLinkageTypeEnum) to high(TFhirLinkageTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirListModeEnumListAsInteger(aSet : TFhirListModeEnumList) : Integer;
var
  a : TFhirListModeEnum;
begin
  result := 0;
  for a := low(TFhirListModeEnum) to high(TFhirListModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirListModeEnumList(i : Integer) : TFhirListModeEnumList;
var
  aLoop : TFhirListModeEnum;
begin
  result := [];
  for aLoop := low(TFhirListModeEnum) to high(TFhirListModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirListStatusEnumListAsInteger(aSet : TFhirListStatusEnumList) : Integer;
var
  a : TFhirListStatusEnum;
begin
  result := 0;
  for a := low(TFhirListStatusEnum) to high(TFhirListStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirListStatusEnumList(i : Integer) : TFhirListStatusEnumList;
var
  aLoop : TFhirListStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirListStatusEnum) to high(TFhirListStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirLocationModeEnumListAsInteger(aSet : TFhirLocationModeEnumList) : Integer;
var
  a : TFhirLocationModeEnum;
begin
  result := 0;
  for a := low(TFhirLocationModeEnum) to high(TFhirLocationModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLocationModeEnumList(i : Integer) : TFhirLocationModeEnumList;
var
  aLoop : TFhirLocationModeEnum;
begin
  result := [];
  for aLoop := low(TFhirLocationModeEnum) to high(TFhirLocationModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirLocationStatusEnumListAsInteger(aSet : TFhirLocationStatusEnumList) : Integer;
var
  a : TFhirLocationStatusEnum;
begin
  result := 0;
  for a := low(TFhirLocationStatusEnum) to high(TFhirLocationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLocationStatusEnumList(i : Integer) : TFhirLocationStatusEnumList;
var
  aLoop : TFhirLocationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirLocationStatusEnum) to high(TFhirLocationStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMeasureReportStatusEnumListAsInteger(aSet : TFhirMeasureReportStatusEnumList) : Integer;
var
  a : TFhirMeasureReportStatusEnum;
begin
  result := 0;
  for a := low(TFhirMeasureReportStatusEnum) to high(TFhirMeasureReportStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMeasureReportStatusEnumList(i : Integer) : TFhirMeasureReportStatusEnumList;
var
  aLoop : TFhirMeasureReportStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirMeasureReportStatusEnum) to high(TFhirMeasureReportStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMeasureReportTypeEnumListAsInteger(aSet : TFhirMeasureReportTypeEnumList) : Integer;
var
  a : TFhirMeasureReportTypeEnum;
begin
  result := 0;
  for a := low(TFhirMeasureReportTypeEnum) to high(TFhirMeasureReportTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMeasureReportTypeEnumList(i : Integer) : TFhirMeasureReportTypeEnumList;
var
  aLoop : TFhirMeasureReportTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirMeasureReportTypeEnum) to high(TFhirMeasureReportTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationAdministrationStatusCodesEnumListAsInteger(aSet : TFhirMedicationAdministrationStatusCodesEnumList) : Integer;
var
  a : TFhirMedicationAdministrationStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirMedicationAdministrationStatusCodesEnum) to high(TFhirMedicationAdministrationStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationAdministrationStatusCodesEnumList(i : Integer) : TFhirMedicationAdministrationStatusCodesEnumList;
var
  aLoop : TFhirMedicationAdministrationStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationAdministrationStatusCodesEnum) to high(TFhirMedicationAdministrationStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationDispenseStatusCodesEnumListAsInteger(aSet : TFhirMedicationDispenseStatusCodesEnumList) : Integer;
var
  a : TFhirMedicationDispenseStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirMedicationDispenseStatusCodesEnum) to high(TFhirMedicationDispenseStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationDispenseStatusCodesEnumList(i : Integer) : TFhirMedicationDispenseStatusCodesEnumList;
var
  aLoop : TFhirMedicationDispenseStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationDispenseStatusCodesEnum) to high(TFhirMedicationDispenseStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationKnowledgeStatusCodesEnumListAsInteger(aSet : TFhirMedicationKnowledgeStatusCodesEnumList) : Integer;
var
  a : TFhirMedicationKnowledgeStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirMedicationKnowledgeStatusCodesEnum) to high(TFhirMedicationKnowledgeStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationKnowledgeStatusCodesEnumList(i : Integer) : TFhirMedicationKnowledgeStatusCodesEnumList;
var
  aLoop : TFhirMedicationKnowledgeStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationKnowledgeStatusCodesEnum) to high(TFhirMedicationKnowledgeStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationRequestIntentEnumListAsInteger(aSet : TFhirMedicationRequestIntentEnumList) : Integer;
var
  a : TFhirMedicationRequestIntentEnum;
begin
  result := 0;
  for a := low(TFhirMedicationRequestIntentEnum) to high(TFhirMedicationRequestIntentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationRequestIntentEnumList(i : Integer) : TFhirMedicationRequestIntentEnumList;
var
  aLoop : TFhirMedicationRequestIntentEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationRequestIntentEnum) to high(TFhirMedicationRequestIntentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationStatusCodesEnumListAsInteger(aSet : TFhirMedicationStatusCodesEnumList) : Integer;
var
  a : TFhirMedicationStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirMedicationStatusCodesEnum) to high(TFhirMedicationStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationStatusCodesEnumList(i : Integer) : TFhirMedicationStatusCodesEnumList;
var
  aLoop : TFhirMedicationStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationStatusCodesEnum) to high(TFhirMedicationStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationUsageStatusCodesEnumListAsInteger(aSet : TFhirMedicationUsageStatusCodesEnumList) : Integer;
var
  a : TFhirMedicationUsageStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirMedicationUsageStatusCodesEnum) to high(TFhirMedicationUsageStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationUsageStatusCodesEnumList(i : Integer) : TFhirMedicationUsageStatusCodesEnumList;
var
  aLoop : TFhirMedicationUsageStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationUsageStatusCodesEnum) to high(TFhirMedicationUsageStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationrequestStatusEnumListAsInteger(aSet : TFhirMedicationrequestStatusEnumList) : Integer;
var
  a : TFhirMedicationrequestStatusEnum;
begin
  result := 0;
  for a := low(TFhirMedicationrequestStatusEnum) to high(TFhirMedicationrequestStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationrequestStatusEnumList(i : Integer) : TFhirMedicationrequestStatusEnumList;
var
  aLoop : TFhirMedicationrequestStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationrequestStatusEnum) to high(TFhirMedicationrequestStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMessageSignificanceCategoryEnumListAsInteger(aSet : TFhirMessageSignificanceCategoryEnumList) : Integer;
var
  a : TFhirMessageSignificanceCategoryEnum;
begin
  result := 0;
  for a := low(TFhirMessageSignificanceCategoryEnum) to high(TFhirMessageSignificanceCategoryEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMessageSignificanceCategoryEnumList(i : Integer) : TFhirMessageSignificanceCategoryEnumList;
var
  aLoop : TFhirMessageSignificanceCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirMessageSignificanceCategoryEnum) to high(TFhirMessageSignificanceCategoryEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMessageheaderResponseRequestEnumListAsInteger(aSet : TFhirMessageheaderResponseRequestEnumList) : Integer;
var
  a : TFhirMessageheaderResponseRequestEnum;
begin
  result := 0;
  for a := low(TFhirMessageheaderResponseRequestEnum) to high(TFhirMessageheaderResponseRequestEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMessageheaderResponseRequestEnumList(i : Integer) : TFhirMessageheaderResponseRequestEnumList;
var
  aLoop : TFhirMessageheaderResponseRequestEnum;
begin
  result := [];
  for aLoop := low(TFhirMessageheaderResponseRequestEnum) to high(TFhirMessageheaderResponseRequestEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNameUseEnumListAsInteger(aSet : TFhirNameUseEnumList) : Integer;
var
  a : TFhirNameUseEnum;
begin
  result := 0;
  for a := low(TFhirNameUseEnum) to high(TFhirNameUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNameUseEnumList(i : Integer) : TFhirNameUseEnumList;
var
  aLoop : TFhirNameUseEnum;
begin
  result := [];
  for aLoop := low(TFhirNameUseEnum) to high(TFhirNameUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNamingSystemIdentifierTypeEnumListAsInteger(aSet : TFhirNamingSystemIdentifierTypeEnumList) : Integer;
var
  a : TFhirNamingSystemIdentifierTypeEnum;
begin
  result := 0;
  for a := low(TFhirNamingSystemIdentifierTypeEnum) to high(TFhirNamingSystemIdentifierTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNamingSystemIdentifierTypeEnumList(i : Integer) : TFhirNamingSystemIdentifierTypeEnumList;
var
  aLoop : TFhirNamingSystemIdentifierTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirNamingSystemIdentifierTypeEnum) to high(TFhirNamingSystemIdentifierTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNamingSystemTypeEnumListAsInteger(aSet : TFhirNamingSystemTypeEnumList) : Integer;
var
  a : TFhirNamingSystemTypeEnum;
begin
  result := 0;
  for a := low(TFhirNamingSystemTypeEnum) to high(TFhirNamingSystemTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNamingSystemTypeEnumList(i : Integer) : TFhirNamingSystemTypeEnumList;
var
  aLoop : TFhirNamingSystemTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirNamingSystemTypeEnum) to high(TFhirNamingSystemTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNarrativeStatusEnumListAsInteger(aSet : TFhirNarrativeStatusEnumList) : Integer;
var
  a : TFhirNarrativeStatusEnum;
begin
  result := 0;
  for a := low(TFhirNarrativeStatusEnum) to high(TFhirNarrativeStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNarrativeStatusEnumList(i : Integer) : TFhirNarrativeStatusEnumList;
var
  aLoop : TFhirNarrativeStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirNarrativeStatusEnum) to high(TFhirNarrativeStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNoteTypeEnumListAsInteger(aSet : TFhirNoteTypeEnumList) : Integer;
var
  a : TFhirNoteTypeEnum;
begin
  result := 0;
  for a := low(TFhirNoteTypeEnum) to high(TFhirNoteTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNoteTypeEnumList(i : Integer) : TFhirNoteTypeEnumList;
var
  aLoop : TFhirNoteTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirNoteTypeEnum) to high(TFhirNoteTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNutritionProductStatusEnumListAsInteger(aSet : TFhirNutritionProductStatusEnumList) : Integer;
var
  a : TFhirNutritionProductStatusEnum;
begin
  result := 0;
  for a := low(TFhirNutritionProductStatusEnum) to high(TFhirNutritionProductStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNutritionProductStatusEnumList(i : Integer) : TFhirNutritionProductStatusEnumList;
var
  aLoop : TFhirNutritionProductStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirNutritionProductStatusEnum) to high(TFhirNutritionProductStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirObservationDataTypeEnumListAsInteger(aSet : TFhirObservationDataTypeEnumList) : Integer;
var
  a : TFhirObservationDataTypeEnum;
begin
  result := 0;
  for a := low(TFhirObservationDataTypeEnum) to high(TFhirObservationDataTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationDataTypeEnumList(i : Integer) : TFhirObservationDataTypeEnumList;
var
  aLoop : TFhirObservationDataTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirObservationDataTypeEnum) to high(TFhirObservationDataTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirObservationRangeCategoryEnumListAsInteger(aSet : TFhirObservationRangeCategoryEnumList) : Integer;
var
  a : TFhirObservationRangeCategoryEnum;
begin
  result := 0;
  for a := low(TFhirObservationRangeCategoryEnum) to high(TFhirObservationRangeCategoryEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationRangeCategoryEnumList(i : Integer) : TFhirObservationRangeCategoryEnumList;
var
  aLoop : TFhirObservationRangeCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirObservationRangeCategoryEnum) to high(TFhirObservationRangeCategoryEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirObservationStatusEnumListAsInteger(aSet : TFhirObservationStatusEnumList) : Integer;
var
  a : TFhirObservationStatusEnum;
begin
  result := 0;
  for a := low(TFhirObservationStatusEnum) to high(TFhirObservationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationStatusEnumList(i : Integer) : TFhirObservationStatusEnumList;
var
  aLoop : TFhirObservationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirObservationStatusEnum) to high(TFhirObservationStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirOperationKindEnumListAsInteger(aSet : TFhirOperationKindEnumList) : Integer;
var
  a : TFhirOperationKindEnum;
begin
  result := 0;
  for a := low(TFhirOperationKindEnum) to high(TFhirOperationKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOperationKindEnumList(i : Integer) : TFhirOperationKindEnumList;
var
  aLoop : TFhirOperationKindEnum;
begin
  result := [];
  for aLoop := low(TFhirOperationKindEnum) to high(TFhirOperationKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirOperationParameterUseEnumListAsInteger(aSet : TFhirOperationParameterUseEnumList) : Integer;
var
  a : TFhirOperationParameterUseEnum;
begin
  result := 0;
  for a := low(TFhirOperationParameterUseEnum) to high(TFhirOperationParameterUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOperationParameterUseEnumList(i : Integer) : TFhirOperationParameterUseEnumList;
var
  aLoop : TFhirOperationParameterUseEnum;
begin
  result := [];
  for aLoop := low(TFhirOperationParameterUseEnum) to high(TFhirOperationParameterUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirOrientationTypeEnumListAsInteger(aSet : TFhirOrientationTypeEnumList) : Integer;
var
  a : TFhirOrientationTypeEnum;
begin
  result := 0;
  for a := low(TFhirOrientationTypeEnum) to high(TFhirOrientationTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOrientationTypeEnumList(i : Integer) : TFhirOrientationTypeEnumList;
var
  aLoop : TFhirOrientationTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirOrientationTypeEnum) to high(TFhirOrientationTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirParticipantRequiredEnumListAsInteger(aSet : TFhirParticipantRequiredEnumList) : Integer;
var
  a : TFhirParticipantRequiredEnum;
begin
  result := 0;
  for a := low(TFhirParticipantRequiredEnum) to high(TFhirParticipantRequiredEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirParticipantRequiredEnumList(i : Integer) : TFhirParticipantRequiredEnumList;
var
  aLoop : TFhirParticipantRequiredEnum;
begin
  result := [];
  for aLoop := low(TFhirParticipantRequiredEnum) to high(TFhirParticipantRequiredEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirParticipationStatusEnumListAsInteger(aSet : TFhirParticipationStatusEnumList) : Integer;
var
  a : TFhirParticipationStatusEnum;
begin
  result := 0;
  for a := low(TFhirParticipationStatusEnum) to high(TFhirParticipationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirParticipationStatusEnumList(i : Integer) : TFhirParticipationStatusEnumList;
var
  aLoop : TFhirParticipationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirParticipationStatusEnum) to high(TFhirParticipationStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirPermissionStatusEnumListAsInteger(aSet : TFhirPermissionStatusEnumList) : Integer;
var
  a : TFhirPermissionStatusEnum;
begin
  result := 0;
  for a := low(TFhirPermissionStatusEnum) to high(TFhirPermissionStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPermissionStatusEnumList(i : Integer) : TFhirPermissionStatusEnumList;
var
  aLoop : TFhirPermissionStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirPermissionStatusEnum) to high(TFhirPermissionStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirPropertyRepresentationEnumListAsInteger(aSet : TFhirPropertyRepresentationEnumList) : Integer;
var
  a : TFhirPropertyRepresentationEnum;
begin
  result := 0;
  for a := low(TFhirPropertyRepresentationEnum) to high(TFhirPropertyRepresentationEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPropertyRepresentationEnumList(i : Integer) : TFhirPropertyRepresentationEnumList;
var
  aLoop : TFhirPropertyRepresentationEnum;
begin
  result := [];
  for aLoop := low(TFhirPropertyRepresentationEnum) to high(TFhirPropertyRepresentationEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirProvenanceEntityRoleEnumListAsInteger(aSet : TFhirProvenanceEntityRoleEnumList) : Integer;
var
  a : TFhirProvenanceEntityRoleEnum;
begin
  result := 0;
  for a := low(TFhirProvenanceEntityRoleEnum) to high(TFhirProvenanceEntityRoleEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProvenanceEntityRoleEnumList(i : Integer) : TFhirProvenanceEntityRoleEnumList;
var
  aLoop : TFhirProvenanceEntityRoleEnum;
begin
  result := [];
  for aLoop := low(TFhirProvenanceEntityRoleEnum) to high(TFhirProvenanceEntityRoleEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirPublicationStatusEnumListAsInteger(aSet : TFhirPublicationStatusEnumList) : Integer;
var
  a : TFhirPublicationStatusEnum;
begin
  result := 0;
  for a := low(TFhirPublicationStatusEnum) to high(TFhirPublicationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPublicationStatusEnumList(i : Integer) : TFhirPublicationStatusEnumList;
var
  aLoop : TFhirPublicationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirPublicationStatusEnum) to high(TFhirPublicationStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQualityTypeEnumListAsInteger(aSet : TFhirQualityTypeEnumList) : Integer;
var
  a : TFhirQualityTypeEnum;
begin
  result := 0;
  for a := low(TFhirQualityTypeEnum) to high(TFhirQualityTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQualityTypeEnumList(i : Integer) : TFhirQualityTypeEnumList;
var
  aLoop : TFhirQualityTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirQualityTypeEnum) to high(TFhirQualityTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQuantityComparatorEnumListAsInteger(aSet : TFhirQuantityComparatorEnumList) : Integer;
var
  a : TFhirQuantityComparatorEnum;
begin
  result := 0;
  for a := low(TFhirQuantityComparatorEnum) to high(TFhirQuantityComparatorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuantityComparatorEnumList(i : Integer) : TFhirQuantityComparatorEnumList;
var
  aLoop : TFhirQuantityComparatorEnum;
begin
  result := [];
  for aLoop := low(TFhirQuantityComparatorEnum) to high(TFhirQuantityComparatorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQuestionnaireItemOperatorEnumListAsInteger(aSet : TFhirQuestionnaireItemOperatorEnumList) : Integer;
var
  a : TFhirQuestionnaireItemOperatorEnum;
begin
  result := 0;
  for a := low(TFhirQuestionnaireItemOperatorEnum) to high(TFhirQuestionnaireItemOperatorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireItemOperatorEnumList(i : Integer) : TFhirQuestionnaireItemOperatorEnumList;
var
  aLoop : TFhirQuestionnaireItemOperatorEnum;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireItemOperatorEnum) to high(TFhirQuestionnaireItemOperatorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQuestionnaireItemTypeEnumListAsInteger(aSet : TFhirQuestionnaireItemTypeEnumList) : Integer;
var
  a : TFhirQuestionnaireItemTypeEnum;
begin
  result := 0;
  for a := low(TFhirQuestionnaireItemTypeEnum) to high(TFhirQuestionnaireItemTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireItemTypeEnumList(i : Integer) : TFhirQuestionnaireItemTypeEnumList;
var
  aLoop : TFhirQuestionnaireItemTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireItemTypeEnum) to high(TFhirQuestionnaireItemTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQuestionnaireResponseStatusEnumListAsInteger(aSet : TFhirQuestionnaireResponseStatusEnumList) : Integer;
var
  a : TFhirQuestionnaireResponseStatusEnum;
begin
  result := 0;
  for a := low(TFhirQuestionnaireResponseStatusEnum) to high(TFhirQuestionnaireResponseStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireResponseStatusEnumList(i : Integer) : TFhirQuestionnaireResponseStatusEnumList;
var
  aLoop : TFhirQuestionnaireResponseStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireResponseStatusEnum) to high(TFhirQuestionnaireResponseStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirReferenceHandlingPolicyEnumListAsInteger(aSet : TFhirReferenceHandlingPolicyEnumList) : Integer;
var
  a : TFhirReferenceHandlingPolicyEnum;
begin
  result := 0;
  for a := low(TFhirReferenceHandlingPolicyEnum) to high(TFhirReferenceHandlingPolicyEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReferenceHandlingPolicyEnumList(i : Integer) : TFhirReferenceHandlingPolicyEnumList;
var
  aLoop : TFhirReferenceHandlingPolicyEnum;
begin
  result := [];
  for aLoop := low(TFhirReferenceHandlingPolicyEnum) to high(TFhirReferenceHandlingPolicyEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirReferenceVersionRulesEnumListAsInteger(aSet : TFhirReferenceVersionRulesEnumList) : Integer;
var
  a : TFhirReferenceVersionRulesEnum;
begin
  result := 0;
  for a := low(TFhirReferenceVersionRulesEnum) to high(TFhirReferenceVersionRulesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReferenceVersionRulesEnumList(i : Integer) : TFhirReferenceVersionRulesEnumList;
var
  aLoop : TFhirReferenceVersionRulesEnum;
begin
  result := [];
  for aLoop := low(TFhirReferenceVersionRulesEnum) to high(TFhirReferenceVersionRulesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRelatedArtifactTypeEnumListAsInteger(aSet : TFhirRelatedArtifactTypeEnumList) : Integer;
var
  a : TFhirRelatedArtifactTypeEnum;
begin
  result := 0;
  for a := low(TFhirRelatedArtifactTypeEnum) to high(TFhirRelatedArtifactTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRelatedArtifactTypeEnumList(i : Integer) : TFhirRelatedArtifactTypeEnumList;
var
  aLoop : TFhirRelatedArtifactTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirRelatedArtifactTypeEnum) to high(TFhirRelatedArtifactTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirReportRelationshipTypeEnumListAsInteger(aSet : TFhirReportRelationshipTypeEnumList) : Integer;
var
  a : TFhirReportRelationshipTypeEnum;
begin
  result := 0;
  for a := low(TFhirReportRelationshipTypeEnum) to high(TFhirReportRelationshipTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReportRelationshipTypeEnumList(i : Integer) : TFhirReportRelationshipTypeEnumList;
var
  aLoop : TFhirReportRelationshipTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirReportRelationshipTypeEnum) to high(TFhirReportRelationshipTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRepositoryTypeEnumListAsInteger(aSet : TFhirRepositoryTypeEnumList) : Integer;
var
  a : TFhirRepositoryTypeEnum;
begin
  result := 0;
  for a := low(TFhirRepositoryTypeEnum) to high(TFhirRepositoryTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRepositoryTypeEnumList(i : Integer) : TFhirRepositoryTypeEnumList;
var
  aLoop : TFhirRepositoryTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirRepositoryTypeEnum) to high(TFhirRepositoryTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRequestIntentEnumListAsInteger(aSet : TFhirRequestIntentEnumList) : Integer;
var
  a : TFhirRequestIntentEnum;
begin
  result := 0;
  for a := low(TFhirRequestIntentEnum) to high(TFhirRequestIntentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRequestIntentEnumList(i : Integer) : TFhirRequestIntentEnumList;
var
  aLoop : TFhirRequestIntentEnum;
begin
  result := [];
  for aLoop := low(TFhirRequestIntentEnum) to high(TFhirRequestIntentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRequestPriorityEnumListAsInteger(aSet : TFhirRequestPriorityEnumList) : Integer;
var
  a : TFhirRequestPriorityEnum;
begin
  result := 0;
  for a := low(TFhirRequestPriorityEnum) to high(TFhirRequestPriorityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRequestPriorityEnumList(i : Integer) : TFhirRequestPriorityEnumList;
var
  aLoop : TFhirRequestPriorityEnum;
begin
  result := [];
  for aLoop := low(TFhirRequestPriorityEnum) to high(TFhirRequestPriorityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRequestResourceTypeEnumListAsInteger(aSet : TFhirRequestResourceTypeEnumList) : Integer;
var
  a : TFhirRequestResourceTypeEnum;
begin
  result := 0;
  for a := low(TFhirRequestResourceTypeEnum) to high(TFhirRequestResourceTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRequestResourceTypeEnumList(i : Integer) : TFhirRequestResourceTypeEnumList;
var
  aLoop : TFhirRequestResourceTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirRequestResourceTypeEnum) to high(TFhirRequestResourceTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRequestStatusEnumListAsInteger(aSet : TFhirRequestStatusEnumList) : Integer;
var
  a : TFhirRequestStatusEnum;
begin
  result := 0;
  for a := low(TFhirRequestStatusEnum) to high(TFhirRequestStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRequestStatusEnumList(i : Integer) : TFhirRequestStatusEnumList;
var
  aLoop : TFhirRequestStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirRequestStatusEnum) to high(TFhirRequestStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirResearchStudyStatusEnumListAsInteger(aSet : TFhirResearchStudyStatusEnumList) : Integer;
var
  a : TFhirResearchStudyStatusEnum;
begin
  result := 0;
  for a := low(TFhirResearchStudyStatusEnum) to high(TFhirResearchStudyStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResearchStudyStatusEnumList(i : Integer) : TFhirResearchStudyStatusEnumList;
var
  aLoop : TFhirResearchStudyStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirResearchStudyStatusEnum) to high(TFhirResearchStudyStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirResearchSubjectStatusEnumListAsInteger(aSet : TFhirResearchSubjectStatusEnumList) : Integer;
var
  a : TFhirResearchSubjectStatusEnum;
begin
  result := 0;
  for a := low(TFhirResearchSubjectStatusEnum) to high(TFhirResearchSubjectStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResearchSubjectStatusEnumList(i : Integer) : TFhirResearchSubjectStatusEnumList;
var
  aLoop : TFhirResearchSubjectStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirResearchSubjectStatusEnum) to high(TFhirResearchSubjectStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirResourceTypesEnumListAsInteger(aSet : TFhirResourceTypesEnumList) : Integer;
var
  a : TFhirResourceTypesEnum;
begin
  result := 0;
  for a := low(TFhirResourceTypesEnum) to high(TFhirResourceTypesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResourceTypesEnumList(i : Integer) : TFhirResourceTypesEnumList;
var
  aLoop : TFhirResourceTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirResourceTypesEnum) to high(TFhirResourceTypesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirResourceVersionPolicyEnumListAsInteger(aSet : TFhirResourceVersionPolicyEnumList) : Integer;
var
  a : TFhirResourceVersionPolicyEnum;
begin
  result := 0;
  for a := low(TFhirResourceVersionPolicyEnum) to high(TFhirResourceVersionPolicyEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResourceVersionPolicyEnumList(i : Integer) : TFhirResourceVersionPolicyEnumList;
var
  aLoop : TFhirResourceVersionPolicyEnum;
begin
  result := [];
  for aLoop := low(TFhirResourceVersionPolicyEnum) to high(TFhirResourceVersionPolicyEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirResponseTypeEnumListAsInteger(aSet : TFhirResponseTypeEnumList) : Integer;
var
  a : TFhirResponseTypeEnum;
begin
  result := 0;
  for a := low(TFhirResponseTypeEnum) to high(TFhirResponseTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResponseTypeEnumList(i : Integer) : TFhirResponseTypeEnumList;
var
  aLoop : TFhirResponseTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirResponseTypeEnum) to high(TFhirResponseTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRestfulCapabilityModeEnumListAsInteger(aSet : TFhirRestfulCapabilityModeEnumList) : Integer;
var
  a : TFhirRestfulCapabilityModeEnum;
begin
  result := 0;
  for a := low(TFhirRestfulCapabilityModeEnum) to high(TFhirRestfulCapabilityModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRestfulCapabilityModeEnumList(i : Integer) : TFhirRestfulCapabilityModeEnumList;
var
  aLoop : TFhirRestfulCapabilityModeEnum;
begin
  result := [];
  for aLoop := low(TFhirRestfulCapabilityModeEnum) to high(TFhirRestfulCapabilityModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSearchComparatorEnumListAsInteger(aSet : TFhirSearchComparatorEnumList) : Integer;
var
  a : TFhirSearchComparatorEnum;
begin
  result := 0;
  for a := low(TFhirSearchComparatorEnum) to high(TFhirSearchComparatorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchComparatorEnumList(i : Integer) : TFhirSearchComparatorEnumList;
var
  aLoop : TFhirSearchComparatorEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchComparatorEnum) to high(TFhirSearchComparatorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSearchEntryModeEnumListAsInteger(aSet : TFhirSearchEntryModeEnumList) : Integer;
var
  a : TFhirSearchEntryModeEnum;
begin
  result := 0;
  for a := low(TFhirSearchEntryModeEnum) to high(TFhirSearchEntryModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchEntryModeEnumList(i : Integer) : TFhirSearchEntryModeEnumList;
var
  aLoop : TFhirSearchEntryModeEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchEntryModeEnum) to high(TFhirSearchEntryModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSearchModifierCodeEnumListAsInteger(aSet : TFhirSearchModifierCodeEnumList) : Integer;
var
  a : TFhirSearchModifierCodeEnum;
begin
  result := 0;
  for a := low(TFhirSearchModifierCodeEnum) to high(TFhirSearchModifierCodeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchModifierCodeEnumList(i : Integer) : TFhirSearchModifierCodeEnumList;
var
  aLoop : TFhirSearchModifierCodeEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchModifierCodeEnum) to high(TFhirSearchModifierCodeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSearchParamTypeEnumListAsInteger(aSet : TFhirSearchParamTypeEnumList) : Integer;
var
  a : TFhirSearchParamTypeEnum;
begin
  result := 0;
  for a := low(TFhirSearchParamTypeEnum) to high(TFhirSearchParamTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchParamTypeEnumList(i : Integer) : TFhirSearchParamTypeEnumList;
var
  aLoop : TFhirSearchParamTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchParamTypeEnum) to high(TFhirSearchParamTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSequenceTypeEnumListAsInteger(aSet : TFhirSequenceTypeEnumList) : Integer;
var
  a : TFhirSequenceTypeEnum;
begin
  result := 0;
  for a := low(TFhirSequenceTypeEnum) to high(TFhirSequenceTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSequenceTypeEnumList(i : Integer) : TFhirSequenceTypeEnumList;
var
  aLoop : TFhirSequenceTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirSequenceTypeEnum) to high(TFhirSequenceTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSlicingRulesEnumListAsInteger(aSet : TFhirSlicingRulesEnumList) : Integer;
var
  a : TFhirSlicingRulesEnum;
begin
  result := 0;
  for a := low(TFhirSlicingRulesEnum) to high(TFhirSlicingRulesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSlicingRulesEnumList(i : Integer) : TFhirSlicingRulesEnumList;
var
  aLoop : TFhirSlicingRulesEnum;
begin
  result := [];
  for aLoop := low(TFhirSlicingRulesEnum) to high(TFhirSlicingRulesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSlotStatusEnumListAsInteger(aSet : TFhirSlotStatusEnumList) : Integer;
var
  a : TFhirSlotStatusEnum;
begin
  result := 0;
  for a := low(TFhirSlotStatusEnum) to high(TFhirSlotStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSlotStatusEnumList(i : Integer) : TFhirSlotStatusEnumList;
var
  aLoop : TFhirSlotStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSlotStatusEnum) to high(TFhirSlotStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSortDirectionEnumListAsInteger(aSet : TFhirSortDirectionEnumList) : Integer;
var
  a : TFhirSortDirectionEnum;
begin
  result := 0;
  for a := low(TFhirSortDirectionEnum) to high(TFhirSortDirectionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSortDirectionEnumList(i : Integer) : TFhirSortDirectionEnumList;
var
  aLoop : TFhirSortDirectionEnum;
begin
  result := [];
  for aLoop := low(TFhirSortDirectionEnum) to high(TFhirSortDirectionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSpecimenContainedPreferenceEnumListAsInteger(aSet : TFhirSpecimenContainedPreferenceEnumList) : Integer;
var
  a : TFhirSpecimenContainedPreferenceEnum;
begin
  result := 0;
  for a := low(TFhirSpecimenContainedPreferenceEnum) to high(TFhirSpecimenContainedPreferenceEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSpecimenContainedPreferenceEnumList(i : Integer) : TFhirSpecimenContainedPreferenceEnumList;
var
  aLoop : TFhirSpecimenContainedPreferenceEnum;
begin
  result := [];
  for aLoop := low(TFhirSpecimenContainedPreferenceEnum) to high(TFhirSpecimenContainedPreferenceEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSpecimenStatusEnumListAsInteger(aSet : TFhirSpecimenStatusEnumList) : Integer;
var
  a : TFhirSpecimenStatusEnum;
begin
  result := 0;
  for a := low(TFhirSpecimenStatusEnum) to high(TFhirSpecimenStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSpecimenStatusEnumList(i : Integer) : TFhirSpecimenStatusEnumList;
var
  aLoop : TFhirSpecimenStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSpecimenStatusEnum) to high(TFhirSpecimenStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStatusEnumListAsInteger(aSet : TFhirStatusEnumList) : Integer;
var
  a : TFhirStatusEnum;
begin
  result := 0;
  for a := low(TFhirStatusEnum) to high(TFhirStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStatusEnumList(i : Integer) : TFhirStatusEnumList;
var
  aLoop : TFhirStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirStatusEnum) to high(TFhirStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStrandTypeEnumListAsInteger(aSet : TFhirStrandTypeEnumList) : Integer;
var
  a : TFhirStrandTypeEnum;
begin
  result := 0;
  for a := low(TFhirStrandTypeEnum) to high(TFhirStrandTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStrandTypeEnumList(i : Integer) : TFhirStrandTypeEnumList;
var
  aLoop : TFhirStrandTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirStrandTypeEnum) to high(TFhirStrandTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureDefinitionKindEnumListAsInteger(aSet : TFhirStructureDefinitionKindEnumList) : Integer;
var
  a : TFhirStructureDefinitionKindEnum;
begin
  result := 0;
  for a := low(TFhirStructureDefinitionKindEnum) to high(TFhirStructureDefinitionKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureDefinitionKindEnumList(i : Integer) : TFhirStructureDefinitionKindEnumList;
var
  aLoop : TFhirStructureDefinitionKindEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureDefinitionKindEnum) to high(TFhirStructureDefinitionKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapContextTypeEnumListAsInteger(aSet : TFhirStructureMapContextTypeEnumList) : Integer;
var
  a : TFhirStructureMapContextTypeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapContextTypeEnum) to high(TFhirStructureMapContextTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapContextTypeEnumList(i : Integer) : TFhirStructureMapContextTypeEnumList;
var
  aLoop : TFhirStructureMapContextTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapContextTypeEnum) to high(TFhirStructureMapContextTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapGroupTypeModeEnumListAsInteger(aSet : TFhirStructureMapGroupTypeModeEnumList) : Integer;
var
  a : TFhirStructureMapGroupTypeModeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapGroupTypeModeEnum) to high(TFhirStructureMapGroupTypeModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapGroupTypeModeEnumList(i : Integer) : TFhirStructureMapGroupTypeModeEnumList;
var
  aLoop : TFhirStructureMapGroupTypeModeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapGroupTypeModeEnum) to high(TFhirStructureMapGroupTypeModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapInputModeEnumListAsInteger(aSet : TFhirStructureMapInputModeEnumList) : Integer;
var
  a : TFhirStructureMapInputModeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapInputModeEnum) to high(TFhirStructureMapInputModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapInputModeEnumList(i : Integer) : TFhirStructureMapInputModeEnumList;
var
  aLoop : TFhirStructureMapInputModeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapInputModeEnum) to high(TFhirStructureMapInputModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapModelModeEnumListAsInteger(aSet : TFhirStructureMapModelModeEnumList) : Integer;
var
  a : TFhirStructureMapModelModeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapModelModeEnum) to high(TFhirStructureMapModelModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapModelModeEnumList(i : Integer) : TFhirStructureMapModelModeEnumList;
var
  aLoop : TFhirStructureMapModelModeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapModelModeEnum) to high(TFhirStructureMapModelModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapSourceListModeEnumListAsInteger(aSet : TFhirStructureMapSourceListModeEnumList) : Integer;
var
  a : TFhirStructureMapSourceListModeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapSourceListModeEnum) to high(TFhirStructureMapSourceListModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapSourceListModeEnumList(i : Integer) : TFhirStructureMapSourceListModeEnumList;
var
  aLoop : TFhirStructureMapSourceListModeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapSourceListModeEnum) to high(TFhirStructureMapSourceListModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapTargetListModeEnumListAsInteger(aSet : TFhirStructureMapTargetListModeEnumList) : Integer;
var
  a : TFhirStructureMapTargetListModeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapTargetListModeEnum) to high(TFhirStructureMapTargetListModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapTargetListModeEnumList(i : Integer) : TFhirStructureMapTargetListModeEnumList;
var
  aLoop : TFhirStructureMapTargetListModeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapTargetListModeEnum) to high(TFhirStructureMapTargetListModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapTransformEnumListAsInteger(aSet : TFhirStructureMapTransformEnumList) : Integer;
var
  a : TFhirStructureMapTransformEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapTransformEnum) to high(TFhirStructureMapTransformEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapTransformEnumList(i : Integer) : TFhirStructureMapTransformEnumList;
var
  aLoop : TFhirStructureMapTransformEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapTransformEnum) to high(TFhirStructureMapTransformEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSubscriptionNotificationTypeEnumListAsInteger(aSet : TFhirSubscriptionNotificationTypeEnumList) : Integer;
var
  a : TFhirSubscriptionNotificationTypeEnum;
begin
  result := 0;
  for a := low(TFhirSubscriptionNotificationTypeEnum) to high(TFhirSubscriptionNotificationTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionNotificationTypeEnumList(i : Integer) : TFhirSubscriptionNotificationTypeEnumList;
var
  aLoop : TFhirSubscriptionNotificationTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionNotificationTypeEnum) to high(TFhirSubscriptionNotificationTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSubscriptionPayloadContentEnumListAsInteger(aSet : TFhirSubscriptionPayloadContentEnumList) : Integer;
var
  a : TFhirSubscriptionPayloadContentEnum;
begin
  result := 0;
  for a := low(TFhirSubscriptionPayloadContentEnum) to high(TFhirSubscriptionPayloadContentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionPayloadContentEnumList(i : Integer) : TFhirSubscriptionPayloadContentEnumList;
var
  aLoop : TFhirSubscriptionPayloadContentEnum;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionPayloadContentEnum) to high(TFhirSubscriptionPayloadContentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSubscriptionSearchModifierEnumListAsInteger(aSet : TFhirSubscriptionSearchModifierEnumList) : Integer;
var
  a : TFhirSubscriptionSearchModifierEnum;
begin
  result := 0;
  for a := low(TFhirSubscriptionSearchModifierEnum) to high(TFhirSubscriptionSearchModifierEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionSearchModifierEnumList(i : Integer) : TFhirSubscriptionSearchModifierEnumList;
var
  aLoop : TFhirSubscriptionSearchModifierEnum;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionSearchModifierEnum) to high(TFhirSubscriptionSearchModifierEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSubscriptionStateEnumListAsInteger(aSet : TFhirSubscriptionStateEnumList) : Integer;
var
  a : TFhirSubscriptionStateEnum;
begin
  result := 0;
  for a := low(TFhirSubscriptionStateEnum) to high(TFhirSubscriptionStateEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionStateEnumList(i : Integer) : TFhirSubscriptionStateEnumList;
var
  aLoop : TFhirSubscriptionStateEnum;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionStateEnum) to high(TFhirSubscriptionStateEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSupplyDeliveryStatusEnumListAsInteger(aSet : TFhirSupplyDeliveryStatusEnumList) : Integer;
var
  a : TFhirSupplyDeliveryStatusEnum;
begin
  result := 0;
  for a := low(TFhirSupplyDeliveryStatusEnum) to high(TFhirSupplyDeliveryStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSupplyDeliveryStatusEnumList(i : Integer) : TFhirSupplyDeliveryStatusEnumList;
var
  aLoop : TFhirSupplyDeliveryStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSupplyDeliveryStatusEnum) to high(TFhirSupplyDeliveryStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSupplyRequestStatusEnumListAsInteger(aSet : TFhirSupplyRequestStatusEnumList) : Integer;
var
  a : TFhirSupplyRequestStatusEnum;
begin
  result := 0;
  for a := low(TFhirSupplyRequestStatusEnum) to high(TFhirSupplyRequestStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSupplyRequestStatusEnumList(i : Integer) : TFhirSupplyRequestStatusEnumList;
var
  aLoop : TFhirSupplyRequestStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSupplyRequestStatusEnum) to high(TFhirSupplyRequestStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSystemRestfulInteractionEnumListAsInteger(aSet : TFhirSystemRestfulInteractionEnumList) : Integer;
var
  a : TFhirSystemRestfulInteractionEnum;
begin
  result := 0;
  for a := low(TFhirSystemRestfulInteractionEnum) to high(TFhirSystemRestfulInteractionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSystemRestfulInteractionEnumList(i : Integer) : TFhirSystemRestfulInteractionEnumList;
var
  aLoop : TFhirSystemRestfulInteractionEnum;
begin
  result := [];
  for aLoop := low(TFhirSystemRestfulInteractionEnum) to high(TFhirSystemRestfulInteractionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTaskIntentEnumListAsInteger(aSet : TFhirTaskIntentEnumList) : Integer;
var
  a : TFhirTaskIntentEnum;
begin
  result := 0;
  for a := low(TFhirTaskIntentEnum) to high(TFhirTaskIntentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTaskIntentEnumList(i : Integer) : TFhirTaskIntentEnumList;
var
  aLoop : TFhirTaskIntentEnum;
begin
  result := [];
  for aLoop := low(TFhirTaskIntentEnum) to high(TFhirTaskIntentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTaskStatusEnumListAsInteger(aSet : TFhirTaskStatusEnumList) : Integer;
var
  a : TFhirTaskStatusEnum;
begin
  result := 0;
  for a := low(TFhirTaskStatusEnum) to high(TFhirTaskStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTaskStatusEnumList(i : Integer) : TFhirTaskStatusEnumList;
var
  aLoop : TFhirTaskStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirTaskStatusEnum) to high(TFhirTaskStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTestReportActionResultEnumListAsInteger(aSet : TFhirTestReportActionResultEnumList) : Integer;
var
  a : TFhirTestReportActionResultEnum;
begin
  result := 0;
  for a := low(TFhirTestReportActionResultEnum) to high(TFhirTestReportActionResultEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTestReportActionResultEnumList(i : Integer) : TFhirTestReportActionResultEnumList;
var
  aLoop : TFhirTestReportActionResultEnum;
begin
  result := [];
  for aLoop := low(TFhirTestReportActionResultEnum) to high(TFhirTestReportActionResultEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTestReportParticipantTypeEnumListAsInteger(aSet : TFhirTestReportParticipantTypeEnumList) : Integer;
var
  a : TFhirTestReportParticipantTypeEnum;
begin
  result := 0;
  for a := low(TFhirTestReportParticipantTypeEnum) to high(TFhirTestReportParticipantTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTestReportParticipantTypeEnumList(i : Integer) : TFhirTestReportParticipantTypeEnumList;
var
  aLoop : TFhirTestReportParticipantTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirTestReportParticipantTypeEnum) to high(TFhirTestReportParticipantTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTestReportResultEnumListAsInteger(aSet : TFhirTestReportResultEnumList) : Integer;
var
  a : TFhirTestReportResultEnum;
begin
  result := 0;
  for a := low(TFhirTestReportResultEnum) to high(TFhirTestReportResultEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTestReportResultEnumList(i : Integer) : TFhirTestReportResultEnumList;
var
  aLoop : TFhirTestReportResultEnum;
begin
  result := [];
  for aLoop := low(TFhirTestReportResultEnum) to high(TFhirTestReportResultEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTestReportStatusEnumListAsInteger(aSet : TFhirTestReportStatusEnumList) : Integer;
var
  a : TFhirTestReportStatusEnum;
begin
  result := 0;
  for a := low(TFhirTestReportStatusEnum) to high(TFhirTestReportStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTestReportStatusEnumList(i : Integer) : TFhirTestReportStatusEnumList;
var
  aLoop : TFhirTestReportStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirTestReportStatusEnum) to high(TFhirTestReportStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTestScriptRequestMethodCodeEnumListAsInteger(aSet : TFhirTestScriptRequestMethodCodeEnumList) : Integer;
var
  a : TFhirTestScriptRequestMethodCodeEnum;
begin
  result := 0;
  for a := low(TFhirTestScriptRequestMethodCodeEnum) to high(TFhirTestScriptRequestMethodCodeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTestScriptRequestMethodCodeEnumList(i : Integer) : TFhirTestScriptRequestMethodCodeEnumList;
var
  aLoop : TFhirTestScriptRequestMethodCodeEnum;
begin
  result := [];
  for aLoop := low(TFhirTestScriptRequestMethodCodeEnum) to high(TFhirTestScriptRequestMethodCodeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTriggerTypeEnumListAsInteger(aSet : TFhirTriggerTypeEnumList) : Integer;
var
  a : TFhirTriggerTypeEnum;
begin
  result := 0;
  for a := low(TFhirTriggerTypeEnum) to high(TFhirTriggerTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTriggerTypeEnumList(i : Integer) : TFhirTriggerTypeEnumList;
var
  aLoop : TFhirTriggerTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirTriggerTypeEnum) to high(TFhirTriggerTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTypeDerivationRuleEnumListAsInteger(aSet : TFhirTypeDerivationRuleEnumList) : Integer;
var
  a : TFhirTypeDerivationRuleEnum;
begin
  result := 0;
  for a := low(TFhirTypeDerivationRuleEnum) to high(TFhirTypeDerivationRuleEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTypeDerivationRuleEnumList(i : Integer) : TFhirTypeDerivationRuleEnumList;
var
  aLoop : TFhirTypeDerivationRuleEnum;
begin
  result := [];
  for aLoop := low(TFhirTypeDerivationRuleEnum) to high(TFhirTypeDerivationRuleEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTypeRestfulInteractionEnumListAsInteger(aSet : TFhirTypeRestfulInteractionEnumList) : Integer;
var
  a : TFhirTypeRestfulInteractionEnum;
begin
  result := 0;
  for a := low(TFhirTypeRestfulInteractionEnum) to high(TFhirTypeRestfulInteractionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTypeRestfulInteractionEnumList(i : Integer) : TFhirTypeRestfulInteractionEnumList;
var
  aLoop : TFhirTypeRestfulInteractionEnum;
begin
  result := [];
  for aLoop := low(TFhirTypeRestfulInteractionEnum) to high(TFhirTypeRestfulInteractionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirUDIEntryTypeEnumListAsInteger(aSet : TFhirUDIEntryTypeEnumList) : Integer;
var
  a : TFhirUDIEntryTypeEnum;
begin
  result := 0;
  for a := low(TFhirUDIEntryTypeEnum) to high(TFhirUDIEntryTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirUDIEntryTypeEnumList(i : Integer) : TFhirUDIEntryTypeEnumList;
var
  aLoop : TFhirUDIEntryTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirUDIEntryTypeEnum) to high(TFhirUDIEntryTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirUnitsOfTimeEnumListAsInteger(aSet : TFhirUnitsOfTimeEnumList) : Integer;
var
  a : TFhirUnitsOfTimeEnum;
begin
  result := 0;
  for a := low(TFhirUnitsOfTimeEnum) to high(TFhirUnitsOfTimeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirUnitsOfTimeEnumList(i : Integer) : TFhirUnitsOfTimeEnumList;
var
  aLoop : TFhirUnitsOfTimeEnum;
begin
  result := [];
  for aLoop := low(TFhirUnitsOfTimeEnum) to high(TFhirUnitsOfTimeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirUseEnumListAsInteger(aSet : TFhirUseEnumList) : Integer;
var
  a : TFhirUseEnum;
begin
  result := 0;
  for a := low(TFhirUseEnum) to high(TFhirUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirUseEnumList(i : Integer) : TFhirUseEnumList;
var
  aLoop : TFhirUseEnum;
begin
  result := [];
  for aLoop := low(TFhirUseEnum) to high(TFhirUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirVisionBaseEnumListAsInteger(aSet : TFhirVisionBaseEnumList) : Integer;
var
  a : TFhirVisionBaseEnum;
begin
  result := 0;
  for a := low(TFhirVisionBaseEnum) to high(TFhirVisionBaseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirVisionBaseEnumList(i : Integer) : TFhirVisionBaseEnumList;
var
  aLoop : TFhirVisionBaseEnum;
begin
  result := [];
  for aLoop := low(TFhirVisionBaseEnum) to high(TFhirVisionBaseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirVisionEyesEnumListAsInteger(aSet : TFhirVisionEyesEnumList) : Integer;
var
  a : TFhirVisionEyesEnum;
begin
  result := 0;
  for a := low(TFhirVisionEyesEnum) to high(TFhirVisionEyesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirVisionEyesEnumList(i : Integer) : TFhirVisionEyesEnumList;
var
  aLoop : TFhirVisionEyesEnum;
begin
  result := [];
  for aLoop := low(TFhirVisionEyesEnum) to high(TFhirVisionEyesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirXPathUsageTypeEnumListAsInteger(aSet : TFhirXPathUsageTypeEnumList) : Integer;
var
  a : TFhirXPathUsageTypeEnum;
begin
  result := 0;
  for a := low(TFhirXPathUsageTypeEnum) to high(TFhirXPathUsageTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirXPathUsageTypeEnumList(i : Integer) : TFhirXPathUsageTypeEnumList;
var
  aLoop : TFhirXPathUsageTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirXPathUsageTypeEnum) to high(TFhirXPathUsageTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

end.

