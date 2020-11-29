unit fhir2_types;

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
{$I fhir2.inc}

interface

// FHIR v1.0.2 generated 2015-10-24T07:41:03+11:00

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_crypto, fsl_stream,
  fhir_objects, fhir_xhtml, 
  fhir2_base;

Type
  // The status of a resource narrative from http://hl7.org/fhir/ValueSet/narrative-status
  TFhirNarrativeStatusEnum = (
    NarrativeStatusNull, // Value is missing from Instance 
    NarrativeStatusGenerated, 
    NarrativeStatusExtensions, 
    NarrativeStatusAdditional, 
    NarrativeStatusEmpty); 
  TFhirNarrativeStatusEnumList = set of TFhirNarrativeStatusEnum;

  // Identifies the purpose for this identifier, if known . from http://hl7.org/fhir/ValueSet/identifier-use
  TFhirIdentifierUseEnum = (
    IdentifierUseNull, // Value is missing from Instance 
    IdentifierUseUsual, 
    IdentifierUseOfficial, 
    IdentifierUseTemp, 
    IdentifierUseSecondary); 
  TFhirIdentifierUseEnumList = set of TFhirIdentifierUseEnum;

  // How the Quantity should be understood and represented. from http://hl7.org/fhir/ValueSet/quantity-comparator
  TFhirQuantityComparatorEnum = (
    QuantityComparatorNull, // Value is missing from Instance 
    QuantityComparatorLessThan, 
    QuantityComparatorLessOrEquals, 
    QuantityComparatorGreaterOrEquals, 
    QuantityComparatorGreaterThan); 
  TFhirQuantityComparatorEnumList = set of TFhirQuantityComparatorEnum;

  // The use of a human name from http://hl7.org/fhir/ValueSet/name-use
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

  // Telecommunications form for contact point from http://hl7.org/fhir/ValueSet/contact-point-system
  TFhirContactPointSystemEnum = (
    ContactPointSystemNull, // Value is missing from Instance 
    ContactPointSystemPhone, 
    ContactPointSystemFax, 
    ContactPointSystemEmail, 
    ContactPointSystemPager, 
    ContactPointSystemOther); 
  TFhirContactPointSystemEnumList = set of TFhirContactPointSystemEnum;

  // Use of contact point from http://hl7.org/fhir/ValueSet/contact-point-use
  TFhirContactPointUseEnum = (
    ContactPointUseNull, // Value is missing from Instance 
    ContactPointUseHome, 
    ContactPointUseWork, 
    ContactPointUseTemp, 
    ContactPointUseOld, 
    ContactPointUseMobile); 
  TFhirContactPointUseEnumList = set of TFhirContactPointUseEnum;

  // The use of an address from http://hl7.org/fhir/ValueSet/address-use
  TFhirAddressUseEnum = (
    AddressUseNull, // Value is missing from Instance 
    AddressUseHome, 
    AddressUseWork, 
    AddressUseTemp, 
    AddressUseOld); 
  TFhirAddressUseEnumList = set of TFhirAddressUseEnum;

  // The type of an address (physical / postal) from http://hl7.org/fhir/ValueSet/address-type
  TFhirAddressTypeEnum = (
    AddressTypeNull, // Value is missing from Instance 
    AddressTypePostal, 
    AddressTypePhysical, 
    AddressTypeBoth); 
  TFhirAddressTypeEnumList = set of TFhirAddressTypeEnum;

  // How a property is represented on the wire. from http://hl7.org/fhir/ValueSet/property-representation
  TFhirPropertyRepresentationEnum = (
    PropertyRepresentationNull, // Value is missing from Instance 
    PropertyRepresentationXmlAttr); 
  TFhirPropertyRepresentationEnumList = set of TFhirPropertyRepresentationEnum;

  // How slices are interpreted when evaluating an instance. from http://hl7.org/fhir/ValueSet/resource-slicing-rules
  TFhirResourceSlicingRulesEnum = (
    ResourceSlicingRulesNull, // Value is missing from Instance 
    ResourceSlicingRulesClosed, 
    ResourceSlicingRulesOpen, 
    ResourceSlicingRulesOpenAtEnd); 
  TFhirResourceSlicingRulesEnumList = set of TFhirResourceSlicingRulesEnum;

  // How resource references can be aggregated. from http://hl7.org/fhir/ValueSet/resource-aggregation-mode
  TFhirResourceAggregationModeEnum = (
    ResourceAggregationModeNull, // Value is missing from Instance 
    ResourceAggregationModeContained, 
    ResourceAggregationModeReferenced, 
    ResourceAggregationModeBundled); 
  TFhirResourceAggregationModeEnumList = set of TFhirResourceAggregationModeEnum;

  // SHALL applications comply with this constraint? from http://hl7.org/fhir/ValueSet/constraint-severity
  TFhirConstraintSeverityEnum = (
    ConstraintSeverityNull, // Value is missing from Instance 
    ConstraintSeverityError, 
    ConstraintSeverityWarning); 
  TFhirConstraintSeverityEnumList = set of TFhirConstraintSeverityEnum;

  // Indication of the degree of conformance expectations associated with a binding. from http://hl7.org/fhir/ValueSet/binding-strength
  TFhirBindingStrengthEnum = (
    BindingStrengthNull, // Value is missing from Instance 
    BindingStrengthRequired, 
    BindingStrengthExtensible, 
    BindingStrengthPreferred, 
    BindingStrengthExample); 
  TFhirBindingStrengthEnumList = set of TFhirBindingStrengthEnum;

  // A unit of time (units from UCUM). from http://hl7.org/fhir/ValueSet/units-of-time
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

  // Real world event that the relating to the schedule. from http://hl7.org/fhir/ValueSet/event-timing
  TFhirEventTimingEnum = (
    EventTimingNull, // Value is missing from Instance 
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

  // Assertion about certainty associated with a propensity, or potential risk, of a reaction to the identified Substance. from http://hl7.org/fhir/ValueSet/allergy-intolerance-status
  TFhirAllergyIntoleranceStatusEnum = (
    AllergyIntoleranceStatusNull, // Value is missing from Instance 
    AllergyIntoleranceStatusActive, 
    AllergyIntoleranceStatusUnconfirmed, 
    AllergyIntoleranceStatusConfirmed, 
    AllergyIntoleranceStatusInactive, 
    AllergyIntoleranceStatusResolved, 
    AllergyIntoleranceStatusRefuted, 
    AllergyIntoleranceStatusEnteredInError); 
  TFhirAllergyIntoleranceStatusEnumList = set of TFhirAllergyIntoleranceStatusEnum;

  // Estimate of the potential clinical harm, or seriousness, of a reaction to an identified Substance. from http://hl7.org/fhir/ValueSet/allergy-intolerance-criticality
  TFhirAllergyIntoleranceCriticalityEnum = (
    AllergyIntoleranceCriticalityNull, // Value is missing from Instance 
    AllergyIntoleranceCriticalityCRITL, 
    AllergyIntoleranceCriticalityCRITH, 
    AllergyIntoleranceCriticalityCRITU); 
  TFhirAllergyIntoleranceCriticalityEnumList = set of TFhirAllergyIntoleranceCriticalityEnum;

  // Identification of the underlying physiological mechanism for a Reaction Risk. from http://hl7.org/fhir/ValueSet/allergy-intolerance-type
  TFhirAllergyIntoleranceTypeEnum = (
    AllergyIntoleranceTypeNull, // Value is missing from Instance 
    AllergyIntoleranceTypeAllergy, 
    AllergyIntoleranceTypeIntolerance); 
  TFhirAllergyIntoleranceTypeEnumList = set of TFhirAllergyIntoleranceTypeEnum;

  // Category of an identified Substance. from http://hl7.org/fhir/ValueSet/allergy-intolerance-category
  TFhirAllergyIntoleranceCategoryEnum = (
    AllergyIntoleranceCategoryNull, // Value is missing from Instance 
    AllergyIntoleranceCategoryFood, 
    AllergyIntoleranceCategoryMedication, 
    AllergyIntoleranceCategoryEnvironment, 
    AllergyIntoleranceCategoryOther); 
  TFhirAllergyIntoleranceCategoryEnumList = set of TFhirAllergyIntoleranceCategoryEnum;

  // Statement about the degree of clinical certainty that a Specific Substance was the cause of the Manifestation in an reaction event. from http://hl7.org/fhir/ValueSet/reaction-event-certainty
  TFhirReactionEventCertaintyEnum = (
    ReactionEventCertaintyNull, // Value is missing from Instance 
    ReactionEventCertaintyUnlikely, 
    ReactionEventCertaintyLikely, 
    ReactionEventCertaintyConfirmed); 
  TFhirReactionEventCertaintyEnumList = set of TFhirReactionEventCertaintyEnum;

  // Clinical assessment of the severity of a reaction event as a whole, potentially considering multiple different manifestations. from http://hl7.org/fhir/ValueSet/reaction-event-severity
  TFhirReactionEventSeverityEnum = (
    ReactionEventSeverityNull, // Value is missing from Instance 
    ReactionEventSeverityMild, 
    ReactionEventSeverityModerate, 
    ReactionEventSeveritySevere); 
  TFhirReactionEventSeverityEnumList = set of TFhirReactionEventSeverityEnum;

  // The free/busy status of an appointment. from http://hl7.org/fhir/ValueSet/appointmentstatus
  TFhirAppointmentstatusEnum = (
    AppointmentstatusNull, // Value is missing from Instance 
    AppointmentstatusProposed, 
    AppointmentstatusPending, 
    AppointmentstatusBooked, 
    AppointmentstatusArrived, 
    AppointmentstatusFulfilled, 
    AppointmentstatusCancelled, 
    AppointmentstatusNoshow); 
  TFhirAppointmentstatusEnumList = set of TFhirAppointmentstatusEnum;

  // Is the Participant required to attend the appointment. from http://hl7.org/fhir/ValueSet/participantrequired
  TFhirParticipantrequiredEnum = (
    ParticipantrequiredNull, // Value is missing from Instance 
    ParticipantrequiredRequired, 
    ParticipantrequiredOptional, 
    ParticipantrequiredInformationOnly); 
  TFhirParticipantrequiredEnumList = set of TFhirParticipantrequiredEnum;

  // The Participation status of an appointment. from http://hl7.org/fhir/ValueSet/participationstatus
  TFhirParticipationstatusEnum = (
    ParticipationstatusNull, // Value is missing from Instance 
    ParticipationstatusAccepted, 
    ParticipationstatusDeclined, 
    ParticipationstatusTentative, 
    ParticipationstatusNeedsAction); 
  TFhirParticipationstatusEnumList = set of TFhirParticipationstatusEnum;

  // The Participation status of an appointment. from http://hl7.org/fhir/ValueSet/participantstatus
  TFhirParticipantstatusEnum = (
    ParticipantstatusNull, // Value is missing from Instance 
    ParticipantstatusAccepted, 
    ParticipantstatusDeclined, 
    ParticipantstatusTentative, 
    ParticipantstatusInProcess, 
    ParticipantstatusCompleted, 
    ParticipantstatusNeedsAction); 
  TFhirParticipantstatusEnumList = set of TFhirParticipantstatusEnum;

  // Indicator for type of action performed during the event that generated the audit. from http://hl7.org/fhir/ValueSet/audit-event-action
  TFhirAuditEventActionEnum = (
    AuditEventActionNull, // Value is missing from Instance 
    AuditEventActionC, 
    AuditEventActionR, 
    AuditEventActionU, 
    AuditEventActionD, 
    AuditEventActionE); 
  TFhirAuditEventActionEnumList = set of TFhirAuditEventActionEnum;

  // Indicates whether the event succeeded or failed from http://hl7.org/fhir/ValueSet/audit-event-outcome
  TFhirAuditEventOutcomeEnum = (
    AuditEventOutcomeNull, // Value is missing from Instance 
    AuditEventOutcome0, 
    AuditEventOutcome4, 
    AuditEventOutcome8, 
    AuditEventOutcome12); 
  TFhirAuditEventOutcomeEnumList = set of TFhirAuditEventOutcomeEnum;

  // The type of network access point of this participant in the audit event from http://hl7.org/fhir/ValueSet/network-type
  TFhirNetworkTypeEnum = (
    NetworkTypeNull, // Value is missing from Instance 
    NetworkType1, 
    NetworkType2, 
    NetworkType3, 
    NetworkType4, 
    NetworkType5); 
  TFhirNetworkTypeEnumList = set of TFhirNetworkTypeEnum;

  // Indicates the purpose of a bundle - how it was intended to be used. from http://hl7.org/fhir/ValueSet/bundle-type
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
    BundleTypeCollection); 
  TFhirBundleTypeEnumList = set of TFhirBundleTypeEnum;

  // Why an entry is in the result set - whether it's included as a match or because of an _include requirement. from http://hl7.org/fhir/ValueSet/search-entry-mode
  TFhirSearchEntryModeEnum = (
    SearchEntryModeNull, // Value is missing from Instance 
    SearchEntryModeMatch, 
    SearchEntryModeInclude, 
    SearchEntryModeOutcome); 
  TFhirSearchEntryModeEnumList = set of TFhirSearchEntryModeEnum;

  // HTTP verbs (in the HTTP command line). from http://hl7.org/fhir/ValueSet/http-verb
  TFhirHttpVerbEnum = (
    HttpVerbNull, // Value is missing from Instance 
    HttpVerbGET, 
    HttpVerbPOST, 
    HttpVerbPUT, 
    HttpVerbDELETE); 
  TFhirHttpVerbEnumList = set of TFhirHttpVerbEnum;

  // Indicates whether the plan is currently being acted upon, represents future intentions or is now a historical record. from http://hl7.org/fhir/ValueSet/care-plan-status
  TFhirCarePlanStatusEnum = (
    CarePlanStatusNull, // Value is missing from Instance 
    CarePlanStatusProposed, 
    CarePlanStatusDraft, 
    CarePlanStatusActive, 
    CarePlanStatusCompleted, 
    CarePlanStatusCancelled); 
  TFhirCarePlanStatusEnumList = set of TFhirCarePlanStatusEnum;

  // Codes identifying the types of relationships between two plans. from http://hl7.org/fhir/ValueSet/care-plan-relationship
  TFhirCarePlanRelationshipEnum = (
    CarePlanRelationshipNull, // Value is missing from Instance 
    CarePlanRelationshipIncludes, 
    CarePlanRelationshipReplaces, 
    CarePlanRelationshipFulfills); 
  TFhirCarePlanRelationshipEnumList = set of TFhirCarePlanRelationshipEnum;

  // Indicates where the activity is at in its overall life cycle. from http://hl7.org/fhir/ValueSet/care-plan-activity-status
  TFhirCarePlanActivityStatusEnum = (
    CarePlanActivityStatusNull, // Value is missing from Instance 
    CarePlanActivityStatusNotStarted, 
    CarePlanActivityStatusScheduled, 
    CarePlanActivityStatusInProgress, 
    CarePlanActivityStatusOnHold, 
    CarePlanActivityStatusCompleted, 
    CarePlanActivityStatusCancelled); 
  TFhirCarePlanActivityStatusEnumList = set of TFhirCarePlanActivityStatusEnum;

  // The type or discipline-style of the claim. from http://hl7.org/fhir/ValueSet/claim-type-link
  TFhirClaimTypeLinkEnum = (
    ClaimTypeLinkNull, // Value is missing from Instance 
    ClaimTypeLinkInstitutional, 
    ClaimTypeLinkOral, 
    ClaimTypeLinkPharmacy, 
    ClaimTypeLinkProfessional, 
    ClaimTypeLinkVision); 
  TFhirClaimTypeLinkEnumList = set of TFhirClaimTypeLinkEnum;

  // Complete, proposed, exploratory, other. from http://hl7.org/fhir/ValueSet/claim-use-link
  TFhirClaimUseLinkEnum = (
    ClaimUseLinkNull, // Value is missing from Instance 
    ClaimUseLinkComplete, 
    ClaimUseLinkProposed, 
    ClaimUseLinkExploratory, 
    ClaimUseLinkOther); 
  TFhirClaimUseLinkEnumList = set of TFhirClaimUseLinkEnum;

  // The outcome of the processing. from http://hl7.org/fhir/ValueSet/remittance-outcome
  TFhirRemittanceOutcomeEnum = (
    RemittanceOutcomeNull, // Value is missing from Instance 
    RemittanceOutcomeComplete, 
    RemittanceOutcomeError); 
  TFhirRemittanceOutcomeEnumList = set of TFhirRemittanceOutcomeEnum;

  // The workflow state of a clinical impression. from http://hl7.org/fhir/ValueSet/clinical-impression-status
  TFhirClinicalImpressionStatusEnum = (
    ClinicalImpressionStatusNull, // Value is missing from Instance 
    ClinicalImpressionStatusInProgress, 
    ClinicalImpressionStatusCompleted, 
    ClinicalImpressionStatusEnteredInError); 
  TFhirClinicalImpressionStatusEnumList = set of TFhirClinicalImpressionStatusEnum;

  // The status of the communication. from http://hl7.org/fhir/ValueSet/communication-status
  TFhirCommunicationStatusEnum = (
    CommunicationStatusNull, // Value is missing from Instance 
    CommunicationStatusInProgress, 
    CommunicationStatusCompleted, 
    CommunicationStatusSuspended, 
    CommunicationStatusRejected, 
    CommunicationStatusFailed); 
  TFhirCommunicationStatusEnumList = set of TFhirCommunicationStatusEnum;

  // The status of the communication. from http://hl7.org/fhir/ValueSet/communication-request-status
  TFhirCommunicationRequestStatusEnum = (
    CommunicationRequestStatusNull, // Value is missing from Instance 
    CommunicationRequestStatusProposed, 
    CommunicationRequestStatusPlanned, 
    CommunicationRequestStatusRequested, 
    CommunicationRequestStatusReceived, 
    CommunicationRequestStatusAccepted, 
    CommunicationRequestStatusInProgress, 
    CommunicationRequestStatusCompleted, 
    CommunicationRequestStatusSuspended, 
    CommunicationRequestStatusRejected, 
    CommunicationRequestStatusFailed); 
  TFhirCommunicationRequestStatusEnumList = set of TFhirCommunicationRequestStatusEnum;

  // The workflow/clinical status of the composition. from http://hl7.org/fhir/ValueSet/composition-status
  TFhirCompositionStatusEnum = (
    CompositionStatusNull, // Value is missing from Instance 
    CompositionStatusPreliminary, 
    CompositionStatusFinal, 
    CompositionStatusAmended, 
    CompositionStatusEnteredInError); 
  TFhirCompositionStatusEnumList = set of TFhirCompositionStatusEnum;

  // Codes specifying the level of confidentiality of the composition. from http://hl7.org/fhir/ValueSet/v3-Confidentiality
  TFhirV3ConfidentialityEnum = (
    V3ConfidentialityNull, // Value is missing from Instance 
    V3Confidentiality_Confidentiality, 
    V3ConfidentialityL, 
    V3ConfidentialityM, 
    V3ConfidentialityN, 
    V3ConfidentialityR, 
    V3ConfidentialityU, 
    V3ConfidentialityV); 
  TFhirV3ConfidentialityEnumList = set of TFhirV3ConfidentialityEnum;

  // The way in which a person authenticated a composition. from http://hl7.org/fhir/ValueSet/composition-attestation-mode
  TFhirCompositionAttestationModeEnum = (
    CompositionAttestationModeNull, // Value is missing from Instance 
    CompositionAttestationModePersonal, 
    CompositionAttestationModeProfessional, 
    CompositionAttestationModeLegal, 
    CompositionAttestationModeOfficial); 
  TFhirCompositionAttestationModeEnumList = set of TFhirCompositionAttestationModeEnum;

  // The processing mode that applies to this section. from http://hl7.org/fhir/ValueSet/list-mode
  TFhirListModeEnum = (
    ListModeNull, // Value is missing from Instance 
    ListModeWorking, 
    ListModeSnapshot, 
    ListModeChanges); 
  TFhirListModeEnumList = set of TFhirListModeEnum;

  // The lifecycle status of a Value Set or Concept Map. from http://hl7.org/fhir/ValueSet/conformance-resource-status
  TFhirConformanceResourceStatusEnum = (
    ConformanceResourceStatusNull, // Value is missing from Instance 
    ConformanceResourceStatusDraft, 
    ConformanceResourceStatusActive, 
    ConformanceResourceStatusRetired); 
  TFhirConformanceResourceStatusEnumList = set of TFhirConformanceResourceStatusEnum;

  // The degree of equivalence between concepts. from http://hl7.org/fhir/ValueSet/concept-map-equivalence
  TFhirConceptMapEquivalenceEnum = (
    ConceptMapEquivalenceNull, // Value is missing from Instance 
    ConceptMapEquivalenceEquivalent, 
    ConceptMapEquivalenceEqual, 
    ConceptMapEquivalenceWider, 
    ConceptMapEquivalenceSubsumes, 
    ConceptMapEquivalenceNarrower, 
    ConceptMapEquivalenceSpecializes, 
    ConceptMapEquivalenceInexact, 
    ConceptMapEquivalenceUnmatched, 
    ConceptMapEquivalenceDisjoint); 
  TFhirConceptMapEquivalenceEnumList = set of TFhirConceptMapEquivalenceEnum;

  // The verification status to support or decline the clinical status of the condition or diagnosis. from http://hl7.org/fhir/ValueSet/condition-ver-status
  TFhirConditionVerStatusEnum = (
    ConditionVerStatusNull, // Value is missing from Instance 
    ConditionVerStatusProvisional, 
    ConditionVerStatusDifferential, 
    ConditionVerStatusConfirmed, 
    ConditionVerStatusRefuted, 
    ConditionVerStatusEnteredInError, 
    ConditionVerStatusUnknown); 
  TFhirConditionVerStatusEnumList = set of TFhirConditionVerStatusEnum;

  // How a conformance statement is intended to be used. from http://hl7.org/fhir/ValueSet/conformance-statement-kind
  TFhirConformanceStatementKindEnum = (
    ConformanceStatementKindNull, // Value is missing from Instance 
    ConformanceStatementKindInstance, 
    ConformanceStatementKindCapability, 
    ConformanceStatementKindRequirements); 
  TFhirConformanceStatementKindEnumList = set of TFhirConformanceStatementKindEnum;

  // A code that indicates whether an application accepts unknown elements or extensions when reading resources. from http://hl7.org/fhir/ValueSet/unknown-content-code
  TFhirUnknownContentCodeEnum = (
    UnknownContentCodeNull, // Value is missing from Instance 
    UnknownContentCodeNo, 
    UnknownContentCodeExtensions, 
    UnknownContentCodeElements, 
    UnknownContentCodeBoth); 
  TFhirUnknownContentCodeEnumList = set of TFhirUnknownContentCodeEnum;

  // The mode of a RESTful conformance statement. from http://hl7.org/fhir/ValueSet/restful-conformance-mode
  TFhirRestfulConformanceModeEnum = (
    RestfulConformanceModeNull, // Value is missing from Instance 
    RestfulConformanceModeClient, 
    RestfulConformanceModeServer); 
  TFhirRestfulConformanceModeEnumList = set of TFhirRestfulConformanceModeEnum;

  // One of the resource types defined as part of FHIR. from http://hl7.org/fhir/ValueSet/resource-types
  TFhirResourceTypesEnum = (
    ResourceTypesNull, // Value is missing from Instance 
    ResourceTypesAccount, 
    ResourceTypesAllergyIntolerance, 
    ResourceTypesAppointment, 
    ResourceTypesAppointmentResponse, 
    ResourceTypesAuditEvent, 
    ResourceTypesBasic, 
    ResourceTypesBinary, 
    ResourceTypesBodySite, 
    ResourceTypesBundle, 
    ResourceTypesCarePlan, 
    ResourceTypesClaim, 
    ResourceTypesClaimResponse, 
    ResourceTypesClinicalImpression, 
    ResourceTypesCommunication, 
    ResourceTypesCommunicationRequest, 
    ResourceTypesComposition, 
    ResourceTypesConceptMap, 
    ResourceTypesCondition, 
    ResourceTypesConformance, 
    ResourceTypesContract, 
    ResourceTypesCoverage, 
    ResourceTypesDataElement, 
    ResourceTypesDetectedIssue, 
    ResourceTypesDevice, 
    ResourceTypesDeviceComponent, 
    ResourceTypesDeviceMetric, 
    ResourceTypesDeviceUseRequest, 
    ResourceTypesDeviceUseStatement, 
    ResourceTypesDiagnosticOrder, 
    ResourceTypesDiagnosticReport, 
    ResourceTypesDocumentManifest, 
    ResourceTypesDocumentReference, 
    ResourceTypesDomainResource, 
    ResourceTypesEligibilityRequest, 
    ResourceTypesEligibilityResponse, 
    ResourceTypesEncounter, 
    ResourceTypesEnrollmentRequest, 
    ResourceTypesEnrollmentResponse, 
    ResourceTypesEpisodeOfCare, 
    ResourceTypesExplanationOfBenefit, 
    ResourceTypesFamilyMemberHistory, 
    ResourceTypesFlag, 
    ResourceTypesGoal, 
    ResourceTypesGroup, 
    ResourceTypesHealthcareService, 
    ResourceTypesImagingObjectSelection, 
    ResourceTypesImagingStudy, 
    ResourceTypesImmunization, 
    ResourceTypesImmunizationRecommendation, 
    ResourceTypesImplementationGuide, 
    ResourceTypesList, 
    ResourceTypesLocation, 
    ResourceTypesMedia, 
    ResourceTypesMedication, 
    ResourceTypesMedicationAdministration, 
    ResourceTypesMedicationDispense, 
    ResourceTypesMedicationOrder, 
    ResourceTypesMedicationStatement, 
    ResourceTypesMessageHeader, 
    ResourceTypesNamingSystem, 
    ResourceTypesNutritionOrder, 
    ResourceTypesObservation, 
    ResourceTypesOperationDefinition, 
    ResourceTypesOperationOutcome, 
    ResourceTypesOrder, 
    ResourceTypesOrderResponse, 
    ResourceTypesOrganization, 
    ResourceTypesParameters, 
    ResourceTypesPatient, 
    ResourceTypesPaymentNotice, 
    ResourceTypesPaymentReconciliation, 
    ResourceTypesPerson, 
    ResourceTypesPractitioner, 
    ResourceTypesProcedure, 
    ResourceTypesProcedureRequest, 
    ResourceTypesProcessRequest, 
    ResourceTypesProcessResponse, 
    ResourceTypesProvenance, 
    ResourceTypesQuestionnaire, 
    ResourceTypesQuestionnaireResponse, 
    ResourceTypesReferralRequest, 
    ResourceTypesRelatedPerson, 
    ResourceTypesResource, 
    ResourceTypesRiskAssessment, 
    ResourceTypesSchedule, 
    ResourceTypesSearchParameter, 
    ResourceTypesSlot, 
    ResourceTypesSpecimen, 
    ResourceTypesStructureDefinition, 
    ResourceTypesSubscription, 
    ResourceTypesSubstance, 
    ResourceTypesSupplyDelivery, 
    ResourceTypesSupplyRequest, 
    ResourceTypesTestScript, 
    ResourceTypesValueSet, 
    ResourceTypesVisionPrescription); 
  TFhirResourceTypesEnumList = set of TFhirResourceTypesEnum;

  // Operations supported by REST at the type or instance level. from http://hl7.org/fhir/ValueSet/type-restful-interaction
  TFhirTypeRestfulInteractionEnum = (
    TypeRestfulInteractionNull, // Value is missing from Instance 
    TypeRestfulInteractionRead, 
    TypeRestfulInteractionVread, 
    TypeRestfulInteractionUpdate, 
    TypeRestfulInteractionDelete, 
    TypeRestfulInteractionHistoryInstance, 
    TypeRestfulInteractionValidate, 
    TypeRestfulInteractionHistoryType, 
    TypeRestfulInteractionCreate, 
    TypeRestfulInteractionSearchType); 
  TFhirTypeRestfulInteractionEnumList = set of TFhirTypeRestfulInteractionEnum;

  // How the system supports versioning for a resource. from http://hl7.org/fhir/ValueSet/versioning-policy
  TFhirVersioningPolicyEnum = (
    VersioningPolicyNull, // Value is missing from Instance 
    VersioningPolicyNoVersion, 
    VersioningPolicyVersioned, 
    VersioningPolicyVersionedUpdate); 
  TFhirVersioningPolicyEnumList = set of TFhirVersioningPolicyEnum;

  // A code that indicates how the server supports conditional delete. from http://hl7.org/fhir/ValueSet/conditional-delete-status
  TFhirConditionalDeleteStatusEnum = (
    ConditionalDeleteStatusNull, // Value is missing from Instance 
    ConditionalDeleteStatusNotSupported, 
    ConditionalDeleteStatusSingle, 
    ConditionalDeleteStatusMultiple); 
  TFhirConditionalDeleteStatusEnumList = set of TFhirConditionalDeleteStatusEnum;

  // Data types allowed to be used for search parameters. from http://hl7.org/fhir/ValueSet/search-param-type
  TFhirSearchParamTypeEnum = (
    SearchParamTypeNull, // Value is missing from Instance 
    SearchParamTypeNumber, 
    SearchParamTypeDate, 
    SearchParamTypeString, 
    SearchParamTypeToken, 
    SearchParamTypeReference, 
    SearchParamTypeComposite, 
    SearchParamTypeQuantity, 
    SearchParamTypeUri); 
  TFhirSearchParamTypeEnumList = set of TFhirSearchParamTypeEnum;

  // A supported modifier for a search parameter. from http://hl7.org/fhir/ValueSet/search-modifier-code
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
    SearchModifierCodeType); 
  TFhirSearchModifierCodeEnumList = set of TFhirSearchModifierCodeEnum;

  // Operations supported by REST at the system level. from http://hl7.org/fhir/ValueSet/system-restful-interaction
  TFhirSystemRestfulInteractionEnum = (
    SystemRestfulInteractionNull, // Value is missing from Instance 
    SystemRestfulInteractionTransaction, 
    SystemRestfulInteractionSearchSystem, 
    SystemRestfulInteractionHistorySystem); 
  TFhirSystemRestfulInteractionEnumList = set of TFhirSystemRestfulInteractionEnum;

  // A code that indicates how transactions are supported. from http://hl7.org/fhir/ValueSet/transaction-mode
  TFhirTransactionModeEnum = (
    TransactionModeNull, // Value is missing from Instance 
    TransactionModeNotSupported, 
    TransactionModeBatch, 
    TransactionModeTransaction, 
    TransactionModeBoth); 
  TFhirTransactionModeEnumList = set of TFhirTransactionModeEnum;

  // The impact of the content of a message. from http://hl7.org/fhir/ValueSet/message-significance-category
  TFhirMessageSignificanceCategoryEnum = (
    MessageSignificanceCategoryNull, // Value is missing from Instance 
    MessageSignificanceCategoryConsequence, 
    MessageSignificanceCategoryCurrency, 
    MessageSignificanceCategoryNotification); 
  TFhirMessageSignificanceCategoryEnumList = set of TFhirMessageSignificanceCategoryEnum;

  // The mode of a message conformance statement. from http://hl7.org/fhir/ValueSet/message-conformance-event-mode
  TFhirMessageConformanceEventModeEnum = (
    MessageConformanceEventModeNull, // Value is missing from Instance 
    MessageConformanceEventModeSender, 
    MessageConformanceEventModeReceiver); 
  TFhirMessageConformanceEventModeEnumList = set of TFhirMessageConformanceEventModeEnum;

  // Whether the application produces or consumes documents. from http://hl7.org/fhir/ValueSet/document-mode
  TFhirDocumentModeEnum = (
    DocumentModeNull, // Value is missing from Instance 
    DocumentModeProducer, 
    DocumentModeConsumer); 
  TFhirDocumentModeEnumList = set of TFhirDocumentModeEnum;

  // Indicates the degree of precision of the data element definition. from http://hl7.org/fhir/ValueSet/dataelement-stringency
  TFhirDataelementStringencyEnum = (
    DataelementStringencyNull, // Value is missing from Instance 
    DataelementStringencyComparable, 
    DataelementStringencyFullySpecified, 
    DataelementStringencyEquivalent, 
    DataelementStringencyConvertable, 
    DataelementStringencyScaleable, 
    DataelementStringencyFlexible); 
  TFhirDataelementStringencyEnumList = set of TFhirDataelementStringencyEnum;

  // Indicates the potential degree of impact of the identified issue on the patient. from http://hl7.org/fhir/ValueSet/detectedissue-severity
  TFhirDetectedissueSeverityEnum = (
    DetectedissueSeverityNull, // Value is missing from Instance 
    DetectedissueSeverityHigh, 
    DetectedissueSeverityModerate, 
    DetectedissueSeverityLow); 
  TFhirDetectedissueSeverityEnumList = set of TFhirDetectedissueSeverityEnum;

  // The availability status of the device. from http://hl7.org/fhir/ValueSet/devicestatus
  TFhirDevicestatusEnum = (
    DevicestatusNull, // Value is missing from Instance 
    DevicestatusAvailable, 
    DevicestatusNotAvailable, 
    DevicestatusEnteredInError); 
  TFhirDevicestatusEnumList = set of TFhirDevicestatusEnum;

  // Different measurement principle supported by the device. from http://hl7.org/fhir/ValueSet/measurement-principle
  TFhirMeasurementPrincipleEnum = (
    MeasurementPrincipleNull, // Value is missing from Instance 
    MeasurementPrincipleOther, 
    MeasurementPrincipleChemical, 
    MeasurementPrincipleElectrical, 
    MeasurementPrincipleImpedance, 
    MeasurementPrincipleNuclear, 
    MeasurementPrincipleOptical, 
    MeasurementPrincipleThermal, 
    MeasurementPrincipleBiological, 
    MeasurementPrincipleMechanical, 
    MeasurementPrincipleAcoustical, 
    MeasurementPrincipleManual); 
  TFhirMeasurementPrincipleEnumList = set of TFhirMeasurementPrincipleEnum;

  // Describes the operational status of the DeviceMetric. from http://hl7.org/fhir/ValueSet/metric-operational-status
  TFhirMetricOperationalStatusEnum = (
    MetricOperationalStatusNull, // Value is missing from Instance 
    MetricOperationalStatusOn, 
    MetricOperationalStatusOff, 
    MetricOperationalStatusStandby); 
  TFhirMetricOperationalStatusEnumList = set of TFhirMetricOperationalStatusEnum;

  // Describes the typical color of representation. from http://hl7.org/fhir/ValueSet/metric-color
  TFhirMetricColorEnum = (
    MetricColorNull, // Value is missing from Instance 
    MetricColorBlack, 
    MetricColorRed, 
    MetricColorGreen, 
    MetricColorYellow, 
    MetricColorBlue, 
    MetricColorMagenta, 
    MetricColorCyan, 
    MetricColorWhite); 
  TFhirMetricColorEnumList = set of TFhirMetricColorEnum;

  // Describes the category of the metric. from http://hl7.org/fhir/ValueSet/metric-category
  TFhirMetricCategoryEnum = (
    MetricCategoryNull, // Value is missing from Instance 
    MetricCategoryMeasurement, 
    MetricCategorySetting, 
    MetricCategoryCalculation, 
    MetricCategoryUnspecified); 
  TFhirMetricCategoryEnumList = set of TFhirMetricCategoryEnum;

  // Describes the type of a metric calibration. from http://hl7.org/fhir/ValueSet/metric-calibration-type
  TFhirMetricCalibrationTypeEnum = (
    MetricCalibrationTypeNull, // Value is missing from Instance 
    MetricCalibrationTypeUnspecified, 
    MetricCalibrationTypeOffset, 
    MetricCalibrationTypeGain, 
    MetricCalibrationTypeTwoPoint); 
  TFhirMetricCalibrationTypeEnumList = set of TFhirMetricCalibrationTypeEnum;

  // Describes the state of a metric calibration. from http://hl7.org/fhir/ValueSet/metric-calibration-state
  TFhirMetricCalibrationStateEnum = (
    MetricCalibrationStateNull, // Value is missing from Instance 
    MetricCalibrationStateNotCalibrated, 
    MetricCalibrationStateCalibrationRequired, 
    MetricCalibrationStateCalibrated, 
    MetricCalibrationStateUnspecified); 
  TFhirMetricCalibrationStateEnumList = set of TFhirMetricCalibrationStateEnum;

  // Codes representing the status of the request. from http://hl7.org/fhir/ValueSet/device-use-request-status
  TFhirDeviceUseRequestStatusEnum = (
    DeviceUseRequestStatusNull, // Value is missing from Instance 
    DeviceUseRequestStatusProposed, 
    DeviceUseRequestStatusPlanned, 
    DeviceUseRequestStatusRequested, 
    DeviceUseRequestStatusReceived, 
    DeviceUseRequestStatusAccepted, 
    DeviceUseRequestStatusInProgress, 
    DeviceUseRequestStatusCompleted, 
    DeviceUseRequestStatusSuspended, 
    DeviceUseRequestStatusRejected, 
    DeviceUseRequestStatusAborted); 
  TFhirDeviceUseRequestStatusEnumList = set of TFhirDeviceUseRequestStatusEnum;

  // Codes representing the priority of the request. from http://hl7.org/fhir/ValueSet/device-use-request-priority
  TFhirDeviceUseRequestPriorityEnum = (
    DeviceUseRequestPriorityNull, // Value is missing from Instance 
    DeviceUseRequestPriorityRoutine, 
    DeviceUseRequestPriorityUrgent, 
    DeviceUseRequestPriorityStat, 
    DeviceUseRequestPriorityAsap); 
  TFhirDeviceUseRequestPriorityEnumList = set of TFhirDeviceUseRequestPriorityEnum;

  // The status of a diagnostic order. from http://hl7.org/fhir/ValueSet/diagnostic-order-status
  TFhirDiagnosticOrderStatusEnum = (
    DiagnosticOrderStatusNull, // Value is missing from Instance 
    DiagnosticOrderStatusProposed, 
    DiagnosticOrderStatusDraft, 
    DiagnosticOrderStatusPlanned, 
    DiagnosticOrderStatusRequested, 
    DiagnosticOrderStatusReceived, 
    DiagnosticOrderStatusAccepted, 
    DiagnosticOrderStatusInProgress, 
    DiagnosticOrderStatusReview, 
    DiagnosticOrderStatusCompleted, 
    DiagnosticOrderStatusCancelled, 
    DiagnosticOrderStatusSuspended, 
    DiagnosticOrderStatusRejected, 
    DiagnosticOrderStatusFailed); 
  TFhirDiagnosticOrderStatusEnumList = set of TFhirDiagnosticOrderStatusEnum;

  // The clinical priority of a diagnostic order. from http://hl7.org/fhir/ValueSet/diagnostic-order-priority
  TFhirDiagnosticOrderPriorityEnum = (
    DiagnosticOrderPriorityNull, // Value is missing from Instance 
    DiagnosticOrderPriorityRoutine, 
    DiagnosticOrderPriorityUrgent, 
    DiagnosticOrderPriorityStat, 
    DiagnosticOrderPriorityAsap); 
  TFhirDiagnosticOrderPriorityEnumList = set of TFhirDiagnosticOrderPriorityEnum;

  // The status of the diagnostic report as a whole. from http://hl7.org/fhir/ValueSet/diagnostic-report-status
  TFhirDiagnosticReportStatusEnum = (
    DiagnosticReportStatusNull, // Value is missing from Instance 
    DiagnosticReportStatusRegistered, 
    DiagnosticReportStatusPartial, 
    DiagnosticReportStatusFinal, 
    DiagnosticReportStatusCorrected, 
    DiagnosticReportStatusAppended, 
    DiagnosticReportStatusCancelled, 
    DiagnosticReportStatusEnteredInError); 
  TFhirDiagnosticReportStatusEnumList = set of TFhirDiagnosticReportStatusEnum;

  // The status of the document reference. from http://hl7.org/fhir/ValueSet/document-reference-status
  TFhirDocumentReferenceStatusEnum = (
    DocumentReferenceStatusNull, // Value is missing from Instance 
    DocumentReferenceStatusCurrent, 
    DocumentReferenceStatusSuperseded, 
    DocumentReferenceStatusEnteredInError); 
  TFhirDocumentReferenceStatusEnumList = set of TFhirDocumentReferenceStatusEnum;

  // The type of relationship between documents. from http://hl7.org/fhir/ValueSet/document-relationship-type
  TFhirDocumentRelationshipTypeEnum = (
    DocumentRelationshipTypeNull, // Value is missing from Instance 
    DocumentRelationshipTypeReplaces, 
    DocumentRelationshipTypeTransforms, 
    DocumentRelationshipTypeSigns, 
    DocumentRelationshipTypeAppends); 
  TFhirDocumentRelationshipTypeEnumList = set of TFhirDocumentRelationshipTypeEnum;

  // Current state of the encounter from http://hl7.org/fhir/ValueSet/encounter-state
  TFhirEncounterStateEnum = (
    EncounterStateNull, // Value is missing from Instance 
    EncounterStatePlanned, 
    EncounterStateArrived, 
    EncounterStateInProgress, 
    EncounterStateOnleave, 
    EncounterStateFinished, 
    EncounterStateCancelled); 
  TFhirEncounterStateEnumList = set of TFhirEncounterStateEnum;

  // Classification of the encounter from http://hl7.org/fhir/ValueSet/encounter-class
  TFhirEncounterClassEnum = (
    EncounterClassNull, // Value is missing from Instance 
    EncounterClassInpatient, 
    EncounterClassOutpatient, 
    EncounterClassAmbulatory, 
    EncounterClassEmergency, 
    EncounterClassHome, 
    EncounterClassField, 
    EncounterClassDaytime, 
    EncounterClassVirtual, 
    EncounterClassOther); 
  TFhirEncounterClassEnumList = set of TFhirEncounterClassEnum;

  // The status of the location. from http://hl7.org/fhir/ValueSet/encounter-location-status
  TFhirEncounterLocationStatusEnum = (
    EncounterLocationStatusNull, // Value is missing from Instance 
    EncounterLocationStatusPlanned, 
    EncounterLocationStatusActive, 
    EncounterLocationStatusReserved, 
    EncounterLocationStatusCompleted); 
  TFhirEncounterLocationStatusEnumList = set of TFhirEncounterLocationStatusEnum;

  // The status of the encounter. from http://hl7.org/fhir/ValueSet/episode-of-care-status
  TFhirEpisodeOfCareStatusEnum = (
    EpisodeOfCareStatusNull, // Value is missing from Instance 
    EpisodeOfCareStatusPlanned, 
    EpisodeOfCareStatusWaitlist, 
    EpisodeOfCareStatusActive, 
    EpisodeOfCareStatusOnhold, 
    EpisodeOfCareStatusFinished, 
    EpisodeOfCareStatusCancelled); 
  TFhirEpisodeOfCareStatusEnumList = set of TFhirEpisodeOfCareStatusEnum;

  // A code that identifies the status of the family history record. from http://hl7.org/fhir/ValueSet/history-status
  TFhirHistoryStatusEnum = (
    HistoryStatusNull, // Value is missing from Instance 
    HistoryStatusPartial, 
    HistoryStatusCompleted, 
    HistoryStatusEnteredInError, 
    HistoryStatusHealthUnknown); 
  TFhirHistoryStatusEnumList = set of TFhirHistoryStatusEnum;

  // The gender of a person used for administrative purposes. from http://hl7.org/fhir/ValueSet/administrative-gender
  TFhirAdministrativeGenderEnum = (
    AdministrativeGenderNull, // Value is missing from Instance 
    AdministrativeGenderMale, 
    AdministrativeGenderFemale, 
    AdministrativeGenderOther, 
    AdministrativeGenderUnknown); 
  TFhirAdministrativeGenderEnumList = set of TFhirAdministrativeGenderEnum;

  // Indicates whether this flag is active and needs to be displayed to a user, or whether it is no longer needed or entered in error. from http://hl7.org/fhir/ValueSet/flag-status
  TFhirFlagStatusEnum = (
    FlagStatusNull, // Value is missing from Instance 
    FlagStatusActive, 
    FlagStatusInactive, 
    FlagStatusEnteredInError); 
  TFhirFlagStatusEnumList = set of TFhirFlagStatusEnum;

  // Indicates whether the goal has been met and is still being targeted from http://hl7.org/fhir/ValueSet/goal-status
  TFhirGoalStatusEnum = (
    GoalStatusNull, // Value is missing from Instance 
    GoalStatusProposed, 
    GoalStatusPlanned, 
    GoalStatusAccepted, 
    GoalStatusRejected, 
    GoalStatusInProgress, 
    GoalStatusAchieved, 
    GoalStatusSustaining, 
    GoalStatusOnHold, 
    GoalStatusCancelled); 
  TFhirGoalStatusEnumList = set of TFhirGoalStatusEnum;

  // Types of resources that are part of group from http://hl7.org/fhir/ValueSet/group-type
  TFhirGroupTypeEnum = (
    GroupTypeNull, // Value is missing from Instance 
    GroupTypePerson, 
    GroupTypeAnimal, 
    GroupTypePractitioner, 
    GroupTypeDevice, 
    GroupTypeMedication, 
    GroupTypeSubstance); 
  TFhirGroupTypeEnumList = set of TFhirGroupTypeEnum;

  // The days of the week. from http://hl7.org/fhir/ValueSet/days-of-week
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

  // Availability of the resource from http://hl7.org/fhir/ValueSet/instance-availability
  TFhirInstanceAvailabilityEnum = (
    InstanceAvailabilityNull, // Value is missing from Instance 
    InstanceAvailabilityONLINE, 
    InstanceAvailabilityOFFLINE, 
    InstanceAvailabilityNEARLINE, 
    InstanceAvailabilityUNAVAILABLE); 
  TFhirInstanceAvailabilityEnumList = set of TFhirInstanceAvailabilityEnum;

  // A set of codes indicating the current status of an Immunization from http://hl7.org/fhir/ValueSet/medication-admin-status
  TFhirMedicationAdminStatusEnum = (
    MedicationAdminStatusNull, // Value is missing from Instance 
    MedicationAdminStatusInProgress, 
    MedicationAdminStatusOnHold, 
    MedicationAdminStatusCompleted, 
    MedicationAdminStatusEnteredInError, 
    MedicationAdminStatusStopped); 
  TFhirMedicationAdminStatusEnumList = set of TFhirMedicationAdminStatusEnum;

  // How a dependency is represented when the guide is published. from http://hl7.org/fhir/ValueSet/guide-dependency-type
  TFhirGuideDependencyTypeEnum = (
    GuideDependencyTypeNull, // Value is missing from Instance 
    GuideDependencyTypeReference, 
    GuideDependencyTypeInclusion); 
  TFhirGuideDependencyTypeEnumList = set of TFhirGuideDependencyTypeEnum;

  // Why a resource is included in the guide. from http://hl7.org/fhir/ValueSet/guide-resource-purpose
  TFhirGuideResourcePurposeEnum = (
    GuideResourcePurposeNull, // Value is missing from Instance 
    GuideResourcePurposeExample, 
    GuideResourcePurposeTerminology, 
    GuideResourcePurposeProfile, 
    GuideResourcePurposeExtension, 
    GuideResourcePurposeDictionary, 
    GuideResourcePurposeLogical); 
  TFhirGuideResourcePurposeEnumList = set of TFhirGuideResourcePurposeEnum;

  // The kind of an included page. from http://hl7.org/fhir/ValueSet/guide-page-kind
  TFhirGuidePageKindEnum = (
    GuidePageKindNull, // Value is missing from Instance 
    GuidePageKindPage, 
    GuidePageKindExample, 
    GuidePageKindList, 
    GuidePageKindInclude, 
    GuidePageKindDirectory, 
    GuidePageKindDictionary, 
    GuidePageKindToc, 
    GuidePageKindResource); 
  TFhirGuidePageKindEnumList = set of TFhirGuidePageKindEnum;

  // The current state of the list from http://hl7.org/fhir/ValueSet/list-status
  TFhirListStatusEnum = (
    ListStatusNull, // Value is missing from Instance 
    ListStatusCurrent, 
    ListStatusRetired, 
    ListStatusEnteredInError); 
  TFhirListStatusEnumList = set of TFhirListStatusEnum;

  // Indicates whether the location is still in use. from http://hl7.org/fhir/ValueSet/location-status
  TFhirLocationStatusEnum = (
    LocationStatusNull, // Value is missing from Instance 
    LocationStatusActive, 
    LocationStatusSuspended, 
    LocationStatusInactive); 
  TFhirLocationStatusEnumList = set of TFhirLocationStatusEnum;

  // Indicates whether a resource instance represents a specific location or a class of locations. from http://hl7.org/fhir/ValueSet/location-mode
  TFhirLocationModeEnum = (
    LocationModeNull, // Value is missing from Instance 
    LocationModeInstance, 
    LocationModeKind); 
  TFhirLocationModeEnumList = set of TFhirLocationModeEnum;

  // Whether the Media is a photo, video, or audio from http://hl7.org/fhir/ValueSet/digital-media-type
  TFhirDigitalMediaTypeEnum = (
    DigitalMediaTypeNull, // Value is missing from Instance 
    DigitalMediaTypePhoto, 
    DigitalMediaTypeVideo, 
    DigitalMediaTypeAudio); 
  TFhirDigitalMediaTypeEnumList = set of TFhirDigitalMediaTypeEnum;

  // A code specifying the state of the dispense event. from http://hl7.org/fhir/ValueSet/medication-dispense-status
  TFhirMedicationDispenseStatusEnum = (
    MedicationDispenseStatusNull, // Value is missing from Instance 
    MedicationDispenseStatusInProgress, 
    MedicationDispenseStatusOnHold, 
    MedicationDispenseStatusCompleted, 
    MedicationDispenseStatusEnteredInError, 
    MedicationDispenseStatusStopped); 
  TFhirMedicationDispenseStatusEnumList = set of TFhirMedicationDispenseStatusEnum;

  // A code specifying the state of the prescribing event. Describes the lifecycle of the prescription. from http://hl7.org/fhir/ValueSet/medication-order-status
  TFhirMedicationOrderStatusEnum = (
    MedicationOrderStatusNull, // Value is missing from Instance 
    MedicationOrderStatusActive, 
    MedicationOrderStatusOnHold, 
    MedicationOrderStatusCompleted, 
    MedicationOrderStatusEnteredInError, 
    MedicationOrderStatusStopped, 
    MedicationOrderStatusDraft); 
  TFhirMedicationOrderStatusEnumList = set of TFhirMedicationOrderStatusEnum;

  // A set of codes indicating the current status of a MedicationStatement. from http://hl7.org/fhir/ValueSet/medication-statement-status
  TFhirMedicationStatementStatusEnum = (
    MedicationStatementStatusNull, // Value is missing from Instance 
    MedicationStatementStatusActive, 
    MedicationStatementStatusCompleted, 
    MedicationStatementStatusEnteredInError, 
    MedicationStatementStatusIntended); 
  TFhirMedicationStatementStatusEnumList = set of TFhirMedicationStatementStatusEnum;

  // The kind of response to a message from http://hl7.org/fhir/ValueSet/response-code
  TFhirResponseCodeEnum = (
    ResponseCodeNull, // Value is missing from Instance 
    ResponseCodeOk, 
    ResponseCodeTransientError, 
    ResponseCodeFatalError); 
  TFhirResponseCodeEnumList = set of TFhirResponseCodeEnum;

  // Identifies the purpose of the naming system. from http://hl7.org/fhir/ValueSet/namingsystem-type
  TFhirNamingsystemTypeEnum = (
    NamingsystemTypeNull, // Value is missing from Instance 
    NamingsystemTypeCodesystem, 
    NamingsystemTypeIdentifier, 
    NamingsystemTypeRoot); 
  TFhirNamingsystemTypeEnumList = set of TFhirNamingsystemTypeEnum;

  // Identifies the style of unique identifier used to identify a namespace. from http://hl7.org/fhir/ValueSet/namingsystem-identifier-type
  TFhirNamingsystemIdentifierTypeEnum = (
    NamingsystemIdentifierTypeNull, // Value is missing from Instance 
    NamingsystemIdentifierTypeOid, 
    NamingsystemIdentifierTypeUuid, 
    NamingsystemIdentifierTypeUri, 
    NamingsystemIdentifierTypeOther); 
  TFhirNamingsystemIdentifierTypeEnumList = set of TFhirNamingsystemIdentifierTypeEnum;

  // Codes specifying the state of the request. Describes the lifecycle of the nutrition order. from http://hl7.org/fhir/ValueSet/nutrition-order-status
  TFhirNutritionOrderStatusEnum = (
    NutritionOrderStatusNull, // Value is missing from Instance 
    NutritionOrderStatusProposed, 
    NutritionOrderStatusDraft, 
    NutritionOrderStatusPlanned, 
    NutritionOrderStatusRequested, 
    NutritionOrderStatusActive, 
    NutritionOrderStatusOnHold, 
    NutritionOrderStatusCompleted, 
    NutritionOrderStatusCancelled); 
  TFhirNutritionOrderStatusEnumList = set of TFhirNutritionOrderStatusEnum;

  // Codes providing the status of an observation. from http://hl7.org/fhir/ValueSet/observation-status
  TFhirObservationStatusEnum = (
    ObservationStatusNull, // Value is missing from Instance 
    ObservationStatusRegistered, 
    ObservationStatusPreliminary, 
    ObservationStatusFinal, 
    ObservationStatusAmended, 
    ObservationStatusCancelled, 
    ObservationStatusEnteredInError, 
    ObservationStatusUnknown); 
  TFhirObservationStatusEnumList = set of TFhirObservationStatusEnum;

  // Codes specifying how two observations are related. from http://hl7.org/fhir/ValueSet/observation-relationshiptypes
  TFhirObservationRelationshiptypesEnum = (
    ObservationRelationshiptypesNull, // Value is missing from Instance 
    ObservationRelationshiptypesHasMember, 
    ObservationRelationshiptypesDerivedFrom, 
    ObservationRelationshiptypesSequelTo, 
    ObservationRelationshiptypesReplaces, 
    ObservationRelationshiptypesQualifiedBy, 
    ObservationRelationshiptypesInterferedBy); 
  TFhirObservationRelationshiptypesEnumList = set of TFhirObservationRelationshiptypesEnum;

  // Whether an operation is a normal operation or a query. from http://hl7.org/fhir/ValueSet/operation-kind
  TFhirOperationKindEnum = (
    OperationKindNull, // Value is missing from Instance 
    OperationKindOperation, 
    OperationKindQuery); 
  TFhirOperationKindEnumList = set of TFhirOperationKindEnum;

  // Whether an operation parameter is an input or an output parameter. from http://hl7.org/fhir/ValueSet/operation-parameter-use
  TFhirOperationParameterUseEnum = (
    OperationParameterUseNull, // Value is missing from Instance 
    OperationParameterUseIn, 
    OperationParameterUseOut); 
  TFhirOperationParameterUseEnumList = set of TFhirOperationParameterUseEnum;

  // The type of a parameter. from http://hl7.org/fhir/ValueSet/operation-parameter-type
  TFhirOperationParameterTypeEnum = (
    OperationParameterTypeNull, // Value is missing from Instance 
    OperationParameterTypeNumber, 
    OperationParameterTypeDate, 
    OperationParameterTypeString, 
    OperationParameterTypeToken, 
    OperationParameterTypeReference, 
    OperationParameterTypeComposite, 
    OperationParameterTypeQuantity, 
    OperationParameterTypeUri, 
    OperationParameterTypeAddress, 
    OperationParameterTypeAge, 
    OperationParameterTypeAnnotation, 
    OperationParameterTypeAttachment, 
    OperationParameterTypeBackboneElement, 
    OperationParameterTypeCodeableConcept, 
    OperationParameterTypeCoding, 
    OperationParameterTypeContactPoint, 
    OperationParameterTypeCount, 
    OperationParameterTypeDistance, 
    OperationParameterTypeDuration, 
    OperationParameterTypeElement, 
    OperationParameterTypeElementDefinition, 
    OperationParameterTypeExtension, 
    OperationParameterTypeHumanName, 
    OperationParameterTypeIdentifier, 
    OperationParameterTypeMeta, 
    OperationParameterTypeMoney, 
    OperationParameterTypeNarrative, 
    OperationParameterTypePeriod, 
    OperationParameterTypeQuantity1, 
    OperationParameterTypeRange, 
    OperationParameterTypeRatio, 
    OperationParameterTypeReference1, 
    OperationParameterTypeSampledData, 
    OperationParameterTypeSignature, 
    OperationParameterTypeSimpleQuantity, 
    OperationParameterTypeTiming, 
    OperationParameterTypeBase64Binary, 
    OperationParameterTypeBoolean, 
    OperationParameterTypeCode, 
    OperationParameterTypeDateTime, 
    OperationParameterTypeDecimal, 
    OperationParameterTypeId, 
    OperationParameterTypeInstant, 
    OperationParameterTypeInteger, 
    OperationParameterTypeMarkdown, 
    OperationParameterTypeOid, 
    OperationParameterTypePositiveInt, 
    OperationParameterTypeTime, 
    OperationParameterTypeUnsignedInt, 
    OperationParameterTypeUuid, 
    OperationParameterTypeXhtml,
    OperationParameterTypeAccount, 
    OperationParameterTypeAllergyIntolerance, 
    OperationParameterTypeAppointment, 
    OperationParameterTypeAppointmentResponse, 
    OperationParameterTypeAuditEvent, 
    OperationParameterTypeBasic, 
    OperationParameterTypeBinary, 
    OperationParameterTypeBodySite, 
    OperationParameterTypeBundle, 
    OperationParameterTypeCarePlan, 
    OperationParameterTypeClaim, 
    OperationParameterTypeClaimResponse, 
    OperationParameterTypeClinicalImpression, 
    OperationParameterTypeCommunication, 
    OperationParameterTypeCommunicationRequest, 
    OperationParameterTypeComposition, 
    OperationParameterTypeConceptMap, 
    OperationParameterTypeCondition, 
    OperationParameterTypeConformance, 
    OperationParameterTypeContract, 
    OperationParameterTypeCoverage, 
    OperationParameterTypeDataElement, 
    OperationParameterTypeDetectedIssue, 
    OperationParameterTypeDevice, 
    OperationParameterTypeDeviceComponent, 
    OperationParameterTypeDeviceMetric, 
    OperationParameterTypeDeviceUseRequest, 
    OperationParameterTypeDeviceUseStatement, 
    OperationParameterTypeDiagnosticOrder, 
    OperationParameterTypeDiagnosticReport, 
    OperationParameterTypeDocumentManifest, 
    OperationParameterTypeDocumentReference, 
    OperationParameterTypeDomainResource, 
    OperationParameterTypeEligibilityRequest, 
    OperationParameterTypeEligibilityResponse, 
    OperationParameterTypeEncounter, 
    OperationParameterTypeEnrollmentRequest, 
    OperationParameterTypeEnrollmentResponse, 
    OperationParameterTypeEpisodeOfCare, 
    OperationParameterTypeExplanationOfBenefit, 
    OperationParameterTypeFamilyMemberHistory, 
    OperationParameterTypeFlag, 
    OperationParameterTypeGoal, 
    OperationParameterTypeGroup, 
    OperationParameterTypeHealthcareService, 
    OperationParameterTypeImagingObjectSelection, 
    OperationParameterTypeImagingStudy, 
    OperationParameterTypeImmunization, 
    OperationParameterTypeImmunizationRecommendation, 
    OperationParameterTypeImplementationGuide, 
    OperationParameterTypeList, 
    OperationParameterTypeLocation, 
    OperationParameterTypeMedia, 
    OperationParameterTypeMedication, 
    OperationParameterTypeMedicationAdministration, 
    OperationParameterTypeMedicationDispense, 
    OperationParameterTypeMedicationOrder, 
    OperationParameterTypeMedicationStatement, 
    OperationParameterTypeMessageHeader, 
    OperationParameterTypeNamingSystem, 
    OperationParameterTypeNutritionOrder, 
    OperationParameterTypeObservation, 
    OperationParameterTypeOperationDefinition, 
    OperationParameterTypeOperationOutcome, 
    OperationParameterTypeOrder, 
    OperationParameterTypeOrderResponse, 
    OperationParameterTypeOrganization, 
    OperationParameterTypeParameters, 
    OperationParameterTypePatient, 
    OperationParameterTypePaymentNotice, 
    OperationParameterTypePaymentReconciliation, 
    OperationParameterTypePerson, 
    OperationParameterTypePractitioner, 
    OperationParameterTypeProcedure, 
    OperationParameterTypeProcedureRequest, 
    OperationParameterTypeProcessRequest, 
    OperationParameterTypeProcessResponse, 
    OperationParameterTypeProvenance, 
    OperationParameterTypeQuestionnaire, 
    OperationParameterTypeQuestionnaireResponse, 
    OperationParameterTypeReferralRequest, 
    OperationParameterTypeRelatedPerson, 
    OperationParameterTypeResource, 
    OperationParameterTypeRiskAssessment, 
    OperationParameterTypeSchedule, 
    OperationParameterTypeSearchParameter, 
    OperationParameterTypeSlot, 
    OperationParameterTypeSpecimen, 
    OperationParameterTypeStructureDefinition, 
    OperationParameterTypeSubscription, 
    OperationParameterTypeSubstance, 
    OperationParameterTypeSupplyDelivery, 
    OperationParameterTypeSupplyRequest, 
    OperationParameterTypeTestScript, 
    OperationParameterTypeValueSet, 
    OperationParameterTypeVisionPrescription); 
  TFhirOperationParameterTypeEnumList = set of TFhirOperationParameterTypeEnum;

  // How the issue affects the success of the action. from http://hl7.org/fhir/ValueSet/issue-severity
  TFhirIssueSeverityEnum = (
    IssueSeverityNull, // Value is missing from Instance 
    IssueSeverityFatal, 
    IssueSeverityError, 
    IssueSeverityWarning, 
    IssueSeverityInformation); 
  TFhirIssueSeverityEnumList = set of TFhirIssueSeverityEnum;

  // A code that describes the type of issue. from http://hl7.org/fhir/ValueSet/issue-type
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
    IssueTypeNotFound, 
    IssueTypeTooLong, 
    IssueTypeCodeInvalid, 
    IssueTypeExtension, 
    IssueTypeTooCostly, 
    IssueTypeBusinessRule, 
    IssueTypeConflict, 
    IssueTypeIncomplete, 
    IssueTypeTransient, 
    IssueTypeLockError, 
    IssueTypeNoStore, 
    IssueTypeException, 
    IssueTypeTimeout, 
    IssueTypeThrottled, 
    IssueTypeInformational); 
  TFhirIssueTypeEnumList = set of TFhirIssueTypeEnum;

  // The status of the response to an order. from http://hl7.org/fhir/ValueSet/order-status
  TFhirOrderStatusEnum = (
    OrderStatusNull, // Value is missing from Instance 
    OrderStatusPending, 
    OrderStatusReview, 
    OrderStatusRejected, 
    OrderStatusError, 
    OrderStatusAccepted, 
    OrderStatusCancelled, 
    OrderStatusReplaced, 
    OrderStatusAborted, 
    OrderStatusCompleted); 
  TFhirOrderStatusEnumList = set of TFhirOrderStatusEnum;

  // The type of link between this patient resource and another patient resource. from http://hl7.org/fhir/ValueSet/link-type
  TFhirLinkTypeEnum = (
    LinkTypeNull, // Value is missing from Instance 
    LinkTypeReplace, 
    LinkTypeRefer, 
    LinkTypeSeealso); 
  TFhirLinkTypeEnumList = set of TFhirLinkTypeEnum;

  // The level of confidence that this link represents the same actual person, based on NIST Authentication Levels. from http://hl7.org/fhir/ValueSet/identity-assuranceLevel
  TFhirIdentityAssuranceLevelEnum = (
    IdentityAssuranceLevelNull, // Value is missing from Instance 
    IdentityAssuranceLevelLevel1, 
    IdentityAssuranceLevelLevel2, 
    IdentityAssuranceLevelLevel3, 
    IdentityAssuranceLevelLevel4); 
  TFhirIdentityAssuranceLevelEnumList = set of TFhirIdentityAssuranceLevelEnum;

  // A code specifying the state of the procedure. from http://hl7.org/fhir/ValueSet/procedure-status
  TFhirProcedureStatusEnum = (
    ProcedureStatusNull, // Value is missing from Instance 
    ProcedureStatusInProgress, 
    ProcedureStatusAborted, 
    ProcedureStatusCompleted, 
    ProcedureStatusEnteredInError); 
  TFhirProcedureStatusEnumList = set of TFhirProcedureStatusEnum;

  // The status of the request. from http://hl7.org/fhir/ValueSet/procedure-request-status
  TFhirProcedureRequestStatusEnum = (
    ProcedureRequestStatusNull, // Value is missing from Instance 
    ProcedureRequestStatusProposed, 
    ProcedureRequestStatusDraft, 
    ProcedureRequestStatusRequested, 
    ProcedureRequestStatusReceived, 
    ProcedureRequestStatusAccepted, 
    ProcedureRequestStatusInProgress, 
    ProcedureRequestStatusCompleted, 
    ProcedureRequestStatusSuspended, 
    ProcedureRequestStatusRejected, 
    ProcedureRequestStatusAborted); 
  TFhirProcedureRequestStatusEnumList = set of TFhirProcedureRequestStatusEnum;

  // The priority of the request. from http://hl7.org/fhir/ValueSet/procedure-request-priority
  TFhirProcedureRequestPriorityEnum = (
    ProcedureRequestPriorityNull, // Value is missing from Instance 
    ProcedureRequestPriorityRoutine, 
    ProcedureRequestPriorityUrgent, 
    ProcedureRequestPriorityStat, 
    ProcedureRequestPriorityAsap); 
  TFhirProcedureRequestPriorityEnumList = set of TFhirProcedureRequestPriorityEnum;

  // List of allowable action which this resource can request. from http://hl7.org/fhir/ValueSet/actionlist
  TFhirActionlistEnum = (
    ActionlistNull, // Value is missing from Instance 
    ActionlistCancel, 
    ActionlistPoll, 
    ActionlistReprocess, 
    ActionlistStatus); 
  TFhirActionlistEnumList = set of TFhirActionlistEnum;

  // How an entity was used in an activity. from http://hl7.org/fhir/ValueSet/provenance-entity-role
  TFhirProvenanceEntityRoleEnum = (
    ProvenanceEntityRoleNull, // Value is missing from Instance 
    ProvenanceEntityRoleDerivation, 
    ProvenanceEntityRoleRevision, 
    ProvenanceEntityRoleQuotation, 
    ProvenanceEntityRoleSource); 
  TFhirProvenanceEntityRoleEnumList = set of TFhirProvenanceEntityRoleEnum;

  // Lifecycle status of the questionnaire. from http://hl7.org/fhir/ValueSet/questionnaire-status
  TFhirQuestionnaireStatusEnum = (
    QuestionnaireStatusNull, // Value is missing from Instance 
    QuestionnaireStatusDraft, 
    QuestionnaireStatusPublished, 
    QuestionnaireStatusRetired); 
  TFhirQuestionnaireStatusEnumList = set of TFhirQuestionnaireStatusEnum;

  // The expected format of an answer. from http://hl7.org/fhir/ValueSet/answer-format
  TFhirAnswerFormatEnum = (
    AnswerFormatNull, // Value is missing from Instance 
    AnswerFormatBoolean, 
    AnswerFormatDecimal, 
    AnswerFormatInteger, 
    AnswerFormatDate, 
    AnswerFormatDateTime, 
    AnswerFormatInstant, 
    AnswerFormatTime, 
    AnswerFormatString, 
    AnswerFormatText, 
    AnswerFormatUrl, 
    AnswerFormatChoice, 
    AnswerFormatOpenChoice, 
    AnswerFormatAttachment, 
    AnswerFormatReference, 
    AnswerFormatQuantity); 
  TFhirAnswerFormatEnumList = set of TFhirAnswerFormatEnum;

  // Lifecycle status of the questionnaire response. from http://hl7.org/fhir/ValueSet/questionnaire-answers-status
  TFhirQuestionnaireAnswersStatusEnum = (
    QuestionnaireAnswersStatusNull, // Value is missing from Instance 
    QuestionnaireAnswersStatusInProgress, 
    QuestionnaireAnswersStatusCompleted, 
    QuestionnaireAnswersStatusAmended); 
  TFhirQuestionnaireAnswersStatusEnumList = set of TFhirQuestionnaireAnswersStatusEnum;

  // The status of the referral. from http://hl7.org/fhir/ValueSet/referralstatus
  TFhirReferralstatusEnum = (
    ReferralstatusNull, // Value is missing from Instance 
    ReferralstatusDraft, 
    ReferralstatusRequested, 
    ReferralstatusActive, 
    ReferralstatusCancelled, 
    ReferralstatusAccepted, 
    ReferralstatusRejected, 
    ReferralstatusCompleted); 
  TFhirReferralstatusEnumList = set of TFhirReferralstatusEnum;

  // How a search parameter relates to the set of elements returned by evaluating its xpath query. from http://hl7.org/fhir/ValueSet/search-xpath-usage
  TFhirSearchXpathUsageEnum = (
    SearchXpathUsageNull, // Value is missing from Instance 
    SearchXpathUsageNormal, 
    SearchXpathUsagePhonetic, 
    SearchXpathUsageNearby, 
    SearchXpathUsageDistance, 
    SearchXpathUsageOther); 
  TFhirSearchXpathUsageEnumList = set of TFhirSearchXpathUsageEnum;

  // The free/busy status of a slot. from http://hl7.org/fhir/ValueSet/slotstatus
  TFhirSlotstatusEnum = (
    SlotstatusNull, // Value is missing from Instance 
    SlotstatusBusy, 
    SlotstatusFree, 
    SlotstatusBusyUnavailable, 
    SlotstatusBusyTentative); 
  TFhirSlotstatusEnumList = set of TFhirSlotstatusEnum;

  // Codes providing the status/availability of a specimen. from http://hl7.org/fhir/ValueSet/specimen-status
  TFhirSpecimenStatusEnum = (
    SpecimenStatusNull, // Value is missing from Instance 
    SpecimenStatusAvailable, 
    SpecimenStatusUnavailable, 
    SpecimenStatusUnsatisfactory, 
    SpecimenStatusEnteredInError); 
  TFhirSpecimenStatusEnumList = set of TFhirSpecimenStatusEnum;

  // Defines the type of structure that a definition is describing. from http://hl7.org/fhir/ValueSet/structure-definition-kind
  TFhirStructureDefinitionKindEnum = (
    StructureDefinitionKindNull, // Value is missing from Instance 
    StructureDefinitionKindDatatype, 
    StructureDefinitionKindResource, 
    StructureDefinitionKindLogical); 
  TFhirStructureDefinitionKindEnumList = set of TFhirStructureDefinitionKindEnum;

  // How an extension context is interpreted. from http://hl7.org/fhir/ValueSet/extension-context
  TFhirExtensionContextEnum = (
    ExtensionContextNull, // Value is missing from Instance 
    ExtensionContextResource, 
    ExtensionContextDatatype, 
    ExtensionContextMapping, 
    ExtensionContextExtension); 
  TFhirExtensionContextEnumList = set of TFhirExtensionContextEnum;

  // The status of a subscription. from http://hl7.org/fhir/ValueSet/subscription-status
  TFhirSubscriptionStatusEnum = (
    SubscriptionStatusNull, // Value is missing from Instance 
    SubscriptionStatusRequested, 
    SubscriptionStatusActive, 
    SubscriptionStatusError, 
    SubscriptionStatusOff); 
  TFhirSubscriptionStatusEnumList = set of TFhirSubscriptionStatusEnum;

  // The type of method used to execute a subscription. from http://hl7.org/fhir/ValueSet/subscription-channel-type
  TFhirSubscriptionChannelTypeEnum = (
    SubscriptionChannelTypeNull, // Value is missing from Instance 
    SubscriptionChannelTypeRestHook, 
    SubscriptionChannelTypeWebsocket, 
    SubscriptionChannelTypeEmail, 
    SubscriptionChannelTypeSms, 
    SubscriptionChannelTypeMessage); 
  TFhirSubscriptionChannelTypeEnumList = set of TFhirSubscriptionChannelTypeEnum;

  // Status of the supply delivery. from http://hl7.org/fhir/ValueSet/supplydelivery-status
  TFhirSupplydeliveryStatusEnum = (
    SupplydeliveryStatusNull, // Value is missing from Instance 
    SupplydeliveryStatusInProgress, 
    SupplydeliveryStatusCompleted, 
    SupplydeliveryStatusAbandoned); 
  TFhirSupplydeliveryStatusEnumList = set of TFhirSupplydeliveryStatusEnum;

  // Status of the supply request from http://hl7.org/fhir/ValueSet/supplyrequest-status
  TFhirSupplyrequestStatusEnum = (
    SupplyrequestStatusNull, // Value is missing from Instance 
    SupplyrequestStatusRequested, 
    SupplyrequestStatusCompleted, 
    SupplyrequestStatusFailed, 
    SupplyrequestStatusCancelled); 
  TFhirSupplyrequestStatusEnumList = set of TFhirSupplyrequestStatusEnum;

  // The content or mime type. from http://hl7.org/fhir/ValueSet/content-type
  TFhirContentTypeEnum = (
    ContentTypeNull, // Value is missing from Instance 
    ContentTypeXml, 
    ContentTypeJson); 
  TFhirContentTypeEnumList = set of TFhirContentTypeEnum;

  // The type of direction to use for assertion. from http://hl7.org/fhir/ValueSet/assert-direction-codes
  TFhirAssertDirectionCodesEnum = (
    AssertDirectionCodesNull, // Value is missing from Instance 
    AssertDirectionCodesResponse, 
    AssertDirectionCodesRequest); 
  TFhirAssertDirectionCodesEnumList = set of TFhirAssertDirectionCodesEnum;

  // The type of operator to use for assertion. from http://hl7.org/fhir/ValueSet/assert-operator-codes
  TFhirAssertOperatorCodesEnum = (
    AssertOperatorCodesNull, // Value is missing from Instance 
    AssertOperatorCodesEquals, 
    AssertOperatorCodesNotEquals, 
    AssertOperatorCodesIn, 
    AssertOperatorCodesNotIn, 
    AssertOperatorCodesGreaterThan, 
    AssertOperatorCodesLessThan, 
    AssertOperatorCodesEmpty, 
    AssertOperatorCodesNotEmpty, 
    AssertOperatorCodesContains, 
    AssertOperatorCodesNotContains); 
  TFhirAssertOperatorCodesEnumList = set of TFhirAssertOperatorCodesEnum;

  // The type of response code to use for assertion. from http://hl7.org/fhir/ValueSet/assert-response-code-types
  TFhirAssertResponseCodeTypesEnum = (
    AssertResponseCodeTypesNull, // Value is missing from Instance 
    AssertResponseCodeTypesOkay, 
    AssertResponseCodeTypesCreated, 
    AssertResponseCodeTypesNoContent, 
    AssertResponseCodeTypesNotModified, 
    AssertResponseCodeTypesBad, 
    AssertResponseCodeTypesForbidden, 
    AssertResponseCodeTypesNotFound, 
    AssertResponseCodeTypesMethodNotAllowed, 
    AssertResponseCodeTypesConflict, 
    AssertResponseCodeTypesGone, 
    AssertResponseCodeTypesPreconditionFailed, 
    AssertResponseCodeTypesUnprocessable); 
  TFhirAssertResponseCodeTypesEnumList = set of TFhirAssertResponseCodeTypesEnum;

  // The kind of operation to perform as a part of a property based filter. from http://hl7.org/fhir/ValueSet/filter-operator
  TFhirFilterOperatorEnum = (
    FilterOperatorNull, // Value is missing from Instance 
    FilterOperatorEqual, 
    FilterOperatorIsA, 
    FilterOperatorIsNotA, 
    FilterOperatorRegex, 
    FilterOperatorIn, 
    FilterOperatorNotIn); 
  TFhirFilterOperatorEnumList = set of TFhirFilterOperatorEnum;

  // A coded concept listing the eye codes. from http://hl7.org/fhir/ValueSet/vision-eye-codes
  TFhirVisionEyeCodesEnum = (
    VisionEyeCodesNull, // Value is missing from Instance 
    VisionEyeCodesRight, 
    VisionEyeCodesLeft); 
  TFhirVisionEyeCodesEnumList = set of TFhirVisionEyeCodesEnum;

  // A coded concept listing the base codes. from http://hl7.org/fhir/ValueSet/vision-base-codes
  TFhirVisionBaseCodesEnum = (
    VisionBaseCodesNull, // Value is missing from Instance 
    VisionBaseCodesUp, 
    VisionBaseCodesDown, 
    VisionBaseCodesIn, 
    VisionBaseCodesOut); 
  TFhirVisionBaseCodesEnumList = set of TFhirVisionBaseCodesEnum;

Type
  TFhirElement = class;
  TFhirElementList = class;
  TFhirBackboneElement = class;
  TFhirBackboneElementList = class;
  TFhirEnum = class;
  TFhirEnumList = class;
  TFhirDateTime = class;
  TFhirDateTimeList = class;
  TFhirDate = class;
  TFhirDateList = class;
  TFhirString = class;
  TFhirStringList = class;
  TFhirInteger = class;
  TFhirIntegerList = class;
  TFhirUri = class;
  TFhirUriList = class;
  TFhirInstant = class;
  TFhirInstantList = class;
  TFhirBoolean = class;
  TFhirBooleanList = class;
  TFhirBase64Binary = class;
  TFhirBase64BinaryList = class;
  TFhirTime = class;
  TFhirTimeList = class;
  TFhirDecimal = class;
  TFhirDecimalList = class;
  TFhirCode = class;
  TFhirCodeList = class;
  TFhirOid = class;
  TFhirOidList = class;
  TFhirUuid = class;
  TFhirUuidList = class;
  TFhirMarkdown = class;
  TFhirMarkdownList = class;
  TFhirUnsignedInt = class;
  TFhirUnsignedIntList = class;
  TFhirId = class;
  TFhirIdList = class;
  TFhirPositiveInt = class;
  TFhirPositiveIntList = class;
  TFhirExtension = class;
  TFhirExtensionList = class;
  TFhirNarrative = class;
  TFhirNarrativeList = class;
  TFhirIdentifier = class;
  TFhirIdentifierList = class;
  TFhirCoding = class;
  TFhirCodingList = class;
  TFhirReference = class;
  TFhirReferenceList = class;
  TFhirSignature = class;
  TFhirSignatureList = class;
  TFhirSampledData = class;
  TFhirSampledDataList = class;
  TFhirPeriod = class;
  TFhirPeriodList = class;
  TFhirQuantity = class;
  TFhirQuantityList = class;
  TFhirAttachment = class;
  TFhirAttachmentList = class;
  TFhirRatio = class;
  TFhirRatioList = class;
  TFhirRange = class;
  TFhirRangeList = class;
  TFhirAnnotation = class;
  TFhirAnnotationList = class;
  TFhirCodeableConcept = class;
  TFhirCodeableConceptList = class;
  TFhirHumanName = class;
  TFhirHumanNameList = class;
  TFhirMeta = class;
  TFhirMetaList = class;
  TFhirContactPoint = class;
  TFhirContactPointList = class;
  TFhirAddress = class;
  TFhirAddressList = class;
  TFhirElementDefinitionSlicing = class;
  TFhirElementDefinitionSlicingList = class;
  TFhirElementDefinitionBase = class;
  TFhirElementDefinitionBaseList = class;
  TFhirElementDefinitionType = class;
  TFhirElementDefinitionTypeList = class;
  TFhirElementDefinitionConstraint = class;
  TFhirElementDefinitionConstraintList = class;
  TFhirElementDefinitionBinding = class;
  TFhirElementDefinitionBindingList = class;
  TFhirElementDefinitionMapping = class;
  TFhirElementDefinitionMappingList = class;
  TFhirElementDefinition = class;
  TFhirElementDefinitionList = class;
  TFhirTimingRepeat = class;
  TFhirTimingRepeatList = class;
  TFhirTiming = class;
  TFhirTimingList = class;

  // Base definition for all elements in a resource.
  TFhirElement = class (TFHIRObject2)
  private
    FDisallowExtensions: boolean;
  protected
    FId : TFhirId;
    FextensionList : TFhirExtensionList;
    Procedure SetId(value : TFhirId);
    Function GetIdST : String;
    Procedure SetIdST(value : String);
    function GetExtensionList : TFhirExtensionList;
    function GetHasExtensionList : Boolean;

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;

    Procedure listElementFieldsInOrder(fields : TStringList);
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirElement; overload;
    function Clone : TFhirElement; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function noExtensions : TFhirElement;
    function hasExtensions : boolean; override;
    property DisallowExtensions : boolean read FDisallowExtensions write FDisallowExtensions;
    function hasExtension(url : string) : boolean; override;
    function getExtensionString(url : String) : String; override;
    function extensionCount(url : String) : integer; override;
    function extensions(url : String) : TFslList<TFHIRObject>; override;
    procedure addExtension(url : String; value : TFHIRObject); override;
  published
    // Typed access to unique id for the element within a resource (for internal references).
    property id : String read GetIdST write SetIdST;
    // unique id for the element within a resource (for internal references).
    property idElement : TFhirId read FId write SetId;

    // May be used to represent additional information that is not part of the basic definition of the element. In order to make the use of extensions safe and manageable, there is a strict set of governance  applied to the definition and use of extensions. Though any implementer is allowed to define an extension, there is a set of requirements that SHALL be met as part of the definition of the extension.
    property extensionList : TFhirExtensionList read GetExtensionList;
    property hasExtensionList : boolean read GetHasExtensionList;

  end;

  TFhirElementListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirElementList;
    function GetCurrent : TFhirElement;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirElementList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElement read GetCurrent;
  end;

  TFhirElementList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirElement;
    procedure SetItemN(index : Integer; value : TFhirElement);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirElementList; Overload;
    function Clone : TFhirElementList; Overload;
    function GetEnumerator : TFhirElementListEnumerator;
    

    //  Add a FhirElement to the end of the list.
    function Append : TFhirElement;

    
    // Add an already existing FhirElement to the end of the list.
    procedure AddItem(value : TFhirElement); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirElement) : Integer;
    

    // Insert FhirElement before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirElement;
    

    // Insert an existing FhirElement before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirElement);
    
    // Get the iIndexth FhirElement. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirElement);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirElement;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirElements[index : Integer] : TFhirElement read GetItemN write SetItemN; default;
  End;

  //  A base FHIR type - (polymorphism support)
  TFhirType = class (TFhirElement)
  Public
    Function Link : TFhirType; Overload;
    Function Clone : TFhirType; Overload;
    Function isType : boolean; Override;
    Function ToString : String; Override;
  End;
  TFHIRTypeClass = class of TFhirType;
  
  // A base FHIR type - (polymorphism support)
  TFHIRPrimitiveType = class (TFhirType)
  Private
    Function GetStringValue : String;
    Procedure SetStringValue(value : String); virtual; abstract;
    Function AsStringValue : String; Virtual; abstract;
  Public
    Function Link : TFHIRPrimitiveType; Overload;
    Function Clone : TFHIRPrimitiveType; Overload;
    Property StringValue : String read GetStringValue write SetStringValue;
    function isPrimitive : boolean; override;
    function hasPrimitiveValue : boolean; override;
    function primitiveValue : string; override;
    function setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject; override;
    function ToString : String; override;
  End;
  TFHIRPrimitiveTypeClass = class of TFHIRPrimitiveType;


  // Base definition for all elements that are defined inside a resource - but not those in a data type.
  TFhirBackboneElement = class (TFHIRElement)
  protected
    FmodifierExtensionList : TFhirExtensionList;
    function GetModifierExtensionList : TFhirExtensionList;
    function GetHasModifierExtensionList : Boolean;

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    Procedure listBackboneElementFieldsInOrder(fields: TStringList);
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirBackboneElement; overload;
    function Clone : TFhirBackboneElement; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  published
    // May be used to represent additional information that is not part of the basic definition of the element, and that modifies the understanding of the element that contains it. Usually modifier elements provide negation or qualification. In order to make the use of extensions safe and manageable, there is a strict set of governance applied to the definition and use of extensions. Though any implementer is allowed to define an extension, there is a set of requirements that SHALL be met as part of the definition of the extension. Applications processing a resource are required to check for modifier extensions.
    property modifierExtensionList : TFhirExtensionList read GetModifierExtensionList;
    property hasModifierExtensionList : boolean read GetHasModifierExtensionList;

  end;

  TFhirBackboneElementListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirBackboneElementList;
    function GetCurrent : TFhirBackboneElement;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirBackboneElementList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirBackboneElement read GetCurrent;
  end;

  TFhirBackboneElementList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirBackboneElement;
    procedure SetItemN(index : Integer; value : TFhirBackboneElement);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirBackboneElementList; Overload;
    function Clone : TFhirBackboneElementList; Overload;
    function GetEnumerator : TFhirBackboneElementListEnumerator;
    

    //  Add a FhirBackboneElement to the end of the list.
    function Append : TFhirBackboneElement;

    
    // Add an already existing FhirBackboneElement to the end of the list.
    procedure AddItem(value : TFhirBackboneElement); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirBackboneElement) : Integer;
    

    // Insert FhirBackboneElement before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirBackboneElement;
    

    // Insert an existing FhirBackboneElement before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirBackboneElement);
    
    // Get the iIndexth FhirBackboneElement. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirBackboneElement);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirBackboneElement;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirBackboneElements[index : Integer] : TFhirBackboneElement read GetItemN write SetItemN; default;
  End;

  // a complex Enum - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Enum, and extensions
  TFhirEnum = class (TFhirPrimitiveType)
  Private
    FValue: String;
    FSystem: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(system : String; value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirEnum; Overload;
    Function Clone : TFhirEnum; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function isEnum : boolean; override;
    function fhirType : string; override;
  Published
    // The actual value of the enum
    property value : String read FValue write SetValue;
    property system : String read FSystem write FSystem;
  End;    

  TFhirEnumListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirEnumList;
    function GetCurrent : TFhirEnum;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirEnumList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEnum read GetCurrent;
  end;

  TFhirEnumList = class (TFHIRObjectList)
  private

    FSystems : Array Of String;

    FCodes : Array Of String;

    function GetItemN(index : Integer) : TFhirEnum;
    procedure SetItemN(index : Integer; value : TFhirEnum);
  protected
    function ItemClass : TFslObjectClass; override;
    function sizeInBytesV : cardinal; override;
  public

    constructor Create(Systems, Codes : Array Of String);

    function Link : TFhirEnumList; Overload;
    function Clone : TFhirEnumList; Overload;
    function GetEnumerator : TFhirEnumListEnumerator;
    

    //  Add a FhirEnum to the end of the list.
    function Append : TFhirEnum;

    
    // Add an already existing FhirEnum to the end of the list.
    procedure AddItem(value : TFhirEnum); overload;

    
    // Add an already existing FhirEnum to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirEnum) : Integer;
    

    // Insert FhirEnum before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirEnum;
    

    // Insert an existing FhirEnum before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirEnum);
    
    // Get the iIndexth FhirEnum. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirEnum);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirEnum;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirEnums[index : Integer] : TFhirEnum read GetItemN write SetItemN; default;
  End;

  // a complex DateTime - has an Id attribute, and extensions.
  //  Used where a FHIR element is a DateTime, and extensions
  TFhirDateTime = class (TFhirPrimitiveType)
  Private
    FValue: TFslDateTime;
    procedure setValue(value: TFslDateTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(value : TFslDateTime); overload;
    destructor Destroy; override;
    
    Function Link : TFhirDateTime; Overload;
    Function Clone : TFhirDateTime; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
    function isDateTime : boolean; override;
    function GetDateValue : TFslDateTime; override;
    procedure SetDateValue(value : TFslDateTime); override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The actual value of the dateTime
    property value : TFslDateTime read FValue write SetValue;
  End;    

  TFhirDateTimeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDateTimeList;
    function GetCurrent : TFhirDateTime;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirDateTimeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDateTime read GetCurrent;
  end;

  TFhirDateTimeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDateTime;
    procedure SetItemN(index : Integer; value : TFhirDateTime);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDateTimeList; Overload;
    function Clone : TFhirDateTimeList; Overload;
    function GetEnumerator : TFhirDateTimeListEnumerator;
    

    //  Add a FhirDateTime to the end of the list.
    function Append : TFhirDateTime;

    
    // Add an already existing FhirDateTime to the end of the list.
    procedure AddItem(value : TFhirDateTime); overload;

    
    // Add an already existing FhirDateTime to the end of the list.
    procedure AddItem(value : TFslDateTime); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDateTime) : Integer;
    

    // Insert FhirDateTime before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDateTime;
    

    // Insert an existing FhirDateTime before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDateTime);
    
    // Get the iIndexth FhirDateTime. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDateTime);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirDateTime;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirDateTimes[index : Integer] : TFhirDateTime read GetItemN write SetItemN; default;
  End;

  // a complex Date - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Date, and extensions
  TFhirDate = class (TFhirPrimitiveType)
  Private
    FValue: TFslDateTime;
    procedure setValue(value: TFslDateTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(value : TFslDateTime); overload;
    destructor Destroy; override;
    
    Function Link : TFhirDate; Overload;
    Function Clone : TFhirDate; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
    function isDateTime : boolean; override;
    function GetDateValue : TFslDateTime; override;
    procedure SetDateValue(value : TFslDateTime); override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The actual value of the date
    property value : TFslDateTime read FValue write SetValue;
  End;    

  TFhirDateListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDateList;
    function GetCurrent : TFhirDate;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirDateList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDate read GetCurrent;
  end;

  TFhirDateList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDate;
    procedure SetItemN(index : Integer; value : TFhirDate);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDateList; Overload;
    function Clone : TFhirDateList; Overload;
    function GetEnumerator : TFhirDateListEnumerator;
    

    //  Add a FhirDate to the end of the list.
    function Append : TFhirDate;

    
    // Add an already existing FhirDate to the end of the list.
    procedure AddItem(value : TFhirDate); overload;

    
    // Add an already existing FhirDate to the end of the list.
    procedure AddItem(value : TFslDateTime); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDate) : Integer;
    

    // Insert FhirDate before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDate;
    

    // Insert an existing FhirDate before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDate);
    
    // Get the iIndexth FhirDate. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDate);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirDate;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirDates[index : Integer] : TFhirDate read GetItemN write SetItemN; default;
  End;

  // a complex String - has an Id attribute, and extensions.
  //  Used where a FHIR element is a String, and extensions
  TFhirString = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirString; Overload;
    Function Clone : TFhirString; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The actual value of the string
    property value : String read FValue write SetValue;
  End;    

  TFhirStringListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirStringList;
    function GetCurrent : TFhirString;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirStringList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirString read GetCurrent;
  end;

  TFhirStringList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirString;
    procedure SetItemN(index : Integer; value : TFhirString);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirStringList; Overload;
    function Clone : TFhirStringList; Overload;
    function GetEnumerator : TFhirStringListEnumerator;
    

    //  Add a FhirString to the end of the list.
    function Append : TFhirString;

    
    // Add an already existing FhirString to the end of the list.
    procedure AddItem(value : TFhirString); overload;

    
    // Add an already existing FhirString to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirString) : Integer;
    

    // Insert FhirString before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirString;
    

    // Insert an existing FhirString before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirString);
    
    // Get the iIndexth FhirString. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirString);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirString;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirStrings[index : Integer] : TFhirString read GetItemN write SetItemN; default;
  End;

  // a complex Integer - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Integer, and extensions
  TFhirInteger = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirInteger; Overload;
    Function Clone : TFhirInteger; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The actual value of the integer
    property value : String read FValue write SetValue;
  End;    

  TFhirIntegerListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirIntegerList;
    function GetCurrent : TFhirInteger;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirIntegerList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirInteger read GetCurrent;
  end;

  TFhirIntegerList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirInteger;
    procedure SetItemN(index : Integer; value : TFhirInteger);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirIntegerList; Overload;
    function Clone : TFhirIntegerList; Overload;
    function GetEnumerator : TFhirIntegerListEnumerator;
    

    //  Add a FhirInteger to the end of the list.
    function Append : TFhirInteger;

    
    // Add an already existing FhirInteger to the end of the list.
    procedure AddItem(value : TFhirInteger); overload;

    
    // Add an already existing FhirInteger to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirInteger) : Integer;
    

    // Insert FhirInteger before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirInteger;
    

    // Insert an existing FhirInteger before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirInteger);
    
    // Get the iIndexth FhirInteger. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirInteger);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirInteger;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirIntegers[index : Integer] : TFhirInteger read GetItemN write SetItemN; default;
  End;

  // a complex Uri - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Uri, and extensions
  TFhirUri = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirUri; Overload;
    Function Clone : TFhirUri; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The actual value of the uri
    property value : String read FValue write SetValue;
  End;    

  TFhirUriListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirUriList;
    function GetCurrent : TFhirUri;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirUriList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirUri read GetCurrent;
  end;

  TFhirUriList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirUri;
    procedure SetItemN(index : Integer; value : TFhirUri);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirUriList; Overload;
    function Clone : TFhirUriList; Overload;
    function GetEnumerator : TFhirUriListEnumerator;
    

    //  Add a FhirUri to the end of the list.
    function Append : TFhirUri;

    
    // Add an already existing FhirUri to the end of the list.
    procedure AddItem(value : TFhirUri); overload;

    
    // Add an already existing FhirUri to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirUri) : Integer;
    

    // Insert FhirUri before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirUri;
    

    // Insert an existing FhirUri before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirUri);
    
    // Get the iIndexth FhirUri. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirUri);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirUri;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirUris[index : Integer] : TFhirUri read GetItemN write SetItemN; default;
  End;

  // a complex Instant - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Instant, and extensions
  TFhirInstant = class (TFhirPrimitiveType)
  Private
    FValue: TFslDateTime;
    procedure setValue(value: TFslDateTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(value : TFslDateTime); overload;
    destructor Destroy; override;
    
    Function Link : TFhirInstant; Overload;
    Function Clone : TFhirInstant; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
    function isDateTime : boolean; override;
    function GetDateValue : TFslDateTime; override;
    procedure SetDateValue(value : TFslDateTime); override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The actual value of the instant
    property value : TFslDateTime read FValue write SetValue;
  End;    

  TFhirInstantListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirInstantList;
    function GetCurrent : TFhirInstant;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirInstantList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirInstant read GetCurrent;
  end;

  TFhirInstantList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirInstant;
    procedure SetItemN(index : Integer; value : TFhirInstant);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirInstantList; Overload;
    function Clone : TFhirInstantList; Overload;
    function GetEnumerator : TFhirInstantListEnumerator;
    

    //  Add a FhirInstant to the end of the list.
    function Append : TFhirInstant;

    
    // Add an already existing FhirInstant to the end of the list.
    procedure AddItem(value : TFhirInstant); overload;

    
    // Add an already existing FhirInstant to the end of the list.
    procedure AddItem(value : TFslDateTime); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirInstant) : Integer;
    

    // Insert FhirInstant before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirInstant;
    

    // Insert an existing FhirInstant before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirInstant);
    
    // Get the iIndexth FhirInstant. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirInstant);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirInstant;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirInstants[index : Integer] : TFhirInstant read GetItemN write SetItemN; default;
  End;

  // a complex Boolean - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Boolean, and extensions
  TFhirBoolean = class (TFhirPrimitiveType)
  Private
    FValue: Boolean;
    procedure setValue(value: Boolean);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(value : Boolean); overload;
    destructor Destroy; override;
    
    Function Link : TFhirBoolean; Overload;
    Function Clone : TFhirBoolean; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
    function isBooleanPrimitive : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The actual value of the boolean
    property value : Boolean read FValue write SetValue;
  End;    

  TFhirBooleanListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirBooleanList;
    function GetCurrent : TFhirBoolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirBooleanList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirBoolean read GetCurrent;
  end;

  TFhirBooleanList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirBoolean;
    procedure SetItemN(index : Integer; value : TFhirBoolean);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirBooleanList; Overload;
    function Clone : TFhirBooleanList; Overload;
    function GetEnumerator : TFhirBooleanListEnumerator;
    

    //  Add a FhirBoolean to the end of the list.
    function Append : TFhirBoolean;

    
    // Add an already existing FhirBoolean to the end of the list.
    procedure AddItem(value : TFhirBoolean); overload;

    
    // Add an already existing FhirBoolean to the end of the list.
    procedure AddItem(value : Boolean); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirBoolean) : Integer;
    

    // Insert FhirBoolean before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirBoolean;
    

    // Insert an existing FhirBoolean before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirBoolean);
    
    // Get the iIndexth FhirBoolean. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirBoolean);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirBoolean;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirBooleans[index : Integer] : TFhirBoolean read GetItemN write SetItemN; default;
  End;

  // a complex Base64Binary - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Base64Binary, and extensions
  TFhirBase64Binary = class (TFhirPrimitiveType)
  Private
    FValue: TBytes;
    procedure setValue(value: TBytes);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(value : TBytes); overload;
    destructor Destroy; override;
    
    Function Link : TFhirBase64Binary; Overload;
    Function Clone : TFhirBase64Binary; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The actual value of the base64Binary
    property value : TBytes read FValue write SetValue;
  End;    

  TFhirBase64BinaryListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirBase64BinaryList;
    function GetCurrent : TFhirBase64Binary;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirBase64BinaryList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirBase64Binary read GetCurrent;
  end;

  TFhirBase64BinaryList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirBase64Binary;
    procedure SetItemN(index : Integer; value : TFhirBase64Binary);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirBase64BinaryList; Overload;
    function Clone : TFhirBase64BinaryList; Overload;
    function GetEnumerator : TFhirBase64BinaryListEnumerator;
    

    //  Add a FhirBase64Binary to the end of the list.
    function Append : TFhirBase64Binary;

    
    // Add an already existing FhirBase64Binary to the end of the list.
    procedure AddItem(value : TFhirBase64Binary); overload;

    
    // Add an already existing FhirBase64Binary to the end of the list.
    procedure AddItem(value : TBytes); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirBase64Binary) : Integer;
    

    // Insert FhirBase64Binary before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirBase64Binary;
    

    // Insert an existing FhirBase64Binary before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirBase64Binary);
    
    // Get the iIndexth FhirBase64Binary. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirBase64Binary);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirBase64Binary;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirBase64Binaries[index : Integer] : TFhirBase64Binary read GetItemN write SetItemN; default;
  End;

  // a complex Time - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Time, and extensions
  TFhirTime = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirTime; Overload;
    Function Clone : TFhirTime; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The actual value of the time
    property value : String read FValue write SetValue;
  End;    

  TFhirTimeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirTimeList;
    function GetCurrent : TFhirTime;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirTimeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirTime read GetCurrent;
  end;

  TFhirTimeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirTime;
    procedure SetItemN(index : Integer; value : TFhirTime);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirTimeList; Overload;
    function Clone : TFhirTimeList; Overload;
    function GetEnumerator : TFhirTimeListEnumerator;
    

    //  Add a FhirTime to the end of the list.
    function Append : TFhirTime;

    
    // Add an already existing FhirTime to the end of the list.
    procedure AddItem(value : TFhirTime); overload;

    
    // Add an already existing FhirTime to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirTime) : Integer;
    

    // Insert FhirTime before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirTime;
    

    // Insert an existing FhirTime before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirTime);
    
    // Get the iIndexth FhirTime. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirTime);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirTime;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirTimes[index : Integer] : TFhirTime read GetItemN write SetItemN; default;
  End;

  // a complex Decimal - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Decimal, and extensions
  TFhirDecimal = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function AsStringValue : String; Override;
    procedure SetStringValue(value : String); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirDecimal; Overload;
    Function Clone : TFhirDecimal; Overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The actual value of the decimal
    property value : String read FValue write SetValue;
  End;    

  TFhirDecimalListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDecimalList;
    function GetCurrent : TFhirDecimal;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirDecimalList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDecimal read GetCurrent;
  end;

  TFhirDecimalList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDecimal;
    procedure SetItemN(index : Integer; value : TFhirDecimal);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDecimalList; Overload;
    function Clone : TFhirDecimalList; Overload;
    function GetEnumerator : TFhirDecimalListEnumerator;
    

    //  Add a FhirDecimal to the end of the list.
    function Append : TFhirDecimal;

    
    // Add an already existing FhirDecimal to the end of the list.
    procedure AddItem(value : TFhirDecimal); overload;

    
    // Add an already existing FhirDecimal to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDecimal) : Integer;
    

    // Insert FhirDecimal before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDecimal;
    

    // Insert an existing FhirDecimal before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDecimal);
    
    // Get the iIndexth FhirDecimal. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDecimal);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirDecimal;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirDecimals[index : Integer] : TFhirDecimal read GetItemN write SetItemN; default;
  End;

  // a complex Code - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Code, and extensions
  TFhirCode = class (TFhirString)
  Private
  protected
    procedure listFieldsInOrder(fields : TStringList); override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirCode; Overload;
    Function Clone : TFhirCode; Overload;
    function fhirType : string; override;
  End;    

  TFhirCodeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirCodeList;
    function GetCurrent : TFhirCode;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirCodeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirCode read GetCurrent;
  end;

  TFhirCodeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirCode;
    procedure SetItemN(index : Integer; value : TFhirCode);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirCodeList; Overload;
    function Clone : TFhirCodeList; Overload;
    function GetEnumerator : TFhirCodeListEnumerator;
    

    //  Add a FhirCode to the end of the list.
    function Append : TFhirCode;

    
    // Add an already existing FhirCode to the end of the list.
    procedure AddItem(value : TFhirCode); overload;

    
    // Add an already existing FhirCode to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirCode) : Integer;
    

    // Insert FhirCode before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirCode;
    

    // Insert an existing FhirCode before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirCode);
    
    // Get the iIndexth FhirCode. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirCode);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirCode;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirCodes[index : Integer] : TFhirCode read GetItemN write SetItemN; default;
  End;

  // a complex Oid - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Oid, and extensions
  TFhirOid = class (TFhirUri)
  Private
  protected
    procedure listFieldsInOrder(fields : TStringList); override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirOid; Overload;
    Function Clone : TFhirOid; Overload;
    function fhirType : string; override;
  End;    

  TFhirOidListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirOidList;
    function GetCurrent : TFhirOid;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirOidList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirOid read GetCurrent;
  end;

  TFhirOidList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirOid;
    procedure SetItemN(index : Integer; value : TFhirOid);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirOidList; Overload;
    function Clone : TFhirOidList; Overload;
    function GetEnumerator : TFhirOidListEnumerator;
    

    //  Add a FhirOid to the end of the list.
    function Append : TFhirOid;

    
    // Add an already existing FhirOid to the end of the list.
    procedure AddItem(value : TFhirOid); overload;

    
    // Add an already existing FhirOid to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirOid) : Integer;
    

    // Insert FhirOid before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirOid;
    

    // Insert an existing FhirOid before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirOid);
    
    // Get the iIndexth FhirOid. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirOid);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirOid;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirOids[index : Integer] : TFhirOid read GetItemN write SetItemN; default;
  End;

  // a complex Uuid - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Uuid, and extensions
  TFhirUuid = class (TFhirUri)
  Private
  protected
    procedure listFieldsInOrder(fields : TStringList); override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirUuid; Overload;
    Function Clone : TFhirUuid; Overload;
    function fhirType : string; override;
  End;    

  TFhirUuidListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirUuidList;
    function GetCurrent : TFhirUuid;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirUuidList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirUuid read GetCurrent;
  end;

  TFhirUuidList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirUuid;
    procedure SetItemN(index : Integer; value : TFhirUuid);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirUuidList; Overload;
    function Clone : TFhirUuidList; Overload;
    function GetEnumerator : TFhirUuidListEnumerator;
    

    //  Add a FhirUuid to the end of the list.
    function Append : TFhirUuid;

    
    // Add an already existing FhirUuid to the end of the list.
    procedure AddItem(value : TFhirUuid); overload;

    
    // Add an already existing FhirUuid to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirUuid) : Integer;
    

    // Insert FhirUuid before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirUuid;
    

    // Insert an existing FhirUuid before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirUuid);
    
    // Get the iIndexth FhirUuid. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirUuid);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirUuid;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirUuids[index : Integer] : TFhirUuid read GetItemN write SetItemN; default;
  End;

  // a complex Markdown - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Markdown, and extensions
  TFhirMarkdown = class (TFhirString)
  Private
  protected
    procedure listFieldsInOrder(fields : TStringList); override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirMarkdown; Overload;
    Function Clone : TFhirMarkdown; Overload;
    function fhirType : string; override;
  End;    

  TFhirMarkdownListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirMarkdownList;
    function GetCurrent : TFhirMarkdown;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirMarkdownList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirMarkdown read GetCurrent;
  end;

  TFhirMarkdownList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirMarkdown;
    procedure SetItemN(index : Integer; value : TFhirMarkdown);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirMarkdownList; Overload;
    function Clone : TFhirMarkdownList; Overload;
    function GetEnumerator : TFhirMarkdownListEnumerator;
    

    //  Add a FhirMarkdown to the end of the list.
    function Append : TFhirMarkdown;

    
    // Add an already existing FhirMarkdown to the end of the list.
    procedure AddItem(value : TFhirMarkdown); overload;

    
    // Add an already existing FhirMarkdown to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirMarkdown) : Integer;
    

    // Insert FhirMarkdown before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirMarkdown;
    

    // Insert an existing FhirMarkdown before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirMarkdown);
    
    // Get the iIndexth FhirMarkdown. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirMarkdown);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirMarkdown;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirMarkdowns[index : Integer] : TFhirMarkdown read GetItemN write SetItemN; default;
  End;

  // a complex UnsignedInt - has an Id attribute, and extensions.
  //  Used where a FHIR element is a UnsignedInt, and extensions
  TFhirUnsignedInt = class (TFhirInteger)
  Private
  protected
    procedure listFieldsInOrder(fields : TStringList); override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirUnsignedInt; Overload;
    Function Clone : TFhirUnsignedInt; Overload;
    function fhirType : string; override;
  End;    

  TFhirUnsignedIntListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirUnsignedIntList;
    function GetCurrent : TFhirUnsignedInt;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirUnsignedIntList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirUnsignedInt read GetCurrent;
  end;

  TFhirUnsignedIntList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirUnsignedInt;
    procedure SetItemN(index : Integer; value : TFhirUnsignedInt);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirUnsignedIntList; Overload;
    function Clone : TFhirUnsignedIntList; Overload;
    function GetEnumerator : TFhirUnsignedIntListEnumerator;
    

    //  Add a FhirUnsignedInt to the end of the list.
    function Append : TFhirUnsignedInt;

    
    // Add an already existing FhirUnsignedInt to the end of the list.
    procedure AddItem(value : TFhirUnsignedInt); overload;

    
    // Add an already existing FhirUnsignedInt to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirUnsignedInt) : Integer;
    

    // Insert FhirUnsignedInt before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirUnsignedInt;
    

    // Insert an existing FhirUnsignedInt before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirUnsignedInt);
    
    // Get the iIndexth FhirUnsignedInt. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirUnsignedInt);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirUnsignedInt;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirUnsignedInts[index : Integer] : TFhirUnsignedInt read GetItemN write SetItemN; default;
  End;

  // a complex Id - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Id, and extensions
  TFhirId = class (TFhirString)
  Private
  protected
    procedure listFieldsInOrder(fields : TStringList); override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirId; Overload;
    Function Clone : TFhirId; Overload;
    function fhirType : string; override;
  End;    

  TFhirIdListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirIdList;
    function GetCurrent : TFhirId;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirIdList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirId read GetCurrent;
  end;

  TFhirIdList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirId;
    procedure SetItemN(index : Integer; value : TFhirId);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirIdList; Overload;
    function Clone : TFhirIdList; Overload;
    function GetEnumerator : TFhirIdListEnumerator;
    

    //  Add a FhirId to the end of the list.
    function Append : TFhirId;

    
    // Add an already existing FhirId to the end of the list.
    procedure AddItem(value : TFhirId); overload;

    
    // Add an already existing FhirId to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirId) : Integer;
    

    // Insert FhirId before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirId;
    

    // Insert an existing FhirId before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirId);
    
    // Get the iIndexth FhirId. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirId);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirId;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirIds[index : Integer] : TFhirId read GetItemN write SetItemN; default;
  End;

  // a complex PositiveInt - has an Id attribute, and extensions.
  //  Used where a FHIR element is a PositiveInt, and extensions
  TFhirPositiveInt = class (TFhirInteger)
  Private
  protected
    procedure listFieldsInOrder(fields : TStringList); override;
  Public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    Function Link : TFhirPositiveInt; Overload;
    Function Clone : TFhirPositiveInt; Overload;
    function fhirType : string; override;
  End;    

  TFhirPositiveIntListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPositiveIntList;
    function GetCurrent : TFhirPositiveInt;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPositiveIntList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPositiveInt read GetCurrent;
  end;

  TFhirPositiveIntList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPositiveInt;
    procedure SetItemN(index : Integer; value : TFhirPositiveInt);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPositiveIntList; Overload;
    function Clone : TFhirPositiveIntList; Overload;
    function GetEnumerator : TFhirPositiveIntListEnumerator;
    

    //  Add a FhirPositiveInt to the end of the list.
    function Append : TFhirPositiveInt;

    
    // Add an already existing FhirPositiveInt to the end of the list.
    procedure AddItem(value : TFhirPositiveInt); overload;

    
    // Add an already existing FhirPositiveInt to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPositiveInt) : Integer;
    

    // Insert FhirPositiveInt before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPositiveInt;
    

    // Insert an existing FhirPositiveInt before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPositiveInt);
    
    // Get the iIndexth FhirPositiveInt. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPositiveInt);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirPositiveInt;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirPositiveInts[index : Integer] : TFhirPositiveInt read GetItemN write SetItemN; default;
  End;

  // Optional Extensions Element - found in all resources.
  TFhirExtension = class (TFHIRType)
  protected
    FUrl : TFhirUri;
    FValue : TFhirType;
    Procedure SetUrl(value : TFhirUri);
    Function GetUrlST : String;
    Procedure SetUrlST(value : String);
    Procedure SetValue(value : TFhirType);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirExtension; overload;
    function Clone : TFhirExtension; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to Source of the definition for the extension code - a logical name or a URL.
    property url : String read GetUrlST write SetUrlST;
    // Source of the definition for the extension code - a logical name or a URL.
    property urlElement : TFhirUri read FUrl write SetUrl;

    // Typed access to Value of extension - may be a resource or one of a constrained set of the data types (see Extensibility in the spec for list). (defined for API consistency)
    property value : TFhirType read FValue write SetValue;
    // Value of extension - may be a resource or one of a constrained set of the data types (see Extensibility in the spec for list).
    property valueElement : TFhirType read FValue write SetValue;

  end;

  TFhirExtensionListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirExtensionList;
    function GetCurrent : TFhirExtension;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirExtensionList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirExtension read GetCurrent;
  end;

  TFhirExtensionList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirExtension;
    procedure SetItemN(index : Integer; value : TFhirExtension);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirExtensionList; Overload;
    function Clone : TFhirExtensionList; Overload;
    function GetEnumerator : TFhirExtensionListEnumerator;
    

    //  Add a FhirExtension to the end of the list.
    function Append : TFhirExtension;

    
    // Add an already existing FhirExtension to the end of the list.
    procedure AddItem(value : TFhirExtension); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirExtension) : Integer;
    

    // Insert FhirExtension before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirExtension;
    

    // Insert an existing FhirExtension before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirExtension);
    
    // Get the iIndexth FhirExtension. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirExtension);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirExtension;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirExtensions[index : Integer] : TFhirExtension read GetItemN write SetItemN; default;
  End;

  // A human-readable formatted text, including images.
  TFhirNarrative = class (TFHIRType)
  protected
    FStatus : TFhirEnum;
    FDiv_ : TFhirXHtmlNode;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirNarrativeStatusEnum;
    Procedure SetStatusST(value : TFhirNarrativeStatusEnum);
    Procedure SetDiv_(value : TFhirXHtmlNode);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirNarrative; overload;
    function Clone : TFhirNarrative; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The status of the narrative - whether it's entirely generated (from just the defined data or the extensions too), or whether a human authored it and it may contain additional data.
    property status : TFhirNarrativeStatusEnum read GetStatusST write SetStatusST;
    property statusElement : TFhirEnum read FStatus write SetStatus;

    // Typed access to The actual narrative content, a stripped down version of XHTML. (defined for API consistency)
    property div_ : TFhirXHtmlNode read FDiv_ write SetDiv_;
    // The actual narrative content, a stripped down version of XHTML.
    property div_Element : TFhirXHtmlNode read FDiv_ write SetDiv_;

  end;

  TFhirNarrativeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirNarrativeList;
    function GetCurrent : TFhirNarrative;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirNarrativeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirNarrative read GetCurrent;
  end;

  TFhirNarrativeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirNarrative;
    procedure SetItemN(index : Integer; value : TFhirNarrative);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirNarrativeList; Overload;
    function Clone : TFhirNarrativeList; Overload;
    function GetEnumerator : TFhirNarrativeListEnumerator;
    

    //  Add a FhirNarrative to the end of the list.
    function Append : TFhirNarrative;

    
    // Add an already existing FhirNarrative to the end of the list.
    procedure AddItem(value : TFhirNarrative); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirNarrative) : Integer;
    

    // Insert FhirNarrative before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirNarrative;
    

    // Insert an existing FhirNarrative before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirNarrative);
    
    // Get the iIndexth FhirNarrative. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirNarrative);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirNarrative;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirNarratives[index : Integer] : TFhirNarrative read GetItemN write SetItemN; default;
  End;

  // A technical identifier - identifies some entity uniquely and unambiguously.
  TFhirIdentifier = class (TFhirType)
  protected
    FUse : TFhirEnum;
    FType_ : TFhirCodeableConcept;
    FSystem : TFhirUri;
    FValue : TFhirString;
    FPeriod : TFhirPeriod;
    FAssigner : TFhirReference{TFhirOrganization};
    Procedure SetUse(value : TFhirEnum);
    Function GetUseST : TFhirIdentifierUseEnum;
    Procedure SetUseST(value : TFhirIdentifierUseEnum);
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetSystem(value : TFhirUri);
    Function GetSystemST : String;
    Procedure SetSystemST(value : String);
    Procedure SetValue(value : TFhirString);
    Function GetValueST : String;
    Procedure SetValueST(value : String);
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetAssigner(value : TFhirReference{TFhirOrganization});
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirIdentifier; overload;
    function Clone : TFhirIdentifier; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The purpose of this identifier.
    property use : TFhirIdentifierUseEnum read GetUseST write SetUseST;
    property useElement : TFhirEnum read FUse write SetUse;

    // Typed access to A coded type for the identifier that can be used to determine which identifier to use for a specific purpose. (defined for API consistency)
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    // A coded type for the identifier that can be used to determine which identifier to use for a specific purpose.
    property type_Element : TFhirCodeableConcept read FType_ write SetType_;

    // Typed access to Establishes the namespace in which set of possible id values is unique.
    property system : String read GetSystemST write SetSystemST;
    // Establishes the namespace in which set of possible id values is unique.
    property systemElement : TFhirUri read FSystem write SetSystem;

    // Typed access to The portion of the identifier typically displayed to the user and which is unique within the context of the system.
    property value : String read GetValueST write SetValueST;
    // The portion of the identifier typically displayed to the user and which is unique within the context of the system.
    property valueElement : TFhirString read FValue write SetValue;

    // Typed access to Time period during which identifier is/was valid for use. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // Time period during which identifier is/was valid for use.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

    // Typed access to Organization that issued/manages the identifier. (defined for API consistency)
    property assigner : TFhirReference{TFhirOrganization} read FAssigner write SetAssigner;
    // Organization that issued/manages the identifier.
    property assignerElement : TFhirReference{TFhirOrganization} read FAssigner write SetAssigner;

  end;

  TFhirIdentifierListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirIdentifierList;
    function GetCurrent : TFhirIdentifier;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirIdentifierList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirIdentifier read GetCurrent;
  end;

  TFhirIdentifierList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirIdentifier;
    procedure SetItemN(index : Integer; value : TFhirIdentifier);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirIdentifierList; Overload;
    function Clone : TFhirIdentifierList; Overload;
    function GetEnumerator : TFhirIdentifierListEnumerator;
    

    //  Add a FhirIdentifier to the end of the list.
    function Append : TFhirIdentifier;

    
    // Add an already existing FhirIdentifier to the end of the list.
    procedure AddItem(value : TFhirIdentifier); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirIdentifier) : Integer;
    

    // Insert FhirIdentifier before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirIdentifier;
    

    // Insert an existing FhirIdentifier before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirIdentifier);
    
    // Get the iIndexth FhirIdentifier. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirIdentifier);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirIdentifier;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirIdentifiers[index : Integer] : TFhirIdentifier read GetItemN write SetItemN; default;
  End;

  // A reference to a code defined by a terminology system.
  TFhirCoding = class (TFhirType)
  protected
    FSystem : TFhirUri;
    FVersion : TFhirString;
    FCode : TFhirCode;
    FDisplay : TFhirString;
    FUserSelected : TFhirBoolean;
    Procedure SetSystem(value : TFhirUri);
    Function GetSystemST : String;
    Procedure SetSystemST(value : String);
    Procedure SetVersion(value : TFhirString);
    Function GetVersionST : String;
    Procedure SetVersionST(value : String);
    Procedure SetCode(value : TFhirCode);
    Function GetCodeST : String;
    Procedure SetCodeST(value : String);
    Procedure SetDisplay(value : TFhirString);
    Function GetDisplayST : String;
    Procedure SetDisplayST(value : String);
    Procedure SetUserSelected(value : TFhirBoolean);
    Function GetUserSelectedST : Boolean;
    Procedure SetUserSelectedST(value : Boolean);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirCoding; overload;
    function Clone : TFhirCoding; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to The identification of the code system that defines the meaning of the symbol in the code.
    property system : String read GetSystemST write SetSystemST;
    // The identification of the code system that defines the meaning of the symbol in the code.
    property systemElement : TFhirUri read FSystem write SetSystem;

    // Typed access to The version of the code system which was used when choosing this code. Note that a well-maintained code system does not need the version reported, because the meaning of codes is consistent across versions. However this cannot consistently be assured. and when the meaning is not guaranteed to be consistent, the version SHOULD be exchanged.
    property version : String read GetVersionST write SetVersionST;
    // The version of the code system which was used when choosing this code. Note that a well-maintained code system does not need the version reported, because the meaning of codes is consistent across versions. However this cannot consistently be assured. and when the meaning is not guaranteed to be consistent, the version SHOULD be exchanged.
    property versionElement : TFhirString read FVersion write SetVersion;

    // Typed access to A symbol in syntax defined by the system. The symbol may be a predefined code or an expression in a syntax defined by the coding system (e.g. post-coordination).
    property code : String read GetCodeST write SetCodeST;
    // A symbol in syntax defined by the system. The symbol may be a predefined code or an expression in a syntax defined by the coding system (e.g. post-coordination).
    property codeElement : TFhirCode read FCode write SetCode;

    // Typed access to A representation of the meaning of the code in the system, following the rules of the system.
    property display : String read GetDisplayST write SetDisplayST;
    // A representation of the meaning of the code in the system, following the rules of the system.
    property displayElement : TFhirString read FDisplay write SetDisplay;

    // Typed access to Indicates that this coding was chosen by a user directly - i.e. off a pick list of available items (codes or displays).
    property userSelected : Boolean read GetUserSelectedST write SetUserSelectedST;
    // Indicates that this coding was chosen by a user directly - i.e. off a pick list of available items (codes or displays).
    property userSelectedElement : TFhirBoolean read FUserSelected write SetUserSelected;

  end;

  TFhirCodingListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirCodingList;
    function GetCurrent : TFhirCoding;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirCodingList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirCoding read GetCurrent;
  end;

  TFhirCodingList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirCoding;
    procedure SetItemN(index : Integer; value : TFhirCoding);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirCodingList; Overload;
    function Clone : TFhirCodingList; Overload;
    function GetEnumerator : TFhirCodingListEnumerator;
    

    //  Add a FhirCoding to the end of the list.
    function Append : TFhirCoding;

    
    // Add an already existing FhirCoding to the end of the list.
    procedure AddItem(value : TFhirCoding); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirCoding) : Integer;
    

    // Insert FhirCoding before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirCoding;
    

    // Insert an existing FhirCoding before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirCoding);
    
    // Get the iIndexth FhirCoding. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirCoding);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirCoding;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirCodings[index : Integer] : TFhirCoding read GetItemN write SetItemN; default;
  End;

  // A reference from one resource to another.
  TFhirReference = class (TFhirType)
  protected
    FReference : TFhirString;
    FDisplay : TFhirString;
    Procedure SetReference(value : TFhirString);
    Function GetReferenceST : String;
    Procedure SetReferenceST(value : String);
    Procedure SetDisplay(value : TFhirString);
    Function GetDisplayST : String;
    Procedure SetDisplayST(value : String);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirReference; overload;
    function Clone : TFhirReference; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to A reference to a location at which the other resource is found. The reference may be a relative reference, in which case it is relative to the service base URL, or an absolute URL that resolves to the location where the resource is found. The reference may be version specific or not. If the reference is not to a FHIR RESTful server, then it should be assumed to be version specific. Internal fragment references (start with '#') refer to contained resources.
    property reference : String read GetReferenceST write SetReferenceST;
    // A reference to a location at which the other resource is found. The reference may be a relative reference, in which case it is relative to the service base URL, or an absolute URL that resolves to the location where the resource is found. The reference may be version specific or not. If the reference is not to a FHIR RESTful server, then it should be assumed to be version specific. Internal fragment references (start with '#') refer to contained resources.
    property referenceElement : TFhirString read FReference write SetReference;

    // Typed access to Plain text narrative that identifies the resource in addition to the resource reference.
    property display : String read GetDisplayST write SetDisplayST;
    // Plain text narrative that identifies the resource in addition to the resource reference.
    property displayElement : TFhirString read FDisplay write SetDisplay;

  end;

  TFhirReferenceListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirReferenceList;
    function GetCurrent : TFhirReference;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirReferenceList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirReference read GetCurrent;
  end;

  TFhirReferenceList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirReference;
    procedure SetItemN(index : Integer; value : TFhirReference);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirReferenceList; Overload;
    function Clone : TFhirReferenceList; Overload;
    function GetEnumerator : TFhirReferenceListEnumerator;
    

    //  Add a FhirReference to the end of the list.
    function Append : TFhirReference;

    
    // Add an already existing FhirReference to the end of the list.
    procedure AddItem(value : TFhirReference); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirReference) : Integer;
    

    // Insert FhirReference before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirReference;
    

    // Insert an existing FhirReference before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirReference);
    
    // Get the iIndexth FhirReference. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirReference);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirReference;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirReferences[index : Integer] : TFhirReference read GetItemN write SetItemN; default;
  End;

  // A digital signature along with supporting context. The signature may be electronic/cryptographic in nature, or a graphical image representing a hand-written signature, or a signature process. Different Signature approaches have different utilities.
  TFhirSignature = class (TFhirType)
  protected
    Ftype_List : TFhirCodingList;
    FWhen : TFhirInstant;
    FWho : TFhirType;
    FContentType : TFhirCode;
    FBlob : TFhirBase64Binary;
    function GetType_List : TFhirCodingList;
    function GetHasType_List : Boolean;
    Procedure SetWhen(value : TFhirInstant);
    Function GetWhenST : TFslDateTime;
    Procedure SetWhenST(value : TFslDateTime);
    Procedure SetWho(value : TFhirType);
    Procedure SetContentType(value : TFhirCode);
    Function GetContentTypeST : String;
    Procedure SetContentTypeST(value : String);
    Procedure SetBlob(value : TFhirBase64Binary);
    Function GetBlobST : TBytes;
    Procedure SetBlobST(value : TBytes);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirSignature; overload;
    function Clone : TFhirSignature; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // An indication of the reason that the entity signed this document. This may be explicitly included as part of the signature information and can be used when determining accountability for various actions concerning the document.
    property type_List : TFhirCodingList read GetType_List;
    property hasType_List : boolean read GetHasType_List;

    // Typed access to When the digital signature was signed.
    property when : TFslDateTime read GetWhenST write SetWhenST;
    // When the digital signature was signed.
    property whenElement : TFhirInstant read FWhen write SetWhen;

    // Typed access to A reference to an application-usable description of the person that signed the certificate (e.g. the signature used their private key). (defined for API consistency)
    property who : TFhirType read FWho write SetWho;
    // A reference to an application-usable description of the person that signed the certificate (e.g. the signature used their private key).
    property whoElement : TFhirType read FWho write SetWho;

    // Typed access to A mime type that indicates the technical format of the signature. Important mime types are application/signature+xml for X ML DigSig, application/jwt for JWT, and image/* for a graphical image of a signature.
    property contentType : String read GetContentTypeST write SetContentTypeST;
    // A mime type that indicates the technical format of the signature. Important mime types are application/signature+xml for X ML DigSig, application/jwt for JWT, and image/* for a graphical image of a signature.
    property contentTypeElement : TFhirCode read FContentType write SetContentType;

    // Typed access to The base64 encoding of the Signature content.
    property blob : TBytes read GetBlobST write SetBlobST;
    // The base64 encoding of the Signature content.
    property blobElement : TFhirBase64Binary read FBlob write SetBlob;

  end;

  TFhirSignatureListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirSignatureList;
    function GetCurrent : TFhirSignature;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirSignatureList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirSignature read GetCurrent;
  end;

  TFhirSignatureList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirSignature;
    procedure SetItemN(index : Integer; value : TFhirSignature);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirSignatureList; Overload;
    function Clone : TFhirSignatureList; Overload;
    function GetEnumerator : TFhirSignatureListEnumerator;
    

    //  Add a FhirSignature to the end of the list.
    function Append : TFhirSignature;

    
    // Add an already existing FhirSignature to the end of the list.
    procedure AddItem(value : TFhirSignature); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirSignature) : Integer;
    

    // Insert FhirSignature before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirSignature;
    

    // Insert an existing FhirSignature before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirSignature);
    
    // Get the iIndexth FhirSignature. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirSignature);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirSignature;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirSignatures[index : Integer] : TFhirSignature read GetItemN write SetItemN; default;
  End;

  // A series of measurements taken by a device, with upper and lower limits. There may be more than one dimension in the data.
  TFhirSampledData = class (TFhirType)
  protected
    FOrigin : TFhirQuantity;
    FPeriod : TFhirDecimal;
    FFactor : TFhirDecimal;
    FLowerLimit : TFhirDecimal;
    FUpperLimit : TFhirDecimal;
    FDimensions : TFhirPositiveInt;
    FData : TFhirString;
    Procedure SetOrigin(value : TFhirQuantity);
    Procedure SetPeriod(value : TFhirDecimal);
    Function GetPeriodST : String;
    Procedure SetPeriodST(value : String);
    Procedure SetFactor(value : TFhirDecimal);
    Function GetFactorST : String;
    Procedure SetFactorST(value : String);
    Procedure SetLowerLimit(value : TFhirDecimal);
    Function GetLowerLimitST : String;
    Procedure SetLowerLimitST(value : String);
    Procedure SetUpperLimit(value : TFhirDecimal);
    Function GetUpperLimitST : String;
    Procedure SetUpperLimitST(value : String);
    Procedure SetDimensions(value : TFhirPositiveInt);
    Function GetDimensionsST : String;
    Procedure SetDimensionsST(value : String);
    Procedure SetData(value : TFhirString);
    Function GetDataST : String;
    Procedure SetDataST(value : String);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirSampledData; overload;
    function Clone : TFhirSampledData; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to The base quantity that a measured value of zero represents. In addition, this provides the units of the entire measurement series. (defined for API consistency)
    property origin : TFhirQuantity read FOrigin write SetOrigin;
    // The base quantity that a measured value of zero represents. In addition, this provides the units of the entire measurement series.
    property originElement : TFhirQuantity read FOrigin write SetOrigin;

    // Typed access to The length of time between sampling times, measured in milliseconds.
    property period : String read GetPeriodST write SetPeriodST;
    // The length of time between sampling times, measured in milliseconds.
    property periodElement : TFhirDecimal read FPeriod write SetPeriod;

    // Typed access to A correction factor that is applied to the sampled data points before they are added to the origin.
    property factor : String read GetFactorST write SetFactorST;
    // A correction factor that is applied to the sampled data points before they are added to the origin.
    property factorElement : TFhirDecimal read FFactor write SetFactor;

    // Typed access to The lower limit of detection of the measured points. This is needed if any of the data points have the value "L" (lower than detection limit).
    property lowerLimit : String read GetLowerLimitST write SetLowerLimitST;
    // The lower limit of detection of the measured points. This is needed if any of the data points have the value "L" (lower than detection limit).
    property lowerLimitElement : TFhirDecimal read FLowerLimit write SetLowerLimit;

    // Typed access to The upper limit of detection of the measured points. This is needed if any of the data points have the value "U" (higher than detection limit).
    property upperLimit : String read GetUpperLimitST write SetUpperLimitST;
    // The upper limit of detection of the measured points. This is needed if any of the data points have the value "U" (higher than detection limit).
    property upperLimitElement : TFhirDecimal read FUpperLimit write SetUpperLimit;

    // Typed access to The number of sample points at each time point. If this value is greater than one, then the dimensions will be interlaced - all the sample points for a point in time will be recorded at once.
    property dimensions : String read GetDimensionsST write SetDimensionsST;
    // The number of sample points at each time point. If this value is greater than one, then the dimensions will be interlaced - all the sample points for a point in time will be recorded at once.
    property dimensionsElement : TFhirPositiveInt read FDimensions write SetDimensions;

    // Typed access to A series of data points which are decimal values separated by a single space (character u20). The special values "E" (error), "L" (below detection limit) and "U" (above detection limit) can also be used in place of a decimal value.
    property data : String read GetDataST write SetDataST;
    // A series of data points which are decimal values separated by a single space (character u20). The special values "E" (error), "L" (below detection limit) and "U" (above detection limit) can also be used in place of a decimal value.
    property dataElement : TFhirString read FData write SetData;

  end;

  TFhirSampledDataListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirSampledDataList;
    function GetCurrent : TFhirSampledData;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirSampledDataList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirSampledData read GetCurrent;
  end;

  TFhirSampledDataList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirSampledData;
    procedure SetItemN(index : Integer; value : TFhirSampledData);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirSampledDataList; Overload;
    function Clone : TFhirSampledDataList; Overload;
    function GetEnumerator : TFhirSampledDataListEnumerator;
    

    //  Add a FhirSampledData to the end of the list.
    function Append : TFhirSampledData;

    
    // Add an already existing FhirSampledData to the end of the list.
    procedure AddItem(value : TFhirSampledData); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirSampledData) : Integer;
    

    // Insert FhirSampledData before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirSampledData;
    

    // Insert an existing FhirSampledData before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirSampledData);
    
    // Get the iIndexth FhirSampledData. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirSampledData);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirSampledData;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirSampledData[index : Integer] : TFhirSampledData read GetItemN write SetItemN; default;
  End;

  // A time period defined by a start and end date and optionally time.
  TFhirPeriod = class (TFhirType)
  protected
    FStart : TFhirDateTime;
    FEnd_ : TFhirDateTime;
    Procedure SetStart(value : TFhirDateTime);
    Function GetStartST : TFslDateTime;
    Procedure SetStartST(value : TFslDateTime);
    Procedure SetEnd_(value : TFhirDateTime);
    Function GetEnd_ST : TFslDateTime;
    Procedure SetEnd_ST(value : TFslDateTime);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPeriod; overload;
    function Clone : TFhirPeriod; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to The start of the period. The boundary is inclusive.
    property start : TFslDateTime read GetStartST write SetStartST;
    // The start of the period. The boundary is inclusive.
    property startElement : TFhirDateTime read FStart write SetStart;

    // Typed access to The end of the period. If the end of the period is missing, it means that the period is ongoing. The start may be in the past, and the end date in the future, which means that period is expected/planned to end at that time.
    property end_ : TFslDateTime read GetEnd_ST write SetEnd_ST;
    // The end of the period. If the end of the period is missing, it means that the period is ongoing. The start may be in the past, and the end date in the future, which means that period is expected/planned to end at that time.
    property end_Element : TFhirDateTime read FEnd_ write SetEnd_;

  end;

  TFhirPeriodListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPeriodList;
    function GetCurrent : TFhirPeriod;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPeriodList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPeriod read GetCurrent;
  end;

  TFhirPeriodList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPeriod;
    procedure SetItemN(index : Integer; value : TFhirPeriod);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPeriodList; Overload;
    function Clone : TFhirPeriodList; Overload;
    function GetEnumerator : TFhirPeriodListEnumerator;
    

    //  Add a FhirPeriod to the end of the list.
    function Append : TFhirPeriod;

    
    // Add an already existing FhirPeriod to the end of the list.
    procedure AddItem(value : TFhirPeriod); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPeriod) : Integer;
    

    // Insert FhirPeriod before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPeriod;
    

    // Insert an existing FhirPeriod before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPeriod);
    
    // Get the iIndexth FhirPeriod. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPeriod);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirPeriod;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirPeriods[index : Integer] : TFhirPeriod read GetItemN write SetItemN; default;
  End;

  // A measured amount (or an amount that can potentially be measured). Note that measured amounts include amounts that are not precisely quantified, including amounts involving arbitrary units and floating currencies.
  TFhirQuantity = class (TFhirType)
  protected
    FValue : TFhirDecimal;
    FComparator : TFhirEnum;
    FUnit_ : TFhirString;
    FSystem : TFhirUri;
    FCode : TFhirCode;
    Procedure SetValue(value : TFhirDecimal);
    Function GetValueST : String;
    Procedure SetValueST(value : String);
    Procedure SetComparator(value : TFhirEnum);
    Function GetComparatorST : TFhirQuantityComparatorEnum;
    Procedure SetComparatorST(value : TFhirQuantityComparatorEnum);
    Procedure SetUnit_(value : TFhirString);
    Function GetUnit_ST : String;
    Procedure SetUnit_ST(value : String);
    Procedure SetSystem(value : TFhirUri);
    Function GetSystemST : String;
    Procedure SetSystemST(value : String);
    Procedure SetCode(value : TFhirCode);
    Function GetCodeST : String;
    Procedure SetCodeST(value : String);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirQuantity; overload;
    function Clone : TFhirQuantity; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to The value of the measured amount. The value includes an implicit precision in the presentation of the value.
    property value : String read GetValueST write SetValueST;
    // The value of the measured amount. The value includes an implicit precision in the presentation of the value.
    property valueElement : TFhirDecimal read FValue write SetValue;

    // How the value should be understood and represented - whether the actual value is greater or less than the stated value due to measurement issues; e.g. if the comparator is "<" , then the real value is < stated value.
    property comparator : TFhirQuantityComparatorEnum read GetComparatorST write SetComparatorST;
    property comparatorElement : TFhirEnum read FComparator write SetComparator;

    // Typed access to A human-readable form of the unit.
    property unit_ : String read GetUnit_ST write SetUnit_ST;
    // A human-readable form of the unit.
    property unit_Element : TFhirString read FUnit_ write SetUnit_;

    // Typed access to The identification of the system that provides the coded form of the unit.
    property system : String read GetSystemST write SetSystemST;
    // The identification of the system that provides the coded form of the unit.
    property systemElement : TFhirUri read FSystem write SetSystem;

    // Typed access to A computer processable form of the unit in some unit representation system.
    property code : String read GetCodeST write SetCodeST;
    // A computer processable form of the unit in some unit representation system.
    property codeElement : TFhirCode read FCode write SetCode;

  end;

  TFhirQuantityListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirQuantityList;
    function GetCurrent : TFhirQuantity;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirQuantityList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirQuantity read GetCurrent;
  end;

  TFhirQuantityList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirQuantity;
    procedure SetItemN(index : Integer; value : TFhirQuantity);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirQuantityList; Overload;
    function Clone : TFhirQuantityList; Overload;
    function GetEnumerator : TFhirQuantityListEnumerator;
    

    //  Add a FhirQuantity to the end of the list.
    function Append : TFhirQuantity;

    
    // Add an already existing FhirQuantity to the end of the list.
    procedure AddItem(value : TFhirQuantity); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirQuantity) : Integer;
    

    // Insert FhirQuantity before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirQuantity;
    

    // Insert an existing FhirQuantity before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirQuantity);
    
    // Get the iIndexth FhirQuantity. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirQuantity);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirQuantity;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirQuantities[index : Integer] : TFhirQuantity read GetItemN write SetItemN; default;
  End;

  // For referring to data content defined in other formats.
  TFhirAttachment = class (TFhirType)
  protected
    FContentType : TFhirCode;
    FLanguage : TFhirCode;
    FData : TFhirBase64Binary;
    FUrl : TFhirUri;
    FSize : TFhirUnsignedInt;
    FHash : TFhirBase64Binary;
    FTitle : TFhirString;
    FCreation : TFhirDateTime;
    Procedure SetContentType(value : TFhirCode);
    Function GetContentTypeST : String;
    Procedure SetContentTypeST(value : String);
    Procedure SetLanguage(value : TFhirCode);
    Function GetLanguageST : String;
    Procedure SetLanguageST(value : String);
    Procedure SetData(value : TFhirBase64Binary);
    Function GetDataST : TBytes;
    Procedure SetDataST(value : TBytes);
    Procedure SetUrl(value : TFhirUri);
    Function GetUrlST : String;
    Procedure SetUrlST(value : String);
    Procedure SetSize(value : TFhirUnsignedInt);
    Function GetSizeST : String;
    Procedure SetSizeST(value : String);
    Procedure SetHash(value : TFhirBase64Binary);
    Function GetHashST : TBytes;
    Procedure SetHashST(value : TBytes);
    Procedure SetTitle(value : TFhirString);
    Function GetTitleST : String;
    Procedure SetTitleST(value : String);
    Procedure SetCreation(value : TFhirDateTime);
    Function GetCreationST : TFslDateTime;
    Procedure SetCreationST(value : TFslDateTime);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirAttachment; overload;
    function Clone : TFhirAttachment; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to Identifies the type of the data in the attachment and allows a method to be chosen to interpret or render the data. Includes mime type parameters such as charset where appropriate.
    property contentType : String read GetContentTypeST write SetContentTypeST;
    // Identifies the type of the data in the attachment and allows a method to be chosen to interpret or render the data. Includes mime type parameters such as charset where appropriate.
    property contentTypeElement : TFhirCode read FContentType write SetContentType;

    // Typed access to The human language of the content. The value can be any valid value according to BCP 47.
    property language : String read GetLanguageST write SetLanguageST;
    // The human language of the content. The value can be any valid value according to BCP 47.
    property languageElement : TFhirCode read FLanguage write SetLanguage;

    // Typed access to The actual data of the attachment - a sequence of bytes. In XML, represented using base64.
    property data : TBytes read GetDataST write SetDataST;
    // The actual data of the attachment - a sequence of bytes. In XML, represented using base64.
    property dataElement : TFhirBase64Binary read FData write SetData;

    // Typed access to An alternative location where the data can be accessed.
    property url : String read GetUrlST write SetUrlST;
    // An alternative location where the data can be accessed.
    property urlElement : TFhirUri read FUrl write SetUrl;

    // Typed access to The number of bytes of data that make up this attachment.
    property size : String read GetSizeST write SetSizeST;
    // The number of bytes of data that make up this attachment.
    property sizeElement : TFhirUnsignedInt read FSize write SetSize;

    // Typed access to The calculated hash of the data using SHA-1. Represented using base64.
    property hash : TBytes read GetHashST write SetHashST;
    // The calculated hash of the data using SHA-1. Represented using base64.
    property hashElement : TFhirBase64Binary read FHash write SetHash;

    // Typed access to A label or set of text to display in place of the data.
    property title : String read GetTitleST write SetTitleST;
    // A label or set of text to display in place of the data.
    property titleElement : TFhirString read FTitle write SetTitle;

    // Typed access to The date that the attachment was first created.
    property creation : TFslDateTime read GetCreationST write SetCreationST;
    // The date that the attachment was first created.
    property creationElement : TFhirDateTime read FCreation write SetCreation;

  end;

  TFhirAttachmentListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirAttachmentList;
    function GetCurrent : TFhirAttachment;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirAttachmentList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirAttachment read GetCurrent;
  end;

  TFhirAttachmentList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirAttachment;
    procedure SetItemN(index : Integer; value : TFhirAttachment);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirAttachmentList; Overload;
    function Clone : TFhirAttachmentList; Overload;
    function GetEnumerator : TFhirAttachmentListEnumerator;
    

    //  Add a FhirAttachment to the end of the list.
    function Append : TFhirAttachment;

    
    // Add an already existing FhirAttachment to the end of the list.
    procedure AddItem(value : TFhirAttachment); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirAttachment) : Integer;
    

    // Insert FhirAttachment before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirAttachment;
    

    // Insert an existing FhirAttachment before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirAttachment);
    
    // Get the iIndexth FhirAttachment. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirAttachment);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirAttachment;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirAttachments[index : Integer] : TFhirAttachment read GetItemN write SetItemN; default;
  End;

  // A relationship of two Quantity values - expressed as a numerator and a denominator.
  TFhirRatio = class (TFhirType)
  protected
    FNumerator : TFhirQuantity;
    FDenominator : TFhirQuantity;
    Procedure SetNumerator(value : TFhirQuantity);
    Procedure SetDenominator(value : TFhirQuantity);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirRatio; overload;
    function Clone : TFhirRatio; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to The value of the numerator. (defined for API consistency)
    property numerator : TFhirQuantity read FNumerator write SetNumerator;
    // The value of the numerator.
    property numeratorElement : TFhirQuantity read FNumerator write SetNumerator;

    // Typed access to The value of the denominator. (defined for API consistency)
    property denominator : TFhirQuantity read FDenominator write SetDenominator;
    // The value of the denominator.
    property denominatorElement : TFhirQuantity read FDenominator write SetDenominator;

  end;

  TFhirRatioListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirRatioList;
    function GetCurrent : TFhirRatio;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirRatioList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirRatio read GetCurrent;
  end;

  TFhirRatioList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirRatio;
    procedure SetItemN(index : Integer; value : TFhirRatio);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirRatioList; Overload;
    function Clone : TFhirRatioList; Overload;
    function GetEnumerator : TFhirRatioListEnumerator;
    

    //  Add a FhirRatio to the end of the list.
    function Append : TFhirRatio;

    
    // Add an already existing FhirRatio to the end of the list.
    procedure AddItem(value : TFhirRatio); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirRatio) : Integer;
    

    // Insert FhirRatio before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirRatio;
    

    // Insert an existing FhirRatio before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirRatio);
    
    // Get the iIndexth FhirRatio. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirRatio);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirRatio;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirRatios[index : Integer] : TFhirRatio read GetItemN write SetItemN; default;
  End;

  // A set of ordered Quantities defined by a low and high limit.
  TFhirRange = class (TFhirType)
  protected
    FLow : TFhirQuantity;
    FHigh : TFhirQuantity;
    Procedure SetLow(value : TFhirQuantity);
    Procedure SetHigh(value : TFhirQuantity);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirRange; overload;
    function Clone : TFhirRange; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to The low limit. The boundary is inclusive. (defined for API consistency)
    property low : TFhirQuantity read FLow write SetLow;
    // The low limit. The boundary is inclusive.
    property lowElement : TFhirQuantity read FLow write SetLow;

    // Typed access to The high limit. The boundary is inclusive. (defined for API consistency)
    property high : TFhirQuantity read FHigh write SetHigh;
    // The high limit. The boundary is inclusive.
    property highElement : TFhirQuantity read FHigh write SetHigh;

  end;

  TFhirRangeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirRangeList;
    function GetCurrent : TFhirRange;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirRangeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirRange read GetCurrent;
  end;

  TFhirRangeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirRange;
    procedure SetItemN(index : Integer; value : TFhirRange);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirRangeList; Overload;
    function Clone : TFhirRangeList; Overload;
    function GetEnumerator : TFhirRangeListEnumerator;
    

    //  Add a FhirRange to the end of the list.
    function Append : TFhirRange;

    
    // Add an already existing FhirRange to the end of the list.
    procedure AddItem(value : TFhirRange); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirRange) : Integer;
    

    // Insert FhirRange before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirRange;
    

    // Insert an existing FhirRange before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirRange);
    
    // Get the iIndexth FhirRange. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirRange);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirRange;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirRanges[index : Integer] : TFhirRange read GetItemN write SetItemN; default;
  End;

  // A  text note which also  contains information about who made the statement and when.
  TFhirAnnotation = class (TFhirType)
  protected
    FAuthor : TFhirType;
    FTime : TFhirDateTime;
    FText : TFhirString;
    Procedure SetAuthor(value : TFhirType);
    Procedure SetTime(value : TFhirDateTime);
    Function GetTimeST : TFslDateTime;
    Procedure SetTimeST(value : TFslDateTime);
    Procedure SetText(value : TFhirString);
    Function GetTextST : String;
    Procedure SetTextST(value : String);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirAnnotation; overload;
    function Clone : TFhirAnnotation; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to The individual responsible for making the annotation. (defined for API consistency)
    property author : TFhirType read FAuthor write SetAuthor;
    // The individual responsible for making the annotation.
    property authorElement : TFhirType read FAuthor write SetAuthor;

    // Typed access to Indicates when this particular annotation was made.
    property time : TFslDateTime read GetTimeST write SetTimeST;
    // Indicates when this particular annotation was made.
    property timeElement : TFhirDateTime read FTime write SetTime;

    // Typed access to The text of the annotation.
    property text : String read GetTextST write SetTextST;
    // The text of the annotation.
    property textElement : TFhirString read FText write SetText;

  end;

  TFhirAnnotationListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirAnnotationList;
    function GetCurrent : TFhirAnnotation;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirAnnotationList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirAnnotation read GetCurrent;
  end;

  TFhirAnnotationList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirAnnotation;
    procedure SetItemN(index : Integer; value : TFhirAnnotation);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirAnnotationList; Overload;
    function Clone : TFhirAnnotationList; Overload;
    function GetEnumerator : TFhirAnnotationListEnumerator;
    

    //  Add a FhirAnnotation to the end of the list.
    function Append : TFhirAnnotation;

    
    // Add an already existing FhirAnnotation to the end of the list.
    procedure AddItem(value : TFhirAnnotation); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirAnnotation) : Integer;
    

    // Insert FhirAnnotation before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirAnnotation;
    

    // Insert an existing FhirAnnotation before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirAnnotation);
    
    // Get the iIndexth FhirAnnotation. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirAnnotation);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirAnnotation;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirAnnotations[index : Integer] : TFhirAnnotation read GetItemN write SetItemN; default;
  End;

  // A concept that may be defined by a formal reference to a terminology or ontology or may be provided by text.
  TFhirCodeableConcept = class (TFhirType)
  protected
    FcodingList : TFhirCodingList;
    FText : TFhirString;
    function GetCodingList : TFhirCodingList;
    function GetHasCodingList : Boolean;
    Procedure SetText(value : TFhirString);
    Function GetTextST : String;
    Procedure SetTextST(value : String);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirCodeableConcept; overload;
    function Clone : TFhirCodeableConcept; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // A reference to a code defined by a terminology system.
    property codingList : TFhirCodingList read GetCodingList;
    property hasCodingList : boolean read GetHasCodingList;

    // Typed access to A human language representation of the concept as seen/selected/uttered by the user who entered the data and/or which represents the intended meaning of the user.
    property text : String read GetTextST write SetTextST;
    // A human language representation of the concept as seen/selected/uttered by the user who entered the data and/or which represents the intended meaning of the user.
    property textElement : TFhirString read FText write SetText;

  end;

  TFhirCodeableConceptListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirCodeableConceptList;
    function GetCurrent : TFhirCodeableConcept;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirCodeableConceptList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirCodeableConcept read GetCurrent;
  end;

  TFhirCodeableConceptList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirCodeableConcept;
    procedure SetItemN(index : Integer; value : TFhirCodeableConcept);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirCodeableConceptList; Overload;
    function Clone : TFhirCodeableConceptList; Overload;
    function GetEnumerator : TFhirCodeableConceptListEnumerator;
    

    //  Add a FhirCodeableConcept to the end of the list.
    function Append : TFhirCodeableConcept;

    
    // Add an already existing FhirCodeableConcept to the end of the list.
    procedure AddItem(value : TFhirCodeableConcept); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirCodeableConcept) : Integer;
    

    // Insert FhirCodeableConcept before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirCodeableConcept;
    

    // Insert an existing FhirCodeableConcept before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirCodeableConcept);
    
    // Get the iIndexth FhirCodeableConcept. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirCodeableConcept);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirCodeableConcept;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirCodeableConcepts[index : Integer] : TFhirCodeableConcept read GetItemN write SetItemN; default;
  End;

  // A human's name with the ability to identify parts and usage.
  TFhirHumanName = class (TFhirType)
  protected
    FUse : TFhirEnum;
    FText : TFhirString;
    FfamilyList : TFhirStringList;
    FgivenList : TFhirStringList;
    FprefixList : TFhirStringList;
    FsuffixList : TFhirStringList;
    FPeriod : TFhirPeriod;
    Procedure SetUse(value : TFhirEnum);
    Function GetUseST : TFhirNameUseEnum;
    Procedure SetUseST(value : TFhirNameUseEnum);
    Procedure SetText(value : TFhirString);
    Function GetTextST : String;
    Procedure SetTextST(value : String);
    function GetFamilyList : TFhirStringList;
    function GetHasFamilyList : Boolean;
    function GetGivenList : TFhirStringList;
    function GetHasGivenList : Boolean;
    function GetPrefixList : TFhirStringList;
    function GetHasPrefixList : Boolean;
    function GetSuffixList : TFhirStringList;
    function GetHasSuffixList : Boolean;
    Procedure SetPeriod(value : TFhirPeriod);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirHumanName; overload;
    function Clone : TFhirHumanName; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Identifies the purpose for this name.
    property use : TFhirNameUseEnum read GetUseST write SetUseST;
    property useElement : TFhirEnum read FUse write SetUse;

    // Typed access to A full text representation of the name.
    property text : String read GetTextST write SetTextST;
    // A full text representation of the name.
    property textElement : TFhirString read FText write SetText;

    // The part of a name that links to the genealogy. In some cultures (e.g. Eritrea) the family name of a son is the first name of his father.
    property familyList : TFhirStringList read GetFamilyList;
    property hasFamilyList : boolean read GetHasFamilyList;

    // Given name.
    property givenList : TFhirStringList read GetGivenList;
    property hasGivenList : boolean read GetHasGivenList;

    // Part of the name that is acquired as a title due to academic, legal, employment or nobility status, etc. and that appears at the start of the name.
    property prefixList : TFhirStringList read GetPrefixList;
    property hasPrefixList : boolean read GetHasPrefixList;

    // Part of the name that is acquired as a title due to academic, legal, employment or nobility status, etc. and that appears at the end of the name.
    property suffixList : TFhirStringList read GetSuffixList;
    property hasSuffixList : boolean read GetHasSuffixList;

    // Typed access to Indicates the period of time when this name was valid for the named person. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // Indicates the period of time when this name was valid for the named person.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

  end;

  TFhirHumanNameListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirHumanNameList;
    function GetCurrent : TFhirHumanName;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirHumanNameList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirHumanName read GetCurrent;
  end;

  TFhirHumanNameList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirHumanName;
    procedure SetItemN(index : Integer; value : TFhirHumanName);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirHumanNameList; Overload;
    function Clone : TFhirHumanNameList; Overload;
    function GetEnumerator : TFhirHumanNameListEnumerator;
    

    //  Add a FhirHumanName to the end of the list.
    function Append : TFhirHumanName;

    
    // Add an already existing FhirHumanName to the end of the list.
    procedure AddItem(value : TFhirHumanName); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirHumanName) : Integer;
    

    // Insert FhirHumanName before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirHumanName;
    

    // Insert an existing FhirHumanName before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirHumanName);
    
    // Get the iIndexth FhirHumanName. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirHumanName);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirHumanName;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirHumanNames[index : Integer] : TFhirHumanName read GetItemN write SetItemN; default;
  End;

  // The metadata about a resource. This is content in the resource that is maintained by the infrastructure. Changes to the content may not always be associated with version changes to the resource.
  TFhirMeta = class (TFhirType)
  protected
    FVersionId : TFhirId;
    FLastUpdated : TFhirInstant;
    FprofileList : TFhirUriList;
    FsecurityList : TFhirCodingList;
    FtagList : TFhirCodingList;
    Procedure SetVersionId(value : TFhirId);
    Function GetVersionIdST : String;
    Procedure SetVersionIdST(value : String);
    Procedure SetLastUpdated(value : TFhirInstant);
    Function GetLastUpdatedST : TFslDateTime;
    Procedure SetLastUpdatedST(value : TFslDateTime);
    function GetProfileList : TFhirUriList;
    function GetHasProfileList : Boolean;
    function GetSecurityList : TFhirCodingList;
    function GetHasSecurityList : Boolean;
    function GetTagList : TFhirCodingList;
    function GetHasTagList : Boolean;
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirMeta; overload;
    function Clone : TFhirMeta; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to The version specific identifier, as it appears in the version portion of the URL. This values changes when the resource is created, updated, or deleted.
    property versionId : String read GetVersionIdST write SetVersionIdST;
    // The version specific identifier, as it appears in the version portion of the URL. This values changes when the resource is created, updated, or deleted.
    property versionIdElement : TFhirId read FVersionId write SetVersionId;

    // Typed access to When the resource last changed - e.g. when the version changed.
    property lastUpdated : TFslDateTime read GetLastUpdatedST write SetLastUpdatedST;
    // When the resource last changed - e.g. when the version changed.
    property lastUpdatedElement : TFhirInstant read FLastUpdated write SetLastUpdated;

    // A list of profiles [[[StructureDefinition]]]s that this resource claims to conform to. The URL is a reference to [[[StructureDefinition.url]]].
    property profileList : TFhirUriList read GetProfileList;
    property hasProfileList : boolean read GetHasProfileList;

    // Security labels applied to this resource. These tags connect specific resources to the overall security policy and infrastructure.
    property securityList : TFhirCodingList read GetSecurityList;
    property hasSecurityList : boolean read GetHasSecurityList;

    // Tags applied to this resource. Tags are intended to be used to identify and relate resources to process and workflow, and applications are not required to consider the tags when interpreting the meaning of a resource.
    property tagList : TFhirCodingList read GetTagList;
    property hasTagList : boolean read GetHasTagList;

  end;

  TFhirMetaListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirMetaList;
    function GetCurrent : TFhirMeta;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirMetaList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirMeta read GetCurrent;
  end;

  TFhirMetaList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirMeta;
    procedure SetItemN(index : Integer; value : TFhirMeta);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirMetaList; Overload;
    function Clone : TFhirMetaList; Overload;
    function GetEnumerator : TFhirMetaListEnumerator;
    

    //  Add a FhirMeta to the end of the list.
    function Append : TFhirMeta;

    
    // Add an already existing FhirMeta to the end of the list.
    procedure AddItem(value : TFhirMeta); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirMeta) : Integer;
    

    // Insert FhirMeta before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirMeta;
    

    // Insert an existing FhirMeta before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirMeta);
    
    // Get the iIndexth FhirMeta. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirMeta);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirMeta;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirMeta[index : Integer] : TFhirMeta read GetItemN write SetItemN; default;
  End;

  // Details for all kinds of technology mediated contact points for a person or organization, including telephone, email, etc.
  TFhirContactPoint = class (TFhirType)
  protected
    FSystem : TFhirEnum;
    FValue : TFhirString;
    FUse : TFhirEnum;
    FRank : TFhirPositiveInt;
    FPeriod : TFhirPeriod;
    Procedure SetSystem(value : TFhirEnum);
    Function GetSystemST : TFhirContactPointSystemEnum;
    Procedure SetSystemST(value : TFhirContactPointSystemEnum);
    Procedure SetValue(value : TFhirString);
    Function GetValueST : String;
    Procedure SetValueST(value : String);
    Procedure SetUse(value : TFhirEnum);
    Function GetUseST : TFhirContactPointUseEnum;
    Procedure SetUseST(value : TFhirContactPointUseEnum);
    Procedure SetRank(value : TFhirPositiveInt);
    Function GetRankST : String;
    Procedure SetRankST(value : String);
    Procedure SetPeriod(value : TFhirPeriod);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirContactPoint; overload;
    function Clone : TFhirContactPoint; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Telecommunications form for contact point - what communications system is required to make use of the contact.
    property system : TFhirContactPointSystemEnum read GetSystemST write SetSystemST;
    property systemElement : TFhirEnum read FSystem write SetSystem;

    // Typed access to The actual contact point details, in a form that is meaningful to the designated communication system (i.e. phone number or email address).
    property value : String read GetValueST write SetValueST;
    // The actual contact point details, in a form that is meaningful to the designated communication system (i.e. phone number or email address).
    property valueElement : TFhirString read FValue write SetValue;

    // Identifies the purpose for the contact point.
    property use : TFhirContactPointUseEnum read GetUseST write SetUseST;
    property useElement : TFhirEnum read FUse write SetUse;

    // Typed access to Specifies a preferred order in which to use a set of contacts. Contacts are ranked with lower values coming before higher values.
    property rank : String read GetRankST write SetRankST;
    // Specifies a preferred order in which to use a set of contacts. Contacts are ranked with lower values coming before higher values.
    property rankElement : TFhirPositiveInt read FRank write SetRank;

    // Typed access to Time period when the contact point was/is in use. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // Time period when the contact point was/is in use.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

  end;

  TFhirContactPointListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirContactPointList;
    function GetCurrent : TFhirContactPoint;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirContactPointList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirContactPoint read GetCurrent;
  end;

  TFhirContactPointList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirContactPoint;
    procedure SetItemN(index : Integer; value : TFhirContactPoint);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirContactPointList; Overload;
    function Clone : TFhirContactPointList; Overload;
    function GetEnumerator : TFhirContactPointListEnumerator;
    

    //  Add a FhirContactPoint to the end of the list.
    function Append : TFhirContactPoint;

    
    // Add an already existing FhirContactPoint to the end of the list.
    procedure AddItem(value : TFhirContactPoint); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirContactPoint) : Integer;
    

    // Insert FhirContactPoint before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirContactPoint;
    

    // Insert an existing FhirContactPoint before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirContactPoint);
    
    // Get the iIndexth FhirContactPoint. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirContactPoint);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirContactPoint;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirContactPoints[index : Integer] : TFhirContactPoint read GetItemN write SetItemN; default;
  End;

  // There is a variety of postal address formats defined around the world. This format defines a superset that is the basis for all addresses around the world.
  TFhirAddress = class (TFhirType)
  protected
    FUse : TFhirEnum;
    FType_ : TFhirEnum;
    FText : TFhirString;
    FlineList : TFhirStringList;
    FCity : TFhirString;
    FDistrict : TFhirString;
    FState : TFhirString;
    FPostalCode : TFhirString;
    FCountry : TFhirString;
    FPeriod : TFhirPeriod;
    Procedure SetUse(value : TFhirEnum);
    Function GetUseST : TFhirAddressUseEnum;
    Procedure SetUseST(value : TFhirAddressUseEnum);
    Procedure SetType_(value : TFhirEnum);
    Function GetType_ST : TFhirAddressTypeEnum;
    Procedure SetType_ST(value : TFhirAddressTypeEnum);
    Procedure SetText(value : TFhirString);
    Function GetTextST : String;
    Procedure SetTextST(value : String);
    function GetLineList : TFhirStringList;
    function GetHasLineList : Boolean;
    Procedure SetCity(value : TFhirString);
    Function GetCityST : String;
    Procedure SetCityST(value : String);
    Procedure SetDistrict(value : TFhirString);
    Function GetDistrictST : String;
    Procedure SetDistrictST(value : String);
    Procedure SetState(value : TFhirString);
    Function GetStateST : String;
    Procedure SetStateST(value : String);
    Procedure SetPostalCode(value : TFhirString);
    Function GetPostalCodeST : String;
    Procedure SetPostalCodeST(value : String);
    Procedure SetCountry(value : TFhirString);
    Function GetCountryST : String;
    Procedure SetCountryST(value : String);
    Procedure SetPeriod(value : TFhirPeriod);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirAddress; overload;
    function Clone : TFhirAddress; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // The purpose of this address.
    property use : TFhirAddressUseEnum read GetUseST write SetUseST;
    property useElement : TFhirEnum read FUse write SetUse;

    // Distinguishes between physical addresses (those you can visit) and mailing addresses (e.g. PO Boxes and care-of addresses). Most addresses are both.
    property type_ : TFhirAddressTypeEnum read GetType_ST write SetType_ST;
    property type_Element : TFhirEnum read FType_ write SetType_;

    // Typed access to A full text representation of the address.
    property text : String read GetTextST write SetTextST;
    // A full text representation of the address.
    property textElement : TFhirString read FText write SetText;

    // This component contains the house number, apartment number, street name, street direction,  P.O. Box number, delivery hints, and similar address information.
    property lineList : TFhirStringList read GetLineList;
    property hasLineList : boolean read GetHasLineList;

    // Typed access to The name of the city, town, village or other community or delivery center.
    property city : String read GetCityST write SetCityST;
    // The name of the city, town, village or other community or delivery center.
    property cityElement : TFhirString read FCity write SetCity;

    // Typed access to The name of the administrative area (county).
    property district : String read GetDistrictST write SetDistrictST;
    // The name of the administrative area (county).
    property districtElement : TFhirString read FDistrict write SetDistrict;

    // Typed access to Sub-unit of a country with limited sovereignty in a federally organized country. A code may be used if codes are in common use (i.e. US 2 letter state codes).
    property state : String read GetStateST write SetStateST;
    // Sub-unit of a country with limited sovereignty in a federally organized country. A code may be used if codes are in common use (i.e. US 2 letter state codes).
    property stateElement : TFhirString read FState write SetState;

    // Typed access to A postal code designating a region defined by the postal service.
    property postalCode : String read GetPostalCodeST write SetPostalCodeST;
    // A postal code designating a region defined by the postal service.
    property postalCodeElement : TFhirString read FPostalCode write SetPostalCode;

    // Typed access to Country - a nation as commonly understood or generally accepted.
    property country : String read GetCountryST write SetCountryST;
    // Country - a nation as commonly understood or generally accepted.
    property countryElement : TFhirString read FCountry write SetCountry;

    // Typed access to Time period when address was/is in use. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // Time period when address was/is in use.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

  end;

  TFhirAddressListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirAddressList;
    function GetCurrent : TFhirAddress;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirAddressList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirAddress read GetCurrent;
  end;

  TFhirAddressList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirAddress;
    procedure SetItemN(index : Integer; value : TFhirAddress);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirAddressList; Overload;
    function Clone : TFhirAddressList; Overload;
    function GetEnumerator : TFhirAddressListEnumerator;
    

    //  Add a FhirAddress to the end of the list.
    function Append : TFhirAddress;

    
    // Add an already existing FhirAddress to the end of the list.
    procedure AddItem(value : TFhirAddress); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirAddress) : Integer;
    

    // Insert FhirAddress before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirAddress;
    

    // Insert an existing FhirAddress before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirAddress);
    
    // Get the iIndexth FhirAddress. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirAddress);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirAddress;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirAddresses[index : Integer] : TFhirAddress read GetItemN write SetItemN; default;
  End;

  // Indicates that the element is sliced into a set of alternative definitions (i.e. in a structure definition, there are multiple different constraints on a single element in the base resource). Slicing can be used in any resource that has cardinality ..* on the base resource, or any resource with a choice of types. The set of slices is any elements that come after this in the element sequence that have the same path, until a shorter path occurs (the shorter path terminates the set).
  TFhirElementDefinitionSlicing = class (TFhirElement)
  protected
    FdiscriminatorList : TFhirStringList;
    FDescription : TFhirString;
    FOrdered : TFhirBoolean;
    FRules : TFhirEnum;
    function GetDiscriminatorList : TFhirStringList;
    function GetHasDiscriminatorList : Boolean;
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetOrdered(value : TFhirBoolean);
    Function GetOrderedST : Boolean;
    Procedure SetOrderedST(value : Boolean);
    Procedure SetRules(value : TFhirEnum);
    Function GetRulesST : TFhirResourceSlicingRulesEnum;
    Procedure SetRulesST(value : TFhirResourceSlicingRulesEnum);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirElementDefinitionSlicing; overload;
    function Clone : TFhirElementDefinitionSlicing; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Designates which child elements are used to discriminate between the slices when processing an instance. If one or more discriminators are provided, the value of the child elements in the instance data SHALL completely distinguish which slice the element in the resource matches based on the allowed values for those elements in each of the slices.
    property discriminatorList : TFhirStringList read GetDiscriminatorList;
    property hasDiscriminatorList : boolean read GetHasDiscriminatorList;

    // Typed access to A human-readable text description of how the slicing works. If there is no discriminator, this is required to be present to provide whatever information is possible about how the slices can be differentiated.
    property description : String read GetDescriptionST write SetDescriptionST;
    // A human-readable text description of how the slicing works. If there is no discriminator, this is required to be present to provide whatever information is possible about how the slices can be differentiated.
    property descriptionElement : TFhirString read FDescription write SetDescription;

    // Typed access to If the matching elements have to occur in the same order as defined in the profile.
    property ordered : Boolean read GetOrderedST write SetOrderedST;
    // If the matching elements have to occur in the same order as defined in the profile.
    property orderedElement : TFhirBoolean read FOrdered write SetOrdered;

    // Whether additional slices are allowed or not. When the slices are ordered, profile authors can also say that additional slices are only allowed at the end.
    property rules : TFhirResourceSlicingRulesEnum read GetRulesST write SetRulesST;
    property rulesElement : TFhirEnum read FRules write SetRules;

  end;

  TFhirElementDefinitionSlicingListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionSlicingList;
    function GetCurrent : TFhirElementDefinitionSlicing;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirElementDefinitionSlicingList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionSlicing read GetCurrent;
  end;

  TFhirElementDefinitionSlicingList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirElementDefinitionSlicing;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionSlicing);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirElementDefinitionSlicingList; Overload;
    function Clone : TFhirElementDefinitionSlicingList; Overload;
    function GetEnumerator : TFhirElementDefinitionSlicingListEnumerator;
    

    //  Add a FhirElementDefinitionSlicing to the end of the list.
    function Append : TFhirElementDefinitionSlicing;

    
    // Add an already existing FhirElementDefinitionSlicing to the end of the list.
    procedure AddItem(value : TFhirElementDefinitionSlicing); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirElementDefinitionSlicing) : Integer;
    

    // Insert FhirElementDefinitionSlicing before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirElementDefinitionSlicing;
    

    // Insert an existing FhirElementDefinitionSlicing before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionSlicing);
    
    // Get the iIndexth FhirElementDefinitionSlicing. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionSlicing);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirElementDefinitionSlicing;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirElementDefinitionSlicings[index : Integer] : TFhirElementDefinitionSlicing read GetItemN write SetItemN; default;
  End;

  // Information about the base definition of the element, provided to make it unncessary for tools to trace the deviation of the element through the derived and related profiles. This information is only provided where the element definition represents a constraint on another element definition, and must be present if there is a base element definition.
  TFhirElementDefinitionBase = class (TFhirElement)
  protected
    FPath : TFhirString;
    FMin : TFhirInteger;
    FMax : TFhirString;
    Procedure SetPath(value : TFhirString);
    Function GetPathST : String;
    Procedure SetPathST(value : String);
    Procedure SetMin(value : TFhirInteger);
    Function GetMinST : String;
    Procedure SetMinST(value : String);
    Procedure SetMax(value : TFhirString);
    Function GetMaxST : String;
    Procedure SetMaxST(value : String);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirElementDefinitionBase; overload;
    function Clone : TFhirElementDefinitionBase; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to The Path that identifies the base element - this matches the ElementDefinition.path for that element. Across FHIR, there is only one base definition of any element - that is, an element definition on a [[[StructureDefinition]]] without a StructureDefinition.base.
    property path : String read GetPathST write SetPathST;
    // The Path that identifies the base element - this matches the ElementDefinition.path for that element. Across FHIR, there is only one base definition of any element - that is, an element definition on a [[[StructureDefinition]]] without a StructureDefinition.base.
    property pathElement : TFhirString read FPath write SetPath;

    // Typed access to Minimum cardinality of the base element identified by the path.
    property min : String read GetMinST write SetMinST;
    // Minimum cardinality of the base element identified by the path.
    property minElement : TFhirInteger read FMin write SetMin;

    // Typed access to Maximum cardinality of the base element identified by the path.
    property max : String read GetMaxST write SetMaxST;
    // Maximum cardinality of the base element identified by the path.
    property maxElement : TFhirString read FMax write SetMax;

  end;

  TFhirElementDefinitionBaseListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionBaseList;
    function GetCurrent : TFhirElementDefinitionBase;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirElementDefinitionBaseList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionBase read GetCurrent;
  end;

  TFhirElementDefinitionBaseList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirElementDefinitionBase;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionBase);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirElementDefinitionBaseList; Overload;
    function Clone : TFhirElementDefinitionBaseList; Overload;
    function GetEnumerator : TFhirElementDefinitionBaseListEnumerator;
    

    //  Add a FhirElementDefinitionBase to the end of the list.
    function Append : TFhirElementDefinitionBase;

    
    // Add an already existing FhirElementDefinitionBase to the end of the list.
    procedure AddItem(value : TFhirElementDefinitionBase); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirElementDefinitionBase) : Integer;
    

    // Insert FhirElementDefinitionBase before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirElementDefinitionBase;
    

    // Insert an existing FhirElementDefinitionBase before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionBase);
    
    // Get the iIndexth FhirElementDefinitionBase. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionBase);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirElementDefinitionBase;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirElementDefinitionBases[index : Integer] : TFhirElementDefinitionBase read GetItemN write SetItemN; default;
  End;

  // The data type or resource that the value of this element is permitted to be.
  TFhirElementDefinitionType = class (TFhirElement)
  protected
    FCode : TFhirCode;
    FprofileList : TFhirUriList;
    FAggregation : TFhirEnumList;
    Procedure SetCode(value : TFhirCode);
    Function GetCodeST : String;
    Procedure SetCodeST(value : String);
    function GetProfileList : TFhirUriList;
    function GetHasProfileList : Boolean;
    function GetAggregation : TFhirEnumList;
    function GetHasAggregation : Boolean;
    Function GetAggregationST : TFhirResourceAggregationModeEnumList;
    Procedure SetAggregationST(value : TFhirResourceAggregationModeEnumList);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirElementDefinitionType; overload;
    function Clone : TFhirElementDefinitionType; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to Name of Data type or Resource that is a(or the) type used for this element.
    property code : String read GetCodeST write SetCodeST;
    // Name of Data type or Resource that is a(or the) type used for this element.
    property codeElement : TFhirCode read FCode write SetCode;

    // Identifies a profile structure or implementation Guide that SHALL hold for resources or datatypes referenced as the type of this element. Can be a local reference - to another structure in this profile, or a reference to a structure in another profile. When more than one profile is specified, the content must conform to all of them. When an implementation guide is specified, the resource SHALL conform to at least one profile defined in the implementation guide.
    property profileList : TFhirUriList read GetProfileList;
    property hasProfileList : boolean read GetHasProfileList;

    // If the type is a reference to another resource, how the resource is or can be aggregated - is it a contained resource, or a reference, and if the context is a bundle, is it included in the bundle.
    property aggregation : TFhirResourceAggregationModeEnumList read GetAggregationST write SetAggregationST;
    property aggregationList : TFhirEnumList read GetAggregation;
    property hasAggregation : boolean read GetHasAggregation;
  end;

  TFhirElementDefinitionTypeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionTypeList;
    function GetCurrent : TFhirElementDefinitionType;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirElementDefinitionTypeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionType read GetCurrent;
  end;

  TFhirElementDefinitionTypeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirElementDefinitionType;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionType);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirElementDefinitionTypeList; Overload;
    function Clone : TFhirElementDefinitionTypeList; Overload;
    function GetEnumerator : TFhirElementDefinitionTypeListEnumerator;
    

    //  Add a FhirElementDefinitionType to the end of the list.
    function Append : TFhirElementDefinitionType;

    
    // Add an already existing FhirElementDefinitionType to the end of the list.
    procedure AddItem(value : TFhirElementDefinitionType); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirElementDefinitionType) : Integer;
    

    // Insert FhirElementDefinitionType before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirElementDefinitionType;
    

    // Insert an existing FhirElementDefinitionType before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionType);
    
    // Get the iIndexth FhirElementDefinitionType. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionType);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirElementDefinitionType;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirElementDefinitionTypes[index : Integer] : TFhirElementDefinitionType read GetItemN write SetItemN; default;
  End;

  // Formal constraints such as co-occurrence and other constraints that can be computationally evaluated within the context of the instance.
  TFhirElementDefinitionConstraint = class (TFhirElement)
  protected
    FKey : TFhirId;
    FRequirements : TFhirString;
    FSeverity : TFhirEnum;
    FHuman : TFhirString;
    FXpath : TFhirString;
    Procedure SetKey(value : TFhirId);
    Function GetKeyST : String;
    Procedure SetKeyST(value : String);
    Procedure SetRequirements(value : TFhirString);
    Function GetRequirementsST : String;
    Procedure SetRequirementsST(value : String);
    Procedure SetSeverity(value : TFhirEnum);
    Function GetSeverityST : TFhirConstraintSeverityEnum;
    Procedure SetSeverityST(value : TFhirConstraintSeverityEnum);
    Procedure SetHuman(value : TFhirString);
    Function GetHumanST : String;
    Procedure SetHumanST(value : String);
    Procedure SetXpath(value : TFhirString);
    Function GetXpathST : String;
    Procedure SetXpathST(value : String);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirElementDefinitionConstraint; overload;
    function Clone : TFhirElementDefinitionConstraint; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to Allows identification of which elements have their cardinalities impacted by the constraint.  Will not be referenced for constraints that do not affect cardinality.
    property key : String read GetKeyST write SetKeyST;
    // Allows identification of which elements have their cardinalities impacted by the constraint.  Will not be referenced for constraints that do not affect cardinality.
    property keyElement : TFhirId read FKey write SetKey;

    // Typed access to Description of why this constraint is necessary or appropriate.
    property requirements : String read GetRequirementsST write SetRequirementsST;
    // Description of why this constraint is necessary or appropriate.
    property requirementsElement : TFhirString read FRequirements write SetRequirements;

    // Identifies the impact constraint violation has on the conformance of the instance.
    property severity : TFhirConstraintSeverityEnum read GetSeverityST write SetSeverityST;
    property severityElement : TFhirEnum read FSeverity write SetSeverity;

    // Typed access to Text that can be used to describe the constraint in messages identifying that the constraint has been violated.
    property human : String read GetHumanST write SetHumanST;
    // Text that can be used to describe the constraint in messages identifying that the constraint has been violated.
    property humanElement : TFhirString read FHuman write SetHuman;

    // Typed access to An XPath expression of constraint that can be executed to see if this constraint is met.
    property xpath : String read GetXpathST write SetXpathST;
    // An XPath expression of constraint that can be executed to see if this constraint is met.
    property xpathElement : TFhirString read FXpath write SetXpath;

  end;

  TFhirElementDefinitionConstraintListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionConstraintList;
    function GetCurrent : TFhirElementDefinitionConstraint;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirElementDefinitionConstraintList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionConstraint read GetCurrent;
  end;

  TFhirElementDefinitionConstraintList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirElementDefinitionConstraint;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionConstraint);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirElementDefinitionConstraintList; Overload;
    function Clone : TFhirElementDefinitionConstraintList; Overload;
    function GetEnumerator : TFhirElementDefinitionConstraintListEnumerator;
    

    //  Add a FhirElementDefinitionConstraint to the end of the list.
    function Append : TFhirElementDefinitionConstraint;

    
    // Add an already existing FhirElementDefinitionConstraint to the end of the list.
    procedure AddItem(value : TFhirElementDefinitionConstraint); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirElementDefinitionConstraint) : Integer;
    

    // Insert FhirElementDefinitionConstraint before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirElementDefinitionConstraint;
    

    // Insert an existing FhirElementDefinitionConstraint before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionConstraint);
    
    // Get the iIndexth FhirElementDefinitionConstraint. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionConstraint);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirElementDefinitionConstraint;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirElementDefinitionConstraints[index : Integer] : TFhirElementDefinitionConstraint read GetItemN write SetItemN; default;
  End;

  // Binds to a value set if this element is coded (code, Coding, CodeableConcept).
  TFhirElementDefinitionBinding = class (TFhirElement)
  protected
    FStrength : TFhirEnum;
    FDescription : TFhirString;
    FValueSet : TFhirType;
    Procedure SetStrength(value : TFhirEnum);
    Function GetStrengthST : TFhirBindingStrengthEnum;
    Procedure SetStrengthST(value : TFhirBindingStrengthEnum);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetValueSet(value : TFhirType);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirElementDefinitionBinding; overload;
    function Clone : TFhirElementDefinitionBinding; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Indicates the degree of conformance expectations associated with this binding - that is, the degree to which the provided value set must be adhered to in the instances.
    property strength : TFhirBindingStrengthEnum read GetStrengthST write SetStrengthST;
    property strengthElement : TFhirEnum read FStrength write SetStrength;

    // Typed access to Describes the intended use of this particular set of codes.
    property description : String read GetDescriptionST write SetDescriptionST;
    // Describes the intended use of this particular set of codes.
    property descriptionElement : TFhirString read FDescription write SetDescription;

    // Typed access to Points to the value set or external definition (e.g. implicit value set) that identifies the set of codes to be used. (defined for API consistency)
    property valueSet : TFhirType read FValueSet write SetValueSet;
    // Points to the value set or external definition (e.g. implicit value set) that identifies the set of codes to be used.
    property valueSetElement : TFhirType read FValueSet write SetValueSet;

  end;

  TFhirElementDefinitionBindingListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionBindingList;
    function GetCurrent : TFhirElementDefinitionBinding;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirElementDefinitionBindingList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionBinding read GetCurrent;
  end;

  TFhirElementDefinitionBindingList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirElementDefinitionBinding;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionBinding);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirElementDefinitionBindingList; Overload;
    function Clone : TFhirElementDefinitionBindingList; Overload;
    function GetEnumerator : TFhirElementDefinitionBindingListEnumerator;
    

    //  Add a FhirElementDefinitionBinding to the end of the list.
    function Append : TFhirElementDefinitionBinding;

    
    // Add an already existing FhirElementDefinitionBinding to the end of the list.
    procedure AddItem(value : TFhirElementDefinitionBinding); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirElementDefinitionBinding) : Integer;
    

    // Insert FhirElementDefinitionBinding before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirElementDefinitionBinding;
    

    // Insert an existing FhirElementDefinitionBinding before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionBinding);
    
    // Get the iIndexth FhirElementDefinitionBinding. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionBinding);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirElementDefinitionBinding;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirElementDefinitionBindings[index : Integer] : TFhirElementDefinitionBinding read GetItemN write SetItemN; default;
  End;

  // Identifies a concept from an external specification that roughly corresponds to this element.
  TFhirElementDefinitionMapping = class (TFhirElement)
  protected
    FIdentity : TFhirId;
    FLanguage : TFhirCode;
    FMap : TFhirString;
    Procedure SetIdentity(value : TFhirId);
    Function GetIdentityST : String;
    Procedure SetIdentityST(value : String);
    Procedure SetLanguage(value : TFhirCode);
    Function GetLanguageST : String;
    Procedure SetLanguageST(value : String);
    Procedure SetMap(value : TFhirString);
    Function GetMapST : String;
    Procedure SetMapST(value : String);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirElementDefinitionMapping; overload;
    function Clone : TFhirElementDefinitionMapping; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to An internal reference to the definition of a mapping.
    property identity : String read GetIdentityST write SetIdentityST;
    // An internal reference to the definition of a mapping.
    property identityElement : TFhirId read FIdentity write SetIdentity;

    // Typed access to Identifies the computable language in which mapping.map is expressed.
    property language : String read GetLanguageST write SetLanguageST;
    // Identifies the computable language in which mapping.map is expressed.
    property languageElement : TFhirCode read FLanguage write SetLanguage;

    // Typed access to Expresses what part of the target specification corresponds to this element.
    property map : String read GetMapST write SetMapST;
    // Expresses what part of the target specification corresponds to this element.
    property mapElement : TFhirString read FMap write SetMap;

  end;

  TFhirElementDefinitionMappingListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionMappingList;
    function GetCurrent : TFhirElementDefinitionMapping;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirElementDefinitionMappingList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionMapping read GetCurrent;
  end;

  TFhirElementDefinitionMappingList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirElementDefinitionMapping;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionMapping);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirElementDefinitionMappingList; Overload;
    function Clone : TFhirElementDefinitionMappingList; Overload;
    function GetEnumerator : TFhirElementDefinitionMappingListEnumerator;
    

    //  Add a FhirElementDefinitionMapping to the end of the list.
    function Append : TFhirElementDefinitionMapping;

    
    // Add an already existing FhirElementDefinitionMapping to the end of the list.
    procedure AddItem(value : TFhirElementDefinitionMapping); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirElementDefinitionMapping) : Integer;
    

    // Insert FhirElementDefinitionMapping before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirElementDefinitionMapping;
    

    // Insert an existing FhirElementDefinitionMapping before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionMapping);
    
    // Get the iIndexth FhirElementDefinitionMapping. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionMapping);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirElementDefinitionMapping;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirElementDefinitionMappings[index : Integer] : TFhirElementDefinitionMapping read GetItemN write SetItemN; default;
  End;

  // Captures constraints on each element within the resource, profile, or extension.
  TFhirElementDefinition = class (TFhirType)
  protected
    FPath : TFhirString;
    FRepresentation : TFhirEnumList;
    FName : TFhirString;
    FLabel_ : TFhirString;
    FcodeList : TFhirCodingList;
    FSlicing : TFhirElementDefinitionSlicing;
    FShort : TFhirString;
    FDefinition : TFhirMarkdown;
    FComments : TFhirMarkdown;
    FRequirements : TFhirMarkdown;
    FaliasList : TFhirStringList;
    FMin : TFhirInteger;
    FMax : TFhirString;
    FBase : TFhirElementDefinitionBase;
    Ftype_List : TFhirElementDefinitionTypeList;
    FNameReference : TFhirString;
    FDefaultValue : TFhirType;
    FMeaningWhenMissing : TFhirMarkdown;
    FFixed : TFhirType;
    FPattern : TFhirType;
    FExample : TFhirType;
    FMinValue : TFhirType;
    FMaxValue : TFhirType;
    FMaxLength : TFhirInteger;
    FconditionList : TFhirIdList;
    FconstraintList : TFhirElementDefinitionConstraintList;
    FMustSupport : TFhirBoolean;
    FIsModifier : TFhirBoolean;
    FIsSummary : TFhirBoolean;
    FBinding : TFhirElementDefinitionBinding;
    FmappingList : TFhirElementDefinitionMappingList;
    Procedure SetPath(value : TFhirString);
    Function GetPathST : String;
    Procedure SetPathST(value : String);
    function GetRepresentation : TFhirEnumList;
    function GetHasRepresentation : Boolean;
    Function GetRepresentationST : TFhirPropertyRepresentationEnumList;
    Procedure SetRepresentationST(value : TFhirPropertyRepresentationEnumList);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetLabel_(value : TFhirString);
    Function GetLabel_ST : String;
    Procedure SetLabel_ST(value : String);
    function GetCodeList : TFhirCodingList;
    function GetHasCodeList : Boolean;
    Procedure SetSlicing(value : TFhirElementDefinitionSlicing);
    Procedure SetShort(value : TFhirString);
    Function GetShortST : String;
    Procedure SetShortST(value : String);
    Procedure SetDefinition(value : TFhirMarkdown);
    Function GetDefinitionST : String;
    Procedure SetDefinitionST(value : String);
    Procedure SetComments(value : TFhirMarkdown);
    Function GetCommentsST : String;
    Procedure SetCommentsST(value : String);
    Procedure SetRequirements(value : TFhirMarkdown);
    Function GetRequirementsST : String;
    Procedure SetRequirementsST(value : String);
    function GetAliasList : TFhirStringList;
    function GetHasAliasList : Boolean;
    Procedure SetMin(value : TFhirInteger);
    Function GetMinST : String;
    Procedure SetMinST(value : String);
    Procedure SetMax(value : TFhirString);
    Function GetMaxST : String;
    Procedure SetMaxST(value : String);
    Procedure SetBase(value : TFhirElementDefinitionBase);
    function GetType_List : TFhirElementDefinitionTypeList;
    function GetHasType_List : Boolean;
    Procedure SetNameReference(value : TFhirString);
    Function GetNameReferenceST : String;
    Procedure SetNameReferenceST(value : String);
    Procedure SetDefaultValue(value : TFhirType);
    Procedure SetMeaningWhenMissing(value : TFhirMarkdown);
    Function GetMeaningWhenMissingST : String;
    Procedure SetMeaningWhenMissingST(value : String);
    Procedure SetFixed(value : TFhirType);
    Procedure SetPattern(value : TFhirType);
    Procedure SetExample(value : TFhirType);
    Procedure SetMinValue(value : TFhirType);
    Procedure SetMaxValue(value : TFhirType);
    Procedure SetMaxLength(value : TFhirInteger);
    Function GetMaxLengthST : String;
    Procedure SetMaxLengthST(value : String);
    function GetConditionList : TFhirIdList;
    function GetHasConditionList : Boolean;
    function GetConstraintList : TFhirElementDefinitionConstraintList;
    function GetHasConstraintList : Boolean;
    Procedure SetMustSupport(value : TFhirBoolean);
    Function GetMustSupportST : Boolean;
    Procedure SetMustSupportST(value : Boolean);
    Procedure SetIsModifier(value : TFhirBoolean);
    Function GetIsModifierST : Boolean;
    Procedure SetIsModifierST(value : Boolean);
    Procedure SetIsSummary(value : TFhirBoolean);
    Function GetIsSummaryST : Boolean;
    Procedure SetIsSummaryST(value : Boolean);
    Procedure SetBinding(value : TFhirElementDefinitionBinding);
    function GetMappingList : TFhirElementDefinitionMappingList;
    function GetHasMappingList : Boolean;
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirElementDefinition; overload;
    function Clone : TFhirElementDefinition; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to The path identifies the element and is expressed as a "."-separated list of ancestor elements, beginning with the name of the resource or extension.
    property path : String read GetPathST write SetPathST;
    // The path identifies the element and is expressed as a "."-separated list of ancestor elements, beginning with the name of the resource or extension.
    property pathElement : TFhirString read FPath write SetPath;

    // Codes that define how this element is represented in instances, when the deviation varies from the normal case.
    property representation : TFhirPropertyRepresentationEnumList read GetRepresentationST write SetRepresentationST;
    property representationList : TFhirEnumList read GetRepresentation;
    property hasRepresentation : boolean read GetHasRepresentation;
    // Typed access to The name of this element definition (to refer to it from other element definitions using ElementDefinition.nameReference). This is a unique name referring to a specific set of constraints applied to this element. One use of this is to provide a name to different slices of the same element.
    property name : String read GetNameST write SetNameST;
    // The name of this element definition (to refer to it from other element definitions using ElementDefinition.nameReference). This is a unique name referring to a specific set of constraints applied to this element. One use of this is to provide a name to different slices of the same element.
    property nameElement : TFhirString read FName write SetName;

    // Typed access to The text to display beside the element indicating its meaning or to use to prompt for the element in a user display or form.
    property label_ : String read GetLabel_ST write SetLabel_ST;
    // The text to display beside the element indicating its meaning or to use to prompt for the element in a user display or form.
    property label_Element : TFhirString read FLabel_ write SetLabel_;

    // A code that provides the meaning for the element according to a particular terminology.
    property codeList : TFhirCodingList read GetCodeList;
    property hasCodeList : boolean read GetHasCodeList;

    // Typed access to Indicates that the element is sliced into a set of alternative definitions (i.e. in a structure definition, there are multiple different constraints on a single element in the base resource). Slicing can be used in any resource that has cardinality ..* on the base resource, or any resource with a choice of types. The set of slices is any elements that come after this in the element sequence that have the same path, until a shorter path occurs (the shorter path terminates the set). (defined for API consistency)
    property slicing : TFhirElementDefinitionSlicing read FSlicing write SetSlicing;
    // Indicates that the element is sliced into a set of alternative definitions (i.e. in a structure definition, there are multiple different constraints on a single element in the base resource). Slicing can be used in any resource that has cardinality ..* on the base resource, or any resource with a choice of types. The set of slices is any elements that come after this in the element sequence that have the same path, until a shorter path occurs (the shorter path terminates the set).
    property slicingElement : TFhirElementDefinitionSlicing read FSlicing write SetSlicing;

    // Typed access to A concise description of what this element means (e.g. for use in autogenerated summaries).
    property short : String read GetShortST write SetShortST;
    // A concise description of what this element means (e.g. for use in autogenerated summaries).
    property shortElement : TFhirString read FShort write SetShort;

    // Typed access to Provides a complete explanation of the meaning of the data element for human readability.  For the case of elements derived from existing elements (e.g. constraints), the definition SHALL be consistent with the base definition, but convey the meaning of the element in the particular context of use of the resource.
    property definition : String read GetDefinitionST write SetDefinitionST;
    // Provides a complete explanation of the meaning of the data element for human readability.  For the case of elements derived from existing elements (e.g. constraints), the definition SHALL be consistent with the base definition, but convey the meaning of the element in the particular context of use of the resource.
    property definitionElement : TFhirMarkdown read FDefinition write SetDefinition;

    // Typed access to Explanatory notes and implementation guidance about the data element, including notes about how to use the data properly, exceptions to proper use, etc.
    property comments : String read GetCommentsST write SetCommentsST;
    // Explanatory notes and implementation guidance about the data element, including notes about how to use the data properly, exceptions to proper use, etc.
    property commentsElement : TFhirMarkdown read FComments write SetComments;

    // Typed access to This element is for traceability of why the element was created and why the constraints exist as they do. This may be used to point to source materials or specifications that drove the structure of this element.
    property requirements : String read GetRequirementsST write SetRequirementsST;
    // This element is for traceability of why the element was created and why the constraints exist as they do. This may be used to point to source materials or specifications that drove the structure of this element.
    property requirementsElement : TFhirMarkdown read FRequirements write SetRequirements;

    // Identifies additional names by which this element might also be known.
    property aliasList : TFhirStringList read GetAliasList;
    property hasAliasList : boolean read GetHasAliasList;

    // Typed access to The minimum number of times this element SHALL appear in the instance.
    property min : String read GetMinST write SetMinST;
    // The minimum number of times this element SHALL appear in the instance.
    property minElement : TFhirInteger read FMin write SetMin;

    // Typed access to The maximum number of times this element is permitted to appear in the instance.
    property max : String read GetMaxST write SetMaxST;
    // The maximum number of times this element is permitted to appear in the instance.
    property maxElement : TFhirString read FMax write SetMax;

    // Typed access to Information about the base definition of the element, provided to make it unncessary for tools to trace the deviation of the element through the derived and related profiles. This information is only provided where the element definition represents a constraint on another element definition, and must be present if there is a base element definition. (defined for API consistency)
    property base : TFhirElementDefinitionBase read FBase write SetBase;
    // Information about the base definition of the element, provided to make it unncessary for tools to trace the deviation of the element through the derived and related profiles. This information is only provided where the element definition represents a constraint on another element definition, and must be present if there is a base element definition.
    property baseElement : TFhirElementDefinitionBase read FBase write SetBase;

    // The data type or resource that the value of this element is permitted to be.
    property type_List : TFhirElementDefinitionTypeList read GetType_List;
    property hasType_List : boolean read GetHasType_List;

    // Typed access to Identifies the name of a slice defined elsewhere in the profile whose constraints should be applied to the current element.
    property nameReference : String read GetNameReferenceST write SetNameReferenceST;
    // Identifies the name of a slice defined elsewhere in the profile whose constraints should be applied to the current element.
    property nameReferenceElement : TFhirString read FNameReference write SetNameReference;

    // Typed access to The value that should be used if there is no value stated in the instance (e.g. 'if not otherwise specified, the abstract is false'). (defined for API consistency)
    property defaultValue : TFhirType read FDefaultValue write SetDefaultValue;
    // The value that should be used if there is no value stated in the instance (e.g. 'if not otherwise specified, the abstract is false').
    property defaultValueElement : TFhirType read FDefaultValue write SetDefaultValue;

    // Typed access to The Implicit meaning that is to be understood when this element is missing (e.g. 'when this element is missing, the period is ongoing'.
    property meaningWhenMissing : String read GetMeaningWhenMissingST write SetMeaningWhenMissingST;
    // The Implicit meaning that is to be understood when this element is missing (e.g. 'when this element is missing, the period is ongoing'.
    property meaningWhenMissingElement : TFhirMarkdown read FMeaningWhenMissing write SetMeaningWhenMissing;

    // Typed access to Specifies a value that SHALL be exactly the value  for this element in the instance. For purposes of comparison, non-significant whitespace is ignored, and all values must be an exact match (case and accent sensitive). Missing elements/attributes must also be missing. (defined for API consistency)
    property fixed : TFhirType read FFixed write SetFixed;
    // Specifies a value that SHALL be exactly the value  for this element in the instance. For purposes of comparison, non-significant whitespace is ignored, and all values must be an exact match (case and accent sensitive). Missing elements/attributes must also be missing.
    property fixedElement : TFhirType read FFixed write SetFixed;

    // Typed access to Specifies a value that the value in the instance SHALL follow - that is, any value in the pattern must be found in the instance. Other additional values may be found too. This is effectively constraint by example.  The values of elements present in the pattern must match exactly (case-sensitive, accent-sensitive, etc.). (defined for API consistency)
    property pattern : TFhirType read FPattern write SetPattern;
    // Specifies a value that the value in the instance SHALL follow - that is, any value in the pattern must be found in the instance. Other additional values may be found too. This is effectively constraint by example.  The values of elements present in the pattern must match exactly (case-sensitive, accent-sensitive, etc.).
    property patternElement : TFhirType read FPattern write SetPattern;

    // Typed access to A sample value for this element demonstrating the type of information that would typically be captured. (defined for API consistency)
    property example : TFhirType read FExample write SetExample;
    // A sample value for this element demonstrating the type of information that would typically be captured.
    property exampleElement : TFhirType read FExample write SetExample;

    // Typed access to The minimum allowed value for the element. The value is inclusive. This is allowed for the types date, dateTime, instant, time, decimal, integer, and Quantity. (defined for API consistency)
    property minValue : TFhirType read FMinValue write SetMinValue;
    // The minimum allowed value for the element. The value is inclusive. This is allowed for the types date, dateTime, instant, time, decimal, integer, and Quantity.
    property minValueElement : TFhirType read FMinValue write SetMinValue;

    // Typed access to The maximum allowed value for the element. The value is inclusive. This is allowed for the types date, dateTime, instant, time, decimal, integer, and Quantity. (defined for API consistency)
    property maxValue : TFhirType read FMaxValue write SetMaxValue;
    // The maximum allowed value for the element. The value is inclusive. This is allowed for the types date, dateTime, instant, time, decimal, integer, and Quantity.
    property maxValueElement : TFhirType read FMaxValue write SetMaxValue;

    // Typed access to Indicates the maximum length in characters that is permitted to be present in conformant instances and which is expected to be supported by conformant consumers that support the element.
    property maxLength : String read GetMaxLengthST write SetMaxLengthST;
    // Indicates the maximum length in characters that is permitted to be present in conformant instances and which is expected to be supported by conformant consumers that support the element.
    property maxLengthElement : TFhirInteger read FMaxLength write SetMaxLength;

    // A reference to an invariant that may make additional statements about the cardinality or value in the instance.
    property conditionList : TFhirIdList read GetConditionList;
    property hasConditionList : boolean read GetHasConditionList;

    // Formal constraints such as co-occurrence and other constraints that can be computationally evaluated within the context of the instance.
    property constraintList : TFhirElementDefinitionConstraintList read GetConstraintList;
    property hasConstraintList : boolean read GetHasConstraintList;

    // Typed access to If true, implementations that produce or consume resources SHALL provide "support" for the element in some meaningful way.  If false, the element may be ignored and not supported.
    property mustSupport : Boolean read GetMustSupportST write SetMustSupportST;
    // If true, implementations that produce or consume resources SHALL provide "support" for the element in some meaningful way.  If false, the element may be ignored and not supported.
    property mustSupportElement : TFhirBoolean read FMustSupport write SetMustSupport;

    // Typed access to If true, the value of this element affects the interpretation of the element or resource that contains it, and the value of the element cannot be ignored. Typically, this is used for status, negation and qualification codes. The effect of this is that the element cannot be ignored by systems: they SHALL either recognize the element and process it, and/or a pre-determination has been made that it is not relevant to their particular system.
    property isModifier : Boolean read GetIsModifierST write SetIsModifierST;
    // If true, the value of this element affects the interpretation of the element or resource that contains it, and the value of the element cannot be ignored. Typically, this is used for status, negation and qualification codes. The effect of this is that the element cannot be ignored by systems: they SHALL either recognize the element and process it, and/or a pre-determination has been made that it is not relevant to their particular system.
    property isModifierElement : TFhirBoolean read FIsModifier write SetIsModifier;

    // Typed access to Whether the element should be included if a client requests a search with the parameter _summary=true.
    property isSummary : Boolean read GetIsSummaryST write SetIsSummaryST;
    // Whether the element should be included if a client requests a search with the parameter _summary=true.
    property isSummaryElement : TFhirBoolean read FIsSummary write SetIsSummary;

    // Typed access to Binds to a value set if this element is coded (code, Coding, CodeableConcept). (defined for API consistency)
    property binding : TFhirElementDefinitionBinding read FBinding write SetBinding;
    // Binds to a value set if this element is coded (code, Coding, CodeableConcept).
    property bindingElement : TFhirElementDefinitionBinding read FBinding write SetBinding;

    // Identifies a concept from an external specification that roughly corresponds to this element.
    property mappingList : TFhirElementDefinitionMappingList read GetMappingList;
    property hasMappingList : boolean read GetHasMappingList;

  end;

  TFhirElementDefinitionListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionList;
    function GetCurrent : TFhirElementDefinition;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirElementDefinitionList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinition read GetCurrent;
  end;

  TFhirElementDefinitionList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirElementDefinition;
    procedure SetItemN(index : Integer; value : TFhirElementDefinition);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirElementDefinitionList; Overload;
    function Clone : TFhirElementDefinitionList; Overload;
    function GetEnumerator : TFhirElementDefinitionListEnumerator;
    

    //  Add a FhirElementDefinition to the end of the list.
    function Append : TFhirElementDefinition;

    
    // Add an already existing FhirElementDefinition to the end of the list.
    procedure AddItem(value : TFhirElementDefinition); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirElementDefinition) : Integer;
    

    // Insert FhirElementDefinition before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirElementDefinition;
    

    // Insert an existing FhirElementDefinition before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirElementDefinition);
    
    // Get the iIndexth FhirElementDefinition. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinition);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirElementDefinition;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirElementDefinitions[index : Integer] : TFhirElementDefinition read GetItemN write SetItemN; default;
  End;

  // A set of rules that describe when the event should occur.
  TFhirTimingRepeat = class (TFhirElement)
  protected
    FBounds : TFhirType;
    FCount : TFhirInteger;
    FDuration : TFhirDecimal;
    FDurationMax : TFhirDecimal;
    FDurationUnits : TFhirEnum;
    FFrequency : TFhirInteger;
    FFrequencyMax : TFhirInteger;
    FPeriod : TFhirDecimal;
    FPeriodMax : TFhirDecimal;
    FPeriodUnits : TFhirEnum;
    FWhen : TFhirEnum;
    Procedure SetBounds(value : TFhirType);
    Procedure SetCount(value : TFhirInteger);
    Function GetCountST : String;
    Procedure SetCountST(value : String);
    Procedure SetDuration(value : TFhirDecimal);
    Function GetDurationST : String;
    Procedure SetDurationST(value : String);
    Procedure SetDurationMax(value : TFhirDecimal);
    Function GetDurationMaxST : String;
    Procedure SetDurationMaxST(value : String);
    Procedure SetDurationUnits(value : TFhirEnum);
    Function GetDurationUnitsST : TFhirUnitsOfTimeEnum;
    Procedure SetDurationUnitsST(value : TFhirUnitsOfTimeEnum);
    Procedure SetFrequency(value : TFhirInteger);
    Function GetFrequencyST : String;
    Procedure SetFrequencyST(value : String);
    Procedure SetFrequencyMax(value : TFhirInteger);
    Function GetFrequencyMaxST : String;
    Procedure SetFrequencyMaxST(value : String);
    Procedure SetPeriod(value : TFhirDecimal);
    Function GetPeriodST : String;
    Procedure SetPeriodST(value : String);
    Procedure SetPeriodMax(value : TFhirDecimal);
    Function GetPeriodMaxST : String;
    Procedure SetPeriodMaxST(value : String);
    Procedure SetPeriodUnits(value : TFhirEnum);
    Function GetPeriodUnitsST : TFhirUnitsOfTimeEnum;
    Procedure SetPeriodUnitsST(value : TFhirUnitsOfTimeEnum);
    Procedure SetWhen(value : TFhirEnum);
    Function GetWhenST : TFhirEventTimingEnum;
    Procedure SetWhenST(value : TFhirEventTimingEnum);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirTimingRepeat; overload;
    function Clone : TFhirTimingRepeat; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Typed access to Either a duration for the length of the timing schedule, a range of possible length, or outer bounds for start and/or end limits of the timing schedule. (defined for API consistency)
    property bounds : TFhirType read FBounds write SetBounds;
    // Either a duration for the length of the timing schedule, a range of possible length, or outer bounds for start and/or end limits of the timing schedule.
    property boundsElement : TFhirType read FBounds write SetBounds;

    // Typed access to A total count of the desired number of repetitions.
    property count : String read GetCountST write SetCountST;
    // A total count of the desired number of repetitions.
    property countElement : TFhirInteger read FCount write SetCount;

    // Typed access to How long this thing happens for when it happens.
    property duration : String read GetDurationST write SetDurationST;
    // How long this thing happens for when it happens.
    property durationElement : TFhirDecimal read FDuration write SetDuration;

    // Typed access to The upper limit of how long this thing happens for when it happens.
    property durationMax : String read GetDurationMaxST write SetDurationMaxST;
    // The upper limit of how long this thing happens for when it happens.
    property durationMaxElement : TFhirDecimal read FDurationMax write SetDurationMax;

    // The units of time for the duration, in UCUM units.
    property durationUnits : TFhirUnitsOfTimeEnum read GetDurationUnitsST write SetDurationUnitsST;
    property durationUnitsElement : TFhirEnum read FDurationUnits write SetDurationUnits;

    // Typed access to The number of times to repeat the action within the specified period / period range (i.e. both period and periodMax provided).
    property frequency : String read GetFrequencyST write SetFrequencyST;
    // The number of times to repeat the action within the specified period / period range (i.e. both period and periodMax provided).
    property frequencyElement : TFhirInteger read FFrequency write SetFrequency;

    // Typed access to If present, indicates that the frequency is a range - so repeat between [frequency] and [frequencyMax] times within the period or period range.
    property frequencyMax : String read GetFrequencyMaxST write SetFrequencyMaxST;
    // If present, indicates that the frequency is a range - so repeat between [frequency] and [frequencyMax] times within the period or period range.
    property frequencyMaxElement : TFhirInteger read FFrequencyMax write SetFrequencyMax;

    // Typed access to Indicates the duration of time over which repetitions are to occur; e.g. to express "3 times per day", 3 would be the frequency and "1 day" would be the period.
    property period : String read GetPeriodST write SetPeriodST;
    // Indicates the duration of time over which repetitions are to occur; e.g. to express "3 times per day", 3 would be the frequency and "1 day" would be the period.
    property periodElement : TFhirDecimal read FPeriod write SetPeriod;

    // Typed access to If present, indicates that the period is a range from [period] to [periodMax], allowing expressing concepts such as "do this once every 3-5 days.
    property periodMax : String read GetPeriodMaxST write SetPeriodMaxST;
    // If present, indicates that the period is a range from [period] to [periodMax], allowing expressing concepts such as "do this once every 3-5 days.
    property periodMaxElement : TFhirDecimal read FPeriodMax write SetPeriodMax;

    // The units of time for the period in UCUM units.
    property periodUnits : TFhirUnitsOfTimeEnum read GetPeriodUnitsST write SetPeriodUnitsST;
    property periodUnitsElement : TFhirEnum read FPeriodUnits write SetPeriodUnits;

    // A real world event that the occurrence of the event should be tied to.
    property when : TFhirEventTimingEnum read GetWhenST write SetWhenST;
    property whenElement : TFhirEnum read FWhen write SetWhen;

  end;

  TFhirTimingRepeatListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirTimingRepeatList;
    function GetCurrent : TFhirTimingRepeat;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirTimingRepeatList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirTimingRepeat read GetCurrent;
  end;

  TFhirTimingRepeatList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirTimingRepeat;
    procedure SetItemN(index : Integer; value : TFhirTimingRepeat);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirTimingRepeatList; Overload;
    function Clone : TFhirTimingRepeatList; Overload;
    function GetEnumerator : TFhirTimingRepeatListEnumerator;
    

    //  Add a FhirTimingRepeat to the end of the list.
    function Append : TFhirTimingRepeat;

    
    // Add an already existing FhirTimingRepeat to the end of the list.
    procedure AddItem(value : TFhirTimingRepeat); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirTimingRepeat) : Integer;
    

    // Insert FhirTimingRepeat before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirTimingRepeat;
    

    // Insert an existing FhirTimingRepeat before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirTimingRepeat);
    
    // Get the iIndexth FhirTimingRepeat. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirTimingRepeat);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirTimingRepeat;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirTimingRepeats[index : Integer] : TFhirTimingRepeat read GetItemN write SetItemN; default;
  End;

  // Specifies an event that may occur multiple times. Timing schedules are used to record when things are expected or requested to occur. The most common usage is in dosage instructions for medications. They are also used when planning care of various kinds.
  TFhirTiming = class (TFhirType)
  protected
    FeventList : TFhirDateTimeList;
    FRepeat_ : TFhirTimingRepeat;
    FCode : TFhirCodeableConcept;
    function GetEventList : TFhirDateTimeList;
    function GetHasEventList : Boolean;
    Procedure SetRepeat_(value : TFhirTimingRepeat);
    Procedure SetCode(value : TFhirCodeableConcept);
  
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirTiming; overload;
    function Clone : TFhirTiming; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}Published{$ENDIF}
    // Identifies specific times when the event occurs.
    property eventList : TFhirDateTimeList read GetEventList;
    property hasEventList : boolean read GetHasEventList;

    // Typed access to A set of rules that describe when the event should occur. (defined for API consistency)
    property repeat_ : TFhirTimingRepeat read FRepeat_ write SetRepeat_;
    // A set of rules that describe when the event should occur.
    property repeat_Element : TFhirTimingRepeat read FRepeat_ write SetRepeat_;

    // Typed access to A code for the timing pattern. Some codes such as BID are ubiquitous, but many institutions define their own additional codes. (defined for API consistency)
    property code : TFhirCodeableConcept read FCode write SetCode;
    // A code for the timing pattern. Some codes such as BID are ubiquitous, but many institutions define their own additional codes.
    property codeElement : TFhirCodeableConcept read FCode write SetCode;

  end;

  TFhirTimingListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirTimingList;
    function GetCurrent : TFhirTiming;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirTimingList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirTiming read GetCurrent;
  end;

  TFhirTimingList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirTiming;
    procedure SetItemN(index : Integer; value : TFhirTiming);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirTimingList; Overload;
    function Clone : TFhirTimingList; Overload;
    function GetEnumerator : TFhirTimingListEnumerator;
    

    //  Add a FhirTiming to the end of the list.
    function Append : TFhirTiming;

    
    // Add an already existing FhirTiming to the end of the list.
    procedure AddItem(value : TFhirTiming); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirTiming) : Integer;
    

    // Insert FhirTiming before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirTiming;
    

    // Insert an existing FhirTiming before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirTiming);
    
    // Get the iIndexth FhirTiming. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirTiming);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirTiming;
    
    // The number of items in the collection
    function Count : Integer; Overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    Property FhirTimings[index : Integer] : TFhirTiming read GetItemN write SetItemN; default;
  End;

Const
  CODES_TFhirNarrativeStatusEnum : Array[TFhirNarrativeStatusEnum] of String = ('', 'generated', 'extensions', 'additional', 'empty');
  SYSTEMS_TFhirNarrativeStatusEnum : Array[TFhirNarrativeStatusEnum] of String = ('', 'http://hl7.org/fhir/narrative-status', 'http://hl7.org/fhir/narrative-status', 'http://hl7.org/fhir/narrative-status', 'http://hl7.org/fhir/narrative-status');
  CODES_TFhirIdentifierUseEnum : Array[TFhirIdentifierUseEnum] of String = ('', 'usual', 'official', 'temp', 'secondary');
  SYSTEMS_TFhirIdentifierUseEnum : Array[TFhirIdentifierUseEnum] of String = ('', 'http://hl7.org/fhir/identifier-use', 'http://hl7.org/fhir/identifier-use', 'http://hl7.org/fhir/identifier-use', 'http://hl7.org/fhir/identifier-use');
  CODES_TFhirQuantityComparatorEnum : Array[TFhirQuantityComparatorEnum] of String = ('', '<', '<=', '>=', '>');
  SYSTEMS_TFhirQuantityComparatorEnum : Array[TFhirQuantityComparatorEnum] of String = ('', 'http://hl7.org/fhir/quantity-comparator', 'http://hl7.org/fhir/quantity-comparator', 'http://hl7.org/fhir/quantity-comparator', 'http://hl7.org/fhir/quantity-comparator');
  CODES_TFhirNameUseEnum : Array[TFhirNameUseEnum] of String = ('', 'usual', 'official', 'temp', 'nickname', 'anonymous', 'old', 'maiden');
  SYSTEMS_TFhirNameUseEnum : Array[TFhirNameUseEnum] of String = ('', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use', 'http://hl7.org/fhir/name-use');
  CODES_TFhirContactPointSystemEnum : Array[TFhirContactPointSystemEnum] of String = ('', 'phone', 'fax', 'email', 'pager', 'other');
  SYSTEMS_TFhirContactPointSystemEnum : Array[TFhirContactPointSystemEnum] of String = ('', 'http://hl7.org/fhir/contact-point-system', 'http://hl7.org/fhir/contact-point-system', 'http://hl7.org/fhir/contact-point-system', 'http://hl7.org/fhir/contact-point-system', 'http://hl7.org/fhir/contact-point-system');
  CODES_TFhirContactPointUseEnum : Array[TFhirContactPointUseEnum] of String = ('', 'home', 'work', 'temp', 'old', 'mobile');
  SYSTEMS_TFhirContactPointUseEnum : Array[TFhirContactPointUseEnum] of String = ('', 'http://hl7.org/fhir/contact-point-use', 'http://hl7.org/fhir/contact-point-use', 'http://hl7.org/fhir/contact-point-use', 'http://hl7.org/fhir/contact-point-use', 'http://hl7.org/fhir/contact-point-use');
  CODES_TFhirAddressUseEnum : Array[TFhirAddressUseEnum] of String = ('', 'home', 'work', 'temp', 'old');
  SYSTEMS_TFhirAddressUseEnum : Array[TFhirAddressUseEnum] of String = ('', 'http://hl7.org/fhir/address-use', 'http://hl7.org/fhir/address-use', 'http://hl7.org/fhir/address-use', 'http://hl7.org/fhir/address-use');
  CODES_TFhirAddressTypeEnum : Array[TFhirAddressTypeEnum] of String = ('', 'postal', 'physical', 'both');
  SYSTEMS_TFhirAddressTypeEnum : Array[TFhirAddressTypeEnum] of String = ('', 'http://hl7.org/fhir/address-type', 'http://hl7.org/fhir/address-type', 'http://hl7.org/fhir/address-type');
  CODES_TFhirPropertyRepresentationEnum : Array[TFhirPropertyRepresentationEnum] of String = ('', 'xmlAttr');
  SYSTEMS_TFhirPropertyRepresentationEnum : Array[TFhirPropertyRepresentationEnum] of String = ('', 'http://hl7.org/fhir/property-representation');
  CODES_TFhirResourceSlicingRulesEnum : Array[TFhirResourceSlicingRulesEnum] of String = ('', 'closed', 'open', 'openAtEnd');
  SYSTEMS_TFhirResourceSlicingRulesEnum : Array[TFhirResourceSlicingRulesEnum] of String = ('', 'http://hl7.org/fhir/resource-slicing-rules', 'http://hl7.org/fhir/resource-slicing-rules', 'http://hl7.org/fhir/resource-slicing-rules');
  CODES_TFhirResourceAggregationModeEnum : Array[TFhirResourceAggregationModeEnum] of String = ('', 'contained', 'referenced', 'bundled');
  SYSTEMS_TFhirResourceAggregationModeEnum : Array[TFhirResourceAggregationModeEnum] of String = ('', 'http://hl7.org/fhir/resource-aggregation-mode', 'http://hl7.org/fhir/resource-aggregation-mode', 'http://hl7.org/fhir/resource-aggregation-mode');
  CODES_TFhirConstraintSeverityEnum : Array[TFhirConstraintSeverityEnum] of String = ('', 'error', 'warning');
  SYSTEMS_TFhirConstraintSeverityEnum : Array[TFhirConstraintSeverityEnum] of String = ('', 'http://hl7.org/fhir/constraint-severity', 'http://hl7.org/fhir/constraint-severity');
  CODES_TFhirBindingStrengthEnum : Array[TFhirBindingStrengthEnum] of String = ('', 'required', 'extensible', 'preferred', 'example');
  SYSTEMS_TFhirBindingStrengthEnum : Array[TFhirBindingStrengthEnum] of String = ('', 'http://hl7.org/fhir/binding-strength', 'http://hl7.org/fhir/binding-strength', 'http://hl7.org/fhir/binding-strength', 'http://hl7.org/fhir/binding-strength');
  CODES_TFhirUnitsOfTimeEnum : Array[TFhirUnitsOfTimeEnum] of String = ('', 's', 'min', 'h', 'd', 'wk', 'mo', 'a');
  SYSTEMS_TFhirUnitsOfTimeEnum : Array[TFhirUnitsOfTimeEnum] of String = ('', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org', 'http://unitsofmeasure.org');
  CODES_TFhirEventTimingEnum : Array[TFhirEventTimingEnum] of String = ('', 'HS', 'WAKE', 'C', 'CM', 'CD', 'CV', 'AC', 'ACM', 'ACD', 'ACV', 'PC', 'PCM', 'PCD', 'PCV');
  SYSTEMS_TFhirEventTimingEnum : Array[TFhirEventTimingEnum] of String = ('', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent', 'http://hl7.org/fhir/v3/TimingEvent');
  CODES_TFhirAllergyIntoleranceStatusEnum : Array[TFhirAllergyIntoleranceStatusEnum] of String = ('', 'active', 'unconfirmed', 'confirmed', 'inactive', 'resolved', 'refuted', 'entered-in-error');
  SYSTEMS_TFhirAllergyIntoleranceStatusEnum : Array[TFhirAllergyIntoleranceStatusEnum] of String = ('', 'http://hl7.org/fhir/allergy-intolerance-status', 'http://hl7.org/fhir/allergy-intolerance-status', 'http://hl7.org/fhir/allergy-intolerance-status', 'http://hl7.org/fhir/allergy-intolerance-status', 'http://hl7.org/fhir/allergy-intolerance-status', 'http://hl7.org/fhir/allergy-intolerance-status', 'http://hl7.org/fhir/allergy-intolerance-status');
  CODES_TFhirAllergyIntoleranceCriticalityEnum : Array[TFhirAllergyIntoleranceCriticalityEnum] of String = ('', 'CRITL', 'CRITH', 'CRITU');
  SYSTEMS_TFhirAllergyIntoleranceCriticalityEnum : Array[TFhirAllergyIntoleranceCriticalityEnum] of String = ('', 'http://hl7.org/fhir/allergy-intolerance-criticality', 'http://hl7.org/fhir/allergy-intolerance-criticality', 'http://hl7.org/fhir/allergy-intolerance-criticality');
  CODES_TFhirAllergyIntoleranceTypeEnum : Array[TFhirAllergyIntoleranceTypeEnum] of String = ('', 'allergy', 'intolerance');
  SYSTEMS_TFhirAllergyIntoleranceTypeEnum : Array[TFhirAllergyIntoleranceTypeEnum] of String = ('', 'http://hl7.org/fhir/allergy-intolerance-type', 'http://hl7.org/fhir/allergy-intolerance-type');
  CODES_TFhirAllergyIntoleranceCategoryEnum : Array[TFhirAllergyIntoleranceCategoryEnum] of String = ('', 'food', 'medication', 'environment', 'other');
  SYSTEMS_TFhirAllergyIntoleranceCategoryEnum : Array[TFhirAllergyIntoleranceCategoryEnum] of String = ('', 'http://hl7.org/fhir/allergy-intolerance-category', 'http://hl7.org/fhir/allergy-intolerance-category', 'http://hl7.org/fhir/allergy-intolerance-category', 'http://hl7.org/fhir/allergy-intolerance-category');
  CODES_TFhirReactionEventCertaintyEnum : Array[TFhirReactionEventCertaintyEnum] of String = ('', 'unlikely', 'likely', 'confirmed');
  SYSTEMS_TFhirReactionEventCertaintyEnum : Array[TFhirReactionEventCertaintyEnum] of String = ('', 'http://hl7.org/fhir/reaction-event-certainty', 'http://hl7.org/fhir/reaction-event-certainty', 'http://hl7.org/fhir/reaction-event-certainty');
  CODES_TFhirReactionEventSeverityEnum : Array[TFhirReactionEventSeverityEnum] of String = ('', 'mild', 'moderate', 'severe');
  SYSTEMS_TFhirReactionEventSeverityEnum : Array[TFhirReactionEventSeverityEnum] of String = ('', 'http://hl7.org/fhir/reaction-event-severity', 'http://hl7.org/fhir/reaction-event-severity', 'http://hl7.org/fhir/reaction-event-severity');
  CODES_TFhirAppointmentstatusEnum : Array[TFhirAppointmentstatusEnum] of String = ('', 'proposed', 'pending', 'booked', 'arrived', 'fulfilled', 'cancelled', 'noshow');
  SYSTEMS_TFhirAppointmentstatusEnum : Array[TFhirAppointmentstatusEnum] of String = ('', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus', 'http://hl7.org/fhir/appointmentstatus');
  CODES_TFhirParticipantrequiredEnum : Array[TFhirParticipantrequiredEnum] of String = ('', 'required', 'optional', 'information-only');
  SYSTEMS_TFhirParticipantrequiredEnum : Array[TFhirParticipantrequiredEnum] of String = ('', 'http://hl7.org/fhir/participantrequired', 'http://hl7.org/fhir/participantrequired', 'http://hl7.org/fhir/participantrequired');
  CODES_TFhirParticipationstatusEnum : Array[TFhirParticipationstatusEnum] of String = ('', 'accepted', 'declined', 'tentative', 'needs-action');
  SYSTEMS_TFhirParticipationstatusEnum : Array[TFhirParticipationstatusEnum] of String = ('', 'http://hl7.org/fhir/participationstatus', 'http://hl7.org/fhir/participationstatus', 'http://hl7.org/fhir/participationstatus', 'http://hl7.org/fhir/participationstatus');
  CODES_TFhirParticipantstatusEnum : Array[TFhirParticipantstatusEnum] of String = ('', 'accepted', 'declined', 'tentative', 'in-process', 'completed', 'needs-action');
  SYSTEMS_TFhirParticipantstatusEnum : Array[TFhirParticipantstatusEnum] of String = ('', 'http://hl7.org/fhir/participantstatus', 'http://hl7.org/fhir/participantstatus', 'http://hl7.org/fhir/participantstatus', 'http://hl7.org/fhir/participantstatus', 'http://hl7.org/fhir/participantstatus', 'http://hl7.org/fhir/participantstatus');
  CODES_TFhirAuditEventActionEnum : Array[TFhirAuditEventActionEnum] of String = ('', 'C', 'R', 'U', 'D', 'E');
  SYSTEMS_TFhirAuditEventActionEnum : Array[TFhirAuditEventActionEnum] of String = ('', 'http://hl7.org/fhir/audit-event-action', 'http://hl7.org/fhir/audit-event-action', 'http://hl7.org/fhir/audit-event-action', 'http://hl7.org/fhir/audit-event-action', 'http://hl7.org/fhir/audit-event-action');
  CODES_TFhirAuditEventOutcomeEnum : Array[TFhirAuditEventOutcomeEnum] of String = ('', '0', '4', '8', '12');
  SYSTEMS_TFhirAuditEventOutcomeEnum : Array[TFhirAuditEventOutcomeEnum] of String = ('', 'http://hl7.org/fhir/audit-event-outcome', 'http://hl7.org/fhir/audit-event-outcome', 'http://hl7.org/fhir/audit-event-outcome', 'http://hl7.org/fhir/audit-event-outcome');
  CODES_TFhirNetworkTypeEnum : Array[TFhirNetworkTypeEnum] of String = ('', '1', '2', '3', '4', '5');
  SYSTEMS_TFhirNetworkTypeEnum : Array[TFhirNetworkTypeEnum] of String = ('', 'http://hl7.org/fhir/network-type', 'http://hl7.org/fhir/network-type', 'http://hl7.org/fhir/network-type', 'http://hl7.org/fhir/network-type', 'http://hl7.org/fhir/network-type');
  CODES_TFhirBundleTypeEnum : Array[TFhirBundleTypeEnum] of String = ('', 'document', 'message', 'transaction', 'transaction-response', 'batch', 'batch-response', 'history', 'searchset', 'collection');
  SYSTEMS_TFhirBundleTypeEnum : Array[TFhirBundleTypeEnum] of String = ('', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type', 'http://hl7.org/fhir/bundle-type');
  CODES_TFhirSearchEntryModeEnum : Array[TFhirSearchEntryModeEnum] of String = ('', 'match', 'include', 'outcome');
  SYSTEMS_TFhirSearchEntryModeEnum : Array[TFhirSearchEntryModeEnum] of String = ('', 'http://hl7.org/fhir/search-entry-mode', 'http://hl7.org/fhir/search-entry-mode', 'http://hl7.org/fhir/search-entry-mode');
  CODES_TFhirHttpVerbEnum : Array[TFhirHttpVerbEnum] of String = ('', 'GET', 'POST', 'PUT', 'DELETE');
  SYSTEMS_TFhirHttpVerbEnum : Array[TFhirHttpVerbEnum] of String = ('', 'http://hl7.org/fhir/http-verb', 'http://hl7.org/fhir/http-verb', 'http://hl7.org/fhir/http-verb', 'http://hl7.org/fhir/http-verb');
  CODES_TFhirCarePlanStatusEnum : Array[TFhirCarePlanStatusEnum] of String = ('', 'proposed', 'draft', 'active', 'completed', 'cancelled');
  SYSTEMS_TFhirCarePlanStatusEnum : Array[TFhirCarePlanStatusEnum] of String = ('', 'http://hl7.org/fhir/care-plan-status', 'http://hl7.org/fhir/care-plan-status', 'http://hl7.org/fhir/care-plan-status', 'http://hl7.org/fhir/care-plan-status', 'http://hl7.org/fhir/care-plan-status');
  CODES_TFhirCarePlanRelationshipEnum : Array[TFhirCarePlanRelationshipEnum] of String = ('', 'includes', 'replaces', 'fulfills');
  SYSTEMS_TFhirCarePlanRelationshipEnum : Array[TFhirCarePlanRelationshipEnum] of String = ('', 'http://hl7.org/fhir/care-plan-relationship', 'http://hl7.org/fhir/care-plan-relationship', 'http://hl7.org/fhir/care-plan-relationship');
  CODES_TFhirCarePlanActivityStatusEnum : Array[TFhirCarePlanActivityStatusEnum] of String = ('', 'not-started', 'scheduled', 'in-progress', 'on-hold', 'completed', 'cancelled');
  SYSTEMS_TFhirCarePlanActivityStatusEnum : Array[TFhirCarePlanActivityStatusEnum] of String = ('', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status', 'http://hl7.org/fhir/care-plan-activity-status');
  CODES_TFhirClaimTypeLinkEnum : Array[TFhirClaimTypeLinkEnum] of String = ('', 'institutional', 'oral', 'pharmacy', 'professional', 'vision');
  SYSTEMS_TFhirClaimTypeLinkEnum : Array[TFhirClaimTypeLinkEnum] of String = ('', 'http://hl7.org/fhir/claim-type-link', 'http://hl7.org/fhir/claim-type-link', 'http://hl7.org/fhir/claim-type-link', 'http://hl7.org/fhir/claim-type-link', 'http://hl7.org/fhir/claim-type-link');
  CODES_TFhirClaimUseLinkEnum : Array[TFhirClaimUseLinkEnum] of String = ('', 'complete', 'proposed', 'exploratory', 'other');
  SYSTEMS_TFhirClaimUseLinkEnum : Array[TFhirClaimUseLinkEnum] of String = ('', 'http://hl7.org/fhir/claim-use-link', 'http://hl7.org/fhir/claim-use-link', 'http://hl7.org/fhir/claim-use-link', 'http://hl7.org/fhir/claim-use-link');
  CODES_TFhirRemittanceOutcomeEnum : Array[TFhirRemittanceOutcomeEnum] of String = ('', 'complete', 'error');
  SYSTEMS_TFhirRemittanceOutcomeEnum : Array[TFhirRemittanceOutcomeEnum] of String = ('', 'http://hl7.org/fhir/remittance-outcome', 'http://hl7.org/fhir/remittance-outcome');
  CODES_TFhirClinicalImpressionStatusEnum : Array[TFhirClinicalImpressionStatusEnum] of String = ('', 'in-progress', 'completed', 'entered-in-error');
  SYSTEMS_TFhirClinicalImpressionStatusEnum : Array[TFhirClinicalImpressionStatusEnum] of String = ('', 'http://hl7.org/fhir/clinical-impression-status', 'http://hl7.org/fhir/clinical-impression-status', 'http://hl7.org/fhir/clinical-impression-status');
  CODES_TFhirCommunicationStatusEnum : Array[TFhirCommunicationStatusEnum] of String = ('', 'in-progress', 'completed', 'suspended', 'rejected', 'failed');
  SYSTEMS_TFhirCommunicationStatusEnum : Array[TFhirCommunicationStatusEnum] of String = ('', 'http://hl7.org/fhir/communication-status', 'http://hl7.org/fhir/communication-status', 'http://hl7.org/fhir/communication-status', 'http://hl7.org/fhir/communication-status', 'http://hl7.org/fhir/communication-status');
  CODES_TFhirCommunicationRequestStatusEnum : Array[TFhirCommunicationRequestStatusEnum] of String = ('', 'proposed', 'planned', 'requested', 'received', 'accepted', 'in-progress', 'completed', 'suspended', 'rejected', 'failed');
  SYSTEMS_TFhirCommunicationRequestStatusEnum : Array[TFhirCommunicationRequestStatusEnum] of String = ('', 'http://hl7.org/fhir/communication-request-status', 'http://hl7.org/fhir/communication-request-status', 'http://hl7.org/fhir/communication-request-status', 'http://hl7.org/fhir/communication-request-status', 'http://hl7.org/fhir/communication-request-status', 'http://hl7.org/fhir/communication-request-status', 'http://hl7.org/fhir/communication-request-status', 'http://hl7.org/fhir/communication-request-status', 'http://hl7.org/fhir/communication-request-status', 'http://hl7.org/fhir/communication-request-status');
  CODES_TFhirCompositionStatusEnum : Array[TFhirCompositionStatusEnum] of String = ('', 'preliminary', 'final', 'amended', 'entered-in-error');
  SYSTEMS_TFhirCompositionStatusEnum : Array[TFhirCompositionStatusEnum] of String = ('', 'http://hl7.org/fhir/composition-status', 'http://hl7.org/fhir/composition-status', 'http://hl7.org/fhir/composition-status', 'http://hl7.org/fhir/composition-status');
  CODES_TFhirV3ConfidentialityEnum : Array[TFhirV3ConfidentialityEnum] of String = ('', '_Confidentiality', 'L', 'M', 'N', 'R', 'U', 'V');
  SYSTEMS_TFhirV3ConfidentialityEnum : Array[TFhirV3ConfidentialityEnum] of String = ('', 'http://hl7.org/fhir/v3/Confidentiality', 'http://hl7.org/fhir/v3/Confidentiality', 'http://hl7.org/fhir/v3/Confidentiality', 'http://hl7.org/fhir/v3/Confidentiality', 'http://hl7.org/fhir/v3/Confidentiality', 'http://hl7.org/fhir/v3/Confidentiality', 'http://hl7.org/fhir/v3/Confidentiality');
  CODES_TFhirCompositionAttestationModeEnum : Array[TFhirCompositionAttestationModeEnum] of String = ('', 'personal', 'professional', 'legal', 'official');
  SYSTEMS_TFhirCompositionAttestationModeEnum : Array[TFhirCompositionAttestationModeEnum] of String = ('', 'http://hl7.org/fhir/composition-attestation-mode', 'http://hl7.org/fhir/composition-attestation-mode', 'http://hl7.org/fhir/composition-attestation-mode', 'http://hl7.org/fhir/composition-attestation-mode');
  CODES_TFhirListModeEnum : Array[TFhirListModeEnum] of String = ('', 'working', 'snapshot', 'changes');
  SYSTEMS_TFhirListModeEnum : Array[TFhirListModeEnum] of String = ('', 'http://hl7.org/fhir/list-mode', 'http://hl7.org/fhir/list-mode', 'http://hl7.org/fhir/list-mode');
  CODES_TFhirConformanceResourceStatusEnum : Array[TFhirConformanceResourceStatusEnum] of String = ('', 'draft', 'active', 'retired');
  SYSTEMS_TFhirConformanceResourceStatusEnum : Array[TFhirConformanceResourceStatusEnum] of String = ('', 'http://hl7.org/fhir/conformance-resource-status', 'http://hl7.org/fhir/conformance-resource-status', 'http://hl7.org/fhir/conformance-resource-status');
  CODES_TFhirConceptMapEquivalenceEnum : Array[TFhirConceptMapEquivalenceEnum] of String = ('', 'equivalent', 'equal', 'wider', 'subsumes', 'narrower', 'specializes', 'inexact', 'unmatched', 'disjoint');
  SYSTEMS_TFhirConceptMapEquivalenceEnum : Array[TFhirConceptMapEquivalenceEnum] of String = ('', 'http://hl7.org/fhir/concept-map-equivalence', 'http://hl7.org/fhir/concept-map-equivalence', 'http://hl7.org/fhir/concept-map-equivalence', 'http://hl7.org/fhir/concept-map-equivalence', 'http://hl7.org/fhir/concept-map-equivalence', 'http://hl7.org/fhir/concept-map-equivalence', 'http://hl7.org/fhir/concept-map-equivalence', 'http://hl7.org/fhir/concept-map-equivalence', 'http://hl7.org/fhir/concept-map-equivalence');
  CODES_TFhirConditionVerStatusEnum : Array[TFhirConditionVerStatusEnum] of String = ('', 'provisional', 'differential', 'confirmed', 'refuted', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirConditionVerStatusEnum : Array[TFhirConditionVerStatusEnum] of String = ('', 'http://hl7.org/fhir/condition-ver-status', 'http://hl7.org/fhir/condition-ver-status', 'http://hl7.org/fhir/condition-ver-status', 'http://hl7.org/fhir/condition-ver-status', 'http://hl7.org/fhir/condition-ver-status', 'http://hl7.org/fhir/condition-ver-status');
  CODES_TFhirConformanceStatementKindEnum : Array[TFhirConformanceStatementKindEnum] of String = ('', 'instance', 'capability', 'requirements');
  SYSTEMS_TFhirConformanceStatementKindEnum : Array[TFhirConformanceStatementKindEnum] of String = ('', 'http://hl7.org/fhir/conformance-statement-kind', 'http://hl7.org/fhir/conformance-statement-kind', 'http://hl7.org/fhir/conformance-statement-kind');
  CODES_TFhirUnknownContentCodeEnum : Array[TFhirUnknownContentCodeEnum] of String = ('', 'no', 'extensions', 'elements', 'both');
  SYSTEMS_TFhirUnknownContentCodeEnum : Array[TFhirUnknownContentCodeEnum] of String = ('', 'http://hl7.org/fhir/unknown-content-code', 'http://hl7.org/fhir/unknown-content-code', 'http://hl7.org/fhir/unknown-content-code', 'http://hl7.org/fhir/unknown-content-code');
  CODES_TFhirRestfulConformanceModeEnum : Array[TFhirRestfulConformanceModeEnum] of String = ('', 'client', 'server');
  SYSTEMS_TFhirRestfulConformanceModeEnum : Array[TFhirRestfulConformanceModeEnum] of String = ('', 'http://hl7.org/fhir/restful-conformance-mode', 'http://hl7.org/fhir/restful-conformance-mode');
  CODES_TFhirResourceTypesEnum : Array[TFhirResourceTypesEnum] of String = ('', 'Account', 'AllergyIntolerance', 'Appointment', 'AppointmentResponse', 'AuditEvent', 'Basic', 'Binary', 'BodySite', 'Bundle', 'CarePlan', 'Claim', 'ClaimResponse', 'ClinicalImpression', 'Communication', 'CommunicationRequest', 'Composition', 'ConceptMap', 'Condition', 'Conformance', 'Contract', 'Coverage', 'DataElement', 'DetectedIssue', 'Device', 'DeviceComponent', 'DeviceMetric', 'DeviceUseRequest', 'DeviceUseStatement', 'DiagnosticOrder', 'DiagnosticReport', 'DocumentManifest', 'DocumentReference', 'DomainResource', 'EligibilityRequest', 'EligibilityResponse', 'Encounter', 'EnrollmentRequest', 'EnrollmentResponse', 'EpisodeOfCare', 'ExplanationOfBenefit', 'FamilyMemberHistory', 'Flag', 'Goal', 'Group', 'HealthcareService', 'ImagingObjectSelection', 'ImagingStudy', 'Immunization', 'ImmunizationRecommendation', 'ImplementationGuide', 'List', 'Location', 'Media', 'Medication', 'MedicationAdministration', 
    'MedicationDispense', 'MedicationOrder', 'MedicationStatement', 'MessageHeader', 'NamingSystem', 'NutritionOrder', 'Observation', 'OperationDefinition', 'OperationOutcome', 'Order', 'OrderResponse', 'Organization', 'Parameters', 'Patient', 'PaymentNotice', 'PaymentReconciliation', 'Person', 'Practitioner', 'Procedure', 'ProcedureRequest', 'ProcessRequest', 'ProcessResponse', 'Provenance', 'Questionnaire', 'QuestionnaireResponse', 'ReferralRequest', 'RelatedPerson', 'Resource', 'RiskAssessment', 'Schedule', 'SearchParameter', 'Slot', 'Specimen', 'StructureDefinition', 'Subscription', 'Substance', 'SupplyDelivery', 'SupplyRequest', 'TestScript', 'ValueSet', 'VisionPrescription');
  SYSTEMS_TFhirResourceTypesEnum : Array[TFhirResourceTypesEnum] of String = ('', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 
    'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 
    'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 
    'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types');
  CODES_TFhirTypeRestfulInteractionEnum : Array[TFhirTypeRestfulInteractionEnum] of String = ('', 'read', 'vread', 'update', 'delete', 'history-instance', 'validate', 'history-type', 'create', 'search-type');
  SYSTEMS_TFhirTypeRestfulInteractionEnum : Array[TFhirTypeRestfulInteractionEnum] of String = ('', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction');
  CODES_TFhirVersioningPolicyEnum : Array[TFhirVersioningPolicyEnum] of String = ('', 'no-version', 'versioned', 'versioned-update');
  SYSTEMS_TFhirVersioningPolicyEnum : Array[TFhirVersioningPolicyEnum] of String = ('', 'http://hl7.org/fhir/versioning-policy', 'http://hl7.org/fhir/versioning-policy', 'http://hl7.org/fhir/versioning-policy');
  CODES_TFhirConditionalDeleteStatusEnum : Array[TFhirConditionalDeleteStatusEnum] of String = ('', 'not-supported', 'single', 'multiple');
  SYSTEMS_TFhirConditionalDeleteStatusEnum : Array[TFhirConditionalDeleteStatusEnum] of String = ('', 'http://hl7.org/fhir/conditional-delete-status', 'http://hl7.org/fhir/conditional-delete-status', 'http://hl7.org/fhir/conditional-delete-status');
  CODES_TFhirSearchParamTypeEnum : Array[TFhirSearchParamTypeEnum] of String = ('', 'number', 'date', 'string', 'token', 'reference', 'composite', 'quantity', 'uri');
  SYSTEMS_TFhirSearchParamTypeEnum : Array[TFhirSearchParamTypeEnum] of String = ('', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type');
  CODES_TFhirSearchModifierCodeEnum : Array[TFhirSearchModifierCodeEnum] of String = ('', 'missing', 'exact', 'contains', 'not', 'text', 'in', 'not-in', 'below', 'above', 'type');
  SYSTEMS_TFhirSearchModifierCodeEnum : Array[TFhirSearchModifierCodeEnum] of String = ('', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code', 'http://hl7.org/fhir/search-modifier-code');
  CODES_TFhirSystemRestfulInteractionEnum : Array[TFhirSystemRestfulInteractionEnum] of String = ('', 'transaction', 'search-system', 'history-system');
  SYSTEMS_TFhirSystemRestfulInteractionEnum : Array[TFhirSystemRestfulInteractionEnum] of String = ('', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction', 'http://hl7.org/fhir/restful-interaction');
  CODES_TFhirTransactionModeEnum : Array[TFhirTransactionModeEnum] of String = ('', 'not-supported', 'batch', 'transaction', 'both');
  SYSTEMS_TFhirTransactionModeEnum : Array[TFhirTransactionModeEnum] of String = ('', 'http://hl7.org/fhir/transaction-mode', 'http://hl7.org/fhir/transaction-mode', 'http://hl7.org/fhir/transaction-mode', 'http://hl7.org/fhir/transaction-mode');
  CODES_TFhirMessageSignificanceCategoryEnum : Array[TFhirMessageSignificanceCategoryEnum] of String = ('', 'Consequence', 'Currency', 'Notification');
  SYSTEMS_TFhirMessageSignificanceCategoryEnum : Array[TFhirMessageSignificanceCategoryEnum] of String = ('', 'http://hl7.org/fhir/message-significance-category', 'http://hl7.org/fhir/message-significance-category', 'http://hl7.org/fhir/message-significance-category');
  CODES_TFhirMessageConformanceEventModeEnum : Array[TFhirMessageConformanceEventModeEnum] of String = ('', 'sender', 'receiver');
  SYSTEMS_TFhirMessageConformanceEventModeEnum : Array[TFhirMessageConformanceEventModeEnum] of String = ('', 'http://hl7.org/fhir/message-conformance-event-mode', 'http://hl7.org/fhir/message-conformance-event-mode');
  CODES_TFhirDocumentModeEnum : Array[TFhirDocumentModeEnum] of String = ('', 'producer', 'consumer');
  SYSTEMS_TFhirDocumentModeEnum : Array[TFhirDocumentModeEnum] of String = ('', 'http://hl7.org/fhir/document-mode', 'http://hl7.org/fhir/document-mode');
  CODES_TFhirDataelementStringencyEnum : Array[TFhirDataelementStringencyEnum] of String = ('', 'comparable', 'fully-specified', 'equivalent', 'convertable', 'scaleable', 'flexible');
  SYSTEMS_TFhirDataelementStringencyEnum : Array[TFhirDataelementStringencyEnum] of String = ('', 'http://hl7.org/fhir/dataelement-stringency', 'http://hl7.org/fhir/dataelement-stringency', 'http://hl7.org/fhir/dataelement-stringency', 'http://hl7.org/fhir/dataelement-stringency', 'http://hl7.org/fhir/dataelement-stringency', 'http://hl7.org/fhir/dataelement-stringency');
  CODES_TFhirDetectedissueSeverityEnum : Array[TFhirDetectedissueSeverityEnum] of String = ('', 'high', 'moderate', 'low');
  SYSTEMS_TFhirDetectedissueSeverityEnum : Array[TFhirDetectedissueSeverityEnum] of String = ('', 'http://hl7.org/fhir/detectedissue-severity', 'http://hl7.org/fhir/detectedissue-severity', 'http://hl7.org/fhir/detectedissue-severity');
  CODES_TFhirDevicestatusEnum : Array[TFhirDevicestatusEnum] of String = ('', 'available', 'not-available', 'entered-in-error');
  SYSTEMS_TFhirDevicestatusEnum : Array[TFhirDevicestatusEnum] of String = ('', 'http://hl7.org/fhir/devicestatus', 'http://hl7.org/fhir/devicestatus', 'http://hl7.org/fhir/devicestatus');
  CODES_TFhirMeasurementPrincipleEnum : Array[TFhirMeasurementPrincipleEnum] of String = ('', 'other', 'chemical', 'electrical', 'impedance', 'nuclear', 'optical', 'thermal', 'biological', 'mechanical', 'acoustical', 'manual');
  SYSTEMS_TFhirMeasurementPrincipleEnum : Array[TFhirMeasurementPrincipleEnum] of String = ('', 'http://hl7.org/fhir/measurement-principle', 'http://hl7.org/fhir/measurement-principle', 'http://hl7.org/fhir/measurement-principle', 'http://hl7.org/fhir/measurement-principle', 'http://hl7.org/fhir/measurement-principle', 'http://hl7.org/fhir/measurement-principle', 'http://hl7.org/fhir/measurement-principle', 'http://hl7.org/fhir/measurement-principle', 'http://hl7.org/fhir/measurement-principle', 'http://hl7.org/fhir/measurement-principle', 'http://hl7.org/fhir/measurement-principle');
  CODES_TFhirMetricOperationalStatusEnum : Array[TFhirMetricOperationalStatusEnum] of String = ('', 'on', 'off', 'standby');
  SYSTEMS_TFhirMetricOperationalStatusEnum : Array[TFhirMetricOperationalStatusEnum] of String = ('', 'http://hl7.org/fhir/metric-operational-status', 'http://hl7.org/fhir/metric-operational-status', 'http://hl7.org/fhir/metric-operational-status');
  CODES_TFhirMetricColorEnum : Array[TFhirMetricColorEnum] of String = ('', 'black', 'red', 'green', 'yellow', 'blue', 'magenta', 'cyan', 'white');
  SYSTEMS_TFhirMetricColorEnum : Array[TFhirMetricColorEnum] of String = ('', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color', 'http://hl7.org/fhir/metric-color');
  CODES_TFhirMetricCategoryEnum : Array[TFhirMetricCategoryEnum] of String = ('', 'measurement', 'setting', 'calculation', 'unspecified');
  SYSTEMS_TFhirMetricCategoryEnum : Array[TFhirMetricCategoryEnum] of String = ('', 'http://hl7.org/fhir/metric-category', 'http://hl7.org/fhir/metric-category', 'http://hl7.org/fhir/metric-category', 'http://hl7.org/fhir/metric-category');
  CODES_TFhirMetricCalibrationTypeEnum : Array[TFhirMetricCalibrationTypeEnum] of String = ('', 'unspecified', 'offset', 'gain', 'two-point');
  SYSTEMS_TFhirMetricCalibrationTypeEnum : Array[TFhirMetricCalibrationTypeEnum] of String = ('', 'http://hl7.org/fhir/metric-calibration-type', 'http://hl7.org/fhir/metric-calibration-type', 'http://hl7.org/fhir/metric-calibration-type', 'http://hl7.org/fhir/metric-calibration-type');
  CODES_TFhirMetricCalibrationStateEnum : Array[TFhirMetricCalibrationStateEnum] of String = ('', 'not-calibrated', 'calibration-required', 'calibrated', 'unspecified');
  SYSTEMS_TFhirMetricCalibrationStateEnum : Array[TFhirMetricCalibrationStateEnum] of String = ('', 'http://hl7.org/fhir/metric-calibration-state', 'http://hl7.org/fhir/metric-calibration-state', 'http://hl7.org/fhir/metric-calibration-state', 'http://hl7.org/fhir/metric-calibration-state');
  CODES_TFhirDeviceUseRequestStatusEnum : Array[TFhirDeviceUseRequestStatusEnum] of String = ('', 'proposed', 'planned', 'requested', 'received', 'accepted', 'in-progress', 'completed', 'suspended', 'rejected', 'aborted');
  SYSTEMS_TFhirDeviceUseRequestStatusEnum : Array[TFhirDeviceUseRequestStatusEnum] of String = ('', 'http://hl7.org/fhir/device-use-request-status', 'http://hl7.org/fhir/device-use-request-status', 'http://hl7.org/fhir/device-use-request-status', 'http://hl7.org/fhir/device-use-request-status', 'http://hl7.org/fhir/device-use-request-status', 'http://hl7.org/fhir/device-use-request-status', 'http://hl7.org/fhir/device-use-request-status', 'http://hl7.org/fhir/device-use-request-status', 'http://hl7.org/fhir/device-use-request-status', 'http://hl7.org/fhir/device-use-request-status');
  CODES_TFhirDeviceUseRequestPriorityEnum : Array[TFhirDeviceUseRequestPriorityEnum] of String = ('', 'routine', 'urgent', 'stat', 'asap');
  SYSTEMS_TFhirDeviceUseRequestPriorityEnum : Array[TFhirDeviceUseRequestPriorityEnum] of String = ('', 'http://hl7.org/fhir/device-use-request-priority', 'http://hl7.org/fhir/device-use-request-priority', 'http://hl7.org/fhir/device-use-request-priority', 'http://hl7.org/fhir/device-use-request-priority');
  CODES_TFhirDiagnosticOrderStatusEnum : Array[TFhirDiagnosticOrderStatusEnum] of String = ('', 'proposed', 'draft', 'planned', 'requested', 'received', 'accepted', 'in-progress', 'review', 'completed', 'cancelled', 'suspended', 'rejected', 'failed');
  SYSTEMS_TFhirDiagnosticOrderStatusEnum : Array[TFhirDiagnosticOrderStatusEnum] of String = ('', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status', 'http://hl7.org/fhir/diagnostic-order-status');
  CODES_TFhirDiagnosticOrderPriorityEnum : Array[TFhirDiagnosticOrderPriorityEnum] of String = ('', 'routine', 'urgent', 'stat', 'asap');
  SYSTEMS_TFhirDiagnosticOrderPriorityEnum : Array[TFhirDiagnosticOrderPriorityEnum] of String = ('', 'http://hl7.org/fhir/diagnostic-order-priority', 'http://hl7.org/fhir/diagnostic-order-priority', 'http://hl7.org/fhir/diagnostic-order-priority', 'http://hl7.org/fhir/diagnostic-order-priority');
  CODES_TFhirDiagnosticReportStatusEnum : Array[TFhirDiagnosticReportStatusEnum] of String = ('', 'registered', 'partial', 'final', 'corrected', 'appended', 'cancelled', 'entered-in-error');
  SYSTEMS_TFhirDiagnosticReportStatusEnum : Array[TFhirDiagnosticReportStatusEnum] of String = ('', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status', 'http://hl7.org/fhir/diagnostic-report-status');
  CODES_TFhirDocumentReferenceStatusEnum : Array[TFhirDocumentReferenceStatusEnum] of String = ('', 'current', 'superseded', 'entered-in-error');
  SYSTEMS_TFhirDocumentReferenceStatusEnum : Array[TFhirDocumentReferenceStatusEnum] of String = ('', 'http://hl7.org/fhir/document-reference-status', 'http://hl7.org/fhir/document-reference-status', 'http://hl7.org/fhir/document-reference-status');
  CODES_TFhirDocumentRelationshipTypeEnum : Array[TFhirDocumentRelationshipTypeEnum] of String = ('', 'replaces', 'transforms', 'signs', 'appends');
  SYSTEMS_TFhirDocumentRelationshipTypeEnum : Array[TFhirDocumentRelationshipTypeEnum] of String = ('', 'http://hl7.org/fhir/document-relationship-type', 'http://hl7.org/fhir/document-relationship-type', 'http://hl7.org/fhir/document-relationship-type', 'http://hl7.org/fhir/document-relationship-type');
  CODES_TFhirEncounterStateEnum : Array[TFhirEncounterStateEnum] of String = ('', 'planned', 'arrived', 'in-progress', 'onleave', 'finished', 'cancelled');
  SYSTEMS_TFhirEncounterStateEnum : Array[TFhirEncounterStateEnum] of String = ('', 'http://hl7.org/fhir/encounter-state', 'http://hl7.org/fhir/encounter-state', 'http://hl7.org/fhir/encounter-state', 'http://hl7.org/fhir/encounter-state', 'http://hl7.org/fhir/encounter-state', 'http://hl7.org/fhir/encounter-state');
  CODES_TFhirEncounterClassEnum : Array[TFhirEncounterClassEnum] of String = ('', 'inpatient', 'outpatient', 'ambulatory', 'emergency', 'home', 'field', 'daytime', 'virtual', 'other');
  SYSTEMS_TFhirEncounterClassEnum : Array[TFhirEncounterClassEnum] of String = ('', 'http://hl7.org/fhir/encounter-class', 'http://hl7.org/fhir/encounter-class', 'http://hl7.org/fhir/encounter-class', 'http://hl7.org/fhir/encounter-class', 'http://hl7.org/fhir/encounter-class', 'http://hl7.org/fhir/encounter-class', 'http://hl7.org/fhir/encounter-class', 'http://hl7.org/fhir/encounter-class', 'http://hl7.org/fhir/encounter-class');
  CODES_TFhirEncounterLocationStatusEnum : Array[TFhirEncounterLocationStatusEnum] of String = ('', 'planned', 'active', 'reserved', 'completed');
  SYSTEMS_TFhirEncounterLocationStatusEnum : Array[TFhirEncounterLocationStatusEnum] of String = ('', 'http://hl7.org/fhir/encounter-location-status', 'http://hl7.org/fhir/encounter-location-status', 'http://hl7.org/fhir/encounter-location-status', 'http://hl7.org/fhir/encounter-location-status');
  CODES_TFhirEpisodeOfCareStatusEnum : Array[TFhirEpisodeOfCareStatusEnum] of String = ('', 'planned', 'waitlist', 'active', 'onhold', 'finished', 'cancelled');
  SYSTEMS_TFhirEpisodeOfCareStatusEnum : Array[TFhirEpisodeOfCareStatusEnum] of String = ('', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status', 'http://hl7.org/fhir/episode-of-care-status');
  CODES_TFhirHistoryStatusEnum : Array[TFhirHistoryStatusEnum] of String = ('', 'partial', 'completed', 'entered-in-error', 'health-unknown');
  SYSTEMS_TFhirHistoryStatusEnum : Array[TFhirHistoryStatusEnum] of String = ('', 'http://hl7.org/fhir/history-status', 'http://hl7.org/fhir/history-status', 'http://hl7.org/fhir/history-status', 'http://hl7.org/fhir/history-status');
  CODES_TFhirAdministrativeGenderEnum : Array[TFhirAdministrativeGenderEnum] of String = ('', 'male', 'female', 'other', 'unknown');
  SYSTEMS_TFhirAdministrativeGenderEnum : Array[TFhirAdministrativeGenderEnum] of String = ('', 'http://hl7.org/fhir/administrative-gender', 'http://hl7.org/fhir/administrative-gender', 'http://hl7.org/fhir/administrative-gender', 'http://hl7.org/fhir/administrative-gender');
  CODES_TFhirFlagStatusEnum : Array[TFhirFlagStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirFlagStatusEnum : Array[TFhirFlagStatusEnum] of String = ('', 'http://hl7.org/fhir/flag-status', 'http://hl7.org/fhir/flag-status', 'http://hl7.org/fhir/flag-status');
  CODES_TFhirGoalStatusEnum : Array[TFhirGoalStatusEnum] of String = ('', 'proposed', 'planned', 'accepted', 'rejected', 'in-progress', 'achieved', 'sustaining', 'on-hold', 'cancelled');
  SYSTEMS_TFhirGoalStatusEnum : Array[TFhirGoalStatusEnum] of String = ('', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status', 'http://hl7.org/fhir/goal-status');
  CODES_TFhirGroupTypeEnum : Array[TFhirGroupTypeEnum] of String = ('', 'person', 'animal', 'practitioner', 'device', 'medication', 'substance');
  SYSTEMS_TFhirGroupTypeEnum : Array[TFhirGroupTypeEnum] of String = ('', 'http://hl7.org/fhir/group-type', 'http://hl7.org/fhir/group-type', 'http://hl7.org/fhir/group-type', 'http://hl7.org/fhir/group-type', 'http://hl7.org/fhir/group-type', 'http://hl7.org/fhir/group-type');
  CODES_TFhirDaysOfWeekEnum : Array[TFhirDaysOfWeekEnum] of String = ('', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun');
  SYSTEMS_TFhirDaysOfWeekEnum : Array[TFhirDaysOfWeekEnum] of String = ('', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week', 'http://hl7.org/fhir/days-of-week');
  CODES_TFhirInstanceAvailabilityEnum : Array[TFhirInstanceAvailabilityEnum] of String = ('', 'ONLINE', 'OFFLINE', 'NEARLINE', 'UNAVAILABLE');
  SYSTEMS_TFhirInstanceAvailabilityEnum : Array[TFhirInstanceAvailabilityEnum] of String = ('', 'http://nema.org/dicom/dicm', 'http://nema.org/dicom/dicm', 'http://nema.org/dicom/dicm', 'http://nema.org/dicom/dicm');
  CODES_TFhirMedicationAdminStatusEnum : Array[TFhirMedicationAdminStatusEnum] of String = ('', 'in-progress', 'on-hold', 'completed', 'entered-in-error', 'stopped');
  SYSTEMS_TFhirMedicationAdminStatusEnum : Array[TFhirMedicationAdminStatusEnum] of String = ('', 'http://hl7.org/fhir/medication-admin-status', 'http://hl7.org/fhir/medication-admin-status', 'http://hl7.org/fhir/medication-admin-status', 'http://hl7.org/fhir/medication-admin-status', 'http://hl7.org/fhir/medication-admin-status');
  CODES_TFhirGuideDependencyTypeEnum : Array[TFhirGuideDependencyTypeEnum] of String = ('', 'reference', 'inclusion');
  SYSTEMS_TFhirGuideDependencyTypeEnum : Array[TFhirGuideDependencyTypeEnum] of String = ('', 'http://hl7.org/fhir/guide-dependency-type', 'http://hl7.org/fhir/guide-dependency-type');
  CODES_TFhirGuideResourcePurposeEnum : Array[TFhirGuideResourcePurposeEnum] of String = ('', 'example', 'terminology', 'profile', 'extension', 'dictionary', 'logical');
  SYSTEMS_TFhirGuideResourcePurposeEnum : Array[TFhirGuideResourcePurposeEnum] of String = ('', 'http://hl7.org/fhir/guide-resource-purpose', 'http://hl7.org/fhir/guide-resource-purpose', 'http://hl7.org/fhir/guide-resource-purpose', 'http://hl7.org/fhir/guide-resource-purpose', 'http://hl7.org/fhir/guide-resource-purpose', 'http://hl7.org/fhir/guide-resource-purpose');
  CODES_TFhirGuidePageKindEnum : Array[TFhirGuidePageKindEnum] of String = ('', 'page', 'example', 'list', 'include', 'directory', 'dictionary', 'toc', 'resource');
  SYSTEMS_TFhirGuidePageKindEnum : Array[TFhirGuidePageKindEnum] of String = ('', 'http://hl7.org/fhir/guide-page-kind', 'http://hl7.org/fhir/guide-page-kind', 'http://hl7.org/fhir/guide-page-kind', 'http://hl7.org/fhir/guide-page-kind', 'http://hl7.org/fhir/guide-page-kind', 'http://hl7.org/fhir/guide-page-kind', 'http://hl7.org/fhir/guide-page-kind', 'http://hl7.org/fhir/guide-page-kind');
  CODES_TFhirListStatusEnum : Array[TFhirListStatusEnum] of String = ('', 'current', 'retired', 'entered-in-error');
  SYSTEMS_TFhirListStatusEnum : Array[TFhirListStatusEnum] of String = ('', 'http://hl7.org/fhir/list-status', 'http://hl7.org/fhir/list-status', 'http://hl7.org/fhir/list-status');
  CODES_TFhirLocationStatusEnum : Array[TFhirLocationStatusEnum] of String = ('', 'active', 'suspended', 'inactive');
  SYSTEMS_TFhirLocationStatusEnum : Array[TFhirLocationStatusEnum] of String = ('', 'http://hl7.org/fhir/location-status', 'http://hl7.org/fhir/location-status', 'http://hl7.org/fhir/location-status');
  CODES_TFhirLocationModeEnum : Array[TFhirLocationModeEnum] of String = ('', 'instance', 'kind');
  SYSTEMS_TFhirLocationModeEnum : Array[TFhirLocationModeEnum] of String = ('', 'http://hl7.org/fhir/location-mode', 'http://hl7.org/fhir/location-mode');
  CODES_TFhirDigitalMediaTypeEnum : Array[TFhirDigitalMediaTypeEnum] of String = ('', 'photo', 'video', 'audio');
  SYSTEMS_TFhirDigitalMediaTypeEnum : Array[TFhirDigitalMediaTypeEnum] of String = ('', 'http://hl7.org/fhir/digital-media-type', 'http://hl7.org/fhir/digital-media-type', 'http://hl7.org/fhir/digital-media-type');
  CODES_TFhirMedicationDispenseStatusEnum : Array[TFhirMedicationDispenseStatusEnum] of String = ('', 'in-progress', 'on-hold', 'completed', 'entered-in-error', 'stopped');
  SYSTEMS_TFhirMedicationDispenseStatusEnum : Array[TFhirMedicationDispenseStatusEnum] of String = ('', 'http://hl7.org/fhir/medication-dispense-status', 'http://hl7.org/fhir/medication-dispense-status', 'http://hl7.org/fhir/medication-dispense-status', 'http://hl7.org/fhir/medication-dispense-status', 'http://hl7.org/fhir/medication-dispense-status');
  CODES_TFhirMedicationOrderStatusEnum : Array[TFhirMedicationOrderStatusEnum] of String = ('', 'active', 'on-hold', 'completed', 'entered-in-error', 'stopped', 'draft');
  SYSTEMS_TFhirMedicationOrderStatusEnum : Array[TFhirMedicationOrderStatusEnum] of String = ('', 'http://hl7.org/fhir/medication-order-status', 'http://hl7.org/fhir/medication-order-status', 'http://hl7.org/fhir/medication-order-status', 'http://hl7.org/fhir/medication-order-status', 'http://hl7.org/fhir/medication-order-status', 'http://hl7.org/fhir/medication-order-status');
  CODES_TFhirMedicationStatementStatusEnum : Array[TFhirMedicationStatementStatusEnum] of String = ('', 'active', 'completed', 'entered-in-error', 'intended');
  SYSTEMS_TFhirMedicationStatementStatusEnum : Array[TFhirMedicationStatementStatusEnum] of String = ('', 'http://hl7.org/fhir/medication-statement-status', 'http://hl7.org/fhir/medication-statement-status', 'http://hl7.org/fhir/medication-statement-status', 'http://hl7.org/fhir/medication-statement-status');
  CODES_TFhirResponseCodeEnum : Array[TFhirResponseCodeEnum] of String = ('', 'ok', 'transient-error', 'fatal-error');
  SYSTEMS_TFhirResponseCodeEnum : Array[TFhirResponseCodeEnum] of String = ('', 'http://hl7.org/fhir/response-code', 'http://hl7.org/fhir/response-code', 'http://hl7.org/fhir/response-code');
  CODES_TFhirNamingsystemTypeEnum : Array[TFhirNamingsystemTypeEnum] of String = ('', 'codesystem', 'identifier', 'root');
  SYSTEMS_TFhirNamingsystemTypeEnum : Array[TFhirNamingsystemTypeEnum] of String = ('', 'http://hl7.org/fhir/namingsystem-type', 'http://hl7.org/fhir/namingsystem-type', 'http://hl7.org/fhir/namingsystem-type');
  CODES_TFhirNamingsystemIdentifierTypeEnum : Array[TFhirNamingsystemIdentifierTypeEnum] of String = ('', 'oid', 'uuid', 'uri', 'other');
  SYSTEMS_TFhirNamingsystemIdentifierTypeEnum : Array[TFhirNamingsystemIdentifierTypeEnum] of String = ('', 'http://hl7.org/fhir/namingsystem-identifier-type', 'http://hl7.org/fhir/namingsystem-identifier-type', 'http://hl7.org/fhir/namingsystem-identifier-type', 'http://hl7.org/fhir/namingsystem-identifier-type');
  CODES_TFhirNutritionOrderStatusEnum : Array[TFhirNutritionOrderStatusEnum] of String = ('', 'proposed', 'draft', 'planned', 'requested', 'active', 'on-hold', 'completed', 'cancelled');
  SYSTEMS_TFhirNutritionOrderStatusEnum : Array[TFhirNutritionOrderStatusEnum] of String = ('', 'http://hl7.org/fhir/nutrition-order-status', 'http://hl7.org/fhir/nutrition-order-status', 'http://hl7.org/fhir/nutrition-order-status', 'http://hl7.org/fhir/nutrition-order-status', 'http://hl7.org/fhir/nutrition-order-status', 'http://hl7.org/fhir/nutrition-order-status', 'http://hl7.org/fhir/nutrition-order-status', 'http://hl7.org/fhir/nutrition-order-status');
  CODES_TFhirObservationStatusEnum : Array[TFhirObservationStatusEnum] of String = ('', 'registered', 'preliminary', 'final', 'amended', 'cancelled', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirObservationStatusEnum : Array[TFhirObservationStatusEnum] of String = ('', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status', 'http://hl7.org/fhir/observation-status');
  CODES_TFhirObservationRelationshiptypesEnum : Array[TFhirObservationRelationshiptypesEnum] of String = ('', 'has-member', 'derived-from', 'sequel-to', 'replaces', 'qualified-by', 'interfered-by');
  SYSTEMS_TFhirObservationRelationshiptypesEnum : Array[TFhirObservationRelationshiptypesEnum] of String = ('', 'http://hl7.org/fhir/observation-relationshiptypes', 'http://hl7.org/fhir/observation-relationshiptypes', 'http://hl7.org/fhir/observation-relationshiptypes', 'http://hl7.org/fhir/observation-relationshiptypes', 'http://hl7.org/fhir/observation-relationshiptypes', 'http://hl7.org/fhir/observation-relationshiptypes');
  CODES_TFhirOperationKindEnum : Array[TFhirOperationKindEnum] of String = ('', 'operation', 'query');
  SYSTEMS_TFhirOperationKindEnum : Array[TFhirOperationKindEnum] of String = ('', 'http://hl7.org/fhir/operation-kind', 'http://hl7.org/fhir/operation-kind');
  CODES_TFhirOperationParameterUseEnum : Array[TFhirOperationParameterUseEnum] of String = ('', 'in', 'out');
  SYSTEMS_TFhirOperationParameterUseEnum : Array[TFhirOperationParameterUseEnum] of String = ('', 'http://hl7.org/fhir/operation-parameter-use', 'http://hl7.org/fhir/operation-parameter-use');
  CODES_TFhirOperationParameterTypeEnum : Array[TFhirOperationParameterTypeEnum] of String = ('', 'number', 'date', 'string', 'token', 'reference', 'composite', 'quantity', 'uri', 'Address', 'Age', 'Annotation', 'Attachment', 'BackboneElement', 'CodeableConcept', 'Coding', 'ContactPoint', 'Count', 'Distance', 'Duration', 'Element', 'ElementDefinition', 'Extension', 'HumanName', 'Identifier', 'Meta', 'Money', 'Narrative', 'Period', 'Quantity', 'Range', 'Ratio', 'Reference', 'SampledData', 'Signature', 'SimpleQuantity', 'Timing', 'base64Binary', 'boolean', 'code', 'dateTime', 'decimal', 'id', 'instant', 'integer', 'markdown', 'oid', 'positiveInt', 'time', 'unsignedInt', 'uuid', 'xhtml', 'Account', 'AllergyIntolerance', 'Appointment', 'AppointmentResponse', 'AuditEvent', 'Basic', 'Binary', 'BodySite', 'Bundle', 'CarePlan', 'Claim', 'ClaimResponse', 'ClinicalImpression', 'Communication', 'CommunicationRequest', 'Composition', 'ConceptMap', 'Condition', 'Conformance', 'Contract', 'Coverage',
    'DataElement', 'DetectedIssue', 'Device', 'DeviceComponent', 'DeviceMetric', 'DeviceUseRequest', 'DeviceUseStatement', 'DiagnosticOrder', 'DiagnosticReport', 'DocumentManifest', 'DocumentReference', 'DomainResource', 'EligibilityRequest', 'EligibilityResponse', 'Encounter', 'EnrollmentRequest', 'EnrollmentResponse', 'EpisodeOfCare', 'ExplanationOfBenefit', 'FamilyMemberHistory', 'Flag', 'Goal', 'Group', 'HealthcareService', 'ImagingObjectSelection', 'ImagingStudy', 'Immunization', 'ImmunizationRecommendation', 'ImplementationGuide', 'List', 'Location', 'Media', 'Medication', 'MedicationAdministration', 'MedicationDispense', 'MedicationOrder', 'MedicationStatement', 'MessageHeader', 'NamingSystem', 'NutritionOrder', 'Observation', 'OperationDefinition', 'OperationOutcome', 'Order', 'OrderResponse', 'Organization', 'Parameters', 'Patient', 'PaymentNotice', 'PaymentReconciliation', 'Person', 'Practitioner', 'Procedure', 'ProcedureRequest', 'ProcessRequest', 'ProcessResponse', 'Provenance',
    'Questionnaire', 'QuestionnaireResponse', 'ReferralRequest', 'RelatedPerson', 'Resource', 'RiskAssessment', 'Schedule', 'SearchParameter', 'Slot', 'Specimen', 'StructureDefinition', 'Subscription', 'Substance', 'SupplyDelivery', 'SupplyRequest', 'TestScript', 'ValueSet', 'VisionPrescription');
  SYSTEMS_TFhirOperationParameterTypeEnum : Array[TFhirOperationParameterTypeEnum] of String = ('', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/search-param-type', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 
    'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/data-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 
    'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 
    'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 
    'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 
    'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types', 'http://hl7.org/fhir/resource-types');
  CODES_TFhirIssueSeverityEnum : Array[TFhirIssueSeverityEnum] of String = ('', 'fatal', 'error', 'warning', 'information');
  SYSTEMS_TFhirIssueSeverityEnum : Array[TFhirIssueSeverityEnum] of String = ('', 'http://hl7.org/fhir/issue-severity', 'http://hl7.org/fhir/issue-severity', 'http://hl7.org/fhir/issue-severity', 'http://hl7.org/fhir/issue-severity');
  CODES_TFhirIssueTypeEnum : Array[TFhirIssueTypeEnum] of String = ('', 'invalid', 'structure', 'required', 'value', 'invariant', 'security', 'login', 'unknown', 'expired', 'forbidden', 'suppressed', 'processing', 'not-supported', 'duplicate', 'not-found', 'too-long', 'code-invalid', 'extension', 'too-costly', 'business-rule', 'conflict', 'incomplete', 'transient', 'lock-error', 'no-store', 'exception', 'timeout', 'throttled', 'informational');
  SYSTEMS_TFhirIssueTypeEnum : Array[TFhirIssueTypeEnum] of String = ('', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type', 
    'http://hl7.org/fhir/issue-type', 'http://hl7.org/fhir/issue-type');
  CODES_TFhirOrderStatusEnum : Array[TFhirOrderStatusEnum] of String = ('', 'pending', 'review', 'rejected', 'error', 'accepted', 'cancelled', 'replaced', 'aborted', 'completed');
  SYSTEMS_TFhirOrderStatusEnum : Array[TFhirOrderStatusEnum] of String = ('', 'http://hl7.org/fhir/order-status', 'http://hl7.org/fhir/order-status', 'http://hl7.org/fhir/order-status', 'http://hl7.org/fhir/order-status', 'http://hl7.org/fhir/order-status', 'http://hl7.org/fhir/order-status', 'http://hl7.org/fhir/order-status', 'http://hl7.org/fhir/order-status', 'http://hl7.org/fhir/order-status');
  CODES_TFhirLinkTypeEnum : Array[TFhirLinkTypeEnum] of String = ('', 'replace', 'refer', 'seealso');
  SYSTEMS_TFhirLinkTypeEnum : Array[TFhirLinkTypeEnum] of String = ('', 'http://hl7.org/fhir/link-type', 'http://hl7.org/fhir/link-type', 'http://hl7.org/fhir/link-type');
  CODES_TFhirIdentityAssuranceLevelEnum : Array[TFhirIdentityAssuranceLevelEnum] of String = ('', 'level1', 'level2', 'level3', 'level4');
  SYSTEMS_TFhirIdentityAssuranceLevelEnum : Array[TFhirIdentityAssuranceLevelEnum] of String = ('', 'http://hl7.org/fhir/identity-assuranceLevel', 'http://hl7.org/fhir/identity-assuranceLevel', 'http://hl7.org/fhir/identity-assuranceLevel', 'http://hl7.org/fhir/identity-assuranceLevel');
  CODES_TFhirProcedureStatusEnum : Array[TFhirProcedureStatusEnum] of String = ('', 'in-progress', 'aborted', 'completed', 'entered-in-error');
  SYSTEMS_TFhirProcedureStatusEnum : Array[TFhirProcedureStatusEnum] of String = ('', 'http://hl7.org/fhir/procedure-status', 'http://hl7.org/fhir/procedure-status', 'http://hl7.org/fhir/procedure-status', 'http://hl7.org/fhir/procedure-status');
  CODES_TFhirProcedureRequestStatusEnum : Array[TFhirProcedureRequestStatusEnum] of String = ('', 'proposed', 'draft', 'requested', 'received', 'accepted', 'in-progress', 'completed', 'suspended', 'rejected', 'aborted');
  SYSTEMS_TFhirProcedureRequestStatusEnum : Array[TFhirProcedureRequestStatusEnum] of String = ('', 'http://hl7.org/fhir/procedure-request-status', 'http://hl7.org/fhir/procedure-request-status', 'http://hl7.org/fhir/procedure-request-status', 'http://hl7.org/fhir/procedure-request-status', 'http://hl7.org/fhir/procedure-request-status', 'http://hl7.org/fhir/procedure-request-status', 'http://hl7.org/fhir/procedure-request-status', 'http://hl7.org/fhir/procedure-request-status', 'http://hl7.org/fhir/procedure-request-status', 'http://hl7.org/fhir/procedure-request-status');
  CODES_TFhirProcedureRequestPriorityEnum : Array[TFhirProcedureRequestPriorityEnum] of String = ('', 'routine', 'urgent', 'stat', 'asap');
  SYSTEMS_TFhirProcedureRequestPriorityEnum : Array[TFhirProcedureRequestPriorityEnum] of String = ('', 'http://hl7.org/fhir/procedure-request-priority', 'http://hl7.org/fhir/procedure-request-priority', 'http://hl7.org/fhir/procedure-request-priority', 'http://hl7.org/fhir/procedure-request-priority');
  CODES_TFhirActionlistEnum : Array[TFhirActionlistEnum] of String = ('', 'cancel', 'poll', 'reprocess', 'status');
  SYSTEMS_TFhirActionlistEnum : Array[TFhirActionlistEnum] of String = ('', 'http://hl7.org/fhir/actionlist', 'http://hl7.org/fhir/actionlist', 'http://hl7.org/fhir/actionlist', 'http://hl7.org/fhir/actionlist');
  CODES_TFhirProvenanceEntityRoleEnum : Array[TFhirProvenanceEntityRoleEnum] of String = ('', 'derivation', 'revision', 'quotation', 'source');
  SYSTEMS_TFhirProvenanceEntityRoleEnum : Array[TFhirProvenanceEntityRoleEnum] of String = ('', 'http://hl7.org/fhir/provenance-entity-role', 'http://hl7.org/fhir/provenance-entity-role', 'http://hl7.org/fhir/provenance-entity-role', 'http://hl7.org/fhir/provenance-entity-role');
  CODES_TFhirQuestionnaireStatusEnum : Array[TFhirQuestionnaireStatusEnum] of String = ('', 'draft', 'Published', 'retired');
  SYSTEMS_TFhirQuestionnaireStatusEnum : Array[TFhirQuestionnaireStatusEnum] of String = ('', 'http://hl7.org/fhir/questionnaire-status', 'http://hl7.org/fhir/questionnaire-status', 'http://hl7.org/fhir/questionnaire-status');
  CODES_TFhirAnswerFormatEnum : Array[TFhirAnswerFormatEnum] of String = ('', 'boolean', 'decimal', 'integer', 'date', 'dateTime', 'instant', 'time', 'string', 'text', 'url', 'choice', 'open-choice', 'attachment', 'reference', 'quantity');
  SYSTEMS_TFhirAnswerFormatEnum : Array[TFhirAnswerFormatEnum] of String = ('', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format', 'http://hl7.org/fhir/answer-format');
  CODES_TFhirQuestionnaireAnswersStatusEnum : Array[TFhirQuestionnaireAnswersStatusEnum] of String = ('', 'in-progress', 'completed', 'amended');
  SYSTEMS_TFhirQuestionnaireAnswersStatusEnum : Array[TFhirQuestionnaireAnswersStatusEnum] of String = ('', 'http://hl7.org/fhir/questionnaire-answers-status', 'http://hl7.org/fhir/questionnaire-answers-status', 'http://hl7.org/fhir/questionnaire-answers-status');
  CODES_TFhirReferralstatusEnum : Array[TFhirReferralstatusEnum] of String = ('', 'draft', 'requested', 'active', 'cancelled', 'accepted', 'rejected', 'completed');
  SYSTEMS_TFhirReferralstatusEnum : Array[TFhirReferralstatusEnum] of String = ('', 'http://hl7.org/fhir/referralstatus', 'http://hl7.org/fhir/referralstatus', 'http://hl7.org/fhir/referralstatus', 'http://hl7.org/fhir/referralstatus', 'http://hl7.org/fhir/referralstatus', 'http://hl7.org/fhir/referralstatus', 'http://hl7.org/fhir/referralstatus');
  CODES_TFhirSearchXpathUsageEnum : Array[TFhirSearchXpathUsageEnum] of String = ('', 'normal', 'phonetic', 'nearby', 'distance', 'other');
  SYSTEMS_TFhirSearchXpathUsageEnum : Array[TFhirSearchXpathUsageEnum] of String = ('', 'http://hl7.org/fhir/search-xpath-usage', 'http://hl7.org/fhir/search-xpath-usage', 'http://hl7.org/fhir/search-xpath-usage', 'http://hl7.org/fhir/search-xpath-usage', 'http://hl7.org/fhir/search-xpath-usage');
  CODES_TFhirSlotstatusEnum : Array[TFhirSlotstatusEnum] of String = ('', 'busy', 'free', 'busy-unavailable', 'busy-tentative');
  SYSTEMS_TFhirSlotstatusEnum : Array[TFhirSlotstatusEnum] of String = ('', 'http://hl7.org/fhir/slotstatus', 'http://hl7.org/fhir/slotstatus', 'http://hl7.org/fhir/slotstatus', 'http://hl7.org/fhir/slotstatus');
  CODES_TFhirSpecimenStatusEnum : Array[TFhirSpecimenStatusEnum] of String = ('', 'available', 'unavailable', 'unsatisfactory', 'entered-in-error');
  SYSTEMS_TFhirSpecimenStatusEnum : Array[TFhirSpecimenStatusEnum] of String = ('', 'http://hl7.org/fhir/specimen-status', 'http://hl7.org/fhir/specimen-status', 'http://hl7.org/fhir/specimen-status', 'http://hl7.org/fhir/specimen-status');
  CODES_TFhirStructureDefinitionKindEnum : Array[TFhirStructureDefinitionKindEnum] of String = ('', 'datatype', 'resource', 'logical');
  SYSTEMS_TFhirStructureDefinitionKindEnum : Array[TFhirStructureDefinitionKindEnum] of String = ('', 'http://hl7.org/fhir/structure-definition-kind', 'http://hl7.org/fhir/structure-definition-kind', 'http://hl7.org/fhir/structure-definition-kind');
  CODES_TFhirExtensionContextEnum : Array[TFhirExtensionContextEnum] of String = ('', 'resource', 'datatype', 'mapping', 'extension');
  SYSTEMS_TFhirExtensionContextEnum : Array[TFhirExtensionContextEnum] of String = ('', 'http://hl7.org/fhir/extension-context', 'http://hl7.org/fhir/extension-context', 'http://hl7.org/fhir/extension-context', 'http://hl7.org/fhir/extension-context');
  CODES_TFhirSubscriptionStatusEnum : Array[TFhirSubscriptionStatusEnum] of String = ('', 'requested', 'active', 'error', 'off');
  SYSTEMS_TFhirSubscriptionStatusEnum : Array[TFhirSubscriptionStatusEnum] of String = ('', 'http://hl7.org/fhir/subscription-status', 'http://hl7.org/fhir/subscription-status', 'http://hl7.org/fhir/subscription-status', 'http://hl7.org/fhir/subscription-status');
  CODES_TFhirSubscriptionChannelTypeEnum : Array[TFhirSubscriptionChannelTypeEnum] of String = ('', 'rest-hook', 'websocket', 'email', 'sms', 'message');
  SYSTEMS_TFhirSubscriptionChannelTypeEnum : Array[TFhirSubscriptionChannelTypeEnum] of String = ('', 'http://hl7.org/fhir/subscription-channel-type', 'http://hl7.org/fhir/subscription-channel-type', 'http://hl7.org/fhir/subscription-channel-type', 'http://hl7.org/fhir/subscription-channel-type', 'http://hl7.org/fhir/subscription-channel-type');
  CODES_TFhirSupplydeliveryStatusEnum : Array[TFhirSupplydeliveryStatusEnum] of String = ('', 'in-progress', 'completed', 'abandoned');
  SYSTEMS_TFhirSupplydeliveryStatusEnum : Array[TFhirSupplydeliveryStatusEnum] of String = ('', 'http://hl7.org/fhir/supplydelivery-status', 'http://hl7.org/fhir/supplydelivery-status', 'http://hl7.org/fhir/supplydelivery-status');
  CODES_TFhirSupplyrequestStatusEnum : Array[TFhirSupplyrequestStatusEnum] of String = ('', 'requested', 'completed', 'failed', 'cancelled');
  SYSTEMS_TFhirSupplyrequestStatusEnum : Array[TFhirSupplyrequestStatusEnum] of String = ('', 'http://hl7.org/fhir/supplyrequest-status', 'http://hl7.org/fhir/supplyrequest-status', 'http://hl7.org/fhir/supplyrequest-status', 'http://hl7.org/fhir/supplyrequest-status');
  CODES_TFhirContentTypeEnum : Array[TFhirContentTypeEnum] of String = ('', 'xml', 'json');
  SYSTEMS_TFhirContentTypeEnum : Array[TFhirContentTypeEnum] of String = ('', 'http://hl7.org/fhir/content-type', 'http://hl7.org/fhir/content-type');
  CODES_TFhirAssertDirectionCodesEnum : Array[TFhirAssertDirectionCodesEnum] of String = ('', 'response', 'request');
  SYSTEMS_TFhirAssertDirectionCodesEnum : Array[TFhirAssertDirectionCodesEnum] of String = ('', 'http://hl7.org/fhir/assert-direction-codes', 'http://hl7.org/fhir/assert-direction-codes');
  CODES_TFhirAssertOperatorCodesEnum : Array[TFhirAssertOperatorCodesEnum] of String = ('', 'equals', 'notEquals', 'in', 'notIn', 'greaterThan', 'lessThan', 'empty', 'notEmpty', 'contains', 'notContains');
  SYSTEMS_TFhirAssertOperatorCodesEnum : Array[TFhirAssertOperatorCodesEnum] of String = ('', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes', 'http://hl7.org/fhir/assert-operator-codes');
  CODES_TFhirAssertResponseCodeTypesEnum : Array[TFhirAssertResponseCodeTypesEnum] of String = ('', 'okay', 'created', 'noContent', 'notModified', 'bad', 'forbidden', 'notFound', 'methodNotAllowed', 'conflict', 'gone', 'preconditionFailed', 'unprocessable');
  SYSTEMS_TFhirAssertResponseCodeTypesEnum : Array[TFhirAssertResponseCodeTypesEnum] of String = ('', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types', 'http://hl7.org/fhir/assert-response-code-types');
  CODES_TFhirFilterOperatorEnum : Array[TFhirFilterOperatorEnum] of String = ('', '=', 'is-a', 'is-not-a', 'regex', 'in', 'not-in');
  SYSTEMS_TFhirFilterOperatorEnum : Array[TFhirFilterOperatorEnum] of String = ('', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator', 'http://hl7.org/fhir/filter-operator');
  CODES_TFhirVisionEyeCodesEnum : Array[TFhirVisionEyeCodesEnum] of String = ('', 'right', 'left');
  SYSTEMS_TFhirVisionEyeCodesEnum : Array[TFhirVisionEyeCodesEnum] of String = ('', 'http://hl7.org/fhir/vision-eye-codes', 'http://hl7.org/fhir/vision-eye-codes');
  CODES_TFhirVisionBaseCodesEnum : Array[TFhirVisionBaseCodesEnum] of String = ('', 'up', 'down', 'in', 'out');
  SYSTEMS_TFhirVisionBaseCodesEnum : Array[TFhirVisionBaseCodesEnum] of String = ('', 'http://hl7.org/fhir/vision-base-codes', 'http://hl7.org/fhir/vision-base-codes', 'http://hl7.org/fhir/vision-base-codes', 'http://hl7.org/fhir/vision-base-codes');

Function TFhirNarrativeStatusEnumListAsInteger(aSet : TFhirNarrativeStatusEnumList) : Integer; overload;
Function IntegerAsTFhirNarrativeStatusEnumList(i : integer) : TFhirNarrativeStatusEnumList; overload;
Function TFhirIdentifierUseEnumListAsInteger(aSet : TFhirIdentifierUseEnumList) : Integer; overload;
Function IntegerAsTFhirIdentifierUseEnumList(i : integer) : TFhirIdentifierUseEnumList; overload;
Function TFhirQuantityComparatorEnumListAsInteger(aSet : TFhirQuantityComparatorEnumList) : Integer; overload;
Function IntegerAsTFhirQuantityComparatorEnumList(i : integer) : TFhirQuantityComparatorEnumList; overload;
Function TFhirNameUseEnumListAsInteger(aSet : TFhirNameUseEnumList) : Integer; overload;
Function IntegerAsTFhirNameUseEnumList(i : integer) : TFhirNameUseEnumList; overload;
Function TFhirContactPointSystemEnumListAsInteger(aSet : TFhirContactPointSystemEnumList) : Integer; overload;
Function IntegerAsTFhirContactPointSystemEnumList(i : integer) : TFhirContactPointSystemEnumList; overload;
Function TFhirContactPointUseEnumListAsInteger(aSet : TFhirContactPointUseEnumList) : Integer; overload;
Function IntegerAsTFhirContactPointUseEnumList(i : integer) : TFhirContactPointUseEnumList; overload;
Function TFhirAddressUseEnumListAsInteger(aSet : TFhirAddressUseEnumList) : Integer; overload;
Function IntegerAsTFhirAddressUseEnumList(i : integer) : TFhirAddressUseEnumList; overload;
Function TFhirAddressTypeEnumListAsInteger(aSet : TFhirAddressTypeEnumList) : Integer; overload;
Function IntegerAsTFhirAddressTypeEnumList(i : integer) : TFhirAddressTypeEnumList; overload;
Function TFhirPropertyRepresentationEnumListAsInteger(aSet : TFhirPropertyRepresentationEnumList) : Integer; overload;
Function IntegerAsTFhirPropertyRepresentationEnumList(i : integer) : TFhirPropertyRepresentationEnumList; overload;
Function TFhirResourceSlicingRulesEnumListAsInteger(aSet : TFhirResourceSlicingRulesEnumList) : Integer; overload;
Function IntegerAsTFhirResourceSlicingRulesEnumList(i : integer) : TFhirResourceSlicingRulesEnumList; overload;
Function TFhirResourceAggregationModeEnumListAsInteger(aSet : TFhirResourceAggregationModeEnumList) : Integer; overload;
Function IntegerAsTFhirResourceAggregationModeEnumList(i : integer) : TFhirResourceAggregationModeEnumList; overload;
Function TFhirConstraintSeverityEnumListAsInteger(aSet : TFhirConstraintSeverityEnumList) : Integer; overload;
Function IntegerAsTFhirConstraintSeverityEnumList(i : integer) : TFhirConstraintSeverityEnumList; overload;
Function TFhirBindingStrengthEnumListAsInteger(aSet : TFhirBindingStrengthEnumList) : Integer; overload;
Function IntegerAsTFhirBindingStrengthEnumList(i : integer) : TFhirBindingStrengthEnumList; overload;
Function TFhirUnitsOfTimeEnumListAsInteger(aSet : TFhirUnitsOfTimeEnumList) : Integer; overload;
Function IntegerAsTFhirUnitsOfTimeEnumList(i : integer) : TFhirUnitsOfTimeEnumList; overload;
Function TFhirEventTimingEnumListAsInteger(aSet : TFhirEventTimingEnumList) : Integer; overload;
Function IntegerAsTFhirEventTimingEnumList(i : integer) : TFhirEventTimingEnumList; overload;
Function TFhirAllergyIntoleranceStatusEnumListAsInteger(aSet : TFhirAllergyIntoleranceStatusEnumList) : Integer; overload;
Function IntegerAsTFhirAllergyIntoleranceStatusEnumList(i : integer) : TFhirAllergyIntoleranceStatusEnumList; overload;
Function TFhirAllergyIntoleranceCriticalityEnumListAsInteger(aSet : TFhirAllergyIntoleranceCriticalityEnumList) : Integer; overload;
Function IntegerAsTFhirAllergyIntoleranceCriticalityEnumList(i : integer) : TFhirAllergyIntoleranceCriticalityEnumList; overload;
Function TFhirAllergyIntoleranceTypeEnumListAsInteger(aSet : TFhirAllergyIntoleranceTypeEnumList) : Integer; overload;
Function IntegerAsTFhirAllergyIntoleranceTypeEnumList(i : integer) : TFhirAllergyIntoleranceTypeEnumList; overload;
Function TFhirAllergyIntoleranceCategoryEnumListAsInteger(aSet : TFhirAllergyIntoleranceCategoryEnumList) : Integer; overload;
Function IntegerAsTFhirAllergyIntoleranceCategoryEnumList(i : integer) : TFhirAllergyIntoleranceCategoryEnumList; overload;
Function TFhirReactionEventCertaintyEnumListAsInteger(aSet : TFhirReactionEventCertaintyEnumList) : Integer; overload;
Function IntegerAsTFhirReactionEventCertaintyEnumList(i : integer) : TFhirReactionEventCertaintyEnumList; overload;
Function TFhirReactionEventSeverityEnumListAsInteger(aSet : TFhirReactionEventSeverityEnumList) : Integer; overload;
Function IntegerAsTFhirReactionEventSeverityEnumList(i : integer) : TFhirReactionEventSeverityEnumList; overload;
Function TFhirAppointmentstatusEnumListAsInteger(aSet : TFhirAppointmentstatusEnumList) : Integer; overload;
Function IntegerAsTFhirAppointmentstatusEnumList(i : integer) : TFhirAppointmentstatusEnumList; overload;
Function TFhirParticipantrequiredEnumListAsInteger(aSet : TFhirParticipantrequiredEnumList) : Integer; overload;
Function IntegerAsTFhirParticipantrequiredEnumList(i : integer) : TFhirParticipantrequiredEnumList; overload;
Function TFhirParticipationstatusEnumListAsInteger(aSet : TFhirParticipationstatusEnumList) : Integer; overload;
Function IntegerAsTFhirParticipationstatusEnumList(i : integer) : TFhirParticipationstatusEnumList; overload;
Function TFhirParticipantstatusEnumListAsInteger(aSet : TFhirParticipantstatusEnumList) : Integer; overload;
Function IntegerAsTFhirParticipantstatusEnumList(i : integer) : TFhirParticipantstatusEnumList; overload;
Function TFhirAuditEventActionEnumListAsInteger(aSet : TFhirAuditEventActionEnumList) : Integer; overload;
Function IntegerAsTFhirAuditEventActionEnumList(i : integer) : TFhirAuditEventActionEnumList; overload;
Function TFhirAuditEventOutcomeEnumListAsInteger(aSet : TFhirAuditEventOutcomeEnumList) : Integer; overload;
Function IntegerAsTFhirAuditEventOutcomeEnumList(i : integer) : TFhirAuditEventOutcomeEnumList; overload;
Function TFhirNetworkTypeEnumListAsInteger(aSet : TFhirNetworkTypeEnumList) : Integer; overload;
Function IntegerAsTFhirNetworkTypeEnumList(i : integer) : TFhirNetworkTypeEnumList; overload;
Function TFhirBundleTypeEnumListAsInteger(aSet : TFhirBundleTypeEnumList) : Integer; overload;
Function IntegerAsTFhirBundleTypeEnumList(i : integer) : TFhirBundleTypeEnumList; overload;
Function TFhirSearchEntryModeEnumListAsInteger(aSet : TFhirSearchEntryModeEnumList) : Integer; overload;
Function IntegerAsTFhirSearchEntryModeEnumList(i : integer) : TFhirSearchEntryModeEnumList; overload;
Function TFhirHttpVerbEnumListAsInteger(aSet : TFhirHttpVerbEnumList) : Integer; overload;
Function IntegerAsTFhirHttpVerbEnumList(i : integer) : TFhirHttpVerbEnumList; overload;
Function TFhirCarePlanStatusEnumListAsInteger(aSet : TFhirCarePlanStatusEnumList) : Integer; overload;
Function IntegerAsTFhirCarePlanStatusEnumList(i : integer) : TFhirCarePlanStatusEnumList; overload;
Function TFhirCarePlanRelationshipEnumListAsInteger(aSet : TFhirCarePlanRelationshipEnumList) : Integer; overload;
Function IntegerAsTFhirCarePlanRelationshipEnumList(i : integer) : TFhirCarePlanRelationshipEnumList; overload;
Function TFhirCarePlanActivityStatusEnumListAsInteger(aSet : TFhirCarePlanActivityStatusEnumList) : Integer; overload;
Function IntegerAsTFhirCarePlanActivityStatusEnumList(i : integer) : TFhirCarePlanActivityStatusEnumList; overload;
Function TFhirClaimTypeLinkEnumListAsInteger(aSet : TFhirClaimTypeLinkEnumList) : Integer; overload;
Function IntegerAsTFhirClaimTypeLinkEnumList(i : integer) : TFhirClaimTypeLinkEnumList; overload;
Function TFhirClaimUseLinkEnumListAsInteger(aSet : TFhirClaimUseLinkEnumList) : Integer; overload;
Function IntegerAsTFhirClaimUseLinkEnumList(i : integer) : TFhirClaimUseLinkEnumList; overload;
Function TFhirRemittanceOutcomeEnumListAsInteger(aSet : TFhirRemittanceOutcomeEnumList) : Integer; overload;
Function IntegerAsTFhirRemittanceOutcomeEnumList(i : integer) : TFhirRemittanceOutcomeEnumList; overload;
Function TFhirClinicalImpressionStatusEnumListAsInteger(aSet : TFhirClinicalImpressionStatusEnumList) : Integer; overload;
Function IntegerAsTFhirClinicalImpressionStatusEnumList(i : integer) : TFhirClinicalImpressionStatusEnumList; overload;
Function TFhirCommunicationStatusEnumListAsInteger(aSet : TFhirCommunicationStatusEnumList) : Integer; overload;
Function IntegerAsTFhirCommunicationStatusEnumList(i : integer) : TFhirCommunicationStatusEnumList; overload;
Function TFhirCommunicationRequestStatusEnumListAsInteger(aSet : TFhirCommunicationRequestStatusEnumList) : Integer; overload;
Function IntegerAsTFhirCommunicationRequestStatusEnumList(i : integer) : TFhirCommunicationRequestStatusEnumList; overload;
Function TFhirCompositionStatusEnumListAsInteger(aSet : TFhirCompositionStatusEnumList) : Integer; overload;
Function IntegerAsTFhirCompositionStatusEnumList(i : integer) : TFhirCompositionStatusEnumList; overload;
Function TFhirV3ConfidentialityEnumListAsInteger(aSet : TFhirV3ConfidentialityEnumList) : Integer; overload;
Function IntegerAsTFhirV3ConfidentialityEnumList(i : integer) : TFhirV3ConfidentialityEnumList; overload;
Function TFhirCompositionAttestationModeEnumListAsInteger(aSet : TFhirCompositionAttestationModeEnumList) : Integer; overload;
Function IntegerAsTFhirCompositionAttestationModeEnumList(i : integer) : TFhirCompositionAttestationModeEnumList; overload;
Function TFhirListModeEnumListAsInteger(aSet : TFhirListModeEnumList) : Integer; overload;
Function IntegerAsTFhirListModeEnumList(i : integer) : TFhirListModeEnumList; overload;
Function TFhirConformanceResourceStatusEnumListAsInteger(aSet : TFhirConformanceResourceStatusEnumList) : Integer; overload;
Function IntegerAsTFhirConformanceResourceStatusEnumList(i : integer) : TFhirConformanceResourceStatusEnumList; overload;
Function TFhirConceptMapEquivalenceEnumListAsInteger(aSet : TFhirConceptMapEquivalenceEnumList) : Integer; overload;
Function IntegerAsTFhirConceptMapEquivalenceEnumList(i : integer) : TFhirConceptMapEquivalenceEnumList; overload;
Function TFhirConditionVerStatusEnumListAsInteger(aSet : TFhirConditionVerStatusEnumList) : Integer; overload;
Function IntegerAsTFhirConditionVerStatusEnumList(i : integer) : TFhirConditionVerStatusEnumList; overload;
Function TFhirConformanceStatementKindEnumListAsInteger(aSet : TFhirConformanceStatementKindEnumList) : Integer; overload;
Function IntegerAsTFhirConformanceStatementKindEnumList(i : integer) : TFhirConformanceStatementKindEnumList; overload;
Function TFhirUnknownContentCodeEnumListAsInteger(aSet : TFhirUnknownContentCodeEnumList) : Integer; overload;
Function IntegerAsTFhirUnknownContentCodeEnumList(i : integer) : TFhirUnknownContentCodeEnumList; overload;
Function TFhirRestfulConformanceModeEnumListAsInteger(aSet : TFhirRestfulConformanceModeEnumList) : Integer; overload;
Function IntegerAsTFhirRestfulConformanceModeEnumList(i : integer) : TFhirRestfulConformanceModeEnumList; overload;
Function TFhirResourceTypesEnumListAsInteger(aSet : TFhirResourceTypesEnumList) : Integer; overload;
Function IntegerAsTFhirResourceTypesEnumList(i : integer) : TFhirResourceTypesEnumList; overload;
Function TFhirTypeRestfulInteractionEnumListAsInteger(aSet : TFhirTypeRestfulInteractionEnumList) : Integer; overload;
Function IntegerAsTFhirTypeRestfulInteractionEnumList(i : integer) : TFhirTypeRestfulInteractionEnumList; overload;
Function TFhirVersioningPolicyEnumListAsInteger(aSet : TFhirVersioningPolicyEnumList) : Integer; overload;
Function IntegerAsTFhirVersioningPolicyEnumList(i : integer) : TFhirVersioningPolicyEnumList; overload;
Function TFhirConditionalDeleteStatusEnumListAsInteger(aSet : TFhirConditionalDeleteStatusEnumList) : Integer; overload;
Function IntegerAsTFhirConditionalDeleteStatusEnumList(i : integer) : TFhirConditionalDeleteStatusEnumList; overload;
Function TFhirSearchParamTypeEnumListAsInteger(aSet : TFhirSearchParamTypeEnumList) : Integer; overload;
Function IntegerAsTFhirSearchParamTypeEnumList(i : integer) : TFhirSearchParamTypeEnumList; overload;
Function TFhirSearchModifierCodeEnumListAsInteger(aSet : TFhirSearchModifierCodeEnumList) : Integer; overload;
Function IntegerAsTFhirSearchModifierCodeEnumList(i : integer) : TFhirSearchModifierCodeEnumList; overload;
Function TFhirSystemRestfulInteractionEnumListAsInteger(aSet : TFhirSystemRestfulInteractionEnumList) : Integer; overload;
Function IntegerAsTFhirSystemRestfulInteractionEnumList(i : integer) : TFhirSystemRestfulInteractionEnumList; overload;
Function TFhirTransactionModeEnumListAsInteger(aSet : TFhirTransactionModeEnumList) : Integer; overload;
Function IntegerAsTFhirTransactionModeEnumList(i : integer) : TFhirTransactionModeEnumList; overload;
Function TFhirMessageSignificanceCategoryEnumListAsInteger(aSet : TFhirMessageSignificanceCategoryEnumList) : Integer; overload;
Function IntegerAsTFhirMessageSignificanceCategoryEnumList(i : integer) : TFhirMessageSignificanceCategoryEnumList; overload;
Function TFhirMessageConformanceEventModeEnumListAsInteger(aSet : TFhirMessageConformanceEventModeEnumList) : Integer; overload;
Function IntegerAsTFhirMessageConformanceEventModeEnumList(i : integer) : TFhirMessageConformanceEventModeEnumList; overload;
Function TFhirDocumentModeEnumListAsInteger(aSet : TFhirDocumentModeEnumList) : Integer; overload;
Function IntegerAsTFhirDocumentModeEnumList(i : integer) : TFhirDocumentModeEnumList; overload;
Function TFhirDataelementStringencyEnumListAsInteger(aSet : TFhirDataelementStringencyEnumList) : Integer; overload;
Function IntegerAsTFhirDataelementStringencyEnumList(i : integer) : TFhirDataelementStringencyEnumList; overload;
Function TFhirDetectedissueSeverityEnumListAsInteger(aSet : TFhirDetectedissueSeverityEnumList) : Integer; overload;
Function IntegerAsTFhirDetectedissueSeverityEnumList(i : integer) : TFhirDetectedissueSeverityEnumList; overload;
Function TFhirDevicestatusEnumListAsInteger(aSet : TFhirDevicestatusEnumList) : Integer; overload;
Function IntegerAsTFhirDevicestatusEnumList(i : integer) : TFhirDevicestatusEnumList; overload;
Function TFhirMeasurementPrincipleEnumListAsInteger(aSet : TFhirMeasurementPrincipleEnumList) : Integer; overload;
Function IntegerAsTFhirMeasurementPrincipleEnumList(i : integer) : TFhirMeasurementPrincipleEnumList; overload;
Function TFhirMetricOperationalStatusEnumListAsInteger(aSet : TFhirMetricOperationalStatusEnumList) : Integer; overload;
Function IntegerAsTFhirMetricOperationalStatusEnumList(i : integer) : TFhirMetricOperationalStatusEnumList; overload;
Function TFhirMetricColorEnumListAsInteger(aSet : TFhirMetricColorEnumList) : Integer; overload;
Function IntegerAsTFhirMetricColorEnumList(i : integer) : TFhirMetricColorEnumList; overload;
Function TFhirMetricCategoryEnumListAsInteger(aSet : TFhirMetricCategoryEnumList) : Integer; overload;
Function IntegerAsTFhirMetricCategoryEnumList(i : integer) : TFhirMetricCategoryEnumList; overload;
Function TFhirMetricCalibrationTypeEnumListAsInteger(aSet : TFhirMetricCalibrationTypeEnumList) : Integer; overload;
Function IntegerAsTFhirMetricCalibrationTypeEnumList(i : integer) : TFhirMetricCalibrationTypeEnumList; overload;
Function TFhirMetricCalibrationStateEnumListAsInteger(aSet : TFhirMetricCalibrationStateEnumList) : Integer; overload;
Function IntegerAsTFhirMetricCalibrationStateEnumList(i : integer) : TFhirMetricCalibrationStateEnumList; overload;
Function TFhirDeviceUseRequestStatusEnumListAsInteger(aSet : TFhirDeviceUseRequestStatusEnumList) : Integer; overload;
Function IntegerAsTFhirDeviceUseRequestStatusEnumList(i : integer) : TFhirDeviceUseRequestStatusEnumList; overload;
Function TFhirDeviceUseRequestPriorityEnumListAsInteger(aSet : TFhirDeviceUseRequestPriorityEnumList) : Integer; overload;
Function IntegerAsTFhirDeviceUseRequestPriorityEnumList(i : integer) : TFhirDeviceUseRequestPriorityEnumList; overload;
Function TFhirDiagnosticOrderStatusEnumListAsInteger(aSet : TFhirDiagnosticOrderStatusEnumList) : Integer; overload;
Function IntegerAsTFhirDiagnosticOrderStatusEnumList(i : integer) : TFhirDiagnosticOrderStatusEnumList; overload;
Function TFhirDiagnosticOrderPriorityEnumListAsInteger(aSet : TFhirDiagnosticOrderPriorityEnumList) : Integer; overload;
Function IntegerAsTFhirDiagnosticOrderPriorityEnumList(i : integer) : TFhirDiagnosticOrderPriorityEnumList; overload;
Function TFhirDiagnosticReportStatusEnumListAsInteger(aSet : TFhirDiagnosticReportStatusEnumList) : Integer; overload;
Function IntegerAsTFhirDiagnosticReportStatusEnumList(i : integer) : TFhirDiagnosticReportStatusEnumList; overload;
Function TFhirDocumentReferenceStatusEnumListAsInteger(aSet : TFhirDocumentReferenceStatusEnumList) : Integer; overload;
Function IntegerAsTFhirDocumentReferenceStatusEnumList(i : integer) : TFhirDocumentReferenceStatusEnumList; overload;
Function TFhirDocumentRelationshipTypeEnumListAsInteger(aSet : TFhirDocumentRelationshipTypeEnumList) : Integer; overload;
Function IntegerAsTFhirDocumentRelationshipTypeEnumList(i : integer) : TFhirDocumentRelationshipTypeEnumList; overload;
Function TFhirEncounterStateEnumListAsInteger(aSet : TFhirEncounterStateEnumList) : Integer; overload;
Function IntegerAsTFhirEncounterStateEnumList(i : integer) : TFhirEncounterStateEnumList; overload;
Function TFhirEncounterClassEnumListAsInteger(aSet : TFhirEncounterClassEnumList) : Integer; overload;
Function IntegerAsTFhirEncounterClassEnumList(i : integer) : TFhirEncounterClassEnumList; overload;
Function TFhirEncounterLocationStatusEnumListAsInteger(aSet : TFhirEncounterLocationStatusEnumList) : Integer; overload;
Function IntegerAsTFhirEncounterLocationStatusEnumList(i : integer) : TFhirEncounterLocationStatusEnumList; overload;
Function TFhirEpisodeOfCareStatusEnumListAsInteger(aSet : TFhirEpisodeOfCareStatusEnumList) : Integer; overload;
Function IntegerAsTFhirEpisodeOfCareStatusEnumList(i : integer) : TFhirEpisodeOfCareStatusEnumList; overload;
Function TFhirHistoryStatusEnumListAsInteger(aSet : TFhirHistoryStatusEnumList) : Integer; overload;
Function IntegerAsTFhirHistoryStatusEnumList(i : integer) : TFhirHistoryStatusEnumList; overload;
Function TFhirAdministrativeGenderEnumListAsInteger(aSet : TFhirAdministrativeGenderEnumList) : Integer; overload;
Function IntegerAsTFhirAdministrativeGenderEnumList(i : integer) : TFhirAdministrativeGenderEnumList; overload;
Function TFhirFlagStatusEnumListAsInteger(aSet : TFhirFlagStatusEnumList) : Integer; overload;
Function IntegerAsTFhirFlagStatusEnumList(i : integer) : TFhirFlagStatusEnumList; overload;
Function TFhirGoalStatusEnumListAsInteger(aSet : TFhirGoalStatusEnumList) : Integer; overload;
Function IntegerAsTFhirGoalStatusEnumList(i : integer) : TFhirGoalStatusEnumList; overload;
Function TFhirGroupTypeEnumListAsInteger(aSet : TFhirGroupTypeEnumList) : Integer; overload;
Function IntegerAsTFhirGroupTypeEnumList(i : integer) : TFhirGroupTypeEnumList; overload;
Function TFhirDaysOfWeekEnumListAsInteger(aSet : TFhirDaysOfWeekEnumList) : Integer; overload;
Function IntegerAsTFhirDaysOfWeekEnumList(i : integer) : TFhirDaysOfWeekEnumList; overload;
Function TFhirInstanceAvailabilityEnumListAsInteger(aSet : TFhirInstanceAvailabilityEnumList) : Integer; overload;
Function IntegerAsTFhirInstanceAvailabilityEnumList(i : integer) : TFhirInstanceAvailabilityEnumList; overload;
Function TFhirMedicationAdminStatusEnumListAsInteger(aSet : TFhirMedicationAdminStatusEnumList) : Integer; overload;
Function IntegerAsTFhirMedicationAdminStatusEnumList(i : integer) : TFhirMedicationAdminStatusEnumList; overload;
Function TFhirGuideDependencyTypeEnumListAsInteger(aSet : TFhirGuideDependencyTypeEnumList) : Integer; overload;
Function IntegerAsTFhirGuideDependencyTypeEnumList(i : integer) : TFhirGuideDependencyTypeEnumList; overload;
Function TFhirGuideResourcePurposeEnumListAsInteger(aSet : TFhirGuideResourcePurposeEnumList) : Integer; overload;
Function IntegerAsTFhirGuideResourcePurposeEnumList(i : integer) : TFhirGuideResourcePurposeEnumList; overload;
Function TFhirGuidePageKindEnumListAsInteger(aSet : TFhirGuidePageKindEnumList) : Integer; overload;
Function IntegerAsTFhirGuidePageKindEnumList(i : integer) : TFhirGuidePageKindEnumList; overload;
Function TFhirListStatusEnumListAsInteger(aSet : TFhirListStatusEnumList) : Integer; overload;
Function IntegerAsTFhirListStatusEnumList(i : integer) : TFhirListStatusEnumList; overload;
Function TFhirLocationStatusEnumListAsInteger(aSet : TFhirLocationStatusEnumList) : Integer; overload;
Function IntegerAsTFhirLocationStatusEnumList(i : integer) : TFhirLocationStatusEnumList; overload;
Function TFhirLocationModeEnumListAsInteger(aSet : TFhirLocationModeEnumList) : Integer; overload;
Function IntegerAsTFhirLocationModeEnumList(i : integer) : TFhirLocationModeEnumList; overload;
Function TFhirDigitalMediaTypeEnumListAsInteger(aSet : TFhirDigitalMediaTypeEnumList) : Integer; overload;
Function IntegerAsTFhirDigitalMediaTypeEnumList(i : integer) : TFhirDigitalMediaTypeEnumList; overload;
Function TFhirMedicationDispenseStatusEnumListAsInteger(aSet : TFhirMedicationDispenseStatusEnumList) : Integer; overload;
Function IntegerAsTFhirMedicationDispenseStatusEnumList(i : integer) : TFhirMedicationDispenseStatusEnumList; overload;
Function TFhirMedicationOrderStatusEnumListAsInteger(aSet : TFhirMedicationOrderStatusEnumList) : Integer; overload;
Function IntegerAsTFhirMedicationOrderStatusEnumList(i : integer) : TFhirMedicationOrderStatusEnumList; overload;
Function TFhirMedicationStatementStatusEnumListAsInteger(aSet : TFhirMedicationStatementStatusEnumList) : Integer; overload;
Function IntegerAsTFhirMedicationStatementStatusEnumList(i : integer) : TFhirMedicationStatementStatusEnumList; overload;
Function TFhirResponseCodeEnumListAsInteger(aSet : TFhirResponseCodeEnumList) : Integer; overload;
Function IntegerAsTFhirResponseCodeEnumList(i : integer) : TFhirResponseCodeEnumList; overload;
Function TFhirNamingsystemTypeEnumListAsInteger(aSet : TFhirNamingsystemTypeEnumList) : Integer; overload;
Function IntegerAsTFhirNamingsystemTypeEnumList(i : integer) : TFhirNamingsystemTypeEnumList; overload;
Function TFhirNamingsystemIdentifierTypeEnumListAsInteger(aSet : TFhirNamingsystemIdentifierTypeEnumList) : Integer; overload;
Function IntegerAsTFhirNamingsystemIdentifierTypeEnumList(i : integer) : TFhirNamingsystemIdentifierTypeEnumList; overload;
Function TFhirNutritionOrderStatusEnumListAsInteger(aSet : TFhirNutritionOrderStatusEnumList) : Integer; overload;
Function IntegerAsTFhirNutritionOrderStatusEnumList(i : integer) : TFhirNutritionOrderStatusEnumList; overload;
Function TFhirObservationStatusEnumListAsInteger(aSet : TFhirObservationStatusEnumList) : Integer; overload;
Function IntegerAsTFhirObservationStatusEnumList(i : integer) : TFhirObservationStatusEnumList; overload;
Function TFhirObservationRelationshiptypesEnumListAsInteger(aSet : TFhirObservationRelationshiptypesEnumList) : Integer; overload;
Function IntegerAsTFhirObservationRelationshiptypesEnumList(i : integer) : TFhirObservationRelationshiptypesEnumList; overload;
Function TFhirOperationKindEnumListAsInteger(aSet : TFhirOperationKindEnumList) : Integer; overload;
Function IntegerAsTFhirOperationKindEnumList(i : integer) : TFhirOperationKindEnumList; overload;
Function TFhirOperationParameterUseEnumListAsInteger(aSet : TFhirOperationParameterUseEnumList) : Integer; overload;
Function IntegerAsTFhirOperationParameterUseEnumList(i : integer) : TFhirOperationParameterUseEnumList; overload;
Function TFhirOperationParameterTypeEnumListAsInteger(aSet : TFhirOperationParameterTypeEnumList) : Integer; overload;
Function IntegerAsTFhirOperationParameterTypeEnumList(i : integer) : TFhirOperationParameterTypeEnumList; overload;
Function TFhirIssueSeverityEnumListAsInteger(aSet : TFhirIssueSeverityEnumList) : Integer; overload;
Function IntegerAsTFhirIssueSeverityEnumList(i : integer) : TFhirIssueSeverityEnumList; overload;
Function TFhirIssueTypeEnumListAsInteger(aSet : TFhirIssueTypeEnumList) : Integer; overload;
Function IntegerAsTFhirIssueTypeEnumList(i : integer) : TFhirIssueTypeEnumList; overload;
Function TFhirOrderStatusEnumListAsInteger(aSet : TFhirOrderStatusEnumList) : Integer; overload;
Function IntegerAsTFhirOrderStatusEnumList(i : integer) : TFhirOrderStatusEnumList; overload;
Function TFhirLinkTypeEnumListAsInteger(aSet : TFhirLinkTypeEnumList) : Integer; overload;
Function IntegerAsTFhirLinkTypeEnumList(i : integer) : TFhirLinkTypeEnumList; overload;
Function TFhirIdentityAssuranceLevelEnumListAsInteger(aSet : TFhirIdentityAssuranceLevelEnumList) : Integer; overload;
Function IntegerAsTFhirIdentityAssuranceLevelEnumList(i : integer) : TFhirIdentityAssuranceLevelEnumList; overload;
Function TFhirProcedureStatusEnumListAsInteger(aSet : TFhirProcedureStatusEnumList) : Integer; overload;
Function IntegerAsTFhirProcedureStatusEnumList(i : integer) : TFhirProcedureStatusEnumList; overload;
Function TFhirProcedureRequestStatusEnumListAsInteger(aSet : TFhirProcedureRequestStatusEnumList) : Integer; overload;
Function IntegerAsTFhirProcedureRequestStatusEnumList(i : integer) : TFhirProcedureRequestStatusEnumList; overload;
Function TFhirProcedureRequestPriorityEnumListAsInteger(aSet : TFhirProcedureRequestPriorityEnumList) : Integer; overload;
Function IntegerAsTFhirProcedureRequestPriorityEnumList(i : integer) : TFhirProcedureRequestPriorityEnumList; overload;
Function TFhirActionlistEnumListAsInteger(aSet : TFhirActionlistEnumList) : Integer; overload;
Function IntegerAsTFhirActionlistEnumList(i : integer) : TFhirActionlistEnumList; overload;
Function TFhirProvenanceEntityRoleEnumListAsInteger(aSet : TFhirProvenanceEntityRoleEnumList) : Integer; overload;
Function IntegerAsTFhirProvenanceEntityRoleEnumList(i : integer) : TFhirProvenanceEntityRoleEnumList; overload;
Function TFhirQuestionnaireStatusEnumListAsInteger(aSet : TFhirQuestionnaireStatusEnumList) : Integer; overload;
Function IntegerAsTFhirQuestionnaireStatusEnumList(i : integer) : TFhirQuestionnaireStatusEnumList; overload;
Function TFhirAnswerFormatEnumListAsInteger(aSet : TFhirAnswerFormatEnumList) : Integer; overload;
Function IntegerAsTFhirAnswerFormatEnumList(i : integer) : TFhirAnswerFormatEnumList; overload;
Function TFhirQuestionnaireAnswersStatusEnumListAsInteger(aSet : TFhirQuestionnaireAnswersStatusEnumList) : Integer; overload;
Function IntegerAsTFhirQuestionnaireAnswersStatusEnumList(i : integer) : TFhirQuestionnaireAnswersStatusEnumList; overload;
Function TFhirReferralstatusEnumListAsInteger(aSet : TFhirReferralstatusEnumList) : Integer; overload;
Function IntegerAsTFhirReferralstatusEnumList(i : integer) : TFhirReferralstatusEnumList; overload;
Function TFhirSearchXpathUsageEnumListAsInteger(aSet : TFhirSearchXpathUsageEnumList) : Integer; overload;
Function IntegerAsTFhirSearchXpathUsageEnumList(i : integer) : TFhirSearchXpathUsageEnumList; overload;
Function TFhirSlotstatusEnumListAsInteger(aSet : TFhirSlotstatusEnumList) : Integer; overload;
Function IntegerAsTFhirSlotstatusEnumList(i : integer) : TFhirSlotstatusEnumList; overload;
Function TFhirSpecimenStatusEnumListAsInteger(aSet : TFhirSpecimenStatusEnumList) : Integer; overload;
Function IntegerAsTFhirSpecimenStatusEnumList(i : integer) : TFhirSpecimenStatusEnumList; overload;
Function TFhirStructureDefinitionKindEnumListAsInteger(aSet : TFhirStructureDefinitionKindEnumList) : Integer; overload;
Function IntegerAsTFhirStructureDefinitionKindEnumList(i : integer) : TFhirStructureDefinitionKindEnumList; overload;
Function TFhirExtensionContextEnumListAsInteger(aSet : TFhirExtensionContextEnumList) : Integer; overload;
Function IntegerAsTFhirExtensionContextEnumList(i : integer) : TFhirExtensionContextEnumList; overload;
Function TFhirSubscriptionStatusEnumListAsInteger(aSet : TFhirSubscriptionStatusEnumList) : Integer; overload;
Function IntegerAsTFhirSubscriptionStatusEnumList(i : integer) : TFhirSubscriptionStatusEnumList; overload;
Function TFhirSubscriptionChannelTypeEnumListAsInteger(aSet : TFhirSubscriptionChannelTypeEnumList) : Integer; overload;
Function IntegerAsTFhirSubscriptionChannelTypeEnumList(i : integer) : TFhirSubscriptionChannelTypeEnumList; overload;
Function TFhirSupplydeliveryStatusEnumListAsInteger(aSet : TFhirSupplydeliveryStatusEnumList) : Integer; overload;
Function IntegerAsTFhirSupplydeliveryStatusEnumList(i : integer) : TFhirSupplydeliveryStatusEnumList; overload;
Function TFhirSupplyrequestStatusEnumListAsInteger(aSet : TFhirSupplyrequestStatusEnumList) : Integer; overload;
Function IntegerAsTFhirSupplyrequestStatusEnumList(i : integer) : TFhirSupplyrequestStatusEnumList; overload;
Function TFhirContentTypeEnumListAsInteger(aSet : TFhirContentTypeEnumList) : Integer; overload;
Function IntegerAsTFhirContentTypeEnumList(i : integer) : TFhirContentTypeEnumList; overload;
Function TFhirAssertDirectionCodesEnumListAsInteger(aSet : TFhirAssertDirectionCodesEnumList) : Integer; overload;
Function IntegerAsTFhirAssertDirectionCodesEnumList(i : integer) : TFhirAssertDirectionCodesEnumList; overload;
Function TFhirAssertOperatorCodesEnumListAsInteger(aSet : TFhirAssertOperatorCodesEnumList) : Integer; overload;
Function IntegerAsTFhirAssertOperatorCodesEnumList(i : integer) : TFhirAssertOperatorCodesEnumList; overload;
Function TFhirAssertResponseCodeTypesEnumListAsInteger(aSet : TFhirAssertResponseCodeTypesEnumList) : Integer; overload;
Function IntegerAsTFhirAssertResponseCodeTypesEnumList(i : integer) : TFhirAssertResponseCodeTypesEnumList; overload;
Function TFhirFilterOperatorEnumListAsInteger(aSet : TFhirFilterOperatorEnumList) : Integer; overload;
Function IntegerAsTFhirFilterOperatorEnumList(i : integer) : TFhirFilterOperatorEnumList; overload;
Function TFhirVisionEyeCodesEnumListAsInteger(aSet : TFhirVisionEyeCodesEnumList) : Integer; overload;
Function IntegerAsTFhirVisionEyeCodesEnumList(i : integer) : TFhirVisionEyeCodesEnumList; overload;
Function TFhirVisionBaseCodesEnumListAsInteger(aSet : TFhirVisionBaseCodesEnumList) : Integer; overload;
Function IntegerAsTFhirVisionBaseCodesEnumList(i : integer) : TFhirVisionBaseCodesEnumList; overload;

function asEnum(systems, values: array of String; obj : TFHIRObject) : TFhirEnum;
function asDateTime(obj : TFHIRObject) : TFhirDateTime;
function asDate(obj : TFHIRObject) : TFhirDate;
function asString(obj : TFHIRObject) : TFhirString;
function asInteger(obj : TFHIRObject) : TFhirInteger;
function asUri(obj : TFHIRObject) : TFhirUri;
function asInstant(obj : TFHIRObject) : TFhirInstant;
function asBoolean(obj : TFHIRObject) : TFhirBoolean;
function asBase64Binary(obj : TFHIRObject) : TFhirBase64Binary;
function asTime(obj : TFHIRObject) : TFhirTime;
function asDecimal(obj : TFHIRObject) : TFhirDecimal;
function asCode(obj : TFHIRObject) : TFhirCode;
function asOid(obj : TFHIRObject) : TFhirOid;
function asUuid(obj : TFHIRObject) : TFhirUuid;
function asMarkdown(obj : TFHIRObject) : TFhirMarkdown;
function asUnsignedInt(obj : TFHIRObject) : TFhirUnsignedInt;
function asId(obj : TFHIRObject) : TFhirId;
function asPositiveInt(obj : TFHIRObject) : TFhirPositiveInt;

implementation

uses
  fhir2_elementmodel, fhir2_utilities;

{ TFhirElement }

constructor TFhirElement.Create;
begin
  inherited;
end;

destructor TFhirElement.Destroy;
begin
  FId.free;
  FExtensionList.Free;
  inherited;
end;

function TFhirElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FId.sizeInBytes);
  inc(result, FextensionList.sizeInBytes);
end;

function TFHIRElement.getId : string;
begin
  result := id;
end;

procedure TFhirElement.setIdValue(id: String);
begin
  SetIdSt(id);
end;

function TFhirElement.noExtensions: TFhirElement;
begin
  DisallowExtensions := true;
  result := self;
end;

procedure TFhirElement.addExtension(url: String; value: TFHIRObject);
var
  ex : TFhirExtension;
begin
  ex := extensionList.Append;
  ex.url := url;
  ex.value := value as TFhirType;
end;

function TFhirElement.extensionCount(url: String): integer;
var
  ex : TFhirExtension;
begin
  result := 0;
  for ex in ExtensionList do
    if ex.url = url then
      inc(result);
end;
      
function TFhirElement.extensions(url: String): TFslList<TFHIRObject>;
var
  ex : TFhirExtension;
begin
  result := TFslList<TFHIRObject>.create;
  try
    for ex in ExtensionList do
      if ex.url = url then
        result.Add(ex.Link);
    result.link;
  finally
    result.Free;
  end;
end;

function TFhirElement.hasExtension(url: string): boolean;
var
  ex : TFhirExtension;
begin
  result := false;
  for ex in ExtensionList do
    if ex.url = url then
      exit(true);
end;
      
function TFhirElement.hasExtensions: boolean;
begin
  result := FExtensionList.Count > 0;
end;

function TFhirElement.getExtensionString(url: String): String;
var
  ex : TFhirExtension;
begin
  result := '';
  for ex in ExtensionList do
  begin
    if ex.url = url then
    begin
      if not ex.value.isPrimitive then
        raise EFHIRException.create('Complex extension '+url)
      else if result <> '' then
        raise EFHIRException.create('Duplicate extension '+url)
      else
        result := ex.value.primitiveValue;
    end;
  end;
end;

procedure TFhirElement.Assign(oSource : TFslObject);
begin
  inherited;
  idElement := TFhirElement(oSource).idElement.Clone;
  if (TFhirElement(oSource).FExtensionList = nil) then
  begin
    FExtensionList.free;
    FExtensionList := nil;
  end
  else
  begin
    if FExtensionList = nil then
      FExtensionList := TFhirExtensionList.Create;
    FExtensionList.Assign(TFhirElement(oSource).FExtensionList);
  end;
end;

procedure TFhirElement.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'id') Then
     list.add(self.link, 'id', FId.Link);
  if (child_name = 'extension') Then
    list.addAll(self, 'extension', FExtensionList);
end;

procedure TFhirElement.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'id', 'id', false, TFhirId, FId.Link));{2}
  oList.add(TFHIRProperty.create(self, 'extension', 'Extension', true, TFhirExtension, FExtensionList.Link)){3};
end;

function TFhirElement.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'id') then
  begin
    IdElement := asId(propValue){5a};
    result := propValue;
  end
  else if (propName = 'extension') then
  begin
    ExtensionList.add(propValue as TFhirExtension){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirElement.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'extension') then ExtensionList.insertItem(index, propValue as TFhirExtension){2a}
  else inherited;
end;

function TFhirElement.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'id') then result := TFhirId.create() {5b}
  else if (propName = 'extension') then result := ExtensionList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirElement.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'id') then result := 'id'
  else if (propName = 'extension') then result := 'Extension'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirElement.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'id') then IdElement := nil
  else if (propName = 'extension') then deletePropertyValue('extension', ExtensionList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirElement.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'id') then IdElement := asId(new){5b}
  else if (propName = 'extension') then replacePropertyValue('extension', ExtensionList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirElement.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'extension') then ExtensionList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirElement.fhirType : string;
begin
  result := 'Element';
end;

function TFhirElement.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FId) and isEmptyProp(FextensionList);
end;

function TFhirElement.equals(other : TObject) : boolean; 
var
  o : TFhirElement;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirElement)) then
    result := false
  else
  begin
    o := TFhirElement(other);
    result := compareDeep(idElement, o.idElement, true) and compareDeep(extensionList, o.extensionList, true);
  end;
end;

function TFhirElement.Link : TFhirElement;
begin
  result := TFhirElement(inherited Link);
end;

procedure TFhirElement.listElementFieldsInOrder(fields: TStringList);
begin
  fields.add('@id');
  fields.add('extension');
end;

procedure TFhirElement.listFieldsInOrder(fields: TStringList);
begin
  fields.add('id');
  fields.add('extension');
end;

function TFhirElement.Clone : TFhirElement;
begin
  result := TFhirElement(inherited Clone);
end;

{ TFhirElement }

Procedure TFhirElement.SetId(value : TFhirId);
begin
  FId.free;
  FId := value;
end;

Function TFhirElement.GetIdST : String;
begin
  if FId = nil then
    result := ''
  else
    result := FId.value;
end;

Procedure TFhirElement.SetIdST(value : String);
begin
  if value <> '' then
  begin
    if FId = nil then
      FId := TFhirId.create;
    FId.value := value
  end
  else if FId <> nil then
    FId.value := '';
end;

Function TFhirElement.GetExtensionList : TFhirExtensionList;
begin
  if FExtensionList = nil then
    FExtensionList := TFhirExtensionList.Create;
  result := FExtensionList;
end;

Function TFhirElement.GetHasExtensionList : boolean;
begin
  result := (FExtensionList <> nil) and (FExtensionList.count > 0);
end;

{ TFhirElementListEnumerator }

Constructor TFhirElementListEnumerator.Create(list : TFhirElementList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirElementListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirElementListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirElementListEnumerator.GetCurrent : TFhirElement;
begin
  Result := FList[FIndex];
end;

function TFhirElementListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirElementList }
procedure TFhirElementList.AddItem(value: TFhirElement);
begin
  assert(value.ClassName = 'TFhirElement', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirElement');
  add(value);
end;

function TFhirElementList.Append: TFhirElement;
begin
  result := TFhirElement.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementList.ClearItems;
begin
  Clear;
end;

function TFhirElementList.GetEnumerator : TFhirElementListEnumerator;
begin
  result := TFhirElementListEnumerator.Create(self.link);
end;

function TFhirElementList.Clone: TFhirElementList;
begin
  result := TFhirElementList(inherited Clone);
end;

function TFhirElementList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirElementList.GetItemN(index: Integer): TFhirElement;
begin
  result := TFhirElement(ObjectByIndex[index]);
end;

function TFhirElementList.ItemClass: TFslObjectClass;
begin
  result := TFhirElement;
end;
function TFhirElementList.IndexOf(value: TFhirElement): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirElementList.Insert(index: Integer): TFhirElement;
begin
  result := TFhirElement.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementList.InsertItem(index: Integer; value: TFhirElement);
begin
  assert(value is TFhirElement);
  Inherited Insert(index, value);
end;

function TFhirElementList.Item(index: Integer): TFhirElement;
begin
  result := TFhirElement(ObjectByIndex[index]);
end;

function TFhirElementList.Link: TFhirElementList;
begin
  result := TFhirElementList(inherited Link);
end;

procedure TFhirElementList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirElementList.SetItemByIndex(index: Integer; value: TFhirElement);
begin
  assert(value is TFhirElement);
  FhirElements[index] := value;
end;

procedure TFhirElementList.SetItemN(index: Integer; value: TFhirElement);
begin
  assert(value is TFhirElement);
  ObjectByIndex[index] := value;
end;

{ TFhirType }

function TFhirType.Link : TFhirType;
begin
  result := TFhirType(inherited Link);
end;

function TFhirType.ToString : String;
begin
  result := gen(self);
end;

function TFhirType.isType : boolean;
begin
  result := true;
end;

function TFhirType.Clone : TFhirType;
begin
  result := TFhirType(inherited Clone);
end;

{ TFHIRPrimitiveType }

function TFHIRPrimitiveType.Link : TFHIRPrimitiveType;
begin
  result := TFHIRPrimitiveType(inherited Link);
end;

function TFHIRPrimitiveType.Clone : TFHIRPrimitiveType;
begin
  result := TFHIRPrimitiveType(inherited Clone);
end;

function TFHIRPrimitiveType.GetStringValue : string;
begin
  if self = nil then
    result := ''
  else
    result := AsStringValue;
end;

function TFHIRPrimitiveType.isPrimitive: boolean;
begin
  result := true;
end;

function TFHIRPrimitiveType.hasPrimitiveValue: boolean;
begin
  result := StringValue <> '';
end;

function TFHIRPrimitiveType.primitiveValue: string;
begin
  result := StringValue;
end;

function TFHIRPrimitiveType.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'value') then
  begin
    StringValue := propValue.primitiveValue;
    propValue.Free;
    result := self;
  end
  else
    result := inherited setProperty(propName, propValue);
end;

function TFHIRPrimitiveType.toString : String;
begin
  result := StringValue;
end;

{ TFhirBackboneElement }

constructor TFhirBackboneElement.Create;
begin
  inherited;
end;

destructor TFhirBackboneElement.Destroy;
begin
  FModifierExtensionList.Free;
  inherited;
end;

procedure TFhirBackboneElement.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirBackboneElement(oSource).FModifierExtensionList = nil) then
  begin
    FModifierExtensionList.free;
    FModifierExtensionList := nil;
  end
  else
  begin
    if FModifierExtensionList = nil then
      FModifierExtensionList := TFhirExtensionList.Create;
    FModifierExtensionList.Assign(TFhirBackboneElement(oSource).FModifierExtensionList);
  end;
end;

procedure TFhirBackboneElement.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'modifierExtension') Then
    list.addAll(self, 'modifierExtension', FModifierExtensionList);
end;

procedure TFhirBackboneElement.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'modifierExtension', 'Extension', true, TFhirExtension, FModifierExtensionList.Link)){3};
end;

function TFhirBackboneElement.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'modifierExtension') then
  begin
    ModifierExtensionList.add(propValue as TFhirExtension){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirBackboneElement.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'modifierExtension') then ModifierExtensionList.insertItem(index, propValue as TFhirExtension){2a}
  else inherited;
end;

function TFhirBackboneElement.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'modifierExtension') then result := ModifierExtensionList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirBackboneElement.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'modifierExtension') then result := 'Extension'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirBackboneElement.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'modifierExtension') then deletePropertyValue('modifierExtension', ModifierExtensionList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirBackboneElement.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'modifierExtension') then replacePropertyValue('modifierExtension', ModifierExtensionList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirBackboneElement.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'modifierExtension') then ModifierExtensionList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirBackboneElement.fhirType : string;
begin
  result := 'BackboneElement';
end;

function TFhirBackboneElement.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FmodifierExtensionList);
end;

function TFhirBackboneElement.equals(other : TObject) : boolean; 
var
  o : TFhirBackboneElement;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirBackboneElement)) then
    result := false
  else
  begin
    o := TFhirBackboneElement(other);
    result := compareDeep(modifierExtensionList, o.modifierExtensionList, true);
  end;
end;

function TFhirBackboneElement.Link : TFhirBackboneElement;
begin
  result := TFhirBackboneElement(inherited Link);
end;

procedure TFhirBackboneElement.listBackboneElementFieldsInOrder(fields: TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('modifierExtension');
end;

procedure TFhirBackboneElement.listFieldsInOrder(fields: TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('modifierExtension');
end;

function TFhirBackboneElement.Clone : TFhirBackboneElement;
begin
  result := TFhirBackboneElement(inherited Clone);
end;

{ TFhirBackboneElement }

Function TFhirBackboneElement.GetModifierExtensionList : TFhirExtensionList;
begin
  if FModifierExtensionList = nil then
    FModifierExtensionList := TFhirExtensionList.Create;
  result := FModifierExtensionList;
end;

Function TFhirBackboneElement.GetHasModifierExtensionList : boolean;
begin
  result := (FModifierExtensionList <> nil) and (FModifierExtensionList.count > 0);
end;

function TFhirBackboneElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FmodifierExtensionList.sizeInBytes);
end;

{ TFhirBackboneElementListEnumerator }

Constructor TFhirBackboneElementListEnumerator.Create(list : TFhirBackboneElementList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirBackboneElementListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirBackboneElementListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirBackboneElementListEnumerator.GetCurrent : TFhirBackboneElement;
begin
  Result := FList[FIndex];
end;

function TFhirBackboneElementListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirBackboneElementList }
procedure TFhirBackboneElementList.AddItem(value: TFhirBackboneElement);
begin
  assert(value.ClassName = 'TFhirBackboneElement', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirBackboneElement');
  add(value);
end;

function TFhirBackboneElementList.Append: TFhirBackboneElement;
begin
  result := TFhirBackboneElement.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirBackboneElementList.ClearItems;
begin
  Clear;
end;

function TFhirBackboneElementList.GetEnumerator : TFhirBackboneElementListEnumerator;
begin
  result := TFhirBackboneElementListEnumerator.Create(self.link);
end;

function TFhirBackboneElementList.Clone: TFhirBackboneElementList;
begin
  result := TFhirBackboneElementList(inherited Clone);
end;

function TFhirBackboneElementList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirBackboneElementList.GetItemN(index: Integer): TFhirBackboneElement;
begin
  result := TFhirBackboneElement(ObjectByIndex[index]);
end;

function TFhirBackboneElementList.ItemClass: TFslObjectClass;
begin
  result := TFhirBackboneElement;
end;
function TFhirBackboneElementList.IndexOf(value: TFhirBackboneElement): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirBackboneElementList.Insert(index: Integer): TFhirBackboneElement;
begin
  result := TFhirBackboneElement.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirBackboneElementList.InsertItem(index: Integer; value: TFhirBackboneElement);
begin
  assert(value is TFhirBackboneElement);
  Inherited Insert(index, value);
end;

function TFhirBackboneElementList.Item(index: Integer): TFhirBackboneElement;
begin
  result := TFhirBackboneElement(ObjectByIndex[index]);
end;

function TFhirBackboneElementList.Link: TFhirBackboneElementList;
begin
  result := TFhirBackboneElementList(inherited Link);
end;

procedure TFhirBackboneElementList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirBackboneElementList.SetItemByIndex(index: Integer; value: TFhirBackboneElement);
begin
  assert(value is TFhirBackboneElement);
  FhirBackboneElements[index] := value;
end;

procedure TFhirBackboneElementList.SetItemN(index: Integer; value: TFhirBackboneElement);
begin
  assert(value is TFhirBackboneElement);
  ObjectByIndex[index] := value;
end;

{ TFhirEnum }

Constructor TFhirEnum.Create(system : String; value : String);
begin
  Create;
  FSystem := system;
  FValue := value;
end;

destructor TFhirEnum.Destroy;
begin
  inherited;
end;

function TFhirEnum.fhirType : string;
begin
  result := 'code';
end;

function TFhirEnum.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, (FSystem.length * sizeof(char)) + 12);
end;

function TFHIREnum.isEnum : boolean;
begin
  result := true;
end;

procedure TFhirEnum.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirEnum.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'enum', false, nil, FValue));
end;

procedure TFhirEnum.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirEnum(oSource).Value;
end;

function TFhirEnum.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirEnum.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirEnum.equals(other : TObject) : boolean; 
var
  o : TFhirEnum;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirEnum)) then
    result := false
  else
  begin
    o := TFhirEnum(other);
    result := o.value = value;
  end;
end;

function TFhirEnum.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirEnum.Link : TFhirEnum;
begin
  result := TFhirEnum(inherited Link);
end;

function TFhirEnum.Clone : TFhirEnum;
begin
  result := TFhirEnum(inherited Clone);
end;

procedure TFhirEnum.setValue(value : String);
begin
  FValue := value;
end;

procedure TFhirEnum.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

{ TFhirEnumListEnumerator }

Constructor TFhirEnumListEnumerator.Create(list : TFhirEnumList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirEnumListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEnumListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirEnumListEnumerator.GetCurrent : TFhirEnum;
begin
  Result := FList[FIndex];
end;

function TFhirEnumListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirEnumList }
procedure TFhirEnumList.AddItem(value: TFhirEnum);
begin
  assert(value.ClassName = 'TFhirEnum', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEnum');
  add(value);
end;

constructor TFhirEnumList.Create(Systems, Codes : Array Of String);
var
  i : integer;
begin
  inherited create;
  SetLength(FSystems, length(systems));
  SetLength(FCodes, length(codes));
  for i := 0 to length(systems) - 1 do
  begin
    FSystems[i] := systems[i];
    FCodes[i] := codes[i];
  end;
end;

procedure TFhirEnumList.AddItem(value: String);
begin
  add(TFhirEnum.create(FSystems[StringArrayIndexOf(FCodes, value)], value));
end;

function TFhirEnumList.Append: TFhirEnum;
begin
  result := TFhirEnum.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEnumList.ClearItems;
begin
  Clear;
end;

function TFhirEnumList.GetEnumerator : TFhirEnumListEnumerator;
begin
  result := TFhirEnumListEnumerator.Create(self.link);
end;

function TFhirEnumList.Clone: TFhirEnumList;
begin
  result := TFhirEnumList(inherited Clone);
end;

function TFhirEnumList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirEnumList.GetItemN(index: Integer): TFhirEnum;
begin
  result := TFhirEnum(ObjectByIndex[index]);
end;

function TFhirEnumList.ItemClass: TFslObjectClass;
begin
  result := TFhirEnum;
end;
function TFhirEnumList.IndexOf(value: TFhirEnum): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirEnumList.Insert(index: Integer): TFhirEnum;
begin
  result := TFhirEnum.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEnumList.InsertItem(index: Integer; value: TFhirEnum);
begin
  assert(value is TFhirEnum);
  Inherited Insert(index, value);
end;

function TFhirEnumList.Item(index: Integer): TFhirEnum;
begin
  result := TFhirEnum(ObjectByIndex[index]);
end;

function TFhirEnumList.Link: TFhirEnumList;
begin
  result := TFhirEnumList(inherited Link);
end;

procedure TFhirEnumList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirEnumList.SetItemByIndex(index: Integer; value: TFhirEnum);
begin
  assert(value is TFhirEnum);
  FhirEnums[index] := value;
end;

procedure TFhirEnumList.SetItemN(index: Integer; value: TFhirEnum);
begin
  assert(value is TFhirEnum);
  ObjectByIndex[index] := value;
end;

function TFhirEnumList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, length(FSystems) * sizeof(pointer));
  inc(result, length(FCodes) * sizeof(pointer));
end;

{ TFhirDateTime }

Constructor TFhirDateTime.Create(value : TFslDateTime);
begin
  Create;
  FValue := value;
end;

destructor TFhirDateTime.Destroy;
begin
  inherited;
end;

function TFhirDateTime.fhirType : string;
begin
  result := 'dateTime';
end;

function TFhirDateTime.getDateValue : TFslDateTime;
begin
  result := FValue;
end;

procedure TFhirDateTime.setDateValue(value: TFslDateTime);
begin
  FValue := value;
end;

procedure TFhirDateTime.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirDateTime.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    if (FValue.notNull) then
      oList.add(TFHIRProperty.create(self, 'value', 'dateTime', false, nil, FValue.ToString));
end;

procedure TFhirDateTime.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirDateTime(oSource).Value;
end;

function TFhirDateTime.AsStringValue : string;
begin
  if (FValue.null) then
    result := ''
  else
    result := FValue.toXml;
end;

procedure TFhirDateTime.SetStringValue(value : string);
begin
  if (value = '') then
    FValue := TFslDateTime.makeNull
  else
    FValue := TFslDateTime.fromXml(value);
end;

function TFhirDateTime.equals(other : TObject) : boolean; 
var
  o : TFhirDateTime;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDateTime)) then
    result := false
  else
  begin
    o := TFhirDateTime(other);
    result := o.value.equal(value);
  end;
end;

function TFhirDateTime.isDateTime: boolean;
begin
  result := true;
end;

function TFhirDateTime.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue.null);
end;

function TFhirDateTime.Link : TFhirDateTime;
begin
  result := TFhirDateTime(inherited Link);
end;

function TFhirDateTime.Clone : TFhirDateTime;
begin
  result := TFhirDateTime(inherited Clone);
end;

procedure TFhirDateTime.setValue(value : TFslDateTime);
begin
  FValue := value;
end;

procedure TFhirDateTime.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

function TFhirDateTime.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TFhirDateTimeListEnumerator }

Constructor TFhirDateTimeListEnumerator.Create(list : TFhirDateTimeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDateTimeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDateTimeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDateTimeListEnumerator.GetCurrent : TFhirDateTime;
begin
  Result := FList[FIndex];
end;

function TFhirDateTimeListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirDateTimeList }
procedure TFhirDateTimeList.AddItem(value: TFhirDateTime);
begin
  assert(value.ClassName = 'TFhirDateTime', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDateTime');
  add(value);
end;

procedure TFhirDateTimeList.AddItem(value: TFslDateTime);
begin
  add(TFhirDateTime.create(value));
end;

function TFhirDateTimeList.Append: TFhirDateTime;
begin
  result := TFhirDateTime.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDateTimeList.ClearItems;
begin
  Clear;
end;

function TFhirDateTimeList.GetEnumerator : TFhirDateTimeListEnumerator;
begin
  result := TFhirDateTimeListEnumerator.Create(self.link);
end;

function TFhirDateTimeList.Clone: TFhirDateTimeList;
begin
  result := TFhirDateTimeList(inherited Clone);
end;

function TFhirDateTimeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDateTimeList.GetItemN(index: Integer): TFhirDateTime;
begin
  result := TFhirDateTime(ObjectByIndex[index]);
end;

function TFhirDateTimeList.ItemClass: TFslObjectClass;
begin
  result := TFhirDateTime;
end;
function TFhirDateTimeList.IndexOf(value: TFhirDateTime): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirDateTimeList.Insert(index: Integer): TFhirDateTime;
begin
  result := TFhirDateTime.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDateTimeList.InsertItem(index: Integer; value: TFhirDateTime);
begin
  assert(value is TFhirDateTime);
  Inherited Insert(index, value);
end;

function TFhirDateTimeList.Item(index: Integer): TFhirDateTime;
begin
  result := TFhirDateTime(ObjectByIndex[index]);
end;

function TFhirDateTimeList.Link: TFhirDateTimeList;
begin
  result := TFhirDateTimeList(inherited Link);
end;

procedure TFhirDateTimeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDateTimeList.SetItemByIndex(index: Integer; value: TFhirDateTime);
begin
  assert(value is TFhirDateTime);
  FhirDateTimes[index] := value;
end;

procedure TFhirDateTimeList.SetItemN(index: Integer; value: TFhirDateTime);
begin
  assert(value is TFhirDateTime);
  ObjectByIndex[index] := value;
end;

{ TFhirDate }

Constructor TFhirDate.Create(value : TFslDateTime);
begin
  Create;
  FValue := value.fixPrecision(dtpDay);
end;

destructor TFhirDate.Destroy;
begin
  inherited;
end;

function TFhirDate.fhirType : string;
begin
  result := 'date';
end;

function TFhirDate.GetdateValue : TFslDateTime;
begin
  result := FValue;
end;

procedure TFhirDate.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirDate.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    if (FValue.notNull) then
      oList.add(TFHIRProperty.create(self, 'value', 'date', false, nil, FValue.ToString));
end;

procedure TFhirDate.setDateValue(value: TFslDateTime);
begin
  FValue := value;
end;

procedure TFhirDate.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirDate(oSource).Value;
end;

function TFhirDate.AsStringValue : string;
begin
  if (FValue.null) then
    result := ''
  else
    result := FValue.toXml;
end;

procedure TFhirDate.SetStringValue(value : string);
begin
  if (value = '') then
    FValue := TFslDateTime.makeNull
  else
    FValue := TFslDateTime.fromXml(value);
end;

function TFhirDate.equals(other : TObject) : boolean; 
var
  o : TFhirDate;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDate)) then
    result := false
  else
  begin
    o := TFhirDate(other);
    result := o.value.equal(value);
  end;
end;

function TFhirDate.isDateTime: boolean;
begin
  result := true;
end;

function TFhirDate.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue.null);
end;

function TFhirDate.Link : TFhirDate;
begin
  result := TFhirDate(inherited Link);
end;

function TFhirDate.Clone : TFhirDate;
begin
  result := TFhirDate(inherited Clone);
end;

procedure TFhirDate.setValue(value : TFslDateTime);
begin
  FValue := value;
end;

procedure TFhirDate.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

function TFhirDate.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TFhirDateListEnumerator }

Constructor TFhirDateListEnumerator.Create(list : TFhirDateList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDateListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDateListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDateListEnumerator.GetCurrent : TFhirDate;
begin
  Result := FList[FIndex];
end;

function TFhirDateListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirDateList }
procedure TFhirDateList.AddItem(value: TFhirDate);
begin
  assert(value.ClassName = 'TFhirDate', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDate');
  add(value);
end;

procedure TFhirDateList.AddItem(value: TFslDateTime);
begin
  add(TFhirDate.create(value));
end;

function TFhirDateList.Append: TFhirDate;
begin
  result := TFhirDate.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDateList.ClearItems;
begin
  Clear;
end;

function TFhirDateList.GetEnumerator : TFhirDateListEnumerator;
begin
  result := TFhirDateListEnumerator.Create(self.link);
end;

function TFhirDateList.Clone: TFhirDateList;
begin
  result := TFhirDateList(inherited Clone);
end;

function TFhirDateList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDateList.GetItemN(index: Integer): TFhirDate;
begin
  result := TFhirDate(ObjectByIndex[index]);
end;

function TFhirDateList.ItemClass: TFslObjectClass;
begin
  result := TFhirDate;
end;
function TFhirDateList.IndexOf(value: TFhirDate): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirDateList.Insert(index: Integer): TFhirDate;
begin
  result := TFhirDate.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDateList.InsertItem(index: Integer; value: TFhirDate);
begin
  assert(value is TFhirDate);
  Inherited Insert(index, value);
end;

function TFhirDateList.Item(index: Integer): TFhirDate;
begin
  result := TFhirDate(ObjectByIndex[index]);
end;

function TFhirDateList.Link: TFhirDateList;
begin
  result := TFhirDateList(inherited Link);
end;

procedure TFhirDateList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDateList.SetItemByIndex(index: Integer; value: TFhirDate);
begin
  assert(value is TFhirDate);
  FhirDates[index] := value;
end;

procedure TFhirDateList.SetItemN(index: Integer; value: TFhirDate);
begin
  assert(value is TFhirDate);
  ObjectByIndex[index] := value;
end;

{ TFhirString }

Constructor TFhirString.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirString.Destroy;
begin
  inherited;
end;

function TFhirString.fhirType : string;
begin
  result := 'string';
end;

procedure TFhirString.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirString.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'string', false, nil, FValue));
end;

procedure TFhirString.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirString(oSource).Value;
end;

function TFhirString.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirString.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirString.equals(other : TObject) : boolean; 
var
  o : TFhirString;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirString)) then
    result := false
  else
  begin
    o := TFhirString(other);
    result := o.value = value;
  end;
end;

function TFhirString.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirString.Link : TFhirString;
begin
  result := TFhirString(inherited Link);
end;

function TFhirString.Clone : TFhirString;
begin
  result := TFhirString(inherited Clone);
end;

procedure TFhirString.setValue(value : String);
begin
  FValue := value;
end;

procedure TFhirString.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

function TFhirString.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TFhirStringListEnumerator }

Constructor TFhirStringListEnumerator.Create(list : TFhirStringList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirStringListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirStringListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirStringListEnumerator.GetCurrent : TFhirString;
begin
  Result := FList[FIndex];
end;

function TFhirStringListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirStringList }
procedure TFhirStringList.AddItem(value: TFhirString);
begin
  assert(value.ClassName = 'TFhirString', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirString');
  add(value);
end;

procedure TFhirStringList.AddItem(value: String);
begin
  add(TFhirString.create(value));
end;

function TFhirStringList.Append: TFhirString;
begin
  result := TFhirString.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirStringList.ClearItems;
begin
  Clear;
end;

function TFhirStringList.GetEnumerator : TFhirStringListEnumerator;
begin
  result := TFhirStringListEnumerator.Create(self.link);
end;

function TFhirStringList.Clone: TFhirStringList;
begin
  result := TFhirStringList(inherited Clone);
end;

function TFhirStringList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirStringList.GetItemN(index: Integer): TFhirString;
begin
  result := TFhirString(ObjectByIndex[index]);
end;

function TFhirStringList.ItemClass: TFslObjectClass;
begin
  result := TFhirString;
end;
function TFhirStringList.IndexOf(value: TFhirString): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirStringList.Insert(index: Integer): TFhirString;
begin
  result := TFhirString.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirStringList.InsertItem(index: Integer; value: TFhirString);
begin
  assert(value is TFhirString);
  Inherited Insert(index, value);
end;

function TFhirStringList.Item(index: Integer): TFhirString;
begin
  result := TFhirString(ObjectByIndex[index]);
end;

function TFhirStringList.Link: TFhirStringList;
begin
  result := TFhirStringList(inherited Link);
end;

procedure TFhirStringList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirStringList.SetItemByIndex(index: Integer; value: TFhirString);
begin
  assert(value is TFhirString);
  FhirStrings[index] := value;
end;

procedure TFhirStringList.SetItemN(index: Integer; value: TFhirString);
begin
  assert(value is TFhirString);
  ObjectByIndex[index] := value;
end;

{ TFhirInteger }

Constructor TFhirInteger.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirInteger.Destroy;
begin
  inherited;
end;

function TFhirInteger.fhirType : string;
begin
  result := 'integer';
end;

procedure TFhirInteger.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirInteger.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'integer', false, nil, FValue));
end;

procedure TFhirInteger.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirInteger(oSource).Value;
end;

function TFhirInteger.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirInteger.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirInteger.equals(other : TObject) : boolean; 
var
  o : TFhirInteger;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirInteger)) then
    result := false
  else
  begin
    o := TFhirInteger(other);
    result := o.value = value;
  end;
end;

function TFhirInteger.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirInteger.Link : TFhirInteger;
begin
  result := TFhirInteger(inherited Link);
end;

function TFhirInteger.Clone : TFhirInteger;
begin
  result := TFhirInteger(inherited Clone);
end;

procedure TFhirInteger.setValue(value : String);
begin
  FValue := value;
end;

procedure TFhirInteger.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

function TFhirInteger.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TFhirIntegerListEnumerator }

Constructor TFhirIntegerListEnumerator.Create(list : TFhirIntegerList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirIntegerListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirIntegerListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirIntegerListEnumerator.GetCurrent : TFhirInteger;
begin
  Result := FList[FIndex];
end;

function TFhirIntegerListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirIntegerList }
procedure TFhirIntegerList.AddItem(value: TFhirInteger);
begin
  assert(value.ClassName = 'TFhirInteger', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirInteger');
  add(value);
end;

procedure TFhirIntegerList.AddItem(value: String);
begin
  add(TFhirInteger.create(value));
end;

function TFhirIntegerList.Append: TFhirInteger;
begin
  result := TFhirInteger.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirIntegerList.ClearItems;
begin
  Clear;
end;

function TFhirIntegerList.GetEnumerator : TFhirIntegerListEnumerator;
begin
  result := TFhirIntegerListEnumerator.Create(self.link);
end;

function TFhirIntegerList.Clone: TFhirIntegerList;
begin
  result := TFhirIntegerList(inherited Clone);
end;

function TFhirIntegerList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirIntegerList.GetItemN(index: Integer): TFhirInteger;
begin
  result := TFhirInteger(ObjectByIndex[index]);
end;

function TFhirIntegerList.ItemClass: TFslObjectClass;
begin
  result := TFhirInteger;
end;
function TFhirIntegerList.IndexOf(value: TFhirInteger): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirIntegerList.Insert(index: Integer): TFhirInteger;
begin
  result := TFhirInteger.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirIntegerList.InsertItem(index: Integer; value: TFhirInteger);
begin
  assert(value is TFhirInteger);
  Inherited Insert(index, value);
end;

function TFhirIntegerList.Item(index: Integer): TFhirInteger;
begin
  result := TFhirInteger(ObjectByIndex[index]);
end;

function TFhirIntegerList.Link: TFhirIntegerList;
begin
  result := TFhirIntegerList(inherited Link);
end;

procedure TFhirIntegerList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirIntegerList.SetItemByIndex(index: Integer; value: TFhirInteger);
begin
  assert(value is TFhirInteger);
  FhirIntegers[index] := value;
end;

procedure TFhirIntegerList.SetItemN(index: Integer; value: TFhirInteger);
begin
  assert(value is TFhirInteger);
  ObjectByIndex[index] := value;
end;

{ TFhirUri }

Constructor TFhirUri.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirUri.Destroy;
begin
  inherited;
end;

function TFhirUri.fhirType : string;
begin
  result := 'uri';
end;

procedure TFhirUri.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirUri.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'uri', false, nil, FValue));
end;

procedure TFhirUri.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirUri(oSource).Value;
end;

function TFhirUri.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirUri.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirUri.equals(other : TObject) : boolean; 
var
  o : TFhirUri;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirUri)) then
    result := false
  else
  begin
    o := TFhirUri(other);
    result := o.value = value;
  end;
end;

function TFhirUri.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirUri.Link : TFhirUri;
begin
  result := TFhirUri(inherited Link);
end;

function TFhirUri.Clone : TFhirUri;
begin
  result := TFhirUri(inherited Clone);
end;

procedure TFhirUri.setValue(value : String);
begin
  FValue := value;
end;

procedure TFhirUri.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

function TFhirUri.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TFhirUriListEnumerator }

Constructor TFhirUriListEnumerator.Create(list : TFhirUriList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirUriListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirUriListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirUriListEnumerator.GetCurrent : TFhirUri;
begin
  Result := FList[FIndex];
end;

function TFhirUriListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirUriList }
procedure TFhirUriList.AddItem(value: TFhirUri);
begin
  assert(value.ClassName = 'TFhirUri', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirUri');
  add(value);
end;

procedure TFhirUriList.AddItem(value: String);
begin
  add(TFhirUri.create(value));
end;

function TFhirUriList.Append: TFhirUri;
begin
  result := TFhirUri.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirUriList.ClearItems;
begin
  Clear;
end;

function TFhirUriList.GetEnumerator : TFhirUriListEnumerator;
begin
  result := TFhirUriListEnumerator.Create(self.link);
end;

function TFhirUriList.Clone: TFhirUriList;
begin
  result := TFhirUriList(inherited Clone);
end;

function TFhirUriList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirUriList.GetItemN(index: Integer): TFhirUri;
begin
  result := TFhirUri(ObjectByIndex[index]);
end;

function TFhirUriList.ItemClass: TFslObjectClass;
begin
  result := TFhirUri;
end;
function TFhirUriList.IndexOf(value: TFhirUri): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirUriList.Insert(index: Integer): TFhirUri;
begin
  result := TFhirUri.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirUriList.InsertItem(index: Integer; value: TFhirUri);
begin
  assert(value is TFhirUri);
  Inherited Insert(index, value);
end;

function TFhirUriList.Item(index: Integer): TFhirUri;
begin
  result := TFhirUri(ObjectByIndex[index]);
end;

function TFhirUriList.Link: TFhirUriList;
begin
  result := TFhirUriList(inherited Link);
end;

procedure TFhirUriList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirUriList.SetItemByIndex(index: Integer; value: TFhirUri);
begin
  assert(value is TFhirUri);
  FhirUris[index] := value;
end;

procedure TFhirUriList.SetItemN(index: Integer; value: TFhirUri);
begin
  assert(value is TFhirUri);
  ObjectByIndex[index] := value;
end;

{ TFhirInstant }

Constructor TFhirInstant.Create(value : TFslDateTime);
begin
  Create;
  FValue := value;
end;

destructor TFhirInstant.Destroy;
begin
  inherited;
end;

function TFhirInstant.fhirType : string;
begin
  result := 'instant';
end;

function TFhirInstant.getdateValue : TFslDateTime;
begin
  result := FValue;
end;

procedure TFhirInstant.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirInstant.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    if (FValue.notNull) then
      oList.add(TFHIRProperty.create(self, 'value', 'instant', false, nil, FValue.ToString));
end;

procedure TFhirInstant.setDateValue(value: TFslDateTime);
begin
  FValue := value;
end;

procedure TFhirInstant.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirInstant(oSource).Value;
end;

function TFhirInstant.AsStringValue : string;
begin
  if (FValue.null) then
    result := ''
  else
    result := FValue.toXml;
end;

procedure TFhirInstant.SetStringValue(value : string);
begin
  if (value = '') then
    FValue := TFslDateTime.makeNull
  else
    FValue := TFslDateTime.fromXml(value);
end;

function TFhirInstant.equals(other : TObject) : boolean; 
var
  o : TFhirInstant;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirInstant)) then
    result := false
  else
  begin
    o := TFhirInstant(other);
    result := o.value.equal(value);
  end;
end;

function TFhirInstant.isDateTime: boolean;
begin
  result := true;
end;

function TFhirInstant.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue.null);
end;

function TFhirInstant.Link : TFhirInstant;
begin
  result := TFhirInstant(inherited Link);
end;

function TFhirInstant.Clone : TFhirInstant;
begin
  result := TFhirInstant(inherited Clone);
end;

procedure TFhirInstant.setValue(value : TFslDateTime);
begin
  FValue := value;
end;

procedure TFhirInstant.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

function TFhirInstant.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TFhirInstantListEnumerator }

Constructor TFhirInstantListEnumerator.Create(list : TFhirInstantList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirInstantListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirInstantListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirInstantListEnumerator.GetCurrent : TFhirInstant;
begin
  Result := FList[FIndex];
end;

function TFhirInstantListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirInstantList }
procedure TFhirInstantList.AddItem(value: TFhirInstant);
begin
  assert(value.ClassName = 'TFhirInstant', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirInstant');
  add(value);
end;

procedure TFhirInstantList.AddItem(value: TFslDateTime);
begin
  add(TFhirInstant.create(value));
end;

function TFhirInstantList.Append: TFhirInstant;
begin
  result := TFhirInstant.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirInstantList.ClearItems;
begin
  Clear;
end;

function TFhirInstantList.GetEnumerator : TFhirInstantListEnumerator;
begin
  result := TFhirInstantListEnumerator.Create(self.link);
end;

function TFhirInstantList.Clone: TFhirInstantList;
begin
  result := TFhirInstantList(inherited Clone);
end;

function TFhirInstantList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirInstantList.GetItemN(index: Integer): TFhirInstant;
begin
  result := TFhirInstant(ObjectByIndex[index]);
end;

function TFhirInstantList.ItemClass: TFslObjectClass;
begin
  result := TFhirInstant;
end;
function TFhirInstantList.IndexOf(value: TFhirInstant): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirInstantList.Insert(index: Integer): TFhirInstant;
begin
  result := TFhirInstant.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirInstantList.InsertItem(index: Integer; value: TFhirInstant);
begin
  assert(value is TFhirInstant);
  Inherited Insert(index, value);
end;

function TFhirInstantList.Item(index: Integer): TFhirInstant;
begin
  result := TFhirInstant(ObjectByIndex[index]);
end;

function TFhirInstantList.Link: TFhirInstantList;
begin
  result := TFhirInstantList(inherited Link);
end;

procedure TFhirInstantList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirInstantList.SetItemByIndex(index: Integer; value: TFhirInstant);
begin
  assert(value is TFhirInstant);
  FhirInstants[index] := value;
end;

procedure TFhirInstantList.SetItemN(index: Integer; value: TFhirInstant);
begin
  assert(value is TFhirInstant);
  ObjectByIndex[index] := value;
end;

{ TFhirBoolean }

Constructor TFhirBoolean.Create(value : Boolean);
begin
  Create;
  FValue := value;
end;

destructor TFhirBoolean.Destroy;
begin
  inherited;
end;

function TFhirBoolean.fhirType : string;
begin
  result := 'boolean';
end;

procedure TFhirBoolean.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirBoolean.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'boolean', false, nil, LCBooleanToString(FValue)));
end;

procedure TFhirBoolean.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirBoolean(oSource).Value;
end;

function TFhirBoolean.AsStringValue : string;
begin
  result := LCBooleanToString(FValue);
end;

procedure TFhirBoolean.SetStringValue(value : string);
begin
  FValue := StringToBoolean(value);
end;

function TFhirBoolean.equals(other : TObject) : boolean; 
var
  o : TFhirBoolean;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirBoolean)) then
    result := false
  else
  begin
    o := TFhirBoolean(other);
    result := o.value = value;
  end;
end;

function TFhirBoolean.isBooleanPrimitive: boolean;
begin
  result := true;
end;

function TFhirBoolean.isEmpty : boolean;
begin
  result := false;
end;

function TFhirBoolean.Link : TFhirBoolean;
begin
  result := TFhirBoolean(inherited Link);
end;

function TFhirBoolean.Clone : TFhirBoolean;
begin
  result := TFhirBoolean(inherited Clone);
end;

procedure TFhirBoolean.setValue(value : Boolean);
begin
  FValue := value;
end;

procedure TFhirBoolean.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

function TFhirBoolean.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TFhirBooleanListEnumerator }

Constructor TFhirBooleanListEnumerator.Create(list : TFhirBooleanList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirBooleanListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirBooleanListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirBooleanListEnumerator.GetCurrent : TFhirBoolean;
begin
  Result := FList[FIndex];
end;

function TFhirBooleanListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirBooleanList }
procedure TFhirBooleanList.AddItem(value: TFhirBoolean);
begin
  assert(value.ClassName = 'TFhirBoolean', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirBoolean');
  add(value);
end;

procedure TFhirBooleanList.AddItem(value: Boolean);
begin
  add(TFhirBoolean.create(value));
end;

function TFhirBooleanList.Append: TFhirBoolean;
begin
  result := TFhirBoolean.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirBooleanList.ClearItems;
begin
  Clear;
end;

function TFhirBooleanList.GetEnumerator : TFhirBooleanListEnumerator;
begin
  result := TFhirBooleanListEnumerator.Create(self.link);
end;

function TFhirBooleanList.Clone: TFhirBooleanList;
begin
  result := TFhirBooleanList(inherited Clone);
end;

function TFhirBooleanList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirBooleanList.GetItemN(index: Integer): TFhirBoolean;
begin
  result := TFhirBoolean(ObjectByIndex[index]);
end;

function TFhirBooleanList.ItemClass: TFslObjectClass;
begin
  result := TFhirBoolean;
end;
function TFhirBooleanList.IndexOf(value: TFhirBoolean): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirBooleanList.Insert(index: Integer): TFhirBoolean;
begin
  result := TFhirBoolean.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirBooleanList.InsertItem(index: Integer; value: TFhirBoolean);
begin
  assert(value is TFhirBoolean);
  Inherited Insert(index, value);
end;

function TFhirBooleanList.Item(index: Integer): TFhirBoolean;
begin
  result := TFhirBoolean(ObjectByIndex[index]);
end;

function TFhirBooleanList.Link: TFhirBooleanList;
begin
  result := TFhirBooleanList(inherited Link);
end;

procedure TFhirBooleanList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirBooleanList.SetItemByIndex(index: Integer; value: TFhirBoolean);
begin
  assert(value is TFhirBoolean);
  FhirBooleans[index] := value;
end;

procedure TFhirBooleanList.SetItemN(index: Integer; value: TFhirBoolean);
begin
  assert(value is TFhirBoolean);
  ObjectByIndex[index] := value;
end;

{ TFhirBase64Binary }

Constructor TFhirBase64Binary.Create(value : TBytes);
begin
  Create;
  FValue := value;
end;

destructor TFhirBase64Binary.Destroy;
begin
  inherited;
end;

function TFhirBase64Binary.fhirType : string;
begin
  result := 'base64Binary';
end;

procedure TFhirBase64Binary.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirBase64Binary.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'base64Binary', false, nil, FValue));
end;

procedure TFhirBase64Binary.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirBase64Binary(oSource).Value;
end;

function TFhirBase64Binary.AsStringValue : string;
begin
  if (length(FValue) = 0) then result := '' else result := string(EncodeBase64(FValue));
end;

procedure TFhirBase64Binary.SetStringValue(value : string);
begin
  if (length(value) = 0) then SetLength(FValue, 0) else FValue := DecodeBase64(wideString(value));
end;

function TFhirBase64Binary.equals(other : TObject) : boolean; 
var
  o : TFhirBase64Binary;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirBase64Binary)) then
    result := false
  else
  begin
    o := TFhirBase64Binary(other);
    result := o.value = value;
  end;
end;

function TFhirBase64Binary.isEmpty : boolean;
begin
  result := inherited isEmpty and (length(FValue) = 0);
end;

function TFhirBase64Binary.Link : TFhirBase64Binary;
begin
  result := TFhirBase64Binary(inherited Link);
end;

function TFhirBase64Binary.Clone : TFhirBase64Binary;
begin
  result := TFhirBase64Binary(inherited Clone);
end;

procedure TFhirBase64Binary.setValue(value : TBytes);
begin
  FValue := value;
end;

procedure TFhirBase64Binary.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

function TFhirBase64Binary.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, length(FValue));
end;

{ TFhirBase64BinaryListEnumerator }

Constructor TFhirBase64BinaryListEnumerator.Create(list : TFhirBase64BinaryList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirBase64BinaryListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirBase64BinaryListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirBase64BinaryListEnumerator.GetCurrent : TFhirBase64Binary;
begin
  Result := FList[FIndex];
end;

function TFhirBase64BinaryListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirBase64BinaryList }
procedure TFhirBase64BinaryList.AddItem(value: TFhirBase64Binary);
begin
  assert(value.ClassName = 'TFhirBase64Binary', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirBase64Binary');
  add(value);
end;

procedure TFhirBase64BinaryList.AddItem(value: TBytes);
begin
  add(TFhirBase64Binary.create(value));
end;

function TFhirBase64BinaryList.Append: TFhirBase64Binary;
begin
  result := TFhirBase64Binary.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirBase64BinaryList.ClearItems;
begin
  Clear;
end;

function TFhirBase64BinaryList.GetEnumerator : TFhirBase64BinaryListEnumerator;
begin
  result := TFhirBase64BinaryListEnumerator.Create(self.link);
end;

function TFhirBase64BinaryList.Clone: TFhirBase64BinaryList;
begin
  result := TFhirBase64BinaryList(inherited Clone);
end;

function TFhirBase64BinaryList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirBase64BinaryList.GetItemN(index: Integer): TFhirBase64Binary;
begin
  result := TFhirBase64Binary(ObjectByIndex[index]);
end;

function TFhirBase64BinaryList.ItemClass: TFslObjectClass;
begin
  result := TFhirBase64Binary;
end;
function TFhirBase64BinaryList.IndexOf(value: TFhirBase64Binary): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirBase64BinaryList.Insert(index: Integer): TFhirBase64Binary;
begin
  result := TFhirBase64Binary.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirBase64BinaryList.InsertItem(index: Integer; value: TFhirBase64Binary);
begin
  assert(value is TFhirBase64Binary);
  Inherited Insert(index, value);
end;

function TFhirBase64BinaryList.Item(index: Integer): TFhirBase64Binary;
begin
  result := TFhirBase64Binary(ObjectByIndex[index]);
end;

function TFhirBase64BinaryList.Link: TFhirBase64BinaryList;
begin
  result := TFhirBase64BinaryList(inherited Link);
end;

procedure TFhirBase64BinaryList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirBase64BinaryList.SetItemByIndex(index: Integer; value: TFhirBase64Binary);
begin
  assert(value is TFhirBase64Binary);
  FhirBase64Binaries[index] := value;
end;

procedure TFhirBase64BinaryList.SetItemN(index: Integer; value: TFhirBase64Binary);
begin
  assert(value is TFhirBase64Binary);
  ObjectByIndex[index] := value;
end;

{ TFhirTime }

Constructor TFhirTime.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirTime.Destroy;
begin
  inherited;
end;

function TFhirTime.fhirType : string;
begin
  result := 'time';
end;

procedure TFhirTime.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirTime.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'time', false, nil, FValue));
end;

procedure TFhirTime.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirTime(oSource).Value;
end;

function TFhirTime.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirTime.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirTime.equals(other : TObject) : boolean; 
var
  o : TFhirTime;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirTime)) then
    result := false
  else
  begin
    o := TFhirTime(other);
    result := o.value = value;
  end;
end;

function TFhirTime.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirTime.Link : TFhirTime;
begin
  result := TFhirTime(inherited Link);
end;

function TFhirTime.Clone : TFhirTime;
begin
  result := TFhirTime(inherited Clone);
end;

procedure TFhirTime.setValue(value : String);
begin
  FValue := value;
end;

procedure TFhirTime.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

function TFhirTime.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TFhirTimeListEnumerator }

Constructor TFhirTimeListEnumerator.Create(list : TFhirTimeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirTimeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirTimeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirTimeListEnumerator.GetCurrent : TFhirTime;
begin
  Result := FList[FIndex];
end;

function TFhirTimeListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirTimeList }
procedure TFhirTimeList.AddItem(value: TFhirTime);
begin
  assert(value.ClassName = 'TFhirTime', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirTime');
  add(value);
end;

procedure TFhirTimeList.AddItem(value: String);
begin
  add(TFhirTime.create(value));
end;

function TFhirTimeList.Append: TFhirTime;
begin
  result := TFhirTime.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirTimeList.ClearItems;
begin
  Clear;
end;

function TFhirTimeList.GetEnumerator : TFhirTimeListEnumerator;
begin
  result := TFhirTimeListEnumerator.Create(self.link);
end;

function TFhirTimeList.Clone: TFhirTimeList;
begin
  result := TFhirTimeList(inherited Clone);
end;

function TFhirTimeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirTimeList.GetItemN(index: Integer): TFhirTime;
begin
  result := TFhirTime(ObjectByIndex[index]);
end;

function TFhirTimeList.ItemClass: TFslObjectClass;
begin
  result := TFhirTime;
end;
function TFhirTimeList.IndexOf(value: TFhirTime): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirTimeList.Insert(index: Integer): TFhirTime;
begin
  result := TFhirTime.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirTimeList.InsertItem(index: Integer; value: TFhirTime);
begin
  assert(value is TFhirTime);
  Inherited Insert(index, value);
end;

function TFhirTimeList.Item(index: Integer): TFhirTime;
begin
  result := TFhirTime(ObjectByIndex[index]);
end;

function TFhirTimeList.Link: TFhirTimeList;
begin
  result := TFhirTimeList(inherited Link);
end;

procedure TFhirTimeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirTimeList.SetItemByIndex(index: Integer; value: TFhirTime);
begin
  assert(value is TFhirTime);
  FhirTimes[index] := value;
end;

procedure TFhirTimeList.SetItemN(index: Integer; value: TFhirTime);
begin
  assert(value is TFhirTime);
  ObjectByIndex[index] := value;
end;

{ TFhirDecimal }

Constructor TFhirDecimal.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirDecimal.Destroy;
begin
  inherited;
end;

function TFhirDecimal.fhirType : string;
begin
  result := 'decimal';
end;

procedure TFhirDecimal.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirDecimal.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'decimal', false, nil, FValue));
end;

procedure TFhirDecimal.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirDecimal(oSource).Value;
end;

function TFhirDecimal.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirDecimal.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirDecimal.equals(other : TObject) : boolean; 
var
  o : TFhirDecimal;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDecimal)) then
    result := false
  else
  begin
    o := TFhirDecimal(other);
    result := o.value = value;
  end;
end;

function TFhirDecimal.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirDecimal.Link : TFhirDecimal;
begin
  result := TFhirDecimal(inherited Link);
end;

function TFhirDecimal.Clone : TFhirDecimal;
begin
  result := TFhirDecimal(inherited Clone);
end;

procedure TFhirDecimal.setValue(value : String);
begin
  FValue := value;
end;

procedure TFhirDecimal.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

function TFhirDecimal.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TFhirDecimalListEnumerator }

Constructor TFhirDecimalListEnumerator.Create(list : TFhirDecimalList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDecimalListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDecimalListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDecimalListEnumerator.GetCurrent : TFhirDecimal;
begin
  Result := FList[FIndex];
end;

function TFhirDecimalListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirDecimalList }
procedure TFhirDecimalList.AddItem(value: TFhirDecimal);
begin
  assert(value.ClassName = 'TFhirDecimal', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDecimal');
  add(value);
end;

procedure TFhirDecimalList.AddItem(value: String);
begin
  add(TFhirDecimal.create(value));
end;

function TFhirDecimalList.Append: TFhirDecimal;
begin
  result := TFhirDecimal.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDecimalList.ClearItems;
begin
  Clear;
end;

function TFhirDecimalList.GetEnumerator : TFhirDecimalListEnumerator;
begin
  result := TFhirDecimalListEnumerator.Create(self.link);
end;

function TFhirDecimalList.Clone: TFhirDecimalList;
begin
  result := TFhirDecimalList(inherited Clone);
end;

function TFhirDecimalList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDecimalList.GetItemN(index: Integer): TFhirDecimal;
begin
  result := TFhirDecimal(ObjectByIndex[index]);
end;

function TFhirDecimalList.ItemClass: TFslObjectClass;
begin
  result := TFhirDecimal;
end;
function TFhirDecimalList.IndexOf(value: TFhirDecimal): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirDecimalList.Insert(index: Integer): TFhirDecimal;
begin
  result := TFhirDecimal.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDecimalList.InsertItem(index: Integer; value: TFhirDecimal);
begin
  assert(value is TFhirDecimal);
  Inherited Insert(index, value);
end;

function TFhirDecimalList.Item(index: Integer): TFhirDecimal;
begin
  result := TFhirDecimal(ObjectByIndex[index]);
end;

function TFhirDecimalList.Link: TFhirDecimalList;
begin
  result := TFhirDecimalList(inherited Link);
end;

procedure TFhirDecimalList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDecimalList.SetItemByIndex(index: Integer; value: TFhirDecimal);
begin
  assert(value is TFhirDecimal);
  FhirDecimals[index] := value;
end;

procedure TFhirDecimalList.SetItemN(index: Integer; value: TFhirDecimal);
begin
  assert(value is TFhirDecimal);
  ObjectByIndex[index] := value;
end;

{ TFhirCode }

Constructor TFhirCode.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirCode.Destroy;
begin
  inherited;
end;

function TFhirCode.fhirType : string;
begin
  result := 'code';
end;

function TFhirCode.Link : TFhirCode;
begin
  result := TFhirCode(inherited Link);
end;

function TFhirCode.Clone : TFhirCode;
begin
  result := TFhirCode(inherited Clone);
end;

procedure TFhirCode.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

{ TFhirCodeListEnumerator }

Constructor TFhirCodeListEnumerator.Create(list : TFhirCodeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirCodeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirCodeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirCodeListEnumerator.GetCurrent : TFhirCode;
begin
  Result := FList[FIndex];
end;

function TFhirCodeListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirCodeList }
procedure TFhirCodeList.AddItem(value: TFhirCode);
begin
  assert(value.ClassName = 'TFhirCode', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirCode');
  add(value);
end;

procedure TFhirCodeList.AddItem(value: String);
begin
  add(TFhirCode.create(value));
end;

function TFhirCodeList.Append: TFhirCode;
begin
  result := TFhirCode.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirCodeList.ClearItems;
begin
  Clear;
end;

function TFhirCodeList.GetEnumerator : TFhirCodeListEnumerator;
begin
  result := TFhirCodeListEnumerator.Create(self.link);
end;

function TFhirCodeList.Clone: TFhirCodeList;
begin
  result := TFhirCodeList(inherited Clone);
end;

function TFhirCodeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirCodeList.GetItemN(index: Integer): TFhirCode;
begin
  result := TFhirCode(ObjectByIndex[index]);
end;

function TFhirCodeList.ItemClass: TFslObjectClass;
begin
  result := TFhirCode;
end;
function TFhirCodeList.IndexOf(value: TFhirCode): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirCodeList.Insert(index: Integer): TFhirCode;
begin
  result := TFhirCode.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirCodeList.InsertItem(index: Integer; value: TFhirCode);
begin
  assert(value is TFhirCode);
  Inherited Insert(index, value);
end;

function TFhirCodeList.Item(index: Integer): TFhirCode;
begin
  result := TFhirCode(ObjectByIndex[index]);
end;

function TFhirCodeList.Link: TFhirCodeList;
begin
  result := TFhirCodeList(inherited Link);
end;

procedure TFhirCodeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirCodeList.SetItemByIndex(index: Integer; value: TFhirCode);
begin
  assert(value is TFhirCode);
  FhirCodes[index] := value;
end;

procedure TFhirCodeList.SetItemN(index: Integer; value: TFhirCode);
begin
  assert(value is TFhirCode);
  ObjectByIndex[index] := value;
end;

{ TFhirOid }

Constructor TFhirOid.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirOid.Destroy;
begin
  inherited;
end;

function TFhirOid.fhirType : string;
begin
  result := 'oid';
end;

function TFhirOid.Link : TFhirOid;
begin
  result := TFhirOid(inherited Link);
end;

function TFhirOid.Clone : TFhirOid;
begin
  result := TFhirOid(inherited Clone);
end;

procedure TFhirOid.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

{ TFhirOidListEnumerator }

Constructor TFhirOidListEnumerator.Create(list : TFhirOidList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirOidListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirOidListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirOidListEnumerator.GetCurrent : TFhirOid;
begin
  Result := FList[FIndex];
end;

function TFhirOidListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirOidList }
procedure TFhirOidList.AddItem(value: TFhirOid);
begin
  assert(value.ClassName = 'TFhirOid', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirOid');
  add(value);
end;

procedure TFhirOidList.AddItem(value: String);
begin
  add(TFhirOid.create(value));
end;

function TFhirOidList.Append: TFhirOid;
begin
  result := TFhirOid.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirOidList.ClearItems;
begin
  Clear;
end;

function TFhirOidList.GetEnumerator : TFhirOidListEnumerator;
begin
  result := TFhirOidListEnumerator.Create(self.link);
end;

function TFhirOidList.Clone: TFhirOidList;
begin
  result := TFhirOidList(inherited Clone);
end;

function TFhirOidList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirOidList.GetItemN(index: Integer): TFhirOid;
begin
  result := TFhirOid(ObjectByIndex[index]);
end;

function TFhirOidList.ItemClass: TFslObjectClass;
begin
  result := TFhirOid;
end;
function TFhirOidList.IndexOf(value: TFhirOid): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirOidList.Insert(index: Integer): TFhirOid;
begin
  result := TFhirOid.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirOidList.InsertItem(index: Integer; value: TFhirOid);
begin
  assert(value is TFhirOid);
  Inherited Insert(index, value);
end;

function TFhirOidList.Item(index: Integer): TFhirOid;
begin
  result := TFhirOid(ObjectByIndex[index]);
end;

function TFhirOidList.Link: TFhirOidList;
begin
  result := TFhirOidList(inherited Link);
end;

procedure TFhirOidList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirOidList.SetItemByIndex(index: Integer; value: TFhirOid);
begin
  assert(value is TFhirOid);
  FhirOids[index] := value;
end;

procedure TFhirOidList.SetItemN(index: Integer; value: TFhirOid);
begin
  assert(value is TFhirOid);
  ObjectByIndex[index] := value;
end;

{ TFhirUuid }

Constructor TFhirUuid.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirUuid.Destroy;
begin
  inherited;
end;

function TFhirUuid.fhirType : string;
begin
  result := 'uuid';
end;

function TFhirUuid.Link : TFhirUuid;
begin
  result := TFhirUuid(inherited Link);
end;

function TFhirUuid.Clone : TFhirUuid;
begin
  result := TFhirUuid(inherited Clone);
end;

procedure TFhirUuid.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

{ TFhirUuidListEnumerator }

Constructor TFhirUuidListEnumerator.Create(list : TFhirUuidList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirUuidListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirUuidListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirUuidListEnumerator.GetCurrent : TFhirUuid;
begin
  Result := FList[FIndex];
end;

function TFhirUuidListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirUuidList }
procedure TFhirUuidList.AddItem(value: TFhirUuid);
begin
  assert(value.ClassName = 'TFhirUuid', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirUuid');
  add(value);
end;

procedure TFhirUuidList.AddItem(value: String);
begin
  add(TFhirUuid.create(value));
end;

function TFhirUuidList.Append: TFhirUuid;
begin
  result := TFhirUuid.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirUuidList.ClearItems;
begin
  Clear;
end;

function TFhirUuidList.GetEnumerator : TFhirUuidListEnumerator;
begin
  result := TFhirUuidListEnumerator.Create(self.link);
end;

function TFhirUuidList.Clone: TFhirUuidList;
begin
  result := TFhirUuidList(inherited Clone);
end;

function TFhirUuidList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirUuidList.GetItemN(index: Integer): TFhirUuid;
begin
  result := TFhirUuid(ObjectByIndex[index]);
end;

function TFhirUuidList.ItemClass: TFslObjectClass;
begin
  result := TFhirUuid;
end;
function TFhirUuidList.IndexOf(value: TFhirUuid): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirUuidList.Insert(index: Integer): TFhirUuid;
begin
  result := TFhirUuid.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirUuidList.InsertItem(index: Integer; value: TFhirUuid);
begin
  assert(value is TFhirUuid);
  Inherited Insert(index, value);
end;

function TFhirUuidList.Item(index: Integer): TFhirUuid;
begin
  result := TFhirUuid(ObjectByIndex[index]);
end;

function TFhirUuidList.Link: TFhirUuidList;
begin
  result := TFhirUuidList(inherited Link);
end;

procedure TFhirUuidList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirUuidList.SetItemByIndex(index: Integer; value: TFhirUuid);
begin
  assert(value is TFhirUuid);
  FhirUuids[index] := value;
end;

procedure TFhirUuidList.SetItemN(index: Integer; value: TFhirUuid);
begin
  assert(value is TFhirUuid);
  ObjectByIndex[index] := value;
end;

{ TFhirMarkdown }

Constructor TFhirMarkdown.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirMarkdown.Destroy;
begin
  inherited;
end;

function TFhirMarkdown.fhirType : string;
begin
  result := 'markdown';
end;

function TFhirMarkdown.Link : TFhirMarkdown;
begin
  result := TFhirMarkdown(inherited Link);
end;

function TFhirMarkdown.Clone : TFhirMarkdown;
begin
  result := TFhirMarkdown(inherited Clone);
end;

procedure TFhirMarkdown.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

{ TFhirMarkdownListEnumerator }

Constructor TFhirMarkdownListEnumerator.Create(list : TFhirMarkdownList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirMarkdownListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirMarkdownListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirMarkdownListEnumerator.GetCurrent : TFhirMarkdown;
begin
  Result := FList[FIndex];
end;

function TFhirMarkdownListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirMarkdownList }
procedure TFhirMarkdownList.AddItem(value: TFhirMarkdown);
begin
  assert(value.ClassName = 'TFhirMarkdown', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirMarkdown');
  add(value);
end;

procedure TFhirMarkdownList.AddItem(value: String);
begin
  add(TFhirMarkdown.create(value));
end;

function TFhirMarkdownList.Append: TFhirMarkdown;
begin
  result := TFhirMarkdown.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirMarkdownList.ClearItems;
begin
  Clear;
end;

function TFhirMarkdownList.GetEnumerator : TFhirMarkdownListEnumerator;
begin
  result := TFhirMarkdownListEnumerator.Create(self.link);
end;

function TFhirMarkdownList.Clone: TFhirMarkdownList;
begin
  result := TFhirMarkdownList(inherited Clone);
end;

function TFhirMarkdownList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirMarkdownList.GetItemN(index: Integer): TFhirMarkdown;
begin
  result := TFhirMarkdown(ObjectByIndex[index]);
end;

function TFhirMarkdownList.ItemClass: TFslObjectClass;
begin
  result := TFhirMarkdown;
end;
function TFhirMarkdownList.IndexOf(value: TFhirMarkdown): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirMarkdownList.Insert(index: Integer): TFhirMarkdown;
begin
  result := TFhirMarkdown.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirMarkdownList.InsertItem(index: Integer; value: TFhirMarkdown);
begin
  assert(value is TFhirMarkdown);
  Inherited Insert(index, value);
end;

function TFhirMarkdownList.Item(index: Integer): TFhirMarkdown;
begin
  result := TFhirMarkdown(ObjectByIndex[index]);
end;

function TFhirMarkdownList.Link: TFhirMarkdownList;
begin
  result := TFhirMarkdownList(inherited Link);
end;

procedure TFhirMarkdownList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirMarkdownList.SetItemByIndex(index: Integer; value: TFhirMarkdown);
begin
  assert(value is TFhirMarkdown);
  FhirMarkdowns[index] := value;
end;

procedure TFhirMarkdownList.SetItemN(index: Integer; value: TFhirMarkdown);
begin
  assert(value is TFhirMarkdown);
  ObjectByIndex[index] := value;
end;

{ TFhirUnsignedInt }

Constructor TFhirUnsignedInt.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirUnsignedInt.Destroy;
begin
  inherited;
end;

function TFhirUnsignedInt.fhirType : string;
begin
  result := 'unsignedInt';
end;

function TFhirUnsignedInt.Link : TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt(inherited Link);
end;

function TFhirUnsignedInt.Clone : TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt(inherited Clone);
end;

procedure TFhirUnsignedInt.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

{ TFhirUnsignedIntListEnumerator }

Constructor TFhirUnsignedIntListEnumerator.Create(list : TFhirUnsignedIntList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirUnsignedIntListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirUnsignedIntListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirUnsignedIntListEnumerator.GetCurrent : TFhirUnsignedInt;
begin
  Result := FList[FIndex];
end;

function TFhirUnsignedIntListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirUnsignedIntList }
procedure TFhirUnsignedIntList.AddItem(value: TFhirUnsignedInt);
begin
  assert(value.ClassName = 'TFhirUnsignedInt', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirUnsignedInt');
  add(value);
end;

procedure TFhirUnsignedIntList.AddItem(value: String);
begin
  add(TFhirUnsignedInt.create(value));
end;

function TFhirUnsignedIntList.Append: TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirUnsignedIntList.ClearItems;
begin
  Clear;
end;

function TFhirUnsignedIntList.GetEnumerator : TFhirUnsignedIntListEnumerator;
begin
  result := TFhirUnsignedIntListEnumerator.Create(self.link);
end;

function TFhirUnsignedIntList.Clone: TFhirUnsignedIntList;
begin
  result := TFhirUnsignedIntList(inherited Clone);
end;

function TFhirUnsignedIntList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirUnsignedIntList.GetItemN(index: Integer): TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt(ObjectByIndex[index]);
end;

function TFhirUnsignedIntList.ItemClass: TFslObjectClass;
begin
  result := TFhirUnsignedInt;
end;
function TFhirUnsignedIntList.IndexOf(value: TFhirUnsignedInt): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirUnsignedIntList.Insert(index: Integer): TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirUnsignedIntList.InsertItem(index: Integer; value: TFhirUnsignedInt);
begin
  assert(value is TFhirUnsignedInt);
  Inherited Insert(index, value);
end;

function TFhirUnsignedIntList.Item(index: Integer): TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt(ObjectByIndex[index]);
end;

function TFhirUnsignedIntList.Link: TFhirUnsignedIntList;
begin
  result := TFhirUnsignedIntList(inherited Link);
end;

procedure TFhirUnsignedIntList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirUnsignedIntList.SetItemByIndex(index: Integer; value: TFhirUnsignedInt);
begin
  assert(value is TFhirUnsignedInt);
  FhirUnsignedInts[index] := value;
end;

procedure TFhirUnsignedIntList.SetItemN(index: Integer; value: TFhirUnsignedInt);
begin
  assert(value is TFhirUnsignedInt);
  ObjectByIndex[index] := value;
end;

{ TFhirId }

Constructor TFhirId.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirId.Destroy;
begin
  inherited;
end;

function TFhirId.fhirType : string;
begin
  result := 'id';
end;

function TFhirId.Link : TFhirId;
begin
  result := TFhirId(inherited Link);
end;

function TFhirId.Clone : TFhirId;
begin
  result := TFhirId(inherited Clone);
end;

procedure TFhirId.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

{ TFhirIdListEnumerator }

Constructor TFhirIdListEnumerator.Create(list : TFhirIdList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirIdListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirIdListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirIdListEnumerator.GetCurrent : TFhirId;
begin
  Result := FList[FIndex];
end;

function TFhirIdListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirIdList }
procedure TFhirIdList.AddItem(value: TFhirId);
begin
  assert(value.ClassName = 'TFhirId', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirId');
  add(value);
end;

procedure TFhirIdList.AddItem(value: String);
begin
  add(TFhirId.create(value));
end;

function TFhirIdList.Append: TFhirId;
begin
  result := TFhirId.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirIdList.ClearItems;
begin
  Clear;
end;

function TFhirIdList.GetEnumerator : TFhirIdListEnumerator;
begin
  result := TFhirIdListEnumerator.Create(self.link);
end;

function TFhirIdList.Clone: TFhirIdList;
begin
  result := TFhirIdList(inherited Clone);
end;

function TFhirIdList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirIdList.GetItemN(index: Integer): TFhirId;
begin
  result := TFhirId(ObjectByIndex[index]);
end;

function TFhirIdList.ItemClass: TFslObjectClass;
begin
  result := TFhirId;
end;
function TFhirIdList.IndexOf(value: TFhirId): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirIdList.Insert(index: Integer): TFhirId;
begin
  result := TFhirId.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirIdList.InsertItem(index: Integer; value: TFhirId);
begin
  assert(value is TFhirId);
  Inherited Insert(index, value);
end;

function TFhirIdList.Item(index: Integer): TFhirId;
begin
  result := TFhirId(ObjectByIndex[index]);
end;

function TFhirIdList.Link: TFhirIdList;
begin
  result := TFhirIdList(inherited Link);
end;

procedure TFhirIdList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirIdList.SetItemByIndex(index: Integer; value: TFhirId);
begin
  assert(value is TFhirId);
  FhirIds[index] := value;
end;

procedure TFhirIdList.SetItemN(index: Integer; value: TFhirId);
begin
  assert(value is TFhirId);
  ObjectByIndex[index] := value;
end;

{ TFhirPositiveInt }

Constructor TFhirPositiveInt.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirPositiveInt.Destroy;
begin
  inherited;
end;

function TFhirPositiveInt.fhirType : string;
begin
  result := 'positiveInt';
end;

function TFhirPositiveInt.Link : TFhirPositiveInt;
begin
  result := TFhirPositiveInt(inherited Link);
end;

function TFhirPositiveInt.Clone : TFhirPositiveInt;
begin
  result := TFhirPositiveInt(inherited Clone);
end;

procedure TFhirPositiveInt.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('@value');
end;

{ TFhirPositiveIntListEnumerator }

Constructor TFhirPositiveIntListEnumerator.Create(list : TFhirPositiveIntList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPositiveIntListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPositiveIntListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPositiveIntListEnumerator.GetCurrent : TFhirPositiveInt;
begin
  Result := FList[FIndex];
end;

function TFhirPositiveIntListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPositiveIntList }
procedure TFhirPositiveIntList.AddItem(value: TFhirPositiveInt);
begin
  assert(value.ClassName = 'TFhirPositiveInt', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPositiveInt');
  add(value);
end;

procedure TFhirPositiveIntList.AddItem(value: String);
begin
  add(TFhirPositiveInt.create(value));
end;

function TFhirPositiveIntList.Append: TFhirPositiveInt;
begin
  result := TFhirPositiveInt.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPositiveIntList.ClearItems;
begin
  Clear;
end;

function TFhirPositiveIntList.GetEnumerator : TFhirPositiveIntListEnumerator;
begin
  result := TFhirPositiveIntListEnumerator.Create(self.link);
end;

function TFhirPositiveIntList.Clone: TFhirPositiveIntList;
begin
  result := TFhirPositiveIntList(inherited Clone);
end;

function TFhirPositiveIntList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPositiveIntList.GetItemN(index: Integer): TFhirPositiveInt;
begin
  result := TFhirPositiveInt(ObjectByIndex[index]);
end;

function TFhirPositiveIntList.ItemClass: TFslObjectClass;
begin
  result := TFhirPositiveInt;
end;
function TFhirPositiveIntList.IndexOf(value: TFhirPositiveInt): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPositiveIntList.Insert(index: Integer): TFhirPositiveInt;
begin
  result := TFhirPositiveInt.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPositiveIntList.InsertItem(index: Integer; value: TFhirPositiveInt);
begin
  assert(value is TFhirPositiveInt);
  Inherited Insert(index, value);
end;

function TFhirPositiveIntList.Item(index: Integer): TFhirPositiveInt;
begin
  result := TFhirPositiveInt(ObjectByIndex[index]);
end;

function TFhirPositiveIntList.Link: TFhirPositiveIntList;
begin
  result := TFhirPositiveIntList(inherited Link);
end;

procedure TFhirPositiveIntList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPositiveIntList.SetItemByIndex(index: Integer; value: TFhirPositiveInt);
begin
  assert(value is TFhirPositiveInt);
  FhirPositiveInts[index] := value;
end;

procedure TFhirPositiveIntList.SetItemN(index: Integer; value: TFhirPositiveInt);
begin
  assert(value is TFhirPositiveInt);
  ObjectByIndex[index] := value;
end;

{ TFhirExtension }

constructor TFhirExtension.Create;
begin
  inherited;
end;

destructor TFhirExtension.Destroy;
begin
  FUrl.free;
  FValue.free;
  inherited;
end;

procedure TFhirExtension.Assign(oSource : TFslObject);
begin
  inherited;
  urlElement := TFhirExtension(oSource).urlElement.Clone;
  value := TFhirExtension(oSource).value.Clone;
end;

procedure TFhirExtension.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'url') Then
     list.add(self.link, 'url', FUrl.Link);
  if (child_name = 'value[x]') or (child_name = 'value') Then
     list.add(self.link, 'value[x]', FValue.Link);
end;

procedure TFhirExtension.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'url', 'uri', false, TFhirUri, FUrl.Link));{2}
  oList.add(TFHIRProperty.create(self, 'value[x]', '*', false, TFhirType, FValue.Link));{2}
end;

function TFhirExtension.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'url') then
  begin
    UrlElement := asUri(propValue){5a};
    result := propValue;
  end
  else if (isMatchingName(propName, 'value', ['*'])) then
  begin
    Value := propValue as TFhirType{4};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirExtension.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirExtension.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'url') then result := TFhirUri.create() {5b}
  else if (isMatchingName(propName, 'value', ['*'])) then raise EFHIRException.create('Cannot make property Value'){4x}
  else result := inherited createPropertyValue(propName);
end;

function TFhirExtension.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'url') then result := 'uri'
  else if (propName = 'value[x]') then result := '*'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirExtension.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'url') then UrlElement := nil
  else if (isMatchingName(propName, 'value', ['*'])) then ValueElement := nil{4x}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirExtension.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'url') then UrlElement := asUri(new){5b}
  else if (isMatchingName(propName, 'value', ['*'])) then ValueElement := new as TFhirType{4x}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirExtension.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirExtension.fhirType : string;
begin
  result := 'Extension';
end;

function TFhirExtension.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FUrl) and isEmptyProp(FValue);
end;

function TFhirExtension.equals(other : TObject) : boolean; 
var
  o : TFhirExtension;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirExtension)) then
    result := false
  else
  begin
    o := TFhirExtension(other);
    result := compareDeep(urlElement, o.urlElement, true) and compareDeep(valueElement, o.valueElement, true);
  end;
end;

function TFhirExtension.Link : TFhirExtension;
begin
  result := TFhirExtension(inherited Link);
end;

function TFhirExtension.Clone : TFhirExtension;
begin
  result := TFhirExtension(inherited Clone);
end;

procedure TFhirExtension.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('value[x]');
end;

{ TFhirExtension }

Procedure TFhirExtension.SetUrl(value : TFhirUri);
begin
  FUrl.free;
  FUrl := value;
end;

Function TFhirExtension.GetUrlST : String;
begin
  if FUrl = nil then
    result := ''
  else
    result := FUrl.value;
end;

Procedure TFhirExtension.SetUrlST(value : String);
begin
  if value <> '' then
  begin
    if FUrl = nil then
      FUrl := TFhirUri.create;
    FUrl.value := value
  end
  else if FUrl <> nil then
    FUrl.value := '';
end;

Procedure TFhirExtension.SetValue(value : TFhirType);
begin
  FValue.free;
  FValue := value;
end;

function TFhirExtension.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FUrl.sizeInBytes);
  inc(result, FValue.sizeInBytes);
end;

{ TFhirExtensionListEnumerator }

Constructor TFhirExtensionListEnumerator.Create(list : TFhirExtensionList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirExtensionListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirExtensionListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirExtensionListEnumerator.GetCurrent : TFhirExtension;
begin
  Result := FList[FIndex];
end;

function TFhirExtensionListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirExtensionList }
procedure TFhirExtensionList.AddItem(value: TFhirExtension);
begin
  assert(value.ClassName = 'TFhirExtension', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirExtension');
  add(value);
end;

function TFhirExtensionList.Append: TFhirExtension;
begin
  result := TFhirExtension.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirExtensionList.ClearItems;
begin
  Clear;
end;

function TFhirExtensionList.GetEnumerator : TFhirExtensionListEnumerator;
begin
  result := TFhirExtensionListEnumerator.Create(self.link);
end;

function TFhirExtensionList.Clone: TFhirExtensionList;
begin
  result := TFhirExtensionList(inherited Clone);
end;

function TFhirExtensionList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirExtensionList.GetItemN(index: Integer): TFhirExtension;
begin
  result := TFhirExtension(ObjectByIndex[index]);
end;

function TFhirExtensionList.ItemClass: TFslObjectClass;
begin
  result := TFhirExtension;
end;
function TFhirExtensionList.IndexOf(value: TFhirExtension): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirExtensionList.Insert(index: Integer): TFhirExtension;
begin
  result := TFhirExtension.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirExtensionList.InsertItem(index: Integer; value: TFhirExtension);
begin
  assert(value is TFhirExtension);
  Inherited Insert(index, value);
end;

function TFhirExtensionList.Item(index: Integer): TFhirExtension;
begin
  result := TFhirExtension(ObjectByIndex[index]);
end;

function TFhirExtensionList.Link: TFhirExtensionList;
begin
  result := TFhirExtensionList(inherited Link);
end;

procedure TFhirExtensionList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirExtensionList.SetItemByIndex(index: Integer; value: TFhirExtension);
begin
  assert(value is TFhirExtension);
  FhirExtensions[index] := value;
end;

procedure TFhirExtensionList.SetItemN(index: Integer; value: TFhirExtension);
begin
  assert(value is TFhirExtension);
  ObjectByIndex[index] := value;
end;

function TFhirNarrativeStatusEnumListAsInteger(aSet : TFhirNarrativeStatusEnumList) : Integer;
var
  a : TFhirNarrativeStatusEnum;
begin
  result := 0;
  for a := low(TFhirNarrativeStatusEnum) to high(TFhirNarrativeStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNarrativeStatusEnumList(i : Integer) : TFhirNarrativeStatusEnumList;
var
  aLoop : TFhirNarrativeStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirNarrativeStatusEnum) to high(TFhirNarrativeStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

{ TFhirNarrative }

constructor TFhirNarrative.Create;
begin
  inherited;
end;

destructor TFhirNarrative.Destroy;
begin
  FStatus.free;
  FDiv_.free;
  inherited;
end;

procedure TFhirNarrative.Assign(oSource : TFslObject);
begin
  inherited;
  FStatus := TFhirNarrative(oSource).FStatus.Link;
  div_ := TFhirNarrative(oSource).div_.Clone;
end;

procedure TFhirNarrative.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'status') Then
     list.add(self.link, 'status', FStatus.Link);
  if (child_name = 'div') Then
     list.add(self.link, 'div', FDiv_.Link);
end;

procedure TFhirNarrative.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'status', 'code', false, TFHIREnum, FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'div', 'xhtml', false, TFhirXHtmlNode, FDiv_.Link));{2}
end;

function TFhirNarrative.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'status') then
  begin
    StatusElement := asEnum(SYSTEMS_TFhirNarrativeStatusEnum, CODES_TFhirNarrativeStatusEnum, propValue);
    result := propValue
  end
  else if (propName = 'div') then
  begin
    Div_Element := asXHtmlNode(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirNarrative.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirNarrative.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'div') then result := TFhirXHtmlNode.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirNarrative.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'status') then result := 'code'
  else if (propName = 'div') then result := 'xhtml'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirNarrative.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'status') then StatusElement := nil
  else if (propName = 'div') then Div_Element := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirNarrative.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'status') then StatusElement := asEnum(SYSTEMS_TFhirNarrativeStatusEnum, CODES_TFhirNarrativeStatusEnum, new){4}
  else if (propName = 'div') then Div_Element := asXHtmlNode(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirNarrative.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirNarrative.fhirType : string;
begin
  result := 'Narrative';
end;

function TFhirNarrative.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FStatus) and isEmptyProp(FDiv_);
end;

function TFhirNarrative.equals(other : TObject) : boolean; 
var
  o : TFhirNarrative;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirNarrative)) then
    result := false
  else
  begin
    o := TFhirNarrative(other);
    result := compareDeep(statusElement, o.statusElement, true) and compareDeep(div_Element, o.div_Element, true);
  end;
end;

function TFhirNarrative.Link : TFhirNarrative;
begin
  result := TFhirNarrative(inherited Link);
end;

function TFhirNarrative.Clone : TFhirNarrative;
begin
  result := TFhirNarrative(inherited Clone);
end;

procedure TFhirNarrative.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('status');
  fields.add('div');
end;

{ TFhirNarrative }

Procedure TFhirNarrative.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirNarrative.GetStatusST : TFhirNarrativeStatusEnum;
begin
  if FStatus = nil then
    result := TFhirNarrativeStatusEnum(0)
  else
    result := TFhirNarrativeStatusEnum(StringArrayIndexOfSensitive(CODES_TFhirNarrativeStatusEnum, FStatus.value));
end;

Procedure TFhirNarrative.SetStatusST(value : TFhirNarrativeStatusEnum);
begin
  if ord(value) = 0 then
    StatusElement := nil
  else
    StatusElement := TFhirEnum.create(SYSTEMS_TFhirNarrativeStatusEnum[value], CODES_TFhirNarrativeStatusEnum[value]);
end;

Procedure TFhirNarrative.SetDiv_(value : TFhirXHtmlNode);
begin
  FDiv_.free;
  FDiv_ := value;
end;

function TFhirNarrative.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStatus.sizeInBytes);
  inc(result, FDiv_.sizeInBytes);
end;

{ TFhirNarrativeListEnumerator }

Constructor TFhirNarrativeListEnumerator.Create(list : TFhirNarrativeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirNarrativeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirNarrativeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirNarrativeListEnumerator.GetCurrent : TFhirNarrative;
begin
  Result := FList[FIndex];
end;

function TFhirNarrativeListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirNarrativeList }
procedure TFhirNarrativeList.AddItem(value: TFhirNarrative);
begin
  assert(value.ClassName = 'TFhirNarrative', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirNarrative');
  add(value);
end;

function TFhirNarrativeList.Append: TFhirNarrative;
begin
  result := TFhirNarrative.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirNarrativeList.ClearItems;
begin
  Clear;
end;

function TFhirNarrativeList.GetEnumerator : TFhirNarrativeListEnumerator;
begin
  result := TFhirNarrativeListEnumerator.Create(self.link);
end;

function TFhirNarrativeList.Clone: TFhirNarrativeList;
begin
  result := TFhirNarrativeList(inherited Clone);
end;

function TFhirNarrativeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirNarrativeList.GetItemN(index: Integer): TFhirNarrative;
begin
  result := TFhirNarrative(ObjectByIndex[index]);
end;

function TFhirNarrativeList.ItemClass: TFslObjectClass;
begin
  result := TFhirNarrative;
end;
function TFhirNarrativeList.IndexOf(value: TFhirNarrative): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirNarrativeList.Insert(index: Integer): TFhirNarrative;
begin
  result := TFhirNarrative.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirNarrativeList.InsertItem(index: Integer; value: TFhirNarrative);
begin
  assert(value is TFhirNarrative);
  Inherited Insert(index, value);
end;

function TFhirNarrativeList.Item(index: Integer): TFhirNarrative;
begin
  result := TFhirNarrative(ObjectByIndex[index]);
end;

function TFhirNarrativeList.Link: TFhirNarrativeList;
begin
  result := TFhirNarrativeList(inherited Link);
end;

procedure TFhirNarrativeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirNarrativeList.SetItemByIndex(index: Integer; value: TFhirNarrative);
begin
  assert(value is TFhirNarrative);
  FhirNarratives[index] := value;
end;

procedure TFhirNarrativeList.SetItemN(index: Integer; value: TFhirNarrative);
begin
  assert(value is TFhirNarrative);
  ObjectByIndex[index] := value;
end;

function TFhirIdentifierUseEnumListAsInteger(aSet : TFhirIdentifierUseEnumList) : Integer;
var
  a : TFhirIdentifierUseEnum;
begin
  result := 0;
  for a := low(TFhirIdentifierUseEnum) to high(TFhirIdentifierUseEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIdentifierUseEnumList(i : Integer) : TFhirIdentifierUseEnumList;
var
  aLoop : TFhirIdentifierUseEnum;
begin
  result := [];
  for aLoop := low(TFhirIdentifierUseEnum) to high(TFhirIdentifierUseEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

{ TFhirIdentifier }

constructor TFhirIdentifier.Create;
begin
  inherited;
end;

destructor TFhirIdentifier.Destroy;
begin
  FUse.free;
  FType_.free;
  FSystem.free;
  FValue.free;
  FPeriod.free;
  FAssigner.free;
  inherited;
end;

procedure TFhirIdentifier.Assign(oSource : TFslObject);
begin
  inherited;
  FUse := TFhirIdentifier(oSource).FUse.Link;
  type_ := TFhirIdentifier(oSource).type_.Clone;
  systemElement := TFhirIdentifier(oSource).systemElement.Clone;
  valueElement := TFhirIdentifier(oSource).valueElement.Clone;
  period := TFhirIdentifier(oSource).period.Clone;
  assigner := TFhirIdentifier(oSource).assigner.Clone;
end;

procedure TFhirIdentifier.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'use') Then
     list.add(self.link, 'use', FUse.Link);
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'system') Then
     list.add(self.link, 'system', FSystem.Link);
  if (child_name = 'value') Then
     list.add(self.link, 'value', FValue.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
  if (child_name = 'assigner') Then
     list.add(self.link, 'assigner', FAssigner.Link);
end;

procedure TFhirIdentifier.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'use', 'code', false, TFHIREnum, FUse.Link));{1}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', false, TFhirCodeableConcept, FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'system', 'uri', false, TFhirUri, FSystem.Link));{2}
  oList.add(TFHIRProperty.create(self, 'value', 'string', false, TFhirString, FValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'assigner', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FAssigner.Link));{2}
end;

function TFhirIdentifier.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'use') then
  begin
    UseElement := asEnum(SYSTEMS_TFhirIdentifierUseEnum, CODES_TFhirIdentifierUseEnum, propValue);
    result := propValue
  end
  else if (propName = 'type') then
  begin
    Type_ := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'system') then
  begin
    SystemElement := asUri(propValue){5a};
    result := propValue;
  end
  else if (propName = 'value') then
  begin
    ValueElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else if (propName = 'assigner') then
  begin
    Assigner := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirIdentifier.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirIdentifier.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'type') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'system') then result := TFhirUri.create() {5b}
  else if (propName = 'value') then result := TFhirString.create() {5b}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else if (propName = 'assigner') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirIdentifier.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'use') then result := 'code'
  else if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'system') then result := 'uri'
  else if (propName = 'value') then result := 'string'
  else if (propName = 'period') then result := 'Period'
  else if (propName = 'assigner') then result := 'Reference'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirIdentifier.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'use') then UseElement := nil
  else if (propName = 'type') then Type_Element := nil
  else if (propName = 'system') then SystemElement := nil
  else if (propName = 'value') then ValueElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else if (propName = 'assigner') then AssignerElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirIdentifier.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'use') then UseElement := asEnum(SYSTEMS_TFhirIdentifierUseEnum, CODES_TFhirIdentifierUseEnum, new){4}
  else if (propName = 'type') then Type_Element := new as TFhirCodeableConcept{4}
  else if (propName = 'system') then SystemElement := asUri(new){5b}
  else if (propName = 'value') then ValueElement := asString(new){5b}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else if (propName = 'assigner') then AssignerElement := new as TFhirReference{TFhirOrganization}{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirIdentifier.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirIdentifier.fhirType : string;
begin
  result := 'Identifier';
end;

function TFhirIdentifier.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FUse) and isEmptyProp(FType_) and isEmptyProp(FSystem) and isEmptyProp(FValue) and isEmptyProp(FPeriod) and isEmptyProp(FAssigner);
end;

function TFhirIdentifier.equals(other : TObject) : boolean; 
var
  o : TFhirIdentifier;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirIdentifier)) then
    result := false
  else
  begin
    o := TFhirIdentifier(other);
    result := compareDeep(useElement, o.useElement, true) and compareDeep(type_Element, o.type_Element, true) and 
      compareDeep(systemElement, o.systemElement, true) and compareDeep(valueElement, o.valueElement, true) and 
      compareDeep(periodElement, o.periodElement, true) and compareDeep(assignerElement, o.assignerElement, true);
  end;
end;

function TFhirIdentifier.Link : TFhirIdentifier;
begin
  result := TFhirIdentifier(inherited Link);
end;

function TFhirIdentifier.Clone : TFhirIdentifier;
begin
  result := TFhirIdentifier(inherited Clone);
end;

procedure TFhirIdentifier.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('use');
  fields.add('type');
  fields.add('system');
  fields.add('value');
  fields.add('period');
  fields.add('assigner');
end;

{ TFhirIdentifier }

Procedure TFhirIdentifier.SetUse(value : TFhirEnum);
begin
  FUse.free;
  FUse := value;
end;

Function TFhirIdentifier.GetUseST : TFhirIdentifierUseEnum;
begin
  if FUse = nil then
    result := TFhirIdentifierUseEnum(0)
  else
    result := TFhirIdentifierUseEnum(StringArrayIndexOfSensitive(CODES_TFhirIdentifierUseEnum, FUse.value));
end;

Procedure TFhirIdentifier.SetUseST(value : TFhirIdentifierUseEnum);
begin
  if ord(value) = 0 then
    UseElement := nil
  else
    UseElement := TFhirEnum.create(SYSTEMS_TFhirIdentifierUseEnum[value], CODES_TFhirIdentifierUseEnum[value]);
end;

Procedure TFhirIdentifier.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirIdentifier.SetSystem(value : TFhirUri);
begin
  FSystem.free;
  FSystem := value;
end;

Function TFhirIdentifier.GetSystemST : String;
begin
  if FSystem = nil then
    result := ''
  else
    result := FSystem.value;
end;

Procedure TFhirIdentifier.SetSystemST(value : String);
begin
  if value <> '' then
  begin
    if FSystem = nil then
      FSystem := TFhirUri.create;
    FSystem.value := value
  end
  else if FSystem <> nil then
    FSystem.value := '';
end;

Procedure TFhirIdentifier.SetValue(value : TFhirString);
begin
  FValue.free;
  FValue := value;
end;

Function TFhirIdentifier.GetValueST : String;
begin
  if FValue = nil then
    result := ''
  else
    result := FValue.value;
end;

Procedure TFhirIdentifier.SetValueST(value : String);
begin
  if value <> '' then
  begin
    if FValue = nil then
      FValue := TFhirString.create;
    FValue.value := value
  end
  else if FValue <> nil then
    FValue.value := '';
end;

Procedure TFhirIdentifier.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Procedure TFhirIdentifier.SetAssigner(value : TFhirReference{TFhirOrganization});
begin
  FAssigner.free;
  FAssigner := value;
end;

function TFhirIdentifier.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FUse.sizeInBytes);
  inc(result, FType_.sizeInBytes);
  inc(result, FSystem.sizeInBytes);
  inc(result, FValue.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FAssigner.sizeInBytes);
end;

{ TFhirIdentifierListEnumerator }

Constructor TFhirIdentifierListEnumerator.Create(list : TFhirIdentifierList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirIdentifierListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirIdentifierListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirIdentifierListEnumerator.GetCurrent : TFhirIdentifier;
begin
  Result := FList[FIndex];
end;

function TFhirIdentifierListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirIdentifierList }
procedure TFhirIdentifierList.AddItem(value: TFhirIdentifier);
begin
  assert(value.ClassName = 'TFhirIdentifier', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirIdentifier');
  add(value);
end;

function TFhirIdentifierList.Append: TFhirIdentifier;
begin
  result := TFhirIdentifier.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirIdentifierList.ClearItems;
begin
  Clear;
end;

function TFhirIdentifierList.GetEnumerator : TFhirIdentifierListEnumerator;
begin
  result := TFhirIdentifierListEnumerator.Create(self.link);
end;

function TFhirIdentifierList.Clone: TFhirIdentifierList;
begin
  result := TFhirIdentifierList(inherited Clone);
end;

function TFhirIdentifierList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirIdentifierList.GetItemN(index: Integer): TFhirIdentifier;
begin
  result := TFhirIdentifier(ObjectByIndex[index]);
end;

function TFhirIdentifierList.ItemClass: TFslObjectClass;
begin
  result := TFhirIdentifier;
end;
function TFhirIdentifierList.IndexOf(value: TFhirIdentifier): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirIdentifierList.Insert(index: Integer): TFhirIdentifier;
begin
  result := TFhirIdentifier.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirIdentifierList.InsertItem(index: Integer; value: TFhirIdentifier);
begin
  assert(value is TFhirIdentifier);
  Inherited Insert(index, value);
end;

function TFhirIdentifierList.Item(index: Integer): TFhirIdentifier;
begin
  result := TFhirIdentifier(ObjectByIndex[index]);
end;

function TFhirIdentifierList.Link: TFhirIdentifierList;
begin
  result := TFhirIdentifierList(inherited Link);
end;

procedure TFhirIdentifierList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirIdentifierList.SetItemByIndex(index: Integer; value: TFhirIdentifier);
begin
  assert(value is TFhirIdentifier);
  FhirIdentifiers[index] := value;
end;

procedure TFhirIdentifierList.SetItemN(index: Integer; value: TFhirIdentifier);
begin
  assert(value is TFhirIdentifier);
  ObjectByIndex[index] := value;
end;

{ TFhirCoding }

constructor TFhirCoding.Create;
begin
  inherited;
end;

destructor TFhirCoding.Destroy;
begin
  FSystem.free;
  FVersion.free;
  FCode.free;
  FDisplay.free;
  FUserSelected.free;
  inherited;
end;

procedure TFhirCoding.Assign(oSource : TFslObject);
begin
  inherited;
  systemElement := TFhirCoding(oSource).systemElement.Clone;
  versionElement := TFhirCoding(oSource).versionElement.Clone;
  codeElement := TFhirCoding(oSource).codeElement.Clone;
  displayElement := TFhirCoding(oSource).displayElement.Clone;
  userSelectedElement := TFhirCoding(oSource).userSelectedElement.Clone;
end;

procedure TFhirCoding.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'system') Then
     list.add(self.link, 'system', FSystem.Link);
  if (child_name = 'version') Then
     list.add(self.link, 'version', FVersion.Link);
  if (child_name = 'code') Then
     list.add(self.link, 'code', FCode.Link);
  if (child_name = 'display') Then
     list.add(self.link, 'display', FDisplay.Link);
  if (child_name = 'userSelected') Then
     list.add(self.link, 'userSelected', FUserSelected.Link);
end;

procedure TFhirCoding.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'system', 'uri', false, TFhirUri, FSystem.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', false, TFhirString, FVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'code', false, TFhirCode, FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'display', 'string', false, TFhirString, FDisplay.Link));{2}
  oList.add(TFHIRProperty.create(self, 'userSelected', 'boolean', false, TFhirBoolean, FUserSelected.Link));{2}
end;

function TFhirCoding.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'system') then
  begin
    SystemElement := asUri(propValue){5a};
    result := propValue;
  end
  else if (propName = 'version') then
  begin
    VersionElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'code') then
  begin
    CodeElement := asCode(propValue);
    result := propValue;
  end
  else if (propName = 'display') then
  begin
    DisplayElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'userSelected') then
  begin
    UserSelectedElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirCoding.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirCoding.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'system') then result := TFhirUri.create() {5b}
  else if (propName = 'version') then result := TFhirString.create() {5b}
  else if (propName = 'code') then result := TFhirCode.create() {5b}
  else if (propName = 'display') then result := TFhirString.create() {5b}
  else if (propName = 'userSelected') then result := TFhirBoolean.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirCoding.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'system') then result := 'uri'
  else if (propName = 'version') then result := 'string'
  else if (propName = 'code') then result := 'code'
  else if (propName = 'display') then result := 'string'
  else if (propName = 'userSelected') then result := 'boolean'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirCoding.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'system') then SystemElement := nil
  else if (propName = 'version') then VersionElement := nil
  else if (propName = 'code') then CodeElement := nil
  else if (propName = 'display') then DisplayElement := nil
  else if (propName = 'userSelected') then UserSelectedElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirCoding.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'system') then SystemElement := asUri(new){5b}
  else if (propName = 'version') then VersionElement := asString(new){5b}
  else if (propName = 'code') then CodeElement := asCode(new){5b}
  else if (propName = 'display') then DisplayElement := asString(new){5b}
  else if (propName = 'userSelected') then UserSelectedElement := asBoolean(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirCoding.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirCoding.fhirType : string;
begin
  result := 'Coding';
end;

function TFhirCoding.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FSystem) and isEmptyProp(FVersion) and isEmptyProp(FCode) and isEmptyProp(FDisplay) and isEmptyProp(FUserSelected);
end;

function TFhirCoding.equals(other : TObject) : boolean; 
var
  o : TFhirCoding;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirCoding)) then
    result := false
  else
  begin
    o := TFhirCoding(other);
    result := compareDeep(systemElement, o.systemElement, true) and compareDeep(versionElement, o.versionElement, true) and 
      compareDeep(codeElement, o.codeElement, true) and compareDeep(displayElement, o.displayElement, true) and 
      compareDeep(userSelectedElement, o.userSelectedElement, true);
  end;
end;

function TFhirCoding.Link : TFhirCoding;
begin
  result := TFhirCoding(inherited Link);
end;

function TFhirCoding.Clone : TFhirCoding;
begin
  result := TFhirCoding(inherited Clone);
end;

procedure TFhirCoding.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('system');
  fields.add('version');
  fields.add('code');
  fields.add('display');
  fields.add('userSelected');
end;

{ TFhirCoding }

Procedure TFhirCoding.SetSystem(value : TFhirUri);
begin
  FSystem.free;
  FSystem := value;
end;

Function TFhirCoding.GetSystemST : String;
begin
  if FSystem = nil then
    result := ''
  else
    result := FSystem.value;
end;

Procedure TFhirCoding.SetSystemST(value : String);
begin
  if value <> '' then
  begin
    if FSystem = nil then
      FSystem := TFhirUri.create;
    FSystem.value := value
  end
  else if FSystem <> nil then
    FSystem.value := '';
end;

Procedure TFhirCoding.SetVersion(value : TFhirString);
begin
  FVersion.free;
  FVersion := value;
end;

Function TFhirCoding.GetVersionST : String;
begin
  if FVersion = nil then
    result := ''
  else
    result := FVersion.value;
end;

Procedure TFhirCoding.SetVersionST(value : String);
begin
  if value <> '' then
  begin
    if FVersion = nil then
      FVersion := TFhirString.create;
    FVersion.value := value
  end
  else if FVersion <> nil then
    FVersion.value := '';
end;

Procedure TFhirCoding.SetCode(value : TFhirCode);
begin
  FCode.free;
  FCode := value;
end;

Function TFhirCoding.GetCodeST : String;
begin
  if FCode = nil then
    result := ''
  else
    result := FCode.value;
end;

Procedure TFhirCoding.SetCodeST(value : String);
begin
  if value <> '' then
  begin
    if FCode = nil then
      FCode := TFhirCode.create;
    FCode.value := value
  end
  else if FCode <> nil then
    FCode.value := '';
end;

Procedure TFhirCoding.SetDisplay(value : TFhirString);
begin
  FDisplay.free;
  FDisplay := value;
end;

Function TFhirCoding.GetDisplayST : String;
begin
  if FDisplay = nil then
    result := ''
  else
    result := FDisplay.value;
end;

Procedure TFhirCoding.SetDisplayST(value : String);
begin
  if value <> '' then
  begin
    if FDisplay = nil then
      FDisplay := TFhirString.create;
    FDisplay.value := value
  end
  else if FDisplay <> nil then
    FDisplay.value := '';
end;

Procedure TFhirCoding.SetUserSelected(value : TFhirBoolean);
begin
  FUserSelected.free;
  FUserSelected := value;
end;

Function TFhirCoding.GetUserSelectedST : Boolean;
begin
  if FUserSelected = nil then
    result := false
  else
    result := FUserSelected.value;
end;

Procedure TFhirCoding.SetUserSelectedST(value : Boolean);
begin
  if FUserSelected = nil then
    FUserSelected := TFhirBoolean.create;
  FUserSelected.value := value
end;

function TFhirCoding.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSystem.sizeInBytes);
  inc(result, FVersion.sizeInBytes);
  inc(result, FCode.sizeInBytes);
  inc(result, FDisplay.sizeInBytes);
  inc(result, FUserSelected.sizeInBytes);
end;

{ TFhirCodingListEnumerator }

Constructor TFhirCodingListEnumerator.Create(list : TFhirCodingList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirCodingListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirCodingListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirCodingListEnumerator.GetCurrent : TFhirCoding;
begin
  Result := FList[FIndex];
end;

function TFhirCodingListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirCodingList }
procedure TFhirCodingList.AddItem(value: TFhirCoding);
begin
  assert(value.ClassName = 'TFhirCoding', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirCoding');
  add(value);
end;

function TFhirCodingList.Append: TFhirCoding;
begin
  result := TFhirCoding.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirCodingList.ClearItems;
begin
  Clear;
end;

function TFhirCodingList.GetEnumerator : TFhirCodingListEnumerator;
begin
  result := TFhirCodingListEnumerator.Create(self.link);
end;

function TFhirCodingList.Clone: TFhirCodingList;
begin
  result := TFhirCodingList(inherited Clone);
end;

function TFhirCodingList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirCodingList.GetItemN(index: Integer): TFhirCoding;
begin
  result := TFhirCoding(ObjectByIndex[index]);
end;

function TFhirCodingList.ItemClass: TFslObjectClass;
begin
  result := TFhirCoding;
end;
function TFhirCodingList.IndexOf(value: TFhirCoding): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirCodingList.Insert(index: Integer): TFhirCoding;
begin
  result := TFhirCoding.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirCodingList.InsertItem(index: Integer; value: TFhirCoding);
begin
  assert(value is TFhirCoding);
  Inherited Insert(index, value);
end;

function TFhirCodingList.Item(index: Integer): TFhirCoding;
begin
  result := TFhirCoding(ObjectByIndex[index]);
end;

function TFhirCodingList.Link: TFhirCodingList;
begin
  result := TFhirCodingList(inherited Link);
end;

procedure TFhirCodingList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirCodingList.SetItemByIndex(index: Integer; value: TFhirCoding);
begin
  assert(value is TFhirCoding);
  FhirCodings[index] := value;
end;

procedure TFhirCodingList.SetItemN(index: Integer; value: TFhirCoding);
begin
  assert(value is TFhirCoding);
  ObjectByIndex[index] := value;
end;

{ TFhirReference }

constructor TFhirReference.Create;
begin
  inherited;
end;

destructor TFhirReference.Destroy;
begin
  FReference.free;
  FDisplay.free;
  inherited;
end;

procedure TFhirReference.Assign(oSource : TFslObject);
begin
  inherited;
  referenceElement := TFhirReference(oSource).referenceElement.Clone;
  displayElement := TFhirReference(oSource).displayElement.Clone;
end;

procedure TFhirReference.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'reference') Then
     list.add(self.link, 'reference', FReference.Link);
  if (child_name = 'display') Then
     list.add(self.link, 'display', FDisplay.Link);
end;

procedure TFhirReference.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'reference', 'string', false, TFhirString, FReference.Link));{2}
  oList.add(TFHIRProperty.create(self, 'display', 'string', false, TFhirString, FDisplay.Link));{2}
end;

function TFhirReference.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'reference') then
  begin
    ReferenceElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'display') then
  begin
    DisplayElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirReference.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirReference.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'reference') then result := TFhirString.create() {5b}
  else if (propName = 'display') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirReference.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'reference') then result := 'string'
  else if (propName = 'display') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirReference.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'reference') then ReferenceElement := nil
  else if (propName = 'display') then DisplayElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirReference.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'reference') then ReferenceElement := asString(new){5b}
  else if (propName = 'display') then DisplayElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirReference.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirReference.fhirType : string;
begin
  result := 'Reference';
end;

function TFhirReference.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FReference) and isEmptyProp(FDisplay);
end;

function TFhirReference.equals(other : TObject) : boolean; 
var
  o : TFhirReference;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirReference)) then
    result := false
  else
  begin
    o := TFhirReference(other);
    result := compareDeep(referenceElement, o.referenceElement, true) and compareDeep(displayElement, o.displayElement, true);
  end;
end;

function TFhirReference.Link : TFhirReference;
begin
  result := TFhirReference(inherited Link);
end;

function TFhirReference.Clone : TFhirReference;
begin
  result := TFhirReference(inherited Clone);
end;

procedure TFhirReference.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('reference');
  fields.add('display');
end;

{ TFhirReference }

Procedure TFhirReference.SetReference(value : TFhirString);
begin
  FReference.free;
  FReference := value;
end;

Function TFhirReference.GetReferenceST : String;
begin
  if FReference = nil then
    result := ''
  else
    result := FReference.value;
end;

Procedure TFhirReference.SetReferenceST(value : String);
begin
  if value <> '' then
  begin
    if FReference = nil then
      FReference := TFhirString.create;
    FReference.value := value
  end
  else if FReference <> nil then
    FReference.value := '';
end;

Procedure TFhirReference.SetDisplay(value : TFhirString);
begin
  FDisplay.free;
  FDisplay := value;
end;

Function TFhirReference.GetDisplayST : String;
begin
  if FDisplay = nil then
    result := ''
  else
    result := FDisplay.value;
end;

Procedure TFhirReference.SetDisplayST(value : String);
begin
  if value <> '' then
  begin
    if FDisplay = nil then
      FDisplay := TFhirString.create;
    FDisplay.value := value
  end
  else if FDisplay <> nil then
    FDisplay.value := '';
end;

function TFhirReference.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReference.sizeInBytes);
  inc(result, FDisplay.sizeInBytes);
end;

{ TFhirReferenceListEnumerator }

Constructor TFhirReferenceListEnumerator.Create(list : TFhirReferenceList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirReferenceListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirReferenceListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirReferenceListEnumerator.GetCurrent : TFhirReference;
begin
  Result := FList[FIndex];
end;

function TFhirReferenceListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirReferenceList }
procedure TFhirReferenceList.AddItem(value: TFhirReference);
begin
  assert(value.ClassName = 'TFhirReference', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirReference');
  add(value);
end;

function TFhirReferenceList.Append: TFhirReference;
begin
  result := TFhirReference.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirReferenceList.ClearItems;
begin
  Clear;
end;

function TFhirReferenceList.GetEnumerator : TFhirReferenceListEnumerator;
begin
  result := TFhirReferenceListEnumerator.Create(self.link);
end;

function TFhirReferenceList.Clone: TFhirReferenceList;
begin
  result := TFhirReferenceList(inherited Clone);
end;

function TFhirReferenceList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirReferenceList.GetItemN(index: Integer): TFhirReference;
begin
  result := TFhirReference(ObjectByIndex[index]);
end;

function TFhirReferenceList.ItemClass: TFslObjectClass;
begin
  result := TFhirReference;
end;
function TFhirReferenceList.IndexOf(value: TFhirReference): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirReferenceList.Insert(index: Integer): TFhirReference;
begin
  result := TFhirReference.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirReferenceList.InsertItem(index: Integer; value: TFhirReference);
begin
  assert(value is TFhirReference);
  Inherited Insert(index, value);
end;

function TFhirReferenceList.Item(index: Integer): TFhirReference;
begin
  result := TFhirReference(ObjectByIndex[index]);
end;

function TFhirReferenceList.Link: TFhirReferenceList;
begin
  result := TFhirReferenceList(inherited Link);
end;

procedure TFhirReferenceList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirReferenceList.SetItemByIndex(index: Integer; value: TFhirReference);
begin
  assert(value is TFhirReference);
  FhirReferences[index] := value;
end;

procedure TFhirReferenceList.SetItemN(index: Integer; value: TFhirReference);
begin
  assert(value is TFhirReference);
  ObjectByIndex[index] := value;
end;

{ TFhirSignature }

constructor TFhirSignature.Create;
begin
  inherited;
end;

destructor TFhirSignature.Destroy;
begin
  FType_List.Free;
  FWhen.free;
  FWho.free;
  FContentType.free;
  FBlob.free;
  inherited;
end;

procedure TFhirSignature.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirSignature(oSource).FType_List = nil) then
  begin
    FType_List.free;
    FType_List := nil;
  end
  else
  begin
    if FType_List = nil then
      FType_List := TFhirCodingList.Create;
    FType_List.Assign(TFhirSignature(oSource).FType_List);
  end;
  whenElement := TFhirSignature(oSource).whenElement.Clone;
  who := TFhirSignature(oSource).who.Clone;
  contentTypeElement := TFhirSignature(oSource).contentTypeElement.Clone;
  blobElement := TFhirSignature(oSource).blobElement.Clone;
end;

procedure TFhirSignature.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'type') Then
    list.addAll(self, 'type', FType_List);
  if (child_name = 'when') Then
     list.add(self.link, 'when', FWhen.Link);
  if (child_name = 'who[x]') or (child_name = 'who') Then
     list.add(self.link, 'who[x]', FWho.Link);
  if (child_name = 'contentType') Then
     list.add(self.link, 'contentType', FContentType.Link);
  if (child_name = 'blob') Then
     list.add(self.link, 'blob', FBlob.Link);
end;

procedure TFhirSignature.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'Coding', true, TFhirCoding, FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'when', 'instant', false, TFhirInstant, FWhen.Link));{2}
  oList.add(TFHIRProperty.create(self, 'who[x]', 'uri|Reference(Practitioner|RelatedPerson|Patient|Device|Organization)', false, TFhirType, FWho.Link));{2}
  oList.add(TFHIRProperty.create(self, 'contentType', 'code', false, TFhirCode, FContentType.Link));{2}
  oList.add(TFHIRProperty.create(self, 'blob', 'base64Binary', false, TFhirBase64Binary, FBlob.Link));{2}
end;

function TFhirSignature.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'type') then
  begin
    Type_List.add(propValue as TFhirCoding){2a};
    result := propValue;
  end
  else if (propName = 'when') then
  begin
    WhenElement := asInstant(propValue){5a};
    result := propValue;
  end
  else if (isMatchingName(propName, 'who', ['Uri', 'Reference'])) then
  begin
    Who := propValue as TFhirType{4};
    result := propValue;
  end
  else if (propName = 'contentType') then
  begin
    ContentTypeElement := asCode(propValue);
    result := propValue;
  end
  else if (propName = 'blob') then
  begin
    BlobElement := asBase64Binary(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirSignature.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'type') then Type_List.insertItem(index, propValue as TFhirCoding){2a}
  else inherited;
end;

function TFhirSignature.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'type') then result := Type_List.new(){2}
  else if (propName = 'when') then result := TFhirInstant.create() {5b}
  else if (isMatchingName(propName, 'who', ['Uri', 'Reference'])) then raise EFHIRException.create('Cannot make property Who'){4x}
  else if (propName = 'contentType') then result := TFhirCode.create() {5b}
  else if (propName = 'blob') then result := TFhirBase64Binary.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirSignature.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'type') then result := 'Coding'
  else if (propName = 'when') then result := 'instant'
  else if (propName = 'who[x]') then result := 'uri|Reference'
  else if (propName = 'contentType') then result := 'code'
  else if (propName = 'blob') then result := 'base64Binary'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirSignature.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'type') then deletePropertyValue('type', Type_List, value) {2}
  else if (propName = 'when') then WhenElement := nil
  else if (isMatchingName(propName, 'who', ['Uri', 'Reference'])) then WhoElement := nil{4x}
  else if (propName = 'contentType') then ContentTypeElement := nil
  else if (propName = 'blob') then BlobElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirSignature.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'type') then replacePropertyValue('type', Type_List, existing, new) {2}
  else if (propName = 'when') then WhenElement := asInstant(new){5b}
  else if (isMatchingName(propName, 'who', ['Uri', 'Reference'])) then WhoElement := new as TFhirType{4x}
  else if (propName = 'contentType') then ContentTypeElement := asCode(new){5b}
  else if (propName = 'blob') then BlobElement := asBase64Binary(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirSignature.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'type') then Type_List.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirSignature.fhirType : string;
begin
  result := 'Signature';
end;

function TFhirSignature.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(Ftype_List) and isEmptyProp(FWhen) and isEmptyProp(FWho) and isEmptyProp(FContentType) and isEmptyProp(FBlob);
end;

function TFhirSignature.equals(other : TObject) : boolean; 
var
  o : TFhirSignature;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirSignature)) then
    result := false
  else
  begin
    o := TFhirSignature(other);
    result := compareDeep(type_List, o.type_List, true) and compareDeep(whenElement, o.whenElement, true) and 
      compareDeep(whoElement, o.whoElement, true) and compareDeep(contentTypeElement, o.contentTypeElement, true) and 
      compareDeep(blobElement, o.blobElement, true);
  end;
end;

function TFhirSignature.Link : TFhirSignature;
begin
  result := TFhirSignature(inherited Link);
end;

function TFhirSignature.Clone : TFhirSignature;
begin
  result := TFhirSignature(inherited Clone);
end;

procedure TFhirSignature.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('type');
  fields.add('when');
  fields.add('who[x]');
  fields.add('contentType');
  fields.add('blob');
end;

{ TFhirSignature }

Function TFhirSignature.GetType_List : TFhirCodingList;
begin
  if FType_List = nil then
    FType_List := TFhirCodingList.Create;
  result := FType_List;
end;

Function TFhirSignature.GetHasType_List : boolean;
begin
  result := (FType_List <> nil) and (FType_List.count > 0);
end;

Procedure TFhirSignature.SetWhen(value : TFhirInstant);
begin
  FWhen.free;
  FWhen := value;
end;

Function TFhirSignature.GetWhenST : TFslDateTime;
begin
  if FWhen = nil then
    result := TFslDateTime.makeNull
  else
    result := FWhen.value;
end;

Procedure TFhirSignature.SetWhenST(value : TFslDateTime);
begin
  if FWhen = nil then
    FWhen := TFhirInstant.create;
  FWhen.value := value
end;

Procedure TFhirSignature.SetWho(value : TFhirType);
begin
  FWho.free;
  FWho := value;
end;

Procedure TFhirSignature.SetContentType(value : TFhirCode);
begin
  FContentType.free;
  FContentType := value;
end;

Function TFhirSignature.GetContentTypeST : String;
begin
  if FContentType = nil then
    result := ''
  else
    result := FContentType.value;
end;

Procedure TFhirSignature.SetContentTypeST(value : String);
begin
  if value <> '' then
  begin
    if FContentType = nil then
      FContentType := TFhirCode.create;
    FContentType.value := value
  end
  else if FContentType <> nil then
    FContentType.value := '';
end;

Procedure TFhirSignature.SetBlob(value : TFhirBase64Binary);
begin
  FBlob.free;
  FBlob := value;
end;

Function TFhirSignature.GetBlobST : TBytes;
begin
  if FBlob = nil then
    result := nil
  else
    result := FBlob.value;
end;

Procedure TFhirSignature.SetBlobST(value : TBytes);
begin
  if value <> nil then
  begin
    if FBlob = nil then
      FBlob := TFhirBase64Binary.create;
    FBlob.value := value
  end
  else if FBlob <> nil then
    FBlob.value := nil;
end;

function TFhirSignature.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Ftype_List.sizeInBytes);
  inc(result, FWhen.sizeInBytes);
  inc(result, FWho.sizeInBytes);
  inc(result, FContentType.sizeInBytes);
  inc(result, FBlob.sizeInBytes);
end;

{ TFhirSignatureListEnumerator }

Constructor TFhirSignatureListEnumerator.Create(list : TFhirSignatureList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirSignatureListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirSignatureListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirSignatureListEnumerator.GetCurrent : TFhirSignature;
begin
  Result := FList[FIndex];
end;

function TFhirSignatureListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirSignatureList }
procedure TFhirSignatureList.AddItem(value: TFhirSignature);
begin
  assert(value.ClassName = 'TFhirSignature', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirSignature');
  add(value);
end;

function TFhirSignatureList.Append: TFhirSignature;
begin
  result := TFhirSignature.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSignatureList.ClearItems;
begin
  Clear;
end;

function TFhirSignatureList.GetEnumerator : TFhirSignatureListEnumerator;
begin
  result := TFhirSignatureListEnumerator.Create(self.link);
end;

function TFhirSignatureList.Clone: TFhirSignatureList;
begin
  result := TFhirSignatureList(inherited Clone);
end;

function TFhirSignatureList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirSignatureList.GetItemN(index: Integer): TFhirSignature;
begin
  result := TFhirSignature(ObjectByIndex[index]);
end;

function TFhirSignatureList.ItemClass: TFslObjectClass;
begin
  result := TFhirSignature;
end;
function TFhirSignatureList.IndexOf(value: TFhirSignature): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirSignatureList.Insert(index: Integer): TFhirSignature;
begin
  result := TFhirSignature.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSignatureList.InsertItem(index: Integer; value: TFhirSignature);
begin
  assert(value is TFhirSignature);
  Inherited Insert(index, value);
end;

function TFhirSignatureList.Item(index: Integer): TFhirSignature;
begin
  result := TFhirSignature(ObjectByIndex[index]);
end;

function TFhirSignatureList.Link: TFhirSignatureList;
begin
  result := TFhirSignatureList(inherited Link);
end;

procedure TFhirSignatureList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirSignatureList.SetItemByIndex(index: Integer; value: TFhirSignature);
begin
  assert(value is TFhirSignature);
  FhirSignatures[index] := value;
end;

procedure TFhirSignatureList.SetItemN(index: Integer; value: TFhirSignature);
begin
  assert(value is TFhirSignature);
  ObjectByIndex[index] := value;
end;

{ TFhirSampledData }

constructor TFhirSampledData.Create;
begin
  inherited;
end;

destructor TFhirSampledData.Destroy;
begin
  FOrigin.free;
  FPeriod.free;
  FFactor.free;
  FLowerLimit.free;
  FUpperLimit.free;
  FDimensions.free;
  FData.free;
  inherited;
end;

procedure TFhirSampledData.Assign(oSource : TFslObject);
begin
  inherited;
  origin := TFhirSampledData(oSource).origin.Clone;
  periodElement := TFhirSampledData(oSource).periodElement.Clone;
  factorElement := TFhirSampledData(oSource).factorElement.Clone;
  lowerLimitElement := TFhirSampledData(oSource).lowerLimitElement.Clone;
  upperLimitElement := TFhirSampledData(oSource).upperLimitElement.Clone;
  dimensionsElement := TFhirSampledData(oSource).dimensionsElement.Clone;
  dataElement := TFhirSampledData(oSource).dataElement.Clone;
end;

procedure TFhirSampledData.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'origin') Then
     list.add(self.link, 'origin', FOrigin.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
  if (child_name = 'factor') Then
     list.add(self.link, 'factor', FFactor.Link);
  if (child_name = 'lowerLimit') Then
     list.add(self.link, 'lowerLimit', FLowerLimit.Link);
  if (child_name = 'upperLimit') Then
     list.add(self.link, 'upperLimit', FUpperLimit.Link);
  if (child_name = 'dimensions') Then
     list.add(self.link, 'dimensions', FDimensions.Link);
  if (child_name = 'data') Then
     list.add(self.link, 'data', FData.Link);
end;

procedure TFhirSampledData.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'origin', 'Quantity', false, TFhirQuantity, FOrigin.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'decimal', false, TFhirDecimal, FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'factor', 'decimal', false, TFhirDecimal, FFactor.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lowerLimit', 'decimal', false, TFhirDecimal, FLowerLimit.Link));{2}
  oList.add(TFHIRProperty.create(self, 'upperLimit', 'decimal', false, TFhirDecimal, FUpperLimit.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dimensions', 'positiveInt', false, TFhirPositiveInt, FDimensions.Link));{2}
  oList.add(TFHIRProperty.create(self, 'data', 'string', false, TFhirString, FData.Link));{2}
end;

function TFhirSampledData.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'origin') then
  begin
    Origin := propValue as TFhirQuantity{4b};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    PeriodElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'factor') then
  begin
    FactorElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'lowerLimit') then
  begin
    LowerLimitElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'upperLimit') then
  begin
    UpperLimitElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'dimensions') then
  begin
    DimensionsElement := asPositiveInt(propValue){5a};
    result := propValue;
  end
  else if (propName = 'data') then
  begin
    DataElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirSampledData.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirSampledData.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'origin') then result := TFhirQuantity.create(){4b}
  else if (propName = 'period') then result := TFhirDecimal.create() {5b}
  else if (propName = 'factor') then result := TFhirDecimal.create() {5b}
  else if (propName = 'lowerLimit') then result := TFhirDecimal.create() {5b}
  else if (propName = 'upperLimit') then result := TFhirDecimal.create() {5b}
  else if (propName = 'dimensions') then result := TFhirPositiveInt.create() {5b}
  else if (propName = 'data') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirSampledData.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'origin') then result := 'Quantity'
  else if (propName = 'period') then result := 'decimal'
  else if (propName = 'factor') then result := 'decimal'
  else if (propName = 'lowerLimit') then result := 'decimal'
  else if (propName = 'upperLimit') then result := 'decimal'
  else if (propName = 'dimensions') then result := 'positiveInt'
  else if (propName = 'data') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirSampledData.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'origin') then OriginElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else if (propName = 'factor') then FactorElement := nil
  else if (propName = 'lowerLimit') then LowerLimitElement := nil
  else if (propName = 'upperLimit') then UpperLimitElement := nil
  else if (propName = 'dimensions') then DimensionsElement := nil
  else if (propName = 'data') then DataElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirSampledData.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'origin') then OriginElement := new as TFhirQuantity{4}
  else if (propName = 'period') then PeriodElement := asDecimal(new){5b}
  else if (propName = 'factor') then FactorElement := asDecimal(new){5b}
  else if (propName = 'lowerLimit') then LowerLimitElement := asDecimal(new){5b}
  else if (propName = 'upperLimit') then UpperLimitElement := asDecimal(new){5b}
  else if (propName = 'dimensions') then DimensionsElement := asPositiveInt(new){5b}
  else if (propName = 'data') then DataElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirSampledData.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirSampledData.fhirType : string;
begin
  result := 'SampledData';
end;

function TFhirSampledData.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FOrigin) and isEmptyProp(FPeriod) and isEmptyProp(FFactor) and isEmptyProp(FLowerLimit) and isEmptyProp(FUpperLimit) and isEmptyProp(FDimensions) and isEmptyProp(FData);
end;

function TFhirSampledData.equals(other : TObject) : boolean; 
var
  o : TFhirSampledData;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirSampledData)) then
    result := false
  else
  begin
    o := TFhirSampledData(other);
    result := compareDeep(originElement, o.originElement, true) and compareDeep(periodElement, o.periodElement, true) and 
      compareDeep(factorElement, o.factorElement, true) and compareDeep(lowerLimitElement, o.lowerLimitElement, true) and 
      compareDeep(upperLimitElement, o.upperLimitElement, true) and compareDeep(dimensionsElement, o.dimensionsElement, true) and 
      compareDeep(dataElement, o.dataElement, true);
  end;
end;

function TFhirSampledData.Link : TFhirSampledData;
begin
  result := TFhirSampledData(inherited Link);
end;

function TFhirSampledData.Clone : TFhirSampledData;
begin
  result := TFhirSampledData(inherited Clone);
end;

procedure TFhirSampledData.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('origin');
  fields.add('period');
  fields.add('factor');
  fields.add('lowerLimit');
  fields.add('upperLimit');
  fields.add('dimensions');
  fields.add('data');
end;

{ TFhirSampledData }

Procedure TFhirSampledData.SetOrigin(value : TFhirQuantity);
begin
  FOrigin.free;
  FOrigin := value;
end;

Procedure TFhirSampledData.SetPeriod(value : TFhirDecimal);
begin
  FPeriod.free;
  FPeriod := value;
end;

Function TFhirSampledData.GetPeriodST : String;
begin
  if FPeriod = nil then
    result := ''
  else
    result := FPeriod.value;
end;

Procedure TFhirSampledData.SetPeriodST(value : String);
begin
  if value <> '' then
  begin
    if FPeriod = nil then
      FPeriod := TFhirDecimal.create;
    FPeriod.value := value
  end
  else if FPeriod <> nil then
    FPeriod.value := '';
end;

Procedure TFhirSampledData.SetFactor(value : TFhirDecimal);
begin
  FFactor.free;
  FFactor := value;
end;

Function TFhirSampledData.GetFactorST : String;
begin
  if FFactor = nil then
    result := ''
  else
    result := FFactor.value;
end;

Procedure TFhirSampledData.SetFactorST(value : String);
begin
  if value <> '' then
  begin
    if FFactor = nil then
      FFactor := TFhirDecimal.create;
    FFactor.value := value
  end
  else if FFactor <> nil then
    FFactor.value := '';
end;

Procedure TFhirSampledData.SetLowerLimit(value : TFhirDecimal);
begin
  FLowerLimit.free;
  FLowerLimit := value;
end;

Function TFhirSampledData.GetLowerLimitST : String;
begin
  if FLowerLimit = nil then
    result := ''
  else
    result := FLowerLimit.value;
end;

Procedure TFhirSampledData.SetLowerLimitST(value : String);
begin
  if value <> '' then
  begin
    if FLowerLimit = nil then
      FLowerLimit := TFhirDecimal.create;
    FLowerLimit.value := value
  end
  else if FLowerLimit <> nil then
    FLowerLimit.value := '';
end;

Procedure TFhirSampledData.SetUpperLimit(value : TFhirDecimal);
begin
  FUpperLimit.free;
  FUpperLimit := value;
end;

Function TFhirSampledData.GetUpperLimitST : String;
begin
  if FUpperLimit = nil then
    result := ''
  else
    result := FUpperLimit.value;
end;

Procedure TFhirSampledData.SetUpperLimitST(value : String);
begin
  if value <> '' then
  begin
    if FUpperLimit = nil then
      FUpperLimit := TFhirDecimal.create;
    FUpperLimit.value := value
  end
  else if FUpperLimit <> nil then
    FUpperLimit.value := '';
end;

Procedure TFhirSampledData.SetDimensions(value : TFhirPositiveInt);
begin
  FDimensions.free;
  FDimensions := value;
end;

Function TFhirSampledData.GetDimensionsST : String;
begin
  if FDimensions = nil then
    result := ''
  else
    result := FDimensions.value;
end;

Procedure TFhirSampledData.SetDimensionsST(value : String);
begin
  if value <> '' then
  begin
    if FDimensions = nil then
      FDimensions := TFhirPositiveInt.create;
    FDimensions.value := value
  end
  else if FDimensions <> nil then
    FDimensions.value := '';
end;

Procedure TFhirSampledData.SetData(value : TFhirString);
begin
  FData.free;
  FData := value;
end;

Function TFhirSampledData.GetDataST : String;
begin
  if FData = nil then
    result := ''
  else
    result := FData.value;
end;

Procedure TFhirSampledData.SetDataST(value : String);
begin
  if value <> '' then
  begin
    if FData = nil then
      FData := TFhirString.create;
    FData.value := value
  end
  else if FData <> nil then
    FData.value := '';
end;

function TFhirSampledData.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOrigin.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FFactor.sizeInBytes);
  inc(result, FLowerLimit.sizeInBytes);
  inc(result, FUpperLimit.sizeInBytes);
  inc(result, FDimensions.sizeInBytes);
  inc(result, FData.sizeInBytes);
end;

{ TFhirSampledDataListEnumerator }

Constructor TFhirSampledDataListEnumerator.Create(list : TFhirSampledDataList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirSampledDataListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirSampledDataListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirSampledDataListEnumerator.GetCurrent : TFhirSampledData;
begin
  Result := FList[FIndex];
end;

function TFhirSampledDataListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirSampledDataList }
procedure TFhirSampledDataList.AddItem(value: TFhirSampledData);
begin
  assert(value.ClassName = 'TFhirSampledData', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirSampledData');
  add(value);
end;

function TFhirSampledDataList.Append: TFhirSampledData;
begin
  result := TFhirSampledData.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSampledDataList.ClearItems;
begin
  Clear;
end;

function TFhirSampledDataList.GetEnumerator : TFhirSampledDataListEnumerator;
begin
  result := TFhirSampledDataListEnumerator.Create(self.link);
end;

function TFhirSampledDataList.Clone: TFhirSampledDataList;
begin
  result := TFhirSampledDataList(inherited Clone);
end;

function TFhirSampledDataList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirSampledDataList.GetItemN(index: Integer): TFhirSampledData;
begin
  result := TFhirSampledData(ObjectByIndex[index]);
end;

function TFhirSampledDataList.ItemClass: TFslObjectClass;
begin
  result := TFhirSampledData;
end;
function TFhirSampledDataList.IndexOf(value: TFhirSampledData): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirSampledDataList.Insert(index: Integer): TFhirSampledData;
begin
  result := TFhirSampledData.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSampledDataList.InsertItem(index: Integer; value: TFhirSampledData);
begin
  assert(value is TFhirSampledData);
  Inherited Insert(index, value);
end;

function TFhirSampledDataList.Item(index: Integer): TFhirSampledData;
begin
  result := TFhirSampledData(ObjectByIndex[index]);
end;

function TFhirSampledDataList.Link: TFhirSampledDataList;
begin
  result := TFhirSampledDataList(inherited Link);
end;

procedure TFhirSampledDataList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirSampledDataList.SetItemByIndex(index: Integer; value: TFhirSampledData);
begin
  assert(value is TFhirSampledData);
  FhirSampledData[index] := value;
end;

procedure TFhirSampledDataList.SetItemN(index: Integer; value: TFhirSampledData);
begin
  assert(value is TFhirSampledData);
  ObjectByIndex[index] := value;
end;

{ TFhirPeriod }

constructor TFhirPeriod.Create;
begin
  inherited;
end;

destructor TFhirPeriod.Destroy;
begin
  FStart.free;
  FEnd_.free;
  inherited;
end;

procedure TFhirPeriod.Assign(oSource : TFslObject);
begin
  inherited;
  startElement := TFhirPeriod(oSource).startElement.Clone;
  end_Element := TFhirPeriod(oSource).end_Element.Clone;
end;

procedure TFhirPeriod.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'start') Then
     list.add(self.link, 'start', FStart.Link);
  if (child_name = 'end') Then
     list.add(self.link, 'end', FEnd_.Link);
end;

procedure TFhirPeriod.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'start', 'dateTime', false, TFhirDateTime, FStart.Link));{2}
  oList.add(TFHIRProperty.create(self, 'end', 'dateTime', false, TFhirDateTime, FEnd_.Link));{2}
end;

function TFhirPeriod.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'start') then
  begin
    StartElement := asDateTime(propValue){5a};
    result := propValue;
  end
  else if (propName = 'end') then
  begin
    End_Element := asDateTime(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirPeriod.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirPeriod.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'start') then result := TFhirDateTime.create() {5b}
  else if (propName = 'end') then result := TFhirDateTime.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPeriod.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'start') then result := 'dateTime'
  else if (propName = 'end') then result := 'dateTime'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPeriod.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'start') then StartElement := nil
  else if (propName = 'end') then End_Element := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPeriod.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'start') then StartElement := asDateTime(new){5b}
  else if (propName = 'end') then End_Element := asDateTime(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPeriod.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirPeriod.fhirType : string;
begin
  result := 'Period';
end;

function TFhirPeriod.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FStart) and isEmptyProp(FEnd_);
end;

function TFhirPeriod.equals(other : TObject) : boolean; 
var
  o : TFhirPeriod;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPeriod)) then
    result := false
  else
  begin
    o := TFhirPeriod(other);
    result := compareDeep(startElement, o.startElement, true) and compareDeep(end_Element, o.end_Element, true);
  end;
end;

function TFhirPeriod.Link : TFhirPeriod;
begin
  result := TFhirPeriod(inherited Link);
end;

function TFhirPeriod.Clone : TFhirPeriod;
begin
  result := TFhirPeriod(inherited Clone);
end;

procedure TFhirPeriod.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('start');
  fields.add('end');
end;

{ TFhirPeriod }

Procedure TFhirPeriod.SetStart(value : TFhirDateTime);
begin
  FStart.free;
  FStart := value;
end;

Function TFhirPeriod.GetStartST : TFslDateTime;
begin
  if FStart = nil then
    result := TFslDateTime.makeNull
  else
    result := FStart.value;
end;

Procedure TFhirPeriod.SetStartST(value : TFslDateTime);
begin
  if FStart = nil then
    FStart := TFhirDateTime.create;
  FStart.value := value
end;

Procedure TFhirPeriod.SetEnd_(value : TFhirDateTime);
begin
  FEnd_.free;
  FEnd_ := value;
end;

Function TFhirPeriod.GetEnd_ST : TFslDateTime;
begin
  if FEnd_ = nil then
    result := TFslDateTime.makeNull
  else
    result := FEnd_.value;
end;

Procedure TFhirPeriod.SetEnd_ST(value : TFslDateTime);
begin
  if FEnd_ = nil then
    FEnd_ := TFhirDateTime.create;
  FEnd_.value := value
end;

function TFhirPeriod.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStart.sizeInBytes);
  inc(result, FEnd_.sizeInBytes);
end;

{ TFhirPeriodListEnumerator }

Constructor TFhirPeriodListEnumerator.Create(list : TFhirPeriodList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPeriodListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPeriodListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPeriodListEnumerator.GetCurrent : TFhirPeriod;
begin
  Result := FList[FIndex];
end;

function TFhirPeriodListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPeriodList }
procedure TFhirPeriodList.AddItem(value: TFhirPeriod);
begin
  assert(value.ClassName = 'TFhirPeriod', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPeriod');
  add(value);
end;

function TFhirPeriodList.Append: TFhirPeriod;
begin
  result := TFhirPeriod.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPeriodList.ClearItems;
begin
  Clear;
end;

function TFhirPeriodList.GetEnumerator : TFhirPeriodListEnumerator;
begin
  result := TFhirPeriodListEnumerator.Create(self.link);
end;

function TFhirPeriodList.Clone: TFhirPeriodList;
begin
  result := TFhirPeriodList(inherited Clone);
end;

function TFhirPeriodList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPeriodList.GetItemN(index: Integer): TFhirPeriod;
begin
  result := TFhirPeriod(ObjectByIndex[index]);
end;

function TFhirPeriodList.ItemClass: TFslObjectClass;
begin
  result := TFhirPeriod;
end;
function TFhirPeriodList.IndexOf(value: TFhirPeriod): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPeriodList.Insert(index: Integer): TFhirPeriod;
begin
  result := TFhirPeriod.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPeriodList.InsertItem(index: Integer; value: TFhirPeriod);
begin
  assert(value is TFhirPeriod);
  Inherited Insert(index, value);
end;

function TFhirPeriodList.Item(index: Integer): TFhirPeriod;
begin
  result := TFhirPeriod(ObjectByIndex[index]);
end;

function TFhirPeriodList.Link: TFhirPeriodList;
begin
  result := TFhirPeriodList(inherited Link);
end;

procedure TFhirPeriodList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPeriodList.SetItemByIndex(index: Integer; value: TFhirPeriod);
begin
  assert(value is TFhirPeriod);
  FhirPeriods[index] := value;
end;

procedure TFhirPeriodList.SetItemN(index: Integer; value: TFhirPeriod);
begin
  assert(value is TFhirPeriod);
  ObjectByIndex[index] := value;
end;

function TFhirQuantityComparatorEnumListAsInteger(aSet : TFhirQuantityComparatorEnumList) : Integer;
var
  a : TFhirQuantityComparatorEnum;
begin
  result := 0;
  for a := low(TFhirQuantityComparatorEnum) to high(TFhirQuantityComparatorEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuantityComparatorEnumList(i : Integer) : TFhirQuantityComparatorEnumList;
var
  aLoop : TFhirQuantityComparatorEnum;
begin
  result := [];
  for aLoop := low(TFhirQuantityComparatorEnum) to high(TFhirQuantityComparatorEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

{ TFhirQuantity }

constructor TFhirQuantity.Create;
begin
  inherited;
end;

destructor TFhirQuantity.Destroy;
begin
  FValue.free;
  FComparator.free;
  FUnit_.free;
  FSystem.free;
  FCode.free;
  inherited;
end;

procedure TFhirQuantity.Assign(oSource : TFslObject);
begin
  inherited;
  valueElement := TFhirQuantity(oSource).valueElement.Clone;
  FComparator := TFhirQuantity(oSource).FComparator.Link;
  unit_Element := TFhirQuantity(oSource).unit_Element.Clone;
  systemElement := TFhirQuantity(oSource).systemElement.Clone;
  codeElement := TFhirQuantity(oSource).codeElement.Clone;
end;

procedure TFhirQuantity.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'value') Then
     list.add(self.link, 'value', FValue.Link);
  if (child_name = 'comparator') Then
     list.add(self.link, 'comparator', FComparator.Link);
  if (child_name = 'unit') Then
     list.add(self.link, 'unit', FUnit_.Link);
  if (child_name = 'system') Then
     list.add(self.link, 'system', FSystem.Link);
  if (child_name = 'code') Then
     list.add(self.link, 'code', FCode.Link);
end;

procedure TFhirQuantity.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'decimal', false, TFhirDecimal, FValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comparator', 'code', false, TFHIREnum, FComparator.Link));{1}
  oList.add(TFHIRProperty.create(self, 'unit', 'string', false, TFhirString, FUnit_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'system', 'uri', false, TFhirUri, FSystem.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'code', false, TFhirCode, FCode.Link));{2}
end;

function TFhirQuantity.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'value') then
  begin
    ValueElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'comparator') then
  begin
    ComparatorElement := asEnum(SYSTEMS_TFhirQuantityComparatorEnum, CODES_TFhirQuantityComparatorEnum, propValue);
    result := propValue
  end
  else if (propName = 'unit') then
  begin
    Unit_Element := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'system') then
  begin
    SystemElement := asUri(propValue){5a};
    result := propValue;
  end
  else if (propName = 'code') then
  begin
    CodeElement := asCode(propValue);
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirQuantity.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirQuantity.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'value') then result := TFhirDecimal.create() {5b}
  else if (propName = 'unit') then result := TFhirString.create() {5b}
  else if (propName = 'system') then result := TFhirUri.create() {5b}
  else if (propName = 'code') then result := TFhirCode.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirQuantity.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'value') then result := 'decimal'
  else if (propName = 'comparator') then result := 'code'
  else if (propName = 'unit') then result := 'string'
  else if (propName = 'system') then result := 'uri'
  else if (propName = 'code') then result := 'code'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirQuantity.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'value') then ValueElement := nil
  else if (propName = 'comparator') then ComparatorElement := nil
  else if (propName = 'unit') then Unit_Element := nil
  else if (propName = 'system') then SystemElement := nil
  else if (propName = 'code') then CodeElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirQuantity.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'value') then ValueElement := asDecimal(new){5b}
  else if (propName = 'comparator') then ComparatorElement := asEnum(SYSTEMS_TFhirQuantityComparatorEnum, CODES_TFhirQuantityComparatorEnum, new){4}
  else if (propName = 'unit') then Unit_Element := asString(new){5b}
  else if (propName = 'system') then SystemElement := asUri(new){5b}
  else if (propName = 'code') then CodeElement := asCode(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirQuantity.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirQuantity.fhirType : string;
begin
  result := 'Quantity';
end;

function TFhirQuantity.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FValue) and isEmptyProp(FComparator) and isEmptyProp(FUnit_) and isEmptyProp(FSystem) and isEmptyProp(FCode);
end;

function TFhirQuantity.equals(other : TObject) : boolean; 
var
  o : TFhirQuantity;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirQuantity)) then
    result := false
  else
  begin
    o := TFhirQuantity(other);
    result := compareDeep(valueElement, o.valueElement, true) and compareDeep(comparatorElement, o.comparatorElement, true) and 
      compareDeep(unit_Element, o.unit_Element, true) and compareDeep(systemElement, o.systemElement, true) and 
      compareDeep(codeElement, o.codeElement, true);
  end;
end;

function TFhirQuantity.Link : TFhirQuantity;
begin
  result := TFhirQuantity(inherited Link);
end;

function TFhirQuantity.Clone : TFhirQuantity;
begin
  result := TFhirQuantity(inherited Clone);
end;

procedure TFhirQuantity.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('value');
  fields.add('comparator');
  fields.add('unit');
  fields.add('system');
  fields.add('code');
end;

{ TFhirQuantity }

Procedure TFhirQuantity.SetValue(value : TFhirDecimal);
begin
  FValue.free;
  FValue := value;
end;

Function TFhirQuantity.GetValueST : String;
begin
  if FValue = nil then
    result := ''
  else
    result := FValue.value;
end;

Procedure TFhirQuantity.SetValueST(value : String);
begin
  if value <> '' then
  begin
    if FValue = nil then
      FValue := TFhirDecimal.create;
    FValue.value := value
  end
  else if FValue <> nil then
    FValue.value := '';
end;

Procedure TFhirQuantity.SetComparator(value : TFhirEnum);
begin
  FComparator.free;
  FComparator := value;
end;

Function TFhirQuantity.GetComparatorST : TFhirQuantityComparatorEnum;
begin
  if FComparator = nil then
    result := TFhirQuantityComparatorEnum(0)
  else
    result := TFhirQuantityComparatorEnum(StringArrayIndexOfSensitive(CODES_TFhirQuantityComparatorEnum, FComparator.value));
end;

Procedure TFhirQuantity.SetComparatorST(value : TFhirQuantityComparatorEnum);
begin
  if ord(value) = 0 then
    ComparatorElement := nil
  else
    ComparatorElement := TFhirEnum.create(SYSTEMS_TFhirQuantityComparatorEnum[value], CODES_TFhirQuantityComparatorEnum[value]);
end;

Procedure TFhirQuantity.SetUnit_(value : TFhirString);
begin
  FUnit_.free;
  FUnit_ := value;
end;

Function TFhirQuantity.GetUnit_ST : String;
begin
  if FUnit_ = nil then
    result := ''
  else
    result := FUnit_.value;
end;

Procedure TFhirQuantity.SetUnit_ST(value : String);
begin
  if value <> '' then
  begin
    if FUnit_ = nil then
      FUnit_ := TFhirString.create;
    FUnit_.value := value
  end
  else if FUnit_ <> nil then
    FUnit_.value := '';
end;

Procedure TFhirQuantity.SetSystem(value : TFhirUri);
begin
  FSystem.free;
  FSystem := value;
end;

Function TFhirQuantity.GetSystemST : String;
begin
  if FSystem = nil then
    result := ''
  else
    result := FSystem.value;
end;

Procedure TFhirQuantity.SetSystemST(value : String);
begin
  if value <> '' then
  begin
    if FSystem = nil then
      FSystem := TFhirUri.create;
    FSystem.value := value
  end
  else if FSystem <> nil then
    FSystem.value := '';
end;

Procedure TFhirQuantity.SetCode(value : TFhirCode);
begin
  FCode.free;
  FCode := value;
end;

Function TFhirQuantity.GetCodeST : String;
begin
  if FCode = nil then
    result := ''
  else
    result := FCode.value;
end;

Procedure TFhirQuantity.SetCodeST(value : String);
begin
  if value <> '' then
  begin
    if FCode = nil then
      FCode := TFhirCode.create;
    FCode.value := value
  end
  else if FCode <> nil then
    FCode.value := '';
end;

function TFhirQuantity.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FValue.sizeInBytes);
  inc(result, FComparator.sizeInBytes);
  inc(result, FUnit_.sizeInBytes);
  inc(result, FSystem.sizeInBytes);
  inc(result, FCode.sizeInBytes);
end;

{ TFhirQuantityListEnumerator }

Constructor TFhirQuantityListEnumerator.Create(list : TFhirQuantityList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirQuantityListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirQuantityListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirQuantityListEnumerator.GetCurrent : TFhirQuantity;
begin
  Result := FList[FIndex];
end;

function TFhirQuantityListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirQuantityList }
procedure TFhirQuantityList.AddItem(value: TFhirQuantity);
begin
  assert(value.ClassName = 'TFhirQuantity', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirQuantity');
  add(value);
end;

function TFhirQuantityList.Append: TFhirQuantity;
begin
  result := TFhirQuantity.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirQuantityList.ClearItems;
begin
  Clear;
end;

function TFhirQuantityList.GetEnumerator : TFhirQuantityListEnumerator;
begin
  result := TFhirQuantityListEnumerator.Create(self.link);
end;

function TFhirQuantityList.Clone: TFhirQuantityList;
begin
  result := TFhirQuantityList(inherited Clone);
end;

function TFhirQuantityList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirQuantityList.GetItemN(index: Integer): TFhirQuantity;
begin
  result := TFhirQuantity(ObjectByIndex[index]);
end;

function TFhirQuantityList.ItemClass: TFslObjectClass;
begin
  result := TFhirQuantity;
end;
function TFhirQuantityList.IndexOf(value: TFhirQuantity): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirQuantityList.Insert(index: Integer): TFhirQuantity;
begin
  result := TFhirQuantity.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirQuantityList.InsertItem(index: Integer; value: TFhirQuantity);
begin
  assert(value is TFhirQuantity);
  Inherited Insert(index, value);
end;

function TFhirQuantityList.Item(index: Integer): TFhirQuantity;
begin
  result := TFhirQuantity(ObjectByIndex[index]);
end;

function TFhirQuantityList.Link: TFhirQuantityList;
begin
  result := TFhirQuantityList(inherited Link);
end;

procedure TFhirQuantityList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirQuantityList.SetItemByIndex(index: Integer; value: TFhirQuantity);
begin
  assert(value is TFhirQuantity);
  FhirQuantities[index] := value;
end;

procedure TFhirQuantityList.SetItemN(index: Integer; value: TFhirQuantity);
begin
  assert(value is TFhirQuantity);
  ObjectByIndex[index] := value;
end;

{ TFhirAttachment }

constructor TFhirAttachment.Create;
begin
  inherited;
end;

destructor TFhirAttachment.Destroy;
begin
  FContentType.free;
  FLanguage.free;
  FData.free;
  FUrl.free;
  FSize.free;
  FHash.free;
  FTitle.free;
  FCreation.free;
  inherited;
end;

procedure TFhirAttachment.Assign(oSource : TFslObject);
begin
  inherited;
  contentTypeElement := TFhirAttachment(oSource).contentTypeElement.Clone;
  languageElement := TFhirAttachment(oSource).languageElement.Clone;
  dataElement := TFhirAttachment(oSource).dataElement.Clone;
  urlElement := TFhirAttachment(oSource).urlElement.Clone;
  sizeElement := TFhirAttachment(oSource).sizeElement.Clone;
  hashElement := TFhirAttachment(oSource).hashElement.Clone;
  titleElement := TFhirAttachment(oSource).titleElement.Clone;
  creationElement := TFhirAttachment(oSource).creationElement.Clone;
end;

procedure TFhirAttachment.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'contentType') Then
     list.add(self.link, 'contentType', FContentType.Link);
  if (child_name = 'language') Then
     list.add(self.link, 'language', FLanguage.Link);
  if (child_name = 'data') Then
     list.add(self.link, 'data', FData.Link);
  if (child_name = 'url') Then
     list.add(self.link, 'url', FUrl.Link);
  if (child_name = 'size') Then
     list.add(self.link, 'size', FSize.Link);
  if (child_name = 'hash') Then
     list.add(self.link, 'hash', FHash.Link);
  if (child_name = 'title') Then
     list.add(self.link, 'title', FTitle.Link);
  if (child_name = 'creation') Then
     list.add(self.link, 'creation', FCreation.Link);
end;

procedure TFhirAttachment.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'contentType', 'code', false, TFhirCode, FContentType.Link));{2}
  oList.add(TFHIRProperty.create(self, 'language', 'code', false, TFhirCode, FLanguage.Link));{2}
  oList.add(TFHIRProperty.create(self, 'data', 'base64Binary', false, TFhirBase64Binary, FData.Link));{2}
  oList.add(TFHIRProperty.create(self, 'url', 'uri', false, TFhirUri, FUrl.Link));{2}
  oList.add(TFHIRProperty.create(self, 'size', 'unsignedInt', false, TFhirUnsignedInt, FSize.Link));{2}
  oList.add(TFHIRProperty.create(self, 'hash', 'base64Binary', false, TFhirBase64Binary, FHash.Link));{2}
  oList.add(TFHIRProperty.create(self, 'title', 'string', false, TFhirString, FTitle.Link));{2}
  oList.add(TFHIRProperty.create(self, 'creation', 'dateTime', false, TFhirDateTime, FCreation.Link));{2}
end;

function TFhirAttachment.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'contentType') then
  begin
    ContentTypeElement := asCode(propValue);
    result := propValue;
  end
  else if (propName = 'language') then
  begin
    LanguageElement := asCode(propValue);
    result := propValue;
  end
  else if (propName = 'data') then
  begin
    DataElement := asBase64Binary(propValue){5a};
    result := propValue;
  end
  else if (propName = 'url') then
  begin
    UrlElement := asUri(propValue){5a};
    result := propValue;
  end
  else if (propName = 'size') then
  begin
    SizeElement := asUnsignedInt(propValue){5a};
    result := propValue;
  end
  else if (propName = 'hash') then
  begin
    HashElement := asBase64Binary(propValue){5a};
    result := propValue;
  end
  else if (propName = 'title') then
  begin
    TitleElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'creation') then
  begin
    CreationElement := asDateTime(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirAttachment.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirAttachment.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'contentType') then result := TFhirCode.create() {5b}
  else if (propName = 'language') then result := TFhirCode.create() {5b}
  else if (propName = 'data') then result := TFhirBase64Binary.create() {5b}
  else if (propName = 'url') then result := TFhirUri.create() {5b}
  else if (propName = 'size') then result := TFhirUnsignedInt.create() {5b}
  else if (propName = 'hash') then result := TFhirBase64Binary.create() {5b}
  else if (propName = 'title') then result := TFhirString.create() {5b}
  else if (propName = 'creation') then result := TFhirDateTime.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirAttachment.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'contentType') then result := 'code'
  else if (propName = 'language') then result := 'code'
  else if (propName = 'data') then result := 'base64Binary'
  else if (propName = 'url') then result := 'uri'
  else if (propName = 'size') then result := 'unsignedInt'
  else if (propName = 'hash') then result := 'base64Binary'
  else if (propName = 'title') then result := 'string'
  else if (propName = 'creation') then result := 'dateTime'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirAttachment.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'contentType') then ContentTypeElement := nil
  else if (propName = 'language') then LanguageElement := nil
  else if (propName = 'data') then DataElement := nil
  else if (propName = 'url') then UrlElement := nil
  else if (propName = 'size') then SizeElement := nil
  else if (propName = 'hash') then HashElement := nil
  else if (propName = 'title') then TitleElement := nil
  else if (propName = 'creation') then CreationElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirAttachment.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'contentType') then ContentTypeElement := asCode(new){5b}
  else if (propName = 'language') then LanguageElement := asCode(new){5b}
  else if (propName = 'data') then DataElement := asBase64Binary(new){5b}
  else if (propName = 'url') then UrlElement := asUri(new){5b}
  else if (propName = 'size') then SizeElement := asUnsignedInt(new){5b}
  else if (propName = 'hash') then HashElement := asBase64Binary(new){5b}
  else if (propName = 'title') then TitleElement := asString(new){5b}
  else if (propName = 'creation') then CreationElement := asDateTime(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirAttachment.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirAttachment.fhirType : string;
begin
  result := 'Attachment';
end;

function TFhirAttachment.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FContentType) and isEmptyProp(FLanguage) and isEmptyProp(FData) and isEmptyProp(FUrl) and isEmptyProp(FSize) and isEmptyProp(FHash) and isEmptyProp(FTitle) and isEmptyProp(FCreation);
end;

function TFhirAttachment.equals(other : TObject) : boolean; 
var
  o : TFhirAttachment;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirAttachment)) then
    result := false
  else
  begin
    o := TFhirAttachment(other);
    result := compareDeep(contentTypeElement, o.contentTypeElement, true) and compareDeep(languageElement, o.languageElement, true) and 
      compareDeep(dataElement, o.dataElement, true) and compareDeep(urlElement, o.urlElement, true) and 
      compareDeep(sizeElement, o.sizeElement, true) and compareDeep(hashElement, o.hashElement, true) and 
      compareDeep(titleElement, o.titleElement, true) and compareDeep(creationElement, o.creationElement, true);
  end;
end;

function TFhirAttachment.Link : TFhirAttachment;
begin
  result := TFhirAttachment(inherited Link);
end;

function TFhirAttachment.Clone : TFhirAttachment;
begin
  result := TFhirAttachment(inherited Clone);
end;

procedure TFhirAttachment.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('contentType');
  fields.add('language');
  fields.add('data');
  fields.add('url');
  fields.add('size');
  fields.add('hash');
  fields.add('title');
  fields.add('creation');
end;

{ TFhirAttachment }

Procedure TFhirAttachment.SetContentType(value : TFhirCode);
begin
  FContentType.free;
  FContentType := value;
end;

Function TFhirAttachment.GetContentTypeST : String;
begin
  if FContentType = nil then
    result := ''
  else
    result := FContentType.value;
end;

Procedure TFhirAttachment.SetContentTypeST(value : String);
begin
  if value <> '' then
  begin
    if FContentType = nil then
      FContentType := TFhirCode.create;
    FContentType.value := value
  end
  else if FContentType <> nil then
    FContentType.value := '';
end;

Procedure TFhirAttachment.SetLanguage(value : TFhirCode);
begin
  FLanguage.free;
  FLanguage := value;
end;

Function TFhirAttachment.GetLanguageST : String;
begin
  if FLanguage = nil then
    result := ''
  else
    result := FLanguage.value;
end;

Procedure TFhirAttachment.SetLanguageST(value : String);
begin
  if value <> '' then
  begin
    if FLanguage = nil then
      FLanguage := TFhirCode.create;
    FLanguage.value := value
  end
  else if FLanguage <> nil then
    FLanguage.value := '';
end;

Procedure TFhirAttachment.SetData(value : TFhirBase64Binary);
begin
  FData.free;
  FData := value;
end;

Function TFhirAttachment.GetDataST : TBytes;
begin
  if FData = nil then
    result := nil
  else
    result := FData.value;
end;

Procedure TFhirAttachment.SetDataST(value : TBytes);
begin
  if value <> nil then
  begin
    if FData = nil then
      FData := TFhirBase64Binary.create;
    FData.value := value
  end
  else if FData <> nil then
    FData.value := nil;
end;

Procedure TFhirAttachment.SetUrl(value : TFhirUri);
begin
  FUrl.free;
  FUrl := value;
end;

Function TFhirAttachment.GetUrlST : String;
begin
  if FUrl = nil then
    result := ''
  else
    result := FUrl.value;
end;

Procedure TFhirAttachment.SetUrlST(value : String);
begin
  if value <> '' then
  begin
    if FUrl = nil then
      FUrl := TFhirUri.create;
    FUrl.value := value
  end
  else if FUrl <> nil then
    FUrl.value := '';
end;

Procedure TFhirAttachment.SetSize(value : TFhirUnsignedInt);
begin
  FSize.free;
  FSize := value;
end;

Function TFhirAttachment.GetSizeST : String;
begin
  if FSize = nil then
    result := ''
  else
    result := FSize.value;
end;

Procedure TFhirAttachment.SetSizeST(value : String);
begin
  if value <> '' then
  begin
    if FSize = nil then
      FSize := TFhirUnsignedInt.create;
    FSize.value := value
  end
  else if FSize <> nil then
    FSize.value := '';
end;

Procedure TFhirAttachment.SetHash(value : TFhirBase64Binary);
begin
  FHash.free;
  FHash := value;
end;

Function TFhirAttachment.GetHashST : TBytes;
begin
  if FHash = nil then
    result := nil
  else
    result := FHash.value;
end;

Procedure TFhirAttachment.SetHashST(value : TBytes);
begin
  if value <> nil then
  begin
    if FHash = nil then
      FHash := TFhirBase64Binary.create;
    FHash.value := value
  end
  else if FHash <> nil then
    FHash.value := nil;
end;

Procedure TFhirAttachment.SetTitle(value : TFhirString);
begin
  FTitle.free;
  FTitle := value;
end;

Function TFhirAttachment.GetTitleST : String;
begin
  if FTitle = nil then
    result := ''
  else
    result := FTitle.value;
end;

Procedure TFhirAttachment.SetTitleST(value : String);
begin
  if value <> '' then
  begin
    if FTitle = nil then
      FTitle := TFhirString.create;
    FTitle.value := value
  end
  else if FTitle <> nil then
    FTitle.value := '';
end;

Procedure TFhirAttachment.SetCreation(value : TFhirDateTime);
begin
  FCreation.free;
  FCreation := value;
end;

Function TFhirAttachment.GetCreationST : TFslDateTime;
begin
  if FCreation = nil then
    result := TFslDateTime.makeNull
  else
    result := FCreation.value;
end;

Procedure TFhirAttachment.SetCreationST(value : TFslDateTime);
begin
  if FCreation = nil then
    FCreation := TFhirDateTime.create;
  FCreation.value := value
end;

function TFhirAttachment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FContentType.sizeInBytes);
  inc(result, FLanguage.sizeInBytes);
  inc(result, FData.sizeInBytes);
  inc(result, FUrl.sizeInBytes);
  inc(result, FSize.sizeInBytes);
  inc(result, FHash.sizeInBytes);
  inc(result, FTitle.sizeInBytes);
  inc(result, FCreation.sizeInBytes);
end;

{ TFhirAttachmentListEnumerator }

Constructor TFhirAttachmentListEnumerator.Create(list : TFhirAttachmentList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirAttachmentListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirAttachmentListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirAttachmentListEnumerator.GetCurrent : TFhirAttachment;
begin
  Result := FList[FIndex];
end;

function TFhirAttachmentListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirAttachmentList }
procedure TFhirAttachmentList.AddItem(value: TFhirAttachment);
begin
  assert(value.ClassName = 'TFhirAttachment', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirAttachment');
  add(value);
end;

function TFhirAttachmentList.Append: TFhirAttachment;
begin
  result := TFhirAttachment.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirAttachmentList.ClearItems;
begin
  Clear;
end;

function TFhirAttachmentList.GetEnumerator : TFhirAttachmentListEnumerator;
begin
  result := TFhirAttachmentListEnumerator.Create(self.link);
end;

function TFhirAttachmentList.Clone: TFhirAttachmentList;
begin
  result := TFhirAttachmentList(inherited Clone);
end;

function TFhirAttachmentList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirAttachmentList.GetItemN(index: Integer): TFhirAttachment;
begin
  result := TFhirAttachment(ObjectByIndex[index]);
end;

function TFhirAttachmentList.ItemClass: TFslObjectClass;
begin
  result := TFhirAttachment;
end;
function TFhirAttachmentList.IndexOf(value: TFhirAttachment): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirAttachmentList.Insert(index: Integer): TFhirAttachment;
begin
  result := TFhirAttachment.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirAttachmentList.InsertItem(index: Integer; value: TFhirAttachment);
begin
  assert(value is TFhirAttachment);
  Inherited Insert(index, value);
end;

function TFhirAttachmentList.Item(index: Integer): TFhirAttachment;
begin
  result := TFhirAttachment(ObjectByIndex[index]);
end;

function TFhirAttachmentList.Link: TFhirAttachmentList;
begin
  result := TFhirAttachmentList(inherited Link);
end;

procedure TFhirAttachmentList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirAttachmentList.SetItemByIndex(index: Integer; value: TFhirAttachment);
begin
  assert(value is TFhirAttachment);
  FhirAttachments[index] := value;
end;

procedure TFhirAttachmentList.SetItemN(index: Integer; value: TFhirAttachment);
begin
  assert(value is TFhirAttachment);
  ObjectByIndex[index] := value;
end;

{ TFhirRatio }

constructor TFhirRatio.Create;
begin
  inherited;
end;

destructor TFhirRatio.Destroy;
begin
  FNumerator.free;
  FDenominator.free;
  inherited;
end;

procedure TFhirRatio.Assign(oSource : TFslObject);
begin
  inherited;
  numerator := TFhirRatio(oSource).numerator.Clone;
  denominator := TFhirRatio(oSource).denominator.Clone;
end;

procedure TFhirRatio.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'numerator') Then
     list.add(self.link, 'numerator', FNumerator.Link);
  if (child_name = 'denominator') Then
     list.add(self.link, 'denominator', FDenominator.Link);
end;

procedure TFhirRatio.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'numerator', 'Quantity', false, TFhirQuantity, FNumerator.Link));{2}
  oList.add(TFHIRProperty.create(self, 'denominator', 'Quantity', false, TFhirQuantity, FDenominator.Link));{2}
end;

function TFhirRatio.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'numerator') then
  begin
    Numerator := propValue as TFhirQuantity{4b};
    result := propValue;
  end
  else if (propName = 'denominator') then
  begin
    Denominator := propValue as TFhirQuantity{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirRatio.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirRatio.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'numerator') then result := TFhirQuantity.create(){4b}
  else if (propName = 'denominator') then result := TFhirQuantity.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirRatio.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'numerator') then result := 'Quantity'
  else if (propName = 'denominator') then result := 'Quantity'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirRatio.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'numerator') then NumeratorElement := nil
  else if (propName = 'denominator') then DenominatorElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirRatio.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'numerator') then NumeratorElement := new as TFhirQuantity{4}
  else if (propName = 'denominator') then DenominatorElement := new as TFhirQuantity{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirRatio.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirRatio.fhirType : string;
begin
  result := 'Ratio';
end;

function TFhirRatio.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FNumerator) and isEmptyProp(FDenominator);
end;

function TFhirRatio.equals(other : TObject) : boolean; 
var
  o : TFhirRatio;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirRatio)) then
    result := false
  else
  begin
    o := TFhirRatio(other);
    result := compareDeep(numeratorElement, o.numeratorElement, true) and compareDeep(denominatorElement, o.denominatorElement, true);
  end;
end;

function TFhirRatio.Link : TFhirRatio;
begin
  result := TFhirRatio(inherited Link);
end;

function TFhirRatio.Clone : TFhirRatio;
begin
  result := TFhirRatio(inherited Clone);
end;

procedure TFhirRatio.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('numerator');
  fields.add('denominator');
end;

{ TFhirRatio }

Procedure TFhirRatio.SetNumerator(value : TFhirQuantity);
begin
  FNumerator.free;
  FNumerator := value;
end;

Procedure TFhirRatio.SetDenominator(value : TFhirQuantity);
begin
  FDenominator.free;
  FDenominator := value;
end;

function TFhirRatio.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FNumerator.sizeInBytes);
  inc(result, FDenominator.sizeInBytes);
end;

{ TFhirRatioListEnumerator }

Constructor TFhirRatioListEnumerator.Create(list : TFhirRatioList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirRatioListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirRatioListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirRatioListEnumerator.GetCurrent : TFhirRatio;
begin
  Result := FList[FIndex];
end;

function TFhirRatioListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirRatioList }
procedure TFhirRatioList.AddItem(value: TFhirRatio);
begin
  assert(value.ClassName = 'TFhirRatio', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirRatio');
  add(value);
end;

function TFhirRatioList.Append: TFhirRatio;
begin
  result := TFhirRatio.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirRatioList.ClearItems;
begin
  Clear;
end;

function TFhirRatioList.GetEnumerator : TFhirRatioListEnumerator;
begin
  result := TFhirRatioListEnumerator.Create(self.link);
end;

function TFhirRatioList.Clone: TFhirRatioList;
begin
  result := TFhirRatioList(inherited Clone);
end;

function TFhirRatioList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirRatioList.GetItemN(index: Integer): TFhirRatio;
begin
  result := TFhirRatio(ObjectByIndex[index]);
end;

function TFhirRatioList.ItemClass: TFslObjectClass;
begin
  result := TFhirRatio;
end;
function TFhirRatioList.IndexOf(value: TFhirRatio): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirRatioList.Insert(index: Integer): TFhirRatio;
begin
  result := TFhirRatio.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirRatioList.InsertItem(index: Integer; value: TFhirRatio);
begin
  assert(value is TFhirRatio);
  Inherited Insert(index, value);
end;

function TFhirRatioList.Item(index: Integer): TFhirRatio;
begin
  result := TFhirRatio(ObjectByIndex[index]);
end;

function TFhirRatioList.Link: TFhirRatioList;
begin
  result := TFhirRatioList(inherited Link);
end;

procedure TFhirRatioList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirRatioList.SetItemByIndex(index: Integer; value: TFhirRatio);
begin
  assert(value is TFhirRatio);
  FhirRatios[index] := value;
end;

procedure TFhirRatioList.SetItemN(index: Integer; value: TFhirRatio);
begin
  assert(value is TFhirRatio);
  ObjectByIndex[index] := value;
end;

{ TFhirRange }

constructor TFhirRange.Create;
begin
  inherited;
end;

destructor TFhirRange.Destroy;
begin
  FLow.free;
  FHigh.free;
  inherited;
end;

procedure TFhirRange.Assign(oSource : TFslObject);
begin
  inherited;
  low := TFhirRange(oSource).low.Clone;
  high := TFhirRange(oSource).high.Clone;
end;

procedure TFhirRange.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'low') Then
     list.add(self.link, 'low', FLow.Link);
  if (child_name = 'high') Then
     list.add(self.link, 'high', FHigh.Link);
end;

procedure TFhirRange.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'low', 'Quantity', false, TFhirQuantity, FLow.Link));{2}
  oList.add(TFHIRProperty.create(self, 'high', 'Quantity', false, TFhirQuantity, FHigh.Link));{2}
end;

function TFhirRange.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'low') then
  begin
    Low := propValue as TFhirQuantity{4b};
    result := propValue;
  end
  else if (propName = 'high') then
  begin
    High := propValue as TFhirQuantity{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirRange.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirRange.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'low') then result := TFhirQuantity.create(){4b}
  else if (propName = 'high') then result := TFhirQuantity.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirRange.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'low') then result := 'Quantity'
  else if (propName = 'high') then result := 'Quantity'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirRange.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'low') then LowElement := nil
  else if (propName = 'high') then HighElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirRange.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'low') then LowElement := new as TFhirQuantity{4}
  else if (propName = 'high') then HighElement := new as TFhirQuantity{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirRange.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirRange.fhirType : string;
begin
  result := 'Range';
end;

function TFhirRange.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FLow) and isEmptyProp(FHigh);
end;

function TFhirRange.equals(other : TObject) : boolean; 
var
  o : TFhirRange;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirRange)) then
    result := false
  else
  begin
    o := TFhirRange(other);
    result := compareDeep(lowElement, o.lowElement, true) and compareDeep(highElement, o.highElement, true);
  end;
end;

function TFhirRange.Link : TFhirRange;
begin
  result := TFhirRange(inherited Link);
end;

function TFhirRange.Clone : TFhirRange;
begin
  result := TFhirRange(inherited Clone);
end;

procedure TFhirRange.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('low');
  fields.add('high');
end;

{ TFhirRange }

Procedure TFhirRange.SetLow(value : TFhirQuantity);
begin
  FLow.free;
  FLow := value;
end;

Procedure TFhirRange.SetHigh(value : TFhirQuantity);
begin
  FHigh.free;
  FHigh := value;
end;

function TFhirRange.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLow.sizeInBytes);
  inc(result, FHigh.sizeInBytes);
end;

{ TFhirRangeListEnumerator }

Constructor TFhirRangeListEnumerator.Create(list : TFhirRangeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirRangeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirRangeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirRangeListEnumerator.GetCurrent : TFhirRange;
begin
  Result := FList[FIndex];
end;

function TFhirRangeListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirRangeList }
procedure TFhirRangeList.AddItem(value: TFhirRange);
begin
  assert(value.ClassName = 'TFhirRange', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirRange');
  add(value);
end;

function TFhirRangeList.Append: TFhirRange;
begin
  result := TFhirRange.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirRangeList.ClearItems;
begin
  Clear;
end;

function TFhirRangeList.GetEnumerator : TFhirRangeListEnumerator;
begin
  result := TFhirRangeListEnumerator.Create(self.link);
end;

function TFhirRangeList.Clone: TFhirRangeList;
begin
  result := TFhirRangeList(inherited Clone);
end;

function TFhirRangeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirRangeList.GetItemN(index: Integer): TFhirRange;
begin
  result := TFhirRange(ObjectByIndex[index]);
end;

function TFhirRangeList.ItemClass: TFslObjectClass;
begin
  result := TFhirRange;
end;
function TFhirRangeList.IndexOf(value: TFhirRange): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirRangeList.Insert(index: Integer): TFhirRange;
begin
  result := TFhirRange.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirRangeList.InsertItem(index: Integer; value: TFhirRange);
begin
  assert(value is TFhirRange);
  Inherited Insert(index, value);
end;

function TFhirRangeList.Item(index: Integer): TFhirRange;
begin
  result := TFhirRange(ObjectByIndex[index]);
end;

function TFhirRangeList.Link: TFhirRangeList;
begin
  result := TFhirRangeList(inherited Link);
end;

procedure TFhirRangeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirRangeList.SetItemByIndex(index: Integer; value: TFhirRange);
begin
  assert(value is TFhirRange);
  FhirRanges[index] := value;
end;

procedure TFhirRangeList.SetItemN(index: Integer; value: TFhirRange);
begin
  assert(value is TFhirRange);
  ObjectByIndex[index] := value;
end;

{ TFhirAnnotation }

constructor TFhirAnnotation.Create;
begin
  inherited;
end;

destructor TFhirAnnotation.Destroy;
begin
  FAuthor.free;
  FTime.free;
  FText.free;
  inherited;
end;

procedure TFhirAnnotation.Assign(oSource : TFslObject);
begin
  inherited;
  author := TFhirAnnotation(oSource).author.Clone;
  timeElement := TFhirAnnotation(oSource).timeElement.Clone;
  textElement := TFhirAnnotation(oSource).textElement.Clone;
end;

procedure TFhirAnnotation.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'author[x]') or (child_name = 'author') Then
     list.add(self.link, 'author[x]', FAuthor.Link);
  if (child_name = 'time') Then
     list.add(self.link, 'time', FTime.Link);
  if (child_name = 'text') Then
     list.add(self.link, 'text', FText.Link);
end;

procedure TFhirAnnotation.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'author[x]', 'Reference(Practitioner|Patient|RelatedPerson)|string', false, TFhirType, FAuthor.Link));{2}
  oList.add(TFHIRProperty.create(self, 'time', 'dateTime', false, TFhirDateTime, FTime.Link));{2}
  oList.add(TFHIRProperty.create(self, 'text', 'string', false, TFhirString, FText.Link));{2}
end;

function TFhirAnnotation.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (isMatchingName(propName, 'author', ['Reference', 'String'])) then
  begin
    Author := propValue as TFhirType{4};
    result := propValue;
  end
  else if (propName = 'time') then
  begin
    TimeElement := asDateTime(propValue){5a};
    result := propValue;
  end
  else if (propName = 'text') then
  begin
    TextElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirAnnotation.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirAnnotation.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (isMatchingName(propName, 'author', ['Reference', 'String'])) then raise EFHIRException.create('Cannot make property Author'){4x}
  else if (propName = 'time') then result := TFhirDateTime.create() {5b}
  else if (propName = 'text') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirAnnotation.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'author[x]') then result := 'Reference|string'
  else if (propName = 'time') then result := 'dateTime'
  else if (propName = 'text') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirAnnotation.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (isMatchingName(propName, 'author', ['Reference', 'String'])) then AuthorElement := nil{4x}
  else if (propName = 'time') then TimeElement := nil
  else if (propName = 'text') then TextElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirAnnotation.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (isMatchingName(propName, 'author', ['Reference', 'String'])) then AuthorElement := new as TFhirType{4x}
  else if (propName = 'time') then TimeElement := asDateTime(new){5b}
  else if (propName = 'text') then TextElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirAnnotation.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirAnnotation.fhirType : string;
begin
  result := 'Annotation';
end;

function TFhirAnnotation.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FAuthor) and isEmptyProp(FTime) and isEmptyProp(FText);
end;

function TFhirAnnotation.equals(other : TObject) : boolean; 
var
  o : TFhirAnnotation;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirAnnotation)) then
    result := false
  else
  begin
    o := TFhirAnnotation(other);
    result := compareDeep(authorElement, o.authorElement, true) and compareDeep(timeElement, o.timeElement, true) and 
      compareDeep(textElement, o.textElement, true);
  end;
end;

function TFhirAnnotation.Link : TFhirAnnotation;
begin
  result := TFhirAnnotation(inherited Link);
end;

function TFhirAnnotation.Clone : TFhirAnnotation;
begin
  result := TFhirAnnotation(inherited Clone);
end;

procedure TFhirAnnotation.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('author[x]');
  fields.add('time');
  fields.add('text');
end;

{ TFhirAnnotation }

Procedure TFhirAnnotation.SetAuthor(value : TFhirType);
begin
  FAuthor.free;
  FAuthor := value;
end;

Procedure TFhirAnnotation.SetTime(value : TFhirDateTime);
begin
  FTime.free;
  FTime := value;
end;

Function TFhirAnnotation.GetTimeST : TFslDateTime;
begin
  if FTime = nil then
    result := TFslDateTime.makeNull
  else
    result := FTime.value;
end;

Procedure TFhirAnnotation.SetTimeST(value : TFslDateTime);
begin
  if FTime = nil then
    FTime := TFhirDateTime.create;
  FTime.value := value
end;

Procedure TFhirAnnotation.SetText(value : TFhirString);
begin
  FText.free;
  FText := value;
end;

Function TFhirAnnotation.GetTextST : String;
begin
  if FText = nil then
    result := ''
  else
    result := FText.value;
end;

Procedure TFhirAnnotation.SetTextST(value : String);
begin
  if value <> '' then
  begin
    if FText = nil then
      FText := TFhirString.create;
    FText.value := value
  end
  else if FText <> nil then
    FText.value := '';
end;

function TFhirAnnotation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAuthor.sizeInBytes);
  inc(result, FTime.sizeInBytes);
  inc(result, FText.sizeInBytes);
end;

{ TFhirAnnotationListEnumerator }

Constructor TFhirAnnotationListEnumerator.Create(list : TFhirAnnotationList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirAnnotationListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirAnnotationListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirAnnotationListEnumerator.GetCurrent : TFhirAnnotation;
begin
  Result := FList[FIndex];
end;

function TFhirAnnotationListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirAnnotationList }
procedure TFhirAnnotationList.AddItem(value: TFhirAnnotation);
begin
  assert(value.ClassName = 'TFhirAnnotation', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirAnnotation');
  add(value);
end;

function TFhirAnnotationList.Append: TFhirAnnotation;
begin
  result := TFhirAnnotation.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirAnnotationList.ClearItems;
begin
  Clear;
end;

function TFhirAnnotationList.GetEnumerator : TFhirAnnotationListEnumerator;
begin
  result := TFhirAnnotationListEnumerator.Create(self.link);
end;

function TFhirAnnotationList.Clone: TFhirAnnotationList;
begin
  result := TFhirAnnotationList(inherited Clone);
end;

function TFhirAnnotationList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirAnnotationList.GetItemN(index: Integer): TFhirAnnotation;
begin
  result := TFhirAnnotation(ObjectByIndex[index]);
end;

function TFhirAnnotationList.ItemClass: TFslObjectClass;
begin
  result := TFhirAnnotation;
end;
function TFhirAnnotationList.IndexOf(value: TFhirAnnotation): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirAnnotationList.Insert(index: Integer): TFhirAnnotation;
begin
  result := TFhirAnnotation.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirAnnotationList.InsertItem(index: Integer; value: TFhirAnnotation);
begin
  assert(value is TFhirAnnotation);
  Inherited Insert(index, value);
end;

function TFhirAnnotationList.Item(index: Integer): TFhirAnnotation;
begin
  result := TFhirAnnotation(ObjectByIndex[index]);
end;

function TFhirAnnotationList.Link: TFhirAnnotationList;
begin
  result := TFhirAnnotationList(inherited Link);
end;

procedure TFhirAnnotationList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirAnnotationList.SetItemByIndex(index: Integer; value: TFhirAnnotation);
begin
  assert(value is TFhirAnnotation);
  FhirAnnotations[index] := value;
end;

procedure TFhirAnnotationList.SetItemN(index: Integer; value: TFhirAnnotation);
begin
  assert(value is TFhirAnnotation);
  ObjectByIndex[index] := value;
end;

{ TFhirCodeableConcept }

constructor TFhirCodeableConcept.Create;
begin
  inherited;
end;

destructor TFhirCodeableConcept.Destroy;
begin
  FCodingList.Free;
  FText.free;
  inherited;
end;

procedure TFhirCodeableConcept.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirCodeableConcept(oSource).FCodingList = nil) then
  begin
    FCodingList.free;
    FCodingList := nil;
  end
  else
  begin
    if FCodingList = nil then
      FCodingList := TFhirCodingList.Create;
    FCodingList.Assign(TFhirCodeableConcept(oSource).FCodingList);
  end;
  textElement := TFhirCodeableConcept(oSource).textElement.Clone;
end;

procedure TFhirCodeableConcept.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'coding') Then
    list.addAll(self, 'coding', FCodingList);
  if (child_name = 'text') Then
     list.add(self.link, 'text', FText.Link);
end;

procedure TFhirCodeableConcept.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'coding', 'Coding', true, TFhirCoding, FCodingList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'text', 'string', false, TFhirString, FText.Link));{2}
end;

function TFhirCodeableConcept.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'coding') then
  begin
    CodingList.add(propValue as TFhirCoding){2a};
    result := propValue;
  end
  else if (propName = 'text') then
  begin
    TextElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirCodeableConcept.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'coding') then CodingList.insertItem(index, propValue as TFhirCoding){2a}
  else inherited;
end;

function TFhirCodeableConcept.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'coding') then result := CodingList.new(){2}
  else if (propName = 'text') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirCodeableConcept.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'coding') then result := 'Coding'
  else if (propName = 'text') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirCodeableConcept.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'coding') then deletePropertyValue('coding', CodingList, value) {2}
  else if (propName = 'text') then TextElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirCodeableConcept.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'coding') then replacePropertyValue('coding', CodingList, existing, new) {2}
  else if (propName = 'text') then TextElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirCodeableConcept.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'coding') then CodingList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirCodeableConcept.fhirType : string;
begin
  result := 'CodeableConcept';
end;

function TFhirCodeableConcept.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FcodingList) and isEmptyProp(FText);
end;

function TFhirCodeableConcept.equals(other : TObject) : boolean; 
var
  o : TFhirCodeableConcept;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirCodeableConcept)) then
    result := false
  else
  begin
    o := TFhirCodeableConcept(other);
    result := compareDeep(codingList, o.codingList, true) and compareDeep(textElement, o.textElement, true);
  end;
end;

function TFhirCodeableConcept.Link : TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept(inherited Link);
end;

function TFhirCodeableConcept.Clone : TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept(inherited Clone);
end;

procedure TFhirCodeableConcept.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('coding');
  fields.add('text');
end;

{ TFhirCodeableConcept }

Function TFhirCodeableConcept.GetCodingList : TFhirCodingList;
begin
  if FCodingList = nil then
    FCodingList := TFhirCodingList.Create;
  result := FCodingList;
end;

Function TFhirCodeableConcept.GetHasCodingList : boolean;
begin
  result := (FCodingList <> nil) and (FCodingList.count > 0);
end;

Procedure TFhirCodeableConcept.SetText(value : TFhirString);
begin
  FText.free;
  FText := value;
end;

Function TFhirCodeableConcept.GetTextST : String;
begin
  if FText = nil then
    result := ''
  else
    result := FText.value;
end;

Procedure TFhirCodeableConcept.SetTextST(value : String);
begin
  if value <> '' then
  begin
    if FText = nil then
      FText := TFhirString.create;
    FText.value := value
  end
  else if FText <> nil then
    FText.value := '';
end;

function TFhirCodeableConcept.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FcodingList.sizeInBytes);
  inc(result, FText.sizeInBytes);
end;

{ TFhirCodeableConceptListEnumerator }

Constructor TFhirCodeableConceptListEnumerator.Create(list : TFhirCodeableConceptList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirCodeableConceptListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirCodeableConceptListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirCodeableConceptListEnumerator.GetCurrent : TFhirCodeableConcept;
begin
  Result := FList[FIndex];
end;

function TFhirCodeableConceptListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirCodeableConceptList }
procedure TFhirCodeableConceptList.AddItem(value: TFhirCodeableConcept);
begin
  assert(value.ClassName = 'TFhirCodeableConcept', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirCodeableConcept');
  add(value);
end;

function TFhirCodeableConceptList.Append: TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirCodeableConceptList.ClearItems;
begin
  Clear;
end;

function TFhirCodeableConceptList.GetEnumerator : TFhirCodeableConceptListEnumerator;
begin
  result := TFhirCodeableConceptListEnumerator.Create(self.link);
end;

function TFhirCodeableConceptList.Clone: TFhirCodeableConceptList;
begin
  result := TFhirCodeableConceptList(inherited Clone);
end;

function TFhirCodeableConceptList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirCodeableConceptList.GetItemN(index: Integer): TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept(ObjectByIndex[index]);
end;

function TFhirCodeableConceptList.ItemClass: TFslObjectClass;
begin
  result := TFhirCodeableConcept;
end;
function TFhirCodeableConceptList.IndexOf(value: TFhirCodeableConcept): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirCodeableConceptList.Insert(index: Integer): TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirCodeableConceptList.InsertItem(index: Integer; value: TFhirCodeableConcept);
begin
  assert(value is TFhirCodeableConcept);
  Inherited Insert(index, value);
end;

function TFhirCodeableConceptList.Item(index: Integer): TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept(ObjectByIndex[index]);
end;

function TFhirCodeableConceptList.Link: TFhirCodeableConceptList;
begin
  result := TFhirCodeableConceptList(inherited Link);
end;

procedure TFhirCodeableConceptList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirCodeableConceptList.SetItemByIndex(index: Integer; value: TFhirCodeableConcept);
begin
  assert(value is TFhirCodeableConcept);
  FhirCodeableConcepts[index] := value;
end;

procedure TFhirCodeableConceptList.SetItemN(index: Integer; value: TFhirCodeableConcept);
begin
  assert(value is TFhirCodeableConcept);
  ObjectByIndex[index] := value;
end;

function TFhirNameUseEnumListAsInteger(aSet : TFhirNameUseEnumList) : Integer;
var
  a : TFhirNameUseEnum;
begin
  result := 0;
  for a := low(TFhirNameUseEnum) to high(TFhirNameUseEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNameUseEnumList(i : Integer) : TFhirNameUseEnumList;
var
  aLoop : TFhirNameUseEnum;
begin
  result := [];
  for aLoop := low(TFhirNameUseEnum) to high(TFhirNameUseEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

{ TFhirHumanName }

constructor TFhirHumanName.Create;
begin
  inherited;
end;

destructor TFhirHumanName.Destroy;
begin
  FUse.free;
  FText.free;
  FFamilyList.Free;
  FGivenList.Free;
  FPrefixList.Free;
  FSuffixList.Free;
  FPeriod.free;
  inherited;
end;

procedure TFhirHumanName.Assign(oSource : TFslObject);
begin
  inherited;
  FUse := TFhirHumanName(oSource).FUse.Link;
  textElement := TFhirHumanName(oSource).textElement.Clone;
  if (TFhirHumanName(oSource).FFamilyList = nil) then
  begin
    FFamilyList.free;
    FFamilyList := nil;
  end
  else
  begin
    if FFamilyList = nil then
      FFamilyList := TFhirStringList.Create;
    FFamilyList.Assign(TFhirHumanName(oSource).FFamilyList);
  end;
  if (TFhirHumanName(oSource).FGivenList = nil) then
  begin
    FGivenList.free;
    FGivenList := nil;
  end
  else
  begin
    if FGivenList = nil then
      FGivenList := TFhirStringList.Create;
    FGivenList.Assign(TFhirHumanName(oSource).FGivenList);
  end;
  if (TFhirHumanName(oSource).FPrefixList = nil) then
  begin
    FPrefixList.free;
    FPrefixList := nil;
  end
  else
  begin
    if FPrefixList = nil then
      FPrefixList := TFhirStringList.Create;
    FPrefixList.Assign(TFhirHumanName(oSource).FPrefixList);
  end;
  if (TFhirHumanName(oSource).FSuffixList = nil) then
  begin
    FSuffixList.free;
    FSuffixList := nil;
  end
  else
  begin
    if FSuffixList = nil then
      FSuffixList := TFhirStringList.Create;
    FSuffixList.Assign(TFhirHumanName(oSource).FSuffixList);
  end;
  period := TFhirHumanName(oSource).period.Clone;
end;

procedure TFhirHumanName.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'use') Then
     list.add(self.link, 'use', FUse.Link);
  if (child_name = 'text') Then
     list.add(self.link, 'text', FText.Link);
  if (child_name = 'family') Then
    list.addAll(self, 'family', FFamilyList);
  if (child_name = 'given') Then
    list.addAll(self, 'given', FGivenList);
  if (child_name = 'prefix') Then
    list.addAll(self, 'prefix', FPrefixList);
  if (child_name = 'suffix') Then
    list.addAll(self, 'suffix', FSuffixList);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
end;

procedure TFhirHumanName.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'use', 'code', false, TFHIREnum, FUse.Link));{1}
  oList.add(TFHIRProperty.create(self, 'text', 'string', false, TFhirString, FText.Link));{2}
  oList.add(TFHIRProperty.create(self, 'family', 'string', true, TFhirString, FFamilyList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'given', 'string', true, TFhirString, FGivenList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'prefix', 'string', true, TFhirString, FPrefixList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'suffix', 'string', true, TFhirString, FSuffixList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
end;

function TFhirHumanName.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'use') then
  begin
    UseElement := asEnum(SYSTEMS_TFhirNameUseEnum, CODES_TFhirNameUseEnum, propValue);
    result := propValue
  end
  else if (propName = 'text') then
  begin
    TextElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'family') then
  begin
    FamilyList.add(asString(propValue)){2};     result := propValue;

  end
  else if (propName = 'given') then
  begin
    GivenList.add(asString(propValue)){2};     result := propValue;

  end
  else if (propName = 'prefix') then
  begin
    PrefixList.add(asString(propValue)){2};     result := propValue;

  end
  else if (propName = 'suffix') then
  begin
    SuffixList.add(asString(propValue)){2};     result := propValue;

  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirHumanName.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'family') then FamilyList.insertItem(index, asString(propValue)){2}
  else if (propName = 'given') then GivenList.insertItem(index, asString(propValue)){2}
  else if (propName = 'prefix') then PrefixList.insertItem(index, asString(propValue)){2}
  else if (propName = 'suffix') then SuffixList.insertItem(index, asString(propValue)){2}
  else inherited;
end;

function TFhirHumanName.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'text') then result := TFhirString.create() {5b}
  else if (propName = 'family') then result := FamilyList.new(){2}
  else if (propName = 'given') then result := GivenList.new(){2}
  else if (propName = 'prefix') then result := PrefixList.new(){2}
  else if (propName = 'suffix') then result := SuffixList.new(){2}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirHumanName.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'use') then result := 'code'
  else if (propName = 'text') then result := 'string'
  else if (propName = 'family') then result := 'string'
  else if (propName = 'given') then result := 'string'
  else if (propName = 'prefix') then result := 'string'
  else if (propName = 'suffix') then result := 'string'
  else if (propName = 'period') then result := 'Period'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirHumanName.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'use') then UseElement := nil
  else if (propName = 'text') then TextElement := nil
  else if (propName = 'family') then deletePropertyValue('family', FamilyList, value) {2}
  else if (propName = 'given') then deletePropertyValue('given', GivenList, value) {2}
  else if (propName = 'prefix') then deletePropertyValue('prefix', PrefixList, value) {2}
  else if (propName = 'suffix') then deletePropertyValue('suffix', SuffixList, value) {2}
  else if (propName = 'period') then PeriodElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirHumanName.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'use') then UseElement := asEnum(SYSTEMS_TFhirNameUseEnum, CODES_TFhirNameUseEnum, new){4}
  else if (propName = 'text') then TextElement := asString(new){5b}
  else if (propName = 'family') then replacePropertyValue('family', FamilyList, existing, new) {2}
  else if (propName = 'given') then replacePropertyValue('given', GivenList, existing, new) {2}
  else if (propName = 'prefix') then replacePropertyValue('prefix', PrefixList, existing, new) {2}
  else if (propName = 'suffix') then replacePropertyValue('suffix', SuffixList, existing, new) {2}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirHumanName.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'family') then FamilyList.move(source, destination){2}
  else if (propName = 'given') then GivenList.move(source, destination){2}
  else if (propName = 'prefix') then PrefixList.move(source, destination){2}
  else if (propName = 'suffix') then SuffixList.move(source, destination){2}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirHumanName.fhirType : string;
begin
  result := 'HumanName';
end;

function TFhirHumanName.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FUse) and isEmptyProp(FText) and isEmptyProp(FfamilyList) and isEmptyProp(FgivenList) and isEmptyProp(FprefixList) and isEmptyProp(FsuffixList) and isEmptyProp(FPeriod);
end;

function TFhirHumanName.equals(other : TObject) : boolean; 
var
  o : TFhirHumanName;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirHumanName)) then
    result := false
  else
  begin
    o := TFhirHumanName(other);
    result := compareDeep(useElement, o.useElement, true) and compareDeep(textElement, o.textElement, true) and 
      compareDeep(familyList, o.familyList, true) and compareDeep(givenList, o.givenList, true) and 
      compareDeep(prefixList, o.prefixList, true) and compareDeep(suffixList, o.suffixList, true) and 
      compareDeep(periodElement, o.periodElement, true);
  end;
end;

function TFhirHumanName.Link : TFhirHumanName;
begin
  result := TFhirHumanName(inherited Link);
end;

function TFhirHumanName.Clone : TFhirHumanName;
begin
  result := TFhirHumanName(inherited Clone);
end;

procedure TFhirHumanName.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('use');
  fields.add('text');
  fields.add('family');
  fields.add('given');
  fields.add('prefix');
  fields.add('suffix');
  fields.add('period');
end;

{ TFhirHumanName }

Procedure TFhirHumanName.SetUse(value : TFhirEnum);
begin
  FUse.free;
  FUse := value;
end;

Function TFhirHumanName.GetUseST : TFhirNameUseEnum;
begin
  if FUse = nil then
    result := TFhirNameUseEnum(0)
  else
    result := TFhirNameUseEnum(StringArrayIndexOfSensitive(CODES_TFhirNameUseEnum, FUse.value));
end;

Procedure TFhirHumanName.SetUseST(value : TFhirNameUseEnum);
begin
  if ord(value) = 0 then
    UseElement := nil
  else
    UseElement := TFhirEnum.create(SYSTEMS_TFhirNameUseEnum[value], CODES_TFhirNameUseEnum[value]);
end;

Procedure TFhirHumanName.SetText(value : TFhirString);
begin
  FText.free;
  FText := value;
end;

Function TFhirHumanName.GetTextST : String;
begin
  if FText = nil then
    result := ''
  else
    result := FText.value;
end;

Procedure TFhirHumanName.SetTextST(value : String);
begin
  if value <> '' then
  begin
    if FText = nil then
      FText := TFhirString.create;
    FText.value := value
  end
  else if FText <> nil then
    FText.value := '';
end;

Function TFhirHumanName.GetFamilyList : TFhirStringList;
begin
  if FFamilyList = nil then
    FFamilyList := TFhirStringList.Create;
  result := FFamilyList;
end;

Function TFhirHumanName.GetHasFamilyList : boolean;
begin
  result := (FFamilyList <> nil) and (FFamilyList.count > 0);
end;

Function TFhirHumanName.GetGivenList : TFhirStringList;
begin
  if FGivenList = nil then
    FGivenList := TFhirStringList.Create;
  result := FGivenList;
end;

Function TFhirHumanName.GetHasGivenList : boolean;
begin
  result := (FGivenList <> nil) and (FGivenList.count > 0);
end;

Function TFhirHumanName.GetPrefixList : TFhirStringList;
begin
  if FPrefixList = nil then
    FPrefixList := TFhirStringList.Create;
  result := FPrefixList;
end;

Function TFhirHumanName.GetHasPrefixList : boolean;
begin
  result := (FPrefixList <> nil) and (FPrefixList.count > 0);
end;

Function TFhirHumanName.GetSuffixList : TFhirStringList;
begin
  if FSuffixList = nil then
    FSuffixList := TFhirStringList.Create;
  result := FSuffixList;
end;

Function TFhirHumanName.GetHasSuffixList : boolean;
begin
  result := (FSuffixList <> nil) and (FSuffixList.count > 0);
end;

Procedure TFhirHumanName.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

function TFhirHumanName.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FUse.sizeInBytes);
  inc(result, FText.sizeInBytes);
  inc(result, FfamilyList.sizeInBytes);
  inc(result, FgivenList.sizeInBytes);
  inc(result, FprefixList.sizeInBytes);
  inc(result, FsuffixList.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
end;

{ TFhirHumanNameListEnumerator }

Constructor TFhirHumanNameListEnumerator.Create(list : TFhirHumanNameList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirHumanNameListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirHumanNameListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirHumanNameListEnumerator.GetCurrent : TFhirHumanName;
begin
  Result := FList[FIndex];
end;

function TFhirHumanNameListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirHumanNameList }
procedure TFhirHumanNameList.AddItem(value: TFhirHumanName);
begin
  assert(value.ClassName = 'TFhirHumanName', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirHumanName');
  add(value);
end;

function TFhirHumanNameList.Append: TFhirHumanName;
begin
  result := TFhirHumanName.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirHumanNameList.ClearItems;
begin
  Clear;
end;

function TFhirHumanNameList.GetEnumerator : TFhirHumanNameListEnumerator;
begin
  result := TFhirHumanNameListEnumerator.Create(self.link);
end;

function TFhirHumanNameList.Clone: TFhirHumanNameList;
begin
  result := TFhirHumanNameList(inherited Clone);
end;

function TFhirHumanNameList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirHumanNameList.GetItemN(index: Integer): TFhirHumanName;
begin
  result := TFhirHumanName(ObjectByIndex[index]);
end;

function TFhirHumanNameList.ItemClass: TFslObjectClass;
begin
  result := TFhirHumanName;
end;
function TFhirHumanNameList.IndexOf(value: TFhirHumanName): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirHumanNameList.Insert(index: Integer): TFhirHumanName;
begin
  result := TFhirHumanName.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirHumanNameList.InsertItem(index: Integer; value: TFhirHumanName);
begin
  assert(value is TFhirHumanName);
  Inherited Insert(index, value);
end;

function TFhirHumanNameList.Item(index: Integer): TFhirHumanName;
begin
  result := TFhirHumanName(ObjectByIndex[index]);
end;

function TFhirHumanNameList.Link: TFhirHumanNameList;
begin
  result := TFhirHumanNameList(inherited Link);
end;

procedure TFhirHumanNameList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirHumanNameList.SetItemByIndex(index: Integer; value: TFhirHumanName);
begin
  assert(value is TFhirHumanName);
  FhirHumanNames[index] := value;
end;

procedure TFhirHumanNameList.SetItemN(index: Integer; value: TFhirHumanName);
begin
  assert(value is TFhirHumanName);
  ObjectByIndex[index] := value;
end;

{ TFhirMeta }

constructor TFhirMeta.Create;
begin
  inherited;
end;

destructor TFhirMeta.Destroy;
begin
  FVersionId.free;
  FLastUpdated.free;
  FProfileList.Free;
  FSecurityList.Free;
  FTagList.Free;
  inherited;
end;

procedure TFhirMeta.Assign(oSource : TFslObject);
begin
  inherited;
  versionIdElement := TFhirMeta(oSource).versionIdElement.Clone;
  lastUpdatedElement := TFhirMeta(oSource).lastUpdatedElement.Clone;
  if (TFhirMeta(oSource).FProfileList = nil) then
  begin
    FProfileList.free;
    FProfileList := nil;
  end
  else
  begin
    if FProfileList = nil then
      FProfileList := TFhirUriList.Create;
    FProfileList.Assign(TFhirMeta(oSource).FProfileList);
  end;
  if (TFhirMeta(oSource).FSecurityList = nil) then
  begin
    FSecurityList.free;
    FSecurityList := nil;
  end
  else
  begin
    if FSecurityList = nil then
      FSecurityList := TFhirCodingList.Create;
    FSecurityList.Assign(TFhirMeta(oSource).FSecurityList);
  end;
  if (TFhirMeta(oSource).FTagList = nil) then
  begin
    FTagList.free;
    FTagList := nil;
  end
  else
  begin
    if FTagList = nil then
      FTagList := TFhirCodingList.Create;
    FTagList.Assign(TFhirMeta(oSource).FTagList);
  end;
end;

procedure TFhirMeta.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'versionId') Then
     list.add(self.link, 'versionId', FVersionId.Link);
  if (child_name = 'lastUpdated') Then
     list.add(self.link, 'lastUpdated', FLastUpdated.Link);
  if (child_name = 'profile') Then
    list.addAll(self, 'profile', FProfileList);
  if (child_name = 'security') Then
    list.addAll(self, 'security', FSecurityList);
  if (child_name = 'tag') Then
    list.addAll(self, 'tag', FTagList);
end;

procedure TFhirMeta.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'versionId', 'id', false, TFhirId, FVersionId.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lastUpdated', 'instant', false, TFhirInstant, FLastUpdated.Link));{2}
  oList.add(TFHIRProperty.create(self, 'profile', 'uri', true, TFhirUri, FProfileList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'security', 'Coding', true, TFhirCoding, FSecurityList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'tag', 'Coding', true, TFhirCoding, FTagList.Link)){3};
end;

function TFhirMeta.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'versionId') then
  begin
    VersionIdElement := asId(propValue){5a};
    result := propValue;
  end
  else if (propName = 'lastUpdated') then
  begin
    LastUpdatedElement := asInstant(propValue){5a};
    result := propValue;
  end
  else if (propName = 'profile') then
  begin
    ProfileList.add(asUri(propValue)){2};     result := propValue;

  end
  else if (propName = 'security') then
  begin
    SecurityList.add(propValue as TFhirCoding){2a};
    result := propValue;
  end
  else if (propName = 'tag') then
  begin
    TagList.add(propValue as TFhirCoding){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirMeta.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'profile') then ProfileList.insertItem(index, asUri(propValue)){2}
  else if (propName = 'security') then SecurityList.insertItem(index, propValue as TFhirCoding){2a}
  else if (propName = 'tag') then TagList.insertItem(index, propValue as TFhirCoding){2a}
  else inherited;
end;

function TFhirMeta.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'versionId') then result := TFhirId.create() {5b}
  else if (propName = 'lastUpdated') then result := TFhirInstant.create() {5b}
  else if (propName = 'profile') then result := ProfileList.new(){2}
  else if (propName = 'security') then result := SecurityList.new(){2}
  else if (propName = 'tag') then result := TagList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirMeta.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'versionId') then result := 'id'
  else if (propName = 'lastUpdated') then result := 'instant'
  else if (propName = 'profile') then result := 'uri'
  else if (propName = 'security') then result := 'Coding'
  else if (propName = 'tag') then result := 'Coding'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirMeta.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'versionId') then VersionIdElement := nil
  else if (propName = 'lastUpdated') then LastUpdatedElement := nil
  else if (propName = 'profile') then deletePropertyValue('profile', ProfileList, value) {2}
  else if (propName = 'security') then deletePropertyValue('security', SecurityList, value) {2}
  else if (propName = 'tag') then deletePropertyValue('tag', TagList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirMeta.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'versionId') then VersionIdElement := asId(new){5b}
  else if (propName = 'lastUpdated') then LastUpdatedElement := asInstant(new){5b}
  else if (propName = 'profile') then replacePropertyValue('profile', ProfileList, existing, new) {2}
  else if (propName = 'security') then replacePropertyValue('security', SecurityList, existing, new) {2}
  else if (propName = 'tag') then replacePropertyValue('tag', TagList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirMeta.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'profile') then ProfileList.move(source, destination){2}
  else if (propName = 'security') then SecurityList.move(source, destination){2a}
  else if (propName = 'tag') then TagList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirMeta.fhirType : string;
begin
  result := 'Meta';
end;

function TFhirMeta.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FVersionId) and isEmptyProp(FLastUpdated) and isEmptyProp(FprofileList) and isEmptyProp(FsecurityList) and isEmptyProp(FtagList);
end;

function TFhirMeta.equals(other : TObject) : boolean; 
var
  o : TFhirMeta;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirMeta)) then
    result := false
  else
  begin
    o := TFhirMeta(other);
    result := compareDeep(versionIdElement, o.versionIdElement, true) and compareDeep(lastUpdatedElement, o.lastUpdatedElement, true) and 
      compareDeep(profileList, o.profileList, true) and compareDeep(securityList, o.securityList, true) and 
      compareDeep(tagList, o.tagList, true);
  end;
end;

function TFhirMeta.Link : TFhirMeta;
begin
  result := TFhirMeta(inherited Link);
end;

function TFhirMeta.Clone : TFhirMeta;
begin
  result := TFhirMeta(inherited Clone);
end;

procedure TFhirMeta.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('versionId');
  fields.add('lastUpdated');
  fields.add('profile');
  fields.add('security');
  fields.add('tag');
end;

{ TFhirMeta }

Procedure TFhirMeta.SetVersionId(value : TFhirId);
begin
  FVersionId.free;
  FVersionId := value;
end;

Function TFhirMeta.GetVersionIdST : String;
begin
  if FVersionId = nil then
    result := ''
  else
    result := FVersionId.value;
end;

Procedure TFhirMeta.SetVersionIdST(value : String);
begin
  if value <> '' then
  begin
    if FVersionId = nil then
      FVersionId := TFhirId.create;
    FVersionId.value := value
  end
  else if FVersionId <> nil then
    FVersionId.value := '';
end;

Procedure TFhirMeta.SetLastUpdated(value : TFhirInstant);
begin
  FLastUpdated.free;
  FLastUpdated := value;
end;

Function TFhirMeta.GetLastUpdatedST : TFslDateTime;
begin
  if FLastUpdated = nil then
    result := TFslDateTime.makeNull
  else
    result := FLastUpdated.value;
end;

Procedure TFhirMeta.SetLastUpdatedST(value : TFslDateTime);
begin
  if FLastUpdated = nil then
    FLastUpdated := TFhirInstant.create;
  FLastUpdated.value := value
end;

Function TFhirMeta.GetProfileList : TFhirUriList;
begin
  if FProfileList = nil then
    FProfileList := TFhirUriList.Create;
  result := FProfileList;
end;

Function TFhirMeta.GetHasProfileList : boolean;
begin
  result := (FProfileList <> nil) and (FProfileList.count > 0);
end;

Function TFhirMeta.GetSecurityList : TFhirCodingList;
begin
  if FSecurityList = nil then
    FSecurityList := TFhirCodingList.Create;
  result := FSecurityList;
end;

Function TFhirMeta.GetHasSecurityList : boolean;
begin
  result := (FSecurityList <> nil) and (FSecurityList.count > 0);
end;

Function TFhirMeta.GetTagList : TFhirCodingList;
begin
  if FTagList = nil then
    FTagList := TFhirCodingList.Create;
  result := FTagList;
end;

Function TFhirMeta.GetHasTagList : boolean;
begin
  result := (FTagList <> nil) and (FTagList.count > 0);
end;

function TFhirMeta.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FVersionId.sizeInBytes);
  inc(result, FLastUpdated.sizeInBytes);
  inc(result, FprofileList.sizeInBytes);
  inc(result, FsecurityList.sizeInBytes);
  inc(result, FtagList.sizeInBytes);
end;

{ TFhirMetaListEnumerator }

Constructor TFhirMetaListEnumerator.Create(list : TFhirMetaList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirMetaListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirMetaListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirMetaListEnumerator.GetCurrent : TFhirMeta;
begin
  Result := FList[FIndex];
end;

function TFhirMetaListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirMetaList }
procedure TFhirMetaList.AddItem(value: TFhirMeta);
begin
  assert(value.ClassName = 'TFhirMeta', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirMeta');
  add(value);
end;

function TFhirMetaList.Append: TFhirMeta;
begin
  result := TFhirMeta.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirMetaList.ClearItems;
begin
  Clear;
end;

function TFhirMetaList.GetEnumerator : TFhirMetaListEnumerator;
begin
  result := TFhirMetaListEnumerator.Create(self.link);
end;

function TFhirMetaList.Clone: TFhirMetaList;
begin
  result := TFhirMetaList(inherited Clone);
end;

function TFhirMetaList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirMetaList.GetItemN(index: Integer): TFhirMeta;
begin
  result := TFhirMeta(ObjectByIndex[index]);
end;

function TFhirMetaList.ItemClass: TFslObjectClass;
begin
  result := TFhirMeta;
end;
function TFhirMetaList.IndexOf(value: TFhirMeta): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirMetaList.Insert(index: Integer): TFhirMeta;
begin
  result := TFhirMeta.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirMetaList.InsertItem(index: Integer; value: TFhirMeta);
begin
  assert(value is TFhirMeta);
  Inherited Insert(index, value);
end;

function TFhirMetaList.Item(index: Integer): TFhirMeta;
begin
  result := TFhirMeta(ObjectByIndex[index]);
end;

function TFhirMetaList.Link: TFhirMetaList;
begin
  result := TFhirMetaList(inherited Link);
end;

procedure TFhirMetaList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirMetaList.SetItemByIndex(index: Integer; value: TFhirMeta);
begin
  assert(value is TFhirMeta);
  FhirMeta[index] := value;
end;

procedure TFhirMetaList.SetItemN(index: Integer; value: TFhirMeta);
begin
  assert(value is TFhirMeta);
  ObjectByIndex[index] := value;
end;

function TFhirContactPointSystemEnumListAsInteger(aSet : TFhirContactPointSystemEnumList) : Integer;
var
  a : TFhirContactPointSystemEnum;
begin
  result := 0;
  for a := low(TFhirContactPointSystemEnum) to high(TFhirContactPointSystemEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContactPointSystemEnumList(i : Integer) : TFhirContactPointSystemEnumList;
var
  aLoop : TFhirContactPointSystemEnum;
begin
  result := [];
  for aLoop := low(TFhirContactPointSystemEnum) to high(TFhirContactPointSystemEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContactPointUseEnumList(i : Integer) : TFhirContactPointUseEnumList;
var
  aLoop : TFhirContactPointUseEnum;
begin
  result := [];
  for aLoop := low(TFhirContactPointUseEnum) to high(TFhirContactPointUseEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

{ TFhirContactPoint }

constructor TFhirContactPoint.Create;
begin
  inherited;
end;

destructor TFhirContactPoint.Destroy;
begin
  FSystem.free;
  FValue.free;
  FUse.free;
  FRank.free;
  FPeriod.free;
  inherited;
end;

procedure TFhirContactPoint.Assign(oSource : TFslObject);
begin
  inherited;
  FSystem := TFhirContactPoint(oSource).FSystem.Link;
  valueElement := TFhirContactPoint(oSource).valueElement.Clone;
  FUse := TFhirContactPoint(oSource).FUse.Link;
  rankElement := TFhirContactPoint(oSource).rankElement.Clone;
  period := TFhirContactPoint(oSource).period.Clone;
end;

procedure TFhirContactPoint.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'system') Then
     list.add(self.link, 'system', FSystem.Link);
  if (child_name = 'value') Then
     list.add(self.link, 'value', FValue.Link);
  if (child_name = 'use') Then
     list.add(self.link, 'use', FUse.Link);
  if (child_name = 'rank') Then
     list.add(self.link, 'rank', FRank.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
end;

procedure TFhirContactPoint.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'system', 'code', false, TFHIREnum, FSystem.Link));{1}
  oList.add(TFHIRProperty.create(self, 'value', 'string', false, TFhirString, FValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'use', 'code', false, TFHIREnum, FUse.Link));{1}
  oList.add(TFHIRProperty.create(self, 'rank', 'positiveInt', false, TFhirPositiveInt, FRank.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
end;

function TFhirContactPoint.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'system') then
  begin
    SystemElement := asEnum(SYSTEMS_TFhirContactPointSystemEnum, CODES_TFhirContactPointSystemEnum, propValue);
    result := propValue
  end
  else if (propName = 'value') then
  begin
    ValueElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'use') then
  begin
    UseElement := asEnum(SYSTEMS_TFhirContactPointUseEnum, CODES_TFhirContactPointUseEnum, propValue);
    result := propValue
  end
  else if (propName = 'rank') then
  begin
    RankElement := asPositiveInt(propValue){5a};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirContactPoint.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirContactPoint.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'value') then result := TFhirString.create() {5b}
  else if (propName = 'rank') then result := TFhirPositiveInt.create() {5b}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirContactPoint.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'system') then result := 'code'
  else if (propName = 'value') then result := 'string'
  else if (propName = 'use') then result := 'code'
  else if (propName = 'rank') then result := 'positiveInt'
  else if (propName = 'period') then result := 'Period'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirContactPoint.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'system') then SystemElement := nil
  else if (propName = 'value') then ValueElement := nil
  else if (propName = 'use') then UseElement := nil
  else if (propName = 'rank') then RankElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirContactPoint.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'system') then SystemElement := asEnum(SYSTEMS_TFhirContactPointSystemEnum, CODES_TFhirContactPointSystemEnum, new){4}
  else if (propName = 'value') then ValueElement := asString(new){5b}
  else if (propName = 'use') then UseElement := asEnum(SYSTEMS_TFhirContactPointUseEnum, CODES_TFhirContactPointUseEnum, new){4}
  else if (propName = 'rank') then RankElement := asPositiveInt(new){5b}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirContactPoint.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirContactPoint.fhirType : string;
begin
  result := 'ContactPoint';
end;

function TFhirContactPoint.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FSystem) and isEmptyProp(FValue) and isEmptyProp(FUse) and isEmptyProp(FRank) and isEmptyProp(FPeriod);
end;

function TFhirContactPoint.equals(other : TObject) : boolean; 
var
  o : TFhirContactPoint;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirContactPoint)) then
    result := false
  else
  begin
    o := TFhirContactPoint(other);
    result := compareDeep(systemElement, o.systemElement, true) and compareDeep(valueElement, o.valueElement, true) and 
      compareDeep(useElement, o.useElement, true) and compareDeep(rankElement, o.rankElement, true) and 
      compareDeep(periodElement, o.periodElement, true);
  end;
end;

function TFhirContactPoint.Link : TFhirContactPoint;
begin
  result := TFhirContactPoint(inherited Link);
end;

function TFhirContactPoint.Clone : TFhirContactPoint;
begin
  result := TFhirContactPoint(inherited Clone);
end;

procedure TFhirContactPoint.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('system');
  fields.add('value');
  fields.add('use');
  fields.add('rank');
  fields.add('period');
end;

{ TFhirContactPoint }

Procedure TFhirContactPoint.SetSystem(value : TFhirEnum);
begin
  FSystem.free;
  FSystem := value;
end;

Function TFhirContactPoint.GetSystemST : TFhirContactPointSystemEnum;
begin
  if FSystem = nil then
    result := TFhirContactPointSystemEnum(0)
  else
    result := TFhirContactPointSystemEnum(StringArrayIndexOfSensitive(CODES_TFhirContactPointSystemEnum, FSystem.value));
end;

Procedure TFhirContactPoint.SetSystemST(value : TFhirContactPointSystemEnum);
begin
  if ord(value) = 0 then
    SystemElement := nil
  else
    SystemElement := TFhirEnum.create(SYSTEMS_TFhirContactPointSystemEnum[value], CODES_TFhirContactPointSystemEnum[value]);
end;

Procedure TFhirContactPoint.SetValue(value : TFhirString);
begin
  FValue.free;
  FValue := value;
end;

Function TFhirContactPoint.GetValueST : String;
begin
  if FValue = nil then
    result := ''
  else
    result := FValue.value;
end;

Procedure TFhirContactPoint.SetValueST(value : String);
begin
  if value <> '' then
  begin
    if FValue = nil then
      FValue := TFhirString.create;
    FValue.value := value
  end
  else if FValue <> nil then
    FValue.value := '';
end;

Procedure TFhirContactPoint.SetUse(value : TFhirEnum);
begin
  FUse.free;
  FUse := value;
end;

Function TFhirContactPoint.GetUseST : TFhirContactPointUseEnum;
begin
  if FUse = nil then
    result := TFhirContactPointUseEnum(0)
  else
    result := TFhirContactPointUseEnum(StringArrayIndexOfSensitive(CODES_TFhirContactPointUseEnum, FUse.value));
end;

Procedure TFhirContactPoint.SetUseST(value : TFhirContactPointUseEnum);
begin
  if ord(value) = 0 then
    UseElement := nil
  else
    UseElement := TFhirEnum.create(SYSTEMS_TFhirContactPointUseEnum[value], CODES_TFhirContactPointUseEnum[value]);
end;

Procedure TFhirContactPoint.SetRank(value : TFhirPositiveInt);
begin
  FRank.free;
  FRank := value;
end;

Function TFhirContactPoint.GetRankST : String;
begin
  if FRank = nil then
    result := ''
  else
    result := FRank.value;
end;

Procedure TFhirContactPoint.SetRankST(value : String);
begin
  if value <> '' then
  begin
    if FRank = nil then
      FRank := TFhirPositiveInt.create;
    FRank.value := value
  end
  else if FRank <> nil then
    FRank.value := '';
end;

Procedure TFhirContactPoint.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

function TFhirContactPoint.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSystem.sizeInBytes);
  inc(result, FValue.sizeInBytes);
  inc(result, FUse.sizeInBytes);
  inc(result, FRank.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
end;

{ TFhirContactPointListEnumerator }

Constructor TFhirContactPointListEnumerator.Create(list : TFhirContactPointList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirContactPointListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirContactPointListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirContactPointListEnumerator.GetCurrent : TFhirContactPoint;
begin
  Result := FList[FIndex];
end;

function TFhirContactPointListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirContactPointList }
procedure TFhirContactPointList.AddItem(value: TFhirContactPoint);
begin
  assert(value.ClassName = 'TFhirContactPoint', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirContactPoint');
  add(value);
end;

function TFhirContactPointList.Append: TFhirContactPoint;
begin
  result := TFhirContactPoint.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirContactPointList.ClearItems;
begin
  Clear;
end;

function TFhirContactPointList.GetEnumerator : TFhirContactPointListEnumerator;
begin
  result := TFhirContactPointListEnumerator.Create(self.link);
end;

function TFhirContactPointList.Clone: TFhirContactPointList;
begin
  result := TFhirContactPointList(inherited Clone);
end;

function TFhirContactPointList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirContactPointList.GetItemN(index: Integer): TFhirContactPoint;
begin
  result := TFhirContactPoint(ObjectByIndex[index]);
end;

function TFhirContactPointList.ItemClass: TFslObjectClass;
begin
  result := TFhirContactPoint;
end;
function TFhirContactPointList.IndexOf(value: TFhirContactPoint): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirContactPointList.Insert(index: Integer): TFhirContactPoint;
begin
  result := TFhirContactPoint.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirContactPointList.InsertItem(index: Integer; value: TFhirContactPoint);
begin
  assert(value is TFhirContactPoint);
  Inherited Insert(index, value);
end;

function TFhirContactPointList.Item(index: Integer): TFhirContactPoint;
begin
  result := TFhirContactPoint(ObjectByIndex[index]);
end;

function TFhirContactPointList.Link: TFhirContactPointList;
begin
  result := TFhirContactPointList(inherited Link);
end;

procedure TFhirContactPointList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirContactPointList.SetItemByIndex(index: Integer; value: TFhirContactPoint);
begin
  assert(value is TFhirContactPoint);
  FhirContactPoints[index] := value;
end;

procedure TFhirContactPointList.SetItemN(index: Integer; value: TFhirContactPoint);
begin
  assert(value is TFhirContactPoint);
  ObjectByIndex[index] := value;
end;

function TFhirAddressUseEnumListAsInteger(aSet : TFhirAddressUseEnumList) : Integer;
var
  a : TFhirAddressUseEnum;
begin
  result := 0;
  for a := low(TFhirAddressUseEnum) to high(TFhirAddressUseEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAddressUseEnumList(i : Integer) : TFhirAddressUseEnumList;
var
  aLoop : TFhirAddressUseEnum;
begin
  result := [];
  for aLoop := low(TFhirAddressUseEnum) to high(TFhirAddressUseEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAddressTypeEnumList(i : Integer) : TFhirAddressTypeEnumList;
var
  aLoop : TFhirAddressTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAddressTypeEnum) to high(TFhirAddressTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

{ TFhirAddress }

constructor TFhirAddress.Create;
begin
  inherited;
end;

destructor TFhirAddress.Destroy;
begin
  FUse.free;
  FType_.free;
  FText.free;
  FLineList.Free;
  FCity.free;
  FDistrict.free;
  FState.free;
  FPostalCode.free;
  FCountry.free;
  FPeriod.free;
  inherited;
end;

procedure TFhirAddress.Assign(oSource : TFslObject);
begin
  inherited;
  FUse := TFhirAddress(oSource).FUse.Link;
  FType_ := TFhirAddress(oSource).FType_.Link;
  textElement := TFhirAddress(oSource).textElement.Clone;
  if (TFhirAddress(oSource).FLineList = nil) then
  begin
    FLineList.free;
    FLineList := nil;
  end
  else
  begin
    if FLineList = nil then
      FLineList := TFhirStringList.Create;
    FLineList.Assign(TFhirAddress(oSource).FLineList);
  end;
  cityElement := TFhirAddress(oSource).cityElement.Clone;
  districtElement := TFhirAddress(oSource).districtElement.Clone;
  stateElement := TFhirAddress(oSource).stateElement.Clone;
  postalCodeElement := TFhirAddress(oSource).postalCodeElement.Clone;
  countryElement := TFhirAddress(oSource).countryElement.Clone;
  period := TFhirAddress(oSource).period.Clone;
end;

procedure TFhirAddress.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'use') Then
     list.add(self.link, 'use', FUse.Link);
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'text') Then
     list.add(self.link, 'text', FText.Link);
  if (child_name = 'line') Then
    list.addAll(self, 'line', FLineList);
  if (child_name = 'city') Then
     list.add(self.link, 'city', FCity.Link);
  if (child_name = 'district') Then
     list.add(self.link, 'district', FDistrict.Link);
  if (child_name = 'state') Then
     list.add(self.link, 'state', FState.Link);
  if (child_name = 'postalCode') Then
     list.add(self.link, 'postalCode', FPostalCode.Link);
  if (child_name = 'country') Then
     list.add(self.link, 'country', FCountry.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
end;

procedure TFhirAddress.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'use', 'code', false, TFHIREnum, FUse.Link));{1}
  oList.add(TFHIRProperty.create(self, 'type', 'code', false, TFHIREnum, FType_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'text', 'string', false, TFhirString, FText.Link));{2}
  oList.add(TFHIRProperty.create(self, 'line', 'string', true, TFhirString, FLineList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'city', 'string', false, TFhirString, FCity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'district', 'string', false, TFhirString, FDistrict.Link));{2}
  oList.add(TFHIRProperty.create(self, 'state', 'string', false, TFhirString, FState.Link));{2}
  oList.add(TFHIRProperty.create(self, 'postalCode', 'string', false, TFhirString, FPostalCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'country', 'string', false, TFhirString, FCountry.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
end;

function TFhirAddress.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'use') then
  begin
    UseElement := asEnum(SYSTEMS_TFhirAddressUseEnum, CODES_TFhirAddressUseEnum, propValue);
    result := propValue
  end
  else if (propName = 'type') then
  begin
    Type_Element := asEnum(SYSTEMS_TFhirAddressTypeEnum, CODES_TFhirAddressTypeEnum, propValue);
    result := propValue
  end
  else if (propName = 'text') then
  begin
    TextElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'line') then
  begin
    LineList.add(asString(propValue)){2};     result := propValue;

  end
  else if (propName = 'city') then
  begin
    CityElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'district') then
  begin
    DistrictElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'state') then
  begin
    StateElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'postalCode') then
  begin
    PostalCodeElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'country') then
  begin
    CountryElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirAddress.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'line') then LineList.insertItem(index, asString(propValue)){2}
  else inherited;
end;

function TFhirAddress.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'text') then result := TFhirString.create() {5b}
  else if (propName = 'line') then result := LineList.new(){2}
  else if (propName = 'city') then result := TFhirString.create() {5b}
  else if (propName = 'district') then result := TFhirString.create() {5b}
  else if (propName = 'state') then result := TFhirString.create() {5b}
  else if (propName = 'postalCode') then result := TFhirString.create() {5b}
  else if (propName = 'country') then result := TFhirString.create() {5b}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirAddress.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'use') then result := 'code'
  else if (propName = 'type') then result := 'code'
  else if (propName = 'text') then result := 'string'
  else if (propName = 'line') then result := 'string'
  else if (propName = 'city') then result := 'string'
  else if (propName = 'district') then result := 'string'
  else if (propName = 'state') then result := 'string'
  else if (propName = 'postalCode') then result := 'string'
  else if (propName = 'country') then result := 'string'
  else if (propName = 'period') then result := 'Period'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirAddress.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'use') then UseElement := nil
  else if (propName = 'type') then Type_Element := nil
  else if (propName = 'text') then TextElement := nil
  else if (propName = 'line') then deletePropertyValue('line', LineList, value) {2}
  else if (propName = 'city') then CityElement := nil
  else if (propName = 'district') then DistrictElement := nil
  else if (propName = 'state') then StateElement := nil
  else if (propName = 'postalCode') then PostalCodeElement := nil
  else if (propName = 'country') then CountryElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirAddress.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'use') then UseElement := asEnum(SYSTEMS_TFhirAddressUseEnum, CODES_TFhirAddressUseEnum, new){4}
  else if (propName = 'type') then Type_Element := asEnum(SYSTEMS_TFhirAddressTypeEnum, CODES_TFhirAddressTypeEnum, new){4}
  else if (propName = 'text') then TextElement := asString(new){5b}
  else if (propName = 'line') then replacePropertyValue('line', LineList, existing, new) {2}
  else if (propName = 'city') then CityElement := asString(new){5b}
  else if (propName = 'district') then DistrictElement := asString(new){5b}
  else if (propName = 'state') then StateElement := asString(new){5b}
  else if (propName = 'postalCode') then PostalCodeElement := asString(new){5b}
  else if (propName = 'country') then CountryElement := asString(new){5b}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirAddress.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'line') then LineList.move(source, destination){2}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirAddress.fhirType : string;
begin
  result := 'Address';
end;

function TFhirAddress.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FUse) and isEmptyProp(FType_) and isEmptyProp(FText) and isEmptyProp(FlineList) and isEmptyProp(FCity) and isEmptyProp(FDistrict) and isEmptyProp(FState) and isEmptyProp(FPostalCode) and isEmptyProp(FCountry) and isEmptyProp(FPeriod);
end;

function TFhirAddress.equals(other : TObject) : boolean; 
var
  o : TFhirAddress;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirAddress)) then
    result := false
  else
  begin
    o := TFhirAddress(other);
    result := compareDeep(useElement, o.useElement, true) and compareDeep(type_Element, o.type_Element, true) and 
      compareDeep(textElement, o.textElement, true) and compareDeep(lineList, o.lineList, true) and 
      compareDeep(cityElement, o.cityElement, true) and compareDeep(districtElement, o.districtElement, true) and 
      compareDeep(stateElement, o.stateElement, true) and compareDeep(postalCodeElement, o.postalCodeElement, true) and 
      compareDeep(countryElement, o.countryElement, true) and compareDeep(periodElement, o.periodElement, true);
  end;
end;

function TFhirAddress.Link : TFhirAddress;
begin
  result := TFhirAddress(inherited Link);
end;

function TFhirAddress.Clone : TFhirAddress;
begin
  result := TFhirAddress(inherited Clone);
end;

procedure TFhirAddress.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('use');
  fields.add('type');
  fields.add('text');
  fields.add('line');
  fields.add('city');
  fields.add('district');
  fields.add('state');
  fields.add('postalCode');
  fields.add('country');
  fields.add('period');
end;

{ TFhirAddress }

Procedure TFhirAddress.SetUse(value : TFhirEnum);
begin
  FUse.free;
  FUse := value;
end;

Function TFhirAddress.GetUseST : TFhirAddressUseEnum;
begin
  if FUse = nil then
    result := TFhirAddressUseEnum(0)
  else
    result := TFhirAddressUseEnum(StringArrayIndexOfSensitive(CODES_TFhirAddressUseEnum, FUse.value));
end;

Procedure TFhirAddress.SetUseST(value : TFhirAddressUseEnum);
begin
  if ord(value) = 0 then
    UseElement := nil
  else
    UseElement := TFhirEnum.create(SYSTEMS_TFhirAddressUseEnum[value], CODES_TFhirAddressUseEnum[value]);
end;

Procedure TFhirAddress.SetType_(value : TFhirEnum);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirAddress.GetType_ST : TFhirAddressTypeEnum;
begin
  if FType_ = nil then
    result := TFhirAddressTypeEnum(0)
  else
    result := TFhirAddressTypeEnum(StringArrayIndexOfSensitive(CODES_TFhirAddressTypeEnum, FType_.value));
end;

Procedure TFhirAddress.SetType_ST(value : TFhirAddressTypeEnum);
begin
  if ord(value) = 0 then
    Type_Element := nil
  else
    Type_Element := TFhirEnum.create(SYSTEMS_TFhirAddressTypeEnum[value], CODES_TFhirAddressTypeEnum[value]);
end;

Procedure TFhirAddress.SetText(value : TFhirString);
begin
  FText.free;
  FText := value;
end;

Function TFhirAddress.GetTextST : String;
begin
  if FText = nil then
    result := ''
  else
    result := FText.value;
end;

Procedure TFhirAddress.SetTextST(value : String);
begin
  if value <> '' then
  begin
    if FText = nil then
      FText := TFhirString.create;
    FText.value := value
  end
  else if FText <> nil then
    FText.value := '';
end;

Function TFhirAddress.GetLineList : TFhirStringList;
begin
  if FLineList = nil then
    FLineList := TFhirStringList.Create;
  result := FLineList;
end;

Function TFhirAddress.GetHasLineList : boolean;
begin
  result := (FLineList <> nil) and (FLineList.count > 0);
end;

Procedure TFhirAddress.SetCity(value : TFhirString);
begin
  FCity.free;
  FCity := value;
end;

Function TFhirAddress.GetCityST : String;
begin
  if FCity = nil then
    result := ''
  else
    result := FCity.value;
end;

Procedure TFhirAddress.SetCityST(value : String);
begin
  if value <> '' then
  begin
    if FCity = nil then
      FCity := TFhirString.create;
    FCity.value := value
  end
  else if FCity <> nil then
    FCity.value := '';
end;

Procedure TFhirAddress.SetDistrict(value : TFhirString);
begin
  FDistrict.free;
  FDistrict := value;
end;

Function TFhirAddress.GetDistrictST : String;
begin
  if FDistrict = nil then
    result := ''
  else
    result := FDistrict.value;
end;

Procedure TFhirAddress.SetDistrictST(value : String);
begin
  if value <> '' then
  begin
    if FDistrict = nil then
      FDistrict := TFhirString.create;
    FDistrict.value := value
  end
  else if FDistrict <> nil then
    FDistrict.value := '';
end;

Procedure TFhirAddress.SetState(value : TFhirString);
begin
  FState.free;
  FState := value;
end;

Function TFhirAddress.GetStateST : String;
begin
  if FState = nil then
    result := ''
  else
    result := FState.value;
end;

Procedure TFhirAddress.SetStateST(value : String);
begin
  if value <> '' then
  begin
    if FState = nil then
      FState := TFhirString.create;
    FState.value := value
  end
  else if FState <> nil then
    FState.value := '';
end;

Procedure TFhirAddress.SetPostalCode(value : TFhirString);
begin
  FPostalCode.free;
  FPostalCode := value;
end;

Function TFhirAddress.GetPostalCodeST : String;
begin
  if FPostalCode = nil then
    result := ''
  else
    result := FPostalCode.value;
end;

Procedure TFhirAddress.SetPostalCodeST(value : String);
begin
  if value <> '' then
  begin
    if FPostalCode = nil then
      FPostalCode := TFhirString.create;
    FPostalCode.value := value
  end
  else if FPostalCode <> nil then
    FPostalCode.value := '';
end;

Procedure TFhirAddress.SetCountry(value : TFhirString);
begin
  FCountry.free;
  FCountry := value;
end;

Function TFhirAddress.GetCountryST : String;
begin
  if FCountry = nil then
    result := ''
  else
    result := FCountry.value;
end;

Procedure TFhirAddress.SetCountryST(value : String);
begin
  if value <> '' then
  begin
    if FCountry = nil then
      FCountry := TFhirString.create;
    FCountry.value := value
  end
  else if FCountry <> nil then
    FCountry.value := '';
end;

Procedure TFhirAddress.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

function TFhirAddress.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FUse.sizeInBytes);
  inc(result, FType_.sizeInBytes);
  inc(result, FText.sizeInBytes);
  inc(result, FlineList.sizeInBytes);
  inc(result, FCity.sizeInBytes);
  inc(result, FDistrict.sizeInBytes);
  inc(result, FState.sizeInBytes);
  inc(result, FPostalCode.sizeInBytes);
  inc(result, FCountry.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
end;

{ TFhirAddressListEnumerator }

Constructor TFhirAddressListEnumerator.Create(list : TFhirAddressList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirAddressListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirAddressListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirAddressListEnumerator.GetCurrent : TFhirAddress;
begin
  Result := FList[FIndex];
end;

function TFhirAddressListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirAddressList }
procedure TFhirAddressList.AddItem(value: TFhirAddress);
begin
  assert(value.ClassName = 'TFhirAddress', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirAddress');
  add(value);
end;

function TFhirAddressList.Append: TFhirAddress;
begin
  result := TFhirAddress.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirAddressList.ClearItems;
begin
  Clear;
end;

function TFhirAddressList.GetEnumerator : TFhirAddressListEnumerator;
begin
  result := TFhirAddressListEnumerator.Create(self.link);
end;

function TFhirAddressList.Clone: TFhirAddressList;
begin
  result := TFhirAddressList(inherited Clone);
end;

function TFhirAddressList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirAddressList.GetItemN(index: Integer): TFhirAddress;
begin
  result := TFhirAddress(ObjectByIndex[index]);
end;

function TFhirAddressList.ItemClass: TFslObjectClass;
begin
  result := TFhirAddress;
end;
function TFhirAddressList.IndexOf(value: TFhirAddress): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirAddressList.Insert(index: Integer): TFhirAddress;
begin
  result := TFhirAddress.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirAddressList.InsertItem(index: Integer; value: TFhirAddress);
begin
  assert(value is TFhirAddress);
  Inherited Insert(index, value);
end;

function TFhirAddressList.Item(index: Integer): TFhirAddress;
begin
  result := TFhirAddress(ObjectByIndex[index]);
end;

function TFhirAddressList.Link: TFhirAddressList;
begin
  result := TFhirAddressList(inherited Link);
end;

procedure TFhirAddressList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirAddressList.SetItemByIndex(index: Integer; value: TFhirAddress);
begin
  assert(value is TFhirAddress);
  FhirAddresses[index] := value;
end;

procedure TFhirAddressList.SetItemN(index: Integer; value: TFhirAddress);
begin
  assert(value is TFhirAddress);
  ObjectByIndex[index] := value;
end;

function TFhirPropertyRepresentationEnumListAsInteger(aSet : TFhirPropertyRepresentationEnumList) : Integer;
var
  a : TFhirPropertyRepresentationEnum;
begin
  result := 0;
  for a := low(TFhirPropertyRepresentationEnum) to high(TFhirPropertyRepresentationEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPropertyRepresentationEnumList(i : Integer) : TFhirPropertyRepresentationEnumList;
var
  aLoop : TFhirPropertyRepresentationEnum;
begin
  result := [];
  for aLoop := low(TFhirPropertyRepresentationEnum) to high(TFhirPropertyRepresentationEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirResourceSlicingRulesEnumListAsInteger(aSet : TFhirResourceSlicingRulesEnumList) : Integer;
var
  a : TFhirResourceSlicingRulesEnum;
begin
  result := 0;
  for a := low(TFhirResourceSlicingRulesEnum) to high(TFhirResourceSlicingRulesEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResourceSlicingRulesEnumList(i : Integer) : TFhirResourceSlicingRulesEnumList;
var
  aLoop : TFhirResourceSlicingRulesEnum;
begin
  result := [];
  for aLoop := low(TFhirResourceSlicingRulesEnum) to high(TFhirResourceSlicingRulesEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirResourceAggregationModeEnumListAsInteger(aSet : TFhirResourceAggregationModeEnumList) : Integer;
var
  a : TFhirResourceAggregationModeEnum;
begin
  result := 0;
  for a := low(TFhirResourceAggregationModeEnum) to high(TFhirResourceAggregationModeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResourceAggregationModeEnumList(i : Integer) : TFhirResourceAggregationModeEnumList;
var
  aLoop : TFhirResourceAggregationModeEnum;
begin
  result := [];
  for aLoop := low(TFhirResourceAggregationModeEnum) to high(TFhirResourceAggregationModeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConstraintSeverityEnumList(i : Integer) : TFhirConstraintSeverityEnumList;
var
  aLoop : TFhirConstraintSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirConstraintSeverityEnum) to high(TFhirConstraintSeverityEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBindingStrengthEnumList(i : Integer) : TFhirBindingStrengthEnumList;
var
  aLoop : TFhirBindingStrengthEnum;
begin
  result := [];
  for aLoop := low(TFhirBindingStrengthEnum) to high(TFhirBindingStrengthEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

{ TFhirElementDefinitionSlicing }

constructor TFhirElementDefinitionSlicing.Create;
begin
  inherited;
end;

destructor TFhirElementDefinitionSlicing.Destroy;
begin
  FDiscriminatorList.Free;
  FDescription.free;
  FOrdered.free;
  FRules.free;
  inherited;
end;

procedure TFhirElementDefinitionSlicing.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirElementDefinitionSlicing(oSource).FDiscriminatorList = nil) then
  begin
    FDiscriminatorList.free;
    FDiscriminatorList := nil;
  end
  else
  begin
    if FDiscriminatorList = nil then
      FDiscriminatorList := TFhirStringList.Create;
    FDiscriminatorList.Assign(TFhirElementDefinitionSlicing(oSource).FDiscriminatorList);
  end;
  descriptionElement := TFhirElementDefinitionSlicing(oSource).descriptionElement.Clone;
  orderedElement := TFhirElementDefinitionSlicing(oSource).orderedElement.Clone;
  FRules := TFhirElementDefinitionSlicing(oSource).FRules.Link;
end;

procedure TFhirElementDefinitionSlicing.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'discriminator') Then
    list.addAll(self, 'discriminator', FDiscriminatorList);
  if (child_name = 'description') Then
     list.add(self.link, 'description', FDescription.Link);
  if (child_name = 'ordered') Then
     list.add(self.link, 'ordered', FOrdered.Link);
  if (child_name = 'rules') Then
     list.add(self.link, 'rules', FRules.Link);
end;

procedure TFhirElementDefinitionSlicing.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'discriminator', 'string', true, TFhirString, FDiscriminatorList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', false, TFhirString, FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'ordered', 'boolean', false, TFhirBoolean, FOrdered.Link));{2}
  oList.add(TFHIRProperty.create(self, 'rules', 'code', false, TFHIREnum, FRules.Link));{1}
end;

function TFhirElementDefinitionSlicing.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'discriminator') then
  begin
    DiscriminatorList.add(asString(propValue)){2};     result := propValue;

  end
  else if (propName = 'description') then
  begin
    DescriptionElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'ordered') then
  begin
    OrderedElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'rules') then
  begin
    RulesElement := asEnum(SYSTEMS_TFhirResourceSlicingRulesEnum, CODES_TFhirResourceSlicingRulesEnum, propValue);
    result := propValue
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirElementDefinitionSlicing.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'discriminator') then DiscriminatorList.insertItem(index, asString(propValue)){2}
  else inherited;
end;

function TFhirElementDefinitionSlicing.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'discriminator') then result := DiscriminatorList.new(){2}
  else if (propName = 'description') then result := TFhirString.create() {5b}
  else if (propName = 'ordered') then result := TFhirBoolean.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirElementDefinitionSlicing.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'discriminator') then result := 'string'
  else if (propName = 'description') then result := 'string'
  else if (propName = 'ordered') then result := 'boolean'
  else if (propName = 'rules') then result := 'code'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirElementDefinitionSlicing.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'discriminator') then deletePropertyValue('discriminator', DiscriminatorList, value) {2}
  else if (propName = 'description') then DescriptionElement := nil
  else if (propName = 'ordered') then OrderedElement := nil
  else if (propName = 'rules') then RulesElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirElementDefinitionSlicing.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'discriminator') then replacePropertyValue('discriminator', DiscriminatorList, existing, new) {2}
  else if (propName = 'description') then DescriptionElement := asString(new){5b}
  else if (propName = 'ordered') then OrderedElement := asBoolean(new){5b}
  else if (propName = 'rules') then RulesElement := asEnum(SYSTEMS_TFhirResourceSlicingRulesEnum, CODES_TFhirResourceSlicingRulesEnum, new){4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirElementDefinitionSlicing.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'discriminator') then DiscriminatorList.move(source, destination){2}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirElementDefinitionSlicing.fhirType : string;
begin
  result := 'slicing';
end;

function TFhirElementDefinitionSlicing.Link : TFhirElementDefinitionSlicing;
begin
  result := TFhirElementDefinitionSlicing(inherited Link);
end;

function TFhirElementDefinitionSlicing.Clone : TFhirElementDefinitionSlicing;
begin
  result := TFhirElementDefinitionSlicing(inherited Clone);
end;

function TFhirElementDefinitionSlicing.equals(other : TObject) : boolean; 
var
  o : TFhirElementDefinitionSlicing;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirElementDefinitionSlicing)) then
    result := false
  else
  begin
    o := TFhirElementDefinitionSlicing(other);
    result := compareDeep(discriminatorList, o.discriminatorList, true) and compareDeep(descriptionElement, o.descriptionElement, true) and 
      compareDeep(orderedElement, o.orderedElement, true) and compareDeep(rulesElement, o.rulesElement, true);
  end;
end;

function TFhirElementDefinitionSlicing.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FdiscriminatorList) and isEmptyProp(FDescription) and isEmptyProp(FOrdered) and isEmptyProp(FRules);
end;

procedure TFhirElementDefinitionSlicing.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('discriminator');
  fields.add('description');
  fields.add('ordered');
  fields.add('rules');
end;

{ TFhirElementDefinitionSlicing }

Function TFhirElementDefinitionSlicing.GetDiscriminatorList : TFhirStringList;
begin
  if FDiscriminatorList = nil then
    FDiscriminatorList := TFhirStringList.Create;
  result := FDiscriminatorList;
end;

Function TFhirElementDefinitionSlicing.GetHasDiscriminatorList : boolean;
begin
  result := (FDiscriminatorList <> nil) and (FDiscriminatorList.count > 0);
end;

Procedure TFhirElementDefinitionSlicing.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirElementDefinitionSlicing.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := FDescription.value;
end;

Procedure TFhirElementDefinitionSlicing.SetDescriptionST(value : String);
begin
  if value <> '' then
  begin
    if FDescription = nil then
      FDescription := TFhirString.create;
    FDescription.value := value
  end
  else if FDescription <> nil then
    FDescription.value := '';
end;

Procedure TFhirElementDefinitionSlicing.SetOrdered(value : TFhirBoolean);
begin
  FOrdered.free;
  FOrdered := value;
end;

Function TFhirElementDefinitionSlicing.GetOrderedST : Boolean;
begin
  if FOrdered = nil then
    result := false
  else
    result := FOrdered.value;
end;

Procedure TFhirElementDefinitionSlicing.SetOrderedST(value : Boolean);
begin
  if FOrdered = nil then
    FOrdered := TFhirBoolean.create;
  FOrdered.value := value
end;

Procedure TFhirElementDefinitionSlicing.SetRules(value : TFhirEnum);
begin
  FRules.free;
  FRules := value;
end;

Function TFhirElementDefinitionSlicing.GetRulesST : TFhirResourceSlicingRulesEnum;
begin
  if FRules = nil then
    result := TFhirResourceSlicingRulesEnum(0)
  else
    result := TFhirResourceSlicingRulesEnum(StringArrayIndexOfSensitive(CODES_TFhirResourceSlicingRulesEnum, FRules.value));
end;

Procedure TFhirElementDefinitionSlicing.SetRulesST(value : TFhirResourceSlicingRulesEnum);
begin
  if ord(value) = 0 then
    RulesElement := nil
  else
    RulesElement := TFhirEnum.create(SYSTEMS_TFhirResourceSlicingRulesEnum[value], CODES_TFhirResourceSlicingRulesEnum[value]);
end;

function TFhirElementDefinitionSlicing.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FdiscriminatorList.sizeInBytes);
  inc(result, FDescription.sizeInBytes);
  inc(result, FOrdered.sizeInBytes);
  inc(result, FRules.sizeInBytes);
end;

{ TFhirElementDefinitionSlicingListEnumerator }

Constructor TFhirElementDefinitionSlicingListEnumerator.Create(list : TFhirElementDefinitionSlicingList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirElementDefinitionSlicingListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirElementDefinitionSlicingListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirElementDefinitionSlicingListEnumerator.GetCurrent : TFhirElementDefinitionSlicing;
begin
  Result := FList[FIndex];
end;

function TFhirElementDefinitionSlicingListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirElementDefinitionSlicingList }
procedure TFhirElementDefinitionSlicingList.AddItem(value: TFhirElementDefinitionSlicing);
begin
  assert(value.ClassName = 'TFhirElementDefinitionSlicing', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirElementDefinitionSlicing');
  add(value);
end;

function TFhirElementDefinitionSlicingList.Append: TFhirElementDefinitionSlicing;
begin
  result := TFhirElementDefinitionSlicing.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionSlicingList.ClearItems;
begin
  Clear;
end;

function TFhirElementDefinitionSlicingList.GetEnumerator : TFhirElementDefinitionSlicingListEnumerator;
begin
  result := TFhirElementDefinitionSlicingListEnumerator.Create(self.link);
end;

function TFhirElementDefinitionSlicingList.Clone: TFhirElementDefinitionSlicingList;
begin
  result := TFhirElementDefinitionSlicingList(inherited Clone);
end;

function TFhirElementDefinitionSlicingList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirElementDefinitionSlicingList.GetItemN(index: Integer): TFhirElementDefinitionSlicing;
begin
  result := TFhirElementDefinitionSlicing(ObjectByIndex[index]);
end;

function TFhirElementDefinitionSlicingList.ItemClass: TFslObjectClass;
begin
  result := TFhirElementDefinitionSlicing;
end;
function TFhirElementDefinitionSlicingList.IndexOf(value: TFhirElementDefinitionSlicing): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirElementDefinitionSlicingList.Insert(index: Integer): TFhirElementDefinitionSlicing;
begin
  result := TFhirElementDefinitionSlicing.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionSlicingList.InsertItem(index: Integer; value: TFhirElementDefinitionSlicing);
begin
  assert(value is TFhirElementDefinitionSlicing);
  Inherited Insert(index, value);
end;

function TFhirElementDefinitionSlicingList.Item(index: Integer): TFhirElementDefinitionSlicing;
begin
  result := TFhirElementDefinitionSlicing(ObjectByIndex[index]);
end;

function TFhirElementDefinitionSlicingList.Link: TFhirElementDefinitionSlicingList;
begin
  result := TFhirElementDefinitionSlicingList(inherited Link);
end;

procedure TFhirElementDefinitionSlicingList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirElementDefinitionSlicingList.SetItemByIndex(index: Integer; value: TFhirElementDefinitionSlicing);
begin
  assert(value is TFhirElementDefinitionSlicing);
  FhirElementDefinitionSlicings[index] := value;
end;

procedure TFhirElementDefinitionSlicingList.SetItemN(index: Integer; value: TFhirElementDefinitionSlicing);
begin
  assert(value is TFhirElementDefinitionSlicing);
  ObjectByIndex[index] := value;
end;

{ TFhirElementDefinitionBase }

constructor TFhirElementDefinitionBase.Create;
begin
  inherited;
end;

destructor TFhirElementDefinitionBase.Destroy;
begin
  FPath.free;
  FMin.free;
  FMax.free;
  inherited;
end;

procedure TFhirElementDefinitionBase.Assign(oSource : TFslObject);
begin
  inherited;
  pathElement := TFhirElementDefinitionBase(oSource).pathElement.Clone;
  minElement := TFhirElementDefinitionBase(oSource).minElement.Clone;
  maxElement := TFhirElementDefinitionBase(oSource).maxElement.Clone;
end;

procedure TFhirElementDefinitionBase.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'path') Then
     list.add(self.link, 'path', FPath.Link);
  if (child_name = 'min') Then
     list.add(self.link, 'min', FMin.Link);
  if (child_name = 'max') Then
     list.add(self.link, 'max', FMax.Link);
end;

procedure TFhirElementDefinitionBase.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'path', 'string', false, TFhirString, FPath.Link));{2}
  oList.add(TFHIRProperty.create(self, 'min', 'integer', false, TFhirInteger, FMin.Link));{2}
  oList.add(TFHIRProperty.create(self, 'max', 'string', false, TFhirString, FMax.Link));{2}
end;

function TFhirElementDefinitionBase.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'path') then
  begin
    PathElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'min') then
  begin
    MinElement := asInteger(propValue){5a};
    result := propValue;
  end
  else if (propName = 'max') then
  begin
    MaxElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirElementDefinitionBase.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirElementDefinitionBase.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'path') then result := TFhirString.create() {5b}
  else if (propName = 'min') then result := TFhirInteger.create() {5b}
  else if (propName = 'max') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirElementDefinitionBase.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'path') then result := 'string'
  else if (propName = 'min') then result := 'integer'
  else if (propName = 'max') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirElementDefinitionBase.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'path') then PathElement := nil
  else if (propName = 'min') then MinElement := nil
  else if (propName = 'max') then MaxElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirElementDefinitionBase.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'path') then PathElement := asString(new){5b}
  else if (propName = 'min') then MinElement := asInteger(new){5b}
  else if (propName = 'max') then MaxElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirElementDefinitionBase.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirElementDefinitionBase.fhirType : string;
begin
  result := 'base';
end;

function TFhirElementDefinitionBase.Link : TFhirElementDefinitionBase;
begin
  result := TFhirElementDefinitionBase(inherited Link);
end;

function TFhirElementDefinitionBase.Clone : TFhirElementDefinitionBase;
begin
  result := TFhirElementDefinitionBase(inherited Clone);
end;

function TFhirElementDefinitionBase.equals(other : TObject) : boolean; 
var
  o : TFhirElementDefinitionBase;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirElementDefinitionBase)) then
    result := false
  else
  begin
    o := TFhirElementDefinitionBase(other);
    result := compareDeep(pathElement, o.pathElement, true) and compareDeep(minElement, o.minElement, true) and 
      compareDeep(maxElement, o.maxElement, true);
  end;
end;

function TFhirElementDefinitionBase.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FPath) and isEmptyProp(FMin) and isEmptyProp(FMax);
end;

procedure TFhirElementDefinitionBase.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('path');
  fields.add('min');
  fields.add('max');
end;

{ TFhirElementDefinitionBase }

Procedure TFhirElementDefinitionBase.SetPath(value : TFhirString);
begin
  FPath.free;
  FPath := value;
end;

Function TFhirElementDefinitionBase.GetPathST : String;
begin
  if FPath = nil then
    result := ''
  else
    result := FPath.value;
end;

Procedure TFhirElementDefinitionBase.SetPathST(value : String);
begin
  if value <> '' then
  begin
    if FPath = nil then
      FPath := TFhirString.create;
    FPath.value := value
  end
  else if FPath <> nil then
    FPath.value := '';
end;

Procedure TFhirElementDefinitionBase.SetMin(value : TFhirInteger);
begin
  FMin.free;
  FMin := value;
end;

Function TFhirElementDefinitionBase.GetMinST : String;
begin
  if FMin = nil then
    result := ''
  else
    result := FMin.value;
end;

Procedure TFhirElementDefinitionBase.SetMinST(value : String);
begin
  if value <> '' then
  begin
    if FMin = nil then
      FMin := TFhirInteger.create;
    FMin.value := value
  end
  else if FMin <> nil then
    FMin.value := '';
end;

Procedure TFhirElementDefinitionBase.SetMax(value : TFhirString);
begin
  FMax.free;
  FMax := value;
end;

Function TFhirElementDefinitionBase.GetMaxST : String;
begin
  if FMax = nil then
    result := ''
  else
    result := FMax.value;
end;

Procedure TFhirElementDefinitionBase.SetMaxST(value : String);
begin
  if value <> '' then
  begin
    if FMax = nil then
      FMax := TFhirString.create;
    FMax.value := value
  end
  else if FMax <> nil then
    FMax.value := '';
end;

function TFhirElementDefinitionBase.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPath.sizeInBytes);
  inc(result, FMin.sizeInBytes);
  inc(result, FMax.sizeInBytes);
end;

{ TFhirElementDefinitionBaseListEnumerator }

Constructor TFhirElementDefinitionBaseListEnumerator.Create(list : TFhirElementDefinitionBaseList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirElementDefinitionBaseListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirElementDefinitionBaseListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirElementDefinitionBaseListEnumerator.GetCurrent : TFhirElementDefinitionBase;
begin
  Result := FList[FIndex];
end;

function TFhirElementDefinitionBaseListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirElementDefinitionBaseList }
procedure TFhirElementDefinitionBaseList.AddItem(value: TFhirElementDefinitionBase);
begin
  assert(value.ClassName = 'TFhirElementDefinitionBase', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirElementDefinitionBase');
  add(value);
end;

function TFhirElementDefinitionBaseList.Append: TFhirElementDefinitionBase;
begin
  result := TFhirElementDefinitionBase.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionBaseList.ClearItems;
begin
  Clear;
end;

function TFhirElementDefinitionBaseList.GetEnumerator : TFhirElementDefinitionBaseListEnumerator;
begin
  result := TFhirElementDefinitionBaseListEnumerator.Create(self.link);
end;

function TFhirElementDefinitionBaseList.Clone: TFhirElementDefinitionBaseList;
begin
  result := TFhirElementDefinitionBaseList(inherited Clone);
end;

function TFhirElementDefinitionBaseList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirElementDefinitionBaseList.GetItemN(index: Integer): TFhirElementDefinitionBase;
begin
  result := TFhirElementDefinitionBase(ObjectByIndex[index]);
end;

function TFhirElementDefinitionBaseList.ItemClass: TFslObjectClass;
begin
  result := TFhirElementDefinitionBase;
end;
function TFhirElementDefinitionBaseList.IndexOf(value: TFhirElementDefinitionBase): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirElementDefinitionBaseList.Insert(index: Integer): TFhirElementDefinitionBase;
begin
  result := TFhirElementDefinitionBase.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionBaseList.InsertItem(index: Integer; value: TFhirElementDefinitionBase);
begin
  assert(value is TFhirElementDefinitionBase);
  Inherited Insert(index, value);
end;

function TFhirElementDefinitionBaseList.Item(index: Integer): TFhirElementDefinitionBase;
begin
  result := TFhirElementDefinitionBase(ObjectByIndex[index]);
end;

function TFhirElementDefinitionBaseList.Link: TFhirElementDefinitionBaseList;
begin
  result := TFhirElementDefinitionBaseList(inherited Link);
end;

procedure TFhirElementDefinitionBaseList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirElementDefinitionBaseList.SetItemByIndex(index: Integer; value: TFhirElementDefinitionBase);
begin
  assert(value is TFhirElementDefinitionBase);
  FhirElementDefinitionBases[index] := value;
end;

procedure TFhirElementDefinitionBaseList.SetItemN(index: Integer; value: TFhirElementDefinitionBase);
begin
  assert(value is TFhirElementDefinitionBase);
  ObjectByIndex[index] := value;
end;

{ TFhirElementDefinitionType }

constructor TFhirElementDefinitionType.Create;
begin
  inherited;
end;

destructor TFhirElementDefinitionType.Destroy;
begin
  FCode.free;
  FProfileList.Free;
  FAggregation.Free;
  inherited;
end;

procedure TFhirElementDefinitionType.Assign(oSource : TFslObject);
begin
  inherited;
  codeElement := TFhirElementDefinitionType(oSource).codeElement.Clone;
  if (TFhirElementDefinitionType(oSource).FProfileList = nil) then
  begin
    FProfileList.free;
    FProfileList := nil;
  end
  else
  begin
    if FProfileList = nil then
      FProfileList := TFhirUriList.Create;
    FProfileList.Assign(TFhirElementDefinitionType(oSource).FProfileList);
  end;
  if (TFhirElementDefinitionType(oSource).FAggregation = nil) then
  begin
    FAggregation.free;
    FAggregation := nil;
  end
  else
  begin
    FAggregation := TFHIREnumList.Create(SYSTEMS_TFhirResourceAggregationModeEnum, CODES_TFhirResourceAggregationModeEnum);
    FAggregation.Assign(TFhirElementDefinitionType(oSource).FAggregation);
  end;
end;

procedure TFhirElementDefinitionType.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'code') Then
     list.add(self.link, 'code', FCode.Link);
  if (child_name = 'profile') Then
    list.addAll(self, 'profile', FProfileList);
  if (child_name = 'aggregation') Then
     list.addAll(self, 'aggregation', FAggregation);
end;

procedure TFhirElementDefinitionType.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'code', 'code', false, TFhirCode, FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'profile', 'uri', true, TFhirUri, FProfileList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'aggregation', 'code', true, TFHIREnum, FAggregation.Link)){3};
end;

function TFhirElementDefinitionType.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'code') then
  begin
    CodeElement := asCode(propValue);
    result := propValue;
  end
  else if (propName = 'profile') then
  begin
    ProfileList.add(asUri(propValue)){2};     result := propValue;

  end
  else if (propName = 'aggregation') then
  begin
    AggregationList.add(asEnum(SYSTEMS_TFhirResourceAggregationModeEnum, CODES_TFhirResourceAggregationModeEnum, propValue)); {1}
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirElementDefinitionType.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'profile') then ProfileList.insertItem(index, asUri(propValue)){2}
  else if (propName = 'aggregation') then FAggregation.insertItem(index, asEnum(SYSTEMS_TFhirResourceAggregationModeEnum, CODES_TFhirResourceAggregationModeEnum, propValue)) {1}
  else inherited;
end;

function TFhirElementDefinitionType.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'code') then result := TFhirCode.create() {5b}
  else if (propName = 'profile') then result := ProfileList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirElementDefinitionType.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'code') then result := 'code'
  else if (propName = 'profile') then result := 'uri'
  else if (propName = 'aggregation') then result := 'code'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirElementDefinitionType.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'code') then CodeElement := nil
  else if (propName = 'profile') then deletePropertyValue('profile', ProfileList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirElementDefinitionType.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'code') then CodeElement := asCode(new){5b}
  else if (propName = 'profile') then replacePropertyValue('profile', ProfileList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirElementDefinitionType.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'profile') then ProfileList.move(source, destination){2}
  else if (propName = 'aggregation') then FAggregation.move(source, destination) {1}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirElementDefinitionType.fhirType : string;
begin
  result := 'type';
end;

function TFhirElementDefinitionType.Link : TFhirElementDefinitionType;
begin
  result := TFhirElementDefinitionType(inherited Link);
end;

function TFhirElementDefinitionType.Clone : TFhirElementDefinitionType;
begin
  result := TFhirElementDefinitionType(inherited Clone);
end;

function TFhirElementDefinitionType.equals(other : TObject) : boolean; 
var
  o : TFhirElementDefinitionType;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirElementDefinitionType)) then
    result := false
  else
  begin
    o := TFhirElementDefinitionType(other);
    result := compareDeep(codeElement, o.codeElement, true) and compareDeep(profileList, o.profileList, true) and 
      compareDeep(aggregationList, o.aggregationList, true);
  end;
end;

function TFhirElementDefinitionType.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FCode) and isEmptyProp(FprofileList) and isEmptyProp(FAggregation);
end;

procedure TFhirElementDefinitionType.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('code');
  fields.add('profile');
  fields.add('aggregation');
end;

{ TFhirElementDefinitionType }

Procedure TFhirElementDefinitionType.SetCode(value : TFhirCode);
begin
  FCode.free;
  FCode := value;
end;

Function TFhirElementDefinitionType.GetCodeST : String;
begin
  if FCode = nil then
    result := ''
  else
    result := FCode.value;
end;

Procedure TFhirElementDefinitionType.SetCodeST(value : String);
begin
  if value <> '' then
  begin
    if FCode = nil then
      FCode := TFhirCode.create;
    FCode.value := value
  end
  else if FCode <> nil then
    FCode.value := '';
end;

Function TFhirElementDefinitionType.GetProfileList : TFhirUriList;
begin
  if FProfileList = nil then
    FProfileList := TFhirUriList.Create;
  result := FProfileList;
end;

Function TFhirElementDefinitionType.GetHasProfileList : boolean;
begin
  result := (FProfileList <> nil) and (FProfileList.count > 0);
end;

Function TFhirElementDefinitionType.GetAggregation : TFhirEnumList;
begin
  if FAggregation = nil then
    FAggregation := TFHIREnumList.Create(SYSTEMS_TFhirResourceAggregationModeEnum, CODES_TFhirResourceAggregationModeEnum);
  result := FAggregation;
end;

Function TFhirElementDefinitionType.GetHasAggregation : boolean;
begin
  result := (FAggregation <> nil) and (FAggregation.count > 0);
end;

Function TFhirElementDefinitionType.GetAggregationST : TFhirResourceAggregationModeEnumList;
  var i : integer;
begin
  result := [];
  if Faggregation <> nil then
    for i := 0 to Faggregation.count - 1 do
      result := result + [TFhirResourceAggregationModeEnum(StringArrayIndexOfSensitive(CODES_TFhirResourceAggregationModeEnum, Faggregation[i].value))];
end;

Procedure TFhirElementDefinitionType.SetAggregationST(value : TFhirResourceAggregationModeEnumList);
var a : TFhirResourceAggregationModeEnum;
begin
  if Faggregation = nil then
    Faggregation := TFhirEnumList.create(SYSTEMS_TFhirResourceAggregationModeEnum, CODES_TFhirResourceAggregationModeEnum);
  Faggregation.clear;
  for a := low(TFhirResourceAggregationModeEnum) to high(TFhirResourceAggregationModeEnum) do
    if a in value then
      begin
         if Faggregation = nil then
           Faggregation := TFhirEnumList.create(SYSTEMS_TFhirResourceAggregationModeEnum, CODES_TFhirResourceAggregationModeEnum);
         Faggregation.add(TFhirEnum.create(SYSTEMS_TFhirResourceAggregationModeEnum[a], CODES_TFhirResourceAggregationModeEnum[a]));
      end;
end;

function TFhirElementDefinitionType.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCode.sizeInBytes);
  inc(result, FprofileList.sizeInBytes);
  inc(result, FAggregation.sizeInBytes);
end;

{ TFhirElementDefinitionTypeListEnumerator }

Constructor TFhirElementDefinitionTypeListEnumerator.Create(list : TFhirElementDefinitionTypeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirElementDefinitionTypeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirElementDefinitionTypeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirElementDefinitionTypeListEnumerator.GetCurrent : TFhirElementDefinitionType;
begin
  Result := FList[FIndex];
end;

function TFhirElementDefinitionTypeListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirElementDefinitionTypeList }
procedure TFhirElementDefinitionTypeList.AddItem(value: TFhirElementDefinitionType);
begin
  assert(value.ClassName = 'TFhirElementDefinitionType', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirElementDefinitionType');
  add(value);
end;

function TFhirElementDefinitionTypeList.Append: TFhirElementDefinitionType;
begin
  result := TFhirElementDefinitionType.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionTypeList.ClearItems;
begin
  Clear;
end;

function TFhirElementDefinitionTypeList.GetEnumerator : TFhirElementDefinitionTypeListEnumerator;
begin
  result := TFhirElementDefinitionTypeListEnumerator.Create(self.link);
end;

function TFhirElementDefinitionTypeList.Clone: TFhirElementDefinitionTypeList;
begin
  result := TFhirElementDefinitionTypeList(inherited Clone);
end;

function TFhirElementDefinitionTypeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirElementDefinitionTypeList.GetItemN(index: Integer): TFhirElementDefinitionType;
begin
  result := TFhirElementDefinitionType(ObjectByIndex[index]);
end;

function TFhirElementDefinitionTypeList.ItemClass: TFslObjectClass;
begin
  result := TFhirElementDefinitionType;
end;
function TFhirElementDefinitionTypeList.IndexOf(value: TFhirElementDefinitionType): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirElementDefinitionTypeList.Insert(index: Integer): TFhirElementDefinitionType;
begin
  result := TFhirElementDefinitionType.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionTypeList.InsertItem(index: Integer; value: TFhirElementDefinitionType);
begin
  assert(value is TFhirElementDefinitionType);
  Inherited Insert(index, value);
end;

function TFhirElementDefinitionTypeList.Item(index: Integer): TFhirElementDefinitionType;
begin
  result := TFhirElementDefinitionType(ObjectByIndex[index]);
end;

function TFhirElementDefinitionTypeList.Link: TFhirElementDefinitionTypeList;
begin
  result := TFhirElementDefinitionTypeList(inherited Link);
end;

procedure TFhirElementDefinitionTypeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirElementDefinitionTypeList.SetItemByIndex(index: Integer; value: TFhirElementDefinitionType);
begin
  assert(value is TFhirElementDefinitionType);
  FhirElementDefinitionTypes[index] := value;
end;

procedure TFhirElementDefinitionTypeList.SetItemN(index: Integer; value: TFhirElementDefinitionType);
begin
  assert(value is TFhirElementDefinitionType);
  ObjectByIndex[index] := value;
end;

{ TFhirElementDefinitionConstraint }

constructor TFhirElementDefinitionConstraint.Create;
begin
  inherited;
end;

destructor TFhirElementDefinitionConstraint.Destroy;
begin
  FKey.free;
  FRequirements.free;
  FSeverity.free;
  FHuman.free;
  FXpath.free;
  inherited;
end;

procedure TFhirElementDefinitionConstraint.Assign(oSource : TFslObject);
begin
  inherited;
  keyElement := TFhirElementDefinitionConstraint(oSource).keyElement.Clone;
  requirementsElement := TFhirElementDefinitionConstraint(oSource).requirementsElement.Clone;
  FSeverity := TFhirElementDefinitionConstraint(oSource).FSeverity.Link;
  humanElement := TFhirElementDefinitionConstraint(oSource).humanElement.Clone;
  xpathElement := TFhirElementDefinitionConstraint(oSource).xpathElement.Clone;
end;

procedure TFhirElementDefinitionConstraint.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'key') Then
     list.add(self.link, 'key', FKey.Link);
  if (child_name = 'requirements') Then
     list.add(self.link, 'requirements', FRequirements.Link);
  if (child_name = 'severity') Then
     list.add(self.link, 'severity', FSeverity.Link);
  if (child_name = 'human') Then
     list.add(self.link, 'human', FHuman.Link);
  if (child_name = 'xpath') Then
     list.add(self.link, 'xpath', FXpath.Link);
end;

procedure TFhirElementDefinitionConstraint.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'key', 'id', false, TFhirId, FKey.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requirements', 'string', false, TFhirString, FRequirements.Link));{2}
  oList.add(TFHIRProperty.create(self, 'severity', 'code', false, TFHIREnum, FSeverity.Link));{1}
  oList.add(TFHIRProperty.create(self, 'human', 'string', false, TFhirString, FHuman.Link));{2}
  oList.add(TFHIRProperty.create(self, 'xpath', 'string', false, TFhirString, FXpath.Link));{2}
end;

function TFhirElementDefinitionConstraint.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'key') then
  begin
    KeyElement := asId(propValue){5a};
    result := propValue;
  end
  else if (propName = 'requirements') then
  begin
    RequirementsElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'severity') then
  begin
    SeverityElement := asEnum(SYSTEMS_TFhirConstraintSeverityEnum, CODES_TFhirConstraintSeverityEnum, propValue);
    result := propValue
  end
  else if (propName = 'human') then
  begin
    HumanElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'xpath') then
  begin
    XpathElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirElementDefinitionConstraint.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirElementDefinitionConstraint.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'key') then result := TFhirId.create() {5b}
  else if (propName = 'requirements') then result := TFhirString.create() {5b}
  else if (propName = 'human') then result := TFhirString.create() {5b}
  else if (propName = 'xpath') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirElementDefinitionConstraint.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'key') then result := 'id'
  else if (propName = 'requirements') then result := 'string'
  else if (propName = 'severity') then result := 'code'
  else if (propName = 'human') then result := 'string'
  else if (propName = 'xpath') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirElementDefinitionConstraint.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'key') then KeyElement := nil
  else if (propName = 'requirements') then RequirementsElement := nil
  else if (propName = 'severity') then SeverityElement := nil
  else if (propName = 'human') then HumanElement := nil
  else if (propName = 'xpath') then XpathElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirElementDefinitionConstraint.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'key') then KeyElement := asId(new){5b}
  else if (propName = 'requirements') then RequirementsElement := asString(new){5b}
  else if (propName = 'severity') then SeverityElement := asEnum(SYSTEMS_TFhirConstraintSeverityEnum, CODES_TFhirConstraintSeverityEnum, new){4}
  else if (propName = 'human') then HumanElement := asString(new){5b}
  else if (propName = 'xpath') then XpathElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirElementDefinitionConstraint.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirElementDefinitionConstraint.fhirType : string;
begin
  result := 'constraint';
end;

function TFhirElementDefinitionConstraint.Link : TFhirElementDefinitionConstraint;
begin
  result := TFhirElementDefinitionConstraint(inherited Link);
end;

function TFhirElementDefinitionConstraint.Clone : TFhirElementDefinitionConstraint;
begin
  result := TFhirElementDefinitionConstraint(inherited Clone);
end;

function TFhirElementDefinitionConstraint.equals(other : TObject) : boolean; 
var
  o : TFhirElementDefinitionConstraint;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirElementDefinitionConstraint)) then
    result := false
  else
  begin
    o := TFhirElementDefinitionConstraint(other);
    result := compareDeep(keyElement, o.keyElement, true) and compareDeep(requirementsElement, o.requirementsElement, true) and 
      compareDeep(severityElement, o.severityElement, true) and compareDeep(humanElement, o.humanElement, true) and 
      compareDeep(xpathElement, o.xpathElement, true);
  end;
end;

function TFhirElementDefinitionConstraint.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FKey) and isEmptyProp(FRequirements) and isEmptyProp(FSeverity) and isEmptyProp(FHuman) and isEmptyProp(FXpath);
end;

procedure TFhirElementDefinitionConstraint.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('key');
  fields.add('requirements');
  fields.add('severity');
  fields.add('human');
  fields.add('xpath');
end;

{ TFhirElementDefinitionConstraint }

Procedure TFhirElementDefinitionConstraint.SetKey(value : TFhirId);
begin
  FKey.free;
  FKey := value;
end;

Function TFhirElementDefinitionConstraint.GetKeyST : String;
begin
  if FKey = nil then
    result := ''
  else
    result := FKey.value;
end;

Procedure TFhirElementDefinitionConstraint.SetKeyST(value : String);
begin
  if value <> '' then
  begin
    if FKey = nil then
      FKey := TFhirId.create;
    FKey.value := value
  end
  else if FKey <> nil then
    FKey.value := '';
end;

Procedure TFhirElementDefinitionConstraint.SetRequirements(value : TFhirString);
begin
  FRequirements.free;
  FRequirements := value;
end;

Function TFhirElementDefinitionConstraint.GetRequirementsST : String;
begin
  if FRequirements = nil then
    result := ''
  else
    result := FRequirements.value;
end;

Procedure TFhirElementDefinitionConstraint.SetRequirementsST(value : String);
begin
  if value <> '' then
  begin
    if FRequirements = nil then
      FRequirements := TFhirString.create;
    FRequirements.value := value
  end
  else if FRequirements <> nil then
    FRequirements.value := '';
end;

Procedure TFhirElementDefinitionConstraint.SetSeverity(value : TFhirEnum);
begin
  FSeverity.free;
  FSeverity := value;
end;

Function TFhirElementDefinitionConstraint.GetSeverityST : TFhirConstraintSeverityEnum;
begin
  if FSeverity = nil then
    result := TFhirConstraintSeverityEnum(0)
  else
    result := TFhirConstraintSeverityEnum(StringArrayIndexOfSensitive(CODES_TFhirConstraintSeverityEnum, FSeverity.value));
end;

Procedure TFhirElementDefinitionConstraint.SetSeverityST(value : TFhirConstraintSeverityEnum);
begin
  if ord(value) = 0 then
    SeverityElement := nil
  else
    SeverityElement := TFhirEnum.create(SYSTEMS_TFhirConstraintSeverityEnum[value], CODES_TFhirConstraintSeverityEnum[value]);
end;

Procedure TFhirElementDefinitionConstraint.SetHuman(value : TFhirString);
begin
  FHuman.free;
  FHuman := value;
end;

Function TFhirElementDefinitionConstraint.GetHumanST : String;
begin
  if FHuman = nil then
    result := ''
  else
    result := FHuman.value;
end;

Procedure TFhirElementDefinitionConstraint.SetHumanST(value : String);
begin
  if value <> '' then
  begin
    if FHuman = nil then
      FHuman := TFhirString.create;
    FHuman.value := value
  end
  else if FHuman <> nil then
    FHuman.value := '';
end;

Procedure TFhirElementDefinitionConstraint.SetXpath(value : TFhirString);
begin
  FXpath.free;
  FXpath := value;
end;

Function TFhirElementDefinitionConstraint.GetXpathST : String;
begin
  if FXpath = nil then
    result := ''
  else
    result := FXpath.value;
end;

Procedure TFhirElementDefinitionConstraint.SetXpathST(value : String);
begin
  if value <> '' then
  begin
    if FXpath = nil then
      FXpath := TFhirString.create;
    FXpath.value := value
  end
  else if FXpath <> nil then
    FXpath.value := '';
end;

function TFhirElementDefinitionConstraint.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FKey.sizeInBytes);
  inc(result, FRequirements.sizeInBytes);
  inc(result, FSeverity.sizeInBytes);
  inc(result, FHuman.sizeInBytes);
  inc(result, FXpath.sizeInBytes);
end;

{ TFhirElementDefinitionConstraintListEnumerator }

Constructor TFhirElementDefinitionConstraintListEnumerator.Create(list : TFhirElementDefinitionConstraintList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirElementDefinitionConstraintListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirElementDefinitionConstraintListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirElementDefinitionConstraintListEnumerator.GetCurrent : TFhirElementDefinitionConstraint;
begin
  Result := FList[FIndex];
end;

function TFhirElementDefinitionConstraintListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirElementDefinitionConstraintList }
procedure TFhirElementDefinitionConstraintList.AddItem(value: TFhirElementDefinitionConstraint);
begin
  assert(value.ClassName = 'TFhirElementDefinitionConstraint', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirElementDefinitionConstraint');
  add(value);
end;

function TFhirElementDefinitionConstraintList.Append: TFhirElementDefinitionConstraint;
begin
  result := TFhirElementDefinitionConstraint.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionConstraintList.ClearItems;
begin
  Clear;
end;

function TFhirElementDefinitionConstraintList.GetEnumerator : TFhirElementDefinitionConstraintListEnumerator;
begin
  result := TFhirElementDefinitionConstraintListEnumerator.Create(self.link);
end;

function TFhirElementDefinitionConstraintList.Clone: TFhirElementDefinitionConstraintList;
begin
  result := TFhirElementDefinitionConstraintList(inherited Clone);
end;

function TFhirElementDefinitionConstraintList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirElementDefinitionConstraintList.GetItemN(index: Integer): TFhirElementDefinitionConstraint;
begin
  result := TFhirElementDefinitionConstraint(ObjectByIndex[index]);
end;

function TFhirElementDefinitionConstraintList.ItemClass: TFslObjectClass;
begin
  result := TFhirElementDefinitionConstraint;
end;
function TFhirElementDefinitionConstraintList.IndexOf(value: TFhirElementDefinitionConstraint): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirElementDefinitionConstraintList.Insert(index: Integer): TFhirElementDefinitionConstraint;
begin
  result := TFhirElementDefinitionConstraint.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionConstraintList.InsertItem(index: Integer; value: TFhirElementDefinitionConstraint);
begin
  assert(value is TFhirElementDefinitionConstraint);
  Inherited Insert(index, value);
end;

function TFhirElementDefinitionConstraintList.Item(index: Integer): TFhirElementDefinitionConstraint;
begin
  result := TFhirElementDefinitionConstraint(ObjectByIndex[index]);
end;

function TFhirElementDefinitionConstraintList.Link: TFhirElementDefinitionConstraintList;
begin
  result := TFhirElementDefinitionConstraintList(inherited Link);
end;

procedure TFhirElementDefinitionConstraintList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirElementDefinitionConstraintList.SetItemByIndex(index: Integer; value: TFhirElementDefinitionConstraint);
begin
  assert(value is TFhirElementDefinitionConstraint);
  FhirElementDefinitionConstraints[index] := value;
end;

procedure TFhirElementDefinitionConstraintList.SetItemN(index: Integer; value: TFhirElementDefinitionConstraint);
begin
  assert(value is TFhirElementDefinitionConstraint);
  ObjectByIndex[index] := value;
end;

{ TFhirElementDefinitionBinding }

constructor TFhirElementDefinitionBinding.Create;
begin
  inherited;
end;

destructor TFhirElementDefinitionBinding.Destroy;
begin
  FStrength.free;
  FDescription.free;
  FValueSet.free;
  inherited;
end;

procedure TFhirElementDefinitionBinding.Assign(oSource : TFslObject);
begin
  inherited;
  FStrength := TFhirElementDefinitionBinding(oSource).FStrength.Link;
  descriptionElement := TFhirElementDefinitionBinding(oSource).descriptionElement.Clone;
  valueSet := TFhirElementDefinitionBinding(oSource).valueSet.Clone;
end;

procedure TFhirElementDefinitionBinding.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'strength') Then
     list.add(self.link, 'strength', FStrength.Link);
  if (child_name = 'description') Then
     list.add(self.link, 'description', FDescription.Link);
  if (child_name = 'valueSet[x]') or (child_name = 'valueSet') Then
     list.add(self.link, 'valueSet[x]', FValueSet.Link);
end;

procedure TFhirElementDefinitionBinding.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'strength', 'code', false, TFHIREnum, FStrength.Link));{1}
  oList.add(TFHIRProperty.create(self, 'description', 'string', false, TFhirString, FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'valueSet[x]', 'uri|Reference(ValueSet)', false, TFhirType, FValueSet.Link));{2}
end;

function TFhirElementDefinitionBinding.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'strength') then
  begin
    StrengthElement := asEnum(SYSTEMS_TFhirBindingStrengthEnum, CODES_TFhirBindingStrengthEnum, propValue);
    result := propValue
  end
  else if (propName = 'description') then
  begin
    DescriptionElement := asString(propValue){5a};
    result := propValue;
  end
  else if (isMatchingName(propName, 'valueSet', ['Uri', 'Reference'])) then
  begin
    ValueSet := propValue as TFhirType{4};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirElementDefinitionBinding.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirElementDefinitionBinding.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'description') then result := TFhirString.create() {5b}
  else if (isMatchingName(propName, 'valueSet', ['Uri', 'Reference'])) then raise EFHIRException.create('Cannot make property ValueSet'){4x}
  else result := inherited createPropertyValue(propName);
end;

function TFhirElementDefinitionBinding.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'strength') then result := 'code'
  else if (propName = 'description') then result := 'string'
  else if (propName = 'valueSet[x]') then result := 'uri|Reference'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirElementDefinitionBinding.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'strength') then StrengthElement := nil
  else if (propName = 'description') then DescriptionElement := nil
  else if (isMatchingName(propName, 'valueSet', ['Uri', 'Reference'])) then ValueSetElement := nil{4x}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirElementDefinitionBinding.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'strength') then StrengthElement := asEnum(SYSTEMS_TFhirBindingStrengthEnum, CODES_TFhirBindingStrengthEnum, new){4}
  else if (propName = 'description') then DescriptionElement := asString(new){5b}
  else if (isMatchingName(propName, 'valueSet', ['Uri', 'Reference'])) then ValueSetElement := new as TFhirType{4x}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirElementDefinitionBinding.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirElementDefinitionBinding.fhirType : string;
begin
  result := 'binding';
end;

function TFhirElementDefinitionBinding.Link : TFhirElementDefinitionBinding;
begin
  result := TFhirElementDefinitionBinding(inherited Link);
end;

function TFhirElementDefinitionBinding.Clone : TFhirElementDefinitionBinding;
begin
  result := TFhirElementDefinitionBinding(inherited Clone);
end;

function TFhirElementDefinitionBinding.equals(other : TObject) : boolean; 
var
  o : TFhirElementDefinitionBinding;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirElementDefinitionBinding)) then
    result := false
  else
  begin
    o := TFhirElementDefinitionBinding(other);
    result := compareDeep(strengthElement, o.strengthElement, true) and compareDeep(descriptionElement, o.descriptionElement, true) and 
      compareDeep(valueSetElement, o.valueSetElement, true);
  end;
end;

function TFhirElementDefinitionBinding.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FStrength) and isEmptyProp(FDescription) and isEmptyProp(FValueSet);
end;

procedure TFhirElementDefinitionBinding.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('strength');
  fields.add('description');
  fields.add('valueSet[x]');
end;

{ TFhirElementDefinitionBinding }

Procedure TFhirElementDefinitionBinding.SetStrength(value : TFhirEnum);
begin
  FStrength.free;
  FStrength := value;
end;

Function TFhirElementDefinitionBinding.GetStrengthST : TFhirBindingStrengthEnum;
begin
  if FStrength = nil then
    result := TFhirBindingStrengthEnum(0)
  else
    result := TFhirBindingStrengthEnum(StringArrayIndexOfSensitive(CODES_TFhirBindingStrengthEnum, FStrength.value));
end;

Procedure TFhirElementDefinitionBinding.SetStrengthST(value : TFhirBindingStrengthEnum);
begin
  if ord(value) = 0 then
    StrengthElement := nil
  else
    StrengthElement := TFhirEnum.create(SYSTEMS_TFhirBindingStrengthEnum[value], CODES_TFhirBindingStrengthEnum[value]);
end;

Procedure TFhirElementDefinitionBinding.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirElementDefinitionBinding.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := FDescription.value;
end;

Procedure TFhirElementDefinitionBinding.SetDescriptionST(value : String);
begin
  if value <> '' then
  begin
    if FDescription = nil then
      FDescription := TFhirString.create;
    FDescription.value := value
  end
  else if FDescription <> nil then
    FDescription.value := '';
end;

Procedure TFhirElementDefinitionBinding.SetValueSet(value : TFhirType);
begin
  FValueSet.free;
  FValueSet := value;
end;

function TFhirElementDefinitionBinding.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStrength.sizeInBytes);
  inc(result, FDescription.sizeInBytes);
  inc(result, FValueSet.sizeInBytes);
end;

{ TFhirElementDefinitionBindingListEnumerator }

Constructor TFhirElementDefinitionBindingListEnumerator.Create(list : TFhirElementDefinitionBindingList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirElementDefinitionBindingListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirElementDefinitionBindingListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirElementDefinitionBindingListEnumerator.GetCurrent : TFhirElementDefinitionBinding;
begin
  Result := FList[FIndex];
end;

function TFhirElementDefinitionBindingListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirElementDefinitionBindingList }
procedure TFhirElementDefinitionBindingList.AddItem(value: TFhirElementDefinitionBinding);
begin
  assert(value.ClassName = 'TFhirElementDefinitionBinding', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirElementDefinitionBinding');
  add(value);
end;

function TFhirElementDefinitionBindingList.Append: TFhirElementDefinitionBinding;
begin
  result := TFhirElementDefinitionBinding.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionBindingList.ClearItems;
begin
  Clear;
end;

function TFhirElementDefinitionBindingList.GetEnumerator : TFhirElementDefinitionBindingListEnumerator;
begin
  result := TFhirElementDefinitionBindingListEnumerator.Create(self.link);
end;

function TFhirElementDefinitionBindingList.Clone: TFhirElementDefinitionBindingList;
begin
  result := TFhirElementDefinitionBindingList(inherited Clone);
end;

function TFhirElementDefinitionBindingList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirElementDefinitionBindingList.GetItemN(index: Integer): TFhirElementDefinitionBinding;
begin
  result := TFhirElementDefinitionBinding(ObjectByIndex[index]);
end;

function TFhirElementDefinitionBindingList.ItemClass: TFslObjectClass;
begin
  result := TFhirElementDefinitionBinding;
end;
function TFhirElementDefinitionBindingList.IndexOf(value: TFhirElementDefinitionBinding): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirElementDefinitionBindingList.Insert(index: Integer): TFhirElementDefinitionBinding;
begin
  result := TFhirElementDefinitionBinding.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionBindingList.InsertItem(index: Integer; value: TFhirElementDefinitionBinding);
begin
  assert(value is TFhirElementDefinitionBinding);
  Inherited Insert(index, value);
end;

function TFhirElementDefinitionBindingList.Item(index: Integer): TFhirElementDefinitionBinding;
begin
  result := TFhirElementDefinitionBinding(ObjectByIndex[index]);
end;

function TFhirElementDefinitionBindingList.Link: TFhirElementDefinitionBindingList;
begin
  result := TFhirElementDefinitionBindingList(inherited Link);
end;

procedure TFhirElementDefinitionBindingList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirElementDefinitionBindingList.SetItemByIndex(index: Integer; value: TFhirElementDefinitionBinding);
begin
  assert(value is TFhirElementDefinitionBinding);
  FhirElementDefinitionBindings[index] := value;
end;

procedure TFhirElementDefinitionBindingList.SetItemN(index: Integer; value: TFhirElementDefinitionBinding);
begin
  assert(value is TFhirElementDefinitionBinding);
  ObjectByIndex[index] := value;
end;

{ TFhirElementDefinitionMapping }

constructor TFhirElementDefinitionMapping.Create;
begin
  inherited;
end;

destructor TFhirElementDefinitionMapping.Destroy;
begin
  FIdentity.free;
  FLanguage.free;
  FMap.free;
  inherited;
end;

procedure TFhirElementDefinitionMapping.Assign(oSource : TFslObject);
begin
  inherited;
  identityElement := TFhirElementDefinitionMapping(oSource).identityElement.Clone;
  languageElement := TFhirElementDefinitionMapping(oSource).languageElement.Clone;
  mapElement := TFhirElementDefinitionMapping(oSource).mapElement.Clone;
end;

procedure TFhirElementDefinitionMapping.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identity') Then
     list.add(self.link, 'identity', FIdentity.Link);
  if (child_name = 'language') Then
     list.add(self.link, 'language', FLanguage.Link);
  if (child_name = 'map') Then
     list.add(self.link, 'map', FMap.Link);
end;

procedure TFhirElementDefinitionMapping.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identity', 'id', false, TFhirId, FIdentity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'language', 'code', false, TFhirCode, FLanguage.Link));{2}
  oList.add(TFHIRProperty.create(self, 'map', 'string', false, TFhirString, FMap.Link));{2}
end;

function TFhirElementDefinitionMapping.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identity') then
  begin
    IdentityElement := asId(propValue){5a};
    result := propValue;
  end
  else if (propName = 'language') then
  begin
    LanguageElement := asCode(propValue);
    result := propValue;
  end
  else if (propName = 'map') then
  begin
    MapElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirElementDefinitionMapping.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirElementDefinitionMapping.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'identity') then result := TFhirId.create() {5b}
  else if (propName = 'language') then result := TFhirCode.create() {5b}
  else if (propName = 'map') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirElementDefinitionMapping.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identity') then result := 'id'
  else if (propName = 'language') then result := 'code'
  else if (propName = 'map') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirElementDefinitionMapping.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'identity') then IdentityElement := nil
  else if (propName = 'language') then LanguageElement := nil
  else if (propName = 'map') then MapElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirElementDefinitionMapping.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identity') then IdentityElement := asId(new){5b}
  else if (propName = 'language') then LanguageElement := asCode(new){5b}
  else if (propName = 'map') then MapElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirElementDefinitionMapping.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirElementDefinitionMapping.fhirType : string;
begin
  result := 'mapping';
end;

function TFhirElementDefinitionMapping.Link : TFhirElementDefinitionMapping;
begin
  result := TFhirElementDefinitionMapping(inherited Link);
end;

function TFhirElementDefinitionMapping.Clone : TFhirElementDefinitionMapping;
begin
  result := TFhirElementDefinitionMapping(inherited Clone);
end;

function TFhirElementDefinitionMapping.equals(other : TObject) : boolean; 
var
  o : TFhirElementDefinitionMapping;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirElementDefinitionMapping)) then
    result := false
  else
  begin
    o := TFhirElementDefinitionMapping(other);
    result := compareDeep(identityElement, o.identityElement, true) and compareDeep(languageElement, o.languageElement, true) and 
      compareDeep(mapElement, o.mapElement, true);
  end;
end;

function TFhirElementDefinitionMapping.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FIdentity) and isEmptyProp(FLanguage) and isEmptyProp(FMap);
end;

procedure TFhirElementDefinitionMapping.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('identity');
  fields.add('language');
  fields.add('map');
end;

{ TFhirElementDefinitionMapping }

Procedure TFhirElementDefinitionMapping.SetIdentity(value : TFhirId);
begin
  FIdentity.free;
  FIdentity := value;
end;

Function TFhirElementDefinitionMapping.GetIdentityST : String;
begin
  if FIdentity = nil then
    result := ''
  else
    result := FIdentity.value;
end;

Procedure TFhirElementDefinitionMapping.SetIdentityST(value : String);
begin
  if value <> '' then
  begin
    if FIdentity = nil then
      FIdentity := TFhirId.create;
    FIdentity.value := value
  end
  else if FIdentity <> nil then
    FIdentity.value := '';
end;

Procedure TFhirElementDefinitionMapping.SetLanguage(value : TFhirCode);
begin
  FLanguage.free;
  FLanguage := value;
end;

Function TFhirElementDefinitionMapping.GetLanguageST : String;
begin
  if FLanguage = nil then
    result := ''
  else
    result := FLanguage.value;
end;

Procedure TFhirElementDefinitionMapping.SetLanguageST(value : String);
begin
  if value <> '' then
  begin
    if FLanguage = nil then
      FLanguage := TFhirCode.create;
    FLanguage.value := value
  end
  else if FLanguage <> nil then
    FLanguage.value := '';
end;

Procedure TFhirElementDefinitionMapping.SetMap(value : TFhirString);
begin
  FMap.free;
  FMap := value;
end;

Function TFhirElementDefinitionMapping.GetMapST : String;
begin
  if FMap = nil then
    result := ''
  else
    result := FMap.value;
end;

Procedure TFhirElementDefinitionMapping.SetMapST(value : String);
begin
  if value <> '' then
  begin
    if FMap = nil then
      FMap := TFhirString.create;
    FMap.value := value
  end
  else if FMap <> nil then
    FMap.value := '';
end;

function TFhirElementDefinitionMapping.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FIdentity.sizeInBytes);
  inc(result, FLanguage.sizeInBytes);
  inc(result, FMap.sizeInBytes);
end;

{ TFhirElementDefinitionMappingListEnumerator }

Constructor TFhirElementDefinitionMappingListEnumerator.Create(list : TFhirElementDefinitionMappingList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirElementDefinitionMappingListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirElementDefinitionMappingListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirElementDefinitionMappingListEnumerator.GetCurrent : TFhirElementDefinitionMapping;
begin
  Result := FList[FIndex];
end;

function TFhirElementDefinitionMappingListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirElementDefinitionMappingList }
procedure TFhirElementDefinitionMappingList.AddItem(value: TFhirElementDefinitionMapping);
begin
  assert(value.ClassName = 'TFhirElementDefinitionMapping', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirElementDefinitionMapping');
  add(value);
end;

function TFhirElementDefinitionMappingList.Append: TFhirElementDefinitionMapping;
begin
  result := TFhirElementDefinitionMapping.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionMappingList.ClearItems;
begin
  Clear;
end;

function TFhirElementDefinitionMappingList.GetEnumerator : TFhirElementDefinitionMappingListEnumerator;
begin
  result := TFhirElementDefinitionMappingListEnumerator.Create(self.link);
end;

function TFhirElementDefinitionMappingList.Clone: TFhirElementDefinitionMappingList;
begin
  result := TFhirElementDefinitionMappingList(inherited Clone);
end;

function TFhirElementDefinitionMappingList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirElementDefinitionMappingList.GetItemN(index: Integer): TFhirElementDefinitionMapping;
begin
  result := TFhirElementDefinitionMapping(ObjectByIndex[index]);
end;

function TFhirElementDefinitionMappingList.ItemClass: TFslObjectClass;
begin
  result := TFhirElementDefinitionMapping;
end;
function TFhirElementDefinitionMappingList.IndexOf(value: TFhirElementDefinitionMapping): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirElementDefinitionMappingList.Insert(index: Integer): TFhirElementDefinitionMapping;
begin
  result := TFhirElementDefinitionMapping.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionMappingList.InsertItem(index: Integer; value: TFhirElementDefinitionMapping);
begin
  assert(value is TFhirElementDefinitionMapping);
  Inherited Insert(index, value);
end;

function TFhirElementDefinitionMappingList.Item(index: Integer): TFhirElementDefinitionMapping;
begin
  result := TFhirElementDefinitionMapping(ObjectByIndex[index]);
end;

function TFhirElementDefinitionMappingList.Link: TFhirElementDefinitionMappingList;
begin
  result := TFhirElementDefinitionMappingList(inherited Link);
end;

procedure TFhirElementDefinitionMappingList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirElementDefinitionMappingList.SetItemByIndex(index: Integer; value: TFhirElementDefinitionMapping);
begin
  assert(value is TFhirElementDefinitionMapping);
  FhirElementDefinitionMappings[index] := value;
end;

procedure TFhirElementDefinitionMappingList.SetItemN(index: Integer; value: TFhirElementDefinitionMapping);
begin
  assert(value is TFhirElementDefinitionMapping);
  ObjectByIndex[index] := value;
end;

{ TFhirElementDefinition }

constructor TFhirElementDefinition.Create;
begin
  inherited;
end;

destructor TFhirElementDefinition.Destroy;
begin
  FPath.free;
  FRepresentation.Free;
  FName.free;
  FLabel_.free;
  FCodeList.Free;
  FSlicing.free;
  FShort.free;
  FDefinition.free;
  FComments.free;
  FRequirements.free;
  FAliasList.Free;
  FMin.free;
  FMax.free;
  FBase.free;
  FType_List.Free;
  FNameReference.free;
  FDefaultValue.free;
  FMeaningWhenMissing.free;
  FFixed.free;
  FPattern.free;
  FExample.free;
  FMinValue.free;
  FMaxValue.free;
  FMaxLength.free;
  FConditionList.Free;
  FConstraintList.Free;
  FMustSupport.free;
  FIsModifier.free;
  FIsSummary.free;
  FBinding.free;
  FMappingList.Free;
  inherited;
end;

procedure TFhirElementDefinition.Assign(oSource : TFslObject);
begin
  inherited;
  pathElement := TFhirElementDefinition(oSource).pathElement.Clone;
  if (TFhirElementDefinition(oSource).FRepresentation = nil) then
  begin
    FRepresentation.free;
    FRepresentation := nil;
  end
  else
  begin
    FRepresentation := TFHIREnumList.Create(SYSTEMS_TFhirPropertyRepresentationEnum, CODES_TFhirPropertyRepresentationEnum);
    FRepresentation.Assign(TFhirElementDefinition(oSource).FRepresentation);
  end;
  nameElement := TFhirElementDefinition(oSource).nameElement.Clone;
  label_Element := TFhirElementDefinition(oSource).label_Element.Clone;
  if (TFhirElementDefinition(oSource).FCodeList = nil) then
  begin
    FCodeList.free;
    FCodeList := nil;
  end
  else
  begin
    if FCodeList = nil then
      FCodeList := TFhirCodingList.Create;
    FCodeList.Assign(TFhirElementDefinition(oSource).FCodeList);
  end;
  slicing := TFhirElementDefinition(oSource).slicing.Clone;
  shortElement := TFhirElementDefinition(oSource).shortElement.Clone;
  definitionElement := TFhirElementDefinition(oSource).definitionElement.Clone;
  commentsElement := TFhirElementDefinition(oSource).commentsElement.Clone;
  requirementsElement := TFhirElementDefinition(oSource).requirementsElement.Clone;
  if (TFhirElementDefinition(oSource).FAliasList = nil) then
  begin
    FAliasList.free;
    FAliasList := nil;
  end
  else
  begin
    if FAliasList = nil then
      FAliasList := TFhirStringList.Create;
    FAliasList.Assign(TFhirElementDefinition(oSource).FAliasList);
  end;
  minElement := TFhirElementDefinition(oSource).minElement.Clone;
  maxElement := TFhirElementDefinition(oSource).maxElement.Clone;
  base := TFhirElementDefinition(oSource).base.Clone;
  if (TFhirElementDefinition(oSource).FType_List = nil) then
  begin
    FType_List.free;
    FType_List := nil;
  end
  else
  begin
    if FType_List = nil then
      FType_List := TFhirElementDefinitionTypeList.Create;
    FType_List.Assign(TFhirElementDefinition(oSource).FType_List);
  end;
  nameReferenceElement := TFhirElementDefinition(oSource).nameReferenceElement.Clone;
  defaultValue := TFhirElementDefinition(oSource).defaultValue.Clone;
  meaningWhenMissingElement := TFhirElementDefinition(oSource).meaningWhenMissingElement.Clone;
  fixed := TFhirElementDefinition(oSource).fixed.Clone;
  pattern := TFhirElementDefinition(oSource).pattern.Clone;
  example := TFhirElementDefinition(oSource).example.Clone;
  minValue := TFhirElementDefinition(oSource).minValue.Clone;
  maxValue := TFhirElementDefinition(oSource).maxValue.Clone;
  maxLengthElement := TFhirElementDefinition(oSource).maxLengthElement.Clone;
  if (TFhirElementDefinition(oSource).FConditionList = nil) then
  begin
    FConditionList.free;
    FConditionList := nil;
  end
  else
  begin
    if FConditionList = nil then
      FConditionList := TFhirIdList.Create;
    FConditionList.Assign(TFhirElementDefinition(oSource).FConditionList);
  end;
  if (TFhirElementDefinition(oSource).FConstraintList = nil) then
  begin
    FConstraintList.free;
    FConstraintList := nil;
  end
  else
  begin
    if FConstraintList = nil then
      FConstraintList := TFhirElementDefinitionConstraintList.Create;
    FConstraintList.Assign(TFhirElementDefinition(oSource).FConstraintList);
  end;
  mustSupportElement := TFhirElementDefinition(oSource).mustSupportElement.Clone;
  isModifierElement := TFhirElementDefinition(oSource).isModifierElement.Clone;
  isSummaryElement := TFhirElementDefinition(oSource).isSummaryElement.Clone;
  binding := TFhirElementDefinition(oSource).binding.Clone;
  if (TFhirElementDefinition(oSource).FMappingList = nil) then
  begin
    FMappingList.free;
    FMappingList := nil;
  end
  else
  begin
    if FMappingList = nil then
      FMappingList := TFhirElementDefinitionMappingList.Create;
    FMappingList.Assign(TFhirElementDefinition(oSource).FMappingList);
  end;
end;

procedure TFhirElementDefinition.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'path') Then
     list.add(self.link, 'path', FPath.Link);
  if (child_name = 'representation') Then
     list.addAll(self, 'representation', FRepresentation);
  if (child_name = 'name') Then
     list.add(self.link, 'name', FName.Link);
  if (child_name = 'label') Then
     list.add(self.link, 'label', FLabel_.Link);
  if (child_name = 'code') Then
    list.addAll(self, 'code', FCodeList);
  if (child_name = 'slicing') Then
     list.add(self.link, 'slicing', FSlicing.Link);
  if (child_name = 'short') Then
     list.add(self.link, 'short', FShort.Link);
  if (child_name = 'definition') Then
     list.add(self.link, 'definition', FDefinition.Link);
  if (child_name = 'comments') Then
     list.add(self.link, 'comments', FComments.Link);
  if (child_name = 'requirements') Then
     list.add(self.link, 'requirements', FRequirements.Link);
  if (child_name = 'alias') Then
    list.addAll(self, 'alias', FAliasList);
  if (child_name = 'min') Then
     list.add(self.link, 'min', FMin.Link);
  if (child_name = 'max') Then
     list.add(self.link, 'max', FMax.Link);
  if (child_name = 'base') Then
     list.add(self.link, 'base', FBase.Link);
  if (child_name = 'type') Then
    list.addAll(self, 'type', FType_List);
  if (child_name = 'nameReference') Then
     list.add(self.link, 'nameReference', FNameReference.Link);
  if (child_name = 'defaultValue[x]') or (child_name = 'defaultValue') Then
     list.add(self.link, 'defaultValue[x]', FDefaultValue.Link);
  if (child_name = 'meaningWhenMissing') Then
     list.add(self.link, 'meaningWhenMissing', FMeaningWhenMissing.Link);
  if (child_name = 'fixed[x]') or (child_name = 'fixed') Then
     list.add(self.link, 'fixed[x]', FFixed.Link);
  if (child_name = 'pattern[x]') or (child_name = 'pattern') Then
     list.add(self.link, 'pattern[x]', FPattern.Link);
  if (child_name = 'example[x]') or (child_name = 'example') Then
     list.add(self.link, 'example[x]', FExample.Link);
  if (child_name = 'minValue[x]') or (child_name = 'minValue') Then
     list.add(self.link, 'minValue[x]', FMinValue.Link);
  if (child_name = 'maxValue[x]') or (child_name = 'maxValue') Then
     list.add(self.link, 'maxValue[x]', FMaxValue.Link);
  if (child_name = 'maxLength') Then
     list.add(self.link, 'maxLength', FMaxLength.Link);
  if (child_name = 'condition') Then
    list.addAll(self, 'condition', FConditionList);
  if (child_name = 'constraint') Then
    list.addAll(self, 'constraint', FConstraintList);
  if (child_name = 'mustSupport') Then
     list.add(self.link, 'mustSupport', FMustSupport.Link);
  if (child_name = 'isModifier') Then
     list.add(self.link, 'isModifier', FIsModifier.Link);
  if (child_name = 'isSummary') Then
     list.add(self.link, 'isSummary', FIsSummary.Link);
  if (child_name = 'binding') Then
     list.add(self.link, 'binding', FBinding.Link);
  if (child_name = 'mapping') Then
    list.addAll(self, 'mapping', FMappingList);
end;

procedure TFhirElementDefinition.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'path', 'string', false, TFhirString, FPath.Link));{2}
  oList.add(TFHIRProperty.create(self, 'representation', 'code', true, TFHIREnum, FRepresentation.Link)){3};
  oList.add(TFHIRProperty.create(self, 'name', 'string', false, TFhirString, FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'label', 'string', false, TFhirString, FLabel_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'Coding', true, TFhirCoding, FCodeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'slicing', '', false, TFhirElementDefinitionSlicing, FSlicing.Link));{2}
  oList.add(TFHIRProperty.create(self, 'short', 'string', false, TFhirString, FShort.Link));{2}
  oList.add(TFHIRProperty.create(self, 'definition', 'markdown', false, TFhirMarkdown, FDefinition.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comments', 'markdown', false, TFhirMarkdown, FComments.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requirements', 'markdown', false, TFhirMarkdown, FRequirements.Link));{2}
  oList.add(TFHIRProperty.create(self, 'alias', 'string', true, TFhirString, FAliasList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'min', 'integer', false, TFhirInteger, FMin.Link));{2}
  oList.add(TFHIRProperty.create(self, 'max', 'string', false, TFhirString, FMax.Link));{2}
  oList.add(TFHIRProperty.create(self, 'base', '', false, TFhirElementDefinitionBase, FBase.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', '', true, TFhirElementDefinitionType, FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'nameReference', 'string', false, TFhirString, FNameReference.Link));{2}
  oList.add(TFHIRProperty.create(self, 'defaultValue[x]', '*', false, TFhirType, FDefaultValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'meaningWhenMissing', 'markdown', false, TFhirMarkdown, FMeaningWhenMissing.Link));{2}
  oList.add(TFHIRProperty.create(self, 'fixed[x]', '*', false, TFhirType, FFixed.Link));{2}
  oList.add(TFHIRProperty.create(self, 'pattern[x]', '*', false, TFhirType, FPattern.Link));{2}
  oList.add(TFHIRProperty.create(self, 'example[x]', '*', false, TFhirType, FExample.Link));{2}
  oList.add(TFHIRProperty.create(self, 'minValue[x]', '*', false, TFhirType, FMinValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'maxValue[x]', '*', false, TFhirType, FMaxValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'maxLength', 'integer', false, TFhirInteger, FMaxLength.Link));{2}
  oList.add(TFHIRProperty.create(self, 'condition', 'id', true, TFhirId, FConditionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'constraint', '', true, TFhirElementDefinitionConstraint, FConstraintList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'mustSupport', 'boolean', false, TFhirBoolean, FMustSupport.Link));{2}
  oList.add(TFHIRProperty.create(self, 'isModifier', 'boolean', false, TFhirBoolean, FIsModifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'isSummary', 'boolean', false, TFhirBoolean, FIsSummary.Link));{2}
  oList.add(TFHIRProperty.create(self, 'binding', '', false, TFhirElementDefinitionBinding, FBinding.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mapping', '', true, TFhirElementDefinitionMapping, FMappingList.Link)){3};
end;

function TFhirElementDefinition.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'path') then
  begin
    PathElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'representation') then
  begin
    RepresentationList.add(asEnum(SYSTEMS_TFhirPropertyRepresentationEnum, CODES_TFhirPropertyRepresentationEnum, propValue)); {1}
    result := propValue;
  end
  else if (propName = 'name') then
  begin
    NameElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'label') then
  begin
    Label_Element := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'code') then
  begin
    CodeList.add(propValue as TFhirCoding){2a};
    result := propValue;
  end
  else if (propName = 'slicing') then
  begin
    Slicing := propValue as TFhirElementDefinitionSlicing{4b};
    result := propValue;
  end
  else if (propName = 'short') then
  begin
    ShortElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'definition') then
  begin
    DefinitionElement := asMarkdown(propValue){5a};
    result := propValue;
  end
  else if (propName = 'comments') then
  begin
    CommentsElement := asMarkdown(propValue){5a};
    result := propValue;
  end
  else if (propName = 'requirements') then
  begin
    RequirementsElement := asMarkdown(propValue){5a};
    result := propValue;
  end
  else if (propName = 'alias') then
  begin
    AliasList.add(asString(propValue)){2};     result := propValue;

  end
  else if (propName = 'min') then
  begin
    MinElement := asInteger(propValue){5a};
    result := propValue;
  end
  else if (propName = 'max') then
  begin
    MaxElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'base') then
  begin
    Base := propValue as TFhirElementDefinitionBase{4b};
    result := propValue;
  end
  else if (propName = 'type') then
  begin
    Type_List.add(propValue as TFhirElementDefinitionType){2a};
    result := propValue;
  end
  else if (propName = 'nameReference') then
  begin
    NameReferenceElement := asString(propValue){5a};
    result := propValue;
  end
  else if (isMatchingName(propName, 'defaultValue', ['*'])) then
  begin
    DefaultValue := propValue as TFhirType{4};
    result := propValue;
  end
  else if (propName = 'meaningWhenMissing') then
  begin
    MeaningWhenMissingElement := asMarkdown(propValue){5a};
    result := propValue;
  end
  else if (isMatchingName(propName, 'fixed', ['*'])) then
  begin
    Fixed := propValue as TFhirType{4};
    result := propValue;
  end
  else if (isMatchingName(propName, 'pattern', ['*'])) then
  begin
    Pattern := propValue as TFhirType{4};
    result := propValue;
  end
  else if (isMatchingName(propName, 'example', ['*'])) then
  begin
    Example := propValue as TFhirType{4};
    result := propValue;
  end
  else if (isMatchingName(propName, 'minValue', ['*'])) then
  begin
    MinValue := propValue as TFhirType{4};
    result := propValue;
  end
  else if (isMatchingName(propName, 'maxValue', ['*'])) then
  begin
    MaxValue := propValue as TFhirType{4};
    result := propValue;
  end
  else if (propName = 'maxLength') then
  begin
    MaxLengthElement := asInteger(propValue){5a};
    result := propValue;
  end
  else if (propName = 'condition') then
  begin
    ConditionList.add(asId(propValue)){2};     result := propValue;

  end
  else if (propName = 'constraint') then
  begin
    ConstraintList.add(propValue as TFhirElementDefinitionConstraint){2a};
    result := propValue;
  end
  else if (propName = 'mustSupport') then
  begin
    MustSupportElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'isModifier') then
  begin
    IsModifierElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'isSummary') then
  begin
    IsSummaryElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'binding') then
  begin
    Binding := propValue as TFhirElementDefinitionBinding{4b};
    result := propValue;
  end
  else if (propName = 'mapping') then
  begin
    MappingList.add(propValue as TFhirElementDefinitionMapping){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirElementDefinition.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'representation') then FRepresentation.insertItem(index, asEnum(SYSTEMS_TFhirPropertyRepresentationEnum, CODES_TFhirPropertyRepresentationEnum, propValue)) {1}
  else if (propName = 'code') then CodeList.insertItem(index, propValue as TFhirCoding){2a}
  else if (propName = 'alias') then AliasList.insertItem(index, asString(propValue)){2}
  else if (propName = 'type') then Type_List.insertItem(index, propValue as TFhirElementDefinitionType){2a}
  else if (propName = 'condition') then ConditionList.insertItem(index, asId(propValue)){2}
  else if (propName = 'constraint') then ConstraintList.insertItem(index, propValue as TFhirElementDefinitionConstraint){2a}
  else if (propName = 'mapping') then MappingList.insertItem(index, propValue as TFhirElementDefinitionMapping){2a}
  else inherited;
end;

function TFhirElementDefinition.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'path') then result := TFhirString.create() {5b}
  else if (propName = 'name') then result := TFhirString.create() {5b}
  else if (propName = 'label') then result := TFhirString.create() {5b}
  else if (propName = 'code') then result := CodeList.new(){2}
  else if (propName = 'slicing') then result := TFhirElementDefinitionSlicing.create(){4b}
  else if (propName = 'short') then result := TFhirString.create() {5b}
  else if (propName = 'definition') then result := TFhirMarkdown.create() {5b}
  else if (propName = 'comments') then result := TFhirMarkdown.create() {5b}
  else if (propName = 'requirements') then result := TFhirMarkdown.create() {5b}
  else if (propName = 'alias') then result := AliasList.new(){2}
  else if (propName = 'min') then result := TFhirInteger.create() {5b}
  else if (propName = 'max') then result := TFhirString.create() {5b}
  else if (propName = 'base') then result := TFhirElementDefinitionBase.create(){4b}
  else if (propName = 'type') then result := Type_List.new(){2}
  else if (propName = 'nameReference') then result := TFhirString.create() {5b}
  else if (isMatchingName(propName, 'defaultValue', ['*'])) then raise EFHIRException.create('Cannot make property DefaultValue'){4x}
  else if (propName = 'meaningWhenMissing') then result := TFhirMarkdown.create() {5b}
  else if (isMatchingName(propName, 'fixed', ['*'])) then raise EFHIRException.create('Cannot make property Fixed'){4x}
  else if (isMatchingName(propName, 'pattern', ['*'])) then raise EFHIRException.create('Cannot make property Pattern'){4x}
  else if (isMatchingName(propName, 'example', ['*'])) then raise EFHIRException.create('Cannot make property Example'){4x}
  else if (isMatchingName(propName, 'minValue', ['*'])) then raise EFHIRException.create('Cannot make property MinValue'){4x}
  else if (isMatchingName(propName, 'maxValue', ['*'])) then raise EFHIRException.create('Cannot make property MaxValue'){4x}
  else if (propName = 'maxLength') then result := TFhirInteger.create() {5b}
  else if (propName = 'condition') then result := ConditionList.new(){2}
  else if (propName = 'constraint') then result := ConstraintList.new(){2}
  else if (propName = 'mustSupport') then result := TFhirBoolean.create() {5b}
  else if (propName = 'isModifier') then result := TFhirBoolean.create() {5b}
  else if (propName = 'isSummary') then result := TFhirBoolean.create() {5b}
  else if (propName = 'binding') then result := TFhirElementDefinitionBinding.create(){4b}
  else if (propName = 'mapping') then result := MappingList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirElementDefinition.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'path') then result := 'string'
  else if (propName = 'representation') then result := 'code'
  else if (propName = 'name') then result := 'string'
  else if (propName = 'label') then result := 'string'
  else if (propName = 'code') then result := 'Coding'
  else if (propName = 'slicing') then result := ''
  else if (propName = 'short') then result := 'string'
  else if (propName = 'definition') then result := 'markdown'
  else if (propName = 'comments') then result := 'markdown'
  else if (propName = 'requirements') then result := 'markdown'
  else if (propName = 'alias') then result := 'string'
  else if (propName = 'min') then result := 'integer'
  else if (propName = 'max') then result := 'string'
  else if (propName = 'base') then result := ''
  else if (propName = 'type') then result := ''
  else if (propName = 'nameReference') then result := 'string'
  else if (propName = 'defaultValue[x]') then result := '*'
  else if (propName = 'meaningWhenMissing') then result := 'markdown'
  else if (propName = 'fixed[x]') then result := '*'
  else if (propName = 'pattern[x]') then result := '*'
  else if (propName = 'example[x]') then result := '*'
  else if (propName = 'minValue[x]') then result := '*'
  else if (propName = 'maxValue[x]') then result := '*'
  else if (propName = 'maxLength') then result := 'integer'
  else if (propName = 'condition') then result := 'id'
  else if (propName = 'constraint') then result := ''
  else if (propName = 'mustSupport') then result := 'boolean'
  else if (propName = 'isModifier') then result := 'boolean'
  else if (propName = 'isSummary') then result := 'boolean'
  else if (propName = 'binding') then result := ''
  else if (propName = 'mapping') then result := ''
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirElementDefinition.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'path') then PathElement := nil
  else if (propName = 'name') then NameElement := nil
  else if (propName = 'label') then Label_Element := nil
  else if (propName = 'code') then deletePropertyValue('code', CodeList, value) {2}
  else if (propName = 'slicing') then SlicingElement := nil
  else if (propName = 'short') then ShortElement := nil
  else if (propName = 'definition') then DefinitionElement := nil
  else if (propName = 'comments') then CommentsElement := nil
  else if (propName = 'requirements') then RequirementsElement := nil
  else if (propName = 'alias') then deletePropertyValue('alias', AliasList, value) {2}
  else if (propName = 'min') then MinElement := nil
  else if (propName = 'max') then MaxElement := nil
  else if (propName = 'base') then BaseElement := nil
  else if (propName = 'type') then deletePropertyValue('type', Type_List, value) {2}
  else if (propName = 'nameReference') then NameReferenceElement := nil
  else if (isMatchingName(propName, 'defaultValue', ['*'])) then DefaultValueElement := nil{4x}
  else if (propName = 'meaningWhenMissing') then MeaningWhenMissingElement := nil
  else if (isMatchingName(propName, 'fixed', ['*'])) then FixedElement := nil{4x}
  else if (isMatchingName(propName, 'pattern', ['*'])) then PatternElement := nil{4x}
  else if (isMatchingName(propName, 'example', ['*'])) then ExampleElement := nil{4x}
  else if (isMatchingName(propName, 'minValue', ['*'])) then MinValueElement := nil{4x}
  else if (isMatchingName(propName, 'maxValue', ['*'])) then MaxValueElement := nil{4x}
  else if (propName = 'maxLength') then MaxLengthElement := nil
  else if (propName = 'condition') then deletePropertyValue('condition', ConditionList, value) {2}
  else if (propName = 'constraint') then deletePropertyValue('constraint', ConstraintList, value) {2}
  else if (propName = 'mustSupport') then MustSupportElement := nil
  else if (propName = 'isModifier') then IsModifierElement := nil
  else if (propName = 'isSummary') then IsSummaryElement := nil
  else if (propName = 'binding') then BindingElement := nil
  else if (propName = 'mapping') then deletePropertyValue('mapping', MappingList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirElementDefinition.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'path') then PathElement := asString(new){5b}
  else if (propName = 'name') then NameElement := asString(new){5b}
  else if (propName = 'label') then Label_Element := asString(new){5b}
  else if (propName = 'code') then replacePropertyValue('code', CodeList, existing, new) {2}
  else if (propName = 'slicing') then SlicingElement := new as TFhirElementDefinitionSlicing{4}
  else if (propName = 'short') then ShortElement := asString(new){5b}
  else if (propName = 'definition') then DefinitionElement := asMarkdown(new){5b}
  else if (propName = 'comments') then CommentsElement := asMarkdown(new){5b}
  else if (propName = 'requirements') then RequirementsElement := asMarkdown(new){5b}
  else if (propName = 'alias') then replacePropertyValue('alias', AliasList, existing, new) {2}
  else if (propName = 'min') then MinElement := asInteger(new){5b}
  else if (propName = 'max') then MaxElement := asString(new){5b}
  else if (propName = 'base') then BaseElement := new as TFhirElementDefinitionBase{4}
  else if (propName = 'type') then replacePropertyValue('type', Type_List, existing, new) {2}
  else if (propName = 'nameReference') then NameReferenceElement := asString(new){5b}
  else if (isMatchingName(propName, 'defaultValue', ['*'])) then DefaultValueElement := new as TFhirType{4x}
  else if (propName = 'meaningWhenMissing') then MeaningWhenMissingElement := asMarkdown(new){5b}
  else if (isMatchingName(propName, 'fixed', ['*'])) then FixedElement := new as TFhirType{4x}
  else if (isMatchingName(propName, 'pattern', ['*'])) then PatternElement := new as TFhirType{4x}
  else if (isMatchingName(propName, 'example', ['*'])) then ExampleElement := new as TFhirType{4x}
  else if (isMatchingName(propName, 'minValue', ['*'])) then MinValueElement := new as TFhirType{4x}
  else if (isMatchingName(propName, 'maxValue', ['*'])) then MaxValueElement := new as TFhirType{4x}
  else if (propName = 'maxLength') then MaxLengthElement := asInteger(new){5b}
  else if (propName = 'condition') then replacePropertyValue('condition', ConditionList, existing, new) {2}
  else if (propName = 'constraint') then replacePropertyValue('constraint', ConstraintList, existing, new) {2}
  else if (propName = 'mustSupport') then MustSupportElement := asBoolean(new){5b}
  else if (propName = 'isModifier') then IsModifierElement := asBoolean(new){5b}
  else if (propName = 'isSummary') then IsSummaryElement := asBoolean(new){5b}
  else if (propName = 'binding') then BindingElement := new as TFhirElementDefinitionBinding{4}
  else if (propName = 'mapping') then replacePropertyValue('mapping', MappingList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirElementDefinition.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'representation') then FRepresentation.move(source, destination) {1}
  else if (propName = 'code') then CodeList.move(source, destination){2a}
  else if (propName = 'alias') then AliasList.move(source, destination){2}
  else if (propName = 'type') then Type_List.move(source, destination){2a}
  else if (propName = 'condition') then ConditionList.move(source, destination){2}
  else if (propName = 'constraint') then ConstraintList.move(source, destination){2a}
  else if (propName = 'mapping') then MappingList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirElementDefinition.fhirType : string;
begin
  result := 'ElementDefinition';
end;

function TFhirElementDefinition.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FPath) and isEmptyProp(FRepresentation) and isEmptyProp(FName) and isEmptyProp(FLabel_) and isEmptyProp(FcodeList) and isEmptyProp(FSlicing) and isEmptyProp(FShort) and isEmptyProp(FDefinition) and isEmptyProp(FComments) and isEmptyProp(FRequirements) and isEmptyProp(FaliasList) and isEmptyProp(FMin) and isEmptyProp(FMax) and isEmptyProp(FBase) and isEmptyProp(Ftype_List) and isEmptyProp(FNameReference) and isEmptyProp(FDefaultValue) and isEmptyProp(FMeaningWhenMissing) and isEmptyProp(FFixed) and isEmptyProp(FPattern) and isEmptyProp(FExample) and isEmptyProp(FMinValue) and isEmptyProp(FMaxValue) and isEmptyProp(FMaxLength) and isEmptyProp(FconditionList) and isEmptyProp(FconstraintList) and isEmptyProp(FMustSupport) and isEmptyProp(FIsModifier) and isEmptyProp(FIsSummary) and isEmptyProp(FBinding) and isEmptyProp(FmappingList);
end;

function TFhirElementDefinition.equals(other : TObject) : boolean; 
var
  o : TFhirElementDefinition;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirElementDefinition)) then
    result := false
  else
  begin
    o := TFhirElementDefinition(other);
    result := compareDeep(pathElement, o.pathElement, true) and compareDeep(representationList, o.representationList, true) and 
      compareDeep(nameElement, o.nameElement, true) and compareDeep(label_Element, o.label_Element, true) and 
      compareDeep(codeList, o.codeList, true) and compareDeep(slicingElement, o.slicingElement, true) and 
      compareDeep(shortElement, o.shortElement, true) and compareDeep(definitionElement, o.definitionElement, true) and 
      compareDeep(commentsElement, o.commentsElement, true) and compareDeep(requirementsElement, o.requirementsElement, true) and 
      compareDeep(aliasList, o.aliasList, true) and compareDeep(minElement, o.minElement, true) and 
      compareDeep(maxElement, o.maxElement, true) and compareDeep(baseElement, o.baseElement, true) and 
      compareDeep(type_List, o.type_List, true) and compareDeep(nameReferenceElement, o.nameReferenceElement, true) and 
      compareDeep(defaultValueElement, o.defaultValueElement, true) and compareDeep(meaningWhenMissingElement, o.meaningWhenMissingElement, true) and 
      compareDeep(fixedElement, o.fixedElement, true) and compareDeep(patternElement, o.patternElement, true) and 
      compareDeep(exampleElement, o.exampleElement, true) and compareDeep(minValueElement, o.minValueElement, true) and 
      compareDeep(maxValueElement, o.maxValueElement, true) and compareDeep(maxLengthElement, o.maxLengthElement, true) and 
      compareDeep(conditionList, o.conditionList, true) and compareDeep(constraintList, o.constraintList, true) and 
      compareDeep(mustSupportElement, o.mustSupportElement, true) and compareDeep(isModifierElement, o.isModifierElement, true) and 
      compareDeep(isSummaryElement, o.isSummaryElement, true) and compareDeep(bindingElement, o.bindingElement, true) and 
      compareDeep(mappingList, o.mappingList, true);
  end;
end;

function TFhirElementDefinition.Link : TFhirElementDefinition;
begin
  result := TFhirElementDefinition(inherited Link);
end;

function TFhirElementDefinition.Clone : TFhirElementDefinition;
begin
  result := TFhirElementDefinition(inherited Clone);
end;

procedure TFhirElementDefinition.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('path');
  fields.add('representation');
  fields.add('name');
  fields.add('label');
  fields.add('code');
  fields.add('slicing');
  fields.add('short');
  fields.add('definition');
  fields.add('comments');
  fields.add('requirements');
  fields.add('alias');
  fields.add('min');
  fields.add('max');
  fields.add('base');
  fields.add('type');
  fields.add('nameReference');
  fields.add('defaultValue[x]');
  fields.add('meaningWhenMissing');
  fields.add('fixed[x]');
  fields.add('pattern[x]');
  fields.add('example[x]');
  fields.add('minValue[x]');
  fields.add('maxValue[x]');
  fields.add('maxLength');
  fields.add('condition');
  fields.add('constraint');
  fields.add('mustSupport');
  fields.add('isModifier');
  fields.add('isSummary');
  fields.add('binding');
  fields.add('mapping');
end;

{ TFhirElementDefinition }

Procedure TFhirElementDefinition.SetPath(value : TFhirString);
begin
  FPath.free;
  FPath := value;
end;

Function TFhirElementDefinition.GetPathST : String;
begin
  if FPath = nil then
    result := ''
  else
    result := FPath.value;
end;

Procedure TFhirElementDefinition.SetPathST(value : String);
begin
  if value <> '' then
  begin
    if FPath = nil then
      FPath := TFhirString.create;
    FPath.value := value
  end
  else if FPath <> nil then
    FPath.value := '';
end;

Function TFhirElementDefinition.GetRepresentation : TFhirEnumList;
begin
  if FRepresentation = nil then
    FRepresentation := TFHIREnumList.Create(SYSTEMS_TFhirPropertyRepresentationEnum, CODES_TFhirPropertyRepresentationEnum);
  result := FRepresentation;
end;

Function TFhirElementDefinition.GetHasRepresentation : boolean;
begin
  result := (FRepresentation <> nil) and (FRepresentation.count > 0);
end;

Function TFhirElementDefinition.GetRepresentationST : TFhirPropertyRepresentationEnumList;
  var i : integer;
begin
  result := [];
  if Frepresentation <> nil then
    for i := 0 to Frepresentation.count - 1 do
      result := result + [TFhirPropertyRepresentationEnum(StringArrayIndexOfSensitive(CODES_TFhirPropertyRepresentationEnum, Frepresentation[i].value))];
end;

Procedure TFhirElementDefinition.SetRepresentationST(value : TFhirPropertyRepresentationEnumList);
var a : TFhirPropertyRepresentationEnum;
begin
  if Frepresentation = nil then
    Frepresentation := TFhirEnumList.create(SYSTEMS_TFhirPropertyRepresentationEnum, CODES_TFhirPropertyRepresentationEnum);
  Frepresentation.clear;
  for a := low(TFhirPropertyRepresentationEnum) to high(TFhirPropertyRepresentationEnum) do
    if a in value then
      begin
         if Frepresentation = nil then
           Frepresentation := TFhirEnumList.create(SYSTEMS_TFhirPropertyRepresentationEnum, CODES_TFhirPropertyRepresentationEnum);
         Frepresentation.add(TFhirEnum.create(SYSTEMS_TFhirPropertyRepresentationEnum[a], CODES_TFhirPropertyRepresentationEnum[a]));
      end;
end;

Procedure TFhirElementDefinition.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirElementDefinition.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := FName.value;
end;

Procedure TFhirElementDefinition.SetNameST(value : String);
begin
  if value <> '' then
  begin
    if FName = nil then
      FName := TFhirString.create;
    FName.value := value
  end
  else if FName <> nil then
    FName.value := '';
end;

Procedure TFhirElementDefinition.SetLabel_(value : TFhirString);
begin
  FLabel_.free;
  FLabel_ := value;
end;

Function TFhirElementDefinition.GetLabel_ST : String;
begin
  if FLabel_ = nil then
    result := ''
  else
    result := FLabel_.value;
end;

Procedure TFhirElementDefinition.SetLabel_ST(value : String);
begin
  if value <> '' then
  begin
    if FLabel_ = nil then
      FLabel_ := TFhirString.create;
    FLabel_.value := value
  end
  else if FLabel_ <> nil then
    FLabel_.value := '';
end;

Function TFhirElementDefinition.GetCodeList : TFhirCodingList;
begin
  if FCodeList = nil then
    FCodeList := TFhirCodingList.Create;
  result := FCodeList;
end;

Function TFhirElementDefinition.GetHasCodeList : boolean;
begin
  result := (FCodeList <> nil) and (FCodeList.count > 0);
end;

Procedure TFhirElementDefinition.SetSlicing(value : TFhirElementDefinitionSlicing);
begin
  FSlicing.free;
  FSlicing := value;
end;

Procedure TFhirElementDefinition.SetShort(value : TFhirString);
begin
  FShort.free;
  FShort := value;
end;

Function TFhirElementDefinition.GetShortST : String;
begin
  if FShort = nil then
    result := ''
  else
    result := FShort.value;
end;

Procedure TFhirElementDefinition.SetShortST(value : String);
begin
  if value <> '' then
  begin
    if FShort = nil then
      FShort := TFhirString.create;
    FShort.value := value
  end
  else if FShort <> nil then
    FShort.value := '';
end;

Procedure TFhirElementDefinition.SetDefinition(value : TFhirMarkdown);
begin
  FDefinition.free;
  FDefinition := value;
end;

Function TFhirElementDefinition.GetDefinitionST : String;
begin
  if FDefinition = nil then
    result := ''
  else
    result := FDefinition.value;
end;

Procedure TFhirElementDefinition.SetDefinitionST(value : String);
begin
  if value <> '' then
  begin
    if FDefinition = nil then
      FDefinition := TFhirMarkdown.create;
    FDefinition.value := value
  end
  else if FDefinition <> nil then
    FDefinition.value := '';
end;

Procedure TFhirElementDefinition.SetComments(value : TFhirMarkdown);
begin
  FComments.free;
  FComments := value;
end;

Function TFhirElementDefinition.GetCommentsST : String;
begin
  if FComments = nil then
    result := ''
  else
    result := FComments.value;
end;

Procedure TFhirElementDefinition.SetCommentsST(value : String);
begin
  if value <> '' then
  begin
    if FComments = nil then
      FComments := TFhirMarkdown.create;
    FComments.value := value
  end
  else if FComments <> nil then
    FComments.value := '';
end;

Procedure TFhirElementDefinition.SetRequirements(value : TFhirMarkdown);
begin
  FRequirements.free;
  FRequirements := value;
end;

Function TFhirElementDefinition.GetRequirementsST : String;
begin
  if FRequirements = nil then
    result := ''
  else
    result := FRequirements.value;
end;

Procedure TFhirElementDefinition.SetRequirementsST(value : String);
begin
  if value <> '' then
  begin
    if FRequirements = nil then
      FRequirements := TFhirMarkdown.create;
    FRequirements.value := value
  end
  else if FRequirements <> nil then
    FRequirements.value := '';
end;

Function TFhirElementDefinition.GetAliasList : TFhirStringList;
begin
  if FAliasList = nil then
    FAliasList := TFhirStringList.Create;
  result := FAliasList;
end;

Function TFhirElementDefinition.GetHasAliasList : boolean;
begin
  result := (FAliasList <> nil) and (FAliasList.count > 0);
end;

Procedure TFhirElementDefinition.SetMin(value : TFhirInteger);
begin
  FMin.free;
  FMin := value;
end;

Function TFhirElementDefinition.GetMinST : String;
begin
  if FMin = nil then
    result := ''
  else
    result := FMin.value;
end;

Procedure TFhirElementDefinition.SetMinST(value : String);
begin
  if value <> '' then
  begin
    if FMin = nil then
      FMin := TFhirInteger.create;
    FMin.value := value
  end
  else if FMin <> nil then
    FMin.value := '';
end;

Procedure TFhirElementDefinition.SetMax(value : TFhirString);
begin
  FMax.free;
  FMax := value;
end;

Function TFhirElementDefinition.GetMaxST : String;
begin
  if FMax = nil then
    result := ''
  else
    result := FMax.value;
end;

Procedure TFhirElementDefinition.SetMaxST(value : String);
begin
  if value <> '' then
  begin
    if FMax = nil then
      FMax := TFhirString.create;
    FMax.value := value
  end
  else if FMax <> nil then
    FMax.value := '';
end;

Procedure TFhirElementDefinition.SetBase(value : TFhirElementDefinitionBase);
begin
  FBase.free;
  FBase := value;
end;

Function TFhirElementDefinition.GetType_List : TFhirElementDefinitionTypeList;
begin
  if FType_List = nil then
    FType_List := TFhirElementDefinitionTypeList.Create;
  result := FType_List;
end;

Function TFhirElementDefinition.GetHasType_List : boolean;
begin
  result := (FType_List <> nil) and (FType_List.count > 0);
end;

Procedure TFhirElementDefinition.SetNameReference(value : TFhirString);
begin
  FNameReference.free;
  FNameReference := value;
end;

Function TFhirElementDefinition.GetNameReferenceST : String;
begin
  if FNameReference = nil then
    result := ''
  else
    result := FNameReference.value;
end;

Procedure TFhirElementDefinition.SetNameReferenceST(value : String);
begin
  if value <> '' then
  begin
    if FNameReference = nil then
      FNameReference := TFhirString.create;
    FNameReference.value := value
  end
  else if FNameReference <> nil then
    FNameReference.value := '';
end;

Procedure TFhirElementDefinition.SetDefaultValue(value : TFhirType);
begin
  FDefaultValue.free;
  FDefaultValue := value;
end;

Procedure TFhirElementDefinition.SetMeaningWhenMissing(value : TFhirMarkdown);
begin
  FMeaningWhenMissing.free;
  FMeaningWhenMissing := value;
end;

Function TFhirElementDefinition.GetMeaningWhenMissingST : String;
begin
  if FMeaningWhenMissing = nil then
    result := ''
  else
    result := FMeaningWhenMissing.value;
end;

Procedure TFhirElementDefinition.SetMeaningWhenMissingST(value : String);
begin
  if value <> '' then
  begin
    if FMeaningWhenMissing = nil then
      FMeaningWhenMissing := TFhirMarkdown.create;
    FMeaningWhenMissing.value := value
  end
  else if FMeaningWhenMissing <> nil then
    FMeaningWhenMissing.value := '';
end;

Procedure TFhirElementDefinition.SetFixed(value : TFhirType);
begin
  FFixed.free;
  FFixed := value;
end;

Procedure TFhirElementDefinition.SetPattern(value : TFhirType);
begin
  FPattern.free;
  FPattern := value;
end;

Procedure TFhirElementDefinition.SetExample(value : TFhirType);
begin
  FExample.free;
  FExample := value;
end;

Procedure TFhirElementDefinition.SetMinValue(value : TFhirType);
begin
  FMinValue.free;
  FMinValue := value;
end;

Procedure TFhirElementDefinition.SetMaxValue(value : TFhirType);
begin
  FMaxValue.free;
  FMaxValue := value;
end;

Procedure TFhirElementDefinition.SetMaxLength(value : TFhirInteger);
begin
  FMaxLength.free;
  FMaxLength := value;
end;

Function TFhirElementDefinition.GetMaxLengthST : String;
begin
  if FMaxLength = nil then
    result := ''
  else
    result := FMaxLength.value;
end;

Procedure TFhirElementDefinition.SetMaxLengthST(value : String);
begin
  if value <> '' then
  begin
    if FMaxLength = nil then
      FMaxLength := TFhirInteger.create;
    FMaxLength.value := value
  end
  else if FMaxLength <> nil then
    FMaxLength.value := '';
end;

Function TFhirElementDefinition.GetConditionList : TFhirIdList;
begin
  if FConditionList = nil then
    FConditionList := TFhirIdList.Create;
  result := FConditionList;
end;

Function TFhirElementDefinition.GetHasConditionList : boolean;
begin
  result := (FConditionList <> nil) and (FConditionList.count > 0);
end;

Function TFhirElementDefinition.GetConstraintList : TFhirElementDefinitionConstraintList;
begin
  if FConstraintList = nil then
    FConstraintList := TFhirElementDefinitionConstraintList.Create;
  result := FConstraintList;
end;

Function TFhirElementDefinition.GetHasConstraintList : boolean;
begin
  result := (FConstraintList <> nil) and (FConstraintList.count > 0);
end;

Procedure TFhirElementDefinition.SetMustSupport(value : TFhirBoolean);
begin
  FMustSupport.free;
  FMustSupport := value;
end;

Function TFhirElementDefinition.GetMustSupportST : Boolean;
begin
  if FMustSupport = nil then
    result := false
  else
    result := FMustSupport.value;
end;

Procedure TFhirElementDefinition.SetMustSupportST(value : Boolean);
begin
  if FMustSupport = nil then
    FMustSupport := TFhirBoolean.create;
  FMustSupport.value := value
end;

Procedure TFhirElementDefinition.SetIsModifier(value : TFhirBoolean);
begin
  FIsModifier.free;
  FIsModifier := value;
end;

Function TFhirElementDefinition.GetIsModifierST : Boolean;
begin
  if FIsModifier = nil then
    result := false
  else
    result := FIsModifier.value;
end;

Procedure TFhirElementDefinition.SetIsModifierST(value : Boolean);
begin
  if FIsModifier = nil then
    FIsModifier := TFhirBoolean.create;
  FIsModifier.value := value
end;

Procedure TFhirElementDefinition.SetIsSummary(value : TFhirBoolean);
begin
  FIsSummary.free;
  FIsSummary := value;
end;

Function TFhirElementDefinition.GetIsSummaryST : Boolean;
begin
  if FIsSummary = nil then
    result := false
  else
    result := FIsSummary.value;
end;

Procedure TFhirElementDefinition.SetIsSummaryST(value : Boolean);
begin
  if FIsSummary = nil then
    FIsSummary := TFhirBoolean.create;
  FIsSummary.value := value
end;

Procedure TFhirElementDefinition.SetBinding(value : TFhirElementDefinitionBinding);
begin
  FBinding.free;
  FBinding := value;
end;

Function TFhirElementDefinition.GetMappingList : TFhirElementDefinitionMappingList;
begin
  if FMappingList = nil then
    FMappingList := TFhirElementDefinitionMappingList.Create;
  result := FMappingList;
end;

Function TFhirElementDefinition.GetHasMappingList : boolean;
begin
  result := (FMappingList <> nil) and (FMappingList.count > 0);
end;

function TFhirElementDefinition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPath.sizeInBytes);
  inc(result, FRepresentation.sizeInBytes);
  inc(result, FName.sizeInBytes);
  inc(result, FLabel_.sizeInBytes);
  inc(result, FcodeList.sizeInBytes);
  inc(result, FSlicing.sizeInBytes);
  inc(result, FShort.sizeInBytes);
  inc(result, FDefinition.sizeInBytes);
  inc(result, FComments.sizeInBytes);
  inc(result, FRequirements.sizeInBytes);
  inc(result, FaliasList.sizeInBytes);
  inc(result, FMin.sizeInBytes);
  inc(result, FMax.sizeInBytes);
  inc(result, FBase.sizeInBytes);
  inc(result, Ftype_List.sizeInBytes);
  inc(result, FNameReference.sizeInBytes);
  inc(result, FDefaultValue.sizeInBytes);
  inc(result, FMeaningWhenMissing.sizeInBytes);
  inc(result, FFixed.sizeInBytes);
  inc(result, FPattern.sizeInBytes);
  inc(result, FExample.sizeInBytes);
  inc(result, FMinValue.sizeInBytes);
  inc(result, FMaxValue.sizeInBytes);
  inc(result, FMaxLength.sizeInBytes);
  inc(result, FconditionList.sizeInBytes);
  inc(result, FconstraintList.sizeInBytes);
  inc(result, FMustSupport.sizeInBytes);
  inc(result, FIsModifier.sizeInBytes);
  inc(result, FIsSummary.sizeInBytes);
  inc(result, FBinding.sizeInBytes);
  inc(result, FmappingList.sizeInBytes);
end;

{ TFhirElementDefinitionListEnumerator }

Constructor TFhirElementDefinitionListEnumerator.Create(list : TFhirElementDefinitionList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirElementDefinitionListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirElementDefinitionListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirElementDefinitionListEnumerator.GetCurrent : TFhirElementDefinition;
begin
  Result := FList[FIndex];
end;

function TFhirElementDefinitionListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirElementDefinitionList }
procedure TFhirElementDefinitionList.AddItem(value: TFhirElementDefinition);
begin
  assert(value.ClassName = 'TFhirElementDefinition', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirElementDefinition');
  add(value);
end;

function TFhirElementDefinitionList.Append: TFhirElementDefinition;
begin
  result := TFhirElementDefinition.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionList.ClearItems;
begin
  Clear;
end;

function TFhirElementDefinitionList.GetEnumerator : TFhirElementDefinitionListEnumerator;
begin
  result := TFhirElementDefinitionListEnumerator.Create(self.link);
end;

function TFhirElementDefinitionList.Clone: TFhirElementDefinitionList;
begin
  result := TFhirElementDefinitionList(inherited Clone);
end;

function TFhirElementDefinitionList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirElementDefinitionList.GetItemN(index: Integer): TFhirElementDefinition;
begin
  result := TFhirElementDefinition(ObjectByIndex[index]);
end;

function TFhirElementDefinitionList.ItemClass: TFslObjectClass;
begin
  result := TFhirElementDefinition;
end;
function TFhirElementDefinitionList.IndexOf(value: TFhirElementDefinition): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirElementDefinitionList.Insert(index: Integer): TFhirElementDefinition;
begin
  result := TFhirElementDefinition.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirElementDefinitionList.InsertItem(index: Integer; value: TFhirElementDefinition);
begin
  assert(value is TFhirElementDefinition);
  Inherited Insert(index, value);
end;

function TFhirElementDefinitionList.Item(index: Integer): TFhirElementDefinition;
begin
  result := TFhirElementDefinition(ObjectByIndex[index]);
end;

function TFhirElementDefinitionList.Link: TFhirElementDefinitionList;
begin
  result := TFhirElementDefinitionList(inherited Link);
end;

procedure TFhirElementDefinitionList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirElementDefinitionList.SetItemByIndex(index: Integer; value: TFhirElementDefinition);
begin
  assert(value is TFhirElementDefinition);
  FhirElementDefinitions[index] := value;
end;

procedure TFhirElementDefinitionList.SetItemN(index: Integer; value: TFhirElementDefinition);
begin
  assert(value is TFhirElementDefinition);
  ObjectByIndex[index] := value;
end;

function TFhirUnitsOfTimeEnumListAsInteger(aSet : TFhirUnitsOfTimeEnumList) : Integer;
var
  a : TFhirUnitsOfTimeEnum;
begin
  result := 0;
  for a := low(TFhirUnitsOfTimeEnum) to high(TFhirUnitsOfTimeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirUnitsOfTimeEnumList(i : Integer) : TFhirUnitsOfTimeEnumList;
var
  aLoop : TFhirUnitsOfTimeEnum;
begin
  result := [];
  for aLoop := low(TFhirUnitsOfTimeEnum) to high(TFhirUnitsOfTimeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEventTimingEnumList(i : Integer) : TFhirEventTimingEnumList;
var
  aLoop : TFhirEventTimingEnum;
begin
  result := [];
  for aLoop := low(TFhirEventTimingEnum) to high(TFhirEventTimingEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

{ TFhirTimingRepeat }

constructor TFhirTimingRepeat.Create;
begin
  inherited;
end;

destructor TFhirTimingRepeat.Destroy;
begin
  FBounds.free;
  FCount.free;
  FDuration.free;
  FDurationMax.free;
  FDurationUnits.free;
  FFrequency.free;
  FFrequencyMax.free;
  FPeriod.free;
  FPeriodMax.free;
  FPeriodUnits.free;
  FWhen.free;
  inherited;
end;

procedure TFhirTimingRepeat.Assign(oSource : TFslObject);
begin
  inherited;
  bounds := TFhirTimingRepeat(oSource).bounds.Clone;
  countElement := TFhirTimingRepeat(oSource).countElement.Clone;
  durationElement := TFhirTimingRepeat(oSource).durationElement.Clone;
  durationMaxElement := TFhirTimingRepeat(oSource).durationMaxElement.Clone;
  FDurationUnits := TFhirTimingRepeat(oSource).FDurationUnits.Link;
  frequencyElement := TFhirTimingRepeat(oSource).frequencyElement.Clone;
  frequencyMaxElement := TFhirTimingRepeat(oSource).frequencyMaxElement.Clone;
  periodElement := TFhirTimingRepeat(oSource).periodElement.Clone;
  periodMaxElement := TFhirTimingRepeat(oSource).periodMaxElement.Clone;
  FPeriodUnits := TFhirTimingRepeat(oSource).FPeriodUnits.Link;
  FWhen := TFhirTimingRepeat(oSource).FWhen.Link;
end;

procedure TFhirTimingRepeat.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'bounds[x]') or (child_name = 'bounds') Then
     list.add(self.link, 'bounds[x]', FBounds.Link);
  if (child_name = 'count') Then
     list.add(self.link, 'count', FCount.Link);
  if (child_name = 'duration') Then
     list.add(self.link, 'duration', FDuration.Link);
  if (child_name = 'durationMax') Then
     list.add(self.link, 'durationMax', FDurationMax.Link);
  if (child_name = 'durationUnits') Then
     list.add(self.link, 'durationUnits', FDurationUnits.Link);
  if (child_name = 'frequency') Then
     list.add(self.link, 'frequency', FFrequency.Link);
  if (child_name = 'frequencyMax') Then
     list.add(self.link, 'frequencyMax', FFrequencyMax.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
  if (child_name = 'periodMax') Then
     list.add(self.link, 'periodMax', FPeriodMax.Link);
  if (child_name = 'periodUnits') Then
     list.add(self.link, 'periodUnits', FPeriodUnits.Link);
  if (child_name = 'when') Then
     list.add(self.link, 'when', FWhen.Link);
end;

procedure TFhirTimingRepeat.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'bounds[x]', 'Quantity|Range|Period', false, TFhirType, FBounds.Link));{2}
  oList.add(TFHIRProperty.create(self, 'count', 'integer', false, TFhirInteger, FCount.Link));{2}
  oList.add(TFHIRProperty.create(self, 'duration', 'decimal', false, TFhirDecimal, FDuration.Link));{2}
  oList.add(TFHIRProperty.create(self, 'durationMax', 'decimal', false, TFhirDecimal, FDurationMax.Link));{2}
  oList.add(TFHIRProperty.create(self, 'durationUnits', 'code', false, TFHIREnum, FDurationUnits.Link));{1}
  oList.add(TFHIRProperty.create(self, 'frequency', 'integer', false, TFhirInteger, FFrequency.Link));{2}
  oList.add(TFHIRProperty.create(self, 'frequencyMax', 'integer', false, TFhirInteger, FFrequencyMax.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'decimal', false, TFhirDecimal, FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'periodMax', 'decimal', false, TFhirDecimal, FPeriodMax.Link));{2}
  oList.add(TFHIRProperty.create(self, 'periodUnits', 'code', false, TFHIREnum, FPeriodUnits.Link));{1}
  oList.add(TFHIRProperty.create(self, 'when', 'code', false, TFHIREnum, FWhen.Link));{1}
end;

function TFhirTimingRepeat.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (isMatchingName(propName, 'bounds', ['Quantity', 'Range', 'Period'])) then
  begin
    Bounds := propValue as TFhirType{4};
    result := propValue;
  end
  else if (propName = 'count') then
  begin
    CountElement := asInteger(propValue){5a};
    result := propValue;
  end
  else if (propName = 'duration') then
  begin
    DurationElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'durationMax') then
  begin
    DurationMaxElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'durationUnits') then
  begin
    DurationUnitsElement := asEnum(SYSTEMS_TFhirUnitsOfTimeEnum, CODES_TFhirUnitsOfTimeEnum, propValue);
    result := propValue
  end
  else if (propName = 'frequency') then
  begin
    FrequencyElement := asInteger(propValue){5a};
    result := propValue;
  end
  else if (propName = 'frequencyMax') then
  begin
    FrequencyMaxElement := asInteger(propValue){5a};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    PeriodElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'periodMax') then
  begin
    PeriodMaxElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'periodUnits') then
  begin
    PeriodUnitsElement := asEnum(SYSTEMS_TFhirUnitsOfTimeEnum, CODES_TFhirUnitsOfTimeEnum, propValue);
    result := propValue
  end
  else if (propName = 'when') then
  begin
    WhenElement := asEnum(SYSTEMS_TFhirEventTimingEnum, CODES_TFhirEventTimingEnum, propValue);
    result := propValue
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirTimingRepeat.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirTimingRepeat.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (isMatchingName(propName, 'bounds', ['Quantity', 'Range', 'Period'])) then raise EFHIRException.create('Cannot make property Bounds'){4x}
  else if (propName = 'count') then result := TFhirInteger.create() {5b}
  else if (propName = 'duration') then result := TFhirDecimal.create() {5b}
  else if (propName = 'durationMax') then result := TFhirDecimal.create() {5b}
  else if (propName = 'frequency') then result := TFhirInteger.create() {5b}
  else if (propName = 'frequencyMax') then result := TFhirInteger.create() {5b}
  else if (propName = 'period') then result := TFhirDecimal.create() {5b}
  else if (propName = 'periodMax') then result := TFhirDecimal.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirTimingRepeat.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'bounds[x]') then result := 'Quantity|Range|Period'
  else if (propName = 'count') then result := 'integer'
  else if (propName = 'duration') then result := 'decimal'
  else if (propName = 'durationMax') then result := 'decimal'
  else if (propName = 'durationUnits') then result := 'code'
  else if (propName = 'frequency') then result := 'integer'
  else if (propName = 'frequencyMax') then result := 'integer'
  else if (propName = 'period') then result := 'decimal'
  else if (propName = 'periodMax') then result := 'decimal'
  else if (propName = 'periodUnits') then result := 'code'
  else if (propName = 'when') then result := 'code'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirTimingRepeat.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (isMatchingName(propName, 'bounds', ['Quantity', 'Range', 'Period'])) then BoundsElement := nil{4x}
  else if (propName = 'count') then CountElement := nil
  else if (propName = 'duration') then DurationElement := nil
  else if (propName = 'durationMax') then DurationMaxElement := nil
  else if (propName = 'durationUnits') then DurationUnitsElement := nil
  else if (propName = 'frequency') then FrequencyElement := nil
  else if (propName = 'frequencyMax') then FrequencyMaxElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else if (propName = 'periodMax') then PeriodMaxElement := nil
  else if (propName = 'periodUnits') then PeriodUnitsElement := nil
  else if (propName = 'when') then WhenElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirTimingRepeat.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (isMatchingName(propName, 'bounds', ['Quantity', 'Range', 'Period'])) then BoundsElement := new as TFhirType{4x}
  else if (propName = 'count') then CountElement := asInteger(new){5b}
  else if (propName = 'duration') then DurationElement := asDecimal(new){5b}
  else if (propName = 'durationMax') then DurationMaxElement := asDecimal(new){5b}
  else if (propName = 'durationUnits') then DurationUnitsElement := asEnum(SYSTEMS_TFhirUnitsOfTimeEnum, CODES_TFhirUnitsOfTimeEnum, new){4}
  else if (propName = 'frequency') then FrequencyElement := asInteger(new){5b}
  else if (propName = 'frequencyMax') then FrequencyMaxElement := asInteger(new){5b}
  else if (propName = 'period') then PeriodElement := asDecimal(new){5b}
  else if (propName = 'periodMax') then PeriodMaxElement := asDecimal(new){5b}
  else if (propName = 'periodUnits') then PeriodUnitsElement := asEnum(SYSTEMS_TFhirUnitsOfTimeEnum, CODES_TFhirUnitsOfTimeEnum, new){4}
  else if (propName = 'when') then WhenElement := asEnum(SYSTEMS_TFhirEventTimingEnum, CODES_TFhirEventTimingEnum, new){4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirTimingRepeat.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirTimingRepeat.fhirType : string;
begin
  result := 'repeat';
end;

function TFhirTimingRepeat.Link : TFhirTimingRepeat;
begin
  result := TFhirTimingRepeat(inherited Link);
end;

function TFhirTimingRepeat.Clone : TFhirTimingRepeat;
begin
  result := TFhirTimingRepeat(inherited Clone);
end;

function TFhirTimingRepeat.equals(other : TObject) : boolean; 
var
  o : TFhirTimingRepeat;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirTimingRepeat)) then
    result := false
  else
  begin
    o := TFhirTimingRepeat(other);
    result := compareDeep(boundsElement, o.boundsElement, true) and compareDeep(countElement, o.countElement, true) and 
      compareDeep(durationElement, o.durationElement, true) and compareDeep(durationMaxElement, o.durationMaxElement, true) and 
      compareDeep(durationUnitsElement, o.durationUnitsElement, true) and compareDeep(frequencyElement, o.frequencyElement, true) and 
      compareDeep(frequencyMaxElement, o.frequencyMaxElement, true) and compareDeep(periodElement, o.periodElement, true) and 
      compareDeep(periodMaxElement, o.periodMaxElement, true) and compareDeep(periodUnitsElement, o.periodUnitsElement, true) and 
      compareDeep(whenElement, o.whenElement, true);
  end;
end;

function TFhirTimingRepeat.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FBounds) and isEmptyProp(FCount) and isEmptyProp(FDuration) and isEmptyProp(FDurationMax) and isEmptyProp(FDurationUnits) and isEmptyProp(FFrequency) and isEmptyProp(FFrequencyMax) and isEmptyProp(FPeriod) and isEmptyProp(FPeriodMax) and isEmptyProp(FPeriodUnits) and isEmptyProp(FWhen);
end;

procedure TFhirTimingRepeat.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('bounds[x]');
  fields.add('count');
  fields.add('duration');
  fields.add('durationMax');
  fields.add('durationUnits');
  fields.add('frequency');
  fields.add('frequencyMax');
  fields.add('period');
  fields.add('periodMax');
  fields.add('periodUnits');
  fields.add('when');
end;

{ TFhirTimingRepeat }

Procedure TFhirTimingRepeat.SetBounds(value : TFhirType);
begin
  FBounds.free;
  FBounds := value;
end;

Procedure TFhirTimingRepeat.SetCount(value : TFhirInteger);
begin
  FCount.free;
  FCount := value;
end;

Function TFhirTimingRepeat.GetCountST : String;
begin
  if FCount = nil then
    result := ''
  else
    result := FCount.value;
end;

Procedure TFhirTimingRepeat.SetCountST(value : String);
begin
  if value <> '' then
  begin
    if FCount = nil then
      FCount := TFhirInteger.create;
    FCount.value := value
  end
  else if FCount <> nil then
    FCount.value := '';
end;

Procedure TFhirTimingRepeat.SetDuration(value : TFhirDecimal);
begin
  FDuration.free;
  FDuration := value;
end;

Function TFhirTimingRepeat.GetDurationST : String;
begin
  if FDuration = nil then
    result := ''
  else
    result := FDuration.value;
end;

Procedure TFhirTimingRepeat.SetDurationST(value : String);
begin
  if value <> '' then
  begin
    if FDuration = nil then
      FDuration := TFhirDecimal.create;
    FDuration.value := value
  end
  else if FDuration <> nil then
    FDuration.value := '';
end;

Procedure TFhirTimingRepeat.SetDurationMax(value : TFhirDecimal);
begin
  FDurationMax.free;
  FDurationMax := value;
end;

Function TFhirTimingRepeat.GetDurationMaxST : String;
begin
  if FDurationMax = nil then
    result := ''
  else
    result := FDurationMax.value;
end;

Procedure TFhirTimingRepeat.SetDurationMaxST(value : String);
begin
  if value <> '' then
  begin
    if FDurationMax = nil then
      FDurationMax := TFhirDecimal.create;
    FDurationMax.value := value
  end
  else if FDurationMax <> nil then
    FDurationMax.value := '';
end;

Procedure TFhirTimingRepeat.SetDurationUnits(value : TFhirEnum);
begin
  FDurationUnits.free;
  FDurationUnits := value;
end;

Function TFhirTimingRepeat.GetDurationUnitsST : TFhirUnitsOfTimeEnum;
begin
  if FDurationUnits = nil then
    result := TFhirUnitsOfTimeEnum(0)
  else
    result := TFhirUnitsOfTimeEnum(StringArrayIndexOfSensitive(CODES_TFhirUnitsOfTimeEnum, FDurationUnits.value));
end;

Procedure TFhirTimingRepeat.SetDurationUnitsST(value : TFhirUnitsOfTimeEnum);
begin
  if ord(value) = 0 then
    DurationUnitsElement := nil
  else
    DurationUnitsElement := TFhirEnum.create(SYSTEMS_TFhirUnitsOfTimeEnum[value], CODES_TFhirUnitsOfTimeEnum[value]);
end;

Procedure TFhirTimingRepeat.SetFrequency(value : TFhirInteger);
begin
  FFrequency.free;
  FFrequency := value;
end;

Function TFhirTimingRepeat.GetFrequencyST : String;
begin
  if FFrequency = nil then
    result := ''
  else
    result := FFrequency.value;
end;

Procedure TFhirTimingRepeat.SetFrequencyST(value : String);
begin
  if value <> '' then
  begin
    if FFrequency = nil then
      FFrequency := TFhirInteger.create;
    FFrequency.value := value
  end
  else if FFrequency <> nil then
    FFrequency.value := '';
end;

Procedure TFhirTimingRepeat.SetFrequencyMax(value : TFhirInteger);
begin
  FFrequencyMax.free;
  FFrequencyMax := value;
end;

Function TFhirTimingRepeat.GetFrequencyMaxST : String;
begin
  if FFrequencyMax = nil then
    result := ''
  else
    result := FFrequencyMax.value;
end;

Procedure TFhirTimingRepeat.SetFrequencyMaxST(value : String);
begin
  if value <> '' then
  begin
    if FFrequencyMax = nil then
      FFrequencyMax := TFhirInteger.create;
    FFrequencyMax.value := value
  end
  else if FFrequencyMax <> nil then
    FFrequencyMax.value := '';
end;

Procedure TFhirTimingRepeat.SetPeriod(value : TFhirDecimal);
begin
  FPeriod.free;
  FPeriod := value;
end;

Function TFhirTimingRepeat.GetPeriodST : String;
begin
  if FPeriod = nil then
    result := ''
  else
    result := FPeriod.value;
end;

Procedure TFhirTimingRepeat.SetPeriodST(value : String);
begin
  if value <> '' then
  begin
    if FPeriod = nil then
      FPeriod := TFhirDecimal.create;
    FPeriod.value := value
  end
  else if FPeriod <> nil then
    FPeriod.value := '';
end;

Procedure TFhirTimingRepeat.SetPeriodMax(value : TFhirDecimal);
begin
  FPeriodMax.free;
  FPeriodMax := value;
end;

Function TFhirTimingRepeat.GetPeriodMaxST : String;
begin
  if FPeriodMax = nil then
    result := ''
  else
    result := FPeriodMax.value;
end;

Procedure TFhirTimingRepeat.SetPeriodMaxST(value : String);
begin
  if value <> '' then
  begin
    if FPeriodMax = nil then
      FPeriodMax := TFhirDecimal.create;
    FPeriodMax.value := value
  end
  else if FPeriodMax <> nil then
    FPeriodMax.value := '';
end;

Procedure TFhirTimingRepeat.SetPeriodUnits(value : TFhirEnum);
begin
  FPeriodUnits.free;
  FPeriodUnits := value;
end;

Function TFhirTimingRepeat.GetPeriodUnitsST : TFhirUnitsOfTimeEnum;
begin
  if FPeriodUnits = nil then
    result := TFhirUnitsOfTimeEnum(0)
  else
    result := TFhirUnitsOfTimeEnum(StringArrayIndexOfSensitive(CODES_TFhirUnitsOfTimeEnum, FPeriodUnits.value));
end;

Procedure TFhirTimingRepeat.SetPeriodUnitsST(value : TFhirUnitsOfTimeEnum);
begin
  if ord(value) = 0 then
    PeriodUnitsElement := nil
  else
    PeriodUnitsElement := TFhirEnum.create(SYSTEMS_TFhirUnitsOfTimeEnum[value], CODES_TFhirUnitsOfTimeEnum[value]);
end;

Procedure TFhirTimingRepeat.SetWhen(value : TFhirEnum);
begin
  FWhen.free;
  FWhen := value;
end;

Function TFhirTimingRepeat.GetWhenST : TFhirEventTimingEnum;
begin
  if FWhen = nil then
    result := TFhirEventTimingEnum(0)
  else
    result := TFhirEventTimingEnum(StringArrayIndexOfSensitive(CODES_TFhirEventTimingEnum, FWhen.value));
end;

Procedure TFhirTimingRepeat.SetWhenST(value : TFhirEventTimingEnum);
begin
  if ord(value) = 0 then
    WhenElement := nil
  else
    WhenElement := TFhirEnum.create(SYSTEMS_TFhirEventTimingEnum[value], CODES_TFhirEventTimingEnum[value]);
end;

function TFhirTimingRepeat.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBounds.sizeInBytes);
  inc(result, FCount.sizeInBytes);
  inc(result, FDuration.sizeInBytes);
  inc(result, FDurationMax.sizeInBytes);
  inc(result, FDurationUnits.sizeInBytes);
  inc(result, FFrequency.sizeInBytes);
  inc(result, FFrequencyMax.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FPeriodMax.sizeInBytes);
  inc(result, FPeriodUnits.sizeInBytes);
  inc(result, FWhen.sizeInBytes);
end;

{ TFhirTimingRepeatListEnumerator }

Constructor TFhirTimingRepeatListEnumerator.Create(list : TFhirTimingRepeatList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirTimingRepeatListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirTimingRepeatListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirTimingRepeatListEnumerator.GetCurrent : TFhirTimingRepeat;
begin
  Result := FList[FIndex];
end;

function TFhirTimingRepeatListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirTimingRepeatList }
procedure TFhirTimingRepeatList.AddItem(value: TFhirTimingRepeat);
begin
  assert(value.ClassName = 'TFhirTimingRepeat', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirTimingRepeat');
  add(value);
end;

function TFhirTimingRepeatList.Append: TFhirTimingRepeat;
begin
  result := TFhirTimingRepeat.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirTimingRepeatList.ClearItems;
begin
  Clear;
end;

function TFhirTimingRepeatList.GetEnumerator : TFhirTimingRepeatListEnumerator;
begin
  result := TFhirTimingRepeatListEnumerator.Create(self.link);
end;

function TFhirTimingRepeatList.Clone: TFhirTimingRepeatList;
begin
  result := TFhirTimingRepeatList(inherited Clone);
end;

function TFhirTimingRepeatList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirTimingRepeatList.GetItemN(index: Integer): TFhirTimingRepeat;
begin
  result := TFhirTimingRepeat(ObjectByIndex[index]);
end;

function TFhirTimingRepeatList.ItemClass: TFslObjectClass;
begin
  result := TFhirTimingRepeat;
end;
function TFhirTimingRepeatList.IndexOf(value: TFhirTimingRepeat): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirTimingRepeatList.Insert(index: Integer): TFhirTimingRepeat;
begin
  result := TFhirTimingRepeat.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirTimingRepeatList.InsertItem(index: Integer; value: TFhirTimingRepeat);
begin
  assert(value is TFhirTimingRepeat);
  Inherited Insert(index, value);
end;

function TFhirTimingRepeatList.Item(index: Integer): TFhirTimingRepeat;
begin
  result := TFhirTimingRepeat(ObjectByIndex[index]);
end;

function TFhirTimingRepeatList.Link: TFhirTimingRepeatList;
begin
  result := TFhirTimingRepeatList(inherited Link);
end;

procedure TFhirTimingRepeatList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirTimingRepeatList.SetItemByIndex(index: Integer; value: TFhirTimingRepeat);
begin
  assert(value is TFhirTimingRepeat);
  FhirTimingRepeats[index] := value;
end;

procedure TFhirTimingRepeatList.SetItemN(index: Integer; value: TFhirTimingRepeat);
begin
  assert(value is TFhirTimingRepeat);
  ObjectByIndex[index] := value;
end;

{ TFhirTiming }

constructor TFhirTiming.Create;
begin
  inherited;
end;

destructor TFhirTiming.Destroy;
begin
  FEventList.Free;
  FRepeat_.free;
  FCode.free;
  inherited;
end;

procedure TFhirTiming.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirTiming(oSource).FEventList = nil) then
  begin
    FEventList.free;
    FEventList := nil;
  end
  else
  begin
    if FEventList = nil then
      FEventList := TFhirDateTimeList.Create;
    FEventList.Assign(TFhirTiming(oSource).FEventList);
  end;
  repeat_ := TFhirTiming(oSource).repeat_.Clone;
  code := TFhirTiming(oSource).code.Clone;
end;

procedure TFhirTiming.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'event') Then
    list.addAll(self, 'event', FEventList);
  if (child_name = 'repeat') Then
     list.add(self.link, 'repeat', FRepeat_.Link);
  if (child_name = 'code') Then
     list.add(self.link, 'code', FCode.Link);
end;

procedure TFhirTiming.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'event', 'dateTime', true, TFhirDateTime, FEventList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'repeat', '', false, TFhirTimingRepeat, FRepeat_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', false, TFhirCodeableConcept, FCode.Link));{2}
end;

function TFhirTiming.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'event') then
  begin
    EventList.add(asDateTime(propValue)){2};     result := propValue;

  end
  else if (propName = 'repeat') then
  begin
    Repeat_ := propValue as TFhirTimingRepeat{4b};
    result := propValue;
  end
  else if (propName = 'code') then
  begin
    Code := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirTiming.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'event') then EventList.insertItem(index, asDateTime(propValue)){2}
  else inherited;
end;

function TFhirTiming.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'event') then result := EventList.new(){2}
  else if (propName = 'repeat') then result := TFhirTimingRepeat.create(){4b}
  else if (propName = 'code') then result := TFhirCodeableConcept.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirTiming.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'event') then result := 'dateTime'
  else if (propName = 'repeat') then result := ''
  else if (propName = 'code') then result := 'CodeableConcept'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirTiming.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'event') then deletePropertyValue('event', EventList, value) {2}
  else if (propName = 'repeat') then Repeat_Element := nil
  else if (propName = 'code') then CodeElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirTiming.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'event') then replacePropertyValue('event', EventList, existing, new) {2}
  else if (propName = 'repeat') then Repeat_Element := new as TFhirTimingRepeat{4}
  else if (propName = 'code') then CodeElement := new as TFhirCodeableConcept{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirTiming.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'event') then EventList.move(source, destination){2}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirTiming.fhirType : string;
begin
  result := 'Timing';
end;

function TFhirTiming.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FeventList) and isEmptyProp(FRepeat_) and isEmptyProp(FCode);
end;

function TFhirTiming.equals(other : TObject) : boolean; 
var
  o : TFhirTiming;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirTiming)) then
    result := false
  else
  begin
    o := TFhirTiming(other);
    result := compareDeep(eventList, o.eventList, true) and compareDeep(repeat_Element, o.repeat_Element, true) and 
      compareDeep(codeElement, o.codeElement, true);
  end;
end;

function TFhirTiming.Link : TFhirTiming;
begin
  result := TFhirTiming(inherited Link);
end;

function TFhirTiming.Clone : TFhirTiming;
begin
  result := TFhirTiming(inherited Clone);
end;

procedure TFhirTiming.listFieldsInOrder(fields : TStringList);
begin
  listElementFieldsInOrder(fields);
  fields.add('event');
  fields.add('repeat');
  fields.add('code');
end;

{ TFhirTiming }

Function TFhirTiming.GetEventList : TFhirDateTimeList;
begin
  if FEventList = nil then
    FEventList := TFhirDateTimeList.Create;
  result := FEventList;
end;

Function TFhirTiming.GetHasEventList : boolean;
begin
  result := (FEventList <> nil) and (FEventList.count > 0);
end;

Procedure TFhirTiming.SetRepeat_(value : TFhirTimingRepeat);
begin
  FRepeat_.free;
  FRepeat_ := value;
end;

Procedure TFhirTiming.SetCode(value : TFhirCodeableConcept);
begin
  FCode.free;
  FCode := value;
end;

function TFhirTiming.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FeventList.sizeInBytes);
  inc(result, FRepeat_.sizeInBytes);
  inc(result, FCode.sizeInBytes);
end;

{ TFhirTimingListEnumerator }

Constructor TFhirTimingListEnumerator.Create(list : TFhirTimingList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirTimingListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirTimingListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirTimingListEnumerator.GetCurrent : TFhirTiming;
begin
  Result := FList[FIndex];
end;

function TFhirTimingListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirTimingList }
procedure TFhirTimingList.AddItem(value: TFhirTiming);
begin
  assert(value.ClassName = 'TFhirTiming', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirTiming');
  add(value);
end;

function TFhirTimingList.Append: TFhirTiming;
begin
  result := TFhirTiming.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirTimingList.ClearItems;
begin
  Clear;
end;

function TFhirTimingList.GetEnumerator : TFhirTimingListEnumerator;
begin
  result := TFhirTimingListEnumerator.Create(self.link);
end;

function TFhirTimingList.Clone: TFhirTimingList;
begin
  result := TFhirTimingList(inherited Clone);
end;

function TFhirTimingList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirTimingList.GetItemN(index: Integer): TFhirTiming;
begin
  result := TFhirTiming(ObjectByIndex[index]);
end;

function TFhirTimingList.ItemClass: TFslObjectClass;
begin
  result := TFhirTiming;
end;
function TFhirTimingList.IndexOf(value: TFhirTiming): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirTimingList.Insert(index: Integer): TFhirTiming;
begin
  result := TFhirTiming.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirTimingList.InsertItem(index: Integer; value: TFhirTiming);
begin
  assert(value is TFhirTiming);
  Inherited Insert(index, value);
end;

function TFhirTimingList.Item(index: Integer): TFhirTiming;
begin
  result := TFhirTiming(ObjectByIndex[index]);
end;

function TFhirTimingList.Link: TFhirTimingList;
begin
  result := TFhirTimingList(inherited Link);
end;

procedure TFhirTimingList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirTimingList.SetItemByIndex(index: Integer; value: TFhirTiming);
begin
  assert(value is TFhirTiming);
  FhirTimings[index] := value;
end;

procedure TFhirTimingList.SetItemN(index: Integer; value: TFhirTiming);
begin
  assert(value is TFhirTiming);
  ObjectByIndex[index] := value;
end;

function TFhirAllergyIntoleranceStatusEnumListAsInteger(aSet : TFhirAllergyIntoleranceStatusEnumList) : Integer;
var
  a : TFhirAllergyIntoleranceStatusEnum;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceStatusEnum) to high(TFhirAllergyIntoleranceStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceStatusEnumList(i : Integer) : TFhirAllergyIntoleranceStatusEnumList;
var
  aLoop : TFhirAllergyIntoleranceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceStatusEnum) to high(TFhirAllergyIntoleranceStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceCriticalityEnumList(i : Integer) : TFhirAllergyIntoleranceCriticalityEnumList;
var
  aLoop : TFhirAllergyIntoleranceCriticalityEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceCriticalityEnum) to high(TFhirAllergyIntoleranceCriticalityEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceTypeEnumList(i : Integer) : TFhirAllergyIntoleranceTypeEnumList;
var
  aLoop : TFhirAllergyIntoleranceTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceTypeEnum) to high(TFhirAllergyIntoleranceTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceCategoryEnumList(i : Integer) : TFhirAllergyIntoleranceCategoryEnumList;
var
  aLoop : TFhirAllergyIntoleranceCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceCategoryEnum) to high(TFhirAllergyIntoleranceCategoryEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirReactionEventCertaintyEnumListAsInteger(aSet : TFhirReactionEventCertaintyEnumList) : Integer;
var
  a : TFhirReactionEventCertaintyEnum;
begin
  result := 0;
  for a := low(TFhirReactionEventCertaintyEnum) to high(TFhirReactionEventCertaintyEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReactionEventCertaintyEnumList(i : Integer) : TFhirReactionEventCertaintyEnumList;
var
  aLoop : TFhirReactionEventCertaintyEnum;
begin
  result := [];
  for aLoop := low(TFhirReactionEventCertaintyEnum) to high(TFhirReactionEventCertaintyEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirReactionEventSeverityEnumListAsInteger(aSet : TFhirReactionEventSeverityEnumList) : Integer;
var
  a : TFhirReactionEventSeverityEnum;
begin
  result := 0;
  for a := low(TFhirReactionEventSeverityEnum) to high(TFhirReactionEventSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReactionEventSeverityEnumList(i : Integer) : TFhirReactionEventSeverityEnumList;
var
  aLoop : TFhirReactionEventSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirReactionEventSeverityEnum) to high(TFhirReactionEventSeverityEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirAppointmentstatusEnumListAsInteger(aSet : TFhirAppointmentstatusEnumList) : Integer;
var
  a : TFhirAppointmentstatusEnum;
begin
  result := 0;
  for a := low(TFhirAppointmentstatusEnum) to high(TFhirAppointmentstatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAppointmentstatusEnumList(i : Integer) : TFhirAppointmentstatusEnumList;
var
  aLoop : TFhirAppointmentstatusEnum;
begin
  result := [];
  for aLoop := low(TFhirAppointmentstatusEnum) to high(TFhirAppointmentstatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirParticipantrequiredEnumListAsInteger(aSet : TFhirParticipantrequiredEnumList) : Integer;
var
  a : TFhirParticipantrequiredEnum;
begin
  result := 0;
  for a := low(TFhirParticipantrequiredEnum) to high(TFhirParticipantrequiredEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirParticipantrequiredEnumList(i : Integer) : TFhirParticipantrequiredEnumList;
var
  aLoop : TFhirParticipantrequiredEnum;
begin
  result := [];
  for aLoop := low(TFhirParticipantrequiredEnum) to high(TFhirParticipantrequiredEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirParticipationstatusEnumListAsInteger(aSet : TFhirParticipationstatusEnumList) : Integer;
var
  a : TFhirParticipationstatusEnum;
begin
  result := 0;
  for a := low(TFhirParticipationstatusEnum) to high(TFhirParticipationstatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirParticipationstatusEnumList(i : Integer) : TFhirParticipationstatusEnumList;
var
  aLoop : TFhirParticipationstatusEnum;
begin
  result := [];
  for aLoop := low(TFhirParticipationstatusEnum) to high(TFhirParticipationstatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirParticipantstatusEnumListAsInteger(aSet : TFhirParticipantstatusEnumList) : Integer;
var
  a : TFhirParticipantstatusEnum;
begin
  result := 0;
  for a := low(TFhirParticipantstatusEnum) to high(TFhirParticipantstatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirParticipantstatusEnumList(i : Integer) : TFhirParticipantstatusEnumList;
var
  aLoop : TFhirParticipantstatusEnum;
begin
  result := [];
  for aLoop := low(TFhirParticipantstatusEnum) to high(TFhirParticipantstatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAuditEventActionEnumList(i : Integer) : TFhirAuditEventActionEnumList;
var
  aLoop : TFhirAuditEventActionEnum;
begin
  result := [];
  for aLoop := low(TFhirAuditEventActionEnum) to high(TFhirAuditEventActionEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirAuditEventOutcomeEnumListAsInteger(aSet : TFhirAuditEventOutcomeEnumList) : Integer;
var
  a : TFhirAuditEventOutcomeEnum;
begin
  result := 0;
  for a := low(TFhirAuditEventOutcomeEnum) to high(TFhirAuditEventOutcomeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAuditEventOutcomeEnumList(i : Integer) : TFhirAuditEventOutcomeEnumList;
var
  aLoop : TFhirAuditEventOutcomeEnum;
begin
  result := [];
  for aLoop := low(TFhirAuditEventOutcomeEnum) to high(TFhirAuditEventOutcomeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirNetworkTypeEnumListAsInteger(aSet : TFhirNetworkTypeEnumList) : Integer;
var
  a : TFhirNetworkTypeEnum;
begin
  result := 0;
  for a := low(TFhirNetworkTypeEnum) to high(TFhirNetworkTypeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNetworkTypeEnumList(i : Integer) : TFhirNetworkTypeEnumList;
var
  aLoop : TFhirNetworkTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirNetworkTypeEnum) to high(TFhirNetworkTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBundleTypeEnumList(i : Integer) : TFhirBundleTypeEnumList;
var
  aLoop : TFhirBundleTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirBundleTypeEnum) to high(TFhirBundleTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchEntryModeEnumList(i : Integer) : TFhirSearchEntryModeEnumList;
var
  aLoop : TFhirSearchEntryModeEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchEntryModeEnum) to high(TFhirSearchEntryModeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirHttpVerbEnumListAsInteger(aSet : TFhirHttpVerbEnumList) : Integer;
var
  a : TFhirHttpVerbEnum;
begin
  result := 0;
  for a := low(TFhirHttpVerbEnum) to high(TFhirHttpVerbEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirHttpVerbEnumList(i : Integer) : TFhirHttpVerbEnumList;
var
  aLoop : TFhirHttpVerbEnum;
begin
  result := [];
  for aLoop := low(TFhirHttpVerbEnum) to high(TFhirHttpVerbEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirCarePlanStatusEnumListAsInteger(aSet : TFhirCarePlanStatusEnumList) : Integer;
var
  a : TFhirCarePlanStatusEnum;
begin
  result := 0;
  for a := low(TFhirCarePlanStatusEnum) to high(TFhirCarePlanStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanStatusEnumList(i : Integer) : TFhirCarePlanStatusEnumList;
var
  aLoop : TFhirCarePlanStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCarePlanStatusEnum) to high(TFhirCarePlanStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirCarePlanRelationshipEnumListAsInteger(aSet : TFhirCarePlanRelationshipEnumList) : Integer;
var
  a : TFhirCarePlanRelationshipEnum;
begin
  result := 0;
  for a := low(TFhirCarePlanRelationshipEnum) to high(TFhirCarePlanRelationshipEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanRelationshipEnumList(i : Integer) : TFhirCarePlanRelationshipEnumList;
var
  aLoop : TFhirCarePlanRelationshipEnum;
begin
  result := [];
  for aLoop := low(TFhirCarePlanRelationshipEnum) to high(TFhirCarePlanRelationshipEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanActivityStatusEnumList(i : Integer) : TFhirCarePlanActivityStatusEnumList;
var
  aLoop : TFhirCarePlanActivityStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCarePlanActivityStatusEnum) to high(TFhirCarePlanActivityStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirClaimTypeLinkEnumListAsInteger(aSet : TFhirClaimTypeLinkEnumList) : Integer;
var
  a : TFhirClaimTypeLinkEnum;
begin
  result := 0;
  for a := low(TFhirClaimTypeLinkEnum) to high(TFhirClaimTypeLinkEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirClaimTypeLinkEnumList(i : Integer) : TFhirClaimTypeLinkEnumList;
var
  aLoop : TFhirClaimTypeLinkEnum;
begin
  result := [];
  for aLoop := low(TFhirClaimTypeLinkEnum) to high(TFhirClaimTypeLinkEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirClaimUseLinkEnumListAsInteger(aSet : TFhirClaimUseLinkEnumList) : Integer;
var
  a : TFhirClaimUseLinkEnum;
begin
  result := 0;
  for a := low(TFhirClaimUseLinkEnum) to high(TFhirClaimUseLinkEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirClaimUseLinkEnumList(i : Integer) : TFhirClaimUseLinkEnumList;
var
  aLoop : TFhirClaimUseLinkEnum;
begin
  result := [];
  for aLoop := low(TFhirClaimUseLinkEnum) to high(TFhirClaimUseLinkEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirRemittanceOutcomeEnumListAsInteger(aSet : TFhirRemittanceOutcomeEnumList) : Integer;
var
  a : TFhirRemittanceOutcomeEnum;
begin
  result := 0;
  for a := low(TFhirRemittanceOutcomeEnum) to high(TFhirRemittanceOutcomeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRemittanceOutcomeEnumList(i : Integer) : TFhirRemittanceOutcomeEnumList;
var
  aLoop : TFhirRemittanceOutcomeEnum;
begin
  result := [];
  for aLoop := low(TFhirRemittanceOutcomeEnum) to high(TFhirRemittanceOutcomeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirClinicalImpressionStatusEnumListAsInteger(aSet : TFhirClinicalImpressionStatusEnumList) : Integer;
var
  a : TFhirClinicalImpressionStatusEnum;
begin
  result := 0;
  for a := low(TFhirClinicalImpressionStatusEnum) to high(TFhirClinicalImpressionStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirClinicalImpressionStatusEnumList(i : Integer) : TFhirClinicalImpressionStatusEnumList;
var
  aLoop : TFhirClinicalImpressionStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirClinicalImpressionStatusEnum) to high(TFhirClinicalImpressionStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirCommunicationStatusEnumListAsInteger(aSet : TFhirCommunicationStatusEnumList) : Integer;
var
  a : TFhirCommunicationStatusEnum;
begin
  result := 0;
  for a := low(TFhirCommunicationStatusEnum) to high(TFhirCommunicationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCommunicationStatusEnumList(i : Integer) : TFhirCommunicationStatusEnumList;
var
  aLoop : TFhirCommunicationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCommunicationStatusEnum) to high(TFhirCommunicationStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirCommunicationRequestStatusEnumListAsInteger(aSet : TFhirCommunicationRequestStatusEnumList) : Integer;
var
  a : TFhirCommunicationRequestStatusEnum;
begin
  result := 0;
  for a := low(TFhirCommunicationRequestStatusEnum) to high(TFhirCommunicationRequestStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCommunicationRequestStatusEnumList(i : Integer) : TFhirCommunicationRequestStatusEnumList;
var
  aLoop : TFhirCommunicationRequestStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCommunicationRequestStatusEnum) to high(TFhirCommunicationRequestStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCompositionStatusEnumList(i : Integer) : TFhirCompositionStatusEnumList;
var
  aLoop : TFhirCompositionStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCompositionStatusEnum) to high(TFhirCompositionStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirV3ConfidentialityEnumListAsInteger(aSet : TFhirV3ConfidentialityEnumList) : Integer;
var
  a : TFhirV3ConfidentialityEnum;
begin
  result := 0;
  for a := low(TFhirV3ConfidentialityEnum) to high(TFhirV3ConfidentialityEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirV3ConfidentialityEnumList(i : Integer) : TFhirV3ConfidentialityEnumList;
var
  aLoop : TFhirV3ConfidentialityEnum;
begin
  result := [];
  for aLoop := low(TFhirV3ConfidentialityEnum) to high(TFhirV3ConfidentialityEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCompositionAttestationModeEnumList(i : Integer) : TFhirCompositionAttestationModeEnumList;
var
  aLoop : TFhirCompositionAttestationModeEnum;
begin
  result := [];
  for aLoop := low(TFhirCompositionAttestationModeEnum) to high(TFhirCompositionAttestationModeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirListModeEnumList(i : Integer) : TFhirListModeEnumList;
var
  aLoop : TFhirListModeEnum;
begin
  result := [];
  for aLoop := low(TFhirListModeEnum) to high(TFhirListModeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirConformanceResourceStatusEnumListAsInteger(aSet : TFhirConformanceResourceStatusEnumList) : Integer;
var
  a : TFhirConformanceResourceStatusEnum;
begin
  result := 0;
  for a := low(TFhirConformanceResourceStatusEnum) to high(TFhirConformanceResourceStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConformanceResourceStatusEnumList(i : Integer) : TFhirConformanceResourceStatusEnumList;
var
  aLoop : TFhirConformanceResourceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirConformanceResourceStatusEnum) to high(TFhirConformanceResourceStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirConceptMapEquivalenceEnumListAsInteger(aSet : TFhirConceptMapEquivalenceEnumList) : Integer;
var
  a : TFhirConceptMapEquivalenceEnum;
begin
  result := 0;
  for a := low(TFhirConceptMapEquivalenceEnum) to high(TFhirConceptMapEquivalenceEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConceptMapEquivalenceEnumList(i : Integer) : TFhirConceptMapEquivalenceEnumList;
var
  aLoop : TFhirConceptMapEquivalenceEnum;
begin
  result := [];
  for aLoop := low(TFhirConceptMapEquivalenceEnum) to high(TFhirConceptMapEquivalenceEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirConditionVerStatusEnumListAsInteger(aSet : TFhirConditionVerStatusEnumList) : Integer;
var
  a : TFhirConditionVerStatusEnum;
begin
  result := 0;
  for a := low(TFhirConditionVerStatusEnum) to high(TFhirConditionVerStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionVerStatusEnumList(i : Integer) : TFhirConditionVerStatusEnumList;
var
  aLoop : TFhirConditionVerStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirConditionVerStatusEnum) to high(TFhirConditionVerStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirConformanceStatementKindEnumListAsInteger(aSet : TFhirConformanceStatementKindEnumList) : Integer;
var
  a : TFhirConformanceStatementKindEnum;
begin
  result := 0;
  for a := low(TFhirConformanceStatementKindEnum) to high(TFhirConformanceStatementKindEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConformanceStatementKindEnumList(i : Integer) : TFhirConformanceStatementKindEnumList;
var
  aLoop : TFhirConformanceStatementKindEnum;
begin
  result := [];
  for aLoop := low(TFhirConformanceStatementKindEnum) to high(TFhirConformanceStatementKindEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirUnknownContentCodeEnumListAsInteger(aSet : TFhirUnknownContentCodeEnumList) : Integer;
var
  a : TFhirUnknownContentCodeEnum;
begin
  result := 0;
  for a := low(TFhirUnknownContentCodeEnum) to high(TFhirUnknownContentCodeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirUnknownContentCodeEnumList(i : Integer) : TFhirUnknownContentCodeEnumList;
var
  aLoop : TFhirUnknownContentCodeEnum;
begin
  result := [];
  for aLoop := low(TFhirUnknownContentCodeEnum) to high(TFhirUnknownContentCodeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirRestfulConformanceModeEnumListAsInteger(aSet : TFhirRestfulConformanceModeEnumList) : Integer;
var
  a : TFhirRestfulConformanceModeEnum;
begin
  result := 0;
  for a := low(TFhirRestfulConformanceModeEnum) to high(TFhirRestfulConformanceModeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRestfulConformanceModeEnumList(i : Integer) : TFhirRestfulConformanceModeEnumList;
var
  aLoop : TFhirRestfulConformanceModeEnum;
begin
  result := [];
  for aLoop := low(TFhirRestfulConformanceModeEnum) to high(TFhirRestfulConformanceModeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResourceTypesEnumList(i : Integer) : TFhirResourceTypesEnumList;
var
  aLoop : TFhirResourceTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirResourceTypesEnum) to high(TFhirResourceTypesEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTypeRestfulInteractionEnumList(i : Integer) : TFhirTypeRestfulInteractionEnumList;
var
  aLoop : TFhirTypeRestfulInteractionEnum;
begin
  result := [];
  for aLoop := low(TFhirTypeRestfulInteractionEnum) to high(TFhirTypeRestfulInteractionEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirVersioningPolicyEnumListAsInteger(aSet : TFhirVersioningPolicyEnumList) : Integer;
var
  a : TFhirVersioningPolicyEnum;
begin
  result := 0;
  for a := low(TFhirVersioningPolicyEnum) to high(TFhirVersioningPolicyEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirVersioningPolicyEnumList(i : Integer) : TFhirVersioningPolicyEnumList;
var
  aLoop : TFhirVersioningPolicyEnum;
begin
  result := [];
  for aLoop := low(TFhirVersioningPolicyEnum) to high(TFhirVersioningPolicyEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionalDeleteStatusEnumList(i : Integer) : TFhirConditionalDeleteStatusEnumList;
var
  aLoop : TFhirConditionalDeleteStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirConditionalDeleteStatusEnum) to high(TFhirConditionalDeleteStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchParamTypeEnumList(i : Integer) : TFhirSearchParamTypeEnumList;
var
  aLoop : TFhirSearchParamTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchParamTypeEnum) to high(TFhirSearchParamTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchModifierCodeEnumList(i : Integer) : TFhirSearchModifierCodeEnumList;
var
  aLoop : TFhirSearchModifierCodeEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchModifierCodeEnum) to high(TFhirSearchModifierCodeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSystemRestfulInteractionEnumList(i : Integer) : TFhirSystemRestfulInteractionEnumList;
var
  aLoop : TFhirSystemRestfulInteractionEnum;
begin
  result := [];
  for aLoop := low(TFhirSystemRestfulInteractionEnum) to high(TFhirSystemRestfulInteractionEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirTransactionModeEnumListAsInteger(aSet : TFhirTransactionModeEnumList) : Integer;
var
  a : TFhirTransactionModeEnum;
begin
  result := 0;
  for a := low(TFhirTransactionModeEnum) to high(TFhirTransactionModeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTransactionModeEnumList(i : Integer) : TFhirTransactionModeEnumList;
var
  aLoop : TFhirTransactionModeEnum;
begin
  result := [];
  for aLoop := low(TFhirTransactionModeEnum) to high(TFhirTransactionModeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMessageSignificanceCategoryEnumList(i : Integer) : TFhirMessageSignificanceCategoryEnumList;
var
  aLoop : TFhirMessageSignificanceCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirMessageSignificanceCategoryEnum) to high(TFhirMessageSignificanceCategoryEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMessageConformanceEventModeEnumListAsInteger(aSet : TFhirMessageConformanceEventModeEnumList) : Integer;
var
  a : TFhirMessageConformanceEventModeEnum;
begin
  result := 0;
  for a := low(TFhirMessageConformanceEventModeEnum) to high(TFhirMessageConformanceEventModeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMessageConformanceEventModeEnumList(i : Integer) : TFhirMessageConformanceEventModeEnumList;
var
  aLoop : TFhirMessageConformanceEventModeEnum;
begin
  result := [];
  for aLoop := low(TFhirMessageConformanceEventModeEnum) to high(TFhirMessageConformanceEventModeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentModeEnumList(i : Integer) : TFhirDocumentModeEnumList;
var
  aLoop : TFhirDocumentModeEnum;
begin
  result := [];
  for aLoop := low(TFhirDocumentModeEnum) to high(TFhirDocumentModeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirDataelementStringencyEnumListAsInteger(aSet : TFhirDataelementStringencyEnumList) : Integer;
var
  a : TFhirDataelementStringencyEnum;
begin
  result := 0;
  for a := low(TFhirDataelementStringencyEnum) to high(TFhirDataelementStringencyEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDataelementStringencyEnumList(i : Integer) : TFhirDataelementStringencyEnumList;
var
  aLoop : TFhirDataelementStringencyEnum;
begin
  result := [];
  for aLoop := low(TFhirDataelementStringencyEnum) to high(TFhirDataelementStringencyEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirDetectedissueSeverityEnumListAsInteger(aSet : TFhirDetectedissueSeverityEnumList) : Integer;
var
  a : TFhirDetectedissueSeverityEnum;
begin
  result := 0;
  for a := low(TFhirDetectedissueSeverityEnum) to high(TFhirDetectedissueSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDetectedissueSeverityEnumList(i : Integer) : TFhirDetectedissueSeverityEnumList;
var
  aLoop : TFhirDetectedissueSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirDetectedissueSeverityEnum) to high(TFhirDetectedissueSeverityEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirDevicestatusEnumListAsInteger(aSet : TFhirDevicestatusEnumList) : Integer;
var
  a : TFhirDevicestatusEnum;
begin
  result := 0;
  for a := low(TFhirDevicestatusEnum) to high(TFhirDevicestatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDevicestatusEnumList(i : Integer) : TFhirDevicestatusEnumList;
var
  aLoop : TFhirDevicestatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDevicestatusEnum) to high(TFhirDevicestatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMeasurementPrincipleEnumListAsInteger(aSet : TFhirMeasurementPrincipleEnumList) : Integer;
var
  a : TFhirMeasurementPrincipleEnum;
begin
  result := 0;
  for a := low(TFhirMeasurementPrincipleEnum) to high(TFhirMeasurementPrincipleEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMeasurementPrincipleEnumList(i : Integer) : TFhirMeasurementPrincipleEnumList;
var
  aLoop : TFhirMeasurementPrincipleEnum;
begin
  result := [];
  for aLoop := low(TFhirMeasurementPrincipleEnum) to high(TFhirMeasurementPrincipleEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMetricOperationalStatusEnumListAsInteger(aSet : TFhirMetricOperationalStatusEnumList) : Integer;
var
  a : TFhirMetricOperationalStatusEnum;
begin
  result := 0;
  for a := low(TFhirMetricOperationalStatusEnum) to high(TFhirMetricOperationalStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMetricOperationalStatusEnumList(i : Integer) : TFhirMetricOperationalStatusEnumList;
var
  aLoop : TFhirMetricOperationalStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirMetricOperationalStatusEnum) to high(TFhirMetricOperationalStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMetricColorEnumListAsInteger(aSet : TFhirMetricColorEnumList) : Integer;
var
  a : TFhirMetricColorEnum;
begin
  result := 0;
  for a := low(TFhirMetricColorEnum) to high(TFhirMetricColorEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMetricColorEnumList(i : Integer) : TFhirMetricColorEnumList;
var
  aLoop : TFhirMetricColorEnum;
begin
  result := [];
  for aLoop := low(TFhirMetricColorEnum) to high(TFhirMetricColorEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMetricCategoryEnumListAsInteger(aSet : TFhirMetricCategoryEnumList) : Integer;
var
  a : TFhirMetricCategoryEnum;
begin
  result := 0;
  for a := low(TFhirMetricCategoryEnum) to high(TFhirMetricCategoryEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMetricCategoryEnumList(i : Integer) : TFhirMetricCategoryEnumList;
var
  aLoop : TFhirMetricCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirMetricCategoryEnum) to high(TFhirMetricCategoryEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMetricCalibrationTypeEnumListAsInteger(aSet : TFhirMetricCalibrationTypeEnumList) : Integer;
var
  a : TFhirMetricCalibrationTypeEnum;
begin
  result := 0;
  for a := low(TFhirMetricCalibrationTypeEnum) to high(TFhirMetricCalibrationTypeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMetricCalibrationTypeEnumList(i : Integer) : TFhirMetricCalibrationTypeEnumList;
var
  aLoop : TFhirMetricCalibrationTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirMetricCalibrationTypeEnum) to high(TFhirMetricCalibrationTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMetricCalibrationStateEnumListAsInteger(aSet : TFhirMetricCalibrationStateEnumList) : Integer;
var
  a : TFhirMetricCalibrationStateEnum;
begin
  result := 0;
  for a := low(TFhirMetricCalibrationStateEnum) to high(TFhirMetricCalibrationStateEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMetricCalibrationStateEnumList(i : Integer) : TFhirMetricCalibrationStateEnumList;
var
  aLoop : TFhirMetricCalibrationStateEnum;
begin
  result := [];
  for aLoop := low(TFhirMetricCalibrationStateEnum) to high(TFhirMetricCalibrationStateEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirDeviceUseRequestStatusEnumListAsInteger(aSet : TFhirDeviceUseRequestStatusEnumList) : Integer;
var
  a : TFhirDeviceUseRequestStatusEnum;
begin
  result := 0;
  for a := low(TFhirDeviceUseRequestStatusEnum) to high(TFhirDeviceUseRequestStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceUseRequestStatusEnumList(i : Integer) : TFhirDeviceUseRequestStatusEnumList;
var
  aLoop : TFhirDeviceUseRequestStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceUseRequestStatusEnum) to high(TFhirDeviceUseRequestStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirDeviceUseRequestPriorityEnumListAsInteger(aSet : TFhirDeviceUseRequestPriorityEnumList) : Integer;
var
  a : TFhirDeviceUseRequestPriorityEnum;
begin
  result := 0;
  for a := low(TFhirDeviceUseRequestPriorityEnum) to high(TFhirDeviceUseRequestPriorityEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceUseRequestPriorityEnumList(i : Integer) : TFhirDeviceUseRequestPriorityEnumList;
var
  aLoop : TFhirDeviceUseRequestPriorityEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceUseRequestPriorityEnum) to high(TFhirDeviceUseRequestPriorityEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirDiagnosticOrderStatusEnumListAsInteger(aSet : TFhirDiagnosticOrderStatusEnumList) : Integer;
var
  a : TFhirDiagnosticOrderStatusEnum;
begin
  result := 0;
  for a := low(TFhirDiagnosticOrderStatusEnum) to high(TFhirDiagnosticOrderStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDiagnosticOrderStatusEnumList(i : Integer) : TFhirDiagnosticOrderStatusEnumList;
var
  aLoop : TFhirDiagnosticOrderStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDiagnosticOrderStatusEnum) to high(TFhirDiagnosticOrderStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirDiagnosticOrderPriorityEnumListAsInteger(aSet : TFhirDiagnosticOrderPriorityEnumList) : Integer;
var
  a : TFhirDiagnosticOrderPriorityEnum;
begin
  result := 0;
  for a := low(TFhirDiagnosticOrderPriorityEnum) to high(TFhirDiagnosticOrderPriorityEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDiagnosticOrderPriorityEnumList(i : Integer) : TFhirDiagnosticOrderPriorityEnumList;
var
  aLoop : TFhirDiagnosticOrderPriorityEnum;
begin
  result := [];
  for aLoop := low(TFhirDiagnosticOrderPriorityEnum) to high(TFhirDiagnosticOrderPriorityEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDiagnosticReportStatusEnumList(i : Integer) : TFhirDiagnosticReportStatusEnumList;
var
  aLoop : TFhirDiagnosticReportStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDiagnosticReportStatusEnum) to high(TFhirDiagnosticReportStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentReferenceStatusEnumList(i : Integer) : TFhirDocumentReferenceStatusEnumList;
var
  aLoop : TFhirDocumentReferenceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDocumentReferenceStatusEnum) to high(TFhirDocumentReferenceStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentRelationshipTypeEnumList(i : Integer) : TFhirDocumentRelationshipTypeEnumList;
var
  aLoop : TFhirDocumentRelationshipTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirDocumentRelationshipTypeEnum) to high(TFhirDocumentRelationshipTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirEncounterStateEnumListAsInteger(aSet : TFhirEncounterStateEnumList) : Integer;
var
  a : TFhirEncounterStateEnum;
begin
  result := 0;
  for a := low(TFhirEncounterStateEnum) to high(TFhirEncounterStateEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEncounterStateEnumList(i : Integer) : TFhirEncounterStateEnumList;
var
  aLoop : TFhirEncounterStateEnum;
begin
  result := [];
  for aLoop := low(TFhirEncounterStateEnum) to high(TFhirEncounterStateEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirEncounterClassEnumListAsInteger(aSet : TFhirEncounterClassEnumList) : Integer;
var
  a : TFhirEncounterClassEnum;
begin
  result := 0;
  for a := low(TFhirEncounterClassEnum) to high(TFhirEncounterClassEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEncounterClassEnumList(i : Integer) : TFhirEncounterClassEnumList;
var
  aLoop : TFhirEncounterClassEnum;
begin
  result := [];
  for aLoop := low(TFhirEncounterClassEnum) to high(TFhirEncounterClassEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEncounterLocationStatusEnumList(i : Integer) : TFhirEncounterLocationStatusEnumList;
var
  aLoop : TFhirEncounterLocationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEncounterLocationStatusEnum) to high(TFhirEncounterLocationStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEpisodeOfCareStatusEnumList(i : Integer) : TFhirEpisodeOfCareStatusEnumList;
var
  aLoop : TFhirEpisodeOfCareStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEpisodeOfCareStatusEnum) to high(TFhirEpisodeOfCareStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirHistoryStatusEnumListAsInteger(aSet : TFhirHistoryStatusEnumList) : Integer;
var
  a : TFhirHistoryStatusEnum;
begin
  result := 0;
  for a := low(TFhirHistoryStatusEnum) to high(TFhirHistoryStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirHistoryStatusEnumList(i : Integer) : TFhirHistoryStatusEnumList;
var
  aLoop : TFhirHistoryStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirHistoryStatusEnum) to high(TFhirHistoryStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAdministrativeGenderEnumList(i : Integer) : TFhirAdministrativeGenderEnumList;
var
  aLoop : TFhirAdministrativeGenderEnum;
begin
  result := [];
  for aLoop := low(TFhirAdministrativeGenderEnum) to high(TFhirAdministrativeGenderEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFlagStatusEnumList(i : Integer) : TFhirFlagStatusEnumList;
var
  aLoop : TFhirFlagStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirFlagStatusEnum) to high(TFhirFlagStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirGoalStatusEnumListAsInteger(aSet : TFhirGoalStatusEnumList) : Integer;
var
  a : TFhirGoalStatusEnum;
begin
  result := 0;
  for a := low(TFhirGoalStatusEnum) to high(TFhirGoalStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGoalStatusEnumList(i : Integer) : TFhirGoalStatusEnumList;
var
  aLoop : TFhirGoalStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirGoalStatusEnum) to high(TFhirGoalStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGroupTypeEnumList(i : Integer) : TFhirGroupTypeEnumList;
var
  aLoop : TFhirGroupTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirGroupTypeEnum) to high(TFhirGroupTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDaysOfWeekEnumList(i : Integer) : TFhirDaysOfWeekEnumList;
var
  aLoop : TFhirDaysOfWeekEnum;
begin
  result := [];
  for aLoop := low(TFhirDaysOfWeekEnum) to high(TFhirDaysOfWeekEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirInstanceAvailabilityEnumListAsInteger(aSet : TFhirInstanceAvailabilityEnumList) : Integer;
var
  a : TFhirInstanceAvailabilityEnum;
begin
  result := 0;
  for a := low(TFhirInstanceAvailabilityEnum) to high(TFhirInstanceAvailabilityEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirInstanceAvailabilityEnumList(i : Integer) : TFhirInstanceAvailabilityEnumList;
var
  aLoop : TFhirInstanceAvailabilityEnum;
begin
  result := [];
  for aLoop := low(TFhirInstanceAvailabilityEnum) to high(TFhirInstanceAvailabilityEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMedicationAdminStatusEnumListAsInteger(aSet : TFhirMedicationAdminStatusEnumList) : Integer;
var
  a : TFhirMedicationAdminStatusEnum;
begin
  result := 0;
  for a := low(TFhirMedicationAdminStatusEnum) to high(TFhirMedicationAdminStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationAdminStatusEnumList(i : Integer) : TFhirMedicationAdminStatusEnumList;
var
  aLoop : TFhirMedicationAdminStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationAdminStatusEnum) to high(TFhirMedicationAdminStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirGuideDependencyTypeEnumListAsInteger(aSet : TFhirGuideDependencyTypeEnumList) : Integer;
var
  a : TFhirGuideDependencyTypeEnum;
begin
  result := 0;
  for a := low(TFhirGuideDependencyTypeEnum) to high(TFhirGuideDependencyTypeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGuideDependencyTypeEnumList(i : Integer) : TFhirGuideDependencyTypeEnumList;
var
  aLoop : TFhirGuideDependencyTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirGuideDependencyTypeEnum) to high(TFhirGuideDependencyTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirGuideResourcePurposeEnumListAsInteger(aSet : TFhirGuideResourcePurposeEnumList) : Integer;
var
  a : TFhirGuideResourcePurposeEnum;
begin
  result := 0;
  for a := low(TFhirGuideResourcePurposeEnum) to high(TFhirGuideResourcePurposeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGuideResourcePurposeEnumList(i : Integer) : TFhirGuideResourcePurposeEnumList;
var
  aLoop : TFhirGuideResourcePurposeEnum;
begin
  result := [];
  for aLoop := low(TFhirGuideResourcePurposeEnum) to high(TFhirGuideResourcePurposeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirGuidePageKindEnumListAsInteger(aSet : TFhirGuidePageKindEnumList) : Integer;
var
  a : TFhirGuidePageKindEnum;
begin
  result := 0;
  for a := low(TFhirGuidePageKindEnum) to high(TFhirGuidePageKindEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGuidePageKindEnumList(i : Integer) : TFhirGuidePageKindEnumList;
var
  aLoop : TFhirGuidePageKindEnum;
begin
  result := [];
  for aLoop := low(TFhirGuidePageKindEnum) to high(TFhirGuidePageKindEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirListStatusEnumList(i : Integer) : TFhirListStatusEnumList;
var
  aLoop : TFhirListStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirListStatusEnum) to high(TFhirListStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLocationStatusEnumList(i : Integer) : TFhirLocationStatusEnumList;
var
  aLoop : TFhirLocationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirLocationStatusEnum) to high(TFhirLocationStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLocationModeEnumList(i : Integer) : TFhirLocationModeEnumList;
var
  aLoop : TFhirLocationModeEnum;
begin
  result := [];
  for aLoop := low(TFhirLocationModeEnum) to high(TFhirLocationModeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirDigitalMediaTypeEnumListAsInteger(aSet : TFhirDigitalMediaTypeEnumList) : Integer;
var
  a : TFhirDigitalMediaTypeEnum;
begin
  result := 0;
  for a := low(TFhirDigitalMediaTypeEnum) to high(TFhirDigitalMediaTypeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDigitalMediaTypeEnumList(i : Integer) : TFhirDigitalMediaTypeEnumList;
var
  aLoop : TFhirDigitalMediaTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirDigitalMediaTypeEnum) to high(TFhirDigitalMediaTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMedicationDispenseStatusEnumListAsInteger(aSet : TFhirMedicationDispenseStatusEnumList) : Integer;
var
  a : TFhirMedicationDispenseStatusEnum;
begin
  result := 0;
  for a := low(TFhirMedicationDispenseStatusEnum) to high(TFhirMedicationDispenseStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationDispenseStatusEnumList(i : Integer) : TFhirMedicationDispenseStatusEnumList;
var
  aLoop : TFhirMedicationDispenseStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationDispenseStatusEnum) to high(TFhirMedicationDispenseStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMedicationOrderStatusEnumListAsInteger(aSet : TFhirMedicationOrderStatusEnumList) : Integer;
var
  a : TFhirMedicationOrderStatusEnum;
begin
  result := 0;
  for a := low(TFhirMedicationOrderStatusEnum) to high(TFhirMedicationOrderStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationOrderStatusEnumList(i : Integer) : TFhirMedicationOrderStatusEnumList;
var
  aLoop : TFhirMedicationOrderStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationOrderStatusEnum) to high(TFhirMedicationOrderStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirMedicationStatementStatusEnumListAsInteger(aSet : TFhirMedicationStatementStatusEnumList) : Integer;
var
  a : TFhirMedicationStatementStatusEnum;
begin
  result := 0;
  for a := low(TFhirMedicationStatementStatusEnum) to high(TFhirMedicationStatementStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationStatementStatusEnumList(i : Integer) : TFhirMedicationStatementStatusEnumList;
var
  aLoop : TFhirMedicationStatementStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationStatementStatusEnum) to high(TFhirMedicationStatementStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirResponseCodeEnumListAsInteger(aSet : TFhirResponseCodeEnumList) : Integer;
var
  a : TFhirResponseCodeEnum;
begin
  result := 0;
  for a := low(TFhirResponseCodeEnum) to high(TFhirResponseCodeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResponseCodeEnumList(i : Integer) : TFhirResponseCodeEnumList;
var
  aLoop : TFhirResponseCodeEnum;
begin
  result := [];
  for aLoop := low(TFhirResponseCodeEnum) to high(TFhirResponseCodeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirNamingsystemTypeEnumListAsInteger(aSet : TFhirNamingsystemTypeEnumList) : Integer;
var
  a : TFhirNamingsystemTypeEnum;
begin
  result := 0;
  for a := low(TFhirNamingsystemTypeEnum) to high(TFhirNamingsystemTypeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNamingsystemTypeEnumList(i : Integer) : TFhirNamingsystemTypeEnumList;
var
  aLoop : TFhirNamingsystemTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirNamingsystemTypeEnum) to high(TFhirNamingsystemTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirNamingsystemIdentifierTypeEnumListAsInteger(aSet : TFhirNamingsystemIdentifierTypeEnumList) : Integer;
var
  a : TFhirNamingsystemIdentifierTypeEnum;
begin
  result := 0;
  for a := low(TFhirNamingsystemIdentifierTypeEnum) to high(TFhirNamingsystemIdentifierTypeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNamingsystemIdentifierTypeEnumList(i : Integer) : TFhirNamingsystemIdentifierTypeEnumList;
var
  aLoop : TFhirNamingsystemIdentifierTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirNamingsystemIdentifierTypeEnum) to high(TFhirNamingsystemIdentifierTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirNutritionOrderStatusEnumListAsInteger(aSet : TFhirNutritionOrderStatusEnumList) : Integer;
var
  a : TFhirNutritionOrderStatusEnum;
begin
  result := 0;
  for a := low(TFhirNutritionOrderStatusEnum) to high(TFhirNutritionOrderStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNutritionOrderStatusEnumList(i : Integer) : TFhirNutritionOrderStatusEnumList;
var
  aLoop : TFhirNutritionOrderStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirNutritionOrderStatusEnum) to high(TFhirNutritionOrderStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationStatusEnumList(i : Integer) : TFhirObservationStatusEnumList;
var
  aLoop : TFhirObservationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirObservationStatusEnum) to high(TFhirObservationStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirObservationRelationshiptypesEnumListAsInteger(aSet : TFhirObservationRelationshiptypesEnumList) : Integer;
var
  a : TFhirObservationRelationshiptypesEnum;
begin
  result := 0;
  for a := low(TFhirObservationRelationshiptypesEnum) to high(TFhirObservationRelationshiptypesEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationRelationshiptypesEnumList(i : Integer) : TFhirObservationRelationshiptypesEnumList;
var
  aLoop : TFhirObservationRelationshiptypesEnum;
begin
  result := [];
  for aLoop := low(TFhirObservationRelationshiptypesEnum) to high(TFhirObservationRelationshiptypesEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOperationKindEnumList(i : Integer) : TFhirOperationKindEnumList;
var
  aLoop : TFhirOperationKindEnum;
begin
  result := [];
  for aLoop := low(TFhirOperationKindEnum) to high(TFhirOperationKindEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOperationParameterUseEnumList(i : Integer) : TFhirOperationParameterUseEnumList;
var
  aLoop : TFhirOperationParameterUseEnum;
begin
  result := [];
  for aLoop := low(TFhirOperationParameterUseEnum) to high(TFhirOperationParameterUseEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirOperationParameterTypeEnumListAsInteger(aSet : TFhirOperationParameterTypeEnumList) : Integer;
var
  a : TFhirOperationParameterTypeEnum;
begin
  result := 0;
  for a := low(TFhirOperationParameterTypeEnum) to high(TFhirOperationParameterTypeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOperationParameterTypeEnumList(i : Integer) : TFhirOperationParameterTypeEnumList;
var
  aLoop : TFhirOperationParameterTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirOperationParameterTypeEnum) to high(TFhirOperationParameterTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIssueSeverityEnumList(i : Integer) : TFhirIssueSeverityEnumList;
var
  aLoop : TFhirIssueSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirIssueSeverityEnum) to high(TFhirIssueSeverityEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIssueTypeEnumList(i : Integer) : TFhirIssueTypeEnumList;
var
  aLoop : TFhirIssueTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirIssueTypeEnum) to high(TFhirIssueTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirOrderStatusEnumListAsInteger(aSet : TFhirOrderStatusEnumList) : Integer;
var
  a : TFhirOrderStatusEnum;
begin
  result := 0;
  for a := low(TFhirOrderStatusEnum) to high(TFhirOrderStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOrderStatusEnumList(i : Integer) : TFhirOrderStatusEnumList;
var
  aLoop : TFhirOrderStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirOrderStatusEnum) to high(TFhirOrderStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLinkTypeEnumList(i : Integer) : TFhirLinkTypeEnumList;
var
  aLoop : TFhirLinkTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirLinkTypeEnum) to high(TFhirLinkTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIdentityAssuranceLevelEnumList(i : Integer) : TFhirIdentityAssuranceLevelEnumList;
var
  aLoop : TFhirIdentityAssuranceLevelEnum;
begin
  result := [];
  for aLoop := low(TFhirIdentityAssuranceLevelEnum) to high(TFhirIdentityAssuranceLevelEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirProcedureStatusEnumListAsInteger(aSet : TFhirProcedureStatusEnumList) : Integer;
var
  a : TFhirProcedureStatusEnum;
begin
  result := 0;
  for a := low(TFhirProcedureStatusEnum) to high(TFhirProcedureStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProcedureStatusEnumList(i : Integer) : TFhirProcedureStatusEnumList;
var
  aLoop : TFhirProcedureStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirProcedureStatusEnum) to high(TFhirProcedureStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirProcedureRequestStatusEnumListAsInteger(aSet : TFhirProcedureRequestStatusEnumList) : Integer;
var
  a : TFhirProcedureRequestStatusEnum;
begin
  result := 0;
  for a := low(TFhirProcedureRequestStatusEnum) to high(TFhirProcedureRequestStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProcedureRequestStatusEnumList(i : Integer) : TFhirProcedureRequestStatusEnumList;
var
  aLoop : TFhirProcedureRequestStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirProcedureRequestStatusEnum) to high(TFhirProcedureRequestStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirProcedureRequestPriorityEnumListAsInteger(aSet : TFhirProcedureRequestPriorityEnumList) : Integer;
var
  a : TFhirProcedureRequestPriorityEnum;
begin
  result := 0;
  for a := low(TFhirProcedureRequestPriorityEnum) to high(TFhirProcedureRequestPriorityEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProcedureRequestPriorityEnumList(i : Integer) : TFhirProcedureRequestPriorityEnumList;
var
  aLoop : TFhirProcedureRequestPriorityEnum;
begin
  result := [];
  for aLoop := low(TFhirProcedureRequestPriorityEnum) to high(TFhirProcedureRequestPriorityEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirActionlistEnumListAsInteger(aSet : TFhirActionlistEnumList) : Integer;
var
  a : TFhirActionlistEnum;
begin
  result := 0;
  for a := low(TFhirActionlistEnum) to high(TFhirActionlistEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionlistEnumList(i : Integer) : TFhirActionlistEnumList;
var
  aLoop : TFhirActionlistEnum;
begin
  result := [];
  for aLoop := low(TFhirActionlistEnum) to high(TFhirActionlistEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProvenanceEntityRoleEnumList(i : Integer) : TFhirProvenanceEntityRoleEnumList;
var
  aLoop : TFhirProvenanceEntityRoleEnum;
begin
  result := [];
  for aLoop := low(TFhirProvenanceEntityRoleEnum) to high(TFhirProvenanceEntityRoleEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirQuestionnaireStatusEnumListAsInteger(aSet : TFhirQuestionnaireStatusEnumList) : Integer;
var
  a : TFhirQuestionnaireStatusEnum;
begin
  result := 0;
  for a := low(TFhirQuestionnaireStatusEnum) to high(TFhirQuestionnaireStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireStatusEnumList(i : Integer) : TFhirQuestionnaireStatusEnumList;
var
  aLoop : TFhirQuestionnaireStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireStatusEnum) to high(TFhirQuestionnaireStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirAnswerFormatEnumListAsInteger(aSet : TFhirAnswerFormatEnumList) : Integer;
var
  a : TFhirAnswerFormatEnum;
begin
  result := 0;
  for a := low(TFhirAnswerFormatEnum) to high(TFhirAnswerFormatEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAnswerFormatEnumList(i : Integer) : TFhirAnswerFormatEnumList;
var
  aLoop : TFhirAnswerFormatEnum;
begin
  result := [];
  for aLoop := low(TFhirAnswerFormatEnum) to high(TFhirAnswerFormatEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirQuestionnaireAnswersStatusEnumListAsInteger(aSet : TFhirQuestionnaireAnswersStatusEnumList) : Integer;
var
  a : TFhirQuestionnaireAnswersStatusEnum;
begin
  result := 0;
  for a := low(TFhirQuestionnaireAnswersStatusEnum) to high(TFhirQuestionnaireAnswersStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireAnswersStatusEnumList(i : Integer) : TFhirQuestionnaireAnswersStatusEnumList;
var
  aLoop : TFhirQuestionnaireAnswersStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireAnswersStatusEnum) to high(TFhirQuestionnaireAnswersStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirReferralstatusEnumListAsInteger(aSet : TFhirReferralstatusEnumList) : Integer;
var
  a : TFhirReferralstatusEnum;
begin
  result := 0;
  for a := low(TFhirReferralstatusEnum) to high(TFhirReferralstatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReferralstatusEnumList(i : Integer) : TFhirReferralstatusEnumList;
var
  aLoop : TFhirReferralstatusEnum;
begin
  result := [];
  for aLoop := low(TFhirReferralstatusEnum) to high(TFhirReferralstatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirSearchXpathUsageEnumListAsInteger(aSet : TFhirSearchXpathUsageEnumList) : Integer;
var
  a : TFhirSearchXpathUsageEnum;
begin
  result := 0;
  for a := low(TFhirSearchXpathUsageEnum) to high(TFhirSearchXpathUsageEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchXpathUsageEnumList(i : Integer) : TFhirSearchXpathUsageEnumList;
var
  aLoop : TFhirSearchXpathUsageEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchXpathUsageEnum) to high(TFhirSearchXpathUsageEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirSlotstatusEnumListAsInteger(aSet : TFhirSlotstatusEnumList) : Integer;
var
  a : TFhirSlotstatusEnum;
begin
  result := 0;
  for a := low(TFhirSlotstatusEnum) to high(TFhirSlotstatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSlotstatusEnumList(i : Integer) : TFhirSlotstatusEnumList;
var
  aLoop : TFhirSlotstatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSlotstatusEnum) to high(TFhirSlotstatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSpecimenStatusEnumList(i : Integer) : TFhirSpecimenStatusEnumList;
var
  aLoop : TFhirSpecimenStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSpecimenStatusEnum) to high(TFhirSpecimenStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureDefinitionKindEnumList(i : Integer) : TFhirStructureDefinitionKindEnumList;
var
  aLoop : TFhirStructureDefinitionKindEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureDefinitionKindEnum) to high(TFhirStructureDefinitionKindEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirExtensionContextEnumListAsInteger(aSet : TFhirExtensionContextEnumList) : Integer;
var
  a : TFhirExtensionContextEnum;
begin
  result := 0;
  for a := low(TFhirExtensionContextEnum) to high(TFhirExtensionContextEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirExtensionContextEnumList(i : Integer) : TFhirExtensionContextEnumList;
var
  aLoop : TFhirExtensionContextEnum;
begin
  result := [];
  for aLoop := low(TFhirExtensionContextEnum) to high(TFhirExtensionContextEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirSubscriptionStatusEnumListAsInteger(aSet : TFhirSubscriptionStatusEnumList) : Integer;
var
  a : TFhirSubscriptionStatusEnum;
begin
  result := 0;
  for a := low(TFhirSubscriptionStatusEnum) to high(TFhirSubscriptionStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionStatusEnumList(i : Integer) : TFhirSubscriptionStatusEnumList;
var
  aLoop : TFhirSubscriptionStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionStatusEnum) to high(TFhirSubscriptionStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirSubscriptionChannelTypeEnumListAsInteger(aSet : TFhirSubscriptionChannelTypeEnumList) : Integer;
var
  a : TFhirSubscriptionChannelTypeEnum;
begin
  result := 0;
  for a := low(TFhirSubscriptionChannelTypeEnum) to high(TFhirSubscriptionChannelTypeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionChannelTypeEnumList(i : Integer) : TFhirSubscriptionChannelTypeEnumList;
var
  aLoop : TFhirSubscriptionChannelTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionChannelTypeEnum) to high(TFhirSubscriptionChannelTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirSupplydeliveryStatusEnumListAsInteger(aSet : TFhirSupplydeliveryStatusEnumList) : Integer;
var
  a : TFhirSupplydeliveryStatusEnum;
begin
  result := 0;
  for a := low(TFhirSupplydeliveryStatusEnum) to high(TFhirSupplydeliveryStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSupplydeliveryStatusEnumList(i : Integer) : TFhirSupplydeliveryStatusEnumList;
var
  aLoop : TFhirSupplydeliveryStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSupplydeliveryStatusEnum) to high(TFhirSupplydeliveryStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirSupplyrequestStatusEnumListAsInteger(aSet : TFhirSupplyrequestStatusEnumList) : Integer;
var
  a : TFhirSupplyrequestStatusEnum;
begin
  result := 0;
  for a := low(TFhirSupplyrequestStatusEnum) to high(TFhirSupplyrequestStatusEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSupplyrequestStatusEnumList(i : Integer) : TFhirSupplyrequestStatusEnumList;
var
  aLoop : TFhirSupplyrequestStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSupplyrequestStatusEnum) to high(TFhirSupplyrequestStatusEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirContentTypeEnumListAsInteger(aSet : TFhirContentTypeEnumList) : Integer;
var
  a : TFhirContentTypeEnum;
begin
  result := 0;
  for a := low(TFhirContentTypeEnum) to high(TFhirContentTypeEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContentTypeEnumList(i : Integer) : TFhirContentTypeEnumList;
var
  aLoop : TFhirContentTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirContentTypeEnum) to high(TFhirContentTypeEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirAssertDirectionCodesEnumListAsInteger(aSet : TFhirAssertDirectionCodesEnumList) : Integer;
var
  a : TFhirAssertDirectionCodesEnum;
begin
  result := 0;
  for a := low(TFhirAssertDirectionCodesEnum) to high(TFhirAssertDirectionCodesEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertDirectionCodesEnumList(i : Integer) : TFhirAssertDirectionCodesEnumList;
var
  aLoop : TFhirAssertDirectionCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirAssertDirectionCodesEnum) to high(TFhirAssertDirectionCodesEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirAssertOperatorCodesEnumListAsInteger(aSet : TFhirAssertOperatorCodesEnumList) : Integer;
var
  a : TFhirAssertOperatorCodesEnum;
begin
  result := 0;
  for a := low(TFhirAssertOperatorCodesEnum) to high(TFhirAssertOperatorCodesEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertOperatorCodesEnumList(i : Integer) : TFhirAssertOperatorCodesEnumList;
var
  aLoop : TFhirAssertOperatorCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirAssertOperatorCodesEnum) to high(TFhirAssertOperatorCodesEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirAssertResponseCodeTypesEnumListAsInteger(aSet : TFhirAssertResponseCodeTypesEnumList) : Integer;
var
  a : TFhirAssertResponseCodeTypesEnum;
begin
  result := 0;
  for a := low(TFhirAssertResponseCodeTypesEnum) to high(TFhirAssertResponseCodeTypesEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertResponseCodeTypesEnumList(i : Integer) : TFhirAssertResponseCodeTypesEnumList;
var
  aLoop : TFhirAssertResponseCodeTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirAssertResponseCodeTypesEnum) to high(TFhirAssertResponseCodeTypesEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
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
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFilterOperatorEnumList(i : Integer) : TFhirFilterOperatorEnumList;
var
  aLoop : TFhirFilterOperatorEnum;
begin
  result := [];
  for aLoop := low(TFhirFilterOperatorEnum) to high(TFhirFilterOperatorEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirVisionEyeCodesEnumListAsInteger(aSet : TFhirVisionEyeCodesEnumList) : Integer;
var
  a : TFhirVisionEyeCodesEnum;
begin
  result := 0;
  for a := low(TFhirVisionEyeCodesEnum) to high(TFhirVisionEyeCodesEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirVisionEyeCodesEnumList(i : Integer) : TFhirVisionEyeCodesEnumList;
var
  aLoop : TFhirVisionEyeCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirVisionEyeCodesEnum) to high(TFhirVisionEyeCodesEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function TFhirVisionBaseCodesEnumListAsInteger(aSet : TFhirVisionBaseCodesEnumList) : Integer;
var
  a : TFhirVisionBaseCodesEnum;
begin
  result := 0;
  for a := low(TFhirVisionBaseCodesEnum) to high(TFhirVisionBaseCodesEnum) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirVisionBaseCodesEnumList(i : Integer) : TFhirVisionBaseCodesEnumList;
var
  aLoop : TFhirVisionBaseCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirVisionBaseCodesEnum) to high(TFhirVisionBaseCodesEnum) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;

function asEnum(systems, values: array of String; obj : TFHIRObject) : TFHIREnum;
begin
  if obj is TFHIREnum then
    result := obj as TFHIREnum
  else if obj is TFHIRCode then
  begin
    result := TFHIREnum.create(systems[StringArrayIndexOf(values, TFHIRCode(obj).value)], TFHIRCode(obj).value);
    obj.Free;
  end
  else if obj is TFHIRString then
  begin
    result := TFHIREnum.create(systems[StringArrayIndexOf(values, TFHIRString(obj).value)], TFHIRString(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRCode"')
  end;
end;
function asDateTime(obj : TFHIRObject) : TFHIRDateTime;
begin
  if obj is TFHIRDateTime then
    result := obj as TFHIRDateTime
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDateTime.create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRDateTime.create(TFslDateTime.fromXml(TFHIRObject(obj).primitiveValue));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRDateTime"')
  end;
end;
function asDate(obj : TFHIRObject) : TFHIRDate;
begin
  if obj is TFHIRDate then
    result := obj as TFHIRDate
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDate.create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRDate.create(TFslDateTime.fromXml(TFHIRObject(obj).primitiveValue));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRDate"')
  end;
end;
function asString(obj : TFHIRObject) : TFHIRString;
begin
  if obj is TFHIRString then
    result := obj as TFHIRString
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRString.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRString.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRString"')
  end;
end;
function asInteger(obj : TFHIRObject) : TFHIRInteger;
begin
  if obj is TFHIRInteger then
    result := obj as TFHIRInteger
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRInteger.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRInteger.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRInteger"')
  end;
end;
function asUri(obj : TFHIRObject) : TFHIRUri;
begin
  if obj is TFHIRUri then
    result := obj as TFHIRUri
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUri.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRUri.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRUri"')
  end;
end;
function asInstant(obj : TFHIRObject) : TFHIRInstant;
begin
  if obj is TFHIRInstant then
    result := obj as TFHIRInstant
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRInstant.create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRInstant.create(TFslDateTime.fromXml(TFHIRObject(obj).primitiveValue));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRInstant"')
  end;
end;
function asBoolean(obj : TFHIRObject) : TFHIRBoolean;
begin
  if obj is TFHIRBoolean then
    result := obj as TFHIRBoolean
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRBoolean.create(TFHIRMMElement(obj).value = 'true');
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRBoolean.create(TFHIRObject(obj).primitiveValue = 'true');
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRBoolean"')
  end;
end;
function asBase64Binary(obj : TFHIRObject) : TFHIRBase64Binary;
begin
  if obj is TFHIRBase64Binary then
    result := obj as TFHIRBase64Binary
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRBase64Binary.create(DecodeBase64(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRBase64Binary.create(DecodeBase64(TFHIRObject(obj).primitiveValue));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRBase64Binary"')
  end;
end;
function asTime(obj : TFHIRObject) : TFHIRTime;
begin
  if obj is TFHIRTime then
    result := obj as TFHIRTime
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRTime.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRTime.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRTime"')
  end;
end;
function asDecimal(obj : TFHIRObject) : TFHIRDecimal;
begin
  if obj is TFHIRDecimal then
    result := obj as TFHIRDecimal
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDecimal.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRDecimal.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRDecimal"')
  end;
end;
function asCode(obj : TFHIRObject) : TFHIRCode;
begin
  if obj is TFHIRCode then
    result := obj as TFHIRCode
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRCode.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRCode.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRCode"')
  end;
end;
function asOid(obj : TFHIRObject) : TFHIROid;
begin
  if obj is TFHIROid then
    result := obj as TFHIROid
  else if obj is TFHIRMMElement then
  begin
    result := TFHIROid.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIROid.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIROid"')
  end;
end;
function asUuid(obj : TFHIRObject) : TFHIRUuid;
begin
  if obj is TFHIRUuid then
    result := obj as TFHIRUuid
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUuid.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRUuid.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRUuid"')
  end;
end;
function asMarkdown(obj : TFHIRObject) : TFHIRMarkdown;
begin
  if obj is TFHIRMarkdown then
    result := obj as TFHIRMarkdown
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRMarkdown.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRMarkdown.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRMarkdown"')
  end;
end;
function asUnsignedInt(obj : TFHIRObject) : TFHIRUnsignedInt;
begin
  if obj is TFHIRUnsignedInt then
    result := obj as TFHIRUnsignedInt
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUnsignedInt.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRUnsignedInt.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRUnsignedInt"')
  end;
end;
function asId(obj : TFHIRObject) : TFHIRId;
begin
  if obj is TFHIRId then
    result := obj as TFHIRId
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRId.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRId.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRId"')
  end;
end;
function asPositiveInt(obj : TFHIRObject) : TFHIRPositiveInt;
begin
  if obj is TFHIRPositiveInt then
    result := obj as TFHIRPositiveInt
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRPositiveInt.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRPositiveInt.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRPositiveInt"')
  end;
end;

end.

