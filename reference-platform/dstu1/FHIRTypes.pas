{!Wrapper uses FHIRBase, FHIRBase_Wrapper}

unit FHIRTypes;

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


{$IFNDEF FHIR_DSTU1}
This is the dstu1 branch of the FHIR code
{$ENDIF}
interface

// FHIR v0.0.82 generated Tue, Sep 30, 2014 18:08+1000

uses
  Classes, SysUtils, DecimalSupport, StringSupport, AdvBuffers,  FHIRBase;

Type
  {@Enum TFhirNarrativeStatus
    The status of a resource narrative
  }
  TFhirNarrativeStatus = (
    NarrativeStatusNull,  {@enum.value NarrativeStatusNull Value is missing from Instance }
    NarrativeStatusGenerated, {@enum.value NarrativeStatusGenerated The contents of the narrative are entirely generated from the structured data in the resource. }
    NarrativeStatusExtensions, {@enum.value NarrativeStatusExtensions The contents of the narrative are entirely generated from the structured data in the resource and some of the content is generated from extensions. }
    NarrativeStatusAdditional, {@enum.value NarrativeStatusAdditional The contents of the narrative contain additional information not found in the structured data. }
    NarrativeStatusEmpty); {@enum.value NarrativeStatusEmpty the contents of the narrative are some equivalent of "No human-readable text provided for this resource". }
  TFhirNarrativeStatusList = set of TFhirNarrativeStatus;

  {@Enum TFhirQuantityComparator
    How the Quantity should be understood and represented
  }
  TFhirQuantityComparator = (
    QuantityComparatorNull,  {@enum.value QuantityComparatorNull Value is missing from Instance }
    QuantityComparatorLessThan, {@enum.value QuantityComparatorLessThan The actual value is less than the given value. }
    QuantityComparatorLessOrEquals, {@enum.value QuantityComparatorLessOrEquals The actual value is less than or equal to the given value. }
    QuantityComparatorGreaterOrEquals, {@enum.value QuantityComparatorGreaterOrEquals The actual value is greater than or equal to the given value. }
    QuantityComparatorGreaterThan); {@enum.value QuantityComparatorGreaterThan The actual value is greater than the given value. }
  TFhirQuantityComparatorList = set of TFhirQuantityComparator;

  {@Enum TFhirIdentifierUse
    Identifies the purpose for this identifier, if known
  }
  TFhirIdentifierUse = (
    IdentifierUseNull,  {@enum.value IdentifierUseNull Value is missing from Instance }
    IdentifierUseUsual, {@enum.value IdentifierUseUsual the identifier recommended for display and use in real-world interactions. }
    IdentifierUseOfficial, {@enum.value IdentifierUseOfficial the identifier considered to be most trusted for the identification of this item. }
    IdentifierUseTemp, {@enum.value IdentifierUseTemp A temporary identifier. }
    IdentifierUseSecondary); {@enum.value IdentifierUseSecondary An identifier that was assigned in secondary use - it serves to identify the object in a relative context, but cannot be consistently assigned to the same object again in a different context. }
  TFhirIdentifierUseList = set of TFhirIdentifierUse;

  {@Enum TFhirEventTiming
    Real world event that the schedule relates to
  }
  TFhirEventTiming = (
    EventTimingNull,  {@enum.value EventTimingNull Value is missing from Instance }
    EventTimingHS, {@enum.value EventTimingHS event occurs [duration] before the hour of sleep (or trying to). }
    EventTimingWAKE, {@enum.value EventTimingWAKE event occurs [duration] after waking. }
    EventTimingAC, {@enum.value EventTimingAC event occurs [duration] before a meal (from the Latin ante cibus). }
    EventTimingACM, {@enum.value EventTimingACM event occurs [duration] before breakfast (from the Latin ante cibus matutinus). }
    EventTimingACD, {@enum.value EventTimingACD event occurs [duration] before lunch (from the Latin ante cibus diurnus). }
    EventTimingACV, {@enum.value EventTimingACV event occurs [duration] before dinner (from the Latin ante cibus vespertinus). }
    EventTimingPC, {@enum.value EventTimingPC event occurs [duration] after a meal (from the Latin post cibus). }
    EventTimingPCM, {@enum.value EventTimingPCM event occurs [duration] after breakfast (from the Latin post cibus matutinus). }
    EventTimingPCD, {@enum.value EventTimingPCD event occurs [duration] after lunch (from the Latin post cibus diurnus). }
    EventTimingPCV); {@enum.value EventTimingPCV event occurs [duration] after dinner (from the Latin post cibus vespertinus). }
  TFhirEventTimingList = set of TFhirEventTiming;

  {@Enum TFhirUnitsOfTime
    A unit of time (units from UCUM)
  }
  TFhirUnitsOfTime = (
    UnitsOfTimeNull,  {@enum.value UnitsOfTimeNull Value is missing from Instance }
    UnitsOfTimeS, {@enum.value UnitsOfTimeS second. }
    UnitsOfTimeMin, {@enum.value UnitsOfTimeMin minute. }
    UnitsOfTimeH, {@enum.value UnitsOfTimeH hour. }
    UnitsOfTimeD, {@enum.value UnitsOfTimeD day. }
    UnitsOfTimeWk, {@enum.value UnitsOfTimeWk week. }
    UnitsOfTimeMo, {@enum.value UnitsOfTimeMo month. }
    UnitsOfTimeA); {@enum.value UnitsOfTimeA year. }
  TFhirUnitsOfTimeList = set of TFhirUnitsOfTime;

  {@Enum TFhirContactSystem
    Telecommunications form for contact
  }
  TFhirContactSystem = (
    ContactSystemNull,  {@enum.value ContactSystemNull Value is missing from Instance }
    ContactSystemPhone, {@enum.value ContactSystemPhone The value is a telephone number used for voice calls. Use of full international numbers starting with + is recommended to enable automatic dialing support but not required. }
    ContactSystemFax, {@enum.value ContactSystemFax The value is a fax machine. Use of full international numbers starting with + is recommended to enable automatic dialing support but not required. }
    ContactSystemEmail, {@enum.value ContactSystemEmail The value is an email address. }
    ContactSystemUrl); {@enum.value ContactSystemUrl The value is a url. This is intended for various personal contacts including blogs, Twitter, Facebook, etc. Do not use for email addresses. }
  TFhirContactSystemList = set of TFhirContactSystem;

  {@Enum TFhirContactUse
    Location, type or status of telecommunications address indicating use
  }
  TFhirContactUse = (
    ContactUseNull,  {@enum.value ContactUseNull Value is missing from Instance }
    ContactUseHome, {@enum.value ContactUseHome A communication contact at a home; attempted contacts for business purposes might intrude privacy and chances are one will contact family or other household members instead of the person one wishes to call. Typically used with urgent cases, or if no other contacts are available. }
    ContactUseWork, {@enum.value ContactUseWork An office contact. First choice for business related contacts during business hours. }
    ContactUseTemp, {@enum.value ContactUseTemp A temporary contact. The period can provide more detailed information. }
    ContactUseOld, {@enum.value ContactUseOld This contact is no longer in use (or was never correct, but retained for records). }
    ContactUseMobile); {@enum.value ContactUseMobile A telecommunication device that moves and stays with its owner. May have characteristics of all other use codes, suitable for urgent matters, not the first choice for routine business. }
  TFhirContactUseList = set of TFhirContactUse;

  {@Enum TFhirAddressUse
    The use of an address
  }
  TFhirAddressUse = (
    AddressUseNull,  {@enum.value AddressUseNull Value is missing from Instance }
    AddressUseHome, {@enum.value AddressUseHome A communication address at a home. }
    AddressUseWork, {@enum.value AddressUseWork An office address. First choice for business related contacts during business hours. }
    AddressUseTemp, {@enum.value AddressUseTemp A temporary address. The period can provide more detailed information. }
    AddressUseOld); {@enum.value AddressUseOld This address is no longer in use (or was never correct, but retained for records). }
  TFhirAddressUseList = set of TFhirAddressUse;

  {@Enum TFhirNameUse
    The use of a human name
  }
  TFhirNameUse = (
    NameUseNull,  {@enum.value NameUseNull Value is missing from Instance }
    NameUseUsual, {@enum.value NameUseUsual Known as/conventional/the one you normally use. }
    NameUseOfficial, {@enum.value NameUseOfficial The formal name as registered in an official (government) registry, but which name might not be commonly used. May be called "legal name". }
    NameUseTemp, {@enum.value NameUseTemp A temporary name. Name.period can provide more detailed information. This may also be used for temporary names assigned at birth or in emergency situations. }
    NameUseNickname, {@enum.value NameUseNickname A name that is used to address the person in an informal manner, but is not part of their formal or usual name. }
    NameUseAnonymous, {@enum.value NameUseAnonymous Anonymous assigned name, alias, or pseudonym (used to protect a person's identity for privacy reasons). }
    NameUseOld, {@enum.value NameUseOld This name is no longer in use (or was never correct, but retained for records). }
    NameUseMaiden); {@enum.value NameUseMaiden A name used prior to marriage. Marriage naming customs vary greatly around the world. This name use is for use by applications that collect and store "maiden" names. Though the concept of maiden name is often gender specific, the use of this term is not gender specific. The use of this term does not imply any particular history for a person's name, nor should the maiden name be determined algorithmically. }
  TFhirNameUseList = set of TFhirNameUse;

  {@Enum TFhirReactionSeverity
    The severity of an adverse reaction.
  }
  TFhirReactionSeverity = (
    ReactionSeverityNull,  {@enum.value ReactionSeverityNull Value is missing from Instance }
    ReactionSeveritySevere, {@enum.value ReactionSeveritySevere Severe complications arose due to the reaction. }
    ReactionSeveritySerious, {@enum.value ReactionSeveritySerious Serious inconvenience to the subject. }
    ReactionSeverityModerate, {@enum.value ReactionSeverityModerate Moderate inconvenience to the subject. }
    ReactionSeverityMinor); {@enum.value ReactionSeverityMinor Minor inconvenience to the subject. }
  TFhirReactionSeverityList = set of TFhirReactionSeverity;

  {@Enum TFhirExposureType
    The type of exposure that resulted in an adverse reaction
  }
  TFhirExposureType = (
    ExposureTypeNull,  {@enum.value ExposureTypeNull Value is missing from Instance }
    ExposureTypeDrugadmin, {@enum.value ExposureTypeDrugadmin Drug Administration. }
    ExposureTypeImmuniz, {@enum.value ExposureTypeImmuniz Immunization. }
    ExposureTypeCoincidental); {@enum.value ExposureTypeCoincidental In the same area as the substance. }
  TFhirExposureTypeList = set of TFhirExposureType;

  {@Enum TFhirCausalityExpectation
    How likely is it that the given exposure caused a reaction
  }
  TFhirCausalityExpectation = (
    CausalityExpectationNull,  {@enum.value CausalityExpectationNull Value is missing from Instance }
    CausalityExpectationLikely, {@enum.value CausalityExpectationLikely Likely that this specific exposure caused the reaction. }
    CausalityExpectationUnlikely, {@enum.value CausalityExpectationUnlikely Unlikely that this specific exposure caused the reaction - the exposure is being linked to for information purposes. }
    CausalityExpectationConfirmed, {@enum.value CausalityExpectationConfirmed It has been confirmed that this exposure was one of the causes of the reaction. }
    CausalityExpectationUnknown); {@enum.value CausalityExpectationUnknown It is unknown whether this exposure had anything to do with the reaction. }
  TFhirCausalityExpectationList = set of TFhirCausalityExpectation;

  {@Enum TFhirAlertStatus
    Indicates whether this alert is active and needs to be displayed to a user, or whether it is no longer needed or entered in error
  }
  TFhirAlertStatus = (
    AlertStatusNull,  {@enum.value AlertStatusNull Value is missing from Instance }
    AlertStatusActive, {@enum.value AlertStatusActive A current alert that should be displayed to a user. A system may use the category to determine which roles should view the alert. }
    AlertStatusInactive, {@enum.value AlertStatusInactive The alert does not need to be displayed any more. }
    AlertStatusEnteredInError); {@enum.value AlertStatusEnteredInError The alert was added in error, and should no longer be displayed. }
  TFhirAlertStatusList = set of TFhirAlertStatus;

  {@Enum TFhirCriticality
    The criticality of an adverse sensitivity
  }
  TFhirCriticality = (
    CriticalityNull,  {@enum.value CriticalityNull Value is missing from Instance }
    CriticalityFatal, {@enum.value CriticalityFatal Likely to result in death if re-exposed. }
    CriticalityHigh, {@enum.value CriticalityHigh Likely to result in reactions that will need to be treated if re-exposed. }
    CriticalityMedium, {@enum.value CriticalityMedium Likely to result in reactions that will inconvenience the subject. }
    CriticalityLow); {@enum.value CriticalityLow Not likely to result in any inconveniences for the subject. }
  TFhirCriticalityList = set of TFhirCriticality;

  {@Enum TFhirSensitivitytype
    The type of an adverse sensitivity
  }
  TFhirSensitivitytype = (
    SensitivitytypeNull,  {@enum.value SensitivitytypeNull Value is missing from Instance }
    SensitivitytypeAllergy, {@enum.value SensitivitytypeAllergy Allergic Reaction. }
    SensitivitytypeIntolerance, {@enum.value SensitivitytypeIntolerance Non-Allergic Reaction. }
    SensitivitytypeUnknown); {@enum.value SensitivitytypeUnknown Unknown type. }
  TFhirSensitivitytypeList = set of TFhirSensitivitytype;

  {@Enum TFhirSensitivitystatus
    The status of the adverse sensitivity
  }
  TFhirSensitivitystatus = (
    SensitivitystatusNull,  {@enum.value SensitivitystatusNull Value is missing from Instance }
    SensitivitystatusSuspected, {@enum.value SensitivitystatusSuspected A suspected sensitivity to a substance. }
    SensitivitystatusConfirmed, {@enum.value SensitivitystatusConfirmed The sensitivity has been confirmed and is active. }
    SensitivitystatusRefuted, {@enum.value SensitivitystatusRefuted The sensitivity has been shown to never have existed. }
    SensitivitystatusResolved); {@enum.value SensitivitystatusResolved The sensitivity used to exist but no longer does. }
  TFhirSensitivitystatusList = set of TFhirSensitivitystatus;

  {@Enum TFhirCarePlanStatus
    Indicates whether the plan is currently being acted upon, represents future intentions or is now just historical record.
  }
  TFhirCarePlanStatus = (
    CarePlanStatusNull,  {@enum.value CarePlanStatusNull Value is missing from Instance }
    CarePlanStatusPlanned, {@enum.value CarePlanStatusPlanned The plan is in development or awaiting use but is not yet intended to be acted upon. }
    CarePlanStatusActive, {@enum.value CarePlanStatusActive The plan is intended to be followed and used as part of patient care. }
    CarePlanStatusCompleted); {@enum.value CarePlanStatusCompleted The plan is no longer in use and is not expected to be followed or used in patient care. }
  TFhirCarePlanStatusList = set of TFhirCarePlanStatus;

  {@Enum TFhirCarePlanGoalStatus
    Indicates whether the goal has been met and is still being targeted
  }
  TFhirCarePlanGoalStatus = (
    CarePlanGoalStatusNull,  {@enum.value CarePlanGoalStatusNull Value is missing from Instance }
    CarePlanGoalStatusInProgress, {@enum.value CarePlanGoalStatusInProgress The goal is being sought but has not yet been reached.  (Also applies if goal was reached in the past but there has been regression and goal is being sought again). }
    CarePlanGoalStatusAchieved, {@enum.value CarePlanGoalStatusAchieved The goal has been met and no further action is needed. }
    CarePlanGoalStatusSustaining, {@enum.value CarePlanGoalStatusSustaining The goal has been met, but ongoing activity is needed to sustain the goal objective. }
    CarePlanGoalStatusCancelled); {@enum.value CarePlanGoalStatusCancelled The goal is no longer being sought. }
  TFhirCarePlanGoalStatusList = set of TFhirCarePlanGoalStatus;

  {@Enum TFhirCarePlanActivityStatus
    Indicates where the activity is at in its overall life cycle
  }
  TFhirCarePlanActivityStatus = (
    CarePlanActivityStatusNull,  {@enum.value CarePlanActivityStatusNull Value is missing from Instance }
    CarePlanActivityStatusNotStarted, {@enum.value CarePlanActivityStatusNotStarted Activity is planned but no action has yet been taken. }
    CarePlanActivityStatusScheduled, {@enum.value CarePlanActivityStatusScheduled Appointment or other booking has occurred but activity has not yet begun. }
    CarePlanActivityStatusInProgress, {@enum.value CarePlanActivityStatusInProgress Activity has been started but is not yet complete. }
    CarePlanActivityStatusOnHold, {@enum.value CarePlanActivityStatusOnHold Activity was started but has temporarily ceased with an expectation of resumption at a future time. }
    CarePlanActivityStatusCompleted, {@enum.value CarePlanActivityStatusCompleted The activities have been completed (more or less) as planned. }
    CarePlanActivityStatusCancelled); {@enum.value CarePlanActivityStatusCancelled The activities have been ended prior to completion (perhaps even before they were started). }
  TFhirCarePlanActivityStatusList = set of TFhirCarePlanActivityStatus;

  {@Enum TFhirCarePlanActivityCategory
    High-level categorization of the type of activity in a care plan.
  }
  TFhirCarePlanActivityCategory = (
    CarePlanActivityCategoryNull,  {@enum.value CarePlanActivityCategoryNull Value is missing from Instance }
    CarePlanActivityCategoryDiet, {@enum.value CarePlanActivityCategoryDiet Plan for the patient to consume food of a specified nature. }
    CarePlanActivityCategoryDrug, {@enum.value CarePlanActivityCategoryDrug Plan for the patient to consume/receive a drug, vaccine or other product. }
    CarePlanActivityCategoryEncounter, {@enum.value CarePlanActivityCategoryEncounter Plan to meet or communicate with the patient (in-patient, out-patient, phone call, etc.). }
    CarePlanActivityCategoryObservation, {@enum.value CarePlanActivityCategoryObservation Plan to capture information about a patient (vitals, labs, diagnostic images, etc.). }
    CarePlanActivityCategoryProcedure, {@enum.value CarePlanActivityCategoryProcedure Plan to modify the patient in some way (surgery, physiotherapy, education, counseling, etc.). }
    CarePlanActivityCategorySupply, {@enum.value CarePlanActivityCategorySupply Plan to provide something to the patient (medication, medical supply, etc.). }
    CarePlanActivityCategoryOther); {@enum.value CarePlanActivityCategoryOther Some other form of action. }
  TFhirCarePlanActivityCategoryList = set of TFhirCarePlanActivityCategory;

  {@Enum TFhirCompositionStatus
    The workflow/clinical status of the composition
  }
  TFhirCompositionStatus = (
    CompositionStatusNull,  {@enum.value CompositionStatusNull Value is missing from Instance }
    CompositionStatusPreliminary, {@enum.value CompositionStatusPreliminary This is a preliminary composition or document (also known as initial or interim). The content may be incomplete or unverified. }
    CompositionStatusFinal, {@enum.value CompositionStatusFinal The composition or document is complete and verified by an appropriate person, and no further work is planned. }
    CompositionStatusAppended, {@enum.value CompositionStatusAppended The composition or document has been modified subsequent to being released as "final", and is complete and verified by an authorized person. The modifications added new information to the composition or document, but did not revise existing content. }
    CompositionStatusAmended, {@enum.value CompositionStatusAmended The composition or document has been modified subsequent to being released as "final", and is complete and verified by an authorized person. }
    CompositionStatusEnteredInError); {@enum.value CompositionStatusEnteredInError The composition or document was originally created/issued in error, and this is an amendment that marks that the entire series should not be considered as valid. }
  TFhirCompositionStatusList = set of TFhirCompositionStatus;

  {@Enum TFhirCompositionAttestationMode
    The way in which a person authenticated a composition
  }
  TFhirCompositionAttestationMode = (
    CompositionAttestationModeNull,  {@enum.value CompositionAttestationModeNull Value is missing from Instance }
    CompositionAttestationModePersonal, {@enum.value CompositionAttestationModePersonal The person authenticated the content in their personal capacity. }
    CompositionAttestationModeProfessional, {@enum.value CompositionAttestationModeProfessional The person authenticated the content in their professional capacity. }
    CompositionAttestationModeLegal, {@enum.value CompositionAttestationModeLegal The person authenticated the content and accepted legal responsibility for its content. }
    CompositionAttestationModeOfficial); {@enum.value CompositionAttestationModeOfficial The organization authenticated the content as consistent with their policies and procedures. }
  TFhirCompositionAttestationModeList = set of TFhirCompositionAttestationMode;

  {@Enum TFhirValuesetStatus
    The lifecycle status of a Value Set or Concept Map
  }
  TFhirValuesetStatus = (
    ValuesetStatusNull,  {@enum.value ValuesetStatusNull Value is missing from Instance }
    ValuesetStatusDraft, {@enum.value ValuesetStatusDraft This valueset is still under development. }
    ValuesetStatusActive, {@enum.value ValuesetStatusActive This valueset is ready for normal use. }
    ValuesetStatusRetired); {@enum.value ValuesetStatusRetired This valueset has been withdrawn or superceded and should no longer be used. }
  TFhirValuesetStatusList = set of TFhirValuesetStatus;

  {@Enum TFhirConceptEquivalence
    The degree of equivalence between concepts
  }
  TFhirConceptEquivalence = (
    ConceptEquivalenceNull,  {@enum.value ConceptEquivalenceNull Value is missing from Instance }
    ConceptEquivalenceEqual, {@enum.value ConceptEquivalenceEqual The definitions of the concepts are exactly the same (i.e. only grammatical differences) and structural implications of meaning are identifical or irrelevant (i.e. intensionally identical). }
    ConceptEquivalenceEquivalent, {@enum.value ConceptEquivalenceEquivalent The definitions of the concepts mean the same thing (including when structural implications of meaning are considered) (i.e. extensionally identical). }
    ConceptEquivalenceWider, {@enum.value ConceptEquivalenceWider The target mapping is wider in meaning than the source concept. }
    ConceptEquivalenceSubsumes, {@enum.value ConceptEquivalenceSubsumes The target mapping subsumes the meaning of the source concept (e.g. the source is-a target). }
    ConceptEquivalenceNarrower, {@enum.value ConceptEquivalenceNarrower The target mapping is narrower in meaning that the source concept. The sense in which the mapping is narrower SHALL be described in the comments in this case, and applications should be careful when atempting to use these mappings operationally. }
    ConceptEquivalenceSpecialises, {@enum.value ConceptEquivalenceSpecialises The target mapping specialises the meaning of the source concept (e.g. the target is-a source). }
    ConceptEquivalenceInexact, {@enum.value ConceptEquivalenceInexact The target mapping overlaps with the source concept, but both source and target cover additional meaning. The sense in which the mapping is narrower SHALL be described in the comments in this case, and applications should be careful when atempting to use these mappings operationally. }
    ConceptEquivalenceUnmatched, {@enum.value ConceptEquivalenceUnmatched There is no match for this concept in the destination concept system. }
    ConceptEquivalenceDisjoint); {@enum.value ConceptEquivalenceDisjoint This is an explicit assertion that there is no mapping between the source and target concept. }
  TFhirConceptEquivalenceList = set of TFhirConceptEquivalence;

  {@Enum TFhirConditionStatus
    The clinical status of the Condition or diagnosis
  }
  TFhirConditionStatus = (
    ConditionStatusNull,  {@enum.value ConditionStatusNull Value is missing from Instance }
    ConditionStatusProvisional, {@enum.value ConditionStatusProvisional This is a tentative diagnosis - still a candidate that is under consideration. }
    ConditionStatusWorking, {@enum.value ConditionStatusWorking The patient is being treated on the basis that this is the condition, but it is still not confirmed. }
    ConditionStatusConfirmed, {@enum.value ConditionStatusConfirmed There is sufficient diagnostic and/or clinical evidence to treat this as a confirmed condition. }
    ConditionStatusRefuted); {@enum.value ConditionStatusRefuted This condition has been ruled out by diagnostic and clinical evidence. }
  TFhirConditionStatusList = set of TFhirConditionStatus;

  {@Enum TFhirConditionRelationshipType
    The type of relationship between a condition and its related item
  }
  TFhirConditionRelationshipType = (
    ConditionRelationshipTypeNull,  {@enum.value ConditionRelationshipTypeNull Value is missing from Instance }
    ConditionRelationshipTypeDueTo, {@enum.value ConditionRelationshipTypeDueTo this condition follows the identified condition/procedure/substance and is a consequence of it. }
    ConditionRelationshipTypeFollowing); {@enum.value ConditionRelationshipTypeFollowing this condition follows the identified condition/procedure/substance, but it is not known whether they are causually linked. }
  TFhirConditionRelationshipTypeList = set of TFhirConditionRelationshipType;

  {@Enum TFhirConformanceStatementStatus
    The status of this conformance statement
  }
  TFhirConformanceStatementStatus = (
    ConformanceStatementStatusNull,  {@enum.value ConformanceStatementStatusNull Value is missing from Instance }
    ConformanceStatementStatusDraft, {@enum.value ConformanceStatementStatusDraft This conformance statement is still under development. }
    ConformanceStatementStatusActive, {@enum.value ConformanceStatementStatusActive This conformance statement is ready for use in production systems. }
    ConformanceStatementStatusRetired); {@enum.value ConformanceStatementStatusRetired This conformance statement has been withdrawn or superceded and should no longer be used. }
  TFhirConformanceStatementStatusList = set of TFhirConformanceStatementStatus;

  {@Enum TFhirRestfulConformanceMode
    The mode of a RESTful conformance statement
  }
  TFhirRestfulConformanceMode = (
    RestfulConformanceModeNull,  {@enum.value RestfulConformanceModeNull Value is missing from Instance }
    RestfulConformanceModeClient, {@enum.value RestfulConformanceModeClient The application acts as a server for this resource. }
    RestfulConformanceModeServer); {@enum.value RestfulConformanceModeServer The application acts as a client for this resource. }
  TFhirRestfulConformanceModeList = set of TFhirRestfulConformanceMode;

  {@Enum TFhirTypeRestfulOperation
    Operations supported by REST at the type or instance level
  }
  TFhirTypeRestfulOperation = (
    TypeRestfulOperationNull,  {@enum.value TypeRestfulOperationNull Value is missing from Instance }
    TypeRestfulOperationRead, {@enum.value TypeRestfulOperationRead  }
    TypeRestfulOperationVread, {@enum.value TypeRestfulOperationVread  }
    TypeRestfulOperationUpdate, {@enum.value TypeRestfulOperationUpdate  }
    TypeRestfulOperationDelete, {@enum.value TypeRestfulOperationDelete  }
    TypeRestfulOperationHistoryInstance, {@enum.value TypeRestfulOperationHistoryInstance  }
    TypeRestfulOperationValidate, {@enum.value TypeRestfulOperationValidate  }
    TypeRestfulOperationHistoryType, {@enum.value TypeRestfulOperationHistoryType  }
    TypeRestfulOperationCreate, {@enum.value TypeRestfulOperationCreate  }
    TypeRestfulOperationSearchType); {@enum.value TypeRestfulOperationSearchType  }
  TFhirTypeRestfulOperationList = set of TFhirTypeRestfulOperation;

  {@Enum TFhirSearchParamType
    Data types allowed to be used for search parameters
  }
  TFhirSearchParamType = (
    SearchParamTypeNull,  {@enum.value SearchParamTypeNull Value is missing from Instance }
    SearchParamTypeNumber, {@enum.value SearchParamTypeNumber Search parameter SHALL be a number (a whole number, or a decimal). }
    SearchParamTypeDate, {@enum.value SearchParamTypeDate Search parameter is on a date/time. The date format is the standard XML format, though other formats may be supported. }
    SearchParamTypeString, {@enum.value SearchParamTypeString Search parameter is a simple string, like a name part. Search is case-insensitive and accent-insensitive. May match just the start of a string. String parameters may contain spaces. }
    SearchParamTypeToken, {@enum.value SearchParamTypeToken Search parameter on a coded element or identifier. May be used to search through the text, displayname, code and code/codesystem (for codes) and label, system and key (for identifier). Its value is either a string or a pair of namespace and value, separated by a "|", depending on the modifier used. }
    SearchParamTypeReference, {@enum.value SearchParamTypeReference A reference to another resource. }
    SearchParamTypeComposite, {@enum.value SearchParamTypeComposite A composite search parameter that combines a search on two values together. }
    SearchParamTypeQuantity); {@enum.value SearchParamTypeQuantity A search parameter that searches on a quantity. }
  TFhirSearchParamTypeList = set of TFhirSearchParamType;

  {@Enum TFhirSystemRestfulOperation
    Operations supported by REST at the system level
  }
  TFhirSystemRestfulOperation = (
    SystemRestfulOperationNull,  {@enum.value SystemRestfulOperationNull Value is missing from Instance }
    SystemRestfulOperationTransaction, {@enum.value SystemRestfulOperationTransaction  }
    SystemRestfulOperationSearchSystem, {@enum.value SystemRestfulOperationSearchSystem  }
    SystemRestfulOperationHistorySystem); {@enum.value SystemRestfulOperationHistorySystem  }
  TFhirSystemRestfulOperationList = set of TFhirSystemRestfulOperation;

  {@Enum TFhirMessageSignificanceCategory
    The impact of the content of a message
  }
  TFhirMessageSignificanceCategory = (
    MessageSignificanceCategoryNull,  {@enum.value MessageSignificanceCategoryNull Value is missing from Instance }
    MessageSignificanceCategoryConsequence, {@enum.value MessageSignificanceCategoryConsequence The message represents/requests a change that should not be processed more than once. E.g. Making a booking for an appointment. }
    MessageSignificanceCategoryCurrency, {@enum.value MessageSignificanceCategoryCurrency The message represents a response to query for current information. Retrospective processing is wrong and/or wasteful. }
    MessageSignificanceCategoryNotification); {@enum.value MessageSignificanceCategoryNotification The content is not necessarily intended to be current, and it can be reprocessed, though there may be version issues created by processing old notifications. }
  TFhirMessageSignificanceCategoryList = set of TFhirMessageSignificanceCategory;

  {@Enum TFhirMessageConformanceEventMode
    The mode of a message conformance statement
  }
  TFhirMessageConformanceEventMode = (
    MessageConformanceEventModeNull,  {@enum.value MessageConformanceEventModeNull Value is missing from Instance }
    MessageConformanceEventModeSender, {@enum.value MessageConformanceEventModeSender The application sends requests and receives responses. }
    MessageConformanceEventModeReceiver); {@enum.value MessageConformanceEventModeReceiver The application receives requests and sends responses. }
  TFhirMessageConformanceEventModeList = set of TFhirMessageConformanceEventMode;

  {@Enum TFhirDocumentMode
    Whether the application produces or consumes documents
  }
  TFhirDocumentMode = (
    DocumentModeNull,  {@enum.value DocumentModeNull Value is missing from Instance }
    DocumentModeProducer, {@enum.value DocumentModeProducer The application produces documents of the specified type. }
    DocumentModeConsumer); {@enum.value DocumentModeConsumer The application consumes documents of the specified type. }
  TFhirDocumentModeList = set of TFhirDocumentMode;

  {@Enum TFhirDiagnosticOrderStatus
    The status of a diagnostic order
  }
  TFhirDiagnosticOrderStatus = (
    DiagnosticOrderStatusNull,  {@enum.value DiagnosticOrderStatusNull Value is missing from Instance }
    DiagnosticOrderStatusRequested, {@enum.value DiagnosticOrderStatusRequested The request has been placed. }
    DiagnosticOrderStatusReceived, {@enum.value DiagnosticOrderStatusReceived The receiving system has received the order, but not yet decided whether it will be performed. }
    DiagnosticOrderStatusAccepted, {@enum.value DiagnosticOrderStatusAccepted The receiving system has accepted the order, but work has not yet commenced. }
    DiagnosticOrderStatusInProgress, {@enum.value DiagnosticOrderStatusInProgress The work to fulfill the order is happening. }
    DiagnosticOrderStatusReview, {@enum.value DiagnosticOrderStatusReview The work is complete, and the outcomes are being reviewed for approval. }
    DiagnosticOrderStatusCompleted, {@enum.value DiagnosticOrderStatusCompleted The work has been complete, the report(s) released, and no further work is planned. }
    DiagnosticOrderStatusSuspended, {@enum.value DiagnosticOrderStatusSuspended The request has been held by originating system/user request. }
    DiagnosticOrderStatusRejected, {@enum.value DiagnosticOrderStatusRejected The receiving system has declined to fulfill the request. }
    DiagnosticOrderStatusFailed); {@enum.value DiagnosticOrderStatusFailed The diagnostic investigation was attempted, but due to some procedural error, it could not be completed. }
  TFhirDiagnosticOrderStatusList = set of TFhirDiagnosticOrderStatus;

  {@Enum TFhirDiagnosticOrderPriority
    The clinical priority of a diagnostic order
  }
  TFhirDiagnosticOrderPriority = (
    DiagnosticOrderPriorityNull,  {@enum.value DiagnosticOrderPriorityNull Value is missing from Instance }
    DiagnosticOrderPriorityRoutine, {@enum.value DiagnosticOrderPriorityRoutine The order has a normal priority. }
    DiagnosticOrderPriorityUrgent, {@enum.value DiagnosticOrderPriorityUrgent The order should be urgently. }
    DiagnosticOrderPriorityStat, {@enum.value DiagnosticOrderPriorityStat The order is time-critical. }
    DiagnosticOrderPriorityAsap); {@enum.value DiagnosticOrderPriorityAsap The order should be acted on as soon as possible. }
  TFhirDiagnosticOrderPriorityList = set of TFhirDiagnosticOrderPriority;

  {@Enum TFhirDiagnosticReportStatus
    The status of the diagnostic report as a whole
  }
  TFhirDiagnosticReportStatus = (
    DiagnosticReportStatusNull,  {@enum.value DiagnosticReportStatusNull Value is missing from Instance }
    DiagnosticReportStatusRegistered, {@enum.value DiagnosticReportStatusRegistered The existence of the report is registered, but there is nothing yet available. }
    DiagnosticReportStatusPartial, {@enum.value DiagnosticReportStatusPartial This is a partial (e.g. initial, interim or preliminary) report: data in the report may be incomplete or unverified. }
    DiagnosticReportStatusFinal, {@enum.value DiagnosticReportStatusFinal The report is complete and verified by an authorized person. }
    DiagnosticReportStatusCorrected, {@enum.value DiagnosticReportStatusCorrected The report has been modified subsequent to being Final, and is complete and verified by an authorized person. }
    DiagnosticReportStatusAmended, {@enum.value DiagnosticReportStatusAmended The report has been modified subsequent to being Final, and is complete and verified by an authorized person, and data has been changed. }
    DiagnosticReportStatusAppended, {@enum.value DiagnosticReportStatusAppended The report has been modified subsequent to being Final, and is complete and verified by an authorized person. New content has been added, but existing content hasn't changed. }
    DiagnosticReportStatusCancelled, {@enum.value DiagnosticReportStatusCancelled The report is unavailable because the measurement was not started or not completed (also sometimes called "aborted"). }
    DiagnosticReportStatusEnteredInError); {@enum.value DiagnosticReportStatusEnteredInError The report has been withdrawn following previous Final release. }
  TFhirDiagnosticReportStatusList = set of TFhirDiagnosticReportStatus;

  {@Enum TFhirDocumentReferenceStatus
    The status of the document reference
  }
  TFhirDocumentReferenceStatus = (
    DocumentReferenceStatusNull,  {@enum.value DocumentReferenceStatusNull Value is missing from Instance }
    DocumentReferenceStatusCurrent, {@enum.value DocumentReferenceStatusCurrent This is the current reference for this document. }
    DocumentReferenceStatusSuperceded, {@enum.value DocumentReferenceStatusSuperceded This reference has been superseded by another reference. }
    DocumentReferenceStatusEnteredInError); {@enum.value DocumentReferenceStatusEnteredInError This reference was created in error. }
  TFhirDocumentReferenceStatusList = set of TFhirDocumentReferenceStatus;

  {@Enum TFhirDocumentRelationshipType
    The type of relationship between documents
  }
  TFhirDocumentRelationshipType = (
    DocumentRelationshipTypeNull,  {@enum.value DocumentRelationshipTypeNull Value is missing from Instance }
    DocumentRelationshipTypeReplaces, {@enum.value DocumentRelationshipTypeReplaces This document logically replaces or supercedes the target document. }
    DocumentRelationshipTypeTransforms, {@enum.value DocumentRelationshipTypeTransforms This document was generated by transforming the target document (e.g. format or language conversion). }
    DocumentRelationshipTypeSigns, {@enum.value DocumentRelationshipTypeSigns This document is a signature of the target document. }
    DocumentRelationshipTypeAppends); {@enum.value DocumentRelationshipTypeAppends This document adds additional information to the target document. }
  TFhirDocumentRelationshipTypeList = set of TFhirDocumentRelationshipType;

  {@Enum TFhirEncounterState
    Current state of the encounter
  }
  TFhirEncounterState = (
    EncounterStateNull,  {@enum.value EncounterStateNull Value is missing from Instance }
    EncounterStatePlanned, {@enum.value EncounterStatePlanned The Encounter has not yet started. }
    EncounterStateInProgress, {@enum.value EncounterStateInProgress The Encounter has begun and the patient is present / the practitioner and the patient are meeting. }
    EncounterStateOnleave, {@enum.value EncounterStateOnleave The Encounter has begun, but the patient is temporarily on leave. }
    EncounterStateFinished, {@enum.value EncounterStateFinished The Encounter has ended. }
    EncounterStateCancelled); {@enum.value EncounterStateCancelled The Encounter has ended before it has begun. }
  TFhirEncounterStateList = set of TFhirEncounterState;

  {@Enum TFhirEncounterClass
    Classification of the encounter
  }
  TFhirEncounterClass = (
    EncounterClassNull,  {@enum.value EncounterClassNull Value is missing from Instance }
    EncounterClassInpatient, {@enum.value EncounterClassInpatient An encounter during which the patient is hospitalized and stays overnight. }
    EncounterClassOutpatient, {@enum.value EncounterClassOutpatient An encounter during which the patient is not hospitalized overnight. }
    EncounterClassAmbulatory, {@enum.value EncounterClassAmbulatory An encounter where the patient visits the practitioner in his/her office, e.g. a G.P. visit. }
    EncounterClassEmergency, {@enum.value EncounterClassEmergency An encounter where the patient needs urgent care. }
    EncounterClassHome, {@enum.value EncounterClassHome An encounter where the practitioner visits the patient at his/her home. }
    EncounterClassField, {@enum.value EncounterClassField An encounter taking place outside the regular environment for giving care. }
    EncounterClassDaytime, {@enum.value EncounterClassDaytime An encounter where the patient needs more prolonged treatment or investigations than outpatients, but who do not need to stay in the hospital overnight. }
    EncounterClassVirtual); {@enum.value EncounterClassVirtual An encounter that takes place where the patient and practitioner do not physically meet but use electronic means for contact. }
  TFhirEncounterClassList = set of TFhirEncounterClass;

  {@Enum TFhirGroupType
    Types of resources that are part of group
  }
  TFhirGroupType = (
    GroupTypeNull,  {@enum.value GroupTypeNull Value is missing from Instance }
    GroupTypePerson, {@enum.value GroupTypePerson Group contains "person" Patient resources. }
    GroupTypeAnimal, {@enum.value GroupTypeAnimal Group contains "animal" Patient resources. }
    GroupTypePractitioner, {@enum.value GroupTypePractitioner Group contains healthcare practitioner resources. }
    GroupTypeDevice, {@enum.value GroupTypeDevice Group contains Device resources. }
    GroupTypeMedication, {@enum.value GroupTypeMedication Group contains Medication resources. }
    GroupTypeSubstance); {@enum.value GroupTypeSubstance Group contains Substance resources. }
  TFhirGroupTypeList = set of TFhirGroupType;

  {@Enum TFhirImagingModality
    Type of acquired image data in the instance
  }
  TFhirImagingModality = (
    ImagingModalityNull,  {@enum.value ImagingModalityNull Value is missing from Instance }
    ImagingModalityAR, {@enum.value ImagingModalityAR  }
    ImagingModalityBMD, {@enum.value ImagingModalityBMD  }
    ImagingModalityBDUS, {@enum.value ImagingModalityBDUS  }
    ImagingModalityEPS, {@enum.value ImagingModalityEPS  }
    ImagingModalityCR, {@enum.value ImagingModalityCR  }
    ImagingModalityCT, {@enum.value ImagingModalityCT  }
    ImagingModalityDX, {@enum.value ImagingModalityDX  }
    ImagingModalityECG, {@enum.value ImagingModalityECG  }
    ImagingModalityES, {@enum.value ImagingModalityES  }
    ImagingModalityXC, {@enum.value ImagingModalityXC  }
    ImagingModalityGM, {@enum.value ImagingModalityGM  }
    ImagingModalityHD, {@enum.value ImagingModalityHD  }
    ImagingModalityIO, {@enum.value ImagingModalityIO  }
    ImagingModalityIVOCT, {@enum.value ImagingModalityIVOCT  }
    ImagingModalityIVUS, {@enum.value ImagingModalityIVUS  }
    ImagingModalityKER, {@enum.value ImagingModalityKER  }
    ImagingModalityLEN, {@enum.value ImagingModalityLEN  }
    ImagingModalityMR, {@enum.value ImagingModalityMR  }
    ImagingModalityMG, {@enum.value ImagingModalityMG  }
    ImagingModalityNM, {@enum.value ImagingModalityNM  }
    ImagingModalityOAM, {@enum.value ImagingModalityOAM  }
    ImagingModalityOCT, {@enum.value ImagingModalityOCT  }
    ImagingModalityOPM, {@enum.value ImagingModalityOPM  }
    ImagingModalityOP, {@enum.value ImagingModalityOP  }
    ImagingModalityOPR, {@enum.value ImagingModalityOPR  }
    ImagingModalityOPT, {@enum.value ImagingModalityOPT  }
    ImagingModalityOPV, {@enum.value ImagingModalityOPV  }
    ImagingModalityPX, {@enum.value ImagingModalityPX  }
    ImagingModalityPT, {@enum.value ImagingModalityPT  }
    ImagingModalityRF, {@enum.value ImagingModalityRF  }
    ImagingModalityRG, {@enum.value ImagingModalityRG  }
    ImagingModalitySM, {@enum.value ImagingModalitySM  }
    ImagingModalitySRF, {@enum.value ImagingModalitySRF  }
    ImagingModalityUS, {@enum.value ImagingModalityUS  }
    ImagingModalityVA, {@enum.value ImagingModalityVA  }
    ImagingModalityXA); {@enum.value ImagingModalityXA  }
  TFhirImagingModalityList = set of TFhirImagingModality;

  {@Enum TFhirInstanceAvailability
    Availability of the resource
  }
  TFhirInstanceAvailability = (
    InstanceAvailabilityNull,  {@enum.value InstanceAvailabilityNull Value is missing from Instance }
    InstanceAvailabilityONLINE, {@enum.value InstanceAvailabilityONLINE Resources are immediately available,. }
    InstanceAvailabilityOFFLINE, {@enum.value InstanceAvailabilityOFFLINE Resources need to be retrieved by manual intervention. }
    InstanceAvailabilityNEARLINE, {@enum.value InstanceAvailabilityNEARLINE Resources need to be retrieved from relatively slow media. }
    InstanceAvailabilityUNAVAILABLE); {@enum.value InstanceAvailabilityUNAVAILABLE Resources cannot be retrieved. }
  TFhirInstanceAvailabilityList = set of TFhirInstanceAvailability;

  {@Enum TFhirModality
    Type of data in the instance
  }
  TFhirModality = (
    ModalityNull,  {@enum.value ModalityNull Value is missing from Instance }
    ModalityAR, {@enum.value ModalityAR  }
    ModalityAU, {@enum.value ModalityAU  }
    ModalityBDUS, {@enum.value ModalityBDUS  }
    ModalityBI, {@enum.value ModalityBI  }
    ModalityBMD, {@enum.value ModalityBMD  }
    ModalityCR, {@enum.value ModalityCR  }
    ModalityCT, {@enum.value ModalityCT  }
    ModalityDG, {@enum.value ModalityDG  }
    ModalityDX, {@enum.value ModalityDX  }
    ModalityECG, {@enum.value ModalityECG  }
    ModalityEPS, {@enum.value ModalityEPS  }
    ModalityES, {@enum.value ModalityES  }
    ModalityGM, {@enum.value ModalityGM  }
    ModalityHC, {@enum.value ModalityHC  }
    ModalityHD, {@enum.value ModalityHD  }
    ModalityIO, {@enum.value ModalityIO  }
    ModalityIVOCT, {@enum.value ModalityIVOCT  }
    ModalityIVUS, {@enum.value ModalityIVUS  }
    ModalityKER, {@enum.value ModalityKER  }
    ModalityKO, {@enum.value ModalityKO  }
    ModalityLEN, {@enum.value ModalityLEN  }
    ModalityLS, {@enum.value ModalityLS  }
    ModalityMG, {@enum.value ModalityMG  }
    ModalityMR, {@enum.value ModalityMR  }
    ModalityNM, {@enum.value ModalityNM  }
    ModalityOAM, {@enum.value ModalityOAM  }
    ModalityOCT, {@enum.value ModalityOCT  }
    ModalityOP, {@enum.value ModalityOP  }
    ModalityOPM, {@enum.value ModalityOPM  }
    ModalityOPT, {@enum.value ModalityOPT  }
    ModalityOPV, {@enum.value ModalityOPV  }
    ModalityOT, {@enum.value ModalityOT  }
    ModalityPR, {@enum.value ModalityPR  }
    ModalityPT, {@enum.value ModalityPT  }
    ModalityPX, {@enum.value ModalityPX  }
    ModalityREG, {@enum.value ModalityREG  }
    ModalityRF, {@enum.value ModalityRF  }
    ModalityRG, {@enum.value ModalityRG  }
    ModalityRTDOSE, {@enum.value ModalityRTDOSE  }
    ModalityRTIMAGE, {@enum.value ModalityRTIMAGE  }
    ModalityRTPLAN, {@enum.value ModalityRTPLAN  }
    ModalityRTRECORD, {@enum.value ModalityRTRECORD  }
    ModalityRTSTRUCT, {@enum.value ModalityRTSTRUCT  }
    ModalitySEG, {@enum.value ModalitySEG  }
    ModalitySM, {@enum.value ModalitySM  }
    ModalitySMR, {@enum.value ModalitySMR  }
    ModalitySR, {@enum.value ModalitySR  }
    ModalitySRF, {@enum.value ModalitySRF  }
    ModalityTG, {@enum.value ModalityTG  }
    ModalityUS, {@enum.value ModalityUS  }
    ModalityVA, {@enum.value ModalityVA  }
    ModalityXA, {@enum.value ModalityXA  }
    ModalityXC); {@enum.value ModalityXC  }
  TFhirModalityList = set of TFhirModality;

  {@Enum TFhirListMode
    The processing mode that applies to this list
  }
  TFhirListMode = (
    ListModeNull,  {@enum.value ListModeNull Value is missing from Instance }
    ListModeWorking, {@enum.value ListModeWorking This list is the master list, maintained in an ongoing fashion with regular updates as the real world list it is tracking changes. }
    ListModeSnapshot, {@enum.value ListModeSnapshot This list was prepared as a snapshot. It should not be assumed to be current. }
    ListModeChanges); {@enum.value ListModeChanges The list is prepared as a statement of changes that have been made or recommended. }
  TFhirListModeList = set of TFhirListMode;

  {@Enum TFhirLocationStatus
    Indicates whether the location is still in use
  }
  TFhirLocationStatus = (
    LocationStatusNull,  {@enum.value LocationStatusNull Value is missing from Instance }
    LocationStatusActive, {@enum.value LocationStatusActive The location is operational. }
    LocationStatusSuspended, {@enum.value LocationStatusSuspended The location is temporarily closed. }
    LocationStatusInactive); {@enum.value LocationStatusInactive The location is no longer used. }
  TFhirLocationStatusList = set of TFhirLocationStatus;

  {@Enum TFhirLocationMode
    Indicates whether a resource instance represents a specific location or a class of locations
  }
  TFhirLocationMode = (
    LocationModeNull,  {@enum.value LocationModeNull Value is missing from Instance }
    LocationModeInstance, {@enum.value LocationModeInstance The Location resource represents a specific instance of a Location. }
    LocationModeKind); {@enum.value LocationModeKind The Location represents a class of Locations. }
  TFhirLocationModeList = set of TFhirLocationMode;

  {@Enum TFhirMediaType
    Whether the Media is a photo, video, or audio
  }
  TFhirMediaType = (
    MediaTypeNull,  {@enum.value MediaTypeNull Value is missing from Instance }
    MediaTypePhoto, {@enum.value MediaTypePhoto The media consists of one or more unmoving images, including photographs, computer-generated graphs and charts, and scanned documents. }
    MediaTypeVideo, {@enum.value MediaTypeVideo The media consists of a series of frames that capture a moving image. }
    MediaTypeAudio); {@enum.value MediaTypeAudio The media consists of a sound recording. }
  TFhirMediaTypeList = set of TFhirMediaType;

  {@Enum TFhirMedicationKind
    Whether the medication is a product or a package
  }
  TFhirMedicationKind = (
    MedicationKindNull,  {@enum.value MedicationKindNull Value is missing from Instance }
    MedicationKindProduct, {@enum.value MedicationKindProduct The medication is a product. }
    MedicationKindPackage); {@enum.value MedicationKindPackage The medication is a package - a contained group of one of more products. }
  TFhirMedicationKindList = set of TFhirMedicationKind;

  {@Enum TFhirMedicationAdminStatus
    A set of codes indicating the current status of a MedicationAdministration
  }
  TFhirMedicationAdminStatus = (
    MedicationAdminStatusNull,  {@enum.value MedicationAdminStatusNull Value is missing from Instance }
    MedicationAdminStatusInProgress, {@enum.value MedicationAdminStatusInProgress The administration has started but has not yet completed. }
    MedicationAdminStatusOnHold, {@enum.value MedicationAdminStatusOnHold Actions implied by the administration have been temporarily halted, but are expected to continue later. May also be called "suspended". }
    MedicationAdminStatusCompleted, {@enum.value MedicationAdminStatusCompleted All actions that are implied by the administration have occurred. }
    MedicationAdminStatusEnteredInError, {@enum.value MedicationAdminStatusEnteredInError The administration was entered in error and therefore nullified. }
    MedicationAdminStatusStopped); {@enum.value MedicationAdminStatusStopped Actions implied by the administration have been permanently halted, before all of them occurred. }
  TFhirMedicationAdminStatusList = set of TFhirMedicationAdminStatus;

  {@Enum TFhirMedicationDispenseStatus
    A code specifying the state of the dispense event.
  }
  TFhirMedicationDispenseStatus = (
    MedicationDispenseStatusNull,  {@enum.value MedicationDispenseStatusNull Value is missing from Instance }
    MedicationDispenseStatusInProgress, {@enum.value MedicationDispenseStatusInProgress The dispense has started but has not yet completed. }
    MedicationDispenseStatusOnHold, {@enum.value MedicationDispenseStatusOnHold Actions implied by the administration have been temporarily halted, but are expected to continue later. May also be called "suspended". }
    MedicationDispenseStatusCompleted, {@enum.value MedicationDispenseStatusCompleted All actions that are implied by the dispense have occurred. }
    MedicationDispenseStatusEnteredInError, {@enum.value MedicationDispenseStatusEnteredInError The dispense was entered in error and therefore nullified. }
    MedicationDispenseStatusStopped); {@enum.value MedicationDispenseStatusStopped Actions implied by the dispense have been permanently halted, before all of them occurred. }
  TFhirMedicationDispenseStatusList = set of TFhirMedicationDispenseStatus;

  {@Enum TFhirMedicationPrescriptionStatus
    A code specifying the state of the prescribing event. Describes the lifecycle of the prescription.
  }
  TFhirMedicationPrescriptionStatus = (
    MedicationPrescriptionStatusNull,  {@enum.value MedicationPrescriptionStatusNull Value is missing from Instance }
    MedicationPrescriptionStatusActive, {@enum.value MedicationPrescriptionStatusActive The prescription is 'actionable', but not all actions that are implied by it have occurred yet. }
    MedicationPrescriptionStatusOnHold, {@enum.value MedicationPrescriptionStatusOnHold Actions implied by the prescription have been temporarily halted, but are expected to continue later.  May also be called "suspended". }
    MedicationPrescriptionStatusCompleted, {@enum.value MedicationPrescriptionStatusCompleted All actions that are implied by the prescription have occurred (this will rarely be made explicit). }
    MedicationPrescriptionStatusEnteredInError, {@enum.value MedicationPrescriptionStatusEnteredInError The prescription was entered in error and therefore nullified. }
    MedicationPrescriptionStatusStopped, {@enum.value MedicationPrescriptionStatusStopped Actions implied by the prescription have been permanently halted, before all of them occurred. }
    MedicationPrescriptionStatusSuperceded); {@enum.value MedicationPrescriptionStatusSuperceded The prescription was replaced by a newer one, which encompasses all the information in the previous one. }
  TFhirMedicationPrescriptionStatusList = set of TFhirMedicationPrescriptionStatus;

  {@Enum TFhirResponseCode
    The kind of response to a message
  }
  TFhirResponseCode = (
    ResponseCodeNull,  {@enum.value ResponseCodeNull Value is missing from Instance }
    ResponseCodeOk, {@enum.value ResponseCodeOk The message was accepted and processed without error. }
    ResponseCodeTransientError, {@enum.value ResponseCodeTransientError Some internal unexpected error occurred - wait and try again. Note - this is usually used for things like database unavailable, which may be expected to resolve, though human intervention may be required. }
    ResponseCodeFatalError); {@enum.value ResponseCodeFatalError The message was rejected because of some content in it. There is no point in re-sending without change. The response narrative SHALL describe what the issue is. }
  TFhirResponseCodeList = set of TFhirResponseCode;

  {@Enum TFhirObservationStatus
    Codes providing the status of an observation
  }
  TFhirObservationStatus = (
    ObservationStatusNull,  {@enum.value ObservationStatusNull Value is missing from Instance }
    ObservationStatusRegistered, {@enum.value ObservationStatusRegistered The existence of the observation is registered, but there is no result yet available. }
    ObservationStatusPreliminary, {@enum.value ObservationStatusPreliminary This is an initial or interim observation: data may be incomplete or unverified. }
    ObservationStatusFinal, {@enum.value ObservationStatusFinal The observation is complete and verified by an authorized person. }
    ObservationStatusAmended, {@enum.value ObservationStatusAmended The observation has been modified subsequent to being Final, and is complete and verified by an authorized person. }
    ObservationStatusCancelled, {@enum.value ObservationStatusCancelled The observation is unavailable because the measurement was not started or not completed (also sometimes called "aborted"). }
    ObservationStatusEnteredInError); {@enum.value ObservationStatusEnteredInError The observation has been withdrawn following previous Final release. }
  TFhirObservationStatusList = set of TFhirObservationStatus;

  {@Enum TFhirObservationReliability
    Codes that provide an estimate of the degree to which quality issues have impacted on the value of an observation
  }
  TFhirObservationReliability = (
    ObservationReliabilityNull,  {@enum.value ObservationReliabilityNull Value is missing from Instance }
    ObservationReliabilityOk, {@enum.value ObservationReliabilityOk The result has no reliability concerns. }
    ObservationReliabilityOngoing, {@enum.value ObservationReliabilityOngoing An early estimate of value; measurement is still occurring. }
    ObservationReliabilityEarly, {@enum.value ObservationReliabilityEarly An early estimate of value; processing is still occurring. }
    ObservationReliabilityQuestionable, {@enum.value ObservationReliabilityQuestionable The observation value should be treated with care. }
    ObservationReliabilityCalibrating, {@enum.value ObservationReliabilityCalibrating The result has been generated while calibration is occurring. }
    ObservationReliabilityError, {@enum.value ObservationReliabilityError The observation could not be completed because of an error. }
    ObservationReliabilityUnknown); {@enum.value ObservationReliabilityUnknown No observation value was available. }
  TFhirObservationReliabilityList = set of TFhirObservationReliability;

  {@Enum TFhirObservationRelationshiptypes
    Codes specifying how two observations are related
  }
  TFhirObservationRelationshiptypes = (
    ObservationRelationshiptypesNull,  {@enum.value ObservationRelationshiptypesNull Value is missing from Instance }
    ObservationRelationshiptypesHasComponent, {@enum.value ObservationRelationshiptypesHasComponent The target observation is a component of this observation (e.g. Systolic and Diastolic Blood Pressure). }
    ObservationRelationshiptypesHasMember, {@enum.value ObservationRelationshiptypesHasMember This observation is a group observation (e.g. a battery, a panel of tests, a set of vital sign measurements) that includes the target as a member of the group. }
    ObservationRelationshiptypesDerivedFrom, {@enum.value ObservationRelationshiptypesDerivedFrom The target observation is part of the information from which this observation value is derived (e.g. calculated anion gap, Apgar score). }
    ObservationRelationshiptypesSequelTo, {@enum.value ObservationRelationshiptypesSequelTo This observation follows the target observation (e.g. timed tests such as Glucose Tolerance Test). }
    ObservationRelationshiptypesReplaces, {@enum.value ObservationRelationshiptypesReplaces This observation replaces a previous observation (i.e. a revised value). The target observation is now obsolete. }
    ObservationRelationshiptypesQualifiedBy, {@enum.value ObservationRelationshiptypesQualifiedBy The value of the target observation qualifies (refines) the semantics of the source observation (e.g. a lipaemia measure target from a plasma measure). }
    ObservationRelationshiptypesInterferedBy); {@enum.value ObservationRelationshiptypesInterferedBy The value of the target observation interferes (degardes quality, or prevents valid observation) with the semantics of the source observation (e.g. a hemolysis measure target from a plasma potassium measure which has no value). }
  TFhirObservationRelationshiptypesList = set of TFhirObservationRelationshiptypes;

  {@Enum TFhirIssueSeverity
    How the issue affects the success of the action
  }
  TFhirIssueSeverity = (
    IssueSeverityNull,  {@enum.value IssueSeverityNull Value is missing from Instance }
    IssueSeverityFatal, {@enum.value IssueSeverityFatal The issue caused the action to fail, and no further checking could be performed. }
    IssueSeverityError, {@enum.value IssueSeverityError The issue is sufficiently important to cause the action to fail. }
    IssueSeverityWarning, {@enum.value IssueSeverityWarning The issue is not important enough to cause the action to fail, but may cause it to be performed suboptimally or in a way that is not as desired. }
    IssueSeverityInformation); {@enum.value IssueSeverityInformation The issue has no relation to the degree of success of the action. }
  TFhirIssueSeverityList = set of TFhirIssueSeverity;

  {@Enum TFhirOrderOutcomeCode
    The status of the response to an order
  }
  TFhirOrderOutcomeCode = (
    OrderOutcomeCodeNull,  {@enum.value OrderOutcomeCodeNull Value is missing from Instance }
    OrderOutcomeCodePending, {@enum.value OrderOutcomeCodePending The order is known, but no processing has occurred at this time. }
    OrderOutcomeCodeReview, {@enum.value OrderOutcomeCodeReview The order is undergoing initial processing to determine whether it will be accepted (usually this involves human review). }
    OrderOutcomeCodeRejected, {@enum.value OrderOutcomeCodeRejected The order was rejected because of a workflow/business logic reason. }
    OrderOutcomeCodeError, {@enum.value OrderOutcomeCodeError The order was unable to be processed because of a technical error (i.e. unexpected error). }
    OrderOutcomeCodeAccepted, {@enum.value OrderOutcomeCodeAccepted The order has been accepted, and work is in progress. }
    OrderOutcomeCodeCancelled, {@enum.value OrderOutcomeCodeCancelled Processing the order was halted at the initiators request. }
    OrderOutcomeCodeReplaced, {@enum.value OrderOutcomeCodeReplaced The order has been cancelled and replaced by another. }
    OrderOutcomeCodeAborted, {@enum.value OrderOutcomeCodeAborted Processing the order was stopped because of some workflow/business logic reason. }
    OrderOutcomeCodeComplete); {@enum.value OrderOutcomeCodeComplete The order has been completed. }
  TFhirOrderOutcomeCodeList = set of TFhirOrderOutcomeCode;

  {@Enum TFhirLinkType
    The type of link between this patient resource and another patient resource.
  }
  TFhirLinkType = (
    LinkTypeNull,  {@enum.value LinkTypeNull Value is missing from Instance }
    LinkTypeReplace, {@enum.value LinkTypeReplace The patient resource containing this link must no longer be used. The link points forward to another patient resource that must be used in lieu of the patient resource that contains the link. }
    LinkTypeRefer, {@enum.value LinkTypeRefer The patient resource containing this link is in use and valid but not considered the main source of information about a patient. The link points forward to another patient resource that should be consulted to retrieve additional patient information. }
    LinkTypeSeealso); {@enum.value LinkTypeSeealso The patient resource containing this link is in use and valid, but points to another patient resource that is known to contain data about the same person. Data in this resource might overlap or contradict information found in the other patient resource. This link does not indicate any relative importance of the resources concerned, and both should be regarded as equally valid. }
  TFhirLinkTypeList = set of TFhirLinkType;

  {@Enum TFhirProcedureRelationshipType
    The nature of the relationship with this procedure
  }
  TFhirProcedureRelationshipType = (
    ProcedureRelationshipTypeNull,  {@enum.value ProcedureRelationshipTypeNull Value is missing from Instance }
    ProcedureRelationshipTypeCausedBy, {@enum.value ProcedureRelationshipTypeCausedBy This procedure had to be performed because of the related one. }
    ProcedureRelationshipTypeBecauseOf); {@enum.value ProcedureRelationshipTypeBecauseOf This procedure caused the related one to be performed. }
  TFhirProcedureRelationshipTypeList = set of TFhirProcedureRelationshipType;

  {@Enum TFhirResourceProfileStatus
    The lifecycle status of a Resource Profile
  }
  TFhirResourceProfileStatus = (
    ResourceProfileStatusNull,  {@enum.value ResourceProfileStatusNull Value is missing from Instance }
    ResourceProfileStatusDraft, {@enum.value ResourceProfileStatusDraft This profile is still under development. }
    ResourceProfileStatusActive, {@enum.value ResourceProfileStatusActive This profile is ready for normal use. }
    ResourceProfileStatusRetired); {@enum.value ResourceProfileStatusRetired This profile has been deprecated, withdrawn or superseded and should no longer be used. }
  TFhirResourceProfileStatusList = set of TFhirResourceProfileStatus;

  {@Enum TFhirPropertyRepresentation
    How a property is represented on the wire
  }
  TFhirPropertyRepresentation = (
    PropertyRepresentationNull,  {@enum.value PropertyRepresentationNull Value is missing from Instance }
    PropertyRepresentationXmlAttr); {@enum.value PropertyRepresentationXmlAttr In XML, this property is represented as an attribute not an element. }
  TFhirPropertyRepresentationList = set of TFhirPropertyRepresentation;

  {@Enum TFhirResourceSlicingRules
    How slices are interpreted when evaluating an instance
  }
  TFhirResourceSlicingRules = (
    ResourceSlicingRulesNull,  {@enum.value ResourceSlicingRulesNull Value is missing from Instance }
    ResourceSlicingRulesClosed, {@enum.value ResourceSlicingRulesClosed No additional content is allowed other than that described by the slices in this profile. }
    ResourceSlicingRulesOpen, {@enum.value ResourceSlicingRulesOpen Additional content is allowed anywhere in the list. }
    ResourceSlicingRulesOpenAtEnd); {@enum.value ResourceSlicingRulesOpenAtEnd Additional content is allowed, but only at the end of the list. }
  TFhirResourceSlicingRulesList = set of TFhirResourceSlicingRules;

  {@Enum TFhirResourceAggregationMode
    How resource references can be aggregated
  }
  TFhirResourceAggregationMode = (
    ResourceAggregationModeNull,  {@enum.value ResourceAggregationModeNull Value is missing from Instance }
    ResourceAggregationModeContained, {@enum.value ResourceAggregationModeContained The reference is a local reference to a contained resource. }
    ResourceAggregationModeReferenced, {@enum.value ResourceAggregationModeReferenced The reference to to a resource that has to be resolved externally to the resource that includes the reference. }
    ResourceAggregationModeBundled); {@enum.value ResourceAggregationModeBundled The resource the reference points to will be found in the same bundle as the resource that includes the reference. }
  TFhirResourceAggregationModeList = set of TFhirResourceAggregationMode;

  {@Enum TFhirConstraintSeverity
    SHALL applications comply with this constraint?
  }
  TFhirConstraintSeverity = (
    ConstraintSeverityNull,  {@enum.value ConstraintSeverityNull Value is missing from Instance }
    ConstraintSeverityError, {@enum.value ConstraintSeverityError If the constraint is violated, the resource is not conformant. }
    ConstraintSeverityWarning); {@enum.value ConstraintSeverityWarning If the constraint is violated, the resource is conformant, but it is not necessarily following best practice. }
  TFhirConstraintSeverityList = set of TFhirConstraintSeverity;

  {@Enum TFhirBindingConformance
    Binding conformance for applications
  }
  TFhirBindingConformance = (
    BindingConformanceNull,  {@enum.value BindingConformanceNull Value is missing from Instance }
    BindingConformanceRequired, {@enum.value BindingConformanceRequired Only codes in the specified set are allowed.  If the binding is extensible, other codes may be used for concepts not covered by the bound set of codes. }
    BindingConformancePreferred, {@enum.value BindingConformancePreferred For greater interoperability, implementers are strongly encouraged to use the bound set of codes, however alternate codes may be used in derived profiles and implementations if necessary without being considered non-conformant. }
    BindingConformanceExample); {@enum.value BindingConformanceExample The codes in the set are an example to illustrate the meaning of the field. There is no particular preference for its use nor any assertion that the provided values are sufficient to meet implementation needs. }
  TFhirBindingConformanceList = set of TFhirBindingConformance;

  {@Enum TFhirExtensionContext
    How an extension context is interpreted
  }
  TFhirExtensionContext = (
    ExtensionContextNull,  {@enum.value ExtensionContextNull Value is missing from Instance }
    ExtensionContextResource, {@enum.value ExtensionContextResource The context is all elements matching a particular resource element path. }
    ExtensionContextDatatype, {@enum.value ExtensionContextDatatype The context is all nodes matching a particular data type element path (root or repeating element) or all elements referencing a particular primitive data type (expressed as the datatype name). }
    ExtensionContextMapping, {@enum.value ExtensionContextMapping The context is all nodes whose mapping to a specified reference model corresponds to a particular mapping structure.  The context identifies the mapping target. The mapping should clearly identify where such an extension could be used. }
    ExtensionContextExtension); {@enum.value ExtensionContextExtension The context is a particular extension from a particular profile.  Expressed as uri#name, where uri identifies the profile and #name identifies the extension code. }
  TFhirExtensionContextList = set of TFhirExtensionContext;

  {@Enum TFhirProvenanceEntityRole
    How an entity was used in an activity
  }
  TFhirProvenanceEntityRole = (
    ProvenanceEntityRoleNull,  {@enum.value ProvenanceEntityRoleNull Value is missing from Instance }
    ProvenanceEntityRoleDerivation, {@enum.value ProvenanceEntityRoleDerivation A transformation of an entity into another, an update of an entity resulting in a new one, or the construction of a new entity based on a preexisting entity. }
    ProvenanceEntityRoleRevision, {@enum.value ProvenanceEntityRoleRevision A derivation for which the resulting entity is a revised version of some original. }
    ProvenanceEntityRoleQuotation, {@enum.value ProvenanceEntityRoleQuotation The repeat of (some or all of) an entity, such as text or image, by someone who may or may not be its original author. }
    ProvenanceEntityRoleSource); {@enum.value ProvenanceEntityRoleSource A primary source for a topic refers to something produced by some agent with direct experience and knowledge about the topic, at the time of the topic's study, without benefit from hindsight. }
  TFhirProvenanceEntityRoleList = set of TFhirProvenanceEntityRole;

  {@Enum TFhirQueryOutcome
    The outcome of processing a query request
  }
  TFhirQueryOutcome = (
    QueryOutcomeNull,  {@enum.value QueryOutcomeNull Value is missing from Instance }
    QueryOutcomeOk, {@enum.value QueryOutcomeOk The query was processed successfully. }
    QueryOutcomeLimited, {@enum.value QueryOutcomeLimited The query was processed successfully, but some additional limitations were added. }
    QueryOutcomeRefused, {@enum.value QueryOutcomeRefused The server refused to process the query. }
    QueryOutcomeError); {@enum.value QueryOutcomeError The server tried to process the query, but some error occurred. }
  TFhirQueryOutcomeList = set of TFhirQueryOutcome;

  {@Enum TFhirQuestionnaireStatus
    Lifecycle status of the questionnaire
  }
  TFhirQuestionnaireStatus = (
    QuestionnaireStatusNull,  {@enum.value QuestionnaireStatusNull Value is missing from Instance }
    QuestionnaireStatusDraft, {@enum.value QuestionnaireStatusDraft This Questionnaire is used as a template but the template is not ready for use or publication. }
    QuestionnaireStatusPublished, {@enum.value QuestionnaireStatusPublished This Questionnaire is used as a template, is published and ready for use. }
    QuestionnaireStatusRetired, {@enum.value QuestionnaireStatusRetired This Questionnaire is used as a template but should no longer be used for new Questionnaires. }
    QuestionnaireStatusInProgress, {@enum.value QuestionnaireStatusInProgress This Questionnaire has been filled out with answers, but changes or additions are still expected to be made to it. }
    QuestionnaireStatusCompleted, {@enum.value QuestionnaireStatusCompleted This Questionnaire has been filled out with answers, and the current content is regarded as definitive. }
    QuestionnaireStatusAmended); {@enum.value QuestionnaireStatusAmended This Questionnaire has been filled out with answers, then marked as complete, yet changes or additions have been made to it afterwards. }
  TFhirQuestionnaireStatusList = set of TFhirQuestionnaireStatus;

  {@Enum TFhirSecurityEventAction
    Indicator for type of action performed during the event that generated the audit.
  }
  TFhirSecurityEventAction = (
    SecurityEventActionNull,  {@enum.value SecurityEventActionNull Value is missing from Instance }
    SecurityEventActionC, {@enum.value SecurityEventActionC Create a new database object, such as Placing an Order. }
    SecurityEventActionR, {@enum.value SecurityEventActionR Display or print data, such as a Doctor Census. }
    SecurityEventActionU, {@enum.value SecurityEventActionU Update data, such as Revise Patient Information. }
    SecurityEventActionD, {@enum.value SecurityEventActionD Delete items, such as a doctor master file record. }
    SecurityEventActionE); {@enum.value SecurityEventActionE Perform a system or application function such as log-on, program execution or use of an object's method, or perform a query/search operation. }
  TFhirSecurityEventActionList = set of TFhirSecurityEventAction;

  {@Enum TFhirSecurityEventOutcome
    Indicates whether the event succeeded or failed
  }
  TFhirSecurityEventOutcome = (
    SecurityEventOutcomeNull,  {@enum.value SecurityEventOutcomeNull Value is missing from Instance }
    SecurityEventOutcome0, {@enum.value SecurityEventOutcome0 The operation completed successfully (whether with warnings or not). }
    SecurityEventOutcome4, {@enum.value SecurityEventOutcome4 The action was not successful due to some kind of catered for error (often equivalent to an HTTP 400 response). }
    SecurityEventOutcome8, {@enum.value SecurityEventOutcome8 The action was not successful due to some kind of unexpected error (often equivalent to an HTTP 500 response). }
    SecurityEventOutcome12); {@enum.value SecurityEventOutcome12 An error of such magnitude occurred that the system is not longer available for use (i.e. the system died). }
  TFhirSecurityEventOutcomeList = set of TFhirSecurityEventOutcome;

  {@Enum TFhirNetworkType
    The type of network access point that originated the audit event
  }
  TFhirNetworkType = (
    NetworkTypeNull,  {@enum.value NetworkTypeNull Value is missing from Instance }
    NetworkType1, {@enum.value NetworkType1 Machine Name, including DNS name. }
    NetworkType2, {@enum.value NetworkType2 IP Address. }
    NetworkType3, {@enum.value NetworkType3 Telephone Number. }
    NetworkType4, {@enum.value NetworkType4 Email address. }
    NetworkType5); {@enum.value NetworkType5 URI (User directory, HTTP-PUT, ftp, etc.). }
  TFhirNetworkTypeList = set of TFhirNetworkType;

  {@Enum TFhirObjectType
    Code for the participant object type being audited
  }
  TFhirObjectType = (
    ObjectTypeNull,  {@enum.value ObjectTypeNull Value is missing from Instance }
    ObjectType1, {@enum.value ObjectType1 Person. }
    ObjectType2, {@enum.value ObjectType2 System Object. }
    ObjectType3, {@enum.value ObjectType3 Organization. }
    ObjectType4); {@enum.value ObjectType4 Other. }
  TFhirObjectTypeList = set of TFhirObjectType;

  {@Enum TFhirObjectRole
    Code representing the functional application role of Participant Object being audited
  }
  TFhirObjectRole = (
    ObjectRoleNull,  {@enum.value ObjectRoleNull Value is missing from Instance }
    ObjectRole1, {@enum.value ObjectRole1 This object is the patient that is the subject of care related to this event.  It is identifiable by patient ID or equivalent.  The patient may be either human or animal. }
    ObjectRole2, {@enum.value ObjectRole2 This is a location identified as related to the event.  This is usually the location where the event took place.  Note that for shipping, the usual events are arrival at a location or departure from a location. }
    ObjectRole3, {@enum.value ObjectRole3 This object is any kind of persistent document created as a result of the event.  This could be a paper report, film, electronic report, DICOM Study, etc.  Issues related to medical records life cycle management are conveyed elsewhere. }
    ObjectRole4, {@enum.value ObjectRole4 A logical object related to the event.  (Deprecated). }
    ObjectRole5, {@enum.value ObjectRole5 This is any configurable file used to control creation of documents.  Examples include the objects maintained by the HL7 Master File transactions, Value Sets, etc. }
    ObjectRole6, {@enum.value ObjectRole6 A human participant not otherwise identified by some other category. }
    ObjectRole7, {@enum.value ObjectRole7 (deprecated). }
    ObjectRole8, {@enum.value ObjectRole8 Typically a licensed person who is providing or performing care related to the event, generally a physician.   The key distinction between doctor and practitioner is with regards to their role, not the licensing.  The doctor is the human who actually performed the work.  The practitioner is the human or organization that is responsible for the work. }
    ObjectRole9, {@enum.value ObjectRole9 A person or system that is being notified as part of the event.  This is relevant in situations where automated systems provide notifications to other parties when an event took place. }
    ObjectRole10, {@enum.value ObjectRole10 Insurance company, or any other organization who accepts responsibility for paying for the healthcare event. }
    ObjectRole11, {@enum.value ObjectRole11 A person or active system object involved in the event with a security role. }
    ObjectRole12, {@enum.value ObjectRole12 A person or system object involved in the event with the authority to modify security roles of other objects. }
    ObjectRole13, {@enum.value ObjectRole13 A passive object, such as a role table, that is relevant to the event. }
    ObjectRole14, {@enum.value ObjectRole14 (deprecated)  Relevant to certain RBAC security methodologies. }
    ObjectRole15, {@enum.value ObjectRole15 Any person or organization responsible for providing care.  This encompasses all forms of care, licensed or otherwise, and all sorts of teams and care groups. Note, the distinction between practitioners and the doctor that actually provided the care to the patient. }
    ObjectRole16, {@enum.value ObjectRole16 The source or destination for data transfer, when it does not match some other role. }
    ObjectRole17, {@enum.value ObjectRole17 A source or destination for data transfer, that acts as an archive, database, or similar role. }
    ObjectRole18, {@enum.value ObjectRole18 An object that holds schedule information.  This could be an appointment book, availability information, etc. }
    ObjectRole19, {@enum.value ObjectRole19 An organization or person that is the recipient of services.  This could be an organization that is buying services for a patient, or a person that is buying services for an animal. }
    ObjectRole20, {@enum.value ObjectRole20 An order, task, work item, procedure step, or other description of work to be performed.  E.g., a particular instance of an MPPS. }
    ObjectRole21, {@enum.value ObjectRole21 A list of jobs or a system that provides lists of jobs.  E.g., an MWL SCP. }
    ObjectRole22, {@enum.value ObjectRole22 (Deprecated). }
    ObjectRole23, {@enum.value ObjectRole23 An object that specifies or controls the routing or delivery of items.  For example, a distribution list is the routing criteria for mail.  The items delivered may be documents, jobs, or other objects. }
    ObjectRole24); {@enum.value ObjectRole24 The contents of a query.  This is used to capture the contents of any kind of query.  For security surveillance purposes knowing the queries being made is very important. }
  TFhirObjectRoleList = set of TFhirObjectRole;

  {@Enum TFhirObjectLifecycle
    Identifier for the data life-cycle stage for the participant object
  }
  TFhirObjectLifecycle = (
    ObjectLifecycleNull,  {@enum.value ObjectLifecycleNull Value is missing from Instance }
    ObjectLifecycle1, {@enum.value ObjectLifecycle1 Origination / Creation. }
    ObjectLifecycle2, {@enum.value ObjectLifecycle2 Import / Copy from original. }
    ObjectLifecycle3, {@enum.value ObjectLifecycle3 Amendment. }
    ObjectLifecycle4, {@enum.value ObjectLifecycle4 Verification. }
    ObjectLifecycle5, {@enum.value ObjectLifecycle5 Translation. }
    ObjectLifecycle6, {@enum.value ObjectLifecycle6 Access / Use. }
    ObjectLifecycle7, {@enum.value ObjectLifecycle7 De-identification. }
    ObjectLifecycle8, {@enum.value ObjectLifecycle8 Aggregation, summarization, derivation. }
    ObjectLifecycle9, {@enum.value ObjectLifecycle9 Report. }
    ObjectLifecycle10, {@enum.value ObjectLifecycle10 Export / Copy to target. }
    ObjectLifecycle11, {@enum.value ObjectLifecycle11 Disclosure. }
    ObjectLifecycle12, {@enum.value ObjectLifecycle12 Receipt of disclosure. }
    ObjectLifecycle13, {@enum.value ObjectLifecycle13 Archiving. }
    ObjectLifecycle14, {@enum.value ObjectLifecycle14 Logical deletion. }
    ObjectLifecycle15); {@enum.value ObjectLifecycle15 Permanent erasure / Physical destruction. }
  TFhirObjectLifecycleList = set of TFhirObjectLifecycle;

  {@Enum TFhirHierarchicalRelationshipType
    Type indicating if this is a parent or child relationship
  }
  TFhirHierarchicalRelationshipType = (
    HierarchicalRelationshipTypeNull,  {@enum.value HierarchicalRelationshipTypeNull Value is missing from Instance }
    HierarchicalRelationshipTypeParent, {@enum.value HierarchicalRelationshipTypeParent The target resource is the parent of the focal specimen resource. }
    HierarchicalRelationshipTypeChild); {@enum.value HierarchicalRelationshipTypeChild The target resource is the child of the focal specimen resource. }
  TFhirHierarchicalRelationshipTypeList = set of TFhirHierarchicalRelationshipType;

  {@Enum TFhirValuesetSupplyStatus
    Status of the supply
  }
  TFhirValuesetSupplyStatus = (
    ValuesetSupplyStatusNull,  {@enum.value ValuesetSupplyStatusNull Value is missing from Instance }
    ValuesetSupplyStatusRequested, {@enum.value ValuesetSupplyStatusRequested Supply has been requested, but not dispensed. }
    ValuesetSupplyStatusDispensed, {@enum.value ValuesetSupplyStatusDispensed Supply is part of a pharmacy order and has been dispensed. }
    ValuesetSupplyStatusReceived, {@enum.value ValuesetSupplyStatusReceived Supply has been received by the requestor. }
    ValuesetSupplyStatusFailed, {@enum.value ValuesetSupplyStatusFailed The supply will not be completed because the supplier was unable or unwilling to supply the item. }
    ValuesetSupplyStatusCancelled); {@enum.value ValuesetSupplyStatusCancelled The orderer of the supply cancelled the request. }
  TFhirValuesetSupplyStatusList = set of TFhirValuesetSupplyStatus;

  {@Enum TFhirValuesetSupplyDispenseStatus
    Status of the dispense
  }
  TFhirValuesetSupplyDispenseStatus = (
    ValuesetSupplyDispenseStatusNull,  {@enum.value ValuesetSupplyDispenseStatusNull Value is missing from Instance }
    ValuesetSupplyDispenseStatusInProgress, {@enum.value ValuesetSupplyDispenseStatusInProgress Supply has been requested, but not dispensed. }
    ValuesetSupplyDispenseStatusDispensed, {@enum.value ValuesetSupplyDispenseStatusDispensed Supply is part of a pharmacy order and has been dispensed. }
    ValuesetSupplyDispenseStatusAbandoned); {@enum.value ValuesetSupplyDispenseStatusAbandoned Dispensing was not completed. }
  TFhirValuesetSupplyDispenseStatusList = set of TFhirValuesetSupplyDispenseStatus;

  {@Enum TFhirFilterOperator
    The kind of operation to perform as a part of a property based filter
  }
  TFhirFilterOperator = (
    FilterOperatorNull,  {@enum.value FilterOperatorNull Value is missing from Instance }
    FilterOperatorEqual, {@enum.value FilterOperatorEqual The property value has the concept specified by the value. }
    FilterOperatorIsA, {@enum.value FilterOperatorIsA The property value has a concept that has an is-a relationship with the value. }
    FilterOperatorIsNotA, {@enum.value FilterOperatorIsNotA The property value has a concept that does not have an is-a relationship with the value. }
    FilterOperatorRegex, {@enum.value FilterOperatorRegex The property value representation matches the regex specified in the value. }
    FilterOperatorIn, {@enum.value FilterOperatorIn The property value is in the set of codes or concepts identified by the value. }
    FilterOperatorNotIn); {@enum.value FilterOperatorNotIn The property value is not in the set of codes or concepts identified by the value. }
  TFhirFilterOperatorList = set of TFhirFilterOperator;

Type
  TFhirElement = class;
  TFhirElementList = class;
  TFhirEnum = class;
  TFhirEnumList = class;
  TFhirInteger = class;
  TFhirIntegerList = class;
  TFhirDateTime = class;
  TFhirDateTimeList = class;
  TFhirDate = class;
  TFhirDateList = class;
  TFhirDecimal = class;
  TFhirDecimalList = class;
  TFhirUri = class;
  TFhirUriList = class;
  TFhirBase64Binary = class;
  TFhirBase64BinaryList = class;
  TFhirString = class;
  TFhirStringList = class;
  TFhirBoolean = class;
  TFhirBooleanList = class;
  TFhirInstant = class;
  TFhirInstantList = class;
  TFhirCode = class;
  TFhirCodeList = class;
  TFhirId = class;
  TFhirIdList = class;
  TFhirOid = class;
  TFhirOidList = class;
  TFhirUuid = class;
  TFhirUuidList = class;
  TFhirExtension = class;
  TFhirExtensionList = class;
  TFhirNarrative = class;
  TFhirNarrativeList = class;
  TFhirPeriod = class;
  TFhirPeriodList = class;
  TFhirCoding = class;
  TFhirCodingList = class;
  TFhirRange = class;
  TFhirRangeList = class;
  TFhirQuantity = class;
  TFhirQuantityList = class;
  TFhirAttachment = class;
  TFhirAttachmentList = class;
  TFhirRatio = class;
  TFhirRatioList = class;
  TFhirSampledData = class;
  TFhirSampledDataList = class;
  TFhirResourceReference = class;
  TFhirResourceReferenceList = class;
  TFhirCodeableConcept = class;
  TFhirCodeableConceptList = class;
  TFhirIdentifier = class;
  TFhirIdentifierList = class;
  TFhirScheduleRepeat = class;
  TFhirScheduleRepeatList = class;
  TFhirSchedule = class;
  TFhirScheduleList = class;
  TFhirContact = class;
  TFhirContactList = class;
  TFhirAddress = class;
  TFhirAddressList = class;
  TFhirHumanName = class;
  TFhirHumanNameList = class;

  {@Class TFhirElement : TFHIRBase
    Base Element Definition - extensions, ids
  }
  {!.Net HL7Connect.Fhir.Element}
  TFhirElement = {abstract} class (TFHIRBase)
  private
    FXmlId: String;
    FExtensionList : TFhirExtensionList;
    function GetExtensionList: TFhirExtensionList;
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirElement; overload;
    function Clone : TFhirElement; overload;
    function HasExtensions : Boolean;
    {!script show}
  published
    {@member xmlId
      the value of the xml id attribute, if present.
    }
    property xmlId : String read FXmlId write FXmlId;
    {@member ExtensionList
      Extensions on this value
    }
    property ExtensionList : TFhirExtensionList read GetExtensionList;
  end;
  

  {@Class TFhirType : TFhirElement
    A base FHIR type - (polymorphism support)
  }
  {!.Net HL7Connect.Fhir.Type}
  TFhirType = class (TFhirElement)
  Public
    {!script hide}
    Function Link : TFhirType; Overload;
    Function Clone : TFhirType; Overload;
    {!script show}
  End;
  TFHIRTypeClass = class of TFhirType;
  
  {@Class TFHIRPrimitiveType : TFhirType
    A base FHIR type - (polymorphism support)
  }
  {!.Net HL7Connect.Fhir.Type}
  TFHIRPrimitiveType = class (TFhirType)
  Public
    {!script hide}
    Function Link : TFHIRPrimitiveType; Overload;
    Function Clone : TFHIRPrimitiveType; Overload;
    Function AsStringValue : String; Overload;
    {!script show}
  End;
  TFHIRPrimitiveTypeClass = class of TFHIRPrimitiveType;
  
  {@Class TFhirBackboneElement : TFHIRBase
    Base Element Definition - extensions, ids
  }
  {!.Net HL7Connect.Fhir.BackboneElement}
  TFHIRBackboneElement = {abstract} class (TFhirElement)
  private
    FModifierExtensionList : TFhirExtensionList;
    function GetModifierExtensionList: TFhirExtensionList;
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFHIRBackboneElement; overload;
    function Clone : TFHIRBackboneElement; overload;
    function HasModifierExtensions : Boolean;
    {!script show}
  published
    {@member ModifierExtensionList
      Modifier Extensions on this value
    }
    property ModifierExtensionList : TFhirExtensionList read GetModifierExtensionList;
  end;
  


  TFhirElementListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirElementList;
    function GetCurrent : TFhirElement;
  public
    Constructor Create(list : TFhirElementList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElement read GetCurrent;
  end;


  {@Class TFhirElementList
    A list of FhirElement
  }
  {!.Net HL7Connect.Fhir.ElementList}
  TFhirElementList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirElement;
    procedure SetItemN(index : Integer; value : TFhirElement);
  public
    {!script hide}
    function Link : TFhirElementList; Overload;
    function Clone : TFhirElementList; Overload;
    function GetEnumerator : TFhirElementListEnumerator;
    {!script show}
    

    
    {@member AddItem
      Add an already existing FhirElement to the end of the list.
    }
    procedure AddItem(value : TFhirElement); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirElement) : Integer;
    

    {@member InsertItem
       Insert an existing FhirElement before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirElement);
    
    {@member Item
       Get the iIndexth FhirElement. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirElement. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirElement);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirElement;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirElements[index : Integer] : TFhirElement read GetItemN write SetItemN; default;
  End;


  {@Class TFhirEnum : TFhirType
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Enum}
  TFhirEnum = class (TFhirType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirEnum; Overload;
    Function Clone : TFhirEnum; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the enum
    }
    property value : String read FValue write SetValue;
  End;    


  TFhirEnumListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirEnumList;
    function GetCurrent : TFhirEnum;
  public
    Constructor Create(list : TFhirEnumList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEnum read GetCurrent;
  end;


  {@Class TFhirEnumList
    A list of FhirEnum
  }
  {!.Net HL7Connect.Fhir.EnumList}
  TFhirEnumList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirEnum;
    procedure SetItemN(index : Integer; value : TFhirEnum);
  public
    {!script hide}
    function Link : TFhirEnumList; Overload;
    function Clone : TFhirEnumList; Overload;
    function GetEnumerator : TFhirEnumListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirEnum to the end of the list.
    }
    function Append : TFhirEnum;

    
    {@member AddItem
      Add an already existing FhirEnum to the end of the list.
    }
    procedure AddItem(value : TFhirEnum); overload;

    
    {@member AddItem
      Add an already existing FhirEnum to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirEnum) : Integer;
    

    {@member Insert
      Insert FhirEnum before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirEnum;
    

    {@member InsertItem
       Insert an existing FhirEnum before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirEnum);
    
    {@member Item
       Get the iIndexth FhirEnum. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirEnum. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirEnum);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirEnum;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirEnums[index : Integer] : TFhirEnum read GetItemN write SetItemN; default;
  End;


  {@Class TFhirInteger : TFhirPrimitiveType
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Integer}
  TFhirInteger = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirInteger; Overload;
    Function Clone : TFhirInteger; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the integer
    }
    property value : String read FValue write SetValue;
  End;    


  TFhirIntegerListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirIntegerList;
    function GetCurrent : TFhirInteger;
  public
    Constructor Create(list : TFhirIntegerList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirInteger read GetCurrent;
  end;


  {@Class TFhirIntegerList
    A list of FhirInteger
  }
  {!.Net HL7Connect.Fhir.IntegerList}
  TFhirIntegerList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirInteger;
    procedure SetItemN(index : Integer; value : TFhirInteger);
  public
    {!script hide}
    function Link : TFhirIntegerList; Overload;
    function Clone : TFhirIntegerList; Overload;
    function GetEnumerator : TFhirIntegerListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirInteger to the end of the list.
    }
    function Append : TFhirInteger;

    
    {@member AddItem
      Add an already existing FhirInteger to the end of the list.
    }
    procedure AddItem(value : TFhirInteger); overload;

    
    {@member AddItem
      Add an already existing FhirInteger to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirInteger) : Integer;
    

    {@member Insert
      Insert FhirInteger before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirInteger;
    

    {@member InsertItem
       Insert an existing FhirInteger before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirInteger);
    
    {@member Item
       Get the iIndexth FhirInteger. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirInteger. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirInteger);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirInteger;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirIntegers[index : Integer] : TFhirInteger read GetItemN write SetItemN; default;
  End;


  {@Class TFhirDateTime : TFhirPrimitiveType
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.DateTime}
  TFhirDateTime = class (TFhirPrimitiveType)
  Private
    FValue: TDateTimeEx;
    procedure setValue(value: TDateTimeEx);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  Public
    Constructor Create(value : TDateTimeEx); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirDateTime; Overload;
    Function Clone : TFhirDateTime; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the dateTime
    }
    property value : TDateTimeEx read FValue write SetValue;
  End;    


  TFhirDateTimeListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirDateTimeList;
    function GetCurrent : TFhirDateTime;
  public
    Constructor Create(list : TFhirDateTimeList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDateTime read GetCurrent;
  end;


  {@Class TFhirDateTimeList
    A list of FhirDateTime
  }
  {!.Net HL7Connect.Fhir.DateTimeList}
  TFhirDateTimeList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirDateTime;
    procedure SetItemN(index : Integer; value : TFhirDateTime);
  public
    {!script hide}
    function Link : TFhirDateTimeList; Overload;
    function Clone : TFhirDateTimeList; Overload;
    function GetEnumerator : TFhirDateTimeListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirDateTime to the end of the list.
    }
    function Append : TFhirDateTime;

    
    {@member AddItem
      Add an already existing FhirDateTime to the end of the list.
    }
    procedure AddItem(value : TFhirDateTime); overload;

    
    {@member AddItem
      Add an already existing FhirDateTime to the end of the list.
    }
    procedure AddItem(value : TDateTimeEx); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirDateTime) : Integer;
    

    {@member Insert
      Insert FhirDateTime before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirDateTime;
    

    {@member InsertItem
       Insert an existing FhirDateTime before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirDateTime);
    
    {@member Item
       Get the iIndexth FhirDateTime. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirDateTime. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirDateTime);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirDateTime;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirDateTimes[index : Integer] : TFhirDateTime read GetItemN write SetItemN; default;
  End;


  {@Class TFhirDate : TFhirPrimitiveType
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Date}
  TFhirDate = class (TFhirPrimitiveType)
  Private
    FValue: TDateTimeEx;
    procedure setValue(value: TDateTimeEx);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  Public
    Constructor Create(value : TDateTimeEx); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirDate; Overload;
    Function Clone : TFhirDate; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the date
    }
    property value : TDateTimeEx read FValue write SetValue;
  End;    


  TFhirDateListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirDateList;
    function GetCurrent : TFhirDate;
  public
    Constructor Create(list : TFhirDateList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDate read GetCurrent;
  end;


  {@Class TFhirDateList
    A list of FhirDate
  }
  {!.Net HL7Connect.Fhir.DateList}
  TFhirDateList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirDate;
    procedure SetItemN(index : Integer; value : TFhirDate);
  public
    {!script hide}
    function Link : TFhirDateList; Overload;
    function Clone : TFhirDateList; Overload;
    function GetEnumerator : TFhirDateListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirDate to the end of the list.
    }
    function Append : TFhirDate;

    
    {@member AddItem
      Add an already existing FhirDate to the end of the list.
    }
    procedure AddItem(value : TFhirDate); overload;

    
    {@member AddItem
      Add an already existing FhirDate to the end of the list.
    }
    procedure AddItem(value : TDateTimeEx); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirDate) : Integer;
    

    {@member Insert
      Insert FhirDate before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirDate;
    

    {@member InsertItem
       Insert an existing FhirDate before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirDate);
    
    {@member Item
       Get the iIndexth FhirDate. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirDate. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirDate);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirDate;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirDates[index : Integer] : TFhirDate read GetItemN write SetItemN; default;
  End;


  {@Class TFhirDecimal : TFhirPrimitiveType
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Decimal}
  TFhirDecimal = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirDecimal; Overload;
    Function Clone : TFhirDecimal; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the decimal
    }
    property value : String read FValue write SetValue;
  End;    


  TFhirDecimalListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirDecimalList;
    function GetCurrent : TFhirDecimal;
  public
    Constructor Create(list : TFhirDecimalList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDecimal read GetCurrent;
  end;


  {@Class TFhirDecimalList
    A list of FhirDecimal
  }
  {!.Net HL7Connect.Fhir.DecimalList}
  TFhirDecimalList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirDecimal;
    procedure SetItemN(index : Integer; value : TFhirDecimal);
  public
    {!script hide}
    function Link : TFhirDecimalList; Overload;
    function Clone : TFhirDecimalList; Overload;
    function GetEnumerator : TFhirDecimalListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirDecimal to the end of the list.
    }
    function Append : TFhirDecimal;

    
    {@member AddItem
      Add an already existing FhirDecimal to the end of the list.
    }
    procedure AddItem(value : TFhirDecimal); overload;

    
    {@member AddItem
      Add an already existing FhirDecimal to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirDecimal) : Integer;
    

    {@member Insert
      Insert FhirDecimal before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirDecimal;
    

    {@member InsertItem
       Insert an existing FhirDecimal before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirDecimal);
    
    {@member Item
       Get the iIndexth FhirDecimal. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirDecimal. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirDecimal);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirDecimal;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirDecimals[index : Integer] : TFhirDecimal read GetItemN write SetItemN; default;
  End;


  {@Class TFhirUri : TFhirPrimitiveType
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Uri}
  TFhirUri = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirUri; Overload;
    Function Clone : TFhirUri; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the uri
    }
    property value : String read FValue write SetValue;
  End;    


  TFhirUriListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirUriList;
    function GetCurrent : TFhirUri;
  public
    Constructor Create(list : TFhirUriList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirUri read GetCurrent;
  end;


  {@Class TFhirUriList
    A list of FhirUri
  }
  {!.Net HL7Connect.Fhir.UriList}
  TFhirUriList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirUri;
    procedure SetItemN(index : Integer; value : TFhirUri);
  public
    {!script hide}
    function Link : TFhirUriList; Overload;
    function Clone : TFhirUriList; Overload;
    function GetEnumerator : TFhirUriListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirUri to the end of the list.
    }
    function Append : TFhirUri;

    
    {@member AddItem
      Add an already existing FhirUri to the end of the list.
    }
    procedure AddItem(value : TFhirUri); overload;

    
    {@member AddItem
      Add an already existing FhirUri to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirUri) : Integer;
    

    {@member Insert
      Insert FhirUri before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirUri;
    

    {@member InsertItem
       Insert an existing FhirUri before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirUri);
    
    {@member Item
       Get the iIndexth FhirUri. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirUri. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirUri);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirUri;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirUris[index : Integer] : TFhirUri read GetItemN write SetItemN; default;
  End;


  {@Class TFhirBase64Binary : TFhirPrimitiveType
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Base64Binary}
  TFhirBase64Binary = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirBase64Binary; Overload;
    Function Clone : TFhirBase64Binary; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the base64Binary
    }
    property value : String read FValue write SetValue;
  End;    


  TFhirBase64BinaryListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirBase64BinaryList;
    function GetCurrent : TFhirBase64Binary;
  public
    Constructor Create(list : TFhirBase64BinaryList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirBase64Binary read GetCurrent;
  end;


  {@Class TFhirBase64BinaryList
    A list of FhirBase64Binary
  }
  {!.Net HL7Connect.Fhir.Base64BinaryList}
  TFhirBase64BinaryList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirBase64Binary;
    procedure SetItemN(index : Integer; value : TFhirBase64Binary);
  public
    {!script hide}
    function Link : TFhirBase64BinaryList; Overload;
    function Clone : TFhirBase64BinaryList; Overload;
    function GetEnumerator : TFhirBase64BinaryListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirBase64Binary to the end of the list.
    }
    function Append : TFhirBase64Binary;

    
    {@member AddItem
      Add an already existing FhirBase64Binary to the end of the list.
    }
    procedure AddItem(value : TFhirBase64Binary); overload;

    
    {@member AddItem
      Add an already existing FhirBase64Binary to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirBase64Binary) : Integer;
    

    {@member Insert
      Insert FhirBase64Binary before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirBase64Binary;
    

    {@member InsertItem
       Insert an existing FhirBase64Binary before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirBase64Binary);
    
    {@member Item
       Get the iIndexth FhirBase64Binary. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirBase64Binary. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirBase64Binary);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirBase64Binary;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirBase64Binaries[index : Integer] : TFhirBase64Binary read GetItemN write SetItemN; default;
  End;


  {@Class TFhirString : TFhirPrimitiveType
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.String}
  TFhirString = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirString; Overload;
    Function Clone : TFhirString; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the string
    }
    property value : String read FValue write SetValue;
  End;    


  TFhirStringListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirStringList;
    function GetCurrent : TFhirString;
  public
    Constructor Create(list : TFhirStringList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirString read GetCurrent;
  end;


  {@Class TFhirStringList
    A list of FhirString
  }
  {!.Net HL7Connect.Fhir.StringList}
  TFhirStringList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirString;
    procedure SetItemN(index : Integer; value : TFhirString);
  public
    {!script hide}
    function Link : TFhirStringList; Overload;
    function Clone : TFhirStringList; Overload;
    function GetEnumerator : TFhirStringListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirString to the end of the list.
    }
    function Append : TFhirString;

    
    {@member AddItem
      Add an already existing FhirString to the end of the list.
    }
    procedure AddItem(value : TFhirString); overload;

    
    {@member AddItem
      Add an already existing FhirString to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirString) : Integer;
    

    {@member Insert
      Insert FhirString before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirString;
    

    {@member InsertItem
       Insert an existing FhirString before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirString);
    
    {@member Item
       Get the iIndexth FhirString. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirString. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirString);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirString;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirStrings[index : Integer] : TFhirString read GetItemN write SetItemN; default;
  End;


  {@Class TFhirBoolean : TFhirPrimitiveType
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Boolean}
  TFhirBoolean = class (TFhirPrimitiveType)
  Private
    FValue: Boolean;
    procedure setValue(value: Boolean);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  Public
    Constructor Create(value : Boolean); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirBoolean; Overload;
    Function Clone : TFhirBoolean; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the boolean
    }
    property value : Boolean read FValue write SetValue;
  End;    


  TFhirBooleanListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirBooleanList;
    function GetCurrent : TFhirBoolean;
  public
    Constructor Create(list : TFhirBooleanList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirBoolean read GetCurrent;
  end;


  {@Class TFhirBooleanList
    A list of FhirBoolean
  }
  {!.Net HL7Connect.Fhir.BooleanList}
  TFhirBooleanList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirBoolean;
    procedure SetItemN(index : Integer; value : TFhirBoolean);
  public
    {!script hide}
    function Link : TFhirBooleanList; Overload;
    function Clone : TFhirBooleanList; Overload;
    function GetEnumerator : TFhirBooleanListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirBoolean to the end of the list.
    }
    function Append : TFhirBoolean;

    
    {@member AddItem
      Add an already existing FhirBoolean to the end of the list.
    }
    procedure AddItem(value : TFhirBoolean); overload;

    
    {@member AddItem
      Add an already existing FhirBoolean to the end of the list.
    }
    procedure AddItem(value : Boolean); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirBoolean) : Integer;
    

    {@member Insert
      Insert FhirBoolean before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirBoolean;
    

    {@member InsertItem
       Insert an existing FhirBoolean before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirBoolean);
    
    {@member Item
       Get the iIndexth FhirBoolean. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirBoolean. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirBoolean);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirBoolean;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirBooleans[index : Integer] : TFhirBoolean read GetItemN write SetItemN; default;
  End;


  {@Class TFhirInstant : TFhirPrimitiveType
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Instant}
  TFhirInstant = class (TFhirPrimitiveType)
  Private
    FValue: TDateTimeEx;
    procedure setValue(value: TDateTimeEx);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  Public
    Constructor Create(value : TDateTimeEx); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirInstant; Overload;
    Function Clone : TFhirInstant; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the instant
    }
    property value : TDateTimeEx read FValue write SetValue;
  End;    


  TFhirInstantListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirInstantList;
    function GetCurrent : TFhirInstant;
  public
    Constructor Create(list : TFhirInstantList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirInstant read GetCurrent;
  end;


  {@Class TFhirInstantList
    A list of FhirInstant
  }
  {!.Net HL7Connect.Fhir.InstantList}
  TFhirInstantList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirInstant;
    procedure SetItemN(index : Integer; value : TFhirInstant);
  public
    {!script hide}
    function Link : TFhirInstantList; Overload;
    function Clone : TFhirInstantList; Overload;
    function GetEnumerator : TFhirInstantListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirInstant to the end of the list.
    }
    function Append : TFhirInstant;

    
    {@member AddItem
      Add an already existing FhirInstant to the end of the list.
    }
    procedure AddItem(value : TFhirInstant); overload;

    
    {@member AddItem
      Add an already existing FhirInstant to the end of the list.
    }
    procedure AddItem(value : TDateTimeEx); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirInstant) : Integer;
    

    {@member Insert
      Insert FhirInstant before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirInstant;
    

    {@member InsertItem
       Insert an existing FhirInstant before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirInstant);
    
    {@member Item
       Get the iIndexth FhirInstant. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirInstant. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirInstant);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirInstant;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirInstants[index : Integer] : TFhirInstant read GetItemN write SetItemN; default;
  End;


  {@Class TFhirCode : TFhirString
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Code}
  TFhirCode = class (TFhirString)
  Private
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirCode; Overload;
    Function Clone : TFhirCode; Overload;
    function FhirType : string; override;
    {!script show}
  End;    


  TFhirCodeListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirCodeList;
    function GetCurrent : TFhirCode;
  public
    Constructor Create(list : TFhirCodeList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirCode read GetCurrent;
  end;


  {@Class TFhirCodeList
    A list of FhirCode
  }
  {!.Net HL7Connect.Fhir.CodeList}
  TFhirCodeList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirCode;
    procedure SetItemN(index : Integer; value : TFhirCode);
  public
    {!script hide}
    function Link : TFhirCodeList; Overload;
    function Clone : TFhirCodeList; Overload;
    function GetEnumerator : TFhirCodeListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirCode to the end of the list.
    }
    function Append : TFhirCode;

    
    {@member AddItem
      Add an already existing FhirCode to the end of the list.
    }
    procedure AddItem(value : TFhirCode); overload;

    
    {@member AddItem
      Add an already existing FhirCode to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirCode) : Integer;
    

    {@member Insert
      Insert FhirCode before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirCode;
    

    {@member InsertItem
       Insert an existing FhirCode before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirCode);
    
    {@member Item
       Get the iIndexth FhirCode. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirCode. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirCode);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirCode;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirCodes[index : Integer] : TFhirCode read GetItemN write SetItemN; default;
  End;


  {@Class TFhirId : TFhirString
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Id}
  TFhirId = class (TFhirString)
  Private
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirId; Overload;
    Function Clone : TFhirId; Overload;
    function FhirType : string; override;
    {!script show}
  End;    


  TFhirIdListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirIdList;
    function GetCurrent : TFhirId;
  public
    Constructor Create(list : TFhirIdList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirId read GetCurrent;
  end;


  {@Class TFhirIdList
    A list of FhirId
  }
  {!.Net HL7Connect.Fhir.IdList}
  TFhirIdList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirId;
    procedure SetItemN(index : Integer; value : TFhirId);
  public
    {!script hide}
    function Link : TFhirIdList; Overload;
    function Clone : TFhirIdList; Overload;
    function GetEnumerator : TFhirIdListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirId to the end of the list.
    }
    function Append : TFhirId;

    
    {@member AddItem
      Add an already existing FhirId to the end of the list.
    }
    procedure AddItem(value : TFhirId); overload;

    
    {@member AddItem
      Add an already existing FhirId to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirId) : Integer;
    

    {@member Insert
      Insert FhirId before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirId;
    

    {@member InsertItem
       Insert an existing FhirId before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirId);
    
    {@member Item
       Get the iIndexth FhirId. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirId. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirId);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirId;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirIds[index : Integer] : TFhirId read GetItemN write SetItemN; default;
  End;


  {@Class TFhirOid : TFhirUri
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Oid}
  TFhirOid = class (TFhirUri)
  Private
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirOid; Overload;
    Function Clone : TFhirOid; Overload;
    function FhirType : string; override;
    {!script show}
  End;    


  TFhirOidListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirOidList;
    function GetCurrent : TFhirOid;
  public
    Constructor Create(list : TFhirOidList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirOid read GetCurrent;
  end;


  {@Class TFhirOidList
    A list of FhirOid
  }
  {!.Net HL7Connect.Fhir.OidList}
  TFhirOidList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirOid;
    procedure SetItemN(index : Integer; value : TFhirOid);
  public
    {!script hide}
    function Link : TFhirOidList; Overload;
    function Clone : TFhirOidList; Overload;
    function GetEnumerator : TFhirOidListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirOid to the end of the list.
    }
    function Append : TFhirOid;

    
    {@member AddItem
      Add an already existing FhirOid to the end of the list.
    }
    procedure AddItem(value : TFhirOid); overload;

    
    {@member AddItem
      Add an already existing FhirOid to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirOid) : Integer;
    

    {@member Insert
      Insert FhirOid before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirOid;
    

    {@member InsertItem
       Insert an existing FhirOid before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirOid);
    
    {@member Item
       Get the iIndexth FhirOid. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirOid. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirOid);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirOid;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirOids[index : Integer] : TFhirOid read GetItemN write SetItemN; default;
  End;


  {@Class TFhirUuid : TFhirUri
    a complex string - has an xmlId attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Uuid}
  TFhirUuid = class (TFhirUri)
  Private
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirUuid; Overload;
    Function Clone : TFhirUuid; Overload;
    function FhirType : string; override;
    {!script show}
  End;    


  TFhirUuidListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirUuidList;
    function GetCurrent : TFhirUuid;
  public
    Constructor Create(list : TFhirUuidList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirUuid read GetCurrent;
  end;


  {@Class TFhirUuidList
    A list of FhirUuid
  }
  {!.Net HL7Connect.Fhir.UuidList}
  TFhirUuidList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirUuid;
    procedure SetItemN(index : Integer; value : TFhirUuid);
  public
    {!script hide}
    function Link : TFhirUuidList; Overload;
    function Clone : TFhirUuidList; Overload;
    function GetEnumerator : TFhirUuidListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirUuid to the end of the list.
    }
    function Append : TFhirUuid;

    
    {@member AddItem
      Add an already existing FhirUuid to the end of the list.
    }
    procedure AddItem(value : TFhirUuid); overload;

    
    {@member AddItem
      Add an already existing FhirUuid to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirUuid) : Integer;
    

    {@member Insert
      Insert FhirUuid before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirUuid;
    

    {@member InsertItem
       Insert an existing FhirUuid before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirUuid);
    
    {@member Item
       Get the iIndexth FhirUuid. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirUuid. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirUuid);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirUuid;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirUuids[index : Integer] : TFhirUuid read GetItemN write SetItemN; default;
  End;


  {@Class TFhirExtension : TFHIRType
    Optional Extensions Element - found in all resources.
  }
  {!.Net HL7Connect.Fhir.Extension}
  TFhirExtension = class (TFHIRType)
  private
    FUrl : TFhirUri;
    FValue : TFhirType;
    Procedure SetUrl(value : TFhirUri);
    Function GetUrlST : String;
    Procedure SetUrlST(value : String);
    Procedure SetValue(value : TFhirType);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirExtension; overload;
    function Clone : TFhirExtension; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member url
      Source of the definition for the extension code - a logical name or a URL.
    }
    {@member url
      Typed access to Source of the definition for the extension code - a logical name or a URL.
    }
    property url : String read GetUrlST write SetUrlST;
    property urlObject : TFhirUri read FUrl write SetUrl;

    {@member value
      Value of extension - may be a resource or one of a constrained set of the data types (see Extensibility in the spec for list).
    }
    property value : TFhirType read FValue write SetValue;
    property valueObject : TFhirType read FValue write SetValue;

  end;


  TFhirExtensionListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirExtensionList;
    function GetCurrent : TFhirExtension;
  public
    Constructor Create(list : TFhirExtensionList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirExtension read GetCurrent;
  end;


  {@Class TFhirExtensionList
    A list of FhirExtension
  }
  {!.Net HL7Connect.Fhir.ExtensionList}
  TFhirExtensionList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirExtension;
    procedure SetItemN(index : Integer; value : TFhirExtension);
  public
    {!script hide}
    function Link : TFhirExtensionList; Overload;
    function Clone : TFhirExtensionList; Overload;
    function GetEnumerator : TFhirExtensionListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirExtension to the end of the list.
    }
    function Append : TFhirExtension;

    
    {@member AddItem
      Add an already existing FhirExtension to the end of the list.
    }
    procedure AddItem(value : TFhirExtension); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirExtension) : Integer;
    

    {@member Insert
      Insert FhirExtension before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirExtension;
    

    {@member InsertItem
       Insert an existing FhirExtension before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirExtension);
    
    {@member Item
       Get the iIndexth FhirExtension. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirExtension. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirExtension);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirExtension;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirExtensions[index : Integer] : TFhirExtension read GetItemN write SetItemN; default;
  End;


  {@Class TFhirNarrative : TFHIRType
    A human-readable formatted text, including images.
  }
  {!.Net HL7Connect.Fhir.Narrative}
  TFhirNarrative = class (TFHIRType)
  private
    FStatus : TFhirEnum;
    FDiv_ : TFhirXHtmlNode;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirNarrativeStatus;
    Procedure SetStatusST(value : TFhirNarrativeStatus);
    Procedure SetDiv_(value : TFhirXHtmlNode);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirNarrative; overload;
    function Clone : TFhirNarrative; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member status
      The status of the narrative - whether it's entirely generated (from just the defined data or the extensions too), or whether a human authored it and it may contain additional data.
    }
    property status : TFhirNarrativeStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member div_
      The actual narrative content, a stripped down version of XHTML.
    }
    property div_ : TFhirXHtmlNode read FDiv_ write SetDiv_;
    property div_Object : TFhirXHtmlNode read FDiv_ write SetDiv_;

  end;


  TFhirNarrativeListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirNarrativeList;
    function GetCurrent : TFhirNarrative;
  public
    Constructor Create(list : TFhirNarrativeList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirNarrative read GetCurrent;
  end;


  {@Class TFhirNarrativeList
    A list of FhirNarrative
  }
  {!.Net HL7Connect.Fhir.NarrativeList}
  TFhirNarrativeList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirNarrative;
    procedure SetItemN(index : Integer; value : TFhirNarrative);
  public
    {!script hide}
    function Link : TFhirNarrativeList; Overload;
    function Clone : TFhirNarrativeList; Overload;
    function GetEnumerator : TFhirNarrativeListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirNarrative to the end of the list.
    }
    function Append : TFhirNarrative;

    
    {@member AddItem
      Add an already existing FhirNarrative to the end of the list.
    }
    procedure AddItem(value : TFhirNarrative); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirNarrative) : Integer;
    

    {@member Insert
      Insert FhirNarrative before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirNarrative;
    

    {@member InsertItem
       Insert an existing FhirNarrative before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirNarrative);
    
    {@member Item
       Get the iIndexth FhirNarrative. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirNarrative. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirNarrative);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirNarrative;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirNarratives[index : Integer] : TFhirNarrative read GetItemN write SetItemN; default;
  End;


  {@Class TFhirPeriod : TFhirType
    A time period defined by a start and end date and optionally time.
  }
  {!.Net HL7Connect.Fhir.Period}
  TFhirPeriod = class (TFhirType)
  private
    FStart : TFhirDateTime;
    FEnd_ : TFhirDateTime;
    Procedure SetStart(value : TFhirDateTime);
    Function GetStartST : TDateTimeEx;
    Procedure SetStartST(value : TDateTimeEx);
    Procedure SetEnd_(value : TFhirDateTime);
    Function GetEnd_ST : TDateTimeEx;
    Procedure SetEnd_ST(value : TDateTimeEx);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirPeriod; overload;
    function Clone : TFhirPeriod; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member start
      The start of the period. The boundary is inclusive.
    }
    {@member start
      Typed access to The start of the period. The boundary is inclusive.
    }
    property start : TDateTimeEx read GetStartST write SetStartST;
    property startObject : TFhirDateTime read FStart write SetStart;

    {@member end_
      The end of the period. If the end of the period is missing, it means that the period is ongoing.
    }
    {@member end_
      Typed access to The end of the period. If the end of the period is missing, it means that the period is ongoing.
    }
    property end_ : TDateTimeEx read GetEnd_ST write SetEnd_ST;
    property end_Object : TFhirDateTime read FEnd_ write SetEnd_;

  end;


  TFhirPeriodListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirPeriodList;
    function GetCurrent : TFhirPeriod;
  public
    Constructor Create(list : TFhirPeriodList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPeriod read GetCurrent;
  end;


  {@Class TFhirPeriodList
    A list of FhirPeriod
  }
  {!.Net HL7Connect.Fhir.PeriodList}
  TFhirPeriodList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirPeriod;
    procedure SetItemN(index : Integer; value : TFhirPeriod);
  public
    {!script hide}
    function Link : TFhirPeriodList; Overload;
    function Clone : TFhirPeriodList; Overload;
    function GetEnumerator : TFhirPeriodListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirPeriod to the end of the list.
    }
    function Append : TFhirPeriod;

    
    {@member AddItem
      Add an already existing FhirPeriod to the end of the list.
    }
    procedure AddItem(value : TFhirPeriod); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirPeriod) : Integer;
    

    {@member Insert
      Insert FhirPeriod before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirPeriod;
    

    {@member InsertItem
       Insert an existing FhirPeriod before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirPeriod);
    
    {@member Item
       Get the iIndexth FhirPeriod. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirPeriod. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirPeriod);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirPeriod;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirPeriods[index : Integer] : TFhirPeriod read GetItemN write SetItemN; default;
  End;


  {@Class TFhirCoding : TFhirType
    A reference to a code defined by a terminology system.
  }
  {!.Net HL7Connect.Fhir.Coding}
  TFhirCoding = class (TFhirType)
  private
    FSystem : TFhirUri;
    FVersion : TFhirString;
    FCode : TFhirCode;
    FDisplay : TFhirString;
    FPrimary : TFhirBoolean;
    FValueSet : TFhirResourceReference{TFhirValueSet};
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
    Procedure SetPrimary(value : TFhirBoolean);
    Function GetPrimaryST : Boolean;
    Procedure SetPrimaryST(value : Boolean);
    Procedure SetValueSet(value : TFhirResourceReference{TFhirValueSet});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirCoding; overload;
    function Clone : TFhirCoding; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member system
      The identification of the code system that defines the meaning of the symbol in the code.
    }
    {@member system
      Typed access to The identification of the code system that defines the meaning of the symbol in the code.
    }
    property system : String read GetSystemST write SetSystemST;
    property systemObject : TFhirUri read FSystem write SetSystem;

    {@member version
      The version of the code system which was used when choosing this code. Note that a well-maintained code system does not need the version reported, because the meaning of codes is consistent across versions. However this cannot consistently be assured. and When the meaning is not guaranteed to be consistent, the version SHOULD be exchanged.
    }
    {@member version
      Typed access to The version of the code system which was used when choosing this code. Note that a well-maintained code system does not need the version reported, because the meaning of codes is consistent across versions. However this cannot consistently be assured. and When the meaning is not guaranteed to be consistent, the version SHOULD be exchanged.
    }
    property version : String read GetVersionST write SetVersionST;
    property versionObject : TFhirString read FVersion write SetVersion;

    {@member code
      A symbol in syntax defined by the system. The symbol may be a predefined code or an expression in a syntax defined by the coding system (e.g. post-coordination).
    }
    {@member code
      Typed access to A symbol in syntax defined by the system. The symbol may be a predefined code or an expression in a syntax defined by the coding system (e.g. post-coordination).
    }
    property code : String read GetCodeST write SetCodeST;
    property codeObject : TFhirCode read FCode write SetCode;

    {@member display
      A representation of the meaning of the code in the system, following the rules of the system.
    }
    {@member display
      Typed access to A representation of the meaning of the code in the system, following the rules of the system.
    }
    property display : String read GetDisplayST write SetDisplayST;
    property displayObject : TFhirString read FDisplay write SetDisplay;

    {@member primary
      Indicates that this code was chosen by a user directly - i.e. off a pick list of available items (codes or displays).
    }
    {@member primary
      Typed access to Indicates that this code was chosen by a user directly - i.e. off a pick list of available items (codes or displays).
    }
    property primary : Boolean read GetPrimaryST write SetPrimaryST;
    property primaryObject : TFhirBoolean read FPrimary write SetPrimary;

    {@member valueSet
      The set of possible coded values this coding was chosen from or constrained by.
    }
    property valueSet : TFhirResourceReference{TFhirValueSet} read FValueSet write SetValueSet;
    property valueSetObject : TFhirResourceReference{TFhirValueSet} read FValueSet write SetValueSet;

  end;


  TFhirCodingListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirCodingList;
    function GetCurrent : TFhirCoding;
  public
    Constructor Create(list : TFhirCodingList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirCoding read GetCurrent;
  end;


  {@Class TFhirCodingList
    A list of FhirCoding
  }
  {!.Net HL7Connect.Fhir.CodingList}
  TFhirCodingList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirCoding;
    procedure SetItemN(index : Integer; value : TFhirCoding);
  public
    {!script hide}
    function Link : TFhirCodingList; Overload;
    function Clone : TFhirCodingList; Overload;
    function GetEnumerator : TFhirCodingListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirCoding to the end of the list.
    }
    function Append : TFhirCoding;

    
    {@member AddItem
      Add an already existing FhirCoding to the end of the list.
    }
    procedure AddItem(value : TFhirCoding); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirCoding) : Integer;
    

    {@member Insert
      Insert FhirCoding before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirCoding;
    

    {@member InsertItem
       Insert an existing FhirCoding before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirCoding);
    
    {@member Item
       Get the iIndexth FhirCoding. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirCoding. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirCoding);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirCoding;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirCodings[index : Integer] : TFhirCoding read GetItemN write SetItemN; default;
  End;


  {@Class TFhirRange : TFhirType
    A set of ordered Quantities defined by a low and high limit.
  }
  {!.Net HL7Connect.Fhir.Range}
  TFhirRange = class (TFhirType)
  private
    FLow : TFhirQuantity;
    FHigh : TFhirQuantity;
    Procedure SetLow(value : TFhirQuantity);
    Procedure SetHigh(value : TFhirQuantity);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirRange; overload;
    function Clone : TFhirRange; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member low
      The low limit. The boundary is inclusive.
    }
    property low : TFhirQuantity read FLow write SetLow;
    property lowObject : TFhirQuantity read FLow write SetLow;

    {@member high
      The high limit. The boundary is inclusive.
    }
    property high : TFhirQuantity read FHigh write SetHigh;
    property highObject : TFhirQuantity read FHigh write SetHigh;

  end;


  TFhirRangeListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirRangeList;
    function GetCurrent : TFhirRange;
  public
    Constructor Create(list : TFhirRangeList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirRange read GetCurrent;
  end;


  {@Class TFhirRangeList
    A list of FhirRange
  }
  {!.Net HL7Connect.Fhir.RangeList}
  TFhirRangeList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirRange;
    procedure SetItemN(index : Integer; value : TFhirRange);
  public
    {!script hide}
    function Link : TFhirRangeList; Overload;
    function Clone : TFhirRangeList; Overload;
    function GetEnumerator : TFhirRangeListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirRange to the end of the list.
    }
    function Append : TFhirRange;

    
    {@member AddItem
      Add an already existing FhirRange to the end of the list.
    }
    procedure AddItem(value : TFhirRange); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirRange) : Integer;
    

    {@member Insert
      Insert FhirRange before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirRange;
    

    {@member InsertItem
       Insert an existing FhirRange before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirRange);
    
    {@member Item
       Get the iIndexth FhirRange. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirRange. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirRange);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirRange;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirRanges[index : Integer] : TFhirRange read GetItemN write SetItemN; default;
  End;


  {@Class TFhirQuantity : TFhirType
    A measured amount (or an amount that can potentially be measured). Note that measured amounts include amounts that are not precisely quantified, including amounts involving arbitrary units and floating currencies.
  }
  {!.Net HL7Connect.Fhir.Quantity}
  TFhirQuantity = class (TFhirType)
  private
    FValue : TFhirDecimal;
    FComparator : TFhirEnum;
    FUnits : TFhirString;
    FSystem : TFhirUri;
    FCode : TFhirCode;
    Procedure SetValue(value : TFhirDecimal);
    Function GetValueST : String;
    Procedure SetValueST(value : String);
    Procedure SetComparator(value : TFhirEnum);
    Function GetComparatorST : TFhirQuantityComparator;
    Procedure SetComparatorST(value : TFhirQuantityComparator);
    Procedure SetUnits(value : TFhirString);
    Function GetUnitsST : String;
    Procedure SetUnitsST(value : String);
    Procedure SetSystem(value : TFhirUri);
    Function GetSystemST : String;
    Procedure SetSystemST(value : String);
    Procedure SetCode(value : TFhirCode);
    Function GetCodeST : String;
    Procedure SetCodeST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirQuantity; overload;
    function Clone : TFhirQuantity; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member value
      The value of the measured amount. The value includes an implicit precision in the presentation of the value.
    }
    {@member value
      Typed access to The value of the measured amount. The value includes an implicit precision in the presentation of the value.
    }
    property value : String read GetValueST write SetValueST;
    property valueObject : TFhirDecimal read FValue write SetValue;

    {@member comparator
      How the value should be understood and represented - whether the actual value is greater or less than the stated value due to measurement issues. E.g. if the comparator is "<" , then the real value is < stated value.
    }
    property comparator : TFhirQuantityComparator read GetComparatorST write SetComparatorST;
    property comparatorObject : TFhirEnum read FComparator write SetComparator;

    {@member units
      A human-readable form of the units.
    }
    {@member units
      Typed access to A human-readable form of the units.
    }
    property units : String read GetUnitsST write SetUnitsST;
    property unitsObject : TFhirString read FUnits write SetUnits;

    {@member system
      The identification of the system that provides the coded form of the unit.
    }
    {@member system
      Typed access to The identification of the system that provides the coded form of the unit.
    }
    property system : String read GetSystemST write SetSystemST;
    property systemObject : TFhirUri read FSystem write SetSystem;

    {@member code
      A computer processable form of the units in some unit representation system.
    }
    {@member code
      Typed access to A computer processable form of the units in some unit representation system.
    }
    property code : String read GetCodeST write SetCodeST;
    property codeObject : TFhirCode read FCode write SetCode;

  end;


  TFhirQuantityListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirQuantityList;
    function GetCurrent : TFhirQuantity;
  public
    Constructor Create(list : TFhirQuantityList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirQuantity read GetCurrent;
  end;


  {@Class TFhirQuantityList
    A list of FhirQuantity
  }
  {!.Net HL7Connect.Fhir.QuantityList}
  TFhirQuantityList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirQuantity;
    procedure SetItemN(index : Integer; value : TFhirQuantity);
  public
    {!script hide}
    function Link : TFhirQuantityList; Overload;
    function Clone : TFhirQuantityList; Overload;
    function GetEnumerator : TFhirQuantityListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirQuantity to the end of the list.
    }
    function Append : TFhirQuantity;

    
    {@member AddItem
      Add an already existing FhirQuantity to the end of the list.
    }
    procedure AddItem(value : TFhirQuantity); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirQuantity) : Integer;
    

    {@member Insert
      Insert FhirQuantity before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirQuantity;
    

    {@member InsertItem
       Insert an existing FhirQuantity before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirQuantity);
    
    {@member Item
       Get the iIndexth FhirQuantity. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirQuantity. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirQuantity);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirQuantity;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirQuantities[index : Integer] : TFhirQuantity read GetItemN write SetItemN; default;
  End;


  {@Class TFhirAttachment : TFhirType
    For referring to data content defined in other formats.
  }
  {!.Net HL7Connect.Fhir.Attachment}
  TFhirAttachment = class (TFhirType)
  private
    FContentType : TFhirCode;
    FLanguage : TFhirCode;
    FData : TFhirBase64Binary;
    FUrl : TFhirUri;
    FSize : TFhirInteger;
    FHash : TFhirBase64Binary;
    FTitle : TFhirString;
    Procedure SetContentType(value : TFhirCode);
    Function GetContentTypeST : String;
    Procedure SetContentTypeST(value : String);
    Procedure SetLanguage(value : TFhirCode);
    Function GetLanguageST : String;
    Procedure SetLanguageST(value : String);
    Procedure SetData(value : TFhirBase64Binary);
    Function GetDataST : String;
    Procedure SetDataST(value : String);
    Procedure SetUrl(value : TFhirUri);
    Function GetUrlST : String;
    Procedure SetUrlST(value : String);
    Procedure SetSize(value : TFhirInteger);
    Function GetSizeST : String;
    Procedure SetSizeST(value : String);
    Procedure SetHash(value : TFhirBase64Binary);
    Function GetHashST : String;
    Procedure SetHashST(value : String);
    Procedure SetTitle(value : TFhirString);
    Function GetTitleST : String;
    Procedure SetTitleST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirAttachment; overload;
    function Clone : TFhirAttachment; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member contentType
      Identifies the type of the data in the attachment and allows a method to be chosen to interpret or render the data. Includes mime type parameters such as charset where appropriate.
    }
    {@member contentType
      Typed access to Identifies the type of the data in the attachment and allows a method to be chosen to interpret or render the data. Includes mime type parameters such as charset where appropriate.
    }
    property contentType : String read GetContentTypeST write SetContentTypeST;
    property contentTypeObject : TFhirCode read FContentType write SetContentType;

    {@member language
      The human language of the content. The value can be any valid value according to BCP 47.
    }
    {@member language
      Typed access to The human language of the content. The value can be any valid value according to BCP 47.
    }
    property language : String read GetLanguageST write SetLanguageST;
    property languageObject : TFhirCode read FLanguage write SetLanguage;

    {@member data
      The actual data of the attachment - a sequence of bytes. In XML, represented using base64.
    }
    {@member data
      Typed access to The actual data of the attachment - a sequence of bytes. In XML, represented using base64.
    }
    property data : String read GetDataST write SetDataST;
    property dataObject : TFhirBase64Binary read FData write SetData;

    {@member url
      An alternative location where the data can be accessed.
    }
    {@member url
      Typed access to An alternative location where the data can be accessed.
    }
    property url : String read GetUrlST write SetUrlST;
    property urlObject : TFhirUri read FUrl write SetUrl;

    {@member size
      The number of bytes of data that make up this attachment.
    }
    {@member size
      Typed access to The number of bytes of data that make up this attachment.
    }
    property size : String read GetSizeST write SetSizeST;
    property sizeObject : TFhirInteger read FSize write SetSize;

    {@member hash
      The calculated hash of the data using SHA-1. Represented using base64.
    }
    {@member hash
      Typed access to The calculated hash of the data using SHA-1. Represented using base64.
    }
    property hash : String read GetHashST write SetHashST;
    property hashObject : TFhirBase64Binary read FHash write SetHash;

    {@member title
      A label or set of text to display in place of the data.
    }
    {@member title
      Typed access to A label or set of text to display in place of the data.
    }
    property title : String read GetTitleST write SetTitleST;
    property titleObject : TFhirString read FTitle write SetTitle;

  end;


  TFhirAttachmentListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirAttachmentList;
    function GetCurrent : TFhirAttachment;
  public
    Constructor Create(list : TFhirAttachmentList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirAttachment read GetCurrent;
  end;


  {@Class TFhirAttachmentList
    A list of FhirAttachment
  }
  {!.Net HL7Connect.Fhir.AttachmentList}
  TFhirAttachmentList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirAttachment;
    procedure SetItemN(index : Integer; value : TFhirAttachment);
  public
    {!script hide}
    function Link : TFhirAttachmentList; Overload;
    function Clone : TFhirAttachmentList; Overload;
    function GetEnumerator : TFhirAttachmentListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirAttachment to the end of the list.
    }
    function Append : TFhirAttachment;

    
    {@member AddItem
      Add an already existing FhirAttachment to the end of the list.
    }
    procedure AddItem(value : TFhirAttachment); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirAttachment) : Integer;
    

    {@member Insert
      Insert FhirAttachment before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirAttachment;
    

    {@member InsertItem
       Insert an existing FhirAttachment before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirAttachment);
    
    {@member Item
       Get the iIndexth FhirAttachment. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirAttachment. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirAttachment);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirAttachment;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirAttachments[index : Integer] : TFhirAttachment read GetItemN write SetItemN; default;
  End;


  {@Class TFhirRatio : TFhirType
    A relationship of two Quantity values - expressed as a numerator and a denominator.
  }
  {!.Net HL7Connect.Fhir.Ratio}
  TFhirRatio = class (TFhirType)
  private
    FNumerator : TFhirQuantity;
    FDenominator : TFhirQuantity;
    Procedure SetNumerator(value : TFhirQuantity);
    Procedure SetDenominator(value : TFhirQuantity);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirRatio; overload;
    function Clone : TFhirRatio; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member numerator
      The value of the numerator.
    }
    property numerator : TFhirQuantity read FNumerator write SetNumerator;
    property numeratorObject : TFhirQuantity read FNumerator write SetNumerator;

    {@member denominator
      The value of the denominator.
    }
    property denominator : TFhirQuantity read FDenominator write SetDenominator;
    property denominatorObject : TFhirQuantity read FDenominator write SetDenominator;

  end;


  TFhirRatioListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirRatioList;
    function GetCurrent : TFhirRatio;
  public
    Constructor Create(list : TFhirRatioList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirRatio read GetCurrent;
  end;


  {@Class TFhirRatioList
    A list of FhirRatio
  }
  {!.Net HL7Connect.Fhir.RatioList}
  TFhirRatioList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirRatio;
    procedure SetItemN(index : Integer; value : TFhirRatio);
  public
    {!script hide}
    function Link : TFhirRatioList; Overload;
    function Clone : TFhirRatioList; Overload;
    function GetEnumerator : TFhirRatioListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirRatio to the end of the list.
    }
    function Append : TFhirRatio;

    
    {@member AddItem
      Add an already existing FhirRatio to the end of the list.
    }
    procedure AddItem(value : TFhirRatio); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirRatio) : Integer;
    

    {@member Insert
      Insert FhirRatio before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirRatio;
    

    {@member InsertItem
       Insert an existing FhirRatio before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirRatio);
    
    {@member Item
       Get the iIndexth FhirRatio. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirRatio. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirRatio);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirRatio;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirRatios[index : Integer] : TFhirRatio read GetItemN write SetItemN; default;
  End;


  {@Class TFhirSampledData : TFhirType
    A series of measurements taken by a device, with upper and lower limits. There may be more than one dimension in the data.
  }
  {!.Net HL7Connect.Fhir.SampledData}
  TFhirSampledData = class (TFhirType)
  private
    FOrigin : TFhirQuantity;
    FPeriod : TFhirDecimal;
    FFactor : TFhirDecimal;
    FLowerLimit : TFhirDecimal;
    FUpperLimit : TFhirDecimal;
    FDimensions : TFhirInteger;
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
    Procedure SetDimensions(value : TFhirInteger);
    Function GetDimensionsST : String;
    Procedure SetDimensionsST(value : String);
    Procedure SetData(value : TFhirString);
    Function GetDataST : String;
    Procedure SetDataST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirSampledData; overload;
    function Clone : TFhirSampledData; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member origin
      The base quantity that a measured value of zero represents. In addition, this provides the units of the entire measurement series.
    }
    property origin : TFhirQuantity read FOrigin write SetOrigin;
    property originObject : TFhirQuantity read FOrigin write SetOrigin;

    {@member period
      The length of time between sampling times, measured in milliseconds.
    }
    {@member period
      Typed access to The length of time between sampling times, measured in milliseconds.
    }
    property period : String read GetPeriodST write SetPeriodST;
    property periodObject : TFhirDecimal read FPeriod write SetPeriod;

    {@member factor
      A correction factor that is applied to the sampled data points before they are added to the origin.
    }
    {@member factor
      Typed access to A correction factor that is applied to the sampled data points before they are added to the origin.
    }
    property factor : String read GetFactorST write SetFactorST;
    property factorObject : TFhirDecimal read FFactor write SetFactor;

    {@member lowerLimit
      The lower limit of detection of the measured points. This is needed if any of the data points have the value "L" (lower than detection limit).
    }
    {@member lowerLimit
      Typed access to The lower limit of detection of the measured points. This is needed if any of the data points have the value "L" (lower than detection limit).
    }
    property lowerLimit : String read GetLowerLimitST write SetLowerLimitST;
    property lowerLimitObject : TFhirDecimal read FLowerLimit write SetLowerLimit;

    {@member upperLimit
      The upper limit of detection of the measured points. This is needed if any of the data points have the value "U" (higher than detection limit).
    }
    {@member upperLimit
      Typed access to The upper limit of detection of the measured points. This is needed if any of the data points have the value "U" (higher than detection limit).
    }
    property upperLimit : String read GetUpperLimitST write SetUpperLimitST;
    property upperLimitObject : TFhirDecimal read FUpperLimit write SetUpperLimit;

    {@member dimensions
      The number of sample points at each time point. If this value is greater than one, then the dimensions will be interlaced - all the sample points for a point in time will be recorded at once.
    }
    {@member dimensions
      Typed access to The number of sample points at each time point. If this value is greater than one, then the dimensions will be interlaced - all the sample points for a point in time will be recorded at once.
    }
    property dimensions : String read GetDimensionsST write SetDimensionsST;
    property dimensionsObject : TFhirInteger read FDimensions write SetDimensions;

    {@member data
      A series of data points which are decimal values separated by a single space (character u20). The special values "E" (error), "L" (below detection limit) and "U" (above detection limit) can also be used in place of a decimal value.
    }
    {@member data
      Typed access to A series of data points which are decimal values separated by a single space (character u20). The special values "E" (error), "L" (below detection limit) and "U" (above detection limit) can also be used in place of a decimal value.
    }
    property data : String read GetDataST write SetDataST;
    property dataObject : TFhirString read FData write SetData;

  end;


  TFhirSampledDataListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirSampledDataList;
    function GetCurrent : TFhirSampledData;
  public
    Constructor Create(list : TFhirSampledDataList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirSampledData read GetCurrent;
  end;


  {@Class TFhirSampledDataList
    A list of FhirSampledData
  }
  {!.Net HL7Connect.Fhir.SampledDataList}
  TFhirSampledDataList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirSampledData;
    procedure SetItemN(index : Integer; value : TFhirSampledData);
  public
    {!script hide}
    function Link : TFhirSampledDataList; Overload;
    function Clone : TFhirSampledDataList; Overload;
    function GetEnumerator : TFhirSampledDataListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirSampledData to the end of the list.
    }
    function Append : TFhirSampledData;

    
    {@member AddItem
      Add an already existing FhirSampledData to the end of the list.
    }
    procedure AddItem(value : TFhirSampledData); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirSampledData) : Integer;
    

    {@member Insert
      Insert FhirSampledData before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirSampledData;
    

    {@member InsertItem
       Insert an existing FhirSampledData before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirSampledData);
    
    {@member Item
       Get the iIndexth FhirSampledData. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirSampledData. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirSampledData);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirSampledData;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirSampledData[index : Integer] : TFhirSampledData read GetItemN write SetItemN; default;
  End;


  {@Class TFhirResourceReference : TFhirType
    A reference from one resource to another.
  }
  {!.Net HL7Connect.Fhir.ResourceReference}
  TFhirResourceReference = class (TFhirType)
  private
    FReference : TFhirString;
    FDisplay : TFhirString;
    Procedure SetReference(value : TFhirString);
    Function GetReferenceST : String;
    Procedure SetReferenceST(value : String);
    Procedure SetDisplay(value : TFhirString);
    Function GetDisplayST : String;
    Procedure SetDisplayST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirResourceReference; overload;
    function Clone : TFhirResourceReference; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member reference
      A reference to a location at which the other resource is found. The reference may a relative reference, in which case it is relative to the service base URL, or an absolute URL that resolves to the location where the resource is found. The reference may be version specific or not. If the reference is not to a FHIR RESTful server, then it should be assumed to be version specific. Internal fragment references (start with '#') refer to contained resources.
    }
    {@member reference
      Typed access to A reference to a location at which the other resource is found. The reference may a relative reference, in which case it is relative to the service base URL, or an absolute URL that resolves to the location where the resource is found. The reference may be version specific or not. If the reference is not to a FHIR RESTful server, then it should be assumed to be version specific. Internal fragment references (start with '#') refer to contained resources.
    }
    property reference : String read GetReferenceST write SetReferenceST;
    property referenceObject : TFhirString read FReference write SetReference;

    {@member display
      Plain text narrative that identifies the resource in addition to the resource reference.
    }
    {@member display
      Typed access to Plain text narrative that identifies the resource in addition to the resource reference.
    }
    property display : String read GetDisplayST write SetDisplayST;
    property displayObject : TFhirString read FDisplay write SetDisplay;

  end;


  TFhirResourceReferenceListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirResourceReferenceList;
    function GetCurrent : TFhirResourceReference;
  public
    Constructor Create(list : TFhirResourceReferenceList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirResourceReference read GetCurrent;
  end;


  {@Class TFhirResourceReferenceList
    A list of FhirResourceReference
  }
  {!.Net HL7Connect.Fhir.ResourceReferenceList}
  TFhirResourceReferenceList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirResourceReference;
    procedure SetItemN(index : Integer; value : TFhirResourceReference);
  public
    {!script hide}
    function Link : TFhirResourceReferenceList; Overload;
    function Clone : TFhirResourceReferenceList; Overload;
    function GetEnumerator : TFhirResourceReferenceListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirResourceReference to the end of the list.
    }
    function Append : TFhirResourceReference;

    
    {@member AddItem
      Add an already existing FhirResourceReference to the end of the list.
    }
    procedure AddItem(value : TFhirResourceReference); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirResourceReference) : Integer;
    

    {@member Insert
      Insert FhirResourceReference before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirResourceReference;
    

    {@member InsertItem
       Insert an existing FhirResourceReference before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirResourceReference);
    
    {@member Item
       Get the iIndexth FhirResourceReference. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirResourceReference. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirResourceReference);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirResourceReference;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirResourceReferences[index : Integer] : TFhirResourceReference read GetItemN write SetItemN; default;
  End;


  {@Class TFhirCodeableConcept : TFhirType
    A concept that may be defined by a formal reference to a terminology or ontology or may be provided by text.
  }
  {!.Net HL7Connect.Fhir.CodeableConcept}
  TFhirCodeableConcept = class (TFhirType)
  private
    FcodingList : TFhirCodingList;
    FText : TFhirString;
    Procedure SetText(value : TFhirString);
    Function GetTextST : String;
    Procedure SetTextST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirCodeableConcept; overload;
    function Clone : TFhirCodeableConcept; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member codingList
      A reference to a code defined by a terminology system.
    }
    property codingList : TFhirCodingList read FCodingList;

    {@member text
      A human language representation of the concept as seen/selected/uttered by the user who entered the data and/or which represents the intended meaning of the user.
    }
    {@member text
      Typed access to A human language representation of the concept as seen/selected/uttered by the user who entered the data and/or which represents the intended meaning of the user.
    }
    property text : String read GetTextST write SetTextST;
    property textObject : TFhirString read FText write SetText;

  end;


  TFhirCodeableConceptListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirCodeableConceptList;
    function GetCurrent : TFhirCodeableConcept;
  public
    Constructor Create(list : TFhirCodeableConceptList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirCodeableConcept read GetCurrent;
  end;


  {@Class TFhirCodeableConceptList
    A list of FhirCodeableConcept
  }
  {!.Net HL7Connect.Fhir.CodeableConceptList}
  TFhirCodeableConceptList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirCodeableConcept;
    procedure SetItemN(index : Integer; value : TFhirCodeableConcept);
  public
    {!script hide}
    function Link : TFhirCodeableConceptList; Overload;
    function Clone : TFhirCodeableConceptList; Overload;
    function GetEnumerator : TFhirCodeableConceptListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirCodeableConcept to the end of the list.
    }
    function Append : TFhirCodeableConcept;

    
    {@member AddItem
      Add an already existing FhirCodeableConcept to the end of the list.
    }
    procedure AddItem(value : TFhirCodeableConcept); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirCodeableConcept) : Integer;
    

    {@member Insert
      Insert FhirCodeableConcept before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirCodeableConcept;
    

    {@member InsertItem
       Insert an existing FhirCodeableConcept before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirCodeableConcept);
    
    {@member Item
       Get the iIndexth FhirCodeableConcept. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirCodeableConcept. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirCodeableConcept);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirCodeableConcept;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirCodeableConcepts[index : Integer] : TFhirCodeableConcept read GetItemN write SetItemN; default;
  End;


  {@Class TFhirIdentifier : TFhirType
    A technical identifier - identifies some entity uniquely and unambiguously.
  }
  {!.Net HL7Connect.Fhir.Identifier}
  TFhirIdentifier = class (TFhirType)
  private
    FUse : TFhirEnum;
    FLabel_ : TFhirString;
    FSystem : TFhirUri;
    FValue : TFhirString;
    FPeriod : TFhirPeriod;
    FAssigner : TFhirResourceReference{TFhirOrganization};
    Procedure SetUse(value : TFhirEnum);
    Function GetUseST : TFhirIdentifierUse;
    Procedure SetUseST(value : TFhirIdentifierUse);
    Procedure SetLabel_(value : TFhirString);
    Function GetLabel_ST : String;
    Procedure SetLabel_ST(value : String);
    Procedure SetSystem(value : TFhirUri);
    Function GetSystemST : String;
    Procedure SetSystemST(value : String);
    Procedure SetValue(value : TFhirString);
    Function GetValueST : String;
    Procedure SetValueST(value : String);
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetAssigner(value : TFhirResourceReference{TFhirOrganization});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirIdentifier; overload;
    function Clone : TFhirIdentifier; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member use
      The purpose of this identifier.
    }
    property use : TFhirIdentifierUse read GetUseST write SetUseST;
    property useObject : TFhirEnum read FUse write SetUse;

    {@member label_
      A text string for the identifier that can be displayed to a human so they can recognize the identifier.
    }
    {@member label_
      Typed access to A text string for the identifier that can be displayed to a human so they can recognize the identifier.
    }
    property label_ : String read GetLabel_ST write SetLabel_ST;
    property label_Object : TFhirString read FLabel_ write SetLabel_;

    {@member system
      Establishes the namespace in which set of possible id values is unique.
    }
    {@member system
      Typed access to Establishes the namespace in which set of possible id values is unique.
    }
    property system : String read GetSystemST write SetSystemST;
    property systemObject : TFhirUri read FSystem write SetSystem;

    {@member value
      The portion of the identifier typically displayed to the user and which is unique within the context of the system.
    }
    {@member value
      Typed access to The portion of the identifier typically displayed to the user and which is unique within the context of the system.
    }
    property value : String read GetValueST write SetValueST;
    property valueObject : TFhirString read FValue write SetValue;

    {@member period
      Time period during which identifier is/was valid for use.
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;
    property periodObject : TFhirPeriod read FPeriod write SetPeriod;

    {@member assigner
      Organization that issued/manages the identifier.
    }
    property assigner : TFhirResourceReference{TFhirOrganization} read FAssigner write SetAssigner;
    property assignerObject : TFhirResourceReference{TFhirOrganization} read FAssigner write SetAssigner;

  end;


  TFhirIdentifierListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirIdentifierList;
    function GetCurrent : TFhirIdentifier;
  public
    Constructor Create(list : TFhirIdentifierList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirIdentifier read GetCurrent;
  end;


  {@Class TFhirIdentifierList
    A list of FhirIdentifier
  }
  {!.Net HL7Connect.Fhir.IdentifierList}
  TFhirIdentifierList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirIdentifier;
    procedure SetItemN(index : Integer; value : TFhirIdentifier);
  public
    {!script hide}
    function Link : TFhirIdentifierList; Overload;
    function Clone : TFhirIdentifierList; Overload;
    function GetEnumerator : TFhirIdentifierListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirIdentifier to the end of the list.
    }
    function Append : TFhirIdentifier;

    
    {@member AddItem
      Add an already existing FhirIdentifier to the end of the list.
    }
    procedure AddItem(value : TFhirIdentifier); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirIdentifier) : Integer;
    

    {@member Insert
      Insert FhirIdentifier before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirIdentifier;
    

    {@member InsertItem
       Insert an existing FhirIdentifier before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirIdentifier);
    
    {@member Item
       Get the iIndexth FhirIdentifier. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirIdentifier. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirIdentifier);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirIdentifier;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirIdentifiers[index : Integer] : TFhirIdentifier read GetItemN write SetItemN; default;
  End;


  {@Class TFhirScheduleRepeat : TFhirElement
    Identifies a repeating pattern to the intended time periods.
  }
  {!.Net HL7Connect.Fhir.ScheduleRepeat}
  TFhirScheduleRepeat = class (TFhirElement)
  private
    FFrequency : TFhirInteger;
    FWhen : TFhirEnum;
    FDuration : TFhirDecimal;
    FUnits : TFhirEnum;
    FCount : TFhirInteger;
    FEnd_ : TFhirDateTime;
    Procedure SetFrequency(value : TFhirInteger);
    Function GetFrequencyST : String;
    Procedure SetFrequencyST(value : String);
    Procedure SetWhen(value : TFhirEnum);
    Function GetWhenST : TFhirEventTiming;
    Procedure SetWhenST(value : TFhirEventTiming);
    Procedure SetDuration(value : TFhirDecimal);
    Function GetDurationST : String;
    Procedure SetDurationST(value : String);
    Procedure SetUnits(value : TFhirEnum);
    Function GetUnitsST : TFhirUnitsOfTime;
    Procedure SetUnitsST(value : TFhirUnitsOfTime);
    Procedure SetCount(value : TFhirInteger);
    Function GetCountST : String;
    Procedure SetCountST(value : String);
    Procedure SetEnd_(value : TFhirDateTime);
    Function GetEnd_ST : TDateTimeEx;
    Procedure SetEnd_ST(value : TDateTimeEx);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirScheduleRepeat; overload;
    function Clone : TFhirScheduleRepeat; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member frequency
      Indicates how often the event should occur.
    }
    {@member frequency
      Typed access to Indicates how often the event should occur.
    }
    property frequency : String read GetFrequencyST write SetFrequencyST;
    property frequencyObject : TFhirInteger read FFrequency write SetFrequency;

    {@member when
      Identifies the occurrence of daily life that determines timing.
    }
    property when : TFhirEventTiming read GetWhenST write SetWhenST;
    property whenObject : TFhirEnum read FWhen write SetWhen;

    {@member duration
      How long each repetition should last.
    }
    {@member duration
      Typed access to How long each repetition should last.
    }
    property duration : String read GetDurationST write SetDurationST;
    property durationObject : TFhirDecimal read FDuration write SetDuration;

    {@member units
      The units of time for the duration.
    }
    property units : TFhirUnitsOfTime read GetUnitsST write SetUnitsST;
    property unitsObject : TFhirEnum read FUnits write SetUnits;

    {@member count
      A total count of the desired number of repetitions.
    }
    {@member count
      Typed access to A total count of the desired number of repetitions.
    }
    property count : String read GetCountST write SetCountST;
    property countObject : TFhirInteger read FCount write SetCount;

    {@member end_
      When to stop repeating the schedule.
    }
    {@member end_
      Typed access to When to stop repeating the schedule.
    }
    property end_ : TDateTimeEx read GetEnd_ST write SetEnd_ST;
    property end_Object : TFhirDateTime read FEnd_ write SetEnd_;

  end;


  TFhirScheduleRepeatListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirScheduleRepeatList;
    function GetCurrent : TFhirScheduleRepeat;
  public
    Constructor Create(list : TFhirScheduleRepeatList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirScheduleRepeat read GetCurrent;
  end;


  {@Class TFhirScheduleRepeatList
    A list of FhirScheduleRepeat
  }
  {!.Net HL7Connect.Fhir.ScheduleRepeatList}
  TFhirScheduleRepeatList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirScheduleRepeat;
    procedure SetItemN(index : Integer; value : TFhirScheduleRepeat);
  public
    {!script hide}
    function Link : TFhirScheduleRepeatList; Overload;
    function Clone : TFhirScheduleRepeatList; Overload;
    function GetEnumerator : TFhirScheduleRepeatListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirScheduleRepeat to the end of the list.
    }
    function Append : TFhirScheduleRepeat;

    
    {@member AddItem
      Add an already existing FhirScheduleRepeat to the end of the list.
    }
    procedure AddItem(value : TFhirScheduleRepeat); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirScheduleRepeat) : Integer;
    

    {@member Insert
      Insert FhirScheduleRepeat before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirScheduleRepeat;
    

    {@member InsertItem
       Insert an existing FhirScheduleRepeat before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirScheduleRepeat);
    
    {@member Item
       Get the iIndexth FhirScheduleRepeat. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirScheduleRepeat. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirScheduleRepeat);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirScheduleRepeat;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirScheduleRepeats[index : Integer] : TFhirScheduleRepeat read GetItemN write SetItemN; default;
  End;


  {@Class TFhirSchedule : TFhirType
    Specifies an event that may occur multiple times. Schedules are used for to reord when things are expected or requested to occur.
  }
  {!.Net HL7Connect.Fhir.Schedule}
  TFhirSchedule = class (TFhirType)
  private
    FeventList : TFhirPeriodList;
    FRepeat_ : TFhirScheduleRepeat;
    Procedure SetRepeat_(value : TFhirScheduleRepeat);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirSchedule; overload;
    function Clone : TFhirSchedule; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member eventList
      Identifies specific time periods when the event should occur.
    }
    property eventList : TFhirPeriodList read FEventList;

    {@member repeat_
      Identifies a repeating pattern to the intended time periods.
    }
    property repeat_ : TFhirScheduleRepeat read FRepeat_ write SetRepeat_;
    property repeat_Object : TFhirScheduleRepeat read FRepeat_ write SetRepeat_;

  end;


  TFhirScheduleListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirScheduleList;
    function GetCurrent : TFhirSchedule;
  public
    Constructor Create(list : TFhirScheduleList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirSchedule read GetCurrent;
  end;


  {@Class TFhirScheduleList
    A list of FhirSchedule
  }
  {!.Net HL7Connect.Fhir.ScheduleList}
  TFhirScheduleList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirSchedule;
    procedure SetItemN(index : Integer; value : TFhirSchedule);
  public
    {!script hide}
    function Link : TFhirScheduleList; Overload;
    function Clone : TFhirScheduleList; Overload;
    function GetEnumerator : TFhirScheduleListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirSchedule to the end of the list.
    }
    function Append : TFhirSchedule;

    
    {@member AddItem
      Add an already existing FhirSchedule to the end of the list.
    }
    procedure AddItem(value : TFhirSchedule); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirSchedule) : Integer;
    

    {@member Insert
      Insert FhirSchedule before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirSchedule;
    

    {@member InsertItem
       Insert an existing FhirSchedule before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirSchedule);
    
    {@member Item
       Get the iIndexth FhirSchedule. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirSchedule. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirSchedule);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirSchedule;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirSchedules[index : Integer] : TFhirSchedule read GetItemN write SetItemN; default;
  End;


  {@Class TFhirContact : TFhirType
    All kinds of technology mediated contact details for a person or organization, including telephone, email, etc.
  }
  {!.Net HL7Connect.Fhir.Contact}
  TFhirContact = class (TFhirType)
  private
    FSystem : TFhirEnum;
    FValue : TFhirString;
    FUse : TFhirEnum;
    FPeriod : TFhirPeriod;
    Procedure SetSystem(value : TFhirEnum);
    Function GetSystemST : TFhirContactSystem;
    Procedure SetSystemST(value : TFhirContactSystem);
    Procedure SetValue(value : TFhirString);
    Function GetValueST : String;
    Procedure SetValueST(value : String);
    Procedure SetUse(value : TFhirEnum);
    Function GetUseST : TFhirContactUse;
    Procedure SetUseST(value : TFhirContactUse);
    Procedure SetPeriod(value : TFhirPeriod);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirContact; overload;
    function Clone : TFhirContact; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member system
      Telecommunications form for contact - what communications system is required to make use of the contact.
    }
    property system : TFhirContactSystem read GetSystemST write SetSystemST;
    property systemObject : TFhirEnum read FSystem write SetSystem;

    {@member value
      The actual contact details, in a form that is meaningful to the designated communication system (i.e. phone number or email address).
    }
    {@member value
      Typed access to The actual contact details, in a form that is meaningful to the designated communication system (i.e. phone number or email address).
    }
    property value : String read GetValueST write SetValueST;
    property valueObject : TFhirString read FValue write SetValue;

    {@member use
      Identifies the purpose for the address.
    }
    property use : TFhirContactUse read GetUseST write SetUseST;
    property useObject : TFhirEnum read FUse write SetUse;

    {@member period
      Time period when the contact was/is in use.
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;
    property periodObject : TFhirPeriod read FPeriod write SetPeriod;

  end;


  TFhirContactListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirContactList;
    function GetCurrent : TFhirContact;
  public
    Constructor Create(list : TFhirContactList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirContact read GetCurrent;
  end;


  {@Class TFhirContactList
    A list of FhirContact
  }
  {!.Net HL7Connect.Fhir.ContactList}
  TFhirContactList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirContact;
    procedure SetItemN(index : Integer; value : TFhirContact);
  public
    {!script hide}
    function Link : TFhirContactList; Overload;
    function Clone : TFhirContactList; Overload;
    function GetEnumerator : TFhirContactListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirContact to the end of the list.
    }
    function Append : TFhirContact;

    
    {@member AddItem
      Add an already existing FhirContact to the end of the list.
    }
    procedure AddItem(value : TFhirContact); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirContact) : Integer;
    

    {@member Insert
      Insert FhirContact before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirContact;
    

    {@member InsertItem
       Insert an existing FhirContact before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirContact);
    
    {@member Item
       Get the iIndexth FhirContact. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirContact. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirContact);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirContact;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirContacts[index : Integer] : TFhirContact read GetItemN write SetItemN; default;
  End;


  {@Class TFhirAddress : TFhirType
    There is a variety of postal address formats defined around the world. This format defines a superset that is the basis for all addresses around the world.
  }
  {!.Net HL7Connect.Fhir.Address}
  TFhirAddress = class (TFhirType)
  private
    FUse : TFhirEnum;
    FText : TFhirString;
    FlineList : TFhirStringList;
    FCity : TFhirString;
    FState : TFhirString;
    FZip : TFhirString;
    FCountry : TFhirString;
    FPeriod : TFhirPeriod;
    Procedure SetUse(value : TFhirEnum);
    Function GetUseST : TFhirAddressUse;
    Procedure SetUseST(value : TFhirAddressUse);
    Procedure SetText(value : TFhirString);
    Function GetTextST : String;
    Procedure SetTextST(value : String);
    Procedure SetCity(value : TFhirString);
    Function GetCityST : String;
    Procedure SetCityST(value : String);
    Procedure SetState(value : TFhirString);
    Function GetStateST : String;
    Procedure SetStateST(value : String);
    Procedure SetZip(value : TFhirString);
    Function GetZipST : String;
    Procedure SetZipST(value : String);
    Procedure SetCountry(value : TFhirString);
    Function GetCountryST : String;
    Procedure SetCountryST(value : String);
    Procedure SetPeriod(value : TFhirPeriod);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirAddress; overload;
    function Clone : TFhirAddress; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member use
      The purpose of this address.
    }
    property use : TFhirAddressUse read GetUseST write SetUseST;
    property useObject : TFhirEnum read FUse write SetUse;

    {@member text
      A full text representation of the address.
    }
    {@member text
      Typed access to A full text representation of the address.
    }
    property text : String read GetTextST write SetTextST;
    property textObject : TFhirString read FText write SetText;

    {@member lineList
      This component contains the house number, apartment number, street name, street direction, 
P.O. Box number, delivery hints, and similar address information.
    }
    property lineList : TFhirStringList read FLineList;

    {@member city
      The name of the city, town, village or other community or delivery center.
    }
    {@member city
      Typed access to The name of the city, town, village or other community or delivery center.
    }
    property city : String read GetCityST write SetCityST;
    property cityObject : TFhirString read FCity write SetCity;

    {@member state
      Sub-unit of a country with limited sovereignty in a federally organized country. A code may be used if codes are in common use (i.e. US 2 letter state codes).
    }
    {@member state
      Typed access to Sub-unit of a country with limited sovereignty in a federally organized country. A code may be used if codes are in common use (i.e. US 2 letter state codes).
    }
    property state : String read GetStateST write SetStateST;
    property stateObject : TFhirString read FState write SetState;

    {@member zip
      A postal code designating a region defined by the postal service.
    }
    {@member zip
      Typed access to A postal code designating a region defined by the postal service.
    }
    property zip : String read GetZipST write SetZipST;
    property zipObject : TFhirString read FZip write SetZip;

    {@member country
      Country - a nation as commonly understood or generally accepted.
    }
    {@member country
      Typed access to Country - a nation as commonly understood or generally accepted.
    }
    property country : String read GetCountryST write SetCountryST;
    property countryObject : TFhirString read FCountry write SetCountry;

    {@member period
      Time period when address was/is in use.
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;
    property periodObject : TFhirPeriod read FPeriod write SetPeriod;

  end;


  TFhirAddressListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirAddressList;
    function GetCurrent : TFhirAddress;
  public
    Constructor Create(list : TFhirAddressList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirAddress read GetCurrent;
  end;


  {@Class TFhirAddressList
    A list of FhirAddress
  }
  {!.Net HL7Connect.Fhir.AddressList}
  TFhirAddressList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirAddress;
    procedure SetItemN(index : Integer; value : TFhirAddress);
  public
    {!script hide}
    function Link : TFhirAddressList; Overload;
    function Clone : TFhirAddressList; Overload;
    function GetEnumerator : TFhirAddressListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirAddress to the end of the list.
    }
    function Append : TFhirAddress;

    
    {@member AddItem
      Add an already existing FhirAddress to the end of the list.
    }
    procedure AddItem(value : TFhirAddress); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirAddress) : Integer;
    

    {@member Insert
      Insert FhirAddress before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirAddress;
    

    {@member InsertItem
       Insert an existing FhirAddress before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirAddress);
    
    {@member Item
       Get the iIndexth FhirAddress. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirAddress. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirAddress);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirAddress;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirAddresses[index : Integer] : TFhirAddress read GetItemN write SetItemN; default;
  End;


  {@Class TFhirHumanName : TFhirType
    A human's name with the ability to identify parts and usage.
  }
  {!.Net HL7Connect.Fhir.HumanName}
  TFhirHumanName = class (TFhirType)
  private
    FUse : TFhirEnum;
    FText : TFhirString;
    FfamilyList : TFhirStringList;
    FgivenList : TFhirStringList;
    FprefixList : TFhirStringList;
    FsuffixList : TFhirStringList;
    FPeriod : TFhirPeriod;
    Procedure SetUse(value : TFhirEnum);
    Function GetUseST : TFhirNameUse;
    Procedure SetUseST(value : TFhirNameUse);
    Procedure SetText(value : TFhirString);
    Function GetTextST : String;
    Procedure SetTextST(value : String);
    Procedure SetPeriod(value : TFhirPeriod);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirHumanName; overload;
    function Clone : TFhirHumanName; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member use
      Identifies the purpose for this name.
    }
    property use : TFhirNameUse read GetUseST write SetUseST;
    property useObject : TFhirEnum read FUse write SetUse;

    {@member text
      A full text representation of the name.
    }
    {@member text
      Typed access to A full text representation of the name.
    }
    property text : String read GetTextST write SetTextST;
    property textObject : TFhirString read FText write SetText;

    {@member familyList
      The part of a name that links to the genealogy. In some cultures (e.g. Eritrea) the family name of a son is the first name of his father.
    }
    property familyList : TFhirStringList read FFamilyList;

    {@member givenList
      Given name.
    }
    property givenList : TFhirStringList read FGivenList;

    {@member prefixList
      Part of the name that is acquired as a title due to academic, legal, employment or nobility status, etc. and that appears at the start of the name.
    }
    property prefixList : TFhirStringList read FPrefixList;

    {@member suffixList
      Part of the name that is acquired as a title due to academic, legal, employment or nobility status, etc. and that appears at the end of the name.
    }
    property suffixList : TFhirStringList read FSuffixList;

    {@member period
      Indicates the period of time when this name was valid for the named person.
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;
    property periodObject : TFhirPeriod read FPeriod write SetPeriod;

  end;


  TFhirHumanNameListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirHumanNameList;
    function GetCurrent : TFhirHumanName;
  public
    Constructor Create(list : TFhirHumanNameList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirHumanName read GetCurrent;
  end;


  {@Class TFhirHumanNameList
    A list of FhirHumanName
  }
  {!.Net HL7Connect.Fhir.HumanNameList}
  TFhirHumanNameList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirHumanName;
    procedure SetItemN(index : Integer; value : TFhirHumanName);
  public
    {!script hide}
    function Link : TFhirHumanNameList; Overload;
    function Clone : TFhirHumanNameList; Overload;
    function GetEnumerator : TFhirHumanNameListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirHumanName to the end of the list.
    }
    function Append : TFhirHumanName;

    
    {@member AddItem
      Add an already existing FhirHumanName to the end of the list.
    }
    procedure AddItem(value : TFhirHumanName); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirHumanName) : Integer;
    

    {@member Insert
      Insert FhirHumanName before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirHumanName;
    

    {@member InsertItem
       Insert an existing FhirHumanName before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirHumanName);
    
    {@member Item
       Get the iIndexth FhirHumanName. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirHumanName. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirHumanName);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirHumanName;
    
    {@member Count
      The number of items in the collection
    }
    function Count : Integer; Overload;
    
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    procedure ClearItems;
    
    Property FhirHumanNames[index : Integer] : TFhirHumanName read GetItemN write SetItemN; default;
  End;


  TFhirAge = TFhirQuantity;

  TFhirCount = TFhirQuantity;

  TFhirMoney = TFhirQuantity;

  TFhirDistance = TFhirQuantity;

  TFhirDuration = TFhirQuantity;

Const
  CODES_TFhirNarrativeStatus : Array[TFhirNarrativeStatus] of String = ('', 'generated', 'extensions', 'additional', 'empty');
  CODES_TFhirQuantityComparator : Array[TFhirQuantityComparator] of String = ('', '<', '<=', '>=', '>');
  CODES_TFhirIdentifierUse : Array[TFhirIdentifierUse] of String = ('', 'usual', 'official', 'temp', 'secondary');
  CODES_TFhirEventTiming : Array[TFhirEventTiming] of String = ('', 'HS', 'WAKE', 'AC', 'ACM', 'ACD', 'ACV', 'PC', 'PCM', 'PCD', 'PCV');
  CODES_TFhirUnitsOfTime : Array[TFhirUnitsOfTime] of String = ('', 's', 'min', 'h', 'd', 'wk', 'mo', 'a');
  CODES_TFhirContactSystem : Array[TFhirContactSystem] of String = ('', 'phone', 'fax', 'email', 'url');
  CODES_TFhirContactUse : Array[TFhirContactUse] of String = ('', 'home', 'work', 'temp', 'old', 'mobile');
  CODES_TFhirAddressUse : Array[TFhirAddressUse] of String = ('', 'home', 'work', 'temp', 'old');
  CODES_TFhirNameUse : Array[TFhirNameUse] of String = ('', 'usual', 'official', 'temp', 'nickname', 'anonymous', 'old', 'maiden');
  CODES_TFhirReactionSeverity : Array[TFhirReactionSeverity] of String = ('', 'severe', 'serious', 'moderate', 'minor');
  CODES_TFhirExposureType : Array[TFhirExposureType] of String = ('', 'drugadmin', 'immuniz', 'coincidental');
  CODES_TFhirCausalityExpectation : Array[TFhirCausalityExpectation] of String = ('', 'likely', 'unlikely', 'confirmed', 'unknown');
  CODES_TFhirAlertStatus : Array[TFhirAlertStatus] of String = ('', 'active', 'inactive', 'entered in error');
  CODES_TFhirCriticality : Array[TFhirCriticality] of String = ('', 'fatal', 'high', 'medium', 'low');
  CODES_TFhirSensitivitytype : Array[TFhirSensitivitytype] of String = ('', 'allergy', 'intolerance', 'unknown');
  CODES_TFhirSensitivitystatus : Array[TFhirSensitivitystatus] of String = ('', 'suspected', 'confirmed', 'refuted', 'resolved');
  CODES_TFhirCarePlanStatus : Array[TFhirCarePlanStatus] of String = ('', 'planned', 'active', 'completed');
  CODES_TFhirCarePlanGoalStatus : Array[TFhirCarePlanGoalStatus] of String = ('', 'in progress', 'achieved', 'sustaining', 'cancelled');
  CODES_TFhirCarePlanActivityStatus : Array[TFhirCarePlanActivityStatus] of String = ('', 'not started', 'scheduled', 'in progress', 'on hold', 'completed', 'cancelled');
  CODES_TFhirCarePlanActivityCategory : Array[TFhirCarePlanActivityCategory] of String = ('', 'diet', 'drug', 'encounter', 'observation', 'procedure', 'supply', 'other');
  CODES_TFhirCompositionStatus : Array[TFhirCompositionStatus] of String = ('', 'preliminary', 'final', 'appended', 'amended', 'entered in error');
  CODES_TFhirCompositionAttestationMode : Array[TFhirCompositionAttestationMode] of String = ('', 'personal', 'professional', 'legal', 'official');
  CODES_TFhirValuesetStatus : Array[TFhirValuesetStatus] of String = ('', 'draft', 'active', 'retired');
  CODES_TFhirConceptEquivalence : Array[TFhirConceptEquivalence] of String = ('', 'equal', 'equivalent', 'wider', 'subsumes', 'narrower', 'specialises', 'inexact', 'unmatched', 'disjoint');
  CODES_TFhirConditionStatus : Array[TFhirConditionStatus] of String = ('', 'provisional', 'working', 'confirmed', 'refuted');
  CODES_TFhirConditionRelationshipType : Array[TFhirConditionRelationshipType] of String = ('', 'due-to', 'following');
  CODES_TFhirConformanceStatementStatus : Array[TFhirConformanceStatementStatus] of String = ('', 'draft', 'active', 'retired');
  CODES_TFhirRestfulConformanceMode : Array[TFhirRestfulConformanceMode] of String = ('', 'client', 'server');
  CODES_TFhirTypeRestfulOperation : Array[TFhirTypeRestfulOperation] of String = ('', 'read', 'vread', 'update', 'delete', 'history-instance', 'validate', 'history-type', 'create', 'search-type');
  CODES_TFhirSearchParamType : Array[TFhirSearchParamType] of String = ('', 'number', 'date', 'string', 'token', 'reference', 'composite', 'quantity');
  CODES_TFhirSystemRestfulOperation : Array[TFhirSystemRestfulOperation] of String = ('', 'transaction', 'search-system', 'history-system');
  CODES_TFhirMessageSignificanceCategory : Array[TFhirMessageSignificanceCategory] of String = ('', 'Consequence', 'Currency', 'Notification');
  CODES_TFhirMessageConformanceEventMode : Array[TFhirMessageConformanceEventMode] of String = ('', 'sender', 'receiver');
  CODES_TFhirDocumentMode : Array[TFhirDocumentMode] of String = ('', 'producer', 'consumer');
  CODES_TFhirDiagnosticOrderStatus : Array[TFhirDiagnosticOrderStatus] of String = ('', 'requested', 'received', 'accepted', 'in progress', 'review', 'completed', 'suspended', 'rejected', 'failed');
  CODES_TFhirDiagnosticOrderPriority : Array[TFhirDiagnosticOrderPriority] of String = ('', 'routine', 'urgent', 'stat', 'asap');
  CODES_TFhirDiagnosticReportStatus : Array[TFhirDiagnosticReportStatus] of String = ('', 'registered', 'partial', 'final', 'corrected', 'amended', 'appended', 'cancelled', 'entered in error');
  CODES_TFhirDocumentReferenceStatus : Array[TFhirDocumentReferenceStatus] of String = ('', 'current', 'superceded', 'entered in error');
  CODES_TFhirDocumentRelationshipType : Array[TFhirDocumentRelationshipType] of String = ('', 'replaces', 'transforms', 'signs', 'appends');
  CODES_TFhirEncounterState : Array[TFhirEncounterState] of String = ('', 'planned', 'in progress', 'onleave', 'finished', 'cancelled');
  CODES_TFhirEncounterClass : Array[TFhirEncounterClass] of String = ('', 'inpatient', 'outpatient', 'ambulatory', 'emergency', 'home', 'field', 'daytime', 'virtual');
  CODES_TFhirGroupType : Array[TFhirGroupType] of String = ('', 'person', 'animal', 'practitioner', 'device', 'medication', 'substance');
  CODES_TFhirImagingModality : Array[TFhirImagingModality] of String = ('', 'AR', 'BMD', 'BDUS', 'EPS', 'CR', 'CT', 'DX', 'ECG', 'ES', 'XC', 'GM', 'HD', 'IO', 'IVOCT', 'IVUS', 'KER', 'LEN', 'MR', 'MG', 'NM', 'OAM', 'OCT', 'OPM', 'OP', 'OPR', 'OPT', 'OPV', 'PX', 'PT', 'RF', 'RG', 'SM', 'SRF', 'US', 'VA', 'XA');
  CODES_TFhirInstanceAvailability : Array[TFhirInstanceAvailability] of String = ('', 'ONLINE', 'OFFLINE', 'NEARLINE', 'UNAVAILABLE');
  CODES_TFhirModality : Array[TFhirModality] of String = ('', 'AR', 'AU', 'BDUS', 'BI', 'BMD', 'CR', 'CT', 'DG', 'DX', 'ECG', 'EPS', 'ES', 'GM', 'HC', 'HD', 'IO', 'IVOCT', 'IVUS', 'KER', 'KO', 'LEN', 'LS', 'MG', 'MR', 'NM', 'OAM', 'OCT', 'OP', 'OPM', 'OPT', 'OPV', 'OT', 'PR', 'PT', 'PX', 'REG', 'RF', 'RG', 'RTDOSE', 'RTIMAGE', 'RTPLAN', 'RTRECORD', 'RTSTRUCT', 'SEG', 'SM', 'SMR', 'SR', 'SRF', 'TG', 'US', 'VA', 'XA', 'XC');
  CODES_TFhirListMode : Array[TFhirListMode] of String = ('', 'working', 'snapshot', 'changes');
  CODES_TFhirLocationStatus : Array[TFhirLocationStatus] of String = ('', 'active', 'suspended', 'inactive');
  CODES_TFhirLocationMode : Array[TFhirLocationMode] of String = ('', 'instance', 'kind');
  CODES_TFhirMediaType : Array[TFhirMediaType] of String = ('', 'photo', 'video', 'audio');
  CODES_TFhirMedicationKind : Array[TFhirMedicationKind] of String = ('', 'product', 'package');
  CODES_TFhirMedicationAdminStatus : Array[TFhirMedicationAdminStatus] of String = ('', 'in progress', 'on hold', 'completed', 'entered in error', 'stopped');
  CODES_TFhirMedicationDispenseStatus : Array[TFhirMedicationDispenseStatus] of String = ('', 'in progress', 'on hold', 'completed', 'entered in error', 'stopped');
  CODES_TFhirMedicationPrescriptionStatus : Array[TFhirMedicationPrescriptionStatus] of String = ('', 'active', 'on hold', 'completed', 'entered in error', 'stopped', 'superceded');
  CODES_TFhirResponseCode : Array[TFhirResponseCode] of String = ('', 'ok', 'transient-error', 'fatal-error');
  CODES_TFhirObservationStatus : Array[TFhirObservationStatus] of String = ('', 'registered', 'preliminary', 'final', 'amended', 'cancelled', 'entered in error');
  CODES_TFhirObservationReliability : Array[TFhirObservationReliability] of String = ('', 'ok', 'ongoing', 'early', 'questionable', 'calibrating', 'error', 'unknown');
  CODES_TFhirObservationRelationshiptypes : Array[TFhirObservationRelationshiptypes] of String = ('', 'has-component', 'has-member', 'derived-from', 'sequel-to', 'replaces', 'qualified-by', 'interfered-by');
  CODES_TFhirIssueSeverity : Array[TFhirIssueSeverity] of String = ('', 'fatal', 'error', 'warning', 'information');
  CODES_TFhirOrderOutcomeCode : Array[TFhirOrderOutcomeCode] of String = ('', 'pending', 'review', 'rejected', 'error', 'accepted', 'cancelled', 'replaced', 'aborted', 'complete');
  CODES_TFhirLinkType : Array[TFhirLinkType] of String = ('', 'replace', 'refer', 'seealso');
  CODES_TFhirProcedureRelationshipType : Array[TFhirProcedureRelationshipType] of String = ('', 'caused-by', 'because-of');
  CODES_TFhirResourceProfileStatus : Array[TFhirResourceProfileStatus] of String = ('', 'draft', 'active', 'retired');
  CODES_TFhirPropertyRepresentation : Array[TFhirPropertyRepresentation] of String = ('', 'xmlAttr');
  CODES_TFhirResourceSlicingRules : Array[TFhirResourceSlicingRules] of String = ('', 'closed', 'open', 'openAtEnd');
  CODES_TFhirResourceAggregationMode : Array[TFhirResourceAggregationMode] of String = ('', 'contained', 'referenced', 'bundled');
  CODES_TFhirConstraintSeverity : Array[TFhirConstraintSeverity] of String = ('', 'error', 'warning');
  CODES_TFhirBindingConformance : Array[TFhirBindingConformance] of String = ('', 'required', 'preferred', 'example');
  CODES_TFhirExtensionContext : Array[TFhirExtensionContext] of String = ('', 'resource', 'datatype', 'mapping', 'extension');
  CODES_TFhirProvenanceEntityRole : Array[TFhirProvenanceEntityRole] of String = ('', 'derivation', 'revision', 'quotation', 'source');
  CODES_TFhirQueryOutcome : Array[TFhirQueryOutcome] of String = ('', 'ok', 'limited', 'refused', 'error');
  CODES_TFhirQuestionnaireStatus : Array[TFhirQuestionnaireStatus] of String = ('', 'draft', 'published', 'retired', 'in progress', 'completed', 'amended');
  CODES_TFhirSecurityEventAction : Array[TFhirSecurityEventAction] of String = ('', 'C', 'R', 'U', 'D', 'E');
  CODES_TFhirSecurityEventOutcome : Array[TFhirSecurityEventOutcome] of String = ('', '0', '4', '8', '12');
  CODES_TFhirNetworkType : Array[TFhirNetworkType] of String = ('', '1', '2', '3', '4', '5');
  CODES_TFhirObjectType : Array[TFhirObjectType] of String = ('', '1', '2', '3', '4');
  CODES_TFhirObjectRole : Array[TFhirObjectRole] of String = ('', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24');
  CODES_TFhirObjectLifecycle : Array[TFhirObjectLifecycle] of String = ('', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15');
  CODES_TFhirHierarchicalRelationshipType : Array[TFhirHierarchicalRelationshipType] of String = ('', 'parent', 'child');
  CODES_TFhirValuesetSupplyStatus : Array[TFhirValuesetSupplyStatus] of String = ('', 'requested', 'dispensed', 'received', 'failed', 'cancelled');
  CODES_TFhirValuesetSupplyDispenseStatus : Array[TFhirValuesetSupplyDispenseStatus] of String = ('', 'in progress', 'dispensed', 'abandoned');
  CODES_TFhirFilterOperator : Array[TFhirFilterOperator] of String = ('', '=', 'is-a', 'is-not-a', 'regex', 'in', 'not in');

Function TFhirNarrativeStatusListAsInteger(aSet : TFhirNarrativeStatusList) : Integer; overload;
Function IntegerAsTFhirNarrativeStatusList(i : integer) : TFhirNarrativeStatusList; overload;
Function TFhirQuantityComparatorListAsInteger(aSet : TFhirQuantityComparatorList) : Integer; overload;
Function IntegerAsTFhirQuantityComparatorList(i : integer) : TFhirQuantityComparatorList; overload;
Function TFhirIdentifierUseListAsInteger(aSet : TFhirIdentifierUseList) : Integer; overload;
Function IntegerAsTFhirIdentifierUseList(i : integer) : TFhirIdentifierUseList; overload;
Function TFhirEventTimingListAsInteger(aSet : TFhirEventTimingList) : Integer; overload;
Function IntegerAsTFhirEventTimingList(i : integer) : TFhirEventTimingList; overload;
Function TFhirUnitsOfTimeListAsInteger(aSet : TFhirUnitsOfTimeList) : Integer; overload;
Function IntegerAsTFhirUnitsOfTimeList(i : integer) : TFhirUnitsOfTimeList; overload;
Function TFhirContactSystemListAsInteger(aSet : TFhirContactSystemList) : Integer; overload;
Function IntegerAsTFhirContactSystemList(i : integer) : TFhirContactSystemList; overload;
Function TFhirContactUseListAsInteger(aSet : TFhirContactUseList) : Integer; overload;
Function IntegerAsTFhirContactUseList(i : integer) : TFhirContactUseList; overload;
Function TFhirAddressUseListAsInteger(aSet : TFhirAddressUseList) : Integer; overload;
Function IntegerAsTFhirAddressUseList(i : integer) : TFhirAddressUseList; overload;
Function TFhirNameUseListAsInteger(aSet : TFhirNameUseList) : Integer; overload;
Function IntegerAsTFhirNameUseList(i : integer) : TFhirNameUseList; overload;
Function TFhirReactionSeverityListAsInteger(aSet : TFhirReactionSeverityList) : Integer; overload;
Function IntegerAsTFhirReactionSeverityList(i : integer) : TFhirReactionSeverityList; overload;
Function TFhirExposureTypeListAsInteger(aSet : TFhirExposureTypeList) : Integer; overload;
Function IntegerAsTFhirExposureTypeList(i : integer) : TFhirExposureTypeList; overload;
Function TFhirCausalityExpectationListAsInteger(aSet : TFhirCausalityExpectationList) : Integer; overload;
Function IntegerAsTFhirCausalityExpectationList(i : integer) : TFhirCausalityExpectationList; overload;
Function TFhirAlertStatusListAsInteger(aSet : TFhirAlertStatusList) : Integer; overload;
Function IntegerAsTFhirAlertStatusList(i : integer) : TFhirAlertStatusList; overload;
Function TFhirCriticalityListAsInteger(aSet : TFhirCriticalityList) : Integer; overload;
Function IntegerAsTFhirCriticalityList(i : integer) : TFhirCriticalityList; overload;
Function TFhirSensitivitytypeListAsInteger(aSet : TFhirSensitivitytypeList) : Integer; overload;
Function IntegerAsTFhirSensitivitytypeList(i : integer) : TFhirSensitivitytypeList; overload;
Function TFhirSensitivitystatusListAsInteger(aSet : TFhirSensitivitystatusList) : Integer; overload;
Function IntegerAsTFhirSensitivitystatusList(i : integer) : TFhirSensitivitystatusList; overload;
Function TFhirCarePlanStatusListAsInteger(aSet : TFhirCarePlanStatusList) : Integer; overload;
Function IntegerAsTFhirCarePlanStatusList(i : integer) : TFhirCarePlanStatusList; overload;
Function TFhirCarePlanGoalStatusListAsInteger(aSet : TFhirCarePlanGoalStatusList) : Integer; overload;
Function IntegerAsTFhirCarePlanGoalStatusList(i : integer) : TFhirCarePlanGoalStatusList; overload;
Function TFhirCarePlanActivityStatusListAsInteger(aSet : TFhirCarePlanActivityStatusList) : Integer; overload;
Function IntegerAsTFhirCarePlanActivityStatusList(i : integer) : TFhirCarePlanActivityStatusList; overload;
Function TFhirCarePlanActivityCategoryListAsInteger(aSet : TFhirCarePlanActivityCategoryList) : Integer; overload;
Function IntegerAsTFhirCarePlanActivityCategoryList(i : integer) : TFhirCarePlanActivityCategoryList; overload;
Function TFhirCompositionStatusListAsInteger(aSet : TFhirCompositionStatusList) : Integer; overload;
Function IntegerAsTFhirCompositionStatusList(i : integer) : TFhirCompositionStatusList; overload;
Function TFhirCompositionAttestationModeListAsInteger(aSet : TFhirCompositionAttestationModeList) : Integer; overload;
Function IntegerAsTFhirCompositionAttestationModeList(i : integer) : TFhirCompositionAttestationModeList; overload;
Function TFhirValuesetStatusListAsInteger(aSet : TFhirValuesetStatusList) : Integer; overload;
Function IntegerAsTFhirValuesetStatusList(i : integer) : TFhirValuesetStatusList; overload;
Function TFhirConceptEquivalenceListAsInteger(aSet : TFhirConceptEquivalenceList) : Integer; overload;
Function IntegerAsTFhirConceptEquivalenceList(i : integer) : TFhirConceptEquivalenceList; overload;
Function TFhirConditionStatusListAsInteger(aSet : TFhirConditionStatusList) : Integer; overload;
Function IntegerAsTFhirConditionStatusList(i : integer) : TFhirConditionStatusList; overload;
Function TFhirConditionRelationshipTypeListAsInteger(aSet : TFhirConditionRelationshipTypeList) : Integer; overload;
Function IntegerAsTFhirConditionRelationshipTypeList(i : integer) : TFhirConditionRelationshipTypeList; overload;
Function TFhirConformanceStatementStatusListAsInteger(aSet : TFhirConformanceStatementStatusList) : Integer; overload;
Function IntegerAsTFhirConformanceStatementStatusList(i : integer) : TFhirConformanceStatementStatusList; overload;
Function TFhirRestfulConformanceModeListAsInteger(aSet : TFhirRestfulConformanceModeList) : Integer; overload;
Function IntegerAsTFhirRestfulConformanceModeList(i : integer) : TFhirRestfulConformanceModeList; overload;
Function TFhirTypeRestfulOperationListAsInteger(aSet : TFhirTypeRestfulOperationList) : Integer; overload;
Function IntegerAsTFhirTypeRestfulOperationList(i : integer) : TFhirTypeRestfulOperationList; overload;
Function TFhirSearchParamTypeListAsInteger(aSet : TFhirSearchParamTypeList) : Integer; overload;
Function IntegerAsTFhirSearchParamTypeList(i : integer) : TFhirSearchParamTypeList; overload;
Function TFhirSystemRestfulOperationListAsInteger(aSet : TFhirSystemRestfulOperationList) : Integer; overload;
Function IntegerAsTFhirSystemRestfulOperationList(i : integer) : TFhirSystemRestfulOperationList; overload;
Function TFhirMessageSignificanceCategoryListAsInteger(aSet : TFhirMessageSignificanceCategoryList) : Integer; overload;
Function IntegerAsTFhirMessageSignificanceCategoryList(i : integer) : TFhirMessageSignificanceCategoryList; overload;
Function TFhirMessageConformanceEventModeListAsInteger(aSet : TFhirMessageConformanceEventModeList) : Integer; overload;
Function IntegerAsTFhirMessageConformanceEventModeList(i : integer) : TFhirMessageConformanceEventModeList; overload;
Function TFhirDocumentModeListAsInteger(aSet : TFhirDocumentModeList) : Integer; overload;
Function IntegerAsTFhirDocumentModeList(i : integer) : TFhirDocumentModeList; overload;
Function TFhirDiagnosticOrderStatusListAsInteger(aSet : TFhirDiagnosticOrderStatusList) : Integer; overload;
Function IntegerAsTFhirDiagnosticOrderStatusList(i : integer) : TFhirDiagnosticOrderStatusList; overload;
Function TFhirDiagnosticOrderPriorityListAsInteger(aSet : TFhirDiagnosticOrderPriorityList) : Integer; overload;
Function IntegerAsTFhirDiagnosticOrderPriorityList(i : integer) : TFhirDiagnosticOrderPriorityList; overload;
Function TFhirDiagnosticReportStatusListAsInteger(aSet : TFhirDiagnosticReportStatusList) : Integer; overload;
Function IntegerAsTFhirDiagnosticReportStatusList(i : integer) : TFhirDiagnosticReportStatusList; overload;
Function TFhirDocumentReferenceStatusListAsInteger(aSet : TFhirDocumentReferenceStatusList) : Integer; overload;
Function IntegerAsTFhirDocumentReferenceStatusList(i : integer) : TFhirDocumentReferenceStatusList; overload;
Function TFhirDocumentRelationshipTypeListAsInteger(aSet : TFhirDocumentRelationshipTypeList) : Integer; overload;
Function IntegerAsTFhirDocumentRelationshipTypeList(i : integer) : TFhirDocumentRelationshipTypeList; overload;
Function TFhirEncounterStateListAsInteger(aSet : TFhirEncounterStateList) : Integer; overload;
Function IntegerAsTFhirEncounterStateList(i : integer) : TFhirEncounterStateList; overload;
Function TFhirEncounterClassListAsInteger(aSet : TFhirEncounterClassList) : Integer; overload;
Function IntegerAsTFhirEncounterClassList(i : integer) : TFhirEncounterClassList; overload;
Function TFhirGroupTypeListAsInteger(aSet : TFhirGroupTypeList) : Integer; overload;
Function IntegerAsTFhirGroupTypeList(i : integer) : TFhirGroupTypeList; overload;
Function TFhirImagingModalityListAsInteger(aSet : TFhirImagingModalityList) : Integer; overload;
Function IntegerAsTFhirImagingModalityList(i : integer) : TFhirImagingModalityList; overload;
Function TFhirInstanceAvailabilityListAsInteger(aSet : TFhirInstanceAvailabilityList) : Integer; overload;
Function IntegerAsTFhirInstanceAvailabilityList(i : integer) : TFhirInstanceAvailabilityList; overload;
Function TFhirModalityListAsInteger(aSet : TFhirModalityList) : Integer; overload;
Function IntegerAsTFhirModalityList(i : integer) : TFhirModalityList; overload;
Function TFhirListModeListAsInteger(aSet : TFhirListModeList) : Integer; overload;
Function IntegerAsTFhirListModeList(i : integer) : TFhirListModeList; overload;
Function TFhirLocationStatusListAsInteger(aSet : TFhirLocationStatusList) : Integer; overload;
Function IntegerAsTFhirLocationStatusList(i : integer) : TFhirLocationStatusList; overload;
Function TFhirLocationModeListAsInteger(aSet : TFhirLocationModeList) : Integer; overload;
Function IntegerAsTFhirLocationModeList(i : integer) : TFhirLocationModeList; overload;
Function TFhirMediaTypeListAsInteger(aSet : TFhirMediaTypeList) : Integer; overload;
Function IntegerAsTFhirMediaTypeList(i : integer) : TFhirMediaTypeList; overload;
Function TFhirMedicationKindListAsInteger(aSet : TFhirMedicationKindList) : Integer; overload;
Function IntegerAsTFhirMedicationKindList(i : integer) : TFhirMedicationKindList; overload;
Function TFhirMedicationAdminStatusListAsInteger(aSet : TFhirMedicationAdminStatusList) : Integer; overload;
Function IntegerAsTFhirMedicationAdminStatusList(i : integer) : TFhirMedicationAdminStatusList; overload;
Function TFhirMedicationDispenseStatusListAsInteger(aSet : TFhirMedicationDispenseStatusList) : Integer; overload;
Function IntegerAsTFhirMedicationDispenseStatusList(i : integer) : TFhirMedicationDispenseStatusList; overload;
Function TFhirMedicationPrescriptionStatusListAsInteger(aSet : TFhirMedicationPrescriptionStatusList) : Integer; overload;
Function IntegerAsTFhirMedicationPrescriptionStatusList(i : integer) : TFhirMedicationPrescriptionStatusList; overload;
Function TFhirResponseCodeListAsInteger(aSet : TFhirResponseCodeList) : Integer; overload;
Function IntegerAsTFhirResponseCodeList(i : integer) : TFhirResponseCodeList; overload;
Function TFhirObservationStatusListAsInteger(aSet : TFhirObservationStatusList) : Integer; overload;
Function IntegerAsTFhirObservationStatusList(i : integer) : TFhirObservationStatusList; overload;
Function TFhirObservationReliabilityListAsInteger(aSet : TFhirObservationReliabilityList) : Integer; overload;
Function IntegerAsTFhirObservationReliabilityList(i : integer) : TFhirObservationReliabilityList; overload;
Function TFhirObservationRelationshiptypesListAsInteger(aSet : TFhirObservationRelationshiptypesList) : Integer; overload;
Function IntegerAsTFhirObservationRelationshiptypesList(i : integer) : TFhirObservationRelationshiptypesList; overload;
Function TFhirIssueSeverityListAsInteger(aSet : TFhirIssueSeverityList) : Integer; overload;
Function IntegerAsTFhirIssueSeverityList(i : integer) : TFhirIssueSeverityList; overload;
Function TFhirOrderOutcomeCodeListAsInteger(aSet : TFhirOrderOutcomeCodeList) : Integer; overload;
Function IntegerAsTFhirOrderOutcomeCodeList(i : integer) : TFhirOrderOutcomeCodeList; overload;
Function TFhirLinkTypeListAsInteger(aSet : TFhirLinkTypeList) : Integer; overload;
Function IntegerAsTFhirLinkTypeList(i : integer) : TFhirLinkTypeList; overload;
Function TFhirProcedureRelationshipTypeListAsInteger(aSet : TFhirProcedureRelationshipTypeList) : Integer; overload;
Function IntegerAsTFhirProcedureRelationshipTypeList(i : integer) : TFhirProcedureRelationshipTypeList; overload;
Function TFhirResourceProfileStatusListAsInteger(aSet : TFhirResourceProfileStatusList) : Integer; overload;
Function IntegerAsTFhirResourceProfileStatusList(i : integer) : TFhirResourceProfileStatusList; overload;
Function TFhirPropertyRepresentationListAsInteger(aSet : TFhirPropertyRepresentationList) : Integer; overload;
Function IntegerAsTFhirPropertyRepresentationList(i : integer) : TFhirPropertyRepresentationList; overload;
Function TFhirResourceSlicingRulesListAsInteger(aSet : TFhirResourceSlicingRulesList) : Integer; overload;
Function IntegerAsTFhirResourceSlicingRulesList(i : integer) : TFhirResourceSlicingRulesList; overload;
Function TFhirResourceAggregationModeListAsInteger(aSet : TFhirResourceAggregationModeList) : Integer; overload;
Function IntegerAsTFhirResourceAggregationModeList(i : integer) : TFhirResourceAggregationModeList; overload;
Function TFhirConstraintSeverityListAsInteger(aSet : TFhirConstraintSeverityList) : Integer; overload;
Function IntegerAsTFhirConstraintSeverityList(i : integer) : TFhirConstraintSeverityList; overload;
Function TFhirBindingConformanceListAsInteger(aSet : TFhirBindingConformanceList) : Integer; overload;
Function IntegerAsTFhirBindingConformanceList(i : integer) : TFhirBindingConformanceList; overload;
Function TFhirExtensionContextListAsInteger(aSet : TFhirExtensionContextList) : Integer; overload;
Function IntegerAsTFhirExtensionContextList(i : integer) : TFhirExtensionContextList; overload;
Function TFhirProvenanceEntityRoleListAsInteger(aSet : TFhirProvenanceEntityRoleList) : Integer; overload;
Function IntegerAsTFhirProvenanceEntityRoleList(i : integer) : TFhirProvenanceEntityRoleList; overload;
Function TFhirQueryOutcomeListAsInteger(aSet : TFhirQueryOutcomeList) : Integer; overload;
Function IntegerAsTFhirQueryOutcomeList(i : integer) : TFhirQueryOutcomeList; overload;
Function TFhirQuestionnaireStatusListAsInteger(aSet : TFhirQuestionnaireStatusList) : Integer; overload;
Function IntegerAsTFhirQuestionnaireStatusList(i : integer) : TFhirQuestionnaireStatusList; overload;
Function TFhirSecurityEventActionListAsInteger(aSet : TFhirSecurityEventActionList) : Integer; overload;
Function IntegerAsTFhirSecurityEventActionList(i : integer) : TFhirSecurityEventActionList; overload;
Function TFhirSecurityEventOutcomeListAsInteger(aSet : TFhirSecurityEventOutcomeList) : Integer; overload;
Function IntegerAsTFhirSecurityEventOutcomeList(i : integer) : TFhirSecurityEventOutcomeList; overload;
Function TFhirNetworkTypeListAsInteger(aSet : TFhirNetworkTypeList) : Integer; overload;
Function IntegerAsTFhirNetworkTypeList(i : integer) : TFhirNetworkTypeList; overload;
Function TFhirObjectTypeListAsInteger(aSet : TFhirObjectTypeList) : Integer; overload;
Function IntegerAsTFhirObjectTypeList(i : integer) : TFhirObjectTypeList; overload;
Function TFhirObjectRoleListAsInteger(aSet : TFhirObjectRoleList) : Integer; overload;
Function IntegerAsTFhirObjectRoleList(i : integer) : TFhirObjectRoleList; overload;
Function TFhirObjectLifecycleListAsInteger(aSet : TFhirObjectLifecycleList) : Integer; overload;
Function IntegerAsTFhirObjectLifecycleList(i : integer) : TFhirObjectLifecycleList; overload;
Function TFhirHierarchicalRelationshipTypeListAsInteger(aSet : TFhirHierarchicalRelationshipTypeList) : Integer; overload;
Function IntegerAsTFhirHierarchicalRelationshipTypeList(i : integer) : TFhirHierarchicalRelationshipTypeList; overload;
Function TFhirValuesetSupplyStatusListAsInteger(aSet : TFhirValuesetSupplyStatusList) : Integer; overload;
Function IntegerAsTFhirValuesetSupplyStatusList(i : integer) : TFhirValuesetSupplyStatusList; overload;
Function TFhirValuesetSupplyDispenseStatusListAsInteger(aSet : TFhirValuesetSupplyDispenseStatusList) : Integer; overload;
Function IntegerAsTFhirValuesetSupplyDispenseStatusList(i : integer) : TFhirValuesetSupplyDispenseStatusList; overload;
Function TFhirFilterOperatorListAsInteger(aSet : TFhirFilterOperatorList) : Integer; overload;
Function IntegerAsTFhirFilterOperatorList(i : integer) : TFhirFilterOperatorList; overload;

implementation

{ TFhirElement }

destructor TFhirElement.Destroy;
begin
  FExtensionList.Free;
  inherited;
end;

procedure TFhirElement.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = '@id' then
    list.add(TFHIRObjectText.create(FXmlId));
  if (child_name = 'extension') Then
     list.addAll(FExtensionList);
end;

procedure TFhirElement.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'xml:id', 'string', FXmlId));
  oList.add(TFHIRProperty.create(self, 'extension', 'Extension', FExtensionList));
end;

procedure TFhirElement.Assign(oSource : TAdvObject);
begin
  inherited;
  FXmlId := TFhirElement(oSource).FXmlId;
  if TFhirElement(oSource).HasExtensions then
    extensionList.assign(TFhirElement(oSource).extensionList)
  else if FExtensionList <> nil then
  begin
    FExtensionList.free;
    FExtensionList := nil;
  end;
end;

function TFhirElement.Link : TFhirElement;
begin
  result := TFhirElement(inherited Link);
end;

function TFhirElement.Clone : TFhirElement;
begin
  result := TFhirElement(inherited Clone);
end;

function TFhirElement.GetExtensionList : TFhirExtensionList;
begin
  if FExtensionList = nil then
    FExtensionList := TFhirExtensionList.Create;
  result := FExtensionList;
end;

function TFhirElement.HasExtensions : boolean;
begin
  result := (FExtensionList <> nil) and (FExtensionList.count > 0);
end;

{ TFHIRBackboneElement }

destructor TFHIRBackboneElement.Destroy;
begin
  FModifierExtensionList.Free;
  inherited;
end;

procedure TFHIRBackboneElement.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'modifierExtension') Then
     list.addAll(FModifierExtensionList);
end;

procedure TFHIRBackboneElement.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'modifierExtension', 'Extension', FModifierExtensionList));
end;

procedure TFHIRBackboneElement.Assign(oSource : TAdvObject);
begin
  inherited;
  if TFHIRBackboneElement(oSource).HasModifierExtensions then
    ModifierExtensionList.assign(TFHIRBackboneElement(oSource).ModifierextensionList)
  else if FModifierExtensionList <> nil then
  begin
    FModifierExtensionList.free;
    FModifierExtensionList := nil;
  end;
end;

function TFHIRBackboneElement.Link : TFHIRBackboneElement;
begin
  result := TFHIRBackboneElement(inherited Link);
end;

function TFHIRBackboneElement.Clone : TFHIRBackboneElement;
begin
  result := TFHIRBackboneElement(inherited Clone);
end;

function TFHIRBackboneElement.GetModifierExtensionList : TFhirExtensionList;
begin
  if FModifierExtensionList = nil then
    FModifierExtensionList := TFhirExtensionList.Create;
  result := FModifierExtensionList;
end;

function TFHIRBackboneElement.HasModifierExtensions : boolean;
begin
  result := (FModifierExtensionList <> nil) and (FModifierExtensionList.count > 0);
end;

{ TFhirType }

function TFhirType.Link : TFhirType;
begin
  result := TFhirType(inherited Link);
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

function TFHIRPrimitiveType.AsStringValue : string;
begin
  raise Exception.create('need to override '+ClassName+'.AsStringValue');
end;


{ TFhirElementListEnumerator }

Constructor TFhirElementListEnumerator.Create(list : TFhirElementList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirElementListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirElementListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirElementListEnumerator.GetCurrent : TFhirElement;
begin
  Result := FList[FIndex];
end;


{ TFhirElementList }
procedure TFhirElementList.AddItem(value: TFhirElement);
begin
  assert(value.ClassName = 'TFhirElement', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirElement');
  add(value);
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

function TFhirElementList.IndexOf(value: TFhirElement): Integer;
begin
  result := IndexByReference(value);
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

{ TFhirEnum }

Constructor TFhirEnum.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirEnum.Destroy;
begin
  inherited;
end;

function TFhirEnum.FhirType : string;
begin
  result := 'enum';
end;

procedure TFhirEnum.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirEnum.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'enum', FValue));
end;

procedure TFhirEnum.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirEnum(oSource).Value;
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


{ TFhirEnumListEnumerator }

Constructor TFhirEnumListEnumerator.Create(list : TFhirEnumList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirEnumListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEnumListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirEnumListEnumerator.GetCurrent : TFhirEnum;
begin
  Result := FList[FIndex];
end;


{ TFhirEnumList }
procedure TFhirEnumList.AddItem(value: TFhirEnum);
begin
  assert(value.ClassName = 'TFhirEnum', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEnum');
  add(value);
end;


procedure TFhirEnumList.AddItem(value: String);
begin
  add(TFhirEnum.create(value));
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

function TFhirEnumList.IndexOf(value: TFhirEnum): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirEnumList.Insert(index: Integer): TFhirEnum;
begin
  result := TFhirEnum.create;
  try
    inherited insert(index, result);
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

{ TFhirInteger }

Constructor TFhirInteger.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirInteger.Destroy;
begin
  inherited;
end;

function TFhirInteger.FhirType : string;
begin
  result := 'integer';
end;

procedure TFhirInteger.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirInteger.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'integer', FValue));
end;

procedure TFhirInteger.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirInteger(oSource).Value;
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


{ TFhirIntegerListEnumerator }

Constructor TFhirIntegerListEnumerator.Create(list : TFhirIntegerList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirIntegerListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirIntegerListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirIntegerListEnumerator.GetCurrent : TFhirInteger;
begin
  Result := FList[FIndex];
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

function TFhirIntegerList.IndexOf(value: TFhirInteger): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirIntegerList.Insert(index: Integer): TFhirInteger;
begin
  result := TFhirInteger.create;
  try
    inherited insert(index, result);
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

{ TFhirDateTime }

Constructor TFhirDateTime.Create(value : TDateTimeEx);
begin
  Create;
  FValue := value;
end;

Destructor TFhirDateTime.Destroy;
begin
  FValue.free;
  inherited;
end;

function TFhirDateTime.FhirType : string;
begin
  result := 'dateTime';
end;

procedure TFhirDateTime.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirDateTime.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'dateTime', FValue.toString));
end;

procedure TFhirDateTime.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirDateTime(oSource).Value.Link;
end;

function TFhirDateTime.Link : TFhirDateTime;
begin
  result := TFhirDateTime(inherited Link);
end;

function TFhirDateTime.Clone : TFhirDateTime;
begin
  result := TFhirDateTime(inherited Clone);
end;

procedure TFhirDateTime.setValue(value : TDateTimeEx);
begin
  FValue.free;
  FValue := value;
end;


{ TFhirDateTimeListEnumerator }

Constructor TFhirDateTimeListEnumerator.Create(list : TFhirDateTimeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirDateTimeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDateTimeListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirDateTimeListEnumerator.GetCurrent : TFhirDateTime;
begin
  Result := FList[FIndex];
end;


{ TFhirDateTimeList }
procedure TFhirDateTimeList.AddItem(value: TFhirDateTime);
begin
  assert(value.ClassName = 'TFhirDateTime', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDateTime');
  add(value);
end;


procedure TFhirDateTimeList.AddItem(value: TDateTimeEx);
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

function TFhirDateTimeList.IndexOf(value: TFhirDateTime): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirDateTimeList.Insert(index: Integer): TFhirDateTime;
begin
  result := TFhirDateTime.create;
  try
    inherited insert(index, result);
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

Constructor TFhirDate.Create(value : TDateTimeEx);
begin
  Create;
  FValue := value;
end;

Destructor TFhirDate.Destroy;
begin
  FValue.free;
  inherited;
end;

function TFhirDate.FhirType : string;
begin
  result := 'date';
end;

procedure TFhirDate.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirDate.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'date', FValue.toString));
end;

procedure TFhirDate.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirDate(oSource).Value.Link;
end;

function TFhirDate.Link : TFhirDate;
begin
  result := TFhirDate(inherited Link);
end;

function TFhirDate.Clone : TFhirDate;
begin
  result := TFhirDate(inherited Clone);
end;

procedure TFhirDate.setValue(value : TDateTimeEx);
begin
  FValue.free;
  FValue := value;
end;


{ TFhirDateListEnumerator }

Constructor TFhirDateListEnumerator.Create(list : TFhirDateList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirDateListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDateListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirDateListEnumerator.GetCurrent : TFhirDate;
begin
  Result := FList[FIndex];
end;


{ TFhirDateList }
procedure TFhirDateList.AddItem(value: TFhirDate);
begin
  assert(value.ClassName = 'TFhirDate', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDate');
  add(value);
end;


procedure TFhirDateList.AddItem(value: TDateTimeEx);
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

function TFhirDateList.IndexOf(value: TFhirDate): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirDateList.Insert(index: Integer): TFhirDate;
begin
  result := TFhirDate.create;
  try
    inherited insert(index, result);
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

{ TFhirDecimal }

Constructor TFhirDecimal.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirDecimal.Destroy;
begin
  inherited;
end;

function TFhirDecimal.FhirType : string;
begin
  result := 'decimal';
end;

procedure TFhirDecimal.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirDecimal.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'decimal', FValue));
end;

procedure TFhirDecimal.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirDecimal(oSource).Value;
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


{ TFhirDecimalListEnumerator }

Constructor TFhirDecimalListEnumerator.Create(list : TFhirDecimalList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirDecimalListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDecimalListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirDecimalListEnumerator.GetCurrent : TFhirDecimal;
begin
  Result := FList[FIndex];
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

function TFhirDecimalList.IndexOf(value: TFhirDecimal): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirDecimalList.Insert(index: Integer): TFhirDecimal;
begin
  result := TFhirDecimal.create;
  try
    inherited insert(index, result);
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

{ TFhirUri }

Constructor TFhirUri.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirUri.Destroy;
begin
  inherited;
end;

function TFhirUri.FhirType : string;
begin
  result := 'uri';
end;

procedure TFhirUri.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirUri.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'uri', FValue));
end;

procedure TFhirUri.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirUri(oSource).Value;
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


{ TFhirUriListEnumerator }

Constructor TFhirUriListEnumerator.Create(list : TFhirUriList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirUriListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirUriListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirUriListEnumerator.GetCurrent : TFhirUri;
begin
  Result := FList[FIndex];
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

function TFhirUriList.IndexOf(value: TFhirUri): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirUriList.Insert(index: Integer): TFhirUri;
begin
  result := TFhirUri.create;
  try
    inherited insert(index, result);
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

{ TFhirBase64Binary }

Constructor TFhirBase64Binary.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirBase64Binary.Destroy;
begin
  inherited;
end;

function TFhirBase64Binary.FhirType : string;
begin
  result := 'base64Binary';
end;

procedure TFhirBase64Binary.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirBase64Binary.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'base64Binary', FValue));
end;

procedure TFhirBase64Binary.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirBase64Binary(oSource).Value;
end;

function TFhirBase64Binary.Link : TFhirBase64Binary;
begin
  result := TFhirBase64Binary(inherited Link);
end;

function TFhirBase64Binary.Clone : TFhirBase64Binary;
begin
  result := TFhirBase64Binary(inherited Clone);
end;

procedure TFhirBase64Binary.setValue(value : String);
begin
  FValue := value;
end;


{ TFhirBase64BinaryListEnumerator }

Constructor TFhirBase64BinaryListEnumerator.Create(list : TFhirBase64BinaryList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirBase64BinaryListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirBase64BinaryListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirBase64BinaryListEnumerator.GetCurrent : TFhirBase64Binary;
begin
  Result := FList[FIndex];
end;


{ TFhirBase64BinaryList }
procedure TFhirBase64BinaryList.AddItem(value: TFhirBase64Binary);
begin
  assert(value.ClassName = 'TFhirBase64Binary', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirBase64Binary');
  add(value);
end;


procedure TFhirBase64BinaryList.AddItem(value: String);
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

function TFhirBase64BinaryList.IndexOf(value: TFhirBase64Binary): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirBase64BinaryList.Insert(index: Integer): TFhirBase64Binary;
begin
  result := TFhirBase64Binary.create;
  try
    inherited insert(index, result);
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

{ TFhirString }

Constructor TFhirString.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirString.Destroy;
begin
  inherited;
end;

function TFhirString.FhirType : string;
begin
  result := 'string';
end;

procedure TFhirString.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirString.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'string', FValue));
end;

procedure TFhirString.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirString(oSource).Value;
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


{ TFhirStringListEnumerator }

Constructor TFhirStringListEnumerator.Create(list : TFhirStringList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirStringListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirStringListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirStringListEnumerator.GetCurrent : TFhirString;
begin
  Result := FList[FIndex];
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

function TFhirStringList.IndexOf(value: TFhirString): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirStringList.Insert(index: Integer): TFhirString;
begin
  result := TFhirString.create;
  try
    inherited insert(index, result);
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

{ TFhirBoolean }

Constructor TFhirBoolean.Create(value : Boolean);
begin
  Create;
  FValue := value;
end;

Destructor TFhirBoolean.Destroy;
begin
  inherited;
end;

function TFhirBoolean.FhirType : string;
begin
  result := 'boolean';
end;

procedure TFhirBoolean.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirBoolean.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'boolean', LCBooleanToString(FValue)));
end;

procedure TFhirBoolean.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirBoolean(oSource).Value;
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


{ TFhirBooleanListEnumerator }

Constructor TFhirBooleanListEnumerator.Create(list : TFhirBooleanList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirBooleanListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirBooleanListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirBooleanListEnumerator.GetCurrent : TFhirBoolean;
begin
  Result := FList[FIndex];
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

function TFhirBooleanList.IndexOf(value: TFhirBoolean): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirBooleanList.Insert(index: Integer): TFhirBoolean;
begin
  result := TFhirBoolean.create;
  try
    inherited insert(index, result);
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

{ TFhirInstant }

Constructor TFhirInstant.Create(value : TDateTimeEx);
begin
  Create;
  FValue := value;
end;

Destructor TFhirInstant.Destroy;
begin
  FValue.free;
  inherited;
end;

function TFhirInstant.FhirType : string;
begin
  result := 'instant';
end;

procedure TFhirInstant.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirInstant.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'instant', FValue.toString));
end;

procedure TFhirInstant.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirInstant(oSource).Value.Link;
end;

function TFhirInstant.Link : TFhirInstant;
begin
  result := TFhirInstant(inherited Link);
end;

function TFhirInstant.Clone : TFhirInstant;
begin
  result := TFhirInstant(inherited Clone);
end;

procedure TFhirInstant.setValue(value : TDateTimeEx);
begin
  FValue.free;
  FValue := value;
end;


{ TFhirInstantListEnumerator }

Constructor TFhirInstantListEnumerator.Create(list : TFhirInstantList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirInstantListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirInstantListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirInstantListEnumerator.GetCurrent : TFhirInstant;
begin
  Result := FList[FIndex];
end;


{ TFhirInstantList }
procedure TFhirInstantList.AddItem(value: TFhirInstant);
begin
  assert(value.ClassName = 'TFhirInstant', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirInstant');
  add(value);
end;


procedure TFhirInstantList.AddItem(value: TDateTimeEx);
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

function TFhirInstantList.IndexOf(value: TFhirInstant): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirInstantList.Insert(index: Integer): TFhirInstant;
begin
  result := TFhirInstant.create;
  try
    inherited insert(index, result);
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

{ TFhirCode }

Constructor TFhirCode.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirCode.Destroy;
begin
  inherited;
end;

function TFhirCode.FhirType : string;
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


{ TFhirCodeListEnumerator }

Constructor TFhirCodeListEnumerator.Create(list : TFhirCodeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirCodeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirCodeListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirCodeListEnumerator.GetCurrent : TFhirCode;
begin
  Result := FList[FIndex];
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

function TFhirCodeList.IndexOf(value: TFhirCode): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirCodeList.Insert(index: Integer): TFhirCode;
begin
  result := TFhirCode.create;
  try
    inherited insert(index, result);
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

{ TFhirId }

Constructor TFhirId.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirId.Destroy;
begin
  inherited;
end;

function TFhirId.FhirType : string;
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


{ TFhirIdListEnumerator }

Constructor TFhirIdListEnumerator.Create(list : TFhirIdList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirIdListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirIdListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirIdListEnumerator.GetCurrent : TFhirId;
begin
  Result := FList[FIndex];
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

function TFhirIdList.IndexOf(value: TFhirId): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirIdList.Insert(index: Integer): TFhirId;
begin
  result := TFhirId.create;
  try
    inherited insert(index, result);
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

{ TFhirOid }

Constructor TFhirOid.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirOid.Destroy;
begin
  inherited;
end;

function TFhirOid.FhirType : string;
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


{ TFhirOidListEnumerator }

Constructor TFhirOidListEnumerator.Create(list : TFhirOidList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirOidListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirOidListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirOidListEnumerator.GetCurrent : TFhirOid;
begin
  Result := FList[FIndex];
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

function TFhirOidList.IndexOf(value: TFhirOid): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirOidList.Insert(index: Integer): TFhirOid;
begin
  result := TFhirOid.create;
  try
    inherited insert(index, result);
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

Destructor TFhirUuid.Destroy;
begin
  inherited;
end;

function TFhirUuid.FhirType : string;
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


{ TFhirUuidListEnumerator }

Constructor TFhirUuidListEnumerator.Create(list : TFhirUuidList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirUuidListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirUuidListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirUuidListEnumerator.GetCurrent : TFhirUuid;
begin
  Result := FList[FIndex];
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

function TFhirUuidList.IndexOf(value: TFhirUuid): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirUuidList.Insert(index: Integer): TFhirUuid;
begin
  result := TFhirUuid.create;
  try
    inherited insert(index, result);
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

procedure TFhirExtension.Assign(oSource : TAdvObject);
begin
  inherited;
  urlObject := TFhirExtension(oSource).urlObject.Clone;
  value := TFhirExtension(oSource).value.Clone;
end;

procedure TFhirExtension.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'url') Then
     list.add(FUrl.Link);
  if (child_name = 'value[x]') Then
     list.add(FValue.Link);
end;

procedure TFhirExtension.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'url', 'uri', FUrl.Link));{2}
  oList.add(TFHIRProperty.create(self, 'value[x]', '*', FValue.Link));{2}
end;

procedure TFhirExtension.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'url') then UrlObject := propValue as TFhirUri{5a}
  else if (propName.startsWith('value')) then Value := propValue as TFhirType{4}
  else inherited;
end;

function TFhirExtension.FhirType : string;
begin
  result := 'Extension';
end;

function TFhirExtension.Link : TFhirExtension;
begin
  result := TFhirExtension(inherited Link);
end;

function TFhirExtension.Clone : TFhirExtension;
begin
  result := TFhirExtension(inherited Clone);
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


{ TFhirExtensionListEnumerator }

Constructor TFhirExtensionListEnumerator.Create(list : TFhirExtensionList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirExtensionListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirExtensionListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirExtensionListEnumerator.GetCurrent : TFhirExtension;
begin
  Result := FList[FIndex];
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

function TFhirExtensionList.IndexOf(value: TFhirExtension): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirExtensionList.Insert(index: Integer): TFhirExtension;
begin
  result := TFhirExtension.create;
  try
    inherited insert(index, result);
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

function TFhirNarrativeStatusListAsInteger(aSet : TFhirNarrativeStatusList) : Integer;
var
  a : TFhirNarrativeStatus;
begin
  result := 0;
  for a := low(TFhirNarrativeStatus) to high(TFhirNarrativeStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNarrativeStatusList(i : Integer) : TFhirNarrativeStatusList;
var
  aLoop : TFhirNarrativeStatus;
begin
  result := [];
  for aLoop := low(TFhirNarrativeStatus) to high(TFhirNarrativeStatus) Do
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

procedure TFhirNarrative.Assign(oSource : TAdvObject);
begin
  inherited;
  FStatus := TFhirNarrative(oSource).FStatus.Link;
  div_ := TFhirNarrative(oSource).div_.Clone;
end;

procedure TFhirNarrative.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'div') Then
     list.add(FDiv_.Link);
end;

procedure TFhirNarrative.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'div', 'xhtml', FDiv_.Link));{2}
end;

procedure TFhirNarrative.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'div') then Div_ := propValue as TFhirXHtmlNode{4b}
  else inherited;
end;

function TFhirNarrative.FhirType : string;
begin
  result := 'Narrative';
end;

function TFhirNarrative.Link : TFhirNarrative;
begin
  result := TFhirNarrative(inherited Link);
end;

function TFhirNarrative.Clone : TFhirNarrative;
begin
  result := TFhirNarrative(inherited Clone);
end;

{ TFhirNarrative }

Procedure TFhirNarrative.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirNarrative.GetStatusST : TFhirNarrativeStatus;
begin
  if FStatus = nil then
    result := TFhirNarrativeStatus(0)
  else
    result := TFhirNarrativeStatus(StringArrayIndexOfSensitive(CODES_TFhirNarrativeStatus, FStatus.value));
end;

Procedure TFhirNarrative.SetStatusST(value : TFhirNarrativeStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirNarrativeStatus[value]);
end;

Procedure TFhirNarrative.SetDiv_(value : TFhirXHtmlNode);
begin
  FDiv_.free;
  FDiv_ := value;
end;


{ TFhirNarrativeListEnumerator }

Constructor TFhirNarrativeListEnumerator.Create(list : TFhirNarrativeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirNarrativeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirNarrativeListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirNarrativeListEnumerator.GetCurrent : TFhirNarrative;
begin
  Result := FList[FIndex];
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

function TFhirNarrativeList.IndexOf(value: TFhirNarrative): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirNarrativeList.Insert(index: Integer): TFhirNarrative;
begin
  result := TFhirNarrative.create;
  try
    inherited insert(index, result);
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

procedure TFhirPeriod.Assign(oSource : TAdvObject);
begin
  inherited;
  startObject := TFhirPeriod(oSource).startObject.Clone;
  end_Object := TFhirPeriod(oSource).end_Object.Clone;
end;

procedure TFhirPeriod.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'start') Then
     list.add(FStart.Link);
  if (child_name = 'end') Then
     list.add(FEnd_.Link);
end;

procedure TFhirPeriod.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'start', 'dateTime', FStart.Link));{2}
  oList.add(TFHIRProperty.create(self, 'end', 'dateTime', FEnd_.Link));{2}
end;

procedure TFhirPeriod.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'start') then StartObject := propValue as TFhirDateTime{5a}
  else if (propName = 'end') then End_Object := propValue as TFhirDateTime{5a}
  else inherited;
end;

function TFhirPeriod.FhirType : string;
begin
  result := 'Period';
end;

function TFhirPeriod.Link : TFhirPeriod;
begin
  result := TFhirPeriod(inherited Link);
end;

function TFhirPeriod.Clone : TFhirPeriod;
begin
  result := TFhirPeriod(inherited Clone);
end;

{ TFhirPeriod }

Procedure TFhirPeriod.SetStart(value : TFhirDateTime);
begin
  FStart.free;
  FStart := value;
end;

Function TFhirPeriod.GetStartST : TDateTimeEx;
begin
  if FStart = nil then
    result := nil
  else
    result := FStart.value;
end;

Procedure TFhirPeriod.SetStartST(value : TDateTimeEx);
begin
  if value <> nil then
  begin
    if FStart = nil then
      FStart := TFhirDateTime.create;
    FStart.value := value
  end
  else if FStart <> nil then
    FStart.value := nil;
end;

Procedure TFhirPeriod.SetEnd_(value : TFhirDateTime);
begin
  FEnd_.free;
  FEnd_ := value;
end;

Function TFhirPeriod.GetEnd_ST : TDateTimeEx;
begin
  if FEnd_ = nil then
    result := nil
  else
    result := FEnd_.value;
end;

Procedure TFhirPeriod.SetEnd_ST(value : TDateTimeEx);
begin
  if value <> nil then
  begin
    if FEnd_ = nil then
      FEnd_ := TFhirDateTime.create;
    FEnd_.value := value
  end
  else if FEnd_ <> nil then
    FEnd_.value := nil;
end;


{ TFhirPeriodListEnumerator }

Constructor TFhirPeriodListEnumerator.Create(list : TFhirPeriodList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirPeriodListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPeriodListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirPeriodListEnumerator.GetCurrent : TFhirPeriod;
begin
  Result := FList[FIndex];
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

function TFhirPeriodList.IndexOf(value: TFhirPeriod): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirPeriodList.Insert(index: Integer): TFhirPeriod;
begin
  result := TFhirPeriod.create;
  try
    inherited insert(index, result);
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
  FPrimary.free;
  FValueSet.free;
  inherited;
end;

procedure TFhirCoding.Assign(oSource : TAdvObject);
begin
  inherited;
  systemObject := TFhirCoding(oSource).systemObject.Clone;
  versionObject := TFhirCoding(oSource).versionObject.Clone;
  codeObject := TFhirCoding(oSource).codeObject.Clone;
  displayObject := TFhirCoding(oSource).displayObject.Clone;
  primaryObject := TFhirCoding(oSource).primaryObject.Clone;
  valueSet := TFhirCoding(oSource).valueSet.Clone;
end;

procedure TFhirCoding.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'system') Then
     list.add(FSystem.Link);
  if (child_name = 'version') Then
     list.add(FVersion.Link);
  if (child_name = 'code') Then
     list.add(FCode.Link);
  if (child_name = 'display') Then
     list.add(FDisplay.Link);
  if (child_name = 'primary') Then
     list.add(FPrimary.Link);
  if (child_name = 'valueSet') Then
     list.add(FValueSet.Link);
end;

procedure TFhirCoding.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'system', 'uri', FSystem.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'code', FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'display', 'string', FDisplay.Link));{2}
  oList.add(TFHIRProperty.create(self, 'primary', 'boolean', FPrimary.Link));{2}
  oList.add(TFHIRProperty.create(self, 'valueSet', 'Resource(ValueSet)', FValueSet.Link));{2}
end;

procedure TFhirCoding.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'system') then SystemObject := propValue as TFhirUri{5a}
  else if (propName = 'version') then VersionObject := propValue as TFhirString{5a}
  else if (propName = 'code') then
    if propValue is TFHIRCode then
      CodeObject := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      CodeObject := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'display') then DisplayObject := propValue as TFhirString{5a}
  else if (propName = 'primary') then PrimaryObject := propValue as TFhirBoolean{5a}
  else if (propName = 'valueSet') then ValueSet := propValue as TFhirResourceReference{TFhirValueSet}{4b}
  else inherited;
end;

function TFhirCoding.FhirType : string;
begin
  result := 'Coding';
end;

function TFhirCoding.Link : TFhirCoding;
begin
  result := TFhirCoding(inherited Link);
end;

function TFhirCoding.Clone : TFhirCoding;
begin
  result := TFhirCoding(inherited Clone);
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

Procedure TFhirCoding.SetPrimary(value : TFhirBoolean);
begin
  FPrimary.free;
  FPrimary := value;
end;

Function TFhirCoding.GetPrimaryST : Boolean;
begin
  if FPrimary = nil then
    result := false
  else
    result := FPrimary.value;
end;

Procedure TFhirCoding.SetPrimaryST(value : Boolean);
begin
  if FPrimary = nil then
    FPrimary := TFhirBoolean.create;
  FPrimary.value := value
end;

Procedure TFhirCoding.SetValueSet(value : TFhirResourceReference{TFhirValueSet});
begin
  FValueSet.free;
  FValueSet := value;
end;


{ TFhirCodingListEnumerator }

Constructor TFhirCodingListEnumerator.Create(list : TFhirCodingList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirCodingListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirCodingListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirCodingListEnumerator.GetCurrent : TFhirCoding;
begin
  Result := FList[FIndex];
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

function TFhirCodingList.IndexOf(value: TFhirCoding): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirCodingList.Insert(index: Integer): TFhirCoding;
begin
  result := TFhirCoding.create;
  try
    inherited insert(index, result);
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

procedure TFhirRange.Assign(oSource : TAdvObject);
begin
  inherited;
  low := TFhirRange(oSource).low.Clone;
  high := TFhirRange(oSource).high.Clone;
end;

procedure TFhirRange.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'low') Then
     list.add(FLow.Link);
  if (child_name = 'high') Then
     list.add(FHigh.Link);
end;

procedure TFhirRange.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'low', 'Quantity', FLow.Link));{2}
  oList.add(TFHIRProperty.create(self, 'high', 'Quantity', FHigh.Link));{2}
end;

procedure TFhirRange.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'low') then Low := propValue as TFhirQuantity{4b}
  else if (propName = 'high') then High := propValue as TFhirQuantity{4b}
  else inherited;
end;

function TFhirRange.FhirType : string;
begin
  result := 'Range';
end;

function TFhirRange.Link : TFhirRange;
begin
  result := TFhirRange(inherited Link);
end;

function TFhirRange.Clone : TFhirRange;
begin
  result := TFhirRange(inherited Clone);
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


{ TFhirRangeListEnumerator }

Constructor TFhirRangeListEnumerator.Create(list : TFhirRangeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirRangeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirRangeListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirRangeListEnumerator.GetCurrent : TFhirRange;
begin
  Result := FList[FIndex];
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

function TFhirRangeList.IndexOf(value: TFhirRange): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirRangeList.Insert(index: Integer): TFhirRange;
begin
  result := TFhirRange.create;
  try
    inherited insert(index, result);
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

function TFhirQuantityComparatorListAsInteger(aSet : TFhirQuantityComparatorList) : Integer;
var
  a : TFhirQuantityComparator;
begin
  result := 0;
  for a := low(TFhirQuantityComparator) to high(TFhirQuantityComparator) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuantityComparatorList(i : Integer) : TFhirQuantityComparatorList;
var
  aLoop : TFhirQuantityComparator;
begin
  result := [];
  for aLoop := low(TFhirQuantityComparator) to high(TFhirQuantityComparator) Do
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
  FUnits.free;
  FSystem.free;
  FCode.free;
  inherited;
end;

procedure TFhirQuantity.Assign(oSource : TAdvObject);
begin
  inherited;
  valueObject := TFhirQuantity(oSource).valueObject.Clone;
  FComparator := TFhirQuantity(oSource).FComparator.Link;
  unitsObject := TFhirQuantity(oSource).unitsObject.Clone;
  systemObject := TFhirQuantity(oSource).systemObject.Clone;
  codeObject := TFhirQuantity(oSource).codeObject.Clone;
end;

procedure TFhirQuantity.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'value') Then
     list.add(FValue.Link);
  if (child_name = 'comparator') Then
     list.add(FComparator.Link);
  if (child_name = 'units') Then
     list.add(FUnits.Link);
  if (child_name = 'system') Then
     list.add(FSystem.Link);
  if (child_name = 'code') Then
     list.add(FCode.Link);
end;

procedure TFhirQuantity.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'decimal', FValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comparator', 'code', FComparator.Link));{1}
  oList.add(TFHIRProperty.create(self, 'units', 'string', FUnits.Link));{2}
  oList.add(TFHIRProperty.create(self, 'system', 'uri', FSystem.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'code', FCode.Link));{2}
end;

procedure TFhirQuantity.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'value') then ValueObject := propValue as TFhirDecimal{5a}
  else if (propName = 'comparator') then ComparatorObject := propValue as TFHIREnum
  else if (propName = 'units') then UnitsObject := propValue as TFhirString{5a}
  else if (propName = 'system') then SystemObject := propValue as TFhirUri{5a}
  else if (propName = 'code') then
    if propValue is TFHIRCode then
      CodeObject := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      CodeObject := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else inherited;
end;

function TFhirQuantity.FhirType : string;
begin
  result := 'Quantity';
end;

function TFhirQuantity.Link : TFhirQuantity;
begin
  result := TFhirQuantity(inherited Link);
end;

function TFhirQuantity.Clone : TFhirQuantity;
begin
  result := TFhirQuantity(inherited Clone);
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

Function TFhirQuantity.GetComparatorST : TFhirQuantityComparator;
begin
  if FComparator = nil then
    result := TFhirQuantityComparator(0)
  else
    result := TFhirQuantityComparator(StringArrayIndexOfSensitive(CODES_TFhirQuantityComparator, FComparator.value));
end;

Procedure TFhirQuantity.SetComparatorST(value : TFhirQuantityComparator);
begin
  if ord(value) = 0 then
    ComparatorObject := nil
  else
    ComparatorObject := TFhirEnum.create(CODES_TFhirQuantityComparator[value]);
end;

Procedure TFhirQuantity.SetUnits(value : TFhirString);
begin
  FUnits.free;
  FUnits := value;
end;

Function TFhirQuantity.GetUnitsST : String;
begin
  if FUnits = nil then
    result := ''
  else
    result := FUnits.value;
end;

Procedure TFhirQuantity.SetUnitsST(value : String);
begin
  if value <> '' then
  begin
    if FUnits = nil then
      FUnits := TFhirString.create;
    FUnits.value := value
  end
  else if FUnits <> nil then
    FUnits.value := '';
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


{ TFhirQuantityListEnumerator }

Constructor TFhirQuantityListEnumerator.Create(list : TFhirQuantityList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirQuantityListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirQuantityListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirQuantityListEnumerator.GetCurrent : TFhirQuantity;
begin
  Result := FList[FIndex];
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

function TFhirQuantityList.IndexOf(value: TFhirQuantity): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirQuantityList.Insert(index: Integer): TFhirQuantity;
begin
  result := TFhirQuantity.create;
  try
    inherited insert(index, result);
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
  inherited;
end;

procedure TFhirAttachment.Assign(oSource : TAdvObject);
begin
  inherited;
  contentTypeObject := TFhirAttachment(oSource).contentTypeObject.Clone;
  languageObject := TFhirAttachment(oSource).languageObject.Clone;
  dataObject := TFhirAttachment(oSource).dataObject.Clone;
  urlObject := TFhirAttachment(oSource).urlObject.Clone;
  sizeObject := TFhirAttachment(oSource).sizeObject.Clone;
  hashObject := TFhirAttachment(oSource).hashObject.Clone;
  titleObject := TFhirAttachment(oSource).titleObject.Clone;
end;

procedure TFhirAttachment.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'contentType') Then
     list.add(FContentType.Link);
  if (child_name = 'language') Then
     list.add(FLanguage.Link);
  if (child_name = 'data') Then
     list.add(FData.Link);
  if (child_name = 'url') Then
     list.add(FUrl.Link);
  if (child_name = 'size') Then
     list.add(FSize.Link);
  if (child_name = 'hash') Then
     list.add(FHash.Link);
  if (child_name = 'title') Then
     list.add(FTitle.Link);
end;

procedure TFhirAttachment.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'contentType', 'code', FContentType.Link));{2}
  oList.add(TFHIRProperty.create(self, 'language', 'code', FLanguage.Link));{2}
  oList.add(TFHIRProperty.create(self, 'data', 'base64Binary', FData.Link));{2}
  oList.add(TFHIRProperty.create(self, 'url', 'uri', FUrl.Link));{2}
  oList.add(TFHIRProperty.create(self, 'size', 'integer', FSize.Link));{2}
  oList.add(TFHIRProperty.create(self, 'hash', 'base64Binary', FHash.Link));{2}
  oList.add(TFHIRProperty.create(self, 'title', 'string', FTitle.Link));{2}
end;

procedure TFhirAttachment.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'contentType') then
    if propValue is TFHIRCode then
      ContentTypeObject := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      ContentTypeObject := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'language') then
    if propValue is TFHIRCode then
      LanguageObject := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      LanguageObject := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'data') then DataObject := propValue as TFhirBase64Binary{5a}
  else if (propName = 'url') then UrlObject := propValue as TFhirUri{5a}
  else if (propName = 'size') then SizeObject := propValue as TFhirInteger{5a}
  else if (propName = 'hash') then HashObject := propValue as TFhirBase64Binary{5a}
  else if (propName = 'title') then TitleObject := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirAttachment.FhirType : string;
begin
  result := 'Attachment';
end;

function TFhirAttachment.Link : TFhirAttachment;
begin
  result := TFhirAttachment(inherited Link);
end;

function TFhirAttachment.Clone : TFhirAttachment;
begin
  result := TFhirAttachment(inherited Clone);
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

Function TFhirAttachment.GetDataST : String;
begin
  if FData = nil then
    result := ''
  else
    result := FData.value;
end;

Procedure TFhirAttachment.SetDataST(value : String);
begin
  if value <> '' then
  begin
    if FData = nil then
      FData := TFhirBase64Binary.create;
    FData.value := value
  end
  else if FData <> nil then
    FData.value := '';
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

Procedure TFhirAttachment.SetSize(value : TFhirInteger);
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
      FSize := TFhirInteger.create;
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

Function TFhirAttachment.GetHashST : String;
begin
  if FHash = nil then
    result := ''
  else
    result := FHash.value;
end;

Procedure TFhirAttachment.SetHashST(value : String);
begin
  if value <> '' then
  begin
    if FHash = nil then
      FHash := TFhirBase64Binary.create;
    FHash.value := value
  end
  else if FHash <> nil then
    FHash.value := '';
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


{ TFhirAttachmentListEnumerator }

Constructor TFhirAttachmentListEnumerator.Create(list : TFhirAttachmentList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirAttachmentListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirAttachmentListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirAttachmentListEnumerator.GetCurrent : TFhirAttachment;
begin
  Result := FList[FIndex];
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

function TFhirAttachmentList.IndexOf(value: TFhirAttachment): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirAttachmentList.Insert(index: Integer): TFhirAttachment;
begin
  result := TFhirAttachment.create;
  try
    inherited insert(index, result);
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

procedure TFhirRatio.Assign(oSource : TAdvObject);
begin
  inherited;
  numerator := TFhirRatio(oSource).numerator.Clone;
  denominator := TFhirRatio(oSource).denominator.Clone;
end;

procedure TFhirRatio.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'numerator') Then
     list.add(FNumerator.Link);
  if (child_name = 'denominator') Then
     list.add(FDenominator.Link);
end;

procedure TFhirRatio.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'numerator', 'Quantity', FNumerator.Link));{2}
  oList.add(TFHIRProperty.create(self, 'denominator', 'Quantity', FDenominator.Link));{2}
end;

procedure TFhirRatio.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'numerator') then Numerator := propValue as TFhirQuantity{4b}
  else if (propName = 'denominator') then Denominator := propValue as TFhirQuantity{4b}
  else inherited;
end;

function TFhirRatio.FhirType : string;
begin
  result := 'Ratio';
end;

function TFhirRatio.Link : TFhirRatio;
begin
  result := TFhirRatio(inherited Link);
end;

function TFhirRatio.Clone : TFhirRatio;
begin
  result := TFhirRatio(inherited Clone);
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


{ TFhirRatioListEnumerator }

Constructor TFhirRatioListEnumerator.Create(list : TFhirRatioList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirRatioListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirRatioListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirRatioListEnumerator.GetCurrent : TFhirRatio;
begin
  Result := FList[FIndex];
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

function TFhirRatioList.IndexOf(value: TFhirRatio): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirRatioList.Insert(index: Integer): TFhirRatio;
begin
  result := TFhirRatio.create;
  try
    inherited insert(index, result);
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

procedure TFhirSampledData.Assign(oSource : TAdvObject);
begin
  inherited;
  origin := TFhirSampledData(oSource).origin.Clone;
  periodObject := TFhirSampledData(oSource).periodObject.Clone;
  factorObject := TFhirSampledData(oSource).factorObject.Clone;
  lowerLimitObject := TFhirSampledData(oSource).lowerLimitObject.Clone;
  upperLimitObject := TFhirSampledData(oSource).upperLimitObject.Clone;
  dimensionsObject := TFhirSampledData(oSource).dimensionsObject.Clone;
  dataObject := TFhirSampledData(oSource).dataObject.Clone;
end;

procedure TFhirSampledData.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'origin') Then
     list.add(FOrigin.Link);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
  if (child_name = 'factor') Then
     list.add(FFactor.Link);
  if (child_name = 'lowerLimit') Then
     list.add(FLowerLimit.Link);
  if (child_name = 'upperLimit') Then
     list.add(FUpperLimit.Link);
  if (child_name = 'dimensions') Then
     list.add(FDimensions.Link);
  if (child_name = 'data') Then
     list.add(FData.Link);
end;

procedure TFhirSampledData.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'origin', 'Quantity', FOrigin.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'decimal', FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'factor', 'decimal', FFactor.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lowerLimit', 'decimal', FLowerLimit.Link));{2}
  oList.add(TFHIRProperty.create(self, 'upperLimit', 'decimal', FUpperLimit.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dimensions', 'integer', FDimensions.Link));{2}
  oList.add(TFHIRProperty.create(self, 'data', 'string', FData.Link));{2}
end;

procedure TFhirSampledData.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'origin') then Origin := propValue as TFhirQuantity{4b}
  else if (propName = 'period') then PeriodObject := propValue as TFhirDecimal{5a}
  else if (propName = 'factor') then FactorObject := propValue as TFhirDecimal{5a}
  else if (propName = 'lowerLimit') then LowerLimitObject := propValue as TFhirDecimal{5a}
  else if (propName = 'upperLimit') then UpperLimitObject := propValue as TFhirDecimal{5a}
  else if (propName = 'dimensions') then DimensionsObject := propValue as TFhirInteger{5a}
  else if (propName = 'data') then DataObject := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirSampledData.FhirType : string;
begin
  result := 'SampledData';
end;

function TFhirSampledData.Link : TFhirSampledData;
begin
  result := TFhirSampledData(inherited Link);
end;

function TFhirSampledData.Clone : TFhirSampledData;
begin
  result := TFhirSampledData(inherited Clone);
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

Procedure TFhirSampledData.SetDimensions(value : TFhirInteger);
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
      FDimensions := TFhirInteger.create;
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


{ TFhirSampledDataListEnumerator }

Constructor TFhirSampledDataListEnumerator.Create(list : TFhirSampledDataList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirSampledDataListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirSampledDataListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirSampledDataListEnumerator.GetCurrent : TFhirSampledData;
begin
  Result := FList[FIndex];
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

function TFhirSampledDataList.IndexOf(value: TFhirSampledData): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirSampledDataList.Insert(index: Integer): TFhirSampledData;
begin
  result := TFhirSampledData.create;
  try
    inherited insert(index, result);
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

{ TFhirResourceReference }

constructor TFhirResourceReference.Create;
begin
  inherited;
end;

destructor TFhirResourceReference.Destroy;
begin
  FReference.free;
  FDisplay.free;
  inherited;
end;

procedure TFhirResourceReference.Assign(oSource : TAdvObject);
begin
  inherited;
  referenceObject := TFhirResourceReference(oSource).referenceObject.Clone;
  displayObject := TFhirResourceReference(oSource).displayObject.Clone;
end;

procedure TFhirResourceReference.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'reference') Then
     list.add(FReference.Link);
  if (child_name = 'display') Then
     list.add(FDisplay.Link);
end;

procedure TFhirResourceReference.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'reference', 'string', FReference.Link));{2}
  oList.add(TFHIRProperty.create(self, 'display', 'string', FDisplay.Link));{2}
end;

procedure TFhirResourceReference.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'reference') then ReferenceObject := propValue as TFhirString{5a}
  else if (propName = 'display') then DisplayObject := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirResourceReference.FhirType : string;
begin
  result := 'ResourceReference';
end;

function TFhirResourceReference.Link : TFhirResourceReference;
begin
  result := TFhirResourceReference(inherited Link);
end;

function TFhirResourceReference.Clone : TFhirResourceReference;
begin
  result := TFhirResourceReference(inherited Clone);
end;

{ TFhirResourceReference }

Procedure TFhirResourceReference.SetReference(value : TFhirString);
begin
  FReference.free;
  FReference := value;
end;

Function TFhirResourceReference.GetReferenceST : String;
begin
  if FReference = nil then
    result := ''
  else
    result := FReference.value;
end;

Procedure TFhirResourceReference.SetReferenceST(value : String);
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

Procedure TFhirResourceReference.SetDisplay(value : TFhirString);
begin
  FDisplay.free;
  FDisplay := value;
end;

Function TFhirResourceReference.GetDisplayST : String;
begin
  if FDisplay = nil then
    result := ''
  else
    result := FDisplay.value;
end;

Procedure TFhirResourceReference.SetDisplayST(value : String);
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


{ TFhirResourceReferenceListEnumerator }

Constructor TFhirResourceReferenceListEnumerator.Create(list : TFhirResourceReferenceList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirResourceReferenceListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirResourceReferenceListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirResourceReferenceListEnumerator.GetCurrent : TFhirResourceReference;
begin
  Result := FList[FIndex];
end;


{ TFhirResourceReferenceList }
procedure TFhirResourceReferenceList.AddItem(value: TFhirResourceReference);
begin
  assert(value.ClassName = 'TFhirResourceReference', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirResourceReference');
  add(value);
end;


function TFhirResourceReferenceList.Append: TFhirResourceReference;
begin
  result := TFhirResourceReference.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirResourceReferenceList.ClearItems;
begin
  Clear;
end;

function TFhirResourceReferenceList.GetEnumerator : TFhirResourceReferenceListEnumerator;
begin
  result := TFhirResourceReferenceListEnumerator.Create(self.link);
end;

function TFhirResourceReferenceList.Clone: TFhirResourceReferenceList;
begin
  result := TFhirResourceReferenceList(inherited Clone);
end;

function TFhirResourceReferenceList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirResourceReferenceList.GetItemN(index: Integer): TFhirResourceReference;
begin
  result := TFhirResourceReference(ObjectByIndex[index]);
end;

function TFhirResourceReferenceList.IndexOf(value: TFhirResourceReference): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirResourceReferenceList.Insert(index: Integer): TFhirResourceReference;
begin
  result := TFhirResourceReference.create;
  try
    inherited insert(index, result);
  finally
    result.free;
  end;
end;


procedure TFhirResourceReferenceList.InsertItem(index: Integer; value: TFhirResourceReference);
begin
  assert(value is TFhirResourceReference);
  Inherited Insert(index, value);
end;

function TFhirResourceReferenceList.Item(index: Integer): TFhirResourceReference;
begin
  result := TFhirResourceReference(ObjectByIndex[index]);
end;

function TFhirResourceReferenceList.Link: TFhirResourceReferenceList;
begin
  result := TFhirResourceReferenceList(inherited Link);
end;

procedure TFhirResourceReferenceList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirResourceReferenceList.SetItemByIndex(index: Integer; value: TFhirResourceReference);
begin
  assert(value is TFhirResourceReference);
  FhirResourceReferences[index] := value;
end;

procedure TFhirResourceReferenceList.SetItemN(index: Integer; value: TFhirResourceReference);
begin
  assert(value is TFhirResourceReference);
  ObjectByIndex[index] := value;
end;

{ TFhirCodeableConcept }

constructor TFhirCodeableConcept.Create;
begin
  inherited;
  FCodingList := TFhirCodingList.Create;
end;

destructor TFhirCodeableConcept.Destroy;
begin
  FCodingList.Free;
  FText.free;
  inherited;
end;

procedure TFhirCodeableConcept.Assign(oSource : TAdvObject);
begin
  inherited;
  FCodingList.Assign(TFhirCodeableConcept(oSource).FCodingList);
  textObject := TFhirCodeableConcept(oSource).textObject.Clone;
end;

procedure TFhirCodeableConcept.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'coding') Then
     list.addAll(FCodingList);
  if (child_name = 'text') Then
     list.add(FText.Link);
end;

procedure TFhirCodeableConcept.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'coding', 'Coding', FCodingList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'text', 'string', FText.Link));{2}
end;

procedure TFhirCodeableConcept.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'coding') then CodingList.add(propValue as TFhirCoding){2}
  else if (propName = 'text') then TextObject := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirCodeableConcept.FhirType : string;
begin
  result := 'CodeableConcept';
end;

function TFhirCodeableConcept.Link : TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept(inherited Link);
end;

function TFhirCodeableConcept.Clone : TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept(inherited Clone);
end;

{ TFhirCodeableConcept }

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


{ TFhirCodeableConceptListEnumerator }

Constructor TFhirCodeableConceptListEnumerator.Create(list : TFhirCodeableConceptList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirCodeableConceptListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirCodeableConceptListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirCodeableConceptListEnumerator.GetCurrent : TFhirCodeableConcept;
begin
  Result := FList[FIndex];
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

function TFhirCodeableConceptList.IndexOf(value: TFhirCodeableConcept): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirCodeableConceptList.Insert(index: Integer): TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept.create;
  try
    inherited insert(index, result);
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

function TFhirIdentifierUseListAsInteger(aSet : TFhirIdentifierUseList) : Integer;
var
  a : TFhirIdentifierUse;
begin
  result := 0;
  for a := low(TFhirIdentifierUse) to high(TFhirIdentifierUse) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIdentifierUseList(i : Integer) : TFhirIdentifierUseList;
var
  aLoop : TFhirIdentifierUse;
begin
  result := [];
  for aLoop := low(TFhirIdentifierUse) to high(TFhirIdentifierUse) Do
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
  FLabel_.free;
  FSystem.free;
  FValue.free;
  FPeriod.free;
  FAssigner.free;
  inherited;
end;

procedure TFhirIdentifier.Assign(oSource : TAdvObject);
begin
  inherited;
  FUse := TFhirIdentifier(oSource).FUse.Link;
  label_Object := TFhirIdentifier(oSource).label_Object.Clone;
  systemObject := TFhirIdentifier(oSource).systemObject.Clone;
  valueObject := TFhirIdentifier(oSource).valueObject.Clone;
  period := TFhirIdentifier(oSource).period.Clone;
  assigner := TFhirIdentifier(oSource).assigner.Clone;
end;

procedure TFhirIdentifier.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'use') Then
     list.add(FUse.Link);
  if (child_name = 'label') Then
     list.add(FLabel_.Link);
  if (child_name = 'system') Then
     list.add(FSystem.Link);
  if (child_name = 'value') Then
     list.add(FValue.Link);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
  if (child_name = 'assigner') Then
     list.add(FAssigner.Link);
end;

procedure TFhirIdentifier.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'use', 'code', FUse.Link));{1}
  oList.add(TFHIRProperty.create(self, 'label', 'string', FLabel_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'system', 'uri', FSystem.Link));{2}
  oList.add(TFHIRProperty.create(self, 'value', 'string', FValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'assigner', 'Resource(Organization)', FAssigner.Link));{2}
end;

procedure TFhirIdentifier.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'use') then UseObject := propValue as TFHIREnum
  else if (propName = 'label') then Label_Object := propValue as TFhirString{5a}
  else if (propName = 'system') then SystemObject := propValue as TFhirUri{5a}
  else if (propName = 'value') then ValueObject := propValue as TFhirString{5a}
  else if (propName = 'period') then Period := propValue as TFhirPeriod{4b}
  else if (propName = 'assigner') then Assigner := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else inherited;
end;

function TFhirIdentifier.FhirType : string;
begin
  result := 'Identifier';
end;

function TFhirIdentifier.Link : TFhirIdentifier;
begin
  result := TFhirIdentifier(inherited Link);
end;

function TFhirIdentifier.Clone : TFhirIdentifier;
begin
  result := TFhirIdentifier(inherited Clone);
end;

{ TFhirIdentifier }

Procedure TFhirIdentifier.SetUse(value : TFhirEnum);
begin
  FUse.free;
  FUse := value;
end;

Function TFhirIdentifier.GetUseST : TFhirIdentifierUse;
begin
  if FUse = nil then
    result := TFhirIdentifierUse(0)
  else
    result := TFhirIdentifierUse(StringArrayIndexOfSensitive(CODES_TFhirIdentifierUse, FUse.value));
end;

Procedure TFhirIdentifier.SetUseST(value : TFhirIdentifierUse);
begin
  if ord(value) = 0 then
    UseObject := nil
  else
    UseObject := TFhirEnum.create(CODES_TFhirIdentifierUse[value]);
end;

Procedure TFhirIdentifier.SetLabel_(value : TFhirString);
begin
  FLabel_.free;
  FLabel_ := value;
end;

Function TFhirIdentifier.GetLabel_ST : String;
begin
  if FLabel_ = nil then
    result := ''
  else
    result := FLabel_.value;
end;

Procedure TFhirIdentifier.SetLabel_ST(value : String);
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

Procedure TFhirIdentifier.SetAssigner(value : TFhirResourceReference{TFhirOrganization});
begin
  FAssigner.free;
  FAssigner := value;
end;


{ TFhirIdentifierListEnumerator }

Constructor TFhirIdentifierListEnumerator.Create(list : TFhirIdentifierList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirIdentifierListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirIdentifierListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirIdentifierListEnumerator.GetCurrent : TFhirIdentifier;
begin
  Result := FList[FIndex];
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

function TFhirIdentifierList.IndexOf(value: TFhirIdentifier): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirIdentifierList.Insert(index: Integer): TFhirIdentifier;
begin
  result := TFhirIdentifier.create;
  try
    inherited insert(index, result);
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

function TFhirEventTimingListAsInteger(aSet : TFhirEventTimingList) : Integer;
var
  a : TFhirEventTiming;
begin
  result := 0;
  for a := low(TFhirEventTiming) to high(TFhirEventTiming) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEventTimingList(i : Integer) : TFhirEventTimingList;
var
  aLoop : TFhirEventTiming;
begin
  result := [];
  for aLoop := low(TFhirEventTiming) to high(TFhirEventTiming) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirUnitsOfTimeListAsInteger(aSet : TFhirUnitsOfTimeList) : Integer;
var
  a : TFhirUnitsOfTime;
begin
  result := 0;
  for a := low(TFhirUnitsOfTime) to high(TFhirUnitsOfTime) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirUnitsOfTimeList(i : Integer) : TFhirUnitsOfTimeList;
var
  aLoop : TFhirUnitsOfTime;
begin
  result := [];
  for aLoop := low(TFhirUnitsOfTime) to high(TFhirUnitsOfTime) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


{ TFhirScheduleRepeat }

constructor TFhirScheduleRepeat.Create;
begin
  inherited;
end;

destructor TFhirScheduleRepeat.Destroy;
begin
  FFrequency.free;
  FWhen.free;
  FDuration.free;
  FUnits.free;
  FCount.free;
  FEnd_.free;
  inherited;
end;

procedure TFhirScheduleRepeat.Assign(oSource : TAdvObject);
begin
  inherited;
  frequencyObject := TFhirScheduleRepeat(oSource).frequencyObject.Clone;
  FWhen := TFhirScheduleRepeat(oSource).FWhen.Link;
  durationObject := TFhirScheduleRepeat(oSource).durationObject.Clone;
  FUnits := TFhirScheduleRepeat(oSource).FUnits.Link;
  countObject := TFhirScheduleRepeat(oSource).countObject.Clone;
  end_Object := TFhirScheduleRepeat(oSource).end_Object.Clone;
end;

procedure TFhirScheduleRepeat.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'frequency') Then
     list.add(FFrequency.Link);
  if (child_name = 'when') Then
     list.add(FWhen.Link);
  if (child_name = 'duration') Then
     list.add(FDuration.Link);
  if (child_name = 'units') Then
     list.add(FUnits.Link);
  if (child_name = 'count') Then
     list.add(FCount.Link);
  if (child_name = 'end') Then
     list.add(FEnd_.Link);
end;

procedure TFhirScheduleRepeat.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'frequency', 'integer', FFrequency.Link));{2}
  oList.add(TFHIRProperty.create(self, 'when', 'code', FWhen.Link));{1}
  oList.add(TFHIRProperty.create(self, 'duration', 'decimal', FDuration.Link));{2}
  oList.add(TFHIRProperty.create(self, 'units', 'code', FUnits.Link));{1}
  oList.add(TFHIRProperty.create(self, 'count', 'integer', FCount.Link));{2}
  oList.add(TFHIRProperty.create(self, 'end', 'dateTime', FEnd_.Link));{2}
end;

procedure TFhirScheduleRepeat.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'frequency') then FrequencyObject := propValue as TFhirInteger{5a}
  else if (propName = 'when') then WhenObject := propValue as TFHIREnum
  else if (propName = 'duration') then DurationObject := propValue as TFhirDecimal{5a}
  else if (propName = 'units') then UnitsObject := propValue as TFHIREnum
  else if (propName = 'count') then CountObject := propValue as TFhirInteger{5a}
  else if (propName = 'end') then End_Object := propValue as TFhirDateTime{5a}
  else inherited;
end;

function TFhirScheduleRepeat.FhirType : string;
begin
  result := 'repeat';
end;

function TFhirScheduleRepeat.Link : TFhirScheduleRepeat;
begin
  result := TFhirScheduleRepeat(inherited Link);
end;

function TFhirScheduleRepeat.Clone : TFhirScheduleRepeat;
begin
  result := TFhirScheduleRepeat(inherited Clone);
end;

{ TFhirScheduleRepeat }

Procedure TFhirScheduleRepeat.SetFrequency(value : TFhirInteger);
begin
  FFrequency.free;
  FFrequency := value;
end;

Function TFhirScheduleRepeat.GetFrequencyST : String;
begin
  if FFrequency = nil then
    result := ''
  else
    result := FFrequency.value;
end;

Procedure TFhirScheduleRepeat.SetFrequencyST(value : String);
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

Procedure TFhirScheduleRepeat.SetWhen(value : TFhirEnum);
begin
  FWhen.free;
  FWhen := value;
end;

Function TFhirScheduleRepeat.GetWhenST : TFhirEventTiming;
begin
  if FWhen = nil then
    result := TFhirEventTiming(0)
  else
    result := TFhirEventTiming(StringArrayIndexOfSensitive(CODES_TFhirEventTiming, FWhen.value));
end;

Procedure TFhirScheduleRepeat.SetWhenST(value : TFhirEventTiming);
begin
  if ord(value) = 0 then
    WhenObject := nil
  else
    WhenObject := TFhirEnum.create(CODES_TFhirEventTiming[value]);
end;

Procedure TFhirScheduleRepeat.SetDuration(value : TFhirDecimal);
begin
  FDuration.free;
  FDuration := value;
end;

Function TFhirScheduleRepeat.GetDurationST : String;
begin
  if FDuration = nil then
    result := ''
  else
    result := FDuration.value;
end;

Procedure TFhirScheduleRepeat.SetDurationST(value : String);
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

Procedure TFhirScheduleRepeat.SetUnits(value : TFhirEnum);
begin
  FUnits.free;
  FUnits := value;
end;

Function TFhirScheduleRepeat.GetUnitsST : TFhirUnitsOfTime;
begin
  if FUnits = nil then
    result := TFhirUnitsOfTime(0)
  else
    result := TFhirUnitsOfTime(StringArrayIndexOfSensitive(CODES_TFhirUnitsOfTime, FUnits.value));
end;

Procedure TFhirScheduleRepeat.SetUnitsST(value : TFhirUnitsOfTime);
begin
  if ord(value) = 0 then
    UnitsObject := nil
  else
    UnitsObject := TFhirEnum.create(CODES_TFhirUnitsOfTime[value]);
end;

Procedure TFhirScheduleRepeat.SetCount(value : TFhirInteger);
begin
  FCount.free;
  FCount := value;
end;

Function TFhirScheduleRepeat.GetCountST : String;
begin
  if FCount = nil then
    result := ''
  else
    result := FCount.value;
end;

Procedure TFhirScheduleRepeat.SetCountST(value : String);
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

Procedure TFhirScheduleRepeat.SetEnd_(value : TFhirDateTime);
begin
  FEnd_.free;
  FEnd_ := value;
end;

Function TFhirScheduleRepeat.GetEnd_ST : TDateTimeEx;
begin
  if FEnd_ = nil then
    result := nil
  else
    result := FEnd_.value;
end;

Procedure TFhirScheduleRepeat.SetEnd_ST(value : TDateTimeEx);
begin
  if value <> nil then
  begin
    if FEnd_ = nil then
      FEnd_ := TFhirDateTime.create;
    FEnd_.value := value
  end
  else if FEnd_ <> nil then
    FEnd_.value := nil;
end;


{ TFhirScheduleRepeatListEnumerator }

Constructor TFhirScheduleRepeatListEnumerator.Create(list : TFhirScheduleRepeatList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirScheduleRepeatListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirScheduleRepeatListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirScheduleRepeatListEnumerator.GetCurrent : TFhirScheduleRepeat;
begin
  Result := FList[FIndex];
end;


{ TFhirScheduleRepeatList }
procedure TFhirScheduleRepeatList.AddItem(value: TFhirScheduleRepeat);
begin
  assert(value.ClassName = 'TFhirScheduleRepeat', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirScheduleRepeat');
  add(value);
end;


function TFhirScheduleRepeatList.Append: TFhirScheduleRepeat;
begin
  result := TFhirScheduleRepeat.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirScheduleRepeatList.ClearItems;
begin
  Clear;
end;

function TFhirScheduleRepeatList.GetEnumerator : TFhirScheduleRepeatListEnumerator;
begin
  result := TFhirScheduleRepeatListEnumerator.Create(self.link);
end;

function TFhirScheduleRepeatList.Clone: TFhirScheduleRepeatList;
begin
  result := TFhirScheduleRepeatList(inherited Clone);
end;

function TFhirScheduleRepeatList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirScheduleRepeatList.GetItemN(index: Integer): TFhirScheduleRepeat;
begin
  result := TFhirScheduleRepeat(ObjectByIndex[index]);
end;

function TFhirScheduleRepeatList.IndexOf(value: TFhirScheduleRepeat): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirScheduleRepeatList.Insert(index: Integer): TFhirScheduleRepeat;
begin
  result := TFhirScheduleRepeat.create;
  try
    inherited insert(index, result);
  finally
    result.free;
  end;
end;


procedure TFhirScheduleRepeatList.InsertItem(index: Integer; value: TFhirScheduleRepeat);
begin
  assert(value is TFhirScheduleRepeat);
  Inherited Insert(index, value);
end;

function TFhirScheduleRepeatList.Item(index: Integer): TFhirScheduleRepeat;
begin
  result := TFhirScheduleRepeat(ObjectByIndex[index]);
end;

function TFhirScheduleRepeatList.Link: TFhirScheduleRepeatList;
begin
  result := TFhirScheduleRepeatList(inherited Link);
end;

procedure TFhirScheduleRepeatList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirScheduleRepeatList.SetItemByIndex(index: Integer; value: TFhirScheduleRepeat);
begin
  assert(value is TFhirScheduleRepeat);
  FhirScheduleRepeats[index] := value;
end;

procedure TFhirScheduleRepeatList.SetItemN(index: Integer; value: TFhirScheduleRepeat);
begin
  assert(value is TFhirScheduleRepeat);
  ObjectByIndex[index] := value;
end;

{ TFhirSchedule }

constructor TFhirSchedule.Create;
begin
  inherited;
  FEventList := TFhirPeriodList.Create;
end;

destructor TFhirSchedule.Destroy;
begin
  FEventList.Free;
  FRepeat_.free;
  inherited;
end;

procedure TFhirSchedule.Assign(oSource : TAdvObject);
begin
  inherited;
  FEventList.Assign(TFhirSchedule(oSource).FEventList);
  repeat_ := TFhirSchedule(oSource).repeat_.Clone;
end;

procedure TFhirSchedule.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'event') Then
     list.addAll(FEventList);
  if (child_name = 'repeat') Then
     list.add(FRepeat_.Link);
end;

procedure TFhirSchedule.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'event', 'Period', FEventList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'repeat', '', FRepeat_.Link));{2}
end;

procedure TFhirSchedule.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'event') then EventList.add(propValue as TFhirPeriod){2}
  else if (propName = 'repeat') then Repeat_ := propValue as TFhirScheduleRepeat{4b}
  else inherited;
end;

function TFhirSchedule.FhirType : string;
begin
  result := 'Schedule';
end;

function TFhirSchedule.Link : TFhirSchedule;
begin
  result := TFhirSchedule(inherited Link);
end;

function TFhirSchedule.Clone : TFhirSchedule;
begin
  result := TFhirSchedule(inherited Clone);
end;

{ TFhirSchedule }

Procedure TFhirSchedule.SetRepeat_(value : TFhirScheduleRepeat);
begin
  FRepeat_.free;
  FRepeat_ := value;
end;


{ TFhirScheduleListEnumerator }

Constructor TFhirScheduleListEnumerator.Create(list : TFhirScheduleList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirScheduleListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirScheduleListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirScheduleListEnumerator.GetCurrent : TFhirSchedule;
begin
  Result := FList[FIndex];
end;


{ TFhirScheduleList }
procedure TFhirScheduleList.AddItem(value: TFhirSchedule);
begin
  assert(value.ClassName = 'TFhirSchedule', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirSchedule');
  add(value);
end;


function TFhirScheduleList.Append: TFhirSchedule;
begin
  result := TFhirSchedule.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirScheduleList.ClearItems;
begin
  Clear;
end;

function TFhirScheduleList.GetEnumerator : TFhirScheduleListEnumerator;
begin
  result := TFhirScheduleListEnumerator.Create(self.link);
end;

function TFhirScheduleList.Clone: TFhirScheduleList;
begin
  result := TFhirScheduleList(inherited Clone);
end;

function TFhirScheduleList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirScheduleList.GetItemN(index: Integer): TFhirSchedule;
begin
  result := TFhirSchedule(ObjectByIndex[index]);
end;

function TFhirScheduleList.IndexOf(value: TFhirSchedule): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirScheduleList.Insert(index: Integer): TFhirSchedule;
begin
  result := TFhirSchedule.create;
  try
    inherited insert(index, result);
  finally
    result.free;
  end;
end;


procedure TFhirScheduleList.InsertItem(index: Integer; value: TFhirSchedule);
begin
  assert(value is TFhirSchedule);
  Inherited Insert(index, value);
end;

function TFhirScheduleList.Item(index: Integer): TFhirSchedule;
begin
  result := TFhirSchedule(ObjectByIndex[index]);
end;

function TFhirScheduleList.Link: TFhirScheduleList;
begin
  result := TFhirScheduleList(inherited Link);
end;

procedure TFhirScheduleList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirScheduleList.SetItemByIndex(index: Integer; value: TFhirSchedule);
begin
  assert(value is TFhirSchedule);
  FhirSchedules[index] := value;
end;

procedure TFhirScheduleList.SetItemN(index: Integer; value: TFhirSchedule);
begin
  assert(value is TFhirSchedule);
  ObjectByIndex[index] := value;
end;

function TFhirContactSystemListAsInteger(aSet : TFhirContactSystemList) : Integer;
var
  a : TFhirContactSystem;
begin
  result := 0;
  for a := low(TFhirContactSystem) to high(TFhirContactSystem) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContactSystemList(i : Integer) : TFhirContactSystemList;
var
  aLoop : TFhirContactSystem;
begin
  result := [];
  for aLoop := low(TFhirContactSystem) to high(TFhirContactSystem) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirContactUseListAsInteger(aSet : TFhirContactUseList) : Integer;
var
  a : TFhirContactUse;
begin
  result := 0;
  for a := low(TFhirContactUse) to high(TFhirContactUse) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContactUseList(i : Integer) : TFhirContactUseList;
var
  aLoop : TFhirContactUse;
begin
  result := [];
  for aLoop := low(TFhirContactUse) to high(TFhirContactUse) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


{ TFhirContact }

constructor TFhirContact.Create;
begin
  inherited;
end;

destructor TFhirContact.Destroy;
begin
  FSystem.free;
  FValue.free;
  FUse.free;
  FPeriod.free;
  inherited;
end;

procedure TFhirContact.Assign(oSource : TAdvObject);
begin
  inherited;
  FSystem := TFhirContact(oSource).FSystem.Link;
  valueObject := TFhirContact(oSource).valueObject.Clone;
  FUse := TFhirContact(oSource).FUse.Link;
  period := TFhirContact(oSource).period.Clone;
end;

procedure TFhirContact.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'system') Then
     list.add(FSystem.Link);
  if (child_name = 'value') Then
     list.add(FValue.Link);
  if (child_name = 'use') Then
     list.add(FUse.Link);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
end;

procedure TFhirContact.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'system', 'code', FSystem.Link));{1}
  oList.add(TFHIRProperty.create(self, 'value', 'string', FValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'use', 'code', FUse.Link));{1}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
end;

procedure TFhirContact.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'system') then SystemObject := propValue as TFHIREnum
  else if (propName = 'value') then ValueObject := propValue as TFhirString{5a}
  else if (propName = 'use') then UseObject := propValue as TFHIREnum
  else if (propName = 'period') then Period := propValue as TFhirPeriod{4b}
  else inherited;
end;

function TFhirContact.FhirType : string;
begin
  result := 'Contact';
end;

function TFhirContact.Link : TFhirContact;
begin
  result := TFhirContact(inherited Link);
end;

function TFhirContact.Clone : TFhirContact;
begin
  result := TFhirContact(inherited Clone);
end;

{ TFhirContact }

Procedure TFhirContact.SetSystem(value : TFhirEnum);
begin
  FSystem.free;
  FSystem := value;
end;

Function TFhirContact.GetSystemST : TFhirContactSystem;
begin
  if FSystem = nil then
    result := TFhirContactSystem(0)
  else
    result := TFhirContactSystem(StringArrayIndexOfSensitive(CODES_TFhirContactSystem, FSystem.value));
end;

Procedure TFhirContact.SetSystemST(value : TFhirContactSystem);
begin
  if ord(value) = 0 then
    SystemObject := nil
  else
    SystemObject := TFhirEnum.create(CODES_TFhirContactSystem[value]);
end;

Procedure TFhirContact.SetValue(value : TFhirString);
begin
  FValue.free;
  FValue := value;
end;

Function TFhirContact.GetValueST : String;
begin
  if FValue = nil then
    result := ''
  else
    result := FValue.value;
end;

Procedure TFhirContact.SetValueST(value : String);
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

Procedure TFhirContact.SetUse(value : TFhirEnum);
begin
  FUse.free;
  FUse := value;
end;

Function TFhirContact.GetUseST : TFhirContactUse;
begin
  if FUse = nil then
    result := TFhirContactUse(0)
  else
    result := TFhirContactUse(StringArrayIndexOfSensitive(CODES_TFhirContactUse, FUse.value));
end;

Procedure TFhirContact.SetUseST(value : TFhirContactUse);
begin
  if ord(value) = 0 then
    UseObject := nil
  else
    UseObject := TFhirEnum.create(CODES_TFhirContactUse[value]);
end;

Procedure TFhirContact.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;


{ TFhirContactListEnumerator }

Constructor TFhirContactListEnumerator.Create(list : TFhirContactList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirContactListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirContactListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirContactListEnumerator.GetCurrent : TFhirContact;
begin
  Result := FList[FIndex];
end;


{ TFhirContactList }
procedure TFhirContactList.AddItem(value: TFhirContact);
begin
  assert(value.ClassName = 'TFhirContact', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirContact');
  add(value);
end;


function TFhirContactList.Append: TFhirContact;
begin
  result := TFhirContact.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirContactList.ClearItems;
begin
  Clear;
end;

function TFhirContactList.GetEnumerator : TFhirContactListEnumerator;
begin
  result := TFhirContactListEnumerator.Create(self.link);
end;

function TFhirContactList.Clone: TFhirContactList;
begin
  result := TFhirContactList(inherited Clone);
end;

function TFhirContactList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirContactList.GetItemN(index: Integer): TFhirContact;
begin
  result := TFhirContact(ObjectByIndex[index]);
end;

function TFhirContactList.IndexOf(value: TFhirContact): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirContactList.Insert(index: Integer): TFhirContact;
begin
  result := TFhirContact.create;
  try
    inherited insert(index, result);
  finally
    result.free;
  end;
end;


procedure TFhirContactList.InsertItem(index: Integer; value: TFhirContact);
begin
  assert(value is TFhirContact);
  Inherited Insert(index, value);
end;

function TFhirContactList.Item(index: Integer): TFhirContact;
begin
  result := TFhirContact(ObjectByIndex[index]);
end;

function TFhirContactList.Link: TFhirContactList;
begin
  result := TFhirContactList(inherited Link);
end;

procedure TFhirContactList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirContactList.SetItemByIndex(index: Integer; value: TFhirContact);
begin
  assert(value is TFhirContact);
  FhirContacts[index] := value;
end;

procedure TFhirContactList.SetItemN(index: Integer; value: TFhirContact);
begin
  assert(value is TFhirContact);
  ObjectByIndex[index] := value;
end;

function TFhirAddressUseListAsInteger(aSet : TFhirAddressUseList) : Integer;
var
  a : TFhirAddressUse;
begin
  result := 0;
  for a := low(TFhirAddressUse) to high(TFhirAddressUse) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAddressUseList(i : Integer) : TFhirAddressUseList;
var
  aLoop : TFhirAddressUse;
begin
  result := [];
  for aLoop := low(TFhirAddressUse) to high(TFhirAddressUse) Do
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
  FLineList := TFhirStringList.Create;
end;

destructor TFhirAddress.Destroy;
begin
  FUse.free;
  FText.free;
  FLineList.Free;
  FCity.free;
  FState.free;
  FZip.free;
  FCountry.free;
  FPeriod.free;
  inherited;
end;

procedure TFhirAddress.Assign(oSource : TAdvObject);
begin
  inherited;
  FUse := TFhirAddress(oSource).FUse.Link;
  textObject := TFhirAddress(oSource).textObject.Clone;
  FLineList.Assign(TFhirAddress(oSource).FLineList);
  cityObject := TFhirAddress(oSource).cityObject.Clone;
  stateObject := TFhirAddress(oSource).stateObject.Clone;
  zipObject := TFhirAddress(oSource).zipObject.Clone;
  countryObject := TFhirAddress(oSource).countryObject.Clone;
  period := TFhirAddress(oSource).period.Clone;
end;

procedure TFhirAddress.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'use') Then
     list.add(FUse.Link);
  if (child_name = 'text') Then
     list.add(FText.Link);
  if (child_name = 'line') Then
     list.addAll(FLineList);
  if (child_name = 'city') Then
     list.add(FCity.Link);
  if (child_name = 'state') Then
     list.add(FState.Link);
  if (child_name = 'zip') Then
     list.add(FZip.Link);
  if (child_name = 'country') Then
     list.add(FCountry.Link);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
end;

procedure TFhirAddress.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'use', 'code', FUse.Link));{1}
  oList.add(TFHIRProperty.create(self, 'text', 'string', FText.Link));{2}
  oList.add(TFHIRProperty.create(self, 'line', 'string', FLineList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'city', 'string', FCity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'state', 'string', FState.Link));{2}
  oList.add(TFHIRProperty.create(self, 'zip', 'string', FZip.Link));{2}
  oList.add(TFHIRProperty.create(self, 'country', 'string', FCountry.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
end;

procedure TFhirAddress.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'use') then UseObject := propValue as TFHIREnum
  else if (propName = 'text') then TextObject := propValue as TFhirString{5a}
  else if (propName = 'line') then LineList.add(propValue as TFhirString){2}
  else if (propName = 'city') then CityObject := propValue as TFhirString{5a}
  else if (propName = 'state') then StateObject := propValue as TFhirString{5a}
  else if (propName = 'zip') then ZipObject := propValue as TFhirString{5a}
  else if (propName = 'country') then CountryObject := propValue as TFhirString{5a}
  else if (propName = 'period') then Period := propValue as TFhirPeriod{4b}
  else inherited;
end;

function TFhirAddress.FhirType : string;
begin
  result := 'Address';
end;

function TFhirAddress.Link : TFhirAddress;
begin
  result := TFhirAddress(inherited Link);
end;

function TFhirAddress.Clone : TFhirAddress;
begin
  result := TFhirAddress(inherited Clone);
end;

{ TFhirAddress }

Procedure TFhirAddress.SetUse(value : TFhirEnum);
begin
  FUse.free;
  FUse := value;
end;

Function TFhirAddress.GetUseST : TFhirAddressUse;
begin
  if FUse = nil then
    result := TFhirAddressUse(0)
  else
    result := TFhirAddressUse(StringArrayIndexOfSensitive(CODES_TFhirAddressUse, FUse.value));
end;

Procedure TFhirAddress.SetUseST(value : TFhirAddressUse);
begin
  if ord(value) = 0 then
    UseObject := nil
  else
    UseObject := TFhirEnum.create(CODES_TFhirAddressUse[value]);
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

Procedure TFhirAddress.SetZip(value : TFhirString);
begin
  FZip.free;
  FZip := value;
end;

Function TFhirAddress.GetZipST : String;
begin
  if FZip = nil then
    result := ''
  else
    result := FZip.value;
end;

Procedure TFhirAddress.SetZipST(value : String);
begin
  if value <> '' then
  begin
    if FZip = nil then
      FZip := TFhirString.create;
    FZip.value := value
  end
  else if FZip <> nil then
    FZip.value := '';
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


{ TFhirAddressListEnumerator }

Constructor TFhirAddressListEnumerator.Create(list : TFhirAddressList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirAddressListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirAddressListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirAddressListEnumerator.GetCurrent : TFhirAddress;
begin
  Result := FList[FIndex];
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

function TFhirAddressList.IndexOf(value: TFhirAddress): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirAddressList.Insert(index: Integer): TFhirAddress;
begin
  result := TFhirAddress.create;
  try
    inherited insert(index, result);
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

function TFhirNameUseListAsInteger(aSet : TFhirNameUseList) : Integer;
var
  a : TFhirNameUse;
begin
  result := 0;
  for a := low(TFhirNameUse) to high(TFhirNameUse) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNameUseList(i : Integer) : TFhirNameUseList;
var
  aLoop : TFhirNameUse;
begin
  result := [];
  for aLoop := low(TFhirNameUse) to high(TFhirNameUse) Do
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
  FFamilyList := TFhirStringList.Create;
  FGivenList := TFhirStringList.Create;
  FPrefixList := TFhirStringList.Create;
  FSuffixList := TFhirStringList.Create;
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

procedure TFhirHumanName.Assign(oSource : TAdvObject);
begin
  inherited;
  FUse := TFhirHumanName(oSource).FUse.Link;
  textObject := TFhirHumanName(oSource).textObject.Clone;
  FFamilyList.Assign(TFhirHumanName(oSource).FFamilyList);
  FGivenList.Assign(TFhirHumanName(oSource).FGivenList);
  FPrefixList.Assign(TFhirHumanName(oSource).FPrefixList);
  FSuffixList.Assign(TFhirHumanName(oSource).FSuffixList);
  period := TFhirHumanName(oSource).period.Clone;
end;

procedure TFhirHumanName.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'use') Then
     list.add(FUse.Link);
  if (child_name = 'text') Then
     list.add(FText.Link);
  if (child_name = 'family') Then
     list.addAll(FFamilyList);
  if (child_name = 'given') Then
     list.addAll(FGivenList);
  if (child_name = 'prefix') Then
     list.addAll(FPrefixList);
  if (child_name = 'suffix') Then
     list.addAll(FSuffixList);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
end;

procedure TFhirHumanName.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'use', 'code', FUse.Link));{1}
  oList.add(TFHIRProperty.create(self, 'text', 'string', FText.Link));{2}
  oList.add(TFHIRProperty.create(self, 'family', 'string', FFamilyList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'given', 'string', FGivenList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'prefix', 'string', FPrefixList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'suffix', 'string', FSuffixList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
end;

procedure TFhirHumanName.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'use') then UseObject := propValue as TFHIREnum
  else if (propName = 'text') then TextObject := propValue as TFhirString{5a}
  else if (propName = 'family') then FamilyList.add(propValue as TFhirString){2}
  else if (propName = 'given') then GivenList.add(propValue as TFhirString){2}
  else if (propName = 'prefix') then PrefixList.add(propValue as TFhirString){2}
  else if (propName = 'suffix') then SuffixList.add(propValue as TFhirString){2}
  else if (propName = 'period') then Period := propValue as TFhirPeriod{4b}
  else inherited;
end;

function TFhirHumanName.FhirType : string;
begin
  result := 'HumanName';
end;

function TFhirHumanName.Link : TFhirHumanName;
begin
  result := TFhirHumanName(inherited Link);
end;

function TFhirHumanName.Clone : TFhirHumanName;
begin
  result := TFhirHumanName(inherited Clone);
end;

{ TFhirHumanName }

Procedure TFhirHumanName.SetUse(value : TFhirEnum);
begin
  FUse.free;
  FUse := value;
end;

Function TFhirHumanName.GetUseST : TFhirNameUse;
begin
  if FUse = nil then
    result := TFhirNameUse(0)
  else
    result := TFhirNameUse(StringArrayIndexOfSensitive(CODES_TFhirNameUse, FUse.value));
end;

Procedure TFhirHumanName.SetUseST(value : TFhirNameUse);
begin
  if ord(value) = 0 then
    UseObject := nil
  else
    UseObject := TFhirEnum.create(CODES_TFhirNameUse[value]);
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

Procedure TFhirHumanName.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;


{ TFhirHumanNameListEnumerator }

Constructor TFhirHumanNameListEnumerator.Create(list : TFhirHumanNameList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirHumanNameListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirHumanNameListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirHumanNameListEnumerator.GetCurrent : TFhirHumanName;
begin
  Result := FList[FIndex];
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

function TFhirHumanNameList.IndexOf(value: TFhirHumanName): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirHumanNameList.Insert(index: Integer): TFhirHumanName;
begin
  result := TFhirHumanName.create;
  try
    inherited insert(index, result);
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

function TFhirReactionSeverityListAsInteger(aSet : TFhirReactionSeverityList) : Integer;
var
  a : TFhirReactionSeverity;
begin
  result := 0;
  for a := low(TFhirReactionSeverity) to high(TFhirReactionSeverity) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReactionSeverityList(i : Integer) : TFhirReactionSeverityList;
var
  aLoop : TFhirReactionSeverity;
begin
  result := [];
  for aLoop := low(TFhirReactionSeverity) to high(TFhirReactionSeverity) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirExposureTypeListAsInteger(aSet : TFhirExposureTypeList) : Integer;
var
  a : TFhirExposureType;
begin
  result := 0;
  for a := low(TFhirExposureType) to high(TFhirExposureType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirExposureTypeList(i : Integer) : TFhirExposureTypeList;
var
  aLoop : TFhirExposureType;
begin
  result := [];
  for aLoop := low(TFhirExposureType) to high(TFhirExposureType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirCausalityExpectationListAsInteger(aSet : TFhirCausalityExpectationList) : Integer;
var
  a : TFhirCausalityExpectation;
begin
  result := 0;
  for a := low(TFhirCausalityExpectation) to high(TFhirCausalityExpectation) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCausalityExpectationList(i : Integer) : TFhirCausalityExpectationList;
var
  aLoop : TFhirCausalityExpectation;
begin
  result := [];
  for aLoop := low(TFhirCausalityExpectation) to high(TFhirCausalityExpectation) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAlertStatusListAsInteger(aSet : TFhirAlertStatusList) : Integer;
var
  a : TFhirAlertStatus;
begin
  result := 0;
  for a := low(TFhirAlertStatus) to high(TFhirAlertStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAlertStatusList(i : Integer) : TFhirAlertStatusList;
var
  aLoop : TFhirAlertStatus;
begin
  result := [];
  for aLoop := low(TFhirAlertStatus) to high(TFhirAlertStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirCriticalityListAsInteger(aSet : TFhirCriticalityList) : Integer;
var
  a : TFhirCriticality;
begin
  result := 0;
  for a := low(TFhirCriticality) to high(TFhirCriticality) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCriticalityList(i : Integer) : TFhirCriticalityList;
var
  aLoop : TFhirCriticality;
begin
  result := [];
  for aLoop := low(TFhirCriticality) to high(TFhirCriticality) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSensitivitytypeListAsInteger(aSet : TFhirSensitivitytypeList) : Integer;
var
  a : TFhirSensitivitytype;
begin
  result := 0;
  for a := low(TFhirSensitivitytype) to high(TFhirSensitivitytype) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSensitivitytypeList(i : Integer) : TFhirSensitivitytypeList;
var
  aLoop : TFhirSensitivitytype;
begin
  result := [];
  for aLoop := low(TFhirSensitivitytype) to high(TFhirSensitivitytype) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSensitivitystatusListAsInteger(aSet : TFhirSensitivitystatusList) : Integer;
var
  a : TFhirSensitivitystatus;
begin
  result := 0;
  for a := low(TFhirSensitivitystatus) to high(TFhirSensitivitystatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSensitivitystatusList(i : Integer) : TFhirSensitivitystatusList;
var
  aLoop : TFhirSensitivitystatus;
begin
  result := [];
  for aLoop := low(TFhirSensitivitystatus) to high(TFhirSensitivitystatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirCarePlanStatusListAsInteger(aSet : TFhirCarePlanStatusList) : Integer;
var
  a : TFhirCarePlanStatus;
begin
  result := 0;
  for a := low(TFhirCarePlanStatus) to high(TFhirCarePlanStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanStatusList(i : Integer) : TFhirCarePlanStatusList;
var
  aLoop : TFhirCarePlanStatus;
begin
  result := [];
  for aLoop := low(TFhirCarePlanStatus) to high(TFhirCarePlanStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirCarePlanGoalStatusListAsInteger(aSet : TFhirCarePlanGoalStatusList) : Integer;
var
  a : TFhirCarePlanGoalStatus;
begin
  result := 0;
  for a := low(TFhirCarePlanGoalStatus) to high(TFhirCarePlanGoalStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanGoalStatusList(i : Integer) : TFhirCarePlanGoalStatusList;
var
  aLoop : TFhirCarePlanGoalStatus;
begin
  result := [];
  for aLoop := low(TFhirCarePlanGoalStatus) to high(TFhirCarePlanGoalStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirCarePlanActivityStatusListAsInteger(aSet : TFhirCarePlanActivityStatusList) : Integer;
var
  a : TFhirCarePlanActivityStatus;
begin
  result := 0;
  for a := low(TFhirCarePlanActivityStatus) to high(TFhirCarePlanActivityStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanActivityStatusList(i : Integer) : TFhirCarePlanActivityStatusList;
var
  aLoop : TFhirCarePlanActivityStatus;
begin
  result := [];
  for aLoop := low(TFhirCarePlanActivityStatus) to high(TFhirCarePlanActivityStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirCarePlanActivityCategoryListAsInteger(aSet : TFhirCarePlanActivityCategoryList) : Integer;
var
  a : TFhirCarePlanActivityCategory;
begin
  result := 0;
  for a := low(TFhirCarePlanActivityCategory) to high(TFhirCarePlanActivityCategory) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanActivityCategoryList(i : Integer) : TFhirCarePlanActivityCategoryList;
var
  aLoop : TFhirCarePlanActivityCategory;
begin
  result := [];
  for aLoop := low(TFhirCarePlanActivityCategory) to high(TFhirCarePlanActivityCategory) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirCompositionStatusListAsInteger(aSet : TFhirCompositionStatusList) : Integer;
var
  a : TFhirCompositionStatus;
begin
  result := 0;
  for a := low(TFhirCompositionStatus) to high(TFhirCompositionStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCompositionStatusList(i : Integer) : TFhirCompositionStatusList;
var
  aLoop : TFhirCompositionStatus;
begin
  result := [];
  for aLoop := low(TFhirCompositionStatus) to high(TFhirCompositionStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirCompositionAttestationModeListAsInteger(aSet : TFhirCompositionAttestationModeList) : Integer;
var
  a : TFhirCompositionAttestationMode;
begin
  result := 0;
  for a := low(TFhirCompositionAttestationMode) to high(TFhirCompositionAttestationMode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCompositionAttestationModeList(i : Integer) : TFhirCompositionAttestationModeList;
var
  aLoop : TFhirCompositionAttestationMode;
begin
  result := [];
  for aLoop := low(TFhirCompositionAttestationMode) to high(TFhirCompositionAttestationMode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirValuesetStatusListAsInteger(aSet : TFhirValuesetStatusList) : Integer;
var
  a : TFhirValuesetStatus;
begin
  result := 0;
  for a := low(TFhirValuesetStatus) to high(TFhirValuesetStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirValuesetStatusList(i : Integer) : TFhirValuesetStatusList;
var
  aLoop : TFhirValuesetStatus;
begin
  result := [];
  for aLoop := low(TFhirValuesetStatus) to high(TFhirValuesetStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirConceptEquivalenceListAsInteger(aSet : TFhirConceptEquivalenceList) : Integer;
var
  a : TFhirConceptEquivalence;
begin
  result := 0;
  for a := low(TFhirConceptEquivalence) to high(TFhirConceptEquivalence) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConceptEquivalenceList(i : Integer) : TFhirConceptEquivalenceList;
var
  aLoop : TFhirConceptEquivalence;
begin
  result := [];
  for aLoop := low(TFhirConceptEquivalence) to high(TFhirConceptEquivalence) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirConditionStatusListAsInteger(aSet : TFhirConditionStatusList) : Integer;
var
  a : TFhirConditionStatus;
begin
  result := 0;
  for a := low(TFhirConditionStatus) to high(TFhirConditionStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionStatusList(i : Integer) : TFhirConditionStatusList;
var
  aLoop : TFhirConditionStatus;
begin
  result := [];
  for aLoop := low(TFhirConditionStatus) to high(TFhirConditionStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirConditionRelationshipTypeListAsInteger(aSet : TFhirConditionRelationshipTypeList) : Integer;
var
  a : TFhirConditionRelationshipType;
begin
  result := 0;
  for a := low(TFhirConditionRelationshipType) to high(TFhirConditionRelationshipType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionRelationshipTypeList(i : Integer) : TFhirConditionRelationshipTypeList;
var
  aLoop : TFhirConditionRelationshipType;
begin
  result := [];
  for aLoop := low(TFhirConditionRelationshipType) to high(TFhirConditionRelationshipType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirConformanceStatementStatusListAsInteger(aSet : TFhirConformanceStatementStatusList) : Integer;
var
  a : TFhirConformanceStatementStatus;
begin
  result := 0;
  for a := low(TFhirConformanceStatementStatus) to high(TFhirConformanceStatementStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConformanceStatementStatusList(i : Integer) : TFhirConformanceStatementStatusList;
var
  aLoop : TFhirConformanceStatementStatus;
begin
  result := [];
  for aLoop := low(TFhirConformanceStatementStatus) to high(TFhirConformanceStatementStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirRestfulConformanceModeListAsInteger(aSet : TFhirRestfulConformanceModeList) : Integer;
var
  a : TFhirRestfulConformanceMode;
begin
  result := 0;
  for a := low(TFhirRestfulConformanceMode) to high(TFhirRestfulConformanceMode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRestfulConformanceModeList(i : Integer) : TFhirRestfulConformanceModeList;
var
  aLoop : TFhirRestfulConformanceMode;
begin
  result := [];
  for aLoop := low(TFhirRestfulConformanceMode) to high(TFhirRestfulConformanceMode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirTypeRestfulOperationListAsInteger(aSet : TFhirTypeRestfulOperationList) : Integer;
var
  a : TFhirTypeRestfulOperation;
begin
  result := 0;
  for a := low(TFhirTypeRestfulOperation) to high(TFhirTypeRestfulOperation) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTypeRestfulOperationList(i : Integer) : TFhirTypeRestfulOperationList;
var
  aLoop : TFhirTypeRestfulOperation;
begin
  result := [];
  for aLoop := low(TFhirTypeRestfulOperation) to high(TFhirTypeRestfulOperation) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSearchParamTypeListAsInteger(aSet : TFhirSearchParamTypeList) : Integer;
var
  a : TFhirSearchParamType;
begin
  result := 0;
  for a := low(TFhirSearchParamType) to high(TFhirSearchParamType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchParamTypeList(i : Integer) : TFhirSearchParamTypeList;
var
  aLoop : TFhirSearchParamType;
begin
  result := [];
  for aLoop := low(TFhirSearchParamType) to high(TFhirSearchParamType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSystemRestfulOperationListAsInteger(aSet : TFhirSystemRestfulOperationList) : Integer;
var
  a : TFhirSystemRestfulOperation;
begin
  result := 0;
  for a := low(TFhirSystemRestfulOperation) to high(TFhirSystemRestfulOperation) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSystemRestfulOperationList(i : Integer) : TFhirSystemRestfulOperationList;
var
  aLoop : TFhirSystemRestfulOperation;
begin
  result := [];
  for aLoop := low(TFhirSystemRestfulOperation) to high(TFhirSystemRestfulOperation) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMessageSignificanceCategoryListAsInteger(aSet : TFhirMessageSignificanceCategoryList) : Integer;
var
  a : TFhirMessageSignificanceCategory;
begin
  result := 0;
  for a := low(TFhirMessageSignificanceCategory) to high(TFhirMessageSignificanceCategory) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMessageSignificanceCategoryList(i : Integer) : TFhirMessageSignificanceCategoryList;
var
  aLoop : TFhirMessageSignificanceCategory;
begin
  result := [];
  for aLoop := low(TFhirMessageSignificanceCategory) to high(TFhirMessageSignificanceCategory) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMessageConformanceEventModeListAsInteger(aSet : TFhirMessageConformanceEventModeList) : Integer;
var
  a : TFhirMessageConformanceEventMode;
begin
  result := 0;
  for a := low(TFhirMessageConformanceEventMode) to high(TFhirMessageConformanceEventMode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMessageConformanceEventModeList(i : Integer) : TFhirMessageConformanceEventModeList;
var
  aLoop : TFhirMessageConformanceEventMode;
begin
  result := [];
  for aLoop := low(TFhirMessageConformanceEventMode) to high(TFhirMessageConformanceEventMode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirDocumentModeListAsInteger(aSet : TFhirDocumentModeList) : Integer;
var
  a : TFhirDocumentMode;
begin
  result := 0;
  for a := low(TFhirDocumentMode) to high(TFhirDocumentMode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentModeList(i : Integer) : TFhirDocumentModeList;
var
  aLoop : TFhirDocumentMode;
begin
  result := [];
  for aLoop := low(TFhirDocumentMode) to high(TFhirDocumentMode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirDiagnosticOrderStatusListAsInteger(aSet : TFhirDiagnosticOrderStatusList) : Integer;
var
  a : TFhirDiagnosticOrderStatus;
begin
  result := 0;
  for a := low(TFhirDiagnosticOrderStatus) to high(TFhirDiagnosticOrderStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDiagnosticOrderStatusList(i : Integer) : TFhirDiagnosticOrderStatusList;
var
  aLoop : TFhirDiagnosticOrderStatus;
begin
  result := [];
  for aLoop := low(TFhirDiagnosticOrderStatus) to high(TFhirDiagnosticOrderStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirDiagnosticOrderPriorityListAsInteger(aSet : TFhirDiagnosticOrderPriorityList) : Integer;
var
  a : TFhirDiagnosticOrderPriority;
begin
  result := 0;
  for a := low(TFhirDiagnosticOrderPriority) to high(TFhirDiagnosticOrderPriority) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDiagnosticOrderPriorityList(i : Integer) : TFhirDiagnosticOrderPriorityList;
var
  aLoop : TFhirDiagnosticOrderPriority;
begin
  result := [];
  for aLoop := low(TFhirDiagnosticOrderPriority) to high(TFhirDiagnosticOrderPriority) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirDiagnosticReportStatusListAsInteger(aSet : TFhirDiagnosticReportStatusList) : Integer;
var
  a : TFhirDiagnosticReportStatus;
begin
  result := 0;
  for a := low(TFhirDiagnosticReportStatus) to high(TFhirDiagnosticReportStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDiagnosticReportStatusList(i : Integer) : TFhirDiagnosticReportStatusList;
var
  aLoop : TFhirDiagnosticReportStatus;
begin
  result := [];
  for aLoop := low(TFhirDiagnosticReportStatus) to high(TFhirDiagnosticReportStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirDocumentReferenceStatusListAsInteger(aSet : TFhirDocumentReferenceStatusList) : Integer;
var
  a : TFhirDocumentReferenceStatus;
begin
  result := 0;
  for a := low(TFhirDocumentReferenceStatus) to high(TFhirDocumentReferenceStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentReferenceStatusList(i : Integer) : TFhirDocumentReferenceStatusList;
var
  aLoop : TFhirDocumentReferenceStatus;
begin
  result := [];
  for aLoop := low(TFhirDocumentReferenceStatus) to high(TFhirDocumentReferenceStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirDocumentRelationshipTypeListAsInteger(aSet : TFhirDocumentRelationshipTypeList) : Integer;
var
  a : TFhirDocumentRelationshipType;
begin
  result := 0;
  for a := low(TFhirDocumentRelationshipType) to high(TFhirDocumentRelationshipType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentRelationshipTypeList(i : Integer) : TFhirDocumentRelationshipTypeList;
var
  aLoop : TFhirDocumentRelationshipType;
begin
  result := [];
  for aLoop := low(TFhirDocumentRelationshipType) to high(TFhirDocumentRelationshipType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirEncounterStateListAsInteger(aSet : TFhirEncounterStateList) : Integer;
var
  a : TFhirEncounterState;
begin
  result := 0;
  for a := low(TFhirEncounterState) to high(TFhirEncounterState) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEncounterStateList(i : Integer) : TFhirEncounterStateList;
var
  aLoop : TFhirEncounterState;
begin
  result := [];
  for aLoop := low(TFhirEncounterState) to high(TFhirEncounterState) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirEncounterClassListAsInteger(aSet : TFhirEncounterClassList) : Integer;
var
  a : TFhirEncounterClass;
begin
  result := 0;
  for a := low(TFhirEncounterClass) to high(TFhirEncounterClass) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEncounterClassList(i : Integer) : TFhirEncounterClassList;
var
  aLoop : TFhirEncounterClass;
begin
  result := [];
  for aLoop := low(TFhirEncounterClass) to high(TFhirEncounterClass) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirGroupTypeListAsInteger(aSet : TFhirGroupTypeList) : Integer;
var
  a : TFhirGroupType;
begin
  result := 0;
  for a := low(TFhirGroupType) to high(TFhirGroupType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGroupTypeList(i : Integer) : TFhirGroupTypeList;
var
  aLoop : TFhirGroupType;
begin
  result := [];
  for aLoop := low(TFhirGroupType) to high(TFhirGroupType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirImagingModalityListAsInteger(aSet : TFhirImagingModalityList) : Integer;
var
  a : TFhirImagingModality;
begin
  result := 0;
  for a := low(TFhirImagingModality) to high(TFhirImagingModality) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirImagingModalityList(i : Integer) : TFhirImagingModalityList;
var
  aLoop : TFhirImagingModality;
begin
  result := [];
  for aLoop := low(TFhirImagingModality) to high(TFhirImagingModality) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirInstanceAvailabilityListAsInteger(aSet : TFhirInstanceAvailabilityList) : Integer;
var
  a : TFhirInstanceAvailability;
begin
  result := 0;
  for a := low(TFhirInstanceAvailability) to high(TFhirInstanceAvailability) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirInstanceAvailabilityList(i : Integer) : TFhirInstanceAvailabilityList;
var
  aLoop : TFhirInstanceAvailability;
begin
  result := [];
  for aLoop := low(TFhirInstanceAvailability) to high(TFhirInstanceAvailability) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirModalityListAsInteger(aSet : TFhirModalityList) : Integer;
var
  a : TFhirModality;
begin
  result := 0;
  for a := low(TFhirModality) to high(TFhirModality) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirModalityList(i : Integer) : TFhirModalityList;
var
  aLoop : TFhirModality;
begin
  result := [];
  for aLoop := low(TFhirModality) to high(TFhirModality) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirListModeListAsInteger(aSet : TFhirListModeList) : Integer;
var
  a : TFhirListMode;
begin
  result := 0;
  for a := low(TFhirListMode) to high(TFhirListMode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirListModeList(i : Integer) : TFhirListModeList;
var
  aLoop : TFhirListMode;
begin
  result := [];
  for aLoop := low(TFhirListMode) to high(TFhirListMode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirLocationStatusListAsInteger(aSet : TFhirLocationStatusList) : Integer;
var
  a : TFhirLocationStatus;
begin
  result := 0;
  for a := low(TFhirLocationStatus) to high(TFhirLocationStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLocationStatusList(i : Integer) : TFhirLocationStatusList;
var
  aLoop : TFhirLocationStatus;
begin
  result := [];
  for aLoop := low(TFhirLocationStatus) to high(TFhirLocationStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirLocationModeListAsInteger(aSet : TFhirLocationModeList) : Integer;
var
  a : TFhirLocationMode;
begin
  result := 0;
  for a := low(TFhirLocationMode) to high(TFhirLocationMode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLocationModeList(i : Integer) : TFhirLocationModeList;
var
  aLoop : TFhirLocationMode;
begin
  result := [];
  for aLoop := low(TFhirLocationMode) to high(TFhirLocationMode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMediaTypeListAsInteger(aSet : TFhirMediaTypeList) : Integer;
var
  a : TFhirMediaType;
begin
  result := 0;
  for a := low(TFhirMediaType) to high(TFhirMediaType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMediaTypeList(i : Integer) : TFhirMediaTypeList;
var
  aLoop : TFhirMediaType;
begin
  result := [];
  for aLoop := low(TFhirMediaType) to high(TFhirMediaType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMedicationKindListAsInteger(aSet : TFhirMedicationKindList) : Integer;
var
  a : TFhirMedicationKind;
begin
  result := 0;
  for a := low(TFhirMedicationKind) to high(TFhirMedicationKind) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationKindList(i : Integer) : TFhirMedicationKindList;
var
  aLoop : TFhirMedicationKind;
begin
  result := [];
  for aLoop := low(TFhirMedicationKind) to high(TFhirMedicationKind) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMedicationAdminStatusListAsInteger(aSet : TFhirMedicationAdminStatusList) : Integer;
var
  a : TFhirMedicationAdminStatus;
begin
  result := 0;
  for a := low(TFhirMedicationAdminStatus) to high(TFhirMedicationAdminStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationAdminStatusList(i : Integer) : TFhirMedicationAdminStatusList;
var
  aLoop : TFhirMedicationAdminStatus;
begin
  result := [];
  for aLoop := low(TFhirMedicationAdminStatus) to high(TFhirMedicationAdminStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMedicationDispenseStatusListAsInteger(aSet : TFhirMedicationDispenseStatusList) : Integer;
var
  a : TFhirMedicationDispenseStatus;
begin
  result := 0;
  for a := low(TFhirMedicationDispenseStatus) to high(TFhirMedicationDispenseStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationDispenseStatusList(i : Integer) : TFhirMedicationDispenseStatusList;
var
  aLoop : TFhirMedicationDispenseStatus;
begin
  result := [];
  for aLoop := low(TFhirMedicationDispenseStatus) to high(TFhirMedicationDispenseStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMedicationPrescriptionStatusListAsInteger(aSet : TFhirMedicationPrescriptionStatusList) : Integer;
var
  a : TFhirMedicationPrescriptionStatus;
begin
  result := 0;
  for a := low(TFhirMedicationPrescriptionStatus) to high(TFhirMedicationPrescriptionStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationPrescriptionStatusList(i : Integer) : TFhirMedicationPrescriptionStatusList;
var
  aLoop : TFhirMedicationPrescriptionStatus;
begin
  result := [];
  for aLoop := low(TFhirMedicationPrescriptionStatus) to high(TFhirMedicationPrescriptionStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirResponseCodeListAsInteger(aSet : TFhirResponseCodeList) : Integer;
var
  a : TFhirResponseCode;
begin
  result := 0;
  for a := low(TFhirResponseCode) to high(TFhirResponseCode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResponseCodeList(i : Integer) : TFhirResponseCodeList;
var
  aLoop : TFhirResponseCode;
begin
  result := [];
  for aLoop := low(TFhirResponseCode) to high(TFhirResponseCode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirObservationStatusListAsInteger(aSet : TFhirObservationStatusList) : Integer;
var
  a : TFhirObservationStatus;
begin
  result := 0;
  for a := low(TFhirObservationStatus) to high(TFhirObservationStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationStatusList(i : Integer) : TFhirObservationStatusList;
var
  aLoop : TFhirObservationStatus;
begin
  result := [];
  for aLoop := low(TFhirObservationStatus) to high(TFhirObservationStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirObservationReliabilityListAsInteger(aSet : TFhirObservationReliabilityList) : Integer;
var
  a : TFhirObservationReliability;
begin
  result := 0;
  for a := low(TFhirObservationReliability) to high(TFhirObservationReliability) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationReliabilityList(i : Integer) : TFhirObservationReliabilityList;
var
  aLoop : TFhirObservationReliability;
begin
  result := [];
  for aLoop := low(TFhirObservationReliability) to high(TFhirObservationReliability) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirObservationRelationshiptypesListAsInteger(aSet : TFhirObservationRelationshiptypesList) : Integer;
var
  a : TFhirObservationRelationshiptypes;
begin
  result := 0;
  for a := low(TFhirObservationRelationshiptypes) to high(TFhirObservationRelationshiptypes) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationRelationshiptypesList(i : Integer) : TFhirObservationRelationshiptypesList;
var
  aLoop : TFhirObservationRelationshiptypes;
begin
  result := [];
  for aLoop := low(TFhirObservationRelationshiptypes) to high(TFhirObservationRelationshiptypes) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirIssueSeverityListAsInteger(aSet : TFhirIssueSeverityList) : Integer;
var
  a : TFhirIssueSeverity;
begin
  result := 0;
  for a := low(TFhirIssueSeverity) to high(TFhirIssueSeverity) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIssueSeverityList(i : Integer) : TFhirIssueSeverityList;
var
  aLoop : TFhirIssueSeverity;
begin
  result := [];
  for aLoop := low(TFhirIssueSeverity) to high(TFhirIssueSeverity) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirOrderOutcomeCodeListAsInteger(aSet : TFhirOrderOutcomeCodeList) : Integer;
var
  a : TFhirOrderOutcomeCode;
begin
  result := 0;
  for a := low(TFhirOrderOutcomeCode) to high(TFhirOrderOutcomeCode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOrderOutcomeCodeList(i : Integer) : TFhirOrderOutcomeCodeList;
var
  aLoop : TFhirOrderOutcomeCode;
begin
  result := [];
  for aLoop := low(TFhirOrderOutcomeCode) to high(TFhirOrderOutcomeCode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirLinkTypeListAsInteger(aSet : TFhirLinkTypeList) : Integer;
var
  a : TFhirLinkType;
begin
  result := 0;
  for a := low(TFhirLinkType) to high(TFhirLinkType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLinkTypeList(i : Integer) : TFhirLinkTypeList;
var
  aLoop : TFhirLinkType;
begin
  result := [];
  for aLoop := low(TFhirLinkType) to high(TFhirLinkType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirProcedureRelationshipTypeListAsInteger(aSet : TFhirProcedureRelationshipTypeList) : Integer;
var
  a : TFhirProcedureRelationshipType;
begin
  result := 0;
  for a := low(TFhirProcedureRelationshipType) to high(TFhirProcedureRelationshipType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProcedureRelationshipTypeList(i : Integer) : TFhirProcedureRelationshipTypeList;
var
  aLoop : TFhirProcedureRelationshipType;
begin
  result := [];
  for aLoop := low(TFhirProcedureRelationshipType) to high(TFhirProcedureRelationshipType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirResourceProfileStatusListAsInteger(aSet : TFhirResourceProfileStatusList) : Integer;
var
  a : TFhirResourceProfileStatus;
begin
  result := 0;
  for a := low(TFhirResourceProfileStatus) to high(TFhirResourceProfileStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResourceProfileStatusList(i : Integer) : TFhirResourceProfileStatusList;
var
  aLoop : TFhirResourceProfileStatus;
begin
  result := [];
  for aLoop := low(TFhirResourceProfileStatus) to high(TFhirResourceProfileStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirPropertyRepresentationListAsInteger(aSet : TFhirPropertyRepresentationList) : Integer;
var
  a : TFhirPropertyRepresentation;
begin
  result := 0;
  for a := low(TFhirPropertyRepresentation) to high(TFhirPropertyRepresentation) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPropertyRepresentationList(i : Integer) : TFhirPropertyRepresentationList;
var
  aLoop : TFhirPropertyRepresentation;
begin
  result := [];
  for aLoop := low(TFhirPropertyRepresentation) to high(TFhirPropertyRepresentation) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirResourceSlicingRulesListAsInteger(aSet : TFhirResourceSlicingRulesList) : Integer;
var
  a : TFhirResourceSlicingRules;
begin
  result := 0;
  for a := low(TFhirResourceSlicingRules) to high(TFhirResourceSlicingRules) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResourceSlicingRulesList(i : Integer) : TFhirResourceSlicingRulesList;
var
  aLoop : TFhirResourceSlicingRules;
begin
  result := [];
  for aLoop := low(TFhirResourceSlicingRules) to high(TFhirResourceSlicingRules) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirResourceAggregationModeListAsInteger(aSet : TFhirResourceAggregationModeList) : Integer;
var
  a : TFhirResourceAggregationMode;
begin
  result := 0;
  for a := low(TFhirResourceAggregationMode) to high(TFhirResourceAggregationMode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResourceAggregationModeList(i : Integer) : TFhirResourceAggregationModeList;
var
  aLoop : TFhirResourceAggregationMode;
begin
  result := [];
  for aLoop := low(TFhirResourceAggregationMode) to high(TFhirResourceAggregationMode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirConstraintSeverityListAsInteger(aSet : TFhirConstraintSeverityList) : Integer;
var
  a : TFhirConstraintSeverity;
begin
  result := 0;
  for a := low(TFhirConstraintSeverity) to high(TFhirConstraintSeverity) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConstraintSeverityList(i : Integer) : TFhirConstraintSeverityList;
var
  aLoop : TFhirConstraintSeverity;
begin
  result := [];
  for aLoop := low(TFhirConstraintSeverity) to high(TFhirConstraintSeverity) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirBindingConformanceListAsInteger(aSet : TFhirBindingConformanceList) : Integer;
var
  a : TFhirBindingConformance;
begin
  result := 0;
  for a := low(TFhirBindingConformance) to high(TFhirBindingConformance) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBindingConformanceList(i : Integer) : TFhirBindingConformanceList;
var
  aLoop : TFhirBindingConformance;
begin
  result := [];
  for aLoop := low(TFhirBindingConformance) to high(TFhirBindingConformance) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirExtensionContextListAsInteger(aSet : TFhirExtensionContextList) : Integer;
var
  a : TFhirExtensionContext;
begin
  result := 0;
  for a := low(TFhirExtensionContext) to high(TFhirExtensionContext) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirExtensionContextList(i : Integer) : TFhirExtensionContextList;
var
  aLoop : TFhirExtensionContext;
begin
  result := [];
  for aLoop := low(TFhirExtensionContext) to high(TFhirExtensionContext) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirProvenanceEntityRoleListAsInteger(aSet : TFhirProvenanceEntityRoleList) : Integer;
var
  a : TFhirProvenanceEntityRole;
begin
  result := 0;
  for a := low(TFhirProvenanceEntityRole) to high(TFhirProvenanceEntityRole) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProvenanceEntityRoleList(i : Integer) : TFhirProvenanceEntityRoleList;
var
  aLoop : TFhirProvenanceEntityRole;
begin
  result := [];
  for aLoop := low(TFhirProvenanceEntityRole) to high(TFhirProvenanceEntityRole) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirQueryOutcomeListAsInteger(aSet : TFhirQueryOutcomeList) : Integer;
var
  a : TFhirQueryOutcome;
begin
  result := 0;
  for a := low(TFhirQueryOutcome) to high(TFhirQueryOutcome) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQueryOutcomeList(i : Integer) : TFhirQueryOutcomeList;
var
  aLoop : TFhirQueryOutcome;
begin
  result := [];
  for aLoop := low(TFhirQueryOutcome) to high(TFhirQueryOutcome) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirQuestionnaireStatusListAsInteger(aSet : TFhirQuestionnaireStatusList) : Integer;
var
  a : TFhirQuestionnaireStatus;
begin
  result := 0;
  for a := low(TFhirQuestionnaireStatus) to high(TFhirQuestionnaireStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireStatusList(i : Integer) : TFhirQuestionnaireStatusList;
var
  aLoop : TFhirQuestionnaireStatus;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireStatus) to high(TFhirQuestionnaireStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSecurityEventActionListAsInteger(aSet : TFhirSecurityEventActionList) : Integer;
var
  a : TFhirSecurityEventAction;
begin
  result := 0;
  for a := low(TFhirSecurityEventAction) to high(TFhirSecurityEventAction) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSecurityEventActionList(i : Integer) : TFhirSecurityEventActionList;
var
  aLoop : TFhirSecurityEventAction;
begin
  result := [];
  for aLoop := low(TFhirSecurityEventAction) to high(TFhirSecurityEventAction) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSecurityEventOutcomeListAsInteger(aSet : TFhirSecurityEventOutcomeList) : Integer;
var
  a : TFhirSecurityEventOutcome;
begin
  result := 0;
  for a := low(TFhirSecurityEventOutcome) to high(TFhirSecurityEventOutcome) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSecurityEventOutcomeList(i : Integer) : TFhirSecurityEventOutcomeList;
var
  aLoop : TFhirSecurityEventOutcome;
begin
  result := [];
  for aLoop := low(TFhirSecurityEventOutcome) to high(TFhirSecurityEventOutcome) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirNetworkTypeListAsInteger(aSet : TFhirNetworkTypeList) : Integer;
var
  a : TFhirNetworkType;
begin
  result := 0;
  for a := low(TFhirNetworkType) to high(TFhirNetworkType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNetworkTypeList(i : Integer) : TFhirNetworkTypeList;
var
  aLoop : TFhirNetworkType;
begin
  result := [];
  for aLoop := low(TFhirNetworkType) to high(TFhirNetworkType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirObjectTypeListAsInteger(aSet : TFhirObjectTypeList) : Integer;
var
  a : TFhirObjectType;
begin
  result := 0;
  for a := low(TFhirObjectType) to high(TFhirObjectType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObjectTypeList(i : Integer) : TFhirObjectTypeList;
var
  aLoop : TFhirObjectType;
begin
  result := [];
  for aLoop := low(TFhirObjectType) to high(TFhirObjectType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirObjectRoleListAsInteger(aSet : TFhirObjectRoleList) : Integer;
var
  a : TFhirObjectRole;
begin
  result := 0;
  for a := low(TFhirObjectRole) to high(TFhirObjectRole) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObjectRoleList(i : Integer) : TFhirObjectRoleList;
var
  aLoop : TFhirObjectRole;
begin
  result := [];
  for aLoop := low(TFhirObjectRole) to high(TFhirObjectRole) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirObjectLifecycleListAsInteger(aSet : TFhirObjectLifecycleList) : Integer;
var
  a : TFhirObjectLifecycle;
begin
  result := 0;
  for a := low(TFhirObjectLifecycle) to high(TFhirObjectLifecycle) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObjectLifecycleList(i : Integer) : TFhirObjectLifecycleList;
var
  aLoop : TFhirObjectLifecycle;
begin
  result := [];
  for aLoop := low(TFhirObjectLifecycle) to high(TFhirObjectLifecycle) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirHierarchicalRelationshipTypeListAsInteger(aSet : TFhirHierarchicalRelationshipTypeList) : Integer;
var
  a : TFhirHierarchicalRelationshipType;
begin
  result := 0;
  for a := low(TFhirHierarchicalRelationshipType) to high(TFhirHierarchicalRelationshipType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirHierarchicalRelationshipTypeList(i : Integer) : TFhirHierarchicalRelationshipTypeList;
var
  aLoop : TFhirHierarchicalRelationshipType;
begin
  result := [];
  for aLoop := low(TFhirHierarchicalRelationshipType) to high(TFhirHierarchicalRelationshipType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirValuesetSupplyStatusListAsInteger(aSet : TFhirValuesetSupplyStatusList) : Integer;
var
  a : TFhirValuesetSupplyStatus;
begin
  result := 0;
  for a := low(TFhirValuesetSupplyStatus) to high(TFhirValuesetSupplyStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirValuesetSupplyStatusList(i : Integer) : TFhirValuesetSupplyStatusList;
var
  aLoop : TFhirValuesetSupplyStatus;
begin
  result := [];
  for aLoop := low(TFhirValuesetSupplyStatus) to high(TFhirValuesetSupplyStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirValuesetSupplyDispenseStatusListAsInteger(aSet : TFhirValuesetSupplyDispenseStatusList) : Integer;
var
  a : TFhirValuesetSupplyDispenseStatus;
begin
  result := 0;
  for a := low(TFhirValuesetSupplyDispenseStatus) to high(TFhirValuesetSupplyDispenseStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirValuesetSupplyDispenseStatusList(i : Integer) : TFhirValuesetSupplyDispenseStatusList;
var
  aLoop : TFhirValuesetSupplyDispenseStatus;
begin
  result := [];
  for aLoop := low(TFhirValuesetSupplyDispenseStatus) to high(TFhirValuesetSupplyDispenseStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirFilterOperatorListAsInteger(aSet : TFhirFilterOperatorList) : Integer;
var
  a : TFhirFilterOperator;
begin
  result := 0;
  for a := low(TFhirFilterOperator) to high(TFhirFilterOperator) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFilterOperatorList(i : Integer) : TFhirFilterOperatorList;
var
  aLoop : TFhirFilterOperator;
begin
  result := [];
  for aLoop := low(TFhirFilterOperator) to high(TFhirFilterOperator) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


end.

