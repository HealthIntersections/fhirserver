{!Wrapper uses FHIRBase, FHIRBase_Wrapper}

unit FHIRTypes;

{
  Copyright (c) 2011+, HL7, Inc.
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


{$IFDEF FHIR-DSTU}
This is the dev branch of the FHIR code
{$ENDIF}

interface

// FHIR v1.0.0 generated Tue, Sep 15, 2015 12:58+1000

uses
  Classes, SysUtils, DecimalSupport, StringSupport, AdvBuffers, EncdDecd, DateAndTime, FHIRBase;

Type
  {@Enum TFhirNarrativeStatus
    The status of a resource narrative
  }
  TFhirNarrativeStatus = (
    NarrativeStatusNull,  {@enum.value NarrativeStatusNull Value is missing from Instance }
    NarrativeStatusGenerated, {@enum.value NarrativeStatusGenerated The contents of the narrative are entirely generated from the structured data in the content. }
    NarrativeStatusExtensions, {@enum.value NarrativeStatusExtensions The contents of the narrative are entirely generated from the structured data in the content and some of the content is generated from extensions }
    NarrativeStatusAdditional, {@enum.value NarrativeStatusAdditional The contents of the narrative contain additional information not found in the structured data }
    NarrativeStatusEmpty); {@enum.value NarrativeStatusEmpty The contents of the narrative are some equivalent of "No human-readable text provided in this case" }
  TFhirNarrativeStatusList = set of TFhirNarrativeStatus;

  {@Enum TFhirQuantityComparator
    How the Quantity should be understood and represented
  }
  TFhirQuantityComparator = (
    QuantityComparatorNull,  {@enum.value QuantityComparatorNull Value is missing from Instance }
    QuantityComparatorLessThan, {@enum.value QuantityComparatorLessThan The actual value is less than the given value }
    QuantityComparatorLessOrEquals, {@enum.value QuantityComparatorLessOrEquals The actual value is less than or equal to the given value }
    QuantityComparatorGreaterOrEquals, {@enum.value QuantityComparatorGreaterOrEquals The actual value is greater than or equal to the given value }
    QuantityComparatorGreaterThan); {@enum.value QuantityComparatorGreaterThan The actual value is greater than the given value }
  TFhirQuantityComparatorList = set of TFhirQuantityComparator;

  {@Enum TFhirIdentifierUse
    Identifies the purpose for this identifier, if known
  }
  TFhirIdentifierUse = (
    IdentifierUseNull,  {@enum.value IdentifierUseNull Value is missing from Instance }
    IdentifierUseUsual, {@enum.value IdentifierUseUsual the identifier recommended for display and use in real-world interactions }
    IdentifierUseOfficial, {@enum.value IdentifierUseOfficial the identifier considered to be most trusted for the identification of this item }
    IdentifierUseTemp, {@enum.value IdentifierUseTemp A temporary identifier }
    IdentifierUseSecondary); {@enum.value IdentifierUseSecondary An identifier that was assigned in secondary use - it serves to identify the object in a relative context, but cannot be consistently assigned to the same object again in a different context }
  TFhirIdentifierUseList = set of TFhirIdentifierUse;

  {@Enum TFhirPropertyRepresentation
    How a property is represented on the wire.
  }
  TFhirPropertyRepresentation = (
    PropertyRepresentationNull,  {@enum.value PropertyRepresentationNull Value is missing from Instance }
    PropertyRepresentationXmlAttr); {@enum.value PropertyRepresentationXmlAttr In XML, this property is represented as an attribute not an element. }
  TFhirPropertyRepresentationList = set of TFhirPropertyRepresentation;

  {@Enum TFhirResourceSlicingRules
    How slices are interpreted when evaluating an instance.
  }
  TFhirResourceSlicingRules = (
    ResourceSlicingRulesNull,  {@enum.value ResourceSlicingRulesNull Value is missing from Instance }
    ResourceSlicingRulesClosed, {@enum.value ResourceSlicingRulesClosed No additional content is allowed other than that described by the slices in this profile. }
    ResourceSlicingRulesOpen, {@enum.value ResourceSlicingRulesOpen Additional content is allowed anywhere in the list. }
    ResourceSlicingRulesOpenAtEnd); {@enum.value ResourceSlicingRulesOpenAtEnd Additional content is allowed, but only at the end of the list. Note that using this requires that the slices be ordered, which makes it hard to share uses. This should only be done where absolutely required. }
  TFhirResourceSlicingRulesList = set of TFhirResourceSlicingRules;

  {@Enum TFhirResourceAggregationMode
    How resource references can be aggregated.
  }
  TFhirResourceAggregationMode = (
    ResourceAggregationModeNull,  {@enum.value ResourceAggregationModeNull Value is missing from Instance }
    ResourceAggregationModeContained, {@enum.value ResourceAggregationModeContained The reference is a local reference to a contained resource. }
    ResourceAggregationModeReferenced, {@enum.value ResourceAggregationModeReferenced The reference to a resource that has to be resolved externally to the resource that includes the reference. }
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

  {@Enum TFhirBindingStrength
    Indication of the degree of conformance expectations associated with a binding.
  }
  TFhirBindingStrength = (
    BindingStrengthNull,  {@enum.value BindingStrengthNull Value is missing from Instance }
    BindingStrengthRequired, {@enum.value BindingStrengthRequired To be conformant, instances of this element SHALL include a code from the specified value set. }
    BindingStrengthExtensible, {@enum.value BindingStrengthExtensible To be conformant, instances of this element SHALL include a code from the specified value set if any of the codes within the value set can apply to the concept being communicated.  If the valueset does not cover the concept (based on human review), alternate codings (or, data type allowing, text) may be included instead. }
    BindingStrengthPreferred, {@enum.value BindingStrengthPreferred Instances are encouraged to draw from the specified codes for interoperability purposes but are not required to do so to be considered conformant. }
    BindingStrengthExample); {@enum.value BindingStrengthExample Instances are not expected or even encouraged to draw from the specified value set.  The value set merely provides examples of the types of concepts intended to be included. }
  TFhirBindingStrengthList = set of TFhirBindingStrength;

  {@Enum TFhirUnitsOfTime
    A unit of time (units from UCUM)
  }
  TFhirUnitsOfTime = (
    UnitsOfTimeNull,  {@enum.value UnitsOfTimeNull Value is missing from Instance }
    UnitsOfTimeS, {@enum.value UnitsOfTimeS  }
    UnitsOfTimeMin, {@enum.value UnitsOfTimeMin  }
    UnitsOfTimeH, {@enum.value UnitsOfTimeH  }
    UnitsOfTimeD, {@enum.value UnitsOfTimeD  }
    UnitsOfTimeWk, {@enum.value UnitsOfTimeWk  }
    UnitsOfTimeMo, {@enum.value UnitsOfTimeMo  }
    UnitsOfTimeA); {@enum.value UnitsOfTimeA  }
  TFhirUnitsOfTimeList = set of TFhirUnitsOfTime;

  {@Enum TFhirEventTiming
    Real world event that the schedule relates to
  }
  TFhirEventTiming = (
    EventTimingNull,  {@enum.value EventTimingNull Value is missing from Instance }
    EventTimingHS, {@enum.value EventTimingHS  }
    EventTimingWAKE, {@enum.value EventTimingWAKE  }
    EventTimingC, {@enum.value EventTimingC  }
    EventTimingCM, {@enum.value EventTimingCM  }
    EventTimingCD, {@enum.value EventTimingCD  }
    EventTimingCV, {@enum.value EventTimingCV  }
    EventTimingAC, {@enum.value EventTimingAC  }
    EventTimingACM, {@enum.value EventTimingACM  }
    EventTimingACD, {@enum.value EventTimingACD  }
    EventTimingACV, {@enum.value EventTimingACV  }
    EventTimingPC, {@enum.value EventTimingPC  }
    EventTimingPCM, {@enum.value EventTimingPCM  }
    EventTimingPCD, {@enum.value EventTimingPCD  }
    EventTimingPCV); {@enum.value EventTimingPCV  }
  TFhirEventTimingList = set of TFhirEventTiming;

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

  {@Enum TFhirAddressType
    The type of an address (physical / postal)
  }
  TFhirAddressType = (
    AddressTypeNull,  {@enum.value AddressTypeNull Value is missing from Instance }
    AddressTypePostal, {@enum.value AddressTypePostal Mailing addresses - PO Boxes and care-of addresses. }
    AddressTypePhysical, {@enum.value AddressTypePhysical A physical address that can be visited. }
    AddressTypeBoth); {@enum.value AddressTypeBoth An address that is both physical and postal. }
  TFhirAddressTypeList = set of TFhirAddressType;

  {@Enum TFhirNameUse
    The use of a human name
  }
  TFhirNameUse = (
    NameUseNull,  {@enum.value NameUseNull Value is missing from Instance }
    NameUseUsual, {@enum.value NameUseUsual Known as/conventional/the one you normally use }
    NameUseOfficial, {@enum.value NameUseOfficial The formal name as registered in an official (government) registry, but which name might not be commonly used. May be called "legal name". }
    NameUseTemp, {@enum.value NameUseTemp A temporary name. Name.period can provide more detailed information. This may also be used for temporary names assigned at birth or in emergency situations. }
    NameUseNickname, {@enum.value NameUseNickname A name that is used to address the person in an informal manner, but is not part of their formal or usual name }
    NameUseAnonymous, {@enum.value NameUseAnonymous Anonymous assigned name, alias, or pseudonym (used to protect a person's identity for privacy reasons) }
    NameUseOld, {@enum.value NameUseOld This name is no longer in use (or was never correct, but retained for records) }
    NameUseMaiden); {@enum.value NameUseMaiden A name used prior to marriage. Marriage naming customs vary greatly around the world. This name use is for use by applications that collect and store "maiden" names. Though the concept of maiden name is often gender specific, the use of this term is not gender specific. The use of this term does not imply any particular history for a person's name, nor should the maiden name be determined algorithmically. }
  TFhirNameUseList = set of TFhirNameUse;

  {@Enum TFhirContactPointSystem
    Telecommunications form for contact point
  }
  TFhirContactPointSystem = (
    ContactPointSystemNull,  {@enum.value ContactPointSystemNull Value is missing from Instance }
    ContactPointSystemPhone, {@enum.value ContactPointSystemPhone The value is a telephone number used for voice calls. Use of full international numbers starting with + is recommended to enable automatic dialing support but not required. }
    ContactPointSystemFax, {@enum.value ContactPointSystemFax The value is a fax machine. Use of full international numbers starting with + is recommended to enable automatic dialing support but not required. }
    ContactPointSystemEmail, {@enum.value ContactPointSystemEmail The value is an email address }
    ContactPointSystemPager, {@enum.value ContactPointSystemPager The value is a pager number. These may be local pager numbers that are only usable on a particular pager system }
    ContactPointSystemOther); {@enum.value ContactPointSystemOther A contact that is not a phone, fax, or email address. The format of the value SHOULD be a URL. This is intended for various personal contacts including blogs, Twitter, Facebook, etc. Do not use for email addresses. If this is not a URL, then it will require human interpretation }
  TFhirContactPointSystemList = set of TFhirContactPointSystem;

  {@Enum TFhirContactPointUse
    Use of contact point
  }
  TFhirContactPointUse = (
    ContactPointUseNull,  {@enum.value ContactPointUseNull Value is missing from Instance }
    ContactPointUseHome, {@enum.value ContactPointUseHome A communication contact point at a home; attempted contacts for business purposes might intrude privacy and chances are one will contact family or other household members instead of the person one wishes to call. Typically used with urgent cases, or if no other contacts are available. }
    ContactPointUseWork, {@enum.value ContactPointUseWork An office contact point. First choice for business related contacts during business hours. }
    ContactPointUseTemp, {@enum.value ContactPointUseTemp A temporary contact point. The period can provide more detailed information. }
    ContactPointUseOld, {@enum.value ContactPointUseOld This contact point is no longer in use (or was never correct, but retained for records) }
    ContactPointUseMobile); {@enum.value ContactPointUseMobile A telecommunication device that moves and stays with its owner. May have characteristics of all other use codes, suitable for urgent matters, not the first choice for routine business }
  TFhirContactPointUseList = set of TFhirContactPointUse;

  {@Enum TFhirAccountStatus
    Indicates whether the account is available to be used.
  }
  TFhirAccountStatus = (
    AccountStatusNull,  {@enum.value AccountStatusNull Value is missing from Instance }
    AccountStatusActive, {@enum.value AccountStatusActive This account is active and may be used. }
    AccountStatusInactive); {@enum.value AccountStatusInactive This account is inactive and should not be used to track financial information. }
  TFhirAccountStatusList = set of TFhirAccountStatus;

  {@Enum TFhirAllergyIntoleranceStatus
    Assertion about certainty associated with a propensity, or potential risk, of a reaction to the identified Substance.
  }
  TFhirAllergyIntoleranceStatus = (
    AllergyIntoleranceStatusNull,  {@enum.value AllergyIntoleranceStatusNull Value is missing from Instance }
    AllergyIntoleranceStatusActive, {@enum.value AllergyIntoleranceStatusActive An active record of a reaction to the identified Substance. }
    AllergyIntoleranceStatusUnconfirmed, {@enum.value AllergyIntoleranceStatusUnconfirmed A low level of certainty about the propensity for a reaction to the identified Substance. }
    AllergyIntoleranceStatusConfirmed, {@enum.value AllergyIntoleranceStatusConfirmed A high level of certainty about the propensity for a reaction to the identified Substance, which may include clinical evidence by testing or rechallenge. }
    AllergyIntoleranceStatusInactive, {@enum.value AllergyIntoleranceStatusInactive An inactive record of a reaction to the identified Substance. }
    AllergyIntoleranceStatusResolved, {@enum.value AllergyIntoleranceStatusResolved A reaction to the identified Substance has been clinically reassessed by testing or rechallenge and considered to be resolved. }
    AllergyIntoleranceStatusRefuted, {@enum.value AllergyIntoleranceStatusRefuted A propensity for a reaction to the identified Substance has been disproven with a high level of clinical certainty, which may include testing or rechallenge, and is refuted. }
    AllergyIntoleranceStatusEnteredInError); {@enum.value AllergyIntoleranceStatusEnteredInError The statement was entered in error and is not valid. }
  TFhirAllergyIntoleranceStatusList = set of TFhirAllergyIntoleranceStatus;

  {@Enum TFhirAllergyIntoleranceCriticality
    Estimate of the potential clinical harm, or seriousness, of a reaction to an identified Substance.
  }
  TFhirAllergyIntoleranceCriticality = (
    AllergyIntoleranceCriticalityNull,  {@enum.value AllergyIntoleranceCriticalityNull Value is missing from Instance }
    AllergyIntoleranceCriticalityCRITL, {@enum.value AllergyIntoleranceCriticalityCRITL The potential clinical impact of a future reaction is estimated as low risk: exposure to substance is unlikely to result in a life threatening or organ system threatening outcome. Future exposure to the Substance is considered a relative contra-indication. }
    AllergyIntoleranceCriticalityCRITH, {@enum.value AllergyIntoleranceCriticalityCRITH The potential clinical impact of a future reaction is estimated as high risk: exposure to substance may result in a life threatening or organ system threatening outcome. Future exposure to the Substance may be considered an absolute contra-indication. }
    AllergyIntoleranceCriticalityCRITU); {@enum.value AllergyIntoleranceCriticalityCRITU Unable to assess the potential clinical impact with the information available. }
  TFhirAllergyIntoleranceCriticalityList = set of TFhirAllergyIntoleranceCriticality;

  {@Enum TFhirAllergyIntoleranceType
    Identification of the underlying physiological mechanism for a Reaction Risk.
  }
  TFhirAllergyIntoleranceType = (
    AllergyIntoleranceTypeNull,  {@enum.value AllergyIntoleranceTypeNull Value is missing from Instance }
    AllergyIntoleranceTypeAllergy, {@enum.value AllergyIntoleranceTypeAllergy A propensity for hypersensitivity reaction(s) to a substance.  These reactions are most typically type I hypersensitivity, plus other "allergy-like" reactions, including pseudoallergy. }
    AllergyIntoleranceTypeIntolerance); {@enum.value AllergyIntoleranceTypeIntolerance A propensity for adverse reactions to a substance that is not judged to be allergic or "allergy-like".  These reactions are typically (but not necessarily) non-immune.  They are to some degree idiosyncratic and/or individually specific (i.e. are not a reaction that is expected to occur with most or all patients given similar circumstances). }
  TFhirAllergyIntoleranceTypeList = set of TFhirAllergyIntoleranceType;

  {@Enum TFhirAllergyIntoleranceCategory
    Category of an identified Substance.
  }
  TFhirAllergyIntoleranceCategory = (
    AllergyIntoleranceCategoryNull,  {@enum.value AllergyIntoleranceCategoryNull Value is missing from Instance }
    AllergyIntoleranceCategoryFood, {@enum.value AllergyIntoleranceCategoryFood Any substance consumed to provide nutritional support for the body. }
    AllergyIntoleranceCategoryMedication, {@enum.value AllergyIntoleranceCategoryMedication Substances administered to achieve a physiological effect. }
    AllergyIntoleranceCategoryEnvironment, {@enum.value AllergyIntoleranceCategoryEnvironment Substances that are encountered in the environment. }
    AllergyIntoleranceCategoryOther); {@enum.value AllergyIntoleranceCategoryOther Other substances that are not covered by any other category. }
  TFhirAllergyIntoleranceCategoryList = set of TFhirAllergyIntoleranceCategory;

  {@Enum TFhirReactionEventCertainty
    Statement about the degree of clinical certainty that a Specific Substance was the cause of the Manifestation in an reaction event.
  }
  TFhirReactionEventCertainty = (
    ReactionEventCertaintyNull,  {@enum.value ReactionEventCertaintyNull Value is missing from Instance }
    ReactionEventCertaintyUnlikely, {@enum.value ReactionEventCertaintyUnlikely There is a low level of clinical certainty that the reaction was caused by the identified Substance. }
    ReactionEventCertaintyLikely, {@enum.value ReactionEventCertaintyLikely There is a high level of clinical certainty that the reaction was caused by the identified Substance. }
    ReactionEventCertaintyConfirmed); {@enum.value ReactionEventCertaintyConfirmed There is a very high level of clinical certainty that the reaction was due to the identified Substance, which may include clinical evidence by testing or rechallenge. }
  TFhirReactionEventCertaintyList = set of TFhirReactionEventCertainty;

  {@Enum TFhirReactionEventSeverity
    Clinical assessment of the severity of a reaction event as a whole, potentially considering multiple different manifestations.
  }
  TFhirReactionEventSeverity = (
    ReactionEventSeverityNull,  {@enum.value ReactionEventSeverityNull Value is missing from Instance }
    ReactionEventSeverityMild, {@enum.value ReactionEventSeverityMild Causes mild physiological effects. }
    ReactionEventSeverityModerate, {@enum.value ReactionEventSeverityModerate Causes moderate physiological effects. }
    ReactionEventSeveritySevere); {@enum.value ReactionEventSeveritySevere Causes severe physiological effects. }
  TFhirReactionEventSeverityList = set of TFhirReactionEventSeverity;

  {@Enum TFhirAppointmentstatus
    The free/busy status of an appointment.
  }
  TFhirAppointmentstatus = (
    AppointmentstatusNull,  {@enum.value AppointmentstatusNull Value is missing from Instance }
    AppointmentstatusProposed, {@enum.value AppointmentstatusProposed None of the participant(s) have finalized their acceptance of the appointment request, and the start/end time may not be set yet. }
    AppointmentstatusPending, {@enum.value AppointmentstatusPending Some or all of the participant(s) have not finalized their acceptance of the appointment request. }
    AppointmentstatusBooked, {@enum.value AppointmentstatusBooked All participant(s) have been considered and the appointment is confirmed to go ahead at the date/times specified. }
    AppointmentstatusArrived, {@enum.value AppointmentstatusArrived Some of the patients have arrived. }
    AppointmentstatusFulfilled, {@enum.value AppointmentstatusFulfilled This appointment has completed and may have resulted in an encounter. }
    AppointmentstatusCancelled, {@enum.value AppointmentstatusCancelled The appointment has been cancelled. }
    AppointmentstatusNoshow); {@enum.value AppointmentstatusNoshow Some or all of the participant(s) have not/did not appear for the appointment (usually the patient). }
  TFhirAppointmentstatusList = set of TFhirAppointmentstatus;

  {@Enum TFhirParticipantrequired
    Is the Participant required to attend the appointment.
  }
  TFhirParticipantrequired = (
    ParticipantrequiredNull,  {@enum.value ParticipantrequiredNull Value is missing from Instance }
    ParticipantrequiredRequired, {@enum.value ParticipantrequiredRequired The participant is required to attend the appointment. }
    ParticipantrequiredOptional, {@enum.value ParticipantrequiredOptional The participant may optionally attend the appointment. }
    ParticipantrequiredInformationOnly); {@enum.value ParticipantrequiredInformationOnly The participant is excluded from the appointment, and may not be informed of the appointment taking place. (appointment is about them, not for them - such as 2 doctors discussing results about a patient's test). }
  TFhirParticipantrequiredList = set of TFhirParticipantrequired;

  {@Enum TFhirParticipationstatus
    The Participation status of an appointment.
  }
  TFhirParticipationstatus = (
    ParticipationstatusNull,  {@enum.value ParticipationstatusNull Value is missing from Instance }
    ParticipationstatusAccepted, {@enum.value ParticipationstatusAccepted The participant has accepted the appointment. }
    ParticipationstatusDeclined, {@enum.value ParticipationstatusDeclined The participant has declined the appointment and will not participate in the appointment. }
    ParticipationstatusTentative, {@enum.value ParticipationstatusTentative The participant has  tentatively accepted the appointment. This could be automatically created by a system and requires further processing before it can be accepted. There is no commitment that attendance will occur. }
    ParticipationstatusNeedsAction); {@enum.value ParticipationstatusNeedsAction The participant needs to indicate if they accept the appointment by changing this status to one of the other statuses. }
  TFhirParticipationstatusList = set of TFhirParticipationstatus;

  {@Enum TFhirParticipantstatus
    The Participation status of an appointment.
  }
  TFhirParticipantstatus = (
    ParticipantstatusNull,  {@enum.value ParticipantstatusNull Value is missing from Instance }
    ParticipantstatusAccepted, {@enum.value ParticipantstatusAccepted The appointment participant has accepted that they can attend the appointment at the time specified in the AppointmentResponse. }
    ParticipantstatusDeclined, {@enum.value ParticipantstatusDeclined The appointment participant has declined the appointment. }
    ParticipantstatusTentative, {@enum.value ParticipantstatusTentative The appointment participant has tentatively accepted the appointment. }
    ParticipantstatusInProcess, {@enum.value ParticipantstatusInProcess The participant has in-process the appointment. }
    ParticipantstatusCompleted, {@enum.value ParticipantstatusCompleted The participant has completed the appointment. }
    ParticipantstatusNeedsAction); {@enum.value ParticipantstatusNeedsAction This is the intitial status of an appointment participant until a participant has replied. It implies that there is no commitment for the appointment. }
  TFhirParticipantstatusList = set of TFhirParticipantstatus;

  {@Enum TFhirAuditEventAction
    Indicator for type of action performed during the event that generated the audit.
  }
  TFhirAuditEventAction = (
    AuditEventActionNull,  {@enum.value AuditEventActionNull Value is missing from Instance }
    AuditEventActionC, {@enum.value AuditEventActionC Create a new database object, such as placing an order. }
    AuditEventActionR, {@enum.value AuditEventActionR Display or print data, such as a doctor census. }
    AuditEventActionU, {@enum.value AuditEventActionU Update data, such as revise patient information. }
    AuditEventActionD, {@enum.value AuditEventActionD Delete items, such as a doctor master file record. }
    AuditEventActionE); {@enum.value AuditEventActionE Perform a system or application function such as log-on, program execution or use of an object's method, or perform a query/search operation. }
  TFhirAuditEventActionList = set of TFhirAuditEventAction;

  {@Enum TFhirAuditEventOutcome
    Indicates whether the event succeeded or failed
  }
  TFhirAuditEventOutcome = (
    AuditEventOutcomeNull,  {@enum.value AuditEventOutcomeNull Value is missing from Instance }
    AuditEventOutcome0, {@enum.value AuditEventOutcome0 The operation completed successfully (whether with warnings or not). }
    AuditEventOutcome4, {@enum.value AuditEventOutcome4 The action was not successful due to some kind of catered for error (often equivalent to an HTTP 400 response). }
    AuditEventOutcome8, {@enum.value AuditEventOutcome8 The action was not successful due to some kind of unexpected error (often equivalent to an HTTP 500 response). }
    AuditEventOutcome12); {@enum.value AuditEventOutcome12 An error of such magnitude occurred that the system is no longer available for use (i.e. the system died). }
  TFhirAuditEventOutcomeList = set of TFhirAuditEventOutcome;

  {@Enum TFhirNetworkType
    The type of network access point of this participant in the audit event
  }
  TFhirNetworkType = (
    NetworkTypeNull,  {@enum.value NetworkTypeNull Value is missing from Instance }
    NetworkType1, {@enum.value NetworkType1 The machine name, including DNS name. }
    NetworkType2, {@enum.value NetworkType2 The assigned Internet Protocol (IP) address. }
    NetworkType3, {@enum.value NetworkType3 The assigned telephone number. }
    NetworkType4, {@enum.value NetworkType4 The assigned email address. }
    NetworkType5); {@enum.value NetworkType5 URI (User directory, HTTP-PUT, ftp, etc.). }
  TFhirNetworkTypeList = set of TFhirNetworkType;

  {@Enum TFhirBundleType
    Indicates the purpose of a bundle - how it was intended to be used.
  }
  TFhirBundleType = (
    BundleTypeNull,  {@enum.value BundleTypeNull Value is missing from Instance }
    BundleTypeDocument, {@enum.value BundleTypeDocument The bundle is a document. The first resource is a Composition. }
    BundleTypeMessage, {@enum.value BundleTypeMessage The bundle is a message. The first resource is a MessageHeader. }
    BundleTypeTransaction, {@enum.value BundleTypeTransaction The bundle is a transaction - intended to be processed by a server as an atomic commit. }
    BundleTypeTransactionResponse, {@enum.value BundleTypeTransactionResponse The bundle is a transaction response. Because the response is a transaction response, the transactionhas succeeded, and all responses are error free. }
    BundleTypeBatch, {@enum.value BundleTypeBatch The bundle is a transaction - intended to be processed by a server as a group of actions. }
    BundleTypeBatchResponse, {@enum.value BundleTypeBatchResponse The bundle is a batch response. Note that as a batch, some responses may indicate failure and others success. }
    BundleTypeHistory, {@enum.value BundleTypeHistory The bundle is a list of resources from a history interaction on a server. }
    BundleTypeSearchset, {@enum.value BundleTypeSearchset The bundle is a list of resources returned as a result of a search/query interaction, operation, or message. }
    BundleTypeCollection); {@enum.value BundleTypeCollection The bundle is a set of resources collected into a single document for ease of distribution. }
  TFhirBundleTypeList = set of TFhirBundleType;

  {@Enum TFhirSearchEntryMode
    Why an entry is in the result set - whether it's included as a match or because of an _include requirement.
  }
  TFhirSearchEntryMode = (
    SearchEntryModeNull,  {@enum.value SearchEntryModeNull Value is missing from Instance }
    SearchEntryModeMatch, {@enum.value SearchEntryModeMatch This resource matched the search specification. }
    SearchEntryModeInclude, {@enum.value SearchEntryModeInclude This resource is returned because it is referred to from another resource in the search set. }
    SearchEntryModeOutcome); {@enum.value SearchEntryModeOutcome An OperationOutcome that provides additional information about the processing of a search. }
  TFhirSearchEntryModeList = set of TFhirSearchEntryMode;

  {@Enum TFhirHttpVerb
    HTTP verbs (in the HTTP command line).
  }
  TFhirHttpVerb = (
    HttpVerbNull,  {@enum.value HttpVerbNull Value is missing from Instance }
    HttpVerbGET, {@enum.value HttpVerbGET HTTP GET }
    HttpVerbPOST, {@enum.value HttpVerbPOST HTTP POST }
    HttpVerbPUT, {@enum.value HttpVerbPUT HTTP PUT }
    HttpVerbDELETE); {@enum.value HttpVerbDELETE HTTP DELETE }
  TFhirHttpVerbList = set of TFhirHttpVerb;

  {@Enum TFhirCarePlanStatus
    Indicates whether the plan is currently being acted upon, represents future intentions or is now just historical record.
  }
  TFhirCarePlanStatus = (
    CarePlanStatusNull,  {@enum.value CarePlanStatusNull Value is missing from Instance }
    CarePlanStatusProposed, {@enum.value CarePlanStatusProposed The plan has been suggested but no commitment to it has yet been made. }
    CarePlanStatusDraft, {@enum.value CarePlanStatusDraft The plan is in development or awaiting use but is not yet intended to be acted upon. }
    CarePlanStatusActive, {@enum.value CarePlanStatusActive The plan is intended to be followed and used as part of patient care. }
    CarePlanStatusCompleted, {@enum.value CarePlanStatusCompleted The plan is no longer in use and is not expected to be followed or used in patient care. }
    CarePlanStatusCancelled); {@enum.value CarePlanStatusCancelled The plan has been terminated prior to reaching completion (though it may have been replaced by a new plan). }
  TFhirCarePlanStatusList = set of TFhirCarePlanStatus;

  {@Enum TFhirCarePlanRelationship
    Codes identifying the types of relationships between two plans.
  }
  TFhirCarePlanRelationship = (
    CarePlanRelationshipNull,  {@enum.value CarePlanRelationshipNull Value is missing from Instance }
    CarePlanRelationshipIncludes, {@enum.value CarePlanRelationshipIncludes The referenced plan is considered to be part of this plan. }
    CarePlanRelationshipReplaces, {@enum.value CarePlanRelationshipReplaces This plan takes the places of the referenced plan. }
    CarePlanRelationshipFulfills); {@enum.value CarePlanRelationshipFulfills This plan provides details about how to perform activities defined at a higher level by the referenced plan. }
  TFhirCarePlanRelationshipList = set of TFhirCarePlanRelationship;

  {@Enum TFhirCarePlanActivityStatus
    Indicates where the activity is at in its overall life cycle.
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

  {@Enum TFhirClaimTypeLink
    The type or discipline-style of the claim.
  }
  TFhirClaimTypeLink = (
    ClaimTypeLinkNull,  {@enum.value ClaimTypeLinkNull Value is missing from Instance }
    ClaimTypeLinkInstitutional, {@enum.value ClaimTypeLinkInstitutional A claim for Institution based, typically in-patient, goods and services. }
    ClaimTypeLinkOral, {@enum.value ClaimTypeLinkOral A claim for Oral Health (Dentist, Denturist, Hygienist) goods and services. }
    ClaimTypeLinkPharmacy, {@enum.value ClaimTypeLinkPharmacy A claim for Pharmacy based goods and services. }
    ClaimTypeLinkProfessional, {@enum.value ClaimTypeLinkProfessional A claim for Professional, typically out-patient, goods and services. }
    ClaimTypeLinkVision); {@enum.value ClaimTypeLinkVision A claim for Vision (Ophthamologist, Optometrist and Optician) goods and services. }
  TFhirClaimTypeLinkList = set of TFhirClaimTypeLink;

  {@Enum TFhirClaimUseLink
    Complete, proposed, exploratory, other.
  }
  TFhirClaimUseLink = (
    ClaimUseLinkNull,  {@enum.value ClaimUseLinkNull Value is missing from Instance }
    ClaimUseLinkComplete, {@enum.value ClaimUseLinkComplete The treatment is complete and this represents a Claim for the services. }
    ClaimUseLinkProposed, {@enum.value ClaimUseLinkProposed The treatment is proposed and this represents a Pre-authorization for the services. }
    ClaimUseLinkExploratory, {@enum.value ClaimUseLinkExploratory The treatment is proposed and this represents a Pre-determination for the services. }
    ClaimUseLinkOther); {@enum.value ClaimUseLinkOther A locally defined or otherwise resolved status. }
  TFhirClaimUseLinkList = set of TFhirClaimUseLink;

  {@Enum TFhirRemittanceOutcome
    The outcome of the processing.
  }
  TFhirRemittanceOutcome = (
    RemittanceOutcomeNull,  {@enum.value RemittanceOutcomeNull Value is missing from Instance }
    RemittanceOutcomeComplete, {@enum.value RemittanceOutcomeComplete The processing completed without errors. }
    RemittanceOutcomeError); {@enum.value RemittanceOutcomeError The processing identified with errors. }
  TFhirRemittanceOutcomeList = set of TFhirRemittanceOutcome;

  {@Enum TFhirClinicalImpressionStatus
    The workflow state of a clinical impression.
  }
  TFhirClinicalImpressionStatus = (
    ClinicalImpressionStatusNull,  {@enum.value ClinicalImpressionStatusNull Value is missing from Instance }
    ClinicalImpressionStatusInProgress, {@enum.value ClinicalImpressionStatusInProgress The assessment is still on-going and results are not yet final. }
    ClinicalImpressionStatusCompleted, {@enum.value ClinicalImpressionStatusCompleted The assessment is done and the results are final. }
    ClinicalImpressionStatusEnteredInError); {@enum.value ClinicalImpressionStatusEnteredInError This assessment was never actually done and the record is erroneous (e.g. Wrong patient). }
  TFhirClinicalImpressionStatusList = set of TFhirClinicalImpressionStatus;

  {@Enum TFhirCommunicationStatus
    The status of the communication.
  }
  TFhirCommunicationStatus = (
    CommunicationStatusNull,  {@enum.value CommunicationStatusNull Value is missing from Instance }
    CommunicationStatusInProgress, {@enum.value CommunicationStatusInProgress The communication transmission is ongoing. }
    CommunicationStatusCompleted, {@enum.value CommunicationStatusCompleted The message transmission is complete, i.e., delivered to the recipient's destination. }
    CommunicationStatusSuspended, {@enum.value CommunicationStatusSuspended The communication transmission has been held by originating system/user request. }
    CommunicationStatusRejected, {@enum.value CommunicationStatusRejected The receiving system has declined to accept the message. }
    CommunicationStatusFailed); {@enum.value CommunicationStatusFailed There was a failure in transmitting the message out. }
  TFhirCommunicationStatusList = set of TFhirCommunicationStatus;

  {@Enum TFhirCommunicationRequestStatus
    The status of the communication.
  }
  TFhirCommunicationRequestStatus = (
    CommunicationRequestStatusNull,  {@enum.value CommunicationRequestStatusNull Value is missing from Instance }
    CommunicationRequestStatusProposed, {@enum.value CommunicationRequestStatusProposed The request has been proposed. }
    CommunicationRequestStatusPlanned, {@enum.value CommunicationRequestStatusPlanned The request has been planned. }
    CommunicationRequestStatusRequested, {@enum.value CommunicationRequestStatusRequested The request has been placed. }
    CommunicationRequestStatusReceived, {@enum.value CommunicationRequestStatusReceived The receiving system has received the request but not yet decided whether it will be performed. }
    CommunicationRequestStatusAccepted, {@enum.value CommunicationRequestStatusAccepted The receiving system has accepted the order, but work has not yet commenced. }
    CommunicationRequestStatusInProgress, {@enum.value CommunicationRequestStatusInProgress The work to fulfill the order is happening. }
    CommunicationRequestStatusCompleted, {@enum.value CommunicationRequestStatusCompleted The work has been complete, the report(s) released, and no further work is planned. }
    CommunicationRequestStatusSuspended, {@enum.value CommunicationRequestStatusSuspended The request has been held by originating system/user request. }
    CommunicationRequestStatusRejected, {@enum.value CommunicationRequestStatusRejected The receiving system has declined to fulfill the request }
    CommunicationRequestStatusFailed); {@enum.value CommunicationRequestStatusFailed The communication was attempted, but due to some procedural error, it could not be completed. }
  TFhirCommunicationRequestStatusList = set of TFhirCommunicationRequestStatus;

  {@Enum TFhirCompositionStatus
    The workflow/clinical status of the composition.
  }
  TFhirCompositionStatus = (
    CompositionStatusNull,  {@enum.value CompositionStatusNull Value is missing from Instance }
    CompositionStatusPreliminary, {@enum.value CompositionStatusPreliminary This is a preliminary composition or document (also known as initial or interim). The content may be incomplete or unverified. }
    CompositionStatusFinal, {@enum.value CompositionStatusFinal This version of the composition is complete and verified by an appropriate person and no further work is planned. Any subsequent updates would be on a new version of the composition. }
    CompositionStatusAmended, {@enum.value CompositionStatusAmended The composition content or the referenced resources have been modified (edited or added to) subsequent to being released as "final" and the composition is complete and verified by an authorized person. }
    CompositionStatusEnteredInError); {@enum.value CompositionStatusEnteredInError The composition or document was originally created/issued in error, and this is an amendment that marks that the entire series should not be considered as valid. }
  TFhirCompositionStatusList = set of TFhirCompositionStatus;

  {@Enum TFhirV3Confidentiality
    Codes specifying the level of confidentiality of the composition.
  }
  TFhirV3Confidentiality = (
    V3ConfidentialityNull,  {@enum.value V3ConfidentialityNull Value is missing from Instance }
    V3Confidentiality_Confidentiality, {@enum.value V3Confidentiality_Confidentiality A specializable code and its leaf codes used in Confidentiality value sets to value the Act.Confidentiality and Role.Confidentiality attribute in accordance with the definition for concept domain "Confidentiality". }
    V3ConfidentialityL, {@enum.value V3ConfidentialityL Definition: Privacy metadata indicating that the information has been de-identified, and there are mitigating circumstances that prevent re-identification, which minimize risk of harm from unauthorized disclosure.  The information requires protection to maintain low sensitivity.

                        
                           Examples: Includes anonymized, pseudonymized, or non-personally identifiable information such as HIPAA limited data sets.

                        
                           Map: No clear map to ISO 13606-4 Sensitivity Level (1) Care Management:   RECORD_COMPONENTs that might need to be accessed by a wide range of administrative staff to manage the subject of care's access to health services.

                        
                           Usage Note: This metadata indicates the receiver may have an obligation to comply with a data use agreement. }
    V3ConfidentialityM, {@enum.value V3ConfidentialityM Definition: Privacy metadata indicating moderately sensitive information, which presents moderate risk of harm if disclosed without authorization.

                        
                           Examples: Includes allergies of non-sensitive nature used inform food service; health information a patient authorizes to be used for marketing, released to a bank for a health credit card or savings account; or information in personal health record systems that are not governed under health privacy laws.

                        
                           Map: Partial Map to ISO 13606-4 Sensitivity Level (2) Clinical Management:  Less sensitive RECORD_COMPONENTs that might need to be accessed by a wider range of personnel not all of whom are actively caring for the patient (e.g. radiology staff).

                        
                           Usage Note: This metadata indicates that the receiver may be obligated to comply with the receiver's terms of use or privacy policies. }
    V3ConfidentialityN, {@enum.value V3ConfidentialityN Definition: Privacy metadata indicating that the information is typical, non-stigmatizing health information, which presents typical risk of harm if disclosed without authorization.

                        
                           Examples: In the US, this includes what HIPAA identifies as the minimum necessary protected health information (PHI) given a covered purpose of use (treatment, payment, or operations).  Includes typical, non-stigmatizing health information disclosed in an application for health, workers compensation, disability, or life insurance.

                        
                           Map: Partial Map to ISO 13606-4 Sensitivity Level (3) Clinical Care:   Default for normal clinical care access (i.e. most clinical staff directly caring for the patient should be able to access nearly all of the EHR).   Maps to normal confidentiality for treatment information but not to ancillary care, payment and operations.

                        
                           Usage Note: This metadata indicates that the receiver may be obligated to comply with applicable jurisdictional privacy law or disclosure authorization. }
    V3ConfidentialityR, {@enum.value V3ConfidentialityR Privacy metadata indicating highly sensitive, potentially stigmatizing information, which presents a high risk to the information subject if disclosed without authorization. May be pre-empted by jurisdictional law, e.g. for public health reporting or emergency treatment.

                        
                           Examples: Includes information that is additionally protected such as sensitive conditions mental health, HIV, substance abuse, domestic violence, child abuse, genetic disease, and reproductive health; or sensitive demographic information such as a patient's standing as an employee or a celebrity. May be used to indicate proprietary or classified information that is not related to an individual, e.g. secret ingredients in a therapeutic substance; or the name of a manufacturer.

                        
                           Map: Partial Map to ISO 13606-4 Sensitivity Level (3) Clinical Care: Default for normal clinical care access (i.e. most clinical staff directly caring for the patient should be able to access nearly all of the EHR). Maps to normal confidentiality for treatment information but not to ancillary care, payment and operations..

                        
                           Usage Note: This metadata indicates that the receiver may be obligated to comply with applicable, prevailing (default) jurisdictional privacy law or disclosure authorization.. }
    V3ConfidentialityU, {@enum.value V3ConfidentialityU Definition: Privacy metadata indicating that the information is not classified as sensitive.

                        
                           Examples: Includes publicly available information, e.g. business name, phone, email or physical address.

                        
                           Usage Note: This metadata indicates that the receiver has no obligation to consider additional policies when making access control decisions.   Note that in some jurisdictions, personally identifiable information must be protected as confidential, so it would not be appropriate to assign a confidentiality code of "unrestricted"  to that information even if it is publicly available. }
    V3ConfidentialityV, {@enum.value V3ConfidentialityV . Privacy metadata indicating that the information is extremely sensitive and likely stigmatizing health information that presents a very high risk if disclosed without authorization.  This information must be kept in the highest confidence.  

                        
                           Examples:  Includes information about a victim of abuse, patient requested information sensitivity, and taboo subjects relating to health status that must be discussed with the patient by an attending provider before sharing with the patient.  May also include information held under ???legal lock??? or attorney-client privilege

                        
                           Map:  This metadata indicates that the receiver may not disclose this information except as directed by the information custodian, who may be the information subject.

                        
                           Usage Note:  This metadata indicates that the receiver may not disclose this information except as directed by the information custodian, who may be the information subject. }
    V3Confidentiality_ConfidentialityByAccessKind, {@enum.value V3Confidentiality_ConfidentialityByAccessKind Description: By accessing subject / role and relationship based  rights  (These concepts are mutually exclusive, one and only one is required for a valid confidentiality coding.)

                        
                           Deprecation Comment:Deprecated due to updated confidentiality codes under ActCode }
    V3ConfidentialityB, {@enum.value V3ConfidentialityB Description: Since the service class can represent knowledge structures that may be considered a trade or business secret, there is sometimes (though rarely) the need to flag those items as of business level confidentiality.  However, no patient related information may ever be of this confidentiality level.

                        
                           Deprecation Comment: Replced by ActCode.B }
    V3ConfidentialityD, {@enum.value V3ConfidentialityD Description: Only clinicians may see this item, billing and administration persons can not access this item without special permission.

                        
                           Deprecation Comment:Deprecated due to updated confidentiality codes under ActCode }
    V3ConfidentialityI, {@enum.value V3ConfidentialityI Description: Access only to individual persons who are mentioned explicitly as actors of this service and whose actor type warrants that access (cf. to actor type code).

                        
                           Deprecation Comment:Deprecated due to updated confidentiality codes under ActCode }
    V3Confidentiality_ConfidentialityByInfoType, {@enum.value V3Confidentiality_ConfidentialityByInfoType Description: By information type, only for service catalog entries (multiples allowed). Not to be used with actual patient data!

                        
                           Deprecation Comment:Deprecated due to updated confidentiality codes under ActCode }
    V3ConfidentialityETH, {@enum.value V3ConfidentialityETH Description: Alcohol/drug-abuse related item

                        
                           Deprecation Comment:Replced by ActCode.ETH }
    V3ConfidentialityHIV, {@enum.value V3ConfidentialityHIV Description: HIV and AIDS related item

                        
                           Deprecation Comment:Replced by ActCode.HIV }
    V3ConfidentialityPSY, {@enum.value V3ConfidentialityPSY Description: Psychiatry related item

                        
                           Deprecation Comment:Replced by ActCode.PSY }
    V3ConfidentialitySDV, {@enum.value V3ConfidentialitySDV Description: Sexual assault / domestic violence related item

                        
                           Deprecation Comment:Replced by ActCode.SDV }
    V3Confidentiality_ConfidentialityModifiers, {@enum.value V3Confidentiality_ConfidentialityModifiers Description: Modifiers of role based access rights  (multiple allowed)

                        
                           Deprecation Comment:Deprecated due to updated confidentiality codes under ActCode }
    V3ConfidentialityC, {@enum.value V3ConfidentialityC Description: Celebrities are people of public interest (VIP) including employees, whose information require special protection.

                        
                           Deprecation Comment:Replced by ActCode.CEL }
    V3ConfidentialityS, {@enum.value V3ConfidentialityS Description: 
                        
Information for which the patient seeks heightened confidentiality. Sensitive information is not to be shared with family members.  Information reported by the patient about family members is sensitive by default. Flag can be set or cleared on patient's request.
                           Deprecation Comment:Deprecated due to updated confidentiality codes under ActCode }
    V3ConfidentialityT); {@enum.value V3ConfidentialityT Description: Information not to be disclosed or discussed with patient except through physician assigned to patient in this case.  This is usually a temporary constraint only, example use is a new fatal diagnosis or finding, such as malignancy or HIV.

                        
                           Deprecation Note:Replced by ActCode.TBOO }
  TFhirV3ConfidentialityList = set of TFhirV3Confidentiality;

  {@Enum TFhirCompositionAttestationMode
    The way in which a person authenticated a composition.
  }
  TFhirCompositionAttestationMode = (
    CompositionAttestationModeNull,  {@enum.value CompositionAttestationModeNull Value is missing from Instance }
    CompositionAttestationModePersonal, {@enum.value CompositionAttestationModePersonal The person authenticated the content in their personal capacity. }
    CompositionAttestationModeProfessional, {@enum.value CompositionAttestationModeProfessional The person authenticated the content in their professional capacity. }
    CompositionAttestationModeLegal, {@enum.value CompositionAttestationModeLegal The person authenticated the content and accepted legal responsibility for its content. }
    CompositionAttestationModeOfficial); {@enum.value CompositionAttestationModeOfficial The organization authenticated the content as consistent with their policies and procedures. }
  TFhirCompositionAttestationModeList = set of TFhirCompositionAttestationMode;

  {@Enum TFhirListMode
    The processing mode that applies to this section.
  }
  TFhirListMode = (
    ListModeNull,  {@enum.value ListModeNull Value is missing from Instance }
    ListModeWorking, {@enum.value ListModeWorking This list is the master list, maintained in an ongoing fashion with regular updates as the real world list it is tracking changes }
    ListModeSnapshot, {@enum.value ListModeSnapshot This list was prepared as a snapshot. It should not be assumed to be current }
    ListModeChanges); {@enum.value ListModeChanges A list that indicates where changes have been made or recommended }
  TFhirListModeList = set of TFhirListMode;

  {@Enum TFhirConformanceResourceStatus
    The lifecycle status of a Value Set or Concept Map.
  }
  TFhirConformanceResourceStatus = (
    ConformanceResourceStatusNull,  {@enum.value ConformanceResourceStatusNull Value is missing from Instance }
    ConformanceResourceStatusDraft, {@enum.value ConformanceResourceStatusDraft This resource is still under development. }
    ConformanceResourceStatusActive, {@enum.value ConformanceResourceStatusActive This resource is ready for normal use. }
    ConformanceResourceStatusRetired); {@enum.value ConformanceResourceStatusRetired This resource has been withdrawn or superseded and should no longer be used. }
  TFhirConformanceResourceStatusList = set of TFhirConformanceResourceStatus;

  {@Enum TFhirConceptMapEquivalence
    The degree of equivalence between concepts.
  }
  TFhirConceptMapEquivalence = (
    ConceptMapEquivalenceNull,  {@enum.value ConceptMapEquivalenceNull Value is missing from Instance }
    ConceptMapEquivalenceEquivalent, {@enum.value ConceptMapEquivalenceEquivalent The definitions of the concepts mean the same thing (including when structural implications of meaning are considered) (i.e. extensionally identical). }
    ConceptMapEquivalenceEqual, {@enum.value ConceptMapEquivalenceEqual The definitions of the concepts are exactly the same (i.e. only grammatical differences) and structural implications of meaning are identical or irrelevant (i.e. intentionally identical). }
    ConceptMapEquivalenceWider, {@enum.value ConceptMapEquivalenceWider The target mapping is wider in meaning than the source concept. }
    ConceptMapEquivalenceSubsumes, {@enum.value ConceptMapEquivalenceSubsumes The target mapping subsumes the meaning of the source concept (e.g. the source is-a target). }
    ConceptMapEquivalenceNarrower, {@enum.value ConceptMapEquivalenceNarrower The target mapping is narrower in meaning that the source concept. The sense in which the mapping is narrower SHALL be described in the comments in this case, and applications should be careful when atempting to use these mappings operationally. }
    ConceptMapEquivalenceSpecializes, {@enum.value ConceptMapEquivalenceSpecializes The target mapping specializes the meaning of the source concept (e.g. the target is-a source). }
    ConceptMapEquivalenceInexact, {@enum.value ConceptMapEquivalenceInexact The target mapping overlaps with the source concept, but both source and target cover additional meaning, or the definitions are imprecise and it is uncertain whether they have the same boundaries to their meaning. The sense in which the mapping is narrower SHALL be described in the comments in this case, and applications should be careful when atempting to use these mappings operationally. }
    ConceptMapEquivalenceUnmatched, {@enum.value ConceptMapEquivalenceUnmatched There is no match for this concept in the destination concept system. }
    ConceptMapEquivalenceDisjoint); {@enum.value ConceptMapEquivalenceDisjoint This is an explicit assertion that there is no mapping between the source and target concept. }
  TFhirConceptMapEquivalenceList = set of TFhirConceptMapEquivalence;

  {@Enum TFhirValuesetConditionClinical
    The clinical status of the Condition or diagnosis.
  }
  TFhirValuesetConditionClinical = (
    ValuesetConditionClinicalNull,  {@enum.value ValuesetConditionClinicalNull Value is missing from Instance }
    ValuesetConditionClinicalActive, {@enum.value ValuesetConditionClinicalActive The subject is currently experiencing the symptoms of the condition. }
    ValuesetConditionClinicalRelapse, {@enum.value ValuesetConditionClinicalRelapse The subject is re-experiencing the symptoms of the condition after a period of remission or presumed resolution. }
    ValuesetConditionClinicalRemission, {@enum.value ValuesetConditionClinicalRemission The subject is no longer experiencing the symptoms of the condition, but there is a risk of the symptoms returning. }
    ValuesetConditionClinicalResolved); {@enum.value ValuesetConditionClinicalResolved The subject is no longer experiencing the symptoms of the condition and there is no perceived risk of the symptoms returning. }
  TFhirValuesetConditionClinicalList = set of TFhirValuesetConditionClinical;

  {@Enum TFhirConditionVerStatus
    The verification status to support or decline the clinical status of the Condition or diagnosis.
  }
  TFhirConditionVerStatus = (
    ConditionVerStatusNull,  {@enum.value ConditionVerStatusNull Value is missing from Instance }
    ConditionVerStatusProvisional, {@enum.value ConditionVerStatusProvisional This is a tentative diagnosis - still a candidate that is under consideration. }
    ConditionVerStatusDifferential, {@enum.value ConditionVerStatusDifferential One of a set of potential (and typically mutually exclusive) diagnosis asserted to further guide the diagnostic process and preliminary treatment. }
    ConditionVerStatusConfirmed, {@enum.value ConditionVerStatusConfirmed There is sufficient diagnostic and/or clinical evidence to treat this as a confirmed condition. }
    ConditionVerStatusRefuted, {@enum.value ConditionVerStatusRefuted This condition has been ruled out by diagnostic and clinical evidence. }
    ConditionVerStatusEnteredInError, {@enum.value ConditionVerStatusEnteredInError The statement was entered in error and Is not valid. }
    ConditionVerStatusUnknown); {@enum.value ConditionVerStatusUnknown The condition status is unknown.  Note that "unknown" is a value of last resort and every attempt should be made to provide a meaningful value other than "unknown". }
  TFhirConditionVerStatusList = set of TFhirConditionVerStatus;

  {@Enum TFhirConformanceStatementKind
    How a conformance statement is intended to be used.
  }
  TFhirConformanceStatementKind = (
    ConformanceStatementKindNull,  {@enum.value ConformanceStatementKindNull Value is missing from Instance }
    ConformanceStatementKindInstance, {@enum.value ConformanceStatementKindInstance The Conformance instance represents the present capabilities of a specific system instance.  This is the kind returned by OPTIONS for a FHIR server end-point. }
    ConformanceStatementKindCapability, {@enum.value ConformanceStatementKindCapability The Conformance instance represents the capabilities of a system or piece of software, independent of a particular installation. }
    ConformanceStatementKindRequirements); {@enum.value ConformanceStatementKindRequirements The Conformance instance represents a set of requirements for other systems to meet; e.g. as part of an implementation guide or 'request for proposal'. }
  TFhirConformanceStatementKindList = set of TFhirConformanceStatementKind;

  {@Enum TFhirUnknownContentCode
    A code that indicates whether an application accepts unknown elements or extensions when reading resources.
  }
  TFhirUnknownContentCode = (
    UnknownContentCodeNull,  {@enum.value UnknownContentCodeNull Value is missing from Instance }
    UnknownContentCodeNo, {@enum.value UnknownContentCodeNo The application does not accept either unknown elements or extensions. }
    UnknownContentCodeExtensions, {@enum.value UnknownContentCodeExtensions The application accepts unknown extensions, but not unknown elements. }
    UnknownContentCodeElements, {@enum.value UnknownContentCodeElements The application accepts unknown elements, but not unknown extensions. }
    UnknownContentCodeBoth); {@enum.value UnknownContentCodeBoth The application accepts unknown elements and extensions. }
  TFhirUnknownContentCodeList = set of TFhirUnknownContentCode;

  {@Enum TFhirRestfulConformanceMode
    The mode of a RESTful conformance statement.
  }
  TFhirRestfulConformanceMode = (
    RestfulConformanceModeNull,  {@enum.value RestfulConformanceModeNull Value is missing from Instance }
    RestfulConformanceModeClient, {@enum.value RestfulConformanceModeClient The application acts as a client for this resource. }
    RestfulConformanceModeServer); {@enum.value RestfulConformanceModeServer The application acts as a server for this resource. }
  TFhirRestfulConformanceModeList = set of TFhirRestfulConformanceMode;

  {@Enum TFhirTypeRestfulInteraction
    Operations supported by REST at the type or instance level.
  }
  TFhirTypeRestfulInteraction = (
    TypeRestfulInteractionNull,  {@enum.value TypeRestfulInteractionNull Value is missing from Instance }
    TypeRestfulInteractionRead, {@enum.value TypeRestfulInteractionRead  }
    TypeRestfulInteractionVread, {@enum.value TypeRestfulInteractionVread  }
    TypeRestfulInteractionUpdate, {@enum.value TypeRestfulInteractionUpdate  }
    TypeRestfulInteractionDelete, {@enum.value TypeRestfulInteractionDelete  }
    TypeRestfulInteractionHistoryInstance, {@enum.value TypeRestfulInteractionHistoryInstance  }
    TypeRestfulInteractionValidate, {@enum.value TypeRestfulInteractionValidate  }
    TypeRestfulInteractionHistoryType, {@enum.value TypeRestfulInteractionHistoryType  }
    TypeRestfulInteractionCreate, {@enum.value TypeRestfulInteractionCreate  }
    TypeRestfulInteractionSearchType); {@enum.value TypeRestfulInteractionSearchType  }
  TFhirTypeRestfulInteractionList = set of TFhirTypeRestfulInteraction;

  {@Enum TFhirVersioningPolicy
    How the system supports versioning for a resource.
  }
  TFhirVersioningPolicy = (
    VersioningPolicyNull,  {@enum.value VersioningPolicyNull Value is missing from Instance }
    VersioningPolicyNoVersion, {@enum.value VersioningPolicyNoVersion VersionId meta-property is not suppoerted (server) or used (client). }
    VersioningPolicyVersioned, {@enum.value VersioningPolicyVersioned VersionId meta-property is suppoerted (server) or used (client). }
    VersioningPolicyVersionedUpdate); {@enum.value VersioningPolicyVersionedUpdate VersionId is must be correct for updates (server) or will be specified (If-match header) for updates (client). }
  TFhirVersioningPolicyList = set of TFhirVersioningPolicy;

  {@Enum TFhirConditionalDeleteStatus
    A code that indicates how the server supports conditional delete.
  }
  TFhirConditionalDeleteStatus = (
    ConditionalDeleteStatusNull,  {@enum.value ConditionalDeleteStatusNull Value is missing from Instance }
    ConditionalDeleteStatusNotSupported, {@enum.value ConditionalDeleteStatusNotSupported No support for conditional deletes. }
    ConditionalDeleteStatusSingle, {@enum.value ConditionalDeleteStatusSingle Conditional deletes are supported, but only single resources at a time. }
    ConditionalDeleteStatusMultiple); {@enum.value ConditionalDeleteStatusMultiple Conditional deletes are supported, and multiple resources can be deleted in a single interaction. }
  TFhirConditionalDeleteStatusList = set of TFhirConditionalDeleteStatus;

  {@Enum TFhirSearchParamType
    Data types allowed to be used for search parameters.
  }
  TFhirSearchParamType = (
    SearchParamTypeNull,  {@enum.value SearchParamTypeNull Value is missing from Instance }
    SearchParamTypeNumber, {@enum.value SearchParamTypeNumber Search parameter SHALL be a number (a whole number, or a decimal). }
    SearchParamTypeDate, {@enum.value SearchParamTypeDate Search parameter is on a date/time. The date format is the standard XML format, though other formats may be supported. }
    SearchParamTypeString, {@enum.value SearchParamTypeString Search parameter is a simple string, like a name part. Search is case-insensitive and accent-insensitive. May match just the start of a string. String parameters may contain spaces. }
    SearchParamTypeToken, {@enum.value SearchParamTypeToken Search parameter on a coded element or identifier. May be used to search through the text, displayname, code and code/codesystem (for codes) and label, system and key (for identifier). Its value is either a string or a pair of namespace and value, separated by a "|", depending on the modifier used. }
    SearchParamTypeReference, {@enum.value SearchParamTypeReference A reference to another resource. }
    SearchParamTypeComposite, {@enum.value SearchParamTypeComposite A composite search parameter that combines a search on two values together. }
    SearchParamTypeQuantity, {@enum.value SearchParamTypeQuantity A search parameter that searches on a quantity. }
    SearchParamTypeUri); {@enum.value SearchParamTypeUri A search parameter that searches on a URI (RFC 3986). }
  TFhirSearchParamTypeList = set of TFhirSearchParamType;

  {@Enum TFhirSearchModifierCode
    A supported modifier for a search parameter.
  }
  TFhirSearchModifierCode = (
    SearchModifierCodeNull,  {@enum.value SearchModifierCodeNull Value is missing from Instance }
    SearchModifierCodeMissing, {@enum.value SearchModifierCodeMissing The search parameter returns resources that have a value or not. }
    SearchModifierCodeExact, {@enum.value SearchModifierCodeExact The search parameter returns resources that have a value that exactly matches the supplied parameter (the whole string, including casing and accents). }
    SearchModifierCodeContains, {@enum.value SearchModifierCodeContains The search parameter returns resources that include the supplied parameter value anywhere within the field being searched. }
    SearchModifierCodeNot, {@enum.value SearchModifierCodeNot The search parameter returns resources that do not contain a match . }
    SearchModifierCodeText, {@enum.value SearchModifierCodeText The search parameter is processed as a string that searches text associated with the code/value - either CodeableConcept.text, Coding.display, or Identifier.type.text. }
    SearchModifierCodeIn, {@enum.value SearchModifierCodeIn The search parameter is a URI (relative or absolute) that identifies a value set, and the search parameter tests whether the coding is in the specified value set. }
    SearchModifierCodeNotIn, {@enum.value SearchModifierCodeNotIn The search parameter is a URI (relative or absolute) that identifies a value set, and the search parameter tests whether the coding is not in the specified value set. }
    SearchModifierCodeBelow, {@enum.value SearchModifierCodeBelow The search parameter tests whether the value in a resource is subsumed by the specified value (is-a, or hierarchical relationships). }
    SearchModifierCodeAbove, {@enum.value SearchModifierCodeAbove The search parameter tests whether the value in a resource subsumes the specified value (is-a, or hierarchical relationships). }
    SearchModifierCodeType); {@enum.value SearchModifierCodeType The search parameter only applies to the Resource Type specified as a modifier (e.g. the modifier is not actually :type, but :Patient etc.). }
  TFhirSearchModifierCodeList = set of TFhirSearchModifierCode;

  {@Enum TFhirSystemRestfulInteraction
    Operations supported by REST at the system level.
  }
  TFhirSystemRestfulInteraction = (
    SystemRestfulInteractionNull,  {@enum.value SystemRestfulInteractionNull Value is missing from Instance }
    SystemRestfulInteractionTransaction, {@enum.value SystemRestfulInteractionTransaction  }
    SystemRestfulInteractionSearchSystem, {@enum.value SystemRestfulInteractionSearchSystem  }
    SystemRestfulInteractionHistorySystem); {@enum.value SystemRestfulInteractionHistorySystem  }
  TFhirSystemRestfulInteractionList = set of TFhirSystemRestfulInteraction;

  {@Enum TFhirTransactionMode
    A code that indicates how transactions are supported.
  }
  TFhirTransactionMode = (
    TransactionModeNull,  {@enum.value TransactionModeNull Value is missing from Instance }
    TransactionModeNotSupported, {@enum.value TransactionModeNotSupported Neither batch or transaction is supported. }
    TransactionModeBatch, {@enum.value TransactionModeBatch Batches are  supported. }
    TransactionModeTransaction, {@enum.value TransactionModeTransaction Transactions are supported. }
    TransactionModeBoth); {@enum.value TransactionModeBoth Both batches and transactions are supported. }
  TFhirTransactionModeList = set of TFhirTransactionMode;

  {@Enum TFhirMessageSignificanceCategory
    The impact of the content of a message.
  }
  TFhirMessageSignificanceCategory = (
    MessageSignificanceCategoryNull,  {@enum.value MessageSignificanceCategoryNull Value is missing from Instance }
    MessageSignificanceCategoryConsequence, {@enum.value MessageSignificanceCategoryConsequence The message represents/requests a change that should not be processed more than once; e.g. Making a booking for an appointment. }
    MessageSignificanceCategoryCurrency, {@enum.value MessageSignificanceCategoryCurrency The message represents a response to query for current information. Retrospective processing is wrong and/or wasteful. }
    MessageSignificanceCategoryNotification); {@enum.value MessageSignificanceCategoryNotification The content is not necessarily intended to be current, and it can be reprocessed, though there may be version issues created by processing old notifications. }
  TFhirMessageSignificanceCategoryList = set of TFhirMessageSignificanceCategory;

  {@Enum TFhirMessageConformanceEventMode
    The mode of a message conformance statement.
  }
  TFhirMessageConformanceEventMode = (
    MessageConformanceEventModeNull,  {@enum.value MessageConformanceEventModeNull Value is missing from Instance }
    MessageConformanceEventModeSender, {@enum.value MessageConformanceEventModeSender The application sends requests and receives responses. }
    MessageConformanceEventModeReceiver); {@enum.value MessageConformanceEventModeReceiver The application receives requests and sends responses. }
  TFhirMessageConformanceEventModeList = set of TFhirMessageConformanceEventMode;

  {@Enum TFhirDocumentMode
    Whether the application produces or consumes documents.
  }
  TFhirDocumentMode = (
    DocumentModeNull,  {@enum.value DocumentModeNull Value is missing from Instance }
    DocumentModeProducer, {@enum.value DocumentModeProducer The application produces documents of the specified type. }
    DocumentModeConsumer); {@enum.value DocumentModeConsumer The application consumes documents of the specified type. }
  TFhirDocumentModeList = set of TFhirDocumentMode;

  {@Enum TFhirDataelementStringency
    Indicates the degree of precision of the data element definition.
  }
  TFhirDataelementStringency = (
    DataelementStringencyNull,  {@enum.value DataelementStringencyNull Value is missing from Instance }
    DataelementStringencyComparable, {@enum.value DataelementStringencyComparable The data element is sufficiently well-constrained that multiple pieces of data captured according to the constraints of the data element will be comparable (though in some cases, a degree of automated conversion/normalization may be required). }
    DataelementStringencyFullySpecified, {@enum.value DataelementStringencyFullySpecified The data element is fully specified down to a single value set, single unit of measure, single data type, etc.  Multiple pieces of data associated with this data element are fully comparable. }
    DataelementStringencyEquivalent, {@enum.value DataelementStringencyEquivalent The data element allows multiple units of measure having equivalent meaning; e.g. "cc" (cubic centimeter) and "mL". }
    DataelementStringencyConvertable, {@enum.value DataelementStringencyConvertable The data element allows multiple units of measure that are convertable between each other (e.g. Inches and centimeters) and/or allows data to be captured in multiple value sets for which a known mapping exists allowing conversion of meaning. }
    DataelementStringencyScaleable, {@enum.value DataelementStringencyScaleable A convertable data element where unit conversions are different only by a power of 10; e.g. g, mg, kg. }
    DataelementStringencyFlexible); {@enum.value DataelementStringencyFlexible The data element is unconstrained in units, choice of data types and/or choice of vocabulary such that automated comparison of data captured using the data element is not possible. }
  TFhirDataelementStringencyList = set of TFhirDataelementStringency;

  {@Enum TFhirDetectedissueSeverity
    Indicates the potential degree of impact of the identified issue on the patient.
  }
  TFhirDetectedissueSeverity = (
    DetectedissueSeverityNull,  {@enum.value DetectedissueSeverityNull Value is missing from Instance }
    DetectedissueSeverityHigh, {@enum.value DetectedissueSeverityHigh Indicates the issue may be life-threatening or has the potential to cause permanent injury. }
    DetectedissueSeverityModerate, {@enum.value DetectedissueSeverityModerate Indicates the issue may result in noticable adverse adverse consequences but is unlikely to be life-threatening or cause permanent injury. }
    DetectedissueSeverityLow); {@enum.value DetectedissueSeverityLow Indicates the issue may result in some adverse consequences but is unlikely to substantially affect the situation of the subject. }
  TFhirDetectedissueSeverityList = set of TFhirDetectedissueSeverity;

  {@Enum TFhirDevicestatus
    The availability status of the device.
  }
  TFhirDevicestatus = (
    DevicestatusNull,  {@enum.value DevicestatusNull Value is missing from Instance }
    DevicestatusAvailable, {@enum.value DevicestatusAvailable The Device is available for use. }
    DevicestatusNotAvailable, {@enum.value DevicestatusNotAvailable The Device is no longer available for use (e.g. lost, expired, damaged). }
    DevicestatusEnteredInError); {@enum.value DevicestatusEnteredInError The Device was entered in error and voided. }
  TFhirDevicestatusList = set of TFhirDevicestatus;

  {@Enum TFhirMeasurementPrinciple
    Different measurement principle supported by the device.
  }
  TFhirMeasurementPrinciple = (
    MeasurementPrincipleNull,  {@enum.value MeasurementPrincipleNull Value is missing from Instance }
    MeasurementPrincipleOther, {@enum.value MeasurementPrincipleOther Measurement principle isn't in the list. }
    MeasurementPrincipleChemical, {@enum.value MeasurementPrincipleChemical Measurement is done using chemical. }
    MeasurementPrincipleElectrical, {@enum.value MeasurementPrincipleElectrical Measurement is done using electrical. }
    MeasurementPrincipleImpedance, {@enum.value MeasurementPrincipleImpedance Measurement is done using impedance. }
    MeasurementPrincipleNuclear, {@enum.value MeasurementPrincipleNuclear Measurement is done using nuclear. }
    MeasurementPrincipleOptical, {@enum.value MeasurementPrincipleOptical Measurement is done using optical. }
    MeasurementPrincipleThermal, {@enum.value MeasurementPrincipleThermal Measurement is done using thermal. }
    MeasurementPrincipleBiological, {@enum.value MeasurementPrincipleBiological Measurement is done using biological. }
    MeasurementPrincipleMechanical, {@enum.value MeasurementPrincipleMechanical Measurement is done using mechanical. }
    MeasurementPrincipleAcoustical, {@enum.value MeasurementPrincipleAcoustical Measurement is done using acoustical. }
    MeasurementPrincipleManual); {@enum.value MeasurementPrincipleManual Measurement is done using manual. }
  TFhirMeasurementPrincipleList = set of TFhirMeasurementPrinciple;

  {@Enum TFhirMetricOperationalStatus
    Describes the operational status of the DeviceMetric.
  }
  TFhirMetricOperationalStatus = (
    MetricOperationalStatusNull,  {@enum.value MetricOperationalStatusNull Value is missing from Instance }
    MetricOperationalStatusOn, {@enum.value MetricOperationalStatusOn The DeviceMetric is operating and will generate DeviceObservations. }
    MetricOperationalStatusOff, {@enum.value MetricOperationalStatusOff The DeviceMetric is not operating. }
    MetricOperationalStatusStandby); {@enum.value MetricOperationalStatusStandby The DeviceMetric is operating, but will not generate any DeviceObservations. }
  TFhirMetricOperationalStatusList = set of TFhirMetricOperationalStatus;

  {@Enum TFhirMetricColor
    Describes the typical color of representation.
  }
  TFhirMetricColor = (
    MetricColorNull,  {@enum.value MetricColorNull Value is missing from Instance }
    MetricColorBlack, {@enum.value MetricColorBlack Color for representation - black. }
    MetricColorRed, {@enum.value MetricColorRed Color for representation - red. }
    MetricColorGreen, {@enum.value MetricColorGreen Color for representation - green. }
    MetricColorYellow, {@enum.value MetricColorYellow Color for representation - yellow. }
    MetricColorBlue, {@enum.value MetricColorBlue Color for representation - blue. }
    MetricColorMagenta, {@enum.value MetricColorMagenta Color for representation - magenta. }
    MetricColorCyan, {@enum.value MetricColorCyan Color for representation - cyan. }
    MetricColorWhite); {@enum.value MetricColorWhite Color for representation - white. }
  TFhirMetricColorList = set of TFhirMetricColor;

  {@Enum TFhirMetricCategory
    Describes the category of the metric.
  }
  TFhirMetricCategory = (
    MetricCategoryNull,  {@enum.value MetricCategoryNull Value is missing from Instance }
    MetricCategoryMeasurement, {@enum.value MetricCategoryMeasurement DeviceObservations generated for this DeviceMetric are measured. }
    MetricCategorySetting, {@enum.value MetricCategorySetting DeviceObservations generated for this DeviceMetric is a setting that will influence the behavior of the Device. }
    MetricCategoryCalculation, {@enum.value MetricCategoryCalculation DeviceObservations generated for this DeviceMetric are calculated. }
    MetricCategoryUnspecified); {@enum.value MetricCategoryUnspecified The category of this DeviceMetric is unspecified. }
  TFhirMetricCategoryList = set of TFhirMetricCategory;

  {@Enum TFhirMetricCalibrationType
    Describes the type of a metric calibration.
  }
  TFhirMetricCalibrationType = (
    MetricCalibrationTypeNull,  {@enum.value MetricCalibrationTypeNull Value is missing from Instance }
    MetricCalibrationTypeUnspecified, {@enum.value MetricCalibrationTypeUnspecified TODO }
    MetricCalibrationTypeOffset, {@enum.value MetricCalibrationTypeOffset TODO }
    MetricCalibrationTypeGain, {@enum.value MetricCalibrationTypeGain TODO }
    MetricCalibrationTypeTwoPoint); {@enum.value MetricCalibrationTypeTwoPoint TODO }
  TFhirMetricCalibrationTypeList = set of TFhirMetricCalibrationType;

  {@Enum TFhirMetricCalibrationState
    Describes the state of a metric calibration.
  }
  TFhirMetricCalibrationState = (
    MetricCalibrationStateNull,  {@enum.value MetricCalibrationStateNull Value is missing from Instance }
    MetricCalibrationStateNotCalibrated, {@enum.value MetricCalibrationStateNotCalibrated The metric has not been calibrated. }
    MetricCalibrationStateCalibrationRequired, {@enum.value MetricCalibrationStateCalibrationRequired The metric needs to be calibrated. }
    MetricCalibrationStateCalibrated, {@enum.value MetricCalibrationStateCalibrated The metric has been calibrated. }
    MetricCalibrationStateUnspecified); {@enum.value MetricCalibrationStateUnspecified The state of calibration of this metric is unspecified. }
  TFhirMetricCalibrationStateList = set of TFhirMetricCalibrationState;

  {@Enum TFhirDeviceUseRequestStatus
    Codes representing the status of the request.
  }
  TFhirDeviceUseRequestStatus = (
    DeviceUseRequestStatusNull,  {@enum.value DeviceUseRequestStatusNull Value is missing from Instance }
    DeviceUseRequestStatusProposed, {@enum.value DeviceUseRequestStatusProposed The request has been proposed. }
    DeviceUseRequestStatusPlanned, {@enum.value DeviceUseRequestStatusPlanned The request has been planned. }
    DeviceUseRequestStatusRequested, {@enum.value DeviceUseRequestStatusRequested The request has been placed. }
    DeviceUseRequestStatusReceived, {@enum.value DeviceUseRequestStatusReceived The receiving system has received the request but not yet decided whether it will be performed. }
    DeviceUseRequestStatusAccepted, {@enum.value DeviceUseRequestStatusAccepted The receiving system has accepted the request but work has not yet commenced. }
    DeviceUseRequestStatusInProgress, {@enum.value DeviceUseRequestStatusInProgress The work to fulfill the order is happening. }
    DeviceUseRequestStatusCompleted, {@enum.value DeviceUseRequestStatusCompleted The work has been complete, the report(s) released, and no further work is planned. }
    DeviceUseRequestStatusSuspended, {@enum.value DeviceUseRequestStatusSuspended The request has been held by originating system/user request. }
    DeviceUseRequestStatusRejected, {@enum.value DeviceUseRequestStatusRejected The receiving system has declined to fulfill the request. }
    DeviceUseRequestStatusAborted); {@enum.value DeviceUseRequestStatusAborted The request was attempted, but due to some procedural error, it could not be completed. }
  TFhirDeviceUseRequestStatusList = set of TFhirDeviceUseRequestStatus;

  {@Enum TFhirDeviceUseRequestPriority
    Codes representing the priority of the request.
  }
  TFhirDeviceUseRequestPriority = (
    DeviceUseRequestPriorityNull,  {@enum.value DeviceUseRequestPriorityNull Value is missing from Instance }
    DeviceUseRequestPriorityRoutine, {@enum.value DeviceUseRequestPriorityRoutine The request has a normal priority. }
    DeviceUseRequestPriorityUrgent, {@enum.value DeviceUseRequestPriorityUrgent The request should be done urgently. }
    DeviceUseRequestPriorityStat, {@enum.value DeviceUseRequestPriorityStat The request is time-critical. }
    DeviceUseRequestPriorityAsap); {@enum.value DeviceUseRequestPriorityAsap The request should be acted on as soon as possible. }
  TFhirDeviceUseRequestPriorityList = set of TFhirDeviceUseRequestPriority;

  {@Enum TFhirDiagnosticOrderStatus
    The status of a diagnostic order.
  }
  TFhirDiagnosticOrderStatus = (
    DiagnosticOrderStatusNull,  {@enum.value DiagnosticOrderStatusNull Value is missing from Instance }
    DiagnosticOrderStatusProposed, {@enum.value DiagnosticOrderStatusProposed The request has been proposed. }
    DiagnosticOrderStatusDraft, {@enum.value DiagnosticOrderStatusDraft the request is in preliminary form prior to being sent. }
    DiagnosticOrderStatusPlanned, {@enum.value DiagnosticOrderStatusPlanned The request has been planned. }
    DiagnosticOrderStatusRequested, {@enum.value DiagnosticOrderStatusRequested The request has been placed. }
    DiagnosticOrderStatusReceived, {@enum.value DiagnosticOrderStatusReceived The receiving system has received the order, but not yet decided whether it will be performed. }
    DiagnosticOrderStatusAccepted, {@enum.value DiagnosticOrderStatusAccepted The receiving system has accepted the order, but work has not yet commenced. }
    DiagnosticOrderStatusInProgress, {@enum.value DiagnosticOrderStatusInProgress The work to fulfill the order is happening. }
    DiagnosticOrderStatusReview, {@enum.value DiagnosticOrderStatusReview The work is complete, and the outcomes are being reviewed for approval. }
    DiagnosticOrderStatusCompleted, {@enum.value DiagnosticOrderStatusCompleted The work has been complete, the report(s) released, and no further work is planned. }
    DiagnosticOrderStatusCancelled, {@enum.value DiagnosticOrderStatusCancelled the request has been withdrawn. }
    DiagnosticOrderStatusSuspended, {@enum.value DiagnosticOrderStatusSuspended The request has been held by originating system/user request. }
    DiagnosticOrderStatusRejected, {@enum.value DiagnosticOrderStatusRejected The receiving system has declined to fulfill the request. }
    DiagnosticOrderStatusFailed); {@enum.value DiagnosticOrderStatusFailed The diagnostic investigation was attempted, but due to some procedural error, it could not be completed. }
  TFhirDiagnosticOrderStatusList = set of TFhirDiagnosticOrderStatus;

  {@Enum TFhirDiagnosticOrderPriority
    The clinical priority of a diagnostic order.
  }
  TFhirDiagnosticOrderPriority = (
    DiagnosticOrderPriorityNull,  {@enum.value DiagnosticOrderPriorityNull Value is missing from Instance }
    DiagnosticOrderPriorityRoutine, {@enum.value DiagnosticOrderPriorityRoutine The order has a normal priority . }
    DiagnosticOrderPriorityUrgent, {@enum.value DiagnosticOrderPriorityUrgent The order should be urgently. }
    DiagnosticOrderPriorityStat, {@enum.value DiagnosticOrderPriorityStat The order is time-critical. }
    DiagnosticOrderPriorityAsap); {@enum.value DiagnosticOrderPriorityAsap The order should be acted on as soon as possible. }
  TFhirDiagnosticOrderPriorityList = set of TFhirDiagnosticOrderPriority;

  {@Enum TFhirDiagnosticReportStatus
    The status of the diagnostic report as a whole.
  }
  TFhirDiagnosticReportStatus = (
    DiagnosticReportStatusNull,  {@enum.value DiagnosticReportStatusNull Value is missing from Instance }
    DiagnosticReportStatusRegistered, {@enum.value DiagnosticReportStatusRegistered The existence of the report is registered, but there is nothing yet available. }
    DiagnosticReportStatusPartial, {@enum.value DiagnosticReportStatusPartial This is a partial (e.g. initial, interim or preliminary) report: data in the report may be incomplete or unverified. }
    DiagnosticReportStatusFinal, {@enum.value DiagnosticReportStatusFinal The report is complete and verified by an authorized person. }
    DiagnosticReportStatusCorrected, {@enum.value DiagnosticReportStatusCorrected The report has been modified subsequent to being Final, and is complete and verified by an authorized person. New content has been added, but existing content hasn't changed }
    DiagnosticReportStatusAppended, {@enum.value DiagnosticReportStatusAppended The report has been modified subsequent to being Final, and is complete and verified by an authorized person. New content has been added, but existing content hasn't changed. }
    DiagnosticReportStatusCancelled, {@enum.value DiagnosticReportStatusCancelled The report is unavailable because the measurement was not started or not completed (also sometimes called "aborted"). }
    DiagnosticReportStatusEnteredInError); {@enum.value DiagnosticReportStatusEnteredInError The report has been withdrawn following previous Final release. }
  TFhirDiagnosticReportStatusList = set of TFhirDiagnosticReportStatus;

  {@Enum TFhirDocumentReferenceStatus
    The status of the document reference.
  }
  TFhirDocumentReferenceStatus = (
    DocumentReferenceStatusNull,  {@enum.value DocumentReferenceStatusNull Value is missing from Instance }
    DocumentReferenceStatusCurrent, {@enum.value DocumentReferenceStatusCurrent This is the current reference for this document. }
    DocumentReferenceStatusSuperseded, {@enum.value DocumentReferenceStatusSuperseded This reference has been superseded by another reference. }
    DocumentReferenceStatusEnteredInError); {@enum.value DocumentReferenceStatusEnteredInError This reference was created in error. }
  TFhirDocumentReferenceStatusList = set of TFhirDocumentReferenceStatus;

  {@Enum TFhirDocumentRelationshipType
    The type of relationship between documents.
  }
  TFhirDocumentRelationshipType = (
    DocumentRelationshipTypeNull,  {@enum.value DocumentRelationshipTypeNull Value is missing from Instance }
    DocumentRelationshipTypeReplaces, {@enum.value DocumentRelationshipTypeReplaces This document logically replaces or supersedes the target document. }
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
    EncounterStateArrived, {@enum.value EncounterStateArrived The Patient is present for the encounter, however is not currently meeting with a practitioner. }
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
    EncounterClassEmergency, {@enum.value EncounterClassEmergency An encounter in the Emergency Care Department. }
    EncounterClassHome, {@enum.value EncounterClassHome An encounter where the practitioner visits the patient at his/her home. }
    EncounterClassField, {@enum.value EncounterClassField An encounter taking place outside the regular environment for giving care. }
    EncounterClassDaytime, {@enum.value EncounterClassDaytime An encounter where the patient needs more prolonged treatment or investigations than outpatients, but who do not need to stay in the hospital overnight. }
    EncounterClassVirtual, {@enum.value EncounterClassVirtual An encounter that takes place where the patient and practitioner do not physically meet but use electronic means for contact. }
    EncounterClassOther); {@enum.value EncounterClassOther Any other encounter type that is not described by one of the other values. Where this is used it is expected that an implementer will include an extension value to define what the actual other type is. }
  TFhirEncounterClassList = set of TFhirEncounterClass;

  {@Enum TFhirEncounterLocationStatus
    The status of the location.
  }
  TFhirEncounterLocationStatus = (
    EncounterLocationStatusNull,  {@enum.value EncounterLocationStatusNull Value is missing from Instance }
    EncounterLocationStatusPlanned, {@enum.value EncounterLocationStatusPlanned The patient is planned to be moved to this location at some point in the future. }
    EncounterLocationStatusActive, {@enum.value EncounterLocationStatusActive The patient is currently at this location, or was between the period specified.

A system may update these records when the patient leaves the location to either reserved, or completed }
    EncounterLocationStatusReserved, {@enum.value EncounterLocationStatusReserved This location is held empty for this patient. }
    EncounterLocationStatusCompleted); {@enum.value EncounterLocationStatusCompleted The patient was at this location during the period specified.

Not to be used when the patient is currently at the location }
  TFhirEncounterLocationStatusList = set of TFhirEncounterLocationStatus;

  {@Enum TFhirEpisodeOfCareStatus
    The status of the encounter.
  }
  TFhirEpisodeOfCareStatus = (
    EpisodeOfCareStatusNull,  {@enum.value EpisodeOfCareStatusNull Value is missing from Instance }
    EpisodeOfCareStatusPlanned, {@enum.value EpisodeOfCareStatusPlanned This episode of care is planned to start at the date specified in the period.start. During this status an organization may perform assessments to determine if they are eligible to receive services, or be organizing to make resources available to provide care services. }
    EpisodeOfCareStatusWaitlist, {@enum.value EpisodeOfCareStatusWaitlist This episode has been placed on a waitlist, pending the episode being made active (or cancelled). }
    EpisodeOfCareStatusActive, {@enum.value EpisodeOfCareStatusActive This episode of care is current. }
    EpisodeOfCareStatusOnhold, {@enum.value EpisodeOfCareStatusOnhold This episode of care is on hold, the organization has limited responsibility for the patient (such as while on respite). }
    EpisodeOfCareStatusFinished, {@enum.value EpisodeOfCareStatusFinished This episode of care is finished at the organization is not expecting to be providing care to the patient. Can also be known as "closed", "completed" or other similar terms. }
    EpisodeOfCareStatusCancelled); {@enum.value EpisodeOfCareStatusCancelled The episode of care was cancelled, or withdrawn from service, often selected during the planned stage as the patient may have gone elsewhere, or the circumstances have changed and the organization is unable to provide the care. It indicates that services terminated outside the planned/expected workflow. }
  TFhirEpisodeOfCareStatusList = set of TFhirEpisodeOfCareStatus;

  {@Enum TFhirHistoryStatus
    A code that identifies the status of the family history record.
  }
  TFhirHistoryStatus = (
    HistoryStatusNull,  {@enum.value HistoryStatusNull Value is missing from Instance }
    HistoryStatusPartial, {@enum.value HistoryStatusPartial Some health information is known and captured, but not complete - see notes for details. }
    HistoryStatusCompleted, {@enum.value HistoryStatusCompleted All relevant health information is known and captured. }
    HistoryStatusEnteredInError, {@enum.value HistoryStatusEnteredInError This instance should not have been part of this patient's medical record. }
    HistoryStatusHealthUnknown); {@enum.value HistoryStatusHealthUnknown Health information for this individual is unavailable/unknown. }
  TFhirHistoryStatusList = set of TFhirHistoryStatus;

  {@Enum TFhirAdministrativeGender
    The gender of a person used for administrative purposes.
  }
  TFhirAdministrativeGender = (
    AdministrativeGenderNull,  {@enum.value AdministrativeGenderNull Value is missing from Instance }
    AdministrativeGenderMale, {@enum.value AdministrativeGenderMale Male }
    AdministrativeGenderFemale, {@enum.value AdministrativeGenderFemale Female }
    AdministrativeGenderOther, {@enum.value AdministrativeGenderOther Other }
    AdministrativeGenderUnknown); {@enum.value AdministrativeGenderUnknown Unknown }
  TFhirAdministrativeGenderList = set of TFhirAdministrativeGender;

  {@Enum TFhirFlagStatus
    Indicates whether this flag is active and needs to be displayed to a user, or whether it is no longer needed or entered in error.
  }
  TFhirFlagStatus = (
    FlagStatusNull,  {@enum.value FlagStatusNull Value is missing from Instance }
    FlagStatusActive, {@enum.value FlagStatusActive A current flag that should be displayed to a user. A system may use the category to determine which roles should view the flag. }
    FlagStatusInactive, {@enum.value FlagStatusInactive The flag does not need to be displayed any more. }
    FlagStatusEnteredInError); {@enum.value FlagStatusEnteredInError The flag was added in error, and should no longer be displayed. }
  TFhirFlagStatusList = set of TFhirFlagStatus;

  {@Enum TFhirGoalStatus
    Indicates whether the goal has been met and is still being targeted
  }
  TFhirGoalStatus = (
    GoalStatusNull,  {@enum.value GoalStatusNull Value is missing from Instance }
    GoalStatusProposed, {@enum.value GoalStatusProposed A goal is proposed for this patient }
    GoalStatusPlanned, {@enum.value GoalStatusPlanned A goal is planned for this patient }
    GoalStatusAccepted, {@enum.value GoalStatusAccepted A proposed goal was accepted }
    GoalStatusRejected, {@enum.value GoalStatusRejected A proposed goal was rejected }
    GoalStatusInProgress, {@enum.value GoalStatusInProgress The goal is being sought but has not yet been reached.  (Also applies if goal was reached in the past but there has been regression and goal is being sought again) }
    GoalStatusAchieved, {@enum.value GoalStatusAchieved The goal has been met and no further action is needed }
    GoalStatusSustaining, {@enum.value GoalStatusSustaining The goal has been met, but ongoing activity is needed to sustain the goal objective }
    GoalStatusOnHold, {@enum.value GoalStatusOnHold The goal remains a long term objective but is no longer being actively pursued for a temporary period of time. }
    GoalStatusCancelled); {@enum.value GoalStatusCancelled The goal is no longer being sought }
  TFhirGoalStatusList = set of TFhirGoalStatus;

  {@Enum TFhirGroupType
    Types of resources that are part of group
  }
  TFhirGroupType = (
    GroupTypeNull,  {@enum.value GroupTypeNull Value is missing from Instance }
    GroupTypePerson, {@enum.value GroupTypePerson Group contains "person" Patient resources }
    GroupTypeAnimal, {@enum.value GroupTypeAnimal Group contains "animal" Patient resources }
    GroupTypePractitioner, {@enum.value GroupTypePractitioner Group contains healthcare practitioner resources }
    GroupTypeDevice, {@enum.value GroupTypeDevice Group contains Device resources }
    GroupTypeMedication, {@enum.value GroupTypeMedication Group contains Medication resources }
    GroupTypeSubstance); {@enum.value GroupTypeSubstance Group contains Substance resources }
  TFhirGroupTypeList = set of TFhirGroupType;

  {@Enum TFhirDaysOfWeek
    The days of the week
  }
  TFhirDaysOfWeek = (
    DaysOfWeekNull,  {@enum.value DaysOfWeekNull Value is missing from Instance }
    DaysOfWeekMon, {@enum.value DaysOfWeekMon Monday }
    DaysOfWeekTue, {@enum.value DaysOfWeekTue Tuesday }
    DaysOfWeekWed, {@enum.value DaysOfWeekWed Wednesday }
    DaysOfWeekThu, {@enum.value DaysOfWeekThu Thursday }
    DaysOfWeekFri, {@enum.value DaysOfWeekFri Friday }
    DaysOfWeekSat, {@enum.value DaysOfWeekSat Saturday }
    DaysOfWeekSun); {@enum.value DaysOfWeekSun Sunday }
  TFhirDaysOfWeekList = set of TFhirDaysOfWeek;

  {@Enum TFhirInstanceAvailability
    Availability of the resource
  }
  TFhirInstanceAvailability = (
    InstanceAvailabilityNull,  {@enum.value InstanceAvailabilityNull Value is missing from Instance }
    InstanceAvailabilityONLINE, {@enum.value InstanceAvailabilityONLINE  }
    InstanceAvailabilityOFFLINE, {@enum.value InstanceAvailabilityOFFLINE  }
    InstanceAvailabilityNEARLINE, {@enum.value InstanceAvailabilityNEARLINE  }
    InstanceAvailabilityUNAVAILABLE); {@enum.value InstanceAvailabilityUNAVAILABLE  }
  TFhirInstanceAvailabilityList = set of TFhirInstanceAvailability;

  {@Enum TFhirMedicationAdminStatus
    A set of codes indicating the current status of an Immunization
  }
  TFhirMedicationAdminStatus = (
    MedicationAdminStatusNull,  {@enum.value MedicationAdminStatusNull Value is missing from Instance }
    MedicationAdminStatusInProgress, {@enum.value MedicationAdminStatusInProgress The administration has started but has not yet completed. }
    MedicationAdminStatusOnHold, {@enum.value MedicationAdminStatusOnHold Actions implied by the administration have been temporarily halted, but are expected to continue later. May also be called "suspended". }
    MedicationAdminStatusCompleted, {@enum.value MedicationAdminStatusCompleted All actions that are implied by the administration have occurred. }
    MedicationAdminStatusEnteredInError, {@enum.value MedicationAdminStatusEnteredInError The administration was entered in error and therefore nullified. }
    MedicationAdminStatusStopped); {@enum.value MedicationAdminStatusStopped Actions implied by the administration have been permanently halted, before all of them occurred. }
  TFhirMedicationAdminStatusList = set of TFhirMedicationAdminStatus;

  {@Enum TFhirGuideDependencyType
    How a dependency is represented when the guide is published
  }
  TFhirGuideDependencyType = (
    GuideDependencyTypeNull,  {@enum.value GuideDependencyTypeNull Value is missing from Instance }
    GuideDependencyTypeReference, {@enum.value GuideDependencyTypeReference The guide is referred to by URL }
    GuideDependencyTypeInclusion); {@enum.value GuideDependencyTypeInclusion The guide is embedded in this guide when published }
  TFhirGuideDependencyTypeList = set of TFhirGuideDependencyType;

  {@Enum TFhirGuideResourcePurpose
    Why a resource is included in the guide
  }
  TFhirGuideResourcePurpose = (
    GuideResourcePurposeNull,  {@enum.value GuideResourcePurposeNull Value is missing from Instance }
    GuideResourcePurposeExample, {@enum.value GuideResourcePurposeExample The resource is intended as an example }
    GuideResourcePurposeTerminology, {@enum.value GuideResourcePurposeTerminology The resource defines a value set or concept map used in the Implementation Guide }
    GuideResourcePurposeProfile, {@enum.value GuideResourcePurposeProfile The resource defines a profile (StructureDefinition) that is used in the implementation guide }
    GuideResourcePurposeExtension, {@enum.value GuideResourcePurposeExtension The resource defines an extension (StructureDefinition) that is used in the implementation guide }
    GuideResourcePurposeDictionary, {@enum.value GuideResourcePurposeDictionary The resource contains a dictionary that is part of the implementation guide }
    GuideResourcePurposeLogical); {@enum.value GuideResourcePurposeLogical The resource defines a logical model (in a StructureDefinition) that is used in the implementation guide }
  TFhirGuideResourcePurposeList = set of TFhirGuideResourcePurpose;

  {@Enum TFhirGuidePageKind
    The kind of an included page
  }
  TFhirGuidePageKind = (
    GuidePageKindNull,  {@enum.value GuidePageKindNull Value is missing from Instance }
    GuidePageKindPage, {@enum.value GuidePageKindPage This is a page of content that is included in the implementation guide. It has no particular function }
    GuidePageKindExample, {@enum.value GuidePageKindExample This is a page that represents a human readable rendering of an example }
    GuidePageKindList, {@enum.value GuidePageKindList This is a page that represents a list of resources of one or more types }
    GuidePageKindInclude, {@enum.value GuidePageKindInclude This is a page that is where an included guide is injected }
    GuidePageKindDirectory, {@enum.value GuidePageKindDirectory This is a page that lists the resources of a given type, and also creates pages for all the listed types as other pages in the section }
    GuidePageKindDictionary, {@enum.value GuidePageKindDictionary This is a page that creates the listed resources as a dictionary }
    GuidePageKindToc, {@enum.value GuidePageKindToc This is a generated page that contains the table of contents }
    GuidePageKindResource); {@enum.value GuidePageKindResource This is a page that represents a presented resource. This is typically used for generated conformance resource presentations }
  TFhirGuidePageKindList = set of TFhirGuidePageKind;

  {@Enum TFhirListStatus
    The current state of the list
  }
  TFhirListStatus = (
    ListStatusNull,  {@enum.value ListStatusNull Value is missing from Instance }
    ListStatusCurrent, {@enum.value ListStatusCurrent The list is considered to be an active part of the patient's record. }
    ListStatusRetired, {@enum.value ListStatusRetired The list is "old" and should no longer be considered accurate or relevant. }
    ListStatusEnteredInError); {@enum.value ListStatusEnteredInError The list was never accurate.  It is retained for medico-legal purposes only. }
  TFhirListStatusList = set of TFhirListStatus;

  {@Enum TFhirLocationStatus
    Indicates whether the location is still in use
  }
  TFhirLocationStatus = (
    LocationStatusNull,  {@enum.value LocationStatusNull Value is missing from Instance }
    LocationStatusActive, {@enum.value LocationStatusActive The location is operational }
    LocationStatusSuspended, {@enum.value LocationStatusSuspended The location is temporarily closed }
    LocationStatusInactive); {@enum.value LocationStatusInactive The location is no longer used }
  TFhirLocationStatusList = set of TFhirLocationStatus;

  {@Enum TFhirLocationMode
    Indicates whether a resource instance represents a specific location or a class of locations
  }
  TFhirLocationMode = (
    LocationModeNull,  {@enum.value LocationModeNull Value is missing from Instance }
    LocationModeInstance, {@enum.value LocationModeInstance The Location resource represents a specific instance of a Location (e.g. Operating Theatre 1A) }
    LocationModeKind); {@enum.value LocationModeKind The Location represents a class of Locations (e.g. Any Operating Theatre). Although this class of locations could be constrained within a specific boundary (such as organization, or parent location, address etc.) }
  TFhirLocationModeList = set of TFhirLocationMode;

  {@Enum TFhirDigitalMediaType
    Whether the Media is a photo, video, or audio
  }
  TFhirDigitalMediaType = (
    DigitalMediaTypeNull,  {@enum.value DigitalMediaTypeNull Value is missing from Instance }
    DigitalMediaTypePhoto, {@enum.value DigitalMediaTypePhoto The media consists of one or more unmoving images, including photographs, computer-generated graphs and charts, and scanned documents }
    DigitalMediaTypeVideo, {@enum.value DigitalMediaTypeVideo The media consists of a series of frames that capture a moving image }
    DigitalMediaTypeAudio); {@enum.value DigitalMediaTypeAudio The media consists of a sound recording }
  TFhirDigitalMediaTypeList = set of TFhirDigitalMediaType;

  {@Enum TFhirMedicationDispenseStatus
    A code specifying the state of the dispense event.
  }
  TFhirMedicationDispenseStatus = (
    MedicationDispenseStatusNull,  {@enum.value MedicationDispenseStatusNull Value is missing from Instance }
    MedicationDispenseStatusInProgress, {@enum.value MedicationDispenseStatusInProgress The dispense has started but has not yet completed. }
    MedicationDispenseStatusOnHold, {@enum.value MedicationDispenseStatusOnHold Actions implied by the administration have been temporarily halted, but are expected to continue later. May also be called "suspended" }
    MedicationDispenseStatusCompleted, {@enum.value MedicationDispenseStatusCompleted All actions that are implied by the dispense have occurred. }
    MedicationDispenseStatusEnteredInError, {@enum.value MedicationDispenseStatusEnteredInError The dispense was entered in error and therefore nullified. }
    MedicationDispenseStatusStopped); {@enum.value MedicationDispenseStatusStopped Actions implied by the dispense have been permanently halted, before all of them occurred. }
  TFhirMedicationDispenseStatusList = set of TFhirMedicationDispenseStatus;

  {@Enum TFhirMedicationOrderStatus
    A code specifying the state of the prescribing event. Describes the lifecycle of the prescription.
  }
  TFhirMedicationOrderStatus = (
    MedicationOrderStatusNull,  {@enum.value MedicationOrderStatusNull Value is missing from Instance }
    MedicationOrderStatusActive, {@enum.value MedicationOrderStatusActive The prescription is 'actionable', but not all actions that are implied by it have occurred yet. }
    MedicationOrderStatusOnHold, {@enum.value MedicationOrderStatusOnHold Actions implied by the prescription are to be temporarily halted, but are expected to continue later.  May also be called "suspended". }
    MedicationOrderStatusCompleted, {@enum.value MedicationOrderStatusCompleted All actions that are implied by the prescription have occurred. }
    MedicationOrderStatusEnteredInError, {@enum.value MedicationOrderStatusEnteredInError The prescription was entered in error. }
    MedicationOrderStatusStopped, {@enum.value MedicationOrderStatusStopped Actions implied by the prescription are to be permanently halted, before all of them occurred. }
    MedicationOrderStatusDraft); {@enum.value MedicationOrderStatusDraft The prescription is not yet 'actionable', i.e. it is a work in progress, requires sign-off or verification, needs to be run through decision support process. }
  TFhirMedicationOrderStatusList = set of TFhirMedicationOrderStatus;

  {@Enum TFhirMedicationStatementStatus
    A set of codes indicating the current status of a MedicationStatement
  }
  TFhirMedicationStatementStatus = (
    MedicationStatementStatusNull,  {@enum.value MedicationStatementStatusNull Value is missing from Instance }
    MedicationStatementStatusActive, {@enum.value MedicationStatementStatusActive The medication is still being taken. }
    MedicationStatementStatusCompleted, {@enum.value MedicationStatementStatusCompleted The medication is no longer being taken. }
    MedicationStatementStatusEnteredInError, {@enum.value MedicationStatementStatusEnteredInError The statement was entered in error. }
    MedicationStatementStatusIntended); {@enum.value MedicationStatementStatusIntended The medication may be taken at some time in the future. }
  TFhirMedicationStatementStatusList = set of TFhirMedicationStatementStatus;

  {@Enum TFhirResponseCode
    The kind of response to a message
  }
  TFhirResponseCode = (
    ResponseCodeNull,  {@enum.value ResponseCodeNull Value is missing from Instance }
    ResponseCodeOk, {@enum.value ResponseCodeOk The message was accepted and processed without error }
    ResponseCodeTransientError, {@enum.value ResponseCodeTransientError Some internal unexpected error occurred - wait and try again. Note - this is usually used for things like database unavailable, which may be expected to resolve, though human intervention may be required }
    ResponseCodeFatalError); {@enum.value ResponseCodeFatalError The message was rejected because of some content in it. There is no point in re-sending without change. The response narrative SHALL describe what the issue is. }
  TFhirResponseCodeList = set of TFhirResponseCode;

  {@Enum TFhirNamingsystemType
    Identifies the purpose of the namingsystem
  }
  TFhirNamingsystemType = (
    NamingsystemTypeNull,  {@enum.value NamingsystemTypeNull Value is missing from Instance }
    NamingsystemTypeCodesystem, {@enum.value NamingsystemTypeCodesystem The naming system is used to define concepts and symbols to represent those concepts; e.g. UCUM, LOINC, NDC code, local lab codes, etc. }
    NamingsystemTypeIdentifier, {@enum.value NamingsystemTypeIdentifier The namingsystem is used to manage identifiers (e.g. license numbers, order numbers, etc.) }
    NamingsystemTypeRoot); {@enum.value NamingsystemTypeRoot The namingsystem is used as the root for other identifiers and namingsystems }
  TFhirNamingsystemTypeList = set of TFhirNamingsystemType;

  {@Enum TFhirNamingsystemIdentifierType
    Identifies the style of unique identifier used to identify a namepace
  }
  TFhirNamingsystemIdentifierType = (
    NamingsystemIdentifierTypeNull,  {@enum.value NamingsystemIdentifierTypeNull Value is missing from Instance }
    NamingsystemIdentifierTypeOid, {@enum.value NamingsystemIdentifierTypeOid An ISO object identifier; e.g. 1.2.3.4.5 }
    NamingsystemIdentifierTypeUuid, {@enum.value NamingsystemIdentifierTypeUuid A universally unique identifier of the form a5afddf4-e880-459b-876e-e4591b0acc11 }
    NamingsystemIdentifierTypeUri, {@enum.value NamingsystemIdentifierTypeUri A uniform resource identifier (ideally a URL - uniform resource locator); e.g. http://unitsofmeasure.org }
    NamingsystemIdentifierTypeOther); {@enum.value NamingsystemIdentifierTypeOther Some other type of unique identifier.  E.g. HL7-assigned reserved string such as LN for LOINC }
  TFhirNamingsystemIdentifierTypeList = set of TFhirNamingsystemIdentifierType;

  {@Enum TFhirNutritionOrderStatus
    Codes specifying the state of the request. Describes the lifecycle of the nutrition order.
  }
  TFhirNutritionOrderStatus = (
    NutritionOrderStatusNull,  {@enum.value NutritionOrderStatusNull Value is missing from Instance }
    NutritionOrderStatusProposed, {@enum.value NutritionOrderStatusProposed The request has been proposed }
    NutritionOrderStatusDraft, {@enum.value NutritionOrderStatusDraft The request is in preliminary form prior to being sent }
    NutritionOrderStatusPlanned, {@enum.value NutritionOrderStatusPlanned The request has been planned }
    NutritionOrderStatusRequested, {@enum.value NutritionOrderStatusRequested The request has been placed }
    NutritionOrderStatusActive, {@enum.value NutritionOrderStatusActive The request is 'actionable', but not all actions that are implied by it have occurred yet. }
    NutritionOrderStatusOnHold, {@enum.value NutritionOrderStatusOnHold Actions implied by the request have been temporarily halted, but are expected to continue later. May also be called "suspended". }
    NutritionOrderStatusCompleted, {@enum.value NutritionOrderStatusCompleted All actions that are implied by the order have occurred and no continuation is planned (this will rarely be made explicit). }
    NutritionOrderStatusCancelled); {@enum.value NutritionOrderStatusCancelled The request has been withdrawn and is no longer actionable. }
  TFhirNutritionOrderStatusList = set of TFhirNutritionOrderStatus;

  {@Enum TFhirObservationStatus
    Codes providing the status of an observation
  }
  TFhirObservationStatus = (
    ObservationStatusNull,  {@enum.value ObservationStatusNull Value is missing from Instance }
    ObservationStatusRegistered, {@enum.value ObservationStatusRegistered The existence of the observation is registered, but there is no result yet available }
    ObservationStatusPreliminary, {@enum.value ObservationStatusPreliminary This is an initial or interim observation: data may be incomplete or unverified }
    ObservationStatusFinal, {@enum.value ObservationStatusFinal The observation is complete and verified by an authorized person }
    ObservationStatusAmended, {@enum.value ObservationStatusAmended The observation has been modified subsequent to being Final, and is complete and verified by an authorized person }
    ObservationStatusCancelled, {@enum.value ObservationStatusCancelled The observation is unavailable because the measurement was not started or not completed (also sometimes called "aborted") }
    ObservationStatusEnteredInError, {@enum.value ObservationStatusEnteredInError The observation has been withdrawn following previous Final release }
    ObservationStatusUnknown); {@enum.value ObservationStatusUnknown The observation status is unknown.  Note that "unknown" is a value of last resort and every attempt should be made to provide a meaningful value other than "unknown" }
  TFhirObservationStatusList = set of TFhirObservationStatus;

  {@Enum TFhirObservationRelationshiptypes
    Codes specifying how two observations are related
  }
  TFhirObservationRelationshiptypes = (
    ObservationRelationshiptypesNull,  {@enum.value ObservationRelationshiptypesNull Value is missing from Instance }
    ObservationRelationshiptypesHasMember, {@enum.value ObservationRelationshiptypesHasMember This observation is a group observation (e.g. a battery, a panel of tests, a set of vital sign measurements) that includes the target as a member of the group }
    ObservationRelationshiptypesDerivedFrom, {@enum.value ObservationRelationshiptypesDerivedFrom The target resource (Observation or QuestionnaireAnswer) is part of the information from which this observation value is derived. (e.g. calculated anion gap, Apgar score)  NOTE:  "derived-from" is only logical choice when referencing QuestionnaireAnswer }
    ObservationRelationshiptypesSequelTo, {@enum.value ObservationRelationshiptypesSequelTo This observation follows the target observation (e.g. timed tests such as Glucose Tolerance Test) }
    ObservationRelationshiptypesReplaces, {@enum.value ObservationRelationshiptypesReplaces This observation replaces a previous observation (i.e. a revised value). The target observation is now obsolete }
    ObservationRelationshiptypesQualifiedBy, {@enum.value ObservationRelationshiptypesQualifiedBy The value of the target observation qualifies (refines) the semantics of the source observation (e.g. a lipemia measure target from a plasma measure) }
    ObservationRelationshiptypesInterferedBy); {@enum.value ObservationRelationshiptypesInterferedBy The value of the target observation interferes (degrades quality, or prevents valid observation) with the semantics of the source observation (e.g. a hemolysis measure target from a plasma potassium measure which has no value) }
  TFhirObservationRelationshiptypesList = set of TFhirObservationRelationshiptypes;

  {@Enum TFhirOperationKind
    Whether an operation is a normal operation or a query
  }
  TFhirOperationKind = (
    OperationKindNull,  {@enum.value OperationKindNull Value is missing from Instance }
    OperationKindOperation, {@enum.value OperationKindOperation This operation is invoked as an operation }
    OperationKindQuery); {@enum.value OperationKindQuery This operation is a named query, invoked using the search mechanism }
  TFhirOperationKindList = set of TFhirOperationKind;

  {@Enum TFhirOperationParameterUse
    Whether an operation parameter is an input or an output parameter
  }
  TFhirOperationParameterUse = (
    OperationParameterUseNull,  {@enum.value OperationParameterUseNull Value is missing from Instance }
    OperationParameterUseIn, {@enum.value OperationParameterUseIn This is an input parameter }
    OperationParameterUseOut); {@enum.value OperationParameterUseOut This is an output parameter }
  TFhirOperationParameterUseList = set of TFhirOperationParameterUse;

  {@Enum TFhirValuesetOperationParameterType
    The type of a parameter
  }
  TFhirValuesetOperationParameterType = (
    ValuesetOperationParameterTypeNull,  {@enum.value ValuesetOperationParameterTypeNull Value is missing from Instance }
    ValuesetOperationParameterTypeNumber, {@enum.value ValuesetOperationParameterTypeNumber Search parameter SHALL be a number (a whole number, or a decimal). }
    ValuesetOperationParameterTypeDate, {@enum.value ValuesetOperationParameterTypeDate Search parameter is on a date/time. The date format is the standard XML format, though other formats may be supported. }
    ValuesetOperationParameterTypeString, {@enum.value ValuesetOperationParameterTypeString Search parameter is a simple string, like a name part. Search is case-insensitive and accent-insensitive. May match just the start of a string. String parameters may contain spaces. }
    ValuesetOperationParameterTypeToken, {@enum.value ValuesetOperationParameterTypeToken Search parameter on a coded element or identifier. May be used to search through the text, displayname, code and code/codesystem (for codes) and label, system and key (for identifier). Its value is either a string or a pair of namespace and value, separated by a "|", depending on the modifier used. }
    ValuesetOperationParameterTypeReference, {@enum.value ValuesetOperationParameterTypeReference A reference to another resource. }
    ValuesetOperationParameterTypeComposite, {@enum.value ValuesetOperationParameterTypeComposite A composite search parameter that combines a search on two values together. }
    ValuesetOperationParameterTypeQuantity, {@enum.value ValuesetOperationParameterTypeQuantity A search parameter that searches on a quantity. }
    ValuesetOperationParameterTypeUri, {@enum.value ValuesetOperationParameterTypeUri A search parameter that searches on a URI (RFC 3986). }
    ValuesetOperationParameterTypeAddress, {@enum.value ValuesetOperationParameterTypeAddress There is a variety of postal address formats defined around the world. This format defines a superset that is the basis for all addresses around the world. }
    ValuesetOperationParameterTypeAge, {@enum.value ValuesetOperationParameterTypeAge  }
    ValuesetOperationParameterTypeAnnotation, {@enum.value ValuesetOperationParameterTypeAnnotation A  text note which also  contains information about who made the statement and when. }
    ValuesetOperationParameterTypeAttachment, {@enum.value ValuesetOperationParameterTypeAttachment For referring to data content defined in other formats. }
    ValuesetOperationParameterTypeBackboneElement, {@enum.value ValuesetOperationParameterTypeBackboneElement Base definition for all elements that are defined inside a resource - but not those in a data type. }
    ValuesetOperationParameterTypeCodeableConcept, {@enum.value ValuesetOperationParameterTypeCodeableConcept A concept that may be defined by a formal reference to a terminology or ontology or may be provided by text. }
    ValuesetOperationParameterTypeCoding, {@enum.value ValuesetOperationParameterTypeCoding A reference to a code defined by a terminology system. }
    ValuesetOperationParameterTypeContactPoint, {@enum.value ValuesetOperationParameterTypeContactPoint Details for All kinds of technology mediated contact points for a person or organization, including telephone, email, etc. }
    ValuesetOperationParameterTypeCount, {@enum.value ValuesetOperationParameterTypeCount  }
    ValuesetOperationParameterTypeDistance, {@enum.value ValuesetOperationParameterTypeDistance  }
    ValuesetOperationParameterTypeDuration, {@enum.value ValuesetOperationParameterTypeDuration  }
    ValuesetOperationParameterTypeElement, {@enum.value ValuesetOperationParameterTypeElement Base definition for all elements in a resource. }
    ValuesetOperationParameterTypeElementDefinition, {@enum.value ValuesetOperationParameterTypeElementDefinition Captures constraints on each element within the resource, profile, or extension. }
    ValuesetOperationParameterTypeExtension, {@enum.value ValuesetOperationParameterTypeExtension Optional Extensions Element - found in all resources. }
    ValuesetOperationParameterTypeHumanName, {@enum.value ValuesetOperationParameterTypeHumanName A human's name with the ability to identify parts and usage. }
    ValuesetOperationParameterTypeIdentifier, {@enum.value ValuesetOperationParameterTypeIdentifier A technical identifier - identifies some entity uniquely and unambiguously. }
    ValuesetOperationParameterTypeMeta, {@enum.value ValuesetOperationParameterTypeMeta The metadata about a resource. This is content in the resource that is maintained by the infrastructure. Changes to the content may not always be associated with version changes to the resource. }
    ValuesetOperationParameterTypeMoney, {@enum.value ValuesetOperationParameterTypeMoney  }
    ValuesetOperationParameterTypeNarrative, {@enum.value ValuesetOperationParameterTypeNarrative A human-readable formatted text, including images. }
    ValuesetOperationParameterTypePeriod, {@enum.value ValuesetOperationParameterTypePeriod A time period defined by a start and end date and optionally time. }
    ValuesetOperationParameterTypeQuantity1, {@enum.value ValuesetOperationParameterTypeQuantity1 A measured amount (or an amount that can potentially be measured). Note that measured amounts include amounts that are not precisely quantified, including amounts involving arbitrary units and floating currencies. }
    ValuesetOperationParameterTypeRange, {@enum.value ValuesetOperationParameterTypeRange A set of ordered Quantities defined by a low and high limit. }
    ValuesetOperationParameterTypeRatio, {@enum.value ValuesetOperationParameterTypeRatio A relationship of two Quantity values - expressed as a numerator and a denominator. }
    ValuesetOperationParameterTypeReference1, {@enum.value ValuesetOperationParameterTypeReference1 A reference from one resource to another. }
    ValuesetOperationParameterTypeSampledData, {@enum.value ValuesetOperationParameterTypeSampledData A series of measurements taken by a device, with upper and lower limits. There may be more than one dimension in the data. }
    ValuesetOperationParameterTypeSignature, {@enum.value ValuesetOperationParameterTypeSignature A digital signature along with supporting context. The signature may be electronic/cryptographic in nature, or a graphical image representing a hand-written signature, or a signature process. Different Signature approaches have different utilities. }
    ValuesetOperationParameterTypeSimpleQuantity, {@enum.value ValuesetOperationParameterTypeSimpleQuantity  }
    ValuesetOperationParameterTypeTiming, {@enum.value ValuesetOperationParameterTypeTiming Specifies an event that may occur multiple times. Timing schedules are used to record when things are expected or requested to occur. The most common usage is in dosage instructions for medications. They are also used when planning care of various kinds. }
    ValuesetOperationParameterTypeBase64Binary, {@enum.value ValuesetOperationParameterTypeBase64Binary A stream of bytes }
    ValuesetOperationParameterTypeBoolean, {@enum.value ValuesetOperationParameterTypeBoolean Value of "true" or "false" }
    ValuesetOperationParameterTypeCode, {@enum.value ValuesetOperationParameterTypeCode A string which has at least one character and no leading or trailing whitespace and where there is no whitespace other than single spaces in the contents }
    ValuesetOperationParameterTypeDateTime, {@enum.value ValuesetOperationParameterTypeDateTime A date, date-time or partial date (e.g. just year or year + month).  If hours and minutes are specified, a time zone SHALL be populated. The format is a union of the schema types gYear, gYearMonth, date and dateTime. Seconds must be provided due to schema type constraints but may be zero-filled and may be ignored.                 Dates SHALL be valid dates. }
    ValuesetOperationParameterTypeDecimal, {@enum.value ValuesetOperationParameterTypeDecimal A rational number with implicit precision }
    ValuesetOperationParameterTypeId, {@enum.value ValuesetOperationParameterTypeId Any combination of letters, numerals, "-" and ".", with a length limit of 64 characters.  (This might be an integer, an unprefixed OID, UUID or any other identifier pattern that meets these constraints.)  Ids are case-insensitive. }
    ValuesetOperationParameterTypeInstant, {@enum.value ValuesetOperationParameterTypeInstant An instant in time - known at least to the second }
    ValuesetOperationParameterTypeInteger, {@enum.value ValuesetOperationParameterTypeInteger A whole number }
    ValuesetOperationParameterTypeMarkdown, {@enum.value ValuesetOperationParameterTypeMarkdown A string that may contain markdown syntax for optional processing by a mark down presentation engine }
    ValuesetOperationParameterTypeOid, {@enum.value ValuesetOperationParameterTypeOid An oid represented as a URI }
    ValuesetOperationParameterTypePositiveInt, {@enum.value ValuesetOperationParameterTypePositiveInt An integer with a value that is positive (e.g. >0) }
    ValuesetOperationParameterTypeTime, {@enum.value ValuesetOperationParameterTypeTime A time during the day, with no date specified }
    ValuesetOperationParameterTypeUnsignedInt, {@enum.value ValuesetOperationParameterTypeUnsignedInt An integer with a value that is not negative (e.g. >= 0) }
    ValuesetOperationParameterTypeUuid, {@enum.value ValuesetOperationParameterTypeUuid A UUID, represented as a URI }
    ValuesetOperationParameterTypeXhtml, {@enum.value ValuesetOperationParameterTypeXhtml XHTML format, as defined by W3C, but restricted usage (mainly, no active content) }
    ValuesetOperationParameterTypeAccount, {@enum.value ValuesetOperationParameterTypeAccount A financial tool for tracking value accrued for a particular purpose.  In the healthcare field, used to track charges for a patient, cost centres, etc. }
    ValuesetOperationParameterTypeAllergyIntolerance, {@enum.value ValuesetOperationParameterTypeAllergyIntolerance Risk of harmful or undesirable, physiological response which is unique to an individual and associated with exposure to a substance. }
    ValuesetOperationParameterTypeAppointment, {@enum.value ValuesetOperationParameterTypeAppointment A booking of a healthcare event among patient(s), practitioner(s), related person(s) and/or device(s) for a specific date/time. This may result in one or more Encounter(s). }
    ValuesetOperationParameterTypeAppointmentResponse, {@enum.value ValuesetOperationParameterTypeAppointmentResponse A reply to an appointment request for a patient and/or practitioner(s), such as a confirmation or rejection. }
    ValuesetOperationParameterTypeAuditEvent, {@enum.value ValuesetOperationParameterTypeAuditEvent A record of an event made for purposes of maintaining a security log. Typical uses include detection of intrusion attempts and monitoring for inappropriate usage. }
    ValuesetOperationParameterTypeBasic, {@enum.value ValuesetOperationParameterTypeBasic Basic is used for handling concepts not yet defined in FHIR, narrative-only resources that don't map to an existing resource, and custom resources not appropriate for inclusion in the FHIR specification. }
    ValuesetOperationParameterTypeBinary, {@enum.value ValuesetOperationParameterTypeBinary A binary resource can contain any content, whether text, image, pdf, zip archive, etc. }
    ValuesetOperationParameterTypeBodySite, {@enum.value ValuesetOperationParameterTypeBodySite Record details about the anatomical location of a specimen or body part.  This resource may be used when a coded concept does not provide the necessary detail needed for the use case. }
    ValuesetOperationParameterTypeBundle, {@enum.value ValuesetOperationParameterTypeBundle A container for a collection of resources. }
    ValuesetOperationParameterTypeCarePlan, {@enum.value ValuesetOperationParameterTypeCarePlan Describes the intention of how one or more practitioners intend to deliver care for a particular patient, group or community for a period of time, possibly limited to care for a specific condition or set of conditions. }
    ValuesetOperationParameterTypeClaim, {@enum.value ValuesetOperationParameterTypeClaim A provider issued list of services and products provided, or to be provided, to a patient which is provided to an insurer for payment recovery. }
    ValuesetOperationParameterTypeClaimResponse, {@enum.value ValuesetOperationParameterTypeClaimResponse This resource provides the adjudication details from the processing of a Claim resource. }
    ValuesetOperationParameterTypeClinicalImpression, {@enum.value ValuesetOperationParameterTypeClinicalImpression A record of a clinical assessment performed to determine what problem(s) may affect the patient and before planning the treatments or management strategies that are best to manage a patient's condition. Assessments are often 1:1 with a clinical consultation / encounter,  but this varies greatly depending on the clinical workflow. This resource is called "ClinicalImpression" rather than "ClinicalAssessment" to avoid confusion with the recording of assessment tools such as Apgar score. }
    ValuesetOperationParameterTypeCommunication, {@enum.value ValuesetOperationParameterTypeCommunication An occurrence of information being transmitted; e.g. an alert that was sent to a responsible provider, a public health agency was notified about a reportable condition. }
    ValuesetOperationParameterTypeCommunicationRequest, {@enum.value ValuesetOperationParameterTypeCommunicationRequest A request to convey information; e.g. the CDS system proposes that an alert be sent to a responsible provider, the CDS system proposes that the public health agency be notified about a reportable condition. }
    ValuesetOperationParameterTypeComposition, {@enum.value ValuesetOperationParameterTypeComposition A set of healthcare-related information that is assembled together into a single logical document that provides a single coherent statement of meaning, establishes its own context and that has clinical attestation with regard to who is making the statement. While a Composition defines the structure, it does not actually contain the content: rather the full content of a document is contained in a Bundle, of which the Composition is the first resource contained. }
    ValuesetOperationParameterTypeConceptMap, {@enum.value ValuesetOperationParameterTypeConceptMap A statement of relationships from one set of concepts to one or more other concepts - either code systems or data elements, or classes in class models. }
    ValuesetOperationParameterTypeCondition, {@enum.value ValuesetOperationParameterTypeCondition Use to record detailed information about conditions, problems or diagnoses recognized by a clinician. There are many uses including: recording a Diagnosis during an Encounter; populating a problem List or a Summary Statement, such as a Discharge Summary. }
    ValuesetOperationParameterTypeConformance, {@enum.value ValuesetOperationParameterTypeConformance A conformance statement is a set of capabilities of a FHIR Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation. }
    ValuesetOperationParameterTypeContract, {@enum.value ValuesetOperationParameterTypeContract A formal agreement between parties regarding the conduct of business, exchange of information or other matters. }
    ValuesetOperationParameterTypeCoverage, {@enum.value ValuesetOperationParameterTypeCoverage Financial instrument which may be used to pay for or reimburse for health care products and services. }
    ValuesetOperationParameterTypeDataElement, {@enum.value ValuesetOperationParameterTypeDataElement The formal description of a single piece of information that can be gathered and reported. }
    ValuesetOperationParameterTypeDetectedIssue, {@enum.value ValuesetOperationParameterTypeDetectedIssue Indicates an actual or potential clinical issue with or between one or more active or proposed clinical actions for a patient; e.g. Drug-drug interaction, Ineffective treatment frequency, Procedure-condition conflict, etc. }
    ValuesetOperationParameterTypeDevice, {@enum.value ValuesetOperationParameterTypeDevice This resource identifies an instance of a manufactured item that is used in the provision of healthcare without being substantially changed through that activity. The device may be a medical or non-medical device.  Medical devices includes durable (reusable) medical equipment, implantable devices, as well as disposable equipment used for diagnostic, treatment, and research for healthcare and public health.  Non-medical devices may include items such as a machine, cellphone, computer, application, etc. }
    ValuesetOperationParameterTypeDeviceComponent, {@enum.value ValuesetOperationParameterTypeDeviceComponent Describes the characteristics, operational status and capabilities of a medical-related component of a medical device. }
    ValuesetOperationParameterTypeDeviceMetric, {@enum.value ValuesetOperationParameterTypeDeviceMetric Describes a measurement, calculation or setting capability of a medical device. }
    ValuesetOperationParameterTypeDeviceUseRequest, {@enum.value ValuesetOperationParameterTypeDeviceUseRequest Represents a request for a patient to employ a medical device. The device may be an implantable device, or an external assistive device, such as a walker. }
    ValuesetOperationParameterTypeDeviceUseStatement, {@enum.value ValuesetOperationParameterTypeDeviceUseStatement A record of a device being used by a patient where the record is the result of a report from the patient or another clinician. }
    ValuesetOperationParameterTypeDiagnosticOrder, {@enum.value ValuesetOperationParameterTypeDiagnosticOrder A record of a request for a diagnostic investigation service to be performed. }
    ValuesetOperationParameterTypeDiagnosticReport, {@enum.value ValuesetOperationParameterTypeDiagnosticReport The findings and interpretation of diagnostic  tests performed on patients, groups of patients, devices, and locations, and/or specimens derived from these. The report includes clinical context such as requesting and provider information, and some mix of atomic results, images, textual and coded interpretation, and formatted representation of diagnostic reports. }
    ValuesetOperationParameterTypeDocumentManifest, {@enum.value ValuesetOperationParameterTypeDocumentManifest A manifest that defines a set of documents. }
    ValuesetOperationParameterTypeDocumentReference, {@enum.value ValuesetOperationParameterTypeDocumentReference A reference to a document. }
    ValuesetOperationParameterTypeDomainResource, {@enum.value ValuesetOperationParameterTypeDomainResource --- Abstract Type! ---A resource that includes narrative, extensions, and contained resources. }
    ValuesetOperationParameterTypeEligibilityRequest, {@enum.value ValuesetOperationParameterTypeEligibilityRequest This resource provides the insurance eligibility details from the insurer regarding a specified coverage and optionally some class of service. }
    ValuesetOperationParameterTypeEligibilityResponse, {@enum.value ValuesetOperationParameterTypeEligibilityResponse This resource provides eligibility and plan details from the processing of an Eligibility resource. }
    ValuesetOperationParameterTypeEncounter, {@enum.value ValuesetOperationParameterTypeEncounter An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient. }
    ValuesetOperationParameterTypeEnrollmentRequest, {@enum.value ValuesetOperationParameterTypeEnrollmentRequest This resource provides the insurance Enrollment details to the insurer regarding a specified coverage. }
    ValuesetOperationParameterTypeEnrollmentResponse, {@enum.value ValuesetOperationParameterTypeEnrollmentResponse This resource provides Enrollment and plan details from the processing of an Enrollment resource. }
    ValuesetOperationParameterTypeEpisodeOfCare, {@enum.value ValuesetOperationParameterTypeEpisodeOfCare An association between a patient and an organization / healthcare provider(s) during which time encounters may occur. The managing organization assumes a level of responsibility for the patient during this time. }
    ValuesetOperationParameterTypeExplanationOfBenefit, {@enum.value ValuesetOperationParameterTypeExplanationOfBenefit This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided. }
    ValuesetOperationParameterTypeFamilyMemberHistory, {@enum.value ValuesetOperationParameterTypeFamilyMemberHistory Significant health events and conditions for a person related to the patient relevant in the context of care for the patient. }
    ValuesetOperationParameterTypeFlag, {@enum.value ValuesetOperationParameterTypeFlag Prospective warnings of potential issues when providing care to the patient. }
    ValuesetOperationParameterTypeGoal, {@enum.value ValuesetOperationParameterTypeGoal Describes the intended objective(s) for a patient, group or organization care, for example, weight loss, restoring an activity of daily living, obtaining herd immunity via immunization, meeting a process improvement objective, etc.;. }
    ValuesetOperationParameterTypeGroup, {@enum.value ValuesetOperationParameterTypeGroup Represents a defined collection of entities that may be discussed or acted upon collectively but which are not expected to act collectively and are not formally or legally recognized; i.e. a collection of entities that isn't an Organization. }
    ValuesetOperationParameterTypeHealthcareService, {@enum.value ValuesetOperationParameterTypeHealthcareService The details of a healthcare service available at a location. }
    ValuesetOperationParameterTypeImagingObjectSelection, {@enum.value ValuesetOperationParameterTypeImagingObjectSelection A manifest of a set of DICOM Service-Object Pair Instances (SOP Instances).  The referenced SOP Instances (images or other content) are for a single patient, and may be from one or more studies. The referenced SOP Instances have been selected for a purpose, such as quality assurance, conference, or consult. Reflecting that range of purposes, typical ImagingObjectSelection resources may include all SOP Instances in a study (perhaps for sharing through a Health Information Exchange); key images from multiple studies (for reference by a referring or treating physician); a multi-frame ultrasound instance ("cine" video clip) and a set of measurements taken from that instance (for inclusion in a teaching file); and so on. }
    ValuesetOperationParameterTypeImagingStudy, {@enum.value ValuesetOperationParameterTypeImagingStudy Representation of the content produced in a DICOM imaging study. A study comprises a set of Series, each of which includes a set of Service-Object Pair Instances (SOP Instances - images or other data) acquired or produced in a common context.  A Series is of only one modality (e.g. X-ray, CT, MR, ultrasound), but a Study may have multiple Series of different modalities. }
    ValuesetOperationParameterTypeImmunization, {@enum.value ValuesetOperationParameterTypeImmunization Describes the event of a patient being administered a vaccination or a record of a vaccination as reported by a patient, a clinician or another party and may include vaccine reaction information and what vaccination protocol was followed. }
    ValuesetOperationParameterTypeImmunizationRecommendation, {@enum.value ValuesetOperationParameterTypeImmunizationRecommendation A patient?s point-in-time immunization and recommendation (i.e. forecasting a patient?s immunization eligibility according to a published schedule) with optional supporting justification. }
    ValuesetOperationParameterTypeImplementationGuide, {@enum.value ValuesetOperationParameterTypeImplementationGuide A set of rules or how FHIR is used to solve a particular problem. This resource is used to gather all the parts of an implementation guide into a logical whole, and to publish a computable definition of all the parts. }
    ValuesetOperationParameterTypeList, {@enum.value ValuesetOperationParameterTypeList A set of information summarized from a list of other resources. }
    ValuesetOperationParameterTypeLocation, {@enum.value ValuesetOperationParameterTypeLocation Details and position information for a physical place where services are provided  and resources and participants may be stored, found, contained or accommodated. }
    ValuesetOperationParameterTypeMedia, {@enum.value ValuesetOperationParameterTypeMedia A photo, video, or audio recording acquired or used in healthcare. The actual content may be inline or provided by direct reference. }
    ValuesetOperationParameterTypeMedication, {@enum.value ValuesetOperationParameterTypeMedication This resource is primarily used for the identification and definition of a medication. It covers the ingredients and the packaging for a medication. }
    ValuesetOperationParameterTypeMedicationAdministration, {@enum.value ValuesetOperationParameterTypeMedicationAdministration Describes the event of a patient consuming or otherwise being administered a medication.  This may be as simple as swallowing a tablet or it may be a long running infusion.  Related resources tie this event to the authorizing prescription, and the specific encounter between patient and health care practitioner. }
    ValuesetOperationParameterTypeMedicationDispense, {@enum.value ValuesetOperationParameterTypeMedicationDispense Indicates that a medication product is to be or has been dispensed for a named person/patient.  This includes a description of the medication product (supply) provided and the instructions for administering the medication.  The medication dispense is the result of a Pharmacy system responding to a Medication Order. }
    ValuesetOperationParameterTypeMedicationOrder, {@enum.value ValuesetOperationParameterTypeMedicationOrder An order for both supply of the medication and the instructions for administration of the medication to a patient. The resource is called "MedicationOrder" rather than "MedicationPrescription" to generalize the use across inpatient and outpatient settings as well as for care plans, etc. }
    ValuesetOperationParameterTypeMedicationStatement, {@enum.value ValuesetOperationParameterTypeMedicationStatement A record of a medication that is being consumed by a patient.   A medication statements may indicate that the patient may be taking the medication now, or has taken the medication in the past or will be taking the medication in the future.  The source of this information can be the patient, significant other (such as a family member or spouse), or a clinician.  A common scenario where this information is captured is during the history taking process during a patient visit or stay.   The medication information may come from e.g. the patients? memory, from a prescription bottle,  or from a list of medications the patient, clinician or other party maintains.

The primary difference between a medication statement and a medication administration is that the medication administration has complete administration information and is based on actual administration information from the person who administered the medication.  A medication statement is often, if not always less specific.  There is no required date/time when the medication was administered, in fact we only know that a source has reported the patient is taking this medication, where details such as time, quantity, or rate or even medication product may be incomplete or missing or less precise.  As stated earlier, the Medication Statement information may come from the patient?s memory, from a prescription bottle or from a list of medications the patient, clinician or other party maintains.  Medication Administration is more formal and is not missing detailed information. }
    ValuesetOperationParameterTypeMessageHeader, {@enum.value ValuesetOperationParameterTypeMessageHeader The header for a message exchange that is either requesting or responding to an action.  The Reference(s) that are the subject of the action as well as other Information related to the action are typically transmitted in a bundle in which the MessageHeader resource instance is the first resource in the bundle. }
    ValuesetOperationParameterTypeNamingSystem, {@enum.value ValuesetOperationParameterTypeNamingSystem A curated namespace that issues unique symbols within that namespace for the identification of concepts, people, devices, etc.  Represents a "System" used within the Identifier and Coding data types. }
    ValuesetOperationParameterTypeNutritionOrder, {@enum.value ValuesetOperationParameterTypeNutritionOrder A request to supply a diet, formula feeding (enteral) or oral nutritional supplement to a patient/resident. }
    ValuesetOperationParameterTypeObservation, {@enum.value ValuesetOperationParameterTypeObservation Measurements and simple assertions made about a patient, device or other subject. }
    ValuesetOperationParameterTypeOperationDefinition, {@enum.value ValuesetOperationParameterTypeOperationDefinition A formal computable definition of an operation (on the RESTful interface) or a named query (using the search interaction). }
    ValuesetOperationParameterTypeOperationOutcome, {@enum.value ValuesetOperationParameterTypeOperationOutcome A collection of error, warning or information messages that result from a system action. }
    ValuesetOperationParameterTypeOrder, {@enum.value ValuesetOperationParameterTypeOrder A request to perform an action. }
    ValuesetOperationParameterTypeOrderResponse, {@enum.value ValuesetOperationParameterTypeOrderResponse A response to an order. }
    ValuesetOperationParameterTypeOrganization, {@enum.value ValuesetOperationParameterTypeOrganization A formally or informally recognized grouping of people or organizations formed for the purpose of achieving some form of collective action.  Includes companies, institutions, corporations, departments, community groups, healthcare practice groups, etc. }
    ValuesetOperationParameterTypeParameters, {@enum.value ValuesetOperationParameterTypeParameters This special resource type is used to represent [operation](operations.html] request and response. It has no other use, and there is no RESTful end=point associated with it. }
    ValuesetOperationParameterTypePatient, {@enum.value ValuesetOperationParameterTypePatient Demographics and other administrative information about an individual or animal receiving care or other health-related services. }
    ValuesetOperationParameterTypePaymentNotice, {@enum.value ValuesetOperationParameterTypePaymentNotice This resource provides the status of the payment for goods and services rendered, and the request and response resource references. }
    ValuesetOperationParameterTypePaymentReconciliation, {@enum.value ValuesetOperationParameterTypePaymentReconciliation This resource provides payment details and claim references supporting a bulk payment. }
    ValuesetOperationParameterTypePerson, {@enum.value ValuesetOperationParameterTypePerson Demographics and administrative information about a person independent of a specific health-related context. }
    ValuesetOperationParameterTypePractitioner, {@enum.value ValuesetOperationParameterTypePractitioner A person who is directly or indirectly involved in the provisioning of healthcare. }
    ValuesetOperationParameterTypeProcedure, {@enum.value ValuesetOperationParameterTypeProcedure An action that is or was performed on a patient. This can be a physical 'thing' like an operation, or less invasive like counseling or hypnotherapy. }
    ValuesetOperationParameterTypeProcedureRequest, {@enum.value ValuesetOperationParameterTypeProcedureRequest A request for a procedure to be performed. May be a proposal or an order. }
    ValuesetOperationParameterTypeProcessRequest, {@enum.value ValuesetOperationParameterTypeProcessRequest This resource provides the target, request and response, and action details for an action to be performed by the target on or about existing resources. }
    ValuesetOperationParameterTypeProcessResponse, {@enum.value ValuesetOperationParameterTypeProcessResponse This resource provides processing status, errors and notes from the processing of a resource. }
    ValuesetOperationParameterTypeProvenance, {@enum.value ValuesetOperationParameterTypeProvenance Provenance of a resource is a record that describes entities and processes involved in producing and delivering or otherwise influencing that resource. Provenance provides a critical foundation for assessing authenticity, enabling trust, and allowing reproducibility. Provenance assertions are a form of contextual metadata and can themselves become important records with their own provenance. Provenance statement indicates clinical significance in terms of confidence in authenticity, reliability, and trustworthiness, integrity, and stage in lifecycle (e.g. Document Completion - has the artifact been legally authenticated), all of which may impact Security, Privacy, and Trust policies. }
    ValuesetOperationParameterTypeQuestionnaire, {@enum.value ValuesetOperationParameterTypeQuestionnaire A structured set of questions intended to guide the collection of answers. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the underlying questions. }
    ValuesetOperationParameterTypeQuestionnaireResponse, {@enum.value ValuesetOperationParameterTypeQuestionnaireResponse A structured set of questions and their answers. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the underlying questions. }
    ValuesetOperationParameterTypeReferralRequest, {@enum.value ValuesetOperationParameterTypeReferralRequest Used to record and send details about a request for referral service or transfer of a patient to the care of another provider or provider organisation. }
    ValuesetOperationParameterTypeRelatedPerson, {@enum.value ValuesetOperationParameterTypeRelatedPerson Information about a person that is involved in the care for a patient, but who is not the target of healthcare, nor has a formal responsibility in the care process. }
    ValuesetOperationParameterTypeResource, {@enum.value ValuesetOperationParameterTypeResource --- Abstract Type! ---Base Resource for everything. }
    ValuesetOperationParameterTypeRiskAssessment, {@enum.value ValuesetOperationParameterTypeRiskAssessment An assessment of the likely outcome(s) for a patient or other subject as well as the likelihood of each outcome. }
    ValuesetOperationParameterTypeSchedule, {@enum.value ValuesetOperationParameterTypeSchedule A container for slot(s) of time that may be available for booking appointments. }
    ValuesetOperationParameterTypeSearchParameter, {@enum.value ValuesetOperationParameterTypeSearchParameter A Search Parameter that defines a named search item that can be used to search/filter on a resource. }
    ValuesetOperationParameterTypeSlot, {@enum.value ValuesetOperationParameterTypeSlot A slot of time on a schedule that may be available for booking appointments. }
    ValuesetOperationParameterTypeSpecimen, {@enum.value ValuesetOperationParameterTypeSpecimen Sample for analysis. }
    ValuesetOperationParameterTypeStructureDefinition, {@enum.value ValuesetOperationParameterTypeStructureDefinition A definition of a FHIR structure. This resource is used to describe the underlying resources, data types defined in FHIR, and also for describing extensions, and constraints on resources and data types. }
    ValuesetOperationParameterTypeSubscription, {@enum.value ValuesetOperationParameterTypeSubscription The subscription resource is used to define a push based subscription from a server to another system. Once a subscription is registered with the server, the server checks every resource that is created or updated, and if the resource matches the given criteria, it sends a message on the defined "channel" so that another system is able to take an appropriate action. }
    ValuesetOperationParameterTypeSubstance, {@enum.value ValuesetOperationParameterTypeSubstance A homogeneous material with a definite composition. }
    ValuesetOperationParameterTypeSupplyDelivery, {@enum.value ValuesetOperationParameterTypeSupplyDelivery Record of delivery of what is in the supply. }
    ValuesetOperationParameterTypeSupplyRequest, {@enum.value ValuesetOperationParameterTypeSupplyRequest A record of a request for a medication, substance or device used in the healthcare setting. }
    ValuesetOperationParameterTypeTestScript, {@enum.value ValuesetOperationParameterTypeTestScript TestScript is a resource that specifies a suite of tests against a FHIR server implementation to determine compliance against the FHIR specification. }
    ValuesetOperationParameterTypeValueSet, {@enum.value ValuesetOperationParameterTypeValueSet A value set specifies a set of codes drawn from one or more code systems. }
    ValuesetOperationParameterTypeVisionPrescription); {@enum.value ValuesetOperationParameterTypeVisionPrescription An authorization for the supply of glasses and/or contact lenses to a patient. }
  TFhirValuesetOperationParameterTypeList = set of TFhirValuesetOperationParameterType;

  {@Enum TFhirIssueSeverity
    How the issue affects the success of the action
  }
  TFhirIssueSeverity = (
    IssueSeverityNull,  {@enum.value IssueSeverityNull Value is missing from Instance }
    IssueSeverityFatal, {@enum.value IssueSeverityFatal The issue caused the action to fail, and no further checking could be performed }
    IssueSeverityError, {@enum.value IssueSeverityError The issue is sufficiently important to cause the action to fail }
    IssueSeverityWarning, {@enum.value IssueSeverityWarning The issue is not important enough to cause the action to fail, but may cause it to be performed suboptimally or in a way that is not as desired }
    IssueSeverityInformation); {@enum.value IssueSeverityInformation The issue has no relation to the degree of success of the action }
  TFhirIssueSeverityList = set of TFhirIssueSeverity;

  {@Enum TFhirIssueType
    A code that describes the type of issue
  }
  TFhirIssueType = (
    IssueTypeNull,  {@enum.value IssueTypeNull Value is missing from Instance }
    IssueTypeInvalid, {@enum.value IssueTypeInvalid Content invalid against the Specification or a Profile }
    IssueTypeStructure, {@enum.value IssueTypeStructure A structural issue in the content such as wrong namespace, or unable to parse the content completely, or invalid json syntax }
    IssueTypeRequired, {@enum.value IssueTypeRequired A required element is missing }
    IssueTypeValue, {@enum.value IssueTypeValue element value invalid }
    IssueTypeInvariant, {@enum.value IssueTypeInvariant A content validation rule failed - e.g. a schematron rule }
    IssueTypeSecurity, {@enum.value IssueTypeSecurity An authentication/authorization/permissions issueof some kind }
    IssueTypeLogin, {@enum.value IssueTypeLogin the client needs to initiate an authentication process }
    IssueTypeUnknown, {@enum.value IssueTypeUnknown The user or system was not able to be authenticated (either there is no process, or the proferred token is unacceptable) }
    IssueTypeExpired, {@enum.value IssueTypeExpired User session expired; a login may be required }
    IssueTypeForbidden, {@enum.value IssueTypeForbidden The user does not have the rights to perform this action }
    IssueTypeSuppressed, {@enum.value IssueTypeSuppressed Some information was not or may not have been returned due to business rules, consent or privacy rules, or access permission constraints.  This information may be accessible through alternate processes }
    IssueTypeProcessing, {@enum.value IssueTypeProcessing Processing issues. These are expected to be final e.g. there is no point resubmitting the same content unchanged }
    IssueTypeNotSupported, {@enum.value IssueTypeNotSupported The resource or profile is not supported }
    IssueTypeDuplicate, {@enum.value IssueTypeDuplicate An attempt was made to create a duplicate record }
    IssueTypeNotFound, {@enum.value IssueTypeNotFound The reference provided was not found. In a pure RESTful environment, this would be an HTTP 404 error, but this code may be used where the content is not found further into the application architecture }
    IssueTypeTooLong, {@enum.value IssueTypeTooLong Provided content is too long (typically, this is a denial of service protection type of error) }
    IssueTypeCodeInvalid, {@enum.value IssueTypeCodeInvalid The code or system could not be understood, or it was not valid in the context of a particular ValueSet }
    IssueTypeExtension, {@enum.value IssueTypeExtension An extension was found that was not acceptable, or that could not be resolved, or a modifierExtension that was not recognised }
    IssueTypeTooCostly, {@enum.value IssueTypeTooCostly The operation was stopped to protect server resources; e.g. a request for a value set expansion on all of SNOMED CT }
    IssueTypeBusinessRule, {@enum.value IssueTypeBusinessRule The content/operation failed to pass some business rule, and so could not proceed }
    IssueTypeConflict, {@enum.value IssueTypeConflict content could not be accepted because of an edit conflict (i.e. version aware updates) (In a pure RESTful environment, this would be an HTTP 404 error, but this code may be used where the conflict is discovered further into the application architecture) }
    IssueTypeIncomplete, {@enum.value IssueTypeIncomplete Not all data sources typically accessed could be reached, or responded in time, so the returned information may not be complete }
    IssueTypeTransient, {@enum.value IssueTypeTransient Transient processing issues. The system receiving the error may be able to resubmit the same content once an underlying issue is resolved }
    IssueTypeLockError, {@enum.value IssueTypeLockError A resource/record locking failure (usually in an underlying database) }
    IssueTypeNoStore, {@enum.value IssueTypeNoStore The persistent store unavailable; e.g. the database is down for maintenance or similar }
    IssueTypeException, {@enum.value IssueTypeException An unexpected internal error }
    IssueTypeTimeout, {@enum.value IssueTypeTimeout An internal timeout occurred }
    IssueTypeThrottled, {@enum.value IssueTypeThrottled The system is not prepared to handle this request due to load management }
    IssueTypeInformational); {@enum.value IssueTypeInformational A message unrelated to the processing success of the completed operation (Examples of the latter include things like reminders of password expiry, system maintenance times, etc.) }
  TFhirIssueTypeList = set of TFhirIssueType;

  {@Enum TFhirOrderStatus
    The status of the response to an order
  }
  TFhirOrderStatus = (
    OrderStatusNull,  {@enum.value OrderStatusNull Value is missing from Instance }
    OrderStatusPending, {@enum.value OrderStatusPending The order is known, but no processing has occurred at this time }
    OrderStatusReview, {@enum.value OrderStatusReview The order is undergoing initial processing to determine whether it will be accepted (usually this involves human review) }
    OrderStatusRejected, {@enum.value OrderStatusRejected The order was rejected because of a workflow/business logic reason }
    OrderStatusError, {@enum.value OrderStatusError The order was unable to be processed because of a technical error (i.e. unexpected error) }
    OrderStatusAccepted, {@enum.value OrderStatusAccepted The order has been accepted, and work is in progress }
    OrderStatusCancelled, {@enum.value OrderStatusCancelled Processing the order was halted at the initiators request }
    OrderStatusReplaced, {@enum.value OrderStatusReplaced The order has been cancelled and replaced by another }
    OrderStatusAborted, {@enum.value OrderStatusAborted Processing the order was stopped because of some workflow/business logic reason }
    OrderStatusCompleted); {@enum.value OrderStatusCompleted The order has been completed }
  TFhirOrderStatusList = set of TFhirOrderStatus;

  {@Enum TFhirLinkType
    The type of link between this patient resource and another patient resource.
  }
  TFhirLinkType = (
    LinkTypeNull,  {@enum.value LinkTypeNull Value is missing from Instance }
    LinkTypeReplace, {@enum.value LinkTypeReplace The patient resource containing this link must no longer be used. The link points forward to another patient resource that must be used in lieu of the patient resource that contains the link. }
    LinkTypeRefer, {@enum.value LinkTypeRefer The patient resource containing this link is in use and valid but not considered the main source of information about a patient. The link points forward to another patient resource that should be consulted to retrieve additional patient information }
    LinkTypeSeealso); {@enum.value LinkTypeSeealso The patient resource containing this link is in use and valid, but points to another patient resource that is known to contain data about the same person. Data in this resource might overlap or contradict information found in the other patient resource. This link does not indicate any relative importance of the resources concerned, and both should be regarded as equally valid. }
  TFhirLinkTypeList = set of TFhirLinkType;

  {@Enum TFhirIdentityAssuranceLevel
    The level of confidence that this link represents the same actual person, based on NIST Authentication Levels
  }
  TFhirIdentityAssuranceLevel = (
    IdentityAssuranceLevelNull,  {@enum.value IdentityAssuranceLevelNull Value is missing from Instance }
    IdentityAssuranceLevelLevel1, {@enum.value IdentityAssuranceLevelLevel1 Little or no confidence in the asserted identity's accuracy. }
    IdentityAssuranceLevelLevel2, {@enum.value IdentityAssuranceLevelLevel2 Some confidence in the asserted identity's accuracy. }
    IdentityAssuranceLevelLevel3, {@enum.value IdentityAssuranceLevelLevel3 High confidence in the asserted identity's accuracy. }
    IdentityAssuranceLevelLevel4); {@enum.value IdentityAssuranceLevelLevel4 Very high confidence in the asserted identity's accuracy. }
  TFhirIdentityAssuranceLevelList = set of TFhirIdentityAssuranceLevel;

  {@Enum TFhirProcedureStatus
    A code specifying the state of the procedure record
  }
  TFhirProcedureStatus = (
    ProcedureStatusNull,  {@enum.value ProcedureStatusNull Value is missing from Instance }
    ProcedureStatusInProgress, {@enum.value ProcedureStatusInProgress The procedure is still occurring }
    ProcedureStatusAborted, {@enum.value ProcedureStatusAborted The procedure was terminated without completing successfully }
    ProcedureStatusCompleted, {@enum.value ProcedureStatusCompleted All actions involved in the procedure have taken place }
    ProcedureStatusEnteredInError); {@enum.value ProcedureStatusEnteredInError The statement was entered in error and Is not valid }
  TFhirProcedureStatusList = set of TFhirProcedureStatus;

  {@Enum TFhirProcedureRequestStatus
    The status of the request
  }
  TFhirProcedureRequestStatus = (
    ProcedureRequestStatusNull,  {@enum.value ProcedureRequestStatusNull Value is missing from Instance }
    ProcedureRequestStatusProposed, {@enum.value ProcedureRequestStatusProposed The request has been proposed }
    ProcedureRequestStatusDraft, {@enum.value ProcedureRequestStatusDraft The request is in preliminary form, prior to being requested }
    ProcedureRequestStatusRequested, {@enum.value ProcedureRequestStatusRequested The request has been placed }
    ProcedureRequestStatusReceived, {@enum.value ProcedureRequestStatusReceived The receiving system has received the request but not yet decided whether it will be performed }
    ProcedureRequestStatusAccepted, {@enum.value ProcedureRequestStatusAccepted The receiving system has accepted the request, but work has not yet commenced }
    ProcedureRequestStatusInProgress, {@enum.value ProcedureRequestStatusInProgress The work to fulfill the request is happening }
    ProcedureRequestStatusCompleted, {@enum.value ProcedureRequestStatusCompleted The work has been complete, the report(s) released, and no further work is planned }
    ProcedureRequestStatusSuspended, {@enum.value ProcedureRequestStatusSuspended The request has been held by originating system/user request }
    ProcedureRequestStatusRejected, {@enum.value ProcedureRequestStatusRejected The receiving system has declined to fulfill the request }
    ProcedureRequestStatusAborted); {@enum.value ProcedureRequestStatusAborted The request was attempted, but due to some procedural error, it could not be completed }
  TFhirProcedureRequestStatusList = set of TFhirProcedureRequestStatus;

  {@Enum TFhirProcedureRequestPriority
    The priority of the request
  }
  TFhirProcedureRequestPriority = (
    ProcedureRequestPriorityNull,  {@enum.value ProcedureRequestPriorityNull Value is missing from Instance }
    ProcedureRequestPriorityRoutine, {@enum.value ProcedureRequestPriorityRoutine The request has a normal priority. }
    ProcedureRequestPriorityUrgent, {@enum.value ProcedureRequestPriorityUrgent The request should be done urgently. }
    ProcedureRequestPriorityStat, {@enum.value ProcedureRequestPriorityStat The request is time-critical. }
    ProcedureRequestPriorityAsap); {@enum.value ProcedureRequestPriorityAsap The request should be acted on as soon as possible. }
  TFhirProcedureRequestPriorityList = set of TFhirProcedureRequestPriority;

  {@Enum TFhirActionlist
    List of allowable action which this resource can request.
  }
  TFhirActionlist = (
    ActionlistNull,  {@enum.value ActionlistNull Value is missing from Instance }
    ActionlistCancel, {@enum.value ActionlistCancel Cancel, reverse or nullify the target resource. }
    ActionlistPoll, {@enum.value ActionlistPoll Check for previously un-read/ not-retrieved resources. }
    ActionlistReprocess, {@enum.value ActionlistReprocess Re-process the target resource. }
    ActionlistStatus); {@enum.value ActionlistStatus Retrieve the processing status of the target resource. }
  TFhirActionlistList = set of TFhirActionlist;

  {@Enum TFhirProvenanceEntityRole
    How an entity was used in an activity
  }
  TFhirProvenanceEntityRole = (
    ProvenanceEntityRoleNull,  {@enum.value ProvenanceEntityRoleNull Value is missing from Instance }
    ProvenanceEntityRoleDerivation, {@enum.value ProvenanceEntityRoleDerivation A transformation of an entity into another, an update of an entity resulting in a new one, or the construction of a new entity based on a preexisting entity }
    ProvenanceEntityRoleRevision, {@enum.value ProvenanceEntityRoleRevision A derivation for which the resulting entity is a revised version of some original. }
    ProvenanceEntityRoleQuotation, {@enum.value ProvenanceEntityRoleQuotation The repeat of (some or all of) an entity, such as text or image, by someone who may or may not be its original author }
    ProvenanceEntityRoleSource); {@enum.value ProvenanceEntityRoleSource A primary source for a topic refers to something produced by some agent with direct experience and knowledge about the topic, at the time of the topic's study, without benefit from hindsight. }
  TFhirProvenanceEntityRoleList = set of TFhirProvenanceEntityRole;

  {@Enum TFhirQuestionnaireStatus
    Lifecycle status of the questionnaire.
  }
  TFhirQuestionnaireStatus = (
    QuestionnaireStatusNull,  {@enum.value QuestionnaireStatusNull Value is missing from Instance }
    QuestionnaireStatusDraft, {@enum.value QuestionnaireStatusDraft This Questionnaire is not ready for official use. }
    QuestionnaireStatusPublished, {@enum.value QuestionnaireStatusPublished This Questionnaire is ready for use. }
    QuestionnaireStatusRetired); {@enum.value QuestionnaireStatusRetired This Questionnaire should no longer be used to gather data. }
  TFhirQuestionnaireStatusList = set of TFhirQuestionnaireStatus;

  {@Enum TFhirAnswerFormat
    The expected format of an answer.
  }
  TFhirAnswerFormat = (
    AnswerFormatNull,  {@enum.value AnswerFormatNull Value is missing from Instance }
    AnswerFormatBoolean, {@enum.value AnswerFormatBoolean Answer is a yes/no answer. }
    AnswerFormatDecimal, {@enum.value AnswerFormatDecimal Answer is a floating point number. }
    AnswerFormatInteger, {@enum.value AnswerFormatInteger Answer is an integer. }
    AnswerFormatDate, {@enum.value AnswerFormatDate Answer is a date. }
    AnswerFormatDateTime, {@enum.value AnswerFormatDateTime Answer is a date and time. }
    AnswerFormatInstant, {@enum.value AnswerFormatInstant Answer is a system timestamp. }
    AnswerFormatTime, {@enum.value AnswerFormatTime Answer is a time (hour/minute/second) independent of date. }
    AnswerFormatString, {@enum.value AnswerFormatString Answer is a short (few words to short sentence) free-text entry. }
    AnswerFormatText, {@enum.value AnswerFormatText Answer is a long (potentially multi-paragraph) free-text entry (still captured as a string). }
    AnswerFormatUrl, {@enum.value AnswerFormatUrl Answer is a url (website, FTP site, etc.). }
    AnswerFormatChoice, {@enum.value AnswerFormatChoice Answer is a Coding drawn from a list of options. }
    AnswerFormatOpenChoice, {@enum.value AnswerFormatOpenChoice Answer is a Coding drawn from a list of options or a free-text entry. }
    AnswerFormatAttachment, {@enum.value AnswerFormatAttachment Answer is binary content such as a image, PDF, etc. }
    AnswerFormatReference, {@enum.value AnswerFormatReference Answer is a reference to another resource (practitioner, organization, etc.). }
    AnswerFormatQuantity); {@enum.value AnswerFormatQuantity Answer is a combination of a numeric value and unit, potentially with a comparator (<, >, etc.). }
  TFhirAnswerFormatList = set of TFhirAnswerFormat;

  {@Enum TFhirQuestionnaireAnswersStatus
    Lifecycle status of the questionnaire response
  }
  TFhirQuestionnaireAnswersStatus = (
    QuestionnaireAnswersStatusNull,  {@enum.value QuestionnaireAnswersStatusNull Value is missing from Instance }
    QuestionnaireAnswersStatusInProgress, {@enum.value QuestionnaireAnswersStatusInProgress This QuestionnaireResponse has been partially filled out with answers, but changes or additions are still expected to be made to it. }
    QuestionnaireAnswersStatusCompleted, {@enum.value QuestionnaireAnswersStatusCompleted This QuestionnaireResponse has been filled out with answers, and the current content is regarded as definitive. }
    QuestionnaireAnswersStatusAmended); {@enum.value QuestionnaireAnswersStatusAmended This QuestionnaireResponse has been filled out with answers, then marked as complete, yet changes or additions have been made to it afterwards. }
  TFhirQuestionnaireAnswersStatusList = set of TFhirQuestionnaireAnswersStatus;

  {@Enum TFhirReferralstatus
    The status of the referral
  }
  TFhirReferralstatus = (
    ReferralstatusNull,  {@enum.value ReferralstatusNull Value is missing from Instance }
    ReferralstatusDraft, {@enum.value ReferralstatusDraft A draft referral that has yet to be send }
    ReferralstatusRequested, {@enum.value ReferralstatusRequested The referral has been transmitted, but not yet acknowledged by the recipient }
    ReferralstatusActive, {@enum.value ReferralstatusActive The referral has been acknowledged by the recipient, and is in the process of being actioned }
    ReferralstatusCancelled, {@enum.value ReferralstatusCancelled The referral has been cancelled without being completed. For example it is no longer needed }
    ReferralstatusAccepted, {@enum.value ReferralstatusAccepted The recipient has agreed to deliver the care requested by the referral }
    ReferralstatusRejected, {@enum.value ReferralstatusRejected The recipient has declined to accept the referral }
    ReferralstatusCompleted); {@enum.value ReferralstatusCompleted The referral has been completely actioned }
  TFhirReferralstatusList = set of TFhirReferralstatus;

  {@Enum TFhirSearchXpathUsage
    How a search parameter relates to the set of elements returned by evaluating the its xpath query
  }
  TFhirSearchXpathUsage = (
    SearchXpathUsageNull,  {@enum.value SearchXpathUsageNull Value is missing from Instance }
    SearchXpathUsageNormal, {@enum.value SearchXpathUsageNormal The search parameter is derived directly from the selected nodes based on the type definitions }
    SearchXpathUsagePhonetic, {@enum.value SearchXpathUsagePhonetic The search parameter is derived by a phonetic transform from the selected nodes }
    SearchXpathUsageNearby, {@enum.value SearchXpathUsageNearby The search parameter is based on a spatial transform of the selected nodes }
    SearchXpathUsageDistance, {@enum.value SearchXpathUsageDistance The search parameter is based on a spatial transform of the selected nodes, using physical distance from the middle }
    SearchXpathUsageOther); {@enum.value SearchXpathUsageOther The interpretation of the xpath statement is unknown (and can't be automated) }
  TFhirSearchXpathUsageList = set of TFhirSearchXpathUsage;

  {@Enum TFhirSlotstatus
    The free/busy status of a slot
  }
  TFhirSlotstatus = (
    SlotstatusNull,  {@enum.value SlotstatusNull Value is missing from Instance }
    SlotstatusBusy, {@enum.value SlotstatusBusy Indicates that the time interval is busy because one  or more events have been scheduled for that interval. }
    SlotstatusFree, {@enum.value SlotstatusFree Indicates that the time interval is free for scheduling. }
    SlotstatusBusyUnavailable, {@enum.value SlotstatusBusyUnavailable Indicates that the time interval is busy and that the interval can not be scheduled. }
    SlotstatusBusyTentative); {@enum.value SlotstatusBusyTentative Indicates that the time interval is busy because one or more events have been tentatively scheduled for that interval }
  TFhirSlotstatusList = set of TFhirSlotstatus;

  {@Enum TFhirSpecimenStatus
    Codes providing the status/availability of a specimen
  }
  TFhirSpecimenStatus = (
    SpecimenStatusNull,  {@enum.value SpecimenStatusNull Value is missing from Instance }
    SpecimenStatusAvailable, {@enum.value SpecimenStatusAvailable The physical specimen is present and in good condition. }
    SpecimenStatusUnavailable, {@enum.value SpecimenStatusUnavailable There is no physical specimen because it is either lost, destroyed or consumed. }
    SpecimenStatusUnsatisfactory, {@enum.value SpecimenStatusUnsatisfactory The specimen cannot be used because of a quality issue such as a broken container, contamination, or too old. }
    SpecimenStatusEnteredInError); {@enum.value SpecimenStatusEnteredInError The specimen was entered in error and therefore nullified. }
  TFhirSpecimenStatusList = set of TFhirSpecimenStatus;

  {@Enum TFhirStructureDefinitionKind
    Defines the type of structure that a definition is describing
  }
  TFhirStructureDefinitionKind = (
    StructureDefinitionKindNull,  {@enum.value StructureDefinitionKindNull Value is missing from Instance }
    StructureDefinitionKindDatatype, {@enum.value StructureDefinitionKindDatatype A data type - either a primitive or complex structure that defines a set of data elements. These can be used throughout Resource and extension definitions }
    StructureDefinitionKindResource, {@enum.value StructureDefinitionKindResource A resource defined by the FHIR specification }
    StructureDefinitionKindLogical); {@enum.value StructureDefinitionKindLogical A logical model - a conceptual package of data that will be mapped to resources for implementation }
  TFhirStructureDefinitionKindList = set of TFhirStructureDefinitionKind;

  {@Enum TFhirExtensionContext
    How an extension context is interpreted
  }
  TFhirExtensionContext = (
    ExtensionContextNull,  {@enum.value ExtensionContextNull Value is missing from Instance }
    ExtensionContextResource, {@enum.value ExtensionContextResource The context is all elements matching a particular resource element path }
    ExtensionContextDatatype, {@enum.value ExtensionContextDatatype The context is all nodes matching a particular data type element path (root or repeating element) or all elements referencing a particular primitive data type (expressed as the datatype name) }
    ExtensionContextMapping, {@enum.value ExtensionContextMapping The context is all nodes whose mapping to a specified reference model corresponds to a particular mapping structure.  The context identifies the mapping target. The mapping should clearly identify where such an extension could be used }
    ExtensionContextExtension); {@enum.value ExtensionContextExtension The context is a particular extension from a particular profile, a uri that identifies the extension definition }
  TFhirExtensionContextList = set of TFhirExtensionContext;

  {@Enum TFhirSubscriptionStatus
    The status of a subscription
  }
  TFhirSubscriptionStatus = (
    SubscriptionStatusNull,  {@enum.value SubscriptionStatusNull Value is missing from Instance }
    SubscriptionStatusRequested, {@enum.value SubscriptionStatusRequested The client has requested the subscription, and the server has not yet set it up }
    SubscriptionStatusActive, {@enum.value SubscriptionStatusActive The subscription is active }
    SubscriptionStatusError, {@enum.value SubscriptionStatusError The server has an error executing the notification }
    SubscriptionStatusOff); {@enum.value SubscriptionStatusOff Too many errors have occurred or the subscription has expired }
  TFhirSubscriptionStatusList = set of TFhirSubscriptionStatus;

  {@Enum TFhirSubscriptionChannelType
    The type of method used to execute a subscription
  }
  TFhirSubscriptionChannelType = (
    SubscriptionChannelTypeNull,  {@enum.value SubscriptionChannelTypeNull Value is missing from Instance }
    SubscriptionChannelTypeRestHook, {@enum.value SubscriptionChannelTypeRestHook The channel is executed by making a post to the URI. If a payload is included, the URL is interpreted as the service base, and an update (PUT) is made }
    SubscriptionChannelTypeWebsocket, {@enum.value SubscriptionChannelTypeWebsocket The channel is executed by sending a packet across a web socket connection maintained by the client. The URL identifies the websocket, and the client binds to this URL }
    SubscriptionChannelTypeEmail, {@enum.value SubscriptionChannelTypeEmail The channel is executed by sending an email to the email addressed in the URI (which must be a mailto:) }
    SubscriptionChannelTypeSms, {@enum.value SubscriptionChannelTypeSms The channel is executed by sending an SMS message to the phone number identified in the URL (tel:) }
    SubscriptionChannelTypeMessage); {@enum.value SubscriptionChannelTypeMessage The channel Is executed by sending a message (e.g. a Bundle with a MessageHeader resource etc.) to the application identified in the URI }
  TFhirSubscriptionChannelTypeList = set of TFhirSubscriptionChannelType;

  {@Enum TFhirSupplydeliveryStatus
    Status of the Supply delivery
  }
  TFhirSupplydeliveryStatus = (
    SupplydeliveryStatusNull,  {@enum.value SupplydeliveryStatusNull Value is missing from Instance }
    SupplydeliveryStatusInProgress, {@enum.value SupplydeliveryStatusInProgress Supply has been requested, but not delivered }
    SupplydeliveryStatusCompleted, {@enum.value SupplydeliveryStatusCompleted Supply has been delivered. ( "completed") }
    SupplydeliveryStatusAbandoned); {@enum.value SupplydeliveryStatusAbandoned Dispensing was not completed }
  TFhirSupplydeliveryStatusList = set of TFhirSupplydeliveryStatus;

  {@Enum TFhirSupplyrequestStatus
    Status of the supply request
  }
  TFhirSupplyrequestStatus = (
    SupplyrequestStatusNull,  {@enum.value SupplyrequestStatusNull Value is missing from Instance }
    SupplyrequestStatusRequested, {@enum.value SupplyrequestStatusRequested Supply has been requested, but not dispensed }
    SupplyrequestStatusCompleted, {@enum.value SupplyrequestStatusCompleted Supply has been received by the requestor }
    SupplyrequestStatusFailed, {@enum.value SupplyrequestStatusFailed The supply will not be completed because the supplier was unable or unwilling to supply the item }
    SupplyrequestStatusCancelled); {@enum.value SupplyrequestStatusCancelled The orderer of the supply cancelled the request }
  TFhirSupplyrequestStatusList = set of TFhirSupplyrequestStatus;

  {@Enum TFhirContentType
    The content or mime type.
  }
  TFhirContentType = (
    ContentTypeNull,  {@enum.value ContentTypeNull Value is missing from Instance }
    ContentTypeXml, {@enum.value ContentTypeXml XML content-type corresponding to the application/xml+fhir mime-type. }
    ContentTypeJson); {@enum.value ContentTypeJson JSON content-type corresponding to the application/json+fhir mime-type. }
  TFhirContentTypeList = set of TFhirContentType;

  {@Enum TFhirAssertDirectionCodes
    The type of direction to use for assertion.
  }
  TFhirAssertDirectionCodes = (
    AssertDirectionCodesNull,  {@enum.value AssertDirectionCodesNull Value is missing from Instance }
    AssertDirectionCodesResponse, {@enum.value AssertDirectionCodesResponse The assertion is evaluated on the response. This is the default value. }
    AssertDirectionCodesRequest); {@enum.value AssertDirectionCodesRequest The assertion is evaluated on the request. }
  TFhirAssertDirectionCodesList = set of TFhirAssertDirectionCodes;

  {@Enum TFhirAssertOperatorCodes
    The type of operator to use for assertion.
  }
  TFhirAssertOperatorCodes = (
    AssertOperatorCodesNull,  {@enum.value AssertOperatorCodesNull Value is missing from Instance }
    AssertOperatorCodesEquals, {@enum.value AssertOperatorCodesEquals Default value. Equals comparison. }
    AssertOperatorCodesNotEquals, {@enum.value AssertOperatorCodesNotEquals Not equals comparison. }
    AssertOperatorCodesIn, {@enum.value AssertOperatorCodesIn Compare value within a known set of values. }
    AssertOperatorCodesNotIn, {@enum.value AssertOperatorCodesNotIn Compare value not within a known set of values. }
    AssertOperatorCodesGreaterThan, {@enum.value AssertOperatorCodesGreaterThan Compare value to be greater than a known value. }
    AssertOperatorCodesLessThan, {@enum.value AssertOperatorCodesLessThan Compare value to be less than a known value. }
    AssertOperatorCodesEmpty, {@enum.value AssertOperatorCodesEmpty Compare value is empty. }
    AssertOperatorCodesNotEmpty, {@enum.value AssertOperatorCodesNotEmpty Compare value is not empty. }
    AssertOperatorCodesContains, {@enum.value AssertOperatorCodesContains Compare value string contains a known value. }
    AssertOperatorCodesNotContains); {@enum.value AssertOperatorCodesNotContains Compare value string does not contain a known value. }
  TFhirAssertOperatorCodesList = set of TFhirAssertOperatorCodes;

  {@Enum TFhirAssertResponseCodeTypes
    The type of response code to use for assertion.
  }
  TFhirAssertResponseCodeTypes = (
    AssertResponseCodeTypesNull,  {@enum.value AssertResponseCodeTypesNull Value is missing from Instance }
    AssertResponseCodeTypesOkay, {@enum.value AssertResponseCodeTypesOkay Response code is 200. }
    AssertResponseCodeTypesCreated, {@enum.value AssertResponseCodeTypesCreated Response code is 201. }
    AssertResponseCodeTypesNoContent, {@enum.value AssertResponseCodeTypesNoContent Response code is 204. }
    AssertResponseCodeTypesNotModified, {@enum.value AssertResponseCodeTypesNotModified Response code is 304. }
    AssertResponseCodeTypesBad, {@enum.value AssertResponseCodeTypesBad Response code is 400. }
    AssertResponseCodeTypesForbidden, {@enum.value AssertResponseCodeTypesForbidden Response code is 403. }
    AssertResponseCodeTypesNotFound, {@enum.value AssertResponseCodeTypesNotFound Response code is 404. }
    AssertResponseCodeTypesMethodNotAllowed, {@enum.value AssertResponseCodeTypesMethodNotAllowed Response code is 405. }
    AssertResponseCodeTypesConflict, {@enum.value AssertResponseCodeTypesConflict Response code is 409. }
    AssertResponseCodeTypesGone, {@enum.value AssertResponseCodeTypesGone Response code is 410. }
    AssertResponseCodeTypesPreconditionFailed, {@enum.value AssertResponseCodeTypesPreconditionFailed Response code is 412. }
    AssertResponseCodeTypesUnprocessable); {@enum.value AssertResponseCodeTypesUnprocessable Response code is 422. }
  TFhirAssertResponseCodeTypesList = set of TFhirAssertResponseCodeTypes;

  {@Enum TFhirFilterOperator
    The kind of operation to perform as a part of a property based filter
  }
  TFhirFilterOperator = (
    FilterOperatorNull,  {@enum.value FilterOperatorNull Value is missing from Instance }
    FilterOperatorEqual, {@enum.value FilterOperatorEqual The specified property of the code equals the provided value }
    FilterOperatorIsA, {@enum.value FilterOperatorIsA Includes all concept ids that have a transitive is-a relationship with the concept Id provided as the value, including the provided concept itself }
    FilterOperatorIsNotA, {@enum.value FilterOperatorIsNotA The specified property of the code does not have an is-a relationship with the provided value }
    FilterOperatorRegex, {@enum.value FilterOperatorRegex The specified property of the code  matches the regex specified in the provided value }
    FilterOperatorIn, {@enum.value FilterOperatorIn The specified property of the code is in the set of codes or concepts specified in the provided value (comma separated list) }
    FilterOperatorNotIn); {@enum.value FilterOperatorNotIn The specified property of the code is not in the set of codes or concepts specified in the provided value (comma separated list) }
  TFhirFilterOperatorList = set of TFhirFilterOperator;

  {@Enum TFhirVisionEyeCodes
    A coded concept listing the eye codes.
  }
  TFhirVisionEyeCodes = (
    VisionEyeCodesNull,  {@enum.value VisionEyeCodesNull Value is missing from Instance }
    VisionEyeCodesRight, {@enum.value VisionEyeCodesRight Right Eye }
    VisionEyeCodesLeft); {@enum.value VisionEyeCodesLeft Left Eye }
  TFhirVisionEyeCodesList = set of TFhirVisionEyeCodes;

  {@Enum TFhirVisionBaseCodes
    A coded concept listing the base codes.
  }
  TFhirVisionBaseCodes = (
    VisionBaseCodesNull,  {@enum.value VisionBaseCodesNull Value is missing from Instance }
    VisionBaseCodesUp, {@enum.value VisionBaseCodesUp top }
    VisionBaseCodesDown, {@enum.value VisionBaseCodesDown bottom }
    VisionBaseCodesIn, {@enum.value VisionBaseCodesIn inner edge }
    VisionBaseCodesOut); {@enum.value VisionBaseCodesOut outer edge }
  TFhirVisionBaseCodesList = set of TFhirVisionBaseCodes;

Type
  TFhirElement = class;
  TFhirElementList = class;
  TFhirBackboneElement = class;
  TFhirBackboneElementList = class;
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
  TFhirTime = class;
  TFhirTimeList = class;
  TFhirString = class;
  TFhirStringList = class;
  TFhirBoolean = class;
  TFhirBooleanList = class;
  TFhirInstant = class;
  TFhirInstantList = class;
  TFhirMarkdown = class;
  TFhirMarkdownList = class;
  TFhirUnsignedInt = class;
  TFhirUnsignedIntList = class;
  TFhirCode = class;
  TFhirCodeList = class;
  TFhirId = class;
  TFhirIdList = class;
  TFhirOid = class;
  TFhirOidList = class;
  TFhirPositiveInt = class;
  TFhirPositiveIntList = class;
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
  TFhirAnnotation = class;
  TFhirAnnotationList = class;
  TFhirSampledData = class;
  TFhirSampledDataList = class;
  TFhirReference = class;
  TFhirReferenceList = class;
  TFhirCodeableConcept = class;
  TFhirCodeableConceptList = class;
  TFhirIdentifier = class;
  TFhirIdentifierList = class;
  TFhirSignature = class;
  TFhirSignatureList = class;
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
  TFhirAddress = class;
  TFhirAddressList = class;
  TFhirHumanName = class;
  TFhirHumanNameList = class;
  TFhirMeta = class;
  TFhirMetaList = class;
  TFhirContactPoint = class;
  TFhirContactPointList = class;

  {@Class TFhirElement : TFHIRBase
    Base definition for all elements in a resource.
  }
  {!.Net HL7Connect.Fhir.Element}
  TFhirElement = class (TFHIRBase)
  private
    FId : TFhirId;
    FextensionList : TFhirExtensionList;
    Procedure SetId(value : TFhirId);
    Function GetIdST : String;
    Procedure SetIdST(value : String);
    function GetExtensionList : TFhirExtensionList;
    function GetHasExtensionList : Boolean;
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirElement; overload;
    function Clone : TFhirElement; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member id
      Typed access to unique id for the element within a resource (for internal references).
    }
    property id : String read GetIdST write SetIdST;
    {@member idElement
      unique id for the element within a resource (for internal references).
    }
    property idElement : TFhirId read FId write SetId;

    {@member extensionList
      May be used to represent additional information that is not part of the basic definition of the element. In order to make the use of extensions safe and manageable, there is a strict set of governance  applied to the definition and use of extensions. Though any implementer is allowed to define an extension, there is a set of requirements that SHALL be met as part of the definition of the extension.
    }
    property extensionList : TFhirExtensionList read GetExtensionList;
    property hasExtensionList : boolean read GetHasExtensionList;

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
    

    {@member Append
      Add a FhirElement to the end of the list.
    }
    function Append : TFhirElement;

    
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
    

    {@member Insert
      Insert FhirElement before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirElement;
    

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


  {@Class TFhirBackboneElement : TFHIRElement
    Base definition for all elements that are defined inside a resource - but not those in a data type.
  }
  {!.Net HL7Connect.Fhir.BackboneElement}
  TFhirBackboneElement = class (TFHIRElement)
  private
    FmodifierExtensionList : TFhirExtensionList;
    function GetModifierExtensionList : TFhirExtensionList;
    function GetHasModifierExtensionList : Boolean;
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirBackboneElement; overload;
    function Clone : TFhirBackboneElement; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member modifierExtensionList
      May be used to represent additional information that is not part of the basic definition of the element, and that modifies the understanding of the element that contains it. Usually modifier elements provide negation or qualification. In order to make the use of extensions safe and manageable, there is a strict set of governance applied to the definition and use of extensions. Though any implementer is allowed to define an extension, there is a set of requirements that SHALL be met as part of the definition of the extension. Applications processing a resource are required to check for modifier extensions.
    }
    property modifierExtensionList : TFhirExtensionList read GetModifierExtensionList;
    property hasModifierExtensionList : boolean read GetHasModifierExtensionList;

  end;


  TFhirBackboneElementListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirBackboneElementList;
    function GetCurrent : TFhirBackboneElement;
  public
    Constructor Create(list : TFhirBackboneElementList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirBackboneElement read GetCurrent;
  end;


  {@Class TFhirBackboneElementList
    A list of FhirBackboneElement
  }
  {!.Net HL7Connect.Fhir.BackboneElementList}
  TFhirBackboneElementList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirBackboneElement;
    procedure SetItemN(index : Integer; value : TFhirBackboneElement);
  public
    {!script hide}
    function Link : TFhirBackboneElementList; Overload;
    function Clone : TFhirBackboneElementList; Overload;
    function GetEnumerator : TFhirBackboneElementListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirBackboneElement to the end of the list.
    }
    function Append : TFhirBackboneElement;

    
    {@member AddItem
      Add an already existing FhirBackboneElement to the end of the list.
    }
    procedure AddItem(value : TFhirBackboneElement); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirBackboneElement) : Integer;
    

    {@member Insert
      Insert FhirBackboneElement before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirBackboneElement;
    

    {@member InsertItem
       Insert an existing FhirBackboneElement before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirBackboneElement);
    
    {@member Item
       Get the iIndexth FhirBackboneElement. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirBackboneElement. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirBackboneElement);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirBackboneElement;
    
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
    
    Property FhirBackboneElements[index : Integer] : TFhirBackboneElement read GetItemN write SetItemN; default;
  End;


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
  Private
    Function GetStringValue : String;
    Function AsStringValue : String; Virtual;
  Public
    {!script hide}
    Function Link : TFHIRPrimitiveType; Overload;
    Function Clone : TFHIRPrimitiveType; Overload;
    Property StringValue : String read GetStringValue;
    {!script show}
  End;
  TFHIRPrimitiveTypeClass = class of TFHIRPrimitiveType;
  

  {@Class TFhirEnum : TFhirPrimitiveType
    a complex string - has an Id attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Enum}
  TFhirEnum = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    function AsStringValue : String; Override;
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
    a complex string - has an Id attribute, and a dataAbsentReason.
    
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
    function AsStringValue : String; Override;
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
    a complex string - has an Id attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.DateTime}
  TFhirDateTime = class (TFhirPrimitiveType)
  Private
    FValue: TDateAndTime;
    procedure setValue(value: TDateAndTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    function AsStringValue : String; Override;
  Public
    Constructor Create(value : TDateAndTime); overload;
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
    property value : TDateAndTime read FValue write SetValue;
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
    procedure AddItem(value : TDateAndTime); overload;

    
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
    a complex string - has an Id attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Date}
  TFhirDate = class (TFhirPrimitiveType)
  Private
    FValue: TDateAndTime;
    procedure setValue(value: TDateAndTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    function AsStringValue : String; Override;
  Public
    Constructor Create(value : TDateAndTime); overload;
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
    property value : TDateAndTime read FValue write SetValue;
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
    procedure AddItem(value : TDateAndTime); overload;

    
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
    a complex string - has an Id attribute, and a dataAbsentReason.
    
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
    function AsStringValue : String; Override;
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
    a complex string - has an Id attribute, and a dataAbsentReason.
    
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
    function AsStringValue : String; Override;
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
    a complex string - has an Id attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Base64Binary}
  TFhirBase64Binary = class (TFhirPrimitiveType)
  Private
    FValue: TBytes;
    procedure setValue(value: TBytes);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    function AsStringValue : String; Override;
  Public
    Constructor Create(value : TBytes); overload;
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
    property value : TBytes read FValue write SetValue;
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
    procedure AddItem(value : TBytes); overload;

    
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


  {@Class TFhirTime : TFhirPrimitiveType
    a complex string - has an Id attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Time}
  TFhirTime = class (TFhirPrimitiveType)
  Private
    FValue: String;
    procedure setValue(value: String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    function AsStringValue : String; Override;
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirTime; Overload;
    Function Clone : TFhirTime; Overload;
    procedure Assign(oSource : TAdvObject); override;
    function FhirType : string; override;
    {!script show}
  Published
    {@member value
      The actual value of the time
    }
    property value : String read FValue write SetValue;
  End;    


  TFhirTimeListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirTimeList;
    function GetCurrent : TFhirTime;
  public
    Constructor Create(list : TFhirTimeList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirTime read GetCurrent;
  end;


  {@Class TFhirTimeList
    A list of FhirTime
  }
  {!.Net HL7Connect.Fhir.TimeList}
  TFhirTimeList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirTime;
    procedure SetItemN(index : Integer; value : TFhirTime);
  public
    {!script hide}
    function Link : TFhirTimeList; Overload;
    function Clone : TFhirTimeList; Overload;
    function GetEnumerator : TFhirTimeListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirTime to the end of the list.
    }
    function Append : TFhirTime;

    
    {@member AddItem
      Add an already existing FhirTime to the end of the list.
    }
    procedure AddItem(value : TFhirTime); overload;

    
    {@member AddItem
      Add an already existing FhirTime to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirTime) : Integer;
    

    {@member Insert
      Insert FhirTime before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirTime;
    

    {@member InsertItem
       Insert an existing FhirTime before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirTime);
    
    {@member Item
       Get the iIndexth FhirTime. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirTime. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirTime);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirTime;
    
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
    
    Property FhirTimes[index : Integer] : TFhirTime read GetItemN write SetItemN; default;
  End;


  {@Class TFhirString : TFhirPrimitiveType
    a complex string - has an Id attribute, and a dataAbsentReason.
    
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
    function AsStringValue : String; Override;
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
    a complex string - has an Id attribute, and a dataAbsentReason.
    
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
    function AsStringValue : String; Override;
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
    a complex string - has an Id attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Instant}
  TFhirInstant = class (TFhirPrimitiveType)
  Private
    FValue: TDateAndTime;
    procedure setValue(value: TDateAndTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    function AsStringValue : String; Override;
  Public
    Constructor Create(value : TDateAndTime); overload;
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
    property value : TDateAndTime read FValue write SetValue;
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
    procedure AddItem(value : TDateAndTime); overload;

    
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


  {@Class TFhirMarkdown : TFhirString
    a complex string - has an Id attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.Markdown}
  TFhirMarkdown = class (TFhirString)
  Private
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirMarkdown; Overload;
    Function Clone : TFhirMarkdown; Overload;
    function FhirType : string; override;
    {!script show}
  End;    


  TFhirMarkdownListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirMarkdownList;
    function GetCurrent : TFhirMarkdown;
  public
    Constructor Create(list : TFhirMarkdownList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirMarkdown read GetCurrent;
  end;


  {@Class TFhirMarkdownList
    A list of FhirMarkdown
  }
  {!.Net HL7Connect.Fhir.MarkdownList}
  TFhirMarkdownList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirMarkdown;
    procedure SetItemN(index : Integer; value : TFhirMarkdown);
  public
    {!script hide}
    function Link : TFhirMarkdownList; Overload;
    function Clone : TFhirMarkdownList; Overload;
    function GetEnumerator : TFhirMarkdownListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirMarkdown to the end of the list.
    }
    function Append : TFhirMarkdown;

    
    {@member AddItem
      Add an already existing FhirMarkdown to the end of the list.
    }
    procedure AddItem(value : TFhirMarkdown); overload;

    
    {@member AddItem
      Add an already existing FhirMarkdown to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirMarkdown) : Integer;
    

    {@member Insert
      Insert FhirMarkdown before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirMarkdown;
    

    {@member InsertItem
       Insert an existing FhirMarkdown before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirMarkdown);
    
    {@member Item
       Get the iIndexth FhirMarkdown. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirMarkdown. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirMarkdown);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirMarkdown;
    
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
    
    Property FhirMarkdowns[index : Integer] : TFhirMarkdown read GetItemN write SetItemN; default;
  End;


  {@Class TFhirUnsignedInt : TFhirInteger
    a complex string - has an Id attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.UnsignedInt}
  TFhirUnsignedInt = class (TFhirInteger)
  Private
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirUnsignedInt; Overload;
    Function Clone : TFhirUnsignedInt; Overload;
    function FhirType : string; override;
    {!script show}
  End;    


  TFhirUnsignedIntListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirUnsignedIntList;
    function GetCurrent : TFhirUnsignedInt;
  public
    Constructor Create(list : TFhirUnsignedIntList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirUnsignedInt read GetCurrent;
  end;


  {@Class TFhirUnsignedIntList
    A list of FhirUnsignedInt
  }
  {!.Net HL7Connect.Fhir.UnsignedIntList}
  TFhirUnsignedIntList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirUnsignedInt;
    procedure SetItemN(index : Integer; value : TFhirUnsignedInt);
  public
    {!script hide}
    function Link : TFhirUnsignedIntList; Overload;
    function Clone : TFhirUnsignedIntList; Overload;
    function GetEnumerator : TFhirUnsignedIntListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirUnsignedInt to the end of the list.
    }
    function Append : TFhirUnsignedInt;

    
    {@member AddItem
      Add an already existing FhirUnsignedInt to the end of the list.
    }
    procedure AddItem(value : TFhirUnsignedInt); overload;

    
    {@member AddItem
      Add an already existing FhirUnsignedInt to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirUnsignedInt) : Integer;
    

    {@member Insert
      Insert FhirUnsignedInt before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirUnsignedInt;
    

    {@member InsertItem
       Insert an existing FhirUnsignedInt before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirUnsignedInt);
    
    {@member Item
       Get the iIndexth FhirUnsignedInt. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirUnsignedInt. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirUnsignedInt);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirUnsignedInt;
    
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
    
    Property FhirUnsignedInts[index : Integer] : TFhirUnsignedInt read GetItemN write SetItemN; default;
  End;


  {@Class TFhirCode : TFhirString
    a complex string - has an Id attribute, and a dataAbsentReason.
    
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
    a complex string - has an Id attribute, and a dataAbsentReason.
    
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
    a complex string - has an Id attribute, and a dataAbsentReason.
    
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


  {@Class TFhirPositiveInt : TFhirInteger
    a complex string - has an Id attribute, and a dataAbsentReason.
    
    Used where a FHIR element is a string, and may have a dataAbsentReason
  }
  {!.Net HL7Connect.Fhir.PositiveInt}
  TFhirPositiveInt = class (TFhirInteger)
  Private
  Public
    Constructor Create(value : String); overload;
    Destructor Destroy; override;
    
    {!script hide}
    Function Link : TFhirPositiveInt; Overload;
    Function Clone : TFhirPositiveInt; Overload;
    function FhirType : string; override;
    {!script show}
  End;    


  TFhirPositiveIntListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirPositiveIntList;
    function GetCurrent : TFhirPositiveInt;
  public
    Constructor Create(list : TFhirPositiveIntList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPositiveInt read GetCurrent;
  end;


  {@Class TFhirPositiveIntList
    A list of FhirPositiveInt
  }
  {!.Net HL7Connect.Fhir.PositiveIntList}
  TFhirPositiveIntList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirPositiveInt;
    procedure SetItemN(index : Integer; value : TFhirPositiveInt);
  public
    {!script hide}
    function Link : TFhirPositiveIntList; Overload;
    function Clone : TFhirPositiveIntList; Overload;
    function GetEnumerator : TFhirPositiveIntListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirPositiveInt to the end of the list.
    }
    function Append : TFhirPositiveInt;

    
    {@member AddItem
      Add an already existing FhirPositiveInt to the end of the list.
    }
    procedure AddItem(value : TFhirPositiveInt); overload;

    
    {@member AddItem
      Add an already existing FhirPositiveInt to the end of the list.
    }
    procedure AddItem(value : String); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirPositiveInt) : Integer;
    

    {@member Insert
      Insert FhirPositiveInt before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirPositiveInt;
    

    {@member InsertItem
       Insert an existing FhirPositiveInt before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirPositiveInt);
    
    {@member Item
       Get the iIndexth FhirPositiveInt. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirPositiveInt. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirPositiveInt);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirPositiveInt;
    
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
    
    Property FhirPositiveInts[index : Integer] : TFhirPositiveInt read GetItemN write SetItemN; default;
  End;


  {@Class TFhirUuid : TFhirUri
    a complex string - has an Id attribute, and a dataAbsentReason.
    
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
      Typed access to Source of the definition for the extension code - a logical name or a URL.
    }
    property url : String read GetUrlST write SetUrlST;
    {@member urlElement
      Source of the definition for the extension code - a logical name or a URL.
    }
    property urlElement : TFhirUri read FUrl write SetUrl;

    {@member value
      Typed access to Value of extension - may be a resource or one of a constrained set of the data types (see Extensibility in the spec for list). (defined for API consistency)
    }
    property value : TFhirType read FValue write SetValue;
    {@member valueElement
      Value of extension - may be a resource or one of a constrained set of the data types (see Extensibility in the spec for list).
    }
    property valueElement : TFhirType read FValue write SetValue;

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
    property statusElement : TFhirEnum read FStatus write SetStatus;

    {@member div_
      Typed access to The actual narrative content, a stripped down version of XHTML. (defined for API consistency)
    }
    property div_ : TFhirXHtmlNode read FDiv_ write SetDiv_;
    {@member div_Element
      The actual narrative content, a stripped down version of XHTML.
    }
    property div_Element : TFhirXHtmlNode read FDiv_ write SetDiv_;

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
    Function GetStartST : TDateAndTime;
    Procedure SetStartST(value : TDateAndTime);
    Procedure SetEnd_(value : TFhirDateTime);
    Function GetEnd_ST : TDateAndTime;
    Procedure SetEnd_ST(value : TDateAndTime);
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
      Typed access to The start of the period. The boundary is inclusive.
    }
    property start : TDateAndTime read GetStartST write SetStartST;
    {@member startElement
      The start of the period. The boundary is inclusive.
    }
    property startElement : TFhirDateTime read FStart write SetStart;

    {@member end_
      Typed access to The end of the period. If the end of the period is missing, it means that the period is ongoing. The start mey be in the past, and the end date in the future, which means that period is expected/planned to end at that time.
    }
    property end_ : TDateAndTime read GetEnd_ST write SetEnd_ST;
    {@member end_Element
      The end of the period. If the end of the period is missing, it means that the period is ongoing. The start mey be in the past, and the end date in the future, which means that period is expected/planned to end at that time.
    }
    property end_Element : TFhirDateTime read FEnd_ write SetEnd_;

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
      Typed access to The identification of the code system that defines the meaning of the symbol in the code.
    }
    property system : String read GetSystemST write SetSystemST;
    {@member systemElement
      The identification of the code system that defines the meaning of the symbol in the code.
    }
    property systemElement : TFhirUri read FSystem write SetSystem;

    {@member version
      Typed access to The version of the code system which was used when choosing this code. Note that a well-maintained code system does not need the version reported, because the meaning of codes is consistent across versions. However this cannot consistently be assured. and when the meaning is not guaranteed to be consistent, the version SHOULD be exchanged.
    }
    property version : String read GetVersionST write SetVersionST;
    {@member versionElement
      The version of the code system which was used when choosing this code. Note that a well-maintained code system does not need the version reported, because the meaning of codes is consistent across versions. However this cannot consistently be assured. and when the meaning is not guaranteed to be consistent, the version SHOULD be exchanged.
    }
    property versionElement : TFhirString read FVersion write SetVersion;

    {@member code
      Typed access to A symbol in syntax defined by the system. The symbol may be a predefined code or an expression in a syntax defined by the coding system (e.g. post-coordination).
    }
    property code : String read GetCodeST write SetCodeST;
    {@member codeElement
      A symbol in syntax defined by the system. The symbol may be a predefined code or an expression in a syntax defined by the coding system (e.g. post-coordination).
    }
    property codeElement : TFhirCode read FCode write SetCode;

    {@member display
      Typed access to A representation of the meaning of the code in the system, following the rules of the system.
    }
    property display : String read GetDisplayST write SetDisplayST;
    {@member displayElement
      A representation of the meaning of the code in the system, following the rules of the system.
    }
    property displayElement : TFhirString read FDisplay write SetDisplay;

    {@member userSelected
      Typed access to Indicates that this coding was chosen by a user directly - i.e. off a pick list of available items (codes or displays).
    }
    property userSelected : Boolean read GetUserSelectedST write SetUserSelectedST;
    {@member userSelectedElement
      Indicates that this coding was chosen by a user directly - i.e. off a pick list of available items (codes or displays).
    }
    property userSelectedElement : TFhirBoolean read FUserSelected write SetUserSelected;

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
      Typed access to The low limit. The boundary is inclusive. (defined for API consistency)
    }
    property low : TFhirQuantity read FLow write SetLow;
    {@member lowElement
      The low limit. The boundary is inclusive.
    }
    property lowElement : TFhirQuantity read FLow write SetLow;

    {@member high
      Typed access to The high limit. The boundary is inclusive. (defined for API consistency)
    }
    property high : TFhirQuantity read FHigh write SetHigh;
    {@member highElement
      The high limit. The boundary is inclusive.
    }
    property highElement : TFhirQuantity read FHigh write SetHigh;

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
    FUnit_ : TFhirString;
    FSystem : TFhirUri;
    FCode : TFhirCode;
    Procedure SetValue(value : TFhirDecimal);
    Function GetValueST : String;
    Procedure SetValueST(value : String);
    Procedure SetComparator(value : TFhirEnum);
    Function GetComparatorST : TFhirQuantityComparator;
    Procedure SetComparatorST(value : TFhirQuantityComparator);
    Procedure SetUnit_(value : TFhirString);
    Function GetUnit_ST : String;
    Procedure SetUnit_ST(value : String);
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
      Typed access to The value of the measured amount. The value includes an implicit precision in the presentation of the value.
    }
    property value : String read GetValueST write SetValueST;
    {@member valueElement
      The value of the measured amount. The value includes an implicit precision in the presentation of the value.
    }
    property valueElement : TFhirDecimal read FValue write SetValue;

    {@member comparator
      How the value should be understood and represented - whether the actual value is greater or less than the stated value due to measurement issues; e.g. if the comparator is "<" , then the real value is < stated value.
    }
    property comparator : TFhirQuantityComparator read GetComparatorST write SetComparatorST;
    property comparatorElement : TFhirEnum read FComparator write SetComparator;

    {@member unit_
      Typed access to A human-readable form of the unit.
    }
    property unit_ : String read GetUnit_ST write SetUnit_ST;
    {@member unit_Element
      A human-readable form of the unit.
    }
    property unit_Element : TFhirString read FUnit_ write SetUnit_;

    {@member system
      Typed access to The identification of the system that provides the coded form of the unit.
    }
    property system : String read GetSystemST write SetSystemST;
    {@member systemElement
      The identification of the system that provides the coded form of the unit.
    }
    property systemElement : TFhirUri read FSystem write SetSystem;

    {@member code
      Typed access to A computer processable form of the unit in some unit representation system.
    }
    property code : String read GetCodeST write SetCodeST;
    {@member codeElement
      A computer processable form of the unit in some unit representation system.
    }
    property codeElement : TFhirCode read FCode write SetCode;

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
    Function GetCreationST : TDateAndTime;
    Procedure SetCreationST(value : TDateAndTime);
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
      Typed access to Identifies the type of the data in the attachment and allows a method to be chosen to interpret or render the data. Includes mime type parameters such as charset where appropriate.
    }
    property contentType : String read GetContentTypeST write SetContentTypeST;
    {@member contentTypeElement
      Identifies the type of the data in the attachment and allows a method to be chosen to interpret or render the data. Includes mime type parameters such as charset where appropriate.
    }
    property contentTypeElement : TFhirCode read FContentType write SetContentType;

    {@member language
      Typed access to The human language of the content. The value can be any valid value according to BCP 47.
    }
    property language : String read GetLanguageST write SetLanguageST;
    {@member languageElement
      The human language of the content. The value can be any valid value according to BCP 47.
    }
    property languageElement : TFhirCode read FLanguage write SetLanguage;

    {@member data
      Typed access to The actual data of the attachment - a sequence of bytes. In XML, represented using base64.
    }
    property data : TBytes read GetDataST write SetDataST;
    {@member dataElement
      The actual data of the attachment - a sequence of bytes. In XML, represented using base64.
    }
    property dataElement : TFhirBase64Binary read FData write SetData;

    {@member url
      Typed access to An alternative location where the data can be accessed.
    }
    property url : String read GetUrlST write SetUrlST;
    {@member urlElement
      An alternative location where the data can be accessed.
    }
    property urlElement : TFhirUri read FUrl write SetUrl;

    {@member size
      Typed access to The number of bytes of data that make up this attachment.
    }
    property size : String read GetSizeST write SetSizeST;
    {@member sizeElement
      The number of bytes of data that make up this attachment.
    }
    property sizeElement : TFhirUnsignedInt read FSize write SetSize;

    {@member hash
      Typed access to The calculated hash of the data using SHA-1. Represented using base64.
    }
    property hash : TBytes read GetHashST write SetHashST;
    {@member hashElement
      The calculated hash of the data using SHA-1. Represented using base64.
    }
    property hashElement : TFhirBase64Binary read FHash write SetHash;

    {@member title
      Typed access to A label or set of text to display in place of the data.
    }
    property title : String read GetTitleST write SetTitleST;
    {@member titleElement
      A label or set of text to display in place of the data.
    }
    property titleElement : TFhirString read FTitle write SetTitle;

    {@member creation
      Typed access to The date that the attachment was first created.
    }
    property creation : TDateAndTime read GetCreationST write SetCreationST;
    {@member creationElement
      The date that the attachment was first created.
    }
    property creationElement : TFhirDateTime read FCreation write SetCreation;

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
      Typed access to The value of the numerator. (defined for API consistency)
    }
    property numerator : TFhirQuantity read FNumerator write SetNumerator;
    {@member numeratorElement
      The value of the numerator.
    }
    property numeratorElement : TFhirQuantity read FNumerator write SetNumerator;

    {@member denominator
      Typed access to The value of the denominator. (defined for API consistency)
    }
    property denominator : TFhirQuantity read FDenominator write SetDenominator;
    {@member denominatorElement
      The value of the denominator.
    }
    property denominatorElement : TFhirQuantity read FDenominator write SetDenominator;

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


  {@Class TFhirAnnotation : TFhirType
    A  text note which also  contains information about who made the statement and when.
  }
  {!.Net HL7Connect.Fhir.Annotation}
  TFhirAnnotation = class (TFhirType)
  private
    FAuthor : TFhirType;
    FTime : TFhirDateTime;
    FText : TFhirString;
    Procedure SetAuthor(value : TFhirType);
    Procedure SetTime(value : TFhirDateTime);
    Function GetTimeST : TDateAndTime;
    Procedure SetTimeST(value : TDateAndTime);
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
    function Link : TFhirAnnotation; overload;
    function Clone : TFhirAnnotation; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member author
      Typed access to The individual responsible for making the annotation. (defined for API consistency)
    }
    property author : TFhirType read FAuthor write SetAuthor;
    {@member authorElement
      The individual responsible for making the annotation.
    }
    property authorElement : TFhirType read FAuthor write SetAuthor;

    {@member time
      Typed access to Indicates when this particular annotation was made.
    }
    property time : TDateAndTime read GetTimeST write SetTimeST;
    {@member timeElement
      Indicates when this particular annotation was made.
    }
    property timeElement : TFhirDateTime read FTime write SetTime;

    {@member text
      Typed access to The text of the annotation.
    }
    property text : String read GetTextST write SetTextST;
    {@member textElement
      The text of the annotation.
    }
    property textElement : TFhirString read FText write SetText;

  end;


  TFhirAnnotationListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirAnnotationList;
    function GetCurrent : TFhirAnnotation;
  public
    Constructor Create(list : TFhirAnnotationList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirAnnotation read GetCurrent;
  end;


  {@Class TFhirAnnotationList
    A list of FhirAnnotation
  }
  {!.Net HL7Connect.Fhir.AnnotationList}
  TFhirAnnotationList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirAnnotation;
    procedure SetItemN(index : Integer; value : TFhirAnnotation);
  public
    {!script hide}
    function Link : TFhirAnnotationList; Overload;
    function Clone : TFhirAnnotationList; Overload;
    function GetEnumerator : TFhirAnnotationListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirAnnotation to the end of the list.
    }
    function Append : TFhirAnnotation;

    
    {@member AddItem
      Add an already existing FhirAnnotation to the end of the list.
    }
    procedure AddItem(value : TFhirAnnotation); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirAnnotation) : Integer;
    

    {@member Insert
      Insert FhirAnnotation before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirAnnotation;
    

    {@member InsertItem
       Insert an existing FhirAnnotation before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirAnnotation);
    
    {@member Item
       Get the iIndexth FhirAnnotation. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirAnnotation. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirAnnotation);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirAnnotation;
    
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
    
    Property FhirAnnotations[index : Integer] : TFhirAnnotation read GetItemN write SetItemN; default;
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
      Typed access to The base quantity that a measured value of zero represents. In addition, this provides the units of the entire measurement series. (defined for API consistency)
    }
    property origin : TFhirQuantity read FOrigin write SetOrigin;
    {@member originElement
      The base quantity that a measured value of zero represents. In addition, this provides the units of the entire measurement series.
    }
    property originElement : TFhirQuantity read FOrigin write SetOrigin;

    {@member period
      Typed access to The length of time between sampling times, measured in milliseconds.
    }
    property period : String read GetPeriodST write SetPeriodST;
    {@member periodElement
      The length of time between sampling times, measured in milliseconds.
    }
    property periodElement : TFhirDecimal read FPeriod write SetPeriod;

    {@member factor
      Typed access to A correction factor that is applied to the sampled data points before they are added to the origin.
    }
    property factor : String read GetFactorST write SetFactorST;
    {@member factorElement
      A correction factor that is applied to the sampled data points before they are added to the origin.
    }
    property factorElement : TFhirDecimal read FFactor write SetFactor;

    {@member lowerLimit
      Typed access to The lower limit of detection of the measured points. This is needed if any of the data points have the value "L" (lower than detection limit).
    }
    property lowerLimit : String read GetLowerLimitST write SetLowerLimitST;
    {@member lowerLimitElement
      The lower limit of detection of the measured points. This is needed if any of the data points have the value "L" (lower than detection limit).
    }
    property lowerLimitElement : TFhirDecimal read FLowerLimit write SetLowerLimit;

    {@member upperLimit
      Typed access to The upper limit of detection of the measured points. This is needed if any of the data points have the value "U" (higher than detection limit).
    }
    property upperLimit : String read GetUpperLimitST write SetUpperLimitST;
    {@member upperLimitElement
      The upper limit of detection of the measured points. This is needed if any of the data points have the value "U" (higher than detection limit).
    }
    property upperLimitElement : TFhirDecimal read FUpperLimit write SetUpperLimit;

    {@member dimensions
      Typed access to The number of sample points at each time point. If this value is greater than one, then the dimensions will be interlaced - all the sample points for a point in time will be recorded at once.
    }
    property dimensions : String read GetDimensionsST write SetDimensionsST;
    {@member dimensionsElement
      The number of sample points at each time point. If this value is greater than one, then the dimensions will be interlaced - all the sample points for a point in time will be recorded at once.
    }
    property dimensionsElement : TFhirPositiveInt read FDimensions write SetDimensions;

    {@member data
      Typed access to A series of data points which are decimal values separated by a single space (character u20). The special values "E" (error), "L" (below detection limit) and "U" (above detection limit) can also be used in place of a decimal value.
    }
    property data : String read GetDataST write SetDataST;
    {@member dataElement
      A series of data points which are decimal values separated by a single space (character u20). The special values "E" (error), "L" (below detection limit) and "U" (above detection limit) can also be used in place of a decimal value.
    }
    property dataElement : TFhirString read FData write SetData;

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


  {@Class TFhirReference : TFhirType
    A reference from one resource to another.
  }
  {!.Net HL7Connect.Fhir.Reference}
  TFhirReference = class (TFhirType)
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
    function Link : TFhirReference; overload;
    function Clone : TFhirReference; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member reference
      Typed access to A reference to a location at which the other resource is found. The reference may be a relative reference, in which case it is relative to the service base URL, or an absolute URL that resolves to the location where the resource is found. The reference may be version specific or not. If the reference is not to a FHIR RESTful server, then it should be assumed to be version specific. Internal fragment references (start with '#') refer to contained resources.
    }
    property reference : String read GetReferenceST write SetReferenceST;
    {@member referenceElement
      A reference to a location at which the other resource is found. The reference may be a relative reference, in which case it is relative to the service base URL, or an absolute URL that resolves to the location where the resource is found. The reference may be version specific or not. If the reference is not to a FHIR RESTful server, then it should be assumed to be version specific. Internal fragment references (start with '#') refer to contained resources.
    }
    property referenceElement : TFhirString read FReference write SetReference;

    {@member display
      Typed access to Plain text narrative that identifies the resource in addition to the resource reference.
    }
    property display : String read GetDisplayST write SetDisplayST;
    {@member displayElement
      Plain text narrative that identifies the resource in addition to the resource reference.
    }
    property displayElement : TFhirString read FDisplay write SetDisplay;

  end;


  TFhirReferenceListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirReferenceList;
    function GetCurrent : TFhirReference;
  public
    Constructor Create(list : TFhirReferenceList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirReference read GetCurrent;
  end;


  {@Class TFhirReferenceList
    A list of FhirReference
  }
  {!.Net HL7Connect.Fhir.ReferenceList}
  TFhirReferenceList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirReference;
    procedure SetItemN(index : Integer; value : TFhirReference);
  public
    {!script hide}
    function Link : TFhirReferenceList; Overload;
    function Clone : TFhirReferenceList; Overload;
    function GetEnumerator : TFhirReferenceListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirReference to the end of the list.
    }
    function Append : TFhirReference;

    
    {@member AddItem
      Add an already existing FhirReference to the end of the list.
    }
    procedure AddItem(value : TFhirReference); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirReference) : Integer;
    

    {@member Insert
      Insert FhirReference before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirReference;
    

    {@member InsertItem
       Insert an existing FhirReference before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirReference);
    
    {@member Item
       Get the iIndexth FhirReference. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirReference. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirReference);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirReference;
    
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
    
    Property FhirReferences[index : Integer] : TFhirReference read GetItemN write SetItemN; default;
  End;


  {@Class TFhirCodeableConcept : TFhirType
    A concept that may be defined by a formal reference to a terminology or ontology or may be provided by text.
  }
  {!.Net HL7Connect.Fhir.CodeableConcept}
  TFhirCodeableConcept = class (TFhirType)
  private
    FcodingList : TFhirCodingList;
    FText : TFhirString;
    function GetCodingList : TFhirCodingList;
    function GetHasCodingList : Boolean;
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
    property codingList : TFhirCodingList read GetCodingList;
    property hasCodingList : boolean read GetHasCodingList;

    {@member text
      Typed access to A human language representation of the concept as seen/selected/uttered by the user who entered the data and/or which represents the intended meaning of the user.
    }
    property text : String read GetTextST write SetTextST;
    {@member textElement
      A human language representation of the concept as seen/selected/uttered by the user who entered the data and/or which represents the intended meaning of the user.
    }
    property textElement : TFhirString read FText write SetText;

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
    FType_ : TFhirCodeableConcept;
    FSystem : TFhirUri;
    FValue : TFhirString;
    FPeriod : TFhirPeriod;
    FAssigner : TFhirReference{TFhirOrganization};
    Procedure SetUse(value : TFhirEnum);
    Function GetUseST : TFhirIdentifierUse;
    Procedure SetUseST(value : TFhirIdentifierUse);
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetSystem(value : TFhirUri);
    Function GetSystemST : String;
    Procedure SetSystemST(value : String);
    Procedure SetValue(value : TFhirString);
    Function GetValueST : String;
    Procedure SetValueST(value : String);
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetAssigner(value : TFhirReference{TFhirOrganization});
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
    property useElement : TFhirEnum read FUse write SetUse;

    {@member type_
      Typed access to A coded type for the identifier that can be used to determine which identifier to use for a specific purpose. (defined for API consistency)
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    {@member type_Element
      A coded type for the identifier that can be used to determine which identifier to use for a specific purpose.
    }
    property type_Element : TFhirCodeableConcept read FType_ write SetType_;

    {@member system
      Typed access to Establishes the namespace in which set of possible id values is unique.
    }
    property system : String read GetSystemST write SetSystemST;
    {@member systemElement
      Establishes the namespace in which set of possible id values is unique.
    }
    property systemElement : TFhirUri read FSystem write SetSystem;

    {@member value
      Typed access to The portion of the identifier typically displayed to the user and which is unique within the context of the system.
    }
    property value : String read GetValueST write SetValueST;
    {@member valueElement
      The portion of the identifier typically displayed to the user and which is unique within the context of the system.
    }
    property valueElement : TFhirString read FValue write SetValue;

    {@member period
      Typed access to Time period during which identifier is/was valid for use. (defined for API consistency)
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;
    {@member periodElement
      Time period during which identifier is/was valid for use.
    }
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

    {@member assigner
      Typed access to Organization that issued/manages the identifier. (defined for API consistency)
    }
    property assigner : TFhirReference{TFhirOrganization} read FAssigner write SetAssigner;
    {@member assignerElement
      Organization that issued/manages the identifier.
    }
    property assignerElement : TFhirReference{TFhirOrganization} read FAssigner write SetAssigner;

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


  {@Class TFhirSignature : TFhirType
    A digital signature along with supporting context. The signature may be electronic/cryptographic in nature, or a graphical image representing a hand-written signature, or a signature process. Different Signature approaches have different utilities.
  }
  {!.Net HL7Connect.Fhir.Signature}
  TFhirSignature = class (TFhirType)
  private
    Ftype_List : TFhirCodingList;
    FWhen : TFhirInstant;
    FWho : TFhirType;
    FContentType : TFhirCode;
    FBlob : TFhirBase64Binary;
    function GetType_List : TFhirCodingList;
    function GetHasType_List : Boolean;
    Procedure SetWhen(value : TFhirInstant);
    Function GetWhenST : TDateAndTime;
    Procedure SetWhenST(value : TDateAndTime);
    Procedure SetWho(value : TFhirType);
    Procedure SetContentType(value : TFhirCode);
    Function GetContentTypeST : String;
    Procedure SetContentTypeST(value : String);
    Procedure SetBlob(value : TFhirBase64Binary);
    Function GetBlobST : TBytes;
    Procedure SetBlobST(value : TBytes);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirSignature; overload;
    function Clone : TFhirSignature; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member type_List
      An indication of the reason that the entity signed this document. This may be explicitly included as part of the signature information and can be used when determining accountability for various actions concerning the document.
    }
    property type_List : TFhirCodingList read GetType_List;
    property hasType_List : boolean read GetHasType_List;

    {@member when
      Typed access to When the digital signature was signed.
    }
    property when : TDateAndTime read GetWhenST write SetWhenST;
    {@member whenElement
      When the digital signature was signed.
    }
    property whenElement : TFhirInstant read FWhen write SetWhen;

    {@member who
      Typed access to A reference to an application-usable description of the person that signed the certificate (e.g. the signature used their private key). (defined for API consistency)
    }
    property who : TFhirType read FWho write SetWho;
    {@member whoElement
      A reference to an application-usable description of the person that signed the certificate (e.g. the signature used their private key).
    }
    property whoElement : TFhirType read FWho write SetWho;

    {@member contentType
      Typed access to A mime type that indicates the technical format of the signature. Important mime types are application/signature+xml for X ML DigSig, application/jwt for JWT, and image/* for a graphical image of a signature.
    }
    property contentType : String read GetContentTypeST write SetContentTypeST;
    {@member contentTypeElement
      A mime type that indicates the technical format of the signature. Important mime types are application/signature+xml for X ML DigSig, application/jwt for JWT, and image/* for a graphical image of a signature.
    }
    property contentTypeElement : TFhirCode read FContentType write SetContentType;

    {@member blob
      Typed access to The base64 encoding of the Signature content.
    }
    property blob : TBytes read GetBlobST write SetBlobST;
    {@member blobElement
      The base64 encoding of the Signature content.
    }
    property blobElement : TFhirBase64Binary read FBlob write SetBlob;

  end;


  TFhirSignatureListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirSignatureList;
    function GetCurrent : TFhirSignature;
  public
    Constructor Create(list : TFhirSignatureList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirSignature read GetCurrent;
  end;


  {@Class TFhirSignatureList
    A list of FhirSignature
  }
  {!.Net HL7Connect.Fhir.SignatureList}
  TFhirSignatureList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirSignature;
    procedure SetItemN(index : Integer; value : TFhirSignature);
  public
    {!script hide}
    function Link : TFhirSignatureList; Overload;
    function Clone : TFhirSignatureList; Overload;
    function GetEnumerator : TFhirSignatureListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirSignature to the end of the list.
    }
    function Append : TFhirSignature;

    
    {@member AddItem
      Add an already existing FhirSignature to the end of the list.
    }
    procedure AddItem(value : TFhirSignature); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirSignature) : Integer;
    

    {@member Insert
      Insert FhirSignature before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirSignature;
    

    {@member InsertItem
       Insert an existing FhirSignature before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirSignature);
    
    {@member Item
       Get the iIndexth FhirSignature. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirSignature. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirSignature);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirSignature;
    
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
    
    Property FhirSignatures[index : Integer] : TFhirSignature read GetItemN write SetItemN; default;
  End;


  {@Class TFhirElementDefinitionSlicing : TFhirElement
    Indicates that the element is sliced into a set of alternative definitions (i.e. in a structure definition, there are multiple different constraints on a single element in the base resource). Slicing can be used in any resource that has cardinality ..* on the base resource, or any resource with a choice of types. The set of slices is any elements that come after this in the element sequence that have the same path, until a shorter path occurs (the shorter path terminates the set).
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionSlicing}
  TFhirElementDefinitionSlicing = class (TFhirElement)
  private
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
    Function GetRulesST : TFhirResourceSlicingRules;
    Procedure SetRulesST(value : TFhirResourceSlicingRules);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirElementDefinitionSlicing; overload;
    function Clone : TFhirElementDefinitionSlicing; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member discriminatorList
      Designates which child elements are used to discriminate between the slices when processing an instance. If one or more discriminators are provided, the value of the child elements in the instance data SHALL completely distinguish which slice the element in the resource matches based on the allowed values for those elements in each of the slices.
    }
    property discriminatorList : TFhirStringList read GetDiscriminatorList;
    property hasDiscriminatorList : boolean read GetHasDiscriminatorList;

    {@member description
      Typed access to A human-readable text description of how the slicing works. If there is no discriminator, this is required to be present to provide whatever information is possible about how the slices can be differentiated.
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    {@member descriptionElement
      A human-readable text description of how the slicing works. If there is no discriminator, this is required to be present to provide whatever information is possible about how the slices can be differentiated.
    }
    property descriptionElement : TFhirString read FDescription write SetDescription;

    {@member ordered
      Typed access to If the matching elements have to occur in the same order as defined in the profile.
    }
    property ordered : Boolean read GetOrderedST write SetOrderedST;
    {@member orderedElement
      If the matching elements have to occur in the same order as defined in the profile.
    }
    property orderedElement : TFhirBoolean read FOrdered write SetOrdered;

    {@member rules
      Whether additional slices are allowed or not. When the slices are ordered, profile authors can also say that additional slices are only allowed at the end.
    }
    property rules : TFhirResourceSlicingRules read GetRulesST write SetRulesST;
    property rulesElement : TFhirEnum read FRules write SetRules;

  end;


  TFhirElementDefinitionSlicingListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionSlicingList;
    function GetCurrent : TFhirElementDefinitionSlicing;
  public
    Constructor Create(list : TFhirElementDefinitionSlicingList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionSlicing read GetCurrent;
  end;


  {@Class TFhirElementDefinitionSlicingList
    A list of FhirElementDefinitionSlicing
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionSlicingList}
  TFhirElementDefinitionSlicingList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirElementDefinitionSlicing;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionSlicing);
  public
    {!script hide}
    function Link : TFhirElementDefinitionSlicingList; Overload;
    function Clone : TFhirElementDefinitionSlicingList; Overload;
    function GetEnumerator : TFhirElementDefinitionSlicingListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirElementDefinitionSlicing to the end of the list.
    }
    function Append : TFhirElementDefinitionSlicing;

    
    {@member AddItem
      Add an already existing FhirElementDefinitionSlicing to the end of the list.
    }
    procedure AddItem(value : TFhirElementDefinitionSlicing); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirElementDefinitionSlicing) : Integer;
    

    {@member Insert
      Insert FhirElementDefinitionSlicing before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirElementDefinitionSlicing;
    

    {@member InsertItem
       Insert an existing FhirElementDefinitionSlicing before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionSlicing);
    
    {@member Item
       Get the iIndexth FhirElementDefinitionSlicing. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirElementDefinitionSlicing. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionSlicing);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirElementDefinitionSlicing;
    
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
    
    Property FhirElementDefinitionSlicings[index : Integer] : TFhirElementDefinitionSlicing read GetItemN write SetItemN; default;
  End;


  {@Class TFhirElementDefinitionBase : TFhirElement
    Information about the base definition of the element, provided to make it unncessary for tools to trace the derviation of the element through the derived and related profiles. This information is only provided where the element definition represents a constraint on another element definition, and must be present if there is a base element definition.
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionBase}
  TFhirElementDefinitionBase = class (TFhirElement)
  private
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
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirElementDefinitionBase; overload;
    function Clone : TFhirElementDefinitionBase; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member path
      Typed access to The Path that identifies the base element - this matches the ElementDefinition.path for that element. Across FHIR, there is only one base definition of any element - that is, an element definition on a [[[StructureDefinition]]] without a StructureDefinition.base.
    }
    property path : String read GetPathST write SetPathST;
    {@member pathElement
      The Path that identifies the base element - this matches the ElementDefinition.path for that element. Across FHIR, there is only one base definition of any element - that is, an element definition on a [[[StructureDefinition]]] without a StructureDefinition.base.
    }
    property pathElement : TFhirString read FPath write SetPath;

    {@member min
      Typed access to Minimum cardinality of the base element identified by the path.
    }
    property min : String read GetMinST write SetMinST;
    {@member minElement
      Minimum cardinality of the base element identified by the path.
    }
    property minElement : TFhirInteger read FMin write SetMin;

    {@member max
      Typed access to Maximum cardinality of the base element identified by the path.
    }
    property max : String read GetMaxST write SetMaxST;
    {@member maxElement
      Maximum cardinality of the base element identified by the path.
    }
    property maxElement : TFhirString read FMax write SetMax;

  end;


  TFhirElementDefinitionBaseListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionBaseList;
    function GetCurrent : TFhirElementDefinitionBase;
  public
    Constructor Create(list : TFhirElementDefinitionBaseList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionBase read GetCurrent;
  end;


  {@Class TFhirElementDefinitionBaseList
    A list of FhirElementDefinitionBase
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionBaseList}
  TFhirElementDefinitionBaseList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirElementDefinitionBase;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionBase);
  public
    {!script hide}
    function Link : TFhirElementDefinitionBaseList; Overload;
    function Clone : TFhirElementDefinitionBaseList; Overload;
    function GetEnumerator : TFhirElementDefinitionBaseListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirElementDefinitionBase to the end of the list.
    }
    function Append : TFhirElementDefinitionBase;

    
    {@member AddItem
      Add an already existing FhirElementDefinitionBase to the end of the list.
    }
    procedure AddItem(value : TFhirElementDefinitionBase); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirElementDefinitionBase) : Integer;
    

    {@member Insert
      Insert FhirElementDefinitionBase before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirElementDefinitionBase;
    

    {@member InsertItem
       Insert an existing FhirElementDefinitionBase before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionBase);
    
    {@member Item
       Get the iIndexth FhirElementDefinitionBase. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirElementDefinitionBase. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionBase);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirElementDefinitionBase;
    
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
    
    Property FhirElementDefinitionBases[index : Integer] : TFhirElementDefinitionBase read GetItemN write SetItemN; default;
  End;


  {@Class TFhirElementDefinitionType : TFhirElement
    The data type or resource that the value of this element is permitted to be.
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionType}
  TFhirElementDefinitionType = class (TFhirElement)
  private
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
    Function GetAggregationST : TFhirResourceAggregationModeList;
    Procedure SetAggregationST(value : TFhirResourceAggregationModeList);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirElementDefinitionType; overload;
    function Clone : TFhirElementDefinitionType; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member code
      Typed access to Name of Data type or Resource that is a(or the) type used for this element.
    }
    property code : String read GetCodeST write SetCodeST;
    {@member codeElement
      Name of Data type or Resource that is a(or the) type used for this element.
    }
    property codeElement : TFhirCode read FCode write SetCode;

    {@member profileList
      Identifies a profile structure or implementation Guide that SHALL hold for resources or datatypes referenced as the type of this element. Can be a local reference - to another structure in this profile, or a reference to a structure in another profile. When more than one profile is specified, the content must conform to all of them. When an implementation guide is specified, the resource SHALL conform to at least one profile defined in the implementation guide.
    }
    property profileList : TFhirUriList read GetProfileList;
    property hasProfileList : boolean read GetHasProfileList;

    {@member aggregation
      If the type is a reference to another resource, how the resource is or can be aggreated - is it a contained resource, or a reference, and if the context is a bundle, is it included in the bundle.
    }
    property aggregation : TFhirResourceAggregationModeList read GetAggregationST write SetAggregationST;
    property aggregationElement : TFhirEnumList read GetAggregation;
    property hasAggregation : boolean read GetHasAggregation;
  end;


  TFhirElementDefinitionTypeListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionTypeList;
    function GetCurrent : TFhirElementDefinitionType;
  public
    Constructor Create(list : TFhirElementDefinitionTypeList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionType read GetCurrent;
  end;


  {@Class TFhirElementDefinitionTypeList
    A list of FhirElementDefinitionType
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionTypeList}
  TFhirElementDefinitionTypeList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirElementDefinitionType;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionType);
  public
    {!script hide}
    function Link : TFhirElementDefinitionTypeList; Overload;
    function Clone : TFhirElementDefinitionTypeList; Overload;
    function GetEnumerator : TFhirElementDefinitionTypeListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirElementDefinitionType to the end of the list.
    }
    function Append : TFhirElementDefinitionType;

    
    {@member AddItem
      Add an already existing FhirElementDefinitionType to the end of the list.
    }
    procedure AddItem(value : TFhirElementDefinitionType); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirElementDefinitionType) : Integer;
    

    {@member Insert
      Insert FhirElementDefinitionType before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirElementDefinitionType;
    

    {@member InsertItem
       Insert an existing FhirElementDefinitionType before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionType);
    
    {@member Item
       Get the iIndexth FhirElementDefinitionType. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirElementDefinitionType. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionType);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirElementDefinitionType;
    
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
    
    Property FhirElementDefinitionTypes[index : Integer] : TFhirElementDefinitionType read GetItemN write SetItemN; default;
  End;


  {@Class TFhirElementDefinitionConstraint : TFhirElement
    Formal constraints such as co-occurrence and other constraints that can be computationally evaluated within the context of the instance.
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionConstraint}
  TFhirElementDefinitionConstraint = class (TFhirElement)
  private
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
    Function GetSeverityST : TFhirConstraintSeverity;
    Procedure SetSeverityST(value : TFhirConstraintSeverity);
    Procedure SetHuman(value : TFhirString);
    Function GetHumanST : String;
    Procedure SetHumanST(value : String);
    Procedure SetXpath(value : TFhirString);
    Function GetXpathST : String;
    Procedure SetXpathST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirElementDefinitionConstraint; overload;
    function Clone : TFhirElementDefinitionConstraint; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member key
      Typed access to Allows identification of which elements have their cardinalities impacted by the constraint.  Will not be referenced for constraints that do not affect cardinality.
    }
    property key : String read GetKeyST write SetKeyST;
    {@member keyElement
      Allows identification of which elements have their cardinalities impacted by the constraint.  Will not be referenced for constraints that do not affect cardinality.
    }
    property keyElement : TFhirId read FKey write SetKey;

    {@member requirements
      Typed access to Description of why this constraint is necessary or appropriate.
    }
    property requirements : String read GetRequirementsST write SetRequirementsST;
    {@member requirementsElement
      Description of why this constraint is necessary or appropriate.
    }
    property requirementsElement : TFhirString read FRequirements write SetRequirements;

    {@member severity
      Identifies the impact constraint violation has on the conformance of the instance.
    }
    property severity : TFhirConstraintSeverity read GetSeverityST write SetSeverityST;
    property severityElement : TFhirEnum read FSeverity write SetSeverity;

    {@member human
      Typed access to Text that can be used to describe the constraint in messages identifying that the constraint has been violated.
    }
    property human : String read GetHumanST write SetHumanST;
    {@member humanElement
      Text that can be used to describe the constraint in messages identifying that the constraint has been violated.
    }
    property humanElement : TFhirString read FHuman write SetHuman;

    {@member xpath
      Typed access to An XPath expression of constraint that can be executed to see if this constraint is met.
    }
    property xpath : String read GetXpathST write SetXpathST;
    {@member xpathElement
      An XPath expression of constraint that can be executed to see if this constraint is met.
    }
    property xpathElement : TFhirString read FXpath write SetXpath;

  end;


  TFhirElementDefinitionConstraintListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionConstraintList;
    function GetCurrent : TFhirElementDefinitionConstraint;
  public
    Constructor Create(list : TFhirElementDefinitionConstraintList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionConstraint read GetCurrent;
  end;


  {@Class TFhirElementDefinitionConstraintList
    A list of FhirElementDefinitionConstraint
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionConstraintList}
  TFhirElementDefinitionConstraintList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirElementDefinitionConstraint;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionConstraint);
  public
    {!script hide}
    function Link : TFhirElementDefinitionConstraintList; Overload;
    function Clone : TFhirElementDefinitionConstraintList; Overload;
    function GetEnumerator : TFhirElementDefinitionConstraintListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirElementDefinitionConstraint to the end of the list.
    }
    function Append : TFhirElementDefinitionConstraint;

    
    {@member AddItem
      Add an already existing FhirElementDefinitionConstraint to the end of the list.
    }
    procedure AddItem(value : TFhirElementDefinitionConstraint); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirElementDefinitionConstraint) : Integer;
    

    {@member Insert
      Insert FhirElementDefinitionConstraint before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirElementDefinitionConstraint;
    

    {@member InsertItem
       Insert an existing FhirElementDefinitionConstraint before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionConstraint);
    
    {@member Item
       Get the iIndexth FhirElementDefinitionConstraint. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirElementDefinitionConstraint. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionConstraint);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirElementDefinitionConstraint;
    
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
    
    Property FhirElementDefinitionConstraints[index : Integer] : TFhirElementDefinitionConstraint read GetItemN write SetItemN; default;
  End;


  {@Class TFhirElementDefinitionBinding : TFhirElement
    Binds to a value set if this element is coded (code, Coding, CodeableConcept).
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionBinding}
  TFhirElementDefinitionBinding = class (TFhirElement)
  private
    FStrength : TFhirEnum;
    FDescription : TFhirString;
    FValueSet : TFhirType;
    Procedure SetStrength(value : TFhirEnum);
    Function GetStrengthST : TFhirBindingStrength;
    Procedure SetStrengthST(value : TFhirBindingStrength);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetValueSet(value : TFhirType);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirElementDefinitionBinding; overload;
    function Clone : TFhirElementDefinitionBinding; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member strength
      Indicates the degree of conformance expectations associated with this binding - that is, the degree to which the provided value set must be adhered to in the instances.
    }
    property strength : TFhirBindingStrength read GetStrengthST write SetStrengthST;
    property strengthElement : TFhirEnum read FStrength write SetStrength;

    {@member description
      Typed access to Describes the intended use of this particular set of codes.
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    {@member descriptionElement
      Describes the intended use of this particular set of codes.
    }
    property descriptionElement : TFhirString read FDescription write SetDescription;

    {@member valueSet
      Typed access to Points to the value set or external definition (e.g. implicit value set) that identifies the set of codes to be used. (defined for API consistency)
    }
    property valueSet : TFhirType read FValueSet write SetValueSet;
    {@member valueSetElement
      Points to the value set or external definition (e.g. implicit value set) that identifies the set of codes to be used.
    }
    property valueSetElement : TFhirType read FValueSet write SetValueSet;

  end;


  TFhirElementDefinitionBindingListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionBindingList;
    function GetCurrent : TFhirElementDefinitionBinding;
  public
    Constructor Create(list : TFhirElementDefinitionBindingList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionBinding read GetCurrent;
  end;


  {@Class TFhirElementDefinitionBindingList
    A list of FhirElementDefinitionBinding
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionBindingList}
  TFhirElementDefinitionBindingList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirElementDefinitionBinding;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionBinding);
  public
    {!script hide}
    function Link : TFhirElementDefinitionBindingList; Overload;
    function Clone : TFhirElementDefinitionBindingList; Overload;
    function GetEnumerator : TFhirElementDefinitionBindingListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirElementDefinitionBinding to the end of the list.
    }
    function Append : TFhirElementDefinitionBinding;

    
    {@member AddItem
      Add an already existing FhirElementDefinitionBinding to the end of the list.
    }
    procedure AddItem(value : TFhirElementDefinitionBinding); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirElementDefinitionBinding) : Integer;
    

    {@member Insert
      Insert FhirElementDefinitionBinding before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirElementDefinitionBinding;
    

    {@member InsertItem
       Insert an existing FhirElementDefinitionBinding before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionBinding);
    
    {@member Item
       Get the iIndexth FhirElementDefinitionBinding. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirElementDefinitionBinding. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionBinding);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirElementDefinitionBinding;
    
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
    
    Property FhirElementDefinitionBindings[index : Integer] : TFhirElementDefinitionBinding read GetItemN write SetItemN; default;
  End;


  {@Class TFhirElementDefinitionMapping : TFhirElement
    Identifies a concept from an external specification that roughly corresponds to this element.
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionMapping}
  TFhirElementDefinitionMapping = class (TFhirElement)
  private
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
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirElementDefinitionMapping; overload;
    function Clone : TFhirElementDefinitionMapping; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identity
      Typed access to An internal reference to the definition of a mapping.
    }
    property identity : String read GetIdentityST write SetIdentityST;
    {@member identityElement
      An internal reference to the definition of a mapping.
    }
    property identityElement : TFhirId read FIdentity write SetIdentity;

    {@member language
      Typed access to Identifies the computable language in which mapping.map is expressed.
    }
    property language : String read GetLanguageST write SetLanguageST;
    {@member languageElement
      Identifies the computable language in which mapping.map is expressed.
    }
    property languageElement : TFhirCode read FLanguage write SetLanguage;

    {@member map
      Typed access to Expresses what part of the target specification corresponds to this element.
    }
    property map : String read GetMapST write SetMapST;
    {@member mapElement
      Expresses what part of the target specification corresponds to this element.
    }
    property mapElement : TFhirString read FMap write SetMap;

  end;


  TFhirElementDefinitionMappingListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionMappingList;
    function GetCurrent : TFhirElementDefinitionMapping;
  public
    Constructor Create(list : TFhirElementDefinitionMappingList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinitionMapping read GetCurrent;
  end;


  {@Class TFhirElementDefinitionMappingList
    A list of FhirElementDefinitionMapping
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionMappingList}
  TFhirElementDefinitionMappingList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirElementDefinitionMapping;
    procedure SetItemN(index : Integer; value : TFhirElementDefinitionMapping);
  public
    {!script hide}
    function Link : TFhirElementDefinitionMappingList; Overload;
    function Clone : TFhirElementDefinitionMappingList; Overload;
    function GetEnumerator : TFhirElementDefinitionMappingListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirElementDefinitionMapping to the end of the list.
    }
    function Append : TFhirElementDefinitionMapping;

    
    {@member AddItem
      Add an already existing FhirElementDefinitionMapping to the end of the list.
    }
    procedure AddItem(value : TFhirElementDefinitionMapping); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirElementDefinitionMapping) : Integer;
    

    {@member Insert
      Insert FhirElementDefinitionMapping before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirElementDefinitionMapping;
    

    {@member InsertItem
       Insert an existing FhirElementDefinitionMapping before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirElementDefinitionMapping);
    
    {@member Item
       Get the iIndexth FhirElementDefinitionMapping. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirElementDefinitionMapping. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinitionMapping);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirElementDefinitionMapping;
    
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
    
    Property FhirElementDefinitionMappings[index : Integer] : TFhirElementDefinitionMapping read GetItemN write SetItemN; default;
  End;


  {@Class TFhirElementDefinition : TFhirType
    Captures constraints on each element within the resource, profile, or extension.
  }
  {!.Net HL7Connect.Fhir.ElementDefinition}
  TFhirElementDefinition = class (TFhirType)
  private
    FPath : TFhirString;
    FRepresentation : TFhirEnumList;
    FName : TFhirString;
    FLabel_ : TFhirString;
    FcodeList : TFhirCodingList;
    FSlicing : TFhirElementDefinitionSlicing;
    FShort : TFhirString;
    FDefinition : TFhirString;
    FComments : TFhirString;
    FRequirements : TFhirString;
    FaliasList : TFhirStringList;
    FMin : TFhirInteger;
    FMax : TFhirString;
    FBase : TFhirElementDefinitionBase;
    Ftype_List : TFhirElementDefinitionTypeList;
    FNameReference : TFhirString;
    FDefaultValue : TFhirType;
    FMeaningWhenMissing : TFhirString;
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
    Function GetRepresentationST : TFhirPropertyRepresentationList;
    Procedure SetRepresentationST(value : TFhirPropertyRepresentationList);
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
    Procedure SetDefinition(value : TFhirString);
    Function GetDefinitionST : String;
    Procedure SetDefinitionST(value : String);
    Procedure SetComments(value : TFhirString);
    Function GetCommentsST : String;
    Procedure SetCommentsST(value : String);
    Procedure SetRequirements(value : TFhirString);
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
    Procedure SetMeaningWhenMissing(value : TFhirString);
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
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirElementDefinition; overload;
    function Clone : TFhirElementDefinition; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member path
      Typed access to The path identifies the element and is expressed as a "."-separated list of ancestor elements, beginning with the name of the resource or extension.
    }
    property path : String read GetPathST write SetPathST;
    {@member pathElement
      The path identifies the element and is expressed as a "."-separated list of ancestor elements, beginning with the name of the resource or extension.
    }
    property pathElement : TFhirString read FPath write SetPath;

    {@member representation
      Codes that define how this element is represented in instances, when the deviation varies from the normal case.
    }
    property representation : TFhirPropertyRepresentationList read GetRepresentationST write SetRepresentationST;
    property representationElement : TFhirEnumList read GetRepresentation;
    property hasRepresentation : boolean read GetHasRepresentation;
    {@member name
      Typed access to The name of this element definition (to refer to it from other element definitions using ElementDefinition.nameReference). This is a unique name referring to a specific set of constraints applied to this element. One use of this is to provide a name to different slices of the same element.
    }
    property name : String read GetNameST write SetNameST;
    {@member nameElement
      The name of this element definition (to refer to it from other element definitions using ElementDefinition.nameReference). This is a unique name referring to a specific set of constraints applied to this element. One use of this is to provide a name to different slices of the same element.
    }
    property nameElement : TFhirString read FName write SetName;

    {@member label_
      Typed access to The text to display beside the element indicating its meaning or to use to prompt for the element in a user display or form.
    }
    property label_ : String read GetLabel_ST write SetLabel_ST;
    {@member label_Element
      The text to display beside the element indicating its meaning or to use to prompt for the element in a user display or form.
    }
    property label_Element : TFhirString read FLabel_ write SetLabel_;

    {@member codeList
      A code that provides the meaning for the element according to a particular terminology.
    }
    property codeList : TFhirCodingList read GetCodeList;
    property hasCodeList : boolean read GetHasCodeList;

    {@member slicing
      Typed access to Indicates that the element is sliced into a set of alternative definitions (i.e. in a structure definition, there are multiple different constraints on a single element in the base resource). Slicing can be used in any resource that has cardinality ..* on the base resource, or any resource with a choice of types. The set of slices is any elements that come after this in the element sequence that have the same path, until a shorter path occurs (the shorter path terminates the set). (defined for API consistency)
    }
    property slicing : TFhirElementDefinitionSlicing read FSlicing write SetSlicing;
    {@member slicingElement
      Indicates that the element is sliced into a set of alternative definitions (i.e. in a structure definition, there are multiple different constraints on a single element in the base resource). Slicing can be used in any resource that has cardinality ..* on the base resource, or any resource with a choice of types. The set of slices is any elements that come after this in the element sequence that have the same path, until a shorter path occurs (the shorter path terminates the set).
    }
    property slicingElement : TFhirElementDefinitionSlicing read FSlicing write SetSlicing;

    {@member short
      Typed access to A concise description of what this element means (e.g. for use in autogenerated summaries).
    }
    property short : String read GetShortST write SetShortST;
    {@member shortElement
      A concise description of what this element means (e.g. for use in autogenerated summaries).
    }
    property shortElement : TFhirString read FShort write SetShort;

    {@member definition
      Typed access to Provides a complete explanation of the meaning of the data element for human readability.  For the case of elements derived from existing elements (e.g. constraints), the definition SHALL be consistent with the base definition, but convey the meaning of the element in the particular context of use of the resource.
    }
    property definition : String read GetDefinitionST write SetDefinitionST;
    {@member definitionElement
      Provides a complete explanation of the meaning of the data element for human readability.  For the case of elements derived from existing elements (e.g. constraints), the definition SHALL be consistent with the base definition, but convey the meaning of the element in the particular context of use of the resource.
    }
    property definitionElement : TFhirString read FDefinition write SetDefinition;

    {@member comments
      Typed access to Explanatory notes and implementation guidance about the data element, including notes about how to use the data properly, exceptions to proper use, etc.
    }
    property comments : String read GetCommentsST write SetCommentsST;
    {@member commentsElement
      Explanatory notes and implementation guidance about the data element, including notes about how to use the data properly, exceptions to proper use, etc.
    }
    property commentsElement : TFhirString read FComments write SetComments;

    {@member requirements
      Typed access to This element is for traceability of why the element was created and why the constraints exist as they do. This may be used to point to source materials or specifications that drove the structure of this element.
    }
    property requirements : String read GetRequirementsST write SetRequirementsST;
    {@member requirementsElement
      This element is for traceability of why the element was created and why the constraints exist as they do. This may be used to point to source materials or specifications that drove the structure of this element.
    }
    property requirementsElement : TFhirString read FRequirements write SetRequirements;

    {@member aliasList
      Identifies additional names by which this element might also be known.
    }
    property aliasList : TFhirStringList read GetAliasList;
    property hasAliasList : boolean read GetHasAliasList;

    {@member min
      Typed access to The minimum number of times this element SHALL appear in the instance.
    }
    property min : String read GetMinST write SetMinST;
    {@member minElement
      The minimum number of times this element SHALL appear in the instance.
    }
    property minElement : TFhirInteger read FMin write SetMin;

    {@member max
      Typed access to The maximum number of times this element is permitted to appear in the instance.
    }
    property max : String read GetMaxST write SetMaxST;
    {@member maxElement
      The maximum number of times this element is permitted to appear in the instance.
    }
    property maxElement : TFhirString read FMax write SetMax;

    {@member base
      Typed access to Information about the base definition of the element, provided to make it unncessary for tools to trace the derviation of the element through the derived and related profiles. This information is only provided where the element definition represents a constraint on another element definition, and must be present if there is a base element definition. (defined for API consistency)
    }
    property base : TFhirElementDefinitionBase read FBase write SetBase;
    {@member baseElement
      Information about the base definition of the element, provided to make it unncessary for tools to trace the derviation of the element through the derived and related profiles. This information is only provided where the element definition represents a constraint on another element definition, and must be present if there is a base element definition.
    }
    property baseElement : TFhirElementDefinitionBase read FBase write SetBase;

    {@member type_List
      The data type or resource that the value of this element is permitted to be.
    }
    property type_List : TFhirElementDefinitionTypeList read GetType_List;
    property hasType_List : boolean read GetHasType_List;

    {@member nameReference
      Typed access to Identifies the name of a slice defined elsewhere in the profile whose constraints should be applied to the current element.
    }
    property nameReference : String read GetNameReferenceST write SetNameReferenceST;
    {@member nameReferenceElement
      Identifies the name of a slice defined elsewhere in the profile whose constraints should be applied to the current element.
    }
    property nameReferenceElement : TFhirString read FNameReference write SetNameReference;

    {@member defaultValue
      Typed access to The value that should be used if there is no value stated in the instance (e.g. 'if not otherwise specified, the abstract is false'). (defined for API consistency)
    }
    property defaultValue : TFhirType read FDefaultValue write SetDefaultValue;
    {@member defaultValueElement
      The value that should be used if there is no value stated in the instance (e.g. 'if not otherwise specified, the abstract is false').
    }
    property defaultValueElement : TFhirType read FDefaultValue write SetDefaultValue;

    {@member meaningWhenMissing
      Typed access to The Implicit meaning that is to be understood when this element is missing (e.g. 'when this element is missing, the period is ongoing'.
    }
    property meaningWhenMissing : String read GetMeaningWhenMissingST write SetMeaningWhenMissingST;
    {@member meaningWhenMissingElement
      The Implicit meaning that is to be understood when this element is missing (e.g. 'when this element is missing, the period is ongoing'.
    }
    property meaningWhenMissingElement : TFhirString read FMeaningWhenMissing write SetMeaningWhenMissing;

    {@member fixed
      Typed access to Specifies a value that SHALL be exactly the value  for this element in the instance. For purposes of comparison, non-signficant whitespace is ignored, and all values must be an exact match (case and accent sensitive). Missing elements/attributes must also be missing. (defined for API consistency)
    }
    property fixed : TFhirType read FFixed write SetFixed;
    {@member fixedElement
      Specifies a value that SHALL be exactly the value  for this element in the instance. For purposes of comparison, non-signficant whitespace is ignored, and all values must be an exact match (case and accent sensitive). Missing elements/attributes must also be missing.
    }
    property fixedElement : TFhirType read FFixed write SetFixed;

    {@member pattern
      Typed access to Specifies a value that the value in the instance SHALL follow - that is, any value in the pattern must be found in the instance. Other additional values may be found too. This is effectively constraint by example.  The values of elements present in the pattern must match exactly (case-senstive, accent-sensitive, etc.). (defined for API consistency)
    }
    property pattern : TFhirType read FPattern write SetPattern;
    {@member patternElement
      Specifies a value that the value in the instance SHALL follow - that is, any value in the pattern must be found in the instance. Other additional values may be found too. This is effectively constraint by example.  The values of elements present in the pattern must match exactly (case-senstive, accent-sensitive, etc.).
    }
    property patternElement : TFhirType read FPattern write SetPattern;

    {@member example
      Typed access to A sample value for this element demonstrating the type of information that would typically be captured. (defined for API consistency)
    }
    property example : TFhirType read FExample write SetExample;
    {@member exampleElement
      A sample value for this element demonstrating the type of information that would typically be captured.
    }
    property exampleElement : TFhirType read FExample write SetExample;

    {@member minValue
      Typed access to The minimum allowed value for the element. The value is inclusive. This is allowed for the types date, dateTime, instant, time, decimal, integer, and Quantity. (defined for API consistency)
    }
    property minValue : TFhirType read FMinValue write SetMinValue;
    {@member minValueElement
      The minimum allowed value for the element. The value is inclusive. This is allowed for the types date, dateTime, instant, time, decimal, integer, and Quantity.
    }
    property minValueElement : TFhirType read FMinValue write SetMinValue;

    {@member maxValue
      Typed access to The maximum allowed value for the element. The value is inclusive. This is allowed for the types date, dateTime, instant, time, decimal, integer, and Quantity. (defined for API consistency)
    }
    property maxValue : TFhirType read FMaxValue write SetMaxValue;
    {@member maxValueElement
      The maximum allowed value for the element. The value is inclusive. This is allowed for the types date, dateTime, instant, time, decimal, integer, and Quantity.
    }
    property maxValueElement : TFhirType read FMaxValue write SetMaxValue;

    {@member maxLength
      Typed access to Indicates the maximum length in characters that is permitted to be present in conformant instances and which is expected to be supported by conformant consumers that support the element.
    }
    property maxLength : String read GetMaxLengthST write SetMaxLengthST;
    {@member maxLengthElement
      Indicates the maximum length in characters that is permitted to be present in conformant instances and which is expected to be supported by conformant consumers that support the element.
    }
    property maxLengthElement : TFhirInteger read FMaxLength write SetMaxLength;

    {@member conditionList
      A reference to an invariant that may make additional statements about the cardinality or value in the instance.
    }
    property conditionList : TFhirIdList read GetConditionList;
    property hasConditionList : boolean read GetHasConditionList;

    {@member constraintList
      Formal constraints such as co-occurrence and other constraints that can be computationally evaluated within the context of the instance.
    }
    property constraintList : TFhirElementDefinitionConstraintList read GetConstraintList;
    property hasConstraintList : boolean read GetHasConstraintList;

    {@member mustSupport
      Typed access to If true, implementations that produce or consume resources SHALL provide "support" for the element in some meaningful way.  If false, the element may be ignored and not supported.
    }
    property mustSupport : Boolean read GetMustSupportST write SetMustSupportST;
    {@member mustSupportElement
      If true, implementations that produce or consume resources SHALL provide "support" for the element in some meaningful way.  If false, the element may be ignored and not supported.
    }
    property mustSupportElement : TFhirBoolean read FMustSupport write SetMustSupport;

    {@member isModifier
      Typed access to If true, the value of this element affects the interpretation of the element or resource that contains it, and the value of the element cannot be ignored. Typically, this is used for status, negation and qualification codes. The effect of this is that the element cannot be ignored by systems: they SHALL either recognize the element and process it, and/or a pre-determination has been made that it is not relevant to their particular system.
    }
    property isModifier : Boolean read GetIsModifierST write SetIsModifierST;
    {@member isModifierElement
      If true, the value of this element affects the interpretation of the element or resource that contains it, and the value of the element cannot be ignored. Typically, this is used for status, negation and qualification codes. The effect of this is that the element cannot be ignored by systems: they SHALL either recognize the element and process it, and/or a pre-determination has been made that it is not relevant to their particular system.
    }
    property isModifierElement : TFhirBoolean read FIsModifier write SetIsModifier;

    {@member isSummary
      Typed access to Whether the element should be included if a client requests a search with the parameter _summary=true.
    }
    property isSummary : Boolean read GetIsSummaryST write SetIsSummaryST;
    {@member isSummaryElement
      Whether the element should be included if a client requests a search with the parameter _summary=true.
    }
    property isSummaryElement : TFhirBoolean read FIsSummary write SetIsSummary;

    {@member binding
      Typed access to Binds to a value set if this element is coded (code, Coding, CodeableConcept). (defined for API consistency)
    }
    property binding : TFhirElementDefinitionBinding read FBinding write SetBinding;
    {@member bindingElement
      Binds to a value set if this element is coded (code, Coding, CodeableConcept).
    }
    property bindingElement : TFhirElementDefinitionBinding read FBinding write SetBinding;

    {@member mappingList
      Identifies a concept from an external specification that roughly corresponds to this element.
    }
    property mappingList : TFhirElementDefinitionMappingList read GetMappingList;
    property hasMappingList : boolean read GetHasMappingList;

  end;


  TFhirElementDefinitionListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirElementDefinitionList;
    function GetCurrent : TFhirElementDefinition;
  public
    Constructor Create(list : TFhirElementDefinitionList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirElementDefinition read GetCurrent;
  end;


  {@Class TFhirElementDefinitionList
    A list of FhirElementDefinition
  }
  {!.Net HL7Connect.Fhir.ElementDefinitionList}
  TFhirElementDefinitionList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirElementDefinition;
    procedure SetItemN(index : Integer; value : TFhirElementDefinition);
  public
    {!script hide}
    function Link : TFhirElementDefinitionList; Overload;
    function Clone : TFhirElementDefinitionList; Overload;
    function GetEnumerator : TFhirElementDefinitionListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirElementDefinition to the end of the list.
    }
    function Append : TFhirElementDefinition;

    
    {@member AddItem
      Add an already existing FhirElementDefinition to the end of the list.
    }
    procedure AddItem(value : TFhirElementDefinition); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirElementDefinition) : Integer;
    

    {@member Insert
      Insert FhirElementDefinition before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirElementDefinition;
    

    {@member InsertItem
       Insert an existing FhirElementDefinition before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirElementDefinition);
    
    {@member Item
       Get the iIndexth FhirElementDefinition. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirElementDefinition. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirElementDefinition);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirElementDefinition;
    
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
    
    Property FhirElementDefinitions[index : Integer] : TFhirElementDefinition read GetItemN write SetItemN; default;
  End;


  {@Class TFhirTimingRepeat : TFhirElement
    A set of rules that describe when the event should occur.
  }
  {!.Net HL7Connect.Fhir.TimingRepeat}
  TFhirTimingRepeat = class (TFhirElement)
  private
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
    Function GetDurationUnitsST : TFhirUnitsOfTime;
    Procedure SetDurationUnitsST(value : TFhirUnitsOfTime);
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
    Function GetPeriodUnitsST : TFhirUnitsOfTime;
    Procedure SetPeriodUnitsST(value : TFhirUnitsOfTime);
    Procedure SetWhen(value : TFhirEnum);
    Function GetWhenST : TFhirEventTiming;
    Procedure SetWhenST(value : TFhirEventTiming);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirTimingRepeat; overload;
    function Clone : TFhirTimingRepeat; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member bounds
      Typed access to Either a duration for the length of the timing schedule, a range of possible length, or outer bounds for start and/or end limits of the timing schedule. (defined for API consistency)
    }
    property bounds : TFhirType read FBounds write SetBounds;
    {@member boundsElement
      Either a duration for the length of the timing schedule, a range of possible length, or outer bounds for start and/or end limits of the timing schedule.
    }
    property boundsElement : TFhirType read FBounds write SetBounds;

    {@member count
      Typed access to A total count of the desired number of repetitions.
    }
    property count : String read GetCountST write SetCountST;
    {@member countElement
      A total count of the desired number of repetitions.
    }
    property countElement : TFhirInteger read FCount write SetCount;

    {@member duration
      Typed access to How long this thing happens for when it happens.
    }
    property duration : String read GetDurationST write SetDurationST;
    {@member durationElement
      How long this thing happens for when it happens.
    }
    property durationElement : TFhirDecimal read FDuration write SetDuration;

    {@member durationMax
      Typed access to The upper limit of how long this thing happens for when it happens.
    }
    property durationMax : String read GetDurationMaxST write SetDurationMaxST;
    {@member durationMaxElement
      The upper limit of how long this thing happens for when it happens.
    }
    property durationMaxElement : TFhirDecimal read FDurationMax write SetDurationMax;

    {@member durationUnits
      The units of time for the duration, in UCUM units.
    }
    property durationUnits : TFhirUnitsOfTime read GetDurationUnitsST write SetDurationUnitsST;
    property durationUnitsElement : TFhirEnum read FDurationUnits write SetDurationUnits;

    {@member frequency
      Typed access to The number of times to repeat the action within the specified period / period range (i.e. both period and periodMax provided).
    }
    property frequency : String read GetFrequencyST write SetFrequencyST;
    {@member frequencyElement
      The number of times to repeat the action within the specified period / period range (i.e. both period and periodMax provided).
    }
    property frequencyElement : TFhirInteger read FFrequency write SetFrequency;

    {@member frequencyMax
      Typed access to If present, indicates that the frequency is a range - so repeat between [frequency] and [frequencyMax] times within the period or period range.
    }
    property frequencyMax : String read GetFrequencyMaxST write SetFrequencyMaxST;
    {@member frequencyMaxElement
      If present, indicates that the frequency is a range - so repeat between [frequency] and [frequencyMax] times within the period or period range.
    }
    property frequencyMaxElement : TFhirInteger read FFrequencyMax write SetFrequencyMax;

    {@member period
      Typed access to Indicates the duration of time over which repetitions are to occur; e.g. to express "3 times per day", 3 would be the frequency and "1 day" would be the period.
    }
    property period : String read GetPeriodST write SetPeriodST;
    {@member periodElement
      Indicates the duration of time over which repetitions are to occur; e.g. to express "3 times per day", 3 would be the frequency and "1 day" would be the period.
    }
    property periodElement : TFhirDecimal read FPeriod write SetPeriod;

    {@member periodMax
      Typed access to If present, indicates that the period is a range from [period] to [periodMax], allowing expressing concepts such as "do this once every 3-5 days.
    }
    property periodMax : String read GetPeriodMaxST write SetPeriodMaxST;
    {@member periodMaxElement
      If present, indicates that the period is a range from [period] to [periodMax], allowing expressing concepts such as "do this once every 3-5 days.
    }
    property periodMaxElement : TFhirDecimal read FPeriodMax write SetPeriodMax;

    {@member periodUnits
      The units of time for the period in UCUM units.
    }
    property periodUnits : TFhirUnitsOfTime read GetPeriodUnitsST write SetPeriodUnitsST;
    property periodUnitsElement : TFhirEnum read FPeriodUnits write SetPeriodUnits;

    {@member when
      A real world event that the occurrence of the event should be tied to.
    }
    property when : TFhirEventTiming read GetWhenST write SetWhenST;
    property whenElement : TFhirEnum read FWhen write SetWhen;

  end;


  TFhirTimingRepeatListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirTimingRepeatList;
    function GetCurrent : TFhirTimingRepeat;
  public
    Constructor Create(list : TFhirTimingRepeatList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirTimingRepeat read GetCurrent;
  end;


  {@Class TFhirTimingRepeatList
    A list of FhirTimingRepeat
  }
  {!.Net HL7Connect.Fhir.TimingRepeatList}
  TFhirTimingRepeatList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirTimingRepeat;
    procedure SetItemN(index : Integer; value : TFhirTimingRepeat);
  public
    {!script hide}
    function Link : TFhirTimingRepeatList; Overload;
    function Clone : TFhirTimingRepeatList; Overload;
    function GetEnumerator : TFhirTimingRepeatListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirTimingRepeat to the end of the list.
    }
    function Append : TFhirTimingRepeat;

    
    {@member AddItem
      Add an already existing FhirTimingRepeat to the end of the list.
    }
    procedure AddItem(value : TFhirTimingRepeat); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirTimingRepeat) : Integer;
    

    {@member Insert
      Insert FhirTimingRepeat before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirTimingRepeat;
    

    {@member InsertItem
       Insert an existing FhirTimingRepeat before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirTimingRepeat);
    
    {@member Item
       Get the iIndexth FhirTimingRepeat. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirTimingRepeat. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirTimingRepeat);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirTimingRepeat;
    
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
    
    Property FhirTimingRepeats[index : Integer] : TFhirTimingRepeat read GetItemN write SetItemN; default;
  End;


  {@Class TFhirTiming : TFhirType
    Specifies an event that may occur multiple times. Timing schedules are used to record when things are expected or requested to occur. The most common usage is in dosage instructions for medications. They are also used when planning care of various kinds.
  }
  {!.Net HL7Connect.Fhir.Timing}
  TFhirTiming = class (TFhirType)
  private
    FeventList : TFhirDateTimeList;
    FRepeat_ : TFhirTimingRepeat;
    FCode : TFhirCodeableConcept;
    function GetEventList : TFhirDateTimeList;
    function GetHasEventList : Boolean;
    Procedure SetRepeat_(value : TFhirTimingRepeat);
    Procedure SetCode(value : TFhirCodeableConcept);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirTiming; overload;
    function Clone : TFhirTiming; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member eventList
      Identifies specific times when the event occurs.
    }
    property eventList : TFhirDateTimeList read GetEventList;
    property hasEventList : boolean read GetHasEventList;

    {@member repeat_
      Typed access to A set of rules that describe when the event should occur. (defined for API consistency)
    }
    property repeat_ : TFhirTimingRepeat read FRepeat_ write SetRepeat_;
    {@member repeat_Element
      A set of rules that describe when the event should occur.
    }
    property repeat_Element : TFhirTimingRepeat read FRepeat_ write SetRepeat_;

    {@member code
      Typed access to A code for the timing pattern. Some codes such as BID are uniquitious, but many institutions define their own additional codes. (defined for API consistency)
    }
    property code : TFhirCodeableConcept read FCode write SetCode;
    {@member codeElement
      A code for the timing pattern. Some codes such as BID are uniquitious, but many institutions define their own additional codes.
    }
    property codeElement : TFhirCodeableConcept read FCode write SetCode;

  end;


  TFhirTimingListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirTimingList;
    function GetCurrent : TFhirTiming;
  public
    Constructor Create(list : TFhirTimingList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirTiming read GetCurrent;
  end;


  {@Class TFhirTimingList
    A list of FhirTiming
  }
  {!.Net HL7Connect.Fhir.TimingList}
  TFhirTimingList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirTiming;
    procedure SetItemN(index : Integer; value : TFhirTiming);
  public
    {!script hide}
    function Link : TFhirTimingList; Overload;
    function Clone : TFhirTimingList; Overload;
    function GetEnumerator : TFhirTimingListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirTiming to the end of the list.
    }
    function Append : TFhirTiming;

    
    {@member AddItem
      Add an already existing FhirTiming to the end of the list.
    }
    procedure AddItem(value : TFhirTiming); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirTiming) : Integer;
    

    {@member Insert
      Insert FhirTiming before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirTiming;
    

    {@member InsertItem
       Insert an existing FhirTiming before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirTiming);
    
    {@member Item
       Get the iIndexth FhirTiming. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirTiming. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirTiming);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirTiming;
    
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
    
    Property FhirTimings[index : Integer] : TFhirTiming read GetItemN write SetItemN; default;
  End;


  {@Class TFhirAddress : TFhirType
    There is a variety of postal address formats defined around the world. This format defines a superset that is the basis for all addresses around the world.
  }
  {!.Net HL7Connect.Fhir.Address}
  TFhirAddress = class (TFhirType)
  private
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
    Function GetUseST : TFhirAddressUse;
    Procedure SetUseST(value : TFhirAddressUse);
    Procedure SetType_(value : TFhirEnum);
    Function GetType_ST : TFhirAddressType;
    Procedure SetType_ST(value : TFhirAddressType);
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
    property useElement : TFhirEnum read FUse write SetUse;

    {@member type_
      Distinguishes between physical addresses (those you can visit) and mailing addresses (e.g. PO Boxes and care-of addresses). Most addresses are both.
    }
    property type_ : TFhirAddressType read GetType_ST write SetType_ST;
    property type_Element : TFhirEnum read FType_ write SetType_;

    {@member text
      Typed access to A full text representation of the address.
    }
    property text : String read GetTextST write SetTextST;
    {@member textElement
      A full text representation of the address.
    }
    property textElement : TFhirString read FText write SetText;

    {@member lineList
      This component contains the house number, apartment number, street name, street direction,  P.O. Box number, delivery hints, and similar address information.
    }
    property lineList : TFhirStringList read GetLineList;
    property hasLineList : boolean read GetHasLineList;

    {@member city
      Typed access to The name of the city, town, village or other community or delivery center.
    }
    property city : String read GetCityST write SetCityST;
    {@member cityElement
      The name of the city, town, village or other community or delivery center.
    }
    property cityElement : TFhirString read FCity write SetCity;

    {@member district
      Typed access to The name of the administrative area (county).
    }
    property district : String read GetDistrictST write SetDistrictST;
    {@member districtElement
      The name of the administrative area (county).
    }
    property districtElement : TFhirString read FDistrict write SetDistrict;

    {@member state
      Typed access to Sub-unit of a country with limited sovereignty in a federally organized country. A code may be used if codes are in common use (i.e. US 2 letter state codes).
    }
    property state : String read GetStateST write SetStateST;
    {@member stateElement
      Sub-unit of a country with limited sovereignty in a federally organized country. A code may be used if codes are in common use (i.e. US 2 letter state codes).
    }
    property stateElement : TFhirString read FState write SetState;

    {@member postalCode
      Typed access to A postal code designating a region defined by the postal service.
    }
    property postalCode : String read GetPostalCodeST write SetPostalCodeST;
    {@member postalCodeElement
      A postal code designating a region defined by the postal service.
    }
    property postalCodeElement : TFhirString read FPostalCode write SetPostalCode;

    {@member country
      Typed access to Country - a nation as commonly understood or generally accepted.
    }
    property country : String read GetCountryST write SetCountryST;
    {@member countryElement
      Country - a nation as commonly understood or generally accepted.
    }
    property countryElement : TFhirString read FCountry write SetCountry;

    {@member period
      Typed access to Time period when address was/is in use. (defined for API consistency)
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;
    {@member periodElement
      Time period when address was/is in use.
    }
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

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
    function GetFamilyList : TFhirStringList;
    function GetHasFamilyList : Boolean;
    function GetGivenList : TFhirStringList;
    function GetHasGivenList : Boolean;
    function GetPrefixList : TFhirStringList;
    function GetHasPrefixList : Boolean;
    function GetSuffixList : TFhirStringList;
    function GetHasSuffixList : Boolean;
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
    property useElement : TFhirEnum read FUse write SetUse;

    {@member text
      Typed access to A full text representation of the name.
    }
    property text : String read GetTextST write SetTextST;
    {@member textElement
      A full text representation of the name.
    }
    property textElement : TFhirString read FText write SetText;

    {@member familyList
      The part of a name that links to the genealogy. In some cultures (e.g. Eritrea) the family name of a son is the first name of his father.
    }
    property familyList : TFhirStringList read GetFamilyList;
    property hasFamilyList : boolean read GetHasFamilyList;

    {@member givenList
      Given name.
    }
    property givenList : TFhirStringList read GetGivenList;
    property hasGivenList : boolean read GetHasGivenList;

    {@member prefixList
      Part of the name that is acquired as a title due to academic, legal, employment or nobility status, etc. and that appears at the start of the name.
    }
    property prefixList : TFhirStringList read GetPrefixList;
    property hasPrefixList : boolean read GetHasPrefixList;

    {@member suffixList
      Part of the name that is acquired as a title due to academic, legal, employment or nobility status, etc. and that appears at the end of the name.
    }
    property suffixList : TFhirStringList read GetSuffixList;
    property hasSuffixList : boolean read GetHasSuffixList;

    {@member period
      Typed access to Indicates the period of time when this name was valid for the named person. (defined for API consistency)
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;
    {@member periodElement
      Indicates the period of time when this name was valid for the named person.
    }
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

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


  {@Class TFhirMeta : TFhirType
    The metadata about a resource. This is content in the resource that is maintained by the infrastructure. Changes to the content may not always be associated with version changes to the resource.
  }
  {!.Net HL7Connect.Fhir.Meta}
  TFhirMeta = class (TFhirType)
  private
    FVersionId : TFhirId;
    FLastUpdated : TFhirInstant;
    FprofileList : TFhirUriList;
    FsecurityList : TFhirCodingList;
    FtagList : TFhirCodingList;
    Procedure SetVersionId(value : TFhirId);
    Function GetVersionIdST : String;
    Procedure SetVersionIdST(value : String);
    Procedure SetLastUpdated(value : TFhirInstant);
    Function GetLastUpdatedST : TDateAndTime;
    Procedure SetLastUpdatedST(value : TDateAndTime);
    function GetProfileList : TFhirUriList;
    function GetHasProfileList : Boolean;
    function GetSecurityList : TFhirCodingList;
    function GetHasSecurityList : Boolean;
    function GetTagList : TFhirCodingList;
    function GetHasTagList : Boolean;
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirMeta; overload;
    function Clone : TFhirMeta; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member versionId
      Typed access to The version specific identifier, as it appears in the version portion of the URL. This values changes when the resource is created, updated, or deleted.
    }
    property versionId : String read GetVersionIdST write SetVersionIdST;
    {@member versionIdElement
      The version specific identifier, as it appears in the version portion of the URL. This values changes when the resource is created, updated, or deleted.
    }
    property versionIdElement : TFhirId read FVersionId write SetVersionId;

    {@member lastUpdated
      Typed access to When the resource last changed - e.g. when the version changed.
    }
    property lastUpdated : TDateAndTime read GetLastUpdatedST write SetLastUpdatedST;
    {@member lastUpdatedElement
      When the resource last changed - e.g. when the version changed.
    }
    property lastUpdatedElement : TFhirInstant read FLastUpdated write SetLastUpdated;

    {@member profileList
      A list of profiles [[[StructureDefinition]]]s that this resource claims to conform to. The URL is a reference to [[[StructureDefinition.url]]].
    }
    property profileList : TFhirUriList read GetProfileList;
    property hasProfileList : boolean read GetHasProfileList;

    {@member securityList
      Security labels applied to this resource. These tags connect specific resources to the overall security policy and infrastructure.
    }
    property securityList : TFhirCodingList read GetSecurityList;
    property hasSecurityList : boolean read GetHasSecurityList;

    {@member tagList
      Tags applied to this resource. Tags are intended to to be used to identify and relate resources to process and workflow, and applications are not required to consider the tags when interpreting the meaning of a resource.
    }
    property tagList : TFhirCodingList read GetTagList;
    property hasTagList : boolean read GetHasTagList;

  end;


  TFhirMetaListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirMetaList;
    function GetCurrent : TFhirMeta;
  public
    Constructor Create(list : TFhirMetaList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirMeta read GetCurrent;
  end;


  {@Class TFhirMetaList
    A list of FhirMeta
  }
  {!.Net HL7Connect.Fhir.MetaList}
  TFhirMetaList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirMeta;
    procedure SetItemN(index : Integer; value : TFhirMeta);
  public
    {!script hide}
    function Link : TFhirMetaList; Overload;
    function Clone : TFhirMetaList; Overload;
    function GetEnumerator : TFhirMetaListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirMeta to the end of the list.
    }
    function Append : TFhirMeta;

    
    {@member AddItem
      Add an already existing FhirMeta to the end of the list.
    }
    procedure AddItem(value : TFhirMeta); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirMeta) : Integer;
    

    {@member Insert
      Insert FhirMeta before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirMeta;
    

    {@member InsertItem
       Insert an existing FhirMeta before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirMeta);
    
    {@member Item
       Get the iIndexth FhirMeta. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirMeta. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirMeta);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirMeta;
    
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
    
    Property FhirMeta[index : Integer] : TFhirMeta read GetItemN write SetItemN; default;
  End;


  {@Class TFhirContactPoint : TFhirType
    Details for All kinds of technology mediated contact points for a person or organization, including telephone, email, etc.
  }
  {!.Net HL7Connect.Fhir.ContactPoint}
  TFhirContactPoint = class (TFhirType)
  private
    FSystem : TFhirEnum;
    FValue : TFhirString;
    FUse : TFhirEnum;
    FRank : TFhirPositiveInt;
    FPeriod : TFhirPeriod;
    Procedure SetSystem(value : TFhirEnum);
    Function GetSystemST : TFhirContactPointSystem;
    Procedure SetSystemST(value : TFhirContactPointSystem);
    Procedure SetValue(value : TFhirString);
    Function GetValueST : String;
    Procedure SetValueST(value : String);
    Procedure SetUse(value : TFhirEnum);
    Function GetUseST : TFhirContactPointUse;
    Procedure SetUseST(value : TFhirContactPointUse);
    Procedure SetRank(value : TFhirPositiveInt);
    Function GetRankST : String;
    Procedure SetRankST(value : String);
    Procedure SetPeriod(value : TFhirPeriod);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirContactPoint; overload;
    function Clone : TFhirContactPoint; overload;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member system
      Telecommunications form for contact point - what communications system is required to make use of the contact.
    }
    property system : TFhirContactPointSystem read GetSystemST write SetSystemST;
    property systemElement : TFhirEnum read FSystem write SetSystem;

    {@member value
      Typed access to The actual contact point details, in a form that is meaningful to the designated communication system (i.e. phone number or email address).
    }
    property value : String read GetValueST write SetValueST;
    {@member valueElement
      The actual contact point details, in a form that is meaningful to the designated communication system (i.e. phone number or email address).
    }
    property valueElement : TFhirString read FValue write SetValue;

    {@member use
      Identifies the purpose for the contact point.
    }
    property use : TFhirContactPointUse read GetUseST write SetUseST;
    property useElement : TFhirEnum read FUse write SetUse;

    {@member rank
      Typed access to Specifies a preferred order in which to use a set of contacts. Contacts are ranked with lower values coming before higher values.
    }
    property rank : String read GetRankST write SetRankST;
    {@member rankElement
      Specifies a preferred order in which to use a set of contacts. Contacts are ranked with lower values coming before higher values.
    }
    property rankElement : TFhirPositiveInt read FRank write SetRank;

    {@member period
      Typed access to Time period when the contact point was/is in use. (defined for API consistency)
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;
    {@member periodElement
      Time period when the contact point was/is in use.
    }
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

  end;


  TFhirContactPointListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirContactPointList;
    function GetCurrent : TFhirContactPoint;
  public
    Constructor Create(list : TFhirContactPointList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirContactPoint read GetCurrent;
  end;


  {@Class TFhirContactPointList
    A list of FhirContactPoint
  }
  {!.Net HL7Connect.Fhir.ContactPointList}
  TFhirContactPointList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirContactPoint;
    procedure SetItemN(index : Integer; value : TFhirContactPoint);
  public
    {!script hide}
    function Link : TFhirContactPointList; Overload;
    function Clone : TFhirContactPointList; Overload;
    function GetEnumerator : TFhirContactPointListEnumerator;
    {!script show}
    

    {@member Append
      Add a FhirContactPoint to the end of the list.
    }
    function Append : TFhirContactPoint;

    
    {@member AddItem
      Add an already existing FhirContactPoint to the end of the list.
    }
    procedure AddItem(value : TFhirContactPoint); overload;

    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirContactPoint) : Integer;
    

    {@member Insert
      Insert FhirContactPoint before the designated index (0 = first item)
    }
    function Insert(index : Integer) : TFhirContactPoint;
    

    {@member InsertItem
       Insert an existing FhirContactPoint before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirContactPoint);
    
    {@member Item
       Get the iIndexth FhirContactPoint. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirContactPoint. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirContactPoint);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirContactPoint;
    
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
    
    Property FhirContactPoints[index : Integer] : TFhirContactPoint read GetItemN write SetItemN; default;
  End;


  TFhirAge = TFhirQuantity;

  TFhirCount = TFhirQuantity;

  TFhirMoney = TFhirQuantity;

  TFhirDistance = TFhirQuantity;

  TFhirDuration = TFhirQuantity;

  TFhirSimpleQuantity = TFhirQuantity;

Const
  CODES_TFhirNarrativeStatus : Array[TFhirNarrativeStatus] of String = ('', 'generated', 'extensions', 'additional', 'empty');
  CODES_TFhirQuantityComparator : Array[TFhirQuantityComparator] of String = ('', '<', '<=', '>=', '>');
  CODES_TFhirIdentifierUse : Array[TFhirIdentifierUse] of String = ('', 'usual', 'official', 'temp', 'secondary');
  CODES_TFhirPropertyRepresentation : Array[TFhirPropertyRepresentation] of String = ('', 'xmlAttr');
  CODES_TFhirResourceSlicingRules : Array[TFhirResourceSlicingRules] of String = ('', 'closed', 'open', 'openAtEnd');
  CODES_TFhirResourceAggregationMode : Array[TFhirResourceAggregationMode] of String = ('', 'contained', 'referenced', 'bundled');
  CODES_TFhirConstraintSeverity : Array[TFhirConstraintSeverity] of String = ('', 'error', 'warning');
  CODES_TFhirBindingStrength : Array[TFhirBindingStrength] of String = ('', 'required', 'extensible', 'preferred', 'example');
  CODES_TFhirUnitsOfTime : Array[TFhirUnitsOfTime] of String = ('', 's', 'min', 'h', 'd', 'wk', 'mo', 'a');
  CODES_TFhirEventTiming : Array[TFhirEventTiming] of String = ('', 'HS', 'WAKE', 'C', 'CM', 'CD', 'CV', 'AC', 'ACM', 'ACD', 'ACV', 'PC', 'PCM', 'PCD', 'PCV');
  CODES_TFhirAddressUse : Array[TFhirAddressUse] of String = ('', 'home', 'work', 'temp', 'old');
  CODES_TFhirAddressType : Array[TFhirAddressType] of String = ('', 'postal', 'physical', 'both');
  CODES_TFhirNameUse : Array[TFhirNameUse] of String = ('', 'usual', 'official', 'temp', 'nickname', 'anonymous', 'old', 'maiden');
  CODES_TFhirContactPointSystem : Array[TFhirContactPointSystem] of String = ('', 'phone', 'fax', 'email', 'pager', 'other');
  CODES_TFhirContactPointUse : Array[TFhirContactPointUse] of String = ('', 'home', 'work', 'temp', 'old', 'mobile');
  CODES_TFhirAccountStatus : Array[TFhirAccountStatus] of String = ('', 'active', 'inactive');
  CODES_TFhirAllergyIntoleranceStatus : Array[TFhirAllergyIntoleranceStatus] of String = ('', 'active', 'unconfirmed', 'confirmed', 'inactive', 'resolved', 'refuted', 'entered-in-error');
  CODES_TFhirAllergyIntoleranceCriticality : Array[TFhirAllergyIntoleranceCriticality] of String = ('', 'CRITL', 'CRITH', 'CRITU');
  CODES_TFhirAllergyIntoleranceType : Array[TFhirAllergyIntoleranceType] of String = ('', 'allergy', 'intolerance');
  CODES_TFhirAllergyIntoleranceCategory : Array[TFhirAllergyIntoleranceCategory] of String = ('', 'food', 'medication', 'environment', 'other');
  CODES_TFhirReactionEventCertainty : Array[TFhirReactionEventCertainty] of String = ('', 'unlikely', 'likely', 'confirmed');
  CODES_TFhirReactionEventSeverity : Array[TFhirReactionEventSeverity] of String = ('', 'mild', 'moderate', 'severe');
  CODES_TFhirAppointmentstatus : Array[TFhirAppointmentstatus] of String = ('', 'proposed', 'pending', 'booked', 'arrived', 'fulfilled', 'cancelled', 'noshow');
  CODES_TFhirParticipantrequired : Array[TFhirParticipantrequired] of String = ('', 'required', 'optional', 'information-only');
  CODES_TFhirParticipationstatus : Array[TFhirParticipationstatus] of String = ('', 'accepted', 'declined', 'tentative', 'needs-action');
  CODES_TFhirParticipantstatus : Array[TFhirParticipantstatus] of String = ('', 'accepted', 'declined', 'tentative', 'in-process', 'completed', 'needs-action');
  CODES_TFhirAuditEventAction : Array[TFhirAuditEventAction] of String = ('', 'C', 'R', 'U', 'D', 'E');
  CODES_TFhirAuditEventOutcome : Array[TFhirAuditEventOutcome] of String = ('', '0', '4', '8', '12');
  CODES_TFhirNetworkType : Array[TFhirNetworkType] of String = ('', '1', '2', '3', '4', '5');
  CODES_TFhirBundleType : Array[TFhirBundleType] of String = ('', 'document', 'message', 'transaction', 'transaction-response', 'batch', 'batch-response', 'history', 'searchset', 'collection');
  CODES_TFhirSearchEntryMode : Array[TFhirSearchEntryMode] of String = ('', 'match', 'include', 'outcome');
  CODES_TFhirHttpVerb : Array[TFhirHttpVerb] of String = ('', 'GET', 'POST', 'PUT', 'DELETE');
  CODES_TFhirCarePlanStatus : Array[TFhirCarePlanStatus] of String = ('', 'proposed', 'draft', 'active', 'completed', 'cancelled');
  CODES_TFhirCarePlanRelationship : Array[TFhirCarePlanRelationship] of String = ('', 'includes', 'replaces', 'fulfills');
  CODES_TFhirCarePlanActivityStatus : Array[TFhirCarePlanActivityStatus] of String = ('', 'not-started', 'scheduled', 'in-progress', 'on-hold', 'completed', 'cancelled');
  CODES_TFhirClaimTypeLink : Array[TFhirClaimTypeLink] of String = ('', 'institutional', 'oral', 'pharmacy', 'professional', 'vision');
  CODES_TFhirClaimUseLink : Array[TFhirClaimUseLink] of String = ('', 'complete', 'proposed', 'exploratory', 'other');
  CODES_TFhirRemittanceOutcome : Array[TFhirRemittanceOutcome] of String = ('', 'complete', 'error');
  CODES_TFhirClinicalImpressionStatus : Array[TFhirClinicalImpressionStatus] of String = ('', 'in-progress', 'completed', 'entered-in-error');
  CODES_TFhirCommunicationStatus : Array[TFhirCommunicationStatus] of String = ('', 'in-progress', 'completed', 'suspended', 'rejected', 'failed');
  CODES_TFhirCommunicationRequestStatus : Array[TFhirCommunicationRequestStatus] of String = ('', 'proposed', 'planned', 'requested', 'received', 'accepted', 'in-progress', 'completed', 'suspended', 'rejected', 'failed');
  CODES_TFhirCompositionStatus : Array[TFhirCompositionStatus] of String = ('', 'preliminary', 'final', 'amended', 'entered-in-error');
  CODES_TFhirV3Confidentiality : Array[TFhirV3Confidentiality] of String = ('', '_Confidentiality', 'L', 'M', 'N', 'R', 'U', 'V', '_ConfidentialityByAccessKind', 'B', 'D', 'I', '_ConfidentialityByInfoType', 'ETH', 'HIV', 'PSY', 'SDV', '_ConfidentialityModifiers', 'C', 'S', 'T');
  CODES_TFhirCompositionAttestationMode : Array[TFhirCompositionAttestationMode] of String = ('', 'personal', 'professional', 'legal', 'official');
  CODES_TFhirListMode : Array[TFhirListMode] of String = ('', 'working', 'snapshot', 'changes');
  CODES_TFhirConformanceResourceStatus : Array[TFhirConformanceResourceStatus] of String = ('', 'draft', 'active', 'retired');
  CODES_TFhirConceptMapEquivalence : Array[TFhirConceptMapEquivalence] of String = ('', 'equivalent', 'equal', 'wider', 'subsumes', 'narrower', 'specializes', 'inexact', 'unmatched', 'disjoint');
  CODES_TFhirValuesetConditionClinical : Array[TFhirValuesetConditionClinical] of String = ('', 'active', 'relapse', 'remission', 'resolved');
  CODES_TFhirConditionVerStatus : Array[TFhirConditionVerStatus] of String = ('', 'provisional', 'differential', 'confirmed', 'refuted', 'entered-in-error', 'unknown');
  CODES_TFhirConformanceStatementKind : Array[TFhirConformanceStatementKind] of String = ('', 'instance', 'capability', 'requirements');
  CODES_TFhirUnknownContentCode : Array[TFhirUnknownContentCode] of String = ('', 'no', 'extensions', 'elements', 'both');
  CODES_TFhirRestfulConformanceMode : Array[TFhirRestfulConformanceMode] of String = ('', 'client', 'server');
  CODES_TFhirTypeRestfulInteraction : Array[TFhirTypeRestfulInteraction] of String = ('', 'read', 'vread', 'update', 'delete', 'history-instance', 'validate', 'history-type', 'create', 'search-type');
  CODES_TFhirVersioningPolicy : Array[TFhirVersioningPolicy] of String = ('', 'no-version', 'versioned', 'versioned-update');
  CODES_TFhirConditionalDeleteStatus : Array[TFhirConditionalDeleteStatus] of String = ('', 'not-supported', 'single', 'multiple');
  CODES_TFhirSearchParamType : Array[TFhirSearchParamType] of String = ('', 'number', 'date', 'string', 'token', 'reference', 'composite', 'quantity', 'uri');
  CODES_TFhirSearchModifierCode : Array[TFhirSearchModifierCode] of String = ('', 'missing', 'exact', 'contains', 'not', 'text', 'in', 'not-in', 'below', 'above', 'type');
  CODES_TFhirSystemRestfulInteraction : Array[TFhirSystemRestfulInteraction] of String = ('', 'transaction', 'search-system', 'history-system');
  CODES_TFhirTransactionMode : Array[TFhirTransactionMode] of String = ('', 'not-supported', 'batch', 'transaction', 'both');
  CODES_TFhirMessageSignificanceCategory : Array[TFhirMessageSignificanceCategory] of String = ('', 'Consequence', 'Currency', 'Notification');
  CODES_TFhirMessageConformanceEventMode : Array[TFhirMessageConformanceEventMode] of String = ('', 'sender', 'receiver');
  CODES_TFhirDocumentMode : Array[TFhirDocumentMode] of String = ('', 'producer', 'consumer');
  CODES_TFhirDataelementStringency : Array[TFhirDataelementStringency] of String = ('', 'comparable', 'fully-specified', 'equivalent', 'convertable', 'scaleable', 'flexible');
  CODES_TFhirDetectedissueSeverity : Array[TFhirDetectedissueSeverity] of String = ('', 'high', 'moderate', 'low');
  CODES_TFhirDevicestatus : Array[TFhirDevicestatus] of String = ('', 'available', 'not-available', 'entered-in-error');
  CODES_TFhirMeasurementPrinciple : Array[TFhirMeasurementPrinciple] of String = ('', 'other', 'chemical', 'electrical', 'impedance', 'nuclear', 'optical', 'thermal', 'biological', 'mechanical', 'acoustical', 'manual');
  CODES_TFhirMetricOperationalStatus : Array[TFhirMetricOperationalStatus] of String = ('', 'on', 'off', 'standby');
  CODES_TFhirMetricColor : Array[TFhirMetricColor] of String = ('', 'black', 'red', 'green', 'yellow', 'blue', 'magenta', 'cyan', 'white');
  CODES_TFhirMetricCategory : Array[TFhirMetricCategory] of String = ('', 'measurement', 'setting', 'calculation', 'unspecified');
  CODES_TFhirMetricCalibrationType : Array[TFhirMetricCalibrationType] of String = ('', 'unspecified', 'offset', 'gain', 'two-point');
  CODES_TFhirMetricCalibrationState : Array[TFhirMetricCalibrationState] of String = ('', 'not-calibrated', 'calibration-required', 'calibrated', 'unspecified');
  CODES_TFhirDeviceUseRequestStatus : Array[TFhirDeviceUseRequestStatus] of String = ('', 'proposed', 'planned', 'requested', 'received', 'accepted', 'in-progress', 'completed', 'suspended', 'rejected', 'aborted');
  CODES_TFhirDeviceUseRequestPriority : Array[TFhirDeviceUseRequestPriority] of String = ('', 'routine', 'urgent', 'stat', 'asap');
  CODES_TFhirDiagnosticOrderStatus : Array[TFhirDiagnosticOrderStatus] of String = ('', 'proposed', 'draft', 'planned', 'requested', 'received', 'accepted', 'in-progress', 'review', 'completed', 'cancelled', 'suspended', 'rejected', 'failed');
  CODES_TFhirDiagnosticOrderPriority : Array[TFhirDiagnosticOrderPriority] of String = ('', 'routine', 'urgent', 'stat', 'asap');
  CODES_TFhirDiagnosticReportStatus : Array[TFhirDiagnosticReportStatus] of String = ('', 'registered', 'partial', 'final', 'corrected', 'appended', 'cancelled', 'entered-in-error');
  CODES_TFhirDocumentReferenceStatus : Array[TFhirDocumentReferenceStatus] of String = ('', 'current', 'superseded', 'entered-in-error');
  CODES_TFhirDocumentRelationshipType : Array[TFhirDocumentRelationshipType] of String = ('', 'replaces', 'transforms', 'signs', 'appends');
  CODES_TFhirEncounterState : Array[TFhirEncounterState] of String = ('', 'planned', 'arrived', 'in-progress', 'onleave', 'finished', 'cancelled');
  CODES_TFhirEncounterClass : Array[TFhirEncounterClass] of String = ('', 'inpatient', 'outpatient', 'ambulatory', 'emergency', 'home', 'field', 'daytime', 'virtual', 'other');
  CODES_TFhirEncounterLocationStatus : Array[TFhirEncounterLocationStatus] of String = ('', 'planned', 'active', 'reserved', 'completed');
  CODES_TFhirEpisodeOfCareStatus : Array[TFhirEpisodeOfCareStatus] of String = ('', 'planned', 'waitlist', 'active', 'onhold', 'finished', 'cancelled');
  CODES_TFhirHistoryStatus : Array[TFhirHistoryStatus] of String = ('', 'partial', 'completed', 'entered-in-error', 'health-unknown');
  CODES_TFhirAdministrativeGender : Array[TFhirAdministrativeGender] of String = ('', 'male', 'female', 'other', 'unknown');
  CODES_TFhirFlagStatus : Array[TFhirFlagStatus] of String = ('', 'active', 'inactive', 'entered-in-error');
  CODES_TFhirGoalStatus : Array[TFhirGoalStatus] of String = ('', 'proposed', 'planned', 'accepted', 'rejected', 'in-progress', 'achieved', 'sustaining', 'on-hold', 'cancelled');
  CODES_TFhirGroupType : Array[TFhirGroupType] of String = ('', 'person', 'animal', 'practitioner', 'device', 'medication', 'substance');
  CODES_TFhirDaysOfWeek : Array[TFhirDaysOfWeek] of String = ('', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun');
  CODES_TFhirInstanceAvailability : Array[TFhirInstanceAvailability] of String = ('', 'ONLINE', 'OFFLINE', 'NEARLINE', 'UNAVAILABLE');
  CODES_TFhirMedicationAdminStatus : Array[TFhirMedicationAdminStatus] of String = ('', 'in-progress', 'on-hold', 'completed', 'entered-in-error', 'stopped');
  CODES_TFhirGuideDependencyType : Array[TFhirGuideDependencyType] of String = ('', 'reference', 'inclusion');
  CODES_TFhirGuideResourcePurpose : Array[TFhirGuideResourcePurpose] of String = ('', 'example', 'terminology', 'profile', 'extension', 'dictionary', 'logical');
  CODES_TFhirGuidePageKind : Array[TFhirGuidePageKind] of String = ('', 'page', 'example', 'list', 'include', 'directory', 'dictionary', 'toc', 'resource');
  CODES_TFhirListStatus : Array[TFhirListStatus] of String = ('', 'current', 'retired', 'entered-in-error');
  CODES_TFhirLocationStatus : Array[TFhirLocationStatus] of String = ('', 'active', 'suspended', 'inactive');
  CODES_TFhirLocationMode : Array[TFhirLocationMode] of String = ('', 'instance', 'kind');
  CODES_TFhirDigitalMediaType : Array[TFhirDigitalMediaType] of String = ('', 'photo', 'video', 'audio');
  CODES_TFhirMedicationDispenseStatus : Array[TFhirMedicationDispenseStatus] of String = ('', 'in-progress', 'on-hold', 'completed', 'entered-in-error', 'stopped');
  CODES_TFhirMedicationOrderStatus : Array[TFhirMedicationOrderStatus] of String = ('', 'active', 'on-hold', 'completed', 'entered-in-error', 'stopped', 'draft');
  CODES_TFhirMedicationStatementStatus : Array[TFhirMedicationStatementStatus] of String = ('', 'active', 'completed', 'entered-in-error', 'intended');
  CODES_TFhirResponseCode : Array[TFhirResponseCode] of String = ('', 'ok', 'transient-error', 'fatal-error');
  CODES_TFhirNamingsystemType : Array[TFhirNamingsystemType] of String = ('', 'codesystem', 'identifier', 'root');
  CODES_TFhirNamingsystemIdentifierType : Array[TFhirNamingsystemIdentifierType] of String = ('', 'oid', 'uuid', 'uri', 'other');
  CODES_TFhirNutritionOrderStatus : Array[TFhirNutritionOrderStatus] of String = ('', 'proposed', 'draft', 'planned', 'requested', 'active', 'on-hold', 'completed', 'cancelled');
  CODES_TFhirObservationStatus : Array[TFhirObservationStatus] of String = ('', 'registered', 'preliminary', 'final', 'amended', 'cancelled', 'entered-in-error', 'unknown');
  CODES_TFhirObservationRelationshiptypes : Array[TFhirObservationRelationshiptypes] of String = ('', 'has-member', 'derived-from', 'sequel-to', 'replaces', 'qualified-by', 'interfered-by');
  CODES_TFhirOperationKind : Array[TFhirOperationKind] of String = ('', 'operation', 'query');
  CODES_TFhirOperationParameterUse : Array[TFhirOperationParameterUse] of String = ('', 'in', 'out');
  CODES_TFhirValuesetOperationParameterType : Array[TFhirValuesetOperationParameterType] of String = ('', 'number', 'date', 'string', 'token', 'reference', 'composite', 'quantity', 'uri', 'Address', 'Age',
  'Annotation', 'Attachment', 'BackboneElement', 'CodeableConcept', 'Coding', 'ContactPoint', 'Count', 'Distance', 'Duration', 'Element', 'ElementDefinition', 'Extension', 'HumanName', 'Identifier', 'Meta',
  'Money', 'Narrative', 'Period', 'Quantity', 'Range', 'Ratio', 'Reference', 'SampledData', 'Signature', 'SimpleQuantity', 'Timing', 'base64Binary', 'boolean', 'code', 'dateTime', 'decimal', 'id', 'instant', 'integer', 'markdown',
  'oid', 'positiveInt', 'time', 'unsignedInt', 'uuid', 'xhtml', 'Account', 'AllergyIntolerance', 'Appointment', 'AppointmentResponse', 'AuditEvent', 'Basic', 'Binary', 'BodySite', 'Bundle', 'CarePlan', 'Claim', 'ClaimResponse', 'ClinicalImpression', 'Communication', 'CommunicationRequest', 'Composition', 'ConceptMap', 'Condition', 'Conformance', 'Contract', 'Coverage', 'DataElement',
  'DetectedIssue', 'Device', 'DeviceComponent', 'DeviceMetric', 'DeviceUseRequest', 'DeviceUseStatement', 'DiagnosticOrder', 'DiagnosticReport', 'DocumentManifest', 'DocumentReference', 'DomainResource', 'EligibilityRequest', 'EligibilityResponse', 'Encounter', 'EnrollmentRequest', 'EnrollmentResponse', 'EpisodeOfCare', 'ExplanationOfBenefit', 'FamilyMemberHistory', 'Flag', 'Goal',
   'Group', 'HealthcareService', 'ImagingObjectSelection', 'ImagingStudy', 'Immunization', 'ImmunizationRecommendation', 'ImplementationGuide', 'List', 'Location', 'Media', 'Medication', 'MedicationAdministration', 'MedicationDispense', 'MedicationOrder', 'MedicationStatement', 'MessageHeader', 'NamingSystem', 'NutritionOrder', 'Observation', 'OperationDefinition', 'OperationOutcome',
   'Order', 'OrderResponse', 'Organization', 'Parameters', 'Patient', 'PaymentNotice', 'PaymentReconciliation', 'Person', 'Practitioner', 'Procedure', 'ProcedureRequest', 'ProcessRequest', 'ProcessResponse', 'Provenance', 'Questionnaire', 'QuestionnaireResponse', 'ReferralRequest', 'RelatedPerson', 'Resource', 'RiskAssessment', 'Schedule', 'SearchParameter', 'Slot', 'Specimen',
   'StructureDefinition', 'Subscription', 'Substance', 'SupplyDelivery', 'SupplyRequest', 'TestScript', 'ValueSet', 'VisionPrescription');
  CODES_TFhirIssueSeverity : Array[TFhirIssueSeverity] of String = ('', 'fatal', 'error', 'warning', 'information');
  CODES_TFhirIssueType : Array[TFhirIssueType] of String = ('', 'invalid', 'structure', 'required', 'value', 'invariant', 'security', 'login', 'unknown', 'expired', 'forbidden', 'suppressed', 'processing', 'not-supported', 'duplicate', 'not-found', 'too-long', 'code-invalid', 'extension', 'too-costly', 'business-rule', 'conflict', 'incomplete', 'transient', 'lock-error', 'no-store', 'exception', 'timeout', 'throttled', 'informational');
  CODES_TFhirOrderStatus : Array[TFhirOrderStatus] of String = ('', 'pending', 'review', 'rejected', 'error', 'accepted', 'cancelled', 'replaced', 'aborted', 'completed');
  CODES_TFhirLinkType : Array[TFhirLinkType] of String = ('', 'replace', 'refer', 'seealso');
  CODES_TFhirIdentityAssuranceLevel : Array[TFhirIdentityAssuranceLevel] of String = ('', 'level1', 'level2', 'level3', 'level4');
  CODES_TFhirProcedureStatus : Array[TFhirProcedureStatus] of String = ('', 'in-progress', 'aborted', 'completed', 'entered-in-error');
  CODES_TFhirProcedureRequestStatus : Array[TFhirProcedureRequestStatus] of String = ('', 'proposed', 'draft', 'requested', 'received', 'accepted', 'in-progress', 'completed', 'suspended', 'rejected', 'aborted');
  CODES_TFhirProcedureRequestPriority : Array[TFhirProcedureRequestPriority] of String = ('', 'routine', 'urgent', 'stat', 'asap');
  CODES_TFhirActionlist : Array[TFhirActionlist] of String = ('', 'cancel', 'poll', 'reprocess', 'status');
  CODES_TFhirProvenanceEntityRole : Array[TFhirProvenanceEntityRole] of String = ('', 'derivation', 'revision', 'quotation', 'source');
  CODES_TFhirQuestionnaireStatus : Array[TFhirQuestionnaireStatus] of String = ('', 'draft', 'published', 'retired');
  CODES_TFhirAnswerFormat : Array[TFhirAnswerFormat] of String = ('', 'boolean', 'decimal', 'integer', 'date', 'dateTime', 'instant', 'time', 'string', 'text', 'url', 'choice', 'open-choice', 'attachment', 'reference', 'quantity');
  CODES_TFhirQuestionnaireAnswersStatus : Array[TFhirQuestionnaireAnswersStatus] of String = ('', 'in-progress', 'completed', 'amended');
  CODES_TFhirReferralstatus : Array[TFhirReferralstatus] of String = ('', 'draft', 'requested', 'active', 'cancelled', 'accepted', 'rejected', 'completed');
  CODES_TFhirSearchXpathUsage : Array[TFhirSearchXpathUsage] of String = ('', 'normal', 'phonetic', 'nearby', 'distance', 'other');
  CODES_TFhirSlotstatus : Array[TFhirSlotstatus] of String = ('', 'busy', 'free', 'busy-unavailable', 'busy-tentative');
  CODES_TFhirSpecimenStatus : Array[TFhirSpecimenStatus] of String = ('', 'available', 'unavailable', 'unsatisfactory', 'entered-in-error');
  CODES_TFhirStructureDefinitionKind : Array[TFhirStructureDefinitionKind] of String = ('', 'datatype', 'resource', 'logical');
  CODES_TFhirExtensionContext : Array[TFhirExtensionContext] of String = ('', 'resource', 'datatype', 'mapping', 'extension');
  CODES_TFhirSubscriptionStatus : Array[TFhirSubscriptionStatus] of String = ('', 'requested', 'active', 'error', 'off');
  CODES_TFhirSubscriptionChannelType : Array[TFhirSubscriptionChannelType] of String = ('', 'rest-hook', 'websocket', 'email', 'sms', 'message');
  CODES_TFhirSupplydeliveryStatus : Array[TFhirSupplydeliveryStatus] of String = ('', 'in-progress', 'completed', 'abandoned');
  CODES_TFhirSupplyrequestStatus : Array[TFhirSupplyrequestStatus] of String = ('', 'requested', 'completed', 'failed', 'cancelled');
  CODES_TFhirContentType : Array[TFhirContentType] of String = ('', 'xml', 'json');
  CODES_TFhirAssertDirectionCodes : Array[TFhirAssertDirectionCodes] of String = ('', 'response', 'request');
  CODES_TFhirAssertOperatorCodes : Array[TFhirAssertOperatorCodes] of String = ('', 'equals', 'notEquals', 'in', 'notIn', 'greaterThan', 'lessThan', 'empty', 'notEmpty', 'contains', 'notContains');
  CODES_TFhirAssertResponseCodeTypes : Array[TFhirAssertResponseCodeTypes] of String = ('', 'okay', 'created', 'noContent', 'notModified', 'bad', 'forbidden', 'notFound', 'methodNotAllowed', 'conflict', 'gone', 'preconditionFailed', 'unprocessable');
  CODES_TFhirFilterOperator : Array[TFhirFilterOperator] of String = ('', '=', 'is-a', 'is-not-a', 'regex', 'in', 'not-in');
  CODES_TFhirVisionEyeCodes : Array[TFhirVisionEyeCodes] of String = ('', 'right', 'left');
  CODES_TFhirVisionBaseCodes : Array[TFhirVisionBaseCodes] of String = ('', 'up', 'down', 'in', 'out');

Function TFhirNarrativeStatusListAsInteger(aSet : TFhirNarrativeStatusList) : Integer; overload;
Function IntegerAsTFhirNarrativeStatusList(i : integer) : TFhirNarrativeStatusList; overload;
Function TFhirQuantityComparatorListAsInteger(aSet : TFhirQuantityComparatorList) : Integer; overload;
Function IntegerAsTFhirQuantityComparatorList(i : integer) : TFhirQuantityComparatorList; overload;
Function TFhirIdentifierUseListAsInteger(aSet : TFhirIdentifierUseList) : Integer; overload;
Function IntegerAsTFhirIdentifierUseList(i : integer) : TFhirIdentifierUseList; overload;
Function TFhirPropertyRepresentationListAsInteger(aSet : TFhirPropertyRepresentationList) : Integer; overload;
Function IntegerAsTFhirPropertyRepresentationList(i : integer) : TFhirPropertyRepresentationList; overload;
Function TFhirResourceSlicingRulesListAsInteger(aSet : TFhirResourceSlicingRulesList) : Integer; overload;
Function IntegerAsTFhirResourceSlicingRulesList(i : integer) : TFhirResourceSlicingRulesList; overload;
Function TFhirResourceAggregationModeListAsInteger(aSet : TFhirResourceAggregationModeList) : Integer; overload;
Function IntegerAsTFhirResourceAggregationModeList(i : integer) : TFhirResourceAggregationModeList; overload;
Function TFhirConstraintSeverityListAsInteger(aSet : TFhirConstraintSeverityList) : Integer; overload;
Function IntegerAsTFhirConstraintSeverityList(i : integer) : TFhirConstraintSeverityList; overload;
Function TFhirBindingStrengthListAsInteger(aSet : TFhirBindingStrengthList) : Integer; overload;
Function IntegerAsTFhirBindingStrengthList(i : integer) : TFhirBindingStrengthList; overload;
Function TFhirUnitsOfTimeListAsInteger(aSet : TFhirUnitsOfTimeList) : Integer; overload;
Function IntegerAsTFhirUnitsOfTimeList(i : integer) : TFhirUnitsOfTimeList; overload;
Function TFhirEventTimingListAsInteger(aSet : TFhirEventTimingList) : Integer; overload;
Function IntegerAsTFhirEventTimingList(i : integer) : TFhirEventTimingList; overload;
Function TFhirAddressUseListAsInteger(aSet : TFhirAddressUseList) : Integer; overload;
Function IntegerAsTFhirAddressUseList(i : integer) : TFhirAddressUseList; overload;
Function TFhirAddressTypeListAsInteger(aSet : TFhirAddressTypeList) : Integer; overload;
Function IntegerAsTFhirAddressTypeList(i : integer) : TFhirAddressTypeList; overload;
Function TFhirNameUseListAsInteger(aSet : TFhirNameUseList) : Integer; overload;
Function IntegerAsTFhirNameUseList(i : integer) : TFhirNameUseList; overload;
Function TFhirContactPointSystemListAsInteger(aSet : TFhirContactPointSystemList) : Integer; overload;
Function IntegerAsTFhirContactPointSystemList(i : integer) : TFhirContactPointSystemList; overload;
Function TFhirContactPointUseListAsInteger(aSet : TFhirContactPointUseList) : Integer; overload;
Function IntegerAsTFhirContactPointUseList(i : integer) : TFhirContactPointUseList; overload;
Function TFhirAccountStatusListAsInteger(aSet : TFhirAccountStatusList) : Integer; overload;
Function IntegerAsTFhirAccountStatusList(i : integer) : TFhirAccountStatusList; overload;
Function TFhirAllergyIntoleranceStatusListAsInteger(aSet : TFhirAllergyIntoleranceStatusList) : Integer; overload;
Function IntegerAsTFhirAllergyIntoleranceStatusList(i : integer) : TFhirAllergyIntoleranceStatusList; overload;
Function TFhirAllergyIntoleranceCriticalityListAsInteger(aSet : TFhirAllergyIntoleranceCriticalityList) : Integer; overload;
Function IntegerAsTFhirAllergyIntoleranceCriticalityList(i : integer) : TFhirAllergyIntoleranceCriticalityList; overload;
Function TFhirAllergyIntoleranceTypeListAsInteger(aSet : TFhirAllergyIntoleranceTypeList) : Integer; overload;
Function IntegerAsTFhirAllergyIntoleranceTypeList(i : integer) : TFhirAllergyIntoleranceTypeList; overload;
Function TFhirAllergyIntoleranceCategoryListAsInteger(aSet : TFhirAllergyIntoleranceCategoryList) : Integer; overload;
Function IntegerAsTFhirAllergyIntoleranceCategoryList(i : integer) : TFhirAllergyIntoleranceCategoryList; overload;
Function TFhirReactionEventCertaintyListAsInteger(aSet : TFhirReactionEventCertaintyList) : Integer; overload;
Function IntegerAsTFhirReactionEventCertaintyList(i : integer) : TFhirReactionEventCertaintyList; overload;
Function TFhirReactionEventSeverityListAsInteger(aSet : TFhirReactionEventSeverityList) : Integer; overload;
Function IntegerAsTFhirReactionEventSeverityList(i : integer) : TFhirReactionEventSeverityList; overload;
Function TFhirAppointmentstatusListAsInteger(aSet : TFhirAppointmentstatusList) : Integer; overload;
Function IntegerAsTFhirAppointmentstatusList(i : integer) : TFhirAppointmentstatusList; overload;
Function TFhirParticipantrequiredListAsInteger(aSet : TFhirParticipantrequiredList) : Integer; overload;
Function IntegerAsTFhirParticipantrequiredList(i : integer) : TFhirParticipantrequiredList; overload;
Function TFhirParticipationstatusListAsInteger(aSet : TFhirParticipationstatusList) : Integer; overload;
Function IntegerAsTFhirParticipationstatusList(i : integer) : TFhirParticipationstatusList; overload;
Function TFhirParticipantstatusListAsInteger(aSet : TFhirParticipantstatusList) : Integer; overload;
Function IntegerAsTFhirParticipantstatusList(i : integer) : TFhirParticipantstatusList; overload;
Function TFhirAuditEventActionListAsInteger(aSet : TFhirAuditEventActionList) : Integer; overload;
Function IntegerAsTFhirAuditEventActionList(i : integer) : TFhirAuditEventActionList; overload;
Function TFhirAuditEventOutcomeListAsInteger(aSet : TFhirAuditEventOutcomeList) : Integer; overload;
Function IntegerAsTFhirAuditEventOutcomeList(i : integer) : TFhirAuditEventOutcomeList; overload;
Function TFhirNetworkTypeListAsInteger(aSet : TFhirNetworkTypeList) : Integer; overload;
Function IntegerAsTFhirNetworkTypeList(i : integer) : TFhirNetworkTypeList; overload;
Function TFhirBundleTypeListAsInteger(aSet : TFhirBundleTypeList) : Integer; overload;
Function IntegerAsTFhirBundleTypeList(i : integer) : TFhirBundleTypeList; overload;
Function TFhirSearchEntryModeListAsInteger(aSet : TFhirSearchEntryModeList) : Integer; overload;
Function IntegerAsTFhirSearchEntryModeList(i : integer) : TFhirSearchEntryModeList; overload;
Function TFhirHttpVerbListAsInteger(aSet : TFhirHttpVerbList) : Integer; overload;
Function IntegerAsTFhirHttpVerbList(i : integer) : TFhirHttpVerbList; overload;
Function TFhirCarePlanStatusListAsInteger(aSet : TFhirCarePlanStatusList) : Integer; overload;
Function IntegerAsTFhirCarePlanStatusList(i : integer) : TFhirCarePlanStatusList; overload;
Function TFhirCarePlanRelationshipListAsInteger(aSet : TFhirCarePlanRelationshipList) : Integer; overload;
Function IntegerAsTFhirCarePlanRelationshipList(i : integer) : TFhirCarePlanRelationshipList; overload;
Function TFhirCarePlanActivityStatusListAsInteger(aSet : TFhirCarePlanActivityStatusList) : Integer; overload;
Function IntegerAsTFhirCarePlanActivityStatusList(i : integer) : TFhirCarePlanActivityStatusList; overload;
Function TFhirClaimTypeLinkListAsInteger(aSet : TFhirClaimTypeLinkList) : Integer; overload;
Function IntegerAsTFhirClaimTypeLinkList(i : integer) : TFhirClaimTypeLinkList; overload;
Function TFhirClaimUseLinkListAsInteger(aSet : TFhirClaimUseLinkList) : Integer; overload;
Function IntegerAsTFhirClaimUseLinkList(i : integer) : TFhirClaimUseLinkList; overload;
Function TFhirRemittanceOutcomeListAsInteger(aSet : TFhirRemittanceOutcomeList) : Integer; overload;
Function IntegerAsTFhirRemittanceOutcomeList(i : integer) : TFhirRemittanceOutcomeList; overload;
Function TFhirClinicalImpressionStatusListAsInteger(aSet : TFhirClinicalImpressionStatusList) : Integer; overload;
Function IntegerAsTFhirClinicalImpressionStatusList(i : integer) : TFhirClinicalImpressionStatusList; overload;
Function TFhirCommunicationStatusListAsInteger(aSet : TFhirCommunicationStatusList) : Integer; overload;
Function IntegerAsTFhirCommunicationStatusList(i : integer) : TFhirCommunicationStatusList; overload;
Function TFhirCommunicationRequestStatusListAsInteger(aSet : TFhirCommunicationRequestStatusList) : Integer; overload;
Function IntegerAsTFhirCommunicationRequestStatusList(i : integer) : TFhirCommunicationRequestStatusList; overload;
Function TFhirCompositionStatusListAsInteger(aSet : TFhirCompositionStatusList) : Integer; overload;
Function IntegerAsTFhirCompositionStatusList(i : integer) : TFhirCompositionStatusList; overload;
Function TFhirV3ConfidentialityListAsInteger(aSet : TFhirV3ConfidentialityList) : Integer; overload;
Function IntegerAsTFhirV3ConfidentialityList(i : integer) : TFhirV3ConfidentialityList; overload;
Function TFhirCompositionAttestationModeListAsInteger(aSet : TFhirCompositionAttestationModeList) : Integer; overload;
Function IntegerAsTFhirCompositionAttestationModeList(i : integer) : TFhirCompositionAttestationModeList; overload;
Function TFhirListModeListAsInteger(aSet : TFhirListModeList) : Integer; overload;
Function IntegerAsTFhirListModeList(i : integer) : TFhirListModeList; overload;
Function TFhirConformanceResourceStatusListAsInteger(aSet : TFhirConformanceResourceStatusList) : Integer; overload;
Function IntegerAsTFhirConformanceResourceStatusList(i : integer) : TFhirConformanceResourceStatusList; overload;
Function TFhirConceptMapEquivalenceListAsInteger(aSet : TFhirConceptMapEquivalenceList) : Integer; overload;
Function IntegerAsTFhirConceptMapEquivalenceList(i : integer) : TFhirConceptMapEquivalenceList; overload;
Function TFhirValuesetConditionClinicalListAsInteger(aSet : TFhirValuesetConditionClinicalList) : Integer; overload;
Function IntegerAsTFhirValuesetConditionClinicalList(i : integer) : TFhirValuesetConditionClinicalList; overload;
Function TFhirConditionVerStatusListAsInteger(aSet : TFhirConditionVerStatusList) : Integer; overload;
Function IntegerAsTFhirConditionVerStatusList(i : integer) : TFhirConditionVerStatusList; overload;
Function TFhirConformanceStatementKindListAsInteger(aSet : TFhirConformanceStatementKindList) : Integer; overload;
Function IntegerAsTFhirConformanceStatementKindList(i : integer) : TFhirConformanceStatementKindList; overload;
Function TFhirUnknownContentCodeListAsInteger(aSet : TFhirUnknownContentCodeList) : Integer; overload;
Function IntegerAsTFhirUnknownContentCodeList(i : integer) : TFhirUnknownContentCodeList; overload;
Function TFhirRestfulConformanceModeListAsInteger(aSet : TFhirRestfulConformanceModeList) : Integer; overload;
Function IntegerAsTFhirRestfulConformanceModeList(i : integer) : TFhirRestfulConformanceModeList; overload;
Function TFhirTypeRestfulInteractionListAsInteger(aSet : TFhirTypeRestfulInteractionList) : Integer; overload;
Function IntegerAsTFhirTypeRestfulInteractionList(i : integer) : TFhirTypeRestfulInteractionList; overload;
Function TFhirVersioningPolicyListAsInteger(aSet : TFhirVersioningPolicyList) : Integer; overload;
Function IntegerAsTFhirVersioningPolicyList(i : integer) : TFhirVersioningPolicyList; overload;
Function TFhirConditionalDeleteStatusListAsInteger(aSet : TFhirConditionalDeleteStatusList) : Integer; overload;
Function IntegerAsTFhirConditionalDeleteStatusList(i : integer) : TFhirConditionalDeleteStatusList; overload;
Function TFhirSearchParamTypeListAsInteger(aSet : TFhirSearchParamTypeList) : Integer; overload;
Function IntegerAsTFhirSearchParamTypeList(i : integer) : TFhirSearchParamTypeList; overload;
Function TFhirSearchModifierCodeListAsInteger(aSet : TFhirSearchModifierCodeList) : Integer; overload;
Function IntegerAsTFhirSearchModifierCodeList(i : integer) : TFhirSearchModifierCodeList; overload;
Function TFhirSystemRestfulInteractionListAsInteger(aSet : TFhirSystemRestfulInteractionList) : Integer; overload;
Function IntegerAsTFhirSystemRestfulInteractionList(i : integer) : TFhirSystemRestfulInteractionList; overload;
Function TFhirTransactionModeListAsInteger(aSet : TFhirTransactionModeList) : Integer; overload;
Function IntegerAsTFhirTransactionModeList(i : integer) : TFhirTransactionModeList; overload;
Function TFhirMessageSignificanceCategoryListAsInteger(aSet : TFhirMessageSignificanceCategoryList) : Integer; overload;
Function IntegerAsTFhirMessageSignificanceCategoryList(i : integer) : TFhirMessageSignificanceCategoryList; overload;
Function TFhirMessageConformanceEventModeListAsInteger(aSet : TFhirMessageConformanceEventModeList) : Integer; overload;
Function IntegerAsTFhirMessageConformanceEventModeList(i : integer) : TFhirMessageConformanceEventModeList; overload;
Function TFhirDocumentModeListAsInteger(aSet : TFhirDocumentModeList) : Integer; overload;
Function IntegerAsTFhirDocumentModeList(i : integer) : TFhirDocumentModeList; overload;
Function TFhirDataelementStringencyListAsInteger(aSet : TFhirDataelementStringencyList) : Integer; overload;
Function IntegerAsTFhirDataelementStringencyList(i : integer) : TFhirDataelementStringencyList; overload;
Function TFhirDetectedissueSeverityListAsInteger(aSet : TFhirDetectedissueSeverityList) : Integer; overload;
Function IntegerAsTFhirDetectedissueSeverityList(i : integer) : TFhirDetectedissueSeverityList; overload;
Function TFhirDevicestatusListAsInteger(aSet : TFhirDevicestatusList) : Integer; overload;
Function IntegerAsTFhirDevicestatusList(i : integer) : TFhirDevicestatusList; overload;
Function TFhirMeasurementPrincipleListAsInteger(aSet : TFhirMeasurementPrincipleList) : Integer; overload;
Function IntegerAsTFhirMeasurementPrincipleList(i : integer) : TFhirMeasurementPrincipleList; overload;
Function TFhirMetricOperationalStatusListAsInteger(aSet : TFhirMetricOperationalStatusList) : Integer; overload;
Function IntegerAsTFhirMetricOperationalStatusList(i : integer) : TFhirMetricOperationalStatusList; overload;
Function TFhirMetricColorListAsInteger(aSet : TFhirMetricColorList) : Integer; overload;
Function IntegerAsTFhirMetricColorList(i : integer) : TFhirMetricColorList; overload;
Function TFhirMetricCategoryListAsInteger(aSet : TFhirMetricCategoryList) : Integer; overload;
Function IntegerAsTFhirMetricCategoryList(i : integer) : TFhirMetricCategoryList; overload;
Function TFhirMetricCalibrationTypeListAsInteger(aSet : TFhirMetricCalibrationTypeList) : Integer; overload;
Function IntegerAsTFhirMetricCalibrationTypeList(i : integer) : TFhirMetricCalibrationTypeList; overload;
Function TFhirMetricCalibrationStateListAsInteger(aSet : TFhirMetricCalibrationStateList) : Integer; overload;
Function IntegerAsTFhirMetricCalibrationStateList(i : integer) : TFhirMetricCalibrationStateList; overload;
Function TFhirDeviceUseRequestStatusListAsInteger(aSet : TFhirDeviceUseRequestStatusList) : Integer; overload;
Function IntegerAsTFhirDeviceUseRequestStatusList(i : integer) : TFhirDeviceUseRequestStatusList; overload;
Function TFhirDeviceUseRequestPriorityListAsInteger(aSet : TFhirDeviceUseRequestPriorityList) : Integer; overload;
Function IntegerAsTFhirDeviceUseRequestPriorityList(i : integer) : TFhirDeviceUseRequestPriorityList; overload;
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
Function TFhirEncounterLocationStatusListAsInteger(aSet : TFhirEncounterLocationStatusList) : Integer; overload;
Function IntegerAsTFhirEncounterLocationStatusList(i : integer) : TFhirEncounterLocationStatusList; overload;
Function TFhirEpisodeOfCareStatusListAsInteger(aSet : TFhirEpisodeOfCareStatusList) : Integer; overload;
Function IntegerAsTFhirEpisodeOfCareStatusList(i : integer) : TFhirEpisodeOfCareStatusList; overload;
Function TFhirHistoryStatusListAsInteger(aSet : TFhirHistoryStatusList) : Integer; overload;
Function IntegerAsTFhirHistoryStatusList(i : integer) : TFhirHistoryStatusList; overload;
Function TFhirAdministrativeGenderListAsInteger(aSet : TFhirAdministrativeGenderList) : Integer; overload;
Function IntegerAsTFhirAdministrativeGenderList(i : integer) : TFhirAdministrativeGenderList; overload;
Function TFhirFlagStatusListAsInteger(aSet : TFhirFlagStatusList) : Integer; overload;
Function IntegerAsTFhirFlagStatusList(i : integer) : TFhirFlagStatusList; overload;
Function TFhirGoalStatusListAsInteger(aSet : TFhirGoalStatusList) : Integer; overload;
Function IntegerAsTFhirGoalStatusList(i : integer) : TFhirGoalStatusList; overload;
Function TFhirGroupTypeListAsInteger(aSet : TFhirGroupTypeList) : Integer; overload;
Function IntegerAsTFhirGroupTypeList(i : integer) : TFhirGroupTypeList; overload;
Function TFhirDaysOfWeekListAsInteger(aSet : TFhirDaysOfWeekList) : Integer; overload;
Function IntegerAsTFhirDaysOfWeekList(i : integer) : TFhirDaysOfWeekList; overload;
Function TFhirInstanceAvailabilityListAsInteger(aSet : TFhirInstanceAvailabilityList) : Integer; overload;
Function IntegerAsTFhirInstanceAvailabilityList(i : integer) : TFhirInstanceAvailabilityList; overload;
Function TFhirMedicationAdminStatusListAsInteger(aSet : TFhirMedicationAdminStatusList) : Integer; overload;
Function IntegerAsTFhirMedicationAdminStatusList(i : integer) : TFhirMedicationAdminStatusList; overload;
Function TFhirGuideDependencyTypeListAsInteger(aSet : TFhirGuideDependencyTypeList) : Integer; overload;
Function IntegerAsTFhirGuideDependencyTypeList(i : integer) : TFhirGuideDependencyTypeList; overload;
Function TFhirGuideResourcePurposeListAsInteger(aSet : TFhirGuideResourcePurposeList) : Integer; overload;
Function IntegerAsTFhirGuideResourcePurposeList(i : integer) : TFhirGuideResourcePurposeList; overload;
Function TFhirGuidePageKindListAsInteger(aSet : TFhirGuidePageKindList) : Integer; overload;
Function IntegerAsTFhirGuidePageKindList(i : integer) : TFhirGuidePageKindList; overload;
Function TFhirListStatusListAsInteger(aSet : TFhirListStatusList) : Integer; overload;
Function IntegerAsTFhirListStatusList(i : integer) : TFhirListStatusList; overload;
Function TFhirLocationStatusListAsInteger(aSet : TFhirLocationStatusList) : Integer; overload;
Function IntegerAsTFhirLocationStatusList(i : integer) : TFhirLocationStatusList; overload;
Function TFhirLocationModeListAsInteger(aSet : TFhirLocationModeList) : Integer; overload;
Function IntegerAsTFhirLocationModeList(i : integer) : TFhirLocationModeList; overload;
Function TFhirDigitalMediaTypeListAsInteger(aSet : TFhirDigitalMediaTypeList) : Integer; overload;
Function IntegerAsTFhirDigitalMediaTypeList(i : integer) : TFhirDigitalMediaTypeList; overload;
Function TFhirMedicationDispenseStatusListAsInteger(aSet : TFhirMedicationDispenseStatusList) : Integer; overload;
Function IntegerAsTFhirMedicationDispenseStatusList(i : integer) : TFhirMedicationDispenseStatusList; overload;
Function TFhirMedicationOrderStatusListAsInteger(aSet : TFhirMedicationOrderStatusList) : Integer; overload;
Function IntegerAsTFhirMedicationOrderStatusList(i : integer) : TFhirMedicationOrderStatusList; overload;
Function TFhirMedicationStatementStatusListAsInteger(aSet : TFhirMedicationStatementStatusList) : Integer; overload;
Function IntegerAsTFhirMedicationStatementStatusList(i : integer) : TFhirMedicationStatementStatusList; overload;
Function TFhirResponseCodeListAsInteger(aSet : TFhirResponseCodeList) : Integer; overload;
Function IntegerAsTFhirResponseCodeList(i : integer) : TFhirResponseCodeList; overload;
Function TFhirNamingsystemTypeListAsInteger(aSet : TFhirNamingsystemTypeList) : Integer; overload;
Function IntegerAsTFhirNamingsystemTypeList(i : integer) : TFhirNamingsystemTypeList; overload;
Function TFhirNamingsystemIdentifierTypeListAsInteger(aSet : TFhirNamingsystemIdentifierTypeList) : Integer; overload;
Function IntegerAsTFhirNamingsystemIdentifierTypeList(i : integer) : TFhirNamingsystemIdentifierTypeList; overload;
Function TFhirNutritionOrderStatusListAsInteger(aSet : TFhirNutritionOrderStatusList) : Integer; overload;
Function IntegerAsTFhirNutritionOrderStatusList(i : integer) : TFhirNutritionOrderStatusList; overload;
Function TFhirObservationStatusListAsInteger(aSet : TFhirObservationStatusList) : Integer; overload;
Function IntegerAsTFhirObservationStatusList(i : integer) : TFhirObservationStatusList; overload;
Function TFhirObservationRelationshiptypesListAsInteger(aSet : TFhirObservationRelationshiptypesList) : Integer; overload;
Function IntegerAsTFhirObservationRelationshiptypesList(i : integer) : TFhirObservationRelationshiptypesList; overload;
Function TFhirOperationKindListAsInteger(aSet : TFhirOperationKindList) : Integer; overload;
Function IntegerAsTFhirOperationKindList(i : integer) : TFhirOperationKindList; overload;
Function TFhirOperationParameterUseListAsInteger(aSet : TFhirOperationParameterUseList) : Integer; overload;
Function IntegerAsTFhirOperationParameterUseList(i : integer) : TFhirOperationParameterUseList; overload;
Function TFhirValuesetOperationParameterTypeListAsInteger(aSet : TFhirValuesetOperationParameterTypeList) : Integer; overload;
Function IntegerAsTFhirValuesetOperationParameterTypeList(i : integer) : TFhirValuesetOperationParameterTypeList; overload;
Function TFhirIssueSeverityListAsInteger(aSet : TFhirIssueSeverityList) : Integer; overload;
Function IntegerAsTFhirIssueSeverityList(i : integer) : TFhirIssueSeverityList; overload;
Function TFhirIssueTypeListAsInteger(aSet : TFhirIssueTypeList) : Integer; overload;
Function IntegerAsTFhirIssueTypeList(i : integer) : TFhirIssueTypeList; overload;
Function TFhirOrderStatusListAsInteger(aSet : TFhirOrderStatusList) : Integer; overload;
Function IntegerAsTFhirOrderStatusList(i : integer) : TFhirOrderStatusList; overload;
Function TFhirLinkTypeListAsInteger(aSet : TFhirLinkTypeList) : Integer; overload;
Function IntegerAsTFhirLinkTypeList(i : integer) : TFhirLinkTypeList; overload;
Function TFhirIdentityAssuranceLevelListAsInteger(aSet : TFhirIdentityAssuranceLevelList) : Integer; overload;
Function IntegerAsTFhirIdentityAssuranceLevelList(i : integer) : TFhirIdentityAssuranceLevelList; overload;
Function TFhirProcedureStatusListAsInteger(aSet : TFhirProcedureStatusList) : Integer; overload;
Function IntegerAsTFhirProcedureStatusList(i : integer) : TFhirProcedureStatusList; overload;
Function TFhirProcedureRequestStatusListAsInteger(aSet : TFhirProcedureRequestStatusList) : Integer; overload;
Function IntegerAsTFhirProcedureRequestStatusList(i : integer) : TFhirProcedureRequestStatusList; overload;
Function TFhirProcedureRequestPriorityListAsInteger(aSet : TFhirProcedureRequestPriorityList) : Integer; overload;
Function IntegerAsTFhirProcedureRequestPriorityList(i : integer) : TFhirProcedureRequestPriorityList; overload;
Function TFhirActionlistListAsInteger(aSet : TFhirActionlistList) : Integer; overload;
Function IntegerAsTFhirActionlistList(i : integer) : TFhirActionlistList; overload;
Function TFhirProvenanceEntityRoleListAsInteger(aSet : TFhirProvenanceEntityRoleList) : Integer; overload;
Function IntegerAsTFhirProvenanceEntityRoleList(i : integer) : TFhirProvenanceEntityRoleList; overload;
Function TFhirQuestionnaireStatusListAsInteger(aSet : TFhirQuestionnaireStatusList) : Integer; overload;
Function IntegerAsTFhirQuestionnaireStatusList(i : integer) : TFhirQuestionnaireStatusList; overload;
Function TFhirAnswerFormatListAsInteger(aSet : TFhirAnswerFormatList) : Integer; overload;
Function IntegerAsTFhirAnswerFormatList(i : integer) : TFhirAnswerFormatList; overload;
Function TFhirQuestionnaireAnswersStatusListAsInteger(aSet : TFhirQuestionnaireAnswersStatusList) : Integer; overload;
Function IntegerAsTFhirQuestionnaireAnswersStatusList(i : integer) : TFhirQuestionnaireAnswersStatusList; overload;
Function TFhirReferralstatusListAsInteger(aSet : TFhirReferralstatusList) : Integer; overload;
Function IntegerAsTFhirReferralstatusList(i : integer) : TFhirReferralstatusList; overload;
Function TFhirSearchXpathUsageListAsInteger(aSet : TFhirSearchXpathUsageList) : Integer; overload;
Function IntegerAsTFhirSearchXpathUsageList(i : integer) : TFhirSearchXpathUsageList; overload;
Function TFhirSlotstatusListAsInteger(aSet : TFhirSlotstatusList) : Integer; overload;
Function IntegerAsTFhirSlotstatusList(i : integer) : TFhirSlotstatusList; overload;
Function TFhirSpecimenStatusListAsInteger(aSet : TFhirSpecimenStatusList) : Integer; overload;
Function IntegerAsTFhirSpecimenStatusList(i : integer) : TFhirSpecimenStatusList; overload;
Function TFhirStructureDefinitionKindListAsInteger(aSet : TFhirStructureDefinitionKindList) : Integer; overload;
Function IntegerAsTFhirStructureDefinitionKindList(i : integer) : TFhirStructureDefinitionKindList; overload;
Function TFhirExtensionContextListAsInteger(aSet : TFhirExtensionContextList) : Integer; overload;
Function IntegerAsTFhirExtensionContextList(i : integer) : TFhirExtensionContextList; overload;
Function TFhirSubscriptionStatusListAsInteger(aSet : TFhirSubscriptionStatusList) : Integer; overload;
Function IntegerAsTFhirSubscriptionStatusList(i : integer) : TFhirSubscriptionStatusList; overload;
Function TFhirSubscriptionChannelTypeListAsInteger(aSet : TFhirSubscriptionChannelTypeList) : Integer; overload;
Function IntegerAsTFhirSubscriptionChannelTypeList(i : integer) : TFhirSubscriptionChannelTypeList; overload;
Function TFhirSupplydeliveryStatusListAsInteger(aSet : TFhirSupplydeliveryStatusList) : Integer; overload;
Function IntegerAsTFhirSupplydeliveryStatusList(i : integer) : TFhirSupplydeliveryStatusList; overload;
Function TFhirSupplyrequestStatusListAsInteger(aSet : TFhirSupplyrequestStatusList) : Integer; overload;
Function IntegerAsTFhirSupplyrequestStatusList(i : integer) : TFhirSupplyrequestStatusList; overload;
Function TFhirContentTypeListAsInteger(aSet : TFhirContentTypeList) : Integer; overload;
Function IntegerAsTFhirContentTypeList(i : integer) : TFhirContentTypeList; overload;
Function TFhirAssertDirectionCodesListAsInteger(aSet : TFhirAssertDirectionCodesList) : Integer; overload;
Function IntegerAsTFhirAssertDirectionCodesList(i : integer) : TFhirAssertDirectionCodesList; overload;
Function TFhirAssertOperatorCodesListAsInteger(aSet : TFhirAssertOperatorCodesList) : Integer; overload;
Function IntegerAsTFhirAssertOperatorCodesList(i : integer) : TFhirAssertOperatorCodesList; overload;
Function TFhirAssertResponseCodeTypesListAsInteger(aSet : TFhirAssertResponseCodeTypesList) : Integer; overload;
Function IntegerAsTFhirAssertResponseCodeTypesList(i : integer) : TFhirAssertResponseCodeTypesList; overload;
Function TFhirFilterOperatorListAsInteger(aSet : TFhirFilterOperatorList) : Integer; overload;
Function IntegerAsTFhirFilterOperatorList(i : integer) : TFhirFilterOperatorList; overload;
Function TFhirVisionEyeCodesListAsInteger(aSet : TFhirVisionEyeCodesList) : Integer; overload;
Function IntegerAsTFhirVisionEyeCodesList(i : integer) : TFhirVisionEyeCodesList; overload;
Function TFhirVisionBaseCodesListAsInteger(aSet : TFhirVisionBaseCodesList) : Integer; overload;
Function IntegerAsTFhirVisionBaseCodesList(i : integer) : TFhirVisionBaseCodesList; overload;

implementation

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

procedure TFhirElement.Assign(oSource : TAdvObject);
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
    FExtensionList := TFhirExtensionList.Create;
    FExtensionList.Assign(TFhirElement(oSource).FExtensionList);
  end;
end;

procedure TFhirElement.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'id') Then
     list.add(FId.Link);
  if (child_name = 'extension') Then
    list.addAll(FExtensionList);
end;

procedure TFhirElement.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'id', 'id', FId.Link));{2}
  oList.add(TFHIRProperty.create(self, 'extension', 'Extension', FExtensionList.Link)){3};
end;

procedure TFhirElement.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'id') then IdElement := propValue as TFhirId{5a}
  else if (propName = 'extension') then ExtensionList.add(propValue as TFhirExtension){2}
  else inherited;
end;

function TFhirElement.FhirType : string;
begin
  result := 'Element';
end;

function TFhirElement.Link : TFhirElement;
begin
  result := TFhirElement(inherited Link);
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

Destructor TFhirElementListEnumerator.Destroy;
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

procedure TFhirBackboneElement.Assign(oSource : TAdvObject);
begin
  inherited;
  if (TFhirBackboneElement(oSource).FModifierExtensionList = nil) then
  begin
    FModifierExtensionList.free;
    FModifierExtensionList := nil;
  end
  else
  begin
    FModifierExtensionList := TFhirExtensionList.Create;
  FModifierExtensionList.Assign(TFhirBackboneElement(oSource).FModifierExtensionList);
end;
end;

procedure TFhirBackboneElement.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'modifierExtension') Then
    list.addAll(FModifierExtensionList);
end;

procedure TFhirBackboneElement.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'modifierExtension', 'Extension', FModifierExtensionList.Link)){3};
end;

procedure TFhirBackboneElement.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'modifierExtension') then ModifierExtensionList.add(propValue as TFhirExtension){2}
  else inherited;
end;

function TFhirBackboneElement.FhirType : string;
begin
  result := 'BackboneElement';
end;

function TFhirBackboneElement.Link : TFhirBackboneElement;
begin
  result := TFhirBackboneElement(inherited Link);
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


{ TFhirBackboneElementListEnumerator }

Constructor TFhirBackboneElementListEnumerator.Create(list : TFhirBackboneElementList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirBackboneElementListEnumerator.Destroy;
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

function TFHIRPrimitiveType.GetStringValue : string;
begin
  if self = nil then
    result := ''
  else
    result := AsStringValue;
end;

function TFHIRPrimitiveType.AsStringValue : string;
begin
  raise Exception.create('need to override '+ClassName+'.AsStringValue');
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

function TFhirEnum.AsStringValue : string;
begin
  result := FValue;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

function TFhirInteger.AsStringValue : string;
begin
  result := FValue;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

{ TFhirDateTime }

Constructor TFhirDateTime.Create(value : TDateAndTime);
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

function TFhirDateTime.AsStringValue : string;
begin
  result := FValue.toString;
end;

function TFhirDateTime.Link : TFhirDateTime;
begin
  result := TFhirDateTime(inherited Link);
end;

function TFhirDateTime.Clone : TFhirDateTime;
begin
  result := TFhirDateTime(inherited Clone);
end;

procedure TFhirDateTime.setValue(value : TDateAndTime);
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
  inc(FIndex);
  Result := FIndex < FList.count;
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


procedure TFhirDateTimeList.AddItem(value: TDateAndTime);
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

Constructor TFhirDate.Create(value : TDateAndTime);
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

function TFhirDate.AsStringValue : string;
begin
  result := FValue.toString;
end;

function TFhirDate.Link : TFhirDate;
begin
  result := TFhirDate(inherited Link);
end;

function TFhirDate.Clone : TFhirDate;
begin
  result := TFhirDate(inherited Clone);
end;

procedure TFhirDate.setValue(value : TDateAndTime);
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
  inc(FIndex);
  Result := FIndex < FList.count;
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


procedure TFhirDateList.AddItem(value: TDateAndTime);
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

function TFhirDecimal.AsStringValue : string;
begin
  result := FValue;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

function TFhirUri.AsStringValue : string;
begin
  result := FValue;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

{ TFhirBase64Binary }

Constructor TFhirBase64Binary.Create(value : TBytes);
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

function TFhirBase64Binary.AsStringValue : string;
begin
  if (length(FValue) = 0) then result := '' else result := EncodeBase64(@FValue[0], length(FValue));
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

Destructor TFhirTime.Destroy;
begin
  inherited;
end;

function TFhirTime.FhirType : string;
begin
  result := 'time';
end;

procedure TFhirTime.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if child_name = 'value' then
    list.add(TFHIRObjectText.create(value));
end;

procedure TFhirTime.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'time', FValue));
end;

procedure TFhirTime.Assign(oSource : TAdvObject);
begin
  inherited;
  FValue := TFhirTime(oSource).Value;
end;

function TFhirTime.AsStringValue : string;
begin
  result := FValue;
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


{ TFhirTimeListEnumerator }

Constructor TFhirTimeListEnumerator.Create(list : TFhirTimeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirTimeListEnumerator.Destroy;
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

function TFhirString.AsStringValue : string;
begin
  result := FValue;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

function TFhirBoolean.AsStringValue : string;
begin
  result := LCBooleanToString(FValue);
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

{ TFhirInstant }

Constructor TFhirInstant.Create(value : TDateAndTime);
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

function TFhirInstant.AsStringValue : string;
begin
  result := FValue.toString;
end;

function TFhirInstant.Link : TFhirInstant;
begin
  result := TFhirInstant(inherited Link);
end;

function TFhirInstant.Clone : TFhirInstant;
begin
  result := TFhirInstant(inherited Clone);
end;

procedure TFhirInstant.setValue(value : TDateAndTime);
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
  inc(FIndex);
  Result := FIndex < FList.count;
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


procedure TFhirInstantList.AddItem(value: TDateAndTime);
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

{ TFhirMarkdown }

Constructor TFhirMarkdown.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirMarkdown.Destroy;
begin
  inherited;
end;

function TFhirMarkdown.FhirType : string;
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


{ TFhirMarkdownListEnumerator }

Constructor TFhirMarkdownListEnumerator.Create(list : TFhirMarkdownList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirMarkdownListEnumerator.Destroy;
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

Destructor TFhirUnsignedInt.Destroy;
begin
  inherited;
end;

function TFhirUnsignedInt.FhirType : string;
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


{ TFhirUnsignedIntListEnumerator }

Constructor TFhirUnsignedIntListEnumerator.Create(list : TFhirUnsignedIntList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirUnsignedIntListEnumerator.Destroy;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

{ TFhirPositiveInt }

Constructor TFhirPositiveInt.Create(value : String);
begin
  Create;
  FValue := value;
end;

Destructor TFhirPositiveInt.Destroy;
begin
  inherited;
end;

function TFhirPositiveInt.FhirType : string;
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


{ TFhirPositiveIntListEnumerator }

Constructor TFhirPositiveIntListEnumerator.Create(list : TFhirPositiveIntList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirPositiveIntListEnumerator.Destroy;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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
  urlElement := TFhirExtension(oSource).urlElement.Clone;
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
  if (propName = 'url') then UrlElement := propValue as TFhirUri{5a}
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
  inc(FIndex);
  Result := FIndex < FList.count;
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
  if (propName = 'status') then StatusElement := propValue as TFHIREnum
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
    StatusElement := nil
  else
    StatusElement := TFhirEnum.create(CODES_TFhirNarrativeStatus[value]);
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
  inc(FIndex);
  Result := FIndex < FList.count;
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
  startElement := TFhirPeriod(oSource).startElement.Clone;
  end_Element := TFhirPeriod(oSource).end_Element.Clone;
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
  if (propName = 'start') then StartElement := propValue as TFhirDateTime{5a}
  else if (propName = 'end') then End_Element := propValue as TFhirDateTime{5a}
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

Function TFhirPeriod.GetStartST : TDateAndTime;
begin
  if FStart = nil then
    result := nil
  else
    result := FStart.value;
end;

Procedure TFhirPeriod.SetStartST(value : TDateAndTime);
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

Function TFhirPeriod.GetEnd_ST : TDateAndTime;
begin
  if FEnd_ = nil then
    result := nil
  else
    result := FEnd_.value;
end;

Procedure TFhirPeriod.SetEnd_ST(value : TDateAndTime);
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

procedure TFhirCoding.Assign(oSource : TAdvObject);
begin
  inherited;
  systemElement := TFhirCoding(oSource).systemElement.Clone;
  versionElement := TFhirCoding(oSource).versionElement.Clone;
  codeElement := TFhirCoding(oSource).codeElement.Clone;
  displayElement := TFhirCoding(oSource).displayElement.Clone;
  userSelectedElement := TFhirCoding(oSource).userSelectedElement.Clone;
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
  if (child_name = 'userSelected') Then
     list.add(FUserSelected.Link);
end;

procedure TFhirCoding.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'system', 'uri', FSystem.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'code', FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'display', 'string', FDisplay.Link));{2}
  oList.add(TFHIRProperty.create(self, 'userSelected', 'boolean', FUserSelected.Link));{2}
end;

procedure TFhirCoding.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'system') then SystemElement := propValue as TFhirUri{5a}
  else if (propName = 'version') then VersionElement := propValue as TFhirString{5a}
  else if (propName = 'code') then
    if propValue is TFHIRCode then
      CodeElement := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      CodeElement := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'display') then DisplayElement := propValue as TFhirString{5a}
  else if (propName = 'userSelected') then UserSelectedElement := propValue as TFhirBoolean{5a}
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
  inc(FIndex);
  Result := FIndex < FList.count;
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
  oList.add(TFHIRProperty.create(self, 'low', 'SimpleQuantity', FLow.Link));{2}
  oList.add(TFHIRProperty.create(self, 'high', 'SimpleQuantity', FHigh.Link));{2}
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
  inc(FIndex);
  Result := FIndex < FList.count;
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
  FUnit_.free;
  FSystem.free;
  FCode.free;
  inherited;
end;

procedure TFhirQuantity.Assign(oSource : TAdvObject);
begin
  inherited;
  valueElement := TFhirQuantity(oSource).valueElement.Clone;
  FComparator := TFhirQuantity(oSource).FComparator.Link;
  unit_Element := TFhirQuantity(oSource).unit_Element.Clone;
  systemElement := TFhirQuantity(oSource).systemElement.Clone;
  codeElement := TFhirQuantity(oSource).codeElement.Clone;
end;

procedure TFhirQuantity.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'value') Then
     list.add(FValue.Link);
  if (child_name = 'comparator') Then
     list.add(FComparator.Link);
  if (child_name = 'unit') Then
     list.add(FUnit_.Link);
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
  oList.add(TFHIRProperty.create(self, 'unit', 'string', FUnit_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'system', 'uri', FSystem.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'code', FCode.Link));{2}
end;

procedure TFhirQuantity.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'value') then ValueElement := propValue as TFhirDecimal{5a}
  else if (propName = 'comparator') then ComparatorElement := propValue as TFHIREnum
  else if (propName = 'unit') then Unit_Element := propValue as TFhirString{5a}
  else if (propName = 'system') then SystemElement := propValue as TFhirUri{5a}
  else if (propName = 'code') then
    if propValue is TFHIRCode then
      CodeElement := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      CodeElement := TFHIRCode.create(TFHIREnum(propValue).value)
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
    ComparatorElement := nil
  else
    ComparatorElement := TFhirEnum.create(CODES_TFhirQuantityComparator[value]);
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

procedure TFhirAttachment.Assign(oSource : TAdvObject);
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
  if (child_name = 'creation') Then
     list.add(FCreation.Link);
end;

procedure TFhirAttachment.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'contentType', 'code', FContentType.Link));{2}
  oList.add(TFHIRProperty.create(self, 'language', 'code', FLanguage.Link));{2}
  oList.add(TFHIRProperty.create(self, 'data', 'base64Binary', FData.Link));{2}
  oList.add(TFHIRProperty.create(self, 'url', 'uri', FUrl.Link));{2}
  oList.add(TFHIRProperty.create(self, 'size', 'unsignedInt', FSize.Link));{2}
  oList.add(TFHIRProperty.create(self, 'hash', 'base64Binary', FHash.Link));{2}
  oList.add(TFHIRProperty.create(self, 'title', 'string', FTitle.Link));{2}
  oList.add(TFHIRProperty.create(self, 'creation', 'dateTime', FCreation.Link));{2}
end;

procedure TFhirAttachment.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'contentType') then
    if propValue is TFHIRCode then
      ContentTypeElement := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      ContentTypeElement := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'language') then
    if propValue is TFHIRCode then
      LanguageElement := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      LanguageElement := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'data') then DataElement := propValue as TFhirBase64Binary{5a}
  else if (propName = 'url') then UrlElement := propValue as TFhirUri{5a}
  else if (propName = 'size') then SizeElement := propValue as TFhirUnsignedInt{5a}
  else if (propName = 'hash') then HashElement := propValue as TFhirBase64Binary{5a}
  else if (propName = 'title') then TitleElement := propValue as TFhirString{5a}
  else if (propName = 'creation') then CreationElement := propValue as TFhirDateTime{5a}
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

Function TFhirAttachment.GetCreationST : TDateAndTime;
begin
  if FCreation = nil then
    result := nil
  else
    result := FCreation.value;
end;

Procedure TFhirAttachment.SetCreationST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FCreation = nil then
      FCreation := TFhirDateTime.create;
    FCreation.value := value
  end
  else if FCreation <> nil then
    FCreation.value := nil;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

procedure TFhirAnnotation.Assign(oSource : TAdvObject);
begin
  inherited;
  author := TFhirAnnotation(oSource).author.Clone;
  timeElement := TFhirAnnotation(oSource).timeElement.Clone;
  textElement := TFhirAnnotation(oSource).textElement.Clone;
end;

procedure TFhirAnnotation.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'author[x]') Then
     list.add(FAuthor.Link);
  if (child_name = 'time') Then
     list.add(FTime.Link);
  if (child_name = 'text') Then
     list.add(FText.Link);
end;

procedure TFhirAnnotation.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'author[x]', 'Reference(Practitioner|Patient|RelatedPerson)|string', FAuthor.Link));{2}
  oList.add(TFHIRProperty.create(self, 'time', 'dateTime', FTime.Link));{2}
  oList.add(TFHIRProperty.create(self, 'text', 'string', FText.Link));{2}
end;

procedure TFhirAnnotation.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName.startsWith('author')) then Author := propValue as TFhirType{4}
  else if (propName = 'time') then TimeElement := propValue as TFhirDateTime{5a}
  else if (propName = 'text') then TextElement := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirAnnotation.FhirType : string;
begin
  result := 'Annotation';
end;

function TFhirAnnotation.Link : TFhirAnnotation;
begin
  result := TFhirAnnotation(inherited Link);
end;

function TFhirAnnotation.Clone : TFhirAnnotation;
begin
  result := TFhirAnnotation(inherited Clone);
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

Function TFhirAnnotation.GetTimeST : TDateAndTime;
begin
  if FTime = nil then
    result := nil
  else
    result := FTime.value;
end;

Procedure TFhirAnnotation.SetTimeST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FTime = nil then
      FTime := TFhirDateTime.create;
    FTime.value := value
  end
  else if FTime <> nil then
    FTime.value := nil;
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


{ TFhirAnnotationListEnumerator }

Constructor TFhirAnnotationListEnumerator.Create(list : TFhirAnnotationList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirAnnotationListEnumerator.Destroy;
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
  periodElement := TFhirSampledData(oSource).periodElement.Clone;
  factorElement := TFhirSampledData(oSource).factorElement.Clone;
  lowerLimitElement := TFhirSampledData(oSource).lowerLimitElement.Clone;
  upperLimitElement := TFhirSampledData(oSource).upperLimitElement.Clone;
  dimensionsElement := TFhirSampledData(oSource).dimensionsElement.Clone;
  dataElement := TFhirSampledData(oSource).dataElement.Clone;
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
  oList.add(TFHIRProperty.create(self, 'origin', 'SimpleQuantity', FOrigin.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'decimal', FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'factor', 'decimal', FFactor.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lowerLimit', 'decimal', FLowerLimit.Link));{2}
  oList.add(TFHIRProperty.create(self, 'upperLimit', 'decimal', FUpperLimit.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dimensions', 'positiveInt', FDimensions.Link));{2}
  oList.add(TFHIRProperty.create(self, 'data', 'string', FData.Link));{2}
end;

procedure TFhirSampledData.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'origin') then Origin := propValue as TFhirQuantity{4b}
  else if (propName = 'period') then PeriodElement := propValue as TFhirDecimal{5a}
  else if (propName = 'factor') then FactorElement := propValue as TFhirDecimal{5a}
  else if (propName = 'lowerLimit') then LowerLimitElement := propValue as TFhirDecimal{5a}
  else if (propName = 'upperLimit') then UpperLimitElement := propValue as TFhirDecimal{5a}
  else if (propName = 'dimensions') then DimensionsElement := propValue as TFhirPositiveInt{5a}
  else if (propName = 'data') then DataElement := propValue as TFhirString{5a}
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

procedure TFhirReference.Assign(oSource : TAdvObject);
begin
  inherited;
  referenceElement := TFhirReference(oSource).referenceElement.Clone;
  displayElement := TFhirReference(oSource).displayElement.Clone;
end;

procedure TFhirReference.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'reference') Then
     list.add(FReference.Link);
  if (child_name = 'display') Then
     list.add(FDisplay.Link);
end;

procedure TFhirReference.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'reference', 'string', FReference.Link));{2}
  oList.add(TFHIRProperty.create(self, 'display', 'string', FDisplay.Link));{2}
end;

procedure TFhirReference.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'reference') then ReferenceElement := propValue as TFhirString{5a}
  else if (propName = 'display') then DisplayElement := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirReference.FhirType : string;
begin
  result := 'Reference';
end;

function TFhirReference.Link : TFhirReference;
begin
  result := TFhirReference(inherited Link);
end;

function TFhirReference.Clone : TFhirReference;
begin
  result := TFhirReference(inherited Clone);
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


{ TFhirReferenceListEnumerator }

Constructor TFhirReferenceListEnumerator.Create(list : TFhirReferenceList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirReferenceListEnumerator.Destroy;
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

procedure TFhirCodeableConcept.Assign(oSource : TAdvObject);
begin
  inherited;
  if (TFhirCodeableConcept(oSource).FCodingList = nil) then
  begin
    FCodingList.free;
    FCodingList := nil;
  end
  else
  begin
    FCodingList := TFhirCodingList.Create;
  FCodingList.Assign(TFhirCodeableConcept(oSource).FCodingList);
  end;
  textElement := TFhirCodeableConcept(oSource).textElement.Clone;
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
  else if (propName = 'text') then TextElement := propValue as TFhirString{5a}
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
  inc(FIndex);
  Result := FIndex < FList.count;
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
  FType_.free;
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
  type_ := TFhirIdentifier(oSource).type_.Clone;
  systemElement := TFhirIdentifier(oSource).systemElement.Clone;
  valueElement := TFhirIdentifier(oSource).valueElement.Clone;
  period := TFhirIdentifier(oSource).period.Clone;
  assigner := TFhirIdentifier(oSource).assigner.Clone;
end;

procedure TFhirIdentifier.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'use') Then
     list.add(FUse.Link);
  if (child_name = 'type') Then
     list.add(FType_.Link);
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
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'system', 'uri', FSystem.Link));{2}
  oList.add(TFHIRProperty.create(self, 'value', 'string', FValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'assigner', 'Reference(Organization)', FAssigner.Link));{2}
end;

procedure TFhirIdentifier.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'use') then UseElement := propValue as TFHIREnum
  else if (propName = 'type') then Type_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'system') then SystemElement := propValue as TFhirUri{5a}
  else if (propName = 'value') then ValueElement := propValue as TFhirString{5a}
  else if (propName = 'period') then Period := propValue as TFhirPeriod{4b}
  else if (propName = 'assigner') then Assigner := propValue as TFhirReference{TFhirOrganization}{4b}
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
    UseElement := nil
  else
    UseElement := TFhirEnum.create(CODES_TFhirIdentifierUse[value]);
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

procedure TFhirSignature.Assign(oSource : TAdvObject);
begin
  inherited;
  if (TFhirSignature(oSource).FType_List = nil) then
begin
    FType_List.free;
    FType_List := nil;
  end
  else
  begin
    FType_List := TFhirCodingList.Create;
    FType_List.Assign(TFhirSignature(oSource).FType_List);
  end;
  whenElement := TFhirSignature(oSource).whenElement.Clone;
  who := TFhirSignature(oSource).who.Clone;
  contentTypeElement := TFhirSignature(oSource).contentTypeElement.Clone;
  blobElement := TFhirSignature(oSource).blobElement.Clone;
 end;

procedure TFhirSignature.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'type') Then
    list.addAll(FType_List);
  if (child_name = 'when') Then
     list.add(FWhen.Link);
  if (child_name = 'who[x]') Then
     list.add(FWho.Link);
  if (child_name = 'contentType') Then
     list.add(FContentType.Link);
  if (child_name = 'blob') Then
     list.add(FBlob.Link);
end;

procedure TFhirSignature.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'Coding', FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'when', 'instant', FWhen.Link));{2}
  oList.add(TFHIRProperty.create(self, 'who[x]', 'uri|Reference(Practitioner|RelatedPerson|Patient|Device|Organization)', FWho.Link));{2}
  oList.add(TFHIRProperty.create(self, 'contentType', 'code', FContentType.Link));{2}
  oList.add(TFHIRProperty.create(self, 'blob', 'base64Binary', FBlob.Link));{2}
end;

procedure TFhirSignature.setProperty(propName: string; propValue: TFHIRObject);
  begin
  if (propName = 'type') then Type_List.add(propValue as TFhirCoding){2}
  else if (propName = 'when') then WhenElement := propValue as TFhirInstant{5a}
  else if (propName.startsWith('who')) then Who := propValue as TFhirType{4}
  else if (propName = 'contentType') then
    if propValue is TFHIRCode then
      ContentTypeElement := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      ContentTypeElement := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'blob') then BlobElement := propValue as TFhirBase64Binary{5a}
  else inherited;
  end;

function TFhirSignature.FhirType : string;
begin
  result := 'Signature';
end;

function TFhirSignature.Link : TFhirSignature;
begin
  result := TFhirSignature(inherited Link);
end;

function TFhirSignature.Clone : TFhirSignature;
  begin
  result := TFhirSignature(inherited Clone);
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

Function TFhirSignature.GetWhenST : TDateAndTime;
begin
  if FWhen = nil then
    result := nil
  else
    result := FWhen.value;
end;

Procedure TFhirSignature.SetWhenST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FWhen = nil then
      FWhen := TFhirInstant.create;
    FWhen.value := value
  end
  else if FWhen <> nil then
    FWhen.value := nil;
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


{ TFhirSignatureListEnumerator }

Constructor TFhirSignatureListEnumerator.Create(list : TFhirSignatureList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirSignatureListEnumerator.Destroy;
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


function TFhirBindingStrengthListAsInteger(aSet : TFhirBindingStrengthList) : Integer;
var
  a : TFhirBindingStrength;
begin
  result := 0;
  for a := low(TFhirBindingStrength) to high(TFhirBindingStrength) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBindingStrengthList(i : Integer) : TFhirBindingStrengthList;
var
  aLoop : TFhirBindingStrength;
begin
  result := [];
  for aLoop := low(TFhirBindingStrength) to high(TFhirBindingStrength) Do
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

procedure TFhirElementDefinitionSlicing.Assign(oSource : TAdvObject);
begin
  inherited;
  if (TFhirElementDefinitionSlicing(oSource).FDiscriminatorList = nil) then
  begin
    FDiscriminatorList.free;
    FDiscriminatorList := nil;
  end
  else
  begin
    FDiscriminatorList := TFhirStringList.Create;
  FDiscriminatorList.Assign(TFhirElementDefinitionSlicing(oSource).FDiscriminatorList);
  end;
  descriptionElement := TFhirElementDefinitionSlicing(oSource).descriptionElement.Clone;
  orderedElement := TFhirElementDefinitionSlicing(oSource).orderedElement.Clone;
  FRules := TFhirElementDefinitionSlicing(oSource).FRules.Link;
end;

procedure TFhirElementDefinitionSlicing.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'discriminator') Then
    list.addAll(FDiscriminatorList);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'ordered') Then
     list.add(FOrdered.Link);
  if (child_name = 'rules') Then
     list.add(FRules.Link);
end;

procedure TFhirElementDefinitionSlicing.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'discriminator', 'string', FDiscriminatorList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'ordered', 'boolean', FOrdered.Link));{2}
  oList.add(TFHIRProperty.create(self, 'rules', 'code', FRules.Link));{1}
end;

procedure TFhirElementDefinitionSlicing.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'discriminator') then DiscriminatorList.add(propValue as TFhirString){2}
  else if (propName = 'description') then DescriptionElement := propValue as TFhirString{5a}
  else if (propName = 'ordered') then OrderedElement := propValue as TFhirBoolean{5a}
  else if (propName = 'rules') then RulesElement := propValue as TFHIREnum
  else inherited;
end;

function TFhirElementDefinitionSlicing.FhirType : string;
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

Function TFhirElementDefinitionSlicing.GetRulesST : TFhirResourceSlicingRules;
begin
  if FRules = nil then
    result := TFhirResourceSlicingRules(0)
  else
    result := TFhirResourceSlicingRules(StringArrayIndexOfSensitive(CODES_TFhirResourceSlicingRules, FRules.value));
end;

Procedure TFhirElementDefinitionSlicing.SetRulesST(value : TFhirResourceSlicingRules);
begin
  if ord(value) = 0 then
    RulesElement := nil
  else
    RulesElement := TFhirEnum.create(CODES_TFhirResourceSlicingRules[value]);
end;


{ TFhirElementDefinitionSlicingListEnumerator }

Constructor TFhirElementDefinitionSlicingListEnumerator.Create(list : TFhirElementDefinitionSlicingList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirElementDefinitionSlicingListEnumerator.Destroy;
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

procedure TFhirElementDefinitionBase.Assign(oSource : TAdvObject);
begin
  inherited;
  pathElement := TFhirElementDefinitionBase(oSource).pathElement.Clone;
  minElement := TFhirElementDefinitionBase(oSource).minElement.Clone;
  maxElement := TFhirElementDefinitionBase(oSource).maxElement.Clone;
end;

procedure TFhirElementDefinitionBase.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'path') Then
     list.add(FPath.Link);
  if (child_name = 'min') Then
     list.add(FMin.Link);
  if (child_name = 'max') Then
     list.add(FMax.Link);
end;

procedure TFhirElementDefinitionBase.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'path', 'string', FPath.Link));{2}
  oList.add(TFHIRProperty.create(self, 'min', 'integer', FMin.Link));{2}
  oList.add(TFHIRProperty.create(self, 'max', 'string', FMax.Link));{2}
end;

procedure TFhirElementDefinitionBase.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'path') then PathElement := propValue as TFhirString{5a}
  else if (propName = 'min') then MinElement := propValue as TFhirInteger{5a}
  else if (propName = 'max') then MaxElement := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirElementDefinitionBase.FhirType : string;
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


{ TFhirElementDefinitionBaseListEnumerator }

Constructor TFhirElementDefinitionBaseListEnumerator.Create(list : TFhirElementDefinitionBaseList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirElementDefinitionBaseListEnumerator.Destroy;
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

procedure TFhirElementDefinitionType.Assign(oSource : TAdvObject);
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
    FAggregation := TFHIREnumList.Create;
  FAggregation.Assign(TFhirElementDefinitionType(oSource).FAggregation);
end;
end;

procedure TFhirElementDefinitionType.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'code') Then
     list.add(FCode.Link);
  if (child_name = 'profile') Then
    list.addAll(FProfileList);
  if (child_name = 'aggregation') Then
     list.addAll(FAggregation);
end;

procedure TFhirElementDefinitionType.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'code', 'code', FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'profile', 'uri', FProfileList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'aggregation', 'code', FAggregation.Link)){3};
end;

procedure TFhirElementDefinitionType.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'code') then
    if propValue is TFHIRCode then
      CodeElement := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      CodeElement := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'profile') then ProfileList.add(propValue as TFhirUri){2}
  else if (propName = 'aggregation') then FAggregation.add(propValue as TFHIREnum) {1}
  else inherited;
end;

function TFhirElementDefinitionType.FhirType : string;
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
    FAggregation := TFHIREnumList.Create;
  result := FAggregation;
end;

Function TFhirElementDefinitionType.GetHasAggregation : boolean;
begin
  result := (FAggregation <> nil) and (FAggregation.count > 0);
end;

Function TFhirElementDefinitionType.GetAggregationST : TFhirResourceAggregationModeList;
  var i : integer;
begin
  result := [];
  if Faggregation <> nil then
  for i := 0 to Faggregation.count - 1 do
    result := result + [TFhirResourceAggregationMode(StringArrayIndexOfSensitive(CODES_TFhirResourceAggregationMode, Faggregation[i].value))];
end;

Procedure TFhirElementDefinitionType.SetAggregationST(value : TFhirResourceAggregationModeList);
var a : TFhirResourceAggregationMode;
begin
  Faggregation.clear;
  for a := low(TFhirResourceAggregationMode) to high(TFhirResourceAggregationMode) do
    if a in value then
      begin
         if Faggregation = nil then
           Faggregation := TFhirEnumList.create;
      Faggregation.add(TFhirEnum.create(CODES_TFhirResourceAggregationMode[a]));
end;
end;


{ TFhirElementDefinitionTypeListEnumerator }

Constructor TFhirElementDefinitionTypeListEnumerator.Create(list : TFhirElementDefinitionTypeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirElementDefinitionTypeListEnumerator.Destroy;
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

procedure TFhirElementDefinitionConstraint.Assign(oSource : TAdvObject);
begin
  inherited;
  keyElement := TFhirElementDefinitionConstraint(oSource).keyElement.Clone;
  requirementsElement := TFhirElementDefinitionConstraint(oSource).requirementsElement.Clone;
  FSeverity := TFhirElementDefinitionConstraint(oSource).FSeverity.Link;
  humanElement := TFhirElementDefinitionConstraint(oSource).humanElement.Clone;
  xpathElement := TFhirElementDefinitionConstraint(oSource).xpathElement.Clone;
end;

procedure TFhirElementDefinitionConstraint.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'key') Then
     list.add(FKey.Link);
  if (child_name = 'requirements') Then
     list.add(FRequirements.Link);
  if (child_name = 'severity') Then
     list.add(FSeverity.Link);
  if (child_name = 'human') Then
     list.add(FHuman.Link);
  if (child_name = 'xpath') Then
     list.add(FXpath.Link);
end;

procedure TFhirElementDefinitionConstraint.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'key', 'id', FKey.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requirements', 'string', FRequirements.Link));{2}
  oList.add(TFHIRProperty.create(self, 'severity', 'code', FSeverity.Link));{1}
  oList.add(TFHIRProperty.create(self, 'human', 'string', FHuman.Link));{2}
  oList.add(TFHIRProperty.create(self, 'xpath', 'string', FXpath.Link));{2}
end;

procedure TFhirElementDefinitionConstraint.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'key') then KeyElement := propValue as TFhirId{5a}
  else if (propName = 'requirements') then RequirementsElement := propValue as TFhirString{5a}
  else if (propName = 'severity') then SeverityElement := propValue as TFHIREnum
  else if (propName = 'human') then HumanElement := propValue as TFhirString{5a}
  else if (propName = 'xpath') then XpathElement := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirElementDefinitionConstraint.FhirType : string;
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

Function TFhirElementDefinitionConstraint.GetSeverityST : TFhirConstraintSeverity;
begin
  if FSeverity = nil then
    result := TFhirConstraintSeverity(0)
  else
    result := TFhirConstraintSeverity(StringArrayIndexOfSensitive(CODES_TFhirConstraintSeverity, FSeverity.value));
end;

Procedure TFhirElementDefinitionConstraint.SetSeverityST(value : TFhirConstraintSeverity);
begin
  if ord(value) = 0 then
    SeverityElement := nil
  else
    SeverityElement := TFhirEnum.create(CODES_TFhirConstraintSeverity[value]);
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


{ TFhirElementDefinitionConstraintListEnumerator }

Constructor TFhirElementDefinitionConstraintListEnumerator.Create(list : TFhirElementDefinitionConstraintList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirElementDefinitionConstraintListEnumerator.Destroy;
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

procedure TFhirElementDefinitionBinding.Assign(oSource : TAdvObject);
begin
  inherited;
  FStrength := TFhirElementDefinitionBinding(oSource).FStrength.Link;
  descriptionElement := TFhirElementDefinitionBinding(oSource).descriptionElement.Clone;
  valueSet := TFhirElementDefinitionBinding(oSource).valueSet.Clone;
end;

procedure TFhirElementDefinitionBinding.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'strength') Then
     list.add(FStrength.Link);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'valueSet[x]') Then
     list.add(FValueSet.Link);
end;

procedure TFhirElementDefinitionBinding.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'strength', 'code', FStrength.Link));{1}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'valueSet[x]', 'uri|Reference(ValueSet)', FValueSet.Link));{2}
end;

procedure TFhirElementDefinitionBinding.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'strength') then StrengthElement := propValue as TFHIREnum
  else if (propName = 'description') then DescriptionElement := propValue as TFhirString{5a}
  else if (propName.startsWith('valueSet')) then ValueSet := propValue as TFhirType{4}
  else inherited;
end;

function TFhirElementDefinitionBinding.FhirType : string;
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

{ TFhirElementDefinitionBinding }

Procedure TFhirElementDefinitionBinding.SetStrength(value : TFhirEnum);
begin
  FStrength.free;
  FStrength := value;
end;

Function TFhirElementDefinitionBinding.GetStrengthST : TFhirBindingStrength;
begin
  if FStrength = nil then
    result := TFhirBindingStrength(0)
  else
    result := TFhirBindingStrength(StringArrayIndexOfSensitive(CODES_TFhirBindingStrength, FStrength.value));
end;

Procedure TFhirElementDefinitionBinding.SetStrengthST(value : TFhirBindingStrength);
begin
  if ord(value) = 0 then
    StrengthElement := nil
  else
    StrengthElement := TFhirEnum.create(CODES_TFhirBindingStrength[value]);
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


{ TFhirElementDefinitionBindingListEnumerator }

Constructor TFhirElementDefinitionBindingListEnumerator.Create(list : TFhirElementDefinitionBindingList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirElementDefinitionBindingListEnumerator.Destroy;
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

procedure TFhirElementDefinitionMapping.Assign(oSource : TAdvObject);
begin
  inherited;
  identityElement := TFhirElementDefinitionMapping(oSource).identityElement.Clone;
  languageElement := TFhirElementDefinitionMapping(oSource).languageElement.Clone;
  mapElement := TFhirElementDefinitionMapping(oSource).mapElement.Clone;
end;

procedure TFhirElementDefinitionMapping.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identity') Then
     list.add(FIdentity.Link);
  if (child_name = 'language') Then
     list.add(FLanguage.Link);
  if (child_name = 'map') Then
     list.add(FMap.Link);
end;

procedure TFhirElementDefinitionMapping.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identity', 'id', FIdentity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'language', 'code', FLanguage.Link));{2}
  oList.add(TFHIRProperty.create(self, 'map', 'string', FMap.Link));{2}
end;

procedure TFhirElementDefinitionMapping.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identity') then IdentityElement := propValue as TFhirId{5a}
  else if (propName = 'language') then
    if propValue is TFHIRCode then
      LanguageElement := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      LanguageElement := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'map') then MapElement := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirElementDefinitionMapping.FhirType : string;
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


{ TFhirElementDefinitionMappingListEnumerator }

Constructor TFhirElementDefinitionMappingListEnumerator.Create(list : TFhirElementDefinitionMappingList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirElementDefinitionMappingListEnumerator.Destroy;
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

procedure TFhirElementDefinition.Assign(oSource : TAdvObject);
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
    FRepresentation := TFHIREnumList.Create;
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
    FMappingList := TFhirElementDefinitionMappingList.Create;
  FMappingList.Assign(TFhirElementDefinition(oSource).FMappingList);
end;
end;

procedure TFhirElementDefinition.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'path') Then
     list.add(FPath.Link);
  if (child_name = 'representation') Then
     list.addAll(FRepresentation);
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'label') Then
     list.add(FLabel_.Link);
  if (child_name = 'code') Then
    list.addAll(FCodeList);
  if (child_name = 'slicing') Then
     list.add(FSlicing.Link);
  if (child_name = 'short') Then
     list.add(FShort.Link);
  if (child_name = 'definition') Then
     list.add(FDefinition.Link);
  if (child_name = 'comments') Then
     list.add(FComments.Link);
  if (child_name = 'requirements') Then
     list.add(FRequirements.Link);
  if (child_name = 'alias') Then
    list.addAll(FAliasList);
  if (child_name = 'min') Then
     list.add(FMin.Link);
  if (child_name = 'max') Then
     list.add(FMax.Link);
  if (child_name = 'base') Then
     list.add(FBase.Link);
  if (child_name = 'type') Then
    list.addAll(FType_List);
  if (child_name = 'nameReference') Then
     list.add(FNameReference.Link);
  if (child_name = 'defaultValue[x]') Then
     list.add(FDefaultValue.Link);
  if (child_name = 'meaningWhenMissing') Then
     list.add(FMeaningWhenMissing.Link);
  if (child_name = 'fixed[x]') Then
     list.add(FFixed.Link);
  if (child_name = 'pattern[x]') Then
     list.add(FPattern.Link);
  if (child_name = 'example[x]') Then
     list.add(FExample.Link);
  if (child_name = 'minValue[x]') Then
     list.add(FMinValue.Link);
  if (child_name = 'maxValue[x]') Then
     list.add(FMaxValue.Link);
  if (child_name = 'maxLength') Then
     list.add(FMaxLength.Link);
  if (child_name = 'condition') Then
    list.addAll(FConditionList);
  if (child_name = 'constraint') Then
    list.addAll(FConstraintList);
  if (child_name = 'mustSupport') Then
     list.add(FMustSupport.Link);
  if (child_name = 'isModifier') Then
     list.add(FIsModifier.Link);
  if (child_name = 'isSummary') Then
     list.add(FIsSummary.Link);
  if (child_name = 'binding') Then
     list.add(FBinding.Link);
  if (child_name = 'mapping') Then
    list.addAll(FMappingList);
end;

procedure TFhirElementDefinition.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'path', 'string', FPath.Link));{2}
  oList.add(TFHIRProperty.create(self, 'representation', 'code', FRepresentation.Link)){3};
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'label', 'string', FLabel_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'Coding', FCodeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'slicing', '', FSlicing.Link));{2}
  oList.add(TFHIRProperty.create(self, 'short', 'string', FShort.Link));{2}
  oList.add(TFHIRProperty.create(self, 'definition', 'markdown', FDefinition.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comments', 'markdown', FComments.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requirements', 'markdown', FRequirements.Link));{2}
  oList.add(TFHIRProperty.create(self, 'alias', 'string', FAliasList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'min', 'integer', FMin.Link));{2}
  oList.add(TFHIRProperty.create(self, 'max', 'string', FMax.Link));{2}
  oList.add(TFHIRProperty.create(self, 'base', '', FBase.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', '', FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'nameReference', 'string', FNameReference.Link));{2}
  oList.add(TFHIRProperty.create(self, 'defaultValue[x]', '*', FDefaultValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'meaningWhenMissing', 'markdown', FMeaningWhenMissing.Link));{2}
  oList.add(TFHIRProperty.create(self, 'fixed[x]', '*', FFixed.Link));{2}
  oList.add(TFHIRProperty.create(self, 'pattern[x]', '*', FPattern.Link));{2}
  oList.add(TFHIRProperty.create(self, 'example[x]', '*', FExample.Link));{2}
  oList.add(TFHIRProperty.create(self, 'minValue[x]', '*', FMinValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'maxValue[x]', '*', FMaxValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'maxLength', 'integer', FMaxLength.Link));{2}
  oList.add(TFHIRProperty.create(self, 'condition', 'id', FConditionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'constraint', '', FConstraintList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'mustSupport', 'boolean', FMustSupport.Link));{2}
  oList.add(TFHIRProperty.create(self, 'isModifier', 'boolean', FIsModifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'isSummary', 'boolean', FIsSummary.Link));{2}
  oList.add(TFHIRProperty.create(self, 'binding', '', FBinding.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mapping', '', FMappingList.Link)){3};
end;

procedure TFhirElementDefinition.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'path') then PathElement := propValue as TFhirString{5a}
  else if (propName = 'representation') then FRepresentation.add(propValue as TFHIREnum) {1}
  else if (propName = 'name') then NameElement := propValue as TFhirString{5a}
  else if (propName = 'label') then Label_Element := propValue as TFhirString{5a}
  else if (propName = 'code') then CodeList.add(propValue as TFhirCoding){2}
  else if (propName = 'slicing') then Slicing := propValue as TFhirElementDefinitionSlicing{4b}
  else if (propName = 'short') then ShortElement := propValue as TFhirString{5a}
  else if (propName = 'definition') then DefinitionElement := propValue as TFhirString{5a}
  else if (propName = 'comments') then CommentsElement := propValue as TFhirString{5a}
  else if (propName = 'requirements') then RequirementsElement := propValue as TFhirString{5a}
  else if (propName = 'alias') then AliasList.add(propValue as TFhirString){2}
  else if (propName = 'min') then MinElement := propValue as TFhirInteger{5a}
  else if (propName = 'max') then MaxElement := propValue as TFhirString{5a}
  else if (propName = 'base') then Base := propValue as TFhirElementDefinitionBase{4b}
  else if (propName = 'type') then Type_List.add(propValue as TFhirElementDefinitionType){2}
  else if (propName = 'nameReference') then NameReferenceElement := propValue as TFhirString{5a}
  else if (propName.startsWith('defaultValue')) then DefaultValue := propValue as TFhirType{4}
  else if (propName = 'meaningWhenMissing') then MeaningWhenMissingElement := propValue as TFhirString{5a}
  else if (propName.startsWith('fixed')) then Fixed := propValue as TFhirType{4}
  else if (propName.startsWith('pattern')) then Pattern := propValue as TFhirType{4}
  else if (propName.startsWith('example')) then Example := propValue as TFhirType{4}
  else if (propName.startsWith('minValue')) then MinValue := propValue as TFhirType{4}
  else if (propName.startsWith('maxValue')) then MaxValue := propValue as TFhirType{4}
  else if (propName = 'maxLength') then MaxLengthElement := propValue as TFhirInteger{5a}
  else if (propName = 'condition') then ConditionList.add(propValue as TFhirId){2}
  else if (propName = 'constraint') then ConstraintList.add(propValue as TFhirElementDefinitionConstraint){2}
  else if (propName = 'mustSupport') then MustSupportElement := propValue as TFhirBoolean{5a}
  else if (propName = 'isModifier') then IsModifierElement := propValue as TFhirBoolean{5a}
  else if (propName = 'isSummary') then IsSummaryElement := propValue as TFhirBoolean{5a}
  else if (propName = 'binding') then Binding := propValue as TFhirElementDefinitionBinding{4b}
  else if (propName = 'mapping') then MappingList.add(propValue as TFhirElementDefinitionMapping){2}
  else inherited;
end;

function TFhirElementDefinition.FhirType : string;
begin
  result := 'ElementDefinition';
end;

function TFhirElementDefinition.Link : TFhirElementDefinition;
begin
  result := TFhirElementDefinition(inherited Link);
end;

function TFhirElementDefinition.Clone : TFhirElementDefinition;
begin
  result := TFhirElementDefinition(inherited Clone);
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
    FRepresentation := TFHIREnumList.Create;
  result := FRepresentation;
end;

Function TFhirElementDefinition.GetHasRepresentation : boolean;
begin
  result := (FRepresentation <> nil) and (FRepresentation.count > 0);
end;

Function TFhirElementDefinition.GetRepresentationST : TFhirPropertyRepresentationList;
  var i : integer;
begin
  result := [];
  if Frepresentation <> nil then
  for i := 0 to Frepresentation.count - 1 do
    result := result + [TFhirPropertyRepresentation(StringArrayIndexOfSensitive(CODES_TFhirPropertyRepresentation, Frepresentation[i].value))];
end;

Procedure TFhirElementDefinition.SetRepresentationST(value : TFhirPropertyRepresentationList);
var a : TFhirPropertyRepresentation;
begin
  Frepresentation.clear;
  for a := low(TFhirPropertyRepresentation) to high(TFhirPropertyRepresentation) do
    if a in value then
      begin
         if Frepresentation = nil then
           Frepresentation := TFhirEnumList.create;
      Frepresentation.add(TFhirEnum.create(CODES_TFhirPropertyRepresentation[a]));
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

Procedure TFhirElementDefinition.SetDefinition(value : TFhirString);
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
      FDefinition := TFhirString.create;
    FDefinition.value := value
  end
  else if FDefinition <> nil then
    FDefinition.value := '';
end;

Procedure TFhirElementDefinition.SetComments(value : TFhirString);
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
      FComments := TFhirString.create;
    FComments.value := value
  end
  else if FComments <> nil then
    FComments.value := '';
end;

Procedure TFhirElementDefinition.SetRequirements(value : TFhirString);
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
      FRequirements := TFhirString.create;
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

Procedure TFhirElementDefinition.SetMeaningWhenMissing(value : TFhirString);
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
      FMeaningWhenMissing := TFhirString.create;
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


{ TFhirElementDefinitionListEnumerator }

Constructor TFhirElementDefinitionListEnumerator.Create(list : TFhirElementDefinitionList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirElementDefinitionListEnumerator.Destroy;
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

procedure TFhirTimingRepeat.Assign(oSource : TAdvObject);
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

procedure TFhirTimingRepeat.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'bounds[x]') Then
     list.add(FBounds.Link);
  if (child_name = 'count') Then
     list.add(FCount.Link);
  if (child_name = 'duration') Then
     list.add(FDuration.Link);
  if (child_name = 'durationMax') Then
     list.add(FDurationMax.Link);
  if (child_name = 'durationUnits') Then
     list.add(FDurationUnits.Link);
  if (child_name = 'frequency') Then
     list.add(FFrequency.Link);
  if (child_name = 'frequencyMax') Then
     list.add(FFrequencyMax.Link);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
  if (child_name = 'periodMax') Then
     list.add(FPeriodMax.Link);
  if (child_name = 'periodUnits') Then
     list.add(FPeriodUnits.Link);
  if (child_name = 'when') Then
     list.add(FWhen.Link);
end;

procedure TFhirTimingRepeat.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'bounds[x]', 'Duration|Range|Period', FBounds.Link));{2}
  oList.add(TFHIRProperty.create(self, 'count', 'integer', FCount.Link));{2}
  oList.add(TFHIRProperty.create(self, 'duration', 'decimal', FDuration.Link));{2}
  oList.add(TFHIRProperty.create(self, 'durationMax', 'decimal', FDurationMax.Link));{2}
  oList.add(TFHIRProperty.create(self, 'durationUnits', 'code', FDurationUnits.Link));{1}
  oList.add(TFHIRProperty.create(self, 'frequency', 'integer', FFrequency.Link));{2}
  oList.add(TFHIRProperty.create(self, 'frequencyMax', 'integer', FFrequencyMax.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'decimal', FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'periodMax', 'decimal', FPeriodMax.Link));{2}
  oList.add(TFHIRProperty.create(self, 'periodUnits', 'code', FPeriodUnits.Link));{1}
  oList.add(TFHIRProperty.create(self, 'when', 'code', FWhen.Link));{1}
end;

procedure TFhirTimingRepeat.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName.startsWith('bounds')) then Bounds := propValue as TFhirType{4}
  else if (propName = 'count') then CountElement := propValue as TFhirInteger{5a}
  else if (propName = 'duration') then DurationElement := propValue as TFhirDecimal{5a}
  else if (propName = 'durationMax') then DurationMaxElement := propValue as TFhirDecimal{5a}
  else if (propName = 'durationUnits') then DurationUnitsElement := propValue as TFHIREnum
  else if (propName = 'frequency') then FrequencyElement := propValue as TFhirInteger{5a}
  else if (propName = 'frequencyMax') then FrequencyMaxElement := propValue as TFhirInteger{5a}
  else if (propName = 'period') then PeriodElement := propValue as TFhirDecimal{5a}
  else if (propName = 'periodMax') then PeriodMaxElement := propValue as TFhirDecimal{5a}
  else if (propName = 'periodUnits') then PeriodUnitsElement := propValue as TFHIREnum
  else if (propName = 'when') then WhenElement := propValue as TFHIREnum
  else inherited;
end;

function TFhirTimingRepeat.FhirType : string;
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

Function TFhirTimingRepeat.GetDurationUnitsST : TFhirUnitsOfTime;
begin
  if FDurationUnits = nil then
    result := TFhirUnitsOfTime(0)
  else
    result := TFhirUnitsOfTime(StringArrayIndexOfSensitive(CODES_TFhirUnitsOfTime, FDurationUnits.value));
end;

Procedure TFhirTimingRepeat.SetDurationUnitsST(value : TFhirUnitsOfTime);
begin
  if ord(value) = 0 then
    DurationUnitsElement := nil
  else
    DurationUnitsElement := TFhirEnum.create(CODES_TFhirUnitsOfTime[value]);
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

Function TFhirTimingRepeat.GetPeriodUnitsST : TFhirUnitsOfTime;
begin
  if FPeriodUnits = nil then
    result := TFhirUnitsOfTime(0)
  else
    result := TFhirUnitsOfTime(StringArrayIndexOfSensitive(CODES_TFhirUnitsOfTime, FPeriodUnits.value));
end;

Procedure TFhirTimingRepeat.SetPeriodUnitsST(value : TFhirUnitsOfTime);
begin
  if ord(value) = 0 then
    PeriodUnitsElement := nil
  else
    PeriodUnitsElement := TFhirEnum.create(CODES_TFhirUnitsOfTime[value]);
end;

Procedure TFhirTimingRepeat.SetWhen(value : TFhirEnum);
begin
  FWhen.free;
  FWhen := value;
end;

Function TFhirTimingRepeat.GetWhenST : TFhirEventTiming;
begin
  if FWhen = nil then
    result := TFhirEventTiming(0)
  else
    result := TFhirEventTiming(StringArrayIndexOfSensitive(CODES_TFhirEventTiming, FWhen.value));
end;

Procedure TFhirTimingRepeat.SetWhenST(value : TFhirEventTiming);
begin
  if ord(value) = 0 then
    WhenElement := nil
  else
    WhenElement := TFhirEnum.create(CODES_TFhirEventTiming[value]);
end;


{ TFhirTimingRepeatListEnumerator }

Constructor TFhirTimingRepeatListEnumerator.Create(list : TFhirTimingRepeatList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirTimingRepeatListEnumerator.Destroy;
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

procedure TFhirTiming.Assign(oSource : TAdvObject);
begin
  inherited;
  if (TFhirTiming(oSource).FEventList = nil) then
  begin
    FEventList.free;
    FEventList := nil;
  end
  else
  begin
    FEventList := TFhirDateTimeList.Create;
  FEventList.Assign(TFhirTiming(oSource).FEventList);
  end;
  repeat_ := TFhirTiming(oSource).repeat_.Clone;
  code := TFhirTiming(oSource).code.Clone;
end;

procedure TFhirTiming.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'event') Then
    list.addAll(FEventList);
  if (child_name = 'repeat') Then
     list.add(FRepeat_.Link);
  if (child_name = 'code') Then
     list.add(FCode.Link);
end;

procedure TFhirTiming.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'event', 'dateTime', FEventList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'repeat', '', FRepeat_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link));{2}
end;

procedure TFhirTiming.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'event') then EventList.add(propValue as TFhirDateTime){2}
  else if (propName = 'repeat') then Repeat_ := propValue as TFhirTimingRepeat{4b}
  else if (propName = 'code') then Code := propValue as TFhirCodeableConcept{4b}
  else inherited;
end;

function TFhirTiming.FhirType : string;
begin
  result := 'Timing';
end;

function TFhirTiming.Link : TFhirTiming;
begin
  result := TFhirTiming(inherited Link);
end;

function TFhirTiming.Clone : TFhirTiming;
begin
  result := TFhirTiming(inherited Clone);
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


{ TFhirTimingListEnumerator }

Constructor TFhirTimingListEnumerator.Create(list : TFhirTimingList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirTimingListEnumerator.Destroy;
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


function TFhirAddressTypeListAsInteger(aSet : TFhirAddressTypeList) : Integer;
var
  a : TFhirAddressType;
begin
  result := 0;
  for a := low(TFhirAddressType) to high(TFhirAddressType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAddressTypeList(i : Integer) : TFhirAddressTypeList;
var
  aLoop : TFhirAddressType;
begin
  result := [];
  for aLoop := low(TFhirAddressType) to high(TFhirAddressType) Do
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

procedure TFhirAddress.Assign(oSource : TAdvObject);
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

procedure TFhirAddress.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'use') Then
     list.add(FUse.Link);
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'text') Then
     list.add(FText.Link);
  if (child_name = 'line') Then
    list.addAll(FLineList);
  if (child_name = 'city') Then
     list.add(FCity.Link);
  if (child_name = 'district') Then
     list.add(FDistrict.Link);
  if (child_name = 'state') Then
     list.add(FState.Link);
  if (child_name = 'postalCode') Then
     list.add(FPostalCode.Link);
  if (child_name = 'country') Then
     list.add(FCountry.Link);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
end;

procedure TFhirAddress.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'use', 'code', FUse.Link));{1}
  oList.add(TFHIRProperty.create(self, 'type', 'code', FType_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'text', 'string', FText.Link));{2}
  oList.add(TFHIRProperty.create(self, 'line', 'string', FLineList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'city', 'string', FCity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'district', 'string', FDistrict.Link));{2}
  oList.add(TFHIRProperty.create(self, 'state', 'string', FState.Link));{2}
  oList.add(TFHIRProperty.create(self, 'postalCode', 'string', FPostalCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'country', 'string', FCountry.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
end;

procedure TFhirAddress.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'use') then UseElement := propValue as TFHIREnum
  else if (propName = 'type') then Type_Element := propValue as TFHIREnum
  else if (propName = 'text') then TextElement := propValue as TFhirString{5a}
  else if (propName = 'line') then LineList.add(propValue as TFhirString){2}
  else if (propName = 'city') then CityElement := propValue as TFhirString{5a}
  else if (propName = 'district') then DistrictElement := propValue as TFhirString{5a}
  else if (propName = 'state') then StateElement := propValue as TFhirString{5a}
  else if (propName = 'postalCode') then PostalCodeElement := propValue as TFhirString{5a}
  else if (propName = 'country') then CountryElement := propValue as TFhirString{5a}
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
    UseElement := nil
  else
    UseElement := TFhirEnum.create(CODES_TFhirAddressUse[value]);
end;

Procedure TFhirAddress.SetType_(value : TFhirEnum);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirAddress.GetType_ST : TFhirAddressType;
begin
  if FType_ = nil then
    result := TFhirAddressType(0)
  else
    result := TFhirAddressType(StringArrayIndexOfSensitive(CODES_TFhirAddressType, FType_.value));
end;

Procedure TFhirAddress.SetType_ST(value : TFhirAddressType);
begin
  if ord(value) = 0 then
    Type_Element := nil
  else
    Type_Element := TFhirEnum.create(CODES_TFhirAddressType[value]);
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
  inc(FIndex);
  Result := FIndex < FList.count;
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
  textElement := TFhirHumanName(oSource).textElement.Clone;
  if (TFhirHumanName(oSource).FFamilyList = nil) then
  begin
    FFamilyList.free;
    FFamilyList := nil;
  end
  else
  begin
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
    FSuffixList := TFhirStringList.Create;
  FSuffixList.Assign(TFhirHumanName(oSource).FSuffixList);
  end;
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
  if (propName = 'use') then UseElement := propValue as TFHIREnum
  else if (propName = 'text') then TextElement := propValue as TFhirString{5a}
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
    UseElement := nil
  else
    UseElement := TFhirEnum.create(CODES_TFhirNameUse[value]);
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
  inc(FIndex);
  Result := FIndex < FList.count;
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

procedure TFhirMeta.Assign(oSource : TAdvObject);
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
    FTagList := TFhirCodingList.Create;
    FTagList.Assign(TFhirMeta(oSource).FTagList);
  end;
end;

procedure TFhirMeta.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'versionId') Then
     list.add(FVersionId.Link);
  if (child_name = 'lastUpdated') Then
     list.add(FLastUpdated.Link);
  if (child_name = 'profile') Then
    list.addAll(FProfileList);
  if (child_name = 'security') Then
    list.addAll(FSecurityList);
  if (child_name = 'tag') Then
    list.addAll(FTagList);
end;

procedure TFhirMeta.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'versionId', 'id', FVersionId.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lastUpdated', 'instant', FLastUpdated.Link));{2}
  oList.add(TFHIRProperty.create(self, 'profile', 'uri', FProfileList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'security', 'Coding', FSecurityList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'tag', 'Coding', FTagList.Link)){3};
end;

procedure TFhirMeta.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'versionId') then VersionIdElement := propValue as TFhirId{5a}
  else if (propName = 'lastUpdated') then LastUpdatedElement := propValue as TFhirInstant{5a}
  else if (propName = 'profile') then ProfileList.add(propValue as TFhirUri){2}
  else if (propName = 'security') then SecurityList.add(propValue as TFhirCoding){2}
  else if (propName = 'tag') then TagList.add(propValue as TFhirCoding){2}
  else inherited;
end;

function TFhirMeta.FhirType : string;
begin
  result := 'Meta';
end;

function TFhirMeta.Link : TFhirMeta;
begin
  result := TFhirMeta(inherited Link);
end;

function TFhirMeta.Clone : TFhirMeta;
begin
  result := TFhirMeta(inherited Clone);
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

Function TFhirMeta.GetLastUpdatedST : TDateAndTime;
begin
  if FLastUpdated = nil then
    result := nil
  else
    result := FLastUpdated.value;
end;

Procedure TFhirMeta.SetLastUpdatedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FLastUpdated = nil then
      FLastUpdated := TFhirInstant.create;
    FLastUpdated.value := value
  end
  else if FLastUpdated <> nil then
    FLastUpdated.value := nil;
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


{ TFhirMetaListEnumerator }

Constructor TFhirMetaListEnumerator.Create(list : TFhirMetaList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirMetaListEnumerator.Destroy;
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

function TFhirContactPointSystemListAsInteger(aSet : TFhirContactPointSystemList) : Integer;
var
  a : TFhirContactPointSystem;
begin
  result := 0;
  for a := low(TFhirContactPointSystem) to high(TFhirContactPointSystem) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContactPointSystemList(i : Integer) : TFhirContactPointSystemList;
var
  aLoop : TFhirContactPointSystem;
begin
  result := [];
  for aLoop := low(TFhirContactPointSystem) to high(TFhirContactPointSystem) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirContactPointUseListAsInteger(aSet : TFhirContactPointUseList) : Integer;
var
  a : TFhirContactPointUse;
begin
  result := 0;
  for a := low(TFhirContactPointUse) to high(TFhirContactPointUse) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContactPointUseList(i : Integer) : TFhirContactPointUseList;
var
  aLoop : TFhirContactPointUse;
begin
  result := [];
  for aLoop := low(TFhirContactPointUse) to high(TFhirContactPointUse) Do
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

procedure TFhirContactPoint.Assign(oSource : TAdvObject);
begin
  inherited;
  FSystem := TFhirContactPoint(oSource).FSystem.Link;
  valueElement := TFhirContactPoint(oSource).valueElement.Clone;
  FUse := TFhirContactPoint(oSource).FUse.Link;
  rankElement := TFhirContactPoint(oSource).rankElement.Clone;
  period := TFhirContactPoint(oSource).period.Clone;
end;

procedure TFhirContactPoint.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'system') Then
     list.add(FSystem.Link);
  if (child_name = 'value') Then
     list.add(FValue.Link);
  if (child_name = 'use') Then
     list.add(FUse.Link);
  if (child_name = 'rank') Then
     list.add(FRank.Link);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
end;

procedure TFhirContactPoint.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'system', 'code', FSystem.Link));{1}
  oList.add(TFHIRProperty.create(self, 'value', 'string', FValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'use', 'code', FUse.Link));{1}
  oList.add(TFHIRProperty.create(self, 'rank', 'positiveInt', FRank.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
end;

procedure TFhirContactPoint.setProperty(propName: string; propValue: TFHIRObject);
begin
  if (propName = 'system') then SystemElement := propValue as TFHIREnum
  else if (propName = 'value') then ValueElement := propValue as TFhirString{5a}
  else if (propName = 'use') then UseElement := propValue as TFHIREnum
  else if (propName = 'rank') then RankElement := propValue as TFhirPositiveInt{5a}
  else if (propName = 'period') then Period := propValue as TFhirPeriod{4b}
  else inherited;
end;

function TFhirContactPoint.FhirType : string;
begin
  result := 'ContactPoint';
end;

function TFhirContactPoint.Link : TFhirContactPoint;
begin
  result := TFhirContactPoint(inherited Link);
end;

function TFhirContactPoint.Clone : TFhirContactPoint;
begin
  result := TFhirContactPoint(inherited Clone);
end;

{ TFhirContactPoint }

Procedure TFhirContactPoint.SetSystem(value : TFhirEnum);
begin
  FSystem.free;
  FSystem := value;
end;

Function TFhirContactPoint.GetSystemST : TFhirContactPointSystem;
begin
  if FSystem = nil then
    result := TFhirContactPointSystem(0)
  else
    result := TFhirContactPointSystem(StringArrayIndexOfSensitive(CODES_TFhirContactPointSystem, FSystem.value));
end;

Procedure TFhirContactPoint.SetSystemST(value : TFhirContactPointSystem);
begin
  if ord(value) = 0 then
    SystemElement := nil
  else
    SystemElement := TFhirEnum.create(CODES_TFhirContactPointSystem[value]);
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

Function TFhirContactPoint.GetUseST : TFhirContactPointUse;
begin
  if FUse = nil then
    result := TFhirContactPointUse(0)
  else
    result := TFhirContactPointUse(StringArrayIndexOfSensitive(CODES_TFhirContactPointUse, FUse.value));
end;

Procedure TFhirContactPoint.SetUseST(value : TFhirContactPointUse);
begin
  if ord(value) = 0 then
    UseElement := nil
  else
    UseElement := TFhirEnum.create(CODES_TFhirContactPointUse[value]);
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


{ TFhirContactPointListEnumerator }

Constructor TFhirContactPointListEnumerator.Create(list : TFhirContactPointList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirContactPointListEnumerator.Destroy;
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

function TFhirAccountStatusListAsInteger(aSet : TFhirAccountStatusList) : Integer;
var
  a : TFhirAccountStatus;
begin
  result := 0;
  for a := low(TFhirAccountStatus) to high(TFhirAccountStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAccountStatusList(i : Integer) : TFhirAccountStatusList;
var
  aLoop : TFhirAccountStatus;
begin
  result := [];
  for aLoop := low(TFhirAccountStatus) to high(TFhirAccountStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAllergyIntoleranceStatusListAsInteger(aSet : TFhirAllergyIntoleranceStatusList) : Integer;
var
  a : TFhirAllergyIntoleranceStatus;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceStatus) to high(TFhirAllergyIntoleranceStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceStatusList(i : Integer) : TFhirAllergyIntoleranceStatusList;
var
  aLoop : TFhirAllergyIntoleranceStatus;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceStatus) to high(TFhirAllergyIntoleranceStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAllergyIntoleranceCriticalityListAsInteger(aSet : TFhirAllergyIntoleranceCriticalityList) : Integer;
var
  a : TFhirAllergyIntoleranceCriticality;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceCriticality) to high(TFhirAllergyIntoleranceCriticality) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceCriticalityList(i : Integer) : TFhirAllergyIntoleranceCriticalityList;
var
  aLoop : TFhirAllergyIntoleranceCriticality;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceCriticality) to high(TFhirAllergyIntoleranceCriticality) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAllergyIntoleranceTypeListAsInteger(aSet : TFhirAllergyIntoleranceTypeList) : Integer;
var
  a : TFhirAllergyIntoleranceType;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceType) to high(TFhirAllergyIntoleranceType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceTypeList(i : Integer) : TFhirAllergyIntoleranceTypeList;
var
  aLoop : TFhirAllergyIntoleranceType;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceType) to high(TFhirAllergyIntoleranceType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAllergyIntoleranceCategoryListAsInteger(aSet : TFhirAllergyIntoleranceCategoryList) : Integer;
var
  a : TFhirAllergyIntoleranceCategory;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceCategory) to high(TFhirAllergyIntoleranceCategory) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceCategoryList(i : Integer) : TFhirAllergyIntoleranceCategoryList;
var
  aLoop : TFhirAllergyIntoleranceCategory;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceCategory) to high(TFhirAllergyIntoleranceCategory) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirReactionEventCertaintyListAsInteger(aSet : TFhirReactionEventCertaintyList) : Integer;
var
  a : TFhirReactionEventCertainty;
begin
  result := 0;
  for a := low(TFhirReactionEventCertainty) to high(TFhirReactionEventCertainty) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReactionEventCertaintyList(i : Integer) : TFhirReactionEventCertaintyList;
var
  aLoop : TFhirReactionEventCertainty;
begin
  result := [];
  for aLoop := low(TFhirReactionEventCertainty) to high(TFhirReactionEventCertainty) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirReactionEventSeverityListAsInteger(aSet : TFhirReactionEventSeverityList) : Integer;
var
  a : TFhirReactionEventSeverity;
begin
  result := 0;
  for a := low(TFhirReactionEventSeverity) to high(TFhirReactionEventSeverity) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReactionEventSeverityList(i : Integer) : TFhirReactionEventSeverityList;
var
  aLoop : TFhirReactionEventSeverity;
begin
  result := [];
  for aLoop := low(TFhirReactionEventSeverity) to high(TFhirReactionEventSeverity) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAppointmentstatusListAsInteger(aSet : TFhirAppointmentstatusList) : Integer;
var
  a : TFhirAppointmentstatus;
begin
  result := 0;
  for a := low(TFhirAppointmentstatus) to high(TFhirAppointmentstatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAppointmentstatusList(i : Integer) : TFhirAppointmentstatusList;
var
  aLoop : TFhirAppointmentstatus;
begin
  result := [];
  for aLoop := low(TFhirAppointmentstatus) to high(TFhirAppointmentstatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirParticipantrequiredListAsInteger(aSet : TFhirParticipantrequiredList) : Integer;
var
  a : TFhirParticipantrequired;
begin
  result := 0;
  for a := low(TFhirParticipantrequired) to high(TFhirParticipantrequired) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirParticipantrequiredList(i : Integer) : TFhirParticipantrequiredList;
var
  aLoop : TFhirParticipantrequired;
begin
  result := [];
  for aLoop := low(TFhirParticipantrequired) to high(TFhirParticipantrequired) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirParticipationstatusListAsInteger(aSet : TFhirParticipationstatusList) : Integer;
var
  a : TFhirParticipationstatus;
begin
  result := 0;
  for a := low(TFhirParticipationstatus) to high(TFhirParticipationstatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirParticipationstatusList(i : Integer) : TFhirParticipationstatusList;
var
  aLoop : TFhirParticipationstatus;
begin
  result := [];
  for aLoop := low(TFhirParticipationstatus) to high(TFhirParticipationstatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirParticipantstatusListAsInteger(aSet : TFhirParticipantstatusList) : Integer;
var
  a : TFhirParticipantstatus;
begin
  result := 0;
  for a := low(TFhirParticipantstatus) to high(TFhirParticipantstatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirParticipantstatusList(i : Integer) : TFhirParticipantstatusList;
var
  aLoop : TFhirParticipantstatus;
begin
  result := [];
  for aLoop := low(TFhirParticipantstatus) to high(TFhirParticipantstatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAuditEventActionListAsInteger(aSet : TFhirAuditEventActionList) : Integer;
var
  a : TFhirAuditEventAction;
begin
  result := 0;
  for a := low(TFhirAuditEventAction) to high(TFhirAuditEventAction) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAuditEventActionList(i : Integer) : TFhirAuditEventActionList;
var
  aLoop : TFhirAuditEventAction;
begin
  result := [];
  for aLoop := low(TFhirAuditEventAction) to high(TFhirAuditEventAction) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAuditEventOutcomeListAsInteger(aSet : TFhirAuditEventOutcomeList) : Integer;
var
  a : TFhirAuditEventOutcome;
begin
  result := 0;
  for a := low(TFhirAuditEventOutcome) to high(TFhirAuditEventOutcome) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAuditEventOutcomeList(i : Integer) : TFhirAuditEventOutcomeList;
var
  aLoop : TFhirAuditEventOutcome;
begin
  result := [];
  for aLoop := low(TFhirAuditEventOutcome) to high(TFhirAuditEventOutcome) Do
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


function TFhirBundleTypeListAsInteger(aSet : TFhirBundleTypeList) : Integer;
var
  a : TFhirBundleType;
begin
  result := 0;
  for a := low(TFhirBundleType) to high(TFhirBundleType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBundleTypeList(i : Integer) : TFhirBundleTypeList;
var
  aLoop : TFhirBundleType;
begin
  result := [];
  for aLoop := low(TFhirBundleType) to high(TFhirBundleType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSearchEntryModeListAsInteger(aSet : TFhirSearchEntryModeList) : Integer;
var
  a : TFhirSearchEntryMode;
begin
  result := 0;
  for a := low(TFhirSearchEntryMode) to high(TFhirSearchEntryMode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchEntryModeList(i : Integer) : TFhirSearchEntryModeList;
var
  aLoop : TFhirSearchEntryMode;
begin
  result := [];
  for aLoop := low(TFhirSearchEntryMode) to high(TFhirSearchEntryMode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirHttpVerbListAsInteger(aSet : TFhirHttpVerbList) : Integer;
var
  a : TFhirHttpVerb;
begin
  result := 0;
  for a := low(TFhirHttpVerb) to high(TFhirHttpVerb) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirHttpVerbList(i : Integer) : TFhirHttpVerbList;
var
  aLoop : TFhirHttpVerb;
begin
  result := [];
  for aLoop := low(TFhirHttpVerb) to high(TFhirHttpVerb) Do
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


function TFhirCarePlanRelationshipListAsInteger(aSet : TFhirCarePlanRelationshipList) : Integer;
var
  a : TFhirCarePlanRelationship;
begin
  result := 0;
  for a := low(TFhirCarePlanRelationship) to high(TFhirCarePlanRelationship) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanRelationshipList(i : Integer) : TFhirCarePlanRelationshipList;
var
  aLoop : TFhirCarePlanRelationship;
begin
  result := [];
  for aLoop := low(TFhirCarePlanRelationship) to high(TFhirCarePlanRelationship) Do
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


function TFhirClaimTypeLinkListAsInteger(aSet : TFhirClaimTypeLinkList) : Integer;
var
  a : TFhirClaimTypeLink;
begin
  result := 0;
  for a := low(TFhirClaimTypeLink) to high(TFhirClaimTypeLink) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirClaimTypeLinkList(i : Integer) : TFhirClaimTypeLinkList;
var
  aLoop : TFhirClaimTypeLink;
begin
  result := [];
  for aLoop := low(TFhirClaimTypeLink) to high(TFhirClaimTypeLink) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirClaimUseLinkListAsInteger(aSet : TFhirClaimUseLinkList) : Integer;
var
  a : TFhirClaimUseLink;
begin
  result := 0;
  for a := low(TFhirClaimUseLink) to high(TFhirClaimUseLink) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirClaimUseLinkList(i : Integer) : TFhirClaimUseLinkList;
var
  aLoop : TFhirClaimUseLink;
begin
  result := [];
  for aLoop := low(TFhirClaimUseLink) to high(TFhirClaimUseLink) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirRemittanceOutcomeListAsInteger(aSet : TFhirRemittanceOutcomeList) : Integer;
var
  a : TFhirRemittanceOutcome;
begin
  result := 0;
  for a := low(TFhirRemittanceOutcome) to high(TFhirRemittanceOutcome) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRemittanceOutcomeList(i : Integer) : TFhirRemittanceOutcomeList;
var
  aLoop : TFhirRemittanceOutcome;
begin
  result := [];
  for aLoop := low(TFhirRemittanceOutcome) to high(TFhirRemittanceOutcome) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirClinicalImpressionStatusListAsInteger(aSet : TFhirClinicalImpressionStatusList) : Integer;
var
  a : TFhirClinicalImpressionStatus;
begin
  result := 0;
  for a := low(TFhirClinicalImpressionStatus) to high(TFhirClinicalImpressionStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirClinicalImpressionStatusList(i : Integer) : TFhirClinicalImpressionStatusList;
var
  aLoop : TFhirClinicalImpressionStatus;
begin
  result := [];
  for aLoop := low(TFhirClinicalImpressionStatus) to high(TFhirClinicalImpressionStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirCommunicationStatusListAsInteger(aSet : TFhirCommunicationStatusList) : Integer;
var
  a : TFhirCommunicationStatus;
begin
  result := 0;
  for a := low(TFhirCommunicationStatus) to high(TFhirCommunicationStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCommunicationStatusList(i : Integer) : TFhirCommunicationStatusList;
var
  aLoop : TFhirCommunicationStatus;
begin
  result := [];
  for aLoop := low(TFhirCommunicationStatus) to high(TFhirCommunicationStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirCommunicationRequestStatusListAsInteger(aSet : TFhirCommunicationRequestStatusList) : Integer;
var
  a : TFhirCommunicationRequestStatus;
begin
  result := 0;
  for a := low(TFhirCommunicationRequestStatus) to high(TFhirCommunicationRequestStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCommunicationRequestStatusList(i : Integer) : TFhirCommunicationRequestStatusList;
var
  aLoop : TFhirCommunicationRequestStatus;
begin
  result := [];
  for aLoop := low(TFhirCommunicationRequestStatus) to high(TFhirCommunicationRequestStatus) Do
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


function TFhirV3ConfidentialityListAsInteger(aSet : TFhirV3ConfidentialityList) : Integer;
var
  a : TFhirV3Confidentiality;
begin
  result := 0;
  for a := low(TFhirV3Confidentiality) to high(TFhirV3Confidentiality) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirV3ConfidentialityList(i : Integer) : TFhirV3ConfidentialityList;
var
  aLoop : TFhirV3Confidentiality;
begin
  result := [];
  for aLoop := low(TFhirV3Confidentiality) to high(TFhirV3Confidentiality) Do
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


function TFhirConformanceResourceStatusListAsInteger(aSet : TFhirConformanceResourceStatusList) : Integer;
var
  a : TFhirConformanceResourceStatus;
begin
  result := 0;
  for a := low(TFhirConformanceResourceStatus) to high(TFhirConformanceResourceStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConformanceResourceStatusList(i : Integer) : TFhirConformanceResourceStatusList;
var
  aLoop : TFhirConformanceResourceStatus;
begin
  result := [];
  for aLoop := low(TFhirConformanceResourceStatus) to high(TFhirConformanceResourceStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirConceptMapEquivalenceListAsInteger(aSet : TFhirConceptMapEquivalenceList) : Integer;
var
  a : TFhirConceptMapEquivalence;
begin
  result := 0;
  for a := low(TFhirConceptMapEquivalence) to high(TFhirConceptMapEquivalence) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConceptMapEquivalenceList(i : Integer) : TFhirConceptMapEquivalenceList;
var
  aLoop : TFhirConceptMapEquivalence;
begin
  result := [];
  for aLoop := low(TFhirConceptMapEquivalence) to high(TFhirConceptMapEquivalence) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirValuesetConditionClinicalListAsInteger(aSet : TFhirValuesetConditionClinicalList) : Integer;
var
  a : TFhirValuesetConditionClinical;
begin
  result := 0;
  for a := low(TFhirValuesetConditionClinical) to high(TFhirValuesetConditionClinical) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirValuesetConditionClinicalList(i : Integer) : TFhirValuesetConditionClinicalList;
var
  aLoop : TFhirValuesetConditionClinical;
begin
  result := [];
  for aLoop := low(TFhirValuesetConditionClinical) to high(TFhirValuesetConditionClinical) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirConditionVerStatusListAsInteger(aSet : TFhirConditionVerStatusList) : Integer;
var
  a : TFhirConditionVerStatus;
begin
  result := 0;
  for a := low(TFhirConditionVerStatus) to high(TFhirConditionVerStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionVerStatusList(i : Integer) : TFhirConditionVerStatusList;
var
  aLoop : TFhirConditionVerStatus;
begin
  result := [];
  for aLoop := low(TFhirConditionVerStatus) to high(TFhirConditionVerStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirConformanceStatementKindListAsInteger(aSet : TFhirConformanceStatementKindList) : Integer;
var
  a : TFhirConformanceStatementKind;
begin
  result := 0;
  for a := low(TFhirConformanceStatementKind) to high(TFhirConformanceStatementKind) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConformanceStatementKindList(i : Integer) : TFhirConformanceStatementKindList;
var
  aLoop : TFhirConformanceStatementKind;
begin
  result := [];
  for aLoop := low(TFhirConformanceStatementKind) to high(TFhirConformanceStatementKind) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirUnknownContentCodeListAsInteger(aSet : TFhirUnknownContentCodeList) : Integer;
var
  a : TFhirUnknownContentCode;
begin
  result := 0;
  for a := low(TFhirUnknownContentCode) to high(TFhirUnknownContentCode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirUnknownContentCodeList(i : Integer) : TFhirUnknownContentCodeList;
var
  aLoop : TFhirUnknownContentCode;
begin
  result := [];
  for aLoop := low(TFhirUnknownContentCode) to high(TFhirUnknownContentCode) Do
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


function TFhirTypeRestfulInteractionListAsInteger(aSet : TFhirTypeRestfulInteractionList) : Integer;
var
  a : TFhirTypeRestfulInteraction;
begin
  result := 0;
  for a := low(TFhirTypeRestfulInteraction) to high(TFhirTypeRestfulInteraction) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTypeRestfulInteractionList(i : Integer) : TFhirTypeRestfulInteractionList;
var
  aLoop : TFhirTypeRestfulInteraction;
begin
  result := [];
  for aLoop := low(TFhirTypeRestfulInteraction) to high(TFhirTypeRestfulInteraction) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirVersioningPolicyListAsInteger(aSet : TFhirVersioningPolicyList) : Integer;
var
  a : TFhirVersioningPolicy;
begin
  result := 0;
  for a := low(TFhirVersioningPolicy) to high(TFhirVersioningPolicy) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirVersioningPolicyList(i : Integer) : TFhirVersioningPolicyList;
var
  aLoop : TFhirVersioningPolicy;
begin
  result := [];
  for aLoop := low(TFhirVersioningPolicy) to high(TFhirVersioningPolicy) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirConditionalDeleteStatusListAsInteger(aSet : TFhirConditionalDeleteStatusList) : Integer;
var
  a : TFhirConditionalDeleteStatus;
begin
  result := 0;
  for a := low(TFhirConditionalDeleteStatus) to high(TFhirConditionalDeleteStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionalDeleteStatusList(i : Integer) : TFhirConditionalDeleteStatusList;
var
  aLoop : TFhirConditionalDeleteStatus;
begin
  result := [];
  for aLoop := low(TFhirConditionalDeleteStatus) to high(TFhirConditionalDeleteStatus) Do
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


function TFhirSearchModifierCodeListAsInteger(aSet : TFhirSearchModifierCodeList) : Integer;
var
  a : TFhirSearchModifierCode;
begin
  result := 0;
  for a := low(TFhirSearchModifierCode) to high(TFhirSearchModifierCode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchModifierCodeList(i : Integer) : TFhirSearchModifierCodeList;
var
  aLoop : TFhirSearchModifierCode;
begin
  result := [];
  for aLoop := low(TFhirSearchModifierCode) to high(TFhirSearchModifierCode) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSystemRestfulInteractionListAsInteger(aSet : TFhirSystemRestfulInteractionList) : Integer;
var
  a : TFhirSystemRestfulInteraction;
begin
  result := 0;
  for a := low(TFhirSystemRestfulInteraction) to high(TFhirSystemRestfulInteraction) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSystemRestfulInteractionList(i : Integer) : TFhirSystemRestfulInteractionList;
var
  aLoop : TFhirSystemRestfulInteraction;
begin
  result := [];
  for aLoop := low(TFhirSystemRestfulInteraction) to high(TFhirSystemRestfulInteraction) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirTransactionModeListAsInteger(aSet : TFhirTransactionModeList) : Integer;
var
  a : TFhirTransactionMode;
begin
  result := 0;
  for a := low(TFhirTransactionMode) to high(TFhirTransactionMode) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTransactionModeList(i : Integer) : TFhirTransactionModeList;
var
  aLoop : TFhirTransactionMode;
begin
  result := [];
  for aLoop := low(TFhirTransactionMode) to high(TFhirTransactionMode) Do
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


function TFhirDataelementStringencyListAsInteger(aSet : TFhirDataelementStringencyList) : Integer;
var
  a : TFhirDataelementStringency;
begin
  result := 0;
  for a := low(TFhirDataelementStringency) to high(TFhirDataelementStringency) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDataelementStringencyList(i : Integer) : TFhirDataelementStringencyList;
var
  aLoop : TFhirDataelementStringency;
begin
  result := [];
  for aLoop := low(TFhirDataelementStringency) to high(TFhirDataelementStringency) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirDetectedissueSeverityListAsInteger(aSet : TFhirDetectedissueSeverityList) : Integer;
var
  a : TFhirDetectedissueSeverity;
begin
  result := 0;
  for a := low(TFhirDetectedissueSeverity) to high(TFhirDetectedissueSeverity) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDetectedissueSeverityList(i : Integer) : TFhirDetectedissueSeverityList;
var
  aLoop : TFhirDetectedissueSeverity;
begin
  result := [];
  for aLoop := low(TFhirDetectedissueSeverity) to high(TFhirDetectedissueSeverity) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirDevicestatusListAsInteger(aSet : TFhirDevicestatusList) : Integer;
var
  a : TFhirDevicestatus;
begin
  result := 0;
  for a := low(TFhirDevicestatus) to high(TFhirDevicestatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDevicestatusList(i : Integer) : TFhirDevicestatusList;
var
  aLoop : TFhirDevicestatus;
begin
  result := [];
  for aLoop := low(TFhirDevicestatus) to high(TFhirDevicestatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMeasurementPrincipleListAsInteger(aSet : TFhirMeasurementPrincipleList) : Integer;
var
  a : TFhirMeasurementPrinciple;
begin
  result := 0;
  for a := low(TFhirMeasurementPrinciple) to high(TFhirMeasurementPrinciple) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMeasurementPrincipleList(i : Integer) : TFhirMeasurementPrincipleList;
var
  aLoop : TFhirMeasurementPrinciple;
begin
  result := [];
  for aLoop := low(TFhirMeasurementPrinciple) to high(TFhirMeasurementPrinciple) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMetricOperationalStatusListAsInteger(aSet : TFhirMetricOperationalStatusList) : Integer;
var
  a : TFhirMetricOperationalStatus;
begin
  result := 0;
  for a := low(TFhirMetricOperationalStatus) to high(TFhirMetricOperationalStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMetricOperationalStatusList(i : Integer) : TFhirMetricOperationalStatusList;
var
  aLoop : TFhirMetricOperationalStatus;
begin
  result := [];
  for aLoop := low(TFhirMetricOperationalStatus) to high(TFhirMetricOperationalStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMetricColorListAsInteger(aSet : TFhirMetricColorList) : Integer;
var
  a : TFhirMetricColor;
begin
  result := 0;
  for a := low(TFhirMetricColor) to high(TFhirMetricColor) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMetricColorList(i : Integer) : TFhirMetricColorList;
var
  aLoop : TFhirMetricColor;
begin
  result := [];
  for aLoop := low(TFhirMetricColor) to high(TFhirMetricColor) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMetricCategoryListAsInteger(aSet : TFhirMetricCategoryList) : Integer;
var
  a : TFhirMetricCategory;
begin
  result := 0;
  for a := low(TFhirMetricCategory) to high(TFhirMetricCategory) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMetricCategoryList(i : Integer) : TFhirMetricCategoryList;
var
  aLoop : TFhirMetricCategory;
begin
  result := [];
  for aLoop := low(TFhirMetricCategory) to high(TFhirMetricCategory) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMetricCalibrationTypeListAsInteger(aSet : TFhirMetricCalibrationTypeList) : Integer;
var
  a : TFhirMetricCalibrationType;
begin
  result := 0;
  for a := low(TFhirMetricCalibrationType) to high(TFhirMetricCalibrationType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMetricCalibrationTypeList(i : Integer) : TFhirMetricCalibrationTypeList;
var
  aLoop : TFhirMetricCalibrationType;
begin
  result := [];
  for aLoop := low(TFhirMetricCalibrationType) to high(TFhirMetricCalibrationType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMetricCalibrationStateListAsInteger(aSet : TFhirMetricCalibrationStateList) : Integer;
var
  a : TFhirMetricCalibrationState;
begin
  result := 0;
  for a := low(TFhirMetricCalibrationState) to high(TFhirMetricCalibrationState) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMetricCalibrationStateList(i : Integer) : TFhirMetricCalibrationStateList;
var
  aLoop : TFhirMetricCalibrationState;
begin
  result := [];
  for aLoop := low(TFhirMetricCalibrationState) to high(TFhirMetricCalibrationState) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirDeviceUseRequestStatusListAsInteger(aSet : TFhirDeviceUseRequestStatusList) : Integer;
var
  a : TFhirDeviceUseRequestStatus;
begin
  result := 0;
  for a := low(TFhirDeviceUseRequestStatus) to high(TFhirDeviceUseRequestStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceUseRequestStatusList(i : Integer) : TFhirDeviceUseRequestStatusList;
var
  aLoop : TFhirDeviceUseRequestStatus;
begin
  result := [];
  for aLoop := low(TFhirDeviceUseRequestStatus) to high(TFhirDeviceUseRequestStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirDeviceUseRequestPriorityListAsInteger(aSet : TFhirDeviceUseRequestPriorityList) : Integer;
var
  a : TFhirDeviceUseRequestPriority;
begin
  result := 0;
  for a := low(TFhirDeviceUseRequestPriority) to high(TFhirDeviceUseRequestPriority) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceUseRequestPriorityList(i : Integer) : TFhirDeviceUseRequestPriorityList;
var
  aLoop : TFhirDeviceUseRequestPriority;
begin
  result := [];
  for aLoop := low(TFhirDeviceUseRequestPriority) to high(TFhirDeviceUseRequestPriority) Do
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


function TFhirEncounterLocationStatusListAsInteger(aSet : TFhirEncounterLocationStatusList) : Integer;
var
  a : TFhirEncounterLocationStatus;
begin
  result := 0;
  for a := low(TFhirEncounterLocationStatus) to high(TFhirEncounterLocationStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEncounterLocationStatusList(i : Integer) : TFhirEncounterLocationStatusList;
var
  aLoop : TFhirEncounterLocationStatus;
begin
  result := [];
  for aLoop := low(TFhirEncounterLocationStatus) to high(TFhirEncounterLocationStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirEpisodeOfCareStatusListAsInteger(aSet : TFhirEpisodeOfCareStatusList) : Integer;
var
  a : TFhirEpisodeOfCareStatus;
begin
  result := 0;
  for a := low(TFhirEpisodeOfCareStatus) to high(TFhirEpisodeOfCareStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEpisodeOfCareStatusList(i : Integer) : TFhirEpisodeOfCareStatusList;
var
  aLoop : TFhirEpisodeOfCareStatus;
begin
  result := [];
  for aLoop := low(TFhirEpisodeOfCareStatus) to high(TFhirEpisodeOfCareStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirHistoryStatusListAsInteger(aSet : TFhirHistoryStatusList) : Integer;
var
  a : TFhirHistoryStatus;
begin
  result := 0;
  for a := low(TFhirHistoryStatus) to high(TFhirHistoryStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirHistoryStatusList(i : Integer) : TFhirHistoryStatusList;
var
  aLoop : TFhirHistoryStatus;
begin
  result := [];
  for aLoop := low(TFhirHistoryStatus) to high(TFhirHistoryStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAdministrativeGenderListAsInteger(aSet : TFhirAdministrativeGenderList) : Integer;
var
  a : TFhirAdministrativeGender;
begin
  result := 0;
  for a := low(TFhirAdministrativeGender) to high(TFhirAdministrativeGender) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAdministrativeGenderList(i : Integer) : TFhirAdministrativeGenderList;
var
  aLoop : TFhirAdministrativeGender;
begin
  result := [];
  for aLoop := low(TFhirAdministrativeGender) to high(TFhirAdministrativeGender) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirFlagStatusListAsInteger(aSet : TFhirFlagStatusList) : Integer;
var
  a : TFhirFlagStatus;
begin
  result := 0;
  for a := low(TFhirFlagStatus) to high(TFhirFlagStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFlagStatusList(i : Integer) : TFhirFlagStatusList;
var
  aLoop : TFhirFlagStatus;
begin
  result := [];
  for aLoop := low(TFhirFlagStatus) to high(TFhirFlagStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirGoalStatusListAsInteger(aSet : TFhirGoalStatusList) : Integer;
var
  a : TFhirGoalStatus;
begin
  result := 0;
  for a := low(TFhirGoalStatus) to high(TFhirGoalStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGoalStatusList(i : Integer) : TFhirGoalStatusList;
var
  aLoop : TFhirGoalStatus;
begin
  result := [];
  for aLoop := low(TFhirGoalStatus) to high(TFhirGoalStatus) Do
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


function TFhirDaysOfWeekListAsInteger(aSet : TFhirDaysOfWeekList) : Integer;
var
  a : TFhirDaysOfWeek;
begin
  result := 0;
  for a := low(TFhirDaysOfWeek) to high(TFhirDaysOfWeek) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDaysOfWeekList(i : Integer) : TFhirDaysOfWeekList;
var
  aLoop : TFhirDaysOfWeek;
begin
  result := [];
  for aLoop := low(TFhirDaysOfWeek) to high(TFhirDaysOfWeek) Do
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


function TFhirGuideDependencyTypeListAsInteger(aSet : TFhirGuideDependencyTypeList) : Integer;
var
  a : TFhirGuideDependencyType;
begin
  result := 0;
  for a := low(TFhirGuideDependencyType) to high(TFhirGuideDependencyType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGuideDependencyTypeList(i : Integer) : TFhirGuideDependencyTypeList;
var
  aLoop : TFhirGuideDependencyType;
begin
  result := [];
  for aLoop := low(TFhirGuideDependencyType) to high(TFhirGuideDependencyType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirGuideResourcePurposeListAsInteger(aSet : TFhirGuideResourcePurposeList) : Integer;
var
  a : TFhirGuideResourcePurpose;
begin
  result := 0;
  for a := low(TFhirGuideResourcePurpose) to high(TFhirGuideResourcePurpose) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGuideResourcePurposeList(i : Integer) : TFhirGuideResourcePurposeList;
var
  aLoop : TFhirGuideResourcePurpose;
begin
  result := [];
  for aLoop := low(TFhirGuideResourcePurpose) to high(TFhirGuideResourcePurpose) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirGuidePageKindListAsInteger(aSet : TFhirGuidePageKindList) : Integer;
var
  a : TFhirGuidePageKind;
begin
  result := 0;
  for a := low(TFhirGuidePageKind) to high(TFhirGuidePageKind) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGuidePageKindList(i : Integer) : TFhirGuidePageKindList;
var
  aLoop : TFhirGuidePageKind;
begin
  result := [];
  for aLoop := low(TFhirGuidePageKind) to high(TFhirGuidePageKind) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirListStatusListAsInteger(aSet : TFhirListStatusList) : Integer;
var
  a : TFhirListStatus;
begin
  result := 0;
  for a := low(TFhirListStatus) to high(TFhirListStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirListStatusList(i : Integer) : TFhirListStatusList;
var
  aLoop : TFhirListStatus;
begin
  result := [];
  for aLoop := low(TFhirListStatus) to high(TFhirListStatus) Do
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


function TFhirDigitalMediaTypeListAsInteger(aSet : TFhirDigitalMediaTypeList) : Integer;
var
  a : TFhirDigitalMediaType;
begin
  result := 0;
  for a := low(TFhirDigitalMediaType) to high(TFhirDigitalMediaType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDigitalMediaTypeList(i : Integer) : TFhirDigitalMediaTypeList;
var
  aLoop : TFhirDigitalMediaType;
begin
  result := [];
  for aLoop := low(TFhirDigitalMediaType) to high(TFhirDigitalMediaType) Do
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


function TFhirMedicationOrderStatusListAsInteger(aSet : TFhirMedicationOrderStatusList) : Integer;
var
  a : TFhirMedicationOrderStatus;
begin
  result := 0;
  for a := low(TFhirMedicationOrderStatus) to high(TFhirMedicationOrderStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationOrderStatusList(i : Integer) : TFhirMedicationOrderStatusList;
var
  aLoop : TFhirMedicationOrderStatus;
begin
  result := [];
  for aLoop := low(TFhirMedicationOrderStatus) to high(TFhirMedicationOrderStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirMedicationStatementStatusListAsInteger(aSet : TFhirMedicationStatementStatusList) : Integer;
var
  a : TFhirMedicationStatementStatus;
begin
  result := 0;
  for a := low(TFhirMedicationStatementStatus) to high(TFhirMedicationStatementStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationStatementStatusList(i : Integer) : TFhirMedicationStatementStatusList;
var
  aLoop : TFhirMedicationStatementStatus;
begin
  result := [];
  for aLoop := low(TFhirMedicationStatementStatus) to high(TFhirMedicationStatementStatus) Do
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


function TFhirNamingsystemTypeListAsInteger(aSet : TFhirNamingsystemTypeList) : Integer;
var
  a : TFhirNamingsystemType;
begin
  result := 0;
  for a := low(TFhirNamingsystemType) to high(TFhirNamingsystemType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNamingsystemTypeList(i : Integer) : TFhirNamingsystemTypeList;
var
  aLoop : TFhirNamingsystemType;
begin
  result := [];
  for aLoop := low(TFhirNamingsystemType) to high(TFhirNamingsystemType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirNamingsystemIdentifierTypeListAsInteger(aSet : TFhirNamingsystemIdentifierTypeList) : Integer;
var
  a : TFhirNamingsystemIdentifierType;
begin
  result := 0;
  for a := low(TFhirNamingsystemIdentifierType) to high(TFhirNamingsystemIdentifierType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNamingsystemIdentifierTypeList(i : Integer) : TFhirNamingsystemIdentifierTypeList;
var
  aLoop : TFhirNamingsystemIdentifierType;
begin
  result := [];
  for aLoop := low(TFhirNamingsystemIdentifierType) to high(TFhirNamingsystemIdentifierType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirNutritionOrderStatusListAsInteger(aSet : TFhirNutritionOrderStatusList) : Integer;
var
  a : TFhirNutritionOrderStatus;
begin
  result := 0;
  for a := low(TFhirNutritionOrderStatus) to high(TFhirNutritionOrderStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNutritionOrderStatusList(i : Integer) : TFhirNutritionOrderStatusList;
var
  aLoop : TFhirNutritionOrderStatus;
begin
  result := [];
  for aLoop := low(TFhirNutritionOrderStatus) to high(TFhirNutritionOrderStatus) Do
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


function TFhirOperationKindListAsInteger(aSet : TFhirOperationKindList) : Integer;
var
  a : TFhirOperationKind;
begin
  result := 0;
  for a := low(TFhirOperationKind) to high(TFhirOperationKind) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOperationKindList(i : Integer) : TFhirOperationKindList;
var
  aLoop : TFhirOperationKind;
begin
  result := [];
  for aLoop := low(TFhirOperationKind) to high(TFhirOperationKind) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirOperationParameterUseListAsInteger(aSet : TFhirOperationParameterUseList) : Integer;
var
  a : TFhirOperationParameterUse;
begin
  result := 0;
  for a := low(TFhirOperationParameterUse) to high(TFhirOperationParameterUse) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOperationParameterUseList(i : Integer) : TFhirOperationParameterUseList;
var
  aLoop : TFhirOperationParameterUse;
begin
  result := [];
  for aLoop := low(TFhirOperationParameterUse) to high(TFhirOperationParameterUse) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirValuesetOperationParameterTypeListAsInteger(aSet : TFhirValuesetOperationParameterTypeList) : Integer;
var
  a : TFhirValuesetOperationParameterType;
begin
  result := 0;
  for a := low(TFhirValuesetOperationParameterType) to high(TFhirValuesetOperationParameterType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirValuesetOperationParameterTypeList(i : Integer) : TFhirValuesetOperationParameterTypeList;
var
  aLoop : TFhirValuesetOperationParameterType;
begin
  result := [];
  for aLoop := low(TFhirValuesetOperationParameterType) to high(TFhirValuesetOperationParameterType) Do
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


function TFhirIssueTypeListAsInteger(aSet : TFhirIssueTypeList) : Integer;
var
  a : TFhirIssueType;
begin
  result := 0;
  for a := low(TFhirIssueType) to high(TFhirIssueType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIssueTypeList(i : Integer) : TFhirIssueTypeList;
var
  aLoop : TFhirIssueType;
begin
  result := [];
  for aLoop := low(TFhirIssueType) to high(TFhirIssueType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirOrderStatusListAsInteger(aSet : TFhirOrderStatusList) : Integer;
var
  a : TFhirOrderStatus;
begin
  result := 0;
  for a := low(TFhirOrderStatus) to high(TFhirOrderStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOrderStatusList(i : Integer) : TFhirOrderStatusList;
var
  aLoop : TFhirOrderStatus;
begin
  result := [];
  for aLoop := low(TFhirOrderStatus) to high(TFhirOrderStatus) Do
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


function TFhirIdentityAssuranceLevelListAsInteger(aSet : TFhirIdentityAssuranceLevelList) : Integer;
var
  a : TFhirIdentityAssuranceLevel;
begin
  result := 0;
  for a := low(TFhirIdentityAssuranceLevel) to high(TFhirIdentityAssuranceLevel) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIdentityAssuranceLevelList(i : Integer) : TFhirIdentityAssuranceLevelList;
var
  aLoop : TFhirIdentityAssuranceLevel;
begin
  result := [];
  for aLoop := low(TFhirIdentityAssuranceLevel) to high(TFhirIdentityAssuranceLevel) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirProcedureStatusListAsInteger(aSet : TFhirProcedureStatusList) : Integer;
var
  a : TFhirProcedureStatus;
begin
  result := 0;
  for a := low(TFhirProcedureStatus) to high(TFhirProcedureStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProcedureStatusList(i : Integer) : TFhirProcedureStatusList;
var
  aLoop : TFhirProcedureStatus;
begin
  result := [];
  for aLoop := low(TFhirProcedureStatus) to high(TFhirProcedureStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirProcedureRequestStatusListAsInteger(aSet : TFhirProcedureRequestStatusList) : Integer;
var
  a : TFhirProcedureRequestStatus;
begin
  result := 0;
  for a := low(TFhirProcedureRequestStatus) to high(TFhirProcedureRequestStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProcedureRequestStatusList(i : Integer) : TFhirProcedureRequestStatusList;
var
  aLoop : TFhirProcedureRequestStatus;
begin
  result := [];
  for aLoop := low(TFhirProcedureRequestStatus) to high(TFhirProcedureRequestStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirProcedureRequestPriorityListAsInteger(aSet : TFhirProcedureRequestPriorityList) : Integer;
var
  a : TFhirProcedureRequestPriority;
begin
  result := 0;
  for a := low(TFhirProcedureRequestPriority) to high(TFhirProcedureRequestPriority) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProcedureRequestPriorityList(i : Integer) : TFhirProcedureRequestPriorityList;
var
  aLoop : TFhirProcedureRequestPriority;
begin
  result := [];
  for aLoop := low(TFhirProcedureRequestPriority) to high(TFhirProcedureRequestPriority) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirActionlistListAsInteger(aSet : TFhirActionlistList) : Integer;
var
  a : TFhirActionlist;
begin
  result := 0;
  for a := low(TFhirActionlist) to high(TFhirActionlist) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionlistList(i : Integer) : TFhirActionlistList;
var
  aLoop : TFhirActionlist;
begin
  result := [];
  for aLoop := low(TFhirActionlist) to high(TFhirActionlist) Do
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


function TFhirAnswerFormatListAsInteger(aSet : TFhirAnswerFormatList) : Integer;
var
  a : TFhirAnswerFormat;
begin
  result := 0;
  for a := low(TFhirAnswerFormat) to high(TFhirAnswerFormat) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAnswerFormatList(i : Integer) : TFhirAnswerFormatList;
var
  aLoop : TFhirAnswerFormat;
begin
  result := [];
  for aLoop := low(TFhirAnswerFormat) to high(TFhirAnswerFormat) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirQuestionnaireAnswersStatusListAsInteger(aSet : TFhirQuestionnaireAnswersStatusList) : Integer;
var
  a : TFhirQuestionnaireAnswersStatus;
begin
  result := 0;
  for a := low(TFhirQuestionnaireAnswersStatus) to high(TFhirQuestionnaireAnswersStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireAnswersStatusList(i : Integer) : TFhirQuestionnaireAnswersStatusList;
var
  aLoop : TFhirQuestionnaireAnswersStatus;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireAnswersStatus) to high(TFhirQuestionnaireAnswersStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirReferralstatusListAsInteger(aSet : TFhirReferralstatusList) : Integer;
var
  a : TFhirReferralstatus;
begin
  result := 0;
  for a := low(TFhirReferralstatus) to high(TFhirReferralstatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReferralstatusList(i : Integer) : TFhirReferralstatusList;
var
  aLoop : TFhirReferralstatus;
begin
  result := [];
  for aLoop := low(TFhirReferralstatus) to high(TFhirReferralstatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSearchXpathUsageListAsInteger(aSet : TFhirSearchXpathUsageList) : Integer;
var
  a : TFhirSearchXpathUsage;
begin
  result := 0;
  for a := low(TFhirSearchXpathUsage) to high(TFhirSearchXpathUsage) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchXpathUsageList(i : Integer) : TFhirSearchXpathUsageList;
var
  aLoop : TFhirSearchXpathUsage;
begin
  result := [];
  for aLoop := low(TFhirSearchXpathUsage) to high(TFhirSearchXpathUsage) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSlotstatusListAsInteger(aSet : TFhirSlotstatusList) : Integer;
var
  a : TFhirSlotstatus;
begin
  result := 0;
  for a := low(TFhirSlotstatus) to high(TFhirSlotstatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSlotstatusList(i : Integer) : TFhirSlotstatusList;
var
  aLoop : TFhirSlotstatus;
begin
  result := [];
  for aLoop := low(TFhirSlotstatus) to high(TFhirSlotstatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSpecimenStatusListAsInteger(aSet : TFhirSpecimenStatusList) : Integer;
var
  a : TFhirSpecimenStatus;
begin
  result := 0;
  for a := low(TFhirSpecimenStatus) to high(TFhirSpecimenStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSpecimenStatusList(i : Integer) : TFhirSpecimenStatusList;
var
  aLoop : TFhirSpecimenStatus;
begin
  result := [];
  for aLoop := low(TFhirSpecimenStatus) to high(TFhirSpecimenStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirStructureDefinitionKindListAsInteger(aSet : TFhirStructureDefinitionKindList) : Integer;
var
  a : TFhirStructureDefinitionKind;
begin
  result := 0;
  for a := low(TFhirStructureDefinitionKind) to high(TFhirStructureDefinitionKind) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureDefinitionKindList(i : Integer) : TFhirStructureDefinitionKindList;
var
  aLoop : TFhirStructureDefinitionKind;
begin
  result := [];
  for aLoop := low(TFhirStructureDefinitionKind) to high(TFhirStructureDefinitionKind) Do
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


function TFhirSubscriptionStatusListAsInteger(aSet : TFhirSubscriptionStatusList) : Integer;
var
  a : TFhirSubscriptionStatus;
begin
  result := 0;
  for a := low(TFhirSubscriptionStatus) to high(TFhirSubscriptionStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionStatusList(i : Integer) : TFhirSubscriptionStatusList;
var
  aLoop : TFhirSubscriptionStatus;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionStatus) to high(TFhirSubscriptionStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSubscriptionChannelTypeListAsInteger(aSet : TFhirSubscriptionChannelTypeList) : Integer;
var
  a : TFhirSubscriptionChannelType;
begin
  result := 0;
  for a := low(TFhirSubscriptionChannelType) to high(TFhirSubscriptionChannelType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionChannelTypeList(i : Integer) : TFhirSubscriptionChannelTypeList;
var
  aLoop : TFhirSubscriptionChannelType;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionChannelType) to high(TFhirSubscriptionChannelType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSupplydeliveryStatusListAsInteger(aSet : TFhirSupplydeliveryStatusList) : Integer;
var
  a : TFhirSupplydeliveryStatus;
begin
  result := 0;
  for a := low(TFhirSupplydeliveryStatus) to high(TFhirSupplydeliveryStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSupplydeliveryStatusList(i : Integer) : TFhirSupplydeliveryStatusList;
var
  aLoop : TFhirSupplydeliveryStatus;
begin
  result := [];
  for aLoop := low(TFhirSupplydeliveryStatus) to high(TFhirSupplydeliveryStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirSupplyrequestStatusListAsInteger(aSet : TFhirSupplyrequestStatusList) : Integer;
var
  a : TFhirSupplyrequestStatus;
begin
  result := 0;
  for a := low(TFhirSupplyrequestStatus) to high(TFhirSupplyrequestStatus) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSupplyrequestStatusList(i : Integer) : TFhirSupplyrequestStatusList;
var
  aLoop : TFhirSupplyrequestStatus;
begin
  result := [];
  for aLoop := low(TFhirSupplyrequestStatus) to high(TFhirSupplyrequestStatus) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirContentTypeListAsInteger(aSet : TFhirContentTypeList) : Integer;
var
  a : TFhirContentType;
begin
  result := 0;
  for a := low(TFhirContentType) to high(TFhirContentType) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContentTypeList(i : Integer) : TFhirContentTypeList;
var
  aLoop : TFhirContentType;
begin
  result := [];
  for aLoop := low(TFhirContentType) to high(TFhirContentType) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAssertDirectionCodesListAsInteger(aSet : TFhirAssertDirectionCodesList) : Integer;
var
  a : TFhirAssertDirectionCodes;
begin
  result := 0;
  for a := low(TFhirAssertDirectionCodes) to high(TFhirAssertDirectionCodes) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertDirectionCodesList(i : Integer) : TFhirAssertDirectionCodesList;
var
  aLoop : TFhirAssertDirectionCodes;
begin
  result := [];
  for aLoop := low(TFhirAssertDirectionCodes) to high(TFhirAssertDirectionCodes) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAssertOperatorCodesListAsInteger(aSet : TFhirAssertOperatorCodesList) : Integer;
var
  a : TFhirAssertOperatorCodes;
begin
  result := 0;
  for a := low(TFhirAssertOperatorCodes) to high(TFhirAssertOperatorCodes) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertOperatorCodesList(i : Integer) : TFhirAssertOperatorCodesList;
var
  aLoop : TFhirAssertOperatorCodes;
begin
  result := [];
  for aLoop := low(TFhirAssertOperatorCodes) to high(TFhirAssertOperatorCodes) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirAssertResponseCodeTypesListAsInteger(aSet : TFhirAssertResponseCodeTypesList) : Integer;
var
  a : TFhirAssertResponseCodeTypes;
begin
  result := 0;
  for a := low(TFhirAssertResponseCodeTypes) to high(TFhirAssertResponseCodeTypes) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertResponseCodeTypesList(i : Integer) : TFhirAssertResponseCodeTypesList;
var
  aLoop : TFhirAssertResponseCodeTypes;
begin
  result := [];
  for aLoop := low(TFhirAssertResponseCodeTypes) to high(TFhirAssertResponseCodeTypes) Do
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


function TFhirVisionEyeCodesListAsInteger(aSet : TFhirVisionEyeCodesList) : Integer;
var
  a : TFhirVisionEyeCodes;
begin
  result := 0;
  for a := low(TFhirVisionEyeCodes) to high(TFhirVisionEyeCodes) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirVisionEyeCodesList(i : Integer) : TFhirVisionEyeCodesList;
var
  aLoop : TFhirVisionEyeCodes;
begin
  result := [];
  for aLoop := low(TFhirVisionEyeCodes) to high(TFhirVisionEyeCodes) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


function TFhirVisionBaseCodesListAsInteger(aSet : TFhirVisionBaseCodesList) : Integer;
var
  a : TFhirVisionBaseCodes;
begin
  result := 0;
  for a := low(TFhirVisionBaseCodes) to high(TFhirVisionBaseCodes) do
  begin
    assert(ord(a) < 32);
    if a in aSet then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirVisionBaseCodesList(i : Integer) : TFhirVisionBaseCodesList;
var
  aLoop : TFhirVisionBaseCodes;
begin
  result := [];
  for aLoop := low(TFhirVisionBaseCodes) to high(TFhirVisionBaseCodes) Do
  begin
    assert(ord(aLoop) < 32);
    if i and (1 shl (ord(aLoop))) > 0 Then
      result := result + [aLoop];
  end;
 end;


end.

