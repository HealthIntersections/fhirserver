{!Wrapper uses FHIRBase, FHIRBase_Wrapper, FHIRTypes, FHIRTypes_Wrapper, FHIRResources, FHIRResources_Wrapper}
{!ignore ALL_RESOURCE_TYPES}

unit FHIRConstants;

{$I fhir.inc}

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

{$IFNDEF FHIR2}
This is the dstu2 version of the FHIR code
{$ENDIF}


interface

// FHIR v1.0.2 generated 2015-10-24T07:41:03+11:00

uses
  SysUtils, Classes, StringSupport, DecimalSupport, AdvBuffers, DateSupport, FHIRBase, FHIRTypes, FHIRResources;

Type
{$IFDEF FHIR_ACCOUNT}
  {@Enum TSearchParamsAccount
    Search Parameters for Account
  }
  TSearchParamsAccount = (
    spAccount__content, {@enum.value "_content" spAccount__content Search on the entire content of the resource }
    spAccount__id, {@enum.value "_id" spAccount__id Logical id of this artifact }
    spAccount__lastUpdated, {@enum.value "_lastUpdated" spAccount__lastUpdated When the resource version last changed }
    spAccount__profile, {@enum.value "_profile" spAccount__profile Profiles this resource claims to conform to }
    spAccount__query, {@enum.value "_query" spAccount__query A custom search profile that describes a specific defined query operation }
    spAccount__security, {@enum.value "_security" spAccount__security Security Labels applied to this resource }
    spAccount__tag, {@enum.value "_tag" spAccount__tag Tags applied to this resource }
    spAccount__text, {@enum.value "_text" spAccount__text Search on the narrative of the resource }
    spAccount_Balance, {@enum.value "balance" spAccount_Balance How much is in account? }
    spAccount_Identifier, {@enum.value "identifier" spAccount_Identifier Account number }
    spAccount_Name, {@enum.value "name" spAccount_Name Human-readable label }
    spAccount_Owner, {@enum.value "owner" spAccount_Owner Who is responsible? }
    spAccount_Patient, {@enum.value "patient" spAccount_Patient What is account tied to? }
    spAccount_Period, {@enum.value "period" spAccount_Period Transaction window }
    spAccount_Status, {@enum.value "status" spAccount_Status active | inactive }
    spAccount_Subject, {@enum.value "subject" spAccount_Subject What is account tied to? }
    spAccount_Type); {@enum.value "type" spAccount_Type E.g. patient, expense, depreciation }
{$ENDIF}

{$IFDEF FHIR_ALLERGYINTOLERANCE}
  {@Enum TSearchParamsAllergyIntolerance
    Search Parameters for AllergyIntolerance
  }
  TSearchParamsAllergyIntolerance = (
    spAllergyIntolerance__content, {@enum.value "_content" spAllergyIntolerance__content Search on the entire content of the resource }
    spAllergyIntolerance__id, {@enum.value "_id" spAllergyIntolerance__id Logical id of this artifact }
    spAllergyIntolerance__lastUpdated, {@enum.value "_lastUpdated" spAllergyIntolerance__lastUpdated When the resource version last changed }
    spAllergyIntolerance__profile, {@enum.value "_profile" spAllergyIntolerance__profile Profiles this resource claims to conform to }
    spAllergyIntolerance__query, {@enum.value "_query" spAllergyIntolerance__query A custom search profile that describes a specific defined query operation }
    spAllergyIntolerance__security, {@enum.value "_security" spAllergyIntolerance__security Security Labels applied to this resource }
    spAllergyIntolerance__tag, {@enum.value "_tag" spAllergyIntolerance__tag Tags applied to this resource }
    spAllergyIntolerance__text, {@enum.value "_text" spAllergyIntolerance__text Search on the narrative of the resource }
    spAllergyIntolerance_Category, {@enum.value "category" spAllergyIntolerance_Category food | medication | environment | other - Category of Substance }
    spAllergyIntolerance_Criticality, {@enum.value "criticality" spAllergyIntolerance_Criticality CRITL | CRITH | CRITU }
    spAllergyIntolerance_Date, {@enum.value "date" spAllergyIntolerance_Date When recorded }
    spAllergyIntolerance_Identifier, {@enum.value "identifier" spAllergyIntolerance_Identifier External ids for this item }
    spAllergyIntolerance_Lastdate, {@enum.value "last-date" spAllergyIntolerance_Lastdate Date(/time) of last known occurrence of a reaction }
    spAllergyIntolerance_Manifestation, {@enum.value "manifestation" spAllergyIntolerance_Manifestation Clinical symptoms/signs associated with the Event }
    spAllergyIntolerance_Onset, {@enum.value "onset" spAllergyIntolerance_Onset Date(/time) when manifestations showed }
    spAllergyIntolerance_Patient, {@enum.value "patient" spAllergyIntolerance_Patient Who the sensitivity is for }
    spAllergyIntolerance_Recorder, {@enum.value "recorder" spAllergyIntolerance_Recorder Who recorded the sensitivity }
    spAllergyIntolerance_Reporter, {@enum.value "reporter" spAllergyIntolerance_Reporter Source of the information about the allergy }
    spAllergyIntolerance_Route, {@enum.value "route" spAllergyIntolerance_Route How the subject was exposed to the substance }
    spAllergyIntolerance_Severity, {@enum.value "severity" spAllergyIntolerance_Severity mild | moderate | severe (of event as a whole) }
    spAllergyIntolerance_Status, {@enum.value "status" spAllergyIntolerance_Status active | unconfirmed | confirmed | inactive | resolved | refuted | entered-in-error }
    spAllergyIntolerance_Substance, {@enum.value "substance" spAllergyIntolerance_Substance Substance, (or class) considered to be responsible for risk }
    spAllergyIntolerance_Type); {@enum.value "type" spAllergyIntolerance_Type allergy | intolerance - Underlying mechanism (if known) }
{$ENDIF}

{$IFDEF FHIR_APPOINTMENT}
  {@Enum TSearchParamsAppointment
    Search Parameters for Appointment
  }
  TSearchParamsAppointment = (
    spAppointment__content, {@enum.value "_content" spAppointment__content Search on the entire content of the resource }
    spAppointment__id, {@enum.value "_id" spAppointment__id Logical id of this artifact }
    spAppointment__lastUpdated, {@enum.value "_lastUpdated" spAppointment__lastUpdated When the resource version last changed }
    spAppointment__profile, {@enum.value "_profile" spAppointment__profile Profiles this resource claims to conform to }
    spAppointment__query, {@enum.value "_query" spAppointment__query A custom search profile that describes a specific defined query operation }
    spAppointment__security, {@enum.value "_security" spAppointment__security Security Labels applied to this resource }
    spAppointment__tag, {@enum.value "_tag" spAppointment__tag Tags applied to this resource }
    spAppointment__text, {@enum.value "_text" spAppointment__text Search on the narrative of the resource }
    spAppointment_Actor, {@enum.value "actor" spAppointment_Actor Any one of the individuals participating in the appointment }
    spAppointment_Date, {@enum.value "date" spAppointment_Date Appointment date/time. }
    spAppointment_Identifier, {@enum.value "identifier" spAppointment_Identifier An Identifier of the Appointment }
    spAppointment_Location, {@enum.value "location" spAppointment_Location This location is listed in the participants of the appointment }
    spAppointment_Partstatus, {@enum.value "part-status" spAppointment_Partstatus The Participation status of the subject, or other participant on the appointment. Can be used to locate participants that have not responded to meeting requests. }
    spAppointment_Patient, {@enum.value "patient" spAppointment_Patient One of the individuals of the appointment is this patient }
    spAppointment_Practitioner, {@enum.value "practitioner" spAppointment_Practitioner One of the individuals of the appointment is this practitioner }
    spAppointment_Status); {@enum.value "status" spAppointment_Status The overall status of the appointment }
{$ENDIF}

{$IFDEF FHIR_APPOINTMENTRESPONSE}
  {@Enum TSearchParamsAppointmentResponse
    Search Parameters for AppointmentResponse
  }
  TSearchParamsAppointmentResponse = (
    spAppointmentResponse__content, {@enum.value "_content" spAppointmentResponse__content Search on the entire content of the resource }
    spAppointmentResponse__id, {@enum.value "_id" spAppointmentResponse__id Logical id of this artifact }
    spAppointmentResponse__lastUpdated, {@enum.value "_lastUpdated" spAppointmentResponse__lastUpdated When the resource version last changed }
    spAppointmentResponse__profile, {@enum.value "_profile" spAppointmentResponse__profile Profiles this resource claims to conform to }
    spAppointmentResponse__query, {@enum.value "_query" spAppointmentResponse__query A custom search profile that describes a specific defined query operation }
    spAppointmentResponse__security, {@enum.value "_security" spAppointmentResponse__security Security Labels applied to this resource }
    spAppointmentResponse__tag, {@enum.value "_tag" spAppointmentResponse__tag Tags applied to this resource }
    spAppointmentResponse__text, {@enum.value "_text" spAppointmentResponse__text Search on the narrative of the resource }
    spAppointmentResponse_Actor, {@enum.value "actor" spAppointmentResponse_Actor The Person, Location/HealthcareService or Device that this appointment response replies for }
    spAppointmentResponse_Appointment, {@enum.value "appointment" spAppointmentResponse_Appointment The appointment that the response is attached to }
    spAppointmentResponse_Identifier, {@enum.value "identifier" spAppointmentResponse_Identifier An Identifier in this appointment response }
    spAppointmentResponse_Location, {@enum.value "location" spAppointmentResponse_Location This Response is for this Location }
    spAppointmentResponse_Partstatus, {@enum.value "part-status" spAppointmentResponse_Partstatus The participants acceptance status for this appointment }
    spAppointmentResponse_Patient, {@enum.value "patient" spAppointmentResponse_Patient This Response is for this Patient }
    spAppointmentResponse_Practitioner); {@enum.value "practitioner" spAppointmentResponse_Practitioner This Response is for this Practitioner }
{$ENDIF}

{$IFDEF FHIR_AUDITEVENT}
  {@Enum TSearchParamsAuditEvent
    Search Parameters for AuditEvent
  }
  TSearchParamsAuditEvent = (
    spAuditEvent__content, {@enum.value "_content" spAuditEvent__content Search on the entire content of the resource }
    spAuditEvent__id, {@enum.value "_id" spAuditEvent__id Logical id of this artifact }
    spAuditEvent__lastUpdated, {@enum.value "_lastUpdated" spAuditEvent__lastUpdated When the resource version last changed }
    spAuditEvent__profile, {@enum.value "_profile" spAuditEvent__profile Profiles this resource claims to conform to }
    spAuditEvent__query, {@enum.value "_query" spAuditEvent__query A custom search profile that describes a specific defined query operation }
    spAuditEvent__security, {@enum.value "_security" spAuditEvent__security Security Labels applied to this resource }
    spAuditEvent__tag, {@enum.value "_tag" spAuditEvent__tag Tags applied to this resource }
    spAuditEvent__text, {@enum.value "_text" spAuditEvent__text Search on the narrative of the resource }
    spAuditEvent_Action, {@enum.value "action" spAuditEvent_Action Type of action performed during the event }
    spAuditEvent_Address, {@enum.value "address" spAuditEvent_Address Identifier for the network access point of the user device }
    spAuditEvent_Altid, {@enum.value "altid" spAuditEvent_Altid Alternative User id e.g. authentication }
    spAuditEvent_Date, {@enum.value "date" spAuditEvent_Date Time when the event occurred on source }
    spAuditEvent_Desc, {@enum.value "desc" spAuditEvent_Desc Instance-specific descriptor for Object }
    spAuditEvent_Identity, {@enum.value "identity" spAuditEvent_Identity Specific instance of object (e.g. versioned) }
    spAuditEvent_Name, {@enum.value "name" spAuditEvent_Name Human-meaningful name for the user }
    spAuditEvent_Objecttype, {@enum.value "object-type" spAuditEvent_Objecttype Type of object involved }
    spAuditEvent_Participant, {@enum.value "participant" spAuditEvent_Participant Direct reference to resource }
    spAuditEvent_Patient, {@enum.value "patient" spAuditEvent_Patient Direct reference to resource }
    spAuditEvent_Policy, {@enum.value "policy" spAuditEvent_Policy Policy that authorized event }
    spAuditEvent_Reference, {@enum.value "reference" spAuditEvent_Reference Specific instance of resource (e.g. versioned) }
    spAuditEvent_Site, {@enum.value "site" spAuditEvent_Site Logical source location within the enterprise }
    spAuditEvent_Source, {@enum.value "source" spAuditEvent_Source The identity of source detecting the event }
    spAuditEvent_Subtype, {@enum.value "subtype" spAuditEvent_Subtype More specific type/id for the event }
    spAuditEvent_Type, {@enum.value "type" spAuditEvent_Type Type/identifier of event }
    spAuditEvent_User); {@enum.value "user" spAuditEvent_User Unique identifier for the user }
{$ENDIF}

{$IFDEF FHIR_BASIC}
  {@Enum TSearchParamsBasic
    Search Parameters for Basic
  }
  TSearchParamsBasic = (
    spBasic__content, {@enum.value "_content" spBasic__content Search on the entire content of the resource }
    spBasic__id, {@enum.value "_id" spBasic__id Logical id of this artifact }
    spBasic__lastUpdated, {@enum.value "_lastUpdated" spBasic__lastUpdated When the resource version last changed }
    spBasic__profile, {@enum.value "_profile" spBasic__profile Profiles this resource claims to conform to }
    spBasic__query, {@enum.value "_query" spBasic__query A custom search profile that describes a specific defined query operation }
    spBasic__security, {@enum.value "_security" spBasic__security Security Labels applied to this resource }
    spBasic__tag, {@enum.value "_tag" spBasic__tag Tags applied to this resource }
    spBasic__text, {@enum.value "_text" spBasic__text Search on the narrative of the resource }
    spBasic_Author, {@enum.value "author" spBasic_Author Who created }
    spBasic_Code, {@enum.value "code" spBasic_Code Kind of Resource }
    spBasic_Created, {@enum.value "created" spBasic_Created When created }
    spBasic_Identifier, {@enum.value "identifier" spBasic_Identifier Business identifier }
    spBasic_Patient, {@enum.value "patient" spBasic_Patient Identifies the focus of this resource }
    spBasic_Subject); {@enum.value "subject" spBasic_Subject Identifies the focus of this resource }
{$ENDIF}

{$IFDEF FHIR_BINARY}
  {@Enum TSearchParamsBinary
    Search Parameters for Binary
  }
  TSearchParamsBinary = (
    spBinary__content, {@enum.value "_content" spBinary__content Search on the entire content of the resource }
    spBinary__id, {@enum.value "_id" spBinary__id Logical id of this artifact }
    spBinary__lastUpdated, {@enum.value "_lastUpdated" spBinary__lastUpdated When the resource version last changed }
    spBinary__profile, {@enum.value "_profile" spBinary__profile Profiles this resource claims to conform to }
    spBinary__query, {@enum.value "_query" spBinary__query A custom search profile that describes a specific defined query operation }
    spBinary__security, {@enum.value "_security" spBinary__security Security Labels applied to this resource }
    spBinary__tag, {@enum.value "_tag" spBinary__tag Tags applied to this resource }
    spBinary__text, {@enum.value "_text" spBinary__text Search on the narrative of the resource }
    spBinary_Contenttype); {@enum.value "contenttype" spBinary_Contenttype MimeType of the binary content }
{$ENDIF}

{$IFDEF FHIR_BODYSITE}
  {@Enum TSearchParamsBodySite
    Search Parameters for BodySite
  }
  TSearchParamsBodySite = (
    spBodySite__content, {@enum.value "_content" spBodySite__content Search on the entire content of the resource }
    spBodySite__id, {@enum.value "_id" spBodySite__id Logical id of this artifact }
    spBodySite__lastUpdated, {@enum.value "_lastUpdated" spBodySite__lastUpdated When the resource version last changed }
    spBodySite__profile, {@enum.value "_profile" spBodySite__profile Profiles this resource claims to conform to }
    spBodySite__query, {@enum.value "_query" spBodySite__query A custom search profile that describes a specific defined query operation }
    spBodySite__security, {@enum.value "_security" spBodySite__security Security Labels applied to this resource }
    spBodySite__tag, {@enum.value "_tag" spBodySite__tag Tags applied to this resource }
    spBodySite__text, {@enum.value "_text" spBodySite__text Search on the narrative of the resource }
    spBodySite_Code, {@enum.value "code" spBodySite_Code Named anatomical location }
    spBodySite_Identifier, {@enum.value "identifier" spBodySite_Identifier Identifier for this instance of the anatomical location }
    spBodySite_Patient); {@enum.value "patient" spBodySite_Patient Patient to whom bodysite belongs }
{$ENDIF}

{$IFDEF FHIR_BUNDLE}
  {@Enum TSearchParamsBundle
    Search Parameters for Bundle
  }
  TSearchParamsBundle = (
    spBundle__content, {@enum.value "_content" spBundle__content Search on the entire content of the resource }
    spBundle__id, {@enum.value "_id" spBundle__id Logical id of this artifact }
    spBundle__lastUpdated, {@enum.value "_lastUpdated" spBundle__lastUpdated When the resource version last changed }
    spBundle__profile, {@enum.value "_profile" spBundle__profile Profiles this resource claims to conform to }
    spBundle__query, {@enum.value "_query" spBundle__query A custom search profile that describes a specific defined query operation }
    spBundle__security, {@enum.value "_security" spBundle__security Security Labels applied to this resource }
    spBundle__tag, {@enum.value "_tag" spBundle__tag Tags applied to this resource }
    spBundle__text, {@enum.value "_text" spBundle__text Search on the narrative of the resource }
    spBundle_Composition, {@enum.value "composition" spBundle_Composition The first resource in the bundle, if the bundle type is "document" - this is a composition, and this parameter provides access to searches its contents }
    spBundle_Message, {@enum.value "message" spBundle_Message The first resource in the bundle, if the bundle type is "message" - this is a message header, and this parameter provides access to search its contents }
    spBundle_Type); {@enum.value "type" spBundle_Type document | message | transaction | transaction-response | batch | batch-response | history | searchset | collection }
{$ENDIF}

{$IFDEF FHIR_CAREPLAN}
  {@Enum TSearchParamsCarePlan
    Search Parameters for CarePlan
  }
  TSearchParamsCarePlan = (
    spCarePlan__content, {@enum.value "_content" spCarePlan__content Search on the entire content of the resource }
    spCarePlan__id, {@enum.value "_id" spCarePlan__id Logical id of this artifact }
    spCarePlan__lastUpdated, {@enum.value "_lastUpdated" spCarePlan__lastUpdated When the resource version last changed }
    spCarePlan__profile, {@enum.value "_profile" spCarePlan__profile Profiles this resource claims to conform to }
    spCarePlan__query, {@enum.value "_query" spCarePlan__query A custom search profile that describes a specific defined query operation }
    spCarePlan__security, {@enum.value "_security" spCarePlan__security Security Labels applied to this resource }
    spCarePlan__tag, {@enum.value "_tag" spCarePlan__tag Tags applied to this resource }
    spCarePlan__text, {@enum.value "_text" spCarePlan__text Search on the narrative of the resource }
    spCarePlan_Activitycode, {@enum.value "activitycode" spCarePlan_Activitycode Detail type of activity }
    spCarePlan_Activitydate, {@enum.value "activitydate" spCarePlan_Activitydate Specified date occurs within period specified by CarePlan.activity.timingSchedule }
    spCarePlan_Activityreference, {@enum.value "activityreference" spCarePlan_Activityreference Activity details defined in specific resource }
    spCarePlan_Condition, {@enum.value "condition" spCarePlan_Condition Health issues this plan addresses }
    spCarePlan_Date, {@enum.value "date" spCarePlan_Date Time period plan covers }
    spCarePlan_Goal, {@enum.value "goal" spCarePlan_Goal Desired outcome of plan }
    spCarePlan_Participant, {@enum.value "participant" spCarePlan_Participant Who is involved }
    spCarePlan_Patient, {@enum.value "patient" spCarePlan_Patient Who care plan is for }
    spCarePlan_Performer, {@enum.value "performer" spCarePlan_Performer Matches if the practitioner is listed as a performer in any of the "simple" activities.  (For performers of the detailed activities, chain through the activitydetail search parameter.) }
    spCarePlan_Related, {@enum.value "related" spCarePlan_Related A combination of the type of relationship and the related plan }
    spCarePlan_Relatedcode, {@enum.value "relatedcode" spCarePlan_Relatedcode includes | replaces | fulfills }
    spCarePlan_Relatedplan, {@enum.value "relatedplan" spCarePlan_Relatedplan Plan relationship exists with }
    spCarePlan_Subject); {@enum.value "subject" spCarePlan_Subject Who care plan is for }
{$ENDIF}

{$IFDEF FHIR_CLAIM}
  {@Enum TSearchParamsClaim
    Search Parameters for Claim
  }
  TSearchParamsClaim = (
    spClaim__content, {@enum.value "_content" spClaim__content Search on the entire content of the resource }
    spClaim__id, {@enum.value "_id" spClaim__id Logical id of this artifact }
    spClaim__lastUpdated, {@enum.value "_lastUpdated" spClaim__lastUpdated When the resource version last changed }
    spClaim__profile, {@enum.value "_profile" spClaim__profile Profiles this resource claims to conform to }
    spClaim__query, {@enum.value "_query" spClaim__query A custom search profile that describes a specific defined query operation }
    spClaim__security, {@enum.value "_security" spClaim__security Security Labels applied to this resource }
    spClaim__tag, {@enum.value "_tag" spClaim__tag Tags applied to this resource }
    spClaim__text, {@enum.value "_text" spClaim__text Search on the narrative of the resource }
    spClaim_Identifier, {@enum.value "identifier" spClaim_Identifier The primary identifier of the financial resource }
    spClaim_Patient, {@enum.value "patient" spClaim_Patient Patient }
    spClaim_Priority, {@enum.value "priority" spClaim_Priority Processing priority requested }
    spClaim_Provider, {@enum.value "provider" spClaim_Provider Provider responsible for the claim }
    spClaim_Use); {@enum.value "use" spClaim_Use The kind of financial resource }
{$ENDIF}

{$IFDEF FHIR_CLAIMRESPONSE}
  {@Enum TSearchParamsClaimResponse
    Search Parameters for ClaimResponse
  }
  TSearchParamsClaimResponse = (
    spClaimResponse__content, {@enum.value "_content" spClaimResponse__content Search on the entire content of the resource }
    spClaimResponse__id, {@enum.value "_id" spClaimResponse__id Logical id of this artifact }
    spClaimResponse__lastUpdated, {@enum.value "_lastUpdated" spClaimResponse__lastUpdated When the resource version last changed }
    spClaimResponse__profile, {@enum.value "_profile" spClaimResponse__profile Profiles this resource claims to conform to }
    spClaimResponse__query, {@enum.value "_query" spClaimResponse__query A custom search profile that describes a specific defined query operation }
    spClaimResponse__security, {@enum.value "_security" spClaimResponse__security Security Labels applied to this resource }
    spClaimResponse__tag, {@enum.value "_tag" spClaimResponse__tag Tags applied to this resource }
    spClaimResponse__text, {@enum.value "_text" spClaimResponse__text Search on the narrative of the resource }
    spClaimResponse_Identifier); {@enum.value "identifier" spClaimResponse_Identifier The identity of the insurer }
{$ENDIF}

{$IFDEF FHIR_CLINICALIMPRESSION}
  {@Enum TSearchParamsClinicalImpression
    Search Parameters for ClinicalImpression
  }
  TSearchParamsClinicalImpression = (
    spClinicalImpression__content, {@enum.value "_content" spClinicalImpression__content Search on the entire content of the resource }
    spClinicalImpression__id, {@enum.value "_id" spClinicalImpression__id Logical id of this artifact }
    spClinicalImpression__lastUpdated, {@enum.value "_lastUpdated" spClinicalImpression__lastUpdated When the resource version last changed }
    spClinicalImpression__profile, {@enum.value "_profile" spClinicalImpression__profile Profiles this resource claims to conform to }
    spClinicalImpression__query, {@enum.value "_query" spClinicalImpression__query A custom search profile that describes a specific defined query operation }
    spClinicalImpression__security, {@enum.value "_security" spClinicalImpression__security Security Labels applied to this resource }
    spClinicalImpression__tag, {@enum.value "_tag" spClinicalImpression__tag Tags applied to this resource }
    spClinicalImpression__text, {@enum.value "_text" spClinicalImpression__text Search on the narrative of the resource }
    spClinicalImpression_Action, {@enum.value "action" spClinicalImpression_Action Actions taken during assessment }
    spClinicalImpression_Assessor, {@enum.value "assessor" spClinicalImpression_Assessor The clinician performing the assessment }
    spClinicalImpression_Date, {@enum.value "date" spClinicalImpression_Date When the assessment occurred }
    spClinicalImpression_Finding, {@enum.value "finding" spClinicalImpression_Finding Specific text or code for finding }
    spClinicalImpression_Investigation, {@enum.value "investigation" spClinicalImpression_Investigation Record of a specific investigation }
    spClinicalImpression_Patient, {@enum.value "patient" spClinicalImpression_Patient The patient being assessed }
    spClinicalImpression_Plan, {@enum.value "plan" spClinicalImpression_Plan Plan of action after assessment }
    spClinicalImpression_Previous, {@enum.value "previous" spClinicalImpression_Previous Reference to last assessment }
    spClinicalImpression_Problem, {@enum.value "problem" spClinicalImpression_Problem General assessment of patient state }
    spClinicalImpression_Resolved, {@enum.value "resolved" spClinicalImpression_Resolved Diagnoses/conditions resolved since previous assessment }
    spClinicalImpression_Ruledout, {@enum.value "ruledout" spClinicalImpression_Ruledout Specific text of code for diagnosis }
    spClinicalImpression_Status, {@enum.value "status" spClinicalImpression_Status in-progress | completed | entered-in-error }
    spClinicalImpression_Trigger, {@enum.value "trigger" spClinicalImpression_Trigger Request or event that necessitated this assessment }
    spClinicalImpression_Triggercode); {@enum.value "trigger-code" spClinicalImpression_Triggercode Request or event that necessitated this assessment }
{$ENDIF}

{$IFDEF FHIR_COMMUNICATION}
  {@Enum TSearchParamsCommunication
    Search Parameters for Communication
  }
  TSearchParamsCommunication = (
    spCommunication__content, {@enum.value "_content" spCommunication__content Search on the entire content of the resource }
    spCommunication__id, {@enum.value "_id" spCommunication__id Logical id of this artifact }
    spCommunication__lastUpdated, {@enum.value "_lastUpdated" spCommunication__lastUpdated When the resource version last changed }
    spCommunication__profile, {@enum.value "_profile" spCommunication__profile Profiles this resource claims to conform to }
    spCommunication__query, {@enum.value "_query" spCommunication__query A custom search profile that describes a specific defined query operation }
    spCommunication__security, {@enum.value "_security" spCommunication__security Security Labels applied to this resource }
    spCommunication__tag, {@enum.value "_tag" spCommunication__tag Tags applied to this resource }
    spCommunication__text, {@enum.value "_text" spCommunication__text Search on the narrative of the resource }
    spCommunication_Category, {@enum.value "category" spCommunication_Category Message category }
    spCommunication_Encounter, {@enum.value "encounter" spCommunication_Encounter Encounter leading to message }
    spCommunication_Identifier, {@enum.value "identifier" spCommunication_Identifier Unique identifier }
    spCommunication_Medium, {@enum.value "medium" spCommunication_Medium A channel of communication }
    spCommunication_Patient, {@enum.value "patient" spCommunication_Patient Focus of message }
    spCommunication_Received, {@enum.value "received" spCommunication_Received When received }
    spCommunication_Recipient, {@enum.value "recipient" spCommunication_Recipient Message recipient }
    spCommunication_Request, {@enum.value "request" spCommunication_Request CommunicationRequest producing this message }
    spCommunication_Sender, {@enum.value "sender" spCommunication_Sender Message sender }
    spCommunication_Sent, {@enum.value "sent" spCommunication_Sent When sent }
    spCommunication_Status, {@enum.value "status" spCommunication_Status in-progress | completed | suspended | rejected | failed }
    spCommunication_Subject); {@enum.value "subject" spCommunication_Subject Focus of message }
{$ENDIF}

{$IFDEF FHIR_COMMUNICATIONREQUEST}
  {@Enum TSearchParamsCommunicationRequest
    Search Parameters for CommunicationRequest
  }
  TSearchParamsCommunicationRequest = (
    spCommunicationRequest__content, {@enum.value "_content" spCommunicationRequest__content Search on the entire content of the resource }
    spCommunicationRequest__id, {@enum.value "_id" spCommunicationRequest__id Logical id of this artifact }
    spCommunicationRequest__lastUpdated, {@enum.value "_lastUpdated" spCommunicationRequest__lastUpdated When the resource version last changed }
    spCommunicationRequest__profile, {@enum.value "_profile" spCommunicationRequest__profile Profiles this resource claims to conform to }
    spCommunicationRequest__query, {@enum.value "_query" spCommunicationRequest__query A custom search profile that describes a specific defined query operation }
    spCommunicationRequest__security, {@enum.value "_security" spCommunicationRequest__security Security Labels applied to this resource }
    spCommunicationRequest__tag, {@enum.value "_tag" spCommunicationRequest__tag Tags applied to this resource }
    spCommunicationRequest__text, {@enum.value "_text" spCommunicationRequest__text Search on the narrative of the resource }
    spCommunicationRequest_Category, {@enum.value "category" spCommunicationRequest_Category Message category }
    spCommunicationRequest_Encounter, {@enum.value "encounter" spCommunicationRequest_Encounter Encounter leading to message }
    spCommunicationRequest_Identifier, {@enum.value "identifier" spCommunicationRequest_Identifier Unique identifier }
    spCommunicationRequest_Medium, {@enum.value "medium" spCommunicationRequest_Medium A channel of communication }
    spCommunicationRequest_Patient, {@enum.value "patient" spCommunicationRequest_Patient Focus of message }
    spCommunicationRequest_Priority, {@enum.value "priority" spCommunicationRequest_Priority Message urgency }
    spCommunicationRequest_Recipient, {@enum.value "recipient" spCommunicationRequest_Recipient Message recipient }
    spCommunicationRequest_Requested, {@enum.value "requested" spCommunicationRequest_Requested When ordered or proposed }
    spCommunicationRequest_Requester, {@enum.value "requester" spCommunicationRequest_Requester An individual who requested a communication }
    spCommunicationRequest_Sender, {@enum.value "sender" spCommunicationRequest_Sender Message sender }
    spCommunicationRequest_Status, {@enum.value "status" spCommunicationRequest_Status proposed | planned | requested | received | accepted | in-progress | completed | suspended | rejected | failed }
    spCommunicationRequest_Subject, {@enum.value "subject" spCommunicationRequest_Subject Focus of message }
    spCommunicationRequest_Time); {@enum.value "time" spCommunicationRequest_Time When scheduled }
{$ENDIF}

{$IFDEF FHIR_COMPOSITION}
  {@Enum TSearchParamsComposition
    Search Parameters for Composition
  }
  TSearchParamsComposition = (
    spComposition__content, {@enum.value "_content" spComposition__content Search on the entire content of the resource }
    spComposition__id, {@enum.value "_id" spComposition__id Logical id of this artifact }
    spComposition__lastUpdated, {@enum.value "_lastUpdated" spComposition__lastUpdated When the resource version last changed }
    spComposition__profile, {@enum.value "_profile" spComposition__profile Profiles this resource claims to conform to }
    spComposition__query, {@enum.value "_query" spComposition__query A custom search profile that describes a specific defined query operation }
    spComposition__security, {@enum.value "_security" spComposition__security Security Labels applied to this resource }
    spComposition__tag, {@enum.value "_tag" spComposition__tag Tags applied to this resource }
    spComposition__text, {@enum.value "_text" spComposition__text Search on the narrative of the resource }
    spComposition_Attester, {@enum.value "attester" spComposition_Attester Who attested the composition }
    spComposition_Author, {@enum.value "author" spComposition_Author Who and/or what authored the composition }
    spComposition_Class, {@enum.value "class" spComposition_Class Categorization of Composition }
    spComposition_Confidentiality, {@enum.value "confidentiality" spComposition_Confidentiality As defined by affinity domain }
    spComposition_Context, {@enum.value "context" spComposition_Context Code(s) that apply to the event being documented }
    spComposition_Date, {@enum.value "date" spComposition_Date Composition editing time }
    spComposition_Encounter, {@enum.value "encounter" spComposition_Encounter Context of the Composition }
    spComposition_Entry, {@enum.value "entry" spComposition_Entry A reference to data that supports this section }
    spComposition_Identifier, {@enum.value "identifier" spComposition_Identifier Logical identifier of composition (version-independent) }
    spComposition_Patient, {@enum.value "patient" spComposition_Patient Who and/or what the composition is about }
    spComposition_Period, {@enum.value "period" spComposition_Period The period covered by the documentation }
    spComposition_Section, {@enum.value "section" spComposition_Section Classification of section (recommended) }
    spComposition_Status, {@enum.value "status" spComposition_Status preliminary | final | amended | entered-in-error }
    spComposition_Subject, {@enum.value "subject" spComposition_Subject Who and/or what the composition is about }
    spComposition_Title, {@enum.value "title" spComposition_Title Human Readable name/title }
    spComposition_Type); {@enum.value "type" spComposition_Type Kind of composition (LOINC if possible) }
{$ENDIF}

{$IFDEF FHIR_CONCEPTMAP}
  {@Enum TSearchParamsConceptMap
    Search Parameters for ConceptMap
  }
  TSearchParamsConceptMap = (
    spConceptMap__content, {@enum.value "_content" spConceptMap__content Search on the entire content of the resource }
    spConceptMap__id, {@enum.value "_id" spConceptMap__id Logical id of this artifact }
    spConceptMap__lastUpdated, {@enum.value "_lastUpdated" spConceptMap__lastUpdated When the resource version last changed }
    spConceptMap__profile, {@enum.value "_profile" spConceptMap__profile Profiles this resource claims to conform to }
    spConceptMap__query, {@enum.value "_query" spConceptMap__query A custom search profile that describes a specific defined query operation }
    spConceptMap__security, {@enum.value "_security" spConceptMap__security Security Labels applied to this resource }
    spConceptMap__tag, {@enum.value "_tag" spConceptMap__tag Tags applied to this resource }
    spConceptMap__text, {@enum.value "_text" spConceptMap__text Search on the narrative of the resource }
    spConceptMap_Context, {@enum.value "context" spConceptMap_Context A use context assigned to the concept map }
    spConceptMap_Date, {@enum.value "date" spConceptMap_Date The concept map publication date }
    spConceptMap_Dependson, {@enum.value "dependson" spConceptMap_Dependson Reference to element/field/ValueSet mapping depends on }
    spConceptMap_Description, {@enum.value "description" spConceptMap_Description Text search in the description of the concept map }
    spConceptMap_Identifier, {@enum.value "identifier" spConceptMap_Identifier Additional identifier for the concept map }
    spConceptMap_Name, {@enum.value "name" spConceptMap_Name Name of the concept map }
    spConceptMap_Product, {@enum.value "product" spConceptMap_Product Reference to element/field/ValueSet mapping depends on }
    spConceptMap_Publisher, {@enum.value "publisher" spConceptMap_Publisher Name of the publisher of the concept map }
    spConceptMap_Source, {@enum.value "source" spConceptMap_Source Identifies the source of the concepts which are being mapped }
    spConceptMap_Sourcecode, {@enum.value "sourcecode" spConceptMap_Sourcecode Identifies element being mapped }
    spConceptMap_Sourcesystem, {@enum.value "sourcesystem" spConceptMap_Sourcesystem Code System (if value set crosses code systems) }
    spConceptMap_Sourceuri, {@enum.value "sourceuri" spConceptMap_Sourceuri Identifies the source of the concepts which are being mapped }
    spConceptMap_Status, {@enum.value "status" spConceptMap_Status Status of the concept map }
    spConceptMap_Target, {@enum.value "target" spConceptMap_Target Provides context to the mappings }
    spConceptMap_Targetcode, {@enum.value "targetcode" spConceptMap_Targetcode Code that identifies the target element }
    spConceptMap_Targetsystem, {@enum.value "targetsystem" spConceptMap_Targetsystem System of the target (if necessary) }
    spConceptMap_Url, {@enum.value "url" spConceptMap_Url The URL of the concept map }
    spConceptMap_Version); {@enum.value "version" spConceptMap_Version The version identifier of the concept map }
{$ENDIF}

{$IFDEF FHIR_CONDITION}
  {@Enum TSearchParamsCondition
    Search Parameters for Condition
  }
  TSearchParamsCondition = (
    spCondition__content, {@enum.value "_content" spCondition__content Search on the entire content of the resource }
    spCondition__id, {@enum.value "_id" spCondition__id Logical id of this artifact }
    spCondition__lastUpdated, {@enum.value "_lastUpdated" spCondition__lastUpdated When the resource version last changed }
    spCondition__profile, {@enum.value "_profile" spCondition__profile Profiles this resource claims to conform to }
    spCondition__query, {@enum.value "_query" spCondition__query A custom search profile that describes a specific defined query operation }
    spCondition__security, {@enum.value "_security" spCondition__security Security Labels applied to this resource }
    spCondition__tag, {@enum.value "_tag" spCondition__tag Tags applied to this resource }
    spCondition__text, {@enum.value "_text" spCondition__text Search on the narrative of the resource }
    spCondition_Age, {@enum.value "age" spCondition_Age Search based on Condition onsetAge }
    spCondition_Asserter, {@enum.value "asserter" spCondition_Asserter Person who asserts this condition }
    spCondition_Bodysite, {@enum.value "body-site" spCondition_Bodysite Anatomical location, if relevant }
    spCondition_Category, {@enum.value "category" spCondition_Category The category of the condition }
    spCondition_Clinicalstatus, {@enum.value "clinicalstatus" spCondition_Clinicalstatus The clinical status of the condition }
    spCondition_Code, {@enum.value "code" spCondition_Code Code for the condition }
    spCondition_Daterecorded, {@enum.value "date-recorded" spCondition_Daterecorded A date, when the Condition statement was documented }
    spCondition_Encounter, {@enum.value "encounter" spCondition_Encounter Encounter when condition first asserted }
    spCondition_Evidence, {@enum.value "evidence" spCondition_Evidence Manifestation/symptom }
    spCondition_Identifier, {@enum.value "identifier" spCondition_Identifier A unique identifier of the condition record }
    spCondition_Onset, {@enum.value "onset" spCondition_Onset Date related onsets (dateTime and Period) }
    spCondition_Onsetinfo, {@enum.value "onset-info" spCondition_Onsetinfo Other onsets (boolean, age, range, string) }
    spCondition_Patient, {@enum.value "patient" spCondition_Patient Who has the condition? }
    spCondition_Severity, {@enum.value "severity" spCondition_Severity The severity of the condition }
    spCondition_Stage); {@enum.value "stage" spCondition_Stage Simple summary (disease specific) }
{$ENDIF}

{$IFDEF FHIR_CONFORMANCE}
  {@Enum TSearchParamsConformance
    Search Parameters for Conformance
  }
  TSearchParamsConformance = (
    spConformance__content, {@enum.value "_content" spConformance__content Search on the entire content of the resource }
    spConformance__id, {@enum.value "_id" spConformance__id Logical id of this artifact }
    spConformance__lastUpdated, {@enum.value "_lastUpdated" spConformance__lastUpdated When the resource version last changed }
    spConformance__profile, {@enum.value "_profile" spConformance__profile Profiles this resource claims to conform to }
    spConformance__query, {@enum.value "_query" spConformance__query A custom search profile that describes a specific defined query operation }
    spConformance__security, {@enum.value "_security" spConformance__security Security Labels applied to this resource }
    spConformance__tag, {@enum.value "_tag" spConformance__tag Tags applied to this resource }
    spConformance__text, {@enum.value "_text" spConformance__text Search on the narrative of the resource }
    spConformance_Date, {@enum.value "date" spConformance_Date The conformance statement publication date }
    spConformance_Description, {@enum.value "description" spConformance_Description Text search in the description of the conformance statement }
    spConformance_Event, {@enum.value "event" spConformance_Event Event code in a conformance statement }
    spConformance_Fhirversion, {@enum.value "fhirversion" spConformance_Fhirversion The version of FHIR }
    spConformance_Format, {@enum.value "format" spConformance_Format formats supported (xml | json | mime type) }
    spConformance_Mode, {@enum.value "mode" spConformance_Mode Mode - restful (server/client) or messaging (sender/receiver) }
    spConformance_Name, {@enum.value "name" spConformance_Name Name of the conformance statement }
    spConformance_Profile, {@enum.value "profile" spConformance_Profile A profile id invoked in a conformance statement }
    spConformance_Publisher, {@enum.value "publisher" spConformance_Publisher Name of the publisher of the conformance statement }
    spConformance_Resource, {@enum.value "resource" spConformance_Resource Name of a resource mentioned in a conformance statement }
    spConformance_Security, {@enum.value "security" spConformance_Security OAuth | SMART-on-FHIR | NTLM | Basic | Kerberos | Certificates }
    spConformance_Software, {@enum.value "software" spConformance_Software Part of a the name of a software application }
    spConformance_Status, {@enum.value "status" spConformance_Status The current status of the conformance statement }
    spConformance_Supportedprofile, {@enum.value "supported-profile" spConformance_Supportedprofile Profiles for use cases supported }
    spConformance_Url, {@enum.value "url" spConformance_Url The uri that identifies the conformance statement }
    spConformance_Version); {@enum.value "version" spConformance_Version The version identifier of the conformance statement }
{$ENDIF}

{$IFDEF FHIR_CONTRACT}
  {@Enum TSearchParamsContract
    Search Parameters for Contract
  }
  TSearchParamsContract = (
    spContract__content, {@enum.value "_content" spContract__content Search on the entire content of the resource }
    spContract__id, {@enum.value "_id" spContract__id Logical id of this artifact }
    spContract__lastUpdated, {@enum.value "_lastUpdated" spContract__lastUpdated When the resource version last changed }
    spContract__profile, {@enum.value "_profile" spContract__profile Profiles this resource claims to conform to }
    spContract__query, {@enum.value "_query" spContract__query A custom search profile that describes a specific defined query operation }
    spContract__security, {@enum.value "_security" spContract__security Security Labels applied to this resource }
    spContract__tag, {@enum.value "_tag" spContract__tag Tags applied to this resource }
    spContract__text, {@enum.value "_text" spContract__text Search on the narrative of the resource }
    spContract_Actor, {@enum.value "actor" spContract_Actor Contract Actor Type }
    spContract_Identifier, {@enum.value "identifier" spContract_Identifier The identity of the contract }
    spContract_Patient, {@enum.value "patient" spContract_Patient The identity of the target of the contract (if a patient) }
    spContract_Signer, {@enum.value "signer" spContract_Signer Contract Signatory Party }
    spContract_Subject); {@enum.value "subject" spContract_Subject The identity of the target of the contract }
{$ENDIF}

{$IFDEF FHIR_COVERAGE}
  {@Enum TSearchParamsCoverage
    Search Parameters for Coverage
  }
  TSearchParamsCoverage = (
    spCoverage__content, {@enum.value "_content" spCoverage__content Search on the entire content of the resource }
    spCoverage__id, {@enum.value "_id" spCoverage__id Logical id of this artifact }
    spCoverage__lastUpdated, {@enum.value "_lastUpdated" spCoverage__lastUpdated When the resource version last changed }
    spCoverage__profile, {@enum.value "_profile" spCoverage__profile Profiles this resource claims to conform to }
    spCoverage__query, {@enum.value "_query" spCoverage__query A custom search profile that describes a specific defined query operation }
    spCoverage__security, {@enum.value "_security" spCoverage__security Security Labels applied to this resource }
    spCoverage__tag, {@enum.value "_tag" spCoverage__tag Tags applied to this resource }
    spCoverage__text, {@enum.value "_text" spCoverage__text Search on the narrative of the resource }
    spCoverage_Dependent, {@enum.value "dependent" spCoverage_Dependent Dependent number }
    spCoverage_Group, {@enum.value "group" spCoverage_Group Group identifier }
    spCoverage_Identifier, {@enum.value "identifier" spCoverage_Identifier The primary identifier of the insured }
    spCoverage_Issuer, {@enum.value "issuer" spCoverage_Issuer The identity of the insurer }
    spCoverage_Plan, {@enum.value "plan" spCoverage_Plan A plan or policy identifier }
    spCoverage_Sequence, {@enum.value "sequence" spCoverage_Sequence Sequence number }
    spCoverage_Subplan, {@enum.value "subplan" spCoverage_Subplan Sub-plan identifier }
    spCoverage_Type); {@enum.value "type" spCoverage_Type The kind of coverage }
{$ENDIF}

{$IFDEF FHIR_DATAELEMENT}
  {@Enum TSearchParamsDataElement
    Search Parameters for DataElement
  }
  TSearchParamsDataElement = (
    spDataElement__content, {@enum.value "_content" spDataElement__content Search on the entire content of the resource }
    spDataElement__id, {@enum.value "_id" spDataElement__id Logical id of this artifact }
    spDataElement__lastUpdated, {@enum.value "_lastUpdated" spDataElement__lastUpdated When the resource version last changed }
    spDataElement__profile, {@enum.value "_profile" spDataElement__profile Profiles this resource claims to conform to }
    spDataElement__query, {@enum.value "_query" spDataElement__query A custom search profile that describes a specific defined query operation }
    spDataElement__security, {@enum.value "_security" spDataElement__security Security Labels applied to this resource }
    spDataElement__tag, {@enum.value "_tag" spDataElement__tag Tags applied to this resource }
    spDataElement__text, {@enum.value "_text" spDataElement__text Search on the narrative of the resource }
    spDataElement_Code, {@enum.value "code" spDataElement_Code A code for the data element (server may choose to do subsumption) }
    spDataElement_Context, {@enum.value "context" spDataElement_Context A use context assigned to the data element }
    spDataElement_Date, {@enum.value "date" spDataElement_Date The data element publication date }
    spDataElement_Description, {@enum.value "description" spDataElement_Description Text search in the description of the data element.  This corresponds to the definition of the first DataElement.element. }
    spDataElement_Identifier, {@enum.value "identifier" spDataElement_Identifier The identifier of the data element }
    spDataElement_Name, {@enum.value "name" spDataElement_Name Name of the data element }
    spDataElement_ObjectClass, {@enum.value "objectClass" spDataElement_ObjectClass Matches on the 11179-objectClass extension value }
    spDataElement_ObjectClassProperty, {@enum.value "objectClassProperty" spDataElement_ObjectClassProperty Matches on the 11179-objectClassProperty extension value }
    spDataElement_Publisher, {@enum.value "publisher" spDataElement_Publisher Name of the publisher of the data element }
    spDataElement_Status, {@enum.value "status" spDataElement_Status The current status of the data element }
    spDataElement_Stringency, {@enum.value "stringency" spDataElement_Stringency The stringency of the data element definition }
    spDataElement_Url, {@enum.value "url" spDataElement_Url The official URL for the data element }
    spDataElement_Version); {@enum.value "version" spDataElement_Version The version identifier of the data element }
{$ENDIF}

{$IFDEF FHIR_DETECTEDISSUE}
  {@Enum TSearchParamsDetectedIssue
    Search Parameters for DetectedIssue
  }
  TSearchParamsDetectedIssue = (
    spDetectedIssue__content, {@enum.value "_content" spDetectedIssue__content Search on the entire content of the resource }
    spDetectedIssue__id, {@enum.value "_id" spDetectedIssue__id Logical id of this artifact }
    spDetectedIssue__lastUpdated, {@enum.value "_lastUpdated" spDetectedIssue__lastUpdated When the resource version last changed }
    spDetectedIssue__profile, {@enum.value "_profile" spDetectedIssue__profile Profiles this resource claims to conform to }
    spDetectedIssue__query, {@enum.value "_query" spDetectedIssue__query A custom search profile that describes a specific defined query operation }
    spDetectedIssue__security, {@enum.value "_security" spDetectedIssue__security Security Labels applied to this resource }
    spDetectedIssue__tag, {@enum.value "_tag" spDetectedIssue__tag Tags applied to this resource }
    spDetectedIssue__text, {@enum.value "_text" spDetectedIssue__text Search on the narrative of the resource }
    spDetectedIssue_Author, {@enum.value "author" spDetectedIssue_Author The provider or device that identified the issue }
    spDetectedIssue_Category, {@enum.value "category" spDetectedIssue_Category Issue Category, e.g. drug-drug, duplicate therapy, etc. }
    spDetectedIssue_Date, {@enum.value "date" spDetectedIssue_Date When identified }
    spDetectedIssue_Identifier, {@enum.value "identifier" spDetectedIssue_Identifier Unique id for the detected issue }
    spDetectedIssue_Implicated, {@enum.value "implicated" spDetectedIssue_Implicated Problem resource }
    spDetectedIssue_Patient); {@enum.value "patient" spDetectedIssue_Patient Associated patient }
{$ENDIF}

{$IFDEF FHIR_DEVICE}
  {@Enum TSearchParamsDevice
    Search Parameters for Device
  }
  TSearchParamsDevice = (
    spDevice__content, {@enum.value "_content" spDevice__content Search on the entire content of the resource }
    spDevice__id, {@enum.value "_id" spDevice__id Logical id of this artifact }
    spDevice__lastUpdated, {@enum.value "_lastUpdated" spDevice__lastUpdated When the resource version last changed }
    spDevice__profile, {@enum.value "_profile" spDevice__profile Profiles this resource claims to conform to }
    spDevice__query, {@enum.value "_query" spDevice__query A custom search profile that describes a specific defined query operation }
    spDevice__security, {@enum.value "_security" spDevice__security Security Labels applied to this resource }
    spDevice__tag, {@enum.value "_tag" spDevice__tag Tags applied to this resource }
    spDevice__text, {@enum.value "_text" spDevice__text Search on the narrative of the resource }
    spDevice_Identifier, {@enum.value "identifier" spDevice_Identifier Instance id from manufacturer, owner, and others }
    spDevice_Location, {@enum.value "location" spDevice_Location A location, where the resource is found }
    spDevice_Manufacturer, {@enum.value "manufacturer" spDevice_Manufacturer The manufacturer of the device }
    spDevice_Model, {@enum.value "model" spDevice_Model The model of the device }
    spDevice_Organization, {@enum.value "organization" spDevice_Organization The organization responsible for the device }
    spDevice_Patient, {@enum.value "patient" spDevice_Patient Patient information, if the resource is affixed to a person }
    spDevice_Type, {@enum.value "type" spDevice_Type The type of the device }
    spDevice_Udi, {@enum.value "udi" spDevice_Udi FDA mandated Unique Device Identifier }
    spDevice_Url); {@enum.value "url" spDevice_Url Network address to contact device }
{$ENDIF}

{$IFDEF FHIR_DEVICECOMPONENT}
  {@Enum TSearchParamsDeviceComponent
    Search Parameters for DeviceComponent
  }
  TSearchParamsDeviceComponent = (
    spDeviceComponent__content, {@enum.value "_content" spDeviceComponent__content Search on the entire content of the resource }
    spDeviceComponent__id, {@enum.value "_id" spDeviceComponent__id Logical id of this artifact }
    spDeviceComponent__lastUpdated, {@enum.value "_lastUpdated" spDeviceComponent__lastUpdated When the resource version last changed }
    spDeviceComponent__profile, {@enum.value "_profile" spDeviceComponent__profile Profiles this resource claims to conform to }
    spDeviceComponent__query, {@enum.value "_query" spDeviceComponent__query A custom search profile that describes a specific defined query operation }
    spDeviceComponent__security, {@enum.value "_security" spDeviceComponent__security Security Labels applied to this resource }
    spDeviceComponent__tag, {@enum.value "_tag" spDeviceComponent__tag Tags applied to this resource }
    spDeviceComponent__text, {@enum.value "_text" spDeviceComponent__text Search on the narrative of the resource }
    spDeviceComponent_Parent, {@enum.value "parent" spDeviceComponent_Parent The parent DeviceComponent resource }
    spDeviceComponent_Source, {@enum.value "source" spDeviceComponent_Source The device source }
    spDeviceComponent_Type); {@enum.value "type" spDeviceComponent_Type The device component type }
{$ENDIF}

{$IFDEF FHIR_DEVICEMETRIC}
  {@Enum TSearchParamsDeviceMetric
    Search Parameters for DeviceMetric
  }
  TSearchParamsDeviceMetric = (
    spDeviceMetric__content, {@enum.value "_content" spDeviceMetric__content Search on the entire content of the resource }
    spDeviceMetric__id, {@enum.value "_id" spDeviceMetric__id Logical id of this artifact }
    spDeviceMetric__lastUpdated, {@enum.value "_lastUpdated" spDeviceMetric__lastUpdated When the resource version last changed }
    spDeviceMetric__profile, {@enum.value "_profile" spDeviceMetric__profile Profiles this resource claims to conform to }
    spDeviceMetric__query, {@enum.value "_query" spDeviceMetric__query A custom search profile that describes a specific defined query operation }
    spDeviceMetric__security, {@enum.value "_security" spDeviceMetric__security Security Labels applied to this resource }
    spDeviceMetric__tag, {@enum.value "_tag" spDeviceMetric__tag Tags applied to this resource }
    spDeviceMetric__text, {@enum.value "_text" spDeviceMetric__text Search on the narrative of the resource }
    spDeviceMetric_Category, {@enum.value "category" spDeviceMetric_Category The category of the metric }
    spDeviceMetric_Identifier, {@enum.value "identifier" spDeviceMetric_Identifier The identifier of the metric }
    spDeviceMetric_Parent, {@enum.value "parent" spDeviceMetric_Parent The parent DeviceMetric resource }
    spDeviceMetric_Source, {@enum.value "source" spDeviceMetric_Source The device resource }
    spDeviceMetric_Type); {@enum.value "type" spDeviceMetric_Type The component type }
{$ENDIF}

{$IFDEF FHIR_DEVICEUSEREQUEST}
  {@Enum TSearchParamsDeviceUseRequest
    Search Parameters for DeviceUseRequest
  }
  TSearchParamsDeviceUseRequest = (
    spDeviceUseRequest__content, {@enum.value "_content" spDeviceUseRequest__content Search on the entire content of the resource }
    spDeviceUseRequest__id, {@enum.value "_id" spDeviceUseRequest__id Logical id of this artifact }
    spDeviceUseRequest__lastUpdated, {@enum.value "_lastUpdated" spDeviceUseRequest__lastUpdated When the resource version last changed }
    spDeviceUseRequest__profile, {@enum.value "_profile" spDeviceUseRequest__profile Profiles this resource claims to conform to }
    spDeviceUseRequest__query, {@enum.value "_query" spDeviceUseRequest__query A custom search profile that describes a specific defined query operation }
    spDeviceUseRequest__security, {@enum.value "_security" spDeviceUseRequest__security Security Labels applied to this resource }
    spDeviceUseRequest__tag, {@enum.value "_tag" spDeviceUseRequest__tag Tags applied to this resource }
    spDeviceUseRequest__text, {@enum.value "_text" spDeviceUseRequest__text Search on the narrative of the resource }
    spDeviceUseRequest_Device, {@enum.value "device" spDeviceUseRequest_Device Device requested }
    spDeviceUseRequest_Patient, {@enum.value "patient" spDeviceUseRequest_Patient Search by subject - a patient }
    spDeviceUseRequest_Subject); {@enum.value "subject" spDeviceUseRequest_Subject Search by subject }
{$ENDIF}

{$IFDEF FHIR_DEVICEUSESTATEMENT}
  {@Enum TSearchParamsDeviceUseStatement
    Search Parameters for DeviceUseStatement
  }
  TSearchParamsDeviceUseStatement = (
    spDeviceUseStatement__content, {@enum.value "_content" spDeviceUseStatement__content Search on the entire content of the resource }
    spDeviceUseStatement__id, {@enum.value "_id" spDeviceUseStatement__id Logical id of this artifact }
    spDeviceUseStatement__lastUpdated, {@enum.value "_lastUpdated" spDeviceUseStatement__lastUpdated When the resource version last changed }
    spDeviceUseStatement__profile, {@enum.value "_profile" spDeviceUseStatement__profile Profiles this resource claims to conform to }
    spDeviceUseStatement__query, {@enum.value "_query" spDeviceUseStatement__query A custom search profile that describes a specific defined query operation }
    spDeviceUseStatement__security, {@enum.value "_security" spDeviceUseStatement__security Security Labels applied to this resource }
    spDeviceUseStatement__tag, {@enum.value "_tag" spDeviceUseStatement__tag Tags applied to this resource }
    spDeviceUseStatement__text, {@enum.value "_text" spDeviceUseStatement__text Search on the narrative of the resource }
    spDeviceUseStatement_Device, {@enum.value "device" spDeviceUseStatement_Device Search by device }
    spDeviceUseStatement_Patient, {@enum.value "patient" spDeviceUseStatement_Patient Search by subject - a patient }
    spDeviceUseStatement_Subject); {@enum.value "subject" spDeviceUseStatement_Subject Search by subject }
{$ENDIF}

{$IFDEF FHIR_DIAGNOSTICORDER}
  {@Enum TSearchParamsDiagnosticOrder
    Search Parameters for DiagnosticOrder
  }
  TSearchParamsDiagnosticOrder = (
    spDiagnosticOrder__content, {@enum.value "_content" spDiagnosticOrder__content Search on the entire content of the resource }
    spDiagnosticOrder__id, {@enum.value "_id" spDiagnosticOrder__id Logical id of this artifact }
    spDiagnosticOrder__lastUpdated, {@enum.value "_lastUpdated" spDiagnosticOrder__lastUpdated When the resource version last changed }
    spDiagnosticOrder__profile, {@enum.value "_profile" spDiagnosticOrder__profile Profiles this resource claims to conform to }
    spDiagnosticOrder__query, {@enum.value "_query" spDiagnosticOrder__query A custom search profile that describes a specific defined query operation }
    spDiagnosticOrder__security, {@enum.value "_security" spDiagnosticOrder__security Security Labels applied to this resource }
    spDiagnosticOrder__tag, {@enum.value "_tag" spDiagnosticOrder__tag Tags applied to this resource }
    spDiagnosticOrder__text, {@enum.value "_text" spDiagnosticOrder__text Search on the narrative of the resource }
    spDiagnosticOrder_Actor, {@enum.value "actor" spDiagnosticOrder_Actor Who recorded or did this }
    spDiagnosticOrder_Bodysite, {@enum.value "bodysite" spDiagnosticOrder_Bodysite Location of requested test (if applicable) }
    spDiagnosticOrder_Code, {@enum.value "code" spDiagnosticOrder_Code Code to indicate the item (test or panel) being ordered }
    spDiagnosticOrder_Encounter, {@enum.value "encounter" spDiagnosticOrder_Encounter The encounter that this diagnostic order is associated with }
    spDiagnosticOrder_Eventdate, {@enum.value "event-date" spDiagnosticOrder_Eventdate The date at which the event happened }
    spDiagnosticOrder_Eventstatus, {@enum.value "event-status" spDiagnosticOrder_Eventstatus proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed }
    spDiagnosticOrder_Eventstatusdate, {@enum.value "event-status-date" spDiagnosticOrder_Eventstatusdate A combination of past-status and date }
    spDiagnosticOrder_Identifier, {@enum.value "identifier" spDiagnosticOrder_Identifier Identifiers assigned to this order }
    spDiagnosticOrder_Itemdate, {@enum.value "item-date" spDiagnosticOrder_Itemdate The date at which the event happened }
    spDiagnosticOrder_Itempaststatus, {@enum.value "item-past-status" spDiagnosticOrder_Itempaststatus proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed }
    spDiagnosticOrder_Itemstatus, {@enum.value "item-status" spDiagnosticOrder_Itemstatus proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed }
    spDiagnosticOrder_Itemstatusdate, {@enum.value "item-status-date" spDiagnosticOrder_Itemstatusdate A combination of item-past-status and item-date }
    spDiagnosticOrder_Orderer, {@enum.value "orderer" spDiagnosticOrder_Orderer Who ordered the test }
    spDiagnosticOrder_Patient, {@enum.value "patient" spDiagnosticOrder_Patient Who and/or what test is about }
    spDiagnosticOrder_Specimen, {@enum.value "specimen" spDiagnosticOrder_Specimen If the whole order relates to specific specimens }
    spDiagnosticOrder_Status, {@enum.value "status" spDiagnosticOrder_Status proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed }
    spDiagnosticOrder_Subject); {@enum.value "subject" spDiagnosticOrder_Subject Who and/or what test is about }
{$ENDIF}

{$IFDEF FHIR_DIAGNOSTICREPORT}
  {@Enum TSearchParamsDiagnosticReport
    Search Parameters for DiagnosticReport
  }
  TSearchParamsDiagnosticReport = (
    spDiagnosticReport__content, {@enum.value "_content" spDiagnosticReport__content Search on the entire content of the resource }
    spDiagnosticReport__id, {@enum.value "_id" spDiagnosticReport__id Logical id of this artifact }
    spDiagnosticReport__lastUpdated, {@enum.value "_lastUpdated" spDiagnosticReport__lastUpdated When the resource version last changed }
    spDiagnosticReport__profile, {@enum.value "_profile" spDiagnosticReport__profile Profiles this resource claims to conform to }
    spDiagnosticReport__query, {@enum.value "_query" spDiagnosticReport__query A custom search profile that describes a specific defined query operation }
    spDiagnosticReport__security, {@enum.value "_security" spDiagnosticReport__security Security Labels applied to this resource }
    spDiagnosticReport__tag, {@enum.value "_tag" spDiagnosticReport__tag Tags applied to this resource }
    spDiagnosticReport__text, {@enum.value "_text" spDiagnosticReport__text Search on the narrative of the resource }
    spDiagnosticReport_Category, {@enum.value "category" spDiagnosticReport_Category Which diagnostic discipline/department created the report }
    spDiagnosticReport_Code, {@enum.value "code" spDiagnosticReport_Code The code for the report as a whole, as opposed to codes for the atomic results, which are the names on the observation resource referred to from the result }
    spDiagnosticReport_Date, {@enum.value "date" spDiagnosticReport_Date The clinically relevant time of the report }
    spDiagnosticReport_Diagnosis, {@enum.value "diagnosis" spDiagnosticReport_Diagnosis A coded diagnosis on the report }
    spDiagnosticReport_Encounter, {@enum.value "encounter" spDiagnosticReport_Encounter The Encounter when the order was made }
    spDiagnosticReport_Identifier, {@enum.value "identifier" spDiagnosticReport_Identifier An identifier for the report }
    spDiagnosticReport_Image, {@enum.value "image" spDiagnosticReport_Image A reference to the image source. }
    spDiagnosticReport_Issued, {@enum.value "issued" spDiagnosticReport_Issued When the report was issued }
    spDiagnosticReport_Patient, {@enum.value "patient" spDiagnosticReport_Patient The subject of the report if a patient }
    spDiagnosticReport_Performer, {@enum.value "performer" spDiagnosticReport_Performer Who was the source of the report (organization) }
    spDiagnosticReport_Request, {@enum.value "request" spDiagnosticReport_Request Reference to the test or procedure request. }
    spDiagnosticReport_Result, {@enum.value "result" spDiagnosticReport_Result Link to an atomic result (observation resource) }
    spDiagnosticReport_Specimen, {@enum.value "specimen" spDiagnosticReport_Specimen The specimen details }
    spDiagnosticReport_Status, {@enum.value "status" spDiagnosticReport_Status The status of the report }
    spDiagnosticReport_Subject); {@enum.value "subject" spDiagnosticReport_Subject The subject of the report }
{$ENDIF}

{$IFDEF FHIR_DOCUMENTMANIFEST}
  {@Enum TSearchParamsDocumentManifest
    Search Parameters for DocumentManifest
  }
  TSearchParamsDocumentManifest = (
    spDocumentManifest__content, {@enum.value "_content" spDocumentManifest__content Search on the entire content of the resource }
    spDocumentManifest__id, {@enum.value "_id" spDocumentManifest__id Logical id of this artifact }
    spDocumentManifest__lastUpdated, {@enum.value "_lastUpdated" spDocumentManifest__lastUpdated When the resource version last changed }
    spDocumentManifest__profile, {@enum.value "_profile" spDocumentManifest__profile Profiles this resource claims to conform to }
    spDocumentManifest__query, {@enum.value "_query" spDocumentManifest__query A custom search profile that describes a specific defined query operation }
    spDocumentManifest__security, {@enum.value "_security" spDocumentManifest__security Security Labels applied to this resource }
    spDocumentManifest__tag, {@enum.value "_tag" spDocumentManifest__tag Tags applied to this resource }
    spDocumentManifest__text, {@enum.value "_text" spDocumentManifest__text Search on the narrative of the resource }
    spDocumentManifest_Author, {@enum.value "author" spDocumentManifest_Author Who and/or what authored the manifest }
    spDocumentManifest_Contentref, {@enum.value "content-ref" spDocumentManifest_Contentref Contents of this set of documents }
    spDocumentManifest_Created, {@enum.value "created" spDocumentManifest_Created When this document manifest created }
    spDocumentManifest_Description, {@enum.value "description" spDocumentManifest_Description Human-readable description (title) }
    spDocumentManifest_Identifier, {@enum.value "identifier" spDocumentManifest_Identifier Unique Identifier for the set of documents }
    spDocumentManifest_Patient, {@enum.value "patient" spDocumentManifest_Patient The subject of the set of documents }
    spDocumentManifest_Recipient, {@enum.value "recipient" spDocumentManifest_Recipient Intended to get notified about this set of documents }
    spDocumentManifest_Relatedid, {@enum.value "related-id" spDocumentManifest_Relatedid Identifiers of things that are related }
    spDocumentManifest_Relatedref, {@enum.value "related-ref" spDocumentManifest_Relatedref Related Resource }
    spDocumentManifest_Source, {@enum.value "source" spDocumentManifest_Source The source system/application/software }
    spDocumentManifest_Status, {@enum.value "status" spDocumentManifest_Status current | superseded | entered-in-error }
    spDocumentManifest_Subject, {@enum.value "subject" spDocumentManifest_Subject The subject of the set of documents }
    spDocumentManifest_Type); {@enum.value "type" spDocumentManifest_Type Kind of document set }
{$ENDIF}

{$IFDEF FHIR_DOCUMENTREFERENCE}
  {@Enum TSearchParamsDocumentReference
    Search Parameters for DocumentReference
  }
  TSearchParamsDocumentReference = (
    spDocumentReference__content, {@enum.value "_content" spDocumentReference__content Search on the entire content of the resource }
    spDocumentReference__id, {@enum.value "_id" spDocumentReference__id Logical id of this artifact }
    spDocumentReference__lastUpdated, {@enum.value "_lastUpdated" spDocumentReference__lastUpdated When the resource version last changed }
    spDocumentReference__profile, {@enum.value "_profile" spDocumentReference__profile Profiles this resource claims to conform to }
    spDocumentReference__query, {@enum.value "_query" spDocumentReference__query A custom search profile that describes a specific defined query operation }
    spDocumentReference__security, {@enum.value "_security" spDocumentReference__security Security Labels applied to this resource }
    spDocumentReference__tag, {@enum.value "_tag" spDocumentReference__tag Tags applied to this resource }
    spDocumentReference__text, {@enum.value "_text" spDocumentReference__text Search on the narrative of the resource }
    spDocumentReference_Authenticator, {@enum.value "authenticator" spDocumentReference_Authenticator Who/what authenticated the document }
    spDocumentReference_Author, {@enum.value "author" spDocumentReference_Author Who and/or what authored the document }
    spDocumentReference_Class, {@enum.value "class" spDocumentReference_Class Categorization of document }
    spDocumentReference_Created, {@enum.value "created" spDocumentReference_Created Document creation time }
    spDocumentReference_Custodian, {@enum.value "custodian" spDocumentReference_Custodian Organization which maintains the document }
    spDocumentReference_Description, {@enum.value "description" spDocumentReference_Description Human-readable description (title) }
    spDocumentReference_Encounter, {@enum.value "encounter" spDocumentReference_Encounter Context of the document  content }
    spDocumentReference_Event, {@enum.value "event" spDocumentReference_Event Main Clinical Acts Documented }
    spDocumentReference_Facility, {@enum.value "facility" spDocumentReference_Facility Kind of facility where patient was seen }
    spDocumentReference_Format, {@enum.value "format" spDocumentReference_Format Format/content rules for the document }
    spDocumentReference_Identifier, {@enum.value "identifier" spDocumentReference_Identifier Master Version Specific Identifier }
    spDocumentReference_Indexed, {@enum.value "indexed" spDocumentReference_Indexed When this document reference created }
    spDocumentReference_Language, {@enum.value "language" spDocumentReference_Language Human language of the content (BCP-47) }
    spDocumentReference_Location, {@enum.value "location" spDocumentReference_Location Uri where the data can be found }
    spDocumentReference_Patient, {@enum.value "patient" spDocumentReference_Patient Who/what is the subject of the document }
    spDocumentReference_Period, {@enum.value "period" spDocumentReference_Period Time of service that is being documented }
    spDocumentReference_Relatedid, {@enum.value "related-id" spDocumentReference_Relatedid Identifier of related objects or events }
    spDocumentReference_Relatedref, {@enum.value "related-ref" spDocumentReference_Relatedref Related Resource }
    spDocumentReference_Relatesto, {@enum.value "relatesto" spDocumentReference_Relatesto Target of the relationship }
    spDocumentReference_Relation, {@enum.value "relation" spDocumentReference_Relation replaces | transforms | signs | appends }
    spDocumentReference_Relationship, {@enum.value "relationship" spDocumentReference_Relationship Combination of relation and relatesTo }
    spDocumentReference_Securitylabel, {@enum.value "securitylabel" spDocumentReference_Securitylabel Document security-tags }
    spDocumentReference_Setting, {@enum.value "setting" spDocumentReference_Setting Additional details about where the content was created (e.g. clinical specialty) }
    spDocumentReference_Status, {@enum.value "status" spDocumentReference_Status current | superseded | entered-in-error }
    spDocumentReference_Subject, {@enum.value "subject" spDocumentReference_Subject Who/what is the subject of the document }
    spDocumentReference_Type); {@enum.value "type" spDocumentReference_Type Kind of document (LOINC if possible) }
{$ENDIF}

{$IFDEF FHIR_ELIGIBILITYREQUEST}
  {@Enum TSearchParamsEligibilityRequest
    Search Parameters for EligibilityRequest
  }
  TSearchParamsEligibilityRequest = (
    spEligibilityRequest__content, {@enum.value "_content" spEligibilityRequest__content Search on the entire content of the resource }
    spEligibilityRequest__id, {@enum.value "_id" spEligibilityRequest__id Logical id of this artifact }
    spEligibilityRequest__lastUpdated, {@enum.value "_lastUpdated" spEligibilityRequest__lastUpdated When the resource version last changed }
    spEligibilityRequest__profile, {@enum.value "_profile" spEligibilityRequest__profile Profiles this resource claims to conform to }
    spEligibilityRequest__query, {@enum.value "_query" spEligibilityRequest__query A custom search profile that describes a specific defined query operation }
    spEligibilityRequest__security, {@enum.value "_security" spEligibilityRequest__security Security Labels applied to this resource }
    spEligibilityRequest__tag, {@enum.value "_tag" spEligibilityRequest__tag Tags applied to this resource }
    spEligibilityRequest__text, {@enum.value "_text" spEligibilityRequest__text Search on the narrative of the resource }
    spEligibilityRequest_Identifier); {@enum.value "identifier" spEligibilityRequest_Identifier The business identifier of the Eligibility }
{$ENDIF}

{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  {@Enum TSearchParamsEligibilityResponse
    Search Parameters for EligibilityResponse
  }
  TSearchParamsEligibilityResponse = (
    spEligibilityResponse__content, {@enum.value "_content" spEligibilityResponse__content Search on the entire content of the resource }
    spEligibilityResponse__id, {@enum.value "_id" spEligibilityResponse__id Logical id of this artifact }
    spEligibilityResponse__lastUpdated, {@enum.value "_lastUpdated" spEligibilityResponse__lastUpdated When the resource version last changed }
    spEligibilityResponse__profile, {@enum.value "_profile" spEligibilityResponse__profile Profiles this resource claims to conform to }
    spEligibilityResponse__query, {@enum.value "_query" spEligibilityResponse__query A custom search profile that describes a specific defined query operation }
    spEligibilityResponse__security, {@enum.value "_security" spEligibilityResponse__security Security Labels applied to this resource }
    spEligibilityResponse__tag, {@enum.value "_tag" spEligibilityResponse__tag Tags applied to this resource }
    spEligibilityResponse__text, {@enum.value "_text" spEligibilityResponse__text Search on the narrative of the resource }
    spEligibilityResponse_Identifier); {@enum.value "identifier" spEligibilityResponse_Identifier The business identifier of the Explanation of Benefit }
{$ENDIF}

{$IFDEF FHIR_ENCOUNTER}
  {@Enum TSearchParamsEncounter
    Search Parameters for Encounter
  }
  TSearchParamsEncounter = (
    spEncounter__content, {@enum.value "_content" spEncounter__content Search on the entire content of the resource }
    spEncounter__id, {@enum.value "_id" spEncounter__id Logical id of this artifact }
    spEncounter__lastUpdated, {@enum.value "_lastUpdated" spEncounter__lastUpdated When the resource version last changed }
    spEncounter__profile, {@enum.value "_profile" spEncounter__profile Profiles this resource claims to conform to }
    spEncounter__query, {@enum.value "_query" spEncounter__query A custom search profile that describes a specific defined query operation }
    spEncounter__security, {@enum.value "_security" spEncounter__security Security Labels applied to this resource }
    spEncounter__tag, {@enum.value "_tag" spEncounter__tag Tags applied to this resource }
    spEncounter__text, {@enum.value "_text" spEncounter__text Search on the narrative of the resource }
    spEncounter_Appointment, {@enum.value "appointment" spEncounter_Appointment The appointment that scheduled this encounter }
    spEncounter_Condition, {@enum.value "condition" spEncounter_Condition Reason the encounter takes place (resource) }
    spEncounter_Date, {@enum.value "date" spEncounter_Date A date within the period the Encounter lasted }
    spEncounter_Episodeofcare, {@enum.value "episodeofcare" spEncounter_Episodeofcare Episode(s) of care that this encounter should be recorded against }
    spEncounter_Identifier, {@enum.value "identifier" spEncounter_Identifier Identifier(s) by which this encounter is known }
    spEncounter_Incomingreferral, {@enum.value "incomingreferral" spEncounter_Incomingreferral The ReferralRequest that initiated this encounter }
    spEncounter_Indication, {@enum.value "indication" spEncounter_Indication Reason the encounter takes place (resource) }
    spEncounter_Length, {@enum.value "length" spEncounter_Length Length of encounter in days }
    spEncounter_Location, {@enum.value "location" spEncounter_Location Location the encounter takes place }
    spEncounter_Locationperiod, {@enum.value "location-period" spEncounter_Locationperiod Time period during which the patient was present at the location }
    spEncounter_Partof, {@enum.value "part-of" spEncounter_Partof Another Encounter this encounter is part of }
    spEncounter_Participant, {@enum.value "participant" spEncounter_Participant Persons involved in the encounter other than the patient }
    spEncounter_Participanttype, {@enum.value "participant-type" spEncounter_Participanttype Role of participant in encounter }
    spEncounter_Patient, {@enum.value "patient" spEncounter_Patient The patient present at the encounter }
    spEncounter_Practitioner, {@enum.value "practitioner" spEncounter_Practitioner Persons involved in the encounter other than the patient }
    spEncounter_Procedure, {@enum.value "procedure" spEncounter_Procedure Reason the encounter takes place (resource) }
    spEncounter_Reason, {@enum.value "reason" spEncounter_Reason Reason the encounter takes place (code) }
    spEncounter_Specialarrangement, {@enum.value "special-arrangement" spEncounter_Specialarrangement Wheelchair, translator, stretcher, etc. }
    spEncounter_Status, {@enum.value "status" spEncounter_Status planned | arrived | in-progress | onleave | finished | cancelled }
    spEncounter_Type); {@enum.value "type" spEncounter_Type Specific type of encounter }
{$ENDIF}

{$IFDEF FHIR_ENROLLMENTREQUEST}
  {@Enum TSearchParamsEnrollmentRequest
    Search Parameters for EnrollmentRequest
  }
  TSearchParamsEnrollmentRequest = (
    spEnrollmentRequest__content, {@enum.value "_content" spEnrollmentRequest__content Search on the entire content of the resource }
    spEnrollmentRequest__id, {@enum.value "_id" spEnrollmentRequest__id Logical id of this artifact }
    spEnrollmentRequest__lastUpdated, {@enum.value "_lastUpdated" spEnrollmentRequest__lastUpdated When the resource version last changed }
    spEnrollmentRequest__profile, {@enum.value "_profile" spEnrollmentRequest__profile Profiles this resource claims to conform to }
    spEnrollmentRequest__query, {@enum.value "_query" spEnrollmentRequest__query A custom search profile that describes a specific defined query operation }
    spEnrollmentRequest__security, {@enum.value "_security" spEnrollmentRequest__security Security Labels applied to this resource }
    spEnrollmentRequest__tag, {@enum.value "_tag" spEnrollmentRequest__tag Tags applied to this resource }
    spEnrollmentRequest__text, {@enum.value "_text" spEnrollmentRequest__text Search on the narrative of the resource }
    spEnrollmentRequest_Identifier, {@enum.value "identifier" spEnrollmentRequest_Identifier The business identifier of the Enrollment }
    spEnrollmentRequest_Patient, {@enum.value "patient" spEnrollmentRequest_Patient The party to be enrolled }
    spEnrollmentRequest_Subject); {@enum.value "subject" spEnrollmentRequest_Subject The party to be enrolled }
{$ENDIF}

{$IFDEF FHIR_ENROLLMENTRESPONSE}
  {@Enum TSearchParamsEnrollmentResponse
    Search Parameters for EnrollmentResponse
  }
  TSearchParamsEnrollmentResponse = (
    spEnrollmentResponse__content, {@enum.value "_content" spEnrollmentResponse__content Search on the entire content of the resource }
    spEnrollmentResponse__id, {@enum.value "_id" spEnrollmentResponse__id Logical id of this artifact }
    spEnrollmentResponse__lastUpdated, {@enum.value "_lastUpdated" spEnrollmentResponse__lastUpdated When the resource version last changed }
    spEnrollmentResponse__profile, {@enum.value "_profile" spEnrollmentResponse__profile Profiles this resource claims to conform to }
    spEnrollmentResponse__query, {@enum.value "_query" spEnrollmentResponse__query A custom search profile that describes a specific defined query operation }
    spEnrollmentResponse__security, {@enum.value "_security" spEnrollmentResponse__security Security Labels applied to this resource }
    spEnrollmentResponse__tag, {@enum.value "_tag" spEnrollmentResponse__tag Tags applied to this resource }
    spEnrollmentResponse__text, {@enum.value "_text" spEnrollmentResponse__text Search on the narrative of the resource }
    spEnrollmentResponse_Identifier); {@enum.value "identifier" spEnrollmentResponse_Identifier The business identifier of the Explanation of Benefit }
{$ENDIF}

{$IFDEF FHIR_EPISODEOFCARE}
  {@Enum TSearchParamsEpisodeOfCare
    Search Parameters for EpisodeOfCare
  }
  TSearchParamsEpisodeOfCare = (
    spEpisodeOfCare__content, {@enum.value "_content" spEpisodeOfCare__content Search on the entire content of the resource }
    spEpisodeOfCare__id, {@enum.value "_id" spEpisodeOfCare__id Logical id of this artifact }
    spEpisodeOfCare__lastUpdated, {@enum.value "_lastUpdated" spEpisodeOfCare__lastUpdated When the resource version last changed }
    spEpisodeOfCare__profile, {@enum.value "_profile" spEpisodeOfCare__profile Profiles this resource claims to conform to }
    spEpisodeOfCare__query, {@enum.value "_query" spEpisodeOfCare__query A custom search profile that describes a specific defined query operation }
    spEpisodeOfCare__security, {@enum.value "_security" spEpisodeOfCare__security Security Labels applied to this resource }
    spEpisodeOfCare__tag, {@enum.value "_tag" spEpisodeOfCare__tag Tags applied to this resource }
    spEpisodeOfCare__text, {@enum.value "_text" spEpisodeOfCare__text Search on the narrative of the resource }
    spEpisodeOfCare_Caremanager, {@enum.value "care-manager" spEpisodeOfCare_Caremanager Care manager/care co-ordinator for the patient }
    spEpisodeOfCare_Condition, {@enum.value "condition" spEpisodeOfCare_Condition Conditions/problems/diagnoses this episode of care is for }
    spEpisodeOfCare_Date, {@enum.value "date" spEpisodeOfCare_Date The provided date search value falls within the episode of care's period }
    spEpisodeOfCare_Identifier, {@enum.value "identifier" spEpisodeOfCare_Identifier Identifier(s) for the EpisodeOfCare }
    spEpisodeOfCare_Incomingreferral, {@enum.value "incomingreferral" spEpisodeOfCare_Incomingreferral Incoming Referral Request }
    spEpisodeOfCare_Organization, {@enum.value "organization" spEpisodeOfCare_Organization The organization that has assumed the specific responsibilities of this EpisodeOfCare }
    spEpisodeOfCare_Patient, {@enum.value "patient" spEpisodeOfCare_Patient Patient for this episode of care }
    spEpisodeOfCare_Status, {@enum.value "status" spEpisodeOfCare_Status The current status of the Episode of Care as provided (does not check the status history collection) }
    spEpisodeOfCare_Teammember, {@enum.value "team-member" spEpisodeOfCare_Teammember A Practitioner or Organization allocated to the care team for this EpisodeOfCare }
    spEpisodeOfCare_Type); {@enum.value "type" spEpisodeOfCare_Type Type/class  - e.g. specialist referral, disease management }
{$ENDIF}

{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  {@Enum TSearchParamsExplanationOfBenefit
    Search Parameters for ExplanationOfBenefit
  }
  TSearchParamsExplanationOfBenefit = (
    spExplanationOfBenefit__content, {@enum.value "_content" spExplanationOfBenefit__content Search on the entire content of the resource }
    spExplanationOfBenefit__id, {@enum.value "_id" spExplanationOfBenefit__id Logical id of this artifact }
    spExplanationOfBenefit__lastUpdated, {@enum.value "_lastUpdated" spExplanationOfBenefit__lastUpdated When the resource version last changed }
    spExplanationOfBenefit__profile, {@enum.value "_profile" spExplanationOfBenefit__profile Profiles this resource claims to conform to }
    spExplanationOfBenefit__query, {@enum.value "_query" spExplanationOfBenefit__query A custom search profile that describes a specific defined query operation }
    spExplanationOfBenefit__security, {@enum.value "_security" spExplanationOfBenefit__security Security Labels applied to this resource }
    spExplanationOfBenefit__tag, {@enum.value "_tag" spExplanationOfBenefit__tag Tags applied to this resource }
    spExplanationOfBenefit__text, {@enum.value "_text" spExplanationOfBenefit__text Search on the narrative of the resource }
    spExplanationOfBenefit_Identifier); {@enum.value "identifier" spExplanationOfBenefit_Identifier The business identifier of the Explanation of Benefit }
{$ENDIF}

{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  {@Enum TSearchParamsFamilyMemberHistory
    Search Parameters for FamilyMemberHistory
  }
  TSearchParamsFamilyMemberHistory = (
    spFamilyMemberHistory__content, {@enum.value "_content" spFamilyMemberHistory__content Search on the entire content of the resource }
    spFamilyMemberHistory__id, {@enum.value "_id" spFamilyMemberHistory__id Logical id of this artifact }
    spFamilyMemberHistory__lastUpdated, {@enum.value "_lastUpdated" spFamilyMemberHistory__lastUpdated When the resource version last changed }
    spFamilyMemberHistory__profile, {@enum.value "_profile" spFamilyMemberHistory__profile Profiles this resource claims to conform to }
    spFamilyMemberHistory__query, {@enum.value "_query" spFamilyMemberHistory__query A custom search profile that describes a specific defined query operation }
    spFamilyMemberHistory__security, {@enum.value "_security" spFamilyMemberHistory__security Security Labels applied to this resource }
    spFamilyMemberHistory__tag, {@enum.value "_tag" spFamilyMemberHistory__tag Tags applied to this resource }
    spFamilyMemberHistory__text, {@enum.value "_text" spFamilyMemberHistory__text Search on the narrative of the resource }
    spFamilyMemberHistory_Code, {@enum.value "code" spFamilyMemberHistory_Code A search by a condition code }
    spFamilyMemberHistory_Condition, {@enum.value "condition" spFamilyMemberHistory_Condition Search for a history of a particular condition within a patient's family. }
    spFamilyMemberHistory_Date, {@enum.value "date" spFamilyMemberHistory_Date When history was captured/updated }
    spFamilyMemberHistory_Gender, {@enum.value "gender" spFamilyMemberHistory_Gender A search by a gender code of a family member }
    spFamilyMemberHistory_Identifier, {@enum.value "identifier" spFamilyMemberHistory_Identifier A search by a record identifier }
    spFamilyMemberHistory_Patient, {@enum.value "patient" spFamilyMemberHistory_Patient The identity of a subject to list family member history items for }
    spFamilyMemberHistory_Relationship); {@enum.value "relationship" spFamilyMemberHistory_Relationship Search for family history of members based on relationship }
{$ENDIF}

{$IFDEF FHIR_FLAG}
  {@Enum TSearchParamsFlag
    Search Parameters for Flag
  }
  TSearchParamsFlag = (
    spFlag__content, {@enum.value "_content" spFlag__content Search on the entire content of the resource }
    spFlag__id, {@enum.value "_id" spFlag__id Logical id of this artifact }
    spFlag__lastUpdated, {@enum.value "_lastUpdated" spFlag__lastUpdated When the resource version last changed }
    spFlag__profile, {@enum.value "_profile" spFlag__profile Profiles this resource claims to conform to }
    spFlag__query, {@enum.value "_query" spFlag__query A custom search profile that describes a specific defined query operation }
    spFlag__security, {@enum.value "_security" spFlag__security Security Labels applied to this resource }
    spFlag__tag, {@enum.value "_tag" spFlag__tag Tags applied to this resource }
    spFlag__text, {@enum.value "_text" spFlag__text Search on the narrative of the resource }
    spFlag_Author, {@enum.value "author" spFlag_Author Flag creator }
    spFlag_Date, {@enum.value "date" spFlag_Date Time period when flag is active }
    spFlag_Encounter, {@enum.value "encounter" spFlag_Encounter Alert relevant during encounter }
    spFlag_Patient, {@enum.value "patient" spFlag_Patient The identity of a subject to list flags for }
    spFlag_Subject); {@enum.value "subject" spFlag_Subject The identity of a subject to list flags for }
{$ENDIF}

{$IFDEF FHIR_GOAL}
  {@Enum TSearchParamsGoal
    Search Parameters for Goal
  }
  TSearchParamsGoal = (
    spGoal__content, {@enum.value "_content" spGoal__content Search on the entire content of the resource }
    spGoal__id, {@enum.value "_id" spGoal__id Logical id of this artifact }
    spGoal__lastUpdated, {@enum.value "_lastUpdated" spGoal__lastUpdated When the resource version last changed }
    spGoal__profile, {@enum.value "_profile" spGoal__profile Profiles this resource claims to conform to }
    spGoal__query, {@enum.value "_query" spGoal__query A custom search profile that describes a specific defined query operation }
    spGoal__security, {@enum.value "_security" spGoal__security Security Labels applied to this resource }
    spGoal__tag, {@enum.value "_tag" spGoal__tag Tags applied to this resource }
    spGoal__text, {@enum.value "_text" spGoal__text Search on the narrative of the resource }
    spGoal_Category, {@enum.value "category" spGoal_Category E.g. Treatment, dietary, behavioral, etc. }
    spGoal_Identifier, {@enum.value "identifier" spGoal_Identifier External Ids for this goal }
    spGoal_Patient, {@enum.value "patient" spGoal_Patient Who this goal is intended for }
    spGoal_Status, {@enum.value "status" spGoal_Status proposed | planned | accepted | rejected | in-progress | achieved | sustaining | on-hold | cancelled }
    spGoal_Subject, {@enum.value "subject" spGoal_Subject Who this goal is intended for }
    spGoal_Targetdate); {@enum.value "targetdate" spGoal_Targetdate Reach goal on or before }
{$ENDIF}

{$IFDEF FHIR_GROUP}
  {@Enum TSearchParamsGroup
    Search Parameters for Group
  }
  TSearchParamsGroup = (
    spGroup__content, {@enum.value "_content" spGroup__content Search on the entire content of the resource }
    spGroup__id, {@enum.value "_id" spGroup__id Logical id of this artifact }
    spGroup__lastUpdated, {@enum.value "_lastUpdated" spGroup__lastUpdated When the resource version last changed }
    spGroup__profile, {@enum.value "_profile" spGroup__profile Profiles this resource claims to conform to }
    spGroup__query, {@enum.value "_query" spGroup__query A custom search profile that describes a specific defined query operation }
    spGroup__security, {@enum.value "_security" spGroup__security Security Labels applied to this resource }
    spGroup__tag, {@enum.value "_tag" spGroup__tag Tags applied to this resource }
    spGroup__text, {@enum.value "_text" spGroup__text Search on the narrative of the resource }
    spGroup_Actual, {@enum.value "actual" spGroup_Actual Descriptive or actual }
    spGroup_Characteristic, {@enum.value "characteristic" spGroup_Characteristic Kind of characteristic }
    spGroup_Characteristicvalue, {@enum.value "characteristic-value" spGroup_Characteristicvalue A composite of both characteristic and value }
    spGroup_Code, {@enum.value "code" spGroup_Code The kind of resources contained }
    spGroup_Exclude, {@enum.value "exclude" spGroup_Exclude Group includes or excludes }
    spGroup_Identifier, {@enum.value "identifier" spGroup_Identifier Unique id }
    spGroup_Member, {@enum.value "member" spGroup_Member Reference to the group member }
    spGroup_Type, {@enum.value "type" spGroup_Type The type of resources the group contains }
    spGroup_Value); {@enum.value "value" spGroup_Value Value held by characteristic }
{$ENDIF}

{$IFDEF FHIR_HEALTHCARESERVICE}
  {@Enum TSearchParamsHealthcareService
    Search Parameters for HealthcareService
  }
  TSearchParamsHealthcareService = (
    spHealthcareService__content, {@enum.value "_content" spHealthcareService__content Search on the entire content of the resource }
    spHealthcareService__id, {@enum.value "_id" spHealthcareService__id Logical id of this artifact }
    spHealthcareService__lastUpdated, {@enum.value "_lastUpdated" spHealthcareService__lastUpdated When the resource version last changed }
    spHealthcareService__profile, {@enum.value "_profile" spHealthcareService__profile Profiles this resource claims to conform to }
    spHealthcareService__query, {@enum.value "_query" spHealthcareService__query A custom search profile that describes a specific defined query operation }
    spHealthcareService__security, {@enum.value "_security" spHealthcareService__security Security Labels applied to this resource }
    spHealthcareService__tag, {@enum.value "_tag" spHealthcareService__tag Tags applied to this resource }
    spHealthcareService__text, {@enum.value "_text" spHealthcareService__text Search on the narrative of the resource }
    spHealthcareService_Characteristic, {@enum.value "characteristic" spHealthcareService_Characteristic One of the HealthcareService's characteristics }
    spHealthcareService_Identifier, {@enum.value "identifier" spHealthcareService_Identifier External identifiers for this item }
    spHealthcareService_Location, {@enum.value "location" spHealthcareService_Location The location of the Healthcare Service }
    spHealthcareService_Name, {@enum.value "name" spHealthcareService_Name A portion of the Healthcare service name }
    spHealthcareService_Organization, {@enum.value "organization" spHealthcareService_Organization The organization that provides this Healthcare Service }
    spHealthcareService_Programname, {@enum.value "programname" spHealthcareService_Programname One of the Program Names serviced by this HealthcareService }
    spHealthcareService_Servicecategory, {@enum.value "servicecategory" spHealthcareService_Servicecategory Service Category of the Healthcare Service }
    spHealthcareService_Servicetype); {@enum.value "servicetype" spHealthcareService_Servicetype The type of service provided by this healthcare service }
{$ENDIF}

{$IFDEF FHIR_IMAGINGOBJECTSELECTION}
  {@Enum TSearchParamsImagingObjectSelection
    Search Parameters for ImagingObjectSelection
  }
  TSearchParamsImagingObjectSelection = (
    spImagingObjectSelection__content, {@enum.value "_content" spImagingObjectSelection__content Search on the entire content of the resource }
    spImagingObjectSelection__id, {@enum.value "_id" spImagingObjectSelection__id Logical id of this artifact }
    spImagingObjectSelection__lastUpdated, {@enum.value "_lastUpdated" spImagingObjectSelection__lastUpdated When the resource version last changed }
    spImagingObjectSelection__profile, {@enum.value "_profile" spImagingObjectSelection__profile Profiles this resource claims to conform to }
    spImagingObjectSelection__query, {@enum.value "_query" spImagingObjectSelection__query A custom search profile that describes a specific defined query operation }
    spImagingObjectSelection__security, {@enum.value "_security" spImagingObjectSelection__security Security Labels applied to this resource }
    spImagingObjectSelection__tag, {@enum.value "_tag" spImagingObjectSelection__tag Tags applied to this resource }
    spImagingObjectSelection__text, {@enum.value "_text" spImagingObjectSelection__text Search on the narrative of the resource }
    spImagingObjectSelection_Author, {@enum.value "author" spImagingObjectSelection_Author Author of key DICOM object selection }
    spImagingObjectSelection_Authoringtime, {@enum.value "authoring-time" spImagingObjectSelection_Authoringtime Time of key DICOM object selection authoring }
    spImagingObjectSelection_Identifier, {@enum.value "identifier" spImagingObjectSelection_Identifier UID of key DICOM object selection }
    spImagingObjectSelection_Patient, {@enum.value "patient" spImagingObjectSelection_Patient Subject of key DICOM object selection }
    spImagingObjectSelection_Selectedstudy, {@enum.value "selected-study" spImagingObjectSelection_Selectedstudy Study selected in key DICOM object selection }
    spImagingObjectSelection_Title); {@enum.value "title" spImagingObjectSelection_Title Title of key DICOM object selection }
{$ENDIF}

{$IFDEF FHIR_IMAGINGSTUDY}
  {@Enum TSearchParamsImagingStudy
    Search Parameters for ImagingStudy
  }
  TSearchParamsImagingStudy = (
    spImagingStudy__content, {@enum.value "_content" spImagingStudy__content Search on the entire content of the resource }
    spImagingStudy__id, {@enum.value "_id" spImagingStudy__id Logical id of this artifact }
    spImagingStudy__lastUpdated, {@enum.value "_lastUpdated" spImagingStudy__lastUpdated When the resource version last changed }
    spImagingStudy__profile, {@enum.value "_profile" spImagingStudy__profile Profiles this resource claims to conform to }
    spImagingStudy__query, {@enum.value "_query" spImagingStudy__query A custom search profile that describes a specific defined query operation }
    spImagingStudy__security, {@enum.value "_security" spImagingStudy__security Security Labels applied to this resource }
    spImagingStudy__tag, {@enum.value "_tag" spImagingStudy__tag Tags applied to this resource }
    spImagingStudy__text, {@enum.value "_text" spImagingStudy__text Search on the narrative of the resource }
    spImagingStudy_Accession, {@enum.value "accession" spImagingStudy_Accession The accession identifier for the study }
    spImagingStudy_Bodysite, {@enum.value "bodysite" spImagingStudy_Bodysite The body site studied }
    spImagingStudy_Dicomclass, {@enum.value "dicom-class" spImagingStudy_Dicomclass The type of the instance }
    spImagingStudy_Modality, {@enum.value "modality" spImagingStudy_Modality The modality of the series }
    spImagingStudy_Order, {@enum.value "order" spImagingStudy_Order The order for the image }
    spImagingStudy_Patient, {@enum.value "patient" spImagingStudy_Patient Who the study is about }
    spImagingStudy_Series, {@enum.value "series" spImagingStudy_Series The identifier of the series of images }
    spImagingStudy_Started, {@enum.value "started" spImagingStudy_Started When the study was started }
    spImagingStudy_Study, {@enum.value "study" spImagingStudy_Study The study identifier for the image }
    spImagingStudy_Uid); {@enum.value "uid" spImagingStudy_Uid The instance unique identifier }
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATION}
  {@Enum TSearchParamsImmunization
    Search Parameters for Immunization
  }
  TSearchParamsImmunization = (
    spImmunization__content, {@enum.value "_content" spImmunization__content Search on the entire content of the resource }
    spImmunization__id, {@enum.value "_id" spImmunization__id Logical id of this artifact }
    spImmunization__lastUpdated, {@enum.value "_lastUpdated" spImmunization__lastUpdated When the resource version last changed }
    spImmunization__profile, {@enum.value "_profile" spImmunization__profile Profiles this resource claims to conform to }
    spImmunization__query, {@enum.value "_query" spImmunization__query A custom search profile that describes a specific defined query operation }
    spImmunization__security, {@enum.value "_security" spImmunization__security Security Labels applied to this resource }
    spImmunization__tag, {@enum.value "_tag" spImmunization__tag Tags applied to this resource }
    spImmunization__text, {@enum.value "_text" spImmunization__text Search on the narrative of the resource }
    spImmunization_Date, {@enum.value "date" spImmunization_Date Vaccination  (non)-Administration Date }
    spImmunization_Dosesequence, {@enum.value "dose-sequence" spImmunization_Dosesequence Dose number within series }
    spImmunization_Identifier, {@enum.value "identifier" spImmunization_Identifier Business identifier }
    spImmunization_Location, {@enum.value "location" spImmunization_Location The service delivery location or facility in which the vaccine was / was to be administered }
    spImmunization_Lotnumber, {@enum.value "lot-number" spImmunization_Lotnumber Vaccine Lot Number }
    spImmunization_Manufacturer, {@enum.value "manufacturer" spImmunization_Manufacturer Vaccine Manufacturer }
    spImmunization_Notgiven, {@enum.value "notgiven" spImmunization_Notgiven Administrations which were not given }
    spImmunization_Patient, {@enum.value "patient" spImmunization_Patient The patient for the vaccination record }
    spImmunization_Performer, {@enum.value "performer" spImmunization_Performer The practitioner who administered the vaccination }
    spImmunization_Reaction, {@enum.value "reaction" spImmunization_Reaction Additional information on reaction }
    spImmunization_Reactiondate, {@enum.value "reaction-date" spImmunization_Reactiondate When reaction started }
    spImmunization_Reason, {@enum.value "reason" spImmunization_Reason Why immunization occurred }
    spImmunization_Reasonnotgiven, {@enum.value "reason-not-given" spImmunization_Reasonnotgiven Explanation of reason vaccination was not administered }
    spImmunization_Requester, {@enum.value "requester" spImmunization_Requester The practitioner who ordered the vaccination }
    spImmunization_Status, {@enum.value "status" spImmunization_Status Immunization event status }
    spImmunization_Vaccinecode); {@enum.value "vaccine-code" spImmunization_Vaccinecode Vaccine Product Administered }
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  {@Enum TSearchParamsImmunizationRecommendation
    Search Parameters for ImmunizationRecommendation
  }
  TSearchParamsImmunizationRecommendation = (
    spImmunizationRecommendation__content, {@enum.value "_content" spImmunizationRecommendation__content Search on the entire content of the resource }
    spImmunizationRecommendation__id, {@enum.value "_id" spImmunizationRecommendation__id Logical id of this artifact }
    spImmunizationRecommendation__lastUpdated, {@enum.value "_lastUpdated" spImmunizationRecommendation__lastUpdated When the resource version last changed }
    spImmunizationRecommendation__profile, {@enum.value "_profile" spImmunizationRecommendation__profile Profiles this resource claims to conform to }
    spImmunizationRecommendation__query, {@enum.value "_query" spImmunizationRecommendation__query A custom search profile that describes a specific defined query operation }
    spImmunizationRecommendation__security, {@enum.value "_security" spImmunizationRecommendation__security Security Labels applied to this resource }
    spImmunizationRecommendation__tag, {@enum.value "_tag" spImmunizationRecommendation__tag Tags applied to this resource }
    spImmunizationRecommendation__text, {@enum.value "_text" spImmunizationRecommendation__text Search on the narrative of the resource }
    spImmunizationRecommendation_Date, {@enum.value "date" spImmunizationRecommendation_Date Date recommendation created }
    spImmunizationRecommendation_Dosenumber, {@enum.value "dose-number" spImmunizationRecommendation_Dosenumber Recommended dose number }
    spImmunizationRecommendation_Dosesequence, {@enum.value "dose-sequence" spImmunizationRecommendation_Dosesequence Dose number within sequence }
    spImmunizationRecommendation_Identifier, {@enum.value "identifier" spImmunizationRecommendation_Identifier Business identifier }
    spImmunizationRecommendation_Information, {@enum.value "information" spImmunizationRecommendation_Information Patient observations supporting recommendation }
    spImmunizationRecommendation_Patient, {@enum.value "patient" spImmunizationRecommendation_Patient Who this profile is for }
    spImmunizationRecommendation_Status, {@enum.value "status" spImmunizationRecommendation_Status Vaccine administration status }
    spImmunizationRecommendation_Support, {@enum.value "support" spImmunizationRecommendation_Support Past immunizations supporting recommendation }
    spImmunizationRecommendation_Vaccinetype); {@enum.value "vaccine-type" spImmunizationRecommendation_Vaccinetype Vaccine recommendation applies to }
{$ENDIF}

{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  {@Enum TSearchParamsImplementationGuide
    Search Parameters for ImplementationGuide
  }
  TSearchParamsImplementationGuide = (
    spImplementationGuide__content, {@enum.value "_content" spImplementationGuide__content Search on the entire content of the resource }
    spImplementationGuide__id, {@enum.value "_id" spImplementationGuide__id Logical id of this artifact }
    spImplementationGuide__lastUpdated, {@enum.value "_lastUpdated" spImplementationGuide__lastUpdated When the resource version last changed }
    spImplementationGuide__profile, {@enum.value "_profile" spImplementationGuide__profile Profiles this resource claims to conform to }
    spImplementationGuide__query, {@enum.value "_query" spImplementationGuide__query A custom search profile that describes a specific defined query operation }
    spImplementationGuide__security, {@enum.value "_security" spImplementationGuide__security Security Labels applied to this resource }
    spImplementationGuide__tag, {@enum.value "_tag" spImplementationGuide__tag Tags applied to this resource }
    spImplementationGuide__text, {@enum.value "_text" spImplementationGuide__text Search on the narrative of the resource }
    spImplementationGuide_Context, {@enum.value "context" spImplementationGuide_Context A use context assigned to the structure }
    spImplementationGuide_Date, {@enum.value "date" spImplementationGuide_Date The implementation guide publication date }
    spImplementationGuide_Dependency, {@enum.value "dependency" spImplementationGuide_Dependency Where to find dependency }
    spImplementationGuide_Description, {@enum.value "description" spImplementationGuide_Description Text search in the description of the implementation guide }
    spImplementationGuide_Experimental, {@enum.value "experimental" spImplementationGuide_Experimental If for testing purposes, not real usage }
    spImplementationGuide_Name, {@enum.value "name" spImplementationGuide_Name Name of the implementation guide }
    spImplementationGuide_Publisher, {@enum.value "publisher" spImplementationGuide_Publisher Name of the publisher of the implementation guide }
    spImplementationGuide_Status, {@enum.value "status" spImplementationGuide_Status The current status of the implementation guide }
    spImplementationGuide_Url, {@enum.value "url" spImplementationGuide_Url Absolute URL used to reference this Implementation Guide }
    spImplementationGuide_Version); {@enum.value "version" spImplementationGuide_Version The version identifier of the implementation guide }
{$ENDIF}

{$IFDEF FHIR_LIST}
  {@Enum TSearchParamsList
    Search Parameters for List
  }
  TSearchParamsList = (
    spList__content, {@enum.value "_content" spList__content Search on the entire content of the resource }
    spList__id, {@enum.value "_id" spList__id Logical id of this artifact }
    spList__lastUpdated, {@enum.value "_lastUpdated" spList__lastUpdated When the resource version last changed }
    spList__profile, {@enum.value "_profile" spList__profile Profiles this resource claims to conform to }
    spList__query, {@enum.value "_query" spList__query A custom search profile that describes a specific defined query operation }
    spList__security, {@enum.value "_security" spList__security Security Labels applied to this resource }
    spList__tag, {@enum.value "_tag" spList__tag Tags applied to this resource }
    spList__text, {@enum.value "_text" spList__text Search on the narrative of the resource }
    spList_Code, {@enum.value "code" spList_Code What the purpose of this list is }
    spList_Date, {@enum.value "date" spList_Date When the list was prepared }
    spList_Emptyreason, {@enum.value "empty-reason" spList_Emptyreason Why list is empty }
    spList_Encounter, {@enum.value "encounter" spList_Encounter Context in which list created }
    spList_Item, {@enum.value "item" spList_Item Actual entry }
    spList_Notes, {@enum.value "notes" spList_Notes Comments about the list }
    spList_Patient, {@enum.value "patient" spList_Patient If all resources have the same subject }
    spList_Source, {@enum.value "source" spList_Source Who and/or what defined the list contents (aka Author) }
    spList_Status, {@enum.value "status" spList_Status current | retired | entered-in-error }
    spList_Subject, {@enum.value "subject" spList_Subject If all resources have the same subject }
    spList_Title); {@enum.value "title" spList_Title Descriptive name for the list }
{$ENDIF}

{$IFDEF FHIR_LOCATION}
  {@Enum TSearchParamsLocation
    Search Parameters for Location
  }
  TSearchParamsLocation = (
    spLocation__content, {@enum.value "_content" spLocation__content Search on the entire content of the resource }
    spLocation__id, {@enum.value "_id" spLocation__id Logical id of this artifact }
    spLocation__lastUpdated, {@enum.value "_lastUpdated" spLocation__lastUpdated When the resource version last changed }
    spLocation__profile, {@enum.value "_profile" spLocation__profile Profiles this resource claims to conform to }
    spLocation__query, {@enum.value "_query" spLocation__query A custom search profile that describes a specific defined query operation }
    spLocation__security, {@enum.value "_security" spLocation__security Security Labels applied to this resource }
    spLocation__tag, {@enum.value "_tag" spLocation__tag Tags applied to this resource }
    spLocation__text, {@enum.value "_text" spLocation__text Search on the narrative of the resource }
    spLocation_Address, {@enum.value "address" spLocation_Address A (part of the) address of the location }
    spLocation_Addresscity, {@enum.value "address-city" spLocation_Addresscity A city specified in an address }
    spLocation_Addresscountry, {@enum.value "address-country" spLocation_Addresscountry A country specified in an address }
    spLocation_Addresspostalcode, {@enum.value "address-postalcode" spLocation_Addresspostalcode A postal code specified in an address }
    spLocation_Addressstate, {@enum.value "address-state" spLocation_Addressstate A state specified in an address }
    spLocation_Addressuse, {@enum.value "address-use" spLocation_Addressuse A use code specified in an address }
    spLocation_Identifier, {@enum.value "identifier" spLocation_Identifier Unique code or number identifying the location to its users }
    spLocation_Name, {@enum.value "name" spLocation_Name A (portion of the) name of the location }
    spLocation_Near, {@enum.value "near" spLocation_Near The coordinates expressed as [lat],[long] (using the WGS84 datum, see notes) to find locations near to (servers may search using a square rather than a circle for efficiency) }
    spLocation_Neardistance, {@enum.value "near-distance" spLocation_Neardistance A distance quantity to limit the near search to locations within a specific distance }
    spLocation_Organization, {@enum.value "organization" spLocation_Organization Searches for locations that are managed by the provided organization }
    spLocation_Partof, {@enum.value "partof" spLocation_Partof The location of which this location is a part }
    spLocation_Status, {@enum.value "status" spLocation_Status Searches for locations with a specific kind of status }
    spLocation_Type); {@enum.value "type" spLocation_Type A code for the type of location }
{$ENDIF}

{$IFDEF FHIR_MEDIA}
  {@Enum TSearchParamsMedia
    Search Parameters for Media
  }
  TSearchParamsMedia = (
    spMedia__content, {@enum.value "_content" spMedia__content Search on the entire content of the resource }
    spMedia__id, {@enum.value "_id" spMedia__id Logical id of this artifact }
    spMedia__lastUpdated, {@enum.value "_lastUpdated" spMedia__lastUpdated When the resource version last changed }
    spMedia__profile, {@enum.value "_profile" spMedia__profile Profiles this resource claims to conform to }
    spMedia__query, {@enum.value "_query" spMedia__query A custom search profile that describes a specific defined query operation }
    spMedia__security, {@enum.value "_security" spMedia__security Security Labels applied to this resource }
    spMedia__tag, {@enum.value "_tag" spMedia__tag Tags applied to this resource }
    spMedia__text, {@enum.value "_text" spMedia__text Search on the narrative of the resource }
    spMedia_Created, {@enum.value "created" spMedia_Created Date attachment was first created }
    spMedia_Identifier, {@enum.value "identifier" spMedia_Identifier Identifier(s) for the image }
    spMedia_Operator, {@enum.value "operator" spMedia_Operator The person who generated the image }
    spMedia_Patient, {@enum.value "patient" spMedia_Patient Who/What this Media is a record of }
    spMedia_Subject, {@enum.value "subject" spMedia_Subject Who/What this Media is a record of }
    spMedia_Subtype, {@enum.value "subtype" spMedia_Subtype The type of acquisition equipment/process }
    spMedia_Type, {@enum.value "type" spMedia_Type photo | video | audio }
    spMedia_View); {@enum.value "view" spMedia_View Imaging view, e.g. Lateral or Antero-posterior }
{$ENDIF}

{$IFDEF FHIR_MEDICATION}
  {@Enum TSearchParamsMedication
    Search Parameters for Medication
  }
  TSearchParamsMedication = (
    spMedication__content, {@enum.value "_content" spMedication__content Search on the entire content of the resource }
    spMedication__id, {@enum.value "_id" spMedication__id Logical id of this artifact }
    spMedication__lastUpdated, {@enum.value "_lastUpdated" spMedication__lastUpdated When the resource version last changed }
    spMedication__profile, {@enum.value "_profile" spMedication__profile Profiles this resource claims to conform to }
    spMedication__query, {@enum.value "_query" spMedication__query A custom search profile that describes a specific defined query operation }
    spMedication__security, {@enum.value "_security" spMedication__security Security Labels applied to this resource }
    spMedication__tag, {@enum.value "_tag" spMedication__tag Tags applied to this resource }
    spMedication__text, {@enum.value "_text" spMedication__text Search on the narrative of the resource }
    spMedication_Code, {@enum.value "code" spMedication_Code Codes that identify this medication }
    spMedication_Container, {@enum.value "container" spMedication_Container E.g. box, vial, blister-pack }
    spMedication_Content, {@enum.value "content" spMedication_Content A product in the package }
    spMedication_Form, {@enum.value "form" spMedication_Form powder | tablets | carton + }
    spMedication_Ingredient, {@enum.value "ingredient" spMedication_Ingredient The product contained }
    spMedication_Manufacturer); {@enum.value "manufacturer" spMedication_Manufacturer Manufacturer of the item }
{$ENDIF}

{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  {@Enum TSearchParamsMedicationAdministration
    Search Parameters for MedicationAdministration
  }
  TSearchParamsMedicationAdministration = (
    spMedicationAdministration__content, {@enum.value "_content" spMedicationAdministration__content Search on the entire content of the resource }
    spMedicationAdministration__id, {@enum.value "_id" spMedicationAdministration__id Logical id of this artifact }
    spMedicationAdministration__lastUpdated, {@enum.value "_lastUpdated" spMedicationAdministration__lastUpdated When the resource version last changed }
    spMedicationAdministration__profile, {@enum.value "_profile" spMedicationAdministration__profile Profiles this resource claims to conform to }
    spMedicationAdministration__query, {@enum.value "_query" spMedicationAdministration__query A custom search profile that describes a specific defined query operation }
    spMedicationAdministration__security, {@enum.value "_security" spMedicationAdministration__security Security Labels applied to this resource }
    spMedicationAdministration__tag, {@enum.value "_tag" spMedicationAdministration__tag Tags applied to this resource }
    spMedicationAdministration__text, {@enum.value "_text" spMedicationAdministration__text Search on the narrative of the resource }
    spMedicationAdministration_Code, {@enum.value "code" spMedicationAdministration_Code Return administrations of this medication code }
    spMedicationAdministration_Device, {@enum.value "device" spMedicationAdministration_Device Return administrations with this administration device identity }
    spMedicationAdministration_Effectivetime, {@enum.value "effectivetime" spMedicationAdministration_Effectivetime Date administration happened (or did not happen) }
    spMedicationAdministration_Encounter, {@enum.value "encounter" spMedicationAdministration_Encounter Return administrations that share this encounter }
    spMedicationAdministration_Identifier, {@enum.value "identifier" spMedicationAdministration_Identifier Return administrations with this external identifier }
    spMedicationAdministration_Medication, {@enum.value "medication" spMedicationAdministration_Medication Return administrations of this medication resource }
    spMedicationAdministration_Notgiven, {@enum.value "notgiven" spMedicationAdministration_Notgiven Administrations that were not made }
    spMedicationAdministration_Patient, {@enum.value "patient" spMedicationAdministration_Patient The identity of a patient to list administrations  for }
    spMedicationAdministration_Practitioner, {@enum.value "practitioner" spMedicationAdministration_Practitioner Who administered substance }
    spMedicationAdministration_Prescription, {@enum.value "prescription" spMedicationAdministration_Prescription The identity of a prescription to list administrations from }
    spMedicationAdministration_Status); {@enum.value "status" spMedicationAdministration_Status MedicationAdministration event status (for example one of active/paused/completed/nullified) }
{$ENDIF}

{$IFDEF FHIR_MEDICATIONDISPENSE}
  {@Enum TSearchParamsMedicationDispense
    Search Parameters for MedicationDispense
  }
  TSearchParamsMedicationDispense = (
    spMedicationDispense__content, {@enum.value "_content" spMedicationDispense__content Search on the entire content of the resource }
    spMedicationDispense__id, {@enum.value "_id" spMedicationDispense__id Logical id of this artifact }
    spMedicationDispense__lastUpdated, {@enum.value "_lastUpdated" spMedicationDispense__lastUpdated When the resource version last changed }
    spMedicationDispense__profile, {@enum.value "_profile" spMedicationDispense__profile Profiles this resource claims to conform to }
    spMedicationDispense__query, {@enum.value "_query" spMedicationDispense__query A custom search profile that describes a specific defined query operation }
    spMedicationDispense__security, {@enum.value "_security" spMedicationDispense__security Security Labels applied to this resource }
    spMedicationDispense__tag, {@enum.value "_tag" spMedicationDispense__tag Tags applied to this resource }
    spMedicationDispense__text, {@enum.value "_text" spMedicationDispense__text Search on the narrative of the resource }
    spMedicationDispense_Code, {@enum.value "code" spMedicationDispense_Code Return dispenses of this medicine code }
    spMedicationDispense_Destination, {@enum.value "destination" spMedicationDispense_Destination Return dispenses that should be sent to a specific destination }
    spMedicationDispense_Dispenser, {@enum.value "dispenser" spMedicationDispense_Dispenser Return all dispenses performed by a specific individual }
    spMedicationDispense_Identifier, {@enum.value "identifier" spMedicationDispense_Identifier Return dispenses with this external identifier }
    spMedicationDispense_Medication, {@enum.value "medication" spMedicationDispense_Medication Return dispenses of this medicine resource }
    spMedicationDispense_Patient, {@enum.value "patient" spMedicationDispense_Patient The identity of a patient to list dispenses  for }
    spMedicationDispense_Prescription, {@enum.value "prescription" spMedicationDispense_Prescription The identity of a prescription to list dispenses from }
    spMedicationDispense_Receiver, {@enum.value "receiver" spMedicationDispense_Receiver Who collected the medication }
    spMedicationDispense_Responsibleparty, {@enum.value "responsibleparty" spMedicationDispense_Responsibleparty Return all dispenses with the specified responsible party }
    spMedicationDispense_Status, {@enum.value "status" spMedicationDispense_Status Status of the dispense }
    spMedicationDispense_Type, {@enum.value "type" spMedicationDispense_Type Return all dispenses of a specific type }
    spMedicationDispense_Whenhandedover, {@enum.value "whenhandedover" spMedicationDispense_Whenhandedover Date when medication handed over to patient (outpatient setting), or supplied to ward or clinic (inpatient setting) }
    spMedicationDispense_Whenprepared); {@enum.value "whenprepared" spMedicationDispense_Whenprepared Date when medication prepared }
{$ENDIF}

{$IFDEF FHIR_MEDICATIONORDER}
  {@Enum TSearchParamsMedicationOrder
    Search Parameters for MedicationOrder
  }
  TSearchParamsMedicationOrder = (
    spMedicationOrder__content, {@enum.value "_content" spMedicationOrder__content Search on the entire content of the resource }
    spMedicationOrder__id, {@enum.value "_id" spMedicationOrder__id Logical id of this artifact }
    spMedicationOrder__lastUpdated, {@enum.value "_lastUpdated" spMedicationOrder__lastUpdated When the resource version last changed }
    spMedicationOrder__profile, {@enum.value "_profile" spMedicationOrder__profile Profiles this resource claims to conform to }
    spMedicationOrder__query, {@enum.value "_query" spMedicationOrder__query A custom search profile that describes a specific defined query operation }
    spMedicationOrder__security, {@enum.value "_security" spMedicationOrder__security Security Labels applied to this resource }
    spMedicationOrder__tag, {@enum.value "_tag" spMedicationOrder__tag Tags applied to this resource }
    spMedicationOrder__text, {@enum.value "_text" spMedicationOrder__text Search on the narrative of the resource }
    spMedicationOrder_Code, {@enum.value "code" spMedicationOrder_Code Return administrations of this medication code }
    spMedicationOrder_Datewritten, {@enum.value "datewritten" spMedicationOrder_Datewritten Return prescriptions written on this date }
    spMedicationOrder_Encounter, {@enum.value "encounter" spMedicationOrder_Encounter Return prescriptions with this encounter identifier }
    spMedicationOrder_Identifier, {@enum.value "identifier" spMedicationOrder_Identifier Return prescriptions with this external identifier }
    spMedicationOrder_Medication, {@enum.value "medication" spMedicationOrder_Medication Return administrations of this medication reference }
    spMedicationOrder_Patient, {@enum.value "patient" spMedicationOrder_Patient The identity of a patient to list orders  for }
    spMedicationOrder_Prescriber, {@enum.value "prescriber" spMedicationOrder_Prescriber Who ordered the medication(s) }
    spMedicationOrder_Status); {@enum.value "status" spMedicationOrder_Status Status of the prescription }
{$ENDIF}

{$IFDEF FHIR_MEDICATIONSTATEMENT}
  {@Enum TSearchParamsMedicationStatement
    Search Parameters for MedicationStatement
  }
  TSearchParamsMedicationStatement = (
    spMedicationStatement__content, {@enum.value "_content" spMedicationStatement__content Search on the entire content of the resource }
    spMedicationStatement__id, {@enum.value "_id" spMedicationStatement__id Logical id of this artifact }
    spMedicationStatement__lastUpdated, {@enum.value "_lastUpdated" spMedicationStatement__lastUpdated When the resource version last changed }
    spMedicationStatement__profile, {@enum.value "_profile" spMedicationStatement__profile Profiles this resource claims to conform to }
    spMedicationStatement__query, {@enum.value "_query" spMedicationStatement__query A custom search profile that describes a specific defined query operation }
    spMedicationStatement__security, {@enum.value "_security" spMedicationStatement__security Security Labels applied to this resource }
    spMedicationStatement__tag, {@enum.value "_tag" spMedicationStatement__tag Tags applied to this resource }
    spMedicationStatement__text, {@enum.value "_text" spMedicationStatement__text Search on the narrative of the resource }
    spMedicationStatement_Code, {@enum.value "code" spMedicationStatement_Code Return administrations of this medication code }
    spMedicationStatement_Effectivedate, {@enum.value "effectivedate" spMedicationStatement_Effectivedate Date when patient was taking (or not taking) the medication }
    spMedicationStatement_Identifier, {@enum.value "identifier" spMedicationStatement_Identifier Return statements with this external identifier }
    spMedicationStatement_Medication, {@enum.value "medication" spMedicationStatement_Medication Return administrations of this medication reference }
    spMedicationStatement_Patient, {@enum.value "patient" spMedicationStatement_Patient The identity of a patient to list statements  for }
    spMedicationStatement_Source, {@enum.value "source" spMedicationStatement_Source Who the information in the statement came from }
    spMedicationStatement_Status); {@enum.value "status" spMedicationStatement_Status Return statements that match the given status }
{$ENDIF}

{$IFDEF FHIR_MESSAGEHEADER}
  {@Enum TSearchParamsMessageHeader
    Search Parameters for MessageHeader
  }
  TSearchParamsMessageHeader = (
    spMessageHeader__content, {@enum.value "_content" spMessageHeader__content Search on the entire content of the resource }
    spMessageHeader__id, {@enum.value "_id" spMessageHeader__id Logical id of this artifact }
    spMessageHeader__lastUpdated, {@enum.value "_lastUpdated" spMessageHeader__lastUpdated When the resource version last changed }
    spMessageHeader__profile, {@enum.value "_profile" spMessageHeader__profile Profiles this resource claims to conform to }
    spMessageHeader__query, {@enum.value "_query" spMessageHeader__query A custom search profile that describes a specific defined query operation }
    spMessageHeader__security, {@enum.value "_security" spMessageHeader__security Security Labels applied to this resource }
    spMessageHeader__tag, {@enum.value "_tag" spMessageHeader__tag Tags applied to this resource }
    spMessageHeader__text, {@enum.value "_text" spMessageHeader__text Search on the narrative of the resource }
    spMessageHeader_Author, {@enum.value "author" spMessageHeader_Author The source of the decision }
    spMessageHeader_Code, {@enum.value "code" spMessageHeader_Code ok | transient-error | fatal-error }
    spMessageHeader_Data, {@enum.value "data" spMessageHeader_Data The actual content of the message }
    spMessageHeader_Destination, {@enum.value "destination" spMessageHeader_Destination Name of system }
    spMessageHeader_Destinationuri, {@enum.value "destination-uri" spMessageHeader_Destinationuri Actual destination address or id }
    spMessageHeader_Enterer, {@enum.value "enterer" spMessageHeader_Enterer The source of the data entry }
    spMessageHeader_Event, {@enum.value "event" spMessageHeader_Event Code for the event this message represents }
    spMessageHeader_Receiver, {@enum.value "receiver" spMessageHeader_Receiver Intended "real-world" recipient for the data }
    spMessageHeader_Responseid, {@enum.value "response-id" spMessageHeader_Responseid Id of original message }
    spMessageHeader_Responsible, {@enum.value "responsible" spMessageHeader_Responsible Final responsibility for event }
    spMessageHeader_Source, {@enum.value "source" spMessageHeader_Source Name of system }
    spMessageHeader_Sourceuri, {@enum.value "source-uri" spMessageHeader_Sourceuri Actual message source address or id }
    spMessageHeader_Target, {@enum.value "target" spMessageHeader_Target Particular delivery destination within the destination }
    spMessageHeader_Timestamp); {@enum.value "timestamp" spMessageHeader_Timestamp Time that the message was sent }
{$ENDIF}

{$IFDEF FHIR_NAMINGSYSTEM}
  {@Enum TSearchParamsNamingSystem
    Search Parameters for NamingSystem
  }
  TSearchParamsNamingSystem = (
    spNamingSystem__content, {@enum.value "_content" spNamingSystem__content Search on the entire content of the resource }
    spNamingSystem__id, {@enum.value "_id" spNamingSystem__id Logical id of this artifact }
    spNamingSystem__lastUpdated, {@enum.value "_lastUpdated" spNamingSystem__lastUpdated When the resource version last changed }
    spNamingSystem__profile, {@enum.value "_profile" spNamingSystem__profile Profiles this resource claims to conform to }
    spNamingSystem__query, {@enum.value "_query" spNamingSystem__query A custom search profile that describes a specific defined query operation }
    spNamingSystem__security, {@enum.value "_security" spNamingSystem__security Security Labels applied to this resource }
    spNamingSystem__tag, {@enum.value "_tag" spNamingSystem__tag Tags applied to this resource }
    spNamingSystem__text, {@enum.value "_text" spNamingSystem__text Search on the narrative of the resource }
    spNamingSystem_Contact, {@enum.value "contact" spNamingSystem_Contact Name of a individual to contact }
    spNamingSystem_Context, {@enum.value "context" spNamingSystem_Context Content intends to support these contexts }
    spNamingSystem_Date, {@enum.value "date" spNamingSystem_Date Publication Date(/time) }
    spNamingSystem_Idtype, {@enum.value "id-type" spNamingSystem_Idtype oid | uuid | uri | other }
    spNamingSystem_Kind, {@enum.value "kind" spNamingSystem_Kind codesystem | identifier | root }
    spNamingSystem_Name, {@enum.value "name" spNamingSystem_Name Human-readable label }
    spNamingSystem_Period, {@enum.value "period" spNamingSystem_Period When is identifier valid? }
    spNamingSystem_Publisher, {@enum.value "publisher" spNamingSystem_Publisher Name of the publisher (Organization or individual) }
    spNamingSystem_Replacedby, {@enum.value "replaced-by" spNamingSystem_Replacedby Use this instead }
    spNamingSystem_Responsible, {@enum.value "responsible" spNamingSystem_Responsible Who maintains system namespace? }
    spNamingSystem_Status, {@enum.value "status" spNamingSystem_Status draft | active | retired }
    spNamingSystem_Telecom, {@enum.value "telecom" spNamingSystem_Telecom Contact details for individual or publisher }
    spNamingSystem_Type, {@enum.value "type" spNamingSystem_Type e.g. driver,  provider,  patient, bank etc. }
    spNamingSystem_Value); {@enum.value "value" spNamingSystem_Value The unique identifier }
{$ENDIF}

{$IFDEF FHIR_NUTRITIONORDER}
  {@Enum TSearchParamsNutritionOrder
    Search Parameters for NutritionOrder
  }
  TSearchParamsNutritionOrder = (
    spNutritionOrder__content, {@enum.value "_content" spNutritionOrder__content Search on the entire content of the resource }
    spNutritionOrder__id, {@enum.value "_id" spNutritionOrder__id Logical id of this artifact }
    spNutritionOrder__lastUpdated, {@enum.value "_lastUpdated" spNutritionOrder__lastUpdated When the resource version last changed }
    spNutritionOrder__profile, {@enum.value "_profile" spNutritionOrder__profile Profiles this resource claims to conform to }
    spNutritionOrder__query, {@enum.value "_query" spNutritionOrder__query A custom search profile that describes a specific defined query operation }
    spNutritionOrder__security, {@enum.value "_security" spNutritionOrder__security Security Labels applied to this resource }
    spNutritionOrder__tag, {@enum.value "_tag" spNutritionOrder__tag Tags applied to this resource }
    spNutritionOrder__text, {@enum.value "_text" spNutritionOrder__text Search on the narrative of the resource }
    spNutritionOrder_Additive, {@enum.value "additive" spNutritionOrder_Additive Type of module component to add to the feeding }
    spNutritionOrder_Datetime, {@enum.value "datetime" spNutritionOrder_Datetime Return nutrition orders requested on this date }
    spNutritionOrder_Encounter, {@enum.value "encounter" spNutritionOrder_Encounter Return nutrition orders with this encounter identifier }
    spNutritionOrder_Formula, {@enum.value "formula" spNutritionOrder_Formula Type of enteral or infant formula }
    spNutritionOrder_Identifier, {@enum.value "identifier" spNutritionOrder_Identifier Return nutrition orders with this external identifier }
    spNutritionOrder_Oraldiet, {@enum.value "oraldiet" spNutritionOrder_Oraldiet Type of diet that can be consumed orally (i.e., take via the mouth). }
    spNutritionOrder_Patient, {@enum.value "patient" spNutritionOrder_Patient The identity of the person who requires the diet, formula or nutritional supplement }
    spNutritionOrder_Provider, {@enum.value "provider" spNutritionOrder_Provider The identify of the provider who placed the nutrition order }
    spNutritionOrder_Status, {@enum.value "status" spNutritionOrder_Status Status of the nutrition order. }
    spNutritionOrder_Supplement); {@enum.value "supplement" spNutritionOrder_Supplement Type of supplement product requested }
{$ENDIF}

{$IFDEF FHIR_OBSERVATION}
  {@Enum TSearchParamsObservation
    Search Parameters for Observation
  }
  TSearchParamsObservation = (
    spObservation__content, {@enum.value "_content" spObservation__content Search on the entire content of the resource }
    spObservation__id, {@enum.value "_id" spObservation__id Logical id of this artifact }
    spObservation__lastUpdated, {@enum.value "_lastUpdated" spObservation__lastUpdated When the resource version last changed }
    spObservation__profile, {@enum.value "_profile" spObservation__profile Profiles this resource claims to conform to }
    spObservation__query, {@enum.value "_query" spObservation__query A custom search profile that describes a specific defined query operation }
    spObservation__security, {@enum.value "_security" spObservation__security Security Labels applied to this resource }
    spObservation__tag, {@enum.value "_tag" spObservation__tag Tags applied to this resource }
    spObservation__text, {@enum.value "_text" spObservation__text Search on the narrative of the resource }
    spObservation_Category, {@enum.value "category" spObservation_Category The classification of the type of observation }
    spObservation_Code, {@enum.value "code" spObservation_Code The code of the observation type }
    spObservation_Codevaluex, {@enum.value "code-value-[x]" spObservation_Codevaluex Both code and one of the value parameters }
    spObservation_Componentcode, {@enum.value "component-code" spObservation_Componentcode The component code of the observation type }
    spObservation_Componentcodevaluex, {@enum.value "component-code-value-[x]" spObservation_Componentcodevaluex Both component code and one of the component value parameters }
    spObservation_Componentdataabsentreason, {@enum.value "component-data-absent-reason" spObservation_Componentdataabsentreason The reason why the expected value in the element Observation.component.value[x] is missing. }
    spObservation_Componentvalueconcept, {@enum.value "component-value-concept" spObservation_Componentvalueconcept The value of the component observation, if the value is a CodeableConcept }
    spObservation_Componentvaluequantity, {@enum.value "component-value-quantity" spObservation_Componentvaluequantity The value of the component observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data) }
    spObservation_Componentvaluestring, {@enum.value "component-value-string" spObservation_Componentvaluestring The value of the component observation, if the value is a string, and also searches in CodeableConcept.text }
    spObservation_Dataabsentreason, {@enum.value "data-absent-reason" spObservation_Dataabsentreason The reason why the expected value in the element Observation.value[x] is missing. }
    spObservation_Date, {@enum.value "date" spObservation_Date Obtained date/time. If the obtained element is a period, a date that falls in the period }
    spObservation_Device, {@enum.value "device" spObservation_Device The Device that generated the observation data. }
    spObservation_Encounter, {@enum.value "encounter" spObservation_Encounter Healthcare event related to the observation }
    spObservation_Identifier, {@enum.value "identifier" spObservation_Identifier The unique id for a particular observation }
    spObservation_Patient, {@enum.value "patient" spObservation_Patient The subject that the observation is about (if patient) }
    spObservation_Performer, {@enum.value "performer" spObservation_Performer Who performed the observation }
    spObservation_Related, {@enum.value "related" spObservation_Related Related Observations - search on related-type and related-target together }
    spObservation_Relatedtarget, {@enum.value "related-target" spObservation_Relatedtarget Resource that is related to this one }
    spObservation_Relatedtype, {@enum.value "related-type" spObservation_Relatedtype has-member | derived-from | sequel-to | replaces | qualified-by | interfered-by }
    spObservation_Specimen, {@enum.value "specimen" spObservation_Specimen Specimen used for this observation }
    spObservation_Status, {@enum.value "status" spObservation_Status The status of the observation }
    spObservation_Subject, {@enum.value "subject" spObservation_Subject The subject that the observation is about }
    spObservation_Valueconcept, {@enum.value "value-concept" spObservation_Valueconcept The value of the observation, if the value is a CodeableConcept }
    spObservation_Valuedate, {@enum.value "value-date" spObservation_Valuedate The value of the observation, if the value is a date or period of time }
    spObservation_Valuequantity, {@enum.value "value-quantity" spObservation_Valuequantity The value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data) }
    spObservation_Valuestring); {@enum.value "value-string" spObservation_Valuestring The value of the observation, if the value is a string, and also searches in CodeableConcept.text }
{$ENDIF}

{$IFDEF FHIR_OPERATIONDEFINITION}
  {@Enum TSearchParamsOperationDefinition
    Search Parameters for OperationDefinition
  }
  TSearchParamsOperationDefinition = (
    spOperationDefinition__content, {@enum.value "_content" spOperationDefinition__content Search on the entire content of the resource }
    spOperationDefinition__id, {@enum.value "_id" spOperationDefinition__id Logical id of this artifact }
    spOperationDefinition__lastUpdated, {@enum.value "_lastUpdated" spOperationDefinition__lastUpdated When the resource version last changed }
    spOperationDefinition__profile, {@enum.value "_profile" spOperationDefinition__profile Profiles this resource claims to conform to }
    spOperationDefinition__query, {@enum.value "_query" spOperationDefinition__query A custom search profile that describes a specific defined query operation }
    spOperationDefinition__security, {@enum.value "_security" spOperationDefinition__security Security Labels applied to this resource }
    spOperationDefinition__tag, {@enum.value "_tag" spOperationDefinition__tag Tags applied to this resource }
    spOperationDefinition__text, {@enum.value "_text" spOperationDefinition__text Search on the narrative of the resource }
    spOperationDefinition_Base, {@enum.value "base" spOperationDefinition_Base Marks this as a profile of the base }
    spOperationDefinition_Code, {@enum.value "code" spOperationDefinition_Code Name used to invoke the operation }
    spOperationDefinition_Date, {@enum.value "date" spOperationDefinition_Date Date for this version of the operation definition }
    spOperationDefinition_Instance, {@enum.value "instance" spOperationDefinition_Instance Invoke on an instance? }
    spOperationDefinition_Kind, {@enum.value "kind" spOperationDefinition_Kind operation | query }
    spOperationDefinition_Name, {@enum.value "name" spOperationDefinition_Name Informal name for this operation }
    spOperationDefinition_Profile, {@enum.value "profile" spOperationDefinition_Profile Profile on the type }
    spOperationDefinition_Publisher, {@enum.value "publisher" spOperationDefinition_Publisher Name of the publisher (Organization or individual) }
    spOperationDefinition_Status, {@enum.value "status" spOperationDefinition_Status draft | active | retired }
    spOperationDefinition_System, {@enum.value "system" spOperationDefinition_System Invoke at the system level? }
    spOperationDefinition_Type, {@enum.value "type" spOperationDefinition_Type Invoke at resource level for these type }
    spOperationDefinition_Url, {@enum.value "url" spOperationDefinition_Url Logical URL to reference this operation definition }
    spOperationDefinition_Version); {@enum.value "version" spOperationDefinition_Version Logical id for this version of the operation definition }
{$ENDIF}

{$IFDEF FHIR_OPERATIONOUTCOME}
  {@Enum TSearchParamsOperationOutcome
    Search Parameters for OperationOutcome
  }
  TSearchParamsOperationOutcome = (
    spOperationOutcome__content, {@enum.value "_content" spOperationOutcome__content Search on the entire content of the resource }
    spOperationOutcome__id, {@enum.value "_id" spOperationOutcome__id Logical id of this artifact }
    spOperationOutcome__lastUpdated, {@enum.value "_lastUpdated" spOperationOutcome__lastUpdated When the resource version last changed }
    spOperationOutcome__profile, {@enum.value "_profile" spOperationOutcome__profile Profiles this resource claims to conform to }
    spOperationOutcome__query, {@enum.value "_query" spOperationOutcome__query A custom search profile that describes a specific defined query operation }
    spOperationOutcome__security, {@enum.value "_security" spOperationOutcome__security Security Labels applied to this resource }
    spOperationOutcome__tag, {@enum.value "_tag" spOperationOutcome__tag Tags applied to this resource }
    spOperationOutcome__text); {@enum.value "_text" spOperationOutcome__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_ORDER}
  {@Enum TSearchParamsOrder
    Search Parameters for Order
  }
  TSearchParamsOrder = (
    spOrder__content, {@enum.value "_content" spOrder__content Search on the entire content of the resource }
    spOrder__id, {@enum.value "_id" spOrder__id Logical id of this artifact }
    spOrder__lastUpdated, {@enum.value "_lastUpdated" spOrder__lastUpdated When the resource version last changed }
    spOrder__profile, {@enum.value "_profile" spOrder__profile Profiles this resource claims to conform to }
    spOrder__query, {@enum.value "_query" spOrder__query A custom search profile that describes a specific defined query operation }
    spOrder__security, {@enum.value "_security" spOrder__security Security Labels applied to this resource }
    spOrder__tag, {@enum.value "_tag" spOrder__tag Tags applied to this resource }
    spOrder__text, {@enum.value "_text" spOrder__text Search on the narrative of the resource }
    spOrder_Date, {@enum.value "date" spOrder_Date When the order was made }
    spOrder_Detail, {@enum.value "detail" spOrder_Detail What action is being ordered }
    spOrder_Identifier, {@enum.value "identifier" spOrder_Identifier Instance id from source, target, and/or  others }
    spOrder_Patient, {@enum.value "patient" spOrder_Patient Patient this order is about }
    spOrder_Source, {@enum.value "source" spOrder_Source Who initiated the order }
    spOrder_Subject, {@enum.value "subject" spOrder_Subject Patient this order is about }
    spOrder_Target, {@enum.value "target" spOrder_Target Who is intended to fulfill the order }
    spOrder_When, {@enum.value "when" spOrder_When A formal schedule }
    spOrder_When_code); {@enum.value "when_code" spOrder_When_code Code specifies when request should be done. The code may simply be a priority code }
{$ENDIF}

{$IFDEF FHIR_ORDERRESPONSE}
  {@Enum TSearchParamsOrderResponse
    Search Parameters for OrderResponse
  }
  TSearchParamsOrderResponse = (
    spOrderResponse__content, {@enum.value "_content" spOrderResponse__content Search on the entire content of the resource }
    spOrderResponse__id, {@enum.value "_id" spOrderResponse__id Logical id of this artifact }
    spOrderResponse__lastUpdated, {@enum.value "_lastUpdated" spOrderResponse__lastUpdated When the resource version last changed }
    spOrderResponse__profile, {@enum.value "_profile" spOrderResponse__profile Profiles this resource claims to conform to }
    spOrderResponse__query, {@enum.value "_query" spOrderResponse__query A custom search profile that describes a specific defined query operation }
    spOrderResponse__security, {@enum.value "_security" spOrderResponse__security Security Labels applied to this resource }
    spOrderResponse__tag, {@enum.value "_tag" spOrderResponse__tag Tags applied to this resource }
    spOrderResponse__text, {@enum.value "_text" spOrderResponse__text Search on the narrative of the resource }
    spOrderResponse_Code, {@enum.value "code" spOrderResponse_Code pending | review | rejected | error | accepted | cancelled | replaced | aborted | completed }
    spOrderResponse_Date, {@enum.value "date" spOrderResponse_Date When the response was made }
    spOrderResponse_Fulfillment, {@enum.value "fulfillment" spOrderResponse_Fulfillment Details of the outcome of performing the order }
    spOrderResponse_Identifier, {@enum.value "identifier" spOrderResponse_Identifier Identifiers assigned to this order by the orderer or by the receiver }
    spOrderResponse_Request, {@enum.value "request" spOrderResponse_Request The order that this is a response to }
    spOrderResponse_Who); {@enum.value "who" spOrderResponse_Who Who made the response }
{$ENDIF}

{$IFDEF FHIR_ORGANIZATION}
  {@Enum TSearchParamsOrganization
    Search Parameters for Organization
  }
  TSearchParamsOrganization = (
    spOrganization__content, {@enum.value "_content" spOrganization__content Search on the entire content of the resource }
    spOrganization__id, {@enum.value "_id" spOrganization__id Logical id of this artifact }
    spOrganization__lastUpdated, {@enum.value "_lastUpdated" spOrganization__lastUpdated When the resource version last changed }
    spOrganization__profile, {@enum.value "_profile" spOrganization__profile Profiles this resource claims to conform to }
    spOrganization__query, {@enum.value "_query" spOrganization__query A custom search profile that describes a specific defined query operation }
    spOrganization__security, {@enum.value "_security" spOrganization__security Security Labels applied to this resource }
    spOrganization__tag, {@enum.value "_tag" spOrganization__tag Tags applied to this resource }
    spOrganization__text, {@enum.value "_text" spOrganization__text Search on the narrative of the resource }
    spOrganization_Active, {@enum.value "active" spOrganization_Active Whether the organization's record is active }
    spOrganization_Address, {@enum.value "address" spOrganization_Address A (part of the) address of the Organization }
    spOrganization_Addresscity, {@enum.value "address-city" spOrganization_Addresscity A city specified in an address }
    spOrganization_Addresscountry, {@enum.value "address-country" spOrganization_Addresscountry A country specified in an address }
    spOrganization_Addresspostalcode, {@enum.value "address-postalcode" spOrganization_Addresspostalcode A postal code specified in an address }
    spOrganization_Addressstate, {@enum.value "address-state" spOrganization_Addressstate A state specified in an address }
    spOrganization_Addressuse, {@enum.value "address-use" spOrganization_Addressuse A use code specified in an address }
    spOrganization_Identifier, {@enum.value "identifier" spOrganization_Identifier Any identifier for the organization (not the accreditation issuer's identifier) }
    spOrganization_Name, {@enum.value "name" spOrganization_Name A portion of the organization's name }
    spOrganization_Partof, {@enum.value "partof" spOrganization_Partof Search all organizations that are part of the given organization }
    spOrganization_Phonetic, {@enum.value "phonetic" spOrganization_Phonetic A portion of the organization's name using some kind of phonetic matching algorithm }
    spOrganization_Type); {@enum.value "type" spOrganization_Type A code for the type of organization }
{$ENDIF}

{$IFDEF FHIR_PATIENT}
  {@Enum TSearchParamsPatient
    Search Parameters for Patient
  }
  TSearchParamsPatient = (
    spPatient__content, {@enum.value "_content" spPatient__content Search on the entire content of the resource }
    spPatient__id, {@enum.value "_id" spPatient__id Logical id of this artifact }
    spPatient__lastUpdated, {@enum.value "_lastUpdated" spPatient__lastUpdated When the resource version last changed }
    spPatient__profile, {@enum.value "_profile" spPatient__profile Profiles this resource claims to conform to }
    spPatient__query, {@enum.value "_query" spPatient__query A custom search profile that describes a specific defined query operation }
    spPatient__security, {@enum.value "_security" spPatient__security Security Labels applied to this resource }
    spPatient__tag, {@enum.value "_tag" spPatient__tag Tags applied to this resource }
    spPatient__text, {@enum.value "_text" spPatient__text Search on the narrative of the resource }
    spPatient_Active, {@enum.value "active" spPatient_Active Whether the patient record is active }
    spPatient_Address, {@enum.value "address" spPatient_Address An address in any kind of address/part of the patient }
    spPatient_Addresscity, {@enum.value "address-city" spPatient_Addresscity A city specified in an address }
    spPatient_Addresscountry, {@enum.value "address-country" spPatient_Addresscountry A country specified in an address }
    spPatient_Addresspostalcode, {@enum.value "address-postalcode" spPatient_Addresspostalcode A postalCode specified in an address }
    spPatient_Addressstate, {@enum.value "address-state" spPatient_Addressstate A state specified in an address }
    spPatient_Addressuse, {@enum.value "address-use" spPatient_Addressuse A use code specified in an address }
    spPatient_Animalbreed, {@enum.value "animal-breed" spPatient_Animalbreed The breed for animal patients }
    spPatient_Animalspecies, {@enum.value "animal-species" spPatient_Animalspecies The species for animal patients }
    spPatient_Birthdate, {@enum.value "birthdate" spPatient_Birthdate The patient's date of birth }
    spPatient_Careprovider, {@enum.value "careprovider" spPatient_Careprovider Patient's nominated care provider, could be a care manager, not the organization that manages the record }
    spPatient_Deathdate, {@enum.value "deathdate" spPatient_Deathdate The date of death has been provided and satisfies this search value }
    spPatient_Deceased, {@enum.value "deceased" spPatient_Deceased This patient has been marked as deceased, or as a death date entered }
    spPatient_Email, {@enum.value "email" spPatient_Email A value in an email contact }
    spPatient_Ethnicity, {@enum.value "ethnicity" spPatient_Ethnicity Returns patients with an ethnicity extension matching the specified code. }
    spPatient_Family, {@enum.value "family" spPatient_Family A portion of the family name of the patient }
    spPatient_Gender, {@enum.value "gender" spPatient_Gender Gender of the patient }
    spPatient_Given, {@enum.value "given" spPatient_Given A portion of the given name of the patient }
    spPatient_Identifier, {@enum.value "identifier" spPatient_Identifier A patient identifier }
    spPatient_Language, {@enum.value "language" spPatient_Language Language code (irrespective of use value) }
    spPatient_Link, {@enum.value "link" spPatient_Link All patients linked to the given patient }
    spPatient_Name, {@enum.value "name" spPatient_Name A portion of either family or given name of the patient }
    spPatient_Organization, {@enum.value "organization" spPatient_Organization The organization at which this person is a patient }
    spPatient_Phone, {@enum.value "phone" spPatient_Phone A value in a phone contact }
    spPatient_Phonetic, {@enum.value "phonetic" spPatient_Phonetic A portion of either family or given name using some kind of phonetic matching algorithm }
    spPatient_Race, {@enum.value "race" spPatient_Race Returns patients with a race extension matching the specified code. }
    spPatient_Telecom); {@enum.value "telecom" spPatient_Telecom The value in any kind of telecom details of the patient }
{$ENDIF}

{$IFDEF FHIR_PAYMENTNOTICE}
  {@Enum TSearchParamsPaymentNotice
    Search Parameters for PaymentNotice
  }
  TSearchParamsPaymentNotice = (
    spPaymentNotice__content, {@enum.value "_content" spPaymentNotice__content Search on the entire content of the resource }
    spPaymentNotice__id, {@enum.value "_id" spPaymentNotice__id Logical id of this artifact }
    spPaymentNotice__lastUpdated, {@enum.value "_lastUpdated" spPaymentNotice__lastUpdated When the resource version last changed }
    spPaymentNotice__profile, {@enum.value "_profile" spPaymentNotice__profile Profiles this resource claims to conform to }
    spPaymentNotice__query, {@enum.value "_query" spPaymentNotice__query A custom search profile that describes a specific defined query operation }
    spPaymentNotice__security, {@enum.value "_security" spPaymentNotice__security Security Labels applied to this resource }
    spPaymentNotice__tag, {@enum.value "_tag" spPaymentNotice__tag Tags applied to this resource }
    spPaymentNotice__text, {@enum.value "_text" spPaymentNotice__text Search on the narrative of the resource }
    spPaymentNotice_Identifier); {@enum.value "identifier" spPaymentNotice_Identifier The business identifier of the Eligibility }
{$ENDIF}

{$IFDEF FHIR_PAYMENTRECONCILIATION}
  {@Enum TSearchParamsPaymentReconciliation
    Search Parameters for PaymentReconciliation
  }
  TSearchParamsPaymentReconciliation = (
    spPaymentReconciliation__content, {@enum.value "_content" spPaymentReconciliation__content Search on the entire content of the resource }
    spPaymentReconciliation__id, {@enum.value "_id" spPaymentReconciliation__id Logical id of this artifact }
    spPaymentReconciliation__lastUpdated, {@enum.value "_lastUpdated" spPaymentReconciliation__lastUpdated When the resource version last changed }
    spPaymentReconciliation__profile, {@enum.value "_profile" spPaymentReconciliation__profile Profiles this resource claims to conform to }
    spPaymentReconciliation__query, {@enum.value "_query" spPaymentReconciliation__query A custom search profile that describes a specific defined query operation }
    spPaymentReconciliation__security, {@enum.value "_security" spPaymentReconciliation__security Security Labels applied to this resource }
    spPaymentReconciliation__tag, {@enum.value "_tag" spPaymentReconciliation__tag Tags applied to this resource }
    spPaymentReconciliation__text, {@enum.value "_text" spPaymentReconciliation__text Search on the narrative of the resource }
    spPaymentReconciliation_Identifier); {@enum.value "identifier" spPaymentReconciliation_Identifier The business identifier of the Explanation of Benefit }
{$ENDIF}

{$IFDEF FHIR_PERSON}
  {@Enum TSearchParamsPerson
    Search Parameters for Person
  }
  TSearchParamsPerson = (
    spPerson__content, {@enum.value "_content" spPerson__content Search on the entire content of the resource }
    spPerson__id, {@enum.value "_id" spPerson__id Logical id of this artifact }
    spPerson__lastUpdated, {@enum.value "_lastUpdated" spPerson__lastUpdated When the resource version last changed }
    spPerson__profile, {@enum.value "_profile" spPerson__profile Profiles this resource claims to conform to }
    spPerson__query, {@enum.value "_query" spPerson__query A custom search profile that describes a specific defined query operation }
    spPerson__security, {@enum.value "_security" spPerson__security Security Labels applied to this resource }
    spPerson__tag, {@enum.value "_tag" spPerson__tag Tags applied to this resource }
    spPerson__text, {@enum.value "_text" spPerson__text Search on the narrative of the resource }
    spPerson_Address, {@enum.value "address" spPerson_Address An address in any kind of address/part }
    spPerson_Addresscity, {@enum.value "address-city" spPerson_Addresscity A city specified in an address }
    spPerson_Addresscountry, {@enum.value "address-country" spPerson_Addresscountry A country specified in an address }
    spPerson_Addresspostalcode, {@enum.value "address-postalcode" spPerson_Addresspostalcode A postal code specified in an address }
    spPerson_Addressstate, {@enum.value "address-state" spPerson_Addressstate A state specified in an address }
    spPerson_Addressuse, {@enum.value "address-use" spPerson_Addressuse A use code specified in an address }
    spPerson_Birthdate, {@enum.value "birthdate" spPerson_Birthdate The person's date of birth }
    spPerson_Email, {@enum.value "email" spPerson_Email A value in an email contact }
    spPerson_Gender, {@enum.value "gender" spPerson_Gender The gender of the person }
    spPerson_Identifier, {@enum.value "identifier" spPerson_Identifier A person Identifier }
    spPerson_Link, {@enum.value "link" spPerson_Link Any link has this Patient, Person, RelatedPerson or Practitioner reference }
    spPerson_Name, {@enum.value "name" spPerson_Name A portion of name in any name part }
    spPerson_Organization, {@enum.value "organization" spPerson_Organization The organization at which this person record is being managed }
    spPerson_Patient, {@enum.value "patient" spPerson_Patient The Person links to this Patient }
    spPerson_Phone, {@enum.value "phone" spPerson_Phone A value in a phone contact }
    spPerson_Phonetic, {@enum.value "phonetic" spPerson_Phonetic A portion of name using some kind of phonetic matching algorithm }
    spPerson_Practitioner, {@enum.value "practitioner" spPerson_Practitioner The Person links to this Practitioner }
    spPerson_Relatedperson, {@enum.value "relatedperson" spPerson_Relatedperson The Person links to this RelatedPerson }
    spPerson_Telecom); {@enum.value "telecom" spPerson_Telecom The value in any kind of contact }
{$ENDIF}

{$IFDEF FHIR_PRACTITIONER}
  {@Enum TSearchParamsPractitioner
    Search Parameters for Practitioner
  }
  TSearchParamsPractitioner = (
    spPractitioner__content, {@enum.value "_content" spPractitioner__content Search on the entire content of the resource }
    spPractitioner__id, {@enum.value "_id" spPractitioner__id Logical id of this artifact }
    spPractitioner__lastUpdated, {@enum.value "_lastUpdated" spPractitioner__lastUpdated When the resource version last changed }
    spPractitioner__profile, {@enum.value "_profile" spPractitioner__profile Profiles this resource claims to conform to }
    spPractitioner__query, {@enum.value "_query" spPractitioner__query A custom search profile that describes a specific defined query operation }
    spPractitioner__security, {@enum.value "_security" spPractitioner__security Security Labels applied to this resource }
    spPractitioner__tag, {@enum.value "_tag" spPractitioner__tag Tags applied to this resource }
    spPractitioner__text, {@enum.value "_text" spPractitioner__text Search on the narrative of the resource }
    spPractitioner_Address, {@enum.value "address" spPractitioner_Address An address in any kind of address/part }
    spPractitioner_Addresscity, {@enum.value "address-city" spPractitioner_Addresscity A city specified in an address }
    spPractitioner_Addresscountry, {@enum.value "address-country" spPractitioner_Addresscountry A country specified in an address }
    spPractitioner_Addresspostalcode, {@enum.value "address-postalcode" spPractitioner_Addresspostalcode A postalCode specified in an address }
    spPractitioner_Addressstate, {@enum.value "address-state" spPractitioner_Addressstate A state specified in an address }
    spPractitioner_Addressuse, {@enum.value "address-use" spPractitioner_Addressuse A use code specified in an address }
    spPractitioner_Communication, {@enum.value "communication" spPractitioner_Communication One of the languages that the practitioner can communicate with }
    spPractitioner_Email, {@enum.value "email" spPractitioner_Email A value in an email contact }
    spPractitioner_Family, {@enum.value "family" spPractitioner_Family A portion of the family name }
    spPractitioner_Gender, {@enum.value "gender" spPractitioner_Gender Gender of the practitioner }
    spPractitioner_Given, {@enum.value "given" spPractitioner_Given A portion of the given name }
    spPractitioner_Identifier, {@enum.value "identifier" spPractitioner_Identifier A practitioner's Identifier }
    spPractitioner_Location, {@enum.value "location" spPractitioner_Location One of the locations at which this practitioner provides care }
    spPractitioner_Name, {@enum.value "name" spPractitioner_Name A portion of either family or given name }
    spPractitioner_Organization, {@enum.value "organization" spPractitioner_Organization The identity of the organization the practitioner represents / acts on behalf of }
    spPractitioner_Phone, {@enum.value "phone" spPractitioner_Phone A value in a phone contact }
    spPractitioner_Phonetic, {@enum.value "phonetic" spPractitioner_Phonetic A portion of either family or given name using some kind of phonetic matching algorithm }
    spPractitioner_Role, {@enum.value "role" spPractitioner_Role The practitioner can perform this role at for the organization }
    spPractitioner_Specialty, {@enum.value "specialty" spPractitioner_Specialty The practitioner has this specialty at an organization }
    spPractitioner_Telecom); {@enum.value "telecom" spPractitioner_Telecom The value in any kind of contact }
{$ENDIF}

{$IFDEF FHIR_PROCEDURE}
  {@Enum TSearchParamsProcedure
    Search Parameters for Procedure
  }
  TSearchParamsProcedure = (
    spProcedure__content, {@enum.value "_content" spProcedure__content Search on the entire content of the resource }
    spProcedure__id, {@enum.value "_id" spProcedure__id Logical id of this artifact }
    spProcedure__lastUpdated, {@enum.value "_lastUpdated" spProcedure__lastUpdated When the resource version last changed }
    spProcedure__profile, {@enum.value "_profile" spProcedure__profile Profiles this resource claims to conform to }
    spProcedure__query, {@enum.value "_query" spProcedure__query A custom search profile that describes a specific defined query operation }
    spProcedure__security, {@enum.value "_security" spProcedure__security Security Labels applied to this resource }
    spProcedure__tag, {@enum.value "_tag" spProcedure__tag Tags applied to this resource }
    spProcedure__text, {@enum.value "_text" spProcedure__text Search on the narrative of the resource }
    spProcedure_Code, {@enum.value "code" spProcedure_Code A code to identify a  procedure }
    spProcedure_Date, {@enum.value "date" spProcedure_Date Date/Period the procedure was performed }
    spProcedure_Encounter, {@enum.value "encounter" spProcedure_Encounter The encounter associated with the procedure }
    spProcedure_Identifier, {@enum.value "identifier" spProcedure_Identifier A unique identifier for a procedure }
    spProcedure_Location, {@enum.value "location" spProcedure_Location Where the procedure happened }
    spProcedure_Patient, {@enum.value "patient" spProcedure_Patient Search by subject - a patient }
    spProcedure_Performer, {@enum.value "performer" spProcedure_Performer The reference to the practitioner }
    spProcedure_Subject); {@enum.value "subject" spProcedure_Subject Search by subject }
{$ENDIF}

{$IFDEF FHIR_PROCEDUREREQUEST}
  {@Enum TSearchParamsProcedureRequest
    Search Parameters for ProcedureRequest
  }
  TSearchParamsProcedureRequest = (
    spProcedureRequest__content, {@enum.value "_content" spProcedureRequest__content Search on the entire content of the resource }
    spProcedureRequest__id, {@enum.value "_id" spProcedureRequest__id Logical id of this artifact }
    spProcedureRequest__lastUpdated, {@enum.value "_lastUpdated" spProcedureRequest__lastUpdated When the resource version last changed }
    spProcedureRequest__profile, {@enum.value "_profile" spProcedureRequest__profile Profiles this resource claims to conform to }
    spProcedureRequest__query, {@enum.value "_query" spProcedureRequest__query A custom search profile that describes a specific defined query operation }
    spProcedureRequest__security, {@enum.value "_security" spProcedureRequest__security Security Labels applied to this resource }
    spProcedureRequest__tag, {@enum.value "_tag" spProcedureRequest__tag Tags applied to this resource }
    spProcedureRequest__text, {@enum.value "_text" spProcedureRequest__text Search on the narrative of the resource }
    spProcedureRequest_Encounter, {@enum.value "encounter" spProcedureRequest_Encounter Encounter request created during }
    spProcedureRequest_Identifier, {@enum.value "identifier" spProcedureRequest_Identifier A unique identifier of the Procedure Request }
    spProcedureRequest_Orderer, {@enum.value "orderer" spProcedureRequest_Orderer Who made request }
    spProcedureRequest_Patient, {@enum.value "patient" spProcedureRequest_Patient Search by subject - a patient }
    spProcedureRequest_Performer, {@enum.value "performer" spProcedureRequest_Performer Who should perform the procedure }
    spProcedureRequest_Subject); {@enum.value "subject" spProcedureRequest_Subject Search by subject }
{$ENDIF}

{$IFDEF FHIR_PROCESSREQUEST}
  {@Enum TSearchParamsProcessRequest
    Search Parameters for ProcessRequest
  }
  TSearchParamsProcessRequest = (
    spProcessRequest__content, {@enum.value "_content" spProcessRequest__content Search on the entire content of the resource }
    spProcessRequest__id, {@enum.value "_id" spProcessRequest__id Logical id of this artifact }
    spProcessRequest__lastUpdated, {@enum.value "_lastUpdated" spProcessRequest__lastUpdated When the resource version last changed }
    spProcessRequest__profile, {@enum.value "_profile" spProcessRequest__profile Profiles this resource claims to conform to }
    spProcessRequest__query, {@enum.value "_query" spProcessRequest__query A custom search profile that describes a specific defined query operation }
    spProcessRequest__security, {@enum.value "_security" spProcessRequest__security Security Labels applied to this resource }
    spProcessRequest__tag, {@enum.value "_tag" spProcessRequest__tag Tags applied to this resource }
    spProcessRequest__text, {@enum.value "_text" spProcessRequest__text Search on the narrative of the resource }
    spProcessRequest_Action, {@enum.value "action" spProcessRequest_Action The action requested by this resource }
    spProcessRequest_Identifier, {@enum.value "identifier" spProcessRequest_Identifier The business identifier of the ProcessRequest }
    spProcessRequest_Organization, {@enum.value "organization" spProcessRequest_Organization The organization who generated this request }
    spProcessRequest_Provider); {@enum.value "provider" spProcessRequest_Provider The provider who regenerated this request }
{$ENDIF}

{$IFDEF FHIR_PROCESSRESPONSE}
  {@Enum TSearchParamsProcessResponse
    Search Parameters for ProcessResponse
  }
  TSearchParamsProcessResponse = (
    spProcessResponse__content, {@enum.value "_content" spProcessResponse__content Search on the entire content of the resource }
    spProcessResponse__id, {@enum.value "_id" spProcessResponse__id Logical id of this artifact }
    spProcessResponse__lastUpdated, {@enum.value "_lastUpdated" spProcessResponse__lastUpdated When the resource version last changed }
    spProcessResponse__profile, {@enum.value "_profile" spProcessResponse__profile Profiles this resource claims to conform to }
    spProcessResponse__query, {@enum.value "_query" spProcessResponse__query A custom search profile that describes a specific defined query operation }
    spProcessResponse__security, {@enum.value "_security" spProcessResponse__security Security Labels applied to this resource }
    spProcessResponse__tag, {@enum.value "_tag" spProcessResponse__tag Tags applied to this resource }
    spProcessResponse__text, {@enum.value "_text" spProcessResponse__text Search on the narrative of the resource }
    spProcessResponse_Identifier, {@enum.value "identifier" spProcessResponse_Identifier The business identifier of the Explanation of Benefit }
    spProcessResponse_Organization, {@enum.value "organization" spProcessResponse_Organization The organization who generated this resource }
    spProcessResponse_Request, {@enum.value "request" spProcessResponse_Request The reference to the claim }
    spProcessResponse_Requestorganization, {@enum.value "requestorganization" spProcessResponse_Requestorganization The Organization who is responsible the request transaction }
    spProcessResponse_Requestprovider); {@enum.value "requestprovider" spProcessResponse_Requestprovider The Provider who is responsible the request transaction }
{$ENDIF}

{$IFDEF FHIR_PROVENANCE}
  {@Enum TSearchParamsProvenance
    Search Parameters for Provenance
  }
  TSearchParamsProvenance = (
    spProvenance__content, {@enum.value "_content" spProvenance__content Search on the entire content of the resource }
    spProvenance__id, {@enum.value "_id" spProvenance__id Logical id of this artifact }
    spProvenance__lastUpdated, {@enum.value "_lastUpdated" spProvenance__lastUpdated When the resource version last changed }
    spProvenance__profile, {@enum.value "_profile" spProvenance__profile Profiles this resource claims to conform to }
    spProvenance__query, {@enum.value "_query" spProvenance__query A custom search profile that describes a specific defined query operation }
    spProvenance__security, {@enum.value "_security" spProvenance__security Security Labels applied to this resource }
    spProvenance__tag, {@enum.value "_tag" spProvenance__tag Tags applied to this resource }
    spProvenance__text, {@enum.value "_text" spProvenance__text Search on the narrative of the resource }
    spProvenance_Agent, {@enum.value "agent" spProvenance_Agent Individual, device or organization playing role }
    spProvenance_End, {@enum.value "end" spProvenance_End End time with inclusive boundary, if not ongoing }
    spProvenance_Entity, {@enum.value "entity" spProvenance_Entity Identity of entity }
    spProvenance_Entitytype, {@enum.value "entitytype" spProvenance_Entitytype The type of resource in this entity }
    spProvenance_Location, {@enum.value "location" spProvenance_Location Where the activity occurred, if relevant }
    spProvenance_Patient, {@enum.value "patient" spProvenance_Patient Target Reference(s) (usually version specific) }
    spProvenance_Sigtype, {@enum.value "sigtype" spProvenance_Sigtype Indication of the reason the entity signed the object(s) }
    spProvenance_Start, {@enum.value "start" spProvenance_Start Starting time with inclusive boundary }
    spProvenance_Target, {@enum.value "target" spProvenance_Target Target Reference(s) (usually version specific) }
    spProvenance_Userid); {@enum.value "userid" spProvenance_Userid Authorization-system identifier for the agent }
{$ENDIF}

{$IFDEF FHIR_QUESTIONNAIRE}
  {@Enum TSearchParamsQuestionnaire
    Search Parameters for Questionnaire
  }
  TSearchParamsQuestionnaire = (
    spQuestionnaire__content, {@enum.value "_content" spQuestionnaire__content Search on the entire content of the resource }
    spQuestionnaire__id, {@enum.value "_id" spQuestionnaire__id Logical id of this artifact }
    spQuestionnaire__lastUpdated, {@enum.value "_lastUpdated" spQuestionnaire__lastUpdated When the resource version last changed }
    spQuestionnaire__profile, {@enum.value "_profile" spQuestionnaire__profile Profiles this resource claims to conform to }
    spQuestionnaire__query, {@enum.value "_query" spQuestionnaire__query A custom search profile that describes a specific defined query operation }
    spQuestionnaire__security, {@enum.value "_security" spQuestionnaire__security Security Labels applied to this resource }
    spQuestionnaire__tag, {@enum.value "_tag" spQuestionnaire__tag Tags applied to this resource }
    spQuestionnaire__text, {@enum.value "_text" spQuestionnaire__text Search on the narrative of the resource }
    spQuestionnaire_Code, {@enum.value "code" spQuestionnaire_Code A code that corresponds to the questionnaire or one of its groups }
    spQuestionnaire_Date, {@enum.value "date" spQuestionnaire_Date When the questionnaire was last changed }
    spQuestionnaire_Identifier, {@enum.value "identifier" spQuestionnaire_Identifier An identifier for the questionnaire }
    spQuestionnaire_Publisher, {@enum.value "publisher" spQuestionnaire_Publisher The author of the questionnaire }
    spQuestionnaire_Status, {@enum.value "status" spQuestionnaire_Status The status of the questionnaire }
    spQuestionnaire_Title, {@enum.value "title" spQuestionnaire_Title All or part of the name of the questionnaire (title for the root group of the questionnaire) }
    spQuestionnaire_Version); {@enum.value "version" spQuestionnaire_Version The business version of the questionnaire }
{$ENDIF}

{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  {@Enum TSearchParamsQuestionnaireResponse
    Search Parameters for QuestionnaireResponse
  }
  TSearchParamsQuestionnaireResponse = (
    spQuestionnaireResponse__content, {@enum.value "_content" spQuestionnaireResponse__content Search on the entire content of the resource }
    spQuestionnaireResponse__id, {@enum.value "_id" spQuestionnaireResponse__id Logical id of this artifact }
    spQuestionnaireResponse__lastUpdated, {@enum.value "_lastUpdated" spQuestionnaireResponse__lastUpdated When the resource version last changed }
    spQuestionnaireResponse__profile, {@enum.value "_profile" spQuestionnaireResponse__profile Profiles this resource claims to conform to }
    spQuestionnaireResponse__query, {@enum.value "_query" spQuestionnaireResponse__query A custom search profile that describes a specific defined query operation }
    spQuestionnaireResponse__security, {@enum.value "_security" spQuestionnaireResponse__security Security Labels applied to this resource }
    spQuestionnaireResponse__tag, {@enum.value "_tag" spQuestionnaireResponse__tag Tags applied to this resource }
    spQuestionnaireResponse__text, {@enum.value "_text" spQuestionnaireResponse__text Search on the narrative of the resource }
    spQuestionnaireResponse_Author, {@enum.value "author" spQuestionnaireResponse_Author The author of the questionnaire }
    spQuestionnaireResponse_Authored, {@enum.value "authored" spQuestionnaireResponse_Authored When the questionnaire was authored }
    spQuestionnaireResponse_Encounter, {@enum.value "encounter" spQuestionnaireResponse_Encounter Encounter during which questionnaire was authored }
    spQuestionnaireResponse_Patient, {@enum.value "patient" spQuestionnaireResponse_Patient The patient that is the subject of the questionnaire }
    spQuestionnaireResponse_Questionnaire, {@enum.value "questionnaire" spQuestionnaireResponse_Questionnaire The questionnaire the answers are provided for }
    spQuestionnaireResponse_Source, {@enum.value "source" spQuestionnaireResponse_Source The person who answered the questions }
    spQuestionnaireResponse_Status, {@enum.value "status" spQuestionnaireResponse_Status The status of the questionnaire response }
    spQuestionnaireResponse_Subject); {@enum.value "subject" spQuestionnaireResponse_Subject The subject of the questionnaire }
{$ENDIF}

{$IFDEF FHIR_REFERRALREQUEST}
  {@Enum TSearchParamsReferralRequest
    Search Parameters for ReferralRequest
  }
  TSearchParamsReferralRequest = (
    spReferralRequest__content, {@enum.value "_content" spReferralRequest__content Search on the entire content of the resource }
    spReferralRequest__id, {@enum.value "_id" spReferralRequest__id Logical id of this artifact }
    spReferralRequest__lastUpdated, {@enum.value "_lastUpdated" spReferralRequest__lastUpdated When the resource version last changed }
    spReferralRequest__profile, {@enum.value "_profile" spReferralRequest__profile Profiles this resource claims to conform to }
    spReferralRequest__query, {@enum.value "_query" spReferralRequest__query A custom search profile that describes a specific defined query operation }
    spReferralRequest__security, {@enum.value "_security" spReferralRequest__security Security Labels applied to this resource }
    spReferralRequest__tag, {@enum.value "_tag" spReferralRequest__tag Tags applied to this resource }
    spReferralRequest__text, {@enum.value "_text" spReferralRequest__text Search on the narrative of the resource }
    spReferralRequest_Date, {@enum.value "date" spReferralRequest_Date Creation or activation date }
    spReferralRequest_Patient, {@enum.value "patient" spReferralRequest_Patient Who the referral is about }
    spReferralRequest_Priority, {@enum.value "priority" spReferralRequest_Priority The priority assigned to the referral }
    spReferralRequest_Recipient, {@enum.value "recipient" spReferralRequest_Recipient The person that the referral was sent to }
    spReferralRequest_Requester, {@enum.value "requester" spReferralRequest_Requester Requester of referral / transfer of care }
    spReferralRequest_Specialty, {@enum.value "specialty" spReferralRequest_Specialty The specialty that the referral is for }
    spReferralRequest_Status, {@enum.value "status" spReferralRequest_Status The status of the referral }
    spReferralRequest_Type); {@enum.value "type" spReferralRequest_Type The type of the referral }
{$ENDIF}

{$IFDEF FHIR_RELATEDPERSON}
  {@Enum TSearchParamsRelatedPerson
    Search Parameters for RelatedPerson
  }
  TSearchParamsRelatedPerson = (
    spRelatedPerson__content, {@enum.value "_content" spRelatedPerson__content Search on the entire content of the resource }
    spRelatedPerson__id, {@enum.value "_id" spRelatedPerson__id Logical id of this artifact }
    spRelatedPerson__lastUpdated, {@enum.value "_lastUpdated" spRelatedPerson__lastUpdated When the resource version last changed }
    spRelatedPerson__profile, {@enum.value "_profile" spRelatedPerson__profile Profiles this resource claims to conform to }
    spRelatedPerson__query, {@enum.value "_query" spRelatedPerson__query A custom search profile that describes a specific defined query operation }
    spRelatedPerson__security, {@enum.value "_security" spRelatedPerson__security Security Labels applied to this resource }
    spRelatedPerson__tag, {@enum.value "_tag" spRelatedPerson__tag Tags applied to this resource }
    spRelatedPerson__text, {@enum.value "_text" spRelatedPerson__text Search on the narrative of the resource }
    spRelatedPerson_Address, {@enum.value "address" spRelatedPerson_Address An address in any kind of address/part }
    spRelatedPerson_Addresscity, {@enum.value "address-city" spRelatedPerson_Addresscity A city specified in an address }
    spRelatedPerson_Addresscountry, {@enum.value "address-country" spRelatedPerson_Addresscountry A country specified in an address }
    spRelatedPerson_Addresspostalcode, {@enum.value "address-postalcode" spRelatedPerson_Addresspostalcode A postal code specified in an address }
    spRelatedPerson_Addressstate, {@enum.value "address-state" spRelatedPerson_Addressstate A state specified in an address }
    spRelatedPerson_Addressuse, {@enum.value "address-use" spRelatedPerson_Addressuse A use code specified in an address }
    spRelatedPerson_Birthdate, {@enum.value "birthdate" spRelatedPerson_Birthdate The Related Person's date of birth }
    spRelatedPerson_Email, {@enum.value "email" spRelatedPerson_Email A value in an email contact }
    spRelatedPerson_Gender, {@enum.value "gender" spRelatedPerson_Gender Gender of the person }
    spRelatedPerson_Identifier, {@enum.value "identifier" spRelatedPerson_Identifier A patient Identifier }
    spRelatedPerson_Name, {@enum.value "name" spRelatedPerson_Name A portion of name in any name part }
    spRelatedPerson_Patient, {@enum.value "patient" spRelatedPerson_Patient The patient this person is related to }
    spRelatedPerson_Phone, {@enum.value "phone" spRelatedPerson_Phone A value in a phone contact }
    spRelatedPerson_Phonetic, {@enum.value "phonetic" spRelatedPerson_Phonetic A portion of name using some kind of phonetic matching algorithm }
    spRelatedPerson_Telecom); {@enum.value "telecom" spRelatedPerson_Telecom The value in any kind of contact }
{$ENDIF}

{$IFDEF FHIR_RISKASSESSMENT}
  {@Enum TSearchParamsRiskAssessment
    Search Parameters for RiskAssessment
  }
  TSearchParamsRiskAssessment = (
    spRiskAssessment__content, {@enum.value "_content" spRiskAssessment__content Search on the entire content of the resource }
    spRiskAssessment__id, {@enum.value "_id" spRiskAssessment__id Logical id of this artifact }
    spRiskAssessment__lastUpdated, {@enum.value "_lastUpdated" spRiskAssessment__lastUpdated When the resource version last changed }
    spRiskAssessment__profile, {@enum.value "_profile" spRiskAssessment__profile Profiles this resource claims to conform to }
    spRiskAssessment__query, {@enum.value "_query" spRiskAssessment__query A custom search profile that describes a specific defined query operation }
    spRiskAssessment__security, {@enum.value "_security" spRiskAssessment__security Security Labels applied to this resource }
    spRiskAssessment__tag, {@enum.value "_tag" spRiskAssessment__tag Tags applied to this resource }
    spRiskAssessment__text, {@enum.value "_text" spRiskAssessment__text Search on the narrative of the resource }
    spRiskAssessment_Condition, {@enum.value "condition" spRiskAssessment_Condition Condition assessed }
    spRiskAssessment_Date, {@enum.value "date" spRiskAssessment_Date When was assessment made? }
    spRiskAssessment_Encounter, {@enum.value "encounter" spRiskAssessment_Encounter Where was assessment performed? }
    spRiskAssessment_Identifier, {@enum.value "identifier" spRiskAssessment_Identifier Unique identifier for the assessment }
    spRiskAssessment_Method, {@enum.value "method" spRiskAssessment_Method Evaluation mechanism }
    spRiskAssessment_Patient, {@enum.value "patient" spRiskAssessment_Patient Who/what does assessment apply to? }
    spRiskAssessment_Performer, {@enum.value "performer" spRiskAssessment_Performer Who did assessment? }
    spRiskAssessment_Subject); {@enum.value "subject" spRiskAssessment_Subject Who/what does assessment apply to? }
{$ENDIF}

{$IFDEF FHIR_SCHEDULE}
  {@Enum TSearchParamsSchedule
    Search Parameters for Schedule
  }
  TSearchParamsSchedule = (
    spSchedule__content, {@enum.value "_content" spSchedule__content Search on the entire content of the resource }
    spSchedule__id, {@enum.value "_id" spSchedule__id Logical id of this artifact }
    spSchedule__lastUpdated, {@enum.value "_lastUpdated" spSchedule__lastUpdated When the resource version last changed }
    spSchedule__profile, {@enum.value "_profile" spSchedule__profile Profiles this resource claims to conform to }
    spSchedule__query, {@enum.value "_query" spSchedule__query A custom search profile that describes a specific defined query operation }
    spSchedule__security, {@enum.value "_security" spSchedule__security Security Labels applied to this resource }
    spSchedule__tag, {@enum.value "_tag" spSchedule__tag Tags applied to this resource }
    spSchedule__text, {@enum.value "_text" spSchedule__text Search on the narrative of the resource }
    spSchedule_Actor, {@enum.value "actor" spSchedule_Actor The individual(HealthcareService, Practitioner, Location, ...) to find a Schedule for }
    spSchedule_Date, {@enum.value "date" spSchedule_Date Search for Schedule resources that have a period that contains this date specified }
    spSchedule_Identifier, {@enum.value "identifier" spSchedule_Identifier A Schedule Identifier }
    spSchedule_Type); {@enum.value "type" spSchedule_Type The type of appointments that can be booked into associated slot(s) }
{$ENDIF}

{$IFDEF FHIR_SEARCHPARAMETER}
  {@Enum TSearchParamsSearchParameter
    Search Parameters for SearchParameter
  }
  TSearchParamsSearchParameter = (
    spSearchParameter__content, {@enum.value "_content" spSearchParameter__content Search on the entire content of the resource }
    spSearchParameter__id, {@enum.value "_id" spSearchParameter__id Logical id of this artifact }
    spSearchParameter__lastUpdated, {@enum.value "_lastUpdated" spSearchParameter__lastUpdated When the resource version last changed }
    spSearchParameter__profile, {@enum.value "_profile" spSearchParameter__profile Profiles this resource claims to conform to }
    spSearchParameter__query, {@enum.value "_query" spSearchParameter__query A custom search profile that describes a specific defined query operation }
    spSearchParameter__security, {@enum.value "_security" spSearchParameter__security Security Labels applied to this resource }
    spSearchParameter__tag, {@enum.value "_tag" spSearchParameter__tag Tags applied to this resource }
    spSearchParameter__text, {@enum.value "_text" spSearchParameter__text Search on the narrative of the resource }
    spSearchParameter_Base, {@enum.value "base" spSearchParameter_Base The resource type this search parameter applies to }
    spSearchParameter_Code, {@enum.value "code" spSearchParameter_Code Code used in URL }
    spSearchParameter_Description, {@enum.value "description" spSearchParameter_Description Documentation for  search parameter }
    spSearchParameter_Name, {@enum.value "name" spSearchParameter_Name Informal name for this search parameter }
    spSearchParameter_Target, {@enum.value "target" spSearchParameter_Target Types of resource (if a resource reference) }
    spSearchParameter_Type, {@enum.value "type" spSearchParameter_Type number | date | string | token | reference | composite | quantity | uri }
    spSearchParameter_Url); {@enum.value "url" spSearchParameter_Url Absolute URL used to reference this search parameter }
{$ENDIF}

{$IFDEF FHIR_SLOT}
  {@Enum TSearchParamsSlot
    Search Parameters for Slot
  }
  TSearchParamsSlot = (
    spSlot__content, {@enum.value "_content" spSlot__content Search on the entire content of the resource }
    spSlot__id, {@enum.value "_id" spSlot__id Logical id of this artifact }
    spSlot__lastUpdated, {@enum.value "_lastUpdated" spSlot__lastUpdated When the resource version last changed }
    spSlot__profile, {@enum.value "_profile" spSlot__profile Profiles this resource claims to conform to }
    spSlot__query, {@enum.value "_query" spSlot__query A custom search profile that describes a specific defined query operation }
    spSlot__security, {@enum.value "_security" spSlot__security Security Labels applied to this resource }
    spSlot__tag, {@enum.value "_tag" spSlot__tag Tags applied to this resource }
    spSlot__text, {@enum.value "_text" spSlot__text Search on the narrative of the resource }
    spSlot_Fbtype, {@enum.value "fb-type" spSlot_Fbtype The free/busy status of the appointment }
    spSlot_Identifier, {@enum.value "identifier" spSlot_Identifier A Slot Identifier }
    spSlot_Schedule, {@enum.value "schedule" spSlot_Schedule The Schedule Resource that we are seeking a slot within }
    spSlot_Slottype, {@enum.value "slot-type" spSlot_Slottype The type of appointments that can be booked into the slot }
    spSlot_Start); {@enum.value "start" spSlot_Start Appointment date/time. }
{$ENDIF}

{$IFDEF FHIR_SPECIMEN}
  {@Enum TSearchParamsSpecimen
    Search Parameters for Specimen
  }
  TSearchParamsSpecimen = (
    spSpecimen__content, {@enum.value "_content" spSpecimen__content Search on the entire content of the resource }
    spSpecimen__id, {@enum.value "_id" spSpecimen__id Logical id of this artifact }
    spSpecimen__lastUpdated, {@enum.value "_lastUpdated" spSpecimen__lastUpdated When the resource version last changed }
    spSpecimen__profile, {@enum.value "_profile" spSpecimen__profile Profiles this resource claims to conform to }
    spSpecimen__query, {@enum.value "_query" spSpecimen__query A custom search profile that describes a specific defined query operation }
    spSpecimen__security, {@enum.value "_security" spSpecimen__security Security Labels applied to this resource }
    spSpecimen__tag, {@enum.value "_tag" spSpecimen__tag Tags applied to this resource }
    spSpecimen__text, {@enum.value "_text" spSpecimen__text Search on the narrative of the resource }
    spSpecimen_Accession, {@enum.value "accession" spSpecimen_Accession The accession number associated with the specimen }
    spSpecimen_Bodysite, {@enum.value "bodysite" spSpecimen_Bodysite The code for the body site from where the specimen originated }
    spSpecimen_Collected, {@enum.value "collected" spSpecimen_Collected The date the specimen was collected }
    spSpecimen_Collector, {@enum.value "collector" spSpecimen_Collector Who collected the specimen }
    spSpecimen_Container, {@enum.value "container" spSpecimen_Container The kind of specimen container }
    spSpecimen_Containerid, {@enum.value "container-id" spSpecimen_Containerid The unique identifier associated with the specimen container }
    spSpecimen_Identifier, {@enum.value "identifier" spSpecimen_Identifier The unique identifier associated with the specimen }
    spSpecimen_Parent, {@enum.value "parent" spSpecimen_Parent The parent of the specimen }
    spSpecimen_Patient, {@enum.value "patient" spSpecimen_Patient The patient the specimen comes from }
    spSpecimen_Subject, {@enum.value "subject" spSpecimen_Subject The subject of the specimen }
    spSpecimen_Type); {@enum.value "type" spSpecimen_Type The specimen type }
{$ENDIF}

{$IFDEF FHIR_STRUCTUREDEFINITION}
  {@Enum TSearchParamsStructureDefinition
    Search Parameters for StructureDefinition
  }
  TSearchParamsStructureDefinition = (
    spStructureDefinition__content, {@enum.value "_content" spStructureDefinition__content Search on the entire content of the resource }
    spStructureDefinition__id, {@enum.value "_id" spStructureDefinition__id Logical id of this artifact }
    spStructureDefinition__lastUpdated, {@enum.value "_lastUpdated" spStructureDefinition__lastUpdated When the resource version last changed }
    spStructureDefinition__profile, {@enum.value "_profile" spStructureDefinition__profile Profiles this resource claims to conform to }
    spStructureDefinition__query, {@enum.value "_query" spStructureDefinition__query A custom search profile that describes a specific defined query operation }
    spStructureDefinition__security, {@enum.value "_security" spStructureDefinition__security Security Labels applied to this resource }
    spStructureDefinition__tag, {@enum.value "_tag" spStructureDefinition__tag Tags applied to this resource }
    spStructureDefinition__text, {@enum.value "_text" spStructureDefinition__text Search on the narrative of the resource }
    spStructureDefinition_Abstract, {@enum.value "abstract" spStructureDefinition_Abstract Whether the structure is abstract }
    spStructureDefinition_Base, {@enum.value "base" spStructureDefinition_Base Structure that this set of constraints applies to }
    spStructureDefinition_Basepath, {@enum.value "base-path" spStructureDefinition_Basepath Path that identifies the base element }
    spStructureDefinition_Code, {@enum.value "code" spStructureDefinition_Code A code for the profile }
    spStructureDefinition_Context, {@enum.value "context" spStructureDefinition_Context A use context assigned to the structure }
    spStructureDefinition_Contexttype, {@enum.value "context-type" spStructureDefinition_Contexttype resource | datatype | mapping | extension }
    spStructureDefinition_Date, {@enum.value "date" spStructureDefinition_Date The profile publication date }
    spStructureDefinition_Description, {@enum.value "description" spStructureDefinition_Description Text search in the description of the profile }
    spStructureDefinition_Display, {@enum.value "display" spStructureDefinition_Display Use this name when displaying the value }
    spStructureDefinition_Experimental, {@enum.value "experimental" spStructureDefinition_Experimental If for testing purposes, not real usage }
    spStructureDefinition_Extcontext, {@enum.value "ext-context" spStructureDefinition_Extcontext Where the extension can be used in instances }
    spStructureDefinition_Identifier, {@enum.value "identifier" spStructureDefinition_Identifier The identifier of the profile }
    spStructureDefinition_Kind, {@enum.value "kind" spStructureDefinition_Kind datatype | resource | logical }
    spStructureDefinition_Name, {@enum.value "name" spStructureDefinition_Name Name of the profile }
    spStructureDefinition_Path, {@enum.value "path" spStructureDefinition_Path A path that is constrained in the profile }
    spStructureDefinition_Publisher, {@enum.value "publisher" spStructureDefinition_Publisher Name of the publisher of the profile }
    spStructureDefinition_Status, {@enum.value "status" spStructureDefinition_Status The current status of the profile }
    spStructureDefinition_Type, {@enum.value "type" spStructureDefinition_Type Any datatype or resource, including abstract ones }
    spStructureDefinition_Url, {@enum.value "url" spStructureDefinition_Url Absolute URL used to reference this StructureDefinition }
    spStructureDefinition_Valueset, {@enum.value "valueset" spStructureDefinition_Valueset A vocabulary binding reference }
    spStructureDefinition_Version); {@enum.value "version" spStructureDefinition_Version The version identifier of the profile }
{$ENDIF}

{$IFDEF FHIR_SUBSCRIPTION}
  {@Enum TSearchParamsSubscription
    Search Parameters for Subscription
  }
  TSearchParamsSubscription = (
    spSubscription__content, {@enum.value "_content" spSubscription__content Search on the entire content of the resource }
    spSubscription__id, {@enum.value "_id" spSubscription__id Logical id of this artifact }
    spSubscription__lastUpdated, {@enum.value "_lastUpdated" spSubscription__lastUpdated When the resource version last changed }
    spSubscription__profile, {@enum.value "_profile" spSubscription__profile Profiles this resource claims to conform to }
    spSubscription__query, {@enum.value "_query" spSubscription__query A custom search profile that describes a specific defined query operation }
    spSubscription__security, {@enum.value "_security" spSubscription__security Security Labels applied to this resource }
    spSubscription__tag, {@enum.value "_tag" spSubscription__tag Tags applied to this resource }
    spSubscription__text, {@enum.value "_text" spSubscription__text Search on the narrative of the resource }
    spSubscription_Contact, {@enum.value "contact" spSubscription_Contact Contact details for source (e.g. troubleshooting) }
    spSubscription_Criteria, {@enum.value "criteria" spSubscription_Criteria Rule for server push criteria }
    spSubscription_Payload, {@enum.value "payload" spSubscription_Payload Mimetype to send, or blank for no payload }
    spSubscription_Status, {@enum.value "status" spSubscription_Status requested | active | error | off }
    spSubscription_Tag, {@enum.value "tag" spSubscription_Tag A tag to add to matching resources }
    spSubscription_Type, {@enum.value "type" spSubscription_Type rest-hook | websocket | email | sms | message }
    spSubscription_Url); {@enum.value "url" spSubscription_Url Where the channel points to }
{$ENDIF}

{$IFDEF FHIR_SUBSTANCE}
  {@Enum TSearchParamsSubstance
    Search Parameters for Substance
  }
  TSearchParamsSubstance = (
    spSubstance__content, {@enum.value "_content" spSubstance__content Search on the entire content of the resource }
    spSubstance__id, {@enum.value "_id" spSubstance__id Logical id of this artifact }
    spSubstance__lastUpdated, {@enum.value "_lastUpdated" spSubstance__lastUpdated When the resource version last changed }
    spSubstance__profile, {@enum.value "_profile" spSubstance__profile Profiles this resource claims to conform to }
    spSubstance__query, {@enum.value "_query" spSubstance__query A custom search profile that describes a specific defined query operation }
    spSubstance__security, {@enum.value "_security" spSubstance__security Security Labels applied to this resource }
    spSubstance__tag, {@enum.value "_tag" spSubstance__tag Tags applied to this resource }
    spSubstance__text, {@enum.value "_text" spSubstance__text Search on the narrative of the resource }
    spSubstance_Category, {@enum.value "category" spSubstance_Category The category of the substance }
    spSubstance_Code, {@enum.value "code" spSubstance_Code The code of the substance }
    spSubstance_Containeridentifier, {@enum.value "container-identifier" spSubstance_Containeridentifier Identifier of the package/container }
    spSubstance_Expiry, {@enum.value "expiry" spSubstance_Expiry Expiry date of package or container of substance }
    spSubstance_Identifier, {@enum.value "identifier" spSubstance_Identifier Unique identifier for the substance }
    spSubstance_Quantity, {@enum.value "quantity" spSubstance_Quantity Amount of substance in the package }
    spSubstance_Substance); {@enum.value "substance" spSubstance_Substance A component of the substance }
{$ENDIF}

{$IFDEF FHIR_SUPPLYDELIVERY}
  {@Enum TSearchParamsSupplyDelivery
    Search Parameters for SupplyDelivery
  }
  TSearchParamsSupplyDelivery = (
    spSupplyDelivery__content, {@enum.value "_content" spSupplyDelivery__content Search on the entire content of the resource }
    spSupplyDelivery__id, {@enum.value "_id" spSupplyDelivery__id Logical id of this artifact }
    spSupplyDelivery__lastUpdated, {@enum.value "_lastUpdated" spSupplyDelivery__lastUpdated When the resource version last changed }
    spSupplyDelivery__profile, {@enum.value "_profile" spSupplyDelivery__profile Profiles this resource claims to conform to }
    spSupplyDelivery__query, {@enum.value "_query" spSupplyDelivery__query A custom search profile that describes a specific defined query operation }
    spSupplyDelivery__security, {@enum.value "_security" spSupplyDelivery__security Security Labels applied to this resource }
    spSupplyDelivery__tag, {@enum.value "_tag" spSupplyDelivery__tag Tags applied to this resource }
    spSupplyDelivery__text, {@enum.value "_text" spSupplyDelivery__text Search on the narrative of the resource }
    spSupplyDelivery_Identifier, {@enum.value "identifier" spSupplyDelivery_Identifier External identifier }
    spSupplyDelivery_Patient, {@enum.value "patient" spSupplyDelivery_Patient Patient for whom the item is supplied }
    spSupplyDelivery_Receiver, {@enum.value "receiver" spSupplyDelivery_Receiver Who collected the Supply }
    spSupplyDelivery_Status, {@enum.value "status" spSupplyDelivery_Status in-progress | completed | abandoned }
    spSupplyDelivery_Supplier); {@enum.value "supplier" spSupplyDelivery_Supplier Dispenser }
{$ENDIF}

{$IFDEF FHIR_SUPPLYREQUEST}
  {@Enum TSearchParamsSupplyRequest
    Search Parameters for SupplyRequest
  }
  TSearchParamsSupplyRequest = (
    spSupplyRequest__content, {@enum.value "_content" spSupplyRequest__content Search on the entire content of the resource }
    spSupplyRequest__id, {@enum.value "_id" spSupplyRequest__id Logical id of this artifact }
    spSupplyRequest__lastUpdated, {@enum.value "_lastUpdated" spSupplyRequest__lastUpdated When the resource version last changed }
    spSupplyRequest__profile, {@enum.value "_profile" spSupplyRequest__profile Profiles this resource claims to conform to }
    spSupplyRequest__query, {@enum.value "_query" spSupplyRequest__query A custom search profile that describes a specific defined query operation }
    spSupplyRequest__security, {@enum.value "_security" spSupplyRequest__security Security Labels applied to this resource }
    spSupplyRequest__tag, {@enum.value "_tag" spSupplyRequest__tag Tags applied to this resource }
    spSupplyRequest__text, {@enum.value "_text" spSupplyRequest__text Search on the narrative of the resource }
    spSupplyRequest_Date, {@enum.value "date" spSupplyRequest_Date When the request was made }
    spSupplyRequest_Identifier, {@enum.value "identifier" spSupplyRequest_Identifier Unique identifier }
    spSupplyRequest_Kind, {@enum.value "kind" spSupplyRequest_Kind The kind of supply (central, non-stock, etc.) }
    spSupplyRequest_Patient, {@enum.value "patient" spSupplyRequest_Patient Patient for whom the item is supplied }
    spSupplyRequest_Source, {@enum.value "source" spSupplyRequest_Source Who initiated this order }
    spSupplyRequest_Status, {@enum.value "status" spSupplyRequest_Status requested | completed | failed | cancelled }
    spSupplyRequest_Supplier); {@enum.value "supplier" spSupplyRequest_Supplier Who is intended to fulfill the request }
{$ENDIF}

{$IFDEF FHIR_TESTSCRIPT}
  {@Enum TSearchParamsTestScript
    Search Parameters for TestScript
  }
  TSearchParamsTestScript = (
    spTestScript__content, {@enum.value "_content" spTestScript__content Search on the entire content of the resource }
    spTestScript__id, {@enum.value "_id" spTestScript__id Logical id of this artifact }
    spTestScript__lastUpdated, {@enum.value "_lastUpdated" spTestScript__lastUpdated When the resource version last changed }
    spTestScript__profile, {@enum.value "_profile" spTestScript__profile Profiles this resource claims to conform to }
    spTestScript__query, {@enum.value "_query" spTestScript__query A custom search profile that describes a specific defined query operation }
    spTestScript__security, {@enum.value "_security" spTestScript__security Security Labels applied to this resource }
    spTestScript__tag, {@enum.value "_tag" spTestScript__tag Tags applied to this resource }
    spTestScript__text, {@enum.value "_text" spTestScript__text Search on the narrative of the resource }
    spTestScript_Description, {@enum.value "description" spTestScript_Description Natural language description of the TestScript }
    spTestScript_Identifier, {@enum.value "identifier" spTestScript_Identifier External identifier }
    spTestScript_Name, {@enum.value "name" spTestScript_Name Informal name for this TestScript }
    spTestScript_Testscriptcapability, {@enum.value "testscript-capability" spTestScript_Testscriptcapability TestScript required and validated capability }
    spTestScript_Testscriptsetupcapability, {@enum.value "testscript-setup-capability" spTestScript_Testscriptsetupcapability TestScript setup required and validated capability }
    spTestScript_Testscripttestcapability, {@enum.value "testscript-test-capability" spTestScript_Testscripttestcapability TestScript test required and validated capability }
    spTestScript_Url); {@enum.value "url" spTestScript_Url Absolute URL used to reference this TestScript }
{$ENDIF}

{$IFDEF FHIR_VALUESET}
  {@Enum TSearchParamsValueSet
    Search Parameters for ValueSet
  }
  TSearchParamsValueSet = (
    spValueSet__content, {@enum.value "_content" spValueSet__content Search on the entire content of the resource }
    spValueSet__id, {@enum.value "_id" spValueSet__id Logical id of this artifact }
    spValueSet__lastUpdated, {@enum.value "_lastUpdated" spValueSet__lastUpdated When the resource version last changed }
    spValueSet__profile, {@enum.value "_profile" spValueSet__profile Profiles this resource claims to conform to }
    spValueSet__query, {@enum.value "_query" spValueSet__query A custom search profile that describes a specific defined query operation }
    spValueSet__security, {@enum.value "_security" spValueSet__security Security Labels applied to this resource }
    spValueSet__tag, {@enum.value "_tag" spValueSet__tag Tags applied to this resource }
    spValueSet__text, {@enum.value "_text" spValueSet__text Search on the narrative of the resource }
    spValueSet_Code, {@enum.value "code" spValueSet_Code A code defined in the value set }
    spValueSet_Context, {@enum.value "context" spValueSet_Context A use context assigned to the value set }
    spValueSet_Date, {@enum.value "date" spValueSet_Date The value set publication date }
    spValueSet_Description, {@enum.value "description" spValueSet_Description Text search in the description of the value set }
    spValueSet_Expansion, {@enum.value "expansion" spValueSet_Expansion Uniquely identifies this expansion }
    spValueSet_Identifier, {@enum.value "identifier" spValueSet_Identifier The identifier for the value set }
    spValueSet_Name, {@enum.value "name" spValueSet_Name The name of the value set }
    spValueSet_Publisher, {@enum.value "publisher" spValueSet_Publisher Name of the publisher of the value set }
    spValueSet_Reference, {@enum.value "reference" spValueSet_Reference A code system included or excluded in the value set or an imported value set }
    spValueSet_Status, {@enum.value "status" spValueSet_Status The status of the value set }
    spValueSet_System, {@enum.value "system" spValueSet_System The system for any codes defined by this value set }
    spValueSet_Url, {@enum.value "url" spValueSet_Url The logical URL for the value set }
    spValueSet_Version); {@enum.value "version" spValueSet_Version The version identifier of the value set }
{$ENDIF}

{$IFDEF FHIR_VISIONPRESCRIPTION}
  {@Enum TSearchParamsVisionPrescription
    Search Parameters for VisionPrescription
  }
  TSearchParamsVisionPrescription = (
    spVisionPrescription__content, {@enum.value "_content" spVisionPrescription__content Search on the entire content of the resource }
    spVisionPrescription__id, {@enum.value "_id" spVisionPrescription__id Logical id of this artifact }
    spVisionPrescription__lastUpdated, {@enum.value "_lastUpdated" spVisionPrescription__lastUpdated When the resource version last changed }
    spVisionPrescription__profile, {@enum.value "_profile" spVisionPrescription__profile Profiles this resource claims to conform to }
    spVisionPrescription__query, {@enum.value "_query" spVisionPrescription__query A custom search profile that describes a specific defined query operation }
    spVisionPrescription__security, {@enum.value "_security" spVisionPrescription__security Security Labels applied to this resource }
    spVisionPrescription__tag, {@enum.value "_tag" spVisionPrescription__tag Tags applied to this resource }
    spVisionPrescription__text, {@enum.value "_text" spVisionPrescription__text Search on the narrative of the resource }
    spVisionPrescription_Datewritten, {@enum.value "datewritten" spVisionPrescription_Datewritten Return prescriptions written on this date }
    spVisionPrescription_Encounter, {@enum.value "encounter" spVisionPrescription_Encounter Return prescriptions with this encounter identifier }
    spVisionPrescription_Identifier, {@enum.value "identifier" spVisionPrescription_Identifier Return prescriptions with this external identifier }
    spVisionPrescription_Patient, {@enum.value "patient" spVisionPrescription_Patient The identity of a patient to list dispenses for }
    spVisionPrescription_Prescriber); {@enum.value "prescriber" spVisionPrescription_Prescriber Who authorizes the vision product }
{$ENDIF}

Const
  CODES_TFhirResourceType : Array[TFhirResourceType] of String = ('', {$IFDEF FHIR_ACCOUNT}'Account',{$ENDIF}
      {$IFDEF FHIR_ALLERGYINTOLERANCE}'AllergyIntolerance',{$ENDIF}
      {$IFDEF FHIR_APPOINTMENT}'Appointment',{$ENDIF}
      {$IFDEF FHIR_APPOINTMENTRESPONSE}'AppointmentResponse',{$ENDIF}
      {$IFDEF FHIR_AUDITEVENT}'AuditEvent',{$ENDIF}
      {$IFDEF FHIR_BASIC}'Basic',{$ENDIF}
      {$IFDEF FHIR_BINARY}'Binary',{$ENDIF}
      {$IFDEF FHIR_BODYSITE}'BodySite',{$ENDIF}
      {$IFDEF FHIR_BUNDLE}'Bundle',{$ENDIF}
      {$IFDEF FHIR_CAREPLAN}'CarePlan',{$ENDIF}
      {$IFDEF FHIR_CLAIM}'Claim',{$ENDIF}
      {$IFDEF FHIR_CLAIMRESPONSE}'ClaimResponse',{$ENDIF}
      {$IFDEF FHIR_CLINICALIMPRESSION}'ClinicalImpression',{$ENDIF}
      {$IFDEF FHIR_COMMUNICATION}'Communication',{$ENDIF}
      {$IFDEF FHIR_COMMUNICATIONREQUEST}'CommunicationRequest',{$ENDIF}
      {$IFDEF FHIR_COMPOSITION}'Composition',{$ENDIF}
      {$IFDEF FHIR_CONCEPTMAP}'ConceptMap',{$ENDIF}
      {$IFDEF FHIR_CONDITION}'Condition',{$ENDIF}
      {$IFDEF FHIR_CONFORMANCE}'Conformance',{$ENDIF}
      {$IFDEF FHIR_CONTRACT}'Contract',{$ENDIF}
      {$IFDEF FHIR_COVERAGE}'Coverage',{$ENDIF}
      {$IFDEF FHIR_DATAELEMENT}'DataElement',{$ENDIF}
      {$IFDEF FHIR_DETECTEDISSUE}'DetectedIssue',{$ENDIF}
      {$IFDEF FHIR_DEVICE}'Device',{$ENDIF}
      {$IFDEF FHIR_DEVICECOMPONENT}'DeviceComponent',{$ENDIF}
      {$IFDEF FHIR_DEVICEMETRIC}'DeviceMetric',{$ENDIF}
      {$IFDEF FHIR_DEVICEUSEREQUEST}'DeviceUseRequest',{$ENDIF}
      {$IFDEF FHIR_DEVICEUSESTATEMENT}'DeviceUseStatement',{$ENDIF}
      {$IFDEF FHIR_DIAGNOSTICORDER}'DiagnosticOrder',{$ENDIF}
      {$IFDEF FHIR_DIAGNOSTICREPORT}'DiagnosticReport',{$ENDIF}
      {$IFDEF FHIR_DOCUMENTMANIFEST}'DocumentManifest',{$ENDIF}
      {$IFDEF FHIR_DOCUMENTREFERENCE}'DocumentReference',{$ENDIF}
      {$IFDEF FHIR_ELIGIBILITYREQUEST}'EligibilityRequest',{$ENDIF}
      {$IFDEF FHIR_ELIGIBILITYRESPONSE}'EligibilityResponse',{$ENDIF}
      {$IFDEF FHIR_ENCOUNTER}'Encounter',{$ENDIF}
      {$IFDEF FHIR_ENROLLMENTREQUEST}'EnrollmentRequest',{$ENDIF}
      {$IFDEF FHIR_ENROLLMENTRESPONSE}'EnrollmentResponse',{$ENDIF}
      {$IFDEF FHIR_EPISODEOFCARE}'EpisodeOfCare',{$ENDIF}
      {$IFDEF FHIR_EXPLANATIONOFBENEFIT}'ExplanationOfBenefit',{$ENDIF}
      {$IFDEF FHIR_FAMILYMEMBERHISTORY}'FamilyMemberHistory',{$ENDIF}
      {$IFDEF FHIR_FLAG}'Flag',{$ENDIF}
      {$IFDEF FHIR_GOAL}'Goal',{$ENDIF}
      {$IFDEF FHIR_GROUP}'Group',{$ENDIF}
      {$IFDEF FHIR_HEALTHCARESERVICE}'HealthcareService',{$ENDIF}
      {$IFDEF FHIR_IMAGINGOBJECTSELECTION}'ImagingObjectSelection',{$ENDIF}
      {$IFDEF FHIR_IMAGINGSTUDY}'ImagingStudy',{$ENDIF}
      {$IFDEF FHIR_IMMUNIZATION}'Immunization',{$ENDIF}
      {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}'ImmunizationRecommendation',{$ENDIF}
      {$IFDEF FHIR_IMPLEMENTATIONGUIDE}'ImplementationGuide',{$ENDIF}
      {$IFDEF FHIR_LIST}'List',{$ENDIF}
      {$IFDEF FHIR_LOCATION}'Location',{$ENDIF}
      {$IFDEF FHIR_MEDIA}'Media',{$ENDIF}
      {$IFDEF FHIR_MEDICATION}'Medication',{$ENDIF}
      {$IFDEF FHIR_MEDICATIONADMINISTRATION}'MedicationAdministration',{$ENDIF}
      {$IFDEF FHIR_MEDICATIONDISPENSE}'MedicationDispense',{$ENDIF}
      {$IFDEF FHIR_MEDICATIONORDER}'MedicationOrder',{$ENDIF}
      
      {$IFDEF FHIR_MEDICATIONSTATEMENT}'MedicationStatement',{$ENDIF}
      {$IFDEF FHIR_MESSAGEHEADER}'MessageHeader',{$ENDIF}
      {$IFDEF FHIR_NAMINGSYSTEM}'NamingSystem',{$ENDIF}
      {$IFDEF FHIR_NUTRITIONORDER}'NutritionOrder',{$ENDIF}
      {$IFDEF FHIR_OBSERVATION}'Observation',{$ENDIF}
      {$IFDEF FHIR_OPERATIONDEFINITION}'OperationDefinition',{$ENDIF}
      {$IFDEF FHIR_OPERATIONOUTCOME}'OperationOutcome',{$ENDIF}
      {$IFDEF FHIR_ORDER}'Order',{$ENDIF}
      {$IFDEF FHIR_ORDERRESPONSE}'OrderResponse',{$ENDIF}
      {$IFDEF FHIR_ORGANIZATION}'Organization',{$ENDIF}
      {$IFDEF FHIR_PARAMETERS}'Parameters',{$ENDIF}
      {$IFDEF FHIR_PATIENT}'Patient',{$ENDIF}
      {$IFDEF FHIR_PAYMENTNOTICE}'PaymentNotice',{$ENDIF}
      {$IFDEF FHIR_PAYMENTRECONCILIATION}'PaymentReconciliation',{$ENDIF}
      {$IFDEF FHIR_PERSON}'Person',{$ENDIF}
      {$IFDEF FHIR_PRACTITIONER}'Practitioner',{$ENDIF}
      {$IFDEF FHIR_PROCEDURE}'Procedure',{$ENDIF}
      {$IFDEF FHIR_PROCEDUREREQUEST}'ProcedureRequest',{$ENDIF}
      {$IFDEF FHIR_PROCESSREQUEST}'ProcessRequest',{$ENDIF}
      {$IFDEF FHIR_PROCESSRESPONSE}'ProcessResponse',{$ENDIF}
      {$IFDEF FHIR_PROVENANCE}'Provenance',{$ENDIF}
      {$IFDEF FHIR_QUESTIONNAIRE}'Questionnaire',{$ENDIF}
      {$IFDEF FHIR_QUESTIONNAIRERESPONSE}'QuestionnaireResponse',{$ENDIF}
      {$IFDEF FHIR_REFERRALREQUEST}'ReferralRequest',{$ENDIF}
      {$IFDEF FHIR_RELATEDPERSON}'RelatedPerson',{$ENDIF}
      {$IFDEF FHIR_RISKASSESSMENT}'RiskAssessment',{$ENDIF}
      {$IFDEF FHIR_SCHEDULE}'Schedule',{$ENDIF}
      {$IFDEF FHIR_SEARCHPARAMETER}'SearchParameter',{$ENDIF}
      {$IFDEF FHIR_SLOT}'Slot',{$ENDIF}
      {$IFDEF FHIR_SPECIMEN}'Specimen',{$ENDIF}
      {$IFDEF FHIR_STRUCTUREDEFINITION}'StructureDefinition',{$ENDIF}
      {$IFDEF FHIR_SUBSCRIPTION}'Subscription',{$ENDIF}
      {$IFDEF FHIR_SUBSTANCE}'Substance',{$ENDIF}
      {$IFDEF FHIR_SUPPLYDELIVERY}'SupplyDelivery',{$ENDIF}
      {$IFDEF FHIR_SUPPLYREQUEST}'SupplyRequest',{$ENDIF}
      {$IFDEF FHIR_TESTSCRIPT}'TestScript',{$ENDIF}
      {$IFDEF FHIR_VALUESET}'ValueSet',{$ENDIF}
      {$IFDEF FHIR_VISIONPRESCRIPTION}'VisionPrescription',{$ENDIF}
       'Custom');
  LOWERCASE_CODES_TFhirResourceType : Array[TFhirResourceType] of String = ('', {$IFDEF FHIR_ACCOUNT}'account',{$ENDIF}
     {$IFDEF FHIR_ALLERGYINTOLERANCE}'allergyintolerance',{$ENDIF}
     {$IFDEF FHIR_APPOINTMENT}'appointment',{$ENDIF}
     {$IFDEF FHIR_APPOINTMENTRESPONSE}'appointmentresponse',{$ENDIF}
     {$IFDEF FHIR_AUDITEVENT}'auditevent',{$ENDIF}
     {$IFDEF FHIR_BASIC}'basic',{$ENDIF}
     {$IFDEF FHIR_BINARY}'binary',{$ENDIF}
     {$IFDEF FHIR_BODYSITE}'bodysite',{$ENDIF}
     {$IFDEF FHIR_BUNDLE}'bundle',{$ENDIF}
     {$IFDEF FHIR_CAREPLAN}'careplan',{$ENDIF}
     {$IFDEF FHIR_CLAIM}'claim',{$ENDIF}
     {$IFDEF FHIR_CLAIMRESPONSE}'claimresponse',{$ENDIF}
     {$IFDEF FHIR_CLINICALIMPRESSION}'clinicalimpression',{$ENDIF}
     {$IFDEF FHIR_COMMUNICATION}'communication',{$ENDIF}
     {$IFDEF FHIR_COMMUNICATIONREQUEST}'communicationrequest',{$ENDIF}
     {$IFDEF FHIR_COMPOSITION}'composition',{$ENDIF}
     {$IFDEF FHIR_CONCEPTMAP}'conceptmap',{$ENDIF}
     {$IFDEF FHIR_CONDITION}'condition',{$ENDIF}
     {$IFDEF FHIR_CONFORMANCE}'conformance',{$ENDIF}
     {$IFDEF FHIR_CONTRACT}'contract',{$ENDIF}
     {$IFDEF FHIR_COVERAGE}'coverage',{$ENDIF}
     {$IFDEF FHIR_DATAELEMENT}'dataelement',{$ENDIF}
     {$IFDEF FHIR_DETECTEDISSUE}'detectedissue',{$ENDIF}
     {$IFDEF FHIR_DEVICE}'device',{$ENDIF}
     {$IFDEF FHIR_DEVICECOMPONENT}'devicecomponent',{$ENDIF}
     {$IFDEF FHIR_DEVICEMETRIC}'devicemetric',{$ENDIF}
     {$IFDEF FHIR_DEVICEUSEREQUEST}'deviceuserequest',{$ENDIF}
     {$IFDEF FHIR_DEVICEUSESTATEMENT}'deviceusestatement',{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICORDER}'diagnosticorder',{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICREPORT}'diagnosticreport',{$ENDIF}
     {$IFDEF FHIR_DOCUMENTMANIFEST}'documentmanifest',{$ENDIF}
     {$IFDEF FHIR_DOCUMENTREFERENCE}'documentreference',{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYREQUEST}'eligibilityrequest',{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYRESPONSE}'eligibilityresponse',{$ENDIF}
     {$IFDEF FHIR_ENCOUNTER}'encounter',{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTREQUEST}'enrollmentrequest',{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTRESPONSE}'enrollmentresponse',{$ENDIF}
     {$IFDEF FHIR_EPISODEOFCARE}'episodeofcare',{$ENDIF}
     {$IFDEF FHIR_EXPLANATIONOFBENEFIT}'explanationofbenefit',{$ENDIF}
     {$IFDEF FHIR_FAMILYMEMBERHISTORY}'familymemberhistory',{$ENDIF}
     {$IFDEF FHIR_FLAG}'flag',{$ENDIF}
     {$IFDEF FHIR_GOAL}'goal',{$ENDIF}
     {$IFDEF FHIR_GROUP}'group',{$ENDIF}
     {$IFDEF FHIR_HEALTHCARESERVICE}'healthcareservice',{$ENDIF}
     {$IFDEF FHIR_IMAGINGOBJECTSELECTION}'imagingobjectselection',{$ENDIF}
     {$IFDEF FHIR_IMAGINGSTUDY}'imagingstudy',{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATION}'immunization',{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}'immunizationrecommendation',{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDE}'implementationguide',{$ENDIF}
     {$IFDEF FHIR_LIST}'list',{$ENDIF}
     {$IFDEF FHIR_LOCATION}'location',{$ENDIF}
     {$IFDEF FHIR_MEDIA}'media',{$ENDIF}
     {$IFDEF FHIR_MEDICATION}'medication',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONADMINISTRATION}'medicationadministration',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONDISPENSE}'medicationdispense',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONORDER}'medicationorder',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONSTATEMENT}'medicationstatement',{$ENDIF}
     {$IFDEF FHIR_MESSAGEHEADER}'messageheader',{$ENDIF}
     {$IFDEF FHIR_NAMINGSYSTEM}'namingsystem',{$ENDIF}
     {$IFDEF FHIR_NUTRITIONORDER}'nutritionorder',{$ENDIF}
     {$IFDEF FHIR_OBSERVATION}'observation',{$ENDIF}
     {$IFDEF FHIR_OPERATIONDEFINITION}'operationdefinition',{$ENDIF}
     {$IFDEF FHIR_OPERATIONOUTCOME}'operationoutcome',{$ENDIF}
     {$IFDEF FHIR_ORDER}'order',{$ENDIF}
     {$IFDEF FHIR_ORDERRESPONSE}'orderresponse',{$ENDIF}
     {$IFDEF FHIR_ORGANIZATION}'organization',{$ENDIF}
     {$IFDEF FHIR_PARAMETERS}'parameters',{$ENDIF}
     {$IFDEF FHIR_PATIENT}'patient',{$ENDIF}
     {$IFDEF FHIR_PAYMENTNOTICE}'paymentnotice',{$ENDIF}
     {$IFDEF FHIR_PAYMENTRECONCILIATION}'paymentreconciliation',{$ENDIF}
     {$IFDEF FHIR_PERSON}'person',{$ENDIF}
     {$IFDEF FHIR_PRACTITIONER}'practitioner',{$ENDIF}
     {$IFDEF FHIR_PROCEDURE}'procedure',{$ENDIF}
     {$IFDEF FHIR_PROCEDUREREQUEST}'procedurerequest',{$ENDIF}
     {$IFDEF FHIR_PROCESSREQUEST}'processrequest',{$ENDIF}
     {$IFDEF FHIR_PROCESSRESPONSE}'processresponse',{$ENDIF}
     {$IFDEF FHIR_PROVENANCE}'provenance',{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRE}'questionnaire',{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRERESPONSE}'questionnaireresponse',{$ENDIF}
     {$IFDEF FHIR_REFERRALREQUEST}'referralrequest',{$ENDIF}
     {$IFDEF FHIR_RELATEDPERSON}'relatedperson',{$ENDIF}
     {$IFDEF FHIR_RISKASSESSMENT}'riskassessment',{$ENDIF}
     {$IFDEF FHIR_SCHEDULE}'schedule',{$ENDIF}
     {$IFDEF FHIR_SEARCHPARAMETER}'searchparameter',{$ENDIF}
     {$IFDEF FHIR_SLOT}'slot',{$ENDIF}
     {$IFDEF FHIR_SPECIMEN}'specimen',{$ENDIF}
     {$IFDEF FHIR_STRUCTUREDEFINITION}'structuredefinition',{$ENDIF}
     {$IFDEF FHIR_SUBSCRIPTION}'subscription',{$ENDIF}
     {$IFDEF FHIR_SUBSTANCE}'substance',{$ENDIF}
     {$IFDEF FHIR_SUPPLYDELIVERY}'supplydelivery',{$ENDIF}
     {$IFDEF FHIR_SUPPLYREQUEST}'supplyrequest',{$ENDIF}
     {$IFDEF FHIR_TESTSCRIPT}'testscript',{$ENDIF}
     {$IFDEF FHIR_VALUESET}'valueset',{$ENDIF}
     {$IFDEF FHIR_VISIONPRESCRIPTION}'visionprescription',{$ENDIF}
     'custom');
     
  CLASSES_TFhirResourceType : Array[TFhirResourceType] of TFhirResourceClass = (nil, {$IFDEF FHIR_ACCOUNT}TFhirAccount,{$ENDIF}
     {$IFDEF FHIR_ALLERGYINTOLERANCE}TFhirAllergyIntolerance,{$ENDIF}
     {$IFDEF FHIR_APPOINTMENT}TFhirAppointment,{$ENDIF}
     {$IFDEF FHIR_APPOINTMENTRESPONSE}TFhirAppointmentResponse,{$ENDIF}
     {$IFDEF FHIR_AUDITEVENT}TFhirAuditEvent,{$ENDIF}
     {$IFDEF FHIR_BASIC}TFhirBasic,{$ENDIF}
     {$IFDEF FHIR_BINARY}TFhirBinary,{$ENDIF}
     {$IFDEF FHIR_BODYSITE}TFhirBodySite,{$ENDIF}
     {$IFDEF FHIR_BUNDLE}TFhirBundle,{$ENDIF}
     {$IFDEF FHIR_CAREPLAN}TFhirCarePlan,{$ENDIF}
     {$IFDEF FHIR_CLAIM}TFhirClaim,{$ENDIF}
     {$IFDEF FHIR_CLAIMRESPONSE}TFhirClaimResponse,{$ENDIF}
     {$IFDEF FHIR_CLINICALIMPRESSION}TFhirClinicalImpression,{$ENDIF}
     {$IFDEF FHIR_COMMUNICATION}TFhirCommunication,{$ENDIF}
     {$IFDEF FHIR_COMMUNICATIONREQUEST}TFhirCommunicationRequest,{$ENDIF}
     {$IFDEF FHIR_COMPOSITION}TFhirComposition,{$ENDIF}
     {$IFDEF FHIR_CONCEPTMAP}TFhirConceptMap,{$ENDIF}
     {$IFDEF FHIR_CONDITION}TFhirCondition,{$ENDIF}
     {$IFDEF FHIR_CONFORMANCE}TFhirConformance,{$ENDIF}
     {$IFDEF FHIR_CONTRACT}TFhirContract,{$ENDIF}
     {$IFDEF FHIR_COVERAGE}TFhirCoverage,{$ENDIF}
     {$IFDEF FHIR_DATAELEMENT}TFhirDataElement,{$ENDIF}
     {$IFDEF FHIR_DETECTEDISSUE}TFhirDetectedIssue,{$ENDIF}
     {$IFDEF FHIR_DEVICE}TFhirDevice,{$ENDIF}
     {$IFDEF FHIR_DEVICECOMPONENT}TFhirDeviceComponent,{$ENDIF}
     {$IFDEF FHIR_DEVICEMETRIC}TFhirDeviceMetric,{$ENDIF}
     {$IFDEF FHIR_DEVICEUSEREQUEST}TFhirDeviceUseRequest,{$ENDIF}
     {$IFDEF FHIR_DEVICEUSESTATEMENT}TFhirDeviceUseStatement,{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICORDER}TFhirDiagnosticOrder,{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICREPORT}TFhirDiagnosticReport,{$ENDIF}
     {$IFDEF FHIR_DOCUMENTMANIFEST}TFhirDocumentManifest,{$ENDIF}
     {$IFDEF FHIR_DOCUMENTREFERENCE}TFhirDocumentReference,{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYREQUEST}TFhirEligibilityRequest,{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYRESPONSE}TFhirEligibilityResponse,{$ENDIF}
     {$IFDEF FHIR_ENCOUNTER}TFhirEncounter,{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTREQUEST}TFhirEnrollmentRequest,{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTRESPONSE}TFhirEnrollmentResponse,{$ENDIF}
     {$IFDEF FHIR_EPISODEOFCARE}TFhirEpisodeOfCare,{$ENDIF}
     {$IFDEF FHIR_EXPLANATIONOFBENEFIT}TFhirExplanationOfBenefit,{$ENDIF}
     {$IFDEF FHIR_FAMILYMEMBERHISTORY}TFhirFamilyMemberHistory,{$ENDIF}
     {$IFDEF FHIR_FLAG}TFhirFlag,{$ENDIF}
     {$IFDEF FHIR_GOAL}TFhirGoal,{$ENDIF}
     {$IFDEF FHIR_GROUP}TFhirGroup,{$ENDIF}
     {$IFDEF FHIR_HEALTHCARESERVICE}TFhirHealthcareService,{$ENDIF}
     {$IFDEF FHIR_IMAGINGOBJECTSELECTION}TFhirImagingObjectSelection,{$ENDIF}
     {$IFDEF FHIR_IMAGINGSTUDY}TFhirImagingStudy,{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATION}TFhirImmunization,{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}TFhirImmunizationRecommendation,{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDE}TFhirImplementationGuide,{$ENDIF}
     {$IFDEF FHIR_LIST}TFhirList,{$ENDIF}
     {$IFDEF FHIR_LOCATION}TFhirLocation,{$ENDIF}
     {$IFDEF FHIR_MEDIA}TFhirMedia,{$ENDIF}
     {$IFDEF FHIR_MEDICATION}TFhirMedication,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONADMINISTRATION}TFhirMedicationAdministration,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONDISPENSE}TFhirMedicationDispense,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONORDER}TFhirMedicationOrder,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONSTATEMENT}TFhirMedicationStatement,{$ENDIF}
     {$IFDEF FHIR_MESSAGEHEADER}TFhirMessageHeader,{$ENDIF}
     {$IFDEF FHIR_NAMINGSYSTEM}TFhirNamingSystem,{$ENDIF}
     {$IFDEF FHIR_NUTRITIONORDER}TFhirNutritionOrder,{$ENDIF}
     {$IFDEF FHIR_OBSERVATION}TFhirObservation,{$ENDIF}
     {$IFDEF FHIR_OPERATIONDEFINITION}TFhirOperationDefinition,{$ENDIF}
     {$IFDEF FHIR_OPERATIONOUTCOME}TFhirOperationOutcome,{$ENDIF}
     {$IFDEF FHIR_ORDER}TFhirOrder,{$ENDIF}
     {$IFDEF FHIR_ORDERRESPONSE}TFhirOrderResponse,{$ENDIF}
     {$IFDEF FHIR_ORGANIZATION}TFhirOrganization,{$ENDIF}
     {$IFDEF FHIR_PARAMETERS}TFhirParameters,{$ENDIF}
     {$IFDEF FHIR_PATIENT}TFhirPatient,{$ENDIF}
     {$IFDEF FHIR_PAYMENTNOTICE}TFhirPaymentNotice,{$ENDIF}
     {$IFDEF FHIR_PAYMENTRECONCILIATION}TFhirPaymentReconciliation,{$ENDIF}
     {$IFDEF FHIR_PERSON}TFhirPerson,{$ENDIF}
     {$IFDEF FHIR_PRACTITIONER}TFhirPractitioner,{$ENDIF}
     {$IFDEF FHIR_PROCEDURE}TFhirProcedure,{$ENDIF}
     {$IFDEF FHIR_PROCEDUREREQUEST}TFhirProcedureRequest,{$ENDIF}
     {$IFDEF FHIR_PROCESSREQUEST}TFhirProcessRequest,{$ENDIF}
     {$IFDEF FHIR_PROCESSRESPONSE}TFhirProcessResponse,{$ENDIF}
     {$IFDEF FHIR_PROVENANCE}TFhirProvenance,{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRE}TFhirQuestionnaire,{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRERESPONSE}TFhirQuestionnaireResponse,{$ENDIF}
     {$IFDEF FHIR_REFERRALREQUEST}TFhirReferralRequest,{$ENDIF}
     {$IFDEF FHIR_RELATEDPERSON}TFhirRelatedPerson,{$ENDIF}
     {$IFDEF FHIR_RISKASSESSMENT}TFhirRiskAssessment,{$ENDIF}
     {$IFDEF FHIR_SCHEDULE}TFhirSchedule,{$ENDIF}
     {$IFDEF FHIR_SEARCHPARAMETER}TFhirSearchParameter,{$ENDIF}
     {$IFDEF FHIR_SLOT}TFhirSlot,{$ENDIF}
     {$IFDEF FHIR_SPECIMEN}TFhirSpecimen,{$ENDIF}
     {$IFDEF FHIR_STRUCTUREDEFINITION}TFhirStructureDefinition,{$ENDIF}
     {$IFDEF FHIR_SUBSCRIPTION}TFhirSubscription,{$ENDIF}
     {$IFDEF FHIR_SUBSTANCE}TFhirSubstance,{$ENDIF}
     {$IFDEF FHIR_SUPPLYDELIVERY}TFhirSupplyDelivery,{$ENDIF}
     {$IFDEF FHIR_SUPPLYREQUEST}TFhirSupplyRequest,{$ENDIF}
     {$IFDEF FHIR_TESTSCRIPT}TFhirTestScript,{$ENDIF}
     {$IFDEF FHIR_VALUESET}TFhirValueSet,{$ENDIF}
     {$IFDEF FHIR_VISIONPRESCRIPTION}TFhirVisionPrescription,{$ENDIF}
     nil);
     
  ALL_RESOURCE_TYPES = [{$IFDEF FHIR_ACCOUNT}frtAccount,{$ENDIF}
     {$IFDEF FHIR_ALLERGYINTOLERANCE}frtAllergyIntolerance,{$ENDIF}
     {$IFDEF FHIR_APPOINTMENT}frtAppointment,{$ENDIF}
     {$IFDEF FHIR_APPOINTMENTRESPONSE}frtAppointmentResponse,{$ENDIF}
     {$IFDEF FHIR_AUDITEVENT}frtAuditEvent,{$ENDIF}
     {$IFDEF FHIR_BASIC}frtBasic,{$ENDIF}
     {$IFDEF FHIR_BINARY}frtBinary,{$ENDIF}
     {$IFDEF FHIR_BODYSITE}frtBodySite,{$ENDIF}
     {$IFDEF FHIR_BUNDLE}frtBundle,{$ENDIF}
     {$IFDEF FHIR_CAREPLAN}frtCarePlan,{$ENDIF}
     {$IFDEF FHIR_CLAIM}frtClaim,{$ENDIF}
     {$IFDEF FHIR_CLAIMRESPONSE}frtClaimResponse,{$ENDIF}
     {$IFDEF FHIR_CLINICALIMPRESSION}frtClinicalImpression,{$ENDIF}
     {$IFDEF FHIR_COMMUNICATION}frtCommunication,{$ENDIF}
     {$IFDEF FHIR_COMMUNICATIONREQUEST}frtCommunicationRequest,{$ENDIF}
     {$IFDEF FHIR_COMPOSITION}frtComposition,{$ENDIF}
     {$IFDEF FHIR_CONCEPTMAP}frtConceptMap,{$ENDIF}
     {$IFDEF FHIR_CONDITION}frtCondition,{$ENDIF}
     {$IFDEF FHIR_CONFORMANCE}frtConformance,{$ENDIF}
     {$IFDEF FHIR_CONTRACT}frtContract,{$ENDIF}
     {$IFDEF FHIR_COVERAGE}frtCoverage,{$ENDIF}
     {$IFDEF FHIR_DATAELEMENT}frtDataElement,{$ENDIF}
     {$IFDEF FHIR_DETECTEDISSUE}frtDetectedIssue,{$ENDIF}
     {$IFDEF FHIR_DEVICE}frtDevice,{$ENDIF}
     {$IFDEF FHIR_DEVICECOMPONENT}frtDeviceComponent,{$ENDIF}
     {$IFDEF FHIR_DEVICEMETRIC}frtDeviceMetric,{$ENDIF}
     {$IFDEF FHIR_DEVICEUSEREQUEST}frtDeviceUseRequest,{$ENDIF}
     {$IFDEF FHIR_DEVICEUSESTATEMENT}frtDeviceUseStatement,{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICORDER}frtDiagnosticOrder,{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICREPORT}frtDiagnosticReport,{$ENDIF}
     {$IFDEF FHIR_DOCUMENTMANIFEST}frtDocumentManifest,{$ENDIF}
     {$IFDEF FHIR_DOCUMENTREFERENCE}frtDocumentReference,{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYREQUEST}frtEligibilityRequest,{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYRESPONSE}frtEligibilityResponse,{$ENDIF}
     {$IFDEF FHIR_ENCOUNTER}frtEncounter,{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTREQUEST}frtEnrollmentRequest,{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTRESPONSE}frtEnrollmentResponse,{$ENDIF}
     {$IFDEF FHIR_EPISODEOFCARE}frtEpisodeOfCare,{$ENDIF}
     {$IFDEF FHIR_EXPLANATIONOFBENEFIT}frtExplanationOfBenefit,{$ENDIF}
     {$IFDEF FHIR_FAMILYMEMBERHISTORY}frtFamilyMemberHistory,{$ENDIF}
     {$IFDEF FHIR_FLAG}frtFlag,{$ENDIF}
     {$IFDEF FHIR_GOAL}frtGoal,{$ENDIF}
     {$IFDEF FHIR_GROUP}frtGroup,{$ENDIF}
     {$IFDEF FHIR_HEALTHCARESERVICE}frtHealthcareService,{$ENDIF}
     {$IFDEF FHIR_IMAGINGOBJECTSELECTION}frtImagingObjectSelection,{$ENDIF}
     {$IFDEF FHIR_IMAGINGSTUDY}frtImagingStudy,{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATION}frtImmunization,{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}frtImmunizationRecommendation,{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDE}frtImplementationGuide,{$ENDIF}
     {$IFDEF FHIR_LIST}frtList,{$ENDIF}
     {$IFDEF FHIR_LOCATION}frtLocation,{$ENDIF}
     {$IFDEF FHIR_MEDIA}frtMedia,{$ENDIF}
     {$IFDEF FHIR_MEDICATION}frtMedication,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONADMINISTRATION}frtMedicationAdministration,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONDISPENSE}frtMedicationDispense,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONORDER}frtMedicationOrder,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONSTATEMENT}frtMedicationStatement,{$ENDIF}
     {$IFDEF FHIR_MESSAGEHEADER}frtMessageHeader,{$ENDIF}
     {$IFDEF FHIR_NAMINGSYSTEM}frtNamingSystem,{$ENDIF}
     {$IFDEF FHIR_NUTRITIONORDER}frtNutritionOrder,{$ENDIF}
     {$IFDEF FHIR_OBSERVATION}frtObservation,{$ENDIF}
     {$IFDEF FHIR_OPERATIONDEFINITION}frtOperationDefinition,{$ENDIF}
     {$IFDEF FHIR_OPERATIONOUTCOME}frtOperationOutcome,{$ENDIF}
     {$IFDEF FHIR_ORDER}frtOrder,{$ENDIF}
     {$IFDEF FHIR_ORDERRESPONSE}frtOrderResponse,{$ENDIF}
     {$IFDEF FHIR_ORGANIZATION}frtOrganization,{$ENDIF}
     {$IFDEF FHIR_PARAMETERS}frtParameters,{$ENDIF}
     {$IFDEF FHIR_PATIENT}frtPatient,{$ENDIF}
     {$IFDEF FHIR_PAYMENTNOTICE}frtPaymentNotice,{$ENDIF}
     {$IFDEF FHIR_PAYMENTRECONCILIATION}frtPaymentReconciliation,{$ENDIF}
     {$IFDEF FHIR_PERSON}frtPerson,{$ENDIF}
     {$IFDEF FHIR_PRACTITIONER}frtPractitioner,{$ENDIF}
     {$IFDEF FHIR_PROCEDURE}frtProcedure,{$ENDIF}
     {$IFDEF FHIR_PROCEDUREREQUEST}frtProcedureRequest,{$ENDIF}
     {$IFDEF FHIR_PROCESSREQUEST}frtProcessRequest,{$ENDIF}
     {$IFDEF FHIR_PROCESSRESPONSE}frtProcessResponse,{$ENDIF}
     {$IFDEF FHIR_PROVENANCE}frtProvenance,{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRE}frtQuestionnaire,{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRERESPONSE}frtQuestionnaireResponse,{$ENDIF}
     {$IFDEF FHIR_REFERRALREQUEST}frtReferralRequest,{$ENDIF}
     {$IFDEF FHIR_RELATEDPERSON}frtRelatedPerson,{$ENDIF}
     {$IFDEF FHIR_RISKASSESSMENT}frtRiskAssessment,{$ENDIF}
     {$IFDEF FHIR_SCHEDULE}frtSchedule,{$ENDIF}
     {$IFDEF FHIR_SEARCHPARAMETER}frtSearchParameter,{$ENDIF}
     {$IFDEF FHIR_SLOT}frtSlot,{$ENDIF}
     {$IFDEF FHIR_SPECIMEN}frtSpecimen,{$ENDIF}
     {$IFDEF FHIR_STRUCTUREDEFINITION}frtStructureDefinition,{$ENDIF}
     {$IFDEF FHIR_SUBSCRIPTION}frtSubscription,{$ENDIF}
     {$IFDEF FHIR_SUBSTANCE}frtSubstance,{$ENDIF}
     {$IFDEF FHIR_SUPPLYDELIVERY}frtSupplyDelivery,{$ENDIF}
     {$IFDEF FHIR_SUPPLYREQUEST}frtSupplyRequest,{$ENDIF}
     {$IFDEF FHIR_TESTSCRIPT}frtTestScript,{$ENDIF}
     {$IFDEF FHIR_VALUESET}frtValueSet,{$ENDIF}
     {$IFDEF FHIR_VISIONPRESCRIPTION}frtVisionPrescription,{$ENDIF}
     frtCustom];
     
  ALL_RESOURCE_TYPE_NAMES : Array [TFHIRResourceType] of String = ('--None--', {$IFDEF FHIR_ACCOUNT}'Account',{$ENDIF}
     {$IFDEF FHIR_ALLERGYINTOLERANCE}'AllergyIntolerance',{$ENDIF}
     {$IFDEF FHIR_APPOINTMENT}'Appointment',{$ENDIF}
     {$IFDEF FHIR_APPOINTMENTRESPONSE}'AppointmentResponse',{$ENDIF}
     {$IFDEF FHIR_AUDITEVENT}'AuditEvent',{$ENDIF}
     {$IFDEF FHIR_BASIC}'Basic',{$ENDIF}
     {$IFDEF FHIR_BINARY}'Binary',{$ENDIF}
     {$IFDEF FHIR_BODYSITE}'BodySite',{$ENDIF}
     {$IFDEF FHIR_BUNDLE}'Bundle',{$ENDIF}
     {$IFDEF FHIR_CAREPLAN}'CarePlan',{$ENDIF}
     {$IFDEF FHIR_CLAIM}'Claim',{$ENDIF}
     {$IFDEF FHIR_CLAIMRESPONSE}'ClaimResponse',{$ENDIF}
     {$IFDEF FHIR_CLINICALIMPRESSION}'ClinicalImpression',{$ENDIF}
     {$IFDEF FHIR_COMMUNICATION}'Communication',{$ENDIF}
     {$IFDEF FHIR_COMMUNICATIONREQUEST}'CommunicationRequest',{$ENDIF}
     {$IFDEF FHIR_COMPOSITION}'Composition',{$ENDIF}
     {$IFDEF FHIR_CONCEPTMAP}'ConceptMap',{$ENDIF}
     {$IFDEF FHIR_CONDITION}'Condition',{$ENDIF}
     {$IFDEF FHIR_CONFORMANCE}'Conformance',{$ENDIF}
     {$IFDEF FHIR_CONTRACT}'Contract',{$ENDIF}
     {$IFDEF FHIR_COVERAGE}'Coverage',{$ENDIF}
     {$IFDEF FHIR_DATAELEMENT}'DataElement',{$ENDIF}
     {$IFDEF FHIR_DETECTEDISSUE}'DetectedIssue',{$ENDIF}
     {$IFDEF FHIR_DEVICE}'Device',{$ENDIF}
     {$IFDEF FHIR_DEVICECOMPONENT}'DeviceComponent',{$ENDIF}
     {$IFDEF FHIR_DEVICEMETRIC}'DeviceMetric',{$ENDIF}
     {$IFDEF FHIR_DEVICEUSEREQUEST}'DeviceUseRequest',{$ENDIF}
     {$IFDEF FHIR_DEVICEUSESTATEMENT}'DeviceUseStatement',{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICORDER}'DiagnosticOrder',{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICREPORT}'DiagnosticReport',{$ENDIF}
     {$IFDEF FHIR_DOCUMENTMANIFEST}'DocumentManifest',{$ENDIF}
     {$IFDEF FHIR_DOCUMENTREFERENCE}'DocumentReference',{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYREQUEST}'EligibilityRequest',{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYRESPONSE}'EligibilityResponse',{$ENDIF}
     {$IFDEF FHIR_ENCOUNTER}'Encounter',{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTREQUEST}'EnrollmentRequest',{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTRESPONSE}'EnrollmentResponse',{$ENDIF}
     {$IFDEF FHIR_EPISODEOFCARE}'EpisodeOfCare',{$ENDIF}
     {$IFDEF FHIR_EXPLANATIONOFBENEFIT}'ExplanationOfBenefit',{$ENDIF}
     {$IFDEF FHIR_FAMILYMEMBERHISTORY}'FamilyMemberHistory',{$ENDIF}
     {$IFDEF FHIR_FLAG}'Flag',{$ENDIF}
     {$IFDEF FHIR_GOAL}'Goal',{$ENDIF}
     {$IFDEF FHIR_GROUP}'Group',{$ENDIF}
     {$IFDEF FHIR_HEALTHCARESERVICE}'HealthcareService',{$ENDIF}
     {$IFDEF FHIR_IMAGINGOBJECTSELECTION}'ImagingObjectSelection',{$ENDIF}
     {$IFDEF FHIR_IMAGINGSTUDY}'ImagingStudy',{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATION}'Immunization',{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}'ImmunizationRecommendation',{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDE}'ImplementationGuide',{$ENDIF}
     {$IFDEF FHIR_LIST}'List',{$ENDIF}
     {$IFDEF FHIR_LOCATION}'Location',{$ENDIF}
     {$IFDEF FHIR_MEDIA}'Media',{$ENDIF}
     {$IFDEF FHIR_MEDICATION}'Medication',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONADMINISTRATION}'MedicationAdministration',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONDISPENSE}'MedicationDispense',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONORDER}'MedicationOrder',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONSTATEMENT}'MedicationStatement',{$ENDIF}
     {$IFDEF FHIR_MESSAGEHEADER}'MessageHeader',{$ENDIF}
     {$IFDEF FHIR_NAMINGSYSTEM}'NamingSystem',{$ENDIF}
     {$IFDEF FHIR_NUTRITIONORDER}'NutritionOrder',{$ENDIF}
     {$IFDEF FHIR_OBSERVATION}'Observation',{$ENDIF}
     {$IFDEF FHIR_OPERATIONDEFINITION}'OperationDefinition',{$ENDIF}
     {$IFDEF FHIR_OPERATIONOUTCOME}'OperationOutcome',{$ENDIF}
     {$IFDEF FHIR_ORDER}'Order',{$ENDIF}
     {$IFDEF FHIR_ORDERRESPONSE}'OrderResponse',{$ENDIF}
     {$IFDEF FHIR_ORGANIZATION}'Organization',{$ENDIF}
     {$IFDEF FHIR_PARAMETERS}'Parameters',{$ENDIF}
     {$IFDEF FHIR_PATIENT}'Patient',{$ENDIF}
     {$IFDEF FHIR_PAYMENTNOTICE}'PaymentNotice',{$ENDIF}
     {$IFDEF FHIR_PAYMENTRECONCILIATION}'PaymentReconciliation',{$ENDIF}
     {$IFDEF FHIR_PERSON}'Person',{$ENDIF}
     {$IFDEF FHIR_PRACTITIONER}'Practitioner',{$ENDIF}
     {$IFDEF FHIR_PROCEDURE}'Procedure',{$ENDIF}
     {$IFDEF FHIR_PROCEDUREREQUEST}'ProcedureRequest',{$ENDIF}
     {$IFDEF FHIR_PROCESSREQUEST}'ProcessRequest',{$ENDIF}
     {$IFDEF FHIR_PROCESSRESPONSE}'ProcessResponse',{$ENDIF}
     {$IFDEF FHIR_PROVENANCE}'Provenance',{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRE}'Questionnaire',{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRERESPONSE}'QuestionnaireResponse',{$ENDIF}
     {$IFDEF FHIR_REFERRALREQUEST}'ReferralRequest',{$ENDIF}
     {$IFDEF FHIR_RELATEDPERSON}'RelatedPerson',{$ENDIF}
     {$IFDEF FHIR_RISKASSESSMENT}'RiskAssessment',{$ENDIF}
     {$IFDEF FHIR_SCHEDULE}'Schedule',{$ENDIF}
     {$IFDEF FHIR_SEARCHPARAMETER}'SearchParameter',{$ENDIF}
     {$IFDEF FHIR_SLOT}'Slot',{$ENDIF}
     {$IFDEF FHIR_SPECIMEN}'Specimen',{$ENDIF}
     {$IFDEF FHIR_STRUCTUREDEFINITION}'StructureDefinition',{$ENDIF}
     {$IFDEF FHIR_SUBSCRIPTION}'Subscription',{$ENDIF}
     {$IFDEF FHIR_SUBSTANCE}'Substance',{$ENDIF}
     {$IFDEF FHIR_SUPPLYDELIVERY}'SupplyDelivery',{$ENDIF}
     {$IFDEF FHIR_SUPPLYREQUEST}'SupplyRequest',{$ENDIF}
     {$IFDEF FHIR_TESTSCRIPT}'TestScript',{$ENDIF}
     {$IFDEF FHIR_VALUESET}'ValueSet',{$ENDIF}
     {$IFDEF FHIR_VISIONPRESCRIPTION}'VisionPrescription',{$ENDIF}
     'Custom');
     
{$IFDEF FHIR_ACCOUNT}
  CODES_TSearchParamsAccount : Array[TSearchParamsAccount] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'balance', 'identifier', 'name', 'owner', 'patient', 'period', 'status', 'subject', 'type');
{$ENDIF}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  CODES_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'category', 'criticality', 'date', 'identifier', 'last-date', 'manifestation', 'onset', 'patient', 'recorder', 'reporter', 'route', 'severity', 'status', 'substance', 'type');
{$ENDIF}
{$IFDEF FHIR_APPOINTMENT}
  CODES_TSearchParamsAppointment : Array[TSearchParamsAppointment] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'actor', 'date', 'identifier', 'location', 'part-status', 'patient', 'practitioner', 'status');
{$ENDIF}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  CODES_TSearchParamsAppointmentResponse : Array[TSearchParamsAppointmentResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'actor', 'appointment', 'identifier', 'location', 'part-status', 'patient', 'practitioner');
{$ENDIF}
{$IFDEF FHIR_AUDITEVENT}
  CODES_TSearchParamsAuditEvent : Array[TSearchParamsAuditEvent] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'action', 'address', 'altid', 'date', 'desc', 'identity', 'name', 'object-type', 'participant', 'patient', 'policy', 'reference', 'site', 'source', 'subtype', 'type', 'user');
{$ENDIF}
{$IFDEF FHIR_BASIC}
  CODES_TSearchParamsBasic : Array[TSearchParamsBasic] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'author', 'code', 'created', 'identifier', 'patient', 'subject');
{$ENDIF}
{$IFDEF FHIR_BINARY}
  CODES_TSearchParamsBinary : Array[TSearchParamsBinary] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'contenttype');
{$ENDIF}
{$IFDEF FHIR_BODYSITE}
  CODES_TSearchParamsBodySite : Array[TSearchParamsBodySite] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'identifier', 'patient');
{$ENDIF}
{$IFDEF FHIR_BUNDLE}
  CODES_TSearchParamsBundle : Array[TSearchParamsBundle] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'composition', 'message', 'type');
{$ENDIF}
{$IFDEF FHIR_CAREPLAN}
  CODES_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'activitycode', 'activitydate', 'activityreference', 'condition', 'date', 'goal', 'participant', 'patient', 'performer', 'related', 'relatedcode', 'relatedplan', 'subject');
{$ENDIF}
{$IFDEF FHIR_CLAIM}
  CODES_TSearchParamsClaim : Array[TSearchParamsClaim] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier', 'patient', 'priority', 'provider', 'use');
{$ENDIF}
{$IFDEF FHIR_CLAIMRESPONSE}
  CODES_TSearchParamsClaimResponse : Array[TSearchParamsClaimResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier');
{$ENDIF}
{$IFDEF FHIR_CLINICALIMPRESSION}
  CODES_TSearchParamsClinicalImpression : Array[TSearchParamsClinicalImpression] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'action', 'assessor', 'date', 'finding', 'investigation', 'patient', 'plan', 'previous', 'problem', 'resolved', 'ruledout', 'status', 'trigger', 'trigger-code');
{$ENDIF}
{$IFDEF FHIR_COMMUNICATION}
  CODES_TSearchParamsCommunication : Array[TSearchParamsCommunication] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'category', 'encounter', 'identifier', 'medium', 'patient', 'received', 'recipient', 'request', 'sender', 'sent', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  CODES_TSearchParamsCommunicationRequest : Array[TSearchParamsCommunicationRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'category', 'encounter', 'identifier', 'medium', 'patient', 'priority', 'recipient', 'requested', 'requester', 'sender', 'status', 'subject', 'time');
{$ENDIF}
{$IFDEF FHIR_COMPOSITION}
  CODES_TSearchParamsComposition : Array[TSearchParamsComposition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'attester', 'author', 'class', 'confidentiality', 'context', 'date', 'encounter', 'entry', 'identifier', 'patient', 'period', 'section', 'status', 'subject', 'title', 'type');
{$ENDIF}
{$IFDEF FHIR_CONCEPTMAP}
  CODES_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'context', 'date', 'dependson', 'description', 'identifier', 'name', 'product', 'publisher', 'source', 'sourcecode', 'sourcesystem', 'sourceuri', 'status', 'target', 'targetcode', 'targetsystem', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_CONDITION}
  CODES_TSearchParamsCondition : Array[TSearchParamsCondition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'age', 'asserter', 'body-site', 'category', 'clinicalstatus', 'code', 'date-recorded', 'encounter', 'evidence', 'identifier', 'onset', 'onset-info', 'patient', 'severity', 'stage');
{$ENDIF}
{$IFDEF FHIR_CONFORMANCE}
  CODES_TSearchParamsConformance : Array[TSearchParamsConformance] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'date', 'description', 'event', 'fhirversion', 'format', 'mode', 'name', 'profile', 'publisher', 'resource', 'security', 'software', 'status', 'supported-profile', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_CONTRACT}
  CODES_TSearchParamsContract : Array[TSearchParamsContract] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'actor', 'identifier', 'patient', 'signer', 'subject');
{$ENDIF}
{$IFDEF FHIR_COVERAGE}
  CODES_TSearchParamsCoverage : Array[TSearchParamsCoverage] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'dependent', 'group', 'identifier', 'issuer', 'plan', 'sequence', 'subplan', 'type');
{$ENDIF}
{$IFDEF FHIR_DATAELEMENT}
  CODES_TSearchParamsDataElement : Array[TSearchParamsDataElement] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'context', 'date', 'description', 'identifier', 'name', 'objectClass', 'objectClassProperty', 'publisher', 'status', 'stringency', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_DETECTEDISSUE}
  CODES_TSearchParamsDetectedIssue : Array[TSearchParamsDetectedIssue] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'author', 'category', 'date', 'identifier', 'implicated', 'patient');
{$ENDIF}
{$IFDEF FHIR_DEVICE}
  CODES_TSearchParamsDevice : Array[TSearchParamsDevice] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier', 'location', 'manufacturer', 'model', 'organization', 'patient', 'type', 'udi', 'url');
{$ENDIF}
{$IFDEF FHIR_DEVICECOMPONENT}
  CODES_TSearchParamsDeviceComponent : Array[TSearchParamsDeviceComponent] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'parent', 'source', 'type');
{$ENDIF}
{$IFDEF FHIR_DEVICEMETRIC}
  CODES_TSearchParamsDeviceMetric : Array[TSearchParamsDeviceMetric] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'category', 'identifier', 'parent', 'source', 'type');
{$ENDIF}
{$IFDEF FHIR_DEVICEUSEREQUEST}
  CODES_TSearchParamsDeviceUseRequest : Array[TSearchParamsDeviceUseRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'device', 'patient', 'subject');
{$ENDIF}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  CODES_TSearchParamsDeviceUseStatement : Array[TSearchParamsDeviceUseStatement] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'device', 'patient', 'subject');
{$ENDIF}
{$IFDEF FHIR_DIAGNOSTICORDER}
  CODES_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'actor', 'bodysite', 'code', 'encounter', 'event-date', 'event-status', 'event-status-date', 'identifier', 'item-date', 'item-past-status', 'item-status', 'item-status-date', 'orderer', 'patient', 'specimen', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  CODES_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'category', 'code', 'date', 'diagnosis', 'encounter', 'identifier', 'image', 'issued', 'patient', 'performer', 'request', 'result', 'specimen', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  CODES_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'author', 'content-ref', 'created', 'description', 'identifier', 'patient', 'recipient', 'related-id', 'related-ref', 'source', 'status', 'subject', 'type');
{$ENDIF}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  CODES_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'authenticator', 'author', 'class', 'created', 'custodian', 'description', 'encounter', 'event', 'facility', 'format', 'identifier', 'indexed', 'language', 'location', 'patient', 'period', 'related-id', 'related-ref', 'relatesto', 'relation', 'relationship', 'securitylabel', 'setting', 'status', 'subject', 'type');
{$ENDIF}
{$IFDEF FHIR_ELIGIBILITYREQUEST}
  CODES_TSearchParamsEligibilityRequest : Array[TSearchParamsEligibilityRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier');
{$ENDIF}
{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  CODES_TSearchParamsEligibilityResponse : Array[TSearchParamsEligibilityResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier');
{$ENDIF}
{$IFDEF FHIR_ENCOUNTER}
  CODES_TSearchParamsEncounter : Array[TSearchParamsEncounter] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'appointment', 'condition', 'date', 'episodeofcare', 'identifier', 'incomingreferral', 'indication', 'length', 'location', 'location-period', 'part-of', 'participant', 'participant-type', 'patient', 'practitioner', 'procedure', 'reason', 'special-arrangement', 'status', 'type');
{$ENDIF}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  CODES_TSearchParamsEnrollmentRequest : Array[TSearchParamsEnrollmentRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier', 'patient', 'subject');
{$ENDIF}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  CODES_TSearchParamsEnrollmentResponse : Array[TSearchParamsEnrollmentResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier');
{$ENDIF}
{$IFDEF FHIR_EPISODEOFCARE}
  CODES_TSearchParamsEpisodeOfCare : Array[TSearchParamsEpisodeOfCare] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'care-manager', 'condition', 'date', 'identifier', 'incomingreferral', 'organization', 'patient', 'status', 'team-member', 'type');
{$ENDIF}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  CODES_TSearchParamsExplanationOfBenefit : Array[TSearchParamsExplanationOfBenefit] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier');
{$ENDIF}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  CODES_TSearchParamsFamilyMemberHistory : Array[TSearchParamsFamilyMemberHistory] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'condition', 'date', 'gender', 'identifier', 'patient', 'relationship');
{$ENDIF}
{$IFDEF FHIR_FLAG}
  CODES_TSearchParamsFlag : Array[TSearchParamsFlag] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'author', 'date', 'encounter', 'patient', 'subject');
{$ENDIF}
{$IFDEF FHIR_GOAL}
  CODES_TSearchParamsGoal : Array[TSearchParamsGoal] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'category', 'identifier', 'patient', 'status', 'subject', 'targetdate');
{$ENDIF}
{$IFDEF FHIR_GROUP}
  CODES_TSearchParamsGroup : Array[TSearchParamsGroup] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'actual', 'characteristic', 'characteristic-value', 'code', 'exclude', 'identifier', 'member', 'type', 'value');
{$ENDIF}
{$IFDEF FHIR_HEALTHCARESERVICE}
  CODES_TSearchParamsHealthcareService : Array[TSearchParamsHealthcareService] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'characteristic', 'identifier', 'location', 'name', 'organization', 'programname', 'servicecategory', 'servicetype');
{$ENDIF}
{$IFDEF FHIR_IMAGINGOBJECTSELECTION}
  CODES_TSearchParamsImagingObjectSelection : Array[TSearchParamsImagingObjectSelection] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'author', 'authoring-time', 'identifier', 'patient', 'selected-study', 'title');
{$ENDIF}
{$IFDEF FHIR_IMAGINGSTUDY}
  CODES_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'accession', 'bodysite', 'dicom-class', 'modality', 'order', 'patient', 'series', 'started', 'study', 'uid');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATION}
  CODES_TSearchParamsImmunization : Array[TSearchParamsImmunization] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'date', 'dose-sequence', 'identifier', 'location', 'lot-number', 'manufacturer', 'notgiven', 'patient', 'performer', 'reaction', 'reaction-date', 'reason', 'reason-not-given', 'requester', 'status', 'vaccine-code');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  CODES_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'date', 'dose-number', 'dose-sequence', 'identifier', 'information', 'patient', 'status', 'support', 'vaccine-type');
{$ENDIF}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  CODES_TSearchParamsImplementationGuide : Array[TSearchParamsImplementationGuide] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'context', 'date', 'dependency', 'description', 'experimental', 'name', 'publisher', 'status', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_LIST}
  CODES_TSearchParamsList : Array[TSearchParamsList] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'date', 'empty-reason', 'encounter', 'item', 'notes', 'patient', 'source', 'status', 'subject', 'title');
{$ENDIF}
{$IFDEF FHIR_LOCATION}
  CODES_TSearchParamsLocation : Array[TSearchParamsLocation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'identifier', 'name', 'near', 'near-distance', 'organization', 'partof', 'status', 'type');
{$ENDIF}
{$IFDEF FHIR_MEDIA}
  CODES_TSearchParamsMedia : Array[TSearchParamsMedia] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'created', 'identifier', 'operator', 'patient', 'subject', 'subtype', 'type', 'view');
{$ENDIF}
{$IFDEF FHIR_MEDICATION}
  CODES_TSearchParamsMedication : Array[TSearchParamsMedication] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'container', 'content', 'form', 'ingredient', 'manufacturer');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  CODES_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'device', 'effectivetime', 'encounter', 'identifier', 'medication', 'notgiven', 'patient', 'practitioner', 'prescription', 'status');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  CODES_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'destination', 'dispenser', 'identifier', 'medication', 'patient', 'prescription', 'receiver', 'responsibleparty', 'status', 'type', 'whenhandedover', 'whenprepared');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONORDER}
  CODES_TSearchParamsMedicationOrder : Array[TSearchParamsMedicationOrder] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'datewritten', 'encounter', 'identifier', 'medication', 'patient', 'prescriber', 'status');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  CODES_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'effectivedate', 'identifier', 'medication', 'patient', 'source', 'status');
{$ENDIF}
{$IFDEF FHIR_MESSAGEHEADER}
  CODES_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'author', 'code', 'data', 'destination', 'destination-uri', 'enterer', 'event', 'receiver', 'response-id', 'responsible', 'source', 'source-uri', 'target', 'timestamp');
{$ENDIF}
{$IFDEF FHIR_NAMINGSYSTEM}
  CODES_TSearchParamsNamingSystem : Array[TSearchParamsNamingSystem] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'contact', 'context', 'date', 'id-type', 'kind', 'name', 'period', 'publisher', 'replaced-by', 'responsible', 'status', 'telecom', 'type', 'value');
{$ENDIF}
{$IFDEF FHIR_NUTRITIONORDER}
  CODES_TSearchParamsNutritionOrder : Array[TSearchParamsNutritionOrder] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'additive', 'datetime', 'encounter', 'formula', 'identifier', 'oraldiet', 'patient', 'provider', 'status', 'supplement');
{$ENDIF}
{$IFDEF FHIR_OBSERVATION}
  CODES_TSearchParamsObservation : Array[TSearchParamsObservation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'category', 'code', 'code-value-[x]', 'component-code', 'component-code-value-[x]', 'component-data-absent-reason', 'component-value-concept', 'component-value-quantity', 'component-value-string', 'data-absent-reason', 'date', 'device', 'encounter', 'identifier', 'patient', 'performer', 'related', 'related-target', 'related-type', 'specimen', 'status', 'subject', 'value-concept', 'value-date', 'value-quantity', 'value-string');
{$ENDIF}
{$IFDEF FHIR_OPERATIONDEFINITION}
  CODES_TSearchParamsOperationDefinition : Array[TSearchParamsOperationDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'base', 'code', 'date', 'instance', 'kind', 'name', 'profile', 'publisher', 'status', 'system', 'type', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_OPERATIONOUTCOME}
  CODES_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_ORDER}
  CODES_TSearchParamsOrder : Array[TSearchParamsOrder] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'date', 'detail', 'identifier', 'patient', 'source', 'subject', 'target', 'when', 'when_code');
{$ENDIF}
{$IFDEF FHIR_ORDERRESPONSE}
  CODES_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'date', 'fulfillment', 'identifier', 'request', 'who');
{$ENDIF}
{$IFDEF FHIR_ORGANIZATION}
  CODES_TSearchParamsOrganization : Array[TSearchParamsOrganization] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'active', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'identifier', 'name', 'partof', 'phonetic', 'type');
{$ENDIF}
{$IFDEF FHIR_PATIENT}
  CODES_TSearchParamsPatient : Array[TSearchParamsPatient] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'active', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'animal-breed', 'animal-species', 'birthdate', 'careprovider', 'deathdate', 'deceased', 'email', 'ethnicity', 'family', 'gender', 'given', 'identifier', 'language', 'link', 'name', 'organization', 'phone', 'phonetic', 'race', 'telecom');
{$ENDIF}
{$IFDEF FHIR_PAYMENTNOTICE}
  CODES_TSearchParamsPaymentNotice : Array[TSearchParamsPaymentNotice] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier');
{$ENDIF}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  CODES_TSearchParamsPaymentReconciliation : Array[TSearchParamsPaymentReconciliation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier');
{$ENDIF}
{$IFDEF FHIR_PERSON}
  CODES_TSearchParamsPerson : Array[TSearchParamsPerson] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'birthdate', 'email', 'gender', 'identifier', 'link', 'name', 'organization', 'patient', 'phone', 'phonetic', 'practitioner', 'relatedperson', 'telecom');
{$ENDIF}
{$IFDEF FHIR_PRACTITIONER}
  CODES_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'communication', 'email', 'family', 'gender', 'given', 'identifier', 'location', 'name', 'organization', 'phone', 'phonetic', 'role', 'specialty', 'telecom');
{$ENDIF}
{$IFDEF FHIR_PROCEDURE}
  CODES_TSearchParamsProcedure : Array[TSearchParamsProcedure] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'date', 'encounter', 'identifier', 'location', 'patient', 'performer', 'subject');
{$ENDIF}
{$IFDEF FHIR_PROCEDUREREQUEST}
  CODES_TSearchParamsProcedureRequest : Array[TSearchParamsProcedureRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'encounter', 'identifier', 'orderer', 'patient', 'performer', 'subject');
{$ENDIF}
{$IFDEF FHIR_PROCESSREQUEST}
  CODES_TSearchParamsProcessRequest : Array[TSearchParamsProcessRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'action', 'identifier', 'organization', 'provider');
{$ENDIF}
{$IFDEF FHIR_PROCESSRESPONSE}
  CODES_TSearchParamsProcessResponse : Array[TSearchParamsProcessResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier', 'organization', 'request', 'requestorganization', 'requestprovider');
{$ENDIF}
{$IFDEF FHIR_PROVENANCE}
  CODES_TSearchParamsProvenance : Array[TSearchParamsProvenance] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'agent', 'end', 'entity', 'entitytype', 'location', 'patient', 'sigtype', 'start', 'target', 'userid');
{$ENDIF}
{$IFDEF FHIR_QUESTIONNAIRE}
  CODES_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'date', 'identifier', 'publisher', 'status', 'title', 'version');
{$ENDIF}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  CODES_TSearchParamsQuestionnaireResponse : Array[TSearchParamsQuestionnaireResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'author', 'authored', 'encounter', 'patient', 'questionnaire', 'source', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_REFERRALREQUEST}
  CODES_TSearchParamsReferralRequest : Array[TSearchParamsReferralRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'date', 'patient', 'priority', 'recipient', 'requester', 'specialty', 'status', 'type');
{$ENDIF}
{$IFDEF FHIR_RELATEDPERSON}
  CODES_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'birthdate', 'email', 'gender', 'identifier', 'name', 'patient', 'phone', 'phonetic', 'telecom');
{$ENDIF}
{$IFDEF FHIR_RISKASSESSMENT}
  CODES_TSearchParamsRiskAssessment : Array[TSearchParamsRiskAssessment] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'condition', 'date', 'encounter', 'identifier', 'method', 'patient', 'performer', 'subject');
{$ENDIF}
{$IFDEF FHIR_SCHEDULE}
  CODES_TSearchParamsSchedule : Array[TSearchParamsSchedule] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'actor', 'date', 'identifier', 'type');
{$ENDIF}
{$IFDEF FHIR_SEARCHPARAMETER}
  CODES_TSearchParamsSearchParameter : Array[TSearchParamsSearchParameter] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'base', 'code', 'description', 'name', 'target', 'type', 'url');
{$ENDIF}
{$IFDEF FHIR_SLOT}
  CODES_TSearchParamsSlot : Array[TSearchParamsSlot] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'fb-type', 'identifier', 'schedule', 'slot-type', 'start');
{$ENDIF}
{$IFDEF FHIR_SPECIMEN}
  CODES_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'accession', 'bodysite', 'collected', 'collector', 'container', 'container-id', 'identifier', 'parent', 'patient', 'subject', 'type');
{$ENDIF}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  CODES_TSearchParamsStructureDefinition : Array[TSearchParamsStructureDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'abstract', 'base', 'base-path', 'code', 'context', 'context-type', 'date', 'description', 'display', 'experimental', 'ext-context', 'identifier', 'kind', 'name', 'path', 'publisher', 'status', 'type', 'url', 'valueset', 'version');
{$ENDIF}
{$IFDEF FHIR_SUBSCRIPTION}
  CODES_TSearchParamsSubscription : Array[TSearchParamsSubscription] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'contact', 'criteria', 'payload', 'status', 'tag', 'type', 'url');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCE}
  CODES_TSearchParamsSubstance : Array[TSearchParamsSubstance] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'category', 'code', 'container-identifier', 'expiry', 'identifier', 'quantity', 'substance');
{$ENDIF}
{$IFDEF FHIR_SUPPLYDELIVERY}
  CODES_TSearchParamsSupplyDelivery : Array[TSearchParamsSupplyDelivery] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'identifier', 'patient', 'receiver', 'status', 'supplier');
{$ENDIF}
{$IFDEF FHIR_SUPPLYREQUEST}
  CODES_TSearchParamsSupplyRequest : Array[TSearchParamsSupplyRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'date', 'identifier', 'kind', 'patient', 'source', 'status', 'supplier');
{$ENDIF}
{$IFDEF FHIR_TESTSCRIPT}
  CODES_TSearchParamsTestScript : Array[TSearchParamsTestScript] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'description', 'identifier', 'name', 'testscript-capability', 'testscript-setup-capability', 'testscript-test-capability', 'url');
{$ENDIF}
{$IFDEF FHIR_VALUESET}
  CODES_TSearchParamsValueSet : Array[TSearchParamsValueSet] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'code', 'context', 'date', 'description', 'expansion', 'identifier', 'name', 'publisher', 'reference', 'status', 'system', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  CODES_TSearchParamsVisionPrescription : Array[TSearchParamsVisionPrescription] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'datewritten', 'encounter', 'identifier', 'patient', 'prescriber');
{$ENDIF}
  FHIR_GENERATED_VERSION = '1.0.2';
  FHIR_GENERATED_VERSION_BASE = '1.0';

  FHIR_GENERATED_PUBLICATION = '2';

  FHIR_GENERATED_DATE = '2015-10-24T07:41:03+11:00';



implementation

end.

