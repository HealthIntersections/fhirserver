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

{$IFNDEF FHIR4}
This is the dstu4 version of the FHIR code
{$ENDIF}


interface

// FHIR v3.2.0 generated 2017-12-20T12:10:38+11:00

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
    spAccount__source, {@enum.value "_source" spAccount__source Identifies where the resource comes from }
    spAccount__tag, {@enum.value "_tag" spAccount__tag Tags applied to this resource }
    spAccount__text, {@enum.value "_text" spAccount__text Search on the narrative of the resource }
    spAccount_Identifier, {@enum.value "identifier" spAccount_Identifier Account number }
    spAccount_Name, {@enum.value "name" spAccount_Name Human-readable label }
    spAccount_Owner, {@enum.value "owner" spAccount_Owner Who is responsible? }
    spAccount_Patient, {@enum.value "patient" spAccount_Patient What is account tied to? }
    spAccount_Period, {@enum.value "period" spAccount_Period Transaction window }
    spAccount_Status, {@enum.value "status" spAccount_Status active | inactive | entered-in-error }
    spAccount_Subject, {@enum.value "subject" spAccount_Subject What is account tied to? }
    spAccount_Type); {@enum.value "type" spAccount_Type E.g. patient, expense, depreciation }
{$ENDIF}

{$IFDEF FHIR_ACTIVITYDEFINITION}
  {@Enum TSearchParamsActivityDefinition
    Search Parameters for ActivityDefinition
  }
  TSearchParamsActivityDefinition = (
    spActivityDefinition__content, {@enum.value "_content" spActivityDefinition__content Search on the entire content of the resource }
    spActivityDefinition__id, {@enum.value "_id" spActivityDefinition__id Logical id of this artifact }
    spActivityDefinition__lastUpdated, {@enum.value "_lastUpdated" spActivityDefinition__lastUpdated When the resource version last changed }
    spActivityDefinition__profile, {@enum.value "_profile" spActivityDefinition__profile Profiles this resource claims to conform to }
    spActivityDefinition__query, {@enum.value "_query" spActivityDefinition__query A custom search profile that describes a specific defined query operation }
    spActivityDefinition__security, {@enum.value "_security" spActivityDefinition__security Security Labels applied to this resource }
    spActivityDefinition__source, {@enum.value "_source" spActivityDefinition__source Identifies where the resource comes from }
    spActivityDefinition__tag, {@enum.value "_tag" spActivityDefinition__tag Tags applied to this resource }
    spActivityDefinition__text, {@enum.value "_text" spActivityDefinition__text Search on the narrative of the resource }
    spActivityDefinition_Composedof, {@enum.value "composed-of" spActivityDefinition_Composedof What resource is being referenced }
    spActivityDefinition_Contexttype, {@enum.value "context-type" spActivityDefinition_Contexttype A type of use context assigned to the activity definition }
    spActivityDefinition_Date, {@enum.value "date" spActivityDefinition_Date The activity definition publication date }
    spActivityDefinition_Dependson, {@enum.value "depends-on" spActivityDefinition_Dependson What resource is being referenced }
    spActivityDefinition_Derivedfrom, {@enum.value "derived-from" spActivityDefinition_Derivedfrom What resource is being referenced }
    spActivityDefinition_Description, {@enum.value "description" spActivityDefinition_Description The description of the activity definition }
    spActivityDefinition_Effective, {@enum.value "effective" spActivityDefinition_Effective The time during which the activity definition is intended to be in use }
    spActivityDefinition_Identifier, {@enum.value "identifier" spActivityDefinition_Identifier External identifier for the activity definition }
    spActivityDefinition_Jurisdiction, {@enum.value "jurisdiction" spActivityDefinition_Jurisdiction Intended jurisdiction for the activity definition }
    spActivityDefinition_Name, {@enum.value "name" spActivityDefinition_Name Computationally friendly name of the activity definition }
    spActivityDefinition_Predecessor, {@enum.value "predecessor" spActivityDefinition_Predecessor What resource is being referenced }
    spActivityDefinition_Publisher, {@enum.value "publisher" spActivityDefinition_Publisher Name of the publisher of the activity definition }
    spActivityDefinition_Status, {@enum.value "status" spActivityDefinition_Status The current status of the activity definition }
    spActivityDefinition_Successor, {@enum.value "successor" spActivityDefinition_Successor What resource is being referenced }
    spActivityDefinition_Title, {@enum.value "title" spActivityDefinition_Title The human-friendly name of the activity definition }
    spActivityDefinition_Topic, {@enum.value "topic" spActivityDefinition_Topic Topics associated with the module }
    spActivityDefinition_Url, {@enum.value "url" spActivityDefinition_Url The uri that identifies the activity definition }
    spActivityDefinition_Version); {@enum.value "version" spActivityDefinition_Version The business version of the activity definition }
{$ENDIF}

{$IFDEF FHIR_ADVERSEEVENT}
  {@Enum TSearchParamsAdverseEvent
    Search Parameters for AdverseEvent
  }
  TSearchParamsAdverseEvent = (
    spAdverseEvent__content, {@enum.value "_content" spAdverseEvent__content Search on the entire content of the resource }
    spAdverseEvent__id, {@enum.value "_id" spAdverseEvent__id Logical id of this artifact }
    spAdverseEvent__lastUpdated, {@enum.value "_lastUpdated" spAdverseEvent__lastUpdated When the resource version last changed }
    spAdverseEvent__profile, {@enum.value "_profile" spAdverseEvent__profile Profiles this resource claims to conform to }
    spAdverseEvent__query, {@enum.value "_query" spAdverseEvent__query A custom search profile that describes a specific defined query operation }
    spAdverseEvent__security, {@enum.value "_security" spAdverseEvent__security Security Labels applied to this resource }
    spAdverseEvent__source, {@enum.value "_source" spAdverseEvent__source Identifies where the resource comes from }
    spAdverseEvent__tag, {@enum.value "_tag" spAdverseEvent__tag Tags applied to this resource }
    spAdverseEvent__text, {@enum.value "_text" spAdverseEvent__text Search on the narrative of the resource }
    spAdverseEvent_Actuality, {@enum.value "actuality" spAdverseEvent_Actuality actual | potential }
    spAdverseEvent_Category, {@enum.value "category" spAdverseEvent_Category ProductProblem | ProductQuality | ProductUseError | WrongDose | IncorrectPrescribingInformation | WrongTechnique | WrongRouteOfAdministration | WrongRate | WrongDuration | WrongTime | ExpiredDrug | MedicalDeviceUseError | ProblemDifferentManufacturer | UnsafePhysicalEnvironment }
    spAdverseEvent_Date, {@enum.value "date" spAdverseEvent_Date When the event occurred }
    spAdverseEvent_Event, {@enum.value "event" spAdverseEvent_Event Type of the event itself in relation to the subject }
    spAdverseEvent_Location, {@enum.value "location" spAdverseEvent_Location Location where adverse event occurred }
    spAdverseEvent_Recorder, {@enum.value "recorder" spAdverseEvent_Recorder Who recorded the adverse event }
    spAdverseEvent_Resultingcondition, {@enum.value "resultingcondition" spAdverseEvent_Resultingcondition Effect on the subject due to this event }
    spAdverseEvent_Seriousness, {@enum.value "seriousness" spAdverseEvent_Seriousness Seriousness of the event }
    spAdverseEvent_Severity, {@enum.value "severity" spAdverseEvent_Severity Mild | Moderate | Severe }
    spAdverseEvent_Study, {@enum.value "study" spAdverseEvent_Study AdverseEvent.study }
    spAdverseEvent_Subject, {@enum.value "subject" spAdverseEvent_Subject Subject impacted by event }
    spAdverseEvent_Substance); {@enum.value "substance" spAdverseEvent_Substance Refers to the specific entity that caused the adverse event }
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
    spAllergyIntolerance__source, {@enum.value "_source" spAllergyIntolerance__source Identifies where the resource comes from }
    spAllergyIntolerance__tag, {@enum.value "_tag" spAllergyIntolerance__tag Tags applied to this resource }
    spAllergyIntolerance__text, {@enum.value "_text" spAllergyIntolerance__text Search on the narrative of the resource }
    spAllergyIntolerance_Asserter, {@enum.value "asserter" spAllergyIntolerance_Asserter Source of the information about the allergy }
    spAllergyIntolerance_Category, {@enum.value "category" spAllergyIntolerance_Category food | medication | environment | biologic }
    spAllergyIntolerance_Clinicalstatus, {@enum.value "clinical-status" spAllergyIntolerance_Clinicalstatus active | inactive | resolved }
    spAllergyIntolerance_Code, {@enum.value "code" spAllergyIntolerance_Code Code that identifies the allergy or intolerance }
    spAllergyIntolerance_Criticality, {@enum.value "criticality" spAllergyIntolerance_Criticality low | high | unable-to-assess }
    spAllergyIntolerance_Date, {@enum.value "date" spAllergyIntolerance_Date Date record was believed accurate }
    spAllergyIntolerance_Identifier, {@enum.value "identifier" spAllergyIntolerance_Identifier External ids for this item }
    spAllergyIntolerance_Lastdate, {@enum.value "last-date" spAllergyIntolerance_Lastdate Date(/time) of last known occurrence of a reaction }
    spAllergyIntolerance_Manifestation, {@enum.value "manifestation" spAllergyIntolerance_Manifestation Clinical symptoms/signs associated with the Event }
    spAllergyIntolerance_Onset, {@enum.value "onset" spAllergyIntolerance_Onset Date(/time) when manifestations showed }
    spAllergyIntolerance_Patient, {@enum.value "patient" spAllergyIntolerance_Patient Who the sensitivity is for }
    spAllergyIntolerance_Recorder, {@enum.value "recorder" spAllergyIntolerance_Recorder Who recorded the sensitivity }
    spAllergyIntolerance_Route, {@enum.value "route" spAllergyIntolerance_Route How the subject was exposed to the substance }
    spAllergyIntolerance_Severity, {@enum.value "severity" spAllergyIntolerance_Severity mild | moderate | severe (of event as a whole) }
    spAllergyIntolerance_Type, {@enum.value "type" spAllergyIntolerance_Type allergy | intolerance - Underlying mechanism (if known) }
    spAllergyIntolerance_Verificationstatus); {@enum.value "verification-status" spAllergyIntolerance_Verificationstatus unconfirmed | confirmed | refuted | entered-in-error }
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
    spAppointment__source, {@enum.value "_source" spAppointment__source Identifies where the resource comes from }
    spAppointment__tag, {@enum.value "_tag" spAppointment__tag Tags applied to this resource }
    spAppointment__text, {@enum.value "_text" spAppointment__text Search on the narrative of the resource }
    spAppointment_Actor, {@enum.value "actor" spAppointment_Actor Any one of the individuals participating in the appointment }
    spAppointment_Appointmenttype, {@enum.value "appointment-type" spAppointment_Appointmenttype The style of appointment or patient that has been booked in the slot (not service type) }
    spAppointment_Date, {@enum.value "date" spAppointment_Date Appointment date/time. }
    spAppointment_Identifier, {@enum.value "identifier" spAppointment_Identifier An Identifier of the Appointment }
    spAppointment_Incomingreferral, {@enum.value "incomingreferral" spAppointment_Incomingreferral The ServiceRequest provided as information to allocate to the Encounter }
    spAppointment_Location, {@enum.value "location" spAppointment_Location This location is listed in the participants of the appointment }
    spAppointment_Partstatus, {@enum.value "part-status" spAppointment_Partstatus The Participation status of the subject, or other participant on the appointment. Can be used to locate participants that have not responded to meeting requests. }
    spAppointment_Patient, {@enum.value "patient" spAppointment_Patient One of the individuals of the appointment is this patient }
    spAppointment_Practitioner, {@enum.value "practitioner" spAppointment_Practitioner One of the individuals of the appointment is this practitioner }
    spAppointment_Servicetype, {@enum.value "service-type" spAppointment_Servicetype The specific service that is to be performed during this appointment }
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
    spAppointmentResponse__source, {@enum.value "_source" spAppointmentResponse__source Identifies where the resource comes from }
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
    spAuditEvent__source, {@enum.value "_source" spAuditEvent__source Identifies where the resource comes from }
    spAuditEvent__tag, {@enum.value "_tag" spAuditEvent__tag Tags applied to this resource }
    spAuditEvent__text, {@enum.value "_text" spAuditEvent__text Search on the narrative of the resource }
    spAuditEvent_Action, {@enum.value "action" spAuditEvent_Action Type of action performed during the event }
    spAuditEvent_Address, {@enum.value "address" spAuditEvent_Address Identifier for the network access point of the user device }
    spAuditEvent_Agent, {@enum.value "agent" spAuditEvent_Agent Direct reference to resource }
    spAuditEvent_Agentname, {@enum.value "agent-name" spAuditEvent_Agentname Human-meaningful name for the agent }
    spAuditEvent_Agentrole, {@enum.value "agent-role" spAuditEvent_Agentrole Agent role in the event }
    spAuditEvent_Altid, {@enum.value "altid" spAuditEvent_Altid Alternative User id e.g. authentication }
    spAuditEvent_Date, {@enum.value "date" spAuditEvent_Date Time when the event occurred on source }
    spAuditEvent_Entity, {@enum.value "entity" spAuditEvent_Entity Specific instance of resource }
    spAuditEvent_Entityid, {@enum.value "entity-id" spAuditEvent_Entityid Specific instance of object }
    spAuditEvent_Entityname, {@enum.value "entity-name" spAuditEvent_Entityname Descriptor for entity }
    spAuditEvent_Entityrole, {@enum.value "entity-role" spAuditEvent_Entityrole What role the entity played }
    spAuditEvent_Entitytype, {@enum.value "entity-type" spAuditEvent_Entitytype Type of entity involved }
    spAuditEvent_Outcome, {@enum.value "outcome" spAuditEvent_Outcome Whether the event succeeded or failed }
    spAuditEvent_Patient, {@enum.value "patient" spAuditEvent_Patient Direct reference to resource }
    spAuditEvent_Policy, {@enum.value "policy" spAuditEvent_Policy Policy that authorized event }
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
    spBasic__source, {@enum.value "_source" spBasic__source Identifies where the resource comes from }
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
    spBinary__source, {@enum.value "_source" spBinary__source Identifies where the resource comes from }
    spBinary__tag, {@enum.value "_tag" spBinary__tag Tags applied to this resource }
    spBinary_Contenttype); {@enum.value "contenttype" spBinary_Contenttype MimeType of the binary content }
{$ENDIF}

{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  {@Enum TSearchParamsBiologicallyDerivedProduct
    Search Parameters for BiologicallyDerivedProduct
  }
  TSearchParamsBiologicallyDerivedProduct = (
    spBiologicallyDerivedProduct__content, {@enum.value "_content" spBiologicallyDerivedProduct__content Search on the entire content of the resource }
    spBiologicallyDerivedProduct__id, {@enum.value "_id" spBiologicallyDerivedProduct__id Logical id of this artifact }
    spBiologicallyDerivedProduct__lastUpdated, {@enum.value "_lastUpdated" spBiologicallyDerivedProduct__lastUpdated When the resource version last changed }
    spBiologicallyDerivedProduct__profile, {@enum.value "_profile" spBiologicallyDerivedProduct__profile Profiles this resource claims to conform to }
    spBiologicallyDerivedProduct__query, {@enum.value "_query" spBiologicallyDerivedProduct__query A custom search profile that describes a specific defined query operation }
    spBiologicallyDerivedProduct__security, {@enum.value "_security" spBiologicallyDerivedProduct__security Security Labels applied to this resource }
    spBiologicallyDerivedProduct__source, {@enum.value "_source" spBiologicallyDerivedProduct__source Identifies where the resource comes from }
    spBiologicallyDerivedProduct__tag, {@enum.value "_tag" spBiologicallyDerivedProduct__tag Tags applied to this resource }
    spBiologicallyDerivedProduct__text); {@enum.value "_text" spBiologicallyDerivedProduct__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_BODYSTRUCTURE}
  {@Enum TSearchParamsBodyStructure
    Search Parameters for BodyStructure
  }
  TSearchParamsBodyStructure = (
    spBodyStructure__content, {@enum.value "_content" spBodyStructure__content Search on the entire content of the resource }
    spBodyStructure__id, {@enum.value "_id" spBodyStructure__id Logical id of this artifact }
    spBodyStructure__lastUpdated, {@enum.value "_lastUpdated" spBodyStructure__lastUpdated When the resource version last changed }
    spBodyStructure__profile, {@enum.value "_profile" spBodyStructure__profile Profiles this resource claims to conform to }
    spBodyStructure__query, {@enum.value "_query" spBodyStructure__query A custom search profile that describes a specific defined query operation }
    spBodyStructure__security, {@enum.value "_security" spBodyStructure__security Security Labels applied to this resource }
    spBodyStructure__source, {@enum.value "_source" spBodyStructure__source Identifies where the resource comes from }
    spBodyStructure__tag, {@enum.value "_tag" spBodyStructure__tag Tags applied to this resource }
    spBodyStructure__text, {@enum.value "_text" spBodyStructure__text Search on the narrative of the resource }
    spBodyStructure_Identifier, {@enum.value "identifier" spBodyStructure_Identifier Bodystructure identifier }
    spBodyStructure_Location, {@enum.value "location" spBodyStructure_Location Body site }
    spBodyStructure_Morphology, {@enum.value "morphology" spBodyStructure_Morphology Kind of Structure }
    spBodyStructure_Patient); {@enum.value "patient" spBodyStructure_Patient Who this is about }
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
    spBundle__source, {@enum.value "_source" spBundle__source Identifies where the resource comes from }
    spBundle__tag, {@enum.value "_tag" spBundle__tag Tags applied to this resource }
    spBundle_Composition, {@enum.value "composition" spBundle_Composition The first resource in the bundle, if the bundle type is "document" - this is a composition, and this parameter provides access to searches its contents }
    spBundle_Identifier, {@enum.value "identifier" spBundle_Identifier Persistent identifier for the bundle }
    spBundle_Message, {@enum.value "message" spBundle_Message The first resource in the bundle, if the bundle type is "message" - this is a message header, and this parameter provides access to search its contents }
    spBundle_Timestamp, {@enum.value "timestamp" spBundle_Timestamp When the bundle was assembled }
    spBundle_Type); {@enum.value "type" spBundle_Type document | message | transaction | transaction-response | batch | batch-response | history | searchset | collection }
{$ENDIF}

{$IFDEF FHIR_CAPABILITYSTATEMENT}
  {@Enum TSearchParamsCapabilityStatement
    Search Parameters for CapabilityStatement
  }
  TSearchParamsCapabilityStatement = (
    spCapabilityStatement__content, {@enum.value "_content" spCapabilityStatement__content Search on the entire content of the resource }
    spCapabilityStatement__id, {@enum.value "_id" spCapabilityStatement__id Logical id of this artifact }
    spCapabilityStatement__lastUpdated, {@enum.value "_lastUpdated" spCapabilityStatement__lastUpdated When the resource version last changed }
    spCapabilityStatement__profile, {@enum.value "_profile" spCapabilityStatement__profile Profiles this resource claims to conform to }
    spCapabilityStatement__query, {@enum.value "_query" spCapabilityStatement__query A custom search profile that describes a specific defined query operation }
    spCapabilityStatement__security, {@enum.value "_security" spCapabilityStatement__security Security Labels applied to this resource }
    spCapabilityStatement__source, {@enum.value "_source" spCapabilityStatement__source Identifies where the resource comes from }
    spCapabilityStatement__tag, {@enum.value "_tag" spCapabilityStatement__tag Tags applied to this resource }
    spCapabilityStatement__text, {@enum.value "_text" spCapabilityStatement__text Search on the narrative of the resource }
    spCapabilityStatement_Date, {@enum.value "date" spCapabilityStatement_Date The capability statement publication date }
    spCapabilityStatement_Description, {@enum.value "description" spCapabilityStatement_Description The description of the capability statement }
    spCapabilityStatement_Fhirversion, {@enum.value "fhirversion" spCapabilityStatement_Fhirversion The version of FHIR }
    spCapabilityStatement_Format, {@enum.value "format" spCapabilityStatement_Format formats supported (xml | json | ttl | mime type) }
    spCapabilityStatement_Guide, {@enum.value "guide" spCapabilityStatement_Guide Implementation guides supported }
    spCapabilityStatement_Jurisdiction, {@enum.value "jurisdiction" spCapabilityStatement_Jurisdiction Intended jurisdiction for the capability statement }
    spCapabilityStatement_Mode, {@enum.value "mode" spCapabilityStatement_Mode Mode - restful (server/client) or messaging (sender/receiver) }
    spCapabilityStatement_Name, {@enum.value "name" spCapabilityStatement_Name Computationally friendly name of the capability statement }
    spCapabilityStatement_Publisher, {@enum.value "publisher" spCapabilityStatement_Publisher Name of the publisher of the capability statement }
    spCapabilityStatement_Resource, {@enum.value "resource" spCapabilityStatement_Resource Name of a resource mentioned in a capability statement }
    spCapabilityStatement_Resourceprofile, {@enum.value "resource-profile" spCapabilityStatement_Resourceprofile A profile id invoked in a capability statement }
    spCapabilityStatement_Securityservice, {@enum.value "security-service" spCapabilityStatement_Securityservice OAuth | SMART-on-FHIR | NTLM | Basic | Kerberos | Certificates }
    spCapabilityStatement_Software, {@enum.value "software" spCapabilityStatement_Software Part of a the name of a software application }
    spCapabilityStatement_Status, {@enum.value "status" spCapabilityStatement_Status The current status of the capability statement }
    spCapabilityStatement_Supportedprofile, {@enum.value "supported-profile" spCapabilityStatement_Supportedprofile Profiles for use cases supported }
    spCapabilityStatement_Title, {@enum.value "title" spCapabilityStatement_Title The human-friendly name of the capability statement }
    spCapabilityStatement_Url, {@enum.value "url" spCapabilityStatement_Url The uri that identifies the capability statement }
    spCapabilityStatement_Version); {@enum.value "version" spCapabilityStatement_Version The business version of the capability statement }
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
    spCarePlan__source, {@enum.value "_source" spCarePlan__source Identifies where the resource comes from }
    spCarePlan__tag, {@enum.value "_tag" spCarePlan__tag Tags applied to this resource }
    spCarePlan__text, {@enum.value "_text" spCarePlan__text Search on the narrative of the resource }
    spCarePlan_Activitycode, {@enum.value "activity-code" spCarePlan_Activitycode Detail type of activity }
    spCarePlan_Activitydate, {@enum.value "activity-date" spCarePlan_Activitydate Specified date occurs within period specified by CarePlan.activity.timingSchedule }
    spCarePlan_Activityreference, {@enum.value "activity-reference" spCarePlan_Activityreference Activity details defined in specific resource }
    spCarePlan_Basedon, {@enum.value "based-on" spCarePlan_Basedon Fulfills care plan }
    spCarePlan_Careteam, {@enum.value "care-team" spCarePlan_Careteam Who's involved in plan? }
    spCarePlan_Category, {@enum.value "category" spCarePlan_Category Type of plan }
    spCarePlan_Condition, {@enum.value "condition" spCarePlan_Condition Health issues this plan addresses }
    spCarePlan_Context, {@enum.value "context" spCarePlan_Context Created in context of }
    spCarePlan_Date, {@enum.value "date" spCarePlan_Date Time period plan covers }
    spCarePlan_Encounter, {@enum.value "encounter" spCarePlan_Encounter Created in context of }
    spCarePlan_Goal, {@enum.value "goal" spCarePlan_Goal Desired outcome of plan }
    spCarePlan_Identifier, {@enum.value "identifier" spCarePlan_Identifier External Ids for this plan }
    spCarePlan_Instantiates, {@enum.value "instantiates" spCarePlan_Instantiates Protocol or definition }
    spCarePlan_Intent, {@enum.value "intent" spCarePlan_Intent proposal | plan | order | option }
    spCarePlan_Partof, {@enum.value "part-of" spCarePlan_Partof Part of referenced CarePlan }
    spCarePlan_Patient, {@enum.value "patient" spCarePlan_Patient Who care plan is for }
    spCarePlan_Performer, {@enum.value "performer" spCarePlan_Performer Matches if the practitioner is listed as a performer in any of the "simple" activities.  (For performers of the detailed activities, chain through the activitydetail search parameter.) }
    spCarePlan_Replaces, {@enum.value "replaces" spCarePlan_Replaces CarePlan replaced by this CarePlan }
    spCarePlan_Status, {@enum.value "status" spCarePlan_Status draft | active | suspended | completed | entered-in-error | cancelled | unknown }
    spCarePlan_Subject); {@enum.value "subject" spCarePlan_Subject Who care plan is for }
{$ENDIF}

{$IFDEF FHIR_CARETEAM}
  {@Enum TSearchParamsCareTeam
    Search Parameters for CareTeam
  }
  TSearchParamsCareTeam = (
    spCareTeam__content, {@enum.value "_content" spCareTeam__content Search on the entire content of the resource }
    spCareTeam__id, {@enum.value "_id" spCareTeam__id Logical id of this artifact }
    spCareTeam__lastUpdated, {@enum.value "_lastUpdated" spCareTeam__lastUpdated When the resource version last changed }
    spCareTeam__profile, {@enum.value "_profile" spCareTeam__profile Profiles this resource claims to conform to }
    spCareTeam__query, {@enum.value "_query" spCareTeam__query A custom search profile that describes a specific defined query operation }
    spCareTeam__security, {@enum.value "_security" spCareTeam__security Security Labels applied to this resource }
    spCareTeam__source, {@enum.value "_source" spCareTeam__source Identifies where the resource comes from }
    spCareTeam__tag, {@enum.value "_tag" spCareTeam__tag Tags applied to this resource }
    spCareTeam__text, {@enum.value "_text" spCareTeam__text Search on the narrative of the resource }
    spCareTeam_Category, {@enum.value "category" spCareTeam_Category Type of team }
    spCareTeam_Context, {@enum.value "context" spCareTeam_Context Encounter or episode associated with CareTeam }
    spCareTeam_Date, {@enum.value "date" spCareTeam_Date Time period team covers }
    spCareTeam_Encounter, {@enum.value "encounter" spCareTeam_Encounter Encounter or episode associated with CareTeam }
    spCareTeam_Identifier, {@enum.value "identifier" spCareTeam_Identifier External Ids for this team }
    spCareTeam_Participant, {@enum.value "participant" spCareTeam_Participant Who is involved }
    spCareTeam_Patient, {@enum.value "patient" spCareTeam_Patient Who care team is for }
    spCareTeam_Status, {@enum.value "status" spCareTeam_Status proposed | active | suspended | inactive | entered-in-error }
    spCareTeam_Subject); {@enum.value "subject" spCareTeam_Subject Who care team is for }
{$ENDIF}

{$IFDEF FHIR_CHARGEITEM}
  {@Enum TSearchParamsChargeItem
    Search Parameters for ChargeItem
  }
  TSearchParamsChargeItem = (
    spChargeItem__content, {@enum.value "_content" spChargeItem__content Search on the entire content of the resource }
    spChargeItem__id, {@enum.value "_id" spChargeItem__id Logical id of this artifact }
    spChargeItem__lastUpdated, {@enum.value "_lastUpdated" spChargeItem__lastUpdated When the resource version last changed }
    spChargeItem__profile, {@enum.value "_profile" spChargeItem__profile Profiles this resource claims to conform to }
    spChargeItem__query, {@enum.value "_query" spChargeItem__query A custom search profile that describes a specific defined query operation }
    spChargeItem__security, {@enum.value "_security" spChargeItem__security Security Labels applied to this resource }
    spChargeItem__source, {@enum.value "_source" spChargeItem__source Identifies where the resource comes from }
    spChargeItem__tag, {@enum.value "_tag" spChargeItem__tag Tags applied to this resource }
    spChargeItem__text, {@enum.value "_text" spChargeItem__text Search on the narrative of the resource }
    spChargeItem_Account, {@enum.value "account" spChargeItem_Account Account to place this charge }
    spChargeItem_Code, {@enum.value "code" spChargeItem_Code A code that identifies the charge, like a billing code }
    spChargeItem_Context, {@enum.value "context" spChargeItem_Context Encounter / Episode associated with event }
    spChargeItem_Entereddate, {@enum.value "entered-date" spChargeItem_Entereddate Date the charge item was entered }
    spChargeItem_Enterer, {@enum.value "enterer" spChargeItem_Enterer Individual who was entering }
    spChargeItem_Factoroverride, {@enum.value "factor-override" spChargeItem_Factoroverride Factor overriding the associated rules }
    spChargeItem_Identifier, {@enum.value "identifier" spChargeItem_Identifier Business Identifier for item }
    spChargeItem_Occurrence, {@enum.value "occurrence" spChargeItem_Occurrence When the charged service was applied }
    spChargeItem_Participantactor, {@enum.value "participant-actor" spChargeItem_Participantactor Individual who was performing }
    spChargeItem_Participantrole, {@enum.value "participant-role" spChargeItem_Participantrole What type of performance was done }
    spChargeItem_Patient, {@enum.value "patient" spChargeItem_Patient Individual service was done for/to }
    spChargeItem_Performingorganization, {@enum.value "performing-organization" spChargeItem_Performingorganization Organization providing the charged sevice }
    spChargeItem_Priceoverride, {@enum.value "price-override" spChargeItem_Priceoverride Price overriding the associated rules }
    spChargeItem_Quantity, {@enum.value "quantity" spChargeItem_Quantity Quantity of which the charge item has been serviced }
    spChargeItem_Requestingorganization, {@enum.value "requesting-organization" spChargeItem_Requestingorganization Organization requesting the charged service }
    spChargeItem_Service, {@enum.value "service" spChargeItem_Service Which rendered service is being charged? }
    spChargeItem_Subject); {@enum.value "subject" spChargeItem_Subject Individual service was done for/to }
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
    spClaim__source, {@enum.value "_source" spClaim__source Identifies where the resource comes from }
    spClaim__tag, {@enum.value "_tag" spClaim__tag Tags applied to this resource }
    spClaim__text, {@enum.value "_text" spClaim__text Search on the narrative of the resource }
    spClaim_Careteam, {@enum.value "care-team" spClaim_Careteam Member of the CareTeam }
    spClaim_Created, {@enum.value "created" spClaim_Created The creation date for the Claim }
    spClaim_Encounter, {@enum.value "encounter" spClaim_Encounter Encounters associated with a billed line item }
    spClaim_Enterer, {@enum.value "enterer" spClaim_Enterer The party responsible for the entry of the Claim }
    spClaim_Facility, {@enum.value "facility" spClaim_Facility Facility responsible for the goods and services }
    spClaim_Identifier, {@enum.value "identifier" spClaim_Identifier The primary identifier of the financial resource }
    spClaim_Insurer, {@enum.value "insurer" spClaim_Insurer The target payor/insurer for the Claim }
    spClaim_Organization, {@enum.value "organization" spClaim_Organization The reference to the providing organization }
    spClaim_Patient, {@enum.value "patient" spClaim_Patient Patient receiving the services }
    spClaim_Payee, {@enum.value "payee" spClaim_Payee The party receiving any payment for the Claim }
    spClaim_Priority, {@enum.value "priority" spClaim_Priority Processing priority requested }
    spClaim_Provider, {@enum.value "provider" spClaim_Provider Provider responsible for the Claim }
    spClaim_Status, {@enum.value "status" spClaim_Status The status of the Claim instance. }
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
    spClaimResponse__source, {@enum.value "_source" spClaimResponse__source Identifies where the resource comes from }
    spClaimResponse__tag, {@enum.value "_tag" spClaimResponse__tag Tags applied to this resource }
    spClaimResponse__text, {@enum.value "_text" spClaimResponse__text Search on the narrative of the resource }
    spClaimResponse_Created, {@enum.value "created" spClaimResponse_Created The creation date }
    spClaimResponse_Disposition, {@enum.value "disposition" spClaimResponse_Disposition The contents of the disposition message }
    spClaimResponse_Identifier, {@enum.value "identifier" spClaimResponse_Identifier The identity of the claimresponse }
    spClaimResponse_Insurer, {@enum.value "insurer" spClaimResponse_Insurer The organization who generated this resource }
    spClaimResponse_Outcome, {@enum.value "outcome" spClaimResponse_Outcome The processing outcome }
    spClaimResponse_Patient, {@enum.value "patient" spClaimResponse_Patient The subject of care. }
    spClaimResponse_Paymentdate, {@enum.value "payment-date" spClaimResponse_Paymentdate The expected paymentDate }
    spClaimResponse_Request, {@enum.value "request" spClaimResponse_Request The claim reference }
    spClaimResponse_Requestprovider, {@enum.value "request-provider" spClaimResponse_Requestprovider The Provider of the claim }
    spClaimResponse_Status); {@enum.value "status" spClaimResponse_Status The status of the claim response }
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
    spClinicalImpression__source, {@enum.value "_source" spClinicalImpression__source Identifies where the resource comes from }
    spClinicalImpression__tag, {@enum.value "_tag" spClinicalImpression__tag Tags applied to this resource }
    spClinicalImpression__text, {@enum.value "_text" spClinicalImpression__text Search on the narrative of the resource }
    spClinicalImpression_Action, {@enum.value "action" spClinicalImpression_Action Action taken as part of assessment procedure }
    spClinicalImpression_Assessor, {@enum.value "assessor" spClinicalImpression_Assessor The clinician performing the assessment }
    spClinicalImpression_Context, {@enum.value "context" spClinicalImpression_Context Encounter or Episode created from }
    spClinicalImpression_Date, {@enum.value "date" spClinicalImpression_Date When the assessment was documented }
    spClinicalImpression_Findingcode, {@enum.value "finding-code" spClinicalImpression_Findingcode What was found }
    spClinicalImpression_Findingref, {@enum.value "finding-ref" spClinicalImpression_Findingref What was found }
    spClinicalImpression_Identifier, {@enum.value "identifier" spClinicalImpression_Identifier Business identifier }
    spClinicalImpression_Investigation, {@enum.value "investigation" spClinicalImpression_Investigation Record of a specific investigation }
    spClinicalImpression_Patient, {@enum.value "patient" spClinicalImpression_Patient Patient or group assessed }
    spClinicalImpression_Previous, {@enum.value "previous" spClinicalImpression_Previous Reference to last assessment }
    spClinicalImpression_Problem, {@enum.value "problem" spClinicalImpression_Problem Relevant impressions of patient state }
    spClinicalImpression_Status, {@enum.value "status" spClinicalImpression_Status draft | completed | entered-in-error }
    spClinicalImpression_Subject); {@enum.value "subject" spClinicalImpression_Subject Patient or group assessed }
{$ENDIF}

{$IFDEF FHIR_CODESYSTEM}
  {@Enum TSearchParamsCodeSystem
    Search Parameters for CodeSystem
  }
  TSearchParamsCodeSystem = (
    spCodeSystem__content, {@enum.value "_content" spCodeSystem__content Search on the entire content of the resource }
    spCodeSystem__id, {@enum.value "_id" spCodeSystem__id Logical id of this artifact }
    spCodeSystem__lastUpdated, {@enum.value "_lastUpdated" spCodeSystem__lastUpdated When the resource version last changed }
    spCodeSystem__profile, {@enum.value "_profile" spCodeSystem__profile Profiles this resource claims to conform to }
    spCodeSystem__query, {@enum.value "_query" spCodeSystem__query A custom search profile that describes a specific defined query operation }
    spCodeSystem__security, {@enum.value "_security" spCodeSystem__security Security Labels applied to this resource }
    spCodeSystem__source, {@enum.value "_source" spCodeSystem__source Identifies where the resource comes from }
    spCodeSystem__tag, {@enum.value "_tag" spCodeSystem__tag Tags applied to this resource }
    spCodeSystem__text, {@enum.value "_text" spCodeSystem__text Search on the narrative of the resource }
    spCodeSystem_Code, {@enum.value "code" spCodeSystem_Code A code defined in the code system }
    spCodeSystem_Contentmode, {@enum.value "content-mode" spCodeSystem_Contentmode not-present | example | fragment | complete | supplement }
    spCodeSystem_Date, {@enum.value "date" spCodeSystem_Date The code system publication date }
    spCodeSystem_Description, {@enum.value "description" spCodeSystem_Description The description of the code system }
    spCodeSystem_Identifier, {@enum.value "identifier" spCodeSystem_Identifier External identifier for the code system }
    spCodeSystem_Jurisdiction, {@enum.value "jurisdiction" spCodeSystem_Jurisdiction Intended jurisdiction for the code system }
    spCodeSystem_Language, {@enum.value "language" spCodeSystem_Language A language in which a designation is provided }
    spCodeSystem_Name, {@enum.value "name" spCodeSystem_Name Computationally friendly name of the code system }
    spCodeSystem_Publisher, {@enum.value "publisher" spCodeSystem_Publisher Name of the publisher of the code system }
    spCodeSystem_Status, {@enum.value "status" spCodeSystem_Status The current status of the code system }
    spCodeSystem_Supplements, {@enum.value "supplements" spCodeSystem_Supplements Code System this adds designations and properties to }
    spCodeSystem_System, {@enum.value "system" spCodeSystem_System The system for any codes defined by this code system (same as 'url') }
    spCodeSystem_Title, {@enum.value "title" spCodeSystem_Title The human-friendly name of the code system }
    spCodeSystem_Url, {@enum.value "url" spCodeSystem_Url The uri that identifies the code system }
    spCodeSystem_Version); {@enum.value "version" spCodeSystem_Version The business version of the code system }
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
    spCommunication__source, {@enum.value "_source" spCommunication__source Identifies where the resource comes from }
    spCommunication__tag, {@enum.value "_tag" spCommunication__tag Tags applied to this resource }
    spCommunication__text, {@enum.value "_text" spCommunication__text Search on the narrative of the resource }
    spCommunication_Basedon, {@enum.value "based-on" spCommunication_Basedon Request fulfilled by this communication }
    spCommunication_Category, {@enum.value "category" spCommunication_Category Message category }
    spCommunication_Context, {@enum.value "context" spCommunication_Context Encounter or episode leading to message }
    spCommunication_Encounter, {@enum.value "encounter" spCommunication_Encounter Encounter leading to message }
    spCommunication_Identifier, {@enum.value "identifier" spCommunication_Identifier Unique identifier }
    spCommunication_Instantiates, {@enum.value "instantiates" spCommunication_Instantiates Instantiates protocol or definition }
    spCommunication_Medium, {@enum.value "medium" spCommunication_Medium A channel of communication }
    spCommunication_Partof, {@enum.value "part-of" spCommunication_Partof Part of this action }
    spCommunication_Patient, {@enum.value "patient" spCommunication_Patient Focus of message }
    spCommunication_Received, {@enum.value "received" spCommunication_Received When received }
    spCommunication_Recipient, {@enum.value "recipient" spCommunication_Recipient Message recipient }
    spCommunication_Sender, {@enum.value "sender" spCommunication_Sender Message sender }
    spCommunication_Sent, {@enum.value "sent" spCommunication_Sent When sent }
    spCommunication_Status, {@enum.value "status" spCommunication_Status preparation | in-progress | not-done | suspended | aborted | completed | entered-in-error }
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
    spCommunicationRequest__source, {@enum.value "_source" spCommunicationRequest__source Identifies where the resource comes from }
    spCommunicationRequest__tag, {@enum.value "_tag" spCommunicationRequest__tag Tags applied to this resource }
    spCommunicationRequest__text, {@enum.value "_text" spCommunicationRequest__text Search on the narrative of the resource }
    spCommunicationRequest_Authored, {@enum.value "authored" spCommunicationRequest_Authored When request transitioned to being actionable }
    spCommunicationRequest_Basedon, {@enum.value "based-on" spCommunicationRequest_Basedon Fulfills plan or proposal }
    spCommunicationRequest_Category, {@enum.value "category" spCommunicationRequest_Category Message category }
    spCommunicationRequest_Context, {@enum.value "context" spCommunicationRequest_Context Encounter or episode leading to message }
    spCommunicationRequest_Encounter, {@enum.value "encounter" spCommunicationRequest_Encounter Encounter leading to message }
    spCommunicationRequest_Groupidentifier, {@enum.value "group-identifier" spCommunicationRequest_Groupidentifier Composite request this is part of }
    spCommunicationRequest_Identifier, {@enum.value "identifier" spCommunicationRequest_Identifier Unique identifier }
    spCommunicationRequest_Medium, {@enum.value "medium" spCommunicationRequest_Medium A channel of communication }
    spCommunicationRequest_Occurrence, {@enum.value "occurrence" spCommunicationRequest_Occurrence When scheduled }
    spCommunicationRequest_Patient, {@enum.value "patient" spCommunicationRequest_Patient Focus of message }
    spCommunicationRequest_Priority, {@enum.value "priority" spCommunicationRequest_Priority Message urgency }
    spCommunicationRequest_Recipient, {@enum.value "recipient" spCommunicationRequest_Recipient Message recipient }
    spCommunicationRequest_Replaces, {@enum.value "replaces" spCommunicationRequest_Replaces Request(s) replaced by this request }
    spCommunicationRequest_Requester, {@enum.value "requester" spCommunicationRequest_Requester Who/what is requesting service }
    spCommunicationRequest_Sender, {@enum.value "sender" spCommunicationRequest_Sender Message sender }
    spCommunicationRequest_Status, {@enum.value "status" spCommunicationRequest_Status draft | active | suspended | cancelled | completed | entered-in-error | unknown }
    spCommunicationRequest_Subject); {@enum.value "subject" spCommunicationRequest_Subject Focus of message }
{$ENDIF}

{$IFDEF FHIR_COMPARTMENTDEFINITION}
  {@Enum TSearchParamsCompartmentDefinition
    Search Parameters for CompartmentDefinition
  }
  TSearchParamsCompartmentDefinition = (
    spCompartmentDefinition__content, {@enum.value "_content" spCompartmentDefinition__content Search on the entire content of the resource }
    spCompartmentDefinition__id, {@enum.value "_id" spCompartmentDefinition__id Logical id of this artifact }
    spCompartmentDefinition__lastUpdated, {@enum.value "_lastUpdated" spCompartmentDefinition__lastUpdated When the resource version last changed }
    spCompartmentDefinition__profile, {@enum.value "_profile" spCompartmentDefinition__profile Profiles this resource claims to conform to }
    spCompartmentDefinition__query, {@enum.value "_query" spCompartmentDefinition__query A custom search profile that describes a specific defined query operation }
    spCompartmentDefinition__security, {@enum.value "_security" spCompartmentDefinition__security Security Labels applied to this resource }
    spCompartmentDefinition__source, {@enum.value "_source" spCompartmentDefinition__source Identifies where the resource comes from }
    spCompartmentDefinition__tag, {@enum.value "_tag" spCompartmentDefinition__tag Tags applied to this resource }
    spCompartmentDefinition__text, {@enum.value "_text" spCompartmentDefinition__text Search on the narrative of the resource }
    spCompartmentDefinition_Code, {@enum.value "code" spCompartmentDefinition_Code Patient | Encounter | RelatedPerson | Practitioner | Device }
    spCompartmentDefinition_Contexttype, {@enum.value "context-type" spCompartmentDefinition_Contexttype A type of use context assigned to the compartment definition }
    spCompartmentDefinition_Date, {@enum.value "date" spCompartmentDefinition_Date The compartment definition publication date }
    spCompartmentDefinition_Description, {@enum.value "description" spCompartmentDefinition_Description The description of the compartment definition }
    spCompartmentDefinition_Jurisdiction, {@enum.value "jurisdiction" spCompartmentDefinition_Jurisdiction Intended jurisdiction for the compartment definition }
    spCompartmentDefinition_Name, {@enum.value "name" spCompartmentDefinition_Name Computationally friendly name of the compartment definition }
    spCompartmentDefinition_Publisher, {@enum.value "publisher" spCompartmentDefinition_Publisher Name of the publisher of the compartment definition }
    spCompartmentDefinition_Resource, {@enum.value "resource" spCompartmentDefinition_Resource Name of resource type }
    spCompartmentDefinition_Status, {@enum.value "status" spCompartmentDefinition_Status The current status of the compartment definition }
    spCompartmentDefinition_Title, {@enum.value "title" spCompartmentDefinition_Title The human-friendly name of the compartment definition }
    spCompartmentDefinition_Url); {@enum.value "url" spCompartmentDefinition_Url The uri that identifies the compartment definition }
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
    spComposition__source, {@enum.value "_source" spComposition__source Identifies where the resource comes from }
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
    spComposition_Relatedid, {@enum.value "related-id" spComposition_Relatedid Target of the relationship }
    spComposition_Relatedref, {@enum.value "related-ref" spComposition_Relatedref Target of the relationship }
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
    spConceptMap__source, {@enum.value "_source" spConceptMap__source Identifies where the resource comes from }
    spConceptMap__tag, {@enum.value "_tag" spConceptMap__tag Tags applied to this resource }
    spConceptMap__text, {@enum.value "_text" spConceptMap__text Search on the narrative of the resource }
    spConceptMap_Date, {@enum.value "date" spConceptMap_Date The concept map publication date }
    spConceptMap_Dependson, {@enum.value "dependson" spConceptMap_Dependson Reference to property mapping depends on }
    spConceptMap_Description, {@enum.value "description" spConceptMap_Description The description of the concept map }
    spConceptMap_Identifier, {@enum.value "identifier" spConceptMap_Identifier External identifier for the concept map }
    spConceptMap_Jurisdiction, {@enum.value "jurisdiction" spConceptMap_Jurisdiction Intended jurisdiction for the concept map }
    spConceptMap_Name, {@enum.value "name" spConceptMap_Name Computationally friendly name of the concept map }
    spConceptMap_Other, {@enum.value "other" spConceptMap_Other Canonical URL for other concept map }
    spConceptMap_Product, {@enum.value "product" spConceptMap_Product Reference to property mapping depends on }
    spConceptMap_Publisher, {@enum.value "publisher" spConceptMap_Publisher Name of the publisher of the concept map }
    spConceptMap_Source, {@enum.value "source" spConceptMap_Source Identifies the source of the concepts which are being mapped }
    spConceptMap_Sourcecode, {@enum.value "source-code" spConceptMap_Sourcecode Identifies element being mapped }
    spConceptMap_Sourcesystem, {@enum.value "source-system" spConceptMap_Sourcesystem Code System (if value set crosses code systems) }
    spConceptMap_Sourceuri, {@enum.value "source-uri" spConceptMap_Sourceuri Identifies the source of the concepts which are being mapped }
    spConceptMap_Status, {@enum.value "status" spConceptMap_Status The current status of the concept map }
    spConceptMap_Target, {@enum.value "target" spConceptMap_Target Provides context to the mappings }
    spConceptMap_Targetcode, {@enum.value "target-code" spConceptMap_Targetcode Code that identifies the target element }
    spConceptMap_Targetsystem, {@enum.value "target-system" spConceptMap_Targetsystem System of the target (if necessary) }
    spConceptMap_Targeturi, {@enum.value "target-uri" spConceptMap_Targeturi Provides context to the mappings }
    spConceptMap_Title, {@enum.value "title" spConceptMap_Title The human-friendly name of the concept map }
    spConceptMap_Url, {@enum.value "url" spConceptMap_Url The uri that identifies the concept map }
    spConceptMap_Version); {@enum.value "version" spConceptMap_Version The business version of the concept map }
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
    spCondition__source, {@enum.value "_source" spCondition__source Identifies where the resource comes from }
    spCondition__tag, {@enum.value "_tag" spCondition__tag Tags applied to this resource }
    spCondition__text, {@enum.value "_text" spCondition__text Search on the narrative of the resource }
    spCondition_Abatementage, {@enum.value "abatement-age" spCondition_Abatementage Abatement as age or age range }
    spCondition_Abatementdate, {@enum.value "abatement-date" spCondition_Abatementdate Date-related abatements (dateTime and period) }
    spCondition_Abatementstring, {@enum.value "abatement-string" spCondition_Abatementstring Abatement as a string }
    spCondition_Asserteddate, {@enum.value "asserted-date" spCondition_Asserteddate Date record was believed accurate }
    spCondition_Asserter, {@enum.value "asserter" spCondition_Asserter Person who asserts this condition }
    spCondition_Bodysite, {@enum.value "body-site" spCondition_Bodysite Anatomical location, if relevant }
    spCondition_Category, {@enum.value "category" spCondition_Category The category of the condition }
    spCondition_Clinicalstatus, {@enum.value "clinical-status" spCondition_Clinicalstatus The clinical status of the condition }
    spCondition_Code, {@enum.value "code" spCondition_Code Code for the condition }
    spCondition_Context, {@enum.value "context" spCondition_Context Encounter or episode when condition first asserted }
    spCondition_Encounter, {@enum.value "encounter" spCondition_Encounter Encounter when condition first asserted }
    spCondition_Evidence, {@enum.value "evidence" spCondition_Evidence Manifestation/symptom }
    spCondition_Evidencedetail, {@enum.value "evidence-detail" spCondition_Evidencedetail Supporting information found elsewhere }
    spCondition_Identifier, {@enum.value "identifier" spCondition_Identifier A unique identifier of the condition record }
    spCondition_Onsetage, {@enum.value "onset-age" spCondition_Onsetage Onsets as age or age range }
    spCondition_Onsetdate, {@enum.value "onset-date" spCondition_Onsetdate Date related onsets (dateTime and Period) }
    spCondition_Onsetinfo, {@enum.value "onset-info" spCondition_Onsetinfo Onsets as a string }
    spCondition_Patient, {@enum.value "patient" spCondition_Patient Who has the condition? }
    spCondition_Severity, {@enum.value "severity" spCondition_Severity The severity of the condition }
    spCondition_Stage, {@enum.value "stage" spCondition_Stage Simple summary (disease specific) }
    spCondition_Subject, {@enum.value "subject" spCondition_Subject Who has the condition? }
    spCondition_Verificationstatus); {@enum.value "verification-status" spCondition_Verificationstatus unconfirmed | provisional | differential | confirmed | refuted | entered-in-error }
{$ENDIF}

{$IFDEF FHIR_CONSENT}
  {@Enum TSearchParamsConsent
    Search Parameters for Consent
  }
  TSearchParamsConsent = (
    spConsent__content, {@enum.value "_content" spConsent__content Search on the entire content of the resource }
    spConsent__id, {@enum.value "_id" spConsent__id Logical id of this artifact }
    spConsent__lastUpdated, {@enum.value "_lastUpdated" spConsent__lastUpdated When the resource version last changed }
    spConsent__profile, {@enum.value "_profile" spConsent__profile Profiles this resource claims to conform to }
    spConsent__query, {@enum.value "_query" spConsent__query A custom search profile that describes a specific defined query operation }
    spConsent__security, {@enum.value "_security" spConsent__security Security Labels applied to this resource }
    spConsent__source, {@enum.value "_source" spConsent__source Identifies where the resource comes from }
    spConsent__tag, {@enum.value "_tag" spConsent__tag Tags applied to this resource }
    spConsent__text, {@enum.value "_text" spConsent__text Search on the narrative of the resource }
    spConsent_Action, {@enum.value "action" spConsent_Action Actions controlled by this rule }
    spConsent_Actor, {@enum.value "actor" spConsent_Actor Resource for the actor (or group, by role) }
    spConsent_Category, {@enum.value "category" spConsent_Category Classification of the consent statement - for indexing/retrieval }
    spConsent_Consentor, {@enum.value "consentor" spConsent_Consentor Who is agreeing to the policy and rules }
    spConsent_Data, {@enum.value "data" spConsent_Data The actual data reference }
    spConsent_Date, {@enum.value "date" spConsent_Date When this Consent was created or indexed }
    spConsent_Identifier, {@enum.value "identifier" spConsent_Identifier Identifier for this record (external references) }
    spConsent_Organization, {@enum.value "organization" spConsent_Organization Custodian of the consent }
    spConsent_Patient, {@enum.value "patient" spConsent_Patient Who the consent applies to }
    spConsent_Period, {@enum.value "period" spConsent_Period Timeframe for this rule }
    spConsent_Purpose, {@enum.value "purpose" spConsent_Purpose Context of activities covered by this rule }
    spConsent_Scope, {@enum.value "scope" spConsent_Scope Which of the four areas this resource covers }
    spConsent_Securitylabel, {@enum.value "securitylabel" spConsent_Securitylabel Security Labels that define affected resources }
    spConsent_Source, {@enum.value "source" spConsent_Source Source from which this consent is taken }
    spConsent_Status); {@enum.value "status" spConsent_Status draft | proposed | active | rejected | inactive | entered-in-error }
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
    spContract__source, {@enum.value "_source" spContract__source Identifies where the resource comes from }
    spContract__tag, {@enum.value "_tag" spContract__tag Tags applied to this resource }
    spContract__text, {@enum.value "_text" spContract__text Search on the narrative of the resource }
    spContract_Authority, {@enum.value "authority" spContract_Authority The authority of the contract }
    spContract_Domain, {@enum.value "domain" spContract_Domain The domain of the contract }
    spContract_Identifier, {@enum.value "identifier" spContract_Identifier The identity of the contract }
    spContract_Issued, {@enum.value "issued" spContract_Issued The date/time the contract was issued }
    spContract_Patient, {@enum.value "patient" spContract_Patient The identity of the subject of the contract (if a patient) }
    spContract_Signer, {@enum.value "signer" spContract_Signer Contract Signatory Party }
    spContract_Status, {@enum.value "status" spContract_Status The status of the contract }
    spContract_Subject); {@enum.value "subject" spContract_Subject The identity of the subject of the contract }
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
    spCoverage__source, {@enum.value "_source" spCoverage__source Identifies where the resource comes from }
    spCoverage__tag, {@enum.value "_tag" spCoverage__tag Tags applied to this resource }
    spCoverage__text, {@enum.value "_text" spCoverage__text Search on the narrative of the resource }
    spCoverage_Beneficiary, {@enum.value "beneficiary" spCoverage_Beneficiary Covered party }
    spCoverage_Class, {@enum.value "class" spCoverage_Class Class identifier }
    spCoverage_Dependent, {@enum.value "dependent" spCoverage_Dependent Dependent number }
    spCoverage_Group, {@enum.value "group" spCoverage_Group Group identifier }
    spCoverage_Identifier, {@enum.value "identifier" spCoverage_Identifier The primary identifier of the insured and the coverage }
    spCoverage_Patient, {@enum.value "patient" spCoverage_Patient Retrieve coverages for a patient }
    spCoverage_Payor, {@enum.value "payor" spCoverage_Payor The identity of the insurer or party paying for services }
    spCoverage_Plan, {@enum.value "plan" spCoverage_Plan A plan or policy identifier }
    spCoverage_Policyholder, {@enum.value "policy-holder" spCoverage_Policyholder Reference to the policyholder }
    spCoverage_Sequence, {@enum.value "sequence" spCoverage_Sequence Sequence number }
    spCoverage_Status, {@enum.value "status" spCoverage_Status The status of the Coverage }
    spCoverage_Subclass, {@enum.value "subclass" spCoverage_Subclass Sub-class identifier }
    spCoverage_Subgroup, {@enum.value "subgroup" spCoverage_Subgroup Sub-group identifier }
    spCoverage_Subplan, {@enum.value "subplan" spCoverage_Subplan Sub-plan identifier }
    spCoverage_Subscriber, {@enum.value "subscriber" spCoverage_Subscriber Reference to the subscriber }
    spCoverage_Type); {@enum.value "type" spCoverage_Type The kind of coverage (health plan, auto, Workers Compensation) }
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
    spDetectedIssue__source, {@enum.value "_source" spDetectedIssue__source Identifies where the resource comes from }
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
    spDevice__source, {@enum.value "_source" spDevice__source Identifies where the resource comes from }
    spDevice__tag, {@enum.value "_tag" spDevice__tag Tags applied to this resource }
    spDevice__text, {@enum.value "_text" spDevice__text Search on the narrative of the resource }
    spDevice_Devicename, {@enum.value "device-name" spDevice_Devicename A server defined search that may match any of the string fields in the Device.udi.name  or Device.type.coding.display or  Device.type.text }
    spDevice_Identifier, {@enum.value "identifier" spDevice_Identifier Instance id from manufacturer, owner, and others }
    spDevice_Location, {@enum.value "location" spDevice_Location A location, where the resource is found }
    spDevice_Manufacturer, {@enum.value "manufacturer" spDevice_Manufacturer The manufacturer of the device }
    spDevice_Model, {@enum.value "model" spDevice_Model The model of the device }
    spDevice_Organization, {@enum.value "organization" spDevice_Organization The organization responsible for the device }
    spDevice_Patient, {@enum.value "patient" spDevice_Patient Patient information, if the resource is affixed to a person }
    spDevice_Status, {@enum.value "status" spDevice_Status active | inactive | entered-in-error | unknown }
    spDevice_Type, {@enum.value "type" spDevice_Type The type of the device }
    spDevice_Udicarrier, {@enum.value "udi-carrier" spDevice_Udicarrier UDI Barcode (RFID or other technology) string either in HRF format or AIDC format converted to base64 string. }
    spDevice_Udidi, {@enum.value "udi-di" spDevice_Udidi The udi Device Identifier (DI) }
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
    spDeviceComponent__source, {@enum.value "_source" spDeviceComponent__source Identifies where the resource comes from }
    spDeviceComponent__tag, {@enum.value "_tag" spDeviceComponent__tag Tags applied to this resource }
    spDeviceComponent__text, {@enum.value "_text" spDeviceComponent__text Search on the narrative of the resource }
    spDeviceComponent_Identifier, {@enum.value "identifier" spDeviceComponent_Identifier The identifier of the component }
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
    spDeviceMetric__source, {@enum.value "_source" spDeviceMetric__source Identifies where the resource comes from }
    spDeviceMetric__tag, {@enum.value "_tag" spDeviceMetric__tag Tags applied to this resource }
    spDeviceMetric__text, {@enum.value "_text" spDeviceMetric__text Search on the narrative of the resource }
    spDeviceMetric_Category, {@enum.value "category" spDeviceMetric_Category The category of the metric }
    spDeviceMetric_Identifier, {@enum.value "identifier" spDeviceMetric_Identifier The identifier of the metric }
    spDeviceMetric_Parent, {@enum.value "parent" spDeviceMetric_Parent The parent DeviceMetric resource }
    spDeviceMetric_Source, {@enum.value "source" spDeviceMetric_Source The device resource }
    spDeviceMetric_Type); {@enum.value "type" spDeviceMetric_Type The component type }
{$ENDIF}

{$IFDEF FHIR_DEVICEREQUEST}
  {@Enum TSearchParamsDeviceRequest
    Search Parameters for DeviceRequest
  }
  TSearchParamsDeviceRequest = (
    spDeviceRequest__content, {@enum.value "_content" spDeviceRequest__content Search on the entire content of the resource }
    spDeviceRequest__id, {@enum.value "_id" spDeviceRequest__id Logical id of this artifact }
    spDeviceRequest__lastUpdated, {@enum.value "_lastUpdated" spDeviceRequest__lastUpdated When the resource version last changed }
    spDeviceRequest__profile, {@enum.value "_profile" spDeviceRequest__profile Profiles this resource claims to conform to }
    spDeviceRequest__query, {@enum.value "_query" spDeviceRequest__query A custom search profile that describes a specific defined query operation }
    spDeviceRequest__security, {@enum.value "_security" spDeviceRequest__security Security Labels applied to this resource }
    spDeviceRequest__source, {@enum.value "_source" spDeviceRequest__source Identifies where the resource comes from }
    spDeviceRequest__tag, {@enum.value "_tag" spDeviceRequest__tag Tags applied to this resource }
    spDeviceRequest__text, {@enum.value "_text" spDeviceRequest__text Search on the narrative of the resource }
    spDeviceRequest_Authoredon, {@enum.value "authored-on" spDeviceRequest_Authoredon When the request transitioned to being actionable }
    spDeviceRequest_Basedon, {@enum.value "based-on" spDeviceRequest_Basedon Plan/proposal/order fulfilled by this request }
    spDeviceRequest_Code, {@enum.value "code" spDeviceRequest_Code Code for what is being requested/ordered }
    spDeviceRequest_Device, {@enum.value "device" spDeviceRequest_Device Reference to resource that is being requested/ordered }
    spDeviceRequest_Encounter, {@enum.value "encounter" spDeviceRequest_Encounter Encounter or Episode during which request was created }
    spDeviceRequest_Eventdate, {@enum.value "event-date" spDeviceRequest_Eventdate When service should occur }
    spDeviceRequest_Groupidentifier, {@enum.value "group-identifier" spDeviceRequest_Groupidentifier Composite request this is part of }
    spDeviceRequest_Identifier, {@enum.value "identifier" spDeviceRequest_Identifier Business identifier for request/order }
    spDeviceRequest_Instantiates, {@enum.value "instantiates" spDeviceRequest_Instantiates Protocol or definition followed by this request }
    spDeviceRequest_Insurance, {@enum.value "insurance" spDeviceRequest_Insurance Associated insurance coverage }
    spDeviceRequest_Intent, {@enum.value "intent" spDeviceRequest_Intent proposal | plan | original-order |reflex-order }
    spDeviceRequest_Patient, {@enum.value "patient" spDeviceRequest_Patient Individual the service is ordered for }
    spDeviceRequest_Performer, {@enum.value "performer" spDeviceRequest_Performer Desired performer for service }
    spDeviceRequest_Priorrequest, {@enum.value "priorrequest" spDeviceRequest_Priorrequest Request takes the place of referenced completed or terminated requests }
    spDeviceRequest_Requester, {@enum.value "requester" spDeviceRequest_Requester Who/what is requesting service? }
    spDeviceRequest_Status, {@enum.value "status" spDeviceRequest_Status entered-in-error | draft | active |suspended | completed? }
    spDeviceRequest_Subject); {@enum.value "subject" spDeviceRequest_Subject Individual the service is ordered for }
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
    spDeviceUseStatement__source, {@enum.value "_source" spDeviceUseStatement__source Identifies where the resource comes from }
    spDeviceUseStatement__tag, {@enum.value "_tag" spDeviceUseStatement__tag Tags applied to this resource }
    spDeviceUseStatement__text, {@enum.value "_text" spDeviceUseStatement__text Search on the narrative of the resource }
    spDeviceUseStatement_Device, {@enum.value "device" spDeviceUseStatement_Device Search by device }
    spDeviceUseStatement_Identifier, {@enum.value "identifier" spDeviceUseStatement_Identifier Search by identifier }
    spDeviceUseStatement_Patient, {@enum.value "patient" spDeviceUseStatement_Patient Search by subject - a patient }
    spDeviceUseStatement_Subject); {@enum.value "subject" spDeviceUseStatement_Subject Search by subject }
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
    spDiagnosticReport__source, {@enum.value "_source" spDiagnosticReport__source Identifies where the resource comes from }
    spDiagnosticReport__tag, {@enum.value "_tag" spDiagnosticReport__tag Tags applied to this resource }
    spDiagnosticReport__text, {@enum.value "_text" spDiagnosticReport__text Search on the narrative of the resource }
    spDiagnosticReport_Basedon, {@enum.value "based-on" spDiagnosticReport_Basedon Reference to the service request. }
    spDiagnosticReport_Category, {@enum.value "category" spDiagnosticReport_Category Which diagnostic discipline/department created the report }
    spDiagnosticReport_Code, {@enum.value "code" spDiagnosticReport_Code The code for the report as a whole, as opposed to codes for the atomic results, which are the names on the observation resource referred to from the result }
    spDiagnosticReport_Context, {@enum.value "context" spDiagnosticReport_Context Healthcare event (Episode of Care or Encounter) related to the report }
    spDiagnosticReport_Date, {@enum.value "date" spDiagnosticReport_Date The clinically relevant time of the report }
    spDiagnosticReport_Diagnosis, {@enum.value "diagnosis" spDiagnosticReport_Diagnosis A coded diagnosis on the report }
    spDiagnosticReport_Encounter, {@enum.value "encounter" spDiagnosticReport_Encounter The Encounter when the order was made }
    spDiagnosticReport_Identifier, {@enum.value "identifier" spDiagnosticReport_Identifier An identifier for the report }
    spDiagnosticReport_Issued, {@enum.value "issued" spDiagnosticReport_Issued When the report was issued }
    spDiagnosticReport_Media, {@enum.value "media" spDiagnosticReport_Media A reference to the image source. }
    spDiagnosticReport_Patient, {@enum.value "patient" spDiagnosticReport_Patient The subject of the report if a patient }
    spDiagnosticReport_Performer, {@enum.value "performer" spDiagnosticReport_Performer Who is responsible for the report }
    spDiagnosticReport_Result, {@enum.value "result" spDiagnosticReport_Result Link to an atomic result (observation resource) }
    spDiagnosticReport_Resultsinterpreter, {@enum.value "results-interpreter" spDiagnosticReport_Resultsinterpreter Who was the source of the report }
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
    spDocumentManifest__source, {@enum.value "_source" spDocumentManifest__source Identifies where the resource comes from }
    spDocumentManifest__tag, {@enum.value "_tag" spDocumentManifest__tag Tags applied to this resource }
    spDocumentManifest__text, {@enum.value "_text" spDocumentManifest__text Search on the narrative of the resource }
    spDocumentManifest_Agent, {@enum.value "agent" spDocumentManifest_Agent Who and/or what had an agent participation }
    spDocumentManifest_Created, {@enum.value "created" spDocumentManifest_Created When this document manifest created }
    spDocumentManifest_Description, {@enum.value "description" spDocumentManifest_Description Human-readable description (title) }
    spDocumentManifest_Identifier, {@enum.value "identifier" spDocumentManifest_Identifier Unique Identifier for the set of documents }
    spDocumentManifest_Item, {@enum.value "item" spDocumentManifest_Item Items in manifest }
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
    spDocumentReference__source, {@enum.value "_source" spDocumentReference__source Identifies where the resource comes from }
    spDocumentReference__tag, {@enum.value "_tag" spDocumentReference__tag Tags applied to this resource }
    spDocumentReference__text, {@enum.value "_text" spDocumentReference__text Search on the narrative of the resource }
    spDocumentReference_Agent, {@enum.value "agent" spDocumentReference_Agent Who and/or what authored the document }
    spDocumentReference_Authenticator, {@enum.value "authenticator" spDocumentReference_Authenticator Who/what authenticated the document }
    spDocumentReference_Class, {@enum.value "class" spDocumentReference_Class Categorization of document }
    spDocumentReference_Contenttype, {@enum.value "contenttype" spDocumentReference_Contenttype Mime type of the content, with charset etc. }
    spDocumentReference_Created, {@enum.value "created" spDocumentReference_Created Document creation time }
    spDocumentReference_Custodian, {@enum.value "custodian" spDocumentReference_Custodian Organization which maintains the document }
    spDocumentReference_Date, {@enum.value "date" spDocumentReference_Date When this document reference was created }
    spDocumentReference_Description, {@enum.value "description" spDocumentReference_Description Human-readable description (title) }
    spDocumentReference_Encounter, {@enum.value "encounter" spDocumentReference_Encounter Context of the document  content }
    spDocumentReference_Event, {@enum.value "event" spDocumentReference_Event Main clinical acts documented }
    spDocumentReference_Facility, {@enum.value "facility" spDocumentReference_Facility Kind of facility where patient was seen }
    spDocumentReference_Format, {@enum.value "format" spDocumentReference_Format Format/content rules for the document }
    spDocumentReference_Identifier, {@enum.value "identifier" spDocumentReference_Identifier Master Version Specific Identifier }
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
    spEligibilityRequest__source, {@enum.value "_source" spEligibilityRequest__source Identifies where the resource comes from }
    spEligibilityRequest__tag, {@enum.value "_tag" spEligibilityRequest__tag Tags applied to this resource }
    spEligibilityRequest__text, {@enum.value "_text" spEligibilityRequest__text Search on the narrative of the resource }
    spEligibilityRequest_Created, {@enum.value "created" spEligibilityRequest_Created The creation date for the EOB }
    spEligibilityRequest_Enterer, {@enum.value "enterer" spEligibilityRequest_Enterer The party who is responsible for the request }
    spEligibilityRequest_Facility, {@enum.value "facility" spEligibilityRequest_Facility Facility responsible for the goods and services }
    spEligibilityRequest_Identifier, {@enum.value "identifier" spEligibilityRequest_Identifier The business identifier of the Eligibility }
    spEligibilityRequest_Organization, {@enum.value "organization" spEligibilityRequest_Organization The reference to the providing organization }
    spEligibilityRequest_Patient, {@enum.value "patient" spEligibilityRequest_Patient The reference to the patient }
    spEligibilityRequest_Provider, {@enum.value "provider" spEligibilityRequest_Provider The reference to the provider }
    spEligibilityRequest_Status); {@enum.value "status" spEligibilityRequest_Status The status of the EligibilityRequest }
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
    spEligibilityResponse__source, {@enum.value "_source" spEligibilityResponse__source Identifies where the resource comes from }
    spEligibilityResponse__tag, {@enum.value "_tag" spEligibilityResponse__tag Tags applied to this resource }
    spEligibilityResponse__text, {@enum.value "_text" spEligibilityResponse__text Search on the narrative of the resource }
    spEligibilityResponse_Created, {@enum.value "created" spEligibilityResponse_Created The creation date }
    spEligibilityResponse_Disposition, {@enum.value "disposition" spEligibilityResponse_Disposition The contents of the disposition message }
    spEligibilityResponse_Identifier, {@enum.value "identifier" spEligibilityResponse_Identifier The business identifier }
    spEligibilityResponse_Insurer, {@enum.value "insurer" spEligibilityResponse_Insurer The organization which generated this resource }
    spEligibilityResponse_Outcome, {@enum.value "outcome" spEligibilityResponse_Outcome The processing outcome }
    spEligibilityResponse_Request, {@enum.value "request" spEligibilityResponse_Request The EligibilityRequest reference }
    spEligibilityResponse_Requestorganization, {@enum.value "request-organization" spEligibilityResponse_Requestorganization The EligibilityRequest organization }
    spEligibilityResponse_Requestprovider, {@enum.value "request-provider" spEligibilityResponse_Requestprovider The EligibilityRequest provider }
    spEligibilityResponse_Status); {@enum.value "status" spEligibilityResponse_Status The EligibilityRequest status }
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
    spEncounter__source, {@enum.value "_source" spEncounter__source Identifies where the resource comes from }
    spEncounter__tag, {@enum.value "_tag" spEncounter__tag Tags applied to this resource }
    spEncounter__text, {@enum.value "_text" spEncounter__text Search on the narrative of the resource }
    spEncounter_Appointment, {@enum.value "appointment" spEncounter_Appointment The appointment that scheduled this encounter }
    spEncounter_Class, {@enum.value "class" spEncounter_Class inpatient | outpatient | ambulatory | emergency + }
    spEncounter_Date, {@enum.value "date" spEncounter_Date A date within the period the Encounter lasted }
    spEncounter_Diagnosis, {@enum.value "diagnosis" spEncounter_Diagnosis Reason the encounter takes place (resource) }
    spEncounter_Episodeofcare, {@enum.value "episodeofcare" spEncounter_Episodeofcare Episode(s) of care that this encounter should be recorded against }
    spEncounter_Identifier, {@enum.value "identifier" spEncounter_Identifier Identifier(s) by which this encounter is known }
    spEncounter_Incomingreferral, {@enum.value "incomingreferral" spEncounter_Incomingreferral The ServiceRequest that initiated this encounter }
    spEncounter_Length, {@enum.value "length" spEncounter_Length Length of encounter in days }
    spEncounter_Location, {@enum.value "location" spEncounter_Location Location the encounter takes place }
    spEncounter_Locationperiod, {@enum.value "location-period" spEncounter_Locationperiod Time period during which the patient was present at the location }
    spEncounter_Partof, {@enum.value "part-of" spEncounter_Partof Another Encounter this encounter is part of }
    spEncounter_Participant, {@enum.value "participant" spEncounter_Participant Persons involved in the encounter other than the patient }
    spEncounter_Participanttype, {@enum.value "participant-type" spEncounter_Participanttype Role of participant in encounter }
    spEncounter_Patient, {@enum.value "patient" spEncounter_Patient The patient or group present at the encounter }
    spEncounter_Practitioner, {@enum.value "practitioner" spEncounter_Practitioner Persons involved in the encounter other than the patient }
    spEncounter_Reason, {@enum.value "reason" spEncounter_Reason Reason the encounter takes place (code) }
    spEncounter_Serviceprovider, {@enum.value "service-provider" spEncounter_Serviceprovider The organization (facility) responsible for this encounter }
    spEncounter_Specialarrangement, {@enum.value "special-arrangement" spEncounter_Specialarrangement Wheelchair, translator, stretcher, etc. }
    spEncounter_Status, {@enum.value "status" spEncounter_Status planned | arrived | triaged | in-progress | onleave | finished | cancelled + }
    spEncounter_Subject, {@enum.value "subject" spEncounter_Subject The patient or group present at the encounter }
    spEncounter_Type); {@enum.value "type" spEncounter_Type Specific type of encounter }
{$ENDIF}

{$IFDEF FHIR_ENDPOINT}
  {@Enum TSearchParamsEndpoint
    Search Parameters for Endpoint
  }
  TSearchParamsEndpoint = (
    spEndpoint__content, {@enum.value "_content" spEndpoint__content Search on the entire content of the resource }
    spEndpoint__id, {@enum.value "_id" spEndpoint__id Logical id of this artifact }
    spEndpoint__lastUpdated, {@enum.value "_lastUpdated" spEndpoint__lastUpdated When the resource version last changed }
    spEndpoint__profile, {@enum.value "_profile" spEndpoint__profile Profiles this resource claims to conform to }
    spEndpoint__query, {@enum.value "_query" spEndpoint__query A custom search profile that describes a specific defined query operation }
    spEndpoint__security, {@enum.value "_security" spEndpoint__security Security Labels applied to this resource }
    spEndpoint__source, {@enum.value "_source" spEndpoint__source Identifies where the resource comes from }
    spEndpoint__tag, {@enum.value "_tag" spEndpoint__tag Tags applied to this resource }
    spEndpoint__text, {@enum.value "_text" spEndpoint__text Search on the narrative of the resource }
    spEndpoint_Connectiontype, {@enum.value "connection-type" spEndpoint_Connectiontype Protocol/Profile/Standard to be used with this endpoint connection }
    spEndpoint_Identifier, {@enum.value "identifier" spEndpoint_Identifier Identifies this endpoint across multiple systems }
    spEndpoint_Name, {@enum.value "name" spEndpoint_Name A name that this endpoint can be identified by }
    spEndpoint_Organization, {@enum.value "organization" spEndpoint_Organization The organization that is managing the endpoint }
    spEndpoint_Payloadtype, {@enum.value "payload-type" spEndpoint_Payloadtype The type of content that may be used at this endpoint (e.g. XDS Discharge summaries) }
    spEndpoint_Status); {@enum.value "status" spEndpoint_Status The current status of the Endpoint (usually expected to be active) }
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
    spEnrollmentRequest__source, {@enum.value "_source" spEnrollmentRequest__source Identifies where the resource comes from }
    spEnrollmentRequest__tag, {@enum.value "_tag" spEnrollmentRequest__tag Tags applied to this resource }
    spEnrollmentRequest__text, {@enum.value "_text" spEnrollmentRequest__text Search on the narrative of the resource }
    spEnrollmentRequest_Identifier, {@enum.value "identifier" spEnrollmentRequest_Identifier The business identifier of the Enrollment }
    spEnrollmentRequest_Organization, {@enum.value "organization" spEnrollmentRequest_Organization The organization who generated this resource }
    spEnrollmentRequest_Patient, {@enum.value "patient" spEnrollmentRequest_Patient The party to be enrolled }
    spEnrollmentRequest_Status, {@enum.value "status" spEnrollmentRequest_Status The status fo the enrollment }
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
    spEnrollmentResponse__source, {@enum.value "_source" spEnrollmentResponse__source Identifies where the resource comes from }
    spEnrollmentResponse__tag, {@enum.value "_tag" spEnrollmentResponse__tag Tags applied to this resource }
    spEnrollmentResponse__text, {@enum.value "_text" spEnrollmentResponse__text Search on the narrative of the resource }
    spEnrollmentResponse_Identifier, {@enum.value "identifier" spEnrollmentResponse_Identifier The business identifier of the EnrollmentResponse }
    spEnrollmentResponse_Organization, {@enum.value "organization" spEnrollmentResponse_Organization The organization who generated this resource }
    spEnrollmentResponse_Request, {@enum.value "request" spEnrollmentResponse_Request The reference to the claim }
    spEnrollmentResponse_Status); {@enum.value "status" spEnrollmentResponse_Status The status of the enrollment response }
{$ENDIF}

{$IFDEF FHIR_ENTRYDEFINITION}
  {@Enum TSearchParamsEntryDefinition
    Search Parameters for EntryDefinition
  }
  TSearchParamsEntryDefinition = (
    spEntryDefinition__content, {@enum.value "_content" spEntryDefinition__content Search on the entire content of the resource }
    spEntryDefinition__id, {@enum.value "_id" spEntryDefinition__id Logical id of this artifact }
    spEntryDefinition__lastUpdated, {@enum.value "_lastUpdated" spEntryDefinition__lastUpdated When the resource version last changed }
    spEntryDefinition__profile, {@enum.value "_profile" spEntryDefinition__profile Profiles this resource claims to conform to }
    spEntryDefinition__query, {@enum.value "_query" spEntryDefinition__query A custom search profile that describes a specific defined query operation }
    spEntryDefinition__security, {@enum.value "_security" spEntryDefinition__security Security Labels applied to this resource }
    spEntryDefinition__source, {@enum.value "_source" spEntryDefinition__source Identifies where the resource comes from }
    spEntryDefinition__tag, {@enum.value "_tag" spEntryDefinition__tag Tags applied to this resource }
    spEntryDefinition__text); {@enum.value "_text" spEntryDefinition__text Search on the narrative of the resource }
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
    spEpisodeOfCare__source, {@enum.value "_source" spEpisodeOfCare__source Identifies where the resource comes from }
    spEpisodeOfCare__tag, {@enum.value "_tag" spEpisodeOfCare__tag Tags applied to this resource }
    spEpisodeOfCare__text, {@enum.value "_text" spEpisodeOfCare__text Search on the narrative of the resource }
    spEpisodeOfCare_Caremanager, {@enum.value "care-manager" spEpisodeOfCare_Caremanager Care manager/care co-ordinator for the patient }
    spEpisodeOfCare_Condition, {@enum.value "condition" spEpisodeOfCare_Condition Conditions/problems/diagnoses this episode of care is for }
    spEpisodeOfCare_Date, {@enum.value "date" spEpisodeOfCare_Date The provided date search value falls within the episode of care's period }
    spEpisodeOfCare_Identifier, {@enum.value "identifier" spEpisodeOfCare_Identifier Business Identifier(s) relevant for this EpisodeOfCare }
    spEpisodeOfCare_Incomingreferral, {@enum.value "incomingreferral" spEpisodeOfCare_Incomingreferral Incoming Referral Request }
    spEpisodeOfCare_Organization, {@enum.value "organization" spEpisodeOfCare_Organization The organization that has assumed the specific responsibilities of this EpisodeOfCare }
    spEpisodeOfCare_Patient, {@enum.value "patient" spEpisodeOfCare_Patient The patient who is the focus of this episode of care }
    spEpisodeOfCare_Status, {@enum.value "status" spEpisodeOfCare_Status The current status of the Episode of Care as provided (does not check the status history collection) }
    spEpisodeOfCare_Type); {@enum.value "type" spEpisodeOfCare_Type Type/class  - e.g. specialist referral, disease management }
{$ENDIF}

{$IFDEF FHIR_EVENTDEFINITION}
  {@Enum TSearchParamsEventDefinition
    Search Parameters for EventDefinition
  }
  TSearchParamsEventDefinition = (
    spEventDefinition__content, {@enum.value "_content" spEventDefinition__content Search on the entire content of the resource }
    spEventDefinition__id, {@enum.value "_id" spEventDefinition__id Logical id of this artifact }
    spEventDefinition__lastUpdated, {@enum.value "_lastUpdated" spEventDefinition__lastUpdated When the resource version last changed }
    spEventDefinition__profile, {@enum.value "_profile" spEventDefinition__profile Profiles this resource claims to conform to }
    spEventDefinition__query, {@enum.value "_query" spEventDefinition__query A custom search profile that describes a specific defined query operation }
    spEventDefinition__security, {@enum.value "_security" spEventDefinition__security Security Labels applied to this resource }
    spEventDefinition__source, {@enum.value "_source" spEventDefinition__source Identifies where the resource comes from }
    spEventDefinition__tag, {@enum.value "_tag" spEventDefinition__tag Tags applied to this resource }
    spEventDefinition__text, {@enum.value "_text" spEventDefinition__text Search on the narrative of the resource }
    spEventDefinition_Composedof, {@enum.value "composed-of" spEventDefinition_Composedof What resource is being referenced }
    spEventDefinition_Date, {@enum.value "date" spEventDefinition_Date The event definition publication date }
    spEventDefinition_Dependson, {@enum.value "depends-on" spEventDefinition_Dependson What resource is being referenced }
    spEventDefinition_Derivedfrom, {@enum.value "derived-from" spEventDefinition_Derivedfrom What resource is being referenced }
    spEventDefinition_Description, {@enum.value "description" spEventDefinition_Description The description of the event definition }
    spEventDefinition_Effective, {@enum.value "effective" spEventDefinition_Effective The time during which the event definition is intended to be in use }
    spEventDefinition_Identifier, {@enum.value "identifier" spEventDefinition_Identifier External identifier for the event definition }
    spEventDefinition_Jurisdiction, {@enum.value "jurisdiction" spEventDefinition_Jurisdiction Intended jurisdiction for the event definition }
    spEventDefinition_Name, {@enum.value "name" spEventDefinition_Name Computationally friendly name of the event definition }
    spEventDefinition_Predecessor, {@enum.value "predecessor" spEventDefinition_Predecessor What resource is being referenced }
    spEventDefinition_Publisher, {@enum.value "publisher" spEventDefinition_Publisher Name of the publisher of the event definition }
    spEventDefinition_Status, {@enum.value "status" spEventDefinition_Status The current status of the event definition }
    spEventDefinition_Successor, {@enum.value "successor" spEventDefinition_Successor What resource is being referenced }
    spEventDefinition_Title, {@enum.value "title" spEventDefinition_Title The human-friendly name of the event definition }
    spEventDefinition_Topic, {@enum.value "topic" spEventDefinition_Topic Topics associated with the module }
    spEventDefinition_Url, {@enum.value "url" spEventDefinition_Url The uri that identifies the event definition }
    spEventDefinition_Version); {@enum.value "version" spEventDefinition_Version The business version of the event definition }
{$ENDIF}

{$IFDEF FHIR_EXAMPLESCENARIO}
  {@Enum TSearchParamsExampleScenario
    Search Parameters for ExampleScenario
  }
  TSearchParamsExampleScenario = (
    spExampleScenario__content, {@enum.value "_content" spExampleScenario__content Search on the entire content of the resource }
    spExampleScenario__id, {@enum.value "_id" spExampleScenario__id Logical id of this artifact }
    spExampleScenario__lastUpdated, {@enum.value "_lastUpdated" spExampleScenario__lastUpdated When the resource version last changed }
    spExampleScenario__profile, {@enum.value "_profile" spExampleScenario__profile Profiles this resource claims to conform to }
    spExampleScenario__query, {@enum.value "_query" spExampleScenario__query A custom search profile that describes a specific defined query operation }
    spExampleScenario__security, {@enum.value "_security" spExampleScenario__security Security Labels applied to this resource }
    spExampleScenario__source, {@enum.value "_source" spExampleScenario__source Identifies where the resource comes from }
    spExampleScenario__tag, {@enum.value "_tag" spExampleScenario__tag Tags applied to this resource }
    spExampleScenario__text, {@enum.value "_text" spExampleScenario__text Search on the narrative of the resource }
    spExampleScenario_Date, {@enum.value "date" spExampleScenario_Date The example scenario publication date }
    spExampleScenario_Description, {@enum.value "description" spExampleScenario_Description The description of the example scenario }
    spExampleScenario_Identifier, {@enum.value "identifier" spExampleScenario_Identifier External identifier for the example scenario }
    spExampleScenario_Jurisdiction, {@enum.value "jurisdiction" spExampleScenario_Jurisdiction Intended jurisdiction for the example scenario }
    spExampleScenario_Name, {@enum.value "name" spExampleScenario_Name Computationally friendly name of the example scenario }
    spExampleScenario_Publisher, {@enum.value "publisher" spExampleScenario_Publisher Name of the publisher of the example scenario }
    spExampleScenario_Status, {@enum.value "status" spExampleScenario_Status The current status of the example scenario }
    spExampleScenario_Title, {@enum.value "title" spExampleScenario_Title The human-friendly name of the example scenario }
    spExampleScenario_Url, {@enum.value "url" spExampleScenario_Url The uri that identifies the example scenario }
    spExampleScenario_Version); {@enum.value "version" spExampleScenario_Version The business version of the example scenario }
{$ENDIF}

{$IFDEF FHIR_EXPANSIONPROFILE}
  {@Enum TSearchParamsExpansionProfile
    Search Parameters for ExpansionProfile
  }
  TSearchParamsExpansionProfile = (
    spExpansionProfile__content, {@enum.value "_content" spExpansionProfile__content Search on the entire content of the resource }
    spExpansionProfile__id, {@enum.value "_id" spExpansionProfile__id Logical id of this artifact }
    spExpansionProfile__lastUpdated, {@enum.value "_lastUpdated" spExpansionProfile__lastUpdated When the resource version last changed }
    spExpansionProfile__profile, {@enum.value "_profile" spExpansionProfile__profile Profiles this resource claims to conform to }
    spExpansionProfile__query, {@enum.value "_query" spExpansionProfile__query A custom search profile that describes a specific defined query operation }
    spExpansionProfile__security, {@enum.value "_security" spExpansionProfile__security Security Labels applied to this resource }
    spExpansionProfile__source, {@enum.value "_source" spExpansionProfile__source Identifies where the resource comes from }
    spExpansionProfile__tag, {@enum.value "_tag" spExpansionProfile__tag Tags applied to this resource }
    spExpansionProfile__text, {@enum.value "_text" spExpansionProfile__text Search on the narrative of the resource }
    spExpansionProfile_Date, {@enum.value "date" spExpansionProfile_Date The expansion profile publication date }
    spExpansionProfile_Description, {@enum.value "description" spExpansionProfile_Description The description of the expansion profile }
    spExpansionProfile_Identifier, {@enum.value "identifier" spExpansionProfile_Identifier External identifier for the expansion profile }
    spExpansionProfile_Jurisdiction, {@enum.value "jurisdiction" spExpansionProfile_Jurisdiction Intended jurisdiction for the expansion profile }
    spExpansionProfile_Name, {@enum.value "name" spExpansionProfile_Name Computationally friendly name of the expansion profile }
    spExpansionProfile_Publisher, {@enum.value "publisher" spExpansionProfile_Publisher Name of the publisher of the expansion profile }
    spExpansionProfile_Status, {@enum.value "status" spExpansionProfile_Status The current status of the expansion profile }
    spExpansionProfile_Url, {@enum.value "url" spExpansionProfile_Url The uri that identifies the expansion profile }
    spExpansionProfile_Version); {@enum.value "version" spExpansionProfile_Version The business version of the expansion profile }
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
    spExplanationOfBenefit__source, {@enum.value "_source" spExplanationOfBenefit__source Identifies where the resource comes from }
    spExplanationOfBenefit__tag, {@enum.value "_tag" spExplanationOfBenefit__tag Tags applied to this resource }
    spExplanationOfBenefit__text, {@enum.value "_text" spExplanationOfBenefit__text Search on the narrative of the resource }
    spExplanationOfBenefit_Careteam, {@enum.value "care-team" spExplanationOfBenefit_Careteam Member of the CareTeam }
    spExplanationOfBenefit_Claim, {@enum.value "claim" spExplanationOfBenefit_Claim The reference to the claim }
    spExplanationOfBenefit_Coverage, {@enum.value "coverage" spExplanationOfBenefit_Coverage The plan under which the claim was adjudicated }
    spExplanationOfBenefit_Created, {@enum.value "created" spExplanationOfBenefit_Created The creation date for the EOB }
    spExplanationOfBenefit_Disposition, {@enum.value "disposition" spExplanationOfBenefit_Disposition The contents of the disposition message }
    spExplanationOfBenefit_Encounter, {@enum.value "encounter" spExplanationOfBenefit_Encounter Encounters associated with a billed line item }
    spExplanationOfBenefit_Enterer, {@enum.value "enterer" spExplanationOfBenefit_Enterer The party responsible for the entry of the Claim }
    spExplanationOfBenefit_Facility, {@enum.value "facility" spExplanationOfBenefit_Facility Facility responsible for the goods and services }
    spExplanationOfBenefit_Identifier, {@enum.value "identifier" spExplanationOfBenefit_Identifier The business identifier of the Explanation of Benefit }
    spExplanationOfBenefit_Organization, {@enum.value "organization" spExplanationOfBenefit_Organization The reference to the providing organization }
    spExplanationOfBenefit_Patient, {@enum.value "patient" spExplanationOfBenefit_Patient The reference to the patient }
    spExplanationOfBenefit_Payee, {@enum.value "payee" spExplanationOfBenefit_Payee The party receiving any payment for the Claim }
    spExplanationOfBenefit_Provider, {@enum.value "provider" spExplanationOfBenefit_Provider The reference to the provider }
    spExplanationOfBenefit_Status); {@enum.value "status" spExplanationOfBenefit_Status Status of the instance }
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
    spFamilyMemberHistory__source, {@enum.value "_source" spFamilyMemberHistory__source Identifies where the resource comes from }
    spFamilyMemberHistory__tag, {@enum.value "_tag" spFamilyMemberHistory__tag Tags applied to this resource }
    spFamilyMemberHistory__text, {@enum.value "_text" spFamilyMemberHistory__text Search on the narrative of the resource }
    spFamilyMemberHistory_Code, {@enum.value "code" spFamilyMemberHistory_Code A search by a condition code }
    spFamilyMemberHistory_Date, {@enum.value "date" spFamilyMemberHistory_Date When history was captured/updated }
    spFamilyMemberHistory_Gender, {@enum.value "gender" spFamilyMemberHistory_Gender A search by a gender code of a family member }
    spFamilyMemberHistory_Identifier, {@enum.value "identifier" spFamilyMemberHistory_Identifier A search by a record identifier }
    spFamilyMemberHistory_Instantiates, {@enum.value "instantiates" spFamilyMemberHistory_Instantiates Instantiates protocol or definition }
    spFamilyMemberHistory_Patient, {@enum.value "patient" spFamilyMemberHistory_Patient The identity of a subject to list family member history items for }
    spFamilyMemberHistory_Relationship, {@enum.value "relationship" spFamilyMemberHistory_Relationship A search by a relationship type }
    spFamilyMemberHistory_Status); {@enum.value "status" spFamilyMemberHistory_Status partial | completed | entered-in-error | health-unknown }
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
    spFlag__source, {@enum.value "_source" spFlag__source Identifies where the resource comes from }
    spFlag__tag, {@enum.value "_tag" spFlag__tag Tags applied to this resource }
    spFlag__text, {@enum.value "_text" spFlag__text Search on the narrative of the resource }
    spFlag_Author, {@enum.value "author" spFlag_Author Flag creator }
    spFlag_Date, {@enum.value "date" spFlag_Date Time period when flag is active }
    spFlag_Encounter, {@enum.value "encounter" spFlag_Encounter Alert relevant during encounter }
    spFlag_Identifier, {@enum.value "identifier" spFlag_Identifier Business identifier }
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
    spGoal__source, {@enum.value "_source" spGoal__source Identifies where the resource comes from }
    spGoal__tag, {@enum.value "_tag" spGoal__tag Tags applied to this resource }
    spGoal__text, {@enum.value "_text" spGoal__text Search on the narrative of the resource }
    spGoal_Category, {@enum.value "category" spGoal_Category E.g. Treatment, dietary, behavioral, etc. }
    spGoal_Identifier, {@enum.value "identifier" spGoal_Identifier External Ids for this goal }
    spGoal_Patient, {@enum.value "patient" spGoal_Patient Who this goal is intended for }
    spGoal_Startdate, {@enum.value "start-date" spGoal_Startdate When goal pursuit begins }
    spGoal_Status, {@enum.value "status" spGoal_Status proposed | accepted | planned | in-progress | on-target | ahead-of-target | behind-target | sustaining | achieved | on-hold | cancelled | entered-in-error | rejected }
    spGoal_Subject, {@enum.value "subject" spGoal_Subject Who this goal is intended for }
    spGoal_Targetdate); {@enum.value "target-date" spGoal_Targetdate Reach goal on or before }
{$ENDIF}

{$IFDEF FHIR_GRAPHDEFINITION}
  {@Enum TSearchParamsGraphDefinition
    Search Parameters for GraphDefinition
  }
  TSearchParamsGraphDefinition = (
    spGraphDefinition__content, {@enum.value "_content" spGraphDefinition__content Search on the entire content of the resource }
    spGraphDefinition__id, {@enum.value "_id" spGraphDefinition__id Logical id of this artifact }
    spGraphDefinition__lastUpdated, {@enum.value "_lastUpdated" spGraphDefinition__lastUpdated When the resource version last changed }
    spGraphDefinition__profile, {@enum.value "_profile" spGraphDefinition__profile Profiles this resource claims to conform to }
    spGraphDefinition__query, {@enum.value "_query" spGraphDefinition__query A custom search profile that describes a specific defined query operation }
    spGraphDefinition__security, {@enum.value "_security" spGraphDefinition__security Security Labels applied to this resource }
    spGraphDefinition__source, {@enum.value "_source" spGraphDefinition__source Identifies where the resource comes from }
    spGraphDefinition__tag, {@enum.value "_tag" spGraphDefinition__tag Tags applied to this resource }
    spGraphDefinition__text, {@enum.value "_text" spGraphDefinition__text Search on the narrative of the resource }
    spGraphDefinition_Date, {@enum.value "date" spGraphDefinition_Date The graph definition publication date }
    spGraphDefinition_Description, {@enum.value "description" spGraphDefinition_Description The description of the graph definition }
    spGraphDefinition_Jurisdiction, {@enum.value "jurisdiction" spGraphDefinition_Jurisdiction Intended jurisdiction for the graph definition }
    spGraphDefinition_Name, {@enum.value "name" spGraphDefinition_Name Computationally friendly name of the graph definition }
    spGraphDefinition_Publisher, {@enum.value "publisher" spGraphDefinition_Publisher Name of the publisher of the graph definition }
    spGraphDefinition_Start, {@enum.value "start" spGraphDefinition_Start Type of resource at which the graph starts }
    spGraphDefinition_Status, {@enum.value "status" spGraphDefinition_Status The current status of the graph definition }
    spGraphDefinition_Url, {@enum.value "url" spGraphDefinition_Url The uri that identifies the graph definition }
    spGraphDefinition_Version); {@enum.value "version" spGraphDefinition_Version The business version of the graph definition }
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
    spGroup__source, {@enum.value "_source" spGroup__source Identifies where the resource comes from }
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

{$IFDEF FHIR_GUIDANCERESPONSE}
  {@Enum TSearchParamsGuidanceResponse
    Search Parameters for GuidanceResponse
  }
  TSearchParamsGuidanceResponse = (
    spGuidanceResponse__content, {@enum.value "_content" spGuidanceResponse__content Search on the entire content of the resource }
    spGuidanceResponse__id, {@enum.value "_id" spGuidanceResponse__id Logical id of this artifact }
    spGuidanceResponse__lastUpdated, {@enum.value "_lastUpdated" spGuidanceResponse__lastUpdated When the resource version last changed }
    spGuidanceResponse__profile, {@enum.value "_profile" spGuidanceResponse__profile Profiles this resource claims to conform to }
    spGuidanceResponse__query, {@enum.value "_query" spGuidanceResponse__query A custom search profile that describes a specific defined query operation }
    spGuidanceResponse__security, {@enum.value "_security" spGuidanceResponse__security Security Labels applied to this resource }
    spGuidanceResponse__source, {@enum.value "_source" spGuidanceResponse__source Identifies where the resource comes from }
    spGuidanceResponse__tag, {@enum.value "_tag" spGuidanceResponse__tag Tags applied to this resource }
    spGuidanceResponse__text, {@enum.value "_text" spGuidanceResponse__text Search on the narrative of the resource }
    spGuidanceResponse_Identifier, {@enum.value "identifier" spGuidanceResponse_Identifier The identifier of the guidance response }
    spGuidanceResponse_Patient, {@enum.value "patient" spGuidanceResponse_Patient The identity of a patient to search for guidance response results }
    spGuidanceResponse_Request, {@enum.value "request" spGuidanceResponse_Request The identifier of the request associated with the response }
    spGuidanceResponse_Subject); {@enum.value "subject" spGuidanceResponse_Subject The subject that the guidance response is about }
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
    spHealthcareService__source, {@enum.value "_source" spHealthcareService__source Identifies where the resource comes from }
    spHealthcareService__tag, {@enum.value "_tag" spHealthcareService__tag Tags applied to this resource }
    spHealthcareService__text, {@enum.value "_text" spHealthcareService__text Search on the narrative of the resource }
    spHealthcareService_Active, {@enum.value "active" spHealthcareService_Active The Healthcare Service is currently marked as active }
    spHealthcareService_Category, {@enum.value "category" spHealthcareService_Category Service Category of the Healthcare Service }
    spHealthcareService_Characteristic, {@enum.value "characteristic" spHealthcareService_Characteristic One of the HealthcareService's characteristics }
    spHealthcareService_Endpoint, {@enum.value "endpoint" spHealthcareService_Endpoint Technical endpoints providing access to services operated for the location }
    spHealthcareService_Identifier, {@enum.value "identifier" spHealthcareService_Identifier External identifiers for this item }
    spHealthcareService_Location, {@enum.value "location" spHealthcareService_Location The location of the Healthcare Service }
    spHealthcareService_Name, {@enum.value "name" spHealthcareService_Name A portion of the Healthcare service name }
    spHealthcareService_Organization, {@enum.value "organization" spHealthcareService_Organization The organization that provides this Healthcare Service }
    spHealthcareService_Programname, {@enum.value "programname" spHealthcareService_Programname One of the Program Names serviced by this HealthcareService }
    spHealthcareService_Type); {@enum.value "type" spHealthcareService_Type The type of service provided by this healthcare service }
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
    spImagingStudy__source, {@enum.value "_source" spImagingStudy__source Identifies where the resource comes from }
    spImagingStudy__tag, {@enum.value "_tag" spImagingStudy__tag Tags applied to this resource }
    spImagingStudy__text, {@enum.value "_text" spImagingStudy__text Search on the narrative of the resource }
    spImagingStudy_Accession, {@enum.value "accession" spImagingStudy_Accession The accession identifier for the study }
    spImagingStudy_Basedon, {@enum.value "basedon" spImagingStudy_Basedon The order for the image }
    spImagingStudy_Bodysite, {@enum.value "bodysite" spImagingStudy_Bodysite The body site studied }
    spImagingStudy_Context, {@enum.value "context" spImagingStudy_Context The context of the study }
    spImagingStudy_Dicomclass, {@enum.value "dicom-class" spImagingStudy_Dicomclass The type of the instance }
    spImagingStudy_Endpoint, {@enum.value "endpoint" spImagingStudy_Endpoint The endpoint for te study or series }
    spImagingStudy_Identifier, {@enum.value "identifier" spImagingStudy_Identifier Other identifiers for the Study }
    spImagingStudy_Modality, {@enum.value "modality" spImagingStudy_Modality The modality of the series }
    spImagingStudy_Patient, {@enum.value "patient" spImagingStudy_Patient Who the study is about }
    spImagingStudy_Performer, {@enum.value "performer" spImagingStudy_Performer The person who performed the study }
    spImagingStudy_Reason, {@enum.value "reason" spImagingStudy_Reason The reason for the study }
    spImagingStudy_Series, {@enum.value "series" spImagingStudy_Series The identifier of the series of images }
    spImagingStudy_Started, {@enum.value "started" spImagingStudy_Started When the study was started }
    spImagingStudy_Study, {@enum.value "study" spImagingStudy_Study The study identifier for the image }
    spImagingStudy_Subject, {@enum.value "subject" spImagingStudy_Subject Who the study is about }
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
    spImmunization__source, {@enum.value "_source" spImmunization__source Identifies where the resource comes from }
    spImmunization__tag, {@enum.value "_tag" spImmunization__tag Tags applied to this resource }
    spImmunization__text, {@enum.value "_text" spImmunization__text Search on the narrative of the resource }
    spImmunization_Date, {@enum.value "date" spImmunization_Date Vaccination  (non)-Administration Date }
    spImmunization_Identifier, {@enum.value "identifier" spImmunization_Identifier Business identifier }
    spImmunization_Location, {@enum.value "location" spImmunization_Location The service delivery location or facility in which the vaccine was / was to be administered }
    spImmunization_Lotnumber, {@enum.value "lot-number" spImmunization_Lotnumber Vaccine Lot Number }
    spImmunization_Manufacturer, {@enum.value "manufacturer" spImmunization_Manufacturer Vaccine Manufacturer }
    spImmunization_Patient, {@enum.value "patient" spImmunization_Patient The patient for the vaccination record }
    spImmunization_Practitioner, {@enum.value "practitioner" spImmunization_Practitioner The practitioner who played a role in the vaccination }
    spImmunization_Reason, {@enum.value "reason" spImmunization_Reason Why immunization did or did not occur }
    spImmunization_Status, {@enum.value "status" spImmunization_Status Immunization event status }
    spImmunization_Vaccinecode); {@enum.value "vaccine-code" spImmunization_Vaccinecode Vaccine Product Administered }
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  {@Enum TSearchParamsImmunizationEvaluation
    Search Parameters for ImmunizationEvaluation
  }
  TSearchParamsImmunizationEvaluation = (
    spImmunizationEvaluation__content, {@enum.value "_content" spImmunizationEvaluation__content Search on the entire content of the resource }
    spImmunizationEvaluation__id, {@enum.value "_id" spImmunizationEvaluation__id Logical id of this artifact }
    spImmunizationEvaluation__lastUpdated, {@enum.value "_lastUpdated" spImmunizationEvaluation__lastUpdated When the resource version last changed }
    spImmunizationEvaluation__profile, {@enum.value "_profile" spImmunizationEvaluation__profile Profiles this resource claims to conform to }
    spImmunizationEvaluation__query, {@enum.value "_query" spImmunizationEvaluation__query A custom search profile that describes a specific defined query operation }
    spImmunizationEvaluation__security, {@enum.value "_security" spImmunizationEvaluation__security Security Labels applied to this resource }
    spImmunizationEvaluation__source, {@enum.value "_source" spImmunizationEvaluation__source Identifies where the resource comes from }
    spImmunizationEvaluation__tag, {@enum.value "_tag" spImmunizationEvaluation__tag Tags applied to this resource }
    spImmunizationEvaluation__text, {@enum.value "_text" spImmunizationEvaluation__text Search on the narrative of the resource }
    spImmunizationEvaluation_Date, {@enum.value "date" spImmunizationEvaluation_Date Date the evaluation was generated }
    spImmunizationEvaluation_Dosestatus, {@enum.value "dose-status" spImmunizationEvaluation_Dosestatus The status of the dose relative to published recommendations }
    spImmunizationEvaluation_Identifier, {@enum.value "identifier" spImmunizationEvaluation_Identifier ID of the evaluation }
    spImmunizationEvaluation_Immunizationevent, {@enum.value "immunization-event" spImmunizationEvaluation_Immunizationevent The vaccine administration event being evaluated }
    spImmunizationEvaluation_Patient, {@enum.value "patient" spImmunizationEvaluation_Patient The patient being evaluated }
    spImmunizationEvaluation_Targetdisease); {@enum.value "target-disease" spImmunizationEvaluation_Targetdisease The vaccine preventable disease being evaluated against }
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
    spImmunizationRecommendation__source, {@enum.value "_source" spImmunizationRecommendation__source Identifies where the resource comes from }
    spImmunizationRecommendation__tag, {@enum.value "_tag" spImmunizationRecommendation__tag Tags applied to this resource }
    spImmunizationRecommendation__text, {@enum.value "_text" spImmunizationRecommendation__text Search on the narrative of the resource }
    spImmunizationRecommendation_Date, {@enum.value "date" spImmunizationRecommendation_Date Date recommendation(s) created }
    spImmunizationRecommendation_Dosenumber, {@enum.value "dose-number" spImmunizationRecommendation_Dosenumber Recommended dose number within series }
    spImmunizationRecommendation_Dosesequence, {@enum.value "dose-sequence" spImmunizationRecommendation_Dosesequence Recommended number of doses for immunity }
    spImmunizationRecommendation_Identifier, {@enum.value "identifier" spImmunizationRecommendation_Identifier Business identifier }
    spImmunizationRecommendation_Information, {@enum.value "information" spImmunizationRecommendation_Information Patient observations supporting recommendation }
    spImmunizationRecommendation_Patient, {@enum.value "patient" spImmunizationRecommendation_Patient Who this profile is for }
    spImmunizationRecommendation_Status, {@enum.value "status" spImmunizationRecommendation_Status Vaccine recommendation status }
    spImmunizationRecommendation_Support, {@enum.value "support" spImmunizationRecommendation_Support Past immunizations supporting recommendation }
    spImmunizationRecommendation_Targetdisease, {@enum.value "target-disease" spImmunizationRecommendation_Targetdisease Disease to be immunized against }
    spImmunizationRecommendation_Vaccinetype); {@enum.value "vaccine-type" spImmunizationRecommendation_Vaccinetype Vaccine  or vaccine group recommendation applies to }
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
    spImplementationGuide__source, {@enum.value "_source" spImplementationGuide__source Identifies where the resource comes from }
    spImplementationGuide__tag, {@enum.value "_tag" spImplementationGuide__tag Tags applied to this resource }
    spImplementationGuide__text, {@enum.value "_text" spImplementationGuide__text Search on the narrative of the resource }
    spImplementationGuide_Date, {@enum.value "date" spImplementationGuide_Date The implementation guide output publication date }
    spImplementationGuide_Dependency, {@enum.value "dependency" spImplementationGuide_Dependency Where to find dependency }
    spImplementationGuide_Description, {@enum.value "description" spImplementationGuide_Description The description of the implementation guide output }
    spImplementationGuide_Experimental, {@enum.value "experimental" spImplementationGuide_Experimental For testing purposes, not real usage }
    spImplementationGuide_Jurisdiction, {@enum.value "jurisdiction" spImplementationGuide_Jurisdiction Intended jurisdiction for the implementation guide output }
    spImplementationGuide_Name, {@enum.value "name" spImplementationGuide_Name Computationally friendly name of the implementation guide output }
    spImplementationGuide_Publisher, {@enum.value "publisher" spImplementationGuide_Publisher Name of the publisher of the implementation guide output }
    spImplementationGuide_Resource, {@enum.value "resource" spImplementationGuide_Resource Location of the resource }
    spImplementationGuide_Status, {@enum.value "status" spImplementationGuide_Status The current status of the implementation guide output }
    spImplementationGuide_Url, {@enum.value "url" spImplementationGuide_Url The uri that identifies the implementation guide output }
    spImplementationGuide_Version); {@enum.value "version" spImplementationGuide_Version The business version of the implementation guide output }
{$ENDIF}

{$IFDEF FHIR_IMPLEMENTATIONGUIDEINPUT}
  {@Enum TSearchParamsImplementationGuideInput
    Search Parameters for ImplementationGuideInput
  }
  TSearchParamsImplementationGuideInput = (
    spImplementationGuideInput__content, {@enum.value "_content" spImplementationGuideInput__content Search on the entire content of the resource }
    spImplementationGuideInput__id, {@enum.value "_id" spImplementationGuideInput__id Logical id of this artifact }
    spImplementationGuideInput__lastUpdated, {@enum.value "_lastUpdated" spImplementationGuideInput__lastUpdated When the resource version last changed }
    spImplementationGuideInput__profile, {@enum.value "_profile" spImplementationGuideInput__profile Profiles this resource claims to conform to }
    spImplementationGuideInput__query, {@enum.value "_query" spImplementationGuideInput__query A custom search profile that describes a specific defined query operation }
    spImplementationGuideInput__security, {@enum.value "_security" spImplementationGuideInput__security Security Labels applied to this resource }
    spImplementationGuideInput__source, {@enum.value "_source" spImplementationGuideInput__source Identifies where the resource comes from }
    spImplementationGuideInput__tag, {@enum.value "_tag" spImplementationGuideInput__tag Tags applied to this resource }
    spImplementationGuideInput__text, {@enum.value "_text" spImplementationGuideInput__text Search on the narrative of the resource }
    spImplementationGuideInput_Date, {@enum.value "date" spImplementationGuideInput_Date The implementation guide input publication date }
    spImplementationGuideInput_Dependency, {@enum.value "dependency" spImplementationGuideInput_Dependency Where to find dependency }
    spImplementationGuideInput_Description, {@enum.value "description" spImplementationGuideInput_Description The description of the implementation guide input }
    spImplementationGuideInput_Experimental, {@enum.value "experimental" spImplementationGuideInput_Experimental For testing purposes, not real usage }
    spImplementationGuideInput_Jurisdiction, {@enum.value "jurisdiction" spImplementationGuideInput_Jurisdiction Intended jurisdiction for the implementation guide input }
    spImplementationGuideInput_Name, {@enum.value "name" spImplementationGuideInput_Name Computationally friendly name of the implementation guide input }
    spImplementationGuideInput_Publisher, {@enum.value "publisher" spImplementationGuideInput_Publisher Name of the publisher of the implementation guide input }
    spImplementationGuideInput_Resource, {@enum.value "resource" spImplementationGuideInput_Resource Location of the resource }
    spImplementationGuideInput_Status, {@enum.value "status" spImplementationGuideInput_Status The current status of the implementation guide input }
    spImplementationGuideInput_Url, {@enum.value "url" spImplementationGuideInput_Url The uri that identifies the implementation guide input }
    spImplementationGuideInput_Version); {@enum.value "version" spImplementationGuideInput_Version The business version of the implementation guide input }
{$ENDIF}

{$IFDEF FHIR_IMPLEMENTATIONGUIDEOUTPUT}
  {@Enum TSearchParamsImplementationGuideOutput
    Search Parameters for ImplementationGuideOutput
  }
  TSearchParamsImplementationGuideOutput = (
    spImplementationGuideOutput__content, {@enum.value "_content" spImplementationGuideOutput__content Search on the entire content of the resource }
    spImplementationGuideOutput__id, {@enum.value "_id" spImplementationGuideOutput__id Logical id of this artifact }
    spImplementationGuideOutput__lastUpdated, {@enum.value "_lastUpdated" spImplementationGuideOutput__lastUpdated When the resource version last changed }
    spImplementationGuideOutput__profile, {@enum.value "_profile" spImplementationGuideOutput__profile Profiles this resource claims to conform to }
    spImplementationGuideOutput__query, {@enum.value "_query" spImplementationGuideOutput__query A custom search profile that describes a specific defined query operation }
    spImplementationGuideOutput__security, {@enum.value "_security" spImplementationGuideOutput__security Security Labels applied to this resource }
    spImplementationGuideOutput__source, {@enum.value "_source" spImplementationGuideOutput__source Identifies where the resource comes from }
    spImplementationGuideOutput__tag, {@enum.value "_tag" spImplementationGuideOutput__tag Tags applied to this resource }
    spImplementationGuideOutput__text, {@enum.value "_text" spImplementationGuideOutput__text Search on the narrative of the resource }
    spImplementationGuideOutput_Date, {@enum.value "date" spImplementationGuideOutput_Date The implementation guide output publication date }
    spImplementationGuideOutput_Dependency, {@enum.value "dependency" spImplementationGuideOutput_Dependency Where to find dependency }
    spImplementationGuideOutput_Description, {@enum.value "description" spImplementationGuideOutput_Description The description of the implementation guide output }
    spImplementationGuideOutput_Experimental, {@enum.value "experimental" spImplementationGuideOutput_Experimental For testing purposes, not real usage }
    spImplementationGuideOutput_Jurisdiction, {@enum.value "jurisdiction" spImplementationGuideOutput_Jurisdiction Intended jurisdiction for the implementation guide output }
    spImplementationGuideOutput_Name, {@enum.value "name" spImplementationGuideOutput_Name Computationally friendly name of the implementation guide output }
    spImplementationGuideOutput_Publisher, {@enum.value "publisher" spImplementationGuideOutput_Publisher Name of the publisher of the implementation guide output }
    spImplementationGuideOutput_Resource, {@enum.value "resource" spImplementationGuideOutput_Resource Location of the resource }
    spImplementationGuideOutput_Status, {@enum.value "status" spImplementationGuideOutput_Status The current status of the implementation guide output }
    spImplementationGuideOutput_Url, {@enum.value "url" spImplementationGuideOutput_Url The uri that identifies the implementation guide output }
    spImplementationGuideOutput_Version); {@enum.value "version" spImplementationGuideOutput_Version The business version of the implementation guide output }
{$ENDIF}

{$IFDEF FHIR_INVOICE}
  {@Enum TSearchParamsInvoice
    Search Parameters for Invoice
  }
  TSearchParamsInvoice = (
    spInvoice__content, {@enum.value "_content" spInvoice__content Search on the entire content of the resource }
    spInvoice__id, {@enum.value "_id" spInvoice__id Logical id of this artifact }
    spInvoice__lastUpdated, {@enum.value "_lastUpdated" spInvoice__lastUpdated When the resource version last changed }
    spInvoice__profile, {@enum.value "_profile" spInvoice__profile Profiles this resource claims to conform to }
    spInvoice__query, {@enum.value "_query" spInvoice__query A custom search profile that describes a specific defined query operation }
    spInvoice__security, {@enum.value "_security" spInvoice__security Security Labels applied to this resource }
    spInvoice__source, {@enum.value "_source" spInvoice__source Identifies where the resource comes from }
    spInvoice__tag, {@enum.value "_tag" spInvoice__tag Tags applied to this resource }
    spInvoice__text, {@enum.value "_text" spInvoice__text Search on the narrative of the resource }
    spInvoice_Account, {@enum.value "account" spInvoice_Account Account that is being balanced }
    spInvoice_Date, {@enum.value "date" spInvoice_Date Invoice date / posting date }
    spInvoice_Identifier, {@enum.value "identifier" spInvoice_Identifier Business Identifier for item }
    spInvoice_Issuer, {@enum.value "issuer" spInvoice_Issuer Issuing Organization of Invoice }
    spInvoice_Participant, {@enum.value "participant" spInvoice_Participant Individual who was involved }
    spInvoice_Participantrole, {@enum.value "participant-role" spInvoice_Participantrole Type of involevent in creation of this Invoice }
    spInvoice_Patient, {@enum.value "patient" spInvoice_Patient Recipient(s) of goods and services }
    spInvoice_Recipient, {@enum.value "recipient" spInvoice_Recipient Recipient of this invoice }
    spInvoice_Status, {@enum.value "status" spInvoice_Status draft | issued | balanced | cancelled | entered-in-error }
    spInvoice_Subject, {@enum.value "subject" spInvoice_Subject Recipient(s) of goods and services }
    spInvoice_Totalgross, {@enum.value "totalgross" spInvoice_Totalgross Gross toal of this Invoice }
    spInvoice_Totalnet, {@enum.value "totalnet" spInvoice_Totalnet Net total of this Invoice }
    spInvoice_Type); {@enum.value "type" spInvoice_Type Type of Invoice }
{$ENDIF}

{$IFDEF FHIR_ITEMINSTANCE}
  {@Enum TSearchParamsItemInstance
    Search Parameters for ItemInstance
  }
  TSearchParamsItemInstance = (
    spItemInstance__content, {@enum.value "_content" spItemInstance__content Search on the entire content of the resource }
    spItemInstance__id, {@enum.value "_id" spItemInstance__id Logical id of this artifact }
    spItemInstance__lastUpdated, {@enum.value "_lastUpdated" spItemInstance__lastUpdated When the resource version last changed }
    spItemInstance__profile, {@enum.value "_profile" spItemInstance__profile Profiles this resource claims to conform to }
    spItemInstance__query, {@enum.value "_query" spItemInstance__query A custom search profile that describes a specific defined query operation }
    spItemInstance__security, {@enum.value "_security" spItemInstance__security Security Labels applied to this resource }
    spItemInstance__source, {@enum.value "_source" spItemInstance__source Identifies where the resource comes from }
    spItemInstance__tag, {@enum.value "_tag" spItemInstance__tag Tags applied to this resource }
    spItemInstance__text, {@enum.value "_text" spItemInstance__text Search on the narrative of the resource }
    spItemInstance_Subject); {@enum.value "subject" spItemInstance_Subject The identifier of the patient who has devices assigned to }
{$ENDIF}

{$IFDEF FHIR_LIBRARY}
  {@Enum TSearchParamsLibrary
    Search Parameters for Library
  }
  TSearchParamsLibrary = (
    spLibrary__content, {@enum.value "_content" spLibrary__content Search on the entire content of the resource }
    spLibrary__id, {@enum.value "_id" spLibrary__id Logical id of this artifact }
    spLibrary__lastUpdated, {@enum.value "_lastUpdated" spLibrary__lastUpdated When the resource version last changed }
    spLibrary__profile, {@enum.value "_profile" spLibrary__profile Profiles this resource claims to conform to }
    spLibrary__query, {@enum.value "_query" spLibrary__query A custom search profile that describes a specific defined query operation }
    spLibrary__security, {@enum.value "_security" spLibrary__security Security Labels applied to this resource }
    spLibrary__source, {@enum.value "_source" spLibrary__source Identifies where the resource comes from }
    spLibrary__tag, {@enum.value "_tag" spLibrary__tag Tags applied to this resource }
    spLibrary__text, {@enum.value "_text" spLibrary__text Search on the narrative of the resource }
    spLibrary_Composedof, {@enum.value "composed-of" spLibrary_Composedof What resource is being referenced }
    spLibrary_Date, {@enum.value "date" spLibrary_Date The library publication date }
    spLibrary_Dependson, {@enum.value "depends-on" spLibrary_Dependson What resource is being referenced }
    spLibrary_Derivedfrom, {@enum.value "derived-from" spLibrary_Derivedfrom What resource is being referenced }
    spLibrary_Description, {@enum.value "description" spLibrary_Description The description of the library }
    spLibrary_Effective, {@enum.value "effective" spLibrary_Effective The time during which the library is intended to be in use }
    spLibrary_Identifier, {@enum.value "identifier" spLibrary_Identifier External identifier for the library }
    spLibrary_Jurisdiction, {@enum.value "jurisdiction" spLibrary_Jurisdiction Intended jurisdiction for the library }
    spLibrary_Name, {@enum.value "name" spLibrary_Name Computationally friendly name of the library }
    spLibrary_Predecessor, {@enum.value "predecessor" spLibrary_Predecessor What resource is being referenced }
    spLibrary_Publisher, {@enum.value "publisher" spLibrary_Publisher Name of the publisher of the library }
    spLibrary_Status, {@enum.value "status" spLibrary_Status The current status of the library }
    spLibrary_Successor, {@enum.value "successor" spLibrary_Successor What resource is being referenced }
    spLibrary_Title, {@enum.value "title" spLibrary_Title The human-friendly name of the library }
    spLibrary_Topic, {@enum.value "topic" spLibrary_Topic Topics associated with the module }
    spLibrary_Type, {@enum.value "type" spLibrary_Type The type of the library (e.g. logic-library, model-definition, asset-collection, module-definition) }
    spLibrary_Url, {@enum.value "url" spLibrary_Url The uri that identifies the library }
    spLibrary_Version); {@enum.value "version" spLibrary_Version The business version of the library }
{$ENDIF}

{$IFDEF FHIR_LINKAGE}
  {@Enum TSearchParamsLinkage
    Search Parameters for Linkage
  }
  TSearchParamsLinkage = (
    spLinkage__content, {@enum.value "_content" spLinkage__content Search on the entire content of the resource }
    spLinkage__id, {@enum.value "_id" spLinkage__id Logical id of this artifact }
    spLinkage__lastUpdated, {@enum.value "_lastUpdated" spLinkage__lastUpdated When the resource version last changed }
    spLinkage__profile, {@enum.value "_profile" spLinkage__profile Profiles this resource claims to conform to }
    spLinkage__query, {@enum.value "_query" spLinkage__query A custom search profile that describes a specific defined query operation }
    spLinkage__security, {@enum.value "_security" spLinkage__security Security Labels applied to this resource }
    spLinkage__source, {@enum.value "_source" spLinkage__source Identifies where the resource comes from }
    spLinkage__tag, {@enum.value "_tag" spLinkage__tag Tags applied to this resource }
    spLinkage__text, {@enum.value "_text" spLinkage__text Search on the narrative of the resource }
    spLinkage_Author, {@enum.value "author" spLinkage_Author Author of the Linkage }
    spLinkage_Item, {@enum.value "item" spLinkage_Item Matches on any item in the Linkage }
    spLinkage_Source); {@enum.value "source" spLinkage_Source Matches on any item in the Linkage with a type of 'source' }
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
    spList__source, {@enum.value "_source" spList__source Identifies where the resource comes from }
    spList__tag, {@enum.value "_tag" spList__tag Tags applied to this resource }
    spList__text, {@enum.value "_text" spList__text Search on the narrative of the resource }
    spList_Code, {@enum.value "code" spList_Code What the purpose of this list is }
    spList_Date, {@enum.value "date" spList_Date When the list was prepared }
    spList_Emptyreason, {@enum.value "empty-reason" spList_Emptyreason Why list is empty }
    spList_Encounter, {@enum.value "encounter" spList_Encounter Context in which list created }
    spList_Identifier, {@enum.value "identifier" spList_Identifier Business identifier }
    spList_Item, {@enum.value "item" spList_Item Actual entry }
    spList_Notes, {@enum.value "notes" spList_Notes The annotation  - text content }
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
    spLocation__source, {@enum.value "_source" spLocation__source Identifies where the resource comes from }
    spLocation__tag, {@enum.value "_tag" spLocation__tag Tags applied to this resource }
    spLocation__text, {@enum.value "_text" spLocation__text Search on the narrative of the resource }
    spLocation_Address, {@enum.value "address" spLocation_Address A (part of the) address of the location }
    spLocation_Addresscity, {@enum.value "address-city" spLocation_Addresscity A city specified in an address }
    spLocation_Addresscountry, {@enum.value "address-country" spLocation_Addresscountry A country specified in an address }
    spLocation_Addresspostalcode, {@enum.value "address-postalcode" spLocation_Addresspostalcode A postal code specified in an address }
    spLocation_Addressstate, {@enum.value "address-state" spLocation_Addressstate A state specified in an address }
    spLocation_Addressuse, {@enum.value "address-use" spLocation_Addressuse A use code specified in an address }
    spLocation_Endpoint, {@enum.value "endpoint" spLocation_Endpoint Technical endpoints providing access to services operated for the location }
    spLocation_Identifier, {@enum.value "identifier" spLocation_Identifier An identifier for the location }
    spLocation_Name, {@enum.value "name" spLocation_Name A portion of the location's name or alias }
    spLocation_Near, {@enum.value "near" spLocation_Near The coordinates expressed as [latitude]:[longitude] (using the WGS84 datum, see notes) to find locations near to (servers may search using a square rather than a circle for efficiency)

Requires the near-distance parameter to be provided also }
    spLocation_Neardistance, {@enum.value "near-distance" spLocation_Neardistance A distance quantity to limit the near search to locations within a specific distance

Requires the near parameter to also be included }
    spLocation_Operationalstatus, {@enum.value "operational-status" spLocation_Operationalstatus Searches for locations (typically bed/room) that have an operational status (e.g. contaminated, housekeeping) }
    spLocation_Organization, {@enum.value "organization" spLocation_Organization Searches for locations that are managed by the provided organization }
    spLocation_Partof, {@enum.value "partof" spLocation_Partof A location of which this location is a part }
    spLocation_Status, {@enum.value "status" spLocation_Status Searches for locations with a specific kind of status }
    spLocation_Type); {@enum.value "type" spLocation_Type A code for the type of location }
{$ENDIF}

{$IFDEF FHIR_MEASURE}
  {@Enum TSearchParamsMeasure
    Search Parameters for Measure
  }
  TSearchParamsMeasure = (
    spMeasure__content, {@enum.value "_content" spMeasure__content Search on the entire content of the resource }
    spMeasure__id, {@enum.value "_id" spMeasure__id Logical id of this artifact }
    spMeasure__lastUpdated, {@enum.value "_lastUpdated" spMeasure__lastUpdated When the resource version last changed }
    spMeasure__profile, {@enum.value "_profile" spMeasure__profile Profiles this resource claims to conform to }
    spMeasure__query, {@enum.value "_query" spMeasure__query A custom search profile that describes a specific defined query operation }
    spMeasure__security, {@enum.value "_security" spMeasure__security Security Labels applied to this resource }
    spMeasure__source, {@enum.value "_source" spMeasure__source Identifies where the resource comes from }
    spMeasure__tag, {@enum.value "_tag" spMeasure__tag Tags applied to this resource }
    spMeasure__text, {@enum.value "_text" spMeasure__text Search on the narrative of the resource }
    spMeasure_Composedof, {@enum.value "composed-of" spMeasure_Composedof What resource is being referenced }
    spMeasure_Date, {@enum.value "date" spMeasure_Date The measure publication date }
    spMeasure_Dependson, {@enum.value "depends-on" spMeasure_Dependson What resource is being referenced }
    spMeasure_Derivedfrom, {@enum.value "derived-from" spMeasure_Derivedfrom What resource is being referenced }
    spMeasure_Description, {@enum.value "description" spMeasure_Description The description of the measure }
    spMeasure_Effective, {@enum.value "effective" spMeasure_Effective The time during which the measure is intended to be in use }
    spMeasure_Identifier, {@enum.value "identifier" spMeasure_Identifier External identifier for the measure }
    spMeasure_Jurisdiction, {@enum.value "jurisdiction" spMeasure_Jurisdiction Intended jurisdiction for the measure }
    spMeasure_Name, {@enum.value "name" spMeasure_Name Computationally friendly name of the measure }
    spMeasure_Predecessor, {@enum.value "predecessor" spMeasure_Predecessor What resource is being referenced }
    spMeasure_Publisher, {@enum.value "publisher" spMeasure_Publisher Name of the publisher of the measure }
    spMeasure_Status, {@enum.value "status" spMeasure_Status The current status of the measure }
    spMeasure_Successor, {@enum.value "successor" spMeasure_Successor What resource is being referenced }
    spMeasure_Title, {@enum.value "title" spMeasure_Title The human-friendly name of the measure }
    spMeasure_Topic, {@enum.value "topic" spMeasure_Topic Topics associated with the module }
    spMeasure_Url, {@enum.value "url" spMeasure_Url The uri that identifies the measure }
    spMeasure_Version); {@enum.value "version" spMeasure_Version The business version of the measure }
{$ENDIF}

{$IFDEF FHIR_MEASUREREPORT}
  {@Enum TSearchParamsMeasureReport
    Search Parameters for MeasureReport
  }
  TSearchParamsMeasureReport = (
    spMeasureReport__content, {@enum.value "_content" spMeasureReport__content Search on the entire content of the resource }
    spMeasureReport__id, {@enum.value "_id" spMeasureReport__id Logical id of this artifact }
    spMeasureReport__lastUpdated, {@enum.value "_lastUpdated" spMeasureReport__lastUpdated When the resource version last changed }
    spMeasureReport__profile, {@enum.value "_profile" spMeasureReport__profile Profiles this resource claims to conform to }
    spMeasureReport__query, {@enum.value "_query" spMeasureReport__query A custom search profile that describes a specific defined query operation }
    spMeasureReport__security, {@enum.value "_security" spMeasureReport__security Security Labels applied to this resource }
    spMeasureReport__source, {@enum.value "_source" spMeasureReport__source Identifies where the resource comes from }
    spMeasureReport__tag, {@enum.value "_tag" spMeasureReport__tag Tags applied to this resource }
    spMeasureReport__text, {@enum.value "_text" spMeasureReport__text Search on the narrative of the resource }
    spMeasureReport_Identifier, {@enum.value "identifier" spMeasureReport_Identifier External identifier of the measure report to be returned }
    spMeasureReport_Patient, {@enum.value "patient" spMeasureReport_Patient The identity of a patient to search for individual measure report results for }
    spMeasureReport_Status, {@enum.value "status" spMeasureReport_Status The status of the measure report }
    spMeasureReport_Subject); {@enum.value "subject" spMeasureReport_Subject The identity of a subject to search for individual measure report results for }
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
    spMedia__source, {@enum.value "_source" spMedia__source Identifies where the resource comes from }
    spMedia__tag, {@enum.value "_tag" spMedia__tag Tags applied to this resource }
    spMedia__text, {@enum.value "_text" spMedia__text Search on the narrative of the resource }
    spMedia_Basedon, {@enum.value "based-on" spMedia_Basedon Procedure that caused this media to be created }
    spMedia_Category, {@enum.value "category" spMedia_Category Classification of  type of media }
    spMedia_Context, {@enum.value "context" spMedia_Context Encounter / Episode associated with media }
    spMedia_Created, {@enum.value "created" spMedia_Created When Media was collected }
    spMedia_Device, {@enum.value "device" spMedia_Device Observing Device }
    spMedia_Identifier, {@enum.value "identifier" spMedia_Identifier Identifier(s) for the image }
    spMedia_Modality, {@enum.value "modality" spMedia_Modality The type of acquisition equipment/process }
    spMedia_Operator, {@enum.value "operator" spMedia_Operator The person who generated the image }
    spMedia_Patient, {@enum.value "patient" spMedia_Patient Who/What this Media is a record of }
    spMedia_Site, {@enum.value "site" spMedia_Site Observed body part }
    spMedia_Status, {@enum.value "status" spMedia_Status preparation | in-progress | not-done | suspended | aborted | completed | entered-in-error | unknown }
    spMedia_Subject, {@enum.value "subject" spMedia_Subject Who/What this Media is a record of }
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
    spMedication__source, {@enum.value "_source" spMedication__source Identifies where the resource comes from }
    spMedication__tag, {@enum.value "_tag" spMedication__tag Tags applied to this resource }
    spMedication__text, {@enum.value "_text" spMedication__text Search on the narrative of the resource }
    spMedication_Code, {@enum.value "code" spMedication_Code Codes that identify this medication }
    spMedication_Form, {@enum.value "form" spMedication_Form powder | tablets | capsule + }
    spMedication_Ingredient, {@enum.value "ingredient" spMedication_Ingredient The product contained }
    spMedication_Ingredientcode, {@enum.value "ingredient-code" spMedication_Ingredientcode The product contained }
    spMedication_Manufacturer, {@enum.value "manufacturer" spMedication_Manufacturer Manufacturer of the item }
    spMedication_Status); {@enum.value "status" spMedication_Status active | inactive | entered-in-error }
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
    spMedicationAdministration__source, {@enum.value "_source" spMedicationAdministration__source Identifies where the resource comes from }
    spMedicationAdministration__tag, {@enum.value "_tag" spMedicationAdministration__tag Tags applied to this resource }
    spMedicationAdministration__text, {@enum.value "_text" spMedicationAdministration__text Search on the narrative of the resource }
    spMedicationAdministration_Code, {@enum.value "code" spMedicationAdministration_Code Return administrations of this medication code }
    spMedicationAdministration_Context, {@enum.value "context" spMedicationAdministration_Context Return administrations that share this encounter or episode of care }
    spMedicationAdministration_Device, {@enum.value "device" spMedicationAdministration_Device Return administrations with this administration device identity }
    spMedicationAdministration_Effectivetime, {@enum.value "effective-time" spMedicationAdministration_Effectivetime Date administration happened (or did not happen) }
    spMedicationAdministration_Identifier, {@enum.value "identifier" spMedicationAdministration_Identifier Return administrations with this external identifier }
    spMedicationAdministration_Medication, {@enum.value "medication" spMedicationAdministration_Medication Return administrations of this medication resource }
    spMedicationAdministration_Patient, {@enum.value "patient" spMedicationAdministration_Patient The identity of a patient to list administrations  for }
    spMedicationAdministration_Performer, {@enum.value "performer" spMedicationAdministration_Performer The identify of the individual who administered the medication }
    spMedicationAdministration_Reasongiven, {@enum.value "reason-given" spMedicationAdministration_Reasongiven Reasons for administering the medication }
    spMedicationAdministration_Reasonnotgiven, {@enum.value "reason-not-given" spMedicationAdministration_Reasonnotgiven Reasons for not administering the medication }
    spMedicationAdministration_Request, {@enum.value "request" spMedicationAdministration_Request The identity of a request to list administrations from }
    spMedicationAdministration_Status, {@enum.value "status" spMedicationAdministration_Status MedicationAdministration event status (for example one of active/paused/completed/nullified) }
    spMedicationAdministration_Subject); {@enum.value "subject" spMedicationAdministration_Subject The identify of the individual or group to list administrations for }
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
    spMedicationDispense__source, {@enum.value "_source" spMedicationDispense__source Identifies where the resource comes from }
    spMedicationDispense__tag, {@enum.value "_tag" spMedicationDispense__tag Tags applied to this resource }
    spMedicationDispense__text, {@enum.value "_text" spMedicationDispense__text Search on the narrative of the resource }
    spMedicationDispense_Code, {@enum.value "code" spMedicationDispense_Code Return dispenses of this medicine code }
    spMedicationDispense_Context, {@enum.value "context" spMedicationDispense_Context Returns dispenses with a specific context (episode or episode of care) }
    spMedicationDispense_Destination, {@enum.value "destination" spMedicationDispense_Destination Return dispenses that should be sent to a specific destination }
    spMedicationDispense_Identifier, {@enum.value "identifier" spMedicationDispense_Identifier Return dispenses with this external identifier }
    spMedicationDispense_Medication, {@enum.value "medication" spMedicationDispense_Medication Return dispenses of this medicine resource }
    spMedicationDispense_Patient, {@enum.value "patient" spMedicationDispense_Patient The identity of a patient to list dispenses  for }
    spMedicationDispense_Performer, {@enum.value "performer" spMedicationDispense_Performer Return dispenses performed by a specific individual }
    spMedicationDispense_Prescription, {@enum.value "prescription" spMedicationDispense_Prescription The identity of a prescription to list dispenses from }
    spMedicationDispense_Receiver, {@enum.value "receiver" spMedicationDispense_Receiver The identity of a receiver to list dispenses for }
    spMedicationDispense_Responsibleparty, {@enum.value "responsibleparty" spMedicationDispense_Responsibleparty Return dispenses with the specified responsible party }
    spMedicationDispense_Status, {@enum.value "status" spMedicationDispense_Status Return dispenses with a specified dispense status }
    spMedicationDispense_Subject, {@enum.value "subject" spMedicationDispense_Subject The identity of a patient to list dispenses  for }
    spMedicationDispense_Type, {@enum.value "type" spMedicationDispense_Type Return dispenses of a specific type }
    spMedicationDispense_Whenhandedover, {@enum.value "whenhandedover" spMedicationDispense_Whenhandedover Returns dispenses handed over on this date }
    spMedicationDispense_Whenprepared); {@enum.value "whenprepared" spMedicationDispense_Whenprepared Returns dispenses prepared on this date }
{$ENDIF}

{$IFDEF FHIR_MEDICATIONREQUEST}
  {@Enum TSearchParamsMedicationRequest
    Search Parameters for MedicationRequest
  }
  TSearchParamsMedicationRequest = (
    spMedicationRequest__content, {@enum.value "_content" spMedicationRequest__content Search on the entire content of the resource }
    spMedicationRequest__id, {@enum.value "_id" spMedicationRequest__id Logical id of this artifact }
    spMedicationRequest__lastUpdated, {@enum.value "_lastUpdated" spMedicationRequest__lastUpdated When the resource version last changed }
    spMedicationRequest__profile, {@enum.value "_profile" spMedicationRequest__profile Profiles this resource claims to conform to }
    spMedicationRequest__query, {@enum.value "_query" spMedicationRequest__query A custom search profile that describes a specific defined query operation }
    spMedicationRequest__security, {@enum.value "_security" spMedicationRequest__security Security Labels applied to this resource }
    spMedicationRequest__source, {@enum.value "_source" spMedicationRequest__source Identifies where the resource comes from }
    spMedicationRequest__tag, {@enum.value "_tag" spMedicationRequest__tag Tags applied to this resource }
    spMedicationRequest__text, {@enum.value "_text" spMedicationRequest__text Search on the narrative of the resource }
    spMedicationRequest_Authoredon, {@enum.value "authoredon" spMedicationRequest_Authoredon Return prescriptions written on this date }
    spMedicationRequest_Category, {@enum.value "category" spMedicationRequest_Category Returns prescriptions with different categories }
    spMedicationRequest_Code, {@enum.value "code" spMedicationRequest_Code Return prescriptions of this medication code }
    spMedicationRequest_Context, {@enum.value "context" spMedicationRequest_Context Return prescriptions with this encounter or episode of care identifier }
    spMedicationRequest_Date, {@enum.value "date" spMedicationRequest_Date Returns medication request to be administered on a specific date }
    spMedicationRequest_Identifier, {@enum.value "identifier" spMedicationRequest_Identifier Return prescriptions with this external identifier }
    spMedicationRequest_Intendeddispenser, {@enum.value "intended-dispenser" spMedicationRequest_Intendeddispenser Returns prescriptions intended to be dispensed by this Organization }
    spMedicationRequest_Intendedperformer, {@enum.value "intended-performer" spMedicationRequest_Intendedperformer Returns the intended performer of the administration of the medication request }
    spMedicationRequest_Intendedperformertype, {@enum.value "intended-performertype" spMedicationRequest_Intendedperformertype Returns requests for a specific type of performer }
    spMedicationRequest_Intent, {@enum.value "intent" spMedicationRequest_Intent Returns prescriptions with different intents }
    spMedicationRequest_Medication, {@enum.value "medication" spMedicationRequest_Medication Return prescriptions of this medication reference }
    spMedicationRequest_Patient, {@enum.value "patient" spMedicationRequest_Patient Returns prescriptions for a specific patient }
    spMedicationRequest_Priority, {@enum.value "priority" spMedicationRequest_Priority Returns prescriptions with different priorities }
    spMedicationRequest_Requester, {@enum.value "requester" spMedicationRequest_Requester Returns prescriptions prescribed by this prescriber }
    spMedicationRequest_Status, {@enum.value "status" spMedicationRequest_Status Status of the prescription }
    spMedicationRequest_Subject); {@enum.value "subject" spMedicationRequest_Subject The identity of a patient to list orders  for }
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
    spMedicationStatement__source, {@enum.value "_source" spMedicationStatement__source Identifies where the resource comes from }
    spMedicationStatement__tag, {@enum.value "_tag" spMedicationStatement__tag Tags applied to this resource }
    spMedicationStatement__text, {@enum.value "_text" spMedicationStatement__text Search on the narrative of the resource }
    spMedicationStatement_Category, {@enum.value "category" spMedicationStatement_Category Returns statements of this category of medicationstatement }
    spMedicationStatement_Code, {@enum.value "code" spMedicationStatement_Code Return statements of this medication code }
    spMedicationStatement_Context, {@enum.value "context" spMedicationStatement_Context Returns statements for a specific context (episode or episode of Care). }
    spMedicationStatement_Effective, {@enum.value "effective" spMedicationStatement_Effective Date when patient was taking (or not taking) the medication }
    spMedicationStatement_Identifier, {@enum.value "identifier" spMedicationStatement_Identifier Return statements with this external identifier }
    spMedicationStatement_Medication, {@enum.value "medication" spMedicationStatement_Medication Return statements of this medication reference }
    spMedicationStatement_Partof, {@enum.value "part-of" spMedicationStatement_Partof Returns statements that are part of another event. }
    spMedicationStatement_Patient, {@enum.value "patient" spMedicationStatement_Patient Returns statements for a specific patient. }
    spMedicationStatement_Source, {@enum.value "source" spMedicationStatement_Source Who or where the information in the statement came from }
    spMedicationStatement_Status, {@enum.value "status" spMedicationStatement_Status Return statements that match the given status }
    spMedicationStatement_Subject); {@enum.value "subject" spMedicationStatement_Subject The identity of a patient, animal or group to list statements for }
{$ENDIF}

{$IFDEF FHIR_MEDICINALPRODUCT}
  {@Enum TSearchParamsMedicinalProduct
    Search Parameters for MedicinalProduct
  }
  TSearchParamsMedicinalProduct = (
    spMedicinalProduct__content, {@enum.value "_content" spMedicinalProduct__content Search on the entire content of the resource }
    spMedicinalProduct__id, {@enum.value "_id" spMedicinalProduct__id Logical id of this artifact }
    spMedicinalProduct__lastUpdated, {@enum.value "_lastUpdated" spMedicinalProduct__lastUpdated When the resource version last changed }
    spMedicinalProduct__profile, {@enum.value "_profile" spMedicinalProduct__profile Profiles this resource claims to conform to }
    spMedicinalProduct__query, {@enum.value "_query" spMedicinalProduct__query A custom search profile that describes a specific defined query operation }
    spMedicinalProduct__security, {@enum.value "_security" spMedicinalProduct__security Security Labels applied to this resource }
    spMedicinalProduct__source, {@enum.value "_source" spMedicinalProduct__source Identifies where the resource comes from }
    spMedicinalProduct__tag, {@enum.value "_tag" spMedicinalProduct__tag Tags applied to this resource }
    spMedicinalProduct__text); {@enum.value "_text" spMedicinalProduct__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_MEDICINALPRODUCTAUTHORIZATION}
  {@Enum TSearchParamsMedicinalProductAuthorization
    Search Parameters for MedicinalProductAuthorization
  }
  TSearchParamsMedicinalProductAuthorization = (
    spMedicinalProductAuthorization__content, {@enum.value "_content" spMedicinalProductAuthorization__content Search on the entire content of the resource }
    spMedicinalProductAuthorization__id, {@enum.value "_id" spMedicinalProductAuthorization__id Logical id of this artifact }
    spMedicinalProductAuthorization__lastUpdated, {@enum.value "_lastUpdated" spMedicinalProductAuthorization__lastUpdated When the resource version last changed }
    spMedicinalProductAuthorization__profile, {@enum.value "_profile" spMedicinalProductAuthorization__profile Profiles this resource claims to conform to }
    spMedicinalProductAuthorization__query, {@enum.value "_query" spMedicinalProductAuthorization__query A custom search profile that describes a specific defined query operation }
    spMedicinalProductAuthorization__security, {@enum.value "_security" spMedicinalProductAuthorization__security Security Labels applied to this resource }
    spMedicinalProductAuthorization__source, {@enum.value "_source" spMedicinalProductAuthorization__source Identifies where the resource comes from }
    spMedicinalProductAuthorization__tag, {@enum.value "_tag" spMedicinalProductAuthorization__tag Tags applied to this resource }
    spMedicinalProductAuthorization__text); {@enum.value "_text" spMedicinalProductAuthorization__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_MEDICINALPRODUCTCLINICALS}
  {@Enum TSearchParamsMedicinalProductClinicals
    Search Parameters for MedicinalProductClinicals
  }
  TSearchParamsMedicinalProductClinicals = (
    spMedicinalProductClinicals__content, {@enum.value "_content" spMedicinalProductClinicals__content Search on the entire content of the resource }
    spMedicinalProductClinicals__id, {@enum.value "_id" spMedicinalProductClinicals__id Logical id of this artifact }
    spMedicinalProductClinicals__lastUpdated, {@enum.value "_lastUpdated" spMedicinalProductClinicals__lastUpdated When the resource version last changed }
    spMedicinalProductClinicals__profile, {@enum.value "_profile" spMedicinalProductClinicals__profile Profiles this resource claims to conform to }
    spMedicinalProductClinicals__query, {@enum.value "_query" spMedicinalProductClinicals__query A custom search profile that describes a specific defined query operation }
    spMedicinalProductClinicals__security, {@enum.value "_security" spMedicinalProductClinicals__security Security Labels applied to this resource }
    spMedicinalProductClinicals__source, {@enum.value "_source" spMedicinalProductClinicals__source Identifies where the resource comes from }
    spMedicinalProductClinicals__tag, {@enum.value "_tag" spMedicinalProductClinicals__tag Tags applied to this resource }
    spMedicinalProductClinicals__text); {@enum.value "_text" spMedicinalProductClinicals__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_MEDICINALPRODUCTDEVICESPEC}
  {@Enum TSearchParamsMedicinalProductDeviceSpec
    Search Parameters for MedicinalProductDeviceSpec
  }
  TSearchParamsMedicinalProductDeviceSpec = (
    spMedicinalProductDeviceSpec__content, {@enum.value "_content" spMedicinalProductDeviceSpec__content Search on the entire content of the resource }
    spMedicinalProductDeviceSpec__id, {@enum.value "_id" spMedicinalProductDeviceSpec__id Logical id of this artifact }
    spMedicinalProductDeviceSpec__lastUpdated, {@enum.value "_lastUpdated" spMedicinalProductDeviceSpec__lastUpdated When the resource version last changed }
    spMedicinalProductDeviceSpec__profile, {@enum.value "_profile" spMedicinalProductDeviceSpec__profile Profiles this resource claims to conform to }
    spMedicinalProductDeviceSpec__query, {@enum.value "_query" spMedicinalProductDeviceSpec__query A custom search profile that describes a specific defined query operation }
    spMedicinalProductDeviceSpec__security, {@enum.value "_security" spMedicinalProductDeviceSpec__security Security Labels applied to this resource }
    spMedicinalProductDeviceSpec__source, {@enum.value "_source" spMedicinalProductDeviceSpec__source Identifies where the resource comes from }
    spMedicinalProductDeviceSpec__tag, {@enum.value "_tag" spMedicinalProductDeviceSpec__tag Tags applied to this resource }
    spMedicinalProductDeviceSpec__text); {@enum.value "_text" spMedicinalProductDeviceSpec__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_MEDICINALPRODUCTINGREDIENT}
  {@Enum TSearchParamsMedicinalProductIngredient
    Search Parameters for MedicinalProductIngredient
  }
  TSearchParamsMedicinalProductIngredient = (
    spMedicinalProductIngredient__content, {@enum.value "_content" spMedicinalProductIngredient__content Search on the entire content of the resource }
    spMedicinalProductIngredient__id, {@enum.value "_id" spMedicinalProductIngredient__id Logical id of this artifact }
    spMedicinalProductIngredient__lastUpdated, {@enum.value "_lastUpdated" spMedicinalProductIngredient__lastUpdated When the resource version last changed }
    spMedicinalProductIngredient__profile, {@enum.value "_profile" spMedicinalProductIngredient__profile Profiles this resource claims to conform to }
    spMedicinalProductIngredient__query, {@enum.value "_query" spMedicinalProductIngredient__query A custom search profile that describes a specific defined query operation }
    spMedicinalProductIngredient__security, {@enum.value "_security" spMedicinalProductIngredient__security Security Labels applied to this resource }
    spMedicinalProductIngredient__source, {@enum.value "_source" spMedicinalProductIngredient__source Identifies where the resource comes from }
    spMedicinalProductIngredient__tag, {@enum.value "_tag" spMedicinalProductIngredient__tag Tags applied to this resource }
    spMedicinalProductIngredient__text); {@enum.value "_text" spMedicinalProductIngredient__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_MEDICINALPRODUCTPACKAGED}
  {@Enum TSearchParamsMedicinalProductPackaged
    Search Parameters for MedicinalProductPackaged
  }
  TSearchParamsMedicinalProductPackaged = (
    spMedicinalProductPackaged__content, {@enum.value "_content" spMedicinalProductPackaged__content Search on the entire content of the resource }
    spMedicinalProductPackaged__id, {@enum.value "_id" spMedicinalProductPackaged__id Logical id of this artifact }
    spMedicinalProductPackaged__lastUpdated, {@enum.value "_lastUpdated" spMedicinalProductPackaged__lastUpdated When the resource version last changed }
    spMedicinalProductPackaged__profile, {@enum.value "_profile" spMedicinalProductPackaged__profile Profiles this resource claims to conform to }
    spMedicinalProductPackaged__query, {@enum.value "_query" spMedicinalProductPackaged__query A custom search profile that describes a specific defined query operation }
    spMedicinalProductPackaged__security, {@enum.value "_security" spMedicinalProductPackaged__security Security Labels applied to this resource }
    spMedicinalProductPackaged__source, {@enum.value "_source" spMedicinalProductPackaged__source Identifies where the resource comes from }
    spMedicinalProductPackaged__tag, {@enum.value "_tag" spMedicinalProductPackaged__tag Tags applied to this resource }
    spMedicinalProductPackaged__text); {@enum.value "_text" spMedicinalProductPackaged__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}
  {@Enum TSearchParamsMedicinalProductPharmaceutical
    Search Parameters for MedicinalProductPharmaceutical
  }
  TSearchParamsMedicinalProductPharmaceutical = (
    spMedicinalProductPharmaceutical__content, {@enum.value "_content" spMedicinalProductPharmaceutical__content Search on the entire content of the resource }
    spMedicinalProductPharmaceutical__id, {@enum.value "_id" spMedicinalProductPharmaceutical__id Logical id of this artifact }
    spMedicinalProductPharmaceutical__lastUpdated, {@enum.value "_lastUpdated" spMedicinalProductPharmaceutical__lastUpdated When the resource version last changed }
    spMedicinalProductPharmaceutical__profile, {@enum.value "_profile" spMedicinalProductPharmaceutical__profile Profiles this resource claims to conform to }
    spMedicinalProductPharmaceutical__query, {@enum.value "_query" spMedicinalProductPharmaceutical__query A custom search profile that describes a specific defined query operation }
    spMedicinalProductPharmaceutical__security, {@enum.value "_security" spMedicinalProductPharmaceutical__security Security Labels applied to this resource }
    spMedicinalProductPharmaceutical__source, {@enum.value "_source" spMedicinalProductPharmaceutical__source Identifies where the resource comes from }
    spMedicinalProductPharmaceutical__tag, {@enum.value "_tag" spMedicinalProductPharmaceutical__tag Tags applied to this resource }
    spMedicinalProductPharmaceutical__text); {@enum.value "_text" spMedicinalProductPharmaceutical__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_MESSAGEDEFINITION}
  {@Enum TSearchParamsMessageDefinition
    Search Parameters for MessageDefinition
  }
  TSearchParamsMessageDefinition = (
    spMessageDefinition__content, {@enum.value "_content" spMessageDefinition__content Search on the entire content of the resource }
    spMessageDefinition__id, {@enum.value "_id" spMessageDefinition__id Logical id of this artifact }
    spMessageDefinition__lastUpdated, {@enum.value "_lastUpdated" spMessageDefinition__lastUpdated When the resource version last changed }
    spMessageDefinition__profile, {@enum.value "_profile" spMessageDefinition__profile Profiles this resource claims to conform to }
    spMessageDefinition__query, {@enum.value "_query" spMessageDefinition__query A custom search profile that describes a specific defined query operation }
    spMessageDefinition__security, {@enum.value "_security" spMessageDefinition__security Security Labels applied to this resource }
    spMessageDefinition__source, {@enum.value "_source" spMessageDefinition__source Identifies where the resource comes from }
    spMessageDefinition__tag, {@enum.value "_tag" spMessageDefinition__tag Tags applied to this resource }
    spMessageDefinition__text, {@enum.value "_text" spMessageDefinition__text Search on the narrative of the resource }
    spMessageDefinition_Category, {@enum.value "category" spMessageDefinition_Category The behavior associated with the message }
    spMessageDefinition_Date, {@enum.value "date" spMessageDefinition_Date The message definition publication date }
    spMessageDefinition_Description, {@enum.value "description" spMessageDefinition_Description The description of the message definition }
    spMessageDefinition_Event, {@enum.value "event" spMessageDefinition_Event The event that triggers the message }
    spMessageDefinition_Focus, {@enum.value "focus" spMessageDefinition_Focus A resource that is a permitted focus of the message }
    spMessageDefinition_Identifier, {@enum.value "identifier" spMessageDefinition_Identifier External identifier for the message definition }
    spMessageDefinition_Jurisdiction, {@enum.value "jurisdiction" spMessageDefinition_Jurisdiction Intended jurisdiction for the message definition }
    spMessageDefinition_Name, {@enum.value "name" spMessageDefinition_Name Computationally friendly name of the message definition }
    spMessageDefinition_Publisher, {@enum.value "publisher" spMessageDefinition_Publisher Name of the publisher of the message definition }
    spMessageDefinition_Status, {@enum.value "status" spMessageDefinition_Status The current status of the message definition }
    spMessageDefinition_Title, {@enum.value "title" spMessageDefinition_Title The human-friendly name of the message definition }
    spMessageDefinition_Url, {@enum.value "url" spMessageDefinition_Url The uri that identifies the message definition }
    spMessageDefinition_Version); {@enum.value "version" spMessageDefinition_Version The business version of the message definition }
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
    spMessageHeader__source, {@enum.value "_source" spMessageHeader__source Identifies where the resource comes from }
    spMessageHeader__tag, {@enum.value "_tag" spMessageHeader__tag Tags applied to this resource }
    spMessageHeader__text, {@enum.value "_text" spMessageHeader__text Search on the narrative of the resource }
    spMessageHeader_Author, {@enum.value "author" spMessageHeader_Author The source of the decision }
    spMessageHeader_Code, {@enum.value "code" spMessageHeader_Code ok | transient-error | fatal-error }
    spMessageHeader_Destination, {@enum.value "destination" spMessageHeader_Destination Name of system }
    spMessageHeader_Destinationuri, {@enum.value "destination-uri" spMessageHeader_Destinationuri Actual destination address or id }
    spMessageHeader_Enterer, {@enum.value "enterer" spMessageHeader_Enterer The source of the data entry }
    spMessageHeader_Event, {@enum.value "event" spMessageHeader_Event Code for the event this message represents }
    spMessageHeader_Focus, {@enum.value "focus" spMessageHeader_Focus The actual content of the message }
    spMessageHeader_Receiver, {@enum.value "receiver" spMessageHeader_Receiver Intended "real-world" recipient for the data }
    spMessageHeader_Responseid, {@enum.value "response-id" spMessageHeader_Responseid Id of original message }
    spMessageHeader_Responsible, {@enum.value "responsible" spMessageHeader_Responsible Final responsibility for event }
    spMessageHeader_Sender, {@enum.value "sender" spMessageHeader_Sender Real world sender of the message }
    spMessageHeader_Source, {@enum.value "source" spMessageHeader_Source Name of system }
    spMessageHeader_Sourceuri, {@enum.value "source-uri" spMessageHeader_Sourceuri Actual message source address or id }
    spMessageHeader_Target); {@enum.value "target" spMessageHeader_Target Particular delivery destination within the destination }
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
    spNamingSystem__source, {@enum.value "_source" spNamingSystem__source Identifies where the resource comes from }
    spNamingSystem__tag, {@enum.value "_tag" spNamingSystem__tag Tags applied to this resource }
    spNamingSystem__text, {@enum.value "_text" spNamingSystem__text Search on the narrative of the resource }
    spNamingSystem_Contact, {@enum.value "contact" spNamingSystem_Contact Name of an individual to contact }
    spNamingSystem_Date, {@enum.value "date" spNamingSystem_Date The naming system publication date }
    spNamingSystem_Description, {@enum.value "description" spNamingSystem_Description The description of the naming system }
    spNamingSystem_Idtype, {@enum.value "id-type" spNamingSystem_Idtype oid | uuid | uri | other }
    spNamingSystem_Jurisdiction, {@enum.value "jurisdiction" spNamingSystem_Jurisdiction Intended jurisdiction for the naming system }
    spNamingSystem_Kind, {@enum.value "kind" spNamingSystem_Kind codesystem | identifier | root }
    spNamingSystem_Name, {@enum.value "name" spNamingSystem_Name Computationally friendly name of the naming system }
    spNamingSystem_Period, {@enum.value "period" spNamingSystem_Period When is identifier valid? }
    spNamingSystem_Publisher, {@enum.value "publisher" spNamingSystem_Publisher Name of the publisher of the naming system }
    spNamingSystem_Responsible, {@enum.value "responsible" spNamingSystem_Responsible Who maintains system namespace? }
    spNamingSystem_Status, {@enum.value "status" spNamingSystem_Status The current status of the naming system }
    spNamingSystem_Telecom, {@enum.value "telecom" spNamingSystem_Telecom Contact details for individual or organization }
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
    spNutritionOrder__source, {@enum.value "_source" spNutritionOrder__source Identifies where the resource comes from }
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
    spObservation__source, {@enum.value "_source" spObservation__source Identifies where the resource comes from }
    spObservation__tag, {@enum.value "_tag" spObservation__tag Tags applied to this resource }
    spObservation__text, {@enum.value "_text" spObservation__text Search on the narrative of the resource }
    spObservation_Basedon, {@enum.value "based-on" spObservation_Basedon Reference to the service request. }
    spObservation_Category, {@enum.value "category" spObservation_Category The classification of the type of observation }
    spObservation_Code, {@enum.value "code" spObservation_Code The code of the observation type }
    spObservation_Codevalueconcept, {@enum.value "code-value-concept" spObservation_Codevalueconcept Code and coded value parameter pair }
    spObservation_Codevaluedate, {@enum.value "code-value-date" spObservation_Codevaluedate Code and date/time value parameter pair }
    spObservation_Codevaluequantity, {@enum.value "code-value-quantity" spObservation_Codevaluequantity Code and quantity value parameter pair }
    spObservation_Codevaluestring, {@enum.value "code-value-string" spObservation_Codevaluestring Code and string value parameter pair }
    spObservation_Combocode, {@enum.value "combo-code" spObservation_Combocode The code of the observation type or component type }
    spObservation_Combocodevalueconcept, {@enum.value "combo-code-value-concept" spObservation_Combocodevalueconcept Code and coded value parameter pair, including in components }
    spObservation_Combocodevaluequantity, {@enum.value "combo-code-value-quantity" spObservation_Combocodevaluequantity Code and quantity value parameter pair, including in components }
    spObservation_Combodataabsentreason, {@enum.value "combo-data-absent-reason" spObservation_Combodataabsentreason The reason why the expected value in the element Observation.value[x] or Observation.component.value[x] is missing. }
    spObservation_Combovalueconcept, {@enum.value "combo-value-concept" spObservation_Combovalueconcept The value or component value of the observation, if the value is a CodeableConcept }
    spObservation_Combovaluequantity, {@enum.value "combo-value-quantity" spObservation_Combovaluequantity The value or component value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data) }
    spObservation_Componentcode, {@enum.value "component-code" spObservation_Componentcode The component code of the observation type }
    spObservation_Componentcodevalueconcept, {@enum.value "component-code-value-concept" spObservation_Componentcodevalueconcept Component code and component coded value parameter pair }
    spObservation_Componentcodevaluequantity, {@enum.value "component-code-value-quantity" spObservation_Componentcodevaluequantity Component code and component quantity value parameter pair }
    spObservation_Componentdataabsentreason, {@enum.value "component-data-absent-reason" spObservation_Componentdataabsentreason The reason why the expected value in the element Observation.component.value[x] is missing. }
    spObservation_Componentvalueconcept, {@enum.value "component-value-concept" spObservation_Componentvalueconcept The value of the component observation, if the value is a CodeableConcept }
    spObservation_Componentvaluequantity, {@enum.value "component-value-quantity" spObservation_Componentvaluequantity The value of the component observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data) }
    spObservation_Context, {@enum.value "context" spObservation_Context Healthcare event  (Episode-of-care or Encounter) related to the observation }
    spObservation_Dataabsentreason, {@enum.value "data-absent-reason" spObservation_Dataabsentreason The reason why the expected value in the element Observation.value[x] is missing. }
    spObservation_Date, {@enum.value "date" spObservation_Date Obtained date/time. If the obtained element is a period, a date that falls in the period }
    spObservation_Derivedfrom, {@enum.value "derived-from" spObservation_Derivedfrom Related measurements the observation is made from }
    spObservation_Device, {@enum.value "device" spObservation_Device The Device that generated the observation data. }
    spObservation_Dnavariant, {@enum.value "dna-variant" spObservation_Dnavariant search for extension http://hl7.org/fhir/StructureDefinition/observation-geneticsDNASequenceVariantName }
    spObservation_Encounter, {@enum.value "encounter" spObservation_Encounter Encounter related to the observation }
    spObservation_Genednavariant, {@enum.value "gene-dnavariant" spObservation_Genednavariant search for extension http://hl7.org/fhir/StructureDefinition/observation-geneticsDNAVariantId }
    spObservation_Geneidentifier, {@enum.value "gene-identifier" spObservation_Geneidentifier search for extension http://hl7.org/fhir/StructureDefinition/observation-geneticsGene }
    spObservation_Hasmember, {@enum.value "has-member" spObservation_Hasmember Related resource that belongs to the Observation group }
    spObservation_Identifier, {@enum.value "identifier" spObservation_Identifier The unique id for a particular observation }
    spObservation_Method, {@enum.value "method" spObservation_Method The method used for the observation }
    spObservation_Partof, {@enum.value "part-of" spObservation_Partof Part of referenced event }
    spObservation_Patient, {@enum.value "patient" spObservation_Patient The subject that the observation is about (if patient) }
    spObservation_Performer, {@enum.value "performer" spObservation_Performer Who performed the observation }
    spObservation_Specimen, {@enum.value "specimen" spObservation_Specimen Specimen used for this observation }
    spObservation_Status, {@enum.value "status" spObservation_Status The status of the observation }
    spObservation_Subject, {@enum.value "subject" spObservation_Subject The subject that the observation is about }
    spObservation_Valueconcept, {@enum.value "value-concept" spObservation_Valueconcept The value of the observation, if the value is a CodeableConcept }
    spObservation_Valuedate, {@enum.value "value-date" spObservation_Valuedate The value of the observation, if the value is a date or period of time }
    spObservation_Valuequantity, {@enum.value "value-quantity" spObservation_Valuequantity The value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data) }
    spObservation_Valuestring); {@enum.value "value-string" spObservation_Valuestring The value of the observation, if the value is a string, and also searches in CodeableConcept.text }
{$ENDIF}

{$IFDEF FHIR_OBSERVATIONDEFINITION}
  {@Enum TSearchParamsObservationDefinition
    Search Parameters for ObservationDefinition
  }
  TSearchParamsObservationDefinition = (
    spObservationDefinition__content, {@enum.value "_content" spObservationDefinition__content Search on the entire content of the resource }
    spObservationDefinition__id, {@enum.value "_id" spObservationDefinition__id Logical id of this artifact }
    spObservationDefinition__lastUpdated, {@enum.value "_lastUpdated" spObservationDefinition__lastUpdated When the resource version last changed }
    spObservationDefinition__profile, {@enum.value "_profile" spObservationDefinition__profile Profiles this resource claims to conform to }
    spObservationDefinition__query, {@enum.value "_query" spObservationDefinition__query A custom search profile that describes a specific defined query operation }
    spObservationDefinition__security, {@enum.value "_security" spObservationDefinition__security Security Labels applied to this resource }
    spObservationDefinition__source, {@enum.value "_source" spObservationDefinition__source Identifies where the resource comes from }
    spObservationDefinition__tag, {@enum.value "_tag" spObservationDefinition__tag Tags applied to this resource }
    spObservationDefinition__text); {@enum.value "_text" spObservationDefinition__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_OCCUPATIONALDATA}
  {@Enum TSearchParamsOccupationalData
    Search Parameters for OccupationalData
  }
  TSearchParamsOccupationalData = (
    spOccupationalData__content, {@enum.value "_content" spOccupationalData__content Search on the entire content of the resource }
    spOccupationalData__id, {@enum.value "_id" spOccupationalData__id Logical id of this artifact }
    spOccupationalData__lastUpdated, {@enum.value "_lastUpdated" spOccupationalData__lastUpdated When the resource version last changed }
    spOccupationalData__profile, {@enum.value "_profile" spOccupationalData__profile Profiles this resource claims to conform to }
    spOccupationalData__query, {@enum.value "_query" spOccupationalData__query A custom search profile that describes a specific defined query operation }
    spOccupationalData__security, {@enum.value "_security" spOccupationalData__security Security Labels applied to this resource }
    spOccupationalData__source, {@enum.value "_source" spOccupationalData__source Identifies where the resource comes from }
    spOccupationalData__tag, {@enum.value "_tag" spOccupationalData__tag Tags applied to this resource }
    spOccupationalData__text, {@enum.value "_text" spOccupationalData__text Search on the narrative of the resource }
    spOccupationalData_Subject); {@enum.value "subject" spOccupationalData_Subject Who the occupational data is collected about }
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
    spOperationDefinition__source, {@enum.value "_source" spOperationDefinition__source Identifies where the resource comes from }
    spOperationDefinition__tag, {@enum.value "_tag" spOperationDefinition__tag Tags applied to this resource }
    spOperationDefinition__text, {@enum.value "_text" spOperationDefinition__text Search on the narrative of the resource }
    spOperationDefinition_Base, {@enum.value "base" spOperationDefinition_Base Marks this as a profile of the base }
    spOperationDefinition_Code, {@enum.value "code" spOperationDefinition_Code Name used to invoke the operation }
    spOperationDefinition_Contexttype, {@enum.value "context-type" spOperationDefinition_Contexttype A type of use context assigned to the operation definition }
    spOperationDefinition_Date, {@enum.value "date" spOperationDefinition_Date The operation definition publication date }
    spOperationDefinition_Description, {@enum.value "description" spOperationDefinition_Description The description of the operation definition }
    spOperationDefinition_Inputprofile, {@enum.value "input-profile" spOperationDefinition_Inputprofile Validation information for in parameters }
    spOperationDefinition_Instance, {@enum.value "instance" spOperationDefinition_Instance Invoke on an instance? }
    spOperationDefinition_Jurisdiction, {@enum.value "jurisdiction" spOperationDefinition_Jurisdiction Intended jurisdiction for the operation definition }
    spOperationDefinition_Kind, {@enum.value "kind" spOperationDefinition_Kind operation | query }
    spOperationDefinition_Name, {@enum.value "name" spOperationDefinition_Name Computationally friendly name of the operation definition }
    spOperationDefinition_Outputprofile, {@enum.value "output-profile" spOperationDefinition_Outputprofile Validation information for out parameters }
    spOperationDefinition_Publisher, {@enum.value "publisher" spOperationDefinition_Publisher Name of the publisher of the operation definition }
    spOperationDefinition_Status, {@enum.value "status" spOperationDefinition_Status The current status of the operation definition }
    spOperationDefinition_System, {@enum.value "system" spOperationDefinition_System Invoke at the system level? }
    spOperationDefinition_Type, {@enum.value "type" spOperationDefinition_Type Invoke at the type level? }
    spOperationDefinition_Url, {@enum.value "url" spOperationDefinition_Url The uri that identifies the operation definition }
    spOperationDefinition_Version); {@enum.value "version" spOperationDefinition_Version The business version of the operation definition }
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
    spOperationOutcome__source, {@enum.value "_source" spOperationOutcome__source Identifies where the resource comes from }
    spOperationOutcome__tag, {@enum.value "_tag" spOperationOutcome__tag Tags applied to this resource }
    spOperationOutcome__text); {@enum.value "_text" spOperationOutcome__text Search on the narrative of the resource }
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
    spOrganization__source, {@enum.value "_source" spOrganization__source Identifies where the resource comes from }
    spOrganization__tag, {@enum.value "_tag" spOrganization__tag Tags applied to this resource }
    spOrganization__text, {@enum.value "_text" spOrganization__text Search on the narrative of the resource }
    spOrganization_Active, {@enum.value "active" spOrganization_Active Is the Organization record active }
    spOrganization_Address, {@enum.value "address" spOrganization_Address A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text }
    spOrganization_Addresscity, {@enum.value "address-city" spOrganization_Addresscity A city specified in an address }
    spOrganization_Addresscountry, {@enum.value "address-country" spOrganization_Addresscountry A country specified in an address }
    spOrganization_Addresspostalcode, {@enum.value "address-postalcode" spOrganization_Addresspostalcode A postal code specified in an address }
    spOrganization_Addressstate, {@enum.value "address-state" spOrganization_Addressstate A state specified in an address }
    spOrganization_Addressuse, {@enum.value "address-use" spOrganization_Addressuse A use code specified in an address }
    spOrganization_Endpoint, {@enum.value "endpoint" spOrganization_Endpoint Technical endpoints providing access to services operated for the organization }
    spOrganization_Identifier, {@enum.value "identifier" spOrganization_Identifier Any identifier for the organization (not the accreditation issuer's identifier) }
    spOrganization_Name, {@enum.value "name" spOrganization_Name A portion of the organization's name or alias }
    spOrganization_Partof, {@enum.value "partof" spOrganization_Partof An organization of which this organization forms a part }
    spOrganization_Phonetic, {@enum.value "phonetic" spOrganization_Phonetic A portion of the organization's name using some kind of phonetic matching algorithm }
    spOrganization_Type); {@enum.value "type" spOrganization_Type A code for the type of organization }
{$ENDIF}

{$IFDEF FHIR_ORGANIZATIONROLE}
  {@Enum TSearchParamsOrganizationRole
    Search Parameters for OrganizationRole
  }
  TSearchParamsOrganizationRole = (
    spOrganizationRole__content, {@enum.value "_content" spOrganizationRole__content Search on the entire content of the resource }
    spOrganizationRole__id, {@enum.value "_id" spOrganizationRole__id Logical id of this artifact }
    spOrganizationRole__lastUpdated, {@enum.value "_lastUpdated" spOrganizationRole__lastUpdated When the resource version last changed }
    spOrganizationRole__profile, {@enum.value "_profile" spOrganizationRole__profile Profiles this resource claims to conform to }
    spOrganizationRole__query, {@enum.value "_query" spOrganizationRole__query A custom search profile that describes a specific defined query operation }
    spOrganizationRole__security, {@enum.value "_security" spOrganizationRole__security Security Labels applied to this resource }
    spOrganizationRole__source, {@enum.value "_source" spOrganizationRole__source Identifies where the resource comes from }
    spOrganizationRole__tag, {@enum.value "_tag" spOrganizationRole__tag Tags applied to this resource }
    spOrganizationRole__text, {@enum.value "_text" spOrganizationRole__text Search on the narrative of the resource }
    spOrganizationRole_Active, {@enum.value "active" spOrganizationRole_Active Whether this practitioner role record is in active use }
    spOrganizationRole_Date, {@enum.value "date" spOrganizationRole_Date The period during which the practitioner is authorized to perform in these role(s) }
    spOrganizationRole_Email, {@enum.value "email" spOrganizationRole_Email A value in an email contact }
    spOrganizationRole_Endpoint, {@enum.value "endpoint" spOrganizationRole_Endpoint Technical endpoints providing access to services operated for the practitioner with this role }
    spOrganizationRole_Identifier, {@enum.value "identifier" spOrganizationRole_Identifier A practitioner's Identifier }
    spOrganizationRole_Location, {@enum.value "location" spOrganizationRole_Location One of the locations at which this practitioner provides care }
    spOrganizationRole_Network, {@enum.value "network" spOrganizationRole_Network One of the locations at which this practitioner provides care }
    spOrganizationRole_Participatingorganization, {@enum.value "participating-organization" spOrganizationRole_Participatingorganization Practitioner that is able to provide the defined services for the organization }
    spOrganizationRole_Phone, {@enum.value "phone" spOrganizationRole_Phone A value in a phone contact }
    spOrganizationRole_Primaryorganization, {@enum.value "primary-organization" spOrganizationRole_Primaryorganization The identity of the organization the practitioner represents / acts on behalf of }
    spOrganizationRole_Role, {@enum.value "role" spOrganizationRole_Role The practitioner can perform this role at for the organization }
    spOrganizationRole_Service, {@enum.value "service" spOrganizationRole_Service The list of healthcare services that this worker provides for this role's Organization/Location(s) }
    spOrganizationRole_Specialty, {@enum.value "specialty" spOrganizationRole_Specialty The practitioner has this specialty at an organization }
    spOrganizationRole_Telecom); {@enum.value "telecom" spOrganizationRole_Telecom The value in any kind of contact }
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
    spPatient__source, {@enum.value "_source" spPatient__source Identifies where the resource comes from }
    spPatient__tag, {@enum.value "_tag" spPatient__tag Tags applied to this resource }
    spPatient__text, {@enum.value "_text" spPatient__text Search on the narrative of the resource }
    spPatient_Active, {@enum.value "active" spPatient_Active Whether the patient record is active }
    spPatient_Address, {@enum.value "address" spPatient_Address A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text }
    spPatient_Addresscity, {@enum.value "address-city" spPatient_Addresscity A city specified in an address }
    spPatient_Addresscountry, {@enum.value "address-country" spPatient_Addresscountry A country specified in an address }
    spPatient_Addresspostalcode, {@enum.value "address-postalcode" spPatient_Addresspostalcode A postalCode specified in an address }
    spPatient_Addressstate, {@enum.value "address-state" spPatient_Addressstate A state specified in an address }
    spPatient_Addressuse, {@enum.value "address-use" spPatient_Addressuse A use code specified in an address }
    spPatient_Animalbreed, {@enum.value "animal-breed" spPatient_Animalbreed The breed for animal patients }
    spPatient_Animalspecies, {@enum.value "animal-species" spPatient_Animalspecies The species for animal patients }
    spPatient_Birthdate, {@enum.value "birthdate" spPatient_Birthdate The patient's date of birth }
    spPatient_Deathdate, {@enum.value "death-date" spPatient_Deathdate The date of death has been provided and satisfies this search value }
    spPatient_Deceased, {@enum.value "deceased" spPatient_Deceased This patient has been marked as deceased, or as a death date entered }
    spPatient_Email, {@enum.value "email" spPatient_Email A value in an email contact }
    spPatient_Family, {@enum.value "family" spPatient_Family A portion of the family name of the patient }
    spPatient_Gender, {@enum.value "gender" spPatient_Gender Gender of the patient }
    spPatient_Generalpractitioner, {@enum.value "general-practitioner" spPatient_Generalpractitioner Patient's nominated general practitioner, not the organization that manages the record }
    spPatient_Given, {@enum.value "given" spPatient_Given A portion of the given name of the patient }
    spPatient_Identifier, {@enum.value "identifier" spPatient_Identifier A patient identifier }
    spPatient_Language, {@enum.value "language" spPatient_Language Language code (irrespective of use value) }
    spPatient_Link, {@enum.value "link" spPatient_Link All patients linked to the given patient }
    spPatient_Name, {@enum.value "name" spPatient_Name A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text }
    spPatient_Organization, {@enum.value "organization" spPatient_Organization The organization at which this person is a patient }
    spPatient_Phone, {@enum.value "phone" spPatient_Phone A value in a phone contact }
    spPatient_Phonetic, {@enum.value "phonetic" spPatient_Phonetic A portion of either family or given name using some kind of phonetic matching algorithm }
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
    spPaymentNotice__source, {@enum.value "_source" spPaymentNotice__source Identifies where the resource comes from }
    spPaymentNotice__tag, {@enum.value "_tag" spPaymentNotice__tag Tags applied to this resource }
    spPaymentNotice__text, {@enum.value "_text" spPaymentNotice__text Search on the narrative of the resource }
    spPaymentNotice_Created, {@enum.value "created" spPaymentNotice_Created Creation date fro the notice }
    spPaymentNotice_Identifier, {@enum.value "identifier" spPaymentNotice_Identifier The business identifier of the notice }
    spPaymentNotice_Organization, {@enum.value "organization" spPaymentNotice_Organization The organization who generated this resource }
    spPaymentNotice_Paymentstatus, {@enum.value "payment-status" spPaymentNotice_Paymentstatus The type of payment notice }
    spPaymentNotice_Provider, {@enum.value "provider" spPaymentNotice_Provider The reference to the provider }
    spPaymentNotice_Request, {@enum.value "request" spPaymentNotice_Request The Claim }
    spPaymentNotice_Response, {@enum.value "response" spPaymentNotice_Response The ClaimResponse }
    spPaymentNotice_Status, {@enum.value "status" spPaymentNotice_Status The status of the payment notice }
    spPaymentNotice_Statusdate); {@enum.value "statusdate" spPaymentNotice_Statusdate The date of the payment action }
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
    spPaymentReconciliation__source, {@enum.value "_source" spPaymentReconciliation__source Identifies where the resource comes from }
    spPaymentReconciliation__tag, {@enum.value "_tag" spPaymentReconciliation__tag Tags applied to this resource }
    spPaymentReconciliation__text, {@enum.value "_text" spPaymentReconciliation__text Search on the narrative of the resource }
    spPaymentReconciliation_Created, {@enum.value "created" spPaymentReconciliation_Created The creation date }
    spPaymentReconciliation_Disposition, {@enum.value "disposition" spPaymentReconciliation_Disposition The contents of the disposition message }
    spPaymentReconciliation_Identifier, {@enum.value "identifier" spPaymentReconciliation_Identifier The business identifier of the Explanation of Benefit }
    spPaymentReconciliation_Organization, {@enum.value "organization" spPaymentReconciliation_Organization The organization who generated this resource }
    spPaymentReconciliation_Outcome, {@enum.value "outcome" spPaymentReconciliation_Outcome The processing outcome }
    spPaymentReconciliation_Request, {@enum.value "request" spPaymentReconciliation_Request The reference to the claim }
    spPaymentReconciliation_Requestorganization, {@enum.value "request-organization" spPaymentReconciliation_Requestorganization The organization who generated this resource }
    spPaymentReconciliation_Requestprovider, {@enum.value "request-provider" spPaymentReconciliation_Requestprovider The reference to the provider who sumbitted the claim }
    spPaymentReconciliation_Status); {@enum.value "status" spPaymentReconciliation_Status The status of the payment reconciliation }
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
    spPerson__source, {@enum.value "_source" spPerson__source Identifies where the resource comes from }
    spPerson__tag, {@enum.value "_tag" spPerson__tag Tags applied to this resource }
    spPerson__text, {@enum.value "_text" spPerson__text Search on the narrative of the resource }
    spPerson_Address, {@enum.value "address" spPerson_Address A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text }
    spPerson_Addresscity, {@enum.value "address-city" spPerson_Addresscity A city specified in an address }
    spPerson_Addresscountry, {@enum.value "address-country" spPerson_Addresscountry A country specified in an address }
    spPerson_Addresspostalcode, {@enum.value "address-postalcode" spPerson_Addresspostalcode A postal code specified in an address }
    spPerson_Addressstate, {@enum.value "address-state" spPerson_Addressstate A state specified in an address }
    spPerson_Addressuse, {@enum.value "address-use" spPerson_Addressuse A use code specified in an address }
    spPerson_Birthdate, {@enum.value "birthdate" spPerson_Birthdate The Related Person's date of birth }
    spPerson_Email, {@enum.value "email" spPerson_Email A value in an email contact }
    spPerson_Gender, {@enum.value "gender" spPerson_Gender Gender of the related person }
    spPerson_Identifier, {@enum.value "identifier" spPerson_Identifier A person Identifier }
    spPerson_Link, {@enum.value "link" spPerson_Link Any link has this Patient, Person, RelatedPerson or Practitioner reference }
    spPerson_Name, {@enum.value "name" spPerson_Name A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text }
    spPerson_Organization, {@enum.value "organization" spPerson_Organization The organization at which this person record is being managed }
    spPerson_Patient, {@enum.value "patient" spPerson_Patient The Person links to this Patient }
    spPerson_Phone, {@enum.value "phone" spPerson_Phone A value in a phone contact }
    spPerson_Phonetic, {@enum.value "phonetic" spPerson_Phonetic A portion of name using some kind of phonetic matching algorithm }
    spPerson_Practitioner, {@enum.value "practitioner" spPerson_Practitioner The Person links to this Practitioner }
    spPerson_Relatedperson, {@enum.value "relatedperson" spPerson_Relatedperson The Person links to this RelatedPerson }
    spPerson_Telecom); {@enum.value "telecom" spPerson_Telecom The value in any kind of contact }
{$ENDIF}

{$IFDEF FHIR_PLANDEFINITION}
  {@Enum TSearchParamsPlanDefinition
    Search Parameters for PlanDefinition
  }
  TSearchParamsPlanDefinition = (
    spPlanDefinition__content, {@enum.value "_content" spPlanDefinition__content Search on the entire content of the resource }
    spPlanDefinition__id, {@enum.value "_id" spPlanDefinition__id Logical id of this artifact }
    spPlanDefinition__lastUpdated, {@enum.value "_lastUpdated" spPlanDefinition__lastUpdated When the resource version last changed }
    spPlanDefinition__profile, {@enum.value "_profile" spPlanDefinition__profile Profiles this resource claims to conform to }
    spPlanDefinition__query, {@enum.value "_query" spPlanDefinition__query A custom search profile that describes a specific defined query operation }
    spPlanDefinition__security, {@enum.value "_security" spPlanDefinition__security Security Labels applied to this resource }
    spPlanDefinition__source, {@enum.value "_source" spPlanDefinition__source Identifies where the resource comes from }
    spPlanDefinition__tag, {@enum.value "_tag" spPlanDefinition__tag Tags applied to this resource }
    spPlanDefinition__text, {@enum.value "_text" spPlanDefinition__text Search on the narrative of the resource }
    spPlanDefinition_Composedof, {@enum.value "composed-of" spPlanDefinition_Composedof What resource is being referenced }
    spPlanDefinition_Date, {@enum.value "date" spPlanDefinition_Date The plan definition publication date }
    spPlanDefinition_Definition, {@enum.value "definition" spPlanDefinition_Definition Activity or plan definitions used by plan definition }
    spPlanDefinition_Dependson, {@enum.value "depends-on" spPlanDefinition_Dependson What resource is being referenced }
    spPlanDefinition_Derivedfrom, {@enum.value "derived-from" spPlanDefinition_Derivedfrom What resource is being referenced }
    spPlanDefinition_Description, {@enum.value "description" spPlanDefinition_Description The description of the plan definition }
    spPlanDefinition_Effective, {@enum.value "effective" spPlanDefinition_Effective The time during which the plan definition is intended to be in use }
    spPlanDefinition_Identifier, {@enum.value "identifier" spPlanDefinition_Identifier External identifier for the plan definition }
    spPlanDefinition_Jurisdiction, {@enum.value "jurisdiction" spPlanDefinition_Jurisdiction Intended jurisdiction for the plan definition }
    spPlanDefinition_Name, {@enum.value "name" spPlanDefinition_Name Computationally friendly name of the plan definition }
    spPlanDefinition_Predecessor, {@enum.value "predecessor" spPlanDefinition_Predecessor What resource is being referenced }
    spPlanDefinition_Publisher, {@enum.value "publisher" spPlanDefinition_Publisher Name of the publisher of the plan definition }
    spPlanDefinition_Status, {@enum.value "status" spPlanDefinition_Status The current status of the plan definition }
    spPlanDefinition_Successor, {@enum.value "successor" spPlanDefinition_Successor What resource is being referenced }
    spPlanDefinition_Title, {@enum.value "title" spPlanDefinition_Title The human-friendly name of the plan definition }
    spPlanDefinition_Topic, {@enum.value "topic" spPlanDefinition_Topic Topics associated with the module }
    spPlanDefinition_Type, {@enum.value "type" spPlanDefinition_Type The type of artifact the plan (e.g. order-set, eca-rule, protocol) }
    spPlanDefinition_Url, {@enum.value "url" spPlanDefinition_Url The uri that identifies the plan definition }
    spPlanDefinition_Version); {@enum.value "version" spPlanDefinition_Version The business version of the plan definition }
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
    spPractitioner__source, {@enum.value "_source" spPractitioner__source Identifies where the resource comes from }
    spPractitioner__tag, {@enum.value "_tag" spPractitioner__tag Tags applied to this resource }
    spPractitioner__text, {@enum.value "_text" spPractitioner__text Search on the narrative of the resource }
    spPractitioner_Active, {@enum.value "active" spPractitioner_Active Whether the practitioner record is active }
    spPractitioner_Address, {@enum.value "address" spPractitioner_Address A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text }
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
    spPractitioner_Name, {@enum.value "name" spPractitioner_Name A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text }
    spPractitioner_Phone, {@enum.value "phone" spPractitioner_Phone A value in a phone contact }
    spPractitioner_Phonetic, {@enum.value "phonetic" spPractitioner_Phonetic A portion of either family or given name using some kind of phonetic matching algorithm }
    spPractitioner_Telecom); {@enum.value "telecom" spPractitioner_Telecom The value in any kind of contact }
{$ENDIF}

{$IFDEF FHIR_PRACTITIONERROLE}
  {@Enum TSearchParamsPractitionerRole
    Search Parameters for PractitionerRole
  }
  TSearchParamsPractitionerRole = (
    spPractitionerRole__content, {@enum.value "_content" spPractitionerRole__content Search on the entire content of the resource }
    spPractitionerRole__id, {@enum.value "_id" spPractitionerRole__id Logical id of this artifact }
    spPractitionerRole__lastUpdated, {@enum.value "_lastUpdated" spPractitionerRole__lastUpdated When the resource version last changed }
    spPractitionerRole__profile, {@enum.value "_profile" spPractitionerRole__profile Profiles this resource claims to conform to }
    spPractitionerRole__query, {@enum.value "_query" spPractitionerRole__query A custom search profile that describes a specific defined query operation }
    spPractitionerRole__security, {@enum.value "_security" spPractitionerRole__security Security Labels applied to this resource }
    spPractitionerRole__source, {@enum.value "_source" spPractitionerRole__source Identifies where the resource comes from }
    spPractitionerRole__tag, {@enum.value "_tag" spPractitionerRole__tag Tags applied to this resource }
    spPractitionerRole__text, {@enum.value "_text" spPractitionerRole__text Search on the narrative of the resource }
    spPractitionerRole_Active, {@enum.value "active" spPractitionerRole_Active Whether this practitioner role record is in active use }
    spPractitionerRole_Date, {@enum.value "date" spPractitionerRole_Date The period during which the practitioner is authorized to perform in these role(s) }
    spPractitionerRole_Email, {@enum.value "email" spPractitionerRole_Email A value in an email contact }
    spPractitionerRole_Endpoint, {@enum.value "endpoint" spPractitionerRole_Endpoint Technical endpoints providing access to services operated for the practitioner with this role }
    spPractitionerRole_Identifier, {@enum.value "identifier" spPractitionerRole_Identifier A practitioner's Identifier }
    spPractitionerRole_Location, {@enum.value "location" spPractitionerRole_Location One of the locations at which this practitioner provides care }
    spPractitionerRole_Organization, {@enum.value "organization" spPractitionerRole_Organization The identity of the organization the practitioner represents / acts on behalf of }
    spPractitionerRole_Phone, {@enum.value "phone" spPractitionerRole_Phone A value in a phone contact }
    spPractitionerRole_Practitioner, {@enum.value "practitioner" spPractitionerRole_Practitioner Practitioner that is able to provide the defined services for the organization }
    spPractitionerRole_Role, {@enum.value "role" spPractitionerRole_Role The practitioner can perform this role at for the organization }
    spPractitionerRole_Service, {@enum.value "service" spPractitionerRole_Service The list of healthcare services that this worker provides for this role's Organization/Location(s) }
    spPractitionerRole_Specialty, {@enum.value "specialty" spPractitionerRole_Specialty The practitioner has this specialty at an organization }
    spPractitionerRole_Telecom); {@enum.value "telecom" spPractitionerRole_Telecom The value in any kind of contact }
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
    spProcedure__source, {@enum.value "_source" spProcedure__source Identifies where the resource comes from }
    spProcedure__tag, {@enum.value "_tag" spProcedure__tag Tags applied to this resource }
    spProcedure__text, {@enum.value "_text" spProcedure__text Search on the narrative of the resource }
    spProcedure_Basedon, {@enum.value "based-on" spProcedure_Basedon A request for this procedure }
    spProcedure_Category, {@enum.value "category" spProcedure_Category Classification of the procedure }
    spProcedure_Code, {@enum.value "code" spProcedure_Code A code to identify a  procedure }
    spProcedure_Context, {@enum.value "context" spProcedure_Context Encounter or episode associated with the procedure }
    spProcedure_Date, {@enum.value "date" spProcedure_Date When the procedure was performed }
    spProcedure_Encounter, {@enum.value "encounter" spProcedure_Encounter Search by encounter }
    spProcedure_Identifier, {@enum.value "identifier" spProcedure_Identifier A unique identifier for a procedure }
    spProcedure_Instantiates, {@enum.value "instantiates" spProcedure_Instantiates Instantiates protocol or definition }
    spProcedure_Location, {@enum.value "location" spProcedure_Location Where the procedure happened }
    spProcedure_Partof, {@enum.value "part-of" spProcedure_Partof Part of referenced event }
    spProcedure_Patient, {@enum.value "patient" spProcedure_Patient Search by subject - a patient }
    spProcedure_Performer, {@enum.value "performer" spProcedure_Performer The reference to the practitioner }
    spProcedure_Status, {@enum.value "status" spProcedure_Status preparation | in-progress | not-done | suspended | aborted | completed | entered-in-error | unknown }
    spProcedure_Subject); {@enum.value "subject" spProcedure_Subject Search by subject }
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
    spProcessRequest__source, {@enum.value "_source" spProcessRequest__source Identifies where the resource comes from }
    spProcessRequest__tag, {@enum.value "_tag" spProcessRequest__tag Tags applied to this resource }
    spProcessRequest__text, {@enum.value "_text" spProcessRequest__text Search on the narrative of the resource }
    spProcessRequest_Action, {@enum.value "action" spProcessRequest_Action The action requested by this resource }
    spProcessRequest_Identifier, {@enum.value "identifier" spProcessRequest_Identifier The business identifier of the ProcessRequest }
    spProcessRequest_Organization, {@enum.value "organization" spProcessRequest_Organization The organization who generated this request }
    spProcessRequest_Provider, {@enum.value "provider" spProcessRequest_Provider The provider who regenerated this request }
    spProcessRequest_Status); {@enum.value "status" spProcessRequest_Status The status of the process request }
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
    spProcessResponse__source, {@enum.value "_source" spProcessResponse__source Identifies where the resource comes from }
    spProcessResponse__tag, {@enum.value "_tag" spProcessResponse__tag Tags applied to this resource }
    spProcessResponse__text, {@enum.value "_text" spProcessResponse__text Search on the narrative of the resource }
    spProcessResponse_Identifier, {@enum.value "identifier" spProcessResponse_Identifier The business identifier of the Explanation of Benefit }
    spProcessResponse_Organization, {@enum.value "organization" spProcessResponse_Organization The organization who generated this resource }
    spProcessResponse_Request, {@enum.value "request" spProcessResponse_Request The reference to the claim }
    spProcessResponse_Requestorganization, {@enum.value "request-organization" spProcessResponse_Requestorganization The Organization who is responsible the request transaction }
    spProcessResponse_Requestprovider, {@enum.value "request-provider" spProcessResponse_Requestprovider The Provider who is responsible the request transaction }
    spProcessResponse_Status); {@enum.value "status" spProcessResponse_Status The status of the process response }
{$ENDIF}

{$IFDEF FHIR_PRODUCTPLAN}
  {@Enum TSearchParamsProductPlan
    Search Parameters for ProductPlan
  }
  TSearchParamsProductPlan = (
    spProductPlan__content, {@enum.value "_content" spProductPlan__content Search on the entire content of the resource }
    spProductPlan__id, {@enum.value "_id" spProductPlan__id Logical id of this artifact }
    spProductPlan__lastUpdated, {@enum.value "_lastUpdated" spProductPlan__lastUpdated When the resource version last changed }
    spProductPlan__profile, {@enum.value "_profile" spProductPlan__profile Profiles this resource claims to conform to }
    spProductPlan__query, {@enum.value "_query" spProductPlan__query A custom search profile that describes a specific defined query operation }
    spProductPlan__security, {@enum.value "_security" spProductPlan__security Security Labels applied to this resource }
    spProductPlan__source, {@enum.value "_source" spProductPlan__source Identifies where the resource comes from }
    spProductPlan__tag, {@enum.value "_tag" spProductPlan__tag Tags applied to this resource }
    spProductPlan__text, {@enum.value "_text" spProductPlan__text Search on the narrative of the resource }
    spProductPlan_Address, {@enum.value "address" spProductPlan_Address A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text }
    spProductPlan_Addresscity, {@enum.value "address-city" spProductPlan_Addresscity A city specified in an address }
    spProductPlan_Addresscountry, {@enum.value "address-country" spProductPlan_Addresscountry A country specified in an address }
    spProductPlan_Addresspostalcode, {@enum.value "address-postalcode" spProductPlan_Addresspostalcode A postal code specified in an address }
    spProductPlan_Addressstate, {@enum.value "address-state" spProductPlan_Addressstate A state specified in an address }
    spProductPlan_Addressuse, {@enum.value "address-use" spProductPlan_Addressuse A use code specified in an address }
    spProductPlan_Administeredby, {@enum.value "administered-by" spProductPlan_Administeredby Administrator of the product/plan }
    spProductPlan_Endpoint, {@enum.value "endpoint" spProductPlan_Endpoint Technical endpoints providing access to services operated for the organization }
    spProductPlan_Identifier, {@enum.value "identifier" spProductPlan_Identifier Any identifier for the organization (not the accreditation issuer's identifier) }
    spProductPlan_Name, {@enum.value "name" spProductPlan_Name A portion of the organization's name or alias }
    spProductPlan_Ownedby, {@enum.value "owned-by" spProductPlan_Ownedby An organization of which this organization forms a part }
    spProductPlan_Phonetic, {@enum.value "phonetic" spProductPlan_Phonetic A portion of the organization's name using some kind of phonetic matching algorithm }
    spProductPlan_Status, {@enum.value "status" spProductPlan_Status Is the Organization record active }
    spProductPlan_Type); {@enum.value "type" spProductPlan_Type A code for the type of organization }
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
    spProvenance__source, {@enum.value "_source" spProvenance__source Identifies where the resource comes from }
    spProvenance__tag, {@enum.value "_tag" spProvenance__tag Tags applied to this resource }
    spProvenance__text, {@enum.value "_text" spProvenance__text Search on the narrative of the resource }
    spProvenance_Agent, {@enum.value "agent" spProvenance_Agent Who participated }
    spProvenance_Agentrole, {@enum.value "agent-role" spProvenance_Agentrole What the agents role was }
    spProvenance_Agenttype, {@enum.value "agent-type" spProvenance_Agenttype How the agent participated }
    spProvenance_Entityid, {@enum.value "entity-id" spProvenance_Entityid Identity of entity }
    spProvenance_Entityref, {@enum.value "entity-ref" spProvenance_Entityref Identity of entity }
    spProvenance_Location, {@enum.value "location" spProvenance_Location Where the activity occurred, if relevant }
    spProvenance_Patient, {@enum.value "patient" spProvenance_Patient Target Reference(s) (usually version specific) }
    spProvenance_Recorded, {@enum.value "recorded" spProvenance_Recorded When the activity was recorded / updated }
    spProvenance_Signaturetype, {@enum.value "signature-type" spProvenance_Signaturetype Indication of the reason the entity signed the object(s) }
    spProvenance_Target, {@enum.value "target" spProvenance_Target Target Reference(s) (usually version specific) }
    spProvenance_When); {@enum.value "when" spProvenance_When When the activity occurred }
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
    spQuestionnaire__source, {@enum.value "_source" spQuestionnaire__source Identifies where the resource comes from }
    spQuestionnaire__tag, {@enum.value "_tag" spQuestionnaire__tag Tags applied to this resource }
    spQuestionnaire__text, {@enum.value "_text" spQuestionnaire__text Search on the narrative of the resource }
    spQuestionnaire_Code, {@enum.value "code" spQuestionnaire_Code A code that corresponds to one of its items in the questionnaire }
    spQuestionnaire_Contexttype, {@enum.value "context-type" spQuestionnaire_Contexttype A type of use context assigned to the questionnaire }
    spQuestionnaire_Date, {@enum.value "date" spQuestionnaire_Date The questionnaire publication date }
    spQuestionnaire_Definition, {@enum.value "definition" spQuestionnaire_Definition ElementDefinition - details for the item }
    spQuestionnaire_Description, {@enum.value "description" spQuestionnaire_Description The description of the questionnaire }
    spQuestionnaire_Effective, {@enum.value "effective" spQuestionnaire_Effective The time during which the questionnaire is intended to be in use }
    spQuestionnaire_Identifier, {@enum.value "identifier" spQuestionnaire_Identifier External identifier for the questionnaire }
    spQuestionnaire_Jurisdiction, {@enum.value "jurisdiction" spQuestionnaire_Jurisdiction Intended jurisdiction for the questionnaire }
    spQuestionnaire_Name, {@enum.value "name" spQuestionnaire_Name Computationally friendly name of the questionnaire }
    spQuestionnaire_Publisher, {@enum.value "publisher" spQuestionnaire_Publisher Name of the publisher of the questionnaire }
    spQuestionnaire_Status, {@enum.value "status" spQuestionnaire_Status The current status of the questionnaire }
    spQuestionnaire_Subjecttype, {@enum.value "subject-type" spQuestionnaire_Subjecttype Resource that can be subject of QuestionnaireResponse }
    spQuestionnaire_Title, {@enum.value "title" spQuestionnaire_Title The human-friendly name of the questionnaire }
    spQuestionnaire_Url, {@enum.value "url" spQuestionnaire_Url The uri that identifies the questionnaire }
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
    spQuestionnaireResponse__source, {@enum.value "_source" spQuestionnaireResponse__source Identifies where the resource comes from }
    spQuestionnaireResponse__tag, {@enum.value "_tag" spQuestionnaireResponse__tag Tags applied to this resource }
    spQuestionnaireResponse__text, {@enum.value "_text" spQuestionnaireResponse__text Search on the narrative of the resource }
    spQuestionnaireResponse_Author, {@enum.value "author" spQuestionnaireResponse_Author The author of the questionnaire response }
    spQuestionnaireResponse_Authored, {@enum.value "authored" spQuestionnaireResponse_Authored When the questionnaire response was last changed }
    spQuestionnaireResponse_Basedon, {@enum.value "based-on" spQuestionnaireResponse_Basedon Plan/proposal/order fulfilled by this questionnaire response }
    spQuestionnaireResponse_Context, {@enum.value "context" spQuestionnaireResponse_Context Encounter or episode associated with the questionnaire response }
    spQuestionnaireResponse_Identifier, {@enum.value "identifier" spQuestionnaireResponse_Identifier The unique identifier for the questionnaire response }
    spQuestionnaireResponse_Partof, {@enum.value "part-of" spQuestionnaireResponse_Partof Procedure or observation this questionnaire response was performed as a part of }
    spQuestionnaireResponse_Patient, {@enum.value "patient" spQuestionnaireResponse_Patient The patient that is the subject of the questionnaire response }
    spQuestionnaireResponse_Questionnaire, {@enum.value "questionnaire" spQuestionnaireResponse_Questionnaire The questionnaire the answers are provided for }
    spQuestionnaireResponse_Source, {@enum.value "source" spQuestionnaireResponse_Source The individual providing the information reflected in the questionnaire respose }
    spQuestionnaireResponse_Status, {@enum.value "status" spQuestionnaireResponse_Status The status of the questionnaire response }
    spQuestionnaireResponse_Subject); {@enum.value "subject" spQuestionnaireResponse_Subject The subject of the questionnaire response }
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
    spRelatedPerson__source, {@enum.value "_source" spRelatedPerson__source Identifies where the resource comes from }
    spRelatedPerson__tag, {@enum.value "_tag" spRelatedPerson__tag Tags applied to this resource }
    spRelatedPerson__text, {@enum.value "_text" spRelatedPerson__text Search on the narrative of the resource }
    spRelatedPerson_Active, {@enum.value "active" spRelatedPerson_Active Indicates if the related person record is active }
    spRelatedPerson_Address, {@enum.value "address" spRelatedPerson_Address A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text }
    spRelatedPerson_Addresscity, {@enum.value "address-city" spRelatedPerson_Addresscity A city specified in an address }
    spRelatedPerson_Addresscountry, {@enum.value "address-country" spRelatedPerson_Addresscountry A country specified in an address }
    spRelatedPerson_Addresspostalcode, {@enum.value "address-postalcode" spRelatedPerson_Addresspostalcode A postal code specified in an address }
    spRelatedPerson_Addressstate, {@enum.value "address-state" spRelatedPerson_Addressstate A state specified in an address }
    spRelatedPerson_Addressuse, {@enum.value "address-use" spRelatedPerson_Addressuse A use code specified in an address }
    spRelatedPerson_Birthdate, {@enum.value "birthdate" spRelatedPerson_Birthdate The Related Person's date of birth }
    spRelatedPerson_Email, {@enum.value "email" spRelatedPerson_Email A value in an email contact }
    spRelatedPerson_Gender, {@enum.value "gender" spRelatedPerson_Gender Gender of the related person }
    spRelatedPerson_Identifier, {@enum.value "identifier" spRelatedPerson_Identifier An Identifier of the RelatedPerson }
    spRelatedPerson_Name, {@enum.value "name" spRelatedPerson_Name A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text }
    spRelatedPerson_Patient, {@enum.value "patient" spRelatedPerson_Patient The patient this related person is related to }
    spRelatedPerson_Phone, {@enum.value "phone" spRelatedPerson_Phone A value in a phone contact }
    spRelatedPerson_Phonetic, {@enum.value "phonetic" spRelatedPerson_Phonetic A portion of name using some kind of phonetic matching algorithm }
    spRelatedPerson_Telecom); {@enum.value "telecom" spRelatedPerson_Telecom The value in any kind of contact }
{$ENDIF}

{$IFDEF FHIR_REQUESTGROUP}
  {@Enum TSearchParamsRequestGroup
    Search Parameters for RequestGroup
  }
  TSearchParamsRequestGroup = (
    spRequestGroup__content, {@enum.value "_content" spRequestGroup__content Search on the entire content of the resource }
    spRequestGroup__id, {@enum.value "_id" spRequestGroup__id Logical id of this artifact }
    spRequestGroup__lastUpdated, {@enum.value "_lastUpdated" spRequestGroup__lastUpdated When the resource version last changed }
    spRequestGroup__profile, {@enum.value "_profile" spRequestGroup__profile Profiles this resource claims to conform to }
    spRequestGroup__query, {@enum.value "_query" spRequestGroup__query A custom search profile that describes a specific defined query operation }
    spRequestGroup__security, {@enum.value "_security" spRequestGroup__security Security Labels applied to this resource }
    spRequestGroup__source, {@enum.value "_source" spRequestGroup__source Identifies where the resource comes from }
    spRequestGroup__tag, {@enum.value "_tag" spRequestGroup__tag Tags applied to this resource }
    spRequestGroup__text, {@enum.value "_text" spRequestGroup__text Search on the narrative of the resource }
    spRequestGroup_Author, {@enum.value "author" spRequestGroup_Author The author of the request group }
    spRequestGroup_Authored, {@enum.value "authored" spRequestGroup_Authored The date the request group was authored }
    spRequestGroup_Code, {@enum.value "code" spRequestGroup_Code The code of the request group }
    spRequestGroup_Context, {@enum.value "context" spRequestGroup_Context The context the request group applies to }
    spRequestGroup_Definition, {@enum.value "definition" spRequestGroup_Definition The definition from which the request group is realized }
    spRequestGroup_Encounter, {@enum.value "encounter" spRequestGroup_Encounter The encounter the request group applies to }
    spRequestGroup_Groupidentifier, {@enum.value "group-identifier" spRequestGroup_Groupidentifier The group identifier for the request group }
    spRequestGroup_Identifier, {@enum.value "identifier" spRequestGroup_Identifier External identifiers for the request group }
    spRequestGroup_Intent, {@enum.value "intent" spRequestGroup_Intent The intent of the request group }
    spRequestGroup_Participant, {@enum.value "participant" spRequestGroup_Participant The participant in the requests in the group }
    spRequestGroup_Patient, {@enum.value "patient" spRequestGroup_Patient The identity of a patient to search for request groups }
    spRequestGroup_Priority, {@enum.value "priority" spRequestGroup_Priority The priority of the request group }
    spRequestGroup_Status, {@enum.value "status" spRequestGroup_Status The status of the request group }
    spRequestGroup_Subject); {@enum.value "subject" spRequestGroup_Subject The subject that the request group is about }
{$ENDIF}

{$IFDEF FHIR_RESEARCHSTUDY}
  {@Enum TSearchParamsResearchStudy
    Search Parameters for ResearchStudy
  }
  TSearchParamsResearchStudy = (
    spResearchStudy__content, {@enum.value "_content" spResearchStudy__content Search on the entire content of the resource }
    spResearchStudy__id, {@enum.value "_id" spResearchStudy__id Logical id of this artifact }
    spResearchStudy__lastUpdated, {@enum.value "_lastUpdated" spResearchStudy__lastUpdated When the resource version last changed }
    spResearchStudy__profile, {@enum.value "_profile" spResearchStudy__profile Profiles this resource claims to conform to }
    spResearchStudy__query, {@enum.value "_query" spResearchStudy__query A custom search profile that describes a specific defined query operation }
    spResearchStudy__security, {@enum.value "_security" spResearchStudy__security Security Labels applied to this resource }
    spResearchStudy__source, {@enum.value "_source" spResearchStudy__source Identifies where the resource comes from }
    spResearchStudy__tag, {@enum.value "_tag" spResearchStudy__tag Tags applied to this resource }
    spResearchStudy__text, {@enum.value "_text" spResearchStudy__text Search on the narrative of the resource }
    spResearchStudy_Category, {@enum.value "category" spResearchStudy_Category Classifications for the study }
    spResearchStudy_Date, {@enum.value "date" spResearchStudy_Date When the study began and ended }
    spResearchStudy_Focus, {@enum.value "focus" spResearchStudy_Focus Drugs, devices, etc. under study }
    spResearchStudy_Identifier, {@enum.value "identifier" spResearchStudy_Identifier Business Identifier for study }
    spResearchStudy_Keyword, {@enum.value "keyword" spResearchStudy_Keyword Used to search for the study }
    spResearchStudy_Location, {@enum.value "location" spResearchStudy_Location Geographic region(s) for study }
    spResearchStudy_Partof, {@enum.value "partof" spResearchStudy_Partof Part of larger study }
    spResearchStudy_Principalinvestigator, {@enum.value "principalinvestigator" spResearchStudy_Principalinvestigator Researcher who oversees multiple aspects of the study }
    spResearchStudy_Protocol, {@enum.value "protocol" spResearchStudy_Protocol Steps followed in executing study }
    spResearchStudy_Site, {@enum.value "site" spResearchStudy_Site Facility where study activities are conducted }
    spResearchStudy_Sponsor, {@enum.value "sponsor" spResearchStudy_Sponsor Organization that initiates and is legally responsible for the study }
    spResearchStudy_Status, {@enum.value "status" spResearchStudy_Status active | administratively-completed | approved | closed-to-accrual | closed-to-accrual-and-intervention | completed | disapproved | in-review | temporarily-closed-to-accrual | temporarily-closed-to-accrual-and-intervention | withdrawn }
    spResearchStudy_Title); {@enum.value "title" spResearchStudy_Title Name for this study }
{$ENDIF}

{$IFDEF FHIR_RESEARCHSUBJECT}
  {@Enum TSearchParamsResearchSubject
    Search Parameters for ResearchSubject
  }
  TSearchParamsResearchSubject = (
    spResearchSubject__content, {@enum.value "_content" spResearchSubject__content Search on the entire content of the resource }
    spResearchSubject__id, {@enum.value "_id" spResearchSubject__id Logical id of this artifact }
    spResearchSubject__lastUpdated, {@enum.value "_lastUpdated" spResearchSubject__lastUpdated When the resource version last changed }
    spResearchSubject__profile, {@enum.value "_profile" spResearchSubject__profile Profiles this resource claims to conform to }
    spResearchSubject__query, {@enum.value "_query" spResearchSubject__query A custom search profile that describes a specific defined query operation }
    spResearchSubject__security, {@enum.value "_security" spResearchSubject__security Security Labels applied to this resource }
    spResearchSubject__source, {@enum.value "_source" spResearchSubject__source Identifies where the resource comes from }
    spResearchSubject__tag, {@enum.value "_tag" spResearchSubject__tag Tags applied to this resource }
    spResearchSubject__text, {@enum.value "_text" spResearchSubject__text Search on the narrative of the resource }
    spResearchSubject_Date, {@enum.value "date" spResearchSubject_Date Start and end of participation }
    spResearchSubject_Identifier, {@enum.value "identifier" spResearchSubject_Identifier Business Identifier for research subject in a study }
    spResearchSubject_Individual, {@enum.value "individual" spResearchSubject_Individual Who is part of study }
    spResearchSubject_Patient, {@enum.value "patient" spResearchSubject_Patient Who is part of study }
    spResearchSubject_Status, {@enum.value "status" spResearchSubject_Status candidate | eligible | follow-up | ineligible | not-registered | off-study | on-study | on-study-intervention | on-study-observation | pending-on-study | potential-candidate | screening | withdrawn }
    spResearchSubject_Study); {@enum.value "study" spResearchSubject_Study Study subject is part of }
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
    spRiskAssessment__source, {@enum.value "_source" spRiskAssessment__source Identifies where the resource comes from }
    spRiskAssessment__tag, {@enum.value "_tag" spRiskAssessment__tag Tags applied to this resource }
    spRiskAssessment__text, {@enum.value "_text" spRiskAssessment__text Search on the narrative of the resource }
    spRiskAssessment_Condition, {@enum.value "condition" spRiskAssessment_Condition Condition assessed }
    spRiskAssessment_Date, {@enum.value "date" spRiskAssessment_Date When was assessment made? }
    spRiskAssessment_Encounter, {@enum.value "encounter" spRiskAssessment_Encounter Where was assessment performed? }
    spRiskAssessment_Identifier, {@enum.value "identifier" spRiskAssessment_Identifier Unique identifier for the assessment }
    spRiskAssessment_Method, {@enum.value "method" spRiskAssessment_Method Evaluation mechanism }
    spRiskAssessment_Patient, {@enum.value "patient" spRiskAssessment_Patient Who/what does assessment apply to? }
    spRiskAssessment_Performer, {@enum.value "performer" spRiskAssessment_Performer Who did assessment? }
    spRiskAssessment_Probability, {@enum.value "probability" spRiskAssessment_Probability Likelihood of specified outcome }
    spRiskAssessment_Risk, {@enum.value "risk" spRiskAssessment_Risk Likelihood of specified outcome as a qualitative value }
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
    spSchedule__source, {@enum.value "_source" spSchedule__source Identifies where the resource comes from }
    spSchedule__tag, {@enum.value "_tag" spSchedule__tag Tags applied to this resource }
    spSchedule__text, {@enum.value "_text" spSchedule__text Search on the narrative of the resource }
    spSchedule_Active, {@enum.value "active" spSchedule_Active Is the schedule in active use }
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
    spSearchParameter__source, {@enum.value "_source" spSearchParameter__source Identifies where the resource comes from }
    spSearchParameter__tag, {@enum.value "_tag" spSearchParameter__tag Tags applied to this resource }
    spSearchParameter__text, {@enum.value "_text" spSearchParameter__text Search on the narrative of the resource }
    spSearchParameter_Base, {@enum.value "base" spSearchParameter_Base The resource type(s) this search parameter applies to }
    spSearchParameter_Code, {@enum.value "code" spSearchParameter_Code Code used in URL }
    spSearchParameter_Component, {@enum.value "component" spSearchParameter_Component Defines how the part works }
    spSearchParameter_Contexttype, {@enum.value "context-type" spSearchParameter_Contexttype A type of use context assigned to the search parameter }
    spSearchParameter_Date, {@enum.value "date" spSearchParameter_Date The search parameter publication date }
    spSearchParameter_Derivedfrom, {@enum.value "derived-from" spSearchParameter_Derivedfrom Original Definition for the search parameter }
    spSearchParameter_Description, {@enum.value "description" spSearchParameter_Description The description of the search parameter }
    spSearchParameter_Jurisdiction, {@enum.value "jurisdiction" spSearchParameter_Jurisdiction Intended jurisdiction for the search parameter }
    spSearchParameter_Name, {@enum.value "name" spSearchParameter_Name Computationally friendly name of the search parameter }
    spSearchParameter_Publisher, {@enum.value "publisher" spSearchParameter_Publisher Name of the publisher of the search parameter }
    spSearchParameter_Status, {@enum.value "status" spSearchParameter_Status The current status of the search parameter }
    spSearchParameter_Target, {@enum.value "target" spSearchParameter_Target Types of resource (if a resource reference) }
    spSearchParameter_Type, {@enum.value "type" spSearchParameter_Type number | date | string | token | reference | composite | quantity | uri }
    spSearchParameter_Url, {@enum.value "url" spSearchParameter_Url The uri that identifies the search parameter }
    spSearchParameter_Version); {@enum.value "version" spSearchParameter_Version The business version of the search parameter }
{$ENDIF}

{$IFDEF FHIR_SEQUENCE}
  {@Enum TSearchParamsSequence
    Search Parameters for Sequence
  }
  TSearchParamsSequence = (
    spSequence__content, {@enum.value "_content" spSequence__content Search on the entire content of the resource }
    spSequence__id, {@enum.value "_id" spSequence__id Logical id of this artifact }
    spSequence__lastUpdated, {@enum.value "_lastUpdated" spSequence__lastUpdated When the resource version last changed }
    spSequence__profile, {@enum.value "_profile" spSequence__profile Profiles this resource claims to conform to }
    spSequence__query, {@enum.value "_query" spSequence__query A custom search profile that describes a specific defined query operation }
    spSequence__security, {@enum.value "_security" spSequence__security Security Labels applied to this resource }
    spSequence__source, {@enum.value "_source" spSequence__source Identifies where the resource comes from }
    spSequence__tag, {@enum.value "_tag" spSequence__tag Tags applied to this resource }
    spSequence__text, {@enum.value "_text" spSequence__text Search on the narrative of the resource }
    spSequence_Chromosome, {@enum.value "chromosome" spSequence_Chromosome Chromosome number of the reference sequence }
    spSequence_Coordinate, {@enum.value "coordinate" spSequence_Coordinate Search parameter for region of the reference DNA sequence string. This will refer to part of a locus or part of a gene where search region will be represented in 1-based system. Since the coordinateSystem can either be 0-based or 1-based, this search query will include the result of both coordinateSystem that contains the equivalent segment of the gene or whole genome sequence. For example, a search for sequence can be represented as `coordinate=1$lt345$gt123`, this means it will search for the Sequence resource on chromosome 1 and with position >123 and <345, where in 1-based system resource, all strings within region 1:124-344 will be revealed, while in 0-based system resource, all strings within region 1:123-344 will be revealed. You may want to check detail about 0-based v.s. 1-based above. }
    spSequence_End, {@enum.value "end" spSequence_End End position (0-based exclusive, which menas the acid at this position will not be included, 1-based inclusive, which means the acid at this position will be included) of the reference sequence. }
    spSequence_Identifier, {@enum.value "identifier" spSequence_Identifier The unique identity for a particular sequence }
    spSequence_Patient, {@enum.value "patient" spSequence_Patient The subject that the observation is about }
    spSequence_Start, {@enum.value "start" spSequence_Start Start position (0-based inclusive, 1-based inclusive, that means the nucleic acid or amino acid at this position will be included) of the reference sequence. }
    spSequence_Type); {@enum.value "type" spSequence_Type Amino Acid Sequence/ DNA Sequence / RNA Sequence }
{$ENDIF}

{$IFDEF FHIR_SERVICEDEFINITION}
  {@Enum TSearchParamsServiceDefinition
    Search Parameters for ServiceDefinition
  }
  TSearchParamsServiceDefinition = (
    spServiceDefinition__content, {@enum.value "_content" spServiceDefinition__content Search on the entire content of the resource }
    spServiceDefinition__id, {@enum.value "_id" spServiceDefinition__id Logical id of this artifact }
    spServiceDefinition__lastUpdated, {@enum.value "_lastUpdated" spServiceDefinition__lastUpdated When the resource version last changed }
    spServiceDefinition__profile, {@enum.value "_profile" spServiceDefinition__profile Profiles this resource claims to conform to }
    spServiceDefinition__query, {@enum.value "_query" spServiceDefinition__query A custom search profile that describes a specific defined query operation }
    spServiceDefinition__security, {@enum.value "_security" spServiceDefinition__security Security Labels applied to this resource }
    spServiceDefinition__source, {@enum.value "_source" spServiceDefinition__source Identifies where the resource comes from }
    spServiceDefinition__tag, {@enum.value "_tag" spServiceDefinition__tag Tags applied to this resource }
    spServiceDefinition__text, {@enum.value "_text" spServiceDefinition__text Search on the narrative of the resource }
    spServiceDefinition_Composedof, {@enum.value "composed-of" spServiceDefinition_Composedof What resource is being referenced }
    spServiceDefinition_Date, {@enum.value "date" spServiceDefinition_Date The service definition publication date }
    spServiceDefinition_Dependson, {@enum.value "depends-on" spServiceDefinition_Dependson What resource is being referenced }
    spServiceDefinition_Derivedfrom, {@enum.value "derived-from" spServiceDefinition_Derivedfrom What resource is being referenced }
    spServiceDefinition_Description, {@enum.value "description" spServiceDefinition_Description The description of the service definition }
    spServiceDefinition_Effective, {@enum.value "effective" spServiceDefinition_Effective The time during which the service definition is intended to be in use }
    spServiceDefinition_Identifier, {@enum.value "identifier" spServiceDefinition_Identifier External identifier for the service definition }
    spServiceDefinition_Jurisdiction, {@enum.value "jurisdiction" spServiceDefinition_Jurisdiction Intended jurisdiction for the service definition }
    spServiceDefinition_Name, {@enum.value "name" spServiceDefinition_Name Computationally friendly name of the service definition }
    spServiceDefinition_Predecessor, {@enum.value "predecessor" spServiceDefinition_Predecessor What resource is being referenced }
    spServiceDefinition_Publisher, {@enum.value "publisher" spServiceDefinition_Publisher Name of the publisher of the service definition }
    spServiceDefinition_Status, {@enum.value "status" spServiceDefinition_Status The current status of the service definition }
    spServiceDefinition_Successor, {@enum.value "successor" spServiceDefinition_Successor What resource is being referenced }
    spServiceDefinition_Title, {@enum.value "title" spServiceDefinition_Title The human-friendly name of the service definition }
    spServiceDefinition_Topic, {@enum.value "topic" spServiceDefinition_Topic Topics associated with the module }
    spServiceDefinition_Url, {@enum.value "url" spServiceDefinition_Url The uri that identifies the service definition }
    spServiceDefinition_Version); {@enum.value "version" spServiceDefinition_Version The business version of the service definition }
{$ENDIF}

{$IFDEF FHIR_SERVICEREQUEST}
  {@Enum TSearchParamsServiceRequest
    Search Parameters for ServiceRequest
  }
  TSearchParamsServiceRequest = (
    spServiceRequest__content, {@enum.value "_content" spServiceRequest__content Search on the entire content of the resource }
    spServiceRequest__id, {@enum.value "_id" spServiceRequest__id Logical id of this artifact }
    spServiceRequest__lastUpdated, {@enum.value "_lastUpdated" spServiceRequest__lastUpdated When the resource version last changed }
    spServiceRequest__profile, {@enum.value "_profile" spServiceRequest__profile Profiles this resource claims to conform to }
    spServiceRequest__query, {@enum.value "_query" spServiceRequest__query A custom search profile that describes a specific defined query operation }
    spServiceRequest__security, {@enum.value "_security" spServiceRequest__security Security Labels applied to this resource }
    spServiceRequest__source, {@enum.value "_source" spServiceRequest__source Identifies where the resource comes from }
    spServiceRequest__tag, {@enum.value "_tag" spServiceRequest__tag Tags applied to this resource }
    spServiceRequest__text, {@enum.value "_text" spServiceRequest__text Search on the narrative of the resource }
    spServiceRequest_Authored, {@enum.value "authored" spServiceRequest_Authored Date request signed }
    spServiceRequest_Basedon, {@enum.value "based-on" spServiceRequest_Basedon What request fulfills }
    spServiceRequest_Bodysite, {@enum.value "body-site" spServiceRequest_Bodysite Where procedure is going to be done }
    spServiceRequest_Category, {@enum.value "category" spServiceRequest_Category Classification of service }
    spServiceRequest_Code, {@enum.value "code" spServiceRequest_Code What is being requested/ordered }
    spServiceRequest_Context, {@enum.value "context" spServiceRequest_Context Encounter or Episode during which request was created }
    spServiceRequest_Encounter, {@enum.value "encounter" spServiceRequest_Encounter An encounter in which this request is made }
    spServiceRequest_Identifier, {@enum.value "identifier" spServiceRequest_Identifier Identifiers assigned to this order }
    spServiceRequest_Instantiates, {@enum.value "instantiates" spServiceRequest_Instantiates Protocol or definition }
    spServiceRequest_Intent, {@enum.value "intent" spServiceRequest_Intent proposal | plan | order + }
    spServiceRequest_Occurrence, {@enum.value "occurrence" spServiceRequest_Occurrence When service should occur }
    spServiceRequest_Patient, {@enum.value "patient" spServiceRequest_Patient Search by subject - a patient }
    spServiceRequest_Performer, {@enum.value "performer" spServiceRequest_Performer Requested perfomer }
    spServiceRequest_Performertype, {@enum.value "performer-type" spServiceRequest_Performertype Performer role }
    spServiceRequest_Priority, {@enum.value "priority" spServiceRequest_Priority routine | urgent | asap | stat }
    spServiceRequest_Replaces, {@enum.value "replaces" spServiceRequest_Replaces What request replaces }
    spServiceRequest_Requester, {@enum.value "requester" spServiceRequest_Requester Who/what is requesting service }
    spServiceRequest_Requisition, {@enum.value "requisition" spServiceRequest_Requisition Composite Request ID }
    spServiceRequest_Specimen, {@enum.value "specimen" spServiceRequest_Specimen Specimen to be tested }
    spServiceRequest_Status, {@enum.value "status" spServiceRequest_Status draft | active | suspended | completed | entered-in-error | cancelled }
    spServiceRequest_Subject); {@enum.value "subject" spServiceRequest_Subject Search by subject }
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
    spSlot__source, {@enum.value "_source" spSlot__source Identifies where the resource comes from }
    spSlot__tag, {@enum.value "_tag" spSlot__tag Tags applied to this resource }
    spSlot__text, {@enum.value "_text" spSlot__text Search on the narrative of the resource }
    spSlot_Identifier, {@enum.value "identifier" spSlot_Identifier A Slot Identifier }
    spSlot_Schedule, {@enum.value "schedule" spSlot_Schedule The Schedule Resource that we are seeking a slot within }
    spSlot_Slottype, {@enum.value "slot-type" spSlot_Slottype The type of appointments that can be booked into the slot }
    spSlot_Start, {@enum.value "start" spSlot_Start Appointment date/time. }
    spSlot_Status); {@enum.value "status" spSlot_Status The free/busy status of the appointment }
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
    spSpecimen__source, {@enum.value "_source" spSpecimen__source Identifies where the resource comes from }
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
    spSpecimen_Status, {@enum.value "status" spSpecimen_Status available | unavailable | unsatisfactory | entered-in-error }
    spSpecimen_Subject, {@enum.value "subject" spSpecimen_Subject The subject of the specimen }
    spSpecimen_Type); {@enum.value "type" spSpecimen_Type The specimen type }
{$ENDIF}

{$IFDEF FHIR_SPECIMENDEFINITION}
  {@Enum TSearchParamsSpecimenDefinition
    Search Parameters for SpecimenDefinition
  }
  TSearchParamsSpecimenDefinition = (
    spSpecimenDefinition__content, {@enum.value "_content" spSpecimenDefinition__content Search on the entire content of the resource }
    spSpecimenDefinition__id, {@enum.value "_id" spSpecimenDefinition__id Logical id of this artifact }
    spSpecimenDefinition__lastUpdated, {@enum.value "_lastUpdated" spSpecimenDefinition__lastUpdated When the resource version last changed }
    spSpecimenDefinition__profile, {@enum.value "_profile" spSpecimenDefinition__profile Profiles this resource claims to conform to }
    spSpecimenDefinition__query, {@enum.value "_query" spSpecimenDefinition__query A custom search profile that describes a specific defined query operation }
    spSpecimenDefinition__security, {@enum.value "_security" spSpecimenDefinition__security Security Labels applied to this resource }
    spSpecimenDefinition__source, {@enum.value "_source" spSpecimenDefinition__source Identifies where the resource comes from }
    spSpecimenDefinition__tag, {@enum.value "_tag" spSpecimenDefinition__tag Tags applied to this resource }
    spSpecimenDefinition__text, {@enum.value "_text" spSpecimenDefinition__text Search on the narrative of the resource }
    spSpecimenDefinition_Container, {@enum.value "container" spSpecimenDefinition_Container The type of specimen conditioned in container expected by the lab }
    spSpecimenDefinition_Identifier, {@enum.value "identifier" spSpecimenDefinition_Identifier The unique identifier associated with the specimen }
    spSpecimenDefinition_Type); {@enum.value "type" spSpecimenDefinition_Type The type of collected specimen }
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
    spStructureDefinition__source, {@enum.value "_source" spStructureDefinition__source Identifies where the resource comes from }
    spStructureDefinition__tag, {@enum.value "_tag" spStructureDefinition__tag Tags applied to this resource }
    spStructureDefinition__text, {@enum.value "_text" spStructureDefinition__text Search on the narrative of the resource }
    spStructureDefinition_Abstract, {@enum.value "abstract" spStructureDefinition_Abstract Whether the structure is abstract }
    spStructureDefinition_Base, {@enum.value "base" spStructureDefinition_Base Definition that this type is constrained/specialized from }
    spStructureDefinition_Basepath, {@enum.value "base-path" spStructureDefinition_Basepath Path that identifies the base element }
    spStructureDefinition_Contexttype, {@enum.value "context-type" spStructureDefinition_Contexttype resource | datatype | extension }
    spStructureDefinition_Date, {@enum.value "date" spStructureDefinition_Date The structure definition publication date }
    spStructureDefinition_Derivation, {@enum.value "derivation" spStructureDefinition_Derivation specialization | constraint - How relates to base definition }
    spStructureDefinition_Description, {@enum.value "description" spStructureDefinition_Description The description of the structure definition }
    spStructureDefinition_Experimental, {@enum.value "experimental" spStructureDefinition_Experimental For testing purposes, not real usage }
    spStructureDefinition_Extcontext, {@enum.value "ext-context" spStructureDefinition_Extcontext Where the extension can be used in instances }
    spStructureDefinition_Identifier, {@enum.value "identifier" spStructureDefinition_Identifier External identifier for the structure definition }
    spStructureDefinition_Jurisdiction, {@enum.value "jurisdiction" spStructureDefinition_Jurisdiction Intended jurisdiction for the structure definition }
    spStructureDefinition_Keyword, {@enum.value "keyword" spStructureDefinition_Keyword A code for the profile }
    spStructureDefinition_Kind, {@enum.value "kind" spStructureDefinition_Kind primitive-type | complex-type | resource | logical }
    spStructureDefinition_Name, {@enum.value "name" spStructureDefinition_Name Computationally friendly name of the structure definition }
    spStructureDefinition_Path, {@enum.value "path" spStructureDefinition_Path A path that is constrained in the profile }
    spStructureDefinition_Publisher, {@enum.value "publisher" spStructureDefinition_Publisher Name of the publisher of the structure definition }
    spStructureDefinition_Status, {@enum.value "status" spStructureDefinition_Status The current status of the structure definition }
    spStructureDefinition_Title, {@enum.value "title" spStructureDefinition_Title The human-friendly name of the structure definition }
    spStructureDefinition_Type, {@enum.value "type" spStructureDefinition_Type Type defined or constrained by this structure }
    spStructureDefinition_Url, {@enum.value "url" spStructureDefinition_Url The uri that identifies the structure definition }
    spStructureDefinition_Valueset, {@enum.value "valueset" spStructureDefinition_Valueset A vocabulary binding reference }
    spStructureDefinition_Version); {@enum.value "version" spStructureDefinition_Version The business version of the structure definition }
{$ENDIF}

{$IFDEF FHIR_STRUCTUREMAP}
  {@Enum TSearchParamsStructureMap
    Search Parameters for StructureMap
  }
  TSearchParamsStructureMap = (
    spStructureMap__content, {@enum.value "_content" spStructureMap__content Search on the entire content of the resource }
    spStructureMap__id, {@enum.value "_id" spStructureMap__id Logical id of this artifact }
    spStructureMap__lastUpdated, {@enum.value "_lastUpdated" spStructureMap__lastUpdated When the resource version last changed }
    spStructureMap__profile, {@enum.value "_profile" spStructureMap__profile Profiles this resource claims to conform to }
    spStructureMap__query, {@enum.value "_query" spStructureMap__query A custom search profile that describes a specific defined query operation }
    spStructureMap__security, {@enum.value "_security" spStructureMap__security Security Labels applied to this resource }
    spStructureMap__source, {@enum.value "_source" spStructureMap__source Identifies where the resource comes from }
    spStructureMap__tag, {@enum.value "_tag" spStructureMap__tag Tags applied to this resource }
    spStructureMap__text, {@enum.value "_text" spStructureMap__text Search on the narrative of the resource }
    spStructureMap_Date, {@enum.value "date" spStructureMap_Date The structure map publication date }
    spStructureMap_Description, {@enum.value "description" spStructureMap_Description The description of the structure map }
    spStructureMap_Identifier, {@enum.value "identifier" spStructureMap_Identifier External identifier for the structure map }
    spStructureMap_Jurisdiction, {@enum.value "jurisdiction" spStructureMap_Jurisdiction Intended jurisdiction for the structure map }
    spStructureMap_Name, {@enum.value "name" spStructureMap_Name Computationally friendly name of the structure map }
    spStructureMap_Publisher, {@enum.value "publisher" spStructureMap_Publisher Name of the publisher of the structure map }
    spStructureMap_Status, {@enum.value "status" spStructureMap_Status The current status of the structure map }
    spStructureMap_Title, {@enum.value "title" spStructureMap_Title The human-friendly name of the structure map }
    spStructureMap_Url, {@enum.value "url" spStructureMap_Url The uri that identifies the structure map }
    spStructureMap_Version); {@enum.value "version" spStructureMap_Version The business version of the structure map }
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
    spSubscription__source, {@enum.value "_source" spSubscription__source Identifies where the resource comes from }
    spSubscription__tag, {@enum.value "_tag" spSubscription__tag Tags applied to this resource }
    spSubscription__text, {@enum.value "_text" spSubscription__text Search on the narrative of the resource }
    spSubscription_Addtag, {@enum.value "add-tag" spSubscription_Addtag A tag to be added to the resource matching the criteria }
    spSubscription_Contact, {@enum.value "contact" spSubscription_Contact Contact details for the subscription }
    spSubscription_Criteria, {@enum.value "criteria" spSubscription_Criteria The search rules used to determine when to send a notification }
    spSubscription_Payload, {@enum.value "payload" spSubscription_Payload The mime-type of the notification payload }
    spSubscription_Status, {@enum.value "status" spSubscription_Status The current state of the subscription }
    spSubscription_Type, {@enum.value "type" spSubscription_Type The type of channel for the sent notifications }
    spSubscription_Url); {@enum.value "url" spSubscription_Url The uri that will receive the notifications }
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
    spSubstance__source, {@enum.value "_source" spSubstance__source Identifies where the resource comes from }
    spSubstance__tag, {@enum.value "_tag" spSubstance__tag Tags applied to this resource }
    spSubstance__text, {@enum.value "_text" spSubstance__text Search on the narrative of the resource }
    spSubstance_Category, {@enum.value "category" spSubstance_Category The category of the substance }
    spSubstance_Code, {@enum.value "code" spSubstance_Code The code of the substance or ingredient }
    spSubstance_Containeridentifier, {@enum.value "container-identifier" spSubstance_Containeridentifier Identifier of the package/container }
    spSubstance_Expiry, {@enum.value "expiry" spSubstance_Expiry Expiry date of package or container of substance }
    spSubstance_Identifier, {@enum.value "identifier" spSubstance_Identifier Unique identifier for the substance }
    spSubstance_Quantity, {@enum.value "quantity" spSubstance_Quantity Amount of substance in the package }
    spSubstance_Status, {@enum.value "status" spSubstance_Status active | inactive | entered-in-error }
    spSubstance_Substancereference); {@enum.value "substance-reference" spSubstance_Substancereference A component of the substance }
{$ENDIF}

{$IFDEF FHIR_SUBSTANCEPOLYMER}
  {@Enum TSearchParamsSubstancePolymer
    Search Parameters for SubstancePolymer
  }
  TSearchParamsSubstancePolymer = (
    spSubstancePolymer__content, {@enum.value "_content" spSubstancePolymer__content Search on the entire content of the resource }
    spSubstancePolymer__id, {@enum.value "_id" spSubstancePolymer__id Logical id of this artifact }
    spSubstancePolymer__lastUpdated, {@enum.value "_lastUpdated" spSubstancePolymer__lastUpdated When the resource version last changed }
    spSubstancePolymer__profile, {@enum.value "_profile" spSubstancePolymer__profile Profiles this resource claims to conform to }
    spSubstancePolymer__query, {@enum.value "_query" spSubstancePolymer__query A custom search profile that describes a specific defined query operation }
    spSubstancePolymer__security, {@enum.value "_security" spSubstancePolymer__security Security Labels applied to this resource }
    spSubstancePolymer__source, {@enum.value "_source" spSubstancePolymer__source Identifies where the resource comes from }
    spSubstancePolymer__tag, {@enum.value "_tag" spSubstancePolymer__tag Tags applied to this resource }
    spSubstancePolymer__text); {@enum.value "_text" spSubstancePolymer__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  {@Enum TSearchParamsSubstanceReferenceInformation
    Search Parameters for SubstanceReferenceInformation
  }
  TSearchParamsSubstanceReferenceInformation = (
    spSubstanceReferenceInformation__content, {@enum.value "_content" spSubstanceReferenceInformation__content Search on the entire content of the resource }
    spSubstanceReferenceInformation__id, {@enum.value "_id" spSubstanceReferenceInformation__id Logical id of this artifact }
    spSubstanceReferenceInformation__lastUpdated, {@enum.value "_lastUpdated" spSubstanceReferenceInformation__lastUpdated When the resource version last changed }
    spSubstanceReferenceInformation__profile, {@enum.value "_profile" spSubstanceReferenceInformation__profile Profiles this resource claims to conform to }
    spSubstanceReferenceInformation__query, {@enum.value "_query" spSubstanceReferenceInformation__query A custom search profile that describes a specific defined query operation }
    spSubstanceReferenceInformation__security, {@enum.value "_security" spSubstanceReferenceInformation__security Security Labels applied to this resource }
    spSubstanceReferenceInformation__source, {@enum.value "_source" spSubstanceReferenceInformation__source Identifies where the resource comes from }
    spSubstanceReferenceInformation__tag, {@enum.value "_tag" spSubstanceReferenceInformation__tag Tags applied to this resource }
    spSubstanceReferenceInformation__text); {@enum.value "_text" spSubstanceReferenceInformation__text Search on the narrative of the resource }
{$ENDIF}

{$IFDEF FHIR_SUBSTANCESPECIFICATION}
  {@Enum TSearchParamsSubstanceSpecification
    Search Parameters for SubstanceSpecification
  }
  TSearchParamsSubstanceSpecification = (
    spSubstanceSpecification__content, {@enum.value "_content" spSubstanceSpecification__content Search on the entire content of the resource }
    spSubstanceSpecification__id, {@enum.value "_id" spSubstanceSpecification__id Logical id of this artifact }
    spSubstanceSpecification__lastUpdated, {@enum.value "_lastUpdated" spSubstanceSpecification__lastUpdated When the resource version last changed }
    spSubstanceSpecification__profile, {@enum.value "_profile" spSubstanceSpecification__profile Profiles this resource claims to conform to }
    spSubstanceSpecification__query, {@enum.value "_query" spSubstanceSpecification__query A custom search profile that describes a specific defined query operation }
    spSubstanceSpecification__security, {@enum.value "_security" spSubstanceSpecification__security Security Labels applied to this resource }
    spSubstanceSpecification__source, {@enum.value "_source" spSubstanceSpecification__source Identifies where the resource comes from }
    spSubstanceSpecification__tag, {@enum.value "_tag" spSubstanceSpecification__tag Tags applied to this resource }
    spSubstanceSpecification__text); {@enum.value "_text" spSubstanceSpecification__text Search on the narrative of the resource }
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
    spSupplyDelivery__source, {@enum.value "_source" spSupplyDelivery__source Identifies where the resource comes from }
    spSupplyDelivery__tag, {@enum.value "_tag" spSupplyDelivery__tag Tags applied to this resource }
    spSupplyDelivery__text, {@enum.value "_text" spSupplyDelivery__text Search on the narrative of the resource }
    spSupplyDelivery_Identifier, {@enum.value "identifier" spSupplyDelivery_Identifier External identifier }
    spSupplyDelivery_Patient, {@enum.value "patient" spSupplyDelivery_Patient Patient for whom the item is supplied }
    spSupplyDelivery_Receiver, {@enum.value "receiver" spSupplyDelivery_Receiver Who collected the Supply }
    spSupplyDelivery_Status, {@enum.value "status" spSupplyDelivery_Status in-progress | completed | abandoned | entered-in-error }
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
    spSupplyRequest__source, {@enum.value "_source" spSupplyRequest__source Identifies where the resource comes from }
    spSupplyRequest__tag, {@enum.value "_tag" spSupplyRequest__tag Tags applied to this resource }
    spSupplyRequest__text, {@enum.value "_text" spSupplyRequest__text Search on the narrative of the resource }
    spSupplyRequest_Category, {@enum.value "category" spSupplyRequest_Category The kind of supply (central, non-stock, etc.) }
    spSupplyRequest_Date, {@enum.value "date" spSupplyRequest_Date When the request was made }
    spSupplyRequest_Identifier, {@enum.value "identifier" spSupplyRequest_Identifier Unique identifier }
    spSupplyRequest_Requester, {@enum.value "requester" spSupplyRequest_Requester Individual making the request }
    spSupplyRequest_Status, {@enum.value "status" spSupplyRequest_Status draft | active | suspended + }
    spSupplyRequest_Supplier); {@enum.value "supplier" spSupplyRequest_Supplier Who is intended to fulfill the request }
{$ENDIF}

{$IFDEF FHIR_TASK}
  {@Enum TSearchParamsTask
    Search Parameters for Task
  }
  TSearchParamsTask = (
    spTask__content, {@enum.value "_content" spTask__content Search on the entire content of the resource }
    spTask__id, {@enum.value "_id" spTask__id Logical id of this artifact }
    spTask__lastUpdated, {@enum.value "_lastUpdated" spTask__lastUpdated When the resource version last changed }
    spTask__profile, {@enum.value "_profile" spTask__profile Profiles this resource claims to conform to }
    spTask__query, {@enum.value "_query" spTask__query A custom search profile that describes a specific defined query operation }
    spTask__security, {@enum.value "_security" spTask__security Security Labels applied to this resource }
    spTask__source, {@enum.value "_source" spTask__source Identifies where the resource comes from }
    spTask__tag, {@enum.value "_tag" spTask__tag Tags applied to this resource }
    spTask__text, {@enum.value "_text" spTask__text Search on the narrative of the resource }
    spTask_Authoredon, {@enum.value "authored-on" spTask_Authoredon Search by creation date }
    spTask_Basedon, {@enum.value "based-on" spTask_Basedon Search by requests this task is based on }
    spTask_Businessstatus, {@enum.value "business-status" spTask_Businessstatus Search by business status }
    spTask_Code, {@enum.value "code" spTask_Code Search by task code }
    spTask_Context, {@enum.value "context" spTask_Context Search by encounter or episode }
    spTask_Focus, {@enum.value "focus" spTask_Focus Search by task focus }
    spTask_Groupidentifier, {@enum.value "group-identifier" spTask_Groupidentifier Search by group identifier }
    spTask_Identifier, {@enum.value "identifier" spTask_Identifier Search for a task instance by its business identifier }
    spTask_Intent, {@enum.value "intent" spTask_Intent Search by task intent }
    spTask_Modified, {@enum.value "modified" spTask_Modified Search by last modification date }
    spTask_Owner, {@enum.value "owner" spTask_Owner Search by task owner }
    spTask_Partof, {@enum.value "part-of" spTask_Partof Search by task this task is part of }
    spTask_Patient, {@enum.value "patient" spTask_Patient Search by patient }
    spTask_Performer, {@enum.value "performer" spTask_Performer Search by recommended type of performer (e.g., Requester, Performer, Scheduler). }
    spTask_Period, {@enum.value "period" spTask_Period Search by period Task is/was underway }
    spTask_Priority, {@enum.value "priority" spTask_Priority Search by task priority }
    spTask_Requester, {@enum.value "requester" spTask_Requester Search by task requester }
    spTask_Status, {@enum.value "status" spTask_Status Search by task status }
    spTask_Subject); {@enum.value "subject" spTask_Subject Search by subject }
{$ENDIF}

{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  {@Enum TSearchParamsTerminologyCapabilities
    Search Parameters for TerminologyCapabilities
  }
  TSearchParamsTerminologyCapabilities = (
    spTerminologyCapabilities__content, {@enum.value "_content" spTerminologyCapabilities__content Search on the entire content of the resource }
    spTerminologyCapabilities__id, {@enum.value "_id" spTerminologyCapabilities__id Logical id of this artifact }
    spTerminologyCapabilities__lastUpdated, {@enum.value "_lastUpdated" spTerminologyCapabilities__lastUpdated When the resource version last changed }
    spTerminologyCapabilities__profile, {@enum.value "_profile" spTerminologyCapabilities__profile Profiles this resource claims to conform to }
    spTerminologyCapabilities__query, {@enum.value "_query" spTerminologyCapabilities__query A custom search profile that describes a specific defined query operation }
    spTerminologyCapabilities__security, {@enum.value "_security" spTerminologyCapabilities__security Security Labels applied to this resource }
    spTerminologyCapabilities__source, {@enum.value "_source" spTerminologyCapabilities__source Identifies where the resource comes from }
    spTerminologyCapabilities__tag, {@enum.value "_tag" spTerminologyCapabilities__tag Tags applied to this resource }
    spTerminologyCapabilities__text, {@enum.value "_text" spTerminologyCapabilities__text Search on the narrative of the resource }
    spTerminologyCapabilities_Date, {@enum.value "date" spTerminologyCapabilities_Date The terminology capabilities publication date }
    spTerminologyCapabilities_Description, {@enum.value "description" spTerminologyCapabilities_Description The description of the terminology capabilities }
    spTerminologyCapabilities_Jurisdiction, {@enum.value "jurisdiction" spTerminologyCapabilities_Jurisdiction Intended jurisdiction for the terminology capabilities }
    spTerminologyCapabilities_Name, {@enum.value "name" spTerminologyCapabilities_Name Computationally friendly name of the terminology capabilities }
    spTerminologyCapabilities_Publisher, {@enum.value "publisher" spTerminologyCapabilities_Publisher Name of the publisher of the terminology capabilities }
    spTerminologyCapabilities_Status, {@enum.value "status" spTerminologyCapabilities_Status The current status of the terminology capabilities }
    spTerminologyCapabilities_Title, {@enum.value "title" spTerminologyCapabilities_Title The human-friendly name of the terminology capabilities }
    spTerminologyCapabilities_Url, {@enum.value "url" spTerminologyCapabilities_Url The uri that identifies the terminology capabilities }
    spTerminologyCapabilities_Version); {@enum.value "version" spTerminologyCapabilities_Version The business version of the terminology capabilities }
{$ENDIF}

{$IFDEF FHIR_TESTREPORT}
  {@Enum TSearchParamsTestReport
    Search Parameters for TestReport
  }
  TSearchParamsTestReport = (
    spTestReport__content, {@enum.value "_content" spTestReport__content Search on the entire content of the resource }
    spTestReport__id, {@enum.value "_id" spTestReport__id Logical id of this artifact }
    spTestReport__lastUpdated, {@enum.value "_lastUpdated" spTestReport__lastUpdated When the resource version last changed }
    spTestReport__profile, {@enum.value "_profile" spTestReport__profile Profiles this resource claims to conform to }
    spTestReport__query, {@enum.value "_query" spTestReport__query A custom search profile that describes a specific defined query operation }
    spTestReport__security, {@enum.value "_security" spTestReport__security Security Labels applied to this resource }
    spTestReport__source, {@enum.value "_source" spTestReport__source Identifies where the resource comes from }
    spTestReport__tag, {@enum.value "_tag" spTestReport__tag Tags applied to this resource }
    spTestReport__text, {@enum.value "_text" spTestReport__text Search on the narrative of the resource }
    spTestReport_Identifier, {@enum.value "identifier" spTestReport_Identifier An external identifier for the test report }
    spTestReport_Issued, {@enum.value "issued" spTestReport_Issued The test report generation date }
    spTestReport_Participant, {@enum.value "participant" spTestReport_Participant The reference to a participant in the test execution }
    spTestReport_Result, {@enum.value "result" spTestReport_Result The result disposition of the test execution }
    spTestReport_Tester, {@enum.value "tester" spTestReport_Tester The name of the testing organization }
    spTestReport_Testscript); {@enum.value "testscript" spTestReport_Testscript The test script executed to produce this report }
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
    spTestScript__source, {@enum.value "_source" spTestScript__source Identifies where the resource comes from }
    spTestScript__tag, {@enum.value "_tag" spTestScript__tag Tags applied to this resource }
    spTestScript__text, {@enum.value "_text" spTestScript__text Search on the narrative of the resource }
    spTestScript_Date, {@enum.value "date" spTestScript_Date The test script publication date }
    spTestScript_Description, {@enum.value "description" spTestScript_Description The description of the test script }
    spTestScript_Identifier, {@enum.value "identifier" spTestScript_Identifier External identifier for the test script }
    spTestScript_Jurisdiction, {@enum.value "jurisdiction" spTestScript_Jurisdiction Intended jurisdiction for the test script }
    spTestScript_Name, {@enum.value "name" spTestScript_Name Computationally friendly name of the test script }
    spTestScript_Publisher, {@enum.value "publisher" spTestScript_Publisher Name of the publisher of the test script }
    spTestScript_Status, {@enum.value "status" spTestScript_Status The current status of the test script }
    spTestScript_Testscriptcapability, {@enum.value "testscript-capability" spTestScript_Testscriptcapability TestScript required and validated capability }
    spTestScript_Title, {@enum.value "title" spTestScript_Title The human-friendly name of the test script }
    spTestScript_Url, {@enum.value "url" spTestScript_Url The uri that identifies the test script }
    spTestScript_Version); {@enum.value "version" spTestScript_Version The business version of the test script }
{$ENDIF}

{$IFDEF FHIR_USERSESSION}
  {@Enum TSearchParamsUserSession
    Search Parameters for UserSession
  }
  TSearchParamsUserSession = (
    spUserSession__content, {@enum.value "_content" spUserSession__content Search on the entire content of the resource }
    spUserSession__id, {@enum.value "_id" spUserSession__id Logical id of this artifact }
    spUserSession__lastUpdated, {@enum.value "_lastUpdated" spUserSession__lastUpdated When the resource version last changed }
    spUserSession__profile, {@enum.value "_profile" spUserSession__profile Profiles this resource claims to conform to }
    spUserSession__query, {@enum.value "_query" spUserSession__query A custom search profile that describes a specific defined query operation }
    spUserSession__security, {@enum.value "_security" spUserSession__security Security Labels applied to this resource }
    spUserSession__source, {@enum.value "_source" spUserSession__source Identifies where the resource comes from }
    spUserSession__tag, {@enum.value "_tag" spUserSession__tag Tags applied to this resource }
    spUserSession__text, {@enum.value "_text" spUserSession__text Search on the narrative of the resource }
    spUserSession_Focus, {@enum.value "focus" spUserSession_Focus The focus of the user session }
    spUserSession_Identifier, {@enum.value "identifier" spUserSession_Identifier External identifiers for the user session }
    spUserSession_Patient, {@enum.value "patient" spUserSession_Patient The identity of a patient to search for user sessions }
    spUserSession_Status, {@enum.value "status" spUserSession_Status The status of the user session }
    spUserSession_User, {@enum.value "user" spUserSession_User The user of the session }
    spUserSession_Workstation); {@enum.value "workstation" spUserSession_Workstation The workstation of the session }
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
    spValueSet__source, {@enum.value "_source" spValueSet__source Identifies where the resource comes from }
    spValueSet__tag, {@enum.value "_tag" spValueSet__tag Tags applied to this resource }
    spValueSet__text, {@enum.value "_text" spValueSet__text Search on the narrative of the resource }
    spValueSet_Code, {@enum.value "code" spValueSet_Code This special parameter searches for codes in the value set. See additional notes on the ValueSet resource }
    spValueSet_Date, {@enum.value "date" spValueSet_Date The value set publication date }
    spValueSet_Description, {@enum.value "description" spValueSet_Description The description of the value set }
    spValueSet_Expansion, {@enum.value "expansion" spValueSet_Expansion Uniquely identifies this expansion }
    spValueSet_Identifier, {@enum.value "identifier" spValueSet_Identifier External identifier for the value set }
    spValueSet_Jurisdiction, {@enum.value "jurisdiction" spValueSet_Jurisdiction Intended jurisdiction for the value set }
    spValueSet_Name, {@enum.value "name" spValueSet_Name Computationally friendly name of the value set }
    spValueSet_Publisher, {@enum.value "publisher" spValueSet_Publisher Name of the publisher of the value set }
    spValueSet_Reference, {@enum.value "reference" spValueSet_Reference A code system included or excluded in the value set or an imported value set }
    spValueSet_Status, {@enum.value "status" spValueSet_Status The current status of the value set }
    spValueSet_Title, {@enum.value "title" spValueSet_Title The human-friendly name of the value set }
    spValueSet_Url, {@enum.value "url" spValueSet_Url The uri that identifies the value set }
    spValueSet_Version); {@enum.value "version" spValueSet_Version The business version of the value set }
{$ENDIF}

{$IFDEF FHIR_VERIFICATIONRESULT}
  {@Enum TSearchParamsVerificationResult
    Search Parameters for VerificationResult
  }
  TSearchParamsVerificationResult = (
    spVerificationResult__content, {@enum.value "_content" spVerificationResult__content Search on the entire content of the resource }
    spVerificationResult__id, {@enum.value "_id" spVerificationResult__id Logical id of this artifact }
    spVerificationResult__lastUpdated, {@enum.value "_lastUpdated" spVerificationResult__lastUpdated When the resource version last changed }
    spVerificationResult__profile, {@enum.value "_profile" spVerificationResult__profile Profiles this resource claims to conform to }
    spVerificationResult__query, {@enum.value "_query" spVerificationResult__query A custom search profile that describes a specific defined query operation }
    spVerificationResult__security, {@enum.value "_security" spVerificationResult__security Security Labels applied to this resource }
    spVerificationResult__source, {@enum.value "_source" spVerificationResult__source Identifies where the resource comes from }
    spVerificationResult__tag, {@enum.value "_tag" spVerificationResult__tag Tags applied to this resource }
    spVerificationResult__text, {@enum.value "_text" spVerificationResult__text Search on the narrative of the resource }
    spVerificationResult_Target); {@enum.value "target" spVerificationResult_Target A resource that was validated }
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
    spVisionPrescription__source, {@enum.value "_source" spVisionPrescription__source Identifies where the resource comes from }
    spVisionPrescription__tag, {@enum.value "_tag" spVisionPrescription__tag Tags applied to this resource }
    spVisionPrescription__text, {@enum.value "_text" spVisionPrescription__text Search on the narrative of the resource }
    spVisionPrescription_Datewritten, {@enum.value "datewritten" spVisionPrescription_Datewritten Return prescriptions written on this date }
    spVisionPrescription_Encounter, {@enum.value "encounter" spVisionPrescription_Encounter Return prescriptions with this encounter identifier }
    spVisionPrescription_Identifier, {@enum.value "identifier" spVisionPrescription_Identifier Return prescriptions with this external identifier }
    spVisionPrescription_Patient, {@enum.value "patient" spVisionPrescription_Patient The identity of a patient to list dispenses for }
    spVisionPrescription_Prescriber, {@enum.value "prescriber" spVisionPrescription_Prescriber Who authorizes the vision product }
    spVisionPrescription_Status); {@enum.value "status" spVisionPrescription_Status The status of the vision prescription }
{$ENDIF}

Const
  CODES_TFhirResourceType : Array[TFhirResourceType] of String = ('', {$IFDEF FHIR_ACCOUNT}'Account',{$ENDIF}
      {$IFDEF FHIR_ACTIVITYDEFINITION}'ActivityDefinition',{$ENDIF}
      {$IFDEF FHIR_ADVERSEEVENT}'AdverseEvent',{$ENDIF}
      {$IFDEF FHIR_ALLERGYINTOLERANCE}'AllergyIntolerance',{$ENDIF}
      {$IFDEF FHIR_APPOINTMENT}'Appointment',{$ENDIF}
      {$IFDEF FHIR_APPOINTMENTRESPONSE}'AppointmentResponse',{$ENDIF}
      {$IFDEF FHIR_AUDITEVENT}'AuditEvent',{$ENDIF}
      {$IFDEF FHIR_BASIC}'Basic',{$ENDIF}
      {$IFDEF FHIR_BINARY}'Binary',{$ENDIF}
      {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}'BiologicallyDerivedProduct',{$ENDIF}
      {$IFDEF FHIR_BODYSTRUCTURE}'BodyStructure',{$ENDIF}
      {$IFDEF FHIR_BUNDLE}'Bundle',{$ENDIF}
      {$IFDEF FHIR_CAPABILITYSTATEMENT}'CapabilityStatement',{$ENDIF}
      {$IFDEF FHIR_CAREPLAN}'CarePlan',{$ENDIF}
      {$IFDEF FHIR_CARETEAM}'CareTeam',{$ENDIF}
      {$IFDEF FHIR_CHARGEITEM}'ChargeItem',{$ENDIF}
      {$IFDEF FHIR_CLAIM}'Claim',{$ENDIF}
      {$IFDEF FHIR_CLAIMRESPONSE}'ClaimResponse',{$ENDIF}
      {$IFDEF FHIR_CLINICALIMPRESSION}'ClinicalImpression',{$ENDIF}
      {$IFDEF FHIR_CODESYSTEM}'CodeSystem',{$ENDIF}
      {$IFDEF FHIR_COMMUNICATION}'Communication',{$ENDIF}
      {$IFDEF FHIR_COMMUNICATIONREQUEST}'CommunicationRequest',{$ENDIF}
      {$IFDEF FHIR_COMPARTMENTDEFINITION}'CompartmentDefinition',{$ENDIF}
      {$IFDEF FHIR_COMPOSITION}'Composition',{$ENDIF}
      {$IFDEF FHIR_CONCEPTMAP}'ConceptMap',{$ENDIF}
      {$IFDEF FHIR_CONDITION}'Condition',{$ENDIF}
      {$IFDEF FHIR_CONSENT}'Consent',{$ENDIF}
      {$IFDEF FHIR_CONTRACT}'Contract',{$ENDIF}
      {$IFDEF FHIR_COVERAGE}'Coverage',{$ENDIF}
      {$IFDEF FHIR_DETECTEDISSUE}'DetectedIssue',{$ENDIF}
      {$IFDEF FHIR_DEVICE}'Device',{$ENDIF}
      {$IFDEF FHIR_DEVICECOMPONENT}'DeviceComponent',{$ENDIF}
      {$IFDEF FHIR_DEVICEMETRIC}'DeviceMetric',{$ENDIF}
      {$IFDEF FHIR_DEVICEREQUEST}'DeviceRequest',{$ENDIF}
      {$IFDEF FHIR_DEVICEUSESTATEMENT}'DeviceUseStatement',{$ENDIF}
      {$IFDEF FHIR_DIAGNOSTICREPORT}'DiagnosticReport',{$ENDIF}
      {$IFDEF FHIR_DOCUMENTMANIFEST}'DocumentManifest',{$ENDIF}
      {$IFDEF FHIR_DOCUMENTREFERENCE}'DocumentReference',{$ENDIF}
      {$IFDEF FHIR_ELIGIBILITYREQUEST}'EligibilityRequest',{$ENDIF}
      {$IFDEF FHIR_ELIGIBILITYRESPONSE}'EligibilityResponse',{$ENDIF}
      {$IFDEF FHIR_ENCOUNTER}'Encounter',{$ENDIF}
      {$IFDEF FHIR_ENDPOINT}'Endpoint',{$ENDIF}
      {$IFDEF FHIR_ENROLLMENTREQUEST}'EnrollmentRequest',{$ENDIF}
      {$IFDEF FHIR_ENROLLMENTRESPONSE}'EnrollmentResponse',{$ENDIF}
      {$IFDEF FHIR_ENTRYDEFINITION}'EntryDefinition',{$ENDIF}
      {$IFDEF FHIR_EPISODEOFCARE}'EpisodeOfCare',{$ENDIF}
      {$IFDEF FHIR_EVENTDEFINITION}'EventDefinition',{$ENDIF}
      {$IFDEF FHIR_EXAMPLESCENARIO}'ExampleScenario',{$ENDIF}
      {$IFDEF FHIR_EXPANSIONPROFILE}'ExpansionProfile',{$ENDIF}
      {$IFDEF FHIR_EXPLANATIONOFBENEFIT}'ExplanationOfBenefit',{$ENDIF}
      {$IFDEF FHIR_FAMILYMEMBERHISTORY}'FamilyMemberHistory',{$ENDIF}
      {$IFDEF FHIR_FLAG}'Flag',{$ENDIF}
      {$IFDEF FHIR_GOAL}'Goal',{$ENDIF}
      {$IFDEF FHIR_GRAPHDEFINITION}'GraphDefinition',{$ENDIF}
      {$IFDEF FHIR_GROUP}'Group',{$ENDIF}
      {$IFDEF FHIR_GUIDANCERESPONSE}'GuidanceResponse',{$ENDIF}
      
      {$IFDEF FHIR_HEALTHCARESERVICE}'HealthcareService',{$ENDIF}
      {$IFDEF FHIR_IMAGINGSTUDY}'ImagingStudy',{$ENDIF}
      {$IFDEF FHIR_IMMUNIZATION}'Immunization',{$ENDIF}
      {$IFDEF FHIR_IMMUNIZATIONEVALUATION}'ImmunizationEvaluation',{$ENDIF}
      {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}'ImmunizationRecommendation',{$ENDIF}
      {$IFDEF FHIR_IMPLEMENTATIONGUIDE}'ImplementationGuide',{$ENDIF}
      {$IFDEF FHIR_IMPLEMENTATIONGUIDEINPUT}'ImplementationGuideInput',{$ENDIF}
      {$IFDEF FHIR_IMPLEMENTATIONGUIDEOUTPUT}'ImplementationGuideOutput',{$ENDIF}
      {$IFDEF FHIR_INVOICE}'Invoice',{$ENDIF}
      {$IFDEF FHIR_ITEMINSTANCE}'ItemInstance',{$ENDIF}
      {$IFDEF FHIR_LIBRARY}'Library',{$ENDIF}
      {$IFDEF FHIR_LINKAGE}'Linkage',{$ENDIF}
      {$IFDEF FHIR_LIST}'List',{$ENDIF}
      {$IFDEF FHIR_LOCATION}'Location',{$ENDIF}
      {$IFDEF FHIR_MEASURE}'Measure',{$ENDIF}
      {$IFDEF FHIR_MEASUREREPORT}'MeasureReport',{$ENDIF}
      {$IFDEF FHIR_MEDIA}'Media',{$ENDIF}
      {$IFDEF FHIR_MEDICATION}'Medication',{$ENDIF}
      {$IFDEF FHIR_MEDICATIONADMINISTRATION}'MedicationAdministration',{$ENDIF}
      {$IFDEF FHIR_MEDICATIONDISPENSE}'MedicationDispense',{$ENDIF}
      {$IFDEF FHIR_MEDICATIONREQUEST}'MedicationRequest',{$ENDIF}
      {$IFDEF FHIR_MEDICATIONSTATEMENT}'MedicationStatement',{$ENDIF}
      {$IFDEF FHIR_MEDICINALPRODUCT}'MedicinalProduct',{$ENDIF}
      {$IFDEF FHIR_MEDICINALPRODUCTAUTHORIZATION}'MedicinalProductAuthorization',{$ENDIF}
      {$IFDEF FHIR_MEDICINALPRODUCTCLINICALS}'MedicinalProductClinicals',{$ENDIF}
      {$IFDEF FHIR_MEDICINALPRODUCTDEVICESPEC}'MedicinalProductDeviceSpec',{$ENDIF}
      {$IFDEF FHIR_MEDICINALPRODUCTINGREDIENT}'MedicinalProductIngredient',{$ENDIF}
      {$IFDEF FHIR_MEDICINALPRODUCTPACKAGED}'MedicinalProductPackaged',{$ENDIF}
      {$IFDEF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}'MedicinalProductPharmaceutical',{$ENDIF}
      {$IFDEF FHIR_MESSAGEDEFINITION}'MessageDefinition',{$ENDIF}
      {$IFDEF FHIR_MESSAGEHEADER}'MessageHeader',{$ENDIF}
      {$IFDEF FHIR_NAMINGSYSTEM}'NamingSystem',{$ENDIF}
      {$IFDEF FHIR_NUTRITIONORDER}'NutritionOrder',{$ENDIF}
      {$IFDEF FHIR_OBSERVATION}'Observation',{$ENDIF}
      {$IFDEF FHIR_OBSERVATIONDEFINITION}'ObservationDefinition',{$ENDIF}
      {$IFDEF FHIR_OCCUPATIONALDATA}'OccupationalData',{$ENDIF}
      {$IFDEF FHIR_OPERATIONDEFINITION}'OperationDefinition',{$ENDIF}
      {$IFDEF FHIR_OPERATIONOUTCOME}'OperationOutcome',{$ENDIF}
      {$IFDEF FHIR_ORGANIZATION}'Organization',{$ENDIF}
      {$IFDEF FHIR_ORGANIZATIONROLE}'OrganizationRole',{$ENDIF}
      {$IFDEF FHIR_PARAMETERS}'Parameters',{$ENDIF}
      {$IFDEF FHIR_PATIENT}'Patient',{$ENDIF}
      {$IFDEF FHIR_PAYMENTNOTICE}'PaymentNotice',{$ENDIF}
      {$IFDEF FHIR_PAYMENTRECONCILIATION}'PaymentReconciliation',{$ENDIF}
      {$IFDEF FHIR_PERSON}'Person',{$ENDIF}
      {$IFDEF FHIR_PLANDEFINITION}'PlanDefinition',{$ENDIF}
      {$IFDEF FHIR_PRACTITIONER}'Practitioner',{$ENDIF}
      {$IFDEF FHIR_PRACTITIONERROLE}'PractitionerRole',{$ENDIF}
      {$IFDEF FHIR_PROCEDURE}'Procedure',{$ENDIF}
      {$IFDEF FHIR_PROCESSREQUEST}'ProcessRequest',{$ENDIF}
      {$IFDEF FHIR_PROCESSRESPONSE}'ProcessResponse',{$ENDIF}
      
      {$IFDEF FHIR_PRODUCTPLAN}'ProductPlan',{$ENDIF}
      {$IFDEF FHIR_PROVENANCE}'Provenance',{$ENDIF}
      {$IFDEF FHIR_QUESTIONNAIRE}'Questionnaire',{$ENDIF}
      {$IFDEF FHIR_QUESTIONNAIRERESPONSE}'QuestionnaireResponse',{$ENDIF}
      {$IFDEF FHIR_RELATEDPERSON}'RelatedPerson',{$ENDIF}
      {$IFDEF FHIR_REQUESTGROUP}'RequestGroup',{$ENDIF}
      {$IFDEF FHIR_RESEARCHSTUDY}'ResearchStudy',{$ENDIF}
      {$IFDEF FHIR_RESEARCHSUBJECT}'ResearchSubject',{$ENDIF}
      {$IFDEF FHIR_RISKASSESSMENT}'RiskAssessment',{$ENDIF}
      {$IFDEF FHIR_SCHEDULE}'Schedule',{$ENDIF}
      {$IFDEF FHIR_SEARCHPARAMETER}'SearchParameter',{$ENDIF}
      {$IFDEF FHIR_SEQUENCE}'Sequence',{$ENDIF}
      {$IFDEF FHIR_SERVICEDEFINITION}'ServiceDefinition',{$ENDIF}
      {$IFDEF FHIR_SERVICEREQUEST}'ServiceRequest',{$ENDIF}
      {$IFDEF FHIR_SLOT}'Slot',{$ENDIF}
      {$IFDEF FHIR_SPECIMEN}'Specimen',{$ENDIF}
      {$IFDEF FHIR_SPECIMENDEFINITION}'SpecimenDefinition',{$ENDIF}
      {$IFDEF FHIR_STRUCTUREDEFINITION}'StructureDefinition',{$ENDIF}
      {$IFDEF FHIR_STRUCTUREMAP}'StructureMap',{$ENDIF}
      {$IFDEF FHIR_SUBSCRIPTION}'Subscription',{$ENDIF}
      {$IFDEF FHIR_SUBSTANCE}'Substance',{$ENDIF}
      {$IFDEF FHIR_SUBSTANCEPOLYMER}'SubstancePolymer',{$ENDIF}
      {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}'SubstanceReferenceInformation',{$ENDIF}
      {$IFDEF FHIR_SUBSTANCESPECIFICATION}'SubstanceSpecification',{$ENDIF}
      {$IFDEF FHIR_SUPPLYDELIVERY}'SupplyDelivery',{$ENDIF}
      {$IFDEF FHIR_SUPPLYREQUEST}'SupplyRequest',{$ENDIF}
      {$IFDEF FHIR_TASK}'Task',{$ENDIF}
      {$IFDEF FHIR_TERMINOLOGYCAPABILITIES}'TerminologyCapabilities',{$ENDIF}
      {$IFDEF FHIR_TESTREPORT}'TestReport',{$ENDIF}
      {$IFDEF FHIR_TESTSCRIPT}'TestScript',{$ENDIF}
      {$IFDEF FHIR_USERSESSION}'UserSession',{$ENDIF}
      {$IFDEF FHIR_VALUESET}'ValueSet',{$ENDIF}
      {$IFDEF FHIR_VERIFICATIONRESULT}'VerificationResult',{$ENDIF}
      {$IFDEF FHIR_VISIONPRESCRIPTION}'VisionPrescription',{$ENDIF}
       'Custom');
  LOWERCASE_CODES_TFhirResourceType : Array[TFhirResourceType] of String = ('', {$IFDEF FHIR_ACCOUNT}'account',{$ENDIF}
     {$IFDEF FHIR_ACTIVITYDEFINITION}'activitydefinition',{$ENDIF}
     {$IFDEF FHIR_ADVERSEEVENT}'adverseevent',{$ENDIF}
     {$IFDEF FHIR_ALLERGYINTOLERANCE}'allergyintolerance',{$ENDIF}
     {$IFDEF FHIR_APPOINTMENT}'appointment',{$ENDIF}
     {$IFDEF FHIR_APPOINTMENTRESPONSE}'appointmentresponse',{$ENDIF}
     {$IFDEF FHIR_AUDITEVENT}'auditevent',{$ENDIF}
     {$IFDEF FHIR_BASIC}'basic',{$ENDIF}
     {$IFDEF FHIR_BINARY}'binary',{$ENDIF}
     {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}'biologicallyderivedproduct',{$ENDIF}
     {$IFDEF FHIR_BODYSTRUCTURE}'bodystructure',{$ENDIF}
     {$IFDEF FHIR_BUNDLE}'bundle',{$ENDIF}
     {$IFDEF FHIR_CAPABILITYSTATEMENT}'capabilitystatement',{$ENDIF}
     {$IFDEF FHIR_CAREPLAN}'careplan',{$ENDIF}
     {$IFDEF FHIR_CARETEAM}'careteam',{$ENDIF}
     {$IFDEF FHIR_CHARGEITEM}'chargeitem',{$ENDIF}
     {$IFDEF FHIR_CLAIM}'claim',{$ENDIF}
     {$IFDEF FHIR_CLAIMRESPONSE}'claimresponse',{$ENDIF}
     {$IFDEF FHIR_CLINICALIMPRESSION}'clinicalimpression',{$ENDIF}
     {$IFDEF FHIR_CODESYSTEM}'codesystem',{$ENDIF}
     {$IFDEF FHIR_COMMUNICATION}'communication',{$ENDIF}
     {$IFDEF FHIR_COMMUNICATIONREQUEST}'communicationrequest',{$ENDIF}
     {$IFDEF FHIR_COMPARTMENTDEFINITION}'compartmentdefinition',{$ENDIF}
     {$IFDEF FHIR_COMPOSITION}'composition',{$ENDIF}
     {$IFDEF FHIR_CONCEPTMAP}'conceptmap',{$ENDIF}
     {$IFDEF FHIR_CONDITION}'condition',{$ENDIF}
     {$IFDEF FHIR_CONSENT}'consent',{$ENDIF}
     {$IFDEF FHIR_CONTRACT}'contract',{$ENDIF}
     {$IFDEF FHIR_COVERAGE}'coverage',{$ENDIF}
     {$IFDEF FHIR_DETECTEDISSUE}'detectedissue',{$ENDIF}
     {$IFDEF FHIR_DEVICE}'device',{$ENDIF}
     {$IFDEF FHIR_DEVICECOMPONENT}'devicecomponent',{$ENDIF}
     {$IFDEF FHIR_DEVICEMETRIC}'devicemetric',{$ENDIF}
     {$IFDEF FHIR_DEVICEREQUEST}'devicerequest',{$ENDIF}
     {$IFDEF FHIR_DEVICEUSESTATEMENT}'deviceusestatement',{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICREPORT}'diagnosticreport',{$ENDIF}
     {$IFDEF FHIR_DOCUMENTMANIFEST}'documentmanifest',{$ENDIF}
     {$IFDEF FHIR_DOCUMENTREFERENCE}'documentreference',{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYREQUEST}'eligibilityrequest',{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYRESPONSE}'eligibilityresponse',{$ENDIF}
     {$IFDEF FHIR_ENCOUNTER}'encounter',{$ENDIF}
     {$IFDEF FHIR_ENDPOINT}'endpoint',{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTREQUEST}'enrollmentrequest',{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTRESPONSE}'enrollmentresponse',{$ENDIF}
     {$IFDEF FHIR_ENTRYDEFINITION}'entrydefinition',{$ENDIF}
     {$IFDEF FHIR_EPISODEOFCARE}'episodeofcare',{$ENDIF}
     {$IFDEF FHIR_EVENTDEFINITION}'eventdefinition',{$ENDIF}
     {$IFDEF FHIR_EXAMPLESCENARIO}'examplescenario',{$ENDIF}
     {$IFDEF FHIR_EXPANSIONPROFILE}'expansionprofile',{$ENDIF}
     {$IFDEF FHIR_EXPLANATIONOFBENEFIT}'explanationofbenefit',{$ENDIF}
     {$IFDEF FHIR_FAMILYMEMBERHISTORY}'familymemberhistory',{$ENDIF}
     {$IFDEF FHIR_FLAG}'flag',{$ENDIF}
     {$IFDEF FHIR_GOAL}'goal',{$ENDIF}
     {$IFDEF FHIR_GRAPHDEFINITION}'graphdefinition',{$ENDIF}
     {$IFDEF FHIR_GROUP}'group',{$ENDIF}
     {$IFDEF FHIR_GUIDANCERESPONSE}'guidanceresponse',{$ENDIF}
     {$IFDEF FHIR_HEALTHCARESERVICE}'healthcareservice',{$ENDIF}
     {$IFDEF FHIR_IMAGINGSTUDY}'imagingstudy',{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATION}'immunization',{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONEVALUATION}'immunizationevaluation',{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}'immunizationrecommendation',{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDE}'implementationguide',{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDEINPUT}'implementationguideinput',{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDEOUTPUT}'implementationguideoutput',{$ENDIF}
     {$IFDEF FHIR_INVOICE}'invoice',{$ENDIF}
     {$IFDEF FHIR_ITEMINSTANCE}'iteminstance',{$ENDIF}
     {$IFDEF FHIR_LIBRARY}'library',{$ENDIF}
     {$IFDEF FHIR_LINKAGE}'linkage',{$ENDIF}
     {$IFDEF FHIR_LIST}'list',{$ENDIF}
     {$IFDEF FHIR_LOCATION}'location',{$ENDIF}
     {$IFDEF FHIR_MEASURE}'measure',{$ENDIF}
     {$IFDEF FHIR_MEASUREREPORT}'measurereport',{$ENDIF}
     {$IFDEF FHIR_MEDIA}'media',{$ENDIF}
     {$IFDEF FHIR_MEDICATION}'medication',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONADMINISTRATION}'medicationadministration',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONDISPENSE}'medicationdispense',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONREQUEST}'medicationrequest',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONSTATEMENT}'medicationstatement',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCT}'medicinalproduct',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTAUTHORIZATION}'medicinalproductauthorization',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTCLINICALS}'medicinalproductclinicals',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTDEVICESPEC}'medicinalproductdevicespec',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTINGREDIENT}'medicinalproductingredient',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTPACKAGED}'medicinalproductpackaged',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}'medicinalproductpharmaceutical',{$ENDIF}
     {$IFDEF FHIR_MESSAGEDEFINITION}'messagedefinition',{$ENDIF}
     {$IFDEF FHIR_MESSAGEHEADER}'messageheader',{$ENDIF}
     {$IFDEF FHIR_NAMINGSYSTEM}'namingsystem',{$ENDIF}
     {$IFDEF FHIR_NUTRITIONORDER}'nutritionorder',{$ENDIF}
     {$IFDEF FHIR_OBSERVATION}'observation',{$ENDIF}
     {$IFDEF FHIR_OBSERVATIONDEFINITION}'observationdefinition',{$ENDIF}
     {$IFDEF FHIR_OCCUPATIONALDATA}'occupationaldata',{$ENDIF}
     {$IFDEF FHIR_OPERATIONDEFINITION}'operationdefinition',{$ENDIF}
     {$IFDEF FHIR_OPERATIONOUTCOME}'operationoutcome',{$ENDIF}
     {$IFDEF FHIR_ORGANIZATION}'organization',{$ENDIF}
     {$IFDEF FHIR_ORGANIZATIONROLE}'organizationrole',{$ENDIF}
     {$IFDEF FHIR_PARAMETERS}'parameters',{$ENDIF}
     {$IFDEF FHIR_PATIENT}'patient',{$ENDIF}
     {$IFDEF FHIR_PAYMENTNOTICE}'paymentnotice',{$ENDIF}
     {$IFDEF FHIR_PAYMENTRECONCILIATION}'paymentreconciliation',{$ENDIF}
     {$IFDEF FHIR_PERSON}'person',{$ENDIF}
     {$IFDEF FHIR_PLANDEFINITION}'plandefinition',{$ENDIF}
     {$IFDEF FHIR_PRACTITIONER}'practitioner',{$ENDIF}
     {$IFDEF FHIR_PRACTITIONERROLE}'practitionerrole',{$ENDIF}
     {$IFDEF FHIR_PROCEDURE}'procedure',{$ENDIF}
     {$IFDEF FHIR_PROCESSREQUEST}'processrequest',{$ENDIF}
     {$IFDEF FHIR_PROCESSRESPONSE}'processresponse',{$ENDIF}
     {$IFDEF FHIR_PRODUCTPLAN}'productplan',{$ENDIF}
     {$IFDEF FHIR_PROVENANCE}'provenance',{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRE}'questionnaire',{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRERESPONSE}'questionnaireresponse',{$ENDIF}
     {$IFDEF FHIR_RELATEDPERSON}'relatedperson',{$ENDIF}
     {$IFDEF FHIR_REQUESTGROUP}'requestgroup',{$ENDIF}
     {$IFDEF FHIR_RESEARCHSTUDY}'researchstudy',{$ENDIF}
     {$IFDEF FHIR_RESEARCHSUBJECT}'researchsubject',{$ENDIF}
     {$IFDEF FHIR_RISKASSESSMENT}'riskassessment',{$ENDIF}
     {$IFDEF FHIR_SCHEDULE}'schedule',{$ENDIF}
     {$IFDEF FHIR_SEARCHPARAMETER}'searchparameter',{$ENDIF}
     {$IFDEF FHIR_SEQUENCE}'sequence',{$ENDIF}
     {$IFDEF FHIR_SERVICEDEFINITION}'servicedefinition',{$ENDIF}
     {$IFDEF FHIR_SERVICEREQUEST}'servicerequest',{$ENDIF}
     {$IFDEF FHIR_SLOT}'slot',{$ENDIF}
     {$IFDEF FHIR_SPECIMEN}'specimen',{$ENDIF}
     {$IFDEF FHIR_SPECIMENDEFINITION}'specimendefinition',{$ENDIF}
     {$IFDEF FHIR_STRUCTUREDEFINITION}'structuredefinition',{$ENDIF}
     {$IFDEF FHIR_STRUCTUREMAP}'structuremap',{$ENDIF}
     {$IFDEF FHIR_SUBSCRIPTION}'subscription',{$ENDIF}
     {$IFDEF FHIR_SUBSTANCE}'substance',{$ENDIF}
     {$IFDEF FHIR_SUBSTANCEPOLYMER}'substancepolymer',{$ENDIF}
     {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}'substancereferenceinformation',{$ENDIF}
     {$IFDEF FHIR_SUBSTANCESPECIFICATION}'substancespecification',{$ENDIF}
     {$IFDEF FHIR_SUPPLYDELIVERY}'supplydelivery',{$ENDIF}
     {$IFDEF FHIR_SUPPLYREQUEST}'supplyrequest',{$ENDIF}
     {$IFDEF FHIR_TASK}'task',{$ENDIF}
     {$IFDEF FHIR_TERMINOLOGYCAPABILITIES}'terminologycapabilities',{$ENDIF}
     {$IFDEF FHIR_TESTREPORT}'testreport',{$ENDIF}
     {$IFDEF FHIR_TESTSCRIPT}'testscript',{$ENDIF}
     {$IFDEF FHIR_USERSESSION}'usersession',{$ENDIF}
     {$IFDEF FHIR_VALUESET}'valueset',{$ENDIF}
     {$IFDEF FHIR_VERIFICATIONRESULT}'verificationresult',{$ENDIF}
     {$IFDEF FHIR_VISIONPRESCRIPTION}'visionprescription',{$ENDIF}
     'custom');
     
  CLASSES_TFhirResourceType : Array[TFhirResourceType] of TFhirResourceClass = (nil, {$IFDEF FHIR_ACCOUNT}TFhirAccount,{$ENDIF}
     {$IFDEF FHIR_ACTIVITYDEFINITION}TFhirActivityDefinition,{$ENDIF}
     {$IFDEF FHIR_ADVERSEEVENT}TFhirAdverseEvent,{$ENDIF}
     {$IFDEF FHIR_ALLERGYINTOLERANCE}TFhirAllergyIntolerance,{$ENDIF}
     {$IFDEF FHIR_APPOINTMENT}TFhirAppointment,{$ENDIF}
     {$IFDEF FHIR_APPOINTMENTRESPONSE}TFhirAppointmentResponse,{$ENDIF}
     {$IFDEF FHIR_AUDITEVENT}TFhirAuditEvent,{$ENDIF}
     {$IFDEF FHIR_BASIC}TFhirBasic,{$ENDIF}
     {$IFDEF FHIR_BINARY}TFhirBinary,{$ENDIF}
     {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}TFhirBiologicallyDerivedProduct,{$ENDIF}
     {$IFDEF FHIR_BODYSTRUCTURE}TFhirBodyStructure,{$ENDIF}
     {$IFDEF FHIR_BUNDLE}TFhirBundle,{$ENDIF}
     {$IFDEF FHIR_CAPABILITYSTATEMENT}TFhirCapabilityStatement,{$ENDIF}
     {$IFDEF FHIR_CAREPLAN}TFhirCarePlan,{$ENDIF}
     {$IFDEF FHIR_CARETEAM}TFhirCareTeam,{$ENDIF}
     {$IFDEF FHIR_CHARGEITEM}TFhirChargeItem,{$ENDIF}
     {$IFDEF FHIR_CLAIM}TFhirClaim,{$ENDIF}
     {$IFDEF FHIR_CLAIMRESPONSE}TFhirClaimResponse,{$ENDIF}
     {$IFDEF FHIR_CLINICALIMPRESSION}TFhirClinicalImpression,{$ENDIF}
     {$IFDEF FHIR_CODESYSTEM}TFhirCodeSystem,{$ENDIF}
     {$IFDEF FHIR_COMMUNICATION}TFhirCommunication,{$ENDIF}
     {$IFDEF FHIR_COMMUNICATIONREQUEST}TFhirCommunicationRequest,{$ENDIF}
     {$IFDEF FHIR_COMPARTMENTDEFINITION}TFhirCompartmentDefinition,{$ENDIF}
     {$IFDEF FHIR_COMPOSITION}TFhirComposition,{$ENDIF}
     {$IFDEF FHIR_CONCEPTMAP}TFhirConceptMap,{$ENDIF}
     {$IFDEF FHIR_CONDITION}TFhirCondition,{$ENDIF}
     {$IFDEF FHIR_CONSENT}TFhirConsent,{$ENDIF}
     {$IFDEF FHIR_CONTRACT}TFhirContract,{$ENDIF}
     {$IFDEF FHIR_COVERAGE}TFhirCoverage,{$ENDIF}
     {$IFDEF FHIR_DETECTEDISSUE}TFhirDetectedIssue,{$ENDIF}
     {$IFDEF FHIR_DEVICE}TFhirDevice,{$ENDIF}
     {$IFDEF FHIR_DEVICECOMPONENT}TFhirDeviceComponent,{$ENDIF}
     {$IFDEF FHIR_DEVICEMETRIC}TFhirDeviceMetric,{$ENDIF}
     {$IFDEF FHIR_DEVICEREQUEST}TFhirDeviceRequest,{$ENDIF}
     {$IFDEF FHIR_DEVICEUSESTATEMENT}TFhirDeviceUseStatement,{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICREPORT}TFhirDiagnosticReport,{$ENDIF}
     {$IFDEF FHIR_DOCUMENTMANIFEST}TFhirDocumentManifest,{$ENDIF}
     {$IFDEF FHIR_DOCUMENTREFERENCE}TFhirDocumentReference,{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYREQUEST}TFhirEligibilityRequest,{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYRESPONSE}TFhirEligibilityResponse,{$ENDIF}
     {$IFDEF FHIR_ENCOUNTER}TFhirEncounter,{$ENDIF}
     {$IFDEF FHIR_ENDPOINT}TFhirEndpoint,{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTREQUEST}TFhirEnrollmentRequest,{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTRESPONSE}TFhirEnrollmentResponse,{$ENDIF}
     {$IFDEF FHIR_ENTRYDEFINITION}TFhirEntryDefinition,{$ENDIF}
     {$IFDEF FHIR_EPISODEOFCARE}TFhirEpisodeOfCare,{$ENDIF}
     {$IFDEF FHIR_EVENTDEFINITION}TFhirEventDefinition,{$ENDIF}
     {$IFDEF FHIR_EXAMPLESCENARIO}TFhirExampleScenario,{$ENDIF}
     {$IFDEF FHIR_EXPANSIONPROFILE}TFhirExpansionProfile,{$ENDIF}
     {$IFDEF FHIR_EXPLANATIONOFBENEFIT}TFhirExplanationOfBenefit,{$ENDIF}
     {$IFDEF FHIR_FAMILYMEMBERHISTORY}TFhirFamilyMemberHistory,{$ENDIF}
     {$IFDEF FHIR_FLAG}TFhirFlag,{$ENDIF}
     {$IFDEF FHIR_GOAL}TFhirGoal,{$ENDIF}
     {$IFDEF FHIR_GRAPHDEFINITION}TFhirGraphDefinition,{$ENDIF}
     {$IFDEF FHIR_GROUP}TFhirGroup,{$ENDIF}
     {$IFDEF FHIR_GUIDANCERESPONSE}TFhirGuidanceResponse,{$ENDIF}
     {$IFDEF FHIR_HEALTHCARESERVICE}TFhirHealthcareService,{$ENDIF}
     {$IFDEF FHIR_IMAGINGSTUDY}TFhirImagingStudy,{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATION}TFhirImmunization,{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONEVALUATION}TFhirImmunizationEvaluation,{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}TFhirImmunizationRecommendation,{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDE}TFhirImplementationGuide,{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDEINPUT}TFhirImplementationGuideInput,{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDEOUTPUT}TFhirImplementationGuideOutput,{$ENDIF}
     {$IFDEF FHIR_INVOICE}TFhirInvoice,{$ENDIF}
     {$IFDEF FHIR_ITEMINSTANCE}TFhirItemInstance,{$ENDIF}
     {$IFDEF FHIR_LIBRARY}TFhirLibrary,{$ENDIF}
     {$IFDEF FHIR_LINKAGE}TFhirLinkage,{$ENDIF}
     {$IFDEF FHIR_LIST}TFhirList,{$ENDIF}
     {$IFDEF FHIR_LOCATION}TFhirLocation,{$ENDIF}
     {$IFDEF FHIR_MEASURE}TFhirMeasure,{$ENDIF}
     {$IFDEF FHIR_MEASUREREPORT}TFhirMeasureReport,{$ENDIF}
     {$IFDEF FHIR_MEDIA}TFhirMedia,{$ENDIF}
     {$IFDEF FHIR_MEDICATION}TFhirMedication,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONADMINISTRATION}TFhirMedicationAdministration,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONDISPENSE}TFhirMedicationDispense,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONREQUEST}TFhirMedicationRequest,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONSTATEMENT}TFhirMedicationStatement,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCT}TFhirMedicinalProduct,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTAUTHORIZATION}TFhirMedicinalProductAuthorization,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTCLINICALS}TFhirMedicinalProductClinicals,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTDEVICESPEC}TFhirMedicinalProductDeviceSpec,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTINGREDIENT}TFhirMedicinalProductIngredient,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTPACKAGED}TFhirMedicinalProductPackaged,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}TFhirMedicinalProductPharmaceutical,{$ENDIF}
     {$IFDEF FHIR_MESSAGEDEFINITION}TFhirMessageDefinition,{$ENDIF}
     {$IFDEF FHIR_MESSAGEHEADER}TFhirMessageHeader,{$ENDIF}
     {$IFDEF FHIR_NAMINGSYSTEM}TFhirNamingSystem,{$ENDIF}
     {$IFDEF FHIR_NUTRITIONORDER}TFhirNutritionOrder,{$ENDIF}
     {$IFDEF FHIR_OBSERVATION}TFhirObservation,{$ENDIF}
     {$IFDEF FHIR_OBSERVATIONDEFINITION}TFhirObservationDefinition,{$ENDIF}
     {$IFDEF FHIR_OCCUPATIONALDATA}TFhirOccupationalData,{$ENDIF}
     {$IFDEF FHIR_OPERATIONDEFINITION}TFhirOperationDefinition,{$ENDIF}
     {$IFDEF FHIR_OPERATIONOUTCOME}TFhirOperationOutcome,{$ENDIF}
     {$IFDEF FHIR_ORGANIZATION}TFhirOrganization,{$ENDIF}
     {$IFDEF FHIR_ORGANIZATIONROLE}TFhirOrganizationRole,{$ENDIF}
     {$IFDEF FHIR_PARAMETERS}TFhirParameters,{$ENDIF}
     {$IFDEF FHIR_PATIENT}TFhirPatient,{$ENDIF}
     {$IFDEF FHIR_PAYMENTNOTICE}TFhirPaymentNotice,{$ENDIF}
     {$IFDEF FHIR_PAYMENTRECONCILIATION}TFhirPaymentReconciliation,{$ENDIF}
     {$IFDEF FHIR_PERSON}TFhirPerson,{$ENDIF}
     {$IFDEF FHIR_PLANDEFINITION}TFhirPlanDefinition,{$ENDIF}
     {$IFDEF FHIR_PRACTITIONER}TFhirPractitioner,{$ENDIF}
     {$IFDEF FHIR_PRACTITIONERROLE}TFhirPractitionerRole,{$ENDIF}
     {$IFDEF FHIR_PROCEDURE}TFhirProcedure,{$ENDIF}
     {$IFDEF FHIR_PROCESSREQUEST}TFhirProcessRequest,{$ENDIF}
     {$IFDEF FHIR_PROCESSRESPONSE}TFhirProcessResponse,{$ENDIF}
     {$IFDEF FHIR_PRODUCTPLAN}TFhirProductPlan,{$ENDIF}
     {$IFDEF FHIR_PROVENANCE}TFhirProvenance,{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRE}TFhirQuestionnaire,{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRERESPONSE}TFhirQuestionnaireResponse,{$ENDIF}
     {$IFDEF FHIR_RELATEDPERSON}TFhirRelatedPerson,{$ENDIF}
     {$IFDEF FHIR_REQUESTGROUP}TFhirRequestGroup,{$ENDIF}
     {$IFDEF FHIR_RESEARCHSTUDY}TFhirResearchStudy,{$ENDIF}
     {$IFDEF FHIR_RESEARCHSUBJECT}TFhirResearchSubject,{$ENDIF}
     {$IFDEF FHIR_RISKASSESSMENT}TFhirRiskAssessment,{$ENDIF}
     {$IFDEF FHIR_SCHEDULE}TFhirSchedule,{$ENDIF}
     {$IFDEF FHIR_SEARCHPARAMETER}TFhirSearchParameter,{$ENDIF}
     {$IFDEF FHIR_SEQUENCE}TFhirSequence,{$ENDIF}
     {$IFDEF FHIR_SERVICEDEFINITION}TFhirServiceDefinition,{$ENDIF}
     {$IFDEF FHIR_SERVICEREQUEST}TFhirServiceRequest,{$ENDIF}
     {$IFDEF FHIR_SLOT}TFhirSlot,{$ENDIF}
     {$IFDEF FHIR_SPECIMEN}TFhirSpecimen,{$ENDIF}
     {$IFDEF FHIR_SPECIMENDEFINITION}TFhirSpecimenDefinition,{$ENDIF}
     {$IFDEF FHIR_STRUCTUREDEFINITION}TFhirStructureDefinition,{$ENDIF}
     {$IFDEF FHIR_STRUCTUREMAP}TFhirStructureMap,{$ENDIF}
     {$IFDEF FHIR_SUBSCRIPTION}TFhirSubscription,{$ENDIF}
     {$IFDEF FHIR_SUBSTANCE}TFhirSubstance,{$ENDIF}
     {$IFDEF FHIR_SUBSTANCEPOLYMER}TFhirSubstancePolymer,{$ENDIF}
     {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}TFhirSubstanceReferenceInformation,{$ENDIF}
     {$IFDEF FHIR_SUBSTANCESPECIFICATION}TFhirSubstanceSpecification,{$ENDIF}
     {$IFDEF FHIR_SUPPLYDELIVERY}TFhirSupplyDelivery,{$ENDIF}
     {$IFDEF FHIR_SUPPLYREQUEST}TFhirSupplyRequest,{$ENDIF}
     {$IFDEF FHIR_TASK}TFhirTask,{$ENDIF}
     {$IFDEF FHIR_TERMINOLOGYCAPABILITIES}TFhirTerminologyCapabilities,{$ENDIF}
     {$IFDEF FHIR_TESTREPORT}TFhirTestReport,{$ENDIF}
     {$IFDEF FHIR_TESTSCRIPT}TFhirTestScript,{$ENDIF}
     {$IFDEF FHIR_USERSESSION}TFhirUserSession,{$ENDIF}
     {$IFDEF FHIR_VALUESET}TFhirValueSet,{$ENDIF}
     {$IFDEF FHIR_VERIFICATIONRESULT}TFhirVerificationResult,{$ENDIF}
     {$IFDEF FHIR_VISIONPRESCRIPTION}TFhirVisionPrescription,{$ENDIF}
     nil);
     
  ALL_RESOURCE_TYPES = [{$IFDEF FHIR_ACCOUNT}frtAccount,{$ENDIF}
     {$IFDEF FHIR_ACTIVITYDEFINITION}frtActivityDefinition,{$ENDIF}
     {$IFDEF FHIR_ADVERSEEVENT}frtAdverseEvent,{$ENDIF}
     {$IFDEF FHIR_ALLERGYINTOLERANCE}frtAllergyIntolerance,{$ENDIF}
     {$IFDEF FHIR_APPOINTMENT}frtAppointment,{$ENDIF}
     {$IFDEF FHIR_APPOINTMENTRESPONSE}frtAppointmentResponse,{$ENDIF}
     {$IFDEF FHIR_AUDITEVENT}frtAuditEvent,{$ENDIF}
     {$IFDEF FHIR_BASIC}frtBasic,{$ENDIF}
     {$IFDEF FHIR_BINARY}frtBinary,{$ENDIF}
     {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}frtBiologicallyDerivedProduct,{$ENDIF}
     {$IFDEF FHIR_BODYSTRUCTURE}frtBodyStructure,{$ENDIF}
     {$IFDEF FHIR_BUNDLE}frtBundle,{$ENDIF}
     {$IFDEF FHIR_CAPABILITYSTATEMENT}frtCapabilityStatement,{$ENDIF}
     {$IFDEF FHIR_CAREPLAN}frtCarePlan,{$ENDIF}
     {$IFDEF FHIR_CARETEAM}frtCareTeam,{$ENDIF}
     {$IFDEF FHIR_CHARGEITEM}frtChargeItem,{$ENDIF}
     {$IFDEF FHIR_CLAIM}frtClaim,{$ENDIF}
     {$IFDEF FHIR_CLAIMRESPONSE}frtClaimResponse,{$ENDIF}
     {$IFDEF FHIR_CLINICALIMPRESSION}frtClinicalImpression,{$ENDIF}
     {$IFDEF FHIR_CODESYSTEM}frtCodeSystem,{$ENDIF}
     {$IFDEF FHIR_COMMUNICATION}frtCommunication,{$ENDIF}
     {$IFDEF FHIR_COMMUNICATIONREQUEST}frtCommunicationRequest,{$ENDIF}
     {$IFDEF FHIR_COMPARTMENTDEFINITION}frtCompartmentDefinition,{$ENDIF}
     {$IFDEF FHIR_COMPOSITION}frtComposition,{$ENDIF}
     {$IFDEF FHIR_CONCEPTMAP}frtConceptMap,{$ENDIF}
     {$IFDEF FHIR_CONDITION}frtCondition,{$ENDIF}
     {$IFDEF FHIR_CONSENT}frtConsent,{$ENDIF}
     {$IFDEF FHIR_CONTRACT}frtContract,{$ENDIF}
     {$IFDEF FHIR_COVERAGE}frtCoverage,{$ENDIF}
     {$IFDEF FHIR_DETECTEDISSUE}frtDetectedIssue,{$ENDIF}
     {$IFDEF FHIR_DEVICE}frtDevice,{$ENDIF}
     {$IFDEF FHIR_DEVICECOMPONENT}frtDeviceComponent,{$ENDIF}
     {$IFDEF FHIR_DEVICEMETRIC}frtDeviceMetric,{$ENDIF}
     {$IFDEF FHIR_DEVICEREQUEST}frtDeviceRequest,{$ENDIF}
     {$IFDEF FHIR_DEVICEUSESTATEMENT}frtDeviceUseStatement,{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICREPORT}frtDiagnosticReport,{$ENDIF}
     {$IFDEF FHIR_DOCUMENTMANIFEST}frtDocumentManifest,{$ENDIF}
     {$IFDEF FHIR_DOCUMENTREFERENCE}frtDocumentReference,{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYREQUEST}frtEligibilityRequest,{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYRESPONSE}frtEligibilityResponse,{$ENDIF}
     {$IFDEF FHIR_ENCOUNTER}frtEncounter,{$ENDIF}
     {$IFDEF FHIR_ENDPOINT}frtEndpoint,{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTREQUEST}frtEnrollmentRequest,{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTRESPONSE}frtEnrollmentResponse,{$ENDIF}
     {$IFDEF FHIR_ENTRYDEFINITION}frtEntryDefinition,{$ENDIF}
     {$IFDEF FHIR_EPISODEOFCARE}frtEpisodeOfCare,{$ENDIF}
     {$IFDEF FHIR_EVENTDEFINITION}frtEventDefinition,{$ENDIF}
     {$IFDEF FHIR_EXAMPLESCENARIO}frtExampleScenario,{$ENDIF}
     {$IFDEF FHIR_EXPANSIONPROFILE}frtExpansionProfile,{$ENDIF}
     {$IFDEF FHIR_EXPLANATIONOFBENEFIT}frtExplanationOfBenefit,{$ENDIF}
     {$IFDEF FHIR_FAMILYMEMBERHISTORY}frtFamilyMemberHistory,{$ENDIF}
     {$IFDEF FHIR_FLAG}frtFlag,{$ENDIF}
     {$IFDEF FHIR_GOAL}frtGoal,{$ENDIF}
     {$IFDEF FHIR_GRAPHDEFINITION}frtGraphDefinition,{$ENDIF}
     {$IFDEF FHIR_GROUP}frtGroup,{$ENDIF}
     {$IFDEF FHIR_GUIDANCERESPONSE}frtGuidanceResponse,{$ENDIF}
     {$IFDEF FHIR_HEALTHCARESERVICE}frtHealthcareService,{$ENDIF}
     {$IFDEF FHIR_IMAGINGSTUDY}frtImagingStudy,{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATION}frtImmunization,{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONEVALUATION}frtImmunizationEvaluation,{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}frtImmunizationRecommendation,{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDE}frtImplementationGuide,{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDEINPUT}frtImplementationGuideInput,{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDEOUTPUT}frtImplementationGuideOutput,{$ENDIF}
     {$IFDEF FHIR_INVOICE}frtInvoice,{$ENDIF}
     {$IFDEF FHIR_ITEMINSTANCE}frtItemInstance,{$ENDIF}
     {$IFDEF FHIR_LIBRARY}frtLibrary,{$ENDIF}
     {$IFDEF FHIR_LINKAGE}frtLinkage,{$ENDIF}
     {$IFDEF FHIR_LIST}frtList,{$ENDIF}
     {$IFDEF FHIR_LOCATION}frtLocation,{$ENDIF}
     {$IFDEF FHIR_MEASURE}frtMeasure,{$ENDIF}
     {$IFDEF FHIR_MEASUREREPORT}frtMeasureReport,{$ENDIF}
     {$IFDEF FHIR_MEDIA}frtMedia,{$ENDIF}
     {$IFDEF FHIR_MEDICATION}frtMedication,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONADMINISTRATION}frtMedicationAdministration,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONDISPENSE}frtMedicationDispense,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONREQUEST}frtMedicationRequest,{$ENDIF}
     {$IFDEF FHIR_MEDICATIONSTATEMENT}frtMedicationStatement,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCT}frtMedicinalProduct,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTAUTHORIZATION}frtMedicinalProductAuthorization,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTCLINICALS}frtMedicinalProductClinicals,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTDEVICESPEC}frtMedicinalProductDeviceSpec,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTINGREDIENT}frtMedicinalProductIngredient,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTPACKAGED}frtMedicinalProductPackaged,{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}frtMedicinalProductPharmaceutical,{$ENDIF}
     {$IFDEF FHIR_MESSAGEDEFINITION}frtMessageDefinition,{$ENDIF}
     {$IFDEF FHIR_MESSAGEHEADER}frtMessageHeader,{$ENDIF}
     {$IFDEF FHIR_NAMINGSYSTEM}frtNamingSystem,{$ENDIF}
     {$IFDEF FHIR_NUTRITIONORDER}frtNutritionOrder,{$ENDIF}
     {$IFDEF FHIR_OBSERVATION}frtObservation,{$ENDIF}
     {$IFDEF FHIR_OBSERVATIONDEFINITION}frtObservationDefinition,{$ENDIF}
     {$IFDEF FHIR_OCCUPATIONALDATA}frtOccupationalData,{$ENDIF}
     {$IFDEF FHIR_OPERATIONDEFINITION}frtOperationDefinition,{$ENDIF}
     {$IFDEF FHIR_OPERATIONOUTCOME}frtOperationOutcome,{$ENDIF}
     {$IFDEF FHIR_ORGANIZATION}frtOrganization,{$ENDIF}
     {$IFDEF FHIR_ORGANIZATIONROLE}frtOrganizationRole,{$ENDIF}
     {$IFDEF FHIR_PARAMETERS}frtParameters,{$ENDIF}
     {$IFDEF FHIR_PATIENT}frtPatient,{$ENDIF}
     {$IFDEF FHIR_PAYMENTNOTICE}frtPaymentNotice,{$ENDIF}
     {$IFDEF FHIR_PAYMENTRECONCILIATION}frtPaymentReconciliation,{$ENDIF}
     {$IFDEF FHIR_PERSON}frtPerson,{$ENDIF}
     {$IFDEF FHIR_PLANDEFINITION}frtPlanDefinition,{$ENDIF}
     {$IFDEF FHIR_PRACTITIONER}frtPractitioner,{$ENDIF}
     {$IFDEF FHIR_PRACTITIONERROLE}frtPractitionerRole,{$ENDIF}
     {$IFDEF FHIR_PROCEDURE}frtProcedure,{$ENDIF}
     {$IFDEF FHIR_PROCESSREQUEST}frtProcessRequest,{$ENDIF}
     {$IFDEF FHIR_PROCESSRESPONSE}frtProcessResponse,{$ENDIF}
     {$IFDEF FHIR_PRODUCTPLAN}frtProductPlan,{$ENDIF}
     {$IFDEF FHIR_PROVENANCE}frtProvenance,{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRE}frtQuestionnaire,{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRERESPONSE}frtQuestionnaireResponse,{$ENDIF}
     {$IFDEF FHIR_RELATEDPERSON}frtRelatedPerson,{$ENDIF}
     {$IFDEF FHIR_REQUESTGROUP}frtRequestGroup,{$ENDIF}
     {$IFDEF FHIR_RESEARCHSTUDY}frtResearchStudy,{$ENDIF}
     {$IFDEF FHIR_RESEARCHSUBJECT}frtResearchSubject,{$ENDIF}
     {$IFDEF FHIR_RISKASSESSMENT}frtRiskAssessment,{$ENDIF}
     {$IFDEF FHIR_SCHEDULE}frtSchedule,{$ENDIF}
     {$IFDEF FHIR_SEARCHPARAMETER}frtSearchParameter,{$ENDIF}
     {$IFDEF FHIR_SEQUENCE}frtSequence,{$ENDIF}
     {$IFDEF FHIR_SERVICEDEFINITION}frtServiceDefinition,{$ENDIF}
     {$IFDEF FHIR_SERVICEREQUEST}frtServiceRequest,{$ENDIF}
     {$IFDEF FHIR_SLOT}frtSlot,{$ENDIF}
     {$IFDEF FHIR_SPECIMEN}frtSpecimen,{$ENDIF}
     {$IFDEF FHIR_SPECIMENDEFINITION}frtSpecimenDefinition,{$ENDIF}
     {$IFDEF FHIR_STRUCTUREDEFINITION}frtStructureDefinition,{$ENDIF}
     {$IFDEF FHIR_STRUCTUREMAP}frtStructureMap,{$ENDIF}
     {$IFDEF FHIR_SUBSCRIPTION}frtSubscription,{$ENDIF}
     {$IFDEF FHIR_SUBSTANCE}frtSubstance,{$ENDIF}
     {$IFDEF FHIR_SUBSTANCEPOLYMER}frtSubstancePolymer,{$ENDIF}
     {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}frtSubstanceReferenceInformation,{$ENDIF}
     {$IFDEF FHIR_SUBSTANCESPECIFICATION}frtSubstanceSpecification,{$ENDIF}
     {$IFDEF FHIR_SUPPLYDELIVERY}frtSupplyDelivery,{$ENDIF}
     {$IFDEF FHIR_SUPPLYREQUEST}frtSupplyRequest,{$ENDIF}
     {$IFDEF FHIR_TASK}frtTask,{$ENDIF}
     {$IFDEF FHIR_TERMINOLOGYCAPABILITIES}frtTerminologyCapabilities,{$ENDIF}
     {$IFDEF FHIR_TESTREPORT}frtTestReport,{$ENDIF}
     {$IFDEF FHIR_TESTSCRIPT}frtTestScript,{$ENDIF}
     {$IFDEF FHIR_USERSESSION}frtUserSession,{$ENDIF}
     {$IFDEF FHIR_VALUESET}frtValueSet,{$ENDIF}
     {$IFDEF FHIR_VERIFICATIONRESULT}frtVerificationResult,{$ENDIF}
     {$IFDEF FHIR_VISIONPRESCRIPTION}frtVisionPrescription,{$ENDIF}
     frtCustom];
     
  ALL_RESOURCE_TYPE_NAMES : Array [TFHIRResourceType] of String = ('--None--', {$IFDEF FHIR_ACCOUNT}'Account',{$ENDIF}
     {$IFDEF FHIR_ACTIVITYDEFINITION}'ActivityDefinition',{$ENDIF}
     {$IFDEF FHIR_ADVERSEEVENT}'AdverseEvent',{$ENDIF}
     {$IFDEF FHIR_ALLERGYINTOLERANCE}'AllergyIntolerance',{$ENDIF}
     {$IFDEF FHIR_APPOINTMENT}'Appointment',{$ENDIF}
     {$IFDEF FHIR_APPOINTMENTRESPONSE}'AppointmentResponse',{$ENDIF}
     {$IFDEF FHIR_AUDITEVENT}'AuditEvent',{$ENDIF}
     {$IFDEF FHIR_BASIC}'Basic',{$ENDIF}
     {$IFDEF FHIR_BINARY}'Binary',{$ENDIF}
     {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}'BiologicallyDerivedProduct',{$ENDIF}
     {$IFDEF FHIR_BODYSTRUCTURE}'BodyStructure',{$ENDIF}
     {$IFDEF FHIR_BUNDLE}'Bundle',{$ENDIF}
     {$IFDEF FHIR_CAPABILITYSTATEMENT}'CapabilityStatement',{$ENDIF}
     {$IFDEF FHIR_CAREPLAN}'CarePlan',{$ENDIF}
     {$IFDEF FHIR_CARETEAM}'CareTeam',{$ENDIF}
     {$IFDEF FHIR_CHARGEITEM}'ChargeItem',{$ENDIF}
     {$IFDEF FHIR_CLAIM}'Claim',{$ENDIF}
     {$IFDEF FHIR_CLAIMRESPONSE}'ClaimResponse',{$ENDIF}
     {$IFDEF FHIR_CLINICALIMPRESSION}'ClinicalImpression',{$ENDIF}
     {$IFDEF FHIR_CODESYSTEM}'CodeSystem',{$ENDIF}
     {$IFDEF FHIR_COMMUNICATION}'Communication',{$ENDIF}
     {$IFDEF FHIR_COMMUNICATIONREQUEST}'CommunicationRequest',{$ENDIF}
     {$IFDEF FHIR_COMPARTMENTDEFINITION}'CompartmentDefinition',{$ENDIF}
     {$IFDEF FHIR_COMPOSITION}'Composition',{$ENDIF}
     {$IFDEF FHIR_CONCEPTMAP}'ConceptMap',{$ENDIF}
     {$IFDEF FHIR_CONDITION}'Condition',{$ENDIF}
     {$IFDEF FHIR_CONSENT}'Consent',{$ENDIF}
     {$IFDEF FHIR_CONTRACT}'Contract',{$ENDIF}
     {$IFDEF FHIR_COVERAGE}'Coverage',{$ENDIF}
     {$IFDEF FHIR_DETECTEDISSUE}'DetectedIssue',{$ENDIF}
     {$IFDEF FHIR_DEVICE}'Device',{$ENDIF}
     {$IFDEF FHIR_DEVICECOMPONENT}'DeviceComponent',{$ENDIF}
     {$IFDEF FHIR_DEVICEMETRIC}'DeviceMetric',{$ENDIF}
     {$IFDEF FHIR_DEVICEREQUEST}'DeviceRequest',{$ENDIF}
     {$IFDEF FHIR_DEVICEUSESTATEMENT}'DeviceUseStatement',{$ENDIF}
     {$IFDEF FHIR_DIAGNOSTICREPORT}'DiagnosticReport',{$ENDIF}
     {$IFDEF FHIR_DOCUMENTMANIFEST}'DocumentManifest',{$ENDIF}
     {$IFDEF FHIR_DOCUMENTREFERENCE}'DocumentReference',{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYREQUEST}'EligibilityRequest',{$ENDIF}
     {$IFDEF FHIR_ELIGIBILITYRESPONSE}'EligibilityResponse',{$ENDIF}
     {$IFDEF FHIR_ENCOUNTER}'Encounter',{$ENDIF}
     {$IFDEF FHIR_ENDPOINT}'Endpoint',{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTREQUEST}'EnrollmentRequest',{$ENDIF}
     {$IFDEF FHIR_ENROLLMENTRESPONSE}'EnrollmentResponse',{$ENDIF}
     {$IFDEF FHIR_ENTRYDEFINITION}'EntryDefinition',{$ENDIF}
     {$IFDEF FHIR_EPISODEOFCARE}'EpisodeOfCare',{$ENDIF}
     {$IFDEF FHIR_EVENTDEFINITION}'EventDefinition',{$ENDIF}
     {$IFDEF FHIR_EXAMPLESCENARIO}'ExampleScenario',{$ENDIF}
     {$IFDEF FHIR_EXPANSIONPROFILE}'ExpansionProfile',{$ENDIF}
     {$IFDEF FHIR_EXPLANATIONOFBENEFIT}'ExplanationOfBenefit',{$ENDIF}
     {$IFDEF FHIR_FAMILYMEMBERHISTORY}'FamilyMemberHistory',{$ENDIF}
     {$IFDEF FHIR_FLAG}'Flag',{$ENDIF}
     {$IFDEF FHIR_GOAL}'Goal',{$ENDIF}
     {$IFDEF FHIR_GRAPHDEFINITION}'GraphDefinition',{$ENDIF}
     {$IFDEF FHIR_GROUP}'Group',{$ENDIF}
     {$IFDEF FHIR_GUIDANCERESPONSE}'GuidanceResponse',{$ENDIF}
     {$IFDEF FHIR_HEALTHCARESERVICE}'HealthcareService',{$ENDIF}
     {$IFDEF FHIR_IMAGINGSTUDY}'ImagingStudy',{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATION}'Immunization',{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONEVALUATION}'ImmunizationEvaluation',{$ENDIF}
     {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}'ImmunizationRecommendation',{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDE}'ImplementationGuide',{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDEINPUT}'ImplementationGuideInput',{$ENDIF}
     {$IFDEF FHIR_IMPLEMENTATIONGUIDEOUTPUT}'ImplementationGuideOutput',{$ENDIF}
     {$IFDEF FHIR_INVOICE}'Invoice',{$ENDIF}
     {$IFDEF FHIR_ITEMINSTANCE}'ItemInstance',{$ENDIF}
     {$IFDEF FHIR_LIBRARY}'Library',{$ENDIF}
     {$IFDEF FHIR_LINKAGE}'Linkage',{$ENDIF}
     {$IFDEF FHIR_LIST}'List',{$ENDIF}
     {$IFDEF FHIR_LOCATION}'Location',{$ENDIF}
     {$IFDEF FHIR_MEASURE}'Measure',{$ENDIF}
     {$IFDEF FHIR_MEASUREREPORT}'MeasureReport',{$ENDIF}
     {$IFDEF FHIR_MEDIA}'Media',{$ENDIF}
     {$IFDEF FHIR_MEDICATION}'Medication',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONADMINISTRATION}'MedicationAdministration',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONDISPENSE}'MedicationDispense',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONREQUEST}'MedicationRequest',{$ENDIF}
     {$IFDEF FHIR_MEDICATIONSTATEMENT}'MedicationStatement',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCT}'MedicinalProduct',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTAUTHORIZATION}'MedicinalProductAuthorization',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTCLINICALS}'MedicinalProductClinicals',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTDEVICESPEC}'MedicinalProductDeviceSpec',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTINGREDIENT}'MedicinalProductIngredient',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTPACKAGED}'MedicinalProductPackaged',{$ENDIF}
     {$IFDEF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}'MedicinalProductPharmaceutical',{$ENDIF}
     {$IFDEF FHIR_MESSAGEDEFINITION}'MessageDefinition',{$ENDIF}
     {$IFDEF FHIR_MESSAGEHEADER}'MessageHeader',{$ENDIF}
     {$IFDEF FHIR_NAMINGSYSTEM}'NamingSystem',{$ENDIF}
     {$IFDEF FHIR_NUTRITIONORDER}'NutritionOrder',{$ENDIF}
     {$IFDEF FHIR_OBSERVATION}'Observation',{$ENDIF}
     {$IFDEF FHIR_OBSERVATIONDEFINITION}'ObservationDefinition',{$ENDIF}
     {$IFDEF FHIR_OCCUPATIONALDATA}'OccupationalData',{$ENDIF}
     {$IFDEF FHIR_OPERATIONDEFINITION}'OperationDefinition',{$ENDIF}
     {$IFDEF FHIR_OPERATIONOUTCOME}'OperationOutcome',{$ENDIF}
     {$IFDEF FHIR_ORGANIZATION}'Organization',{$ENDIF}
     {$IFDEF FHIR_ORGANIZATIONROLE}'OrganizationRole',{$ENDIF}
     {$IFDEF FHIR_PARAMETERS}'Parameters',{$ENDIF}
     {$IFDEF FHIR_PATIENT}'Patient',{$ENDIF}
     {$IFDEF FHIR_PAYMENTNOTICE}'PaymentNotice',{$ENDIF}
     {$IFDEF FHIR_PAYMENTRECONCILIATION}'PaymentReconciliation',{$ENDIF}
     {$IFDEF FHIR_PERSON}'Person',{$ENDIF}
     {$IFDEF FHIR_PLANDEFINITION}'PlanDefinition',{$ENDIF}
     {$IFDEF FHIR_PRACTITIONER}'Practitioner',{$ENDIF}
     {$IFDEF FHIR_PRACTITIONERROLE}'PractitionerRole',{$ENDIF}
     {$IFDEF FHIR_PROCEDURE}'Procedure',{$ENDIF}
     {$IFDEF FHIR_PROCESSREQUEST}'ProcessRequest',{$ENDIF}
     {$IFDEF FHIR_PROCESSRESPONSE}'ProcessResponse',{$ENDIF}
     {$IFDEF FHIR_PRODUCTPLAN}'ProductPlan',{$ENDIF}
     {$IFDEF FHIR_PROVENANCE}'Provenance',{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRE}'Questionnaire',{$ENDIF}
     {$IFDEF FHIR_QUESTIONNAIRERESPONSE}'QuestionnaireResponse',{$ENDIF}
     {$IFDEF FHIR_RELATEDPERSON}'RelatedPerson',{$ENDIF}
     {$IFDEF FHIR_REQUESTGROUP}'RequestGroup',{$ENDIF}
     {$IFDEF FHIR_RESEARCHSTUDY}'ResearchStudy',{$ENDIF}
     {$IFDEF FHIR_RESEARCHSUBJECT}'ResearchSubject',{$ENDIF}
     {$IFDEF FHIR_RISKASSESSMENT}'RiskAssessment',{$ENDIF}
     {$IFDEF FHIR_SCHEDULE}'Schedule',{$ENDIF}
     {$IFDEF FHIR_SEARCHPARAMETER}'SearchParameter',{$ENDIF}
     {$IFDEF FHIR_SEQUENCE}'Sequence',{$ENDIF}
     {$IFDEF FHIR_SERVICEDEFINITION}'ServiceDefinition',{$ENDIF}
     {$IFDEF FHIR_SERVICEREQUEST}'ServiceRequest',{$ENDIF}
     {$IFDEF FHIR_SLOT}'Slot',{$ENDIF}
     {$IFDEF FHIR_SPECIMEN}'Specimen',{$ENDIF}
     {$IFDEF FHIR_SPECIMENDEFINITION}'SpecimenDefinition',{$ENDIF}
     {$IFDEF FHIR_STRUCTUREDEFINITION}'StructureDefinition',{$ENDIF}
     {$IFDEF FHIR_STRUCTUREMAP}'StructureMap',{$ENDIF}
     {$IFDEF FHIR_SUBSCRIPTION}'Subscription',{$ENDIF}
     {$IFDEF FHIR_SUBSTANCE}'Substance',{$ENDIF}
     {$IFDEF FHIR_SUBSTANCEPOLYMER}'SubstancePolymer',{$ENDIF}
     {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}'SubstanceReferenceInformation',{$ENDIF}
     {$IFDEF FHIR_SUBSTANCESPECIFICATION}'SubstanceSpecification',{$ENDIF}
     {$IFDEF FHIR_SUPPLYDELIVERY}'SupplyDelivery',{$ENDIF}
     {$IFDEF FHIR_SUPPLYREQUEST}'SupplyRequest',{$ENDIF}
     {$IFDEF FHIR_TASK}'Task',{$ENDIF}
     {$IFDEF FHIR_TERMINOLOGYCAPABILITIES}'TerminologyCapabilities',{$ENDIF}
     {$IFDEF FHIR_TESTREPORT}'TestReport',{$ENDIF}
     {$IFDEF FHIR_TESTSCRIPT}'TestScript',{$ENDIF}
     {$IFDEF FHIR_USERSESSION}'UserSession',{$ENDIF}
     {$IFDEF FHIR_VALUESET}'ValueSet',{$ENDIF}
     {$IFDEF FHIR_VERIFICATIONRESULT}'VerificationResult',{$ENDIF}
     {$IFDEF FHIR_VISIONPRESCRIPTION}'VisionPrescription',{$ENDIF}
     'Custom');
     
{$IFDEF FHIR_ACCOUNT}
  CODES_TSearchParamsAccount : Array[TSearchParamsAccount] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'name', 'owner', 'patient', 'period', 'status', 'subject', 'type');
{$ENDIF}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  CODES_TSearchParamsActivityDefinition : Array[TSearchParamsActivityDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'composed-of', 'context-type', 'date', 'depends-on', 'derived-from', 'description', 'effective', 'identifier', 'jurisdiction', 'name', 'predecessor', 'publisher', 'status', 'successor', 'title', 'topic', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_ADVERSEEVENT}
  CODES_TSearchParamsAdverseEvent : Array[TSearchParamsAdverseEvent] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'actuality', 'category', 'date', 'event', 'location', 'recorder', 'resultingcondition', 'seriousness', 'severity', 'study', 'subject', 'substance');
{$ENDIF}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  CODES_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'asserter', 'category', 'clinical-status', 'code', 'criticality', 'date', 'identifier', 'last-date', 'manifestation', 'onset', 'patient', 'recorder', 'route', 'severity', 'type', 'verification-status');
{$ENDIF}
{$IFDEF FHIR_APPOINTMENT}
  CODES_TSearchParamsAppointment : Array[TSearchParamsAppointment] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'actor', 'appointment-type', 'date', 'identifier', 'incomingreferral', 'location', 'part-status', 'patient', 'practitioner', 'service-type', 'status');
{$ENDIF}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  CODES_TSearchParamsAppointmentResponse : Array[TSearchParamsAppointmentResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'actor', 'appointment', 'identifier', 'location', 'part-status', 'patient', 'practitioner');
{$ENDIF}
{$IFDEF FHIR_AUDITEVENT}
  CODES_TSearchParamsAuditEvent : Array[TSearchParamsAuditEvent] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'action', 'address', 'agent', 'agent-name', 'agent-role', 'altid', 'date', 'entity', 'entity-id', 'entity-name', 'entity-role', 'entity-type', 'outcome', 'patient', 'policy', 'site', 'source', 'subtype', 'type', 'user');
{$ENDIF}
{$IFDEF FHIR_BASIC}
  CODES_TSearchParamsBasic : Array[TSearchParamsBasic] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'author', 'code', 'created', 'identifier', 'patient', 'subject');
{$ENDIF}
{$IFDEF FHIR_BINARY}
  CODES_TSearchParamsBinary : Array[TSearchParamsBinary] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', 'contenttype');
{$ENDIF}
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  CODES_TSearchParamsBiologicallyDerivedProduct : Array[TSearchParamsBiologicallyDerivedProduct] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_BODYSTRUCTURE}
  CODES_TSearchParamsBodyStructure : Array[TSearchParamsBodyStructure] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'location', 'morphology', 'patient');
{$ENDIF}
{$IFDEF FHIR_BUNDLE}
  CODES_TSearchParamsBundle : Array[TSearchParamsBundle] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', 'composition', 'identifier', 'message', 'timestamp', 'type');
{$ENDIF}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  CODES_TSearchParamsCapabilityStatement : Array[TSearchParamsCapabilityStatement] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'description', 'fhirversion', 'format', 'guide', 'jurisdiction', 'mode', 'name', 'publisher', 'resource', 'resource-profile', 'security-service', 'software', 'status', 'supported-profile', 'title', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_CAREPLAN}
  CODES_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'activity-code', 'activity-date', 'activity-reference', 'based-on', 'care-team', 'category', 'condition', 'context', 'date', 'encounter', 'goal', 'identifier', 'instantiates', 'intent', 'part-of', 'patient', 'performer', 'replaces', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_CARETEAM}
  CODES_TSearchParamsCareTeam : Array[TSearchParamsCareTeam] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'category', 'context', 'date', 'encounter', 'identifier', 'participant', 'patient', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_CHARGEITEM}
  CODES_TSearchParamsChargeItem : Array[TSearchParamsChargeItem] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'account', 'code', 'context', 'entered-date', 'enterer', 'factor-override', 'identifier', 'occurrence', 'participant-actor', 'participant-role', 'patient', 'performing-organization', 'price-override', 'quantity', 'requesting-organization', 'service', 'subject');
{$ENDIF}
{$IFDEF FHIR_CLAIM}
  CODES_TSearchParamsClaim : Array[TSearchParamsClaim] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'care-team', 'created', 'encounter', 'enterer', 'facility', 'identifier', 'insurer', 'organization', 'patient', 'payee', 'priority', 'provider', 'status', 'use');
{$ENDIF}
{$IFDEF FHIR_CLAIMRESPONSE}
  CODES_TSearchParamsClaimResponse : Array[TSearchParamsClaimResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'created', 'disposition', 'identifier', 'insurer', 'outcome', 'patient', 'payment-date', 'request', 'request-provider', 'status');
{$ENDIF}
{$IFDEF FHIR_CLINICALIMPRESSION}
  CODES_TSearchParamsClinicalImpression : Array[TSearchParamsClinicalImpression] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'action', 'assessor', 'context', 'date', 'finding-code', 'finding-ref', 'identifier', 'investigation', 'patient', 'previous', 'problem', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_CODESYSTEM}
  CODES_TSearchParamsCodeSystem : Array[TSearchParamsCodeSystem] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'code', 'content-mode', 'date', 'description', 'identifier', 'jurisdiction', 'language', 'name', 'publisher', 'status', 'supplements', 'system', 'title', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_COMMUNICATION}
  CODES_TSearchParamsCommunication : Array[TSearchParamsCommunication] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'based-on', 'category', 'context', 'encounter', 'identifier', 'instantiates', 'medium', 'part-of', 'patient', 'received', 'recipient', 'sender', 'sent', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  CODES_TSearchParamsCommunicationRequest : Array[TSearchParamsCommunicationRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'authored', 'based-on', 'category', 'context', 'encounter', 'group-identifier', 'identifier', 'medium', 'occurrence', 'patient', 'priority', 'recipient', 'replaces', 'requester', 'sender', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  CODES_TSearchParamsCompartmentDefinition : Array[TSearchParamsCompartmentDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'code', 'context-type', 'date', 'description', 'jurisdiction', 'name', 'publisher', 'resource', 'status', 'title', 'url');
{$ENDIF}
{$IFDEF FHIR_COMPOSITION}
  CODES_TSearchParamsComposition : Array[TSearchParamsComposition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'attester', 'author', 'class', 'confidentiality', 'context', 'date', 'encounter', 'entry', 'identifier', 'patient', 'period', 'related-id', 'related-ref', 'section', 'status', 'subject', 'title', 'type');
{$ENDIF}
{$IFDEF FHIR_CONCEPTMAP}
  CODES_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'dependson', 'description', 'identifier', 'jurisdiction', 'name', 'other', 'product', 'publisher', 'source', 'source-code', 'source-system', 'source-uri', 'status', 'target', 'target-code', 'target-system', 'target-uri', 'title', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_CONDITION}
  CODES_TSearchParamsCondition : Array[TSearchParamsCondition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'abatement-age', 'abatement-date', 'abatement-string', 'asserted-date', 'asserter', 'body-site', 'category', 'clinical-status', 'code', 'context', 'encounter', 'evidence', 'evidence-detail', 'identifier', 'onset-age', 'onset-date', 'onset-info', 'patient', 'severity', 'stage', 'subject', 'verification-status');
{$ENDIF}
{$IFDEF FHIR_CONSENT}
  CODES_TSearchParamsConsent : Array[TSearchParamsConsent] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'action', 'actor', 'category', 'consentor', 'data', 'date', 'identifier', 'organization', 'patient', 'period', 'purpose', 'scope', 'securitylabel', 'source', 'status');
{$ENDIF}
{$IFDEF FHIR_CONTRACT}
  CODES_TSearchParamsContract : Array[TSearchParamsContract] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'authority', 'domain', 'identifier', 'issued', 'patient', 'signer', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_COVERAGE}
  CODES_TSearchParamsCoverage : Array[TSearchParamsCoverage] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'beneficiary', 'class', 'dependent', 'group', 'identifier', 'patient', 'payor', 'plan', 'policy-holder', 'sequence', 'status', 'subclass', 'subgroup', 'subplan', 'subscriber', 'type');
{$ENDIF}
{$IFDEF FHIR_DETECTEDISSUE}
  CODES_TSearchParamsDetectedIssue : Array[TSearchParamsDetectedIssue] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'author', 'category', 'date', 'identifier', 'implicated', 'patient');
{$ENDIF}
{$IFDEF FHIR_DEVICE}
  CODES_TSearchParamsDevice : Array[TSearchParamsDevice] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'device-name', 'identifier', 'location', 'manufacturer', 'model', 'organization', 'patient', 'status', 'type', 'udi-carrier', 'udi-di', 'url');
{$ENDIF}
{$IFDEF FHIR_DEVICECOMPONENT}
  CODES_TSearchParamsDeviceComponent : Array[TSearchParamsDeviceComponent] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'parent', 'source', 'type');
{$ENDIF}
{$IFDEF FHIR_DEVICEMETRIC}
  CODES_TSearchParamsDeviceMetric : Array[TSearchParamsDeviceMetric] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'category', 'identifier', 'parent', 'source', 'type');
{$ENDIF}
{$IFDEF FHIR_DEVICEREQUEST}
  CODES_TSearchParamsDeviceRequest : Array[TSearchParamsDeviceRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'authored-on', 'based-on', 'code', 'device', 'encounter', 'event-date', 'group-identifier', 'identifier', 'instantiates', 'insurance', 'intent', 'patient', 'performer', 'priorrequest', 'requester', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  CODES_TSearchParamsDeviceUseStatement : Array[TSearchParamsDeviceUseStatement] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'device', 'identifier', 'patient', 'subject');
{$ENDIF}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  CODES_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'based-on', 'category', 'code', 'context', 'date', 'diagnosis', 'encounter', 'identifier', 'issued', 'media', 'patient', 'performer', 'result', 'results-interpreter', 'specimen', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  CODES_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'agent', 'created', 'description', 'identifier', 'item', 'patient', 'recipient', 'related-id', 'related-ref', 'source', 'status', 'subject', 'type');
{$ENDIF}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  CODES_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'agent', 'authenticator', 'class', 'contenttype', 'created', 'custodian', 'date', 'description', 'encounter', 'event', 'facility', 'format', 'identifier', 'language', 'location', 'patient', 'period', 'related-id', 'related-ref', 'relatesto', 'relation', 'relationship', 'securitylabel', 'setting', 'status', 'subject', 'type');
{$ENDIF}
{$IFDEF FHIR_ELIGIBILITYREQUEST}
  CODES_TSearchParamsEligibilityRequest : Array[TSearchParamsEligibilityRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'created', 'enterer', 'facility', 'identifier', 'organization', 'patient', 'provider', 'status');
{$ENDIF}
{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  CODES_TSearchParamsEligibilityResponse : Array[TSearchParamsEligibilityResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'created', 'disposition', 'identifier', 'insurer', 'outcome', 'request', 'request-organization', 'request-provider', 'status');
{$ENDIF}
{$IFDEF FHIR_ENCOUNTER}
  CODES_TSearchParamsEncounter : Array[TSearchParamsEncounter] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'appointment', 'class', 'date', 'diagnosis', 'episodeofcare', 'identifier', 'incomingreferral', 'length', 'location', 'location-period', 'part-of', 'participant', 'participant-type', 'patient', 'practitioner', 'reason', 'service-provider', 'special-arrangement', 'status', 'subject', 'type');
{$ENDIF}
{$IFDEF FHIR_ENDPOINT}
  CODES_TSearchParamsEndpoint : Array[TSearchParamsEndpoint] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'connection-type', 'identifier', 'name', 'organization', 'payload-type', 'status');
{$ENDIF}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  CODES_TSearchParamsEnrollmentRequest : Array[TSearchParamsEnrollmentRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'organization', 'patient', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  CODES_TSearchParamsEnrollmentResponse : Array[TSearchParamsEnrollmentResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'organization', 'request', 'status');
{$ENDIF}
{$IFDEF FHIR_ENTRYDEFINITION}
  CODES_TSearchParamsEntryDefinition : Array[TSearchParamsEntryDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_EPISODEOFCARE}
  CODES_TSearchParamsEpisodeOfCare : Array[TSearchParamsEpisodeOfCare] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'care-manager', 'condition', 'date', 'identifier', 'incomingreferral', 'organization', 'patient', 'status', 'type');
{$ENDIF}
{$IFDEF FHIR_EVENTDEFINITION}
  CODES_TSearchParamsEventDefinition : Array[TSearchParamsEventDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'composed-of', 'date', 'depends-on', 'derived-from', 'description', 'effective', 'identifier', 'jurisdiction', 'name', 'predecessor', 'publisher', 'status', 'successor', 'title', 'topic', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_EXAMPLESCENARIO}
  CODES_TSearchParamsExampleScenario : Array[TSearchParamsExampleScenario] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'description', 'identifier', 'jurisdiction', 'name', 'publisher', 'status', 'title', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_EXPANSIONPROFILE}
  CODES_TSearchParamsExpansionProfile : Array[TSearchParamsExpansionProfile] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'description', 'identifier', 'jurisdiction', 'name', 'publisher', 'status', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  CODES_TSearchParamsExplanationOfBenefit : Array[TSearchParamsExplanationOfBenefit] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'care-team', 'claim', 'coverage', 'created', 'disposition', 'encounter', 'enterer', 'facility', 'identifier', 'organization', 'patient', 'payee', 'provider', 'status');
{$ENDIF}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  CODES_TSearchParamsFamilyMemberHistory : Array[TSearchParamsFamilyMemberHistory] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'code', 'date', 'gender', 'identifier', 'instantiates', 'patient', 'relationship', 'status');
{$ENDIF}
{$IFDEF FHIR_FLAG}
  CODES_TSearchParamsFlag : Array[TSearchParamsFlag] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'author', 'date', 'encounter', 'identifier', 'patient', 'subject');
{$ENDIF}
{$IFDEF FHIR_GOAL}
  CODES_TSearchParamsGoal : Array[TSearchParamsGoal] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'category', 'identifier', 'patient', 'start-date', 'status', 'subject', 'target-date');
{$ENDIF}
{$IFDEF FHIR_GRAPHDEFINITION}
  CODES_TSearchParamsGraphDefinition : Array[TSearchParamsGraphDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'description', 'jurisdiction', 'name', 'publisher', 'start', 'status', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_GROUP}
  CODES_TSearchParamsGroup : Array[TSearchParamsGroup] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'actual', 'characteristic', 'characteristic-value', 'code', 'exclude', 'identifier', 'member', 'type', 'value');
{$ENDIF}
{$IFDEF FHIR_GUIDANCERESPONSE}
  CODES_TSearchParamsGuidanceResponse : Array[TSearchParamsGuidanceResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'patient', 'request', 'subject');
{$ENDIF}
{$IFDEF FHIR_HEALTHCARESERVICE}
  CODES_TSearchParamsHealthcareService : Array[TSearchParamsHealthcareService] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'active', 'category', 'characteristic', 'endpoint', 'identifier', 'location', 'name', 'organization', 'programname', 'type');
{$ENDIF}
{$IFDEF FHIR_IMAGINGSTUDY}
  CODES_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'accession', 'basedon', 'bodysite', 'context', 'dicom-class', 'endpoint', 'identifier', 'modality', 'patient', 'performer', 'reason', 'series', 'started', 'study', 'subject', 'uid');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATION}
  CODES_TSearchParamsImmunization : Array[TSearchParamsImmunization] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'identifier', 'location', 'lot-number', 'manufacturer', 'patient', 'practitioner', 'reason', 'status', 'vaccine-code');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  CODES_TSearchParamsImmunizationEvaluation : Array[TSearchParamsImmunizationEvaluation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'dose-status', 'identifier', 'immunization-event', 'patient', 'target-disease');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  CODES_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'dose-number', 'dose-sequence', 'identifier', 'information', 'patient', 'status', 'support', 'target-disease', 'vaccine-type');
{$ENDIF}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  CODES_TSearchParamsImplementationGuide : Array[TSearchParamsImplementationGuide] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'dependency', 'description', 'experimental', 'jurisdiction', 'name', 'publisher', 'resource', 'status', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_IMPLEMENTATIONGUIDEINPUT}
  CODES_TSearchParamsImplementationGuideInput : Array[TSearchParamsImplementationGuideInput] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'dependency', 'description', 'experimental', 'jurisdiction', 'name', 'publisher', 'resource', 'status', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_IMPLEMENTATIONGUIDEOUTPUT}
  CODES_TSearchParamsImplementationGuideOutput : Array[TSearchParamsImplementationGuideOutput] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'dependency', 'description', 'experimental', 'jurisdiction', 'name', 'publisher', 'resource', 'status', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_INVOICE}
  CODES_TSearchParamsInvoice : Array[TSearchParamsInvoice] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'account', 'date', 'identifier', 'issuer', 'participant', 'participant-role', 'patient', 'recipient', 'status', 'subject', 'totalgross', 'totalnet', 'type');
{$ENDIF}
{$IFDEF FHIR_ITEMINSTANCE}
  CODES_TSearchParamsItemInstance : Array[TSearchParamsItemInstance] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'subject');
{$ENDIF}
{$IFDEF FHIR_LIBRARY}
  CODES_TSearchParamsLibrary : Array[TSearchParamsLibrary] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'composed-of', 'date', 'depends-on', 'derived-from', 'description', 'effective', 'identifier', 'jurisdiction', 'name', 'predecessor', 'publisher', 'status', 'successor', 'title', 'topic', 'type', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_LINKAGE}
  CODES_TSearchParamsLinkage : Array[TSearchParamsLinkage] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'author', 'item', 'source');
{$ENDIF}
{$IFDEF FHIR_LIST}
  CODES_TSearchParamsList : Array[TSearchParamsList] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'code', 'date', 'empty-reason', 'encounter', 'identifier', 'item', 'notes', 'patient', 'source', 'status', 'subject', 'title');
{$ENDIF}
{$IFDEF FHIR_LOCATION}
  CODES_TSearchParamsLocation : Array[TSearchParamsLocation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'endpoint', 'identifier', 'name', 'near', 'near-distance', 'operational-status', 'organization', 'partof', 'status', 'type');
{$ENDIF}
{$IFDEF FHIR_MEASURE}
  CODES_TSearchParamsMeasure : Array[TSearchParamsMeasure] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'composed-of', 'date', 'depends-on', 'derived-from', 'description', 'effective', 'identifier', 'jurisdiction', 'name', 'predecessor', 'publisher', 'status', 'successor', 'title', 'topic', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_MEASUREREPORT}
  CODES_TSearchParamsMeasureReport : Array[TSearchParamsMeasureReport] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'patient', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_MEDIA}
  CODES_TSearchParamsMedia : Array[TSearchParamsMedia] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'based-on', 'category', 'context', 'created', 'device', 'identifier', 'modality', 'operator', 'patient', 'site', 'status', 'subject', 'view');
{$ENDIF}
{$IFDEF FHIR_MEDICATION}
  CODES_TSearchParamsMedication : Array[TSearchParamsMedication] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'code', 'form', 'ingredient', 'ingredient-code', 'manufacturer', 'status');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  CODES_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'code', 'context', 'device', 'effective-time', 'identifier', 'medication', 'patient', 'performer', 'reason-given', 'reason-not-given', 'request', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  CODES_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'code', 'context', 'destination', 'identifier', 'medication', 'patient', 'performer', 'prescription', 'receiver', 'responsibleparty', 'status', 'subject', 'type', 'whenhandedover', 'whenprepared');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONREQUEST}
  CODES_TSearchParamsMedicationRequest : Array[TSearchParamsMedicationRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'authoredon', 'category', 'code', 'context', 'date', 'identifier', 'intended-dispenser', 'intended-performer', 'intended-performertype', 'intent', 'medication', 'patient', 'priority', 'requester', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  CODES_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'category', 'code', 'context', 'effective', 'identifier', 'medication', 'part-of', 'patient', 'source', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_MEDICINALPRODUCT}
  CODES_TSearchParamsMedicinalProduct : Array[TSearchParamsMedicinalProduct] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_MEDICINALPRODUCTAUTHORIZATION}
  CODES_TSearchParamsMedicinalProductAuthorization : Array[TSearchParamsMedicinalProductAuthorization] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_MEDICINALPRODUCTCLINICALS}
  CODES_TSearchParamsMedicinalProductClinicals : Array[TSearchParamsMedicinalProductClinicals] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_MEDICINALPRODUCTDEVICESPEC}
  CODES_TSearchParamsMedicinalProductDeviceSpec : Array[TSearchParamsMedicinalProductDeviceSpec] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_MEDICINALPRODUCTINGREDIENT}
  CODES_TSearchParamsMedicinalProductIngredient : Array[TSearchParamsMedicinalProductIngredient] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_MEDICINALPRODUCTPACKAGED}
  CODES_TSearchParamsMedicinalProductPackaged : Array[TSearchParamsMedicinalProductPackaged] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_MEDICINALPRODUCTPHARMACEUTICAL}
  CODES_TSearchParamsMedicinalProductPharmaceutical : Array[TSearchParamsMedicinalProductPharmaceutical] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_MESSAGEDEFINITION}
  CODES_TSearchParamsMessageDefinition : Array[TSearchParamsMessageDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'category', 'date', 'description', 'event', 'focus', 'identifier', 'jurisdiction', 'name', 'publisher', 'status', 'title', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_MESSAGEHEADER}
  CODES_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'author', 'code', 'destination', 'destination-uri', 'enterer', 'event', 'focus', 'receiver', 'response-id', 'responsible', 'sender', 'source', 'source-uri', 'target');
{$ENDIF}
{$IFDEF FHIR_NAMINGSYSTEM}
  CODES_TSearchParamsNamingSystem : Array[TSearchParamsNamingSystem] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'contact', 'date', 'description', 'id-type', 'jurisdiction', 'kind', 'name', 'period', 'publisher', 'responsible', 'status', 'telecom', 'type', 'value');
{$ENDIF}
{$IFDEF FHIR_NUTRITIONORDER}
  CODES_TSearchParamsNutritionOrder : Array[TSearchParamsNutritionOrder] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'additive', 'datetime', 'encounter', 'formula', 'identifier', 'oraldiet', 'patient', 'provider', 'status', 'supplement');
{$ENDIF}
{$IFDEF FHIR_OBSERVATION}
  CODES_TSearchParamsObservation : Array[TSearchParamsObservation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'based-on', 'category', 'code', 'code-value-concept', 'code-value-date', 'code-value-quantity', 'code-value-string', 'combo-code', 'combo-code-value-concept', 'combo-code-value-quantity', 'combo-data-absent-reason', 'combo-value-concept', 'combo-value-quantity', 'component-code', 'component-code-value-concept', 'component-code-value-quantity', 'component-data-absent-reason', 'component-value-concept', 'component-value-quantity', 'context', 'data-absent-reason', 'date', 'derived-from', 'device', 'dna-variant', 'encounter', 'gene-dnavariant', 'gene-identifier', 'has-member', 'identifier', 'method', 'part-of', 'patient', 'performer', 'specimen', 'status', 'subject', 'value-concept', 'value-date', 'value-quantity', 'value-string');
{$ENDIF}
{$IFDEF FHIR_OBSERVATIONDEFINITION}
  CODES_TSearchParamsObservationDefinition : Array[TSearchParamsObservationDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_OCCUPATIONALDATA}
  CODES_TSearchParamsOccupationalData : Array[TSearchParamsOccupationalData] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'subject');
{$ENDIF}
{$IFDEF FHIR_OPERATIONDEFINITION}
  CODES_TSearchParamsOperationDefinition : Array[TSearchParamsOperationDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'base', 'code', 'context-type', 'date', 'description', 'input-profile', 'instance', 'jurisdiction', 'kind', 'name', 'output-profile', 'publisher', 'status', 'system', 'type', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_OPERATIONOUTCOME}
  CODES_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_ORGANIZATION}
  CODES_TSearchParamsOrganization : Array[TSearchParamsOrganization] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'active', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'endpoint', 'identifier', 'name', 'partof', 'phonetic', 'type');
{$ENDIF}
{$IFDEF FHIR_ORGANIZATIONROLE}
  CODES_TSearchParamsOrganizationRole : Array[TSearchParamsOrganizationRole] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'active', 'date', 'email', 'endpoint', 'identifier', 'location', 'network', 'participating-organization', 'phone', 'primary-organization', 'role', 'service', 'specialty', 'telecom');
{$ENDIF}
{$IFDEF FHIR_PATIENT}
  CODES_TSearchParamsPatient : Array[TSearchParamsPatient] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'active', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'animal-breed', 'animal-species', 'birthdate', 'death-date', 'deceased', 'email', 'family', 'gender', 'general-practitioner', 'given', 'identifier', 'language', 'link', 'name', 'organization', 'phone', 'phonetic', 'telecom');
{$ENDIF}
{$IFDEF FHIR_PAYMENTNOTICE}
  CODES_TSearchParamsPaymentNotice : Array[TSearchParamsPaymentNotice] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'created', 'identifier', 'organization', 'payment-status', 'provider', 'request', 'response', 'status', 'statusdate');
{$ENDIF}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  CODES_TSearchParamsPaymentReconciliation : Array[TSearchParamsPaymentReconciliation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'created', 'disposition', 'identifier', 'organization', 'outcome', 'request', 'request-organization', 'request-provider', 'status');
{$ENDIF}
{$IFDEF FHIR_PERSON}
  CODES_TSearchParamsPerson : Array[TSearchParamsPerson] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'birthdate', 'email', 'gender', 'identifier', 'link', 'name', 'organization', 'patient', 'phone', 'phonetic', 'practitioner', 'relatedperson', 'telecom');
{$ENDIF}
{$IFDEF FHIR_PLANDEFINITION}
  CODES_TSearchParamsPlanDefinition : Array[TSearchParamsPlanDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'composed-of', 'date', 'definition', 'depends-on', 'derived-from', 'description', 'effective', 'identifier', 'jurisdiction', 'name', 'predecessor', 'publisher', 'status', 'successor', 'title', 'topic', 'type', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_PRACTITIONER}
  CODES_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'active', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'communication', 'email', 'family', 'gender', 'given', 'identifier', 'name', 'phone', 'phonetic', 'telecom');
{$ENDIF}
{$IFDEF FHIR_PRACTITIONERROLE}
  CODES_TSearchParamsPractitionerRole : Array[TSearchParamsPractitionerRole] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'active', 'date', 'email', 'endpoint', 'identifier', 'location', 'organization', 'phone', 'practitioner', 'role', 'service', 'specialty', 'telecom');
{$ENDIF}
{$IFDEF FHIR_PROCEDURE}
  CODES_TSearchParamsProcedure : Array[TSearchParamsProcedure] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'based-on', 'category', 'code', 'context', 'date', 'encounter', 'identifier', 'instantiates', 'location', 'part-of', 'patient', 'performer', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_PROCESSREQUEST}
  CODES_TSearchParamsProcessRequest : Array[TSearchParamsProcessRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'action', 'identifier', 'organization', 'provider', 'status');
{$ENDIF}
{$IFDEF FHIR_PROCESSRESPONSE}
  CODES_TSearchParamsProcessResponse : Array[TSearchParamsProcessResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'organization', 'request', 'request-organization', 'request-provider', 'status');
{$ENDIF}
{$IFDEF FHIR_PRODUCTPLAN}
  CODES_TSearchParamsProductPlan : Array[TSearchParamsProductPlan] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'administered-by', 'endpoint', 'identifier', 'name', 'owned-by', 'phonetic', 'status', 'type');
{$ENDIF}
{$IFDEF FHIR_PROVENANCE}
  CODES_TSearchParamsProvenance : Array[TSearchParamsProvenance] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'agent', 'agent-role', 'agent-type', 'entity-id', 'entity-ref', 'location', 'patient', 'recorded', 'signature-type', 'target', 'when');
{$ENDIF}
{$IFDEF FHIR_QUESTIONNAIRE}
  CODES_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'code', 'context-type', 'date', 'definition', 'description', 'effective', 'identifier', 'jurisdiction', 'name', 'publisher', 'status', 'subject-type', 'title', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  CODES_TSearchParamsQuestionnaireResponse : Array[TSearchParamsQuestionnaireResponse] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'author', 'authored', 'based-on', 'context', 'identifier', 'part-of', 'patient', 'questionnaire', 'source', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_RELATEDPERSON}
  CODES_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'active', 'address', 'address-city', 'address-country', 'address-postalcode', 'address-state', 'address-use', 'birthdate', 'email', 'gender', 'identifier', 'name', 'patient', 'phone', 'phonetic', 'telecom');
{$ENDIF}
{$IFDEF FHIR_REQUESTGROUP}
  CODES_TSearchParamsRequestGroup : Array[TSearchParamsRequestGroup] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'author', 'authored', 'code', 'context', 'definition', 'encounter', 'group-identifier', 'identifier', 'intent', 'participant', 'patient', 'priority', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_RESEARCHSTUDY}
  CODES_TSearchParamsResearchStudy : Array[TSearchParamsResearchStudy] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'category', 'date', 'focus', 'identifier', 'keyword', 'location', 'partof', 'principalinvestigator', 'protocol', 'site', 'sponsor', 'status', 'title');
{$ENDIF}
{$IFDEF FHIR_RESEARCHSUBJECT}
  CODES_TSearchParamsResearchSubject : Array[TSearchParamsResearchSubject] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'identifier', 'individual', 'patient', 'status', 'study');
{$ENDIF}
{$IFDEF FHIR_RISKASSESSMENT}
  CODES_TSearchParamsRiskAssessment : Array[TSearchParamsRiskAssessment] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'condition', 'date', 'encounter', 'identifier', 'method', 'patient', 'performer', 'probability', 'risk', 'subject');
{$ENDIF}
{$IFDEF FHIR_SCHEDULE}
  CODES_TSearchParamsSchedule : Array[TSearchParamsSchedule] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'active', 'actor', 'date', 'identifier', 'type');
{$ENDIF}
{$IFDEF FHIR_SEARCHPARAMETER}
  CODES_TSearchParamsSearchParameter : Array[TSearchParamsSearchParameter] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'base', 'code', 'component', 'context-type', 'date', 'derived-from', 'description', 'jurisdiction', 'name', 'publisher', 'status', 'target', 'type', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_SEQUENCE}
  CODES_TSearchParamsSequence : Array[TSearchParamsSequence] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'chromosome', 'coordinate', 'end', 'identifier', 'patient', 'start', 'type');
{$ENDIF}
{$IFDEF FHIR_SERVICEDEFINITION}
  CODES_TSearchParamsServiceDefinition : Array[TSearchParamsServiceDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'composed-of', 'date', 'depends-on', 'derived-from', 'description', 'effective', 'identifier', 'jurisdiction', 'name', 'predecessor', 'publisher', 'status', 'successor', 'title', 'topic', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_SERVICEREQUEST}
  CODES_TSearchParamsServiceRequest : Array[TSearchParamsServiceRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'authored', 'based-on', 'body-site', 'category', 'code', 'context', 'encounter', 'identifier', 'instantiates', 'intent', 'occurrence', 'patient', 'performer', 'performer-type', 'priority', 'replaces', 'requester', 'requisition', 'specimen', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_SLOT}
  CODES_TSearchParamsSlot : Array[TSearchParamsSlot] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'schedule', 'slot-type', 'start', 'status');
{$ENDIF}
{$IFDEF FHIR_SPECIMEN}
  CODES_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'accession', 'bodysite', 'collected', 'collector', 'container', 'container-id', 'identifier', 'parent', 'patient', 'status', 'subject', 'type');
{$ENDIF}
{$IFDEF FHIR_SPECIMENDEFINITION}
  CODES_TSearchParamsSpecimenDefinition : Array[TSearchParamsSpecimenDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'container', 'identifier', 'type');
{$ENDIF}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  CODES_TSearchParamsStructureDefinition : Array[TSearchParamsStructureDefinition] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'abstract', 'base', 'base-path', 'context-type', 'date', 'derivation', 'description', 'experimental', 'ext-context', 'identifier', 'jurisdiction', 'keyword', 'kind', 'name', 'path', 'publisher', 'status', 'title', 'type', 'url', 'valueset', 'version');
{$ENDIF}
{$IFDEF FHIR_STRUCTUREMAP}
  CODES_TSearchParamsStructureMap : Array[TSearchParamsStructureMap] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'description', 'identifier', 'jurisdiction', 'name', 'publisher', 'status', 'title', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_SUBSCRIPTION}
  CODES_TSearchParamsSubscription : Array[TSearchParamsSubscription] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'add-tag', 'contact', 'criteria', 'payload', 'status', 'type', 'url');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCE}
  CODES_TSearchParamsSubstance : Array[TSearchParamsSubstance] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'category', 'code', 'container-identifier', 'expiry', 'identifier', 'quantity', 'status', 'substance-reference');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCEPOLYMER}
  CODES_TSearchParamsSubstancePolymer : Array[TSearchParamsSubstancePolymer] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  CODES_TSearchParamsSubstanceReferenceInformation : Array[TSearchParamsSubstanceReferenceInformation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCESPECIFICATION}
  CODES_TSearchParamsSubstanceSpecification : Array[TSearchParamsSubstanceSpecification] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text');
{$ENDIF}
{$IFDEF FHIR_SUPPLYDELIVERY}
  CODES_TSearchParamsSupplyDelivery : Array[TSearchParamsSupplyDelivery] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'patient', 'receiver', 'status', 'supplier');
{$ENDIF}
{$IFDEF FHIR_SUPPLYREQUEST}
  CODES_TSearchParamsSupplyRequest : Array[TSearchParamsSupplyRequest] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'category', 'date', 'identifier', 'requester', 'status', 'supplier');
{$ENDIF}
{$IFDEF FHIR_TASK}
  CODES_TSearchParamsTask : Array[TSearchParamsTask] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'authored-on', 'based-on', 'business-status', 'code', 'context', 'focus', 'group-identifier', 'identifier', 'intent', 'modified', 'owner', 'part-of', 'patient', 'performer', 'period', 'priority', 'requester', 'status', 'subject');
{$ENDIF}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  CODES_TSearchParamsTerminologyCapabilities : Array[TSearchParamsTerminologyCapabilities] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'description', 'jurisdiction', 'name', 'publisher', 'status', 'title', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_TESTREPORT}
  CODES_TSearchParamsTestReport : Array[TSearchParamsTestReport] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'identifier', 'issued', 'participant', 'result', 'tester', 'testscript');
{$ENDIF}
{$IFDEF FHIR_TESTSCRIPT}
  CODES_TSearchParamsTestScript : Array[TSearchParamsTestScript] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'date', 'description', 'identifier', 'jurisdiction', 'name', 'publisher', 'status', 'testscript-capability', 'title', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_USERSESSION}
  CODES_TSearchParamsUserSession : Array[TSearchParamsUserSession] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'focus', 'identifier', 'patient', 'status', 'user', 'workstation');
{$ENDIF}
{$IFDEF FHIR_VALUESET}
  CODES_TSearchParamsValueSet : Array[TSearchParamsValueSet] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'code', 'date', 'description', 'expansion', 'identifier', 'jurisdiction', 'name', 'publisher', 'reference', 'status', 'title', 'url', 'version');
{$ENDIF}
{$IFDEF FHIR_VERIFICATIONRESULT}
  CODES_TSearchParamsVerificationResult : Array[TSearchParamsVerificationResult] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'target');
{$ENDIF}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  CODES_TSearchParamsVisionPrescription : Array[TSearchParamsVisionPrescription] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_source', '_tag', '_text', 'datewritten', 'encounter', 'identifier', 'patient', 'prescriber', 'status');
{$ENDIF}
  FHIR_GENERATED_VERSION = '3.2.0';
  FHIR_GENERATED_VERSION_BASE = '3.2';

  FHIR_GENERATED_PUBLICATION = '4';

  FHIR_GENERATED_DATE = '2017-12-20T12:10:38+11:00';



implementation

end.

